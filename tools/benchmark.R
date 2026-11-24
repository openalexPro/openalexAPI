# Speed benchmark on a realistic corpus (~11,000 works).
#
# Downloads the corpus once (openalexPro, ~55 requests) into
# tools::R_user_dir("openalexAPI", "cache")/bench_corpus, prepares it, and
# times every feature as OQO, directly against the database (no HTTP, no
# translation): filters, paging, sample, select, group_by on every column,
# search in all forms up to OpenAlex's 114-term systematic-review example,
# single works. Prints the cases sorted by time; anything over 1 s is a
# candidate to fix.
#
# Run from the package root:  Rscript tools/benchmark.R
# Writes: tools/benchmark.csv

pkgload::load_all(quiet = TRUE)

corpus <- file.path(tools::R_user_dir("openalexAPI", "cache"), "bench_corpus")
if (!dir.exists(file.path(corpus, "parquet"))) {
  message("Downloading the benchmark corpus...")
  openalexPro::pro_fetch(
    openalexPro::pro_query(entity = "works", primary_topic.id = "T10341",
                           publication_year = "2017-2020"),
    project_folder = corpus, progress = FALSE, verbose = FALSE
  )
}

# the prepared database is kept between runs; OAAPI_BENCH_REBUILD=true times a fresh build
cache <- file.path(tools::R_user_dir("openalexAPI", "cache"), "bench_db")
if (identical(Sys.getenv("OAAPI_BENCH_REBUILD"), "true")) unlink(cache, recursive = TRUE)
t_prep <- system.time(db <- suppressMessages(prepare_corpus(file.path(corpus, "parquet"),
                                                            cache_dir = cache)))[["elapsed"]]
con <- openalexAPI:::.api_open(db)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
ctx <- list(con = con, columns = DBI::dbGetQuery(con, "DESCRIBE works")$column_name,
            served = openalexAPI:::.api_served_columns(con))
n_works <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM works")$n
message(sprintf("%d works; prepare_corpus(): %.1f s; database %.0f MB", n_works, t_prep,
                file.size(db) / 1e6))

# sample values from the corpus itself
one <- function(sql) DBI::dbGetQuery(con, sql)[[1]][[1]]
w <- DBI::dbGetQuery(con, "SELECT id, doi, oaapi_refs FROM works
                           WHERE doi IS NOT NULL AND len(oaapi_refs) > 5 LIMIT 1")
author <- one("SELECT unnest(list_transform(authorships, lambda a: a.author.id)) FROM works LIMIT 1")
inst <- one("SELECT unnest(flatten(list_transform(authorships, lambda a:
               list_transform(a.institutions, lambda i: i.id)))) FROM works LIMIT 1 OFFSET 3")
ids50 <- DBI::dbGetQuery(con, "SELECT id FROM works USING SAMPLE 50 ROWS")$id

L <- function(col, v, op = NULL, neg = FALSE) {
  Filter(Negate(is.null), list(column_id = col, value = v, operator = op, is_negated = neg))
}
S <- function(col, v, neg = FALSE) list(column_id = col, value = v, operator = "has", is_negated = neg)
OR <- function(...) list(join = "or", filters = list(...))
q <- function(rows = NULL, group = NULL, sort = NULL, sample = NULL, seed = NULL) {
  Filter(Negate(is.null), list(get_rows = "works", filter_rows = rows,
    group_by = if (!is.null(group)) list(list(column_id = group)),
    sort_by = sort, sample = sample, seed = seed))
}

# OpenAlex's 114-term systematic-review example (row 78 of its OQL corpus)
yml <- tempfile(fileext = ".yaml")
row78 <- tryCatch({
  utils::download.file(paste0("https://raw.githubusercontent.com/ourresearch/",
    "openalex-elastic-api/master/docs/oql/corpus.yaml"), yml, quiet = TRUE, mode = "wb")
  txt <- rawToChar(readBin(yml, "raw", file.size(yml)))
  Encoding(txt) <- "UTF-8"
  Filter(function(x) identical(as.integer(x$id), 78L), yaml::yaml.load(txt)$rows)[[1]]$oqo
}, error = function(e) NULL)

words <- c("bleaching", "temperature", "symbiont", "fish", "reef", "coral", "climate",
           "ocean", "acidification", "larvae", "algae", "recovery", "disease", "genome",
           "sediment", "nutrient", "herbivore", "heat", "stress", "restoration")
search20 <- do.call(OR, lapply(words, function(x) S("fulltext.search", x)))

cases <- list(
  "no filter, page 1" = list(q()),
  "type" = list(q(list(L("type", "article")))),
  "year range" = list(q(list(L("publication_year", 2018, ">="), L("publication_year", 2019, "<=")))),
  "negation" = list(q(list(L("type", "article", neg = TRUE)))),
  "country (list)" = list(q(list(L("authorships.countries", "AU")))),
  "institution (list of lists)" = list(q(list(L("authorships.institutions.id", inst)))),
  "author" = list(q(list(L("authorships.author.id", author)))),
  "OR of 50 IDs" = list(q(list(do.call(OR, lapply(ids50, function(i) L("ids.openalex", i)))))),
  "doi" = list(q(list(L("doi", w$doi)))),
  "cites" = list(q(list(L("referenced_works", w$oaapi_refs[[1]][[1]])))),
  "cited_by" = list(q(list(L("cited_by", w$id)))),
  "null (missing source)" = list(q(list(list(column_id = "primary_location.source.id", value = NULL)))),
  "8 filters AND" = list(q(list(L("type", "article"), L("language", "en"),
    L("open_access.is_oa", TRUE), L("publication_year", 2018, ">="), L("has_abstract", TRUE),
    L("cited_by_count", 5, ">"), L("authorships.countries", "US"), L("is_retracted", FALSE)))),
  "sort cited_by_count" = list(q(sort = list(list(column_id = "cited_by_count", direction = "desc")))),
  "deep page (200 per page, page 50)" = list(q(), list(per_page = 200, page = 50)),
  "all fields, 200 per page" = list(q(), list(per_page = 200, select = NULL)),
  "sample 500, page 2" = list(q(sample = 500, seed = "1"), list(per_page = 200, page = 2)),
  "single work by id" = list("single", w$id),
  "single work by doi" = list("single", w$doi),
  "search 1 word" = list(q(list(S("fulltext.search", "bleaching")))),
  "search 3 words" = list(q(list(S("fulltext.search", "coral heat stress")))),
  "search phrase" = list(q(list(S("fulltext.search", '"heat stress"')))),
  "search exact" = list(q(list(S("fulltext.search.exact", "bleaching")))),
  "search wildcard" = list(q(list(S("fulltext.search.exact", "acidif*")))),
  "search title only" = list(q(list(S("display_name.search", "bleaching")))),
  "search NOT" = list(q(list(S("fulltext.search", "coral"), S("fulltext.search", "bleaching", TRUE)))),
  "search 20-term OR" = list(q(list(search20))),
  "search + filters + sort relevance" = list(q(list(S("fulltext.search", "bleaching"),
    L("type", "article")), sort = list(list(column_id = "relevance_score", direction = "desc")))),
  "search + group_by country" = list(q(list(S("fulltext.search", "bleaching")), group = "authorships.countries")),
  "OpenAlex example: 114-term search" = if (!is.null(row78)) list(row78)
)
for (g in names(openalexAPI:::.api_groups_works())) {
  cases[[paste("group_by", g)]] <- list(q(group = g))
}
cases[["group_by source:include_unknown"]] <- list(q(group = "primary_location.source.id:include_unknown"))
cases <- Filter(Negate(is.null), cases)

time_case <- function(case) {
  t0 <- Sys.time()
  res <- tryCatch({
    if (identical(case[[1]], "single")) {
      openalexAPI:::.api_single(ctx, case[[2]])
    } else {
      view <- if (length(case) > 1) case[[2]] else list(per_page = 25)
      if (!"select" %in% names(view)) view$select <- "id,display_name"
      if (is.null(view$select)) view$select <- NULL
      openalexAPI:::.api_execute(ctx, case[[1]], view)
    }
    "ok"
  }, error = function(e) paste("error:", substr(conditionMessage(e), 1, 80)))
  list(seconds = round(as.numeric(Sys.time() - t0, units = "secs"), 2), result = res)
}

# warm up (FTS extension, first query)
invisible(time_case(cases[[1]]))
only <- Sys.getenv("OAAPI_BENCH_ONLY") # a regex to run some cases only
if (nzchar(only)) cases <- cases[grepl(only, names(cases))]
out <- do.call(rbind, lapply(names(cases), function(nm) {
  r <- time_case(cases[[nm]])
  message(sprintf("%6.2f s  %s %s", r$seconds, nm, if (r$result == "ok") "" else r$result))
  data.frame(case = nm, seconds = r$seconds, result = r$result)
}))
out <- out[order(-out$seconds), ]
utils::write.csv(out, "tools/benchmark.csv", row.names = FALSE)
cat(sprintf("\n%d works, prepare_corpus() %.1f s\n\n", n_works, t_prep))
print(out, row.names = FALSE)
cat(sprintf("\nslow (> 1 s): %d of %d\n", sum(out$seconds > 1), nrow(out)))
