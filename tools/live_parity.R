# Live parity check: does the local server give OpenAlex's answers?
#
# 1. Download a corpus Q from OpenAlex with openalexPro (all fields).
# 2. Serve it locally with serve_corpus().
# 3. For sub-queries "Q AND f" and for group_by dimensions, compare the count
#    (or the groups) from OpenAlex with the local one.
#
# Because the local corpus is exactly Q's result set, every filter count and
# every group must be equal -- except where OpenAlex's data changed between
# the download and the check (e.g. cited_by_count), and for search, where the
# local index (title + abstract, Porter stemmer) is expected to differ; search
# rows are reported, not counted as failures.
#
# Run from the package root:  Rscript tools/live_parity.R
# Costs: one download of Q (|Q| / 200 requests) plus one request per check;
# with the default Q (a few hundred works) well under 100 requests. Uses
# openalexPro::pro_api_key() if set, else OpenAlex's shared free budget.
# Writes: tools/live_parity.csv.
#
# Not run yet: written, not verified against OpenAlex.

pkgload::load_all(quiet = TRUE)

# Q: a small, varied corpus (several types, languages, countries, OA states)
q_filter <- "primary_topic.id:T10341,publication_year:2015"
checks <- c(
  "type:article", "type:!article", "type:article|review", "language:en",
  "open_access.is_oa:true", "oa_status:gold", "best_oa_location.license:cc-by",
  "has_doi:true", "has_abstract:true", "has_references:false",
  "cited_by_count:>50", "fwci:>1", "authors_count:<4",
  "authorships.countries:au", "institutions.country_code:us",
  "authorships.institutions.type:education", "primary_location.source.type:journal",
  "primary_topic.field.id:23", "sustainable_development_goals.id:14",
  "is_retracted:false", "from_publication_date:2015-06-01", "primary_location.source.id:null",
  "authorships.institutions.lineage:I2801453606", "funders.id:F4320306076",
  "publication_date:2015-03-01", "from_created_date:2016-01-01",
  "citation_normalized_percentile.value:>0.9", "citation_normalized_percentile.is_in_top_1_percent:true",
  "open_access.any_repository_has_fulltext:true", "primary_location.source.issn:0722-4028",
  "primary_location.source.publisher_lineage:P4310319965", "primary_topic.subfield.id:2303",
  "raw_affiliation_strings.search:james cook university",
  "raw_affiliation_strings.search:\"james cook university\"",
  "raw_author_name.search:hughes"
)
searches <- c("bleaching", "\"coral reef\"", "temperature AND symbiont", "coral NOT bleaching",
              "\"coral bleaching\"~3")
group_dims <- c("type", "language", "open_access.oa_status", "authorships.countries",
                "primary_location.source.type", "is_retracted", "authorships.institutions.lineage",
                "funders.id", "primary_topic.subfield.id", "primary_location.source.issn",
                "citation_normalized_percentile.is_in_top_10_percent")

oa <- "https://api.openalex.org"
key <- tryCatch(openalexPro::pro_api_key(), error = function(e) NULL)
get_json <- function(root, path) {
  req <- httr2::request(paste0(root, path)) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_throttle(capacity = 5, fill_time_s = 1)
  if (identical(root, oa) && length(key) == 1L && nzchar(key)) {
    req <- httr2::req_auth_bearer_token(req, key)
  }
  httr2::resp_body_json(httr2::req_perform(req))
}
count_of <- function(root, query) {
  x <- get_json(root, paste0("/works?per_page=1&select=id&", query))
  x$meta$count %||% NA
}

# 1. download Q
work <- file.path(tempdir(), "oaapi-parity")
unlink(work, recursive = TRUE)
n_q <- count_of(oa, paste0("filter=", q_filter))
message("Q = ", q_filter, ": ", n_q, " works on OpenAlex")
stopifnot(n_q > 0, n_q < 5000)
openalexPro::pro_fetch(
  openalexPro::pro_query(entity = "works", primary_topic.id = "T10341",
                         publication_year = 2015),
  project_folder = work, progress = FALSE, verbose = FALSE
)

# 2. serve it
port <- httpuv::randomPort()
srv <- callr::r_bg(function(pkg, corpus, port) {
  pkgload::load_all(pkg, quiet = TRUE)
  serve_corpus(corpus, port = port, cache_dir = file.path(tempdir(), "oaapi-parity-cache"))
}, args = list(pkg = normalizePath("."), corpus = file.path(work, "parquet"), port = port))
on.exit(srv$kill(), add = TRUE)
local <- paste0("http://127.0.0.1:", port)
for (i in 1:150) {
  if (!is.null(tryCatch(get_json(local, "/corpus"), error = function(e) NULL))) break
  Sys.sleep(0.2)
}

# 3. compare
rows <- list()
add <- function(kind, query, a, b, expected_diff = FALSE) {
  rows[[length(rows) + 1L]] <<- data.frame(
    kind = kind, query = query, openalex = a, local = b,
    status = if (identical(a, b)) "equal" else if (expected_diff) "differs (expected)" else "DIFFERS"
  )
}
add("corpus", q_filter, n_q, count_of(local, ""))
for (f in checks) {
  fe <- utils::URLencode(f, reserved = FALSE)
  add("filter", f, count_of(oa, paste0("filter=", q_filter, ",", fe)),
      count_of(local, paste0("filter=", fe)),
      expected_diff = grepl("cited_by_count|fwci", f))
}
for (s in searches) {
  enc <- utils::URLencode(s, reserved = TRUE)
  add("search", s, count_of(oa, paste0("filter=", q_filter, "&search=", enc)),
      count_of(local, paste0("search=", enc)), expected_diff = TRUE)
}
for (d in group_dims) {
  g_oa <- get_json(oa, paste0("/works?filter=", q_filter, "&group_by=", d))$group_by
  g_lo <- get_json(local, paste0("/works?group_by=", d))$group_by
  to_map <- function(g) stats::setNames(vapply(g, function(x) x$count, 1),
                                        vapply(g, function(x) x$key, ""))
  a <- to_map(g_oa)
  b <- to_map(g_lo)
  keys <- union(names(a), names(b))
  same <- setequal(names(a), names(b)) && all(a[keys] == b[keys])
  add("group_by", d, length(a), length(b))
  rows[[length(rows)]]$status <- if (same) "equal" else "DIFFERS"
  if (!same) {
    bad <- keys[is.na(a[keys]) | is.na(b[keys]) | a[keys] != b[keys]]
    message("group_by ", d, " differs for: ",
            paste(sprintf("%s (%s vs %s)", bad, a[bad], b[bad]), collapse = "; "))
  }
}

result <- do.call(rbind, rows)
utils::write.csv(result, "tools/live_parity.csv", row.names = FALSE)
print(result, row.names = FALSE)
cat(sprintf("\n%d checks: %d equal, %d differ as expected, %d DIFFER\n", nrow(result),
            sum(result$status == "equal"), sum(result$status == "differs (expected)"),
            sum(result$status == "DIFFERS")))
