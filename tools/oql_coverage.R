# Coverage report: which of OpenAlex's own OQL example queries can openalexAPI run?
#
# OpenAlex keeps a normative corpus of OQL examples with their OQO
# (docs/oql/corpus.yaml in ourresearch/openalex-elastic-api; real-world
# questions, librarian search strategies, vendor docs, support tickets). Each
# row carries its OQO, so no translation calls are needed. For every works
# row that OpenAlex can run (status ok / hint), this script compiles the OQO
# with oqo_to_sql() and runs it on the test corpus, then reports:
#
#   * how many rows run, and why the others do not;
#   * the unsupported filter, group_by and sort columns, by how often they are
#     needed -- the list of what to add to the registry next.
#
# Run from the package root:  Rscript tools/oql_coverage.R
# Needs: network (one download of corpus.yaml, from GitHub), yaml, pkgload.
# Writes: tools/oql_coverage.csv (one row per example).
#
# Known issue (open): one of the examples runs for minutes on the 369-work
# test corpus, so a full run does not finish in reasonable time yet. Likely
# one of the long systematic-review strategies (up to 114 search leaves, each
# a BM25 lookup per work); to be identified and fixed in openalexAPI.

pkgload::load_all(quiet = TRUE)

url <- paste0("https://raw.githubusercontent.com/ourresearch/openalex-elastic-api/",
              "master/docs/oql/corpus.yaml")
yml <- tempfile(fileext = ".yaml")
utils::download.file(url, yml, quiet = TRUE, mode = "wb")
txt <- rawToChar(readBin(yml, "raw", file.size(yml)))
Encoding(txt) <- "UTF-8"
rows <- yaml::yaml.load(txt)$rows

db <- prepare_corpus("tests/testthat/fixtures/corpus_jq",
                     cache_dir = file.path(tempdir(), "oaapi-coverage"))
con <- openalexAPI:::.api_open(db)
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
ctx <- list(
  con = con,
  columns = DBI::dbGetQuery(con, "DESCRIBE works")$column_name,
  served = openalexAPI:::.api_served_columns(con)
)
registry <- names(openalexAPI:::.api_registry_works())
groups <- names(openalexAPI:::.api_groups_works())

# every column_id in a filter tree
leaf_columns <- function(rows) {
  unlist(lapply(rows, function(r) {
    if (!is.null(r$filters)) leaf_columns(r$filters) else r$column_id
  }))
}

result <- do.call(rbind, lapply(rows, function(r) {
  oqo <- r$oqo
  status <- r$status %||% ""
  base <- data.frame(id = r$id, status = status, get_rows = oqo$get_rows %||% NA,
                     tags = paste(unlist(r$tags), collapse = " "), outcome = NA,
                     reason = NA, missing = NA, oql = r$oql %||% NA)
  if (!status %in% c("ok", "hint")) {
    base$outcome <- "skipped"
    base$reason <- paste("OpenAlex status:", status)
    return(base)
  }
  if (!identical(oqo$get_rows %||% "works", "works")) {
    base$outcome <- "skipped"
    base$reason <- paste("entity:", oqo$get_rows)
    return(base)
  }
  cols <- unique(leaf_columns(oqo$filter_rows))
  gb <- vapply(oqo$group_by %||% list(), function(g) sub(":include_unknown$", "", g$column_id), "")
  sorts <- vapply(oqo$sort_by %||% list(), function(s) s$column_id, "")
  missing <- c(
    setdiff(cols, registry),
    paste0("group_by:", setdiff(gb, groups)),
    paste0("sort:", setdiff(sorts, c(registry, "relevance_score", "count", "key")))
  )
  missing <- missing[!grepl(":$", missing)]
  base$missing <- paste(missing, collapse = " ")
  # run it: the compiler and the query on the test corpus
  res <- tryCatch({
    openalexAPI:::.api_execute(ctx, oqo, list(per_page = 1, select = "id"))
    list(outcome = "runs", reason = NA)
  }, oaapi_error = function(e) {
    list(outcome = "400", reason = conditionMessage(e))
  }, error = function(e) {
    list(outcome = "ERROR", reason = conditionMessage(e))
  })
  base$outcome <- res$outcome
  base$reason <- substr(sub("Supported: .*", "", res$reason %||% NA), 1, 200)
  base
}))

utils::write.csv(result, "tools/oql_coverage.csv", row.names = FALSE)

works <- result[result$outcome != "skipped", ]
cat(sprintf("OpenAlex OQL corpus: %d rows, %d runnable works queries (status ok/hint)\n",
            nrow(result), nrow(works)))
cat(sprintf("  run locally:  %d (%.0f%%)\n", sum(works$outcome == "runs"),
            100 * mean(works$outcome == "runs")))
cat(sprintf("  refused (400): %d\n", sum(works$outcome == "400")))
cat(sprintf("  ERRORS (bugs): %d\n", sum(works$outcome == "ERROR")))
if (any(works$outcome == "ERROR")) {
  print(works[works$outcome == "ERROR", c("id", "reason")], row.names = FALSE)
}
need <- unlist(strsplit(as.character(works$missing[works$outcome == "400"]), " "))
need <- need[nzchar(need)]
if (length(need)) {
  cat("\nUnsupported columns, by number of queries that need them:\n")
  tab <- sort(table(need), decreasing = TRUE)
  print(data.frame(column = names(tab), queries = as.integer(tab)), row.names = FALSE)
}
other <- works[works$outcome == "400" & !nzchar(works$missing), ]
if (nrow(other)) {
  cat("\nRefused for other reasons:\n")
  print(table(sub("`[^`]*`", "`...`", other$reason)))
}
