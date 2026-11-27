test_that("prepare_corpus() caches by file state and rebuilds on change", {
  cache <- withr::local_tempdir()
  dir <- withr::local_tempdir()
  file.copy(list.files(fixture("corpus_api"), full.names = TRUE), dir)
  db1 <- prepare_corpus(dir, cache_dir = cache)
  t1 <- file.mtime(db1)
  expect_identical(prepare_corpus(dir, cache_dir = cache), db1)
  expect_identical(file.mtime(db1), t1)

  Sys.setFileTime(list.files(dir, full.names = TRUE)[[1]], Sys.time() + 60)
  expect_false(identical(prepare_corpus(dir, cache_dir = cache), db1))
  expect_error(prepare_corpus(withr::local_tempdir(), cache_dir = cache), "No Parquet")
})

test_that("the database goes next to the corpus by default", {
  project <- withr::local_tempdir()
  dir <- file.path(project, "parquet")
  dir.create(dir)
  file.copy(list.files(fixture("corpus_api"), full.names = TRUE), dir)
  db <- prepare_corpus(dir)
  expect_identical(dirname(db), normalizePath(project))
})

test_that("duplicate works across partitions are served once", {
  dir <- withr::local_tempdir()
  for (part in c("query=a", "query=b")) {
    dir.create(file.path(dir, part))
    file.copy(list.files(fixture("corpus_api"), full.names = TRUE), file.path(dir, part))
  }
  con <- openalexAPI:::.api_open(prepare_corpus(dir, cache_dir = withr::local_tempdir()))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_equal(n_sql(con, "true"), 2)
  expect_false("query" %in% DBI::dbGetQuery(con, "DESCRIBE works")$column_name)
})

test_that("abstracts: text for search, inverted index served, added columns hidden", {
  for (name in c("corpus_jq", "corpus_api")) {
    con <- local_fixture_con(name)
    # the served inverted index rebuilds exactly the abstract text
    bad <- n_sql(con, paste0("oaapi_abstract IS NOT NULL AND (",
                             openalexPro::oa_works_abstract_sql(), ") <> oaapi_abstract"))
    expect_equal(bad, 0)
    expect_equal(n_sql(con, "oaapi_abstract IS NOT NULL"),
                 n_sql(con, "abstract_inverted_index IS NOT NULL"))
    served <- openalexAPI:::.api_served_columns(con)
    expect_true("abstract_inverted_index" %in% served)
    expect_false(any(c("abstract", "citation", "page", "relevance_score") %in% served))
    expect_false(any(startsWith(served, "oaapi_")))
  }
})

test_that("Parquet files that hold no works are skipped", {
  dir <- withr::local_tempdir()
  file.copy(list.files(fixture("corpus_api"), full.names = TRUE), dir)
  con0 <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con0, sprintf(
    "COPY (SELECT 'W1' AS \"from\", 'W2' AS \"to\") TO '%s/edges.parquet' (FORMAT parquet)", dir))
  dir.create(file.path(dir, "members"))
  DBI::dbExecute(con0, sprintf(
    "COPY (SELECT 'W9' AS id, now() AS added_at) TO '%s/members/m.parquet' (FORMAT parquet)", dir))
  DBI::dbDisconnect(con0, shutdown = TRUE)
  expect_message(db <- prepare_corpus(dir, cache_dir = withr::local_tempdir()), "Skipping 2")
  con <- openalexAPI:::.api_open(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_equal(n_sql(con, "true"), 2)
  expect_equal(n_sql(con, "id IS NULL"), 0)
})

test_that("the build version is part of the cache key", {
  cache <- withr::local_tempdir()
  db1 <- prepare_corpus(fixture("corpus_api"), cache_dir = cache)
  local_mocked_bindings(.api_build_version = "test")
  expect_false(identical(prepare_corpus(fixture("corpus_api"), cache_dir = cache), db1))
})

test_that("the translation cache is capped", {
  local_mocked_bindings(.api_forward_query = function(body, root, api_key = NULL) {
    list(status = 200L, body = '{"oqo":{"get_rows":"works"},"validation":{"valid":true}}')
  }, .api_cache_max = 3L)
  cache <- openalexAPI:::.api_cache
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  for (i in 1:5) openalexAPI:::.api_translate(list(oql = paste("q", i)), "http://x")
  expect_lte(length(cache), 3L)
  expect_true(length(cache) >= 1L)
})

test_that("filter columns whose type differs between files are cast back", {
  dir <- withr::local_tempdir()
  con0 <- DBI::dbConnect(duckdb::duckdb())
  src <- sprintf("read_parquet('%s/**/*.parquet', union_by_name = true)", fixture("corpus_jq"))
  DBI::dbExecute(con0, sprintf(
    "COPY (SELECT * FROM %s ORDER BY id LIMIT 200) TO '%s/a.parquet' (FORMAT parquet)", src, dir))
  # the other works: cited_by_count as text (one unreadable), year as double,
  # and a non-filter column with two types
  DBI::dbExecute(con0, sprintf(
    "COPY (SELECT * EXCLUDE (rn) REPLACE (
       CASE WHEN rn = 1 THEN 'n/a' ELSE CAST(cited_by_count AS VARCHAR) END AS cited_by_count,
       CAST(publication_year AS DOUBLE) AS publication_year,
       CAST(locations_count AS VARCHAR) AS locations_count)
     FROM (SELECT *, row_number() OVER (ORDER BY id) AS rn FROM (
       SELECT * FROM %s ORDER BY id OFFSET 200)))
     TO '%s/b.parquet' (FORMAT parquet)", src, dir))
  DBI::dbDisconnect(con0, shutdown = TRUE)

  msgs <- testthat::capture_messages(db <- prepare_corpus(dir, cache_dir = withr::local_tempdir()))
  expect_true(any(grepl("`cited_by_count` .*converted back to BIGINT \\(1 value", msgs)))
  expect_true(any(grepl("`publication_year` .*converted back to INTEGER", msgs)))
  expect_true(any(grepl("`locations_count` has different types", msgs)))

  con <- openalexAPI:::.api_open(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  types <- DBI::dbGetQuery(con, "DESCRIBE works")
  type_of <- function(x) types$column_type[types$column_name == x]
  expect_equal(type_of("cited_by_count"), "BIGINT")
  expect_equal(type_of("publication_year"), "INTEGER")
  expect_equal(type_of("locations_count"), "VARCHAR") # not a filter column: common type

  ref <- local_fixture_con()
  expect_equal(n_oqo(con, list(leaf("cited_by_count", 10, ">"))),
               n_sql(ref, "cited_by_count > 10"))
  expect_equal(n_oqo(con, list(leaf("publication_year", 2005, ">="))),
               n_sql(ref, "publication_year >= 2005"))
  expect_equal(n_sql(con, "cited_by_count IS NULL"), 1)
  # served as numbers again
  ctx <- list(con = con, columns = types$column_name,
              served = openalexAPI:::.api_served_columns(con))
  r <- jsonlite::fromJSON(openalexAPI:::.api_execute(ctx, list(get_rows = "works"),
    list(per_page = 1, select = "cited_by_count,publication_year")), simplifyVector = FALSE)
  expect_type(r$results[[1]]$cited_by_count, "integer")
  expect_type(r$results[[1]]$publication_year, "integer")
})

test_that("structs that differ only in field order are not reported", {
  # corpus_jq's two files have `ids` with the fields in different order
  expect_no_message(prepare_corpus(fixture("corpus_jq"), cache_dir = withr::local_tempdir()))
})

test_that("a snapshot corpus (inverted index as a JSON string) is read as a MAP", {
  dir <- withr::local_tempdir()
  ref <- local_fixture_con()
  first <- DBI::dbGetQuery(ref, "SELECT min(id) AS id FROM works")$id
  # the snapshot shape, written from the prepared fixture: no `abstract`, the
  # index as JSON text; one record repeats a key, as some snapshot records do
  keep <- setdiff(DBI::dbGetQuery(ref, "DESCRIBE works")$column_name,
                  c("abstract", "abstract_inverted_index"))
  keep <- DBI::dbQuoteIdentifier(ref, keep[!startsWith(keep, "oaapi_")])
  DBI::dbExecute(ref, sprintf(
    "COPY (SELECT %s, CASE WHEN id = '%s' THEN '{\"Coral\":[0],\"reefs\":[1,3],\"Coral\":[2]}'
             ELSE CAST(to_json(abstract_inverted_index) AS VARCHAR) END AS abstract_inverted_index
           FROM works) TO '%s/works.parquet' (FORMAT parquet)",
    paste(keep, collapse = ", "), first, dir))

  msgs <- testthat::capture_messages(db <- prepare_corpus(dir, cache_dir = withr::local_tempdir()))
  expect_true(any(grepl("`abstract_inverted_index` .*converted back to MAP", msgs)))
  expect_false(any(grepl("could not be read", msgs)))
  con <- openalexAPI:::.api_open(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  types <- DBI::dbGetQuery(con, "DESCRIBE works")
  expect_equal(types$column_type[types$column_name == "abstract_inverted_index"],
               "MAP(VARCHAR, BIGINT[])")
  # every abstract survives; the repeated key has its positions merged
  same <- "SELECT id, oaapi_abstract FROM works WHERE id <> ? ORDER BY id"
  expect_equal(DBI::dbGetQuery(con, same, params = list(first)),
               DBI::dbGetQuery(ref, same, params = list(first)))
  expect_equal(DBI::dbGetQuery(con, "SELECT oaapi_abstract FROM works WHERE id = ?",
                               params = list(first))$oaapi_abstract, "Coral reefs Coral reefs")
  # and is searchable
  expect_equal(n_oqo(con, list(leaf("ids.openalex", first),
                               list(column_id = "abstract.search", value = "reefs", operator = "has"))), 1)
})
