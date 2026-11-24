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

test_that("duplicate works across partitions are served once", {
  dir <- withr::local_tempdir()
  for (part in c("query=a", "query=b")) {
    dir.create(file.path(dir, part))
    file.copy(list.files(fixture("corpus_api"), full.names = TRUE), file.path(dir, part))
  }
  con <- openalexAPI:::.api_open(prepare_corpus(dir, cache_dir = withr::local_tempdir()))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
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
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
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
