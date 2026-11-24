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
