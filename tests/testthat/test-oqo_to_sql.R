test_that("leaves of each kind match hand-written SQL", {
  con <- local_fixture_con()
  expect_equal(n_oqo(con, list(leaf("type", "article"))), n_sql(con, "type = 'article'"))
  expect_equal(n_oqo(con, list(leaf("type", "ARTICLE"))), n_sql(con, "type = 'article'"))
  expect_equal(n_oqo(con, list(leaf("publication_year", 2000, ">="))),
               n_sql(con, "publication_year >= 2000"))
  expect_equal(n_oqo(con, list(leaf("from_publication_date", "2000-01-01"))),
               n_sql(con, "publication_date >= DATE '2000-01-01'"))
  expect_equal(n_oqo(con, list(leaf("open_access.is_oa", TRUE))),
               n_sql(con, "open_access.is_oa"))
  expect_equal(n_oqo(con, list(leaf("has_doi", FALSE))), n_sql(con, "doi IS NULL"))
  expect_equal(n_oqo(con, list(leaf("authors_count", 5, "<"))),
               n_sql(con, "len(authorships) < 5"))
  expect_equal(n_oqo(con, list(leaf("authorships.countries", "ch"))),
               n_sql(con, "list_contains(flatten(list_transform(authorships, lambda a: a.countries)), 'CH')"))
  expect_gt(n_oqo(con, list(leaf("authorships.countries", "ch"))), 0)
  expect_equal(n_oqo(con, list(leaf("sustainable_development_goals.id", "13"))),
               n_sql(con, "list_contains(list_transform(sustainable_development_goals, lambda s: s.id), 'https://metadata.un.org/sdg/13')"))
})

test_that("OR branches, AND rows and negation, with missing values", {
  con <- local_fixture_con()
  total <- n_sql(con, "true")
  expect_equal(n_oqo(con, list(list(join = "or", filters = list(leaf("type", "article"),
                                                                leaf("type", "book"))))),
               n_sql(con, "type IN ('article', 'book')"))
  expect_equal(n_oqo(con, list(leaf("type", "article"), leaf("publication_year", 1999))),
               n_sql(con, "type = 'article' AND publication_year = 1999"))
  # a negated leaf also matches works where the value is missing
  expect_gt(n_sql(con, "primary_location.source.type IS NULL"), 0)
  expect_equal(n_oqo(con, list(leaf("primary_location.source.type", "journal",
                                    is_negated = TRUE))),
               total - n_sql(con, "primary_location.source.type = 'journal'"))
  # a negated branch
  expect_equal(n_oqo(con, list(list(join = "or", is_negated = TRUE, filters = list(
    leaf("type", "article"), leaf("language", "en"))))),
    total - n_sql(con, "type = 'article' OR language = 'en'"))
})

test_that("IDs match in any form, on both sides", {
  dir <- withr::local_tempdir()
  # half of the works with bare IDs, some lower-case
  con0 <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con0, sprintf(
    "COPY (SELECT * REPLACE (CASE WHEN row_number() OVER () %% 2 = 0
       THEN lower(regexp_replace(id, '^.*/', '')) ELSE id END AS id)
     FROM read_parquet('%s/**/*.parquet', union_by_name = true))
     TO '%s/w.parquet' (FORMAT parquet)", fixture("corpus_jq"), dir))
  DBI::dbDisconnect(con0, shutdown = TRUE)
  con <- openalexAPI:::.api_open(prepare_corpus(dir, cache_dir = withr::local_tempdir()))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  ids <- DBI::dbGetQuery(con, "(SELECT id FROM works WHERE id LIKE 'http%' LIMIT 2)
                             UNION ALL (SELECT id FROM works WHERE id NOT LIKE 'http%' LIMIT 2)")$id
  expect_true(any(!grepl("/", ids)) && any(grepl("/", ids)))
  for (id in ids) {
    key <- toupper(sub("^.*/", "", id))
    for (form in c(key, tolower(key), paste0("https://openalex.org/", key))) {
      expect_equal(n_oqo(con, list(leaf("ids.openalex", form))), 1)
    }
  }
})

test_that("cites and cited_by work within the corpus", {
  con <- local_fixture_con()
  w <- DBI::dbGetQuery(con, "SELECT oaapi_id, oaapi_refs FROM works
                             WHERE len(oaapi_refs) > 0 LIMIT 1")
  ref <- w$oaapi_refs[[1]][[1]]
  expect_equal(n_oqo(con, list(leaf("referenced_works", ref))),
               n_sql(con, sprintf("list_contains(oaapi_refs, '%s')", ref)))
  expect_equal(n_oqo(con, list(leaf("cited_by", w$oaapi_id))),
               n_sql(con, sprintf("list_contains(['%s'], oaapi_id)",
                                  paste(w$oaapi_refs[[1]], collapse = "','"))))
})

test_that("unsupported input stops with a clear 400", {
  con <- local_fixture_con()
  err <- function(rows, ...) {
    tryCatch(n_oqo(con, rows, ...), oaapi_error = function(e) e)
  }
  e <- err(list(leaf("no_such", "x")))
  expect_s3_class(e, "oaapi_error")
  expect_equal(e$status, 400L)
  expect_match(conditionMessage(e), "not supported")
  expect_match(conditionMessage(err(list(leaf("fulltext.search", "x", "has")))), "Search")
  expect_match(conditionMessage(err(list(leaf("type", "x", ">")))), "Operator")
  expect_match(conditionMessage(err(list(leaf("publication_year", "abc")))), "number")
  expect_match(conditionMessage(err(list(leaf("from_publication_date", "2020-13-45")))), "date")
  expect_match(conditionMessage(err(list(leaf("type", "x")), columns = "id")), "not available")
  # values are quoted, never pasted into the SQL
  expect_equal(n_oqo(con, list(leaf("type", "x' OR 1=1 --"))), 0)
  expect_error(oqo_to_sql(list(get_rows = "authors"), con), "Only works")
  expect_equal(oqo_to_sql(list(get_rows = "works", group_by = list(list(column_id = "type"))),
                          con)$group$column, "type")
})

test_that("sort_by becomes ORDER BY terms", {
  con <- local_fixture_con()
  s <- oqo_to_sql(list(get_rows = "works", sort_by = list(
    list(column_id = "cited_by_count", direction = "desc"),
    list(column_id = "publication_year"))), con)
  expect_equal(s$order, c("cited_by_count DESC NULLS LAST", "publication_year ASC NULLS LAST"))
  expect_error(oqo_to_sql(list(sort_by = list(list(column_id = "authorships.countries"))), con),
               "Cannot sort")
})

test_that("a null value matches works where the field is missing", {
  con <- local_fixture_con()
  total <- n_sql(con, "true")
  missing <- list(list(column_id = "primary_location.source.id", value = NULL))
  n <- n_sql(con, "primary_location.source.id IS NULL")
  expect_gt(n, 0)
  expect_equal(n_oqo(con, missing), n)
  missing[[1]]$is_negated <- TRUE
  expect_equal(n_oqo(con, missing), total - n)
  expect_equal(n_oqo(con, list(list(column_id = "has_references", value = NULL))) |>
                 tryCatch(oaapi_error = function(e) "refused"), "refused")
  expect_equal(n_oqo(con, list(list(column_id = "referenced_works", value = NULL))),
               n_sql(con, "coalesce(len(oaapi_refs), 0) = 0"))
})

test_that("malformed numbers are a 400, not invalid SQL", {
  con <- local_fixture_con()
  for (v in c("1.2.3", ".", "1e", "")) {
    e <- tryCatch(n_oqo(con, list(leaf("cited_by_count", v, ">"))), oaapi_error = function(e) e)
    expect_s3_class(e, "oaapi_error")
    expect_match(conditionMessage(e), "number")
  }
  expect_equal(n_oqo(con, list(leaf("cited_by_count", "10", ">"))), n_sql(con, "cited_by_count > 10"))
})

test_that("a struct field the corpus lacks is a 400", {
  dir <- withr::local_tempdir()
  con0 <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con0, sprintf(
    "COPY (SELECT * REPLACE ({'is_oa': open_access.is_oa} AS open_access)
     FROM read_parquet('%s/**/*.parquet', union_by_name = true))
     TO '%s/w.parquet' (FORMAT parquet)", fixture("corpus_jq"), dir))
  DBI::dbDisconnect(con0, shutdown = TRUE)
  con <- openalexAPI:::.api_open(prepare_corpus(dir, cache_dir = withr::local_tempdir()))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  ctx <- list(con = con, columns = DBI::dbGetQuery(con, "DESCRIBE works")$column_name,
              served = openalexAPI:::.api_served_columns(con))
  e <- tryCatch(openalexAPI:::.api_execute(ctx, list(get_rows = "works", filter_rows = list(
    leaf("open_access.oa_status", "gold")))), oaapi_error = function(e) e)
  expect_s3_class(e, "oaapi_error")
  expect_equal(e$status, 400L)
  expect_match(conditionMessage(e), "not available")
})
