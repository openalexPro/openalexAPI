# The server in a background process. OQO requests run offline; classic URLs
# and OQL need OpenAlex's /query endpoint (free) and skip when offline.

oqo_body <- function(filter_rows, ...) {
  list(oqo = list(get_rows = "works", filter_rows = filter_rows), ...)
}

test_that("OQO queries run offline with OpenAlex-shaped responses", {
  url <- local_server()
  con <- local_fixture_con()
  r <- call_api(url, "/", oqo_body(list(leaf("type", "book")), per_page = 2, select = "id,type"))
  expect_equal(r$status, 200)
  expect_equal(r$json$meta$count, n_sql(con, "type = 'book'"))
  expect_equal(r$json$meta$per_page, 2)
  expect_length(r$json$results, 2)
  expect_equal(names(r$json$results[[1]]), c("id", "type"))
  expect_equal(r$json$group_by, list())
  expect_null(r$json$meta$groups_count)
  expect_equal(r$json$meta$x_query$oqo$filter_rows[[1]]$value, "book")

  # GET /?oqo= is the same query
  q <- utils::URLencode(as.character(jsonlite::toJSON(oqo_body(list(leaf("type", "book")))$oqo,
                                                      auto_unbox = TRUE)), reserved = TRUE)
  expect_equal(call_api(url, paste0("/?oqo=", q))$json$meta$count, r$json$meta$count)
})

test_that("cursor paging returns every work once; page paging matches", {
  url <- local_server()
  ids <- character(0)
  cursor <- "*"
  while (!is.null(cursor)) {
    r <- call_api(url, "/", oqo_body(list(leaf("type", "article")), per_page = 50,
                                     cursor = cursor, select = "id"))
    ids <- c(ids, vapply(r$json$results, function(x) x$id, ""))
    cursor <- r$json$meta$next_cursor
  }
  expect_length(ids, r$json$meta$count)
  expect_false(anyDuplicated(ids) > 0)
  p2 <- call_api(url, "/", oqo_body(list(leaf("type", "article")), per_page = 50, page = 2,
                                    select = "id"))
  expect_equal(vapply(p2$json$results, function(x) x$id, ""), ids[51:100])
  expect_equal(call_api(url, "/", oqo_body(list(), per_page = 201))$status, 400)
})

test_that("sort, sample with seed, and single works", {
  url <- local_server()
  r <- call_api(url, "/", list(oqo = list(get_rows = "works"), sort = "publication_year:asc",
                               per_page = 5, select = "publication_year"))
  years <- vapply(r$json$results, function(x) x$publication_year, 1)
  expect_false(is.unsorted(years))

  s <- function() call_api(url, "/", list(oqo = list(get_rows = "works", sample = 7, seed = "42"),
                                          per_page = 50, select = "id"))
  a <- s()
  expect_equal(a$json$meta$count, 7)
  expect_equal(a$json$results, s()$json$results)

  id <- a$json$results[[1]]$id
  one <- call_api(url, paste0("/works/", sub("^.*/", "", id)))
  expect_equal(one$status, 200)
  expect_equal(one$json$id, id)
  expect_null(one$json$meta)
  expect_equal(call_api(url, paste0("/works/", utils::URLencode(id, reserved = TRUE)))$json$id, id)
  expect_equal(call_api(url, "/works/W1")$status, 404)
})

test_that("errors have an OpenAlex shape and status", {
  url <- local_server()
  e <- call_api(url, "/", oqo_body(list(leaf("no_such", "x"))))
  expect_equal(e$status, 400)
  expect_match(e$json$message, "not supported")
  expect_equal(call_api(url, "/", list(per_page = 1))$status, 400)
  expect_equal(call_api(url, "/", oqo_body(list(), select = "nope"))$status, 400)
  info <- call_api(url, "/corpus")$json
  expect_equal(info$works, 369)
  expect_true("type" %in% unlist(info$filters))
})

test_that("classic URLs and OQL are translated by OpenAlex", {
  skip_on_cran()
  skip_if_offline("api.openalex.org")
  url <- local_server()
  con <- local_fixture_con()
  n <- n_sql(con, "type = 'article' AND publication_year = 1999")
  r <- call_api(url, "/works?filter=type:article,publication_year:1999&select=id")
  expect_equal(r$json$meta$count, n)
  expect_match(r$json$meta$x_query$oql, "1999")
  expect_equal(call_api(url, "/?oql=works%20where%20type%20is%20article")$json$meta$count,
               n_sql(con, "type = 'article'"))
  # OpenAlex's validation error is passed through unchanged
  bad <- call_api(url, "/works?filter=no_such:1")
  expect_equal(bad$status, 400)
  expect_false(bad$json$validation$valid)
  # POST /query is forwarded
  tr <- call_api(url, "/query", list(oxurl = "/works?filter=type:book"))
  expect_equal(tr$json$oqo$filter_rows[[1]]$column_id, "type")
})

test_that("openalexPro downloads from the server like from OpenAlex", {
  skip_on_cran()
  skip_if_offline("api.openalex.org")
  url <- local_server()
  con <- local_fixture_con()
  want <- sort(DBI::dbGetQuery(con, "SELECT id FROM works
    WHERE type = 'article' AND publication_year BETWEEN 1990 AND 2000")$id)
  q <- openalexPro::pro_query(entity = "works", type = "article",
                              publication_year = "1990-2000", endpoint = url)
  for (via in c("url", "oqo")) {
    out <- withr::local_tempdir()
    openalexPro::pro_fetch(q, project_folder = out, via = via, progress = FALSE, verbose = FALSE)
    got <- DBI::dbGetQuery(con, sprintf(
      "SELECT id, abstract FROM read_parquet('%s/**/*.parquet')", out))
    expect_equal(sort(got$id), want, label = via)
    # openalexPro rebuilt the abstracts from the served inverted index
    expect_equal(sum(!is.na(got$abstract)), n_sql(con, sprintf(
      "oaapi_abstract IS NOT NULL AND list_contains(['%s'], id)",
      paste(want, collapse = "','"))), label = via)
  }
})

test_that("a single work carries the inverted index, by ID or DOI, with select", {
  url <- local_server()
  con <- local_fixture_con()
  w <- DBI::dbGetQuery(con, "SELECT id, doi FROM works
    WHERE oaapi_abstract IS NOT NULL AND doi IS NOT NULL ORDER BY id LIMIT 1")
  one <- call_api(url, paste0("/works/", sub("^.*/", "", w$id)))$json
  expect_gt(length(one$abstract_inverted_index), 0)
  expect_false(any(c("abstract", "citation", "page") %in% names(one)))
  doi <- sub("^https://doi.org/", "", w$doi)
  for (form in c(w$doi, paste0("doi:", doi), doi)) {
    r <- call_api(url, paste0("/works/", form, "?select=id,doi"))
    expect_equal(r$json, list(id = w$id, doi = w$doi), label = form)
  }
})

test_that("a sample spanning pages is the same on every page, seeded or not", {
  url <- local_server()
  ids <- function(oqo) {
    out <- character(0)
    cursor <- "*"
    while (!is.null(cursor)) {
      r <- call_api(url, "/", list(oqo = oqo, per_page = 25, cursor = cursor, select = "id"))
      out <- c(out, vapply(r$json$results, function(x) x$id, ""))
      cursor <- r$json$meta$next_cursor
    }
    out
  }
  for (oqo in list(list(get_rows = "works", sample = 60),
                   list(get_rows = "works", sample = 60, seed = "7"))) {
    a <- ids(oqo)
    expect_length(a, 60)
    expect_false(anyDuplicated(a) > 0)
    expect_equal(ids(oqo), a)
  }
})

test_that("`+` in a classic query means a space", {
  skip_on_cran()
  skip_if_offline("api.openalex.org")
  url <- local_server()
  con <- local_fixture_con()
  expect_equal(call_api(url, "/?oql=works+where+type+is+article")$json$meta$count,
               n_sql(con, "type = 'article'"))
})
