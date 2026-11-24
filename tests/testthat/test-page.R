# The web page's URL logic (inst/www/query.js), run with V8, and the URLs it
# builds checked against the server

page_js <- function() {
  skip_if_not_installed("V8")
  ctx <- V8::v8()
  ctx$source(system.file("www", "query.js", package = "openalexAPI"))
  ctx
}
build <- function(ctx, form, page = NULL) {
  ctx$call("OAQ.buildUrl", form, if (is.null(page)) V8::JS("undefined") else page)
}
query_of <- function(url) httr2::url_parse(paste0("http://x", url))$query

test_that("page URLs carry view parameters, copied URLs only the query", {
  ctx <- page_js()
  f <- list(mode = "classic", filter = " type:article , publication_year:>2000 ",
            search = "coral bleaching", exact = FALSE, groupBy = "", sort = "", perPage = 50)
  u <- build(ctx, f, 2)
  q <- query_of(u)
  expect_match(u, "^/works\\?")
  expect_equal(q$filter, "type:article,publication_year:>2000")
  expect_equal(q$search, "coral bleaching")
  expect_equal(q$per_page, "50")
  expect_equal(q$page, "2")
  expect_match(q$select, "relevance_score")

  copy <- query_of(build(ctx, f))
  expect_equal(names(copy), c("filter", "search"))

  f$exact <- TRUE
  expect_equal(names(query_of(build(ctx, f))), c("filter", "search.exact"))
  f$groupBy <- "type"
  g <- query_of(build(ctx, f, 1))
  expect_equal(g$group_by, "type")
  expect_null(g$select)

  o <- build(ctx, list(mode = "oql", oql = " works where year >= 2000 ", sort = "cited_by_count:desc"))
  expect_equal(query_of(o), list(oql = "works where year >= 2000", sort = "cited_by_count:desc"))
  expect_error(build(ctx, list(mode = "oql", oql = "  ")), "OQL")
})

test_that("group keys become filter values; grouped responses are recognised", {
  ctx <- page_js()
  expect_equal(ctx$call("OAQ.groupFilterValue", "https://openalex.org/types/article"), "article")
  expect_equal(ctx$call("OAQ.groupFilterValue", "https://openalex.org/countries/CH"), "CH")
  expect_equal(ctx$call("OAQ.groupFilterValue", "https://openalex.org/T10341"),
               "https://openalex.org/T10341")
  expect_true(ctx$call("OAQ.isGrouped", list(meta = list(groups_count = 0L), group_by = list())))
  expect_false(ctx$call("OAQ.isGrouped", V8::JS('({meta: {groups_count: null}, results: []})')))
  expect_equal(ctx$call("OAQ.parseBody", "<html>bad gateway</html>", 502L)$message,
               "HTTP 502: <html>bad gateway</html>")
  expect_equal(ctx$call("OAQ.parseBody", '{"a":1}', 200L), list(a = 1L))
})

test_that("the URLs the page builds are accepted by the server", {
  skip_on_cran()
  skip_if_offline("api.openalex.org") # classic URLs and OQL are translated
  ctx <- page_js()
  url <- local_server()
  forms <- list(
    list(mode = "classic", filter = "type:article", search = "forest", exact = FALSE,
         groupBy = "", sort = "", perPage = 25),
    list(mode = "classic", filter = "", search = "defor*", exact = TRUE,
         groupBy = "authorships.countries", sort = "key:asc", perPage = 25),
    list(mode = "oql", oql = "works where type is article and year >= 2005",
         sort = "publication_year:desc", perPage = 25)
  )
  for (f in forms) {
    page <- call_api(url, build(ctx, f, 1))
    expect_equal(page$status, 200, label = build(ctx, f, 1))
    copy <- call_api(url, build(ctx, f))
    expect_equal(copy$status, 200, label = build(ctx, f))
    expect_equal(copy$json$meta$count, page$json$meta$count)
    # a copied records URL returns full records, not the page's columns
    if (length(copy$json$results)) {
      expect_true("abstract_inverted_index" %in% names(copy$json$results[[1]]))
    }
  }
})
