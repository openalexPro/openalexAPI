# group_by: works counted once per key, OpenAlex's key formats and response shape

grouped <- function(con, column, view = list(), sort_by = NULL, filter_rows = NULL) {
  ctx <- list(con = con, columns = DBI::dbGetQuery(con, "DESCRIBE works")$column_name,
              served = openalexAPI:::.api_served_columns(con))
  oqo <- Filter(Negate(is.null), list(get_rows = "works", filter_rows = filter_rows,
    group_by = list(list(column_id = column)), sort_by = sort_by))
  jsonlite::fromJSON(openalexAPI:::.api_execute(ctx, oqo, view), simplifyVector = FALSE)
}
counts <- function(x) {
  stats::setNames(vapply(x$group_by, function(g) g$count, 1),
                  vapply(x$group_by, function(g) g$key, ""))
}

test_that("a scalar column: one group per value, keys as OpenAlex writes them", {
  con <- local_fixture_con()
  x <- grouped(con, "type")
  want <- DBI::dbGetQuery(con, "SELECT type, count(*) AS n FROM works GROUP BY type")
  expect_equal(counts(x)[paste0("https://openalex.org/types/", want$type)],
               stats::setNames(as.numeric(want$n), paste0("https://openalex.org/types/", want$type)))
  expect_equal(x$meta$count, n_sql(con, "true"))
  expect_equal(x$meta$groups_count, nrow(want))
  expect_null(x$results)
  expect_false(is.unsorted(rev(unname(counts(x))))) # count, descending
  expect_equal(x$group_by[[1]]$key_display_name, sub(".*/", "", x$group_by[[1]]$key))
})

test_that("a list column counts each work once per key", {
  con <- local_fixture_con()
  x <- counts(grouped(con, "authorships.countries"))
  want <- DBI::dbGetQuery(con, "
    SELECT c, count(DISTINCT oaapi_id) AS n FROM (
      SELECT oaapi_id, unnest(flatten(list_transform(authorships, lambda a: a.countries))) AS c
      FROM works) GROUP BY c")
  expect_equal(x[paste0("https://openalex.org/countries/", want$c)],
               stats::setNames(as.numeric(want$n), paste0("https://openalex.org/countries/", want$c)))
})

test_that("filters apply before grouping; booleans and entities keep their format", {
  con <- local_fixture_con()
  x <- counts(grouped(con, "open_access.is_oa",
                      filter_rows = list(leaf("type", "article"))))
  expect_equal(unname(x[["true"]]), n_sql(con, "type = 'article' AND open_access.is_oa"))
  t <- grouped(con, "primary_topic.id", view = list(per_page = 1))$group_by[[1]]
  name <- DBI::dbGetQuery(con, sprintf(
    "SELECT any_value(primary_topic.display_name) AS d FROM works WHERE primary_topic.id = '%s'",
    t$key))$d
  expect_equal(t$key_display_name, name)
})

test_that("the unknown group appears only with include_unknown", {
  con <- local_fixture_con()
  plain <- counts(grouped(con, "primary_location.source.id"))
  expect_false("unknown" %in% names(plain))
  x <- counts(grouped(con, "primary_location.source.id:include_unknown"))
  expect_equal(unname(x[["unknown"]]), n_sql(con, "primary_location.source.id IS NULL"))
  y <- counts(grouped(con, "primary_location.source.id", view = list(include_unknown = TRUE)))
  expect_equal(y, x)
})

test_that("groups are paged and sorted by key or count", {
  con <- local_fixture_con()
  all <- counts(grouped(con, "authorships.countries"))
  got <- numeric(0)
  cursor <- "*"
  while (!is.null(cursor)) {
    x <- grouped(con, "authorships.countries", view = list(per_page = 7, cursor = cursor))
    expect_lte(x$meta$groups_count, 7)
    got <- c(got, counts(x))
    cursor <- x$meta$next_cursor
  }
  expect_equal(got, all)
  keys <- names(counts(grouped(con, "type", sort_by = list(list(column_id = "key",
                                                               direction = "asc")))))
  expect_equal(keys, sort(keys))
})

test_that("what cannot be grouped stops with a 400", {
  con <- local_fixture_con()
  err <- function(expr) tryCatch(expr, oaapi_error = function(e) conditionMessage(e))
  expect_match(err(grouped(con, "fwci")), "Cannot group by `fwci`")
  expect_match(err(grouped(con, "doi")), "Cannot group by `doi`")
  expect_match(err(oqo_to_sql(list(group_by = list(list(column_id = "type"),
                                                   list(column_id = "language"))), con)),
               "one `group_by`")
  expect_match(err(grouped(con, "type", sort_by = list(list(column_id = "publication_year")))),
               "Cannot sort groups")
  ctx <- list(con = con, columns = NULL, served = "id")
  expect_match(err(openalexAPI:::.api_execute(ctx, list(get_rows = "works", sample = 5,
    group_by = list(list(column_id = "type"))))), "sample")
})
