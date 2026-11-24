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

test_that("keys sort as numbers where numeric, the unknown group last", {
  con <- local_fixture_con()
  for (dir in c("asc", "desc")) {
    keys <- names(counts(grouped(con, "cited_by_count",
                                 sort_by = list(list(column_id = "key", direction = dir)))))
    want <- sort(as.numeric(keys), decreasing = dir == "desc")
    expect_equal(as.numeric(keys), want)
    k <- names(counts(grouped(con, "primary_location.source.id:include_unknown",
                              sort_by = list(list(column_id = "key", direction = dir)))))
    expect_equal(k[[length(k)]], "unknown")
  }
})

# A copy of corpus_jq with mixed ID forms, a missing source name and a real
# oa_status 'unknown'
local_odd_corpus <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  con0 <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con0, shutdown = TRUE))
  DBI::dbExecute(con0, sprintf("
    COPY (SELECT * REPLACE (
      CASE WHEN rn %% 2 = 0
           THEN list_transform(referenced_works, lambda x: regexp_replace(x, '^.*/', ''))
           ELSE referenced_works END AS referenced_works,
      struct_update(primary_location, source := struct_update(primary_location.source,
        display_name := CASE WHEN rn %% 3 = 0 THEN NULL
                             ELSE primary_location.source.display_name END)) AS primary_location,
      struct_update(open_access, oa_status := CASE WHEN rn %% 5 = 0 THEN 'unknown'
        WHEN rn %% 5 = 1 THEN NULL ELSE open_access.oa_status END) AS open_access)
    FROM (SELECT *, row_number() OVER () AS rn
          FROM read_parquet('%s/**/*.parquet', union_by_name = true)))
    TO '%s/w.parquet' (FORMAT parquet)", fixture("corpus_jq"), dir))
  con <- openalexAPI:::.api_open(prepare_corpus(dir, cache_dir = withr::local_tempdir(.local_envir = env)))
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  con
}

test_that("an ID stored in several forms is one group", {
  con <- local_odd_corpus()
  x <- numeric(0)
  cursor <- "*"
  while (!is.null(cursor)) {
    page <- grouped(con, "referenced_works", view = list(cursor = cursor))
    x <- c(x, counts(page))
    cursor <- page$meta$next_cursor
  }
  want <- DBI::dbGetQuery(con, "SELECT count(DISTINCT r) AS n FROM
    (SELECT unnest(oaapi_refs) AS r FROM works)")$n
  expect_length(x, want)
  expect_false(anyDuplicated(toupper(sub("^.*/", "", names(x)))) > 0)
  top <- DBI::dbGetQuery(con, "SELECT r, count(DISTINCT oaapi_id) AS n FROM
    (SELECT oaapi_id, unnest(oaapi_refs) AS r FROM works) GROUP BY r ORDER BY n DESC, r LIMIT 1")
  expect_equal(unname(x[[1]]), top$n)
  expect_equal(toupper(sub("^.*/", "", names(x)[[1]])), top$r)
})

test_that("a missing display name is null, a real `unknown` value merges", {
  con <- local_odd_corpus()
  g <- grouped(con, "primary_location.source.id")$group_by
  expect_true(all(vapply(g, function(x) "key_display_name" %in% names(x), TRUE)))
  expect_true(any(vapply(g, function(x) is.null(x$key_display_name), TRUE)))

  x <- counts(grouped(con, "open_access.oa_status:include_unknown"))
  expect_equal(sum(names(x) == "unknown"), 1)
  expect_equal(unname(x[["unknown"]]),
               n_sql(con, "open_access.oa_status = 'unknown' OR open_access.oa_status IS NULL"))
})

test_that("slug keys are URL-encoded; true/false columns list both values", {
  con <- local_fixture_con()
  keys <- names(counts(grouped(con, "primary_location.source.type")))
  expect_false(any(grepl(" ", keys)))
  if (any(grepl("ebook", keys))) expect_true("https://openalex.org/source-types/ebook%20platform" %in% keys)
  x <- counts(grouped(con, "is_retracted"))
  expect_setequal(names(x), c("true", "false"))
  expect_equal(unname(x[["true"]]), n_sql(con, "is_retracted"))
  expect_equal(unname(x[["false"]]), n_sql(con, "NOT is_retracted"))
})

test_that("the added group_by columns", {
  con <- local_fixture_con()
  lin <- counts(grouped(con, "authorships.institutions.lineage"))
  want <- DBI::dbGetQuery(con, "SELECT l, count(DISTINCT oaapi_id) AS n FROM (SELECT oaapi_id,
    unnest(flatten(list_transform(authorships, lambda a: flatten(list_transform(a.institutions,
      lambda i: i.lineage))))) AS l FROM works) GROUP BY l ORDER BY n DESC, l LIMIT 5")
  expect_equal(unname(lin[want$l]), as.numeric(want$n))
  sf <- grouped(con, "primary_topic.subfield.id")$group_by[[1]]
  expect_match(sf$key, "^https://openalex.org/subfields/")
  expect_false(is.null(sf$key_display_name))
  x <- counts(grouped(con, "citation_normalized_percentile.is_in_top_1_percent"))
  expect_setequal(names(x), c("true", "false"))
  issn <- counts(grouped(con, "primary_location.source.issn"))
  expect_false(any(grepl("^https://", names(issn))))
})

test_that("publisher groups carry the publishers' names", {
  con <- local_fixture_con()
  g <- grouped(con, "primary_location.source.publisher_lineage")$group_by
  expect_gt(length(g), 0)
  named <- vapply(g, function(x) !identical(x$key_display_name, x$key), TRUE)
  expect_true(any(named))
  expect_equal(grouped(con, "primary_location.source.host_organization_lineage")$group_by, g)
})
