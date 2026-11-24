# Full-text search over title and abstract

has <- function(column, value, is_negated = FALSE) {
  list(column_id = column, value = value, operator = "has", is_negated = is_negated)
}
# works whose title + abstract match a case-insensitive regex
n_re <- function(con, re) {
  n_sql(con, paste0("regexp_matches(concat_ws(' ', oaapi_title, oaapi_abstract), ",
                    DBI::dbQuoteLiteral(con, paste0("(?i)", re)), ")"))
}

test_that("exact search: words (AND), phrases in order, wildcards", {
  con <- local_fixture_con()
  ex <- "fulltext.search.exact"
  expect_gt(n_oqo(con, list(has(ex, "deforestation"))), 0)
  expect_equal(n_oqo(con, list(has(ex, "deforestation"))), n_re(con, "\\bdeforestation\\b"))
  expect_equal(n_oqo(con, list(has(ex, "tropical deforestation"))),
               n_sql(con, paste(
                 "regexp_matches(concat_ws(' ', oaapi_title, oaapi_abstract), '(?i)\\btropical\\b')",
                 "AND regexp_matches(concat_ws(' ', oaapi_title, oaapi_abstract), '(?i)\\bdeforestation\\b')")))
  expect_equal(n_oqo(con, list(has(ex, '"tropical deforestation"'))),
               n_re(con, "\\btropical\\W+deforestation\\b"))
  expect_gt(n_oqo(con, list(has(ex, '"tropical deforestation"'))), 0)
  expect_equal(n_oqo(con, list(has(ex, '"deforestation tropical"'))),
               n_re(con, "\\bdeforestation\\W+tropical\\b"))
  expect_equal(n_oqo(con, list(has(ex, "defor*"))), n_re(con, "\\bdefor\\w*\\b"))
})

test_that("stemmed search folds word forms; fields narrow the search", {
  con <- local_fixture_con()
  st <- function(v, col = "fulltext.search") n_oqo(con, list(has(col, v)))
  expect_equal(st("forests"), st("forest"))
  expect_gt(st("forest"), n_oqo(con, list(has("fulltext.search.exact", "forest"))))
  expect_equal(st("forest", "title_and_abstract.search"), st("forest"))
  expect_lt(st("forest", "display_name.search"), st("forest"))
  expect_lte(st("forest", "abstract.search"), st("forest"))
  # a word only in a title is found by the title search, not the abstract one
  t <- DBI::dbGetQuery(con, "SELECT oaapi_title FROM works WHERE oaapi_title ILIKE '%democracy%' LIMIT 1")
  expect_gt(st("democracy", "display_name.search"), 0)
})

test_that("negation and OR combine searches", {
  con <- local_fixture_con()
  f <- has("fulltext.search", "forest")
  t <- has("fulltext.search", "tropical")
  both <- n_oqo(con, list(f, t))
  expect_equal(n_oqo(con, list(f, has("fulltext.search", "tropical", TRUE))),
               n_oqo(con, list(f)) - both)
  expect_equal(n_oqo(con, list(list(join = "or", filters = list(f, t)))),
               n_oqo(con, list(f)) + n_oqo(con, list(t)) - both)
})

test_that("relevance: records carry relevance_score, most relevant first", {
  con <- local_fixture_con()
  ctx <- list(con = con, columns = DBI::dbGetQuery(con, "DESCRIBE works")$column_name,
              served = openalexAPI:::.api_served_columns(con))
  run <- function(oqo, view = list(per_page = 20)) {
    jsonlite::fromJSON(openalexAPI:::.api_execute(ctx, oqo, view), simplifyVector = FALSE)
  }
  r <- run(list(get_rows = "works", filter_rows = list(has("fulltext.search", "tropical forest"))))
  s <- vapply(r$results, function(x) x$relevance_score, 1)
  expect_gt(length(s), 1)
  expect_false(is.unsorted(rev(s)))
  expect_true(all(s > 0))
  asc <- run(list(get_rows = "works", filter_rows = list(has("fulltext.search", "forest")),
                  sort_by = list(list(column_id = "relevance_score", direction = "asc"))))
  s <- vapply(asc$results, function(x) x$relevance_score, 1)
  expect_false(is.unsorted(s))
  # without a search there is no relevance
  plain <- run(list(get_rows = "works"), list(per_page = 1))
  expect_null(plain$results[[1]]$relevance_score)
  expect_error(oqo_to_sql(list(sort_by = list(list(column_id = "relevance_score"))), con),
               "needs a search")
  # search and group_by combine
  g <- run(list(get_rows = "works", filter_rows = list(has("fulltext.search", "forest")),
                group_by = list(list(column_id = "type"))))
  expect_equal(sum(vapply(g$group_by, function(x) x$count, 1)),
               n_oqo(con, list(has("fulltext.search", "forest"))))
})

test_that("unsupported search input is a 400", {
  con <- local_fixture_con()
  err <- function(rows) tryCatch(n_oqo(con, rows), oaapi_error = function(e) conditionMessage(e))
  expect_match(err(list(has("fulltext.search", "defor*"))), "exact")
  expect_match(err(list(has("fulltext.search", '"open'))), "quote")
  expect_match(err(list(has("fulltext.search", "  "))), "needs search terms")
  expect_match(err(list(has("no_such_field.search", "zurich"))), "not supported")
  expect_match(err(list(list(column_id = "fulltext.search", value = "x", operator = ">"))),
               "Operator")
})

test_that("a deep tree of many search terms runs in reasonable time", {
  # systematic-review strategies have 100+ terms in nested AND/OR groups; a
  # per-work BM25 call per term did not finish in minutes
  con <- local_fixture_con()
  words <- c("forest", "climate", "carbon", "water", "policy", "land", "species",
             "tropical", "energy", "health", "soil", "river", "urban", "fire", "ocean",
             "coral", "biodiversity", "emission", "agriculture", "conservation")
  group <- function(ws) list(join = "or", filters = lapply(ws, function(w) has("fulltext.search", w)))
  tree <- list(list(join = "and", filters = list(
    group(words), group(rev(words)),
    list(join = "or", filters = list(group(words[1:10]), group(words[11:20]),
                                     has("fulltext.search", '"tropical forest"')))
  )))
  t <- system.time(n <- n_oqo(con, tree))[["elapsed"]]
  expect_lt(t, 60)
  expect_equal(n, n_oqo(con, list(group(words))))
})

test_that("a wildcard-only search can be sorted by relevance", {
  con <- local_fixture_con()
  ctx <- list(con = con, columns = DBI::dbGetQuery(con, "DESCRIBE works")$column_name,
              served = openalexAPI:::.api_served_columns(con))
  r <- jsonlite::fromJSON(openalexAPI:::.api_execute(ctx, list(get_rows = "works",
    filter_rows = list(has("fulltext.search.exact", "defor*"))), list(per_page = 2)),
    simplifyVector = FALSE)
  expect_gt(r$meta$count, 0)
  expect_equal(r$results[[1]]$relevance_score, 0)
})

test_that("proximity: terms within N words, any order or (classic) in order", {
  con <- local_fixture_con()
  ex <- "fulltext.search.exact"
  gap <- "\\W+(?:\\w+\\W+){0,3}"
  any_order <- n_re(con, paste0("\\b(?:tropical", gap, "deforestation|deforestation", gap, "tropical)\\b"))
  expect_gt(any_order, 0)
  expect_equal(n_oqo(con, list(has(ex, '"tropical"~3~"deforestation"'))), any_order)
  expect_equal(n_oqo(con, list(has(ex, '"deforestation"~3~"tropical"'))), any_order)
  in_order <- n_re(con, paste0("\\btropical", gap, "deforestation\\b"))
  expect_equal(n_oqo(con, list(has(ex, '"tropical deforestation"~3'))), in_order)
  expect_lte(in_order, any_order)
  # wildcards inside proximity need the exact search; a stemmed one is refused
  expect_gt(n_oqo(con, list(has(ex, '"tropical"~3~"defor*"'))), 0)
  expect_error(n_oqo(con, list(has("fulltext.search", '"tropical"~3~"defor*"'))), "exact")
  # within one affiliation string
  aff <- "flatten(list_transform(authorships, lambda a: a.raw_affiliation_strings))"
  one <- n_sql(con, sprintf(paste0("coalesce(list_bool_or(list_transform(%s, lambda s: regexp_matches(s, ",
    "'(?i)\\b(?:columbia\\W+(?:\\w+\\W+){0,2}university|university\\W+(?:\\w+\\W+){0,2}columbia)\\b'))), false)"), aff))
  expect_gt(one, 0)
  expect_equal(n_oqo(con, list(has("raw_affiliation_strings.search.exact", '"columbia"~2~"university"'))), one)
})
