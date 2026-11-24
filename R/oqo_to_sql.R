#' Compile an OQO into DuckDB SQL
#'
#' Walks the filter tree of an OQO (OpenAlex Query Object): the top-level
#' `filter_rows` are joined with AND, a branch with its `join` (`and` /
#' `or`), and `is_negated` negates a leaf or branch. Each leaf goes through
#' the works column registry. A leaf whose value is missing never matches,
#' and its negation does, as with `!` in OpenAlex; a `null` leaf value
#' matches works where the field is missing. The SQL refers to the
#' `works` table built by [prepare_corpus()].
#'
#' @param oqo An OQO as a list (e.g. from `jsonlite::fromJSON(x,
#'   simplifyVector = FALSE)`).
#' @param con A DuckDB connection (used to quote values).
#' @param columns The corpus columns; registry columns that need others
#'   are refused. `NULL` skips the check.
#' @return A list with `where` (SQL condition, `"true"` without filters),
#'   `order` (SQL `ORDER BY` terms from `sort_by`, or `NULL`; with `group_by`
#'   terms on `n` (count), `k` (key) and `u` (1 for the unknown group)) and
#'   `group` (`NULL`, or the `group_by` dimension: `column`, `pairs` (SQL
#'   giving each work a list of `{g, k, d}` identity / key / display-name
#'   structs) and `include_unknown`), `score` (the BM25 relevance of the
#'   search leaves that are not negated, or `NULL` without a search), and
#'   `prelude` (SQL statements to run first: each search term's matches and
#'   scores as a temp table, computed once) with `temp` (their names, to
#'   drop afterwards). Run with `.api_run_prelude()`.
#' @export
oqo_to_sql <- function(oqo, con, columns = NULL) {
  registry <- .api_registry_works()
  if (!identical(oqo$get_rows %||% "works", "works")) {
    .api_abort(sprintf("Only works can be queried, not `%s`.", oqo$get_rows))
  }
  group <- .api_group_spec(oqo$group_by, columns)
  if (identical(oqo$corpus, "all") || is.list(oqo$corpus)) {
    # corpus choice is meaningless locally: the corpus is what was downloaded
    oqo$corpus <- NULL
  }
  # search terms: each BM25 query once, as a temp table oaapi_s<n>(i, s)
  fts <- character(0)      # BM25 query -> temp table name
  prelude <- character(0)
  positive <- character(0) # tables of the search leaves that are not negated
  any_search <- FALSE
  fts_table <- function(bm25, table) {
    if (!is.na(fts[bm25])) return(unname(fts[bm25]))
    name <- paste0("oaapi_s", length(fts) + 1L)
    fts[bm25] <<- name
    prelude <<- c(prelude, paste0(
      "CREATE OR REPLACE TEMP TABLE ", name, " AS SELECT * FROM (SELECT oaapi_id AS i, ",
      bm25, " AS s FROM ", table, ") WHERE s IS NOT NULL"
    ))
    name
  }
  node <- function(x, negated = FALSE) {
    neg <- isTRUE(x$is_negated)
    negated <- xor(negated, neg)
    sql <- if (!is.null(x$filters)) {
      join <- toupper(x$join %||% "and")
      if (!join %in% c("AND", "OR")) .api_abort(sprintf("Unknown join `%s`.", x$join))
      if (!length(x$filters)) "true" else
        paste0("(", paste(vapply(x$filters, node, "", negated = negated),
                          collapse = paste0(" ", join, " ")), ")")
    } else {
      leaf(x, negated)
    }
    if (neg) paste0("(NOT ", sql, ")") else sql
  }
  leaf <- function(x, negated) {
    column <- x$column_id %||% ""
    entry <- registry[[column]]
    if (is.null(entry)) {
      if (grepl("\\.search(\\.exact)?$", column)) {
        .api_abort(sprintf(
          "Search column `%s` is not supported; the corpus has title and abstract (%s).",
          column, paste(names(.api_search_entries()), collapse = ", ")))
      }
      .api_abort(sprintf(
        "Column `%s` is not supported. Supported: %s.",
        column, paste(sort(names(registry)), collapse = ", ")
      ))
    }
    if (!is.null(columns) && !all(entry$need %in% columns)) {
      .api_abort(sprintf("Column `%s` is not available in this corpus.", column))
    }
    op <- x$operator %||% "is" # an equality leaf has no operator key
    if (is.null(x$value)) {
      # `null`: works where the field is missing
      if (is.null(entry$missing) || op != "is") {
        .api_abort(sprintf("`%s` needs a value.", column))
      }
      return(paste0("coalesce(", entry$missing, ", false)"))
    }
    if (is.list(x$value) && length(x$value) == 1L) x$value <- x$value[[1]]
    if (length(x$value) != 1L) {
      .api_abort(sprintf("`%s` needs a single value.", column))
    }
    cond <- if (isTRUE(entry$search)) {
      any_search <<- TRUE
      p <- entry$parts(op, x$value, con, column)
      tbl <- if (!is.null(p$bm25)) fts_table(p$bm25, p$table)
      if (!is.null(tbl) && !negated) positive <<- c(positive, tbl)
      paste0("(", paste(c(if (!is.null(tbl)) paste0("oaapi_id IN (SELECT i FROM ", tbl, ")"),
                          p$regex), collapse = " AND "), ")")
    } else {
      entry$cond(op, x$value, con, column)
    }
    # never NULL: a missing value does not match, and its negation does
    paste0("coalesce(", cond, ", false)")
  }
  where <- if (length(oqo$filter_rows)) {
    paste(vapply(oqo$filter_rows, node, ""), collapse = " AND ")
  } else {
    "true"
  }
  # relevance: the sum of the non-negated leaves' scores, one lookup per work
  score <- if (length(positive)) {
    prelude <- c(prelude, paste0(
      "CREATE OR REPLACE TEMP TABLE oaapi_score AS SELECT i, sum(s) AS s FROM (",
      paste0("SELECT i, s FROM ", positive, collapse = " UNION ALL "), ") GROUP BY i"
    ))
    "coalesce((SELECT oaapi_score.s FROM oaapi_score WHERE oaapi_score.i = oaapi_id), 0)"
  } else if (any_search) {
    "CAST(0 AS DOUBLE)" # wildcards only: no BM25; a bare 0 in ORDER BY is a column number
  }
  order <- NULL
  for (s in oqo$sort_by) {
    if (identical(s$column_id, "relevance_score") && is.null(group)) {
      if (is.null(score)) .api_abort("Sorting by `relevance_score` needs a search.")
      dir <- toupper(s$direction %||% "desc")
      if (!dir %in% c("ASC", "DESC")) .api_abort(sprintf("Unknown sort direction `%s`.", dir))
      order <- c(order, paste0("(", score, ") ", dir))
      next
    }
    if (!is.null(group)) {
      # grouped: by the number of works or the key
      by <- s$column_id %||% ""
      if (!by %in% c("count", "key")) {
        .api_abort(sprintf("Cannot sort groups by `%s`.", by))
      }
      dir <- toupper(s$direction %||% "asc")
      if (!dir %in% c("ASC", "DESC")) .api_abort(sprintf("Unknown sort direction `%s`.", dir))
      # keys: numeric ones by number, the `unknown` group last (u)
      order <- c(order, if (by == "count") paste("n", dir) else
        c("u ASC", paste("TRY_CAST(k AS DOUBLE)", dir, "NULLS LAST"), paste("k", dir)))
      next
    }
    entry <- registry[[s$column_id %||% ""]]
    if (is.null(entry) || is.null(entry$sort)) {
      .api_abort(sprintf("Cannot sort by `%s`.", s$column_id))
    }
    dir <- toupper(s$direction %||% "asc")
    if (!dir %in% c("ASC", "DESC")) .api_abort(sprintf("Unknown sort direction `%s`.", dir))
    order <- c(order, paste(entry$sort, dir, "NULLS LAST"))
  }
  temp <- c(unname(fts), if (length(positive)) "oaapi_score")
  list(where = where, order = order, group = group, score = score,
       prelude = prelude, temp = temp)
}

#' Run an OQO's prelude (search terms as temp tables); returns a function
#' that drops them again
#' @keywords internal
#' @noRd
.api_run_prelude <- function(con, sql) {
  for (stmt in sql$prelude) .api_get(con, stmt, exec = TRUE)
  function() {
    for (t in sql$temp) DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", t))
  }
}

#' The group_by dimension: column, its `{k, d}` list SQL, include_unknown
#'
#' `NULL` without `group_by`. A `:include_unknown` suffix (classic URLs) also
#' shows the works without a value as the group `unknown`.
#' @keywords internal
#' @noRd
.api_group_spec <- function(group_by, columns = NULL) {
  if (!length(group_by)) return(NULL)
  if (length(group_by) > 1L) {
    .api_abort("Only one `group_by` dimension is supported.")
  }
  column <- group_by[[1]]$column_id %||% ""
  unknown <- grepl(":include_unknown$", column)
  column <- sub(":include_unknown$", "", column)
  pairs <- .api_groups_works()[column]
  if (is.na(pairs)) {
    .api_abort(sprintf("Cannot group by `%s`.", column))
  }
  need <- .api_registry_works()[[column]]$need
  if (!is.null(columns) && !all(need %in% columns)) {
    .api_abort(sprintf("Column `%s` is not available in this corpus.", column))
  }
  # OpenAlex lists both values of a true/false column, also with count 0
  boolean <- column %in% c("open_access.is_oa", "is_retracted", "is_paratext", "has_doi",
                           "has_abstract", "has_references",
                           "open_access.any_repository_has_fulltext",
                           "citation_normalized_percentile.is_in_top_1_percent",
                           "citation_normalized_percentile.is_in_top_10_percent")
  list(column = column, pairs = unname(pairs), include_unknown = unknown, boolean = boolean)
}
