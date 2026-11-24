#' Compile an OQO into DuckDB SQL
#'
#' Walks the filter tree of an OQO (OpenAlex Query Object): the top-level
#' `filter_rows` are joined with AND, a branch with its `join` (`and` /
#' `or`), and `is_negated` negates a leaf or branch. Each leaf goes through
#' the works column registry. A leaf whose value is missing never matches,
#' and its negation does, as with `!` in OpenAlex. The SQL refers to the
#' `works` table built by [prepare_corpus()].
#'
#' @param oqo An OQO as a list (e.g. from `jsonlite::fromJSON(x,
#'   simplifyVector = FALSE)`).
#' @param con A DuckDB connection (used to quote values).
#' @param columns The corpus columns; registry columns that need others
#'   are refused. `NULL` skips the check.
#' @return A list with `where` (SQL condition, `"true"` without filters) and
#'   `order` (SQL `ORDER BY` terms from `sort_by`, or `NULL`).
#' @export
oqo_to_sql <- function(oqo, con, columns = NULL) {
  registry <- .api_registry_works()
  if (!identical(oqo$get_rows %||% "works", "works")) {
    .api_abort(sprintf("Only works can be queried, not `%s`.", oqo$get_rows))
  }
  if (length(oqo$group_by)) {
    .api_abort("`group_by` is not supported yet.")
  }
  if (identical(oqo$corpus, "all") || is.list(oqo$corpus)) {
    # corpus choice is meaningless locally: the corpus is what was downloaded
    oqo$corpus <- NULL
  }
  node <- function(x) {
    neg <- isTRUE(x$is_negated)
    sql <- if (!is.null(x$filters)) {
      join <- toupper(x$join %||% "and")
      if (!join %in% c("AND", "OR")) .api_abort(sprintf("Unknown join `%s`.", x$join))
      if (!length(x$filters)) "true" else
        paste0("(", paste(vapply(x$filters, node, ""), collapse = paste0(" ", join, " ")), ")")
    } else {
      leaf(x)
    }
    if (neg) paste0("(NOT ", sql, ")") else sql
  }
  leaf <- function(x) {
    column <- x$column_id %||% ""
    entry <- registry[[column]]
    if (is.null(entry)) {
      if (grepl("\\.search(\\.exact)?$", column)) {
        .api_abort(sprintf("Search (`%s`) is not supported yet.", column))
      }
      .api_abort(sprintf(
        "Column `%s` is not supported. Supported: %s.",
        column, paste(sort(names(registry)), collapse = ", ")
      ))
    }
    if (!is.null(columns) && !all(entry$need %in% columns)) {
      .api_abort(sprintf("Column `%s` is not available in this corpus.", column))
    }
    if (is.null(x$value)) {
      .api_abort(sprintf("`%s` needs a value.", column))
    }
    if (is.list(x$value) && length(x$value) == 1L) x$value <- x$value[[1]]
    if (length(x$value) != 1L) {
      .api_abort(sprintf("`%s` needs a single value.", column))
    }
    op <- x$operator %||% "is" # an equality leaf has no operator key
    # never NULL: a missing value does not match, and its negation does
    paste0("coalesce(", entry$cond(op, x$value, con, column), ", false)")
  }
  where <- if (length(oqo$filter_rows)) {
    paste(vapply(oqo$filter_rows, node, ""), collapse = " AND ")
  } else {
    "true"
  }
  order <- NULL
  for (s in oqo$sort_by) {
    entry <- registry[[s$column_id %||% ""]]
    if (is.null(entry) || is.null(entry$sort)) {
      .api_abort(sprintf("Cannot sort by `%s`.", s$column_id))
    }
    dir <- toupper(s$direction %||% "asc")
    if (!dir %in% c("ASC", "DESC")) .api_abort(sprintf("Unknown sort direction `%s`.", dir))
    order <- c(order, paste(entry$sort, dir, "NULLS LAST"))
  }
  list(where = where, order = order)
}
