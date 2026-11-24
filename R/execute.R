# Running an OQO against the prepared corpus and building the OpenAlex-shaped
# response. The JSON of the records is made by DuckDB (to_json) and passed
# through: a round trip through R would change empty arrays, nulls, 64-bit
# integers and nested lists.

#' Parse a classic `sort=` value into OQO `sort_by`
#' @keywords internal
#' @noRd
.api_parse_sort <- function(sort) {
  lapply(strsplit(sort, ",", fixed = TRUE)[[1]], function(s) {
    p <- strsplit(s, ":", fixed = TRUE)[[1]]
    list(column_id = p[[1]], direction = if (length(p) > 1L) p[[2]] else "asc")
  })
}

#' An integer view parameter within limits
#' @keywords internal
#' @noRd
.api_int <- function(x, name, default, min = 1L, max = .Machine$integer.max) {
  if (is.null(x)) return(default)
  v <- suppressWarnings(as.integer(x))
  if (length(v) != 1L || is.na(v) || v < min || v > max) {
    .api_abort(sprintf("`%s` must be a whole number from %d to %d.", name, min, max))
  }
  v
}

#' Run a query and return the response JSON
#'
#' @param ctx The server context (connection, columns).
#' @param oqo The OQO (list).
#' @param view View parameters: `select`, `sort`, `per_page`, `page`, `cursor`.
#' @param oql The OQL text for `meta.x_query`, or `NULL`.
#' @return A JSON string.
#' @keywords internal
#' @noRd
.api_execute <- function(ctx, oqo, view = list(), oql = NULL) {
  t0 <- Sys.time()
  con <- ctx$con
  if (!is.null(view$sort)) {
    oqo$sort_by <- .api_parse_sort(view$sort) # a sibling `sort` wins
  }
  sql <- oqo_to_sql(oqo, con, ctx$columns)
  if (!is.null(sql$group)) {
    if (!is.null(oqo$sample)) .api_abort("`sample` cannot be combined with `group_by`.")
    return(.api_execute_groups(ctx, oqo, sql, view, oql, t0))
  }

  per_page <- .api_int(view$per_page, "per_page", 25L, max = 200L)
  cursor <- view$cursor
  offset <- .api_offset(view, per_page)

  from <- paste("(SELECT * FROM works WHERE", sql$where, ")")
  if (!is.null(oqo$sample)) {
    # Every page draws the sample again, so it must be the same each time: a
    # seed (derived from the query when none is given), rows in a fixed order,
    # and one thread (DuckDB repeats a seeded sample only single-threaded)
    n <- .api_int(oqo$sample, "sample", NA_integer_)
    seed <- oqo$seed %||% .api_json(oqo[setdiff(names(oqo), c("sort_by", "seed"))])
    seed <- strtoi(substr(.api_md5(as.character(seed)), 1, 7), 16L)
    from <- paste0("(SELECT * FROM (SELECT * FROM works WHERE ", sql$where,
                   " ORDER BY oaapi_id) USING SAMPLE reservoir(", n, " ROWS) REPEATABLE (",
                   seed, "))")
    threads <- DBI::dbGetQuery(con, "SELECT current_setting('threads') AS t")$t
    DBI::dbExecute(con, "SET threads = 1")
    on.exit(DBI::dbExecute(con, paste("SET threads =", threads)), add = TRUE)
  }
  count <- .api_get(con, paste("SELECT count(*) AS n FROM", from, "w"))$n

  order <- sql$order
  if (!length(order) && !is.null(sql$score)) {
    order <- paste0("(", sql$score, ") DESC") # a search: most relevant first
  }
  if (!length(order) && "cited_by_count" %in% ctx$columns) {
    order <- "cited_by_count DESC NULLS LAST"
  }
  order <- c(order, "oaapi_id") # stable paging

  # with a search, records carry `relevance_score`, as in OpenAlex
  cols <- .api_select(view$select, c(ctx$served, if (!is.null(sql$score)) "relevance_score"))
  q <- DBI::dbQuoteIdentifier(con, cols)
  expr <- ifelse(cols == "relevance_score", paste0("(", sql$score %||% "NULL", ")"), q)
  rows <- .api_get(con, paste0(
    "SELECT to_json(struct_pack(", paste0(q, " := ", expr, collapse = ", "), "))::VARCHAR AS j ",
    "FROM ", from, " w ORDER BY ", paste(order, collapse = ", "),
    " LIMIT ", per_page, " OFFSET ", offset
  ))$j

  next_cursor <- if (!is.null(cursor) && offset + per_page < count) {
    jsonlite::base64_enc(as.character(offset + per_page))
  }
  meta <- list(
    count = count,
    db_response_time_ms = round(as.numeric(Sys.time() - t0, units = "secs") * 1000),
    page = if (is.null(cursor)) offset %/% per_page + 1L,
    per_page = per_page,
    next_cursor = next_cursor,
    groups_count = NULL,
    x_query = list(oql = oql, oqo = oqo)
  )
  paste0(
    '{"meta":', .api_json(meta),
    ',"results":[', paste(rows, collapse = ","), ']',
    ',"group_by":[]}'
  )
}

#' Row offset from `page` or `cursor` (an opaque base64 offset; `*` starts)
#' @keywords internal
#' @noRd
.api_offset <- function(view, per_page) {
  cursor <- view$cursor
  if (!is.null(cursor) && !is.null(view$page)) {
    .api_abort("Use either `page` or `cursor`, not both.")
  }
  if (is.null(cursor)) {
    return((.api_int(view$page, "page", 1L) - 1L) * per_page)
  }
  if (identical(cursor, "*")) {
    return(0L)
  }
  v <- tryCatch(as.integer(rawToChar(jsonlite::base64_dec(cursor))),
                error = function(e) NA_integer_, warning = function(w) NA_integer_)
  if (is.na(v) || v < 0L) .api_abort("Invalid `cursor`.")
  v
}

#' A grouped query: works counted once per key, as OpenAlex's group_by
#'
#' The response has `group_by` (`key`, `key_display_name`, `count`) and no
#' `results`; `meta.count` is the number of matching works and
#' `meta.groups_count` the number of groups on this page. Sorted by count
#' (descending) unless `sort` says `key` / `count`; 200 groups per page by
#' default.
#' @keywords internal
#' @noRd
.api_execute_groups <- function(ctx, oqo, sql, view, oql, t0) {
  con <- ctx$con
  per_page <- .api_int(view$per_page, "per_page", 200L, max = 200L)
  offset <- .api_offset(view, per_page)
  # The filter runs once: the matching works' keys, then their groups, are
  # temp tables for this request (requests are handled one at a time)
  on.exit(DBI::dbExecute(con, "DROP TABLE IF EXISTS oaapi_m; DROP TABLE IF EXISTS oaapi_g"),
          add = TRUE)
  .api_get(con, paste0(
    "CREATE OR REPLACE TEMP TABLE oaapi_m AS SELECT oaapi_id, list_filter(",
    sql$group$pairs, ", lambda p: p.k IS NOT NULL) AS ks FROM works WHERE ", sql$where
  ), exec = TRUE)
  count <- .api_get(con, "SELECT count(*) AS n FROM oaapi_m")$n
  groups <- paste(
    "SELECT p.g AS g, min(p.k) AS k, min(p.d) AS d, count(DISTINCT oaapi_id) AS n, 0 AS u",
    "FROM (SELECT oaapi_id, unnest(ks) AS p FROM oaapi_m) GROUP BY p.g"
  )
  if (isTRUE(sql$group$include_unknown) || isTRUE(view$include_unknown)) {
    groups <- paste(
      groups, "UNION ALL SELECT 'unknown', 'unknown', 'unknown', count(*), 1 FROM oaapi_m",
      "WHERE coalesce(len(ks), 0) = 0 HAVING count(*) > 0"
    )
  }
  # a real value `unknown` and the unknown group are one group (their works
  # are disjoint: with a key, without one)
  .api_get(con, paste(
    "CREATE OR REPLACE TEMP TABLE oaapi_g AS",
    "SELECT g, min(k) AS k, min(d) AS d, sum(n) AS n, max(u) AS u FROM (", groups, ")",
    "GROUP BY g"
  ), exec = TRUE)
  n_groups <- .api_get(con, "SELECT count(*) AS n FROM oaapi_g")$n
  order <- c(sql$order %||% "n DESC", "k ASC")
  rows <- .api_get(con, paste0(
    "SELECT k AS key, d AS key_display_name, n AS count FROM oaapi_g ",
    "ORDER BY ", paste(order, collapse = ", "), " LIMIT ", per_page, " OFFSET ", offset
  ))
  next_cursor <- if (!is.null(view$cursor) && offset + per_page < n_groups) {
    jsonlite::base64_enc(as.character(offset + per_page))
  }
  meta <- list(
    count = count,
    db_response_time_ms = round(as.numeric(Sys.time() - t0, units = "secs") * 1000),
    page = if (is.null(view$cursor)) offset %/% per_page + 1L,
    per_page = per_page,
    next_cursor = next_cursor,
    groups_count = nrow(rows),
    x_query = list(oql = oql, oqo = oqo)
  )
  rows$count <- as.integer(rows$count)
  paste0(
    '{"meta":', .api_json(meta),
    ',"group_by":', if (nrow(rows)) .api_json(rows, na = "null") else "[]", '}'
  )
}

#' One work by ID (any form), or 404
#' @keywords internal
#' @noRd
.api_single <- function(ctx, id, select = NULL) {
  con <- ctx$con
  q <- DBI::dbQuoteIdentifier(con, .api_select(select, ctx$served))
  is_doi <- grepl("^(doi:|https?://(dx\\.)?doi\\.org/|10\\.)", id, ignore.case = TRUE)
  where <- if (is_doi) {
    paste("oaapi_doi =", DBI::dbQuoteLiteral(con, .api_doi_key(id)))
  } else {
    paste("oaapi_id =", DBI::dbQuoteLiteral(con, .api_id_key(id)))
  }
  j <- .api_get(con, paste0(
    "SELECT to_json(struct_pack(", paste0(q, " := ", q, collapse = ", "), "))::VARCHAR AS j ",
    "FROM works WHERE ", where, " LIMIT 1"
  ))$j
  if (!length(j)) {
    .api_abort(sprintf("Work `%s` is not in this corpus.", id), 404L)
  }
  j[[1]]
}

#' Run a query; a field the corpus lacks (DuckDB binder error) is a 400
#'
#' The registry checks only top-level columns; a struct field can still be
#' missing in corpora with an older or narrower schema.
#' @keywords internal
#' @noRd
.api_get <- function(con, sql, exec = FALSE) {
  run <- if (exec) DBI::dbExecute else DBI::dbGetQuery
  tryCatch(run(con, sql), error = function(e) {
    msg <- conditionMessage(e)
    if (!grepl("Binder Error", msg, fixed = TRUE)) stop(e)
    .api_abort(paste0(
      "A field of this query is not available in this corpus (",
      sub("\n.*", "", sub("^.*Binder Error: ", "", msg)), ")."
    ))
  })
}

#' Columns to return for `select` (comma-separated), checked
#' @keywords internal
#' @noRd
.api_select <- function(select, served) {
  if (is.null(select) || !nzchar(select)) return(served)
  sel <- trimws(strsplit(select, ",", fixed = TRUE)[[1]])
  bad <- setdiff(sel, served)
  if (length(bad)) {
    .api_abort(sprintf("Unknown `select` field(s): %s.", paste(bad, collapse = ", ")))
  }
  sel
}

#' Serialise a response list to JSON text
#' @keywords internal
#' @noRd
.api_json <- function(x, ...) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA, ...))
}
