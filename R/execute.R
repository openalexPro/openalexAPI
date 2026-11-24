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

  per_page <- .api_int(view$per_page, "per_page", 25L, max = 200L)
  cursor <- view$cursor
  if (!is.null(cursor) && !is.null(view$page)) {
    .api_abort("Use either `page` or `cursor`, not both.")
  }
  offset <- if (is.null(cursor) || identical(cursor, "*")) {
    if (is.null(cursor)) (.api_int(view$page, "page", 1L) - 1L) * per_page else 0L
  } else {
    v <- tryCatch(as.integer(rawToChar(jsonlite::base64_dec(cursor))),
                  error = function(e) NA_integer_, warning = function(w) NA_integer_)
    if (is.na(v) || v < 0L) .api_abort("Invalid `cursor`.")
    v
  }

  from <- paste("(SELECT * FROM works WHERE", sql$where, ")")
  if (!is.null(oqo$sample)) {
    n <- .api_int(oqo$sample, "sample", NA_integer_)
    seed <- if (is.null(oqo$seed)) "" else
      paste0(" REPEATABLE (", strtoi(substr(.api_md5(as.character(oqo$seed)), 1, 7), 16L), ")")
    from <- paste0("(SELECT * FROM ", from, " USING SAMPLE reservoir(", n, " ROWS)", seed, ")")
  }
  count <- DBI::dbGetQuery(con, paste("SELECT count(*) AS n FROM", from, "w"))$n

  order <- sql$order
  if (!length(order) && "cited_by_count" %in% ctx$columns) {
    order <- "cited_by_count DESC NULLS LAST"
  }
  order <- c(order, "oaapi_id") # stable paging

  cols <- .api_select(view$select, ctx$served)
  q <- DBI::dbQuoteIdentifier(con, cols)
  rows <- DBI::dbGetQuery(con, paste0(
    "SELECT to_json(struct_pack(", paste0(q, " := ", q, collapse = ", "), "))::VARCHAR AS j ",
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

#' One work by ID (any form), or 404
#' @keywords internal
#' @noRd
.api_single <- function(ctx, id) {
  con <- ctx$con
  q <- DBI::dbQuoteIdentifier(con, ctx$served)
  j <- DBI::dbGetQuery(con, paste0(
    "SELECT to_json(struct_pack(", paste0(q, " := ", q, collapse = ", "), "))::VARCHAR AS j ",
    "FROM works WHERE oaapi_id = ", DBI::dbQuoteLiteral(con, .api_id_key(id))
  ))$j
  if (!length(j)) {
    .api_abort(sprintf("Work `%s` is not in this corpus.", id), 404L)
  }
  j[[1]]
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
.api_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA))
}
