# Full-text search leaves: `<field>.search` (stemmed) and `<field>.search.exact`.
#
# A leaf value holds words, quoted phrases and (exact only) wildcards; AND/OR/NOT
# are already lifted into the OQO tree by OpenAlex's translator. All words must
# match (OpenAlex's default AND), scored with BM25 over the corpus's title and
# abstract (there is no full text locally: `fulltext.search` searches those).
# A phrase must also appear in this word order; a wildcard is matched as a
# regular expression on the text. Proximity (`"a b"~N`) is not supported.

#' Fields searched per OpenAlex search column
#' @keywords internal
#' @noRd
.api_search_fields <- c(
  fulltext = "title,abstract",
  title_and_abstract = "title,abstract",
  display_name = "title",
  abstract = "abstract"
)

#' Registry entries for the search columns
#' @keywords internal
#' @noRd
.api_search_entries <- function() {
  out <- list()
  for (f in names(.api_search_fields)) {
    for (exact in c(FALSE, TRUE)) {
      name <- paste0(f, ".search", if (exact) ".exact")
      out[[name]] <- .api_search_entry(.api_search_fields[[f]], exact)
    }
  }
  out
}

#' Split a search value into words, wildcards and phrases
#' @keywords internal
#' @noRd
.api_search_parse <- function(v, column, exact) {
  if (!is.character(v) || length(v) != 1L || is.na(v) || !nzchar(trimws(v))) {
    .api_abort(sprintf("`%s` needs search terms.", column))
  }
  if (grepl('"[^"]*"~[0-9]+', v)) {
    .api_abort("Proximity search (`\"...\"~N`) is not supported.")
  }
  phrases <- gsub('"', "", regmatches(v, gregexpr('"[^"]*"', v))[[1]])
  rest <- gsub('"[^"]*"', " ", v)
  if (grepl('"', rest, fixed = TRUE)) {
    .api_abort(sprintf("Unbalanced quote in `%s`.", column))
  }
  words <- strsplit(trimws(rest), "\\s+")[[1]]
  words <- words[nzchar(words)]
  wild <- grepl("[*?]", words)
  if (any(wild) && !exact) {
    .api_abort("Wildcards need the exact search (`search.exact`).")
  }
  list(words = words[!wild], wild = words[wild], phrases = phrases[nzchar(trimws(phrases))])
}

#' A regex-escaped word
#' @keywords internal
#' @noRd
.api_re_escape <- function(x) gsub("([.^$|()\\[\\]{}+*?\\\\])", "\\\\\\1", x, perl = TRUE)

#' One search column: `cond()` gives the condition, `score()` the BM25 score
#' @keywords internal
#' @noRd
.api_search_entry <- function(fields, exact) {
  fts <- if (exact) "fts_main_oaapi_docs_exact" else "fts_main_oaapi_docs_stem"
  text <- switch(fields,
    "title,abstract" = "concat_ws(' ', oaapi_title, oaapi_abstract)",
    "title" = "coalesce(oaapi_title, '')",
    "abstract" = "coalesce(oaapi_abstract, '')"
  )
  terms <- function(p) {
    unlist(c(p$words, strsplit(trimws(p$phrases), "\\s+")))
  }
  bm25 <- function(p, con) {
    t <- terms(p)
    if (!length(t)) return(NULL)
    paste0(fts, ".match_bm25(oaapi_id, ", DBI::dbQuoteLiteral(con, paste(t, collapse = " ")),
           ", fields := '", fields, "', conjunctive := 1)")
  }
  regex <- function(re, con) {
    paste0("regexp_matches(", text, ", ", DBI::dbQuoteLiteral(con, paste0("(?i)", re)), ")")
  }
  list(
    need = "id", sort = NULL, missing = NULL, search = TRUE,
    cond = function(op, v, con, column) {
      .api_op_has(op, column)
      p <- .api_search_parse(v, column, exact)
      score <- bm25(p, con)
      parts <- c(
        if (!is.null(score)) paste(score, "IS NOT NULL"),
        vapply(p$phrases, function(ph) {
          w <- .api_re_escape(strsplit(trimws(ph), "\\s+")[[1]])
          regex(paste0("\\b", paste(w, collapse = "\\W+"), "\\b"), con)
        }, ""),
        vapply(p$wild, function(w) {
          re <- gsub("\\\\\\*", "\\\\w*", gsub("\\\\\\?", "\\\\w", .api_re_escape(w)))
          regex(paste0("\\b", re, "\\b"), con)
        }, "")
      )
      paste0("(", paste(parts, collapse = " AND "), ")")
    },
    # wildcards alone have no BM25 terms: score 0, so records still carry
    # relevance_score
    score = function(op, v, con, column) {
      bm25(.api_search_parse(v, column, exact), con) %||% "0"
    }
  )
}

#' Search leaves take `has` (OQO) or `is`
#' @keywords internal
#' @noRd
.api_op_has <- function(op, column) {
  if (!op %in% c("has", "is")) {
    .api_abort(sprintf("Operator `%s` is not supported for `%s`.", op, column))
  }
}
