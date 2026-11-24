# Full-text search leaves: `<field>.search` (stemmed) and `<field>.search.exact`.
#
# A leaf value holds words, quoted phrases and (exact only) wildcards; AND/OR/NOT
# are already lifted into the OQO tree by OpenAlex's translator. All words must
# match (OpenAlex's default AND), scored with BM25 over the corpus's title and
# abstract (there is no full text locally: `fulltext.search` searches those).
# A phrase must also appear in this word order; a wildcard is matched as a
# regular expression on the text. Proximity -- `"a"~N~"b"` (OQL v2, any order)
# and `"a b"~N` (in order) -- allows up to N words between the terms
# (`.api_prox_re()`, an approximation of Lucene slop).

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
  lists <- c(
    raw_affiliation_strings = "flatten(list_transform(authorships, lambda a: a.raw_affiliation_strings))",
    raw_author_name = "list_transform(authorships, lambda a: a.raw_author_name)"
  )
  for (f in names(lists)) {
    for (exact in c(FALSE, TRUE)) {
      name <- paste0(f, ".search", if (exact) ".exact")
      out[[name]] <- .api_list_search_entry(lists[[f]], exact)
    }
  }
  out
}

#' A search over a list of strings per work (affiliations, author names)
#'
#' Not in the full-text index; matched as OpenAlex describes for these
#' fields: unquoted words may come from different strings of the work, a
#' quoted phrase must be within one string. Words match whole and ignoring
#' case; the stemmed and the exact column behave the same here (no
#' stemming), and they add nothing to `relevance_score`.
#' @keywords internal
#' @noRd
.api_list_search_entry <- function(list_expr, exact) {
  joined <- paste0("array_to_string(list_filter(", list_expr, ", lambda x: x IS NOT NULL), ' | ')")
  re <- function(target, pattern, con) {
    paste0("regexp_matches(", target, ", ", DBI::dbQuoteLiteral(con, paste0("(?i)", pattern)), ")")
  }
  list(
    need = "authorships", sort = NULL, missing = NULL, search = TRUE,
    parts = function(op, v, con, column) {
      .api_op_has(op, column)
      p <- .api_search_parse(v, column, exact)
      words <- vapply(p$words, function(w) re(joined, paste0("\\b", .api_re_escape(w), "\\b"), con), "")
      wild <- vapply(p$wild, function(w) {
        x <- gsub("\\\\\\*", "\\\\w*", gsub("\\\\\\?", "\\\\w", .api_re_escape(w)))
        re(joined, paste0("\\b", x, "\\b"), con)
      }, "")
      phrases <- vapply(p$phrases, function(ph) {
        w <- .api_re_escape(strsplit(trimws(ph), "\\s+")[[1]])
        paste0("coalesce(list_bool_or(list_transform(", list_expr, ", lambda s: ",
               re("s", paste0("\\b", paste(w, collapse = "\\W+"), "\\b"), con), ")), false)")
      }, "")
      prox <- vapply(unlist(lapply(p$prox, .api_prox_re)), function(x) {
        paste0("coalesce(list_bool_or(list_transform(", list_expr, ", lambda s: ",
               re("s", x, con), ")), false)")
      }, "")
      list(table = NULL, bm25 = NULL, regex = c(words, wild, phrases, prox))
    }
  )
}

#' Split a search value into words, wildcards and phrases
#' @keywords internal
#' @noRd
.api_search_parse <- function(v, column, exact) {
  if (!is.character(v) || length(v) != 1L || is.na(v) || !nzchar(trimws(v))) {
    .api_abort(sprintf("`%s` needs search terms.", column))
  }
  # proximity, taken out first:
  #   "a"~N~"b"[~"c"]  terms within N words of each other, any order (OQL v2)
  #   "a b"~N          the phrase's words in order, up to N words between them
  prox <- list()
  chain <- '"[^"]*"~[0-9]+(~"[^"]*")+'
  for (m in regmatches(v, gregexpr(chain, v))[[1]]) {
    prox[[length(prox) + 1L]] <- list(
      terms = gsub('"', "", regmatches(m, gregexpr('"[^"]*"', m))[[1]]),
      n = as.integer(sub('^"[^"]*"~([0-9]+).*', "\\1", m)), ordered = FALSE)
  }
  v <- gsub(chain, " ", v)
  for (m in regmatches(v, gregexpr('"[^"]*"~[0-9]+', v))[[1]]) {
    prox[[length(prox) + 1L]] <- list(
      terms = strsplit(trimws(gsub('"|~[0-9]+$', "", m)), "\\s+")[[1]],
      n = as.integer(sub(".*~", "", m)), ordered = TRUE)
  }
  v <- gsub('"[^"]*"~[0-9]+', " ", v)
  if (any(grepl("[*?]", unlist(lapply(prox, `[[`, "terms")))) && !exact) {
    .api_abort("Wildcards need the exact search (`search.exact`).")
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
  if (!length(words) && !length(phrases) && !length(prox)) {
    .api_abort(sprintf("`%s` needs search terms.", column))
  }
  list(words = words[!wild], wild = words[wild], phrases = phrases[nzchar(trimws(phrases))],
       prox = prox)
}

#' Regex for one search term (a word or phrase, `*` / `?` wildcards)
#' @keywords internal
#' @noRd
.api_term_re <- function(term) {
  w <- .api_re_escape(strsplit(trimws(term), "\\s+")[[1]])
  w <- gsub("\\\\\\*", "\\\\w*", gsub("\\\\\\?", "\\\\w", w))
  paste(w, collapse = "\\W+")
}

#' Regexes for a proximity spec: up to `n` words between the terms
#'
#' Unordered ("a"~N~"b"): each consecutive pair in either order. Ordered
#' ("a b"~N): all words in order. An approximation of Lucene slop.
#' @keywords internal
#' @noRd
.api_prox_re <- function(p) {
  gap <- paste0("\\W+(?:\\w+\\W+){0,", p$n, "}")
  t <- vapply(p$terms, .api_term_re, "")
  if (p$ordered) {
    return(paste0("\\b", paste(t, collapse = gap), "\\b"))
  }
  vapply(seq_len(length(t) - 1L), function(i) {
    paste0("\\b(?:", t[i], gap, t[i + 1], "|", t[i + 1], gap, t[i], ")\\b")
  }, "")
}

#' A regex-escaped word
#' @keywords internal
#' @noRd
.api_re_escape <- function(x) gsub("([.^$|()\\[\\]{}+*?\\\\])", "\\\\\\1", x, perl = TRUE)

#' One search column: `parts()` splits a value into its BM25 query and checks
#'
#' `parts()` returns `bm25` (the FTS `match_bm25()` call over `table`, or
#' `NULL` when the value has only wildcards) and `regex` (phrase and wildcard
#' checks on the text). `oqo_to_sql()` computes each BM25 query once per
#' request into a temp table of matching works and scores (see its
#' `prelude`): a per-work BM25 call per term made queries with many search
#' terms unusably slow (a 20-term OR took 21 s on 11,000 works, OpenAlex's
#' 114-term systematic-review example did not finish).
#' @keywords internal
#' @noRd
.api_search_entry <- function(fields, exact) {
  fts <- if (exact) "fts_main_oaapi_docs_exact" else "fts_main_oaapi_docs_stem"
  text <- switch(fields,
    "title,abstract" = "concat_ws(' ', oaapi_title, oaapi_abstract)",
    "title" = "coalesce(oaapi_title, '')",
    "abstract" = "coalesce(oaapi_abstract, '')"
  )
  regex <- function(re, con) {
    paste0("regexp_matches(", text, ", ", DBI::dbQuoteLiteral(con, paste0("(?i)", re)), ")")
  }
  list(
    need = "id", sort = NULL, missing = NULL, search = TRUE,
    parts = function(op, v, con, column) {
      .api_op_has(op, column)
      p <- .api_search_parse(v, column, exact)
      prox_words <- unlist(lapply(p$prox, function(x) strsplit(trimws(x$terms), "\\s+")))
      terms <- unlist(c(p$words, strsplit(trimws(p$phrases), "\\s+"),
                        prox_words[!grepl("[*?]", prox_words)]))
      list(
        table = sub("^fts_main_", "", fts),
        bm25 = if (length(terms)) {
          paste0(fts, ".match_bm25(oaapi_id, ",
                 DBI::dbQuoteLiteral(con, paste(terms, collapse = " ")),
                 ", fields := '", fields, "', conjunctive := 1)")
        },
        regex = c(
          vapply(p$phrases, function(ph) {
            w <- .api_re_escape(strsplit(trimws(ph), "\\s+")[[1]])
            regex(paste0("\\b", paste(w, collapse = "\\W+"), "\\b"), con)
          }, ""),
          vapply(p$wild, function(w) {
            re <- gsub("\\\\\\*", "\\\\w*", gsub("\\\\\\?", "\\\\w", .api_re_escape(w)))
            regex(paste0("\\b", re, "\\b"), con)
          }, ""),
          vapply(unlist(lapply(p$prox, .api_prox_re)), regex, "", con = con)
        )
      )
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
