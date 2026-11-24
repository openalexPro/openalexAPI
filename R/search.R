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
  #   "a"~N~"b"[~M~"c"]  each term within N (then M, ...) words of the next,
  #                      any order (OQL v2); a pair without a number takes the
  #                      previous one
  #   "a b"~N            the phrase's words in order, up to N words between them
  prox <- list()
  chain <- '"[^"]*"~[0-9]+~"[^"]*"((~[0-9]+)?~"[^"]*")*'
  for (m in regmatches(v, gregexpr(chain, v))[[1]]) {
    tok <- regmatches(m, gregexpr('"[^"]*"|~[0-9]+', m))[[1]]
    terms <- character(0)
    n <- integer(0)
    last <- NA_integer_
    for (t in tok) {
      if (startsWith(t, "~")) {
        last <- as.integer(sub("~", "", t))
      } else {
        if (length(terms)) n <- c(n, last)
        terms <- c(terms, gsub('"', "", t))
      }
    }
    prox[[length(prox) + 1L]] <- list(terms = terms, n = n, ordered = FALSE)
  }
  v <- gsub(chain, " ", v)
  for (m in regmatches(v, gregexpr('"[^"]*"~[0-9]+', v))[[1]]) {
    prox[[length(prox) + 1L]] <- list(
      terms = strsplit(trimws(gsub('"|~[0-9]+$', "", m)), "\\s+")[[1]],
      n = as.integer(sub(".*~", "", m)), ordered = TRUE)
  }
  v <- gsub('"[^"]*"~[0-9]+', " ", v)
  # the regex engine allows at most 1000 repetitions
  if (any(unlist(lapply(prox, `[[`, "n")) > 1000L)) {
    .api_abort("A proximity distance (`~N`) must be from 0 to 1000.")
  }
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
#'
#' `stem`: a function giving each word's stem, for the stemmed search
#' columns: a word then matches its stem plus any ending (`reefs` -> `reef\\w*`
#' matches reef, reefs), so phrase and proximity checks agree with the
#' stemmed BM25 match. Wildcard words are used as written.
#' @keywords internal
#' @noRd
.api_term_re <- function(term, stem = NULL) {
  words <- strsplit(trimws(term), "\\s+")[[1]]
  wild <- grepl("[*?]", words)
  w <- words
  if (!is.null(stem) && any(!wild)) w[!wild] <- stem(words[!wild])
  w <- .api_re_escape(w)
  w <- gsub("\\\\\\*", "\\\\w*", gsub("\\\\\\?", "\\\\w", w))
  if (!is.null(stem)) w[!wild] <- paste0(w[!wild], "\\w*")
  paste(w, collapse = "\\W+")
}

#' Regexes for a proximity spec: up to `n` words between the terms
#'
#' Unordered ("a"~N~"b"): each consecutive pair in either order. Ordered
#' ("a b"~N): all words in order. An approximation of Lucene slop.
#' @keywords internal
#' @noRd
.api_prox_re <- function(p, stem = NULL) {
  gap <- function(n) paste0("\\W+(?:\\w+\\W+){0,", n, "}")
  t <- vapply(p$terms, .api_term_re, "", stem = stem)
  if (p$ordered) {
    return(paste0("\\b", paste(t, collapse = gap(p$n[[1]])), "\\b"))
  }
  vapply(seq_len(length(t) - 1L), function(i) {
    g <- gap(p$n[[i]])
    paste0("\\b(?:", t[i], g, t[i + 1], "|", t[i + 1], g, t[i], ")\\b")
  }, "")
}

#' Porter stems of words, from DuckDB's FTS `stem()` (the stemmer of the index)
#' @keywords internal
#' @noRd
.api_stemmer <- function(con) {
  # stem() comes with the fts extension; loaded already on the server's
  # connection, not necessarily on one passed to oqo_to_sql()
  tryCatch(DBI::dbExecute(con, "LOAD fts"),
           error = function(e) DBI::dbExecute(con, "INSTALL fts; LOAD fts"))
  function(words) {
    vapply(words, function(w) {
      DBI::dbGetQuery(con, paste0("SELECT stem(", DBI::dbQuoteLiteral(con, tolower(w)),
                                  ", 'porter') AS s"))$s
    }, "", USE.NAMES = FALSE)
  }
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
        regex = {
          stem <- if (!exact) .api_stemmer(con)
          c(
            vapply(p$phrases, function(ph) {
              regex(paste0("\\b", .api_term_re(ph, stem), "\\b"), con)
            }, ""),
            vapply(p$wild, function(w) regex(paste0("\\b", .api_term_re(w), "\\b"), con), ""),
            vapply(unlist(lapply(p$prox, .api_prox_re, stem = stem)), regex, "", con = con)
          )
        }
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
