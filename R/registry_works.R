# Column registry for works: OQO `column_id` -> SQL.
#
# The keys are the column ids OpenAlex's /query endpoint emits, as surveyed in
# tools/spike_findings.md (not the column reference doc). Each entry is a
# list with
#   need: top-level corpus columns it needs (else "not available")
#   cond: function(op, value, con) -> SQL condition
#   sort: SQL expression to sort by (scalars only), or NULL
#   missing: SQL condition for a `null` value (field missing), or NULL
#
# Adding a column: one entry here and one test.

#' Error with an HTTP status, turned into an OpenAlex-shaped response
#' @keywords internal
#' @noRd
.api_abort <- function(message, status = 400L, body = NULL) {
  # `body`: a JSON response to pass through unchanged (OpenAlex's own error)
  stop(structure(
    class = c("oaapi_error", "error", "condition"),
    list(message = message, status = status, body = body, call = NULL)
  ))
}

# -- value checks ---------------------------------------------------------------

.api_num <- function(v, column) {
  n <- if (is.numeric(v) || is.character(v)) suppressWarnings(as.numeric(v)) else NA
  if (length(n) != 1L || !is.finite(n)) {
    .api_abort(sprintf("`%s` needs a number, not `%s`.", column, format(v)))
  }
  format(n, scientific = FALSE)
}

.api_bool <- function(v, column) {
  b <- if (is.logical(v)) v else if (is.character(v)) tolower(v) %in% c("true") else NA
  if (is.character(v) && !tolower(v) %in% c("true", "false")) b <- NA
  if (length(b) != 1L || is.na(b)) {
    .api_abort(sprintf("`%s` needs true or false, not `%s`.", column, format(v)))
  }
  b
}

.api_date <- function(v, column) {
  if (!is.character(v) || is.na(as.Date(v, optional = TRUE))) {
    .api_abort(sprintf("`%s` needs a date (YYYY-MM-DD), not `%s`.", column, format(v)))
  }
  paste0("DATE '", format(as.Date(v)), "'")
}

.api_ops <- c(is = "=", ">" = ">", ">=" = ">=", "<" = "<", "<=" = "<=")

.api_op <- function(op, column, allowed = names(.api_ops)) {
  if (!op %in% allowed) {
    .api_abort(sprintf("Operator `%s` is not supported for `%s`.", op, column))
  }
  .api_ops[[op]]
}

# -- entry constructors -----------------------------------------------------------

# A list that is missing or empty
.reg_empty <- function(expr) paste0("coalesce(len(", expr, "), 0) = 0")

# A number column (`>`, `>=`, `<`, `<=`, `is`)
.reg_num <- function(expr, need) {
  list(need = need, sort = expr, missing = paste(expr, "IS NULL"), cond = function(op, v, con, column) {
    paste(expr, .api_op(op, column), .api_num(v, column))
  })
}

# A date column; `fixed_op` for from_/to_publication_date
.reg_date <- function(expr, need, fixed_op = NULL) {
  list(need = need, sort = expr, missing = if (is.null(fixed_op)) paste(expr, "IS NULL"),
       cond = function(op, v, con, column) {
    if (!is.null(fixed_op)) {
      .api_op(op, column, "is")
      op <- fixed_op
    }
    paste(expr, .api_op(op, column), .api_date(v, column))
  })
}

# A string column, compared ignoring case
.reg_str <- function(expr, need) {
  list(need = need, sort = expr, missing = paste(expr, "IS NULL"), cond = function(op, v, con, column) {
    .api_op(op, column, "is")
    paste0("lower(", expr, ") = lower(", DBI::dbQuoteLiteral(con, as.character(v)), ")")
  })
}

# A list of strings, any element matching (ignoring case)
.reg_str_list <- function(expr, need) {
  list(need = need, sort = NULL, missing = .reg_empty(expr), cond = function(op, v, con, column) {
    .api_op(op, column, "is")
    paste0("list_contains(list_transform(", expr, ", lambda x: lower(x)), lower(",
           DBI::dbQuoteLiteral(con, as.character(v)), "))")
  })
}

# An ID column, compared on id_key()
.reg_id <- function(expr, need) {
  list(need = need, sort = NULL, missing = paste(expr, "IS NULL"), cond = function(op, v, con, column) {
    .api_op(op, column, "is")
    paste0("id_key(", expr, ") = ", DBI::dbQuoteLiteral(con, .api_id_key(v)))
  })
}

# A list of IDs, any element matching on id_key()
.reg_id_list <- function(expr, need) {
  list(need = need, sort = NULL, missing = .reg_empty(expr), cond = function(op, v, con, column) {
    .api_op(op, column, "is")
    paste0("list_contains(list_transform(", expr, ", lambda x: id_key(x)), ",
           DBI::dbQuoteLiteral(con, .api_id_key(v)), ")")
  })
}

# A boolean column, or a condition (`has_doi`): true -> it, false -> its negation
.reg_bool <- function(expr, need) {
  list(need = need, sort = NULL, cond = function(op, v, con, column) {
    .api_op(op, column, "is")
    if (.api_bool(v, column)) paste0("(", expr, ")") else paste0("NOT (", expr, ")")
  })
}

# -- the registry -----------------------------------------------------------------

#' The works column registry
#' @keywords internal
#' @noRd
.api_registry_works <- function() {
  inst_list <- function(field) {
    paste0("flatten(list_transform(authorships, lambda a: ",
           "list_transform(a.institutions, lambda i: i.", field, ")))")
  }
  list(
    "type" = .reg_str("type", "type"),
    "publication_year" = .reg_num("publication_year", "publication_year"),
    "from_publication_date" = .reg_date("publication_date", "publication_date", ">="),
    "to_publication_date" = .reg_date("publication_date", "publication_date", "<="),
    "language" = .reg_str("language", "language"),
    "open_access.is_oa" = .reg_bool("coalesce(open_access.is_oa, false)", "open_access"),
    "open_access.oa_status" = .reg_str("open_access.oa_status", "open_access"),
    "best_oa_location.license" = .reg_str("best_oa_location.license", "best_oa_location"),
    "is_retracted" = .reg_bool("coalesce(is_retracted, false)", "is_retracted"),
    "is_paratext" = .reg_bool("coalesce(is_paratext, false)", "is_paratext"),
    "has_doi" = .reg_bool("oaapi_doi IS NOT NULL", "doi"),
    "has_abstract" = .reg_bool("coalesce(oaapi_abstract, '') <> ''", "id"),
    "has_references" = .reg_bool("coalesce(len(referenced_works), 0) > 0", "referenced_works"),
    "cited_by_count" = .reg_num("cited_by_count", "cited_by_count"),
    "fwci" = .reg_num("fwci", "fwci"),
    "authors_count" = .reg_num("coalesce(len(authorships), 0)", "authorships"),
    "authorships.author.id" = .reg_id_list(
      "list_transform(authorships, lambda a: a.author.id)", "authorships"),
    "authorships.institutions.id" = .reg_id_list(inst_list("id"), "authorships"),
    "authorships.institutions.country_code" = .reg_str_list(inst_list("country_code"),
                                                            "authorships"),
    "authorships.institutions.type" = .reg_str_list(inst_list("type"), "authorships"),
    "authorships.countries" = .reg_str_list(
      "flatten(list_transform(authorships, lambda a: a.countries))", "authorships"),
    "primary_location.source.id" = .reg_id("primary_location.source.id", "primary_location"),
    "primary_location.source.type" = .reg_str("primary_location.source.type",
                                              "primary_location"),
    "primary_topic.id" = .reg_id("primary_topic.id", "primary_topic"),
    "primary_topic.field.id" = .reg_id("primary_topic.field.id", "primary_topic"),
    "primary_topic.domain.id" = .reg_id("primary_topic.domain.id", "primary_topic"),
    "topics.id" = .reg_id_list("list_transform(topics, lambda t: t.id)", "topics"),
    "keywords.id" = .reg_id_list("list_transform(keywords, lambda k: k.id)", "keywords"),
    "sustainable_development_goals.id" = .reg_id_list(
      "list_transform(sustainable_development_goals, lambda s: s.id)",
      "sustainable_development_goals"),
    "ids.openalex" = .reg_id("id", "id"),
    "doi" = list(need = "doi", sort = NULL, missing = "oaapi_doi IS NULL", cond = function(op, v, con, column) {
      .api_op(op, column, "is")
      paste0("oaapi_doi = ", DBI::dbQuoteLiteral(con, .api_doi_key(v)))
    }),
    # `cites:` -- works of the corpus whose references contain the ID
    "referenced_works" = list(need = "referenced_works", sort = NULL,
      missing = .reg_empty("oaapi_refs"), cond = function(op, v, con, column) {
        .api_op(op, column, "is")
        paste0("list_contains(oaapi_refs, ", DBI::dbQuoteLiteral(con, .api_id_key(v)), ")")
      }),
    # works the given work cites, as far as they are in the corpus
    "cited_by" = list(need = "referenced_works", sort = NULL,
      cond = function(op, v, con, column) {
        .api_op(op, column, "is")
        paste0("oaapi_id IN (SELECT unnest(oaapi_refs) FROM works WHERE oaapi_id = ",
               DBI::dbQuoteLiteral(con, .api_id_key(v)), ")")
      })
  )
}

# -- group_by keys ----------------------------------------------------------------

#' Group keys for works: `column_id` -> SQL giving each work a list of
#' `{g, k, d}` structs: the group identity, the key as returned, the display
#' name. For IDs `g` is `id_key()`, so an ID stored in several forms is one
#' group (shown with one of its stored forms).
#'
#' Key formats follow OpenAlex's group_by responses (surveyed live): entity IDs
#' as stored (`https://openalex.org/T10341`), slugs of non-native entities
#' under `https://openalex.org/<entity>/` (`types/article`, `countries/AU`),
#' plain values for years, counts, booleans, `oa_status` and institution
#' types. Display names are the entity's `display_name`; for countries,
#' languages and licenses OpenAlex shows a full name ("Australia"), the corpus
#' only has the code, which is shown instead. Columns OpenAlex cannot group by
#' (dates, `fwci`, `doi`, `cited_by`) are absent.
#' @keywords internal
#' @noRd
.api_groups_works <- function() {
  pair <- function(k, d, g) {
    sprintf("{'g': CAST(%s AS VARCHAR), 'k': CAST(%s AS VARCHAR), 'd': CAST(%s AS VARCHAR)}",
            g, k, d)
  }
  one <- function(k, d = k, id = FALSE) {
    paste0("[", pair(k, d, if (id) paste0("id_key(", k, ")") else k), "]")
  }
  many <- function(lst, k, d = k, id = FALSE) {
    sprintf("list_transform(%s, lambda x: %s)",
            lst, pair(k, d, if (id) paste0("id_key(", k, ")") else k))
  }
  pre <- function(entity, x) sprintf("'https://openalex.org/%s/' || %s", entity, x)
  inst <- "flatten(list_transform(authorships, lambda a: a.institutions))"
  countries <- "flatten(list_transform(authorships, lambda a: a.countries))"
  c(
    "type" = one(pre("types", "type"), "type"),
    "publication_year" = one("publication_year"),
    "language" = one(pre("languages", "language"), "language"),
    "open_access.is_oa" = one("open_access.is_oa"),
    "open_access.oa_status" = one("open_access.oa_status"),
    "best_oa_location.license" = one(pre("licenses", "best_oa_location.license"),
                                     "best_oa_location.license"),
    "is_retracted" = one("is_retracted"),
    "is_paratext" = one("is_paratext"),
    "has_doi" = one("oaapi_doi IS NOT NULL"),
    "has_abstract" = one("coalesce(oaapi_abstract, '') <> ''"),
    "has_references" = one("coalesce(len(referenced_works), 0) > 0"),
    "cited_by_count" = one("cited_by_count"),
    "authors_count" = one("coalesce(len(authorships), 0)"),
    "authorships.author.id" = many("authorships", "x.author.id", "x.author.display_name",
                                   id = TRUE),
    "authorships.institutions.id" = many(inst, "x.id", "x.display_name", id = TRUE),
    "authorships.institutions.country_code" = many(
      inst, pre("countries", "upper(x.country_code)"), "upper(x.country_code)"),
    "authorships.institutions.type" = many(inst, "x.type"),
    "authorships.countries" = many(countries, pre("countries", "upper(x)"), "upper(x)"),
    "primary_location.source.id" = one("primary_location.source.id",
                                       "primary_location.source.display_name", id = TRUE),
    "primary_location.source.type" = one(pre("source-types", "primary_location.source.type"),
                                         "primary_location.source.type"),
    "primary_topic.id" = one("primary_topic.id", "primary_topic.display_name", id = TRUE),
    "primary_topic.field.id" = one("primary_topic.field.id",
                                   "primary_topic.field.display_name", id = TRUE),
    "primary_topic.domain.id" = one("primary_topic.domain.id",
                                    "primary_topic.domain.display_name", id = TRUE),
    "topics.id" = many("topics", "x.id", "x.display_name", id = TRUE),
    "keywords.id" = many("keywords", "x.id", "x.display_name", id = TRUE),
    "sustainable_development_goals.id" = many(
      "sustainable_development_goals", pre("sdgs", "id_key(x.id)"), "x.display_name"),
    "ids.openalex" = one("id", id = TRUE),
    "referenced_works" = many("referenced_works", "x", id = TRUE)
  )
}
