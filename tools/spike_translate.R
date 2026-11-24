# Survey: how does OpenAlex's free POST /query endpoint translate classic
# /works URLs into OQO? The result defines the column registry keys
# (R/registry_works.R), so the registry matches what OpenAlex actually emits
# rather than what the column reference doc says.
#
# Run from the package root:  Rscript tools/spike_translate.R
# Writes inst/extdata/oqo_translations.csv (one row per leaf) and
# inst/extdata/oqo_translations.jsonl (full responses). Network needed; free.

inputs <- c(
  # filters
  "filter=type:article",
  "filter=type:!article",
  "filter=type:article|book",
  "filter=publication_year:2020",
  "filter=publication_year:>2020",
  "filter=publication_year:2018-2020",
  "filter=from_publication_date:2020-01-01",
  "filter=to_publication_date:2021-12-31",
  "filter=language:en",
  "filter=is_oa:true",
  "filter=open_access.is_oa:true",
  "filter=oa_status:gold",
  "filter=best_oa_location.license:cc-by",
  "filter=is_retracted:false",
  "filter=is_paratext:false",
  "filter=has_doi:true",
  "filter=has_abstract:true",
  "filter=has_references:true",
  "filter=cited_by_count:>10",
  "filter=fwci:>1",
  "filter=authors_count:<5",
  "filter=authorships.author.id:A5023888391",
  "filter=author.id:A5023888391",
  "filter=authorships.institutions.id:I136199984",
  "filter=institutions.id:I136199984",
  "filter=authorships.countries:ch",
  "filter=institutions.country_code:ch",
  "filter=authorships.institutions.country_code:ch",
  "filter=authorships.institutions.type:education",
  "filter=primary_location.source.id:S137773608",
  "filter=primary_location.source.type:journal",
  "filter=primary_topic.id:T10012",
  "filter=primary_topic.subfield.id:1201",
  "filter=primary_topic.field.id:12",
  "filter=primary_topic.domain.id:1",
  "filter=topics.id:T10012",
  "filter=keywords.id:climate-change",
  "filter=sustainable_development_goals.id:13",
  "filter=ids.openalex:W2741809807",
  "filter=openalex:W2741809807",
  "filter=doi:10.7717/peerj.4375",
  "filter=cites:W2741809807",
  "filter=cited_by:W2741809807",
  "filter=referenced_works:W2741809807",
  "filter=type:article,publication_year:2020",
  # field searches
  "filter=title.search:biodiversity",
  "filter=abstract.search:biodiversity",
  "filter=title_and_abstract.search:biodiversity",
  "filter=default.search:biodiversity",
  "filter=display_name.search:biodiversity",
  # search parameter
  "search=biodiversity",
  "search=biodiversity loss",
  "search=\"biodiversity loss\"",
  "search=biodiversity AND (forest OR grassland)",
  "search=biodiversity NOT marine",
  "search.exact=biodiversity",
  "search.exact=biodivers*",
  "search=\"biodiversity loss\"~3",
  # group_by / sort
  "group_by=type",
  "group_by=authorships.countries",
  "group_by=publication_year",
  "group_by=primary_topic.id",
  "group_by=open_access.is_oa",
  "filter=type:article&group_by=publication_year",
  "filter=type:article&sort=cited_by_count:desc"
)

translate <- function(q) {
  body <- list(oxurl = paste0("/works?", q))
  req <- httr2::request("https://api.openalex.org") |>
    httr2::req_url_path_append("query") |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_throttle(capacity = 5, fill_time_s = 1)
  resp <- httr2::req_perform(req)
  out <- tryCatch(httr2::resp_body_json(resp), error = function(e) list())
  out$http_status <- httr2::resp_status(resp)
  out$input <- q
  out
}

leaves <- function(rows, depth = 0L, path = "") {
  do.call(rbind, lapply(seq_along(rows), function(i) {
    r <- rows[[i]]
    p <- paste0(path, "/", i)
    if (!is.null(r$filters)) {
      rbind(
        data.frame(path = p, join = r$join, column_id = NA, operator = NA,
                   value = NA, is_negated = isTRUE(r$is_negated)),
        leaves(r$filters, depth + 1L, p)
      )
    } else {
      data.frame(
        path = p, join = NA,
        column_id = r$column_id %||% NA,
        operator = r$operator %||% NA,
        value = if (is.null(r$value)) NA else paste(unlist(r$value), collapse = "|"),
        is_negated = isTRUE(r$is_negated)
      )
    }
  }))
}
`%||%` <- function(x, y) if (is.null(x)) y else x

res <- lapply(inputs, function(q) {
  message(q)
  translate(q)
})

jsonl <- "inst/extdata/oqo_translations.jsonl"
writeLines(
  vapply(res, function(r) jsonlite::toJSON(r, auto_unbox = TRUE, null = "null"), ""),
  jsonl
)

rows <- do.call(rbind, lapply(res, function(r) {
  meta <- data.frame(
    input = r$input,
    http_status = r$http_status,
    valid = isTRUE(r$validation$valid),
    errors = paste(vapply(r$validation$errors %||% list(),
                          function(e) e$message %||% "", ""), collapse = "; "),
    group_by = paste(vapply(r$oqo$group_by %||% list(),
                            function(g) g$column_id %||% "", ""), collapse = ","),
    sort_by = paste(vapply(r$oqo$sort_by %||% list(),
                           function(s) paste0(s$column_id, ":", s$direction %||% ""), ""),
                    collapse = ","),
    oql = r$oql_oneline %||% NA
  )
  l <- if (length(r$oqo$filter_rows)) leaves(r$oqo$filter_rows) else
    data.frame(path = NA, join = NA, column_id = NA, operator = NA,
               value = NA, is_negated = NA)
  cbind(meta[rep(1, nrow(l)), ], l, row.names = NULL)
}))
utils::write.csv(rows, "inst/extdata/oqo_translations.csv", row.names = FALSE)
message("wrote ", nrow(rows), " rows")
