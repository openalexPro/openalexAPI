# Translation of OQL and classic URLs to OQO by OpenAlex's free POST /query.
#
# Called directly with httr2 rather than through openalexPro::pro_translate():
# a forwarded POST /query must return OpenAlex's status and body unchanged,
# and one code path serves both.

.api_cache <- new.env(parent = emptyenv())
.api_cache_max <- 500L

#' POST a body to OpenAlex's /query; status and body text, unchanged
#'
#' Stops with HTTP 503 if OpenAlex cannot be reached.
#' @keywords internal
#' @noRd
.api_forward_query <- function(body, root, api_key = NULL) {
  req <- httr2::request(root) |>
    httr2::req_url_path_append("query") |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(60) |>
    httr2::req_user_agent(paste0("openalexAPI/", utils::packageVersion("openalexAPI")))
  if (length(api_key) == 1L && !is.na(api_key) && nzchar(api_key)) {
    req <- httr2::req_auth_bearer_token(req, api_key)
  }
  resp <- tryCatch(httr2::req_perform(req), error = function(e) {
    .api_abort(paste0(
      "Translation unavailable: OpenAlex's /query endpoint could not be reached (",
      conditionMessage(e), "). An OQO runs without it: POST / or GET /?oqo=."
    ), 503L)
  })
  list(status = httr2::resp_status(resp), body = httr2::resp_body_string(resp))
}

#' Translate a body (`oql`, `oxurl` or `oqo`) to OQO, cached per session
#'
#' An invalid query stops with OpenAlex's own status and response body.
#' @return The parsed /query response (`oqo`, `oql_oneline`, ...).
#' @keywords internal
#' @noRd
.api_translate <- function(body, root, api_key = NULL) {
  key <- paste(root, jsonlite::toJSON(body, auto_unbox = TRUE))
  hit <- .api_cache[[key]]
  if (!is.null(hit)) {
    return(hit)
  }
  resp <- .api_forward_query(body, root, api_key)
  out <- tryCatch(jsonlite::fromJSON(resp$body, simplifyVector = FALSE),
                  error = function(e) NULL)
  if (resp$status != 200L || is.null(out$oqo) || isFALSE(out$validation$valid)) {
    .api_abort("OpenAlex could not translate the query.",
               status = if (resp$status == 200L) 400L else resp$status, body = resp$body)
  }
  if (length(.api_cache) >= .api_cache_max) {
    rm(list = ls(.api_cache, all.names = TRUE), envir = .api_cache) # ponytail: drop all, not LRU
  }
  assign(key, out, envir = .api_cache)
  out
}
