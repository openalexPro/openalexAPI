#' Serve a corpus as an OpenAlex-compatible API
#'
#' Prepares the corpus (see [prepare_corpus()]) and serves it like the
#' OpenAlex API, so `openalexPro::pro_query(endpoint = "http://127.0.0.1:8000")`
#' and the download functions of openalexPro work against it unchanged:
#'
#' * `GET /works?filter=...&sort=...&per_page=...&cursor=...`, also with
#'   `group_by=` (one dimension; `:include_unknown` shows works without a
#'   value as the group `unknown`)
#' * `GET /works/{id}` (any ID form, or a DOI)
#' * `GET /?oql=...` and `GET /?oqo=...`, `POST /` with `oqo` or `oql` and the
#'   view parameters `select`, `sort`, `per_page`, `page`, `cursor`
#' * `POST /query`: forwarded to OpenAlex (translation)
#' * `GET /corpus`: what is served (filters, group_by columns)
#' * `/`: a web page for running queries in the browser (`/www/index.html`)
#'
#' Classic URLs and OQL are translated to OQO by OpenAlex's free `/query`
#' endpoint (the network is needed, no credits are spent); an OQO runs fully
#' offline. Requests are handled one at a time.
#'
#' @param corpus Directory with the corpus Parquet files.
#' @param port,host Where to serve. The default host is only reachable from
#'   this computer; another host makes the corpus (and translation with
#'   `api_key`) available to the network.
#' @param api_key OpenAlex API key for translation requests that bring none.
#'   Not needed: translation is free.
#' @param root The OpenAlex API used for translation.
#' @param block Block the R session while serving.
#' @param ... Passed to [prepare_corpus()].
#' @return The plumber2 api object, invisibly.
#' @export
serve_corpus <- function(
  corpus,
  port = 8000L,
  host = "127.0.0.1",
  api_key = NULL,
  root = "https://api.openalex.org",
  block = TRUE,
  ...
) {
  if (!host %in% c("127.0.0.1", "localhost", "::1")) {
    warning("`host = \"", host, "\"` makes the corpus reachable from the network.",
            call. = FALSE)
  }
  db <- prepare_corpus(corpus, ...)
  app <- .api_app(db, api_key = api_key, root = root)
  plumber2::api_run(app, host = host, port = port, block = block,
                    showcase = FALSE, silent = TRUE)
  invisible(app)
}

#' View parameters: not part of the query, not sent for translation
#' @keywords internal
#' @noRd
.api_view_params <- c("select", "sort", "per_page", "page", "cursor")

#' Never forwarded or used
#' @keywords internal
#' @noRd
.api_drop_params <- c("api_key", "mailto")

#' Build the plumber2 app for a prepared corpus database
#' @keywords internal
#' @noRd
.api_app <- function(db, api_key = NULL, root = "https://api.openalex.org") {
  con <- .api_open(db)
  columns <- DBI::dbGetQuery(con, "DESCRIBE works")$column_name
  ctx <- list(
    con = con, db = db, columns = columns, served = .api_served_columns(con),
    root = root, api_key = api_key,
    corpus = DBI::dbGetQuery(con, "SELECT corpus FROM oaapi_meta")$corpus
  )
  raw <- list("application/json" = function(x) x)
  json_ok <- function(response, json, status = 200L) {
    response$status <- status
    json
  }
  # Every handler: an oaapi_error becomes its status with OpenAlex's body or
  # {error, message}; anything else a 500
  guard <- function(fn) {
    force(fn)
    function(request, response, ...) {
      tryCatch(fn(request, response, ...), error = function(e) {
        status <- if (inherits(e, "oaapi_error")) e$status else 500L
        body <- if (inherits(e, "oaapi_error") && !is.null(e$body)) e$body else
          .api_json(list(error = if (status >= 500L) "Internal error" else "Invalid query",
                         message = conditionMessage(e)))
        json_ok(response, body, status)
      })
    }
  }
  key_of <- function(request) {
    h <- request$get_header("authorization")
    if (length(h) && grepl("^Bearer ", h)) sub("^Bearer ", "", h) else ctx$api_key
  }
  query_of <- function(request) {
    # form encoding: `+` is a space (a literal plus arrives as %2B)
    qs <- gsub("+", "%20", request$querystring, fixed = TRUE)
    q <- httr2::url_parse(paste0("http://x/", qs))$query %||% list()
    q[setdiff(names(q), .api_drop_params)]
  }
  body_of <- function(request) {
    raw_body <- request$body_raw
    if (!length(raw_body)) return(list())
    tryCatch(jsonlite::fromJSON(rawToChar(raw_body), simplifyVector = FALSE),
             error = function(e) .api_abort("The request body is not valid JSON."))
  }
  run <- function(request, response, query, view) {
    # query: list with one of oqo (list or JSON text), oql, oxurl
    if (!is.null(query$oqo)) {
      oqo <- if (is.character(query$oqo)) {
        tryCatch(jsonlite::fromJSON(query$oqo, simplifyVector = FALSE),
                 error = function(e) .api_abort("`oqo` is not valid JSON."))
      } else {
        query$oqo
      }
      oql <- NULL
    } else {
      tr <- .api_translate(query, ctx$root, key_of(request))
      oqo <- tr$oqo
      oql <- tr$oql_oneline
    }
    json_ok(response, .api_execute(ctx, oqo, view, oql))
  }

  plumber2::api() |>
    plumber2::api_get("/works", guard(function(request, response, ...) {
      q <- query_of(request)
      view <- q[intersect(names(q), .api_view_params)]
      rest <- q[setdiff(names(q), .api_view_params)]
      # OpenAlex's translator refuses `:include_unknown`: taken out, kept as view
      if (!is.null(rest$group_by) && grepl(":include_unknown$", rest$group_by)) {
        rest$group_by <- sub(":include_unknown$", "", rest$group_by)
        view$include_unknown <- TRUE
      }
      if (!length(rest)) {
        return(json_ok(response, .api_execute(ctx, list(get_rows = "works"), view)))
      }
      oxurl <- paste0("/works?", paste(
        paste0(names(rest), "=", vapply(rest, utils::URLencode, "", reserved = TRUE)),
        collapse = "&"
      ))
      run(request, response, list(oxurl = oxurl), view)
    }), serializers = raw) |>
    # any ID form, also an unencoded URL with slashes (https://doi.org/10.x/y)
    plumber2::api_get("/works/*", guard(function(request, response, ...) {
      id <- utils::URLdecode(sub("^/works/", "", request$path))
      json_ok(response, .api_single(ctx, id, query_of(request)$select))
    }), serializers = raw) |>
    plumber2::api_get("/corpus", guard(function(request, response, ...) {
      reg <- .api_registry_works()
      ok <- vapply(reg, function(e) all(e$need %in% ctx$columns), TRUE)
      json_ok(response, .api_json(list(
        corpus = ctx$corpus,
        works = DBI::dbGetQuery(ctx$con, "SELECT count(*) AS n FROM works")$n,
        columns = ctx$served,
        filters = sort(names(reg)[ok]),
        filters_unavailable = sort(names(reg)[!ok]),
        group_by = intersect(names(.api_groups_works()), names(reg)[ok])
      )))
    }), serializers = raw) |>
    plumber2::api_get("/", guard(function(request, response, ...) {
      q <- query_of(request)
      view <- q[intersect(names(q), .api_view_params)]
      if (is.null(q$oql) && is.null(q$oqo)) {
        # the web page; `/` itself stays JSON-only for API clients
        response$status <- 302L
        response$set_header("Location", "/www/index.html")
        return("")
      }
      run(request, response, q[intersect(names(q), c("oql", "oqo"))], view)
    }), serializers = raw) |>
    plumber2::api_post("/", guard(function(request, response, ...) {
      b <- body_of(request)
      if (sum(c("oql", "oqo") %in% names(b)) != 1L) {
        .api_abort("The body needs exactly one of `oql`, `oqo`.")
      }
      view <- b[intersect(names(b), .api_view_params)]
      run(request, response, b[intersect(names(b), c("oql", "oqo"))], view)
    }), serializers = raw) |>
    plumber2::api_post("/query", guard(function(request, response, ...) {
      fwd <- .api_forward_query(body_of(request), ctx$root, key_of(request))
      json_ok(response, fwd$body, fwd$status)
    }), serializers = raw) |>
    plumber2::api_assets("/www", system.file("www", package = "openalexAPI"))
}
