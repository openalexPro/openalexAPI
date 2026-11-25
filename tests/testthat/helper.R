# Test corpora (see tools/make_fixtures.R)
fixture <- function(name) test_path("fixtures", name)

# A prepared corpus database in a per-session cache
fixture_db <- function(name = "corpus_jq") {
  prepare_corpus(fixture(name), cache_dir = file.path(tempdir(), "oaapi-cache"))
}

# A read-only connection to a prepared fixture, closed with the calling frame
local_fixture_con <- function(name = "corpus_jq", env = parent.frame()) {
  con <- openalexAPI:::.api_open(fixture_db(name))
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  con
}

# Number of works matching an OQO, through oqo_to_sql()
n_oqo <- function(con, filter_rows, columns = NULL) {
  s <- oqo_to_sql(list(get_rows = "works", filter_rows = filter_rows), con, columns)
  drop <- openalexAPI:::.api_run_prelude(con, s)
  on.exit(drop(), add = TRUE)
  DBI::dbGetQuery(con, paste("SELECT count(*) AS n FROM works WHERE", s$where))$n
}

# Number of works matching hand-written SQL
n_sql <- function(con, where) {
  DBI::dbGetQuery(con, paste("SELECT count(*) AS n FROM works WHERE", where))$n
}

leaf <- function(column_id, value, operator = NULL, is_negated = FALSE) {
  Filter(Negate(is.null), list(column_id = column_id, value = value,
                               operator = operator, is_negated = is_negated))
}

# A server for a corpus in a background R process; stopped with the calling
# frame. Returns its root URL.
local_server <- function(corpus = fixture("corpus_jq"), env = parent.frame()) {
  skip_if_not_installed("callr")
  port <- httpuv::randomPort()
  cache <- file.path(tempdir(), "oaapi-cache")
  pkg <- normalizePath(test_path("..", ".."))
  p <- callr::r_bg(function(pkg, corpus, port, cache) {
    # a source tree (devtools::test) has R/; an installed package (covr,
    # R CMD check) has a DESCRIPTION but no sources, so load it instead
    if (dir.exists(file.path(pkg, "R")) && length(list.files(file.path(pkg, "R"), "[.]R$")) > 0) {
      pkgload::load_all(pkg, quiet = TRUE)
    } else {
      library(openalexAPI)
    }
    serve_corpus(corpus, port = port, cache_dir = cache)
  }, args = list(pkg = pkg, corpus = normalizePath(corpus), port = port, cache = cache))
  withr::defer(p$kill(), envir = env)
  url <- paste0("http://127.0.0.1:", port)
  for (i in 1:100) {
    ok <- tryCatch(httr2::req_perform(httr2::request(paste0(url, "/corpus"))), error = function(e) NULL)
    if (!is.null(ok)) return(url)
    if (!p$is_alive()) stop("server died: ", p$read_all_error())
    Sys.sleep(0.2)
  }
  stop("server did not start: ", p$read_error())
}

# GET or POST against the test server; list(status, json, text)
call_api <- function(url, path, body = NULL) {
  r <- httr2::request(paste0(url, path)) |> httr2::req_error(is_error = function(x) FALSE)
  if (!is.null(body)) r <- httr2::req_body_json(r, body, auto_unbox = TRUE)
  resp <- httr2::req_perform(r)
  text <- httr2::resp_body_string(resp)
  skip_if_upstream_5xx(httr2::resp_status(resp), text)
  list(status = httr2::resp_status(resp), text = text,
       json = jsonlite::fromJSON(text, simplifyVector = FALSE))
}

# A 5xx from the server is OpenAlex's /query failing (passed through, or 503
# when unreachable): skip the online test rather than fail it. The server's own
# crash (500 "Internal error") still fails.
skip_if_upstream_5xx <- function(status, text) {
  if (status < 500) return(invisible())
  json <- tryCatch(jsonlite::fromJSON(text), error = function(e) NULL)
  if (status == 500 && identical(json$error, "Internal error")) return(invisible())
  skip(paste0("OpenAlex /query failed upstream (HTTP ", status, ")"))
}

# openalexPro::pro_fetch(), skipping when it fails on such a 5xx
fetch <- function(...) {
  tryCatch(openalexPro::pro_fetch(...), error = function(e) {
    for (cnd in list(e, e$parent, e$parent$parent)) {
      if (inherits(cnd, "httr2_http") && !is.null(cnd$resp)) {
        skip_if_upstream_5xx(httr2::resp_status(cnd$resp), httr2::resp_body_string(cnd$resp))
      }
    }
    # via = "oqo": openalexPro rewraps a failed translation, only the message is left
    status <- regmatches(conditionMessage(e), regexpr("(?<=HTTP )5[0-9]{2}", conditionMessage(e), perl = TRUE))
    if (length(status)) skip_if_upstream_5xx(as.integer(status), "")
    stop(e)
  })
}
