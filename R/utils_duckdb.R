# DuckDB connection helpers.
#
# Follows openalexPro's `.pro_duckdb()` / `.pro_con()` (openalexPro/R/utils_duckdb.R),
# copied rather than called via `:::`.

#' Default operator
#'
#' Base R only gained `%||%` in 4.4.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' DuckDB driver without the duckdb 2.0 storage-location message
#'
#' @keywords internal
#' @noRd
.api_duckdb <- function(dbdir = ":memory:", read_only = FALSE) {
  user_chose <- !is.null(getOption("duckdb.home")) ||
    nzchar(Sys.getenv("DUCKDB_R_HOME"))
  if (user_chose || !"shared_home" %in% names(formals(duckdb::duckdb))) {
    return(duckdb::duckdb(dbdir = dbdir, read_only = read_only))
  }
  # duckdb reuses an open instance for the same file and refuses `shared_home`
  # for it ("can't be applied to the database instance"), e.g. when a server
  # in this session still holds the database: open that instance as it is.
  tryCatch(
    duckdb::duckdb(
      dbdir = dbdir,
      read_only = read_only,
      shared_home = dir.exists("~/.duckdb")
    ),
    error = function(e) duckdb::duckdb(dbdir = dbdir, read_only = read_only)
  )
}

#' Open a DuckDB connection with the json and fts extensions and the id macros
#'
#' Extensions are installed and loaded explicitly: CI runners do not
#' auto-download them.
#'
#' @param dbdir Database file, or `":memory:"`.
#' @param read_only Open read-only.
#' @return A DBI connection. The caller disconnects.
#' @keywords internal
#' @noRd
.api_con <- function(dbdir = ":memory:", read_only = FALSE) {
  con <- DBI::dbConnect(.api_duckdb(dbdir, read_only))
  DBI::dbExecute(con, "INSTALL json; LOAD json; INSTALL fts; LOAD fts;")
  .api_macros(con)
  con
}

#' Create the id / DOI normalisation macros
#'
#' IDs come in several forms: a full URL (`https://openalex.org/W123`), bare
#' (`W123`), or with any other letter prefix (`ABC123`, `wZ...`). `id_key()`
#' strips everything up to the last `/` and upper-cases the rest, making no
#' assumption about the prefix. `doi_key()` strips a `doi.org` URL or `doi:`
#' prefix and lower-cases. Both are `TEMP` macros, so they also work on a
#' read-only connection.
#'
#' @keywords internal
#' @noRd
.api_macros <- function(con) {
  DBI::dbExecute(con, paste(
    "CREATE OR REPLACE TEMP MACRO id_key(x) AS",
    "upper(regexp_replace(CAST(x AS VARCHAR), '^.*/', ''))"
  ))
  DBI::dbExecute(con, paste(
    "CREATE OR REPLACE TEMP MACRO doi_key(x) AS",
    "lower(regexp_replace(CAST(x AS VARCHAR),",
    "'^(https?://(dx\\.)?doi\\.org/|doi:)', '', 'i'))"
  ))
  invisible(con)
}

#' Same normalisation in R, for values arriving in requests
#'
#' @keywords internal
#' @noRd
.api_id_key <- function(x) toupper(sub("^.*/", "", as.character(x)))

#' @keywords internal
#' @noRd
.api_doi_key <- function(x) {
  tolower(sub(
    "^(https?://(dx\\.)?doi\\.org/|doi:)", "",
    as.character(x),
    ignore.case = TRUE
  ))
}
