#' Prepare a Parquet corpus for serving
#'
#' Reads all Parquet files below `corpus` into one DuckDB database file: one
#' row per work (duplicates across partitions removed), with helper columns
#' for ID and DOI matching and the abstract as text. The database is cached
#' and reused while the corpus files are unchanged (same paths, sizes and
#' modification times).
#'
#' The corpus can come from `openalexPro::pro_request_parquet()` (keeps
#' `abstract_inverted_index` and adds `abstract`), from the JSONL path (only
#' `abstract`), from a snapshot extraction, or from any source with the
#' OpenAlex works schema. Hive partition columns are not read.
#'
#' @param corpus Directory with the corpus Parquet files (searched
#'   recursively).
#' @param rebuild Rebuild the database even if a cached one exists.
#' @param cache_dir Where the database files are kept.
#' @return The path of the DuckDB database file, invisibly.
#' @export
prepare_corpus <- function(
  corpus,
  rebuild = FALSE,
  cache_dir = tools::R_user_dir("openalexAPI", "cache")
) {
  corpus <- normalizePath(corpus, mustWork = TRUE)
  files <- list.files(corpus, "\\.parquet$", recursive = TRUE, full.names = TRUE)
  if (!length(files)) {
    stop("No Parquet files found in `", corpus, "`.", call. = FALSE)
  }
  info <- file.info(files)
  key <- .api_md5(c(corpus, paste(files, info$size, as.numeric(info$mtime))))
  db <- file.path(cache_dir, paste0(key, ".duckdb"))
  if (file.exists(db) && !isTRUE(rebuild)) {
    return(invisible(db))
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(db, ".part")
  unlink(c(tmp, paste0(tmp, ".wal")))
  con <- .api_con(tmp)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .api_build(con, files)
  DBI::dbExecute(con, "CREATE TABLE oaapi_meta (corpus VARCHAR, built TIMESTAMP)")
  DBI::dbExecute(con, paste0(
    "INSERT INTO oaapi_meta VALUES (", DBI::dbQuoteLiteral(con, corpus), ", now())"
  ))
  DBI::dbDisconnect(con, shutdown = TRUE)
  on.exit()
  file.rename(tmp, db)
  invisible(db)
}

#' Columns added by openalexPro or a query, not OpenAlex fields: never served
#' @keywords internal
#' @noRd
.api_not_served <- c("page", "citation", "abstract", "relevance_score")

#' Build the `works` table from the Parquet files
#' @keywords internal
#' @noRd
.api_build <- function(con, files) {
  src <- paste0(
    "read_parquet(", .api_sql_list(con, files),
    ", union_by_name = true, hive_partitioning = false)"
  )
  cols <- DBI::dbGetQuery(con, paste("DESCRIBE SELECT * FROM", src))$column_name
  if (!"id" %in% cols) {
    stop("The corpus has no `id` column.", call. = FALSE)
  }
  has <- function(x) x %in% cols
  abstract_text <- if (has("abstract")) {
    "abstract"
  } else if (has("abstract_inverted_index")) {
    openalexPro::oa_works_abstract_sql()
  } else {
    "NULL"
  }
  DBI::dbExecute(con, paste0(
    "CREATE TABLE works AS SELECT *, ",
    "id_key(id) AS oaapi_id, ",
    if (has("doi")) "doi_key(doi)" else "NULL::VARCHAR", " AS oaapi_doi, ",
    if (has("referenced_works")) "list_transform(referenced_works, lambda x: id_key(x))" else
      "NULL::VARCHAR[]", " AS oaapi_refs, ",
    abstract_text, " AS oaapi_abstract ",
    "FROM ", src, " ",
    "QUALIFY row_number() OVER (PARTITION BY id_key(id) ORDER BY id) = 1"
  ))
  if (!has("abstract_inverted_index")) {
    .api_add_inverted_index(con)
  }
  invisible(con)
}

#' Rebuild `abstract_inverted_index` from the abstract text (JSONL-path corpora)
#'
#' The JSONL path of openalexPro deletes it, but OpenAlex responses carry it,
#' and openalexPro rebuilds `abstract` from it when it converts a response.
#' Words are split on single spaces, as openalexPro joins them.
#' @keywords internal
#' @noRd
.api_add_inverted_index <- function(con) {
  DBI::dbExecute(con, "
    CREATE TEMP TABLE oaapi_aii AS
    SELECT oaapi_id,
           map_from_entries(list({'k': w, 'v': pos} ORDER BY w)) AS aii
    FROM (
      SELECT oaapi_id, w, list(i - 1 ORDER BY i) AS pos
      FROM (
        SELECT oaapi_id, unnest(ws) AS w, unnest(range(1, len(ws) + 1)) AS i
        FROM (SELECT oaapi_id, string_split(oaapi_abstract, ' ') AS ws
              FROM works WHERE oaapi_abstract IS NOT NULL AND oaapi_abstract <> '')
      )
      GROUP BY oaapi_id, w
    )
    GROUP BY oaapi_id")
  DBI::dbExecute(con, "ALTER TABLE works ADD COLUMN abstract_inverted_index MAP(VARCHAR, BIGINT[])")
  DBI::dbExecute(con, "
    UPDATE works SET abstract_inverted_index = oaapi_aii.aii
    FROM oaapi_aii WHERE works.oaapi_id = oaapi_aii.oaapi_id")
  invisible(con)
}

#' Columns of `works` that are served in responses
#' @keywords internal
#' @noRd
.api_served_columns <- function(con) {
  cols <- DBI::dbGetQuery(con, "DESCRIBE works")$column_name
  cols[!startsWith(cols, "oaapi_") & !cols %in% .api_not_served]
}

#' Open a prepared corpus database read-only
#' @keywords internal
#' @noRd
.api_open <- function(db) {
  .api_con(db, read_only = TRUE)
}

#' md5 of a character vector
#' @keywords internal
#' @noRd
.api_md5 <- function(x) {
  f <- tempfile()
  on.exit(unlink(f))
  writeLines(x, f, useBytes = TRUE)
  unname(tools::md5sum(f))
}

#' A SQL list literal of strings: `['a', 'b']`
#' @keywords internal
#' @noRd
.api_sql_list <- function(con, x) {
  paste0("[", paste(DBI::dbQuoteLiteral(con, x), collapse = ", "), "]")
}
