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
  key <- .api_md5(c(.api_build_version, corpus,
                    paste(files, info$size, as.numeric(info$mtime))))
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

#' Version of the database layout; part of the cache key, so a cache built by
#' a version with another layout is rebuilt. Bump it when `.api_build()` changes.
#' @keywords internal
#' @noRd
.api_build_version <- "6"

#' Columns added by openalexPro or a query, not OpenAlex fields: never served
#' @keywords internal
#' @noRd
.api_not_served <- c("page", "citation", "abstract", "relevance_score")

#' Build the `works` table from the Parquet files
#' @keywords internal
#' @noRd
.api_build <- function(con, files) {
  wf <- .api_works_files(con, files)
  files <- wf$files
  src <- paste0(
    "read_parquet(", .api_sql_list(con, files),
    ", union_by_name = true, hive_partitioning = false)"
  )
  desc <- DBI::dbGetQuery(con, paste("DESCRIBE SELECT * FROM", src))
  cols <- desc$column_name
  .api_report_merged_types(wf$types)
  casts <- .api_restore_types(con, src, desc)
  if (!"id" %in% cols) {
    stop("The corpus has no `id` column.", call. = FALSE)
  }
  has <- function(x) x %in% cols
  # row order is irrelevant (every sort ends on id); lets DuckDB stream the
  # build instead of holding it in memory
  DBI::dbExecute(con, "SET preserve_insertion_order = false")
  # the window function keeps whole rows in memory (9.6 million snapshot works
  # ran out of 29 GB), so deduplicate only when there are duplicates
  dups <- DBI::dbGetQuery(con, paste(
    "SELECT count(*) - count(DISTINCT id_key(id)) AS n FROM", src, "WHERE id IS NOT NULL"
  ))$n > 0
  # the snapshot stores the inverted index as a JSON string (cast in `casts`)
  aii_text <- has("abstract_inverted_index") &&
    !grepl("^MAP\\(", desc$column_type[desc$column_name == "abstract_inverted_index"])
  abstract_text <- if (has("abstract")) {
    "abstract"
  } else if (aii_text) {
    gsub("abstract_inverted_index", .api_aii_cast, openalexPro::oa_works_abstract_sql(), fixed = TRUE)
  } else if (has("abstract_inverted_index")) {
    openalexPro::oa_works_abstract_sql()
  } else {
    "NULL"
  }
  # publishers (host organization lineage) with their names, for group_by
  pl <- desc$column_type[desc$column_name == "primary_location"]
  lin <- "primary_location.source.host_organization_lineage"
  publishers <- if (length(pl) && grepl("host_organization_lineage_names", pl, fixed = TRUE)) {
    paste0("list_transform(generate_series(1, coalesce(len(", lin, "), 0)), lambda i: ",
           "{'id': ", lin, "[i], 'name': primary_location.source.host_organization_lineage_names[i]})")
  } else if (length(pl) && grepl("host_organization_lineage", pl, fixed = TRUE)) {
    paste0("list_transform(", lin, ", lambda x: {'id': x, 'name': NULL::VARCHAR})")
  } else {
    "NULL::STRUCT(id VARCHAR, name VARCHAR)[]"
  }
  DBI::dbExecute(con, paste0(
    "CREATE TABLE works AS SELECT * ", casts, ", ",
    publishers, " AS oaapi_publishers, ",
    "id_key(id) AS oaapi_id, ",
    if (has("doi")) "doi_key(doi)" else "NULL::VARCHAR", " AS oaapi_doi, ",
    if (has("referenced_works")) "list_transform(referenced_works, lambda x: id_key(x))" else
      "NULL::VARCHAR[]", " AS oaapi_refs, ",
    abstract_text, " AS oaapi_abstract, ",
    if (has("title")) "title" else if (has("display_name")) "display_name" else "NULL::VARCHAR",
    " AS oaapi_title ",
    "FROM ", src, " WHERE id IS NOT NULL ",
    if (dups) "QUALIFY row_number() OVER (PARTITION BY id_key(id) ORDER BY id) = 1"
  ))
  if (!has("abstract_inverted_index")) {
    .api_add_inverted_index(con)
  }
  if (aii_text) {
    .api_merge_duplicate_keys(con, src)
  }
  .api_add_fts(con)
  invisible(con)
}

#' Full-text indexes over title and abstract, BM25
#'
#' Two copies: `oaapi_docs_stem` (Porter stemmer, English stop words) serves
#' the `*.search` columns, `oaapi_docs_exact` (no stemming, no stop words)
#' the `*.search.exact` columns, as OpenAlex's `search` / `search.exact`.
#' Each index is a table; DuckDB keeps one index per table.
#' @keywords internal
#' @noRd
.api_add_fts <- function(con) {
  for (kind in c("stem", "exact")) {
    tbl <- paste0("oaapi_docs_", kind)
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", tbl, " AS SELECT oaapi_id, oaapi_title AS title, ",
      "oaapi_abstract AS abstract FROM works"
    ))
    DBI::dbExecute(con, paste0(
      "PRAGMA create_fts_index('", tbl, "', 'oaapi_id', 'title', 'abstract', ",
      if (kind == "stem") "stemmer = 'porter', stopwords = 'english'" else
        "stemmer = 'none', stopwords = 'none'",
      ")"
    ))
  }
  invisible(con)
}

#' The files that hold works: an `id` and a `title` or `display_name` column
#'
#' A project folder can hold other tables too (openalexSnowball edges, the
#' `members/` of a collection); they are skipped with a message.
#' @keywords internal
#' @noRd
.api_works_files <- function(con, files) {
  schemas <- lapply(files, function(f) {
    DBI::dbGetQuery(con, paste0(
      "DESCRIBE SELECT * FROM read_parquet(", DBI::dbQuoteLiteral(con, f), ")"
    ))[, c("column_name", "column_type")]
  })
  is_works <- vapply(schemas, function(d) {
    "id" %in% d$column_name && any(c("title", "display_name") %in% d$column_name)
  }, TRUE)
  if (!any(is_works)) {
    stop("None of the Parquet files holds works (columns `id` and `title` or ",
         "`display_name`).", call. = FALSE)
  }
  if (!all(is_works)) {
    message("Skipping ", sum(!is_works), " Parquet file(s) that hold no works, e.g. ",
            files[!is_works][[1]])
  }
  list(files = files[is_works], types = unique(do.call(rbind, schemas[is_works])))
}

#' Columns the filters compare as numbers, dates or booleans, and the types
#' they accept (a regex on the DuckDB type)
#' @keywords internal
#' @noRd
.api_column_types <- list(
  publication_year = c("INTEGER", "^(TINYINT|SMALLINT|INTEGER|BIGINT|HUGEINT|U(TINY|SMALL|BIG)?INT(EGER)?)$"),
  cited_by_count = c("BIGINT", "^(TINYINT|SMALLINT|INTEGER|BIGINT|HUGEINT|U(TINY|SMALL|BIG)?INT(EGER)?)$"),
  fwci = c("DOUBLE", "^(TINYINT|SMALLINT|INTEGER|BIGINT|HUGEINT|FLOAT|DOUBLE|DECIMAL.*)$"),
  publication_date = c("DATE", "^(DATE|TIMESTAMP.*)$"),
  is_retracted = c("BOOLEAN", "^BOOLEAN$"),
  is_paratext = c("BOOLEAN", "^BOOLEAN$"),
  # the snapshot stores it as a JSON string; served and searched as a MAP
  abstract_inverted_index = c("MAP(VARCHAR, BIGINT[])", "^MAP\\(")
)

#' Tell which columns have different types in different files
#'
#' DuckDB then reads them as one common type, often text (a number in one
#' download, a string in another). The filter columns are cast back
#' (`.api_restore_types()`); other columns are served in the common type.
#' @keywords internal
#' @noRd
.api_report_merged_types <- function(types) {
  # compare the kind of type only (VARCHAR, BIGINT, STRUCT, STRUCT[], ...):
  # structs differing in field order or in nested fields are merged by name,
  # which is harmless
  kind <- types$column_type
  repeat {
    k <- gsub("\\([^()]*\\)", "", kind)
    if (identical(k, kind)) break
    kind <- k
  }
  kinds <- unique(data.frame(column_name = types$column_name, kind = kind))
  n <- table(kinds$column_name)
  mixed <- names(n)[n > 1]
  for (col in mixed) {
    message("Column `", col, "` has different types in different files (",
            paste(unique(types$column_type[types$column_name == col]), collapse = ", "),
            "); they are read as one common type.")
  }
  invisible(mixed)
}

#' `REPLACE (...)` casting filter columns back to their type, or ""
#'
#' A column read as another type (e.g. `cited_by_count` as text, because one
#' file had it as text) is cast with TRY_CAST; values that cannot be read
#' become NULL, with a message saying how many.
#' @keywords internal
#' @noRd
.api_restore_types <- function(con, src, desc) {
  out <- character(0)
  for (col in intersect(names(.api_column_types), desc$column_name)) {
    want <- .api_column_types[[col]]
    type <- desc$column_type[desc$column_name == col]
    if (grepl(want[[2]], type)) next
    q <- DBI::dbQuoteIdentifier(con, col)
    # a MAP is read from text as JSON (DuckDB's own text form of a MAP differs)
    if (startsWith(want[[1]], "MAP")) q <- paste0("TRY_CAST(", q, " AS JSON)")
    bad <- if (startsWith(want[[1]], "MAP")) {
      paste0("NOT json_valid(", DBI::dbQuoteIdentifier(con, col), ")")
    } else {
      paste0("TRY_CAST(", q, " AS ", want[[1]], ") IS NULL")
    }
    lost <- DBI::dbGetQuery(con, paste0(
      "SELECT count(*) AS n FROM ", src, " WHERE ", q, " IS NOT NULL AND ", bad
    ))$n
    message("Column `", col, "` is read as ", type, "; converted back to ", want[[1]],
            if (lost > 0) paste0(" (", lost, " value(s) could not be read and are missing)"),
            ".")
    out <- c(out, paste0("TRY_CAST(", q, " AS ", want[[1]], ") AS ", DBI::dbQuoteIdentifier(con, col)))
  }
  if (length(out)) paste0("REPLACE (", paste(out, collapse = ", "), ")") else ""
}

#' A JSON-string inverted index as a MAP (NULL if it cannot be cast)
#' @keywords internal
#' @noRd
.api_aii_cast <- "TRY_CAST(TRY_CAST(abstract_inverted_index AS JSON) AS MAP(VARCHAR, BIGINT[]))"

#' Inverted indexes with duplicate keys
#'
#' Some snapshot records repeat a word as a key (`{"social": [3], ...,
#' "social": [9]}`). Such JSON cannot be cast to a MAP (the cast leaves NULL),
#' so the positions of each word are merged here, and the abstract text is
#' built from the merged index.
#'
#' @keywords internal
#' @noRd
.api_merge_duplicate_keys <- function(con, src) {
  DBI::dbExecute(con, paste0(
    "CREATE TEMP TABLE oaapi_dup AS ",
    "SELECT i, map_from_entries(list({key: k, value: p})) AS m FROM (",
    "  SELECT id_key(s.id) AS i, e.key AS k, ",
    "    list_sort(list_distinct(flatten(list(CAST(e.value AS BIGINT[]))))) AS p ",
    "  FROM (SELECT id, abstract_inverted_index AS j FROM ", src,
    "        WHERE id IS NOT NULL AND json_valid(abstract_inverted_index) AND ",
    .api_aii_cast, " IS NULL) s, json_each(s.j) e ",
    "  GROUP BY ALL) GROUP BY i"
  ))
  on.exit(DBI::dbExecute(con, "DROP TABLE IF EXISTS oaapi_dup"), add = TRUE)
  DBI::dbExecute(con, paste0(
    "UPDATE works SET abstract_inverted_index = oaapi_dup.m FROM oaapi_dup ",
    "WHERE works.oaapi_id = oaapi_dup.i AND works.abstract_inverted_index IS NULL"
  ))
  DBI::dbExecute(con, paste0(
    "UPDATE works SET oaapi_abstract = ", openalexPro::oa_works_abstract_sql(),
    " WHERE oaapi_id IN (SELECT i FROM oaapi_dup)"
  ))
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
