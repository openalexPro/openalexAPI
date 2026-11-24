# Full-text engine benchmark: DuckDB FTS vs SQLite FTS5 on the CPR passages.
#
# Question: can openalexAPI search full text (cnp's passages) with DuckDB's
# FTS extension, or does it need SQLite FTS5? Both get the same sample of
# Climate Policy Radar passages; measured per engine:
#   * build time and index size;
#   * query time (count of hits, and top 10 by BM25) for a rare word, a
#     common word, two words (AND), a phrase, and a prefix wildcard;
#   * the number of hits (the engines tokenise and stem a little differently);
#   * an update: 10 documents removed and added again (FTS5 incrementally;
#     DuckDB's FTS index can only be rebuilt).
#
# `level` "passage" indexes each passage; "document" joins the passages of a
# document (in reading order) into one text, as OpenAlex's work-level
# fulltext.search sees it: hits are then documents, and AND matches words
# anywhere in the document.
#
# Usage:  Rscript tools/fts_benchmark.R <n_passages> <work_dir> [passage|document] [cpr_dir]
# Appends to <work_dir>/results_<level>.csv. Needs duckdb, RSQLite (with FTS5).

args <- commandArgs(trailingOnly = TRUE)
n <- as.numeric(args[[1]])
work <- args[[2]]
level <- if (length(args) > 2) match.arg(args[[3]], c("passage", "document")) else "passage"
cpr <- if (length(args) > 3) args[[4]] else "/Volumes/Climate_Policy_Radar/cnp/raw/cpr"
dir.create(work, recursive = TRUE, showWarnings = FALSE)
tag <- format(n, scientific = FALSE, big.mark = "")
log <- function(...) message(format(Sys.time(), "%H:%M:%S"), " [", tag, "] ", ...)
results <- list()
rec <- function(engine, step, seconds, value = NA, note = NA) {
  results[[length(results) + 1L]] <<- data.frame(
    passages = n, level = level, engine = engine, step = step, seconds = round(seconds, 3),
    value = value, note = note)
  log(engine, " ", step, ": ", round(seconds, 2), " s", if (!is.na(value)) paste0(", ", value))
}
timed <- function(expr) {
  t <- system.time(v <- force(expr))[["elapsed"]]
  list(t = t, v = v)
}
size_of <- function(paths) sum(file.size(paths[file.exists(paths)])) / 2^20

# -- the sample: the first n passages with text (whole documents, file order) --
sample_pq <- file.path(work, paste0("sample_", tag, ".parquet"))
duck <- function(path = ":memory:") DBI::dbConnect(duckdb::duckdb(dbdir = path, shared_home = TRUE))
if (!file.exists(sample_pq)) {
  con <- duck()
  r <- timed(DBI::dbExecute(con, sprintf(
    "COPY (SELECT row_number() OVER () AS pid, document_id AS doc, \"text_block.text\" AS text
           FROM read_parquet('%s/*.parquet') WHERE \"text_block.text\" IS NOT NULL LIMIT %s)
     TO '%s' (FORMAT parquet)", cpr, tag, sample_pq)))
  DBI::dbDisconnect(con, shutdown = TRUE)
  rec("sample", "extract", r$t)
}
if (level == "document") {
  doc_pq <- file.path(work, paste0("sample_", tag, "_doc.parquet"))
  if (!file.exists(doc_pq)) {
    con <- duck()
    r <- timed(DBI::dbExecute(con, sprintf(
      "COPY (SELECT row_number() OVER (ORDER BY doc) AS pid, doc,
                    string_agg(text, ' ' ORDER BY pid) AS text
             FROM '%s' GROUP BY doc)
       TO '%s' (FORMAT parquet)", sample_pq, doc_pq)))
    DBI::dbDisconnect(con, shutdown = TRUE)
    rec("sample", "join passages per document", r$t)
  }
  sample_pq <- doc_pq
  tag <- paste0(tag, "_doc")
}
con <- duck()
info <- DBI::dbGetQuery(con, sprintf(
  "SELECT count(*) AS n, count(DISTINCT doc) AS docs, sum(length(text)) AS chars FROM '%s'", sample_pq))
DBI::dbDisconnect(con, shutdown = TRUE)
rec("sample", "rows indexed", 0, info$n, sprintf("%d documents, %.0f MB text", info$docs, info$chars / 2^20))
docs_update <- {
  con <- duck()
  # 10 documents of ordinary length (CPR documents average ~2,500 passages)
  d <- DBI::dbGetQuery(con, sprintf(
    "SELECT doc FROM (SELECT DISTINCT doc FROM '%s') USING SAMPLE 10 ROWS (reservoir, 1)",
    sample_pq))$doc
  DBI::dbDisconnect(con, shutdown = TRUE)
  d
}
in_docs <- paste0("('", paste(gsub("'", "''", docs_update), collapse = "','"), "')")

queries <- list(
  rare = list(words = "peatland", fts5 = "peatland"),
  common = list(words = "climate", fts5 = "climate"),
  two_words = list(words = "forest finance", fts5 = "forest AND finance"),
  phrase = list(words = "loss damage", phrase = "loss and damage", fts5 = "\"loss and damage\""),
  prefix = list(prefix = "adapt", fts5 = "adapt*")
)

# -- DuckDB FTS --------------------------------------------------------------------
db_path <- file.path(work, paste0("duck_", tag, ".duckdb"))
unlink(c(db_path, paste0(db_path, ".wal")))
con <- duck(db_path)
DBI::dbExecute(con, "INSTALL fts; LOAD fts")
r <- timed(DBI::dbExecute(con, sprintf("CREATE TABLE p AS SELECT * FROM '%s'", sample_pq)))
rec("duckdb", "load table", r$t)
build_duck <- function() {
  DBI::dbExecute(con, paste(
    "PRAGMA create_fts_index('p', 'pid', 'text', stemmer = 'porter',",
    "stopwords = 'english', overwrite = 1)"))
}
r <- timed(build_duck())
rec("duckdb", "build index", r$t)
DBI::dbExecute(con, "CHECKPOINT")
rec("duckdb", "size MB (table + index)", 0, round(size_of(db_path)))

duck_where <- function(q) {
  parts <- character(0)
  if (!is.null(q$words)) {
    parts <- c(parts, sprintf("fts_main_p.match_bm25(pid, '%s', conjunctive := 1) IS NOT NULL", q$words))
  }
  if (!is.null(q$phrase)) {
    parts <- c(parts, sprintf("regexp_matches(text, '(?i)\\b%s\\b')", gsub(" ", "\\\\W+", q$phrase)))
  }
  if (!is.null(q$prefix)) {
    parts <- c(parts, sprintf("regexp_matches(text, '(?i)\\b%s\\w*')", q$prefix))
  }
  paste(parts, collapse = " AND ")
}
for (name in names(queries)) {
  q <- queries[[name]]
  r <- timed(DBI::dbGetQuery(con, paste("SELECT count(*) AS n FROM p WHERE", duck_where(q)))$n)
  rec("duckdb", paste0("count: ", name), r$t, r$v)
  score <- if (!is.null(q$words)) {
    sprintf("fts_main_p.match_bm25(pid, '%s', conjunctive := 1)", q$words)
  } else {
    "0"
  }
  r <- timed(nrow(DBI::dbGetQuery(con, sprintf(
    "SELECT pid, %s AS s FROM p WHERE %s ORDER BY s DESC LIMIT 10", score, duck_where(q)))))
  rec("duckdb", paste0("top 10: ", name), r$t)
}
# update: no incremental index -- delete + insert, then rebuild the index
r <- timed({
  DBI::dbExecute(con, paste("DELETE FROM p WHERE doc IN", in_docs))
  DBI::dbExecute(con, sprintf("INSERT INTO p SELECT * FROM '%s' WHERE doc IN %s", sample_pq, in_docs))
  build_duck()
})
rec("duckdb", "update 10 docs (rebuild)", r$t)
DBI::dbDisconnect(con, shutdown = TRUE)

# -- SQLite FTS5 -------------------------------------------------------------------
sq_path <- file.path(work, paste0("fts5_", tag, ".sqlite"))
unlink(sq_path)
sq <- DBI::dbConnect(RSQLite::SQLite(), sq_path)
DBI::dbExecute(sq, "PRAGMA journal_mode = OFF")
DBI::dbExecute(sq, "PRAGMA synchronous = OFF")
DBI::dbExecute(sq, paste(
  "CREATE VIRTUAL TABLE f USING fts5(text, doc UNINDEXED, pid UNINDEXED,",
  "tokenize = 'porter unicode61')"))
src <- duck()
res <- DBI::dbSendQuery(src, sprintf("SELECT text, doc, pid FROM '%s'", sample_pq))
r <- timed({
  DBI::dbBegin(sq)
  while (!DBI::dbHasCompleted(res)) {
    chunk <- DBI::dbFetch(res, n = 200000)
    if (nrow(chunk)) DBI::dbAppendTable(sq, "f", chunk)
  }
  DBI::dbCommit(sq)
})
DBI::dbClearResult(res)
rec("fts5", "build index (insert)", r$t)
r <- timed(DBI::dbExecute(sq, "INSERT INTO f(f) VALUES ('optimize')"))
rec("fts5", "optimize", r$t)
rec("fts5", "size MB (text + index)", 0, round(size_of(sq_path)))

for (name in names(queries)) {
  q <- queries[[name]]
  m <- DBI::dbQuoteString(sq, q$fts5)
  r <- timed(DBI::dbGetQuery(sq, paste("SELECT count(*) AS n FROM f WHERE f MATCH", m))$n)
  rec("fts5", paste0("count: ", name), r$t, r$v)
  r <- timed(nrow(DBI::dbGetQuery(sq, paste(
    "SELECT pid, rank FROM f WHERE f MATCH", m, "ORDER BY rank LIMIT 10"))))
  rec("fts5", paste0("top 10: ", name), r$t)
}
r <- timed(DBI::dbGetQuery(sq, "SELECT count(*) AS n FROM f WHERE f MATCH 'NEAR(forest finance, 5)'")$n)
rec("fts5", "count: near (5 words)", r$t, r$v)
# update: incremental delete + insert
upd <- DBI::dbGetQuery(src, sprintf("SELECT text, doc, pid FROM '%s' WHERE doc IN %s", sample_pq, in_docs))
r <- timed({
  DBI::dbBegin(sq)
  DBI::dbExecute(sq, paste("DELETE FROM f WHERE doc IN", in_docs))
  DBI::dbAppendTable(sq, "f", upd)
  DBI::dbCommit(sq)
})
rec("fts5", "update 10 docs (incremental)", r$t, nrow(upd))
DBI::dbDisconnect(sq)
DBI::dbDisconnect(src, shutdown = TRUE)

out <- do.call(rbind, results)
csv <- file.path(work, paste0("results_", level, ".csv"))
utils::write.table(out, csv, sep = ",", row.names = FALSE, append = file.exists(csv),
                   col.names = !file.exists(csv))
print(out, row.names = FALSE)
