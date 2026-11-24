# openalexAPI (development version)

- `prepare_corpus()` reads a Parquet corpus into a cached DuckDB database:
  one row per work, IDs and DOIs matched in any form, the abstract as text,
  and `abstract_inverted_index` rebuilt for corpora from the JSONL path.
- `serve_corpus()` serves it like the OpenAlex API (`GET /works`,
  `GET /works/{id}`, `GET /?oql=` / `?oqo=`, `POST /`, `POST /query`,
  `GET /corpus`), so openalexPro downloads from it unchanged. Classic URLs and
  OQL are translated by OpenAlex's free `/query`; an OQO runs offline.
- `oqo_to_sql()` compiles an OQO to DuckDB SQL: AND / OR / negation, sort,
  `null` values (field missing), and 33 of the most-used works filters.
  Search and `group_by` come next.
- Parquet files that hold no works (e.g. snowball edges, collection
  `members/`) are skipped; a sample spanning pages is the same on every page;
  `GET /works/{id}` also takes a DOI and `select`.
- New package. Skeleton, DuckDB connection helper, and a recorded survey of
  how OpenAlex's `/query` endpoint translates classic filters and searches
  into OQO (`tools/spike_translate.R`, `inst/extdata/oqo_translations.csv`),
  which defines the column registry.
