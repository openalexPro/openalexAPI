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
- Full-text search over title and abstract (the corpus has no full text):
  `search` / `search.exact` and the `<field>.search[.exact]` filters for
  `fulltext`, `title_and_abstract`, `display_name` and `abstract`. All words
  must match (as in OpenAlex), quoted phrases in order, wildcards with
  `.exact`; ranked by BM25 as `relevance_score`, the default sort when
  searching. The stemmer (Porter) folds more word forms than OpenAlex's, so
  stemmed counts can be higher; proximity (`"a b"~N`) is not supported.
- `group_by` (one dimension): works counted once per key, keys and response
  shape as OpenAlex's (`group_by` with `key`, `key_display_name`, `count`;
  no `results`), sorted by count or key, paged; `:include_unknown` adds the
  works without a value. Countries, languages and licenses show the code as
  display name (OpenAlex shows the full name).
- A web page at `/` (`/app`, served from `inst/www`, no build step): search and
  filters or OQL, `group_by` as bars (a click narrows the filters to that
  group), records with authors and relevance, paging, the translated OQL, and
  "Copy API URL" for `openalexPro::pro_fetch()`. Light and dark, phone width.
  The page's files are linked with a stamp of their content, so a browser
  loads new ones after an upgrade.
- Parquet files that hold no works (e.g. snowball edges, collection
  `members/`) are skipped; a sample spanning pages is the same on every page;
  `GET /works/{id}` also takes a DOI and `select`.
- New package. Skeleton, DuckDB connection helper, and a recorded survey of
  how OpenAlex's `/query` endpoint translates classic filters and searches
  into OQO (`tools/spike_translate.R`, `inst/extdata/oqo_translations.csv`),
  which defines the column registry.
