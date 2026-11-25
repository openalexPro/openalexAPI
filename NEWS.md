# openalexAPI 0.1.2

- Snapshot corpora (openalexSnapshot, OpenAlex's Parquet snapshot) work: their
  `abstract_inverted_index` is a JSON string, now read as a map. Records whose
  index repeats a word (about 0.6 % in the snapshot) keep their abstract, with
  the positions merged. Cached databases are rebuilt once.
- Building the database needs less memory: works are deduplicated only when
  the corpus has duplicates, and rows are not kept in order. A 9.6 million
  work snapshot partition now builds on a 36 GB machine (13 minutes).
- The cache key uses file times in whole seconds, so a database built inside
  a Docker image is reused when the container starts.
- Docker: each release is published as `ghcr.io/openalexpro/openalexapi`
  (amd64 and arm64). `docker/compose.yaml` runs it behind Caddy (HTTPS), so a
  server needs only Docker, two configuration files and the Parquet files;
  see the vignette. `docker/Dockerfile` builds the image from a checkout.
- README: query times, database size, build time and memory for 10,000 to
  9.6 million works, with recommendations.
- Tests: the online tests skip when OpenAlex's `/query` fails with a 5xx
  (macOS CI runs failed on OpenAlex's 504s); the server's own errors still fail.

# openalexAPI 0.1.1

- Zenodo DOI (10.5281/zenodo.24514237, all versions) in `DESCRIPTION`, `CITATION`,
  `CITATION.cff` and the README.
- Tests: the server tests also run from the installed package (covr), and no
  test leaves a database connection open.

# openalexAPI 0.1.0

First version (alpha): not on r-universe, installed from GitHub.

- 15 more filters, from OpenAlex's own OQL examples (`tools/oql_coverage.R`):
  `authorships.institutions.lineage`, `authorships.author.orcid`,
  `funders.id`, `publication_date`, `from_created_date` / `to_created_date`,
  `citation_normalized_percentile.value` / `.is_in_top_1_percent` /
  `.is_in_top_10_percent`, `open_access.any_repository_has_fulltext`,
  `ids.pmid`, `ids.pmcid`, `primary_location.source.issn`,
  `primary_location.source.publisher_lineage` (`host_organization_lineage`),
  `primary_topic.subfield.id`; `group_by` on those OpenAlex can group by.
- Search in affiliation strings and author names
  (`raw_affiliation_strings.search[.exact]`, `raw_author_name.search[.exact]`),
  and proximity search (`"a"~N~"b"`, chains with a distance per pair,
  `"a b"~N`; N up to 1000). Phrases and proximity in the stemmed search
  columns match word stems (`"coral reefs"` also finds "coral reef").
- `group_by` on publisher lineage shows the publishers' names.
- Coverage: 156 of OpenAlex's 174 OQL example queries run locally (was 115);
  the rest need collections, semantic search, multi-dimension `group_by`,
  comparisons or nested queries. Live comparison with OpenAlex
  (`tools/live_parity.R`): all 47 filter and `group_by` checks equal, the 5
  full-text searches differ as expected (OpenAlex also searches full texts).
- Speed: search terms are computed once per request (temp tables) instead of
  per work. On 11,000 works a 20-term OR search went from 21 s to 0.3 s and
  OpenAlex's 114-term systematic-review example from not finishing to 1.7 s;
  all other features take under 0.35 s (`tools/benchmark.R`). A
  wildcard-only search sorted by relevance no longer fails.
- `group_by` matches OpenAlex in two more details, found by a live comparison
  (`tools/live_parity.R`: 29 of 33 checks equal, the 4 search checks differ as
  expected): slug keys are URL-encoded (`source-types/ebook%20platform`), and
  true/false columns list both values, also with count 0.
- Documentation: README, `vignette("openalexAPI")`, pkgdown site config,
  `CITATION` / `CITATION.cff` / `.zenodo.json`.
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
- Columns whose type differs between Parquet files (e.g. a number in one
  download, text in another) are reported when the corpus is prepared; the
  columns filters compare as numbers, dates or booleans (`publication_year`,
  `cited_by_count`, `fwci`, `publication_date`, `is_retracted`,
  `is_paratext`) are converted back, with a count of unreadable values. Other
  columns are served in the common type DuckDB reads them as. Extra,
  non-OpenAlex columns are served as they are (not filterable).
- Parquet files that hold no works (e.g. snowball edges, collection
  `members/`) are skipped; a sample spanning pages is the same on every page;
  `GET /works/{id}` also takes a DOI and `select`.
- New package. Skeleton, DuckDB connection helper, and a recorded survey of
  how OpenAlex's `/query` endpoint translates classic filters and searches
  into OQO (`tools/spike_translate.R`, `inst/extdata/oqo_translations.csv`),
  which defines the column registry.
