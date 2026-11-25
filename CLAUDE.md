# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Package Does

**openalexAPI** serves a local Parquet corpus of OpenAlex works as a web API that answers like
the OpenAlex API, so the same classic URLs, OQL and OQO queries (with `group_by`, `sort`,
paging) run against local data, and **openalexPro's download functions work against it
unchanged** (`pro_query(endpoint = "http://127.0.0.1:8000")`). A static web page is included.
Works only; project-sized corpora, not the whole snapshot.

Exported: `prepare_corpus()`, `serve_corpus()`, `oqo_to_sql()`.

## Request flow

```
GET /works?filter=&search=&group_by=   --> POST {root}/query {oxurl}  (OpenAlex, free) --+
GET /?oql=  /  POST / {oql}            --> POST {root}/query {oql}                      --+--> OQO
GET /?oqo=  /  POST / {oqo}            ------------------------------------ (offline) ---+
POST /query                            --> forwarded to OpenAlex unchanged (openalexPro via = "oqo")
OQO --> oqo_to_sql() (registry) --> .api_execute() / .api_execute_groups() --> JSON built by DuckDB
```

| File | Role |
|---|---|
| `R/corpus.R` | `prepare_corpus()`: cached DuckDB db per corpus state; `works` table + `oaapi_*` helper columns, FTS tables |
| `R/registry_works.R` | filter registry (`column_id` -> SQL) and `.api_groups_works()` (group keys) |
| `R/search.R` | search columns (`<field>.search[.exact]`): BM25, phrases, wildcards |
| `R/oqo_to_sql.R` | tree walk: AND/OR/negation, NULL-safe leaves, sort, group spec, relevance score |
| `R/execute.R` | records and grouped responses, paging, sample, single works |
| `R/translate.R` | OpenAlex `/query` calls (httr2), cache |
| `R/routes.R` | plumber2 app, `serve_corpus()`, the page (`/app`, `/assets/`) |
| `inst/www/` | page: `index.html` (template, `{{v}}` = asset stamp), `query.js` (pure, tested), `app.js` (DOM) |
| `inst/extdata/oqo_translations.*` | recorded `/query` survey (see `tools/`) |

## Key design decisions (keep these)

- **Translation is OpenAlex's**, via its free `POST /query`; there is no local OQL/URL parser.
  Called with httr2 directly, **not** `openalexPro::pro_translate()`: the forwarded `POST /query`
  must return OpenAlex's status and body unchanged. OpenAlex validation errors are passed
  through as they are (`.api_abort(body = ...)`).
- **Registry keys come from the survey, not the docs.** `tools/spike_translate.R` records which
  OQO `column_id` OpenAlex emits for each classic filter (`tools/spike_findings.md`): e.g.
  `search=` -> `fulltext.search`, `cites:` -> `referenced_works`, `title.search` ->
  `display_name.search`, `from_publication_date` stays its own column, an equality leaf has no
  `operator` key, ranges are two leaves, `null` values mean "field missing". Re-run it when
  OpenAlex changes; a new or renamed `column_id` is a 400 here until the registry has it.
- **Group key formats were surveyed live** (all groupable columns): IDs as stored, slugs under
  `https://openalex.org/<entity>/`, plain years/counts/booleans/`oa_status`/institution types.
  Groups use an identity `g` (`id_key()` for IDs). `:include_unknown` is refused by OpenAlex's
  translator, so `GET /works` strips it and passes it as a view flag.
- **Response JSON is built by DuckDB** (`to_json(struct_pack(...))`) and pasted into the
  envelope; never round-trip records through jsonlite (empty arrays, nulls, int64, nesting).
  Group rows are small and go through `.api_json(rows, na = "null")`.
- **`/` stays JSON-only for queries**; without `oql`/`oqo` it redirects to `/app`. plumber2
  picks the serializer by `Accept`, and httr2 (openalexPro) sends none, so HTML and JSON on one
  route would mislabel JSON and openalexPro would reject it.
- **IDs match in any form**: `id_key()` / `doi_key()` SQL macros (R twins `.api_id_key()`,
  `.api_doi_key()`), applied to both sides; no assumption about the letter prefix (`wZ…` IDs).
- **The corpus**: all `*.parquet` below the folder, works files only (`id` + `title`/
  `display_name`), `union_by_name`, no hive columns, deduplicated on `id_key(id)`. Extra
  columns are served as they are. Not served: `.api_not_served` (`page`, `citation`,
  `abstract`, `relevance_score`) and `oaapi_*`. `abstract_inverted_index` is rebuilt for
  JSONL-path corpora (round-trips exactly). Filter columns whose type differs between files are
  cast back (`.api_column_types`). The snapshot's `abstract_inverted_index` is a JSON string: cast to
  `MAP(VARCHAR, BIGINT[])` via JSON; records with repeated keys (~0.6 %) fail that cast and are
  merged by `.api_merge_duplicate_keys()` (`json_each`).
- **`.api_build_version`** is part of the cache key: **bump it whenever `.api_build()` (or
  anything it creates) changes**, or users keep a stale cache.
- **Search**: two FTS tables (`oaapi_docs_stem` Porter + English stop words, `oaapi_docs_exact`
  none), conjunctive BM25 (OpenAlex ANDs words, checked live), phrases and wildcards as regex
  on `oaapi_title`/`oaapi_abstract`; `relevance_score` = sum over non-negated search leaves.
- **Search terms are computed once per request** (`oqo_to_sql()$prelude`, run by
  `.api_run_prelude()`): each BM25 query into a temp table `oaapi_s<n>(i, s)`, the leaf is
  `oaapi_id IN (SELECT i FROM oaapi_s<n>)`, the score one lookup in `oaapi_score`. Never put
  `match_bm25()` per work into the WHERE or ORDER BY: on 11,000 works a 20-term OR took 21 s
  that way (now 0.3 s) and OpenAlex's 114-term example did not finish (now 1.7 s). Anything
  that runs an `oqo_to_sql()` result must run its prelude first.
- **Samples** must be identical on every page: always seeded (from the query if none), input
  ordered by `oaapi_id`, `SET threads = 1` for the query.
- **Grouped requests** run the filter once into temp tables (`oaapi_m`, `oaapi_g`); fine on the
  read-only connection, requests are handled one at a time.
- **Errors**: `.api_abort(message, status, body)`; DuckDB binder errors (a struct field the
  corpus lacks) become a 400 in `.api_get()`.

## DuckDB notes

- Connections via `.api_con()` (`R/utils_duckdb.R`), which loads `json` and `fts` explicitly:
  `autoinstall_known_extensions` is FALSE, so autoloading fails on fresh CI runners.
- Lambdas use the new syntax `lambda x: …`; the arrow form `x -> …` is an error in duckdb 2.0.
- `SELECT * EXCLUDE (...) REPLACE (...)`: EXCLUDE comes first.

## Speed

`tools/benchmark.R` downloads an ~11,000-work corpus once and times every feature (filters,
paging, sample, every `group_by`, all search forms, the 114-term example); run it after
changes to the compiler or execution. On 11,000 works everything is under 0.35 s except the
114-term search (1.7 s); building the database takes ~2 s. Scaling on snapshot partitions
(10k to 9.6M works, `tools/benchmark_scaling.csv`, figure `tools/plot_scaling.R` ->
`man/figures/scaling.svg`, shown in the README): queries stay flat while the database fits in
RAM (~7 GB per million works) and slow down sharply beyond. The build must not keep whole rows
in memory: the dedup window function runs only when ids repeat, `preserve_insertion_order = false`.

## Docker

`docker/` (Dockerfile, `serve.R` entrypoint with a `prepare` argument, `compose.yaml` with
Caddy pulling the published image, `compose.build.yaml` to build from the checkout instead).
The image installs the package from the checkout; the corpus is mounted at `/corpus`, the
database lives in a `/cache` volume. Build from the package root:
`docker build -f docker/Dockerfile -t openalexapi .` (colima works).
`.github/workflows/docker.yaml` publishes `ghcr.io/openalexpro/openalexapi` on each release
(and manual runs: branch tag), amd64 and arm64 on native runners, merged by digest. The
vignette points at `main`'s `docker/compose.yaml` and `Caddyfile`, so keep those working.

## Common Commands

```r
devtools::load_all()
devtools::document()
devtools::test()          # NOT_CRAN=true for the server tests
devtools::check(vignettes = FALSE)
serve_corpus("tests/testthat/fixtures/corpus_jq", port = 8790)   # try the page
```

`devtools::build_readme()` fails (it installs the package and cannot resolve openalexPro from
CRAN); README chunks are not evaluated, so render with `rmarkdown::render("README.Rmd")` and
delete `README.html`.

## Tests

- Fixtures (`tools/make_fixtures.R`): `corpus_jq` (369 works, JSONL shape: `abstract`, no
  inverted index; from openalexConvert) and `corpus_api` (2 works, API shape; from openalexPro).
  Variants with mixed ID forms, missing names or mixed types are built inside the tests.
- `test-server.R` starts `serve_corpus()` in a `callr` process (`local_server()` in `helper.R`).
  Tests that need OpenAlex's `/query` (classic URLs, OQL, openalexPro round trips) use
  `skip_if_offline("api.openalex.org")`; OQO tests run offline.
- Counts are always checked against hand-written SQL on the same fixture.
- openalexPro round trips (`pro_fetch(pro_query(..., endpoint = local))`, `via = "url"` and
  `"oqo"`, multi-page, grouped, search) are the compatibility contract: keep them passing.
- `test-page.R` runs `inst/www/query.js` with V8; keep page logic that can be pure in
  `query.js`.

## Branching

- `claude/<description>` branches from `dev`; merge into `dev`; `main` gets release commits only.
- Public since 0.1.0 (alpha); archived on Zenodo, concept DOI 10.5281/zenodo.24514237 (cite that one). Not on the openalexPro r-universe (it is not in the r-universe
  registry; add it there only when it leaves alpha); installed from GitHub. Listed on the org
  landing page (openalexpro.github.io) and org profile (dot-github) as Alpha.
- Stacked PRs, merge in order: #1 filters, #2 group_by, #3 search, #4 page, #5 docs,
  #6 performance, #7 more filters (lineage, funders, affiliation/author search, proximity, ...).

## Coverage and parity

- `tools/oql_coverage.R` runs OpenAlex's OQL example corpus (156 of 174 runnable works queries
  run; the rest need collections, semantic search, multi-dimension `group_by`, comparisons or
  nested `in (...)` queries). Run it after adding registry columns.
- `tools/live_parity.R` compares counts and groups with OpenAlex on a downloaded corpus; filter
  and group checks must be equal, full-text searches differ (OpenAlex also searches full
  texts).
- Affiliation and author-name searches (`.api_list_search_entry()`) are regex over the lists,
  not FTS: words may come from different strings, a phrase must be within one (OpenAlex's rule
  for these fields). Proximity is `.api_prox_re()`: up to N words between terms.
