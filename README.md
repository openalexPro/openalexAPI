
<!-- README.md is generated from README.Rmd. Please edit that file -->

# openalexAPI

**openalexAPI** serves a local Parquet corpus of OpenAlex works through
a web API that answers like the [OpenAlex
API](https://api.openalex.org). You can run the same queries against
your own data that you run against OpenAlex: classic `filter=` /
`search=` URLs, OQL (OpenAlex Query Language), OQO (OpenAlex Query
Object), `group_by`, `sort` and paging. It also includes a small web
page for running queries in the browser.

Because the answers look like OpenAlex’s, the download functions of
[openalexPro](https://github.com/openalexPro/openalexPro) work against
the local server unchanged. Point `pro_query()` at it with `endpoint =`.

The corpus can come from:

- `openalexPro::pro_request_parquet()` / `pro_fetch()` downloads
- an extraction from the OpenAlex snapshot (openalexSnapshot)
- any other source with the OpenAlex works schema

It is meant for **project-sized corpora**, not the whole snapshot.

## Installation

The repository is private for now, so install from GitHub with a token
that can read it:

``` r
# install.packages("pak")
pak::pak("openalexPro/openalexAPI")
```

## Quick start

``` r
library(openalexAPI)

# A corpus downloaded with openalexPro, or any folder of works Parquet files
serve_corpus("my_project/parquet", port = 8000)
```

Then open <http://127.0.0.1:8000/> for the web page, or query the API:

    http://127.0.0.1:8000/works?filter=type:article,publication_year:>2015&search=coral bleaching
    http://127.0.0.1:8000/works?filter=type:article&group_by=authorships.countries
    http://127.0.0.1:8000/?oql=works where type is article and year >= 2015
    http://127.0.0.1:8000/works/W2741809807

From another R session, download a subset of the corpus with
openalexPro, exactly as from OpenAlex:

``` r
library(openalexPro)
q <- pro_query(
  entity = "works", type = "article", search = "coral bleaching",
  endpoint = "http://127.0.0.1:8000"
)
pro_fetch(q, project_folder = "subset")
```

## How it works

    classic URL or OQL --> OpenAlex /query (free) --> OQO --> DuckDB SQL --> OpenAlex-shaped JSON
    OQO                -----------------------------^  (offline)

`prepare_corpus()` (run by `serve_corpus()`) reads the Parquet files
into a cached DuckDB database. The cache holds one row per work, IDs and
DOIs normalised so they match in any form, and full-text indexes over
title and abstract. OpenAlex’s free `/query` endpoint translates classic
URLs and OQL into an OQO, so the query languages are exactly OpenAlex’s.
`oqo_to_sql()` compiles the OQO into SQL. An OQO sent directly runs
without network access.

## What is supported

- **48 works filters:** type, year and dates (also the creation date),
  language, open access, citations and percentiles, authors and ORCIDs,
  institutions (also with their lineage), countries, funders, sources,
  ISSNs, publishers, topics down to subfields, keywords, SDGs, IDs
  (OpenAlex, DOI, PMID, PMCID), `cites` and `cited_by`. AND, OR (`a|b`),
  negation (`!a`), ranges, and `null` for “field missing” all work.
- **`group_by`** on one dimension, with OpenAlex’s key formats.
  `:include_unknown` is supported.
- **Full-text search** over title and abstract, with `search`,
  `search.exact` and the `<field>.search` filters, plus searches in
  affiliation strings and author names. All words must match, quoted
  phrases must appear in order, proximity (`"a"~N~"b"`, `"a b"~N`)
  works, and wildcards work with `.exact`. Results are ranked by
  `relevance_score` (BM25).
- **Records** are returned as stored, including fields that are not
  OpenAlex’s. You can use `select`, `sort`, `per_page`, `page`, `cursor`
  and `sample`/`seed`, and look up single works by ID or DOI.

## Differences from OpenAlex

- **Local data:** counts come from the corpus, not from all of OpenAlex.
- **Search** covers title and abstract only, because there is no full
  text. The stemmer folds more word forms than OpenAlex’s, so stemmed
  counts can be higher. Proximity allows up to N words between the
  terms, an approximation of OpenAlex’s.
- **Display names:** countries, languages and licenses use the code as
  the display name in `group_by` results (“AU” rather than “Australia”).
- **Unsupported features** get a clear HTTP 400 that names them:
  OpenAlex collections, semantic search, `group_by` on several
  dimensions, and OQL’s comparisons and nested queries. 156 of
  OpenAlex’s 174 OQL example queries run locally
  (`tools/oql_coverage.R`).

See `vignette("openalexAPI")` for the details.
