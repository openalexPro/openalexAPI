# Serve a corpus as an OpenAlex-compatible API

Prepares the corpus (see
[`prepare_corpus()`](https://openalexpro.github.io/openalexAPI/reference/prepare_corpus.md))
and serves it like the OpenAlex API, so
`openalexPro::pro_query(endpoint = "http://127.0.0.1:8000")` and the
download functions of openalexPro work against it unchanged:

## Usage

``` r
serve_corpus(
  corpus,
  port = 8000L,
  host = "127.0.0.1",
  api_key = NULL,
  root = "https://api.openalex.org",
  block = TRUE,
  ...
)
```

## Arguments

- corpus:

  Directory with the corpus Parquet files.

- port, host:

  Where to serve. The default host is only reachable from this computer;
  another host makes the corpus (and translation with `api_key`)
  available to the network.

- api_key:

  OpenAlex API key for translation requests that bring none. Not needed:
  translation is free.

- root:

  The OpenAlex API used for translation.

- block:

  Block the R session while serving.

- ...:

  Passed to
  [`prepare_corpus()`](https://openalexpro.github.io/openalexAPI/reference/prepare_corpus.md).

## Value

The plumber2 api object, invisibly.

## Details

- `GET /works?filter=...&sort=...&per_page=...&cursor=...`, also with
  `group_by=` (one dimension; `:include_unknown` shows works without a
  value as the group `unknown`)

- `GET /works/{id}` (any ID form, or a DOI)

- `GET /?oql=...` and `GET /?oqo=...`, `POST /` with `oqo` or `oql` and
  the view parameters `select`, `sort`, `per_page`, `page`, `cursor`

- `POST /query`: forwarded to OpenAlex (translation)

- `GET /corpus`: what is served (filters, group_by columns)

- `/`: a web page for running queries in the browser (`/app`)

Classic URLs and OQL are translated to OQO by OpenAlex's free `/query`
endpoint (the network is needed, no credits are spent); an OQO runs
fully offline. Requests are handled one at a time.
