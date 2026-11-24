# Prepare a Parquet corpus for serving

Reads all Parquet files below `corpus` into one DuckDB database file:
one row per work (duplicates across partitions removed), with helper
columns for ID and DOI matching and the abstract as text. The database
is cached and reused while the corpus files are unchanged (same paths,
sizes and modification times).

## Usage

``` r
prepare_corpus(
  corpus,
  rebuild = FALSE,
  cache_dir = tools::R_user_dir("openalexAPI", "cache")
)
```

## Arguments

- corpus:

  Directory with the corpus Parquet files (searched recursively).

- rebuild:

  Rebuild the database even if a cached one exists.

- cache_dir:

  Where the database files are kept.

## Value

The path of the DuckDB database file, invisibly.

## Details

The corpus can come from
[`openalexPro::pro_request_parquet()`](https://openalexpro.github.io/openalexPro/reference/pro_request_parquet.html)
(keeps `abstract_inverted_index` and adds `abstract`), from the JSONL
path (only `abstract`), from a snapshot extraction, or from any source
with the OpenAlex works schema. Hive partition columns are not read.
