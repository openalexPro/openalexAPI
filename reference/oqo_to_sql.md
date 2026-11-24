# Compile an OQO into DuckDB SQL

Walks the filter tree of an OQO (OpenAlex Query Object): the top-level
`filter_rows` are joined with AND, a branch with its `join` (`and` /
`or`), and `is_negated` negates a leaf or branch. Each leaf goes through
the works column registry. A leaf whose value is missing never matches,
and its negation does, as with `!` in OpenAlex; a `null` leaf value
matches works where the field is missing. The SQL refers to the `works`
table built by
[`prepare_corpus()`](https://openalexpro.github.io/openalexAPI/reference/prepare_corpus.md).

## Usage

``` r
oqo_to_sql(oqo, con, columns = NULL)
```

## Arguments

- oqo:

  An OQO as a list (e.g. from
  `jsonlite::fromJSON(x, simplifyVector = FALSE)`).

- con:

  A DuckDB connection (used to quote values).

- columns:

  The corpus columns; registry columns that need others are refused.
  `NULL` skips the check.

## Value

A list with `where` (SQL condition, `"true"` without filters), `order`
(SQL `ORDER BY` terms from `sort_by`, or `NULL`; with `group_by` terms
on `n` (count), `k` (key) and `u` (1 for the unknown group)) and `group`
(`NULL`, or the `group_by` dimension: `column`, `pairs` (SQL giving each
work a list of `{g, k, d}` identity / key / display-name structs) and
`include_unknown`), `score` (the BM25 relevance of the search leaves
that are not negated, or `NULL` without a search), and `prelude` (SQL
statements to run first: each search term's matches and scores as a temp
table, computed once) with `temp` (their names, to drop afterwards). Run
with `.api_run_prelude()`.
