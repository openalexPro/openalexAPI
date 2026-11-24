# openalexAPI: Local OpenAlex-Compatible Web API over a Parquet Corpus

Serves a local Parquet corpus of OpenAlex works through a web API that
answers like the OpenAlex '/works' endpoint, so the same classic filter
URLs, OQL (OpenAlex Query Language) and OQO (OpenAlex Query Object)
queries, including 'group_by', can be run against local data. Queries
are translated to OQO by the free OpenAlex '/query' endpoint and
compiled locally to DuckDB SQL. A small static web page is included for
running searches in the browser.

## See also

Useful links:

- <https://github.com/openalexPro/openalexAPI>

- <https://openalexpro.github.io/openalexAPI/>

- Report bugs at <https://github.com/openalexPro/openalexAPI/issues>

## Author

**Maintainer**: Rainer M Krug <Rainer@krugs.de>
([ORCID](https://orcid.org/0000-0002-7490-0066))

Authors:

- Rainer M Krug <Rainer@krugs.de>
  ([ORCID](https://orcid.org/0000-0002-7490-0066))
