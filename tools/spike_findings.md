# `/query` translation survey — findings (2026-11-24)

Source: `tools/spike_translate.R` →
- `inst/extdata/oqo_translations.csv`: one row per OQO node
- `inst/extdata/oqo_translations.jsonl`: full responses

These findings define the registry keys and operator handling in `R/registry_works.R`.

## Operators and values

- **`is` is implicit.** A plain equality leaf has **no `operator` key**. Treat a missing operator as `is`.
- **Ranges are split.**
  - `publication_year:2018-2020` becomes two leaves: `>= 2018` and `<= 2020`.
  - `>`/`<` arrive as operators.
- **Typed values.**
  - Booleans arrive as JSON `true`/`false`.
  - Years and counts arrive as numbers.
  - Country codes are **upper-cased** (`ch` → `CH`).
  - IDs are bare (`A5023888391`, `W2741809807`).
- **OR is lifted into the tree.** `type:article|book` becomes `{join: "or", filters: [type=article, type=book]}`.
- **Negation is per leaf.** `type:!article` gives `is_negated: true`.
- **`filter_rows` come back canonically sorted**: `publication_year` before `type`.
- **`sort=` is part of the OQO:** `sort_by: [{column_id, direction}]`, as in spec v1.6.
- **`group_by`:** `group_by: [{column_id}]`, using the same `column_id`s as filters.

## Column ids (input → OQO `column_id`)

| URL filter | OQO `column_id` |
|---|---|
| `is_oa`, `open_access.is_oa` | `open_access.is_oa` |
| `oa_status` | `open_access.oa_status` |
| `author.id` | `authorships.author.id` |
| `institutions.id` | `authorships.institutions.id` |
| `institutions.country_code` | `authorships.institutions.country_code` (separate from `authorships.countries`) |
| `openalex` | `ids.openalex` |
| `from_publication_date` / `to_publication_date` | **kept as their own column ids**, not `publication_date >=` |
| `cites`, `referenced_works` | `referenced_works` |
| `cited_by` | `cited_by` |
| `title.search` | `display_name.search` |
| `abstract.search` | `abstract.search` |
| `default.search` | **`fulltext.search`** |
| `search=` | **`fulltext.search`** |
| `search.exact=` | **`fulltext.search.exact`** |

All other surveyed filters keep their URL name unchanged: `type`, `language`, `best_oa_location.license`, `is_retracted`, `is_paratext`, `has_doi`, `has_abstract`, `has_references`, `cited_by_count`, `fwci`, `authors_count`, `authorships.countries`, `authorships.institutions.type`, `primary_location.source.id`, `primary_location.source.type`, `primary_topic.id`, `primary_topic.field.id`, `primary_topic.domain.id`, `topics.id`, `keywords.id`, `sustainable_development_goals.id`, `doi`.

## Search

- **Boolean `search=` strings are lifted into the tree.**
  - `a AND (b OR c)` becomes AND of `has a` with an OR branch.
  - `a NOT b` becomes a negated `has b` leaf.
  - Leaf values hold only terms, quoted phrases, wildcards (exact only) and proximity (`"a b"~3`).
- **Locally there is no fulltext.** The `fulltext.search` and `fulltext.search.exact` columns are served from title + abstract, and this must be documented.
- **Proximity translates fine on OpenAlex**, so the local 400 for proximity is our own limitation.

## Open points

- **`primary_topic.subfield.id:1201`** gave 400: "not a valid subfields code". Find the accepted value form (perhaps `subfields/1201` or a full URL) before supporting it.
- **`topics.id`** (any topic, not only primary) and **`abstract.search`** exist. Add them to the v0.1 registry, since they're cheap.
