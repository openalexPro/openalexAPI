# Builds the test corpora in tests/testthat/fixtures/. Run from the package root.
#
# corpus_jq/  369 works from openalexConvert's test corpus: the shape the
#             JSONL path writes (`abstract` text, no `abstract_inverted_index`).
# corpus_api/ 2 works from openalexPro's keypaper JSON fixture, converted with
#             pro_request_parquet(): the API shape (`abstract_inverted_index`
#             kept, `abstract`, `citation`, `page` added).

eco <- normalizePath("..")
out <- "tests/testthat/fixtures"
unlink(file.path(out, c("corpus_jq", "corpus_api")), recursive = TRUE)

dir.create(file.path(out, "corpus_jq"), recursive = TRUE)
file.copy(
  list.files(file.path(eco, "openalexConvert/tests/fixtures/corpus"), full.names = TRUE),
  file.path(out, "corpus_jq"), recursive = TRUE
)

openalexPro::pro_request_parquet(
  input_json = file.path(eco, "openalexPro/tests/fixtures/keypaper_json"),
  output = file.path(out, "corpus_api"),
  verbose = FALSE
)
