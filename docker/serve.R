# Starts the server in the container; settings come from the environment.
key <- Sys.getenv("OPENALEX_API_KEY")
corpus <- Sys.getenv("OAAPI_CORPUS")
cache <- Sys.getenv("OAAPI_CACHE")
if (identical(commandArgs(TRUE), "prepare")) {
  # build the database only (docker compose run --rm openalexapi prepare)
  openalexAPI::prepare_corpus(corpus, cache_dir = cache)
} else {
  # all interfaces inside the container: publish the port to 127.0.0.1 only
  suppressWarnings(openalexAPI::serve_corpus(
    corpus, host = "0.0.0.0", port = as.integer(Sys.getenv("OAAPI_PORT")),
    cache_dir = cache, api_key = if (nzchar(key)) key
  ))
}
