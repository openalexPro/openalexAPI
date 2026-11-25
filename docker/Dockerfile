# openalexAPI server image. Build from the package root:
#   docker build -f docker/Dockerfile -t openalexapi .
# The corpus is not part of the image: mount it at /corpus (read-only) and a
# volume at /cache for the database (see docker/compose.yaml).
ARG R_VERSION=4.5
FROM rocker/r-ver:${R_VERSION}

# openalexPro comes from its r-universe; CRAN packages are binaries (rocker)
RUN R -q -e 'options(repos = c(openalexpro = "https://openalexpro.r-universe.dev", getOption("repos"))); \
             install.packages("pak"); \
             pak::pkg_install(c("DBI", "duckdb", "httr2", "jsonlite", "openalexPro", "plumber2"))'

COPY . /src
RUN R -q -e 'options(repos = c(openalexpro = "https://openalexpro.r-universe.dev", getOption("repos"))); \
             pak::local_install("/src", dependencies = NA, upgrade = FALSE)' \
 && rm -rf /src

# DuckDB's json and fts extensions go into ~/.duckdb at build time, so the
# container does not download them at every start
RUN useradd --create-home oaapi && mkdir -p /corpus /cache && chown oaapi /cache
USER oaapi
RUN mkdir ~/.duckdb && R -q -e 'DBI::dbDisconnect(openalexAPI:::.api_con(":memory:"), shutdown = TRUE)'

COPY docker/serve.R /usr/local/bin/oaapi-serve.R
ENV OAAPI_CORPUS=/corpus OAAPI_CACHE=/cache OAAPI_PORT=8000
EXPOSE 8000
ENTRYPOINT ["Rscript", "/usr/local/bin/oaapi-serve.R"]
