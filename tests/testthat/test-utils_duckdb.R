test_that("id_key normalises every id form, in SQL and in R, identically", {
  ids <- c(
    "https://openalex.org/W123", "W123", "w123",
    "https://openalex.org/authors/A5023888391", "ABC42", "wZ7"
  )
  want <- c("W123", "W123", "W123", "A5023888391", "ABC42", "WZ7")
  expect_identical(openalexAPI:::.api_id_key(ids), want)

  con <- openalexAPI:::.api_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "t", data.frame(x = ids))
  got <- DBI::dbGetQuery(con, "SELECT id_key(x) AS k FROM t")$k
  expect_identical(got, want)
})

test_that("doi_key normalises DOI forms, in SQL and in R, identically", {
  dois <- c(
    "https://doi.org/10.1/ABC", "http://dx.doi.org/10.1/abc",
    "doi:10.1/Abc", "10.1/abc"
  )
  expect_identical(openalexAPI:::.api_doi_key(dois), rep("10.1/abc", 4))

  con <- openalexAPI:::.api_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "t", data.frame(x = dois))
  expect_identical(
    DBI::dbGetQuery(con, "SELECT doi_key(x) AS k FROM t")$k,
    rep("10.1/abc", 4)
  )
})
