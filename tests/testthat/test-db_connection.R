context("db_connection")

library(dplyr)

test_that("nfi_connect works", {

  conn <- nfi_connect()

  expect_is(conn, 'Pool')
  expect_is(tbl(conn, 'PLOTS'), 'tbl')

  nfi_close(conn)

  expect_error(tbl(conn, 'PLOTS'))
})
