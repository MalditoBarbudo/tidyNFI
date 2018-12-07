context("get_results_data")

conn <- nfi_connect()

test_that("plot level works", {
  expect_s3_class(
    nfi_results_data(conn, 'nfi_2', 'none', FALSE, .collect = TRUE), 'tbl_df'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_2', 'none', FALSE, .collect = FALSE), 'tbl_sql'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_4', 'none', FALSE, .collect = TRUE), 'tbl_df'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_4', 'none', FALSE, .collect = FALSE), 'tbl_sql'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_2_nfi_3', 'none', FALSE, .collect = TRUE), 'tbl_df'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_2_nfi_3', 'none', FALSE, .collect = FALSE), 'tbl_sql'
  )
})

test_that("species level works", {
  expect_s3_class(
    nfi_results_data(conn, 'nfi_3', 'species', FALSE, .collect = TRUE), 'tbl_df'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_3', 'species', FALSE, .collect = FALSE), 'tbl_sql'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_4', 'species', FALSE, .collect = TRUE), 'tbl_df'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_4', 'species', FALSE, .collect = FALSE), 'tbl_sql'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_3_nfi_4', 'species', FALSE, .collect = TRUE), 'tbl_df'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_3_nfi_4', 'species', FALSE, .collect = FALSE), 'tbl_sql'
  )
})

test_that("diamclass level works", {
  expect_s3_class(
    nfi_results_data(conn, 'nfi_3', 'genus', TRUE, .collect = FALSE), 'tbl_sql'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_3', 'simplified_species', TRUE, .collect = FALSE), 'tbl_sql'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_2', 'dec', TRUE, .collect = FALSE), 'tbl_sql'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_4', 'bc', TRUE, .collect = FALSE), 'tbl_sql'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_2_nfi_3', 'none', TRUE, .collect = FALSE), 'tbl_sql'
  )
  expect_s3_class(
    nfi_results_data(conn, 'nfi_3_nfi_4', 'species', TRUE, .collect = FALSE), 'tbl_sql'
  )
})

nfi_close(conn)