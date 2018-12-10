context("filter_results")

conn <- nfi_connect()

test_that("vars_in_PLOTS helper works", {
  expect_length(
    tidyNFI:::vars_in_PLOTS(c('genus_id', 'basal_area', 'admin_province'), conn),
    1
  )
  expect_length(
    tidyNFI:::vars_in_PLOTS(c('genus_id', 'basal_area'), conn),
    0
  )
  expect_length(
    tidyNFI:::vars_in_PLOTS(
      c('genus_id', 'basal_area', 'admin_province', 'feat_sampling_year'), conn
    ),
    1
  )
})

test_that("vars_in_PLOTS_DYNAMIC helper works", {
  expect_length(
    tidyNFI:::vars_in_PLOTS_DYNAMIC(
      c('genus_id', 'basal_area', 'feat_sampling_year'), 'NFI_3', conn
    ),
    1
  )
  expect_length(
    tidyNFI:::vars_in_PLOTS_DYNAMIC(c('genus_id', 'basal_area'), 'NFI_3', conn),
    0
  )
  expect_length(
    tidyNFI:::vars_in_PLOTS_DYNAMIC(
      c('genus_id', 'basal_area', 'admin_province', 'feat_sampling_year'),
      'NFI_3', conn
    ),
    1
  )
  expect_length(
    tidyNFI:::vars_in_PLOTS_DYNAMIC(
      c('genus_id', 'basal_area', 'feat_sampling_year'), 'COMP_NFI3_NFI4', conn
    ),
    0
  )
  expect_warning(
    tidyNFI:::vars_in_PLOTS_DYNAMIC(
      c('genus_id', 'basal_area', 'feat_sampling_year'), 'COMP_NFI3_NFI4', conn
    ),
    "Comparision tables can not be filtered by variables from nfi dynamic info tables"
  )
})

test_that("vars_in_nfi_data works", {
  nfi_data_collected <- nfi_results_data(conn, 'nfi_4', 'genus', FALSE, TRUE)
  nfi_data_nocollected <- nfi_results_data(conn, 'nfi_4', 'genus', FALSE, FALSE)

  expect_length(
    tidyNFI:::vars_in_nfi_data(
      c('genus_id', 'basal_area', 'feat_sampling_year'), nfi_data_collected
    ),
    2
  )
  expect_length(
    tidyNFI:::vars_in_nfi_data(c('feat_sampling_year'), nfi_data_collected),
    0
  )
  expect_length(
    tidyNFI:::vars_in_nfi_data(
      c('genus_id', 'basal_area', 'feat_sampling_year'), nfi_data_nocollected
    ),
    2
  )
  expect_length(
    tidyNFI:::vars_in_nfi_data(c('feat_sampling_year'), nfi_data_nocollected),
    0
  )
})

test_that("filter collected works", {
  nfi_data_collected <- nfi_results_data(conn, 'nfi_4', 'genus', FALSE, TRUE)

  expect_s3_class(
    nfi_results_filter(
      nfi_data_collected,
      variables = c('genus_id', 'basal_area', 'admin_province'),
      conn = conn,
      genus_id == 'Pinus', basal_area > 15, admin_province == 'Barcelona',
      .collect = TRUE
    ),
    'tbl_df'
  )
  expect_s3_class(
    nfi_results_filter(
      nfi_data_collected,
      variables = c('genus_id', 'basal_area', 'admin_province'),
      conn = conn,
      genus_id == 'Pinus', basal_area > 15, admin_province == 'Barcelona',
      .collect = FALSE
    ),
    'tbl_df'
  )
  expect_warning(
    nfi_results_filter(
      nfi_data_collected,
      variables = c('genus_id', 'basal_area', 'admin_province'),
      conn = conn,
      genus_id == 'Pinus', basal_area > 15, admin_province == 'Barcelona',
      .collect = FALSE
    ),
    '.collect set to FALSE, but nfi_data already collected. Returning collected filter results'
  )
})

test_that("filter no collected works", {
  nfi_data_nocollected <- nfi_results_data(conn, 'nfi_4', 'genus', FALSE, FALSE)

  expect_s3_class(
    nfi_results_filter(
      nfi_data_nocollected,
      variables = c('genus_id', 'basal_area', 'admin_province'),
      conn = conn,
      genus_id == 'Pinus', basal_area > 15, admin_province == 'Barcelona',
      .collect = TRUE
    ),
    'tbl_df'
  )
  expect_s3_class(
    nfi_results_filter(
      nfi_data_nocollected,
      variables = c('genus_id', 'basal_area', 'admin_province'),
      conn = conn,
      genus_id == 'Pinus', basal_area > 15, admin_province == 'Barcelona',
      .collect = FALSE
    ),
    'tbl_sql'
  )
})

nfi_close(conn)
