context("summarise_results")

conn <- nfi_connect()

test_that("summarise collected works", {
  data <- nfi_results_data(conn, 'nfi_3') %>%
    left_join(tbl(conn, 'PLOTS') %>% select(plot_id, starts_with('admin_')) %>% collect())

  data_genus <- nfi_results_data(conn, 'nfi_4', 'genus') %>%
    left_join(tbl(conn, 'PLOTS') %>% select(plot_id, starts_with('admin_')) %>% collect())

  data_fil <- nfi_results_data(conn, 'nfi_3') %>%
    nfi_results_filter(c('basal_area'), conn, basal_area > 15) %>%
    left_join(tbl(conn, 'PLOTS') %>% select(plot_id, starts_with('admin_')) %>% collect())

  data_fil_genus <- nfi_results_data(conn, 'nfi_4', 'genus') %>%
    nfi_results_filter(c('basal_area'), conn, basal_area > 15) %>%
    left_join(tbl(conn, 'PLOTS') %>% select(plot_id, starts_with('admin_')) %>% collect())

  expect_s3_class(
    nfi_results_summarise(
      data, polygon_group = 'admin_province', diameter_classes = FALSE
    ), 'tbl_df'
  )
  expect_equal(
    nfi_results_summarise(
      data, polygon_group = 'admin_province', diameter_classes = FALSE
    ) %>% nrow(), 5
  )

  expect_s3_class(
    nfi_results_summarise(
      data_genus, polygon_group = 'admin_province', functional_group = 'genus_id',
      diameter_classes = FALSE
    ), 'tbl_df'
  )
  expect_equal(
    nfi_results_summarise(
      data_genus, polygon_group = 'admin_province', functional_group = 'genus_id',
      diameter_classes = FALSE
    ) %>% nrow(), 129
  )

  expect_s3_class(
    nfi_results_summarise(
      data_fil, polygon_group = 'admin_province', diameter_classes = FALSE
    ), 'tbl_df'
  )
  expect_equal(
    nfi_results_summarise(
      data_fil, polygon_group = 'admin_province', diameter_classes = FALSE
    ) %>% nrow(), 4
  )

  expect_s3_class(
    nfi_results_summarise(
      data_fil_genus, polygon_group = 'admin_province', functional_group = 'genus_id',
      diameter_classes = FALSE
    ), 'tbl_df'
  )
  expect_equal(
    nfi_results_summarise(
      data_fil_genus, polygon_group = 'admin_province', functional_group = 'genus_id',
      diameter_classes = FALSE
    ) %>% nrow(), 34
  )
})

test_that("summarise no collected works", {
  data <- nfi_results_data(conn, 'nfi_3', .collect = FALSE) %>%
    left_join(tbl(conn, 'PLOTS') %>% select(plot_id, starts_with('admin_')))

  data_genus <- nfi_results_data(conn, 'nfi_4', 'genus', .collect = FALSE) %>%
    left_join(tbl(conn, 'PLOTS') %>% select(plot_id, starts_with('admin_')))

  data_fil <- nfi_results_data(conn, 'nfi_3', .collect = FALSE) %>%
    nfi_results_filter(c('basal_area'), conn, basal_area > 15, .collect = FALSE) %>%
    left_join(tbl(conn, 'PLOTS') %>% select(plot_id, starts_with('admin_')))

  data_fil_genus <- nfi_results_data(conn, 'nfi_4', 'genus', .collect = FALSE) %>%
    nfi_results_filter(c('basal_area'), conn, basal_area > 15, .collect = FALSE) %>%
    left_join(tbl(conn, 'PLOTS') %>% select(plot_id, starts_with('admin_')))

  expect_s3_class(
    nfi_results_summarise(
      data, polygon_group = 'admin_province', diameter_classes = FALSE, .collect = FALSE
    ), 'tbl_sql'
  )

  expect_s3_class(
    nfi_results_summarise(
      data_genus, polygon_group = 'admin_province', functional_group = 'genus_id',
      diameter_classes = FALSE, .collect = FALSE
    ), 'tbl_sql'
  )

  expect_s3_class(
    nfi_results_summarise(
      data_fil, polygon_group = 'admin_province', diameter_classes = FALSE,
      .collect = FALSE
    ), 'tbl_sql'
  )

  expect_s3_class(
    nfi_results_summarise(
      data_fil_genus, polygon_group = 'admin_province', functional_group = 'genus_id',
      diameter_classes = FALSE, .collect = FALSE
    ), 'tbl_sql'
  )
})
