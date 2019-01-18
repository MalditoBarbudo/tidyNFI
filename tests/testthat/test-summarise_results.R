context("summarise_results")

conn <- nfi_connect()

test_that("summarise collected works", {
  data <- nfi_results_data(conn, 'nfi_3')

  data_genus <- nfi_results_data(conn, 'nfi_4', 'genus')

  data_fil <- nfi_results_data(conn, 'nfi_3') %>%
    nfi_results_filter(c('basal_area'), conn, basal_area > 15)

  data_fil_genus <- nfi_results_data(conn, 'nfi_4', 'genus') %>%
    nfi_results_filter(c('basal_area'), conn, basal_area > 15)

  expect_s3_class(
    nfi_results_summarise(
      data, polygon_group = 'province', diameter_classes = FALSE, conn = conn
    ), 'tbl_df'
  )
  expect_equal(
    nfi_results_summarise(
      data, polygon_group = 'province', diameter_classes = FALSE, conn = conn
    ) %>% nrow(), 4
  )

  expect_s3_class(
    nfi_results_summarise(
      data_genus, polygon_group = 'province', functional_group = 'genus',
      diameter_classes = FALSE, conn = conn
    ), 'tbl_df'
  )
  expect_equal(
    nfi_results_summarise(
      data_genus, polygon_group = 'province', functional_group = 'genus',
      diameter_classes = FALSE, conn = conn
    ) %>% nrow(), 129
  )

  expect_s3_class(
    nfi_results_summarise(
      data_fil, polygon_group = 'province', diameter_classes = FALSE, conn = conn
    ), 'tbl_df'
  )
  expect_equal(
    nfi_results_summarise(
      data_fil, polygon_group = 'province', diameter_classes = FALSE, conn = conn
    ) %>% nrow(), 4
  )

  expect_s3_class(
    nfi_results_summarise(
      data_fil_genus, polygon_group = 'province', functional_group = 'genus',
      diameter_classes = FALSE, conn = conn
    ), 'tbl_df'
  )
  expect_equal(
    nfi_results_summarise(
      data_fil_genus, polygon_group = 'province', functional_group = 'genus',
      diameter_classes = FALSE, conn = conn
    ) %>% nrow(), 34
  )
})

test_that("summarise no collected works", {
  data <- nfi_results_data(conn, 'nfi_3', .collect = FALSE)

  data_genus <- nfi_results_data(conn, 'nfi_4', 'genus', .collect = FALSE)

  data_fil <- nfi_results_data(conn, 'nfi_3', .collect = FALSE) %>%
    nfi_results_filter(c('basal_area'), conn, basal_area > 15, .collect = FALSE)

  data_fil_genus <- nfi_results_data(conn, 'nfi_4', 'genus', .collect = FALSE) %>%
    nfi_results_filter(c('basal_area'), conn, basal_area > 15, .collect = FALSE)

  expect_s3_class(
    nfi_results_summarise(
      data, polygon_group = 'province', diameter_classes = FALSE, conn = conn,
      .collect = FALSE
    ), 'tbl_sql'
  )

  expect_s3_class(
    nfi_results_summarise(
      data_genus, polygon_group = 'province', functional_group = 'genus',
      diameter_classes = FALSE, conn = conn, .collect = FALSE
    ), 'tbl_sql'
  )

  expect_s3_class(
    nfi_results_summarise(
      data_fil, polygon_group = 'province', diameter_classes = FALSE, conn = conn,
      .collect = FALSE
    ), 'tbl_sql'
  )

  expect_s3_class(
    nfi_results_summarise(
      data_fil_genus, polygon_group = 'aut_community', functional_group = 'genus',
      diameter_classes = FALSE, conn = conn, .collect = FALSE
    ), 'tbl_sql'
  )
})

test_that('summarise when admin data is already there works', {
  data <- nfi_results_data(conn, 'nfi_3', .collect = FALSE) %>%
    dplyr::left_join(
      dplyr::tbl(conn, 'PLOTS') %>% dplyr::select(plot_id, dplyr::starts_with('admin_'))
    )

  data_coll <- nfi_results_data(conn, 'nfi_3', .collect = TRUE) %>%
    dplyr::left_join(
      dplyr::tbl(conn, 'PLOTS') %>% dplyr::select(plot_id, dplyr::starts_with('admin_')) %>%
        dplyr::collect()
    )

  expect_s3_class(
    nfi_results_summarise(
      data, polygon_group = 'province', diameter_classes = FALSE, conn = conn,
      .collect = FALSE
    ), 'tbl_sql'
  )

  expect_s3_class(
    nfi_results_summarise(
      data_coll, polygon_group = 'province', diameter_classes = FALSE, conn = conn,
      .collect = TRUE
    ), 'tbl_df'
  )
})

nfi_close(conn)

