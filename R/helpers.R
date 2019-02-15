#' vars_in_PLOTS
#'
#' Given a variables character list, returns an index with which vars are in the
#' PLOTS table
#'
#' @param variables character vector with the names of the variables to check
#' @param conn pool object with the database connection
#'
vars_in_PLOTS <- function(variables, conn) {
  res <- which(variables %in% (
    pool::dbGetQuery(conn, 'SELECT * FROM "PLOTS" LIMIT 1') %>% names()
  ))
  return(res)
}

#' var_in_PLOTS_DYNAMIC
#'
#' Given a variables character list, returns an index with which vars are in the
#' corresponding PLOTS_*_DYNAMIC_INFO table
#'
#' @param variables character vector with the names of the variables to check
#' @param nfi character indicating the nfi version
#' @param conn pool object with the database connection
#'
vars_in_PLOTS_DYNAMIC <- function(variables, nfi, conn) {

  # check if nfi is for comparision, as then it can not be filtered by
  # dynamic info
  if (nfi %in% c('COMP_NFI2_NFI3', 'COMP_NFI3_NFI4')) {
    warning('Comparision tables can not be filtered by variables from nfi dynamic info tables')
    return(numeric())
  }

  if (nfi %in% c(
    'SHRUB_NFI_2_INFO', 'SHRUB_NFI_3_INFO', 'SHRUB_NFI_4_INFO',
    'REGENERATION_NFI_2', 'REGENERATION_NFI_3', 'REGENERATION_NFI_4'
  )) {
    nfi <- stringr::str_extract(nfi, 'NFI_[2-4]')
  }

  res <- which(variables %in% (
    pool::dbGetQuery(
      conn, glue::glue('SELECT * FROM "PLOTS_{nfi}_DYNAMIC_INFO" LIMIT 1')
    ) %>% names()
  ))
  return(res)
}

#' vars_in_nfi_data
#'
#' helper to get the names in data, checking if it is already collected or not
#'
#' @param variables character vector with the names of the variables to check
#' @param nfi_data nfi_data
#'
vars_in_nfi_data <- function(variables, nfi_data) {

  if (any(class(nfi_data) == 'tbl_df')) {
    res <- which(variables %in% names(nfi_data))
  } else {
    if (any(class(nfi_data) == 'tbl_sql')) {
      res <- which(variables %in% (
        nfi_data %>% head(1) %>% dplyr::collect() %>% names()
      ))
    } else {
      res <- numeric()
    }
  }
  return(res)
}

#' NFI scenario
#'
#' Return the current scenario as character
#'
#' There are four main scenarios to work with in the NFI data:
#' \itemize{
#'   \item{Scenario 1. Total by plot. In this scenario we can have the raw total
#'   and the dominant functional group (in two flavours, basal area or density)}
#'   \item{Scenario 2. Plot broken down by functional group, regardless the
#'   dominance}
#'   \item{Scenarios 3 and 4. The same as 1 and 2 but summarised by polygon,
#'   being the polygon the administrative divisions or custom polygons}
#' }
#'
#' @section viz_shape:
#' \code{plot} or \code{polygon}
#'
#' @section agg_level:
#' \code{none}, \code{species}, \code{simpspecies}, \code{genus}, \code{dec} or \code{bc}
#'
#' @param viz_shape The shape to visualize, plots or polygons
#' @param agg_level The breakdown level, none, species...
nfi_get_scenario <- function(viz_shape, agg_level) {

  agg_level <- switch(
    agg_level,
    none = 'plot',
    plot = 'plot',
    species = 'species',
    simpspecies = 'simpspecies',
    genus = 'genus',
    dec = 'dec',
    bc = 'dec'
  )

  # plot shape
  if (viz_shape == 'plot') {
    if (agg_level == 'plot') {
      scenario <- 'scenario1'
    } else {
      scenario <- 'scenario2'
    }
  } else {
    if (agg_level == 'plot') {
      scenario <- 'scenario3'
    } else {
      scenario <- 'scenario4'
    }
  }

  return(scenario)
}

#' sf builder for leaflet custom polygons plugin
#'
#' Helper to create sf objects from leaflet polygons
#'
#' @param custom_polygon object as the one returned by the leaflet plugin
custom_poly_to_sf <- function(custom_polygon) {
  custom_polygon[['geometry']][['coordinates']] %>%
    purrr::flatten() %>%
    purrr::modify_depth(1, purrr::set_names, nm = c('long', 'lat')) %>%
    dplyr::bind_rows() %>%
    {list(as.matrix(.))} %>%
    sf::st_polygon() %>%
    sf::st_sfc() %>%
    sf::st_sf(crs = "+proj=longlat +datum=WGS84")
}

#' Custom polygon filter expression builder
#'
#' @param conn pool object with the database connection
#' @param custom_polygon sf or sp object with the custom polygon to filter plots by
custom_polygon_filter_expr <- function(custom_polygon, conn) {

  if (is.null(custom_polygon)) {
    return(rlang::quo())
  }

  # if sp, then to sf
  if (class(custom_polygon) == 'Polygons') {
    custom_polygon <- sf::st_as_sf(custom_polygon)
  }

  plots_tmp <- dplyr::tbl(conn, 'PLOTS') %>%
    dplyr::select(plot_id, coords_longitude, coords_latitude) %>%
    dplyr::collect()

  plots_tmp %>% sf::st_as_sf(
    coords = c('coords_longitude', 'coords_latitude'),
    crs = "+proj=longlat +datum=WGS84"
  ) %>%
    sf::st_intersects(custom_polygon, sparse = FALSE) %>%
    which() -> points_inside_index

  plots_tmp %>%
    dplyr::pull(plot_id) %>%
    magrittr::extract(points_inside_index) -> plots_inside_polygon

  filter_expression <- rlang::quo(
    plot_id %in% !! plots_inside_polygon
  )

  return(filter_expression)

}
