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
nif_get_scenario <- function(viz_shape, agg_level) {

  agg_level <- switch(
    agg_level,
    none = 'PLOT',
    species = 'SPECIES',
    simpspecies = 'SIMPSPECIES',
    genus = 'GENUS',
    dec = 'DEC',
    bc = 'BC'
  )

  # plot shape
  if (viz_shape == 'plot') {
    if (agg_level == 'none') {
      scenario <- 'scenario1'
    } else {
      scenario <- 'scenario2'
    }
  } else {
    if (agg_level == 'none') {
      scenario <- 'scenario3'
    } else {
      scenario <- 'scenario4'
    }
  }

  return(scenario)
}

