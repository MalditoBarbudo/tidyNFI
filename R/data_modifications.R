#' Filter the nfi_results table
#'
#' Filter the results table by vars in the PLOTS and PLOTS_DYNAMIC tables as
#' well as by columns in the data itself
#'
#' @param nfi_data tbl from \code{\link{nfi_results_data}}
#' @param variables character vector with the variables to filter by
#' @param conn pool connection to the database
#' @param ... Filter expressions, in the same order as \code{variables} argument
#' @param .collect Logical indicating if the tbl must be collected locally.
#'   Default to TRUE
#'
#' @export
nfi_results_filter <- function(
  nfi_data,
  variables,
  conn,
  ...,
  .collect = TRUE
) {

  # capture filter expressions
  dots <- rlang::quos(...)

  # var dispatching
  PLOTS_fil_index <- vars_in_PLOTS(variables, conn)
  PLOTS_DYNAMIC_fil_index <- vars_in_PLOTS_DYNAMIC(
    variables, attr(nfi_data, 'nfi'), conn
  )
  data_fil_index <- vars_in_nfi_data(variables, nfi_data)

  # filters
  dplyr::tbl(conn, 'PLOTS') %>%
    dplyr::filter(!!! dots[PLOTS_fil_index]) %>%
    dplyr::select(plot_id) -> PLOTS_plots

  dplyr::tbl(conn, glue::glue("PLOTS_{attr(nfi_data, 'nfi')}_DYNAMIC_INFO")) %>%
    dplyr::filter(!!! dots[PLOTS_DYNAMIC_fil_index]) %>%
    dplyr::select(plot_id) -> PLOTS_DYNAMIC_plots


  # if data is collected, we need to collect also the plots and plots_dynamic
  # before joining
  if (any(class(nfi_data) == 'tbl_df')) {
    PLOTS_plots <- dplyr::collect(PLOTS_plots)
    PLOTS_DYNAMIC_plots <- dplyr::collect(PLOTS_DYNAMIC_plots)
  }

  # inner joins to get only the records wanted
  nfi_data %>%
    dplyr::filter(!!! dots[data_fil_index]) %>%
    dplyr::inner_join(PLOTS_plots, by = 'plot_id') %>%
    dplyr::inner_join(PLOTS_DYNAMIC_plots, by = 'plot_id') -> res

  if (!isTRUE(.collect)) {
    if (any(class(nfi_data) == 'tbl_df')) {
      warning(
        ".collect set to FALSE, but nfi_data already collected. Returning collected filter results"
      )
    }
  }

  if (isTRUE(.collect) & any(class(nfi_data) == 'tbl_sql')) {
    res <- dplyr::collect(res)
  }

  return(res)

}

#' Summarise the nfi results data (raw or filtered)
#'
#' Summarise the data by polygon groups and functional groups (if any)
#'
#' turururururur
#'
#' @section Functional groups:
#' \code{functional_group} parameter allows to retrieve the table of plots
#'   broken down by the desired group. Allowed values are:
#'   \itemize{
#'     \item{\code{"none"} (No breakdown)}
#'     \item{\code{"species"}}
#'     \item{\code{"simplified_species"}}
#'     \item{\code{"genus"}}
#'     \item{\code{"dec"} (Deciduous/Esclerophyllous/Conifer)}
#'     \item{\code{"bc"} (Broadleaf/Conifer)}
#'   }
#'
#' @param nfi_data tbl from \code{\link{nfi_results_data}} or
#'   \code{\link{nfi_results_filter}}
#' @param polygon_group character indicating the polygon group to summarise
#' @param functional_group Functional group to retrieve table for. Default to 'none'
#'   (no functional group). See details for more information
#' @param diameter_classes Logical indicating if the table contains diameter classes
#' @param .funs functions list (as obtained from \code{\link[dplyr]{funs}}) with the
#'   summarise functions
#' @param .collect Logical indicating if the tbl must be collected locally.
#'   Default to TRUE
#'
#' @importFrom stats sd quantile
#'
#' @export
nfi_results_summarise <- function(
  nfi_data, polygon_group, functional_group = 'none', diameter_classes,
  .funs = dplyr::funs(
    mean = mean(., na.rm = TRUE),
    sd = sd(., na.rm = TRUE),
    min = min(., na.rm = TRUE),
    max = max(., na.rm = TRUE)
  ),
  .collect = TRUE
) {

  # preparing the grouping vars
  if (functional_group == 'none') {functional_group <- ''}
  grouping_vars <- rlang::quos(
    !!rlang::sym(polygon_group), !!rlang::sym(functional_group)
  )

  if (isTRUE(diameter_classes)) {
    grouping_vars <- c(grouping_vars, rlang::quo(diamclass_id))
  }

  grouping_vars <- grouping_vars[!vapply(
    grouping_vars, rlang::quo_is_missing, logical(1)
  )]

  # go for it
  res <- nfi_data %>%
    dplyr::group_by(!!! grouping_vars) %>%
    dplyr::summarise_if(is.numeric, .funs = .funs)

  # check for collect
  if (!isTRUE(.collect)) {
    if (any(class(nfi_data) == 'tbl_df')) {
      warning(
        ".collect set to FALSE, but nfi_data already collected. Returning collected filter results"
      )
    }
  }

  if (isTRUE(.collect) & any(class(nfi_data) == 'tbl_sql')) {
    res <- dplyr::collect(res)
  }

  return(res)
}