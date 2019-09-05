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

  if (attr(nfi_data, 'nfi') %in% c(
    'SHRUB_NFI_2_INFO', 'SHRUB_NFI_3_INFO', 'SHRUB_NFI_4_INFO',
    'REGENERATION_NFI_2', 'REGENERATION_NFI_3', 'REGENERATION_NFI_4'
  )) {
    nfi_strip <- stringr::str_extract(attr(nfi_data, 'nfi'), 'NFI_[2-4]')
    dplyr::tbl(conn, glue::glue("PLOTS_{nfi_strip}_DYNAMIC_INFO")) %>%
      dplyr::filter(!!! dots[PLOTS_DYNAMIC_fil_index]) %>%
      dplyr::select(plot_id) -> PLOTS_DYNAMIC_plots
  } else {
    if (!(attr(nfi_data, 'nfi') %in% c('COMP_NFI2_NFI3', 'COMP_NFI3_NFI4'))) {
      dplyr::tbl(conn, glue::glue("PLOTS_{attr(nfi_data, 'nfi')}_DYNAMIC_INFO")) %>%
        dplyr::filter(!!! dots[PLOTS_DYNAMIC_fil_index]) %>%
        dplyr::select(plot_id) -> PLOTS_DYNAMIC_plots
    }
  }

  # if data is collected, we need to collect also the plots and plots_dynamic
  # before joining
  if (any(class(nfi_data) == 'tbl_df')) {
    PLOTS_plots <- dplyr::collect(PLOTS_plots)
    if (!(attr(nfi_data, 'nfi') %in% c('COMP_NFI2_NFI3', 'COMP_NFI3_NFI4'))) {
      PLOTS_DYNAMIC_plots <- dplyr::collect(PLOTS_DYNAMIC_plots)
    }
  }

  # inner joins to get only the records wanted
  nfi_data %>%
    dplyr::filter(!!! dots[data_fil_index]) %>%
    dplyr::inner_join(PLOTS_plots, by = 'plot_id') -> res

  if (!(attr(nfi_data, 'nfi') %in% c('COMP_NFI2_NFI3', 'COMP_NFI3_NFI4'))) {
    res <- res %>%
      dplyr::inner_join(PLOTS_DYNAMIC_plots, by = 'plot_id')
  }

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
#' @section Functional groups:
#' \code{functional_group} parameter allows to retrieve the table of plots
#'   broken down by the desired group. Allowed values are:
#'   \itemize{
#'     \item{\code{"none"} (No breakdown)}
#'     \item{\code{"species"}}
#'     \item{\code{"simpspecies"}}
#'     \item{\code{"genus"}}
#'     \item{\code{"dec"} (Deciduous/Esclerophyllous/Conifer)}
#'     \item{\code{"bc"} (Broadleaf/Conifer)}
#'   }
#'
#' @param nfi_data tbl from \code{\link{nfi_results_data}} or
#'   \code{\link{nfi_results_filter}}
#' @param polygon_group character indicating the polygon group to summarise, if any.
#' @param functional_group Functional group to retrieve table for. Default to 'none'
#'   (no functional group). See details for more information
#' @param diameter_classes Logical indicating if the table contains diameter classes
#' @param dominant_group Dominant functional group to summarise data, if any.
#' @param dominant_criteria Character with the dominancy criteria, basal area
#'   (\code{"basal_area"}) or density (\code{"density"}). Default to density.
#' @param dominant_nfi Character with the nfi version for the dominancy in the case of
#'   the comparision tables. Defauilt to \code{"none"}.
#' @param conn pool connection to the database
#' @param .funs functions list (as obtained from \code{\link[dplyr]{funs}}) with the
#'   summarise functions
#' @param .collect Logical indicating if the tbl must be collected locally.
#'   Default to TRUE
#'
#' @importFrom stats sd quantile
#' @importFrom dplyr n
#'
#' @export
nfi_results_summarise <- function(
  nfi_data, polygon_group = 'none', functional_group = 'none', diameter_classes,
  dominant_group = 'none', dominant_criteria = "density", dominant_nfi = 'none',
  conn,
  .funs = dplyr::funs(
    mean = mean(., na.rm = TRUE),
    se = sd(., na.rm = TRUE)/sqrt(n()),
    min = min(., na.rm = TRUE),
    max = max(., na.rm = TRUE),
    n = n()
  ),
  .collect = TRUE
) {

  # dominant nfi
  dominant_nfi <- switch(
    dominant_nfi,
    none = '',
    nfi2 = '_nfi2',
    nfi3 = '_nfi3',
    nfi4 = '_nfi4'
  )

  # dominant group
  dominant_group <- switch(
    dominant_group,
    none = '',
    species = glue::glue("{dominant_criteria}_species_dominant{dominant_nfi}"),
    simpspecies = glue::glue("{dominant_criteria}_simpspecies_dominant{dominant_nfi}"),
    genus = glue::glue("{dominant_criteria}_genus_dominant{dominant_nfi}"),
    dec = glue::glue("{dominant_criteria}_dec_dominant{dominant_nfi}"),
    bc = glue::glue("{dominant_criteria}_bc_dominant{dominant_nfi}")
  )

  # polygon_group
  polygon_group <- switch(
    polygon_group,
    none = '',
    aut_community = 'admin_aut_community',
    province = 'admin_province',
    region = 'admin_region',
    vegueria = 'admin_vegueria',
    municipality = 'admin_municipality',
    natural_interest_area = 'admin_natural_interest_area',
    special_protection_natural_area = 'admin_special_protection_natural_area',
    natura_network_2000 = 'admin_natura_network_2000',
    file = 'poly_id'
  )

  # functional_group
  functional_group <- switch(
    functional_group,
    none = '',
    species = 'species_id',
    simpspecies = 'simpspecies_id',
    genus = 'genus_id',
    dec = 'dec_id',
    bc = 'bc_id',
    plot = ''
  )

  # preparing the data, if the admin variables are not in the data, lets join them,
  # except for the none case
  if (polygon_group != '') {
    if (any(class(nfi_data) == 'tbl_df')) {
      if (!(polygon_group %in% names(nfi_data))) {
        nfi_data <- nfi_data %>%
          dplyr::left_join(
            dplyr::tbl(conn, 'PLOTS') %>%
              dplyr::select(dplyr::one_of(c('plot_id', polygon_group))) %>%
              # dplyr::select(plot_id, !!rlang::sym(polygon_group)) %>%
              dplyr::collect(),
            by = 'plot_id'
          )
      }
    } else {
      if (!(polygon_group %in% (nfi_data %>% head(1) %>% dplyr::collect() %>% names()))) {
        nfi_data <- nfi_data %>%
          dplyr::left_join(
            dplyr::tbl(conn, 'PLOTS') %>%
              dplyr::select(dplyr::one_of(c('plot_id', polygon_group))),
              # dplyr::select(plot_id, !!rlang::sym(polygon_group)),
            by = 'plot_id'
          )
      }
    }
  }

  # preparing the grouping vars
  if (isTRUE(diameter_classes)) {

    # check if there is diamclass_id var
    if ('diamclass_id' %in% (nfi_data %>% head(1) %>% dplyr::collect() %>% names())) {

      # if there is diamclass_id var, it means there is not dominant var, so check
      # if the user have provide one
      if (dominant_group != '') {
        warning("when summarising by diameter classes, dominancy is ignored")
        dominant_group <-  ''
      }

      grouping_vars <- rlang::quos(
        !!rlang::sym(polygon_group),
        !!rlang::sym(functional_group),
        !!rlang::sym(dominant_group),
        diamclass_id
      ) %>%
        magrittr::extract(
          !purrr::map_lgl(., rlang::quo_is_missing)
        )
    } else {
      stop("diameter_classes set to TRUE, but no diameter classes variable found")
    }
  } else {
    grouping_vars <- rlang::quos(
      !!rlang::sym(polygon_group),
      !!rlang::sym(functional_group),
      !!rlang::sym(dominant_group)
    ) %>%
      magrittr::extract(
        !purrr::map_lgl(., rlang::quo_is_missing)
      )
  }

  # go for it
  res <- nfi_data %>%
    dplyr::group_by(!!! grouping_vars) %>%
    dplyr::summarise_if(is.numeric, .funs = .funs)

  # check for collect
  if (!isTRUE(.collect)) {
    if (any(class(nfi_data) == 'tbl_df')) {
      warning(
        ".collect set to FALSE, but nfi_data already collected. Returning collected summarised results"
      )
    }
  }

  if (isTRUE(.collect) & any(class(nfi_data) == 'tbl_sql')) {
    res <- dplyr::collect(res)
  }

  return(res)
}
