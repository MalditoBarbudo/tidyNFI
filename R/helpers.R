#' vars_in_PLOTS
#'
#' Given a variables character list, returns an index with which vars are in the
#' PLOTS table
#'
vars_in_PLOTS <- function(variables, conn) {
  res <- which(variables %in% (
    tbl(conn, 'PLOTS') %>%
      filter(row_number() == 1L) %>%
      collect() %>%
      names()
  ))
  return(res)
}

#' var_in_PLOTS_DYNAMIC
#'
#' Given a variables character list, returns an index with which vars are in the
#' corresponding PLOTS_*_DYNAMIC_INFO table
#'
var_in_PLOTS_DYNAMIC <- function(variables, nfi, conn) {

  # check if nfi is for comparision, as then it can not be filtered by
  # dynamic info
  if (nfi %in% c('COMP_NFI2_NFI3', 'COMP_NFI3_NFI4')) {
    warning('Comparision tables can not be filtered by variables from nfi dynamic info tables')
    return(numeric())
  }

  res <- which(variables %in% (
    tbl(conn, glue::glue("PLOTS_{nfi}_DYNAMIC_INFO")) %>%
      filter(row_number() == 1L) %>%
      collect() %>%
      names()
  ))
  return(res)
}