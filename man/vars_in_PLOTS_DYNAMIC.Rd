% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{vars_in_PLOTS_DYNAMIC}
\alias{vars_in_PLOTS_DYNAMIC}
\title{var_in_PLOTS_DYNAMIC}
\usage{
vars_in_PLOTS_DYNAMIC(variables, nfi, conn)
}
\arguments{
\item{variables}{character vector with the names of the variables to check}

\item{nfi}{character indicating the nfi version}

\item{conn}{pool object with the database connection}
}
\description{
Given a variables character list, returns an index with which vars are in the
corresponding PLOTS_*_DYNAMIC_INFO table
}
