% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_Suggest.R
\name{solve_opt}
\alias{solve_opt}
\title{Solve the optimization problem}
\usage{
solve_opt(
  points,
  budget,
  D,
  wria_sel,
  huc_sel,
  owner_sel,
  obj,
  w_urb,
  w_ag,
  w_nat,
  w_temp,
  hq,
  species_sel,
  barrier_idp,
  cost,
  mean_design_cost,
  mean_construction_cost
)
}
\arguments{
\item{points}{A simple features point data frame containing culvert locations and attributes.}

\item{budget}{A number specifying the budget constraint.}

\item{D}{A connectivity matrix.}

\item{owner_sel}{A vector of owner ID numbers of interest.}

\item{obj}{An indicator for whether objective function is to max quant (obj = 1) or max weighted sum of attributes (obj = 2).}

\item{w_urb}{A weight on urban habitat quantity.}

\item{w_ag}{A weight on agricultural habitat quantity.}

\item{w_nat}{A weight on natural habitat quantity.}

\item{w_temp}{A weight on ideal temperature.}

\item{hq}{A value for habitat quality definition (1 = length, 2 = area, 3 = volume).}

\item{species_sel}{A vector of species ID numbers of interest.}

\item{barrier_idp}{A vector of planned culvert IDs.}

\item{area_sel}{A vector of WRIA ID numbers of interest.}

\item{subarea_sel}{A vector of WRIA ID numbers of interest.}
}
\value{
A logical vector of TRUE/FALSE values.
}
\description{
Solve the optimization problem
}
