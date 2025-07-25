% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rrmse.R
\name{rrmse}
\alias{rrmse}
\alias{rrmse.data.frame}
\alias{rrmse_vec}
\title{Relative root mean squared error}
\usage{
rrmse(data, ...)

\method{rrmse}{data.frame}(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  normalization = "mean",
  ...
)

rrmse_vec(
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  normalization = "mean",
  ...
)
}
\arguments{
\item{data}{A `data.frame` containing the columns specified by the `truth`
and `estimate` arguments.}

\item{...}{Additional arguments passed to lower-level functions.}

\item{truth}{The column identifier for the true results (numeric).}

\item{estimate}{The column identifier for the predicted results (numeric).}

\item{na_rm}{A `logical` value indicating whether `NA` values should be removed.}

\item{case_weights}{Currently not implemented.}

\item{normalization}{Character string specifying the normalization method:
`"mean"` (default) or `"range"`.}
}
\description{
Calculate the relative root mean squared error. `rrmse()` expresses
the RMSE as a proportion of either the **mean** or **range** of the true values.
}
