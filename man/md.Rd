% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md.R
\name{md}
\alias{md}
\alias{md.data.frame}
\alias{md_vec}
\title{Mean Difference (Mean Signed Deviation)}
\usage{
md(data, ...)

\method{md}{data.frame}(data, truth, estimate, na_rm = TRUE, case_weights = NULL, ...)

md_vec(truth, estimate, na_rm = TRUE, case_weights = NULL, ...)
}
\arguments{
\item{data}{A data frame containing the `truth` and `estimate` columns.}

\item{...}{Additional arguments (currently ignored).}

\item{truth}{The column identifier for the true (observed) values (bare or quoted).}

\item{estimate}{The column identifier for the predicted (estimated) values (bare or quoted).}

\item{na_rm}{A logical value indicating whether `NA` values should be removed before computation.}

\item{case_weights}{An optional column of case weights.}
}
\value{
A single numeric value representing the mean signed deviation (`estimate - truth`).
}
\description{
Computes the mean signed deviation between `estimate` and `truth`, calculated as
the mean of (`estimate - truth`). This is a wrapper around `yardstick::msd()` that
reverses the default sign, so positive values indicate overestimation.
}
\details{
`yardstick::msd()` computes the mean signed deviation as `truth - estimate`, which means
positive values indicate underestimation. This metric reverses the sign by returning
`-(truth - estimate)`, effectively calculating `estimate - truth`.

This metric is useful for assessing average directional bias in predictions.
Like other `yardstick` metrics, it supports missing value handling and case weights.
}
