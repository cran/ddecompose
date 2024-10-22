% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ob_decompose.R
\name{estimate_ob_decompose}
\alias{estimate_ob_decompose}
\title{Estimate OB decomposition}
\usage{
estimate_ob_decompose(
  formula,
  data_used,
  reference_0,
  normalize_factors,
  compute_analytical_se,
  return_model_fit,
  reweighting,
  rifreg,
  rifreg_statistic,
  rifreg_probs,
  custom_rif_function,
  na.action,
  vcov,
  ...
)
}
\arguments{
\item{formula}{\code{formula} object}

\item{data_used}{\code{data.frame} with data used for estimation (including weight and group variable)}

\item{reference_0}{boolean: indicating if group 0 is the reference group and if its coefficients are used to compute the counterfactual mean.}

\item{normalize_factors}{boolean: If `TRUE`, then factor variables are normalized as proposed by Gardeazabal/Ugidos (2004)}

\item{compute_analytical_se}{boolean: If `TRUE`, then analytical standard errors for decomposition terms are calculated (assuming independence between groups).}

\item{return_model_fit}{boolean: If `TRUE`, then model objects are returned.}

\item{reweighting}{boolean: if `TRUE`, then the decomposition is performed with
with respect to reweighted reference group.}

\item{rifreg}{boolean: if `TRUE`, then RIF decomposition is performed}

\item{rifreg_statistic}{string containing the distributional statistic for which to compute the RIF.}

\item{rifreg_probs}{a vector of length 1 or more with probabilities of quantiles.}

\item{custom_rif_function}{the RIF function to compute the RIF of the custom distributional statistic.}

\item{na.action}{generic function that defines how NAs in the data should be handled.}

\item{vcov}{unction estimating covariance matrix of regression coefficients if \code{compute_analytical_se == TRUE}}

\item{...}{additional parameters passed to custom_rif_function}
}
\description{
The function performs the linear Oaxaca-Blinder decomposition.
}
