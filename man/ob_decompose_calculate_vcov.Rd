% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ob_decompose.R
\name{ob_decompose_calculate_vcov}
\alias{ob_decompose_calculate_vcov}
\title{Calculate covariance matrix for OB decomposition terms}
\usage{
ob_decompose_calculate_vcov(
  beta0,
  beta1,
  X0,
  X1,
  weights0,
  weights1,
  Cov_beta0,
  Cov_beta1,
  reference_0
)
}
\arguments{
\item{beta0}{vector of estimated coefficients of group 0}

\item{beta1}{vector of estimated coefficients of group 1}

\item{X0}{\code{model.matrix} of group 0}

\item{X1}{\code{model.matrix} of group 1}

\item{weights0}{vector of observation weights of group 0}

\item{weights1}{vector of observation weights of group 1}

\item{Cov_beta0}{estimated covariance matrix of coefficients of group 0}

\item{Cov_beta1}{estimated covariance matrix of coefficients of group 1}

\item{reference_0}{boolean: indicating if group 0 is the reference group and if its coefficients are used to compute the counterfactual mean.}
}
\description{
The function calculate the covariance matrix for the decomposition terms
of the linear Oaxaca-Blinder decomposition assuming independence between
groups.
}
\references{
Jann, Ben, 2005. "Standard errors for the Blinder-Oaxaca decomposition." *3rd German Stata Users’ Group Meeting 2005*. Available from [https://boris.unibe.ch/69506/1/oaxaca_se_handout.pdf](https://boris.unibe.ch/69506/1/oaxaca_se_handout.pdf).
}
