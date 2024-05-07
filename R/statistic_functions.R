#' Estimate distributional statistics
#'
#' Estimate weighted distributional statistics for the reference or
#' the counterfactual group.
#'
#' @param dep_var vector of outcome variable
#' @param weights vector of observations weights
#' @param group_variable vector of group assignment
#' @param group identifier of group for which distributional statistics are calculated
#' @param statistics vector of statistics to be calculated
#' @param custom_statistic_function a custom statistic function to be evaluated
#' @param probs probabilities of quantiles to be calculated
#' @param log_transformed indicator if outcome variable is log transformed
#'
get_distributional_statistics <- function(dep_var,
                                          weights,
                                          group_variable,
                                          group,
                                          statistics,
                                          custom_statistic_function = NULL,
                                          probs = 1:9 / 10,
                                          log_transformed) {
  group <- levels(group_variable)[group + 1]
  dep_var <- dep_var[which(group_variable == group & weights != 0)]
  weights <- weights[which(group_variable == group & weights != 0)]

  results <- NULL
  if ("quantiles" %in% statistics) {
    results <- c(results, Hmisc::wtd.quantile(x = dep_var, weights = weights, probs = probs))
    names(results) <- paste0(names(results), "-quantile")
  }

  if ("mean" %in% statistics) {
    results <- c(results, weighted.mean(x = dep_var, w = weights))
    names(results)[length(results)] <- "Mean"
  }

  if ("variance" %in% statistics) {
    results <- c(results, Hmisc::wtd.var(x = dep_var, weight = weights))
    names(results)[length(results)] <- "Variance"
  }

  if ("gini" %in% statistics) {
    if (log_transformed) {
      results <- c(results, rifreg::compute_gini(dep_var = exp(dep_var), weights = weights))
      names(results)[length(results)] <- "Gini of untransformed Y (=exp(log(Y)))"
    } else {
      results <- c(results, rifreg::compute_gini(dep_var = dep_var, weights = weights))
      names(results)[length(results)] <- "Gini"
    }
  }

  if ("iq_range_p90_p10" %in% statistics) {
    results <- c(results, estimate_iq_range(dep_var = dep_var, weights = weights, probs = c(0.9, 0.1)))
    names(results)[length(results)] <- "Interquantile range p90-p10"
  }

  if ("iq_range_p90_p50" %in% statistics) {
    results <- c(results, estimate_iq_range(dep_var = dep_var, weights = weights, probs = c(0.9, 0.5)))
    names(results)[length(results)] <- "Interquantile range p90-p50"
  }

  if ("iq_range_p50_p10" %in% statistics) {
    results <- c(results, estimate_iq_range(dep_var = dep_var, weights = weights, probs = c(0.5, 0.1)))
    names(results)[length(results)] <- "Interquantile range p50-p10"
  }

  if ("iq_ratio_p90_p10" %in% statistics) {
    results <- c(results, estimate_iq_ratio(dep_var = dep_var, weights = weights, probs = c(0.9, 0.1)))
    names(results)[length(results)] <- "Interquantile ratio p90-p10"
  }

  if ("iq_ratio_p90_p50" %in% statistics) {
    results <- c(results, estimate_iq_ratio(dep_var = dep_var, weights = weights, probs = c(0.9, 0.5)))
    names(results)[length(results)] <- "Interquantile ratio p90/p50"
  }

  if ("iq_ratio_p90_p10" %in% statistics) {
    results <- c(results, estimate_iq_ratio(dep_var = dep_var, weights = weights, probs = c(0.5, 0.1)))
    names(results)[length(results)] <- "Interquantile ratio p50/p10"
  }

  if (!is.null(custom_statistic_function)) {
    results <- c(results, custom_statistic_function(dep_var = dep_var, weights = weights))
    names(results)[length(results)] <- "Custom statistic"
  }

  return(results)
}

#' Get normalized differences
#'
#' The function calculates normalized differences between covariate means of comparison
#' group and reweighted reference group.
#'
#' @param formula model formula used to calulate the conditional probabilities of the reweighting factor
#' @param data_used \code{data.frame} with the observation for the estimation of the reweighting factor
#' @param weights vector with observations weights
#' @param psi vector with the estimated reweighting factor
#' @param group_variable variable with group identifier
#' @param reference_group identifier of (reweighted) reference group
#'
#' @references Imbens, Guido W. and Jeffrey M. Wooldridge. 2009. Recent developments in the econometrics of program evaluation. Journal of Economic Literature 47, no. 1: 5-86.
#'
get_normalized_difference <- function(formula,
                                      data_used,
                                      weights,
                                      psi,
                                      group_variable,
                                      reference_group) {
  select_comparison <- which(group_variable != reference_group)
  select_reference <- which(group_variable == reference_group)

  mean_comparison <- apply(model.matrix(formula, data_used[select_comparison, ]), 2, function(x) weighted.mean(x = x, w = weights[select_comparison]))
  mean_reference <- apply(model.matrix(formula, data_used[select_reference, ]), 2, function(x) weighted.mean(x = x, w = weights[select_reference] * psi[select_reference]))
  sd_comparison <- apply(model.matrix(formula, data_used[select_comparison, ]), 2, function(x) sqrt(Hmisc::wtd.var(x = x, weights = weights[select_comparison])))
  sd_reference <- apply(model.matrix(formula, data_used[select_reference, ]), 2, function(x) sqrt(Hmisc::wtd.var(x = x, weights = weights[select_reference] * psi[select_reference])))
  normalized_difference <- (mean_comparison - mean_reference) / sqrt(sd_comparison^2 + sd_reference^2)

  results <- as.data.frame(cbind(
    mean_comparison,
    mean_reference,
    sd_comparison,
    sd_reference,
    normalized_difference
  ))[-1, ]

  comparison_group <- setdiff(unique(group_variable), reference_group)
  names(results) <- c(
    paste0("Mean ", comparison_group),
    paste0("Mean ", reference_group, " (reweighted)"),
    paste0("SD ", comparison_group),
    paste0("SD ", reference_group, " (reweighted)"),
    "Normalized  difference"
  )
  return(results)
}

#' Interquantile ratio
#'
#' @param dep_var numeric vector of outcome variable
#' @param weights numeric vector of weights
#' @param probs a vector with probabilities whose range defines the interquantile range
#'
#' @return a numeric value indicating the (weighted) interquantile ratio
#'
#' @export
#'
estimate_iq_ratio <- function(dep_var,
                              weights,
                              probs = c(0.1, 0.9)) {
  probs <- range(probs)
  iqr <- Hmisc::wtd.quantile(x = dep_var, weights = weights, probs = probs)
  iqr <- iqr[2] / iqr[1]
  return(iqr)
}

#' Interquantile range
#'
#' @param dep_var numeric vector of outcome variable
#' @param weights numeric vector of weights
#' @param probs a vector with probabilities whose range defines the interquantile range
#'
#' @return a numeric value indicating the (weighted) interquantile range
#'
#' @export
#'
estimate_iq_range <- function(dep_var,
                              weights,
                              probs = c(0.1, 0.9)) {
  probs <- range(probs)
  iqr <- Hmisc::wtd.quantile(x = dep_var, weights = weights, probs = probs)
  iqr <- iqr[2] - iqr[1]
  return(iqr)
}
