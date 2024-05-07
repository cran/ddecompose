#' Plot decomposition terms for quantiles
#'
#' The function plots decomposition terms for quantiles estimated
#' with \code{dfl_decompose} over the  unit interval.
#'
#' @param x an object of class "dfl_decompose", usually, a result of a call to [dfl_decompose()] with [statistics = "quantiles"].
#' @param ... other parameters to be passed through to plot function.
#' @param confidence_bands If `TRUE` (default) and if standard errors have been bootstrapped, confidence bands are plotted.
#' @param confidence_level numeric value between 0 and 1 (default = 0.95) that defines the confidence interval
#'              plotted as a ribbon and defined as \code{qnorm((1-confidence_level)/2)} * standard error.
#' @param uniform_bands If `FALSE` (default), pointwise confidence bands are computed. Otherwise, uniform bands are constructed
#'              based on the bootstrapped Kolmogrov-Smirnov statistic (see \link{summary.dfl_decompose}).
#'
#' @return a ggplot illustrating the decomposition terms for quantiles.
#' @export
#'
#' @examples
#' data("men8305")
#' flf_model <- log(wage) ~ union * (education + experience) + education * experience
#' flf_male_inequality <- dfl_decompose(flf_model,
#'   data = men8305,
#'   weights = weights,
#'   group = year
#' )
#' plot(flf_male_inequality)
#'
plot.dfl_decompose <- function(x,
                               ...,
                               confidence_bands = TRUE,
                               confidence_level = 0.95,
                               uniform_bands = FALSE) {
  if (is.null(x$decomposition_quantiles)) {
    stop("Difference must be decomposed at least at a single quantile.")
  }

  decomposition_quantiles <- stats::reshape(x$decomposition_quantiles,
    idvar = c("probs"),
    times = setdiff(names(x$decomposition_quantiles), c("probs")),
    timevar = "effect",
    varying = list(setdiff(names(x$decomposition_quantiles), c("probs"))),
    direction = "long",
    v.names = "value"
  )
  confidence_bands <- ifelse(confidence_bands &
    !is.null(x$bootstrapped_standard_errors),
  TRUE,
  FALSE
  )

  if (confidence_bands) {
    decomposition_quantiles_se <- x$bootstrapped_standard_errors$decomposition_quantiles
    decomposition_quantiles_se <- stats::reshape(decomposition_quantiles_se,
      idvar = c("probs"),
      times = setdiff(names(decomposition_quantiles_se), c("probs")),
      timevar = "effect",
      varying = list(setdiff(names(decomposition_quantiles_se), c("probs"))),
      direction = "long",
      v.names = "se"
    )

    kolmogorov_smirnov_stat <- x$bootstrapped_standard_errors$decomposition_quantiles_kms_distribution
    kolmogorov_smirnov_stat <- lapply(
      split(kolmogorov_smirnov_stat, kolmogorov_smirnov_stat$effect),
      function(x) {
        data.frame(
          effect = x$effect[1],
          t_value = quantile(x$kms_t_value, confidence_level)
        )
      }
    )
    kolmogorov_smirnov_stat <- do.call("rbind", kolmogorov_smirnov_stat)
    rn <- names(decomposition_quantiles_se)
    decomposition_quantiles_se <- cbind(
      decomposition_quantiles_se,
      kolmogorov_smirnov_stat[match(decomposition_quantiles_se$effect, kolmogorov_smirnov_stat$effect), "t_value"]
    )
    names(decomposition_quantiles_se) <- c(rn, "t_value")
    decomposition_quantiles_se$effect_probs <- paste0(decomposition_quantiles_se$effect, decomposition_quantiles_se$probs)
    decomposition_quantiles$effect_probs <- paste0(decomposition_quantiles$effect, decomposition_quantiles$probs)
    decomposition_quantiles <- cbind(
      decomposition_quantiles,
      decomposition_quantiles_se[match(decomposition_quantiles$effect_probs, decomposition_quantiles_se$effect_probs), c("se", "t_value")]
    )
    decomposition_quantiles$effect_probs <- NULL


    if (!uniform_bands) {
      decomposition_quantiles$t_value <- stats::qnorm(1 - (1 - confidence_level) / 2)
    }
    decomposition_quantiles$effect <- relevel(as.factor(decomposition_quantiles$effect), ref = "Observed difference")
    plot <- ggplot(data = decomposition_quantiles, aes(
      x = probs,
      y = value,
      ymin = value - t_value * se,
      ymax = value + t_value * se
    )) +
      geom_hline(yintercept = 0, col = "darkgrey", linewidth = .75) +
      geom_ribbon(alpha = 0.2, col = NA, fill = "red") +
      geom_line(col = "red") +
      geom_point(col = "red") +
      facet_wrap(~effect) +
      labs(y = "Difference", x = "Quantile rank") +
      theme(legend.position = "none")
  } else {
    decomposition_quantiles$effect <- relevel(as.factor(decomposition_quantiles$effect), ref = "Observed difference")
    plot <- ggplot(decomposition_quantiles, aes(probs, value)) +
      geom_hline(yintercept = 0, col = "darkgrey", linewidth = .75) +
      geom_line(col = "red") +
      geom_point(col = "red") +
      facet_wrap(~effect) +
      labs(y = "Difference", x = "Quantile rank") +
      theme(legend.position = "none")
  }

  return(plot)
}




#' Plot decomposition terms for quantiles
#'
#' The function plots decomposition terms for quantiles estimtated
#' with \code{ob_decompose} over the  unit interval.
#'
#' @param x an object of class "ob_decompose", usually, a result of a call to [ob_decompose()] with [statistics = "quantiles"].
#' @param ... other parameters to be passed through to plot function.
#' @param detailed_effects If `TRUE` (default), then the detailed effects are plotted. Otherwise only the total (aggregate) effects are plotted.
#' @param confidence_bands If `TRUE` and if standard errors have been bootstrapped, confidence bands are plotted.
#' @param confidence_level numeric value between 0 and 1 (default = 0.95) that defines the confidence interval
#'              plotted as a ribbon and defined as \code{qnorm((1-confidence_level)/2)} * standard error.
#' @param aggregate_factors boolean, if `TRUE` (default) terms associated with detailed factor
#' levels are aggregated to a single term for every factor variable.
#' @param custom_aggregation list specifying the aggregation of detailed decomposition
#' terms. The parameter `custom_aggregation` overrides the parameter `aggregate_factors`.
#' If `NULL` (default), then either all detailed terms or all terms associated with
#' a single variable are returned.
#'
#' @return a ggplot illustrating the decomposition terms for quantiles.
#' @export
#'
#' @examples
#' data("nlys00")
#'
#' mod1 <- log(wage) ~ age + central_city + msa + region + black +
#'   hispanic + education + afqt + family_responsibility + years_worked_civilian +
#'   years_worked_military + part_time + industry
#'
#' # plotting RIF regression decomposition of deciles
#' \donttest{
#' decompose_rifreg_deciles <- ob_decompose(
#'   formula = mod1,
#'   data = nlys00,
#'   group = female,
#'   reweighting = TRUE,
#'   rifreg_statistic = "quantiles",
#'   bootstrap = TRUE,
#'   bootstrap_iterations = 50,
#'   reference_0 = FALSE
#' )
#'
#' plot(decompose_rifreg_deciles)
#'
#' plot(decompose_rifreg_deciles,
#'   confidence_bands = TRUE
#' )
#' }
#'
#' # plotting Oaxaca-Blinder decomposition
#'
#' decompose_ob_mean <- ob_decompose(
#'   formula = mod1,
#'   data = nlys00,
#'   group = female,
#'   reweighting = TRUE,
#'   bootstrap = FALSE,
#'   reference_0 = FALSE
#' )
#'
#' plot(decompose_ob_mean)
#' plot(decompose_ob_mean, detailed_effects = FALSE)
#'
#'
#' # With custom aggregation
#'
#' custom_aggregation <- list(
#'   `Age, race, region, etc.` = c(
#'     "age",
#'     "blackyes",
#'     "hispanicyes",
#'     "regionNorth-central",
#'     "regionSouth",
#'     "regionWest",
#'     "central_cityyes",
#'     "msayes"
#'   ),
#'   `Education` = c(
#'     "education<10 yrs",
#'     "educationHS grad (diploma)",
#'     "educationHS grad (GED)",
#'     "educationSome college",
#'     "educationBA or equiv. degree",
#'     "educationMA or equiv. degree",
#'     "educationPh.D or prof. degree"
#'   ),
#'   `AFTQ` = "afqt",
#'   `L.T. withdrawal due to family` = "family_responsibility",
#'   `Life-time work experience` = c(
#'     "years_worked_civilian",
#'     "years_worked_military",
#'     "part_time"
#'   ),
#'   `Industrial sectors` = c(
#'     "industryManufacturing",
#'     "industryEducation, Health, Public Admin.",
#'     "industryOther services"
#'   )
#' )
#'
#' plot(decompose_ob_mean, custom_aggregation = custom_aggregation)
#'
plot.ob_decompose <- function(x,
                              ...,
                              detailed_effects = TRUE,
                              aggregate_factors = TRUE,
                              custom_aggregation = NULL,
                              confidence_bands = FALSE,
                              confidence_level = 0.95) {
  if (!is.null(x$input_parameters$rifreg_statistic) &&
    x$input_parameters$rifreg_statistic == "quantiles" &&
    length(x$input_parameters$rifreg_probs) > 1 &&
    !detailed_effects) {
    # Plot for several quantiles
    n_quantiles <- length(x) - 5
    col_names <- c("probs", names(x[[1]][["decomposition_terms"]][-1]))

    na_matrix <- matrix(NA, nrow = n_quantiles, ncol = length(col_names))
    decompose_results <- as.data.frame(na_matrix)
    colnames(decompose_results) <- col_names

    decompose_results$probs <- x$input_parameters$rifreg_probs

    for (i in 1:n_quantiles) {
      decompose_results[i, 2:length(col_names)] <- x[[i]]$decomposition_terms[1, 2:length(col_names)]
    }


    decomposition_quantiles <- stats::reshape(decompose_results,
      idvar = c("probs"),
      times = setdiff(names(decompose_results), c("probs")),
      timevar = "effect",
      varying = list(setdiff(names(decompose_results), c("probs"))),
      direction = "long",
      v.names = "value"
    )

    confidence_bands <- ifelse(confidence_bands &
      x$input_parameters$bootstrap,
    TRUE,
    FALSE
    )
    if (confidence_bands) {
      decompose_se <- as.data.frame(na_matrix)
      colnames(decompose_se) <- col_names

      decompose_se$probs <- x$input_parameters$rifreg_probs

      for (i in 1:n_quantiles) {
        decompose_se[i, 2:length(col_names)] <- x[[i]]$decomposition_vcov$decomposition_terms_se[1, 2:length(col_names)]
      }

      decomposition_quantiles_se <- stats::reshape(decompose_se,
        idvar = c("probs"),
        times = setdiff(names(decompose_se), c("probs")),
        timevar = "effect",
        varying = list(setdiff(names(decompose_se), c("probs"))),
        direction = "long",
        v.names = "se"
      )

      decomposition_quantiles$se <- decomposition_quantiles_se$se


      decomposition_quantiles$t_value <- stats::qnorm(1 - (1 - confidence_level) / 2)

      decomposition_quantiles$effect <- relevel(as.factor(decomposition_quantiles$effect), ref = "Observed_difference")
      plot <- ggplot(data = decomposition_quantiles, aes(
        x = probs,
        y = value,
        fill = effect,
        ymin = value - t_value * se,
        ymax = value + t_value * se
      )) +
        geom_hline(yintercept = 0, col = "darkgrey", linewidth = .75) +
        geom_ribbon(alpha = 0.2, col = NA, fill = "red") +
        geom_line(col = "red") +
        geom_point(col = "red") +
        facet_wrap(~effect) +
        labs(y = "Difference", x = "Quantile rank") +
        theme(legend.position = "none")
    } else {
      decomposition_quantiles$effect <- relevel(as.factor(decomposition_quantiles$effect), ref = "Observed_difference")
      plot <- ggplot(decomposition_quantiles, aes(probs, value, col = effect, shape = effect)) +
        geom_hline(yintercept = 0, col = "darkgrey", linewidth = .75) +
        geom_line() +
        geom_point() +
        labs(y = "Difference", x = "Quantile rank")
    }
  } else {
    # Plot for single statistic and detailed rifreg quantiles

    reweighting <- x$input_parameters$reweighting

    n_probs <- ifelse(!is.null(x$input_parameters$rifreg_statistic) &&
      x$input_parameters$rifreg_statistic == "quantiles" &&
      length(x$input_parameters$rifreg_probs) > 1,
    length(x$input_parameters$rifreg_probs),
    1
    )
    sequence <- 1:n_probs
    decomposition_results_all <- NULL

    for (i in sequence) {
      if (aggregate_factors | !is.null(custom_aggregation)) {
        aggregated_terms <- aggregate_terms(x[[i]],
          aggregate_factors = aggregate_factors,
          custom_aggregation = custom_aggregation,
          reweighting = reweighting
        )

        results <- aggregated_terms$decomposition_terms
      } else {
        results <- x[[i]]$decomposition_terms
      }

      results <- results[ifelse(detailed_effects, -1, 1), ]

      decomposition_results <- stats::reshape(
        data = results,
        idvar = c("Variable"),
        times = setdiff(names(results), c("Variable")),
        timevar = "effect",
        varying = list(setdiff(names(results), c("Variable"))),
        direction = "long",
        v.names = "value"
      )

      if (reweighting) {
        decomposition_results$effect <- factor(decomposition_results$effect,
          levels = c(
            "Observed_difference",
            "Composition_effect",
            "Structure_effect",
            "Specification_error",
            "Reweighting_error"
          )
        )
        levels(decomposition_results$effect) <- gsub("_", " ", levels(decomposition_results$effect))
      } else {
        decomposition_results$effect <- factor(decomposition_results$effect,
          levels = c(
            "Observed_difference",
            "Composition_effect",
            "Structure_effect"
          )
        )
        levels(decomposition_results$effect) <- gsub("_", " ", levels(decomposition_results$effect))
      }
      if (n_probs > 1) {
        decomposition_results$probs <- x$input_parameters$rifreg_probs[i]
        decomposition_results_all <- rbind(decomposition_results_all, decomposition_results)
      }
    }

    if (n_probs == 1) {
      plot <- ggplot(decomposition_results, aes(x = effect, y = value, fill = Variable)) +
        geom_hline(yintercept = 0, col = "darkgrey", linewidth = .75) +
        geom_bar(stat = "identity", position = "stack") +
        xlab("Effect") +
        ylab("Difference") +
        labs(fill = "Variable") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
    } else {
      plot <- ggplot(decomposition_results_all, aes(x = probs, y = value, fill = Variable)) +
        geom_hline(yintercept = 0, col = "darkgrey", linewidth = .75) +
        geom_bar(stat = "identity", position = "stack") +
        facet_wrap(~effect) +
        xlab("Quantile rank") +
        ylab("Difference") +
        labs(fill = "Variable")
    }
  }
  return(plot)
}
