#' print method for class "dfl_decompose"
#'
#' @param x an object of class "dfl_decompose", usually , a result of a call to [dfl_decompose()].
#' @param ... other parameters to be passed through to printing functions.
#'
#' @return The function \code{print.dfl_decompose()} displays the decompositions terms saved in \code{x}.
#'
#' @export
#'
print.dfl_decompose <- function(x, ...) {
  if (!x$subtract_1_from_0) {
    cat(
      "Decomposition of difference between",
      paste0(x$group_variable_name, " == '", x$group_variable_levels[2], "'"),
      "(group 1) and\n",
      paste0(x$group_variable_name, " == '", x$group_variable_levels[1], "'"),
      "(group 0)\n\n"
    )
  } else {
    cat(
      "Decomposition of difference between",
      paste0(x$group_variable_name, " == '", x$group_variable_levels[1], "'"),
      "(group 0) and\n",
      paste0(x$group_variable_name, " == '", x$group_variable_levels[2], "'"),
      "(group 1)\n\n"
    )
  }

  cat("Reweighted reference group:", paste0(x$group_variable_name, " == '", x$reference_group, "' (group ", ifelse(x$group_variable_levels[1] == x$reference_group, 0, 1), ")"), "\n \n")

  if (!is.null(x$decomposition_quantiles)) {
    cat("Decomposition of difference at conditional quantiles:\n\n")
    print(x$decomposition_quantiles[, -1])
    cat("\n")
  }
  if (!is.null(x$decomposition_other_statistics)) {
    if ("Gini of untransformed Y (=exp(log(Y)))" %in% x$decomposition_other_statistics$statistic) {
      select_row <- which(x$decomposition_other_statistics$statistic == "Gini of untransformed Y (=exp(log(Y)))")
      rownames(x$decomposition_other_statistics)[select_row] <- x$decomposition_other_statistics[select_row, "statistic"] <- "Gini*"
      legend_to_table <- "*Gini of untransformed Y (=exp(log(Y)))\n\n"
    } else {
      legend_to_table <- NULL
    }

    cat("Decomposition of differences in other statistics\n\n")
    print(x$decomposition_other_statistics[, -1])
    cat("\n")
    if (!is.null(legend_to_table)) {
      cat(legend_to_table)
      cat("\n")
    }
  }
}


#' print method for class "ob_decompose"
#'
#' @param x an object of class "ob_decompose", usually , a result of a call to [ob_decompose()].
#' @param ... other parameters to be passed through to printing functions.
#'
#' @return The function \code{print.ob_decompose()} displays the decompositions terms saved in \code{x}.
#'
#' @export
#'
print.ob_decompose <- function(x, ...) {
  reweighting <- x$input_parameters$reweighting

  if (is.null(x$input_parameters$rifreg_statistic)) {
    if (!reweighting) {
      decomposition_type <- "\n\nOaxaca-Blinder decomposition of mean difference\nbetween"
    } else {
      decomposition_type <- "\n\nDobuly robust Oaxaca-Blinder decomposition of mean difference\nbetween"
    }
  } else {
    if (!reweighting) {
      decomposition_type <- paste0(
        "\n\nRIF regression decomposition of difference in ",
        x$input_parameters$rifreg_statistic,
        "\nbetween"
      )
    } else {
      decomposition_type <- paste0(
        "\n\nReweighted RIF regression decomposition of difference in ",
        x$input_parameters$rifreg_statistic,
        "\nbetween"
      )
    }
  }
  cat(
    decomposition_type,
    paste0(x$group_variable_name, " == '", x$group_variable_levels[2], "'"),
    "(group 1) and",
    paste0(x$group_variable_name, " == '", x$group_variable_levels[1], "'"),
    "(group 0). \nThe reference group is", paste0("'", x$reference_group, "'."), "\n\n"
  )


  n_decompositions <- length(x) - 5

  for (i in 1:n_decompositions) {
    if (!is.null(x$input_parameters$rifreg_statistic) &&
      x$input_parameters$rifreg_statistic == "quantiles") {
      cat("\n*** Quantile:", x$input_parameters$rifreg_probs[i], "***")
      cat("\n\n")
    }

    decomposition_terms <- x[[i]]["decomposition_terms"][["decomposition_terms"]][, -1]

    names(decomposition_terms) <- gsub("_", " ", names(decomposition_terms))
    aggregate_decomposition <- decomposition_terms[1, ]
    detailed_decomposition <- decomposition_terms[-1, ]
    rownames(aggregate_decomposition) <-
      paste0(
        "Total difference ",
        paste0(rep(" ", max(c(max(nchar(rownames(detailed_decomposition))) -
          nchar("Total difference "), 0))), collapse = "")
      )
    cat("Aggregate decomposition:\n\n")
    print(aggregate_decomposition, ...)
    cat("\n")
    cat("Detailed decomposition:\n\n")
    print(detailed_decomposition, ...)
    cat("\n")
  }
}
