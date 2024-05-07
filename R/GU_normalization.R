#' Gardeazabal and Ugidos normalization of factor variables
#'
#' The function performs the normalization of the factor variables
#' proposed by Gardeazabal and Ugidos (2004, GU) to estimate detailed decompositions
#' that do not depend on the chosen reference levels of the factor variables.
#'
#' @param formula an object of class "formula". See \link[stats]{lm} for further details.
#' @param data a data frame containing the variables in the model.
#' @param weights numeric vector of non-negative observation weights, hence of same length as \code{dep_var}.
#' @param group name of the a binary variable (numeric or factor) identifying the two groups that will be compared.
#'
#' @return a list containing the adjusted formula, adjusted data, adjusted coefficient names,
#' and the normalized regressors for prediction and the
#'
#' @references Gardeazabal, Javier, and Arantza Ugidos. 2004. "More on identification in detailed wage decompositions."
#' \emph{Review of Economics and Statistics} 86(4): 1034-1036.
#'
#'
#' @export
#' @examples
#' data("men8305")
#' mod1 <- log(wage) ~ union + married + nonwhite + education + experience
#' normalized_data <- GU_normalization(
#'   formula = mod1,
#'   data = men8305,
#'   weights = weights,
#'   group = year
#' )
#'
GU_normalization <- function(formula, data, weights, group) {
  function_call <- match.call()
  data_arguments_index <- match(c("formula", "data", "weights", "group"), names(function_call), 0)
  data_arguments <- function_call[c(1, data_arguments_index)]

  # Assign formula to model.frame attributes
  data_arguments[[1]] <- as.name("model.frame")
  data_arguments$formula <- formula
  data_used <- eval.parent(data_arguments)
  function_terms <- attr(data_used, "terms")

  # Stop if there are interactions terms:
  if (sum(attr(function_terms, "order")) > length(attr(function_terms, "order"))) {
    stop("GU normalization does not allow interactions.")
  }
  message("\nFactor variables are normalized as proposed by Gardeazabal & Ugidos (2004)\n")

  term_labels <- attr(function_terms, "term.labels")

  unadjusted_regressors <- data_used[, -c(1, which(names(data_used) %in% c("(weights)", "(group)")))]
  if (is.null(nrow(unadjusted_regressors))) {
    unadjusted_regressors <- data.frame(unadjusted_regressors)
    names(unadjusted_regressors) <- term_labels
  }

  n_obs <- nrow(unadjusted_regressors)
  n_regressors <- ncol(unadjusted_regressors)
  if (n_regressors == 0) {
    stop("GU normalization requires a model with at least one variable regressor.")
  }
  adjusted_regressors <- data.frame(matrix(nrow = n_obs, ncol = 0))
  regressors_for_prediction <- matrix(nrow = n_obs, ncol = 0)

  if (attr(function_terms, "intercept") == 1) {
    # regressors_for_prediction <- model.matrix(object = stats::update(formula, . ~ . - 1), data =  data)
    regressors_for_prediction <- cbind(rep(1, nrow(regressors_for_prediction)), regressors_for_prediction)
    colnames(regressors_for_prediction)[1] <- "(Intercept)"
  }


  adjusted_coefficient_names <- list()

  for (i in 1:n_regressors) {
    regressor_i <- unadjusted_regressors[, i]
    regressor_name_i <- colnames(unadjusted_regressors)[i]

    # if it is a factor variable: normalize
    if (is.factor(regressor_i)) {
      formula_i <- as.formula(paste("~", regressor_name_i, "+ 0", sep = ""))
      mod_matrix_i <- model.matrix(formula_i, unadjusted_regressors)
      regressors_for_prediction <- cbind(regressors_for_prediction, mod_matrix_i)

      # adjusted_coefficient_names_i <- colnames(mod_matrix_i) <- make.names(colnames(mod_matrix_i))
      adjusted_coefficient_names_i <- colnames(mod_matrix_i)
      mod_matrix_i <- mod_matrix_i[, 2:ncol(mod_matrix_i)] - mod_matrix_i[, 1]
      mod_matrix_i <- as.data.frame(mod_matrix_i)
      names(mod_matrix_i) <- adjusted_coefficient_names_i[-1]
      adjusted_regressors <- cbind(adjusted_regressors, mod_matrix_i)

      term_labels <- c(
        term_labels[-pmatch(
          grep(regressor_name_i,
            term_labels,
            value = TRUE
          ),
          term_labels
        )],
        adjusted_coefficient_names_i[-1]
      )


      select_to_add_parenthesis <- which(make.names(adjusted_coefficient_names_i) != adjusted_coefficient_names_i)
      adjusted_coefficient_names_i[select_to_add_parenthesis] <- paste0("`", adjusted_coefficient_names_i[select_to_add_parenthesis], "`")
      adjusted_coefficient_names[[i]] <- adjusted_coefficient_names_i
      names(adjusted_coefficient_names)[i] <- regressor_name_i

    } else {
      # if not: pass unadjusted variable through
      adjusted_regressors <- cbind(adjusted_regressors, regressor_i)
      regressors_for_prediction <- cbind(regressors_for_prediction, regressor_i)

      colnames(adjusted_regressors)[ncol(adjusted_regressors)] <- regressor_name_i
      colnames(regressors_for_prediction)[ncol(regressors_for_prediction)] <- regressor_name_i
      names(adjusted_coefficient_names)[i] <- adjusted_coefficient_names[[i]] <- regressor_name_i
    }
  }

  adjusted_data <- as.data.frame(cbind(
    data[, 1],
    adjusted_regressors,
    data[, which(names(data) %in% c("weights", "group"))]
  ))
  names(adjusted_data)[1] <- names(data)[1]


  adjusted_formula <- update(formula, as.formula(paste0(". ~ ", paste0(paste0("`", term_labels, "`"), collapse = " + "))))

  if (attr(function_terms, "intercept") == 1) {
    adjusted_coefficient_names <- c(list(`(Intercept)` = "(Intercept)"), adjusted_coefficient_names)
  }

  return(list(
    formula = adjusted_formula,
    data = adjusted_data,
    regressors_for_prediction = regressors_for_prediction,
    adjusted_coefficient_names = adjusted_coefficient_names
  ))
}



#' Sum coefficients for GU normalization
#'
#' This function sums the coefficients of a single factor variable to construct
#' an additional coefficient for the left-out reference level.
#'
#' @param coef_names names of the dummy coefficients of a factor variable
#' @param est_coef estimated coefficient vector
#'
GU_normalization_sum_coefficients <- function(coef_names, est_coef) {
  est_coef <- est_coef[which(names(est_coef) %in% coef_names)]
  if (length(coef_names) > 1) {
    coefficient_reference_level <- -sum(est_coef)
    est_coef <- c(coefficient_reference_level, est_coef)
    names(est_coef)[1] <- coef_names[1]
  }
  return(est_coef)
}


#' Get coefficients for GU normalization
#'
#' This function constructs sums the coefficients of each factor variable to
#' construct a additional coefficients for their originally left-out reference
#' levels and adds them to the estimated coefficients vector.
#'
#' @param coef_names list with coefficients of every factor variable that need to be adjusted
#' @param est_coef vector of estimated coefficients
#'
GU_normalization_get_coefficients <- function(coef_names, est_coef) {
  est_coef <- do.call("c", lapply(coef_names,
    GU_normalization_sum_coefficients,
    est_coef = est_coef
  ))
  if (!all(!grepl("\\.", names(coef_names)))) {
    stop("Category names of covariates cannot contain dots (.) in GU_normalization.
         Please remove dots from the category names of all covariates you want to normalize. ")
  }
  names(est_coef) <- sapply(strsplit(names(est_coef), split = "\\."), function(x) x[2])
  return(est_coef)
}


#' Sum covariance matrix for GU normalization
#'
#' This function adjusts the covariance matrix for the additional coefficient
#' of the originally left-out reference level of a single factor variable.
#'
#' @param coef_names names of the dummy coefficients of a factor variable
#' @param Cov_beta estimated covariance matrix of the regression coefficients
#'
GU_normalization_sum_vcov <- function(coef_names, Cov_beta) {
  if (length(coef_names) > 1) {
    Cov_beta_adjusted <- matrix(NA, nrow = nrow(Cov_beta) + 1, ncol = ncol(Cov_beta) + 1)
    index_factors <- which(colnames(Cov_beta) %in% coef_names)
    ri <- range(index_factors)

    # Column/row indices of coefficients ranked before reference level of factor  to be added
    if (ri[1] != 1) {
      index_lower <- 1:(ri[1] - 1)
    } else {
      index_lower <- NULL
    }
    # Column/row indices of coefficients ranked after reference level of factor variable to be added
    index_greater <- ri[1]:ncol(Cov_beta)

    # Covariances and variance of coefficient of reference level to be added
    # b0 = - b1 - ... - bk --> Cov(c1, b0) = -Cov(g1, b1) - ... - Cov(c1, bk)
    cov_reference_level_i <- -rowSums(as.matrix(Cov_beta[, index_factors]))
    var_reference_level_i <- sum(Cov_beta[index_factors, index_factors]) #-sum(cov_reference_level_i[index_factors]) #

    cov_reference_level_i <- c(
      cov_reference_level_i[index_lower],
      var_reference_level_i,
      cov_reference_level_i[index_greater]
    )

    names(cov_reference_level_i)[ri[1]] <- coef_names[1]

    Cov_beta_adjusted[ri[1], ] <- cov_reference_level_i
    Cov_beta_adjusted[, ri[1]] <- cov_reference_level_i

    Cov_beta_adjusted[index_lower, index_lower] <- Cov_beta[index_lower, index_lower]
    Cov_beta_adjusted[index_lower, index_greater + 1] <- Cov_beta[index_lower, index_greater]
    Cov_beta_adjusted[index_greater + 1, index_lower] <- Cov_beta[index_greater, index_lower]
    Cov_beta_adjusted[index_greater + 1, index_greater + 1] <- Cov_beta[index_greater, index_greater]

    rownames(Cov_beta_adjusted) <- colnames(Cov_beta_adjusted) <- names(cov_reference_level_i)
  } else {
    Cov_beta_adjusted <- Cov_beta
  }
  return(Cov_beta_adjusted)
}

#' Get covariance matrix for GU normalization
#'
#' This function adjusts the covariance matrix for the additional coefficients
#' of the originally left-out reference levels of all factor variable.
#'
#' @param coef_names list with coefficients of every factor variable that need to be adjusted
#' @param Cov_beta estimated covariance matrix of the regression coefficients
#'
GU_normalization_get_vcov <- function(coef_names, Cov_beta) {
  for (i in 1:length(coef_names)) {
    Cov_beta <- GU_normalization_sum_vcov(coef_names = coef_names[[i]], Cov_beta = Cov_beta)
  }
  return(Cov_beta)
}
