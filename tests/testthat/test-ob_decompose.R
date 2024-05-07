test_that("ob_decompose() does not throw an error", {
  set.seed(43825081)
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  # Using female coefficients (reference_0 = TRUE) to estimate counterfactual mean
  decompose_female_as_reference <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE
  )
  testthat::expect_error(decompose_female_as_reference, NA)
})


#
# test_that("ob_decompose() provides expected output", {
#
#   set.seed(43825081)
#   library("AER")
#   data("CPS1985")
#   formula <- wage ~ education + experience + union
#   data_used <- CPS1985
#   data_used <- get_all_vars(formula, data_used, group=gender)
#   data_used$union <- ifelse(data_used$union == "no", 0, 1)
#
#   decompose_results <- ob_decompose(formula = formula,
#                           data = data_used,
#                           group = group,
#                           subtract_1_from_0 = TRUE)
#
#
#   ## manual calculation
#   data_female <- data_used[data_used$group == "female", ]
#   data_male <- data_used[data_used$group == "male", ]
#
#   # regression coefs
#   reg_female <- lm(formula, data_female)
#   reg_male <- lm(formula, data_male)
#
#   # group means
#   mean_female <- colMeans(data_female[, 1:4])
#   mean_male <- colMeans(data_male[, 1:4])
#
#   # check that regression outputs are the same
#   testthat::expect_equal(decompose_results$ob_decompose$model_fits$fit_group_0$coefficients,
#                          reg_male$coefficients)
#   testthat::expect_equal(decompose_results$ob_decompose$model_fits$fit_group_0$residuals,
#                          reg_male$residuals)
#   testthat::expect_equal(decompose_results$ob_decompose$model_fits$fit_group_0$fitted.values,
#                          reg_male$fitted.values)
#
#   testthat::expect_equal(decompose_results$ob_decompose$model_fits$fit_group_1$coefficients,
#                          reg_female$coefficients)
#   testthat::expect_equal(decompose_results$ob_decompose$model_fits$fit_group_1$residuals,
#                          reg_female$residuals)
#   testthat::expect_equal(decompose_results$ob_decompose$model_fits$fit_group_1$fitted.values,
#                          reg_female$fitted.values)
#
#
#   # check that means are the same
#
#   # check that overall diff is as expected
#   overall_diff <- mean_male[1] - mean_female[1]
#
#   female_observed <- sum(mean_female[2:4] * reg_female$coefficients[2:4],
#                          reg_female$coefficients[1])
#   testthat::expect_equal(female_observed, mean_female[[1]])
#
#   male_observed <- sum(mean_male[2:4] * reg_male$coefficients[2:4],
#                          reg_male$coefficients[1])
#   testthat::expect_equal(male_observed, mean_male[[1]])
#   testthat::expect_equal(overall_diff[[1]],male_observed -  female_observed)
#   testthat::expect_equal(overall_diff[[1]], decompose_results$ob_decompose$decomposition_terms$Observed_difference[1])
#
#
#   # check that decomposition results are as expected
#   female_counterfactual <- sum(mean_female[2:4] * reg_male$coefficients[2:4],
#                              reg_male$coefficients[1])
#
#   unexplained <- female_counterfactual - female_observed
#   explained <- male_observed -female_counterfactual
#
#   testthat::expect_equal(decompose_results$ob_decompose$decomposition_terms$Composition_effect[1],
#                          explained)
#   testthat::expect_equal(decompose_results$ob_decompose$decomposition_terms$Structure_effect[1],
#                          unexplained)
# })


# test_that("ob_decompose() analytical and bootstrapped se are the same", {
#
#   set.seed(43825081)
#   library("AER")
#   data("CPS1985")
#   formula <- log(wage) ~ education + experience + union + ethnicity
#   data_used <- CPS1985
#   data_used$weights <- runif(nrow(CPS1985), 0.5, 1.5)
#   data_used <- get_all_vars(formula, data_used, weights=weights, group=gender)
#
#   decompose_analytical_se <- ob_decompose(formula = formula,
#                                 data = data_used,
#                                 group = group)
#   decompose_bootstrapped_se <- ob_decompose(formula = formula,
#                                   data = data_used,
#                                   group = group,
#                                   bootstrap = TRUE,
#                                   bootstrap_iterations = 400)
#
#   testthat::expect_equal(decompose_analytical_se$ob_decompose$decomposition_vcov$decomposition_terms_se,
#                          decompose_bootstrapped_se$ob_decompose$decomposition_vcov$decomposition_terms_se[1:4],
#                          tolerance = 0.04)
# })


test_that("reweighted ob decomposition does not throw an error", {
  set.seed(43825081)
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  reweighted_decompose_results <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    reweighting = TRUE
  )

  testthat::expect_error(reweighted_decompose_results, NA)
})

test_that("reweighted ob decomposition aggregate results are as expected", {
  set.seed(43825081)
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  reweighted_decompose_results <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    reweighting = TRUE
  )

  decompose_results <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    reweighting = FALSE
  )

  testthat::expect_equal(
    reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Observed_difference,
    decompose_results$ob_decompose$decomposition_terms$Observed_difference
  )
  testthat::expect_equal(
    reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Composition_effect +
      reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Specification_error +
      reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Structure_effect +
      reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Reweighting_error,
    decompose_results$ob_decompose$decomposition_terms$Observed_difference
  )
})

test_that("reweighted ob decomposition aggregate results are as expected with male reference", {
  set.seed(43825081)
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  reweighted_decompose_results <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = FALSE,
    reweighting = TRUE
  )

  decompose_results <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = FALSE,
    reweighting = FALSE
  )

  testthat::expect_equal(
    reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Observed_difference,
    decompose_results$ob_decompose$decomposition_terms$Observed_difference
  )
  testthat::expect_equal(
    reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Composition_effect +
      reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Specification_error +
      reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Structure_effect +
      reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Reweighting_error,
    decompose_results$ob_decompose$decomposition_terms$Observed_difference
  )
})


test_that("reweighted ob decomposition aggregate results are as expected with normalization", {
  set.seed(43825081)
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  reweighted_decompose_results <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    reweighting = TRUE,
    normalize_factors = TRUE
  )

  decompose_results <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    reweighting = FALSE,
    normalize_factors = TRUE
  )

  testthat::expect_equal(
    reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Observed_difference,
    decompose_results$ob_decompose$decomposition_terms$Observed_difference
  )
  testthat::expect_equal(
    reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Composition_effect +
      reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Specification_error +
      reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Structure_effect +
      reweighted_decompose_results$reweighted_ob_decompose$decomposition_terms$Reweighting_error,
    decompose_results$ob_decompose$decomposition_terms$Observed_difference
  )
})


test_that("reweighted ob decomposition computes bootstrap SE", {
  set.seed(43825081)
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  reweighted_decompose_results <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    reweighting = TRUE,
    normalize_factors = TRUE,
    bootstrap = TRUE
  )

  testthat::expect_error(reweighted_decompose_results, NA)
})

test_that("rifreg decomposition does not throw an error", {
  set.seed(43825081)
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  rifreg_decompose_results <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    reweighting = TRUE,
    rifreg_statistic = "quantiles",
    rifreg_probs = 0.5,
    normalize_factors = TRUE,
    bootstrap = FALSE
  )

  testthat::expect_error(rifreg_decompose_results, NA)
})

test_that("rifreg decomposition does not throw an error with bootstrap", {
  set.seed(43825081)
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  rifreg_decompose_results <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    reweighting = TRUE,
    rifreg_statistic = "quantiles",
    rifreg_probs = 0.5,
    normalize_factors = TRUE,
    bootstrap = TRUE
  )

  testthat::expect_error(rifreg_decompose_results, NA)
})

test_that("rifreg decomposition does not throw an error with multiple quantiles", {
  set.seed(43825081)
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  rifreg_decompose_results <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    reweighting = TRUE,
    rifreg_statistic = "quantiles",
    rifreg_probs = c(1:9) / 10,
    normalize_factors = TRUE,
    bootstrap = FALSE
  )

  testthat::expect_error(rifreg_decompose_results, NA)
})

test_that("rifreg decomposition does not throw an error with multiple quantiles and bootstrap", {
  set.seed(43825081)
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  rifreg_decompose_results <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    reweighting = TRUE,
    rifreg_statistic = "quantiles",
    rifreg_probs = c(1:9) / 10,
    normalize_factors = TRUE,
    bootstrap = TRUE,
    bootstrap_iterations = 5
  )

  testthat::expect_error(rifreg_decompose_results, NA)
})
