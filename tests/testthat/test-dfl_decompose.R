# test_that("Replicate FFL 2011, table 5, p. 69", {
#
#   # Full data set
#   load("data-raw/men8305_full.rda")
#   men8305_full$weights <- men8305_full$weights/sum(men8305_full$weights) * length(men8305_full$weights)
#
#   flf_model <- log(wage) ~ union*(education + experience) + education*experience
#
#   # Reweighting sample from 1983-85
#   flf_male_inequality  <- dfl_decompose(flf_model,
#                                    data = men8305_full,
#                                    weights = weights,
#                                    group = year)
#
#   FFL_results <- data.frame(effect=c("Observed difference", "Composition effect", "Structure effect"),
#                             p90p10=c(0.1091, 0.0756, 0.0336),
#                             p90p50=c(0.1827, 0.0191, 0.1637),
#                             p50p10=c(-0.0736, 0.0565, -0.1301),
#                             variance = c(0.0617, 0.0208, 0.0408))[,-1]
#   FFL_results <- t(as.matrix(FFL_results))
#   colnames(FFL_results) <- c("Observed difference", "Composition effect", "Structure effect")
#   estimated_statistics <- flf_male_inequality$decomposition_other_statistics[c(4,5,6,2), c(2, 4, 3)]
#   estimated_statistics <- as.matrix(estimated_statistics)
#   rownames(estimated_statistics) <- rownames(FFL_results)
#
#   expect_equal(FFL_results, estimated_statistics, tolerance = 0.01)
# })






test_that("dfl_decompose() does not throw an error", {
  set.seed(89342395)

  model_sequential <- log(wage) ~ union + experience + education | experience + education | education

  decompose_results <- dfl_decompose(model_sequential,
    data = men8305[1:1000, ],
    weights = weights,
    group = year
  )

  testthat::expect_error(decompose_results, NA)
})


test_that("dfl_decompose() does not return fitted models if required", {
  set.seed(89342395)

  model_sequential <- log(wage) ~ union + experience + education | experience + education | education

  decompose_results <- dfl_decompose(model_sequential,
    data = men8305[1:1000, ],
    weights = weights,
    group = year,
    return_model = FALSE
  )

  testthat::expect_error(decompose_results, NA)
})

test_that("bootstrapping dfl_decompose() does not throw an error", {
  flf_model <- log(wage) ~ union + experience + education

  set.seed(123)
  decompose_results_boot_single_core <- dfl_decompose(flf_model,
    data = men8305[1:1000, ],
    weights = weights,
    group = year,
    bootstrap = TRUE,
    bootstrap_iterations = 10,
    cores = 1
  )
  # # Does not work in check()
  # set.seed(123)
  # decompose_results_boot_parallel  <- dfl_decompose(flf_model,
  #                                         data = men8305[1:1000, ],
  #                                         weights = weights,
  #                                         group = year,
  #                                         bootstrap = TRUE,
  #                                         bootstrap_iterations = 10,
  #                                         cores = 2)

  testthat::expect_error(decompose_results_boot_single_core, NA)
})

test_that("dfl_decompose_estimate() does not throw an error", {
  set.seed(89342395)

  data_sample <- men8305[sample(1:nrow(men8305), size = 10000), ]
  formula <- Formula::as.Formula(log(wage) ~ union * (education + experience) + education * experience)
  data_used <- model.frame(formula, data_sample, weights = weights, group = year)
  dep_var <- model.response(data_used, "numeric")

  weights <- model.weights(data_used)
  group_variable_name <- "year"
  group_variable <- data_used[, ncol(data_used)]
  names(data_used)[ncol(data_used)] <- "group_variable"

  reference_0 <- TRUE
  reference_group <- ifelse(reference_0, 0, 1)

  statistics <- c("quantiles", "mean", "variance", "gini", "iq_range_p90_p10", "iq_range_p90_p50", "iq_range_p50_p10")
  probs <- c(1:9) / 10
  trimming <- TRUE
  trimming_threshold <- 0.00005
  right_to_left <- TRUE
  method <- "logit"

  decompose_results <- dfl_decompose_estimate(
    formula = formula,
    dep_var = dep_var,
    data_used = data_used,
    weights = weights,
    group_variable = group_variable,
    reference_group = reference_group,
    method = method,
    estimate_statistics = TRUE,
    statistics = statistics,
    custom_statistic_function = NULL,
    estimate_normalized_difference = TRUE,
    probs = probs,
    right_to_left = right_to_left,
    trimming = trimming,
    trimming_threshold = trimming_threshold,
    return_model = TRUE
  )

  testthat::expect_error(decompose_results, NA)
})


test_that("fit_and_predict_probabilities works properly with fastglm estimation", {
  set.seed(89342395)

  data_sample <- men8305[sample(1:nrow(men8305), size = 10000), ]
  formula <- Formula::as.Formula(log(wage) ~ union + education + experience)
  data_used <- model.frame(formula, data_sample, weights = weights, group = year)
  dep_var <- model.response(data_used, "numeric")

  weights <- model.weights(data_used)
  group_variable_name <- "year"
  group_variable <- data_used[, ncol(data_used)]
  names(data_used)[ncol(data_used)] <- "group_variable"


  formula_new <- group_variable ~ union + education + experience

  method <- "fastglm"
  fits_fastglm <- fit_and_predict_probabilities(formula_new,
    data_used,
    weights,
    method = method,
    return_model = TRUE,
    newdata = NULL
  )

  method <- "logit"
  fits_logit <- fit_and_predict_probabilities(formula_new,
    data_used,
    weights,
    method = method,
    return_model = TRUE,
    newdata = NULL
  )

  testthat::expect_equal(fits_fastglm[[1]][1:30], fits_logit[[1]][1:30], tolerance = 0.0001)
})



test_that("fit_and_predict_probabilities works properly with ranger random forests estimation", {
  set.seed(89342395)

  data_sample <- men8305[sample(1:nrow(men8305), size = 10000), ]
  formula <- Formula::as.Formula(log(wage) ~ union + education + experience)
  data_used <- model.frame(formula, data_sample, weights = weights, group = year)
  dep_var <- model.response(data_used, "numeric")

  weights <- model.weights(data_used)
  group_variable_name <- "year"
  group_variable <- data_used[, ncol(data_used)]
  names(data_used)[ncol(data_used)] <- "group_variable"

  method <- "random_forest"
  formula_new <- group_variable ~ union + education + experience

  fits <- fit_and_predict_probabilities(formula_new,
    data_used,
    weights,
    method = method,
    return_model = TRUE,
    newdata = NULL
  )

  testthat::expect_error(fits, NA)
})


test_that("dfl_decompose_estimate() works properly with ranger random forests estimation", {
  set.seed(89342395)

  data_sample <- men8305[sample(1:nrow(men8305), size = 10000), ]
  formula <- Formula::as.Formula(log(wage) ~ union + education + experience)
  data_used <- model.frame(formula, data_sample, weights = weights, group = year)
  dep_var <- model.response(data_used, "numeric")

  weights <- model.weights(data_used)
  group_variable_name <- "year"
  group_variable <- data_used[, ncol(data_used)]
  names(data_used)[ncol(data_used)] <- "group_variable"

  reference_0 <- TRUE
  reference_group <- ifelse(reference_0, 0, 1)

  statistics <- c("quantiles", "mean", "variance", "gini", "iq_range_p90_p10", "iq_range_p90_p50", "iq_range_p50_p10")
  probs <- c(1:9) / 10
  trimming <- FALSE
  trimming_threshold <- NULL
  right_to_left <- TRUE
  method <- "random_forest"

  decompose_results <- dfl_decompose_estimate(
    formula = formula,
    dep_var = dep_var,
    data_used = data_used,
    weights = weights,
    group_variable = group_variable,
    reference_group = reference_group,
    method = method,
    estimate_statistics = TRUE,
    statistics = statistics,
    custom_statistic_function = NULL,
    estimate_normalized_difference = TRUE,
    probs = probs,
    right_to_left = right_to_left,
    trimming = trimming,
    trimming_threshold = trimming_threshold,
    return_model = TRUE
  )

  testthat::expect_error(decompose_results, NA)
})



test_that("dfl_decompose() does not throw an error without estimating statistics", {
  set.seed(89342395)
  formula <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  decompose_results <- dfl_decompose(
    formula = formula,
    data = nlys00[1:300, ],
    weights = runif(nrow(nlys00[1:300, ]), 0.5, 1.5),
    group = female,
    estimate_statistics = FALSE
  )

  testthat::expect_error(decompose_results, NA)
})



test_that("select_observations_to_be_trimmed() trimms observations as intended", {
  set.seed(123)
  expected_trimmed_observations <- c(1:5, 101:102)
  reweighting_factor <- rep(1 / 100, 200)
  group_variable <- as.factor(rep(c("no", "yes"), each = 100))
  group <- 0
  reweighting_factor[expected_trimmed_observations] <- 20
  trimming_threshold <- NULL

  trimmed_observations <- select_observations_to_be_trimmed(
    reweighting_factor,
    group_variable,
    group,
    trimming_threshold
  )

  testthat::expect_equal(
    trimmed_observations,
    expected_trimmed_observations
  )
})

test_that("dfl_decompose() trimms estimated factors as expected", {
  set.seed(123)
  data_used <- data.frame(
    x = c(
      c(1, 1, rep(2, 98)),
      c(rep(1, 90), rep(2, 10))
    ),
    group = rep(c(0, 1), each = 100)
  )
  data_used$y <- data_used$x + data_used$group + rnorm(200, 0, 0.5)
  data_used$x <- as.factor(data_used$x)
  levels(data_used$x) <- c("A", "B")
  data_used$group <- as.factor(data_used$group)
  expected_trimmed_observations <- which(data_used$x == "A")

  # p_g_given_X <- data_used %>%
  #   dplyr::group_by(x) %>%
  #   dplyr::summarise(p = mean(group == "1")) %>%
  #   dplyr::pull(p)
  #
  # data_used$psi <- p_g_given_X[1]/(1-p_g_given_X[1])
  # data_used[which(data_used$x == "B"), "psi"] <-   p_g_given_X[2]/(1-p_g_given_X[2])
  #

  # reweighting_factor <-   data_used$psi
  # group_variable <- data_used$group
  # group <- 0
  # trimming_threshold = NULL
  #
  # trimmed_observations <- select_observations_to_be_trimmed(reweighting_factor,
  #                                                           group_variable,
  #                                                           group,
  #                                                           trimming_threshold)
  # decompose_results_untrimmed <- dfl_decompose(y ~ x,
  #                                   data_used,
  #                                   group = group)

  decompose_results_trimmed <- dfl_decompose(y ~ x,
    data_used,
    group = group,
    trimming = TRUE
  )

  testthat::expect_equal(
    decompose_results_trimmed$trimmed_observations,
    expected_trimmed_observations
  )
})


test_that("get_distributional_statistics() does not throw an error with custom_statistic_function", {
  data_sample <- men8305
  formula <- Formula::as.Formula(log(wage) ~ union * (education + experience) + education * experience)
  data_used <- model.frame(formula, data_sample, weights = weights, group = year)
  dep_var <- model.response(data_used, "numeric")

  weights <- model.weights(data_used)
  group_variable_name <- "year"
  group_variable <- data_used[, ncol(data_used)]
  names(data_used)[ncol(data_used)] <- "group_variable"

  reference_0 <- TRUE
  reference_group <- ifelse(reference_0, 0, 1)

  statistics <- c("quantiles", "mean", "variance", "gini", "iq_range_p90_p10", "iq_range_p90_p50", "iq_range_p50_p10")
  probs <- c(1:9) / 10
  log_transformed <- TRUE

  top_share <- function(dep_var,
                        weights,
                        top_percent = 0.1) {
    threshold <- Hmisc::wtd.quantile(dep_var, weights = weights, probs = 1 - top_percent)
    share <- sum(weights[which(dep_var > threshold)] * dep_var[which(dep_var > threshold)]) / sum(weights * dep_var)
    return(share)
  }

  direct_estimation <- top_share(dep_var[which(group_variable == "2003-2005")], weights[which(group_variable == "2003-2005")], 0.1)

  nu1 <- get_distributional_statistics(
    dep_var = dep_var,
    weights = weights,
    group_variable = group_variable,
    group = 1,
    statistics = statistics,
    custom_statistic_function = top_share,
    probs = probs,
    log_transformed = log_transformed
  )

  testthat::expect_equal(direct_estimation, as.numeric(nu1[length(nu1)]), tolerance = 0.000001)
})




# test_that("glm and surveyglm return the same coefficients", {
#   set.seed(123)
#   data("nlys00")
#   formula <- female ~ education + afqt + industry + family_responsibility + part_time
#   data_used <- get_all_vars(formula, nlys00)
#   data_used$weights <- runif(nrow(data_used), 0.5, 1.5)
#
#   design <- survey::svydesign(~0,
#                               data=data_used,
#                               weights=~weights)
#   model_fit_surveyglm <- survey::svyglm(formula,
#                                         data=data_used,
#                                         design=design,family=quasibinomial(link="logit"))
#
#   model_fit_glm <- glm(formula, data = data_used, weights = weights, family = binomial(link = "logit"))
#   model_fit_quasiglm <- glm(formula, data = data_used, weights = weights, family = quasibinomial(link = "logit"))
#
#   sglm <- summary(model_fit_glm)
#   squasiglm <- summary(model_fit_quasiglm)
#   ssurveryglm <- summary(model_fit_surveyglm)
#
#   cbind(sglm$coefficients[,2],
#         squasiglm$coefficients[,2],
#         ssurveryglm$coefficients[,2])
#
#   cbind(coef(model_fit_glm), coef(model_fit_surveyglm))
#   testthat::expect_equal(coef(model_fit_glm),
#                          coef(model_fit_surveyglm),
#                          tolerance = 0.000001)
#
#   cbind(coef(model_fit_quasiglm), coef(model_fit_surveyglm))
#   testthat::expect_equal(coef(model_fit_quasiglm),
#                          coef(model_fit_surveyglm),
#                          tolerance = 0.000001)
#
# })


# test_that("dfl_decompose() replicates Table 5, p. 67, in FLF 2011 Handbook Chapter", {
#
#   load("data-raw/men8305_full.rda")
#   men8305_full$weights <- men8305_full$weights/sum(men8305_full$weights) * length(men8305_full$weights)
#   flf_model <- log(wage) ~ union*(education + experience) + education*experience
#
#   # Replicate statistics in table 5, p.67, in in FLF (2011)
#   flf_male_inequality_table_5  <- dfl_decompose(flf_model,
#                                            data = men8305_full,
#                                            weights = weights,
#                                            group = year,
#                                            reference_0 = TRUE,
#                                            statistics = c("iq_range_p90_p10",
#                                                            "iq_range_p90_p50",
#                                                            "iq_range_p50_p10",
#                                                             "variance"))
#   results_dfl_decompose <- flf_male_inequality_table_5$decomposition_other_statistics
#   published_results_FLF_table_5 <- data.frame(statistic = results_dfl_decompose$statistic,
#                                               `Observed difference` = c(0.0617, 0.1091, 0.1827, -0.0736),
#                                               `Structure effect` = c(0.0408, 0.0336, 0.1637, -0.1301),
#                                               `Composition effect` = c(0.0208, 0.0756, 0.0191, 0.0565))
#   rownames(results_dfl_decompose) <- rownames(published_results_FLF_table_5) <- 1:4
#   names(published_results_FLF_table_5) <- names(results_dfl_decompose)
#   testthat::expect_equal(results_dfl_decompose,
#                          published_results_FLF_table_5,
#                          tolerance = 0.0075)
# })
