test_that("Plot function does not throw an error with dfl_decompose", {
  set.seed(89342395)

  model_sequential <- log(wage) ~ union + experience + education | experience + education | education

  decompose_results <- dfl_decompose(model_sequential,
    data = men8305[1:1000, ],
    weights = weights,
    group = year
  )

  plot(decompose_results)
  testthat::expect_error(plot(decompose_results), NA)
})

test_that("Plot function does not throw an error with rifreg in ob_decompose", {
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  decompose_male_as_reference <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reweighting = TRUE,
    rifreg_statistic = "quantiles",
    rifreg_probs = c(0.1, 0.5, 0.9),
    bootstrap = TRUE,
    bootstrap_iterations = 10,
    reference_0 = FALSE
  )

  testthat::expect_error(plot(decompose_male_as_reference,
    confidence_bands = FALSE
  ), NA)
  testthat::expect_error(plot(decompose_male_as_reference), NA)
})


test_that("Plot function does not throw an error with classic ob_decompose", {
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  decompose_male_as_reference <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    rifreg_statistic = "variance",
    reweighting = TRUE,
    bootstrap = TRUE,
    bootstrap_iterations = 10,
    reference_0 = FALSE
  )

  testthat::expect_error(
    plot(decompose_male_as_reference),
    NA
  )
  testthat::expect_error(
    plot(decompose_male_as_reference, detailed_effects = FALSE),
    NA
  )

  testthat::expect_error(
    plot(decompose_male_as_reference,
      aggregate_factors = FALSE
    ),
    NA
  )

  custom_aggregation <- list(
    `Age, race, region, etc.` = c(
      "age",
      "blackyes",
      "hispanicyes",
      "regionNorth-central",
      "regionSouth",
      "regionWest",
      "central_cityyes",
      "msayes"
    ),
    `Education` = c(
      "education<10 yrs",
      "educationHS grad (diploma)",
      "educationHS grad (GED)",
      "educationSome college",
      "educationBA or equiv. degree",
      "educationMA or equiv. degree",
      "educationPh.D or prof. degree"
    ),
    `AFTQ` = "afqt",
    `L.T. withdrawal due to family` = "family_responsibility",
    `Life-time work experience` = c(
      "years_worked_civilian",
      "years_worked_military",
      "part_time"
    ),
    `Industrial sectors` = c(
      "industryManufacturing",
      "industryEducation, Health, Public Admin.",
      "industryOther services"
    )
  )

  testthat::expect_error(
    plot(decompose_male_as_reference, custom_aggregation = custom_aggregation),
    NA
  )
})
