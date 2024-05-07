test_that("Summary function does not throw an error", {
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  decompose_male_as_reference <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = FALSE
  )

  testthat::expect_error(summary(decompose_male_as_reference), NA)
})


test_that("Summary works with different data", {
  data("men8305")

  model_rifreg <- log(wage) ~ union * (education + experience) + education * experience

  # Variance
  variance_decomposition <- ob_decompose(
    formula = model_rifreg,
    data = men8305,
    group = year,
    reweighting = TRUE,
    rifreg_statistic = "variance",
    bootstrap = F
  )

  testthat::expect_error(summary(variance_decomposition, aggregate_factors = FALSE), NA)
})

test_that("Summary function does not throw an error with custom aggregation", {
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  decompose_male_as_reference <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = FALSE
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
    summary(decompose_male_as_reference,
      custom_aggregation = custom_aggregation
    ),
    NA
  )
})

test_that("Summary function does not throw an error with aggregation and bootstrapped SE", {
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  decompose_female_as_reference_bs <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    bootstrap = TRUE,
    bootstrap_iterations = 10
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
    summary(decompose_female_as_reference_bs,
      custom_aggregation = custom_aggregation
    ),
    NA
  )
})


test_that("Summary function does not throw an error with Variance and Gini (and aggregation, BS-SE)", {
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  decompose_female_as_reference_bs <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    normalize_factors = T,
    rifreg_statistic = "gini",
    bootstrap = TRUE,
    bootstrap_iterations = 10
  )

  testthat::expect_error(
    summary(decompose_female_as_reference_bs),
    NA
  )

  testthat::expect_error(
    summary(decompose_female_as_reference_bs,
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
    summary(decompose_female_as_reference_bs,
      custom_aggregation = custom_aggregation
    ),
    NA
  )
})



test_that("Summary function does not throw an error in example", {
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  # Define aggregation of decomposition terms
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

  decompose_female_as_reference <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE
  )

  summary <- summary(decompose_female_as_reference, custom_aggregation = custom_aggregation)

  testthat::expect_error(summary, NA)

  summary_detailed <- summary(decompose_female_as_reference, aggregate_factors = FALSE)
  testthat::expect_error(summary_detailed, NA)
})



test_that("Summary function does not throw an error with reweighting (no SE)", {
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry | part_time * industry

  # Define aggregation of decomposition terms
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

  decompose_female_as_reference <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    reweighting = TRUE
  )

  summary <- summary(decompose_female_as_reference, custom_aggregation = custom_aggregation)

  testthat::expect_error(summary, NA)

  summary_detailed <- summary(decompose_female_as_reference, aggregate_factors = FALSE)
  testthat::expect_error(summary_detailed, NA)
})

test_that("Summary function does not throw an error with reweighting and SE", {
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  # Define aggregation of decomposition terms
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

  decompose_female_as_reference <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    reweighting = TRUE,
    bootstrap = TRUE,
    bootstrap_iterations = 10
  )

  summary <- summary(decompose_female_as_reference, custom_aggregation = custom_aggregation)

  testthat::expect_error(summary, NA)

  summary_detailed <- summary(decompose_female_as_reference, aggregate_factors = FALSE)
  testthat::expect_error(summary_detailed, NA)
})


test_that("Summary function does not throw an error with reweighting RIFREG and SE", {
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  # Define aggregation of decomposition terms
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

  decompose_female_as_reference <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    rifreg_statistic = "variance",
    reweighting = TRUE,
    bootstrap = TRUE,
    bootstrap_iterations = 10
  )

  summary <- summary(decompose_female_as_reference, custom_aggregation = custom_aggregation)

  testthat::expect_error(summary, NA)

  summary_detailed <- summary(decompose_female_as_reference, aggregate_factors = FALSE)
  testthat::expect_error(summary_detailed, NA)
})


test_that("Summary function does not throw an error with multiple quantiles", {
  data("nlys00")

  mod1 <- log(wage) ~ age + central_city + msa + region + black +
    hispanic + education + afqt + family_responsibility + years_worked_civilian +
    years_worked_military + part_time + industry

  # Define aggregation of decomposition terms
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

  decompose_female_as_reference <- ob_decompose(
    formula = mod1,
    data = nlys00,
    group = female,
    reference_0 = TRUE,
    rifreg_statistic = "quantiles",
    rifreg_probs = c(0.1, 0.5, 0.9),
    reweighting = TRUE,
    bootstrap = TRUE,
    bootstrap_iterations = 10
  )

  summary <- summary(decompose_female_as_reference, custom_aggregation = custom_aggregation)

  testthat::expect_error(summary, NA)
})
