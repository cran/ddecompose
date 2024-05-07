#### Validation ---------------------

# This code replicates the results in Firpo, Fortin, and Lemieux (2018) and from
# other Stata code. Data and additional files are available upon request.


# test_that("ob_decompose() replicates Table A1, p. 32, in FFL (2018)", {
#
#   #load("data/men8816.rda")
#
#   # Split the dataframe based on year criteria
#   men_88_90 <- men8816[men8816$year == "88-90", ]
#   men_14_16 <- men8816[men8816$year == "14-16", ]
#
#   # Function to calculate means for numeric columns
#   calculate_means <- function(df, weight_col) {
#     numeric_columns <- sapply(df, is.numeric)
#     numeric_columns[weight_col] <- FALSE
#
#     # Calculating weighted means
#     means <- sapply(df[, numeric_columns, drop = FALSE],
#                     function(x) round(weighted.mean(x, df[[weight_col]], na.rm = TRUE), 3))
#
#     labels <- attr(df, "var.labels")[names(means)]
#
#     data.frame(variable = names(means), mean = means, label = labels)
#   }
#
#   # Apply the function to each subset
#   means_88_90 <- calculate_means(men_88_90, "eweight")
#   means_14_16 <- calculate_means(men_14_16, "eweight")
#
#   # Year 88-90
#   testthat::expect_equal(means_88_90$mean[5], 2.860)
#   testthat::expect_equal(means_88_90$mean[7], 0.223)
#   testthat::expect_equal(means_88_90$mean[3], 0.134)
#   testthat::expect_equal(means_88_90$mean[8], 0.388)
#   testthat::expect_equal(means_88_90$mean[1], 36.204)
#
#   ## Education
#   testthat::expect_equal(means_88_90$mean[10], 0.059)
#   testthat::expect_equal(means_88_90$mean[11], 0.118)
#   testthat::expect_equal(means_88_90$mean[12], 0.381)
#   testthat::expect_equal(means_88_90$mean[13], 0.202)
#   testthat::expect_equal(means_88_90$mean[14], 0.139)
#   testthat::expect_equal(means_88_90$mean[15], 0.101)
#
#
#   ## Occupations
#   testthat::expect_equal(means_88_90$mean[25], 0.082)
#   testthat::expect_equal(means_88_90$mean[26], 0.040)
#   testthat::expect_equal(means_88_90$mean[27], 0.061)
#   testthat::expect_equal(means_88_90$mean[28], 0.014)
#   testthat::expect_equal(means_88_90$mean[29], 0.052)
#   # only first 5
#
#   ## Industries
#   testthat::expect_equal(means_88_90$mean[42], 0.033)
#   testthat::expect_equal(means_88_90$mean[43], 0.097)
#   testthat::expect_equal(means_88_90$mean[44], 0.102)
#   testthat::expect_equal(means_88_90$mean[45], 0.137)
#   testthat::expect_equal(means_88_90$mean[46], 0.051)
#   # only first 5
#
#
#   # Year 14-16
#   testthat::expect_equal(means_14_16$mean[5], 2.901)
#   testthat::expect_equal(means_14_16$mean[7], 0.127)
#   testthat::expect_equal(means_14_16$mean[3], 0.186)
#   testthat::expect_equal(means_14_16$mean[8], 0.457)
#   testthat::expect_equal(means_14_16$mean[1], 39.882)
#
#   ## Education
#   testthat::expect_equal(means_14_16$mean[10], 0.034)
#   testthat::expect_equal(means_14_16$mean[11], 0.054)
#   testthat::expect_equal(means_14_16$mean[12], 0.307)
#   testthat::expect_equal(means_14_16$mean[13], 0.275)
#   testthat::expect_equal(means_14_16$mean[14], 0.218)
#   testthat::expect_equal(means_14_16$mean[15], 0.113)
#
#
#   ## Occupations
#   testthat::expect_equal(means_14_16$mean[25], 0.080)
#   testthat::expect_equal(means_14_16$mean[26], 0.068)
#   testthat::expect_equal(means_14_16$mean[27], 0.081)
#   testthat::expect_equal(means_14_16$mean[28], 0.010)
#   testthat::expect_equal(means_14_16$mean[29], 0.061)
#   testthat::expect_equal(means_14_16$mean[30], 0.015)
#   testthat::expect_equal(means_14_16$mean[31], 0.019)
#   testthat::expect_equal(means_14_16$mean[32], 0.068)
#   testthat::expect_equal(means_14_16$mean[33], 0.085)
#   testthat::expect_equal(means_14_16$mean[34], 0.006)
#   # only first 10
#
#   ## Industries
#   testthat::expect_equal(means_14_16$mean[42], 0.026)
#   testthat::expect_equal(means_14_16$mean[43], 0.101)
#   testthat::expect_equal(means_14_16$mean[44], 0.066)
#   testthat::expect_equal(means_14_16$mean[45], 0.087)
#   testthat::expect_equal(means_14_16$mean[46], 0.033)
#   # only first 5
#
# })
#
#
#
#
# test_that("ob_decompose() replicates Table 1, p. 21, in FFL (2018)", {
#
#   # load("data/men8816.rda")
#   rif_model <- as.formula(paste("lwage1 ~ covered + nonwhite + nmarr +
#     ed0 + ed1 + ed3 + ed4 + ed5 + ",
#     paste(grep(paste0("^ex(", paste(c(1:4, 6:9), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#     paste(grep(paste0("^occd(", paste(c(11:60, 80:91), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#     paste(grep(paste0("^indd(", paste(c(1, 3:14), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + pub"))
#
#
#   rif_quantiles_88_90  <- rifreg::rifreg(rif_model,
#                                    data = men8816[men8816$year == "88-90", ],
#                                    weights = eweight,
#                                    statistic = "quantiles",
#                                    probs = c(0.1, 0.5, 0.9))
#
#   rif_quantiles_14_16  <- rifreg::rifreg(rif_model,
#                                    data = men8816[men8816$year == "14-16", ],
#                                    weights = eweight,
#                                    statistic = "quantiles",
#                                    probs = c(0.1, 0.5, 0.9))
#
#    # Test year 88-90
#    ## 1. Decile
#   testthat::expect_equal(round(as.numeric(rif_quantiles_88_90$estimates[2:17,1]), 3),
#                          c(0.146, -0.063, -0.111, -0.301, -0.305, 0.055, 0.143, 0.094,
#                            -0.486, -0.056, -0.005, 0.002, 0.010, 0.017, 0.022, 0.068), tolerance = 0.05)
#
#   ## 5. Decile
#   testthat::expect_equal(round(as.numeric(rif_quantiles_88_90$estimates[2:17,2]), 3),
#                          c(0.343, -0.137, -0.109, -0.312, -0.112, 0.135, 0.343, 0.418,
#                            -0.448, -0.270, -0.122, -0.051, 0.033, 0.048, 0.028, 0.020), tolerance = 0.04)
#
#   ## 9. Decile
#   testthat::expect_equal(round(as.numeric(rif_quantiles_88_90$estimates[2:17,3]), 3),
#                          c(0.025, -0.072, -0.031, -0.109, 0.005, 0.112, 0.410, 0.772,
#                            -0.312, -0.278, -0.172, -0.091, 0.060, 0.071, 0.061, -0.010), tolerance = 0.04)
#
#
#
#   # Test year 14-16
#   ## 1. Decile
#   testthat::expect_equal(round(as.numeric(rif_quantiles_14_16$estimates[2:17,1]), 3),
#                          c(0.058, -0.053, -0.046, -0.212, -0.275, 0.036, 0.125, 0.099,
#                            -0.335, -0.067, -0.022, -0.009, -0.001, 0.008, 0.013, 0.030), tolerance = 0.02)
#
#   ## 5. Decile
#   testthat::expect_equal(round(as.numeric(rif_quantiles_14_16$estimates[2:17,2]), 3),
#                          c(0.240, -0.106, -0.107, -0.415, -0.215, 0.098, 0.409, 0.418,
#                            -0.425, -0.285, -0.157, -0.051, 0.020, 0.037, 0.054, 0.058), tolerance = 0.04)
#
#   ## 9. Decile
#   testthat::expect_equal(round(as.numeric(rif_quantiles_14_16$estimates[2:17,3]), 3),
#                          c(-0.008, -0.041, -0.064, -0.110, 0.002, 0.023, 0.493, 0.962,
#                            -0.301, -0.306, -0.182, -0.034, 0.036, 0.042, 0.062, -0.013), tolerance = 0.05)
#
#
#
# })
#
#
#
# test_that("ob_decompose() replicates Table 2, p. 25-26, in FFL (2018)", {
#
#
#   # load("data/men8816.rda")
#
#   # # Create 'wage_var' equal to 'lwage2' where 'lwage2' is less than or equal to 7.4
#   # men8816$wage_var <- ifelse(men8816$lwage2 <= 7.4, men8816$lwage2, NA)
#   #
#   # # Create 'wage_gini' as the exponential of 'lwage2' where 'lwage2' is less than or equal to 7.4
#   # men8816$wage_gini <- ifelse(men8816$lwage2 <= 7.4, exp(men8816$lwage2), NA)
#
#
#   var_model <- as.formula(paste("wage_var ~ covered + nonwhite + nmarr +
#     ed0 + ed1 + ed3 + ed4 + ed5 + ",
#     paste(grep(paste0("^ex(", paste(c(1:4, 6:9), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#     paste(grep(paste0("^occd(", paste(c(11:60, 80:91), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#     paste(grep(paste0("^indd(", paste(c(1, 3:14), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + pub"))
#
#   gini_model <- as.formula(paste("wage_gini ~ covered + nonwhite + nmarr +
#     ed0 + ed1 + ed3 + ed4 + ed5 + ",
#     paste(grep(paste0("^ex(", paste(c(1:4, 6:9), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#     paste(grep(paste0("^occd(", paste(c(11:60, 80:91), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#     paste(grep(paste0("^indd(", paste(c(1, 3:14), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + pub"))
#
#
#
# #
# #   men_88_90 <- men8816[men8816$year == "88-90", ]
# #   men_88_90<- na.omit(men_88_90)
# #
# #   men_14_16 <- men8816[men8816$year == "14-16", ]
# #   men_14_16<- na.omit(men_14_16)
# browser()
#
#   # men_88_90 <- readstata13::read.dta13("data-raw/validation_data/FFL_2018_replication_code/usmen8890_t2.dta")
#   # men_14_16 <- readstata13::read.dta13("data-raw/validation_data/FFL_2018_replication_code/usmen1416_t2.dta")
#
#   men_88_90 <- men_88_90[complete.cases(men_88_90$wage_var), ]
#   men_14_16 <- men_14_16[complete.cases(men_14_16$wage_var), ]
#
#
#
#   rifreg_var_88_90  <- rifreg::rifreg(var_model,
#                                 data = men_88_90,
#                                 weights = eweight,
#                                 statistic = "variance")
#
#   rifreg_var_14_16  <- rifreg::rifreg(var_model,
#                                 data = men_14_16,
#                                 weights = eweight,
#                                 statistic = "variance")
#
#   rifreg_gini_88_90  <- rifreg::rifreg(gini_model,
#                                  data = men_88_90,
#                                  weights = eweight,
#                                  statistic = "gini")
#
#   rifreg_gini_14_16  <- rifreg::rifreg(gini_model,
#                                  data = men_14_16,
#                                  weights = eweight,
#                                  statistic = "gini")
#
#   # Variance
#   ## Test year 88-90
#
#   ### Inequality Measures - Estimated Values
#   weighted_variance <- function(dep_var, weights) {
#
#     # Ensure weights sum to 1
#     weights <- weights / sum(weights, na.rm = TRUE)
#
#     # Mean of the data
#     weighted_mean <- sum(weights * dep_var, na.rm = TRUE)
#
#     # Weighted variance
#     sum(weights * (dep_var - weighted_mean)^2, na.rm = TRUE)
#   }
#
#   var_88_90 <- weighted_variance(dep_var = men_88_90$wage_var, weights = men_88_90$eweight)
#   testthat::expect_equal(round(as.numeric(var_88_90), 3), 0.341)
#
#
#   ### Boolean and Education
#   testthat::expect_equal(round(as.numeric(rifreg_var_88_90$estimates[1:9,1]), 3),
#                          c(0.203, -0.075, -0.002, 0.039, 0.074, 0.104, 0.028, 0.121, 0.301))
#   ### Experience
#   testthat::expect_equal(round(as.numeric(rifreg_var_88_90$estimates[10:17,1]), 3),
#                          c(0.047, -0.098, -0.078, -0.050, 0.023, 0.022, 0.015, -0.031))
#   ### Occupations
#   testthat::expect_equal(round(as.numeric(rifreg_var_88_90$estimates[18:33,1]), 3),
#                          c(0.235, 0.090, 0.107, 0.081, -0.001, 0.524, -0.020, 0.013,
#                            0.088, 0.208, 0.525, 0.188, 0.226, 0.004, 0.119, 0.015))
#   ### Industries
#   testthat::expect_equal(round(as.numeric(rifreg_var_88_90$estimates[34:47,1]), 3),
#                          c(0.079, 0.018, -0.037, -0.012, 0.060, 0.013, -0.001, 0.065,
#                            0.048, 0.018, -0.008, 0.136, -0.038, -0.058))
#
#   ## Test year 14-16
#
#   ### Inequality Measures - Estimated Values
#   var_14_16 <- weighted_variance(dep_var = men_14_16$wage_var, weights = men_14_16$eweight)
#   testthat::expect_equal(round(as.numeric(var_14_16), 3), 0.418)
#
#
#   ### Boolean and Education
#   testthat::expect_equal(round(as.numeric(rifreg_var_14_16$estimates[1:9,1]), 3),
#                          c(0.205, -0.040, 0.005, 0.001, 0.073, 0.129, -0.001, 0.166, 0.401))
#   ### Experience
#   testthat::expect_equal(round(as.numeric(rifreg_var_14_16$estimates[10:17,1]), 3),
#                          c(0.027, -0.093, -0.070, -0.006, 0.024, 0.017, 0.022, -0.012))
#   ### Occupations
#   testthat::expect_equal(round(as.numeric(rifreg_var_14_16$estimates[18:33,1]), 3),
#                          c(0.415, 0.200, 0.202, 0.134, 0.065, 0.637, 0.115, 0.069,
#                            0.177, 0.197, 0.409, 0.208, 0.222, 0.020, 0.145, 0.042))
#   ### Industries
#   testthat::expect_equal(round(as.numeric(rifreg_var_14_16$estimates[34:47,1]), 3),
#                          c(0.013, 0.014, -0.053, -0.027, 0.016, -0.029, 0.055, 0.064,
#                            0.071, -0.042, -0.064, 0.054, -0.071, -0.055))
#
#
#
#   # Gini
#   ## Test year 88-90
#
#   ### Inequality Measures - Estimated Values
#   gini_88_90 <- rifreg::compute_gini(dep_var = men_88_90$wage_gini, weights = men_88_90$eweight)
#   testthat::expect_equal(round(as.numeric(gini_88_90), 3), 0.330, tolerance = 0.004)
#
#   ### Boolean and Education
#   testthat::expect_equal(round(as.numeric(rifreg_gini_88_90$estimates[1:9,1]), 3),
#                          c(0.261, -0.067, 0.006, 0.022, 0.051, 0.048, 0.006, 0.053, 0.157))
#   ### Experience
#   testthat::expect_equal(round(as.numeric(rifreg_gini_88_90$estimates[10:17,1]), 3),
#                          c(0.031, -0.036, -0.035, -0.026, 0.012, 0.008, 0.008, -0.015))
#   ### Occupations
#   testthat::expect_equal(round(as.numeric(rifreg_gini_88_90$estimates[18:33,1]), 3),
#                          c(0.132, 0.027, 0.013, 0.025, -0.012, 0.337, -0.035, 0.017,
#                            0.043, 0.152, 0.429, 0.101, 0.114, 0.011, 0.079, 0.030))
#   ### Industries
#   testthat::expect_equal(round(as.numeric(rifreg_gini_88_90$estimates[34:47,1]), 3),
#                          c(0.036, -0.001, -0.011, 0.001, 0.038, -0.005, -0.010, 0.052,
#                            0.018, 0.019, -0.001, 0.051, -0.030, -0.036))
#
#
#   ## Test year 14-16
#
#   ### Inequality Measures - Estimated Values
#   gini_14_16 <- rifreg::compute_gini(dep_var = men_14_16$wage_gini, weights = men_14_16$eweight)
#   testthat::expect_equal(round(as.numeric(gini_14_16), 3), 0.396)
#
#   ### Boolean and Education
#   testthat::expect_equal(round(as.numeric(rifreg_gini_14_16$estimates[1:9,1]), 3),
#                          c(0.290, -0.039, 0.005, 0.008, 0.057, 0.063, -0.006, 0.061, 0.177))
#
#   ### Experience
#   testthat::expect_equal(round(as.numeric(rifreg_gini_14_16$estimates[10:17,1]), 3),
#                          c(0.021, -0.030, -0.028, 0.003, 0.014, 0.007, 0.008, -0.005))
#   ### Occupations
#   testthat::expect_equal(round(as.numeric(rifreg_gini_14_16$estimates[18:33,1]), 3),
#                          c(0.203, 0.080, 0.054, 0.068, 0.012, 0.363, 0.011, 0.044,
#                            0.084, 0.105, 0.219, 0.107, 0.127, 0.028, 0.094, 0.040))
#   ### Industries
#   testthat::expect_equal(round(as.numeric(rifreg_gini_14_16$estimates[34:47,1]), 3),
#                          c(-0.001, 0.002, -0.019, -0.006, 0.023, -0.019, 0.041, 0.053,
#                            0.035, -0.014, -0.018, 0.023, -0.029, -0.048))
#
# })

# test_that("ob_decompose() replicates Table 3, p. 29, in FFL (2018)", {
#   set.seed(98437)
#
#   var_model <- as.formula(paste("lwage2 ~ covered + nonwhite + nmarr +
#     ed0 + ed1 + ed3 + ed4 + ed5 + ",
#                                 paste(grep(paste0("^ex(", paste(c(1:4, 6:9), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#                                 paste(grep(paste0("^occd(", paste(c(11:60, 80:91), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#                                 paste(grep(paste0("^indd(", paste(c(1, 3:14), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + pub"))
#
#   #
#   #   # get cleaned Stata data
#   #   men8816_t3 <- readstata13::read.dta13("data-raw/validation_data/FFL_2018_replication_code/usmen8816_t3.dta")
#   #   men8816_t3 <- men8816_t3[men8816_t3$time <= 1,]
#   #
#   #
#   decompose_90_10  <- ddecompose::ob_decompose(formula = var_model,
#                                 data = men8816_t3,
#                                 weights = eweight,
#                                 group = time,
#                                 reference_0 = TRUE,
#                                 rifreg_statistic = "interquantile_range",
#                                 rifreg_probs = c(0.9, 0.1),
#                                 bw = 0.065,
#                                 kernel = "epanechnikov")
#
#
#   # Overall
#   testthat::expect_equal(as.numeric(decompose_90_10$interquantile_range$decomposition_term$Observed_difference[1]), 0.1251959 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[1]), 0.0880184 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Structure_effect[1]), 100*0.0371775, tolerance = 0.014)
#   # (2. bw und kernel Stata)0.1256141; (2.bw und kernel R-default) 0.1256292
#   # (2. bw und kernel Stata)0.0810027; (2.bw und kernel R-default) 0.07632009
#   # (2. bw und kernel Stata)0.04461135; (2.bw und kernel R-default) 0.04930909
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Composition_effect[3]), 100*0.0163294, tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[11:18]))), 100*0.0188145, tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[6:10])), 100*0.0086518 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[19:34])), 100*0.0186474, tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[35:48])), 100*0.0255752 , tolerance = 0.01)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Structure_effect[3]), 100*0.0134943 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[11:18]))), 100*-0.0480036 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[6:10])), 100*0.0162826, tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[19:34])), 100*0.0505249 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[35:48])), 100*-.0749435 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[2])), 100*0.0798227 , tolerance = 0.01)
#
#
#
#   decompose_50_10  <- ddecompose::ob_decompose(formula = var_model,
#                                 data = men8816_t3,
#                                 weights = eweight,
#                                 group = time,
#                                 reference_0 = TRUE,
#                                 rifreg_statistic = "interquantile_range",
#                                 rifreg_probs = c(0.5, 0.1),
#                                 bw = 0.065,
#                                 kernel = "epanechnikov")
#
#
#   # Overall
#   testthat::expect_equal(as.numeric(decompose_50_10$interquantile_range$decomposition_term$Observed_difference[1]), -.0754307, tolerance = 0.04)
#   testthat::expect_equal(as.numeric(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[1]), .036705, tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Structure_effect[1]), 100*-.1121357, tolerance = 0.03)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Composition_effect[3]), 100*-.0187761, tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[11:18]))), 100*.0080743, tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[6:10])), 100*.0133806  , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[19:34])), 100*.0213631, tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[35:48])), 100*.0126631, tolerance = 0.01)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Structure_effect[3]), 100*-.0019561, tolerance = 0.05)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[11:18]))), 100*-.0342662  , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[6:10])), 100*.0084491, tolerance = 0.05)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[19:34])), 100*-.0667522 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[35:48])), 100*-.0478626  , tolerance = 0.02)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[2])), 100*.0302523, tolerance = 0.1)
#
#
#   decompose_90_50  <- ddecompose::ob_decompose(formula = var_model,
#                                 data = men8816_t3,
#                                 weights = eweight,
#                                 group = time,
#                                 reference_0 = TRUE,
#                                 rifreg_statistic = "interquantile_range",
#                                 rifreg_probs = c(0.9, 0.5),
#                                 bw = 0.065,
#                                 kernel = "epanechnikov")
#
#   # Overall
#   testthat::expect_equal(as.numeric(decompose_90_50$interquantile_range$decomposition_term$Observed_difference[1]), .2006267, tolerance = 0.01)
#   testthat::expect_equal(as.numeric(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[1]), .0513134 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Structure_effect[1]), 100*.1493133, tolerance = 0.013)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Composition_effect[3]), 100*.0351056 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[11:18]))), 100*.0107402 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[6:10])), 100*-.0047288 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[19:34])), 100*-.0027157, tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[35:48])), 100*.0129121  , tolerance = 0.01)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Structure_effect[3]), 100*.0154504 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[11:18]))), 100*-.0137374  , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[6:10])), 100*.0078335, tolerance = 0.06)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[19:34])), 100*.1172772 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[35:48])), 100*-.0270809  , tolerance = 0.03)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[2])), 100*.0495704  , tolerance = 0.07)
#
#
#   decompose_variance  <- ddecompose::ob_decompose(formula = var_model,
#                                    data = men8816_t3,
#                                    weights = eweight,
#                                    group = time,
#                                    rifreg_statistic = "variance")
#
#
#   # Overall
#   testthat::expect_equal(round(as.numeric(100*decompose_variance$variance$decomposition_term$Observed_difference[1]), 3), 7.775137, tolerance = 0.0001)
#   testthat::expect_equal(round(as.numeric(100*decompose_variance$variance$decomposition_term$Composition_effect[1]), 3), 4.146447, tolerance = 0.001)
#   testthat::expect_equal(round(as.numeric(100*decompose_variance$variance$decomposition_term$Structure_effect[1]), 3),  3.62869 , tolerance = 0.0001)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_variance$variance$decomposition_term$Composition_effect[3]), .7134834 , tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_variance$variance$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_variance$variance$decomposition_term$Composition_effect[11:18]))), .9839646, tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Composition_effect[6:10])), .6652325 , tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Composition_effect[19:34])), .6553098, tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Composition_effect[35:48])),  1.128457 , tolerance = 0.000001)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_variance$variance$decomposition_term$Structure_effect[3]), .4369568  , tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_variance$variance$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_variance$variance$decomposition_term$Structure_effect[11:18]))), -.9844021  , tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[6:10])), 1.484122, tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[19:34])), 5.349821, tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[35:48])), -2.99592 , tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[2])), .338112 , tolerance = 0.000001)
#
#
#   gini_model <- as.formula(paste("exp(lwage2) ~ covered + nonwhite + nmarr +
#     ed0 + ed1 + ed3 + ed4 + ed5 + ",
#                                  paste(grep(paste0("^ex(", paste(c(1:4, 6:9), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#                                  paste(grep(paste0("^occd(", paste(c(11:60, 80:91), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#                                  paste(grep(paste0("^indd(", paste(c(1, 3:14), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + pub"))
#
#
#
#   decompose_gini  <- ddecompose::ob_decompose(formula = gini_model,
#                                data = men8816_t3,
#                                weights = eweight,
#                                group = time,
#                                rifreg_statistic = "gini")
#
#   # Overall
#   testthat::expect_equal(round(as.numeric(100*decompose_gini$gini$decomposition_term$Observed_difference[1]), 3), 6.599147, tolerance = 0.0001)
#   testthat::expect_equal(round(as.numeric(100*decompose_gini$gini$decomposition_term$Composition_effect[1]), 3), 1.956308, tolerance = 0.0002)
#   testthat::expect_equal(round(as.numeric(100*decompose_gini$gini$decomposition_term$Structure_effect[1]), 3), 4.64284, tolerance = 0.0001)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_gini$gini$decomposition_term$Composition_effect[3]), .6385443, tolerance = 0.00001)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_gini$gini$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_gini$gini$decomposition_term$Composition_effect[11:18]))), .4729887, tolerance = 0.00001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Composition_effect[6:10])), .2069459, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Composition_effect[19:34])), .1017178, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Composition_effect[35:48])),  .536111, tolerance = 0.0001)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_gini$gini$decomposition_term$Structure_effect[3]), .3575257, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_gini$gini$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_gini$gini$decomposition_term$Structure_effect[11:18]))), -.1623484, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[6:10])), .2063659, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[19:34])), 2.294733, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[35:48])), -.9556691, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[2])), 2.902232, tolerance = 0.0001)
#
#
# })
#
#
#
# test_that("ob_decompose() replicates Table 3 Bootstrap SE, p. 29, in FFL (2018)", {
#   set.seed(98437)
#
#   var_model <- as.formula(paste("lwage2 ~ covered + nonwhite + nmarr +
#     ed0 + ed1 + ed3 + ed4 + ed5 + ",
#     paste(grep(paste0("^ex(", paste(c(1:4, 6:9), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#     paste(grep(paste0("^occd(", paste(c(11:60, 80:91), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#     paste(grep(paste0("^indd(", paste(c(1, 3:14), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + pub"))
#
#
#   # # get cleaned Stata data
#   # men8816_t3 <- readstata13::read.dta13("data-raw/validation_data/FFL_2018_replication_code/usmen8816_t3.dta")
#   # men8816_t3 <- men8816_t3[men8816_t3$time <= 1,]
#
#   #   decompose_90_10  <- ddecompose::ob_decompose(formula = var_model,
#   #                                 data = men8816_t3,
#   #                                 weights = eweight,
#   #                                 group = time,
#   #                                 reference_0 = TRUE,
#   #                                 rifreg_statistic = "interquantile_range",
#   #                                 rifreg_probs = c(0.9, 0.1),
#   #                                 bw = 0.065,
#   #                                 kernel = "epanechnikov",
#   #                                 bootstrap = TRUE,
#   #                                 bootstrap_iterations = 100)
#   #
#   #   # Overall
#   #   testthat::expect_equal(as.numeric(decompose_90_10$interquantile_range$decomposition_term$Observed_difference[1]), 0.1251959 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[1]), 0.0880184 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Structure_effect[1]), 100*0.0371775, tolerance = 0.014)
#   #
#   #   # Composition Effects
#   #   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Composition_effect[3]), 100*0.0163294, tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*(sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[4:5]) +
#   #                                            sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[11:18]))), 100*0.0188145, tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[6:10])), 100*0.0086518 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[19:34])), 100*0.0186474, tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[35:48])), 100*0.0255752 , tolerance = 0.01)
#   #
#   #   # Wage Structure Effects
#   #   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Structure_effect[3]), 100*0.0134943 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*(sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[4:5]) +
#   #                                            sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[11:18]))), 100*-0.0480036 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[6:10])), 100*0.0162826, tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[19:34])), 100*0.0505249 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[35:48])), 100*-.0749435 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[2])), 100*0.0798227 , tolerance = 0.01)
#   #
#   #
#   #
#   #   decompose_50_10  <- ddecompose::ob_decompose(formula = var_model,
#   #                                 data = men8816_t3,
#   #                                 weights = eweight,
#   #                                 group = time,
#   #                                 reference_0 = TRUE,
#   #                                 rifreg_statistic = "interquantile_range",
#   #                                 rifreg_probs = c(0.5, 0.1),
#   #                                 bw = 0.065,
#   #                                 kernel = "epanechnikov")
#   #
#   #
#   #   # Overall
#   #   testthat::expect_equal(as.numeric(decompose_50_10$interquantile_range$decomposition_term$Observed_difference[1]), -.0754307, tolerance = 0.04)
#   #   testthat::expect_equal(as.numeric(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[1]), .036705, tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Structure_effect[1]), 100*-.1121357, tolerance = 0.03)
#   #
#   #   # Composition Effects
#   #   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Composition_effect[3]), 100*-.0187761, tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*(sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[4:5]) +
#   #                                            sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[11:18]))), 100*.0080743, tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[6:10])), 100*.0133806  , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[19:34])), 100*.0213631, tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[35:48])), 100*.0126631, tolerance = 0.01)
#   #
#   #   # Wage Structure Effects
#   #   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Structure_effect[3]), 100*-.0019561, tolerance = 0.05)
#   #   testthat::expect_equal(as.numeric(100*(sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[4:5]) +
#   #                                            sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[11:18]))), 100*-.0342662  , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[6:10])), 100*.0084491, tolerance = 0.05)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[19:34])), 100*-.0667522 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[35:48])), 100*-.0478626  , tolerance = 0.02)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[2])), 100*.0302523, tolerance = 0.1)
#   #
#   #
#   #   decompose_90_50  <- ddecompose::ob_decompose(formula = var_model,
#   #                                 data = men8816_t3,
#   #                                 weights = eweight,
#   #                                 group = time,
#   #                                 reference_0 = TRUE,
#   #                                 rifreg_statistic = "interquantile_range",
#   #                                 rifreg_probs = c(0.9, 0.5),
#   #                                 bw = 0.065,
#   #                                 kernel = "epanechnikov")
#   #
#   #   # Overall
#   #   testthat::expect_equal(as.numeric(decompose_90_50$interquantile_range$decomposition_term$Observed_difference[1]), .2006267, tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[1]), .0513134 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Structure_effect[1]), 100*.1493133, tolerance = 0.013)
#   #
#   #   # Composition Effects
#   #   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Composition_effect[3]), 100*.0351056 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*(sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[4:5]) +
#   #                                            sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[11:18]))), 100*.0107402 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[6:10])), 100*-.0047288 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[19:34])), 100*-.0027157, tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[35:48])), 100*.0129121  , tolerance = 0.01)
#   #
#   #   # Wage Structure Effects
#   #   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Structure_effect[3]), 100*.0154504 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*(sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[4:5]) +
#   #                                            sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[11:18]))), 100*-.0137374  , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[6:10])), 100*.0078335, tolerance = 0.06)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[19:34])), 100*.1172772 , tolerance = 0.01)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[35:48])), 100*-.0270809  , tolerance = 0.03)
#   #   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[2])), 100*.0495704  , tolerance = 0.07)
#
#   decompose_variance  <- ddecompose::ob_decompose(formula = var_model,
#                                    data = men8816_t3,
#                                    weights = eweight,
#                                    group = time,
#                                    rifreg_statistic = "variance",
#                                    bootstrap = TRUE,
#                                    bootstrap_iterations = 100)
#
#
#   # Overall
#   testthat::expect_equal(round(as.numeric(100*decompose_variance$variance$decomposition_term$Observed_difference[1]), 3), 7.775137, tolerance = 0.0001)
#   testthat::expect_equal(round(as.numeric(100*decompose_variance$variance$decomposition_term$Composition_effect[1]), 3), 4.146447, tolerance = 0.001)
#   testthat::expect_equal(round(as.numeric(100*decompose_variance$variance$decomposition_term$Structure_effect[1]), 3),  3.62869 , tolerance = 0.0001)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_variance$variance$decomposition_term$Composition_effect[3]), .7134834 , tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_variance$variance$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_variance$variance$decomposition_term$Composition_effect[11:18]))), .9839646, tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Composition_effect[6:10])), .6652325 , tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Composition_effect[19:34])), .6553098, tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Composition_effect[35:48])),  1.128457 , tolerance = 0.000001)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_variance$variance$decomposition_term$Structure_effect[3]), .4369568  , tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_variance$variance$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_variance$variance$decomposition_term$Structure_effect[11:18]))), -.9844021  , tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[6:10])), 1.484122, tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[19:34])), 5.349821, tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[35:48])), -2.99592 , tolerance = 0.000001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[2])), .338112 , tolerance = 0.000001)
#
#
#   gini_model <- as.formula(paste("exp(lwage2) ~ covered + nonwhite + nmarr +
#     ed0 + ed1 + ed3 + ed4 + ed5 + ",
#     paste(grep(paste0("^ex(", paste(c(1:4, 6:9), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#     paste(grep(paste0("^occd(", paste(c(11:60, 80:91), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + ",
#     paste(grep(paste0("^indd(", paste(c(1, 3:14), collapse = "|"), ")$"), names(men8816), value = T), collapse = " + "), " + pub"))
#
#
#
#   decompose_gini  <- ddecompose::ob_decompose(formula = gini_model,
#                                data = men8816_t3,
#                                weights = eweight,
#                                group = time,
#                                rifreg_statistic = "gini",
#                                bootstrap = TRUE,
#                                bootstrap_iterations = 100)
#
#   # Overall
#   testthat::expect_equal(round(as.numeric(100*decompose_gini$gini$decomposition_term$Observed_difference[1]), 3), 6.599147, tolerance = 0.0001)
#   testthat::expect_equal(round(as.numeric(100*decompose_gini$gini$decomposition_term$Composition_effect[1]), 3), 1.956308, tolerance = 0.0002)
#   testthat::expect_equal(round(as.numeric(100*decompose_gini$gini$decomposition_term$Structure_effect[1]), 3), 4.64284, tolerance = 0.0001)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_gini$gini$decomposition_term$Composition_effect[3]), .6385443, tolerance = 0.00001)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_gini$gini$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_gini$gini$decomposition_term$Composition_effect[11:18]))), .4729887, tolerance = 0.00001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Composition_effect[6:10])), .2069459, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Composition_effect[19:34])), .1017178, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Composition_effect[35:48])),  .536111, tolerance = 0.0001)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_gini$gini$decomposition_term$Structure_effect[3]), .3575257, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_gini$gini$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_gini$gini$decomposition_term$Structure_effect[11:18]))), -.1623484, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[6:10])), .2063659, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[19:34])), 2.294733, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[35:48])), -.9556691, tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[2])), 2.902232, tolerance = 0.0001)
#
#
# })
#
# test_that("ob_decompose() replicates Table 4, p. 30, in FFL (2018)", {
#   set.seed(9283274)
#
#   var_model <- as.formula(paste("lwage2 ~ covered + nonwhite + nmarr +
#                                 ed0 + ed1 + ed3 + ed4 + ed5 + ",
#                                 paste(grep(paste0("^ex(", paste(c(1:4, 6:9), collapse = "|"), ")$"), names(men8816_t4), value = T), collapse = " + "), " + ",
#                                 paste(grep(paste0("^occd(", paste(c(11:60, 80:91), collapse = "|"), ")$"), names(men8816_t4), value = T), collapse = " + "), " + ",
#                                 paste(grep(paste0("^indd(", paste(c(1, 3:14), collapse = "|"), ")$"), names(men8816_t4), value = T), collapse = " + "), " + pub | ",
#                                 paste("covered + nonwhite +",
#                                       paste(grep("^marr", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(c("ed0", "ed1", "ed3", "ed4", "ed5"), collapse = " + "), "+",
#                                       paste(grep("^ex[1-4]|^ex[6-9]", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^uned", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^unex", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^ex[1-9]ed", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^pub", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^indd(1|1e|[3-9]|10|11|13|14)(?!2)", names(men8816_t4), perl = TRUE, value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^occd", names(men8816_t4), value = TRUE), collapse = " + "))))
#
#
#
#
#
#
#     # # get cleaned Stata data
#     # men8816_t4 <- readstata13::read.dta13("data-raw/validation_data/FFL_2018_replication_code/usmen8816_t4.dta")
#     # men8816_t4 <- men8816_t4[men8816_t4$time <= 1,]
#     #
#     #
#     # men8816_sample <- readstata13::read.dta13("data-raw/validation_data/FFL_2018_replication_code/usmen8816_sample.dta")
#     # men8816_sample <- men8816_sample[men8816_sample$time <= 1,]
#
#   # IQR 90-10
#   decompose_90_10  <- ddecompose::ob_decompose(formula = var_model,
#                                 data = men8816_t4,
#                                 weights = eweight,
#                                 group = time,
#                                 reference_0 = TRUE,
#                                 rifreg_statistic = "interquantile_range",
#                                 rifreg_probs = c(0.9, 0.1),
#                                 bw = 0.065,
#                                 kernel = "epanechnikov",
#                                 reweighting = TRUE,
#                                 trimming = TRUE,
#                                 trimming_threshold = 100)
#
#
#
#   # Overall
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Observed_difference[1]), 100*(.0923746  + .0328213) , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Composition_effect[1]), 100*.0900131  , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Structure_effect[1]), 100*.034831, tolerance = 0.017)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Composition_effect[3]), 100*.0162751, tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[11:18]))), 100*.0192389 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[6:10])), 100*.0072483, tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[19:34])), 100*.0207977 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[35:48])), 100*.0264531  , tolerance = 0.005)
#
#   # Total Specification Error
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Specification_error[1]),  100*.0023615  , tolerance = 0.04)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Structure_effect[3]), 100*.0121873, tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[11:18]))), 100*-.0481368 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[6:10])), 100*.0564411, tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[19:34])), 100*.0117771 , tolerance = 0.07)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[35:48])), 100*-.086405  , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[2])), 100*.0889673, tolerance = 0.005)
#
#   # Total Reweighting Error
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Reweighting_error[1]),  100*-.0020097, tolerance = 0.005)
#
#
#   #IQR 50-10
#   decompose_50_10  <- ddecompose::ob_decompose(formula = var_model,
#                                 data = men8816_t4,
#                                 weights = eweight,
#                                 group = time,
#                                 reference_0 = TRUE,
#                                 rifreg_statistic = "interquantile_range",
#                                 rifreg_probs = c(0.5, 0.1),
#                                 bw = 0.065,
#                                 kernel = "epanechnikov",
#                                 reweighting = TRUE,
#                                 trimming = TRUE,
#                                 trimming_threshold = 100)
#
#
#
#   # Overall
#   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Observed_difference[1]), 100*(.0283386  -.1037693)   , tolerance = 0.04)
#   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Composition_effect[1]), 100*.0380101   , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Structure_effect[1]), 100*-.1038954 , tolerance = 0.005)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Composition_effect[3]), 100*-.0187136 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[11:18]))), 100*.0085151 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[6:10])), 100*.0126624 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[19:34])), 100*.0226244  , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[35:48])), 100*.0129219  , tolerance = 0.005)
#
#   # Total Specification Error
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Specification_error[1])),  100*-.0096715   , tolerance = 0.3)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Structure_effect[3]), 100*-.0050529 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[11:18]))), 100*-.0260159 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[6:10])), 100*.0103851 , tolerance = 0.03)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[19:34])), 100*-.0757565 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[35:48])), 100*-.0291649  , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[2])), 100*.0217097 , tolerance = 0.005)
#
#   # Total Reweighting Error
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Reweighting_error[1])),  100*.0001261, tolerance = 0.013)
#
#
#   #IQR 90-50
#   decompose_90_50  <- ddecompose::ob_decompose(formula = var_model,
#                                 data = men8816_t4,
#                                 weights = eweight,
#                                 group = time,
#                                 reference_0 = TRUE,
#                                 rifreg_statistic = "interquantile_range",
#                                 rifreg_probs = c(0.9, 0.5),
#                                 bw = 0.065,
#                                 kernel = "epanechnikov",
#                                 reweighting = TRUE,
#                                 trimming = TRUE,
#                                 trimming_threshold = 100)
#
#
#
#   # Overall
#   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Observed_difference[1]),100*(.0640361  + .1365906)   , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Composition_effect[1]), 100*.052003   , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Structure_effect[1]), 100*.1387263, tolerance = 0.005)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Composition_effect[3]), 100*.0349887 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[11:18]))), 100*.0107239  , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[6:10])), 100*-.0054142 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[19:34])), 100*-.0018267  , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[35:48])), 100*.0135312   , tolerance = 0.1)
#
#   # Total Specification Error
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Specification_error[1])), 100* .0120331, tolerance = 0.21)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Structure_effect[3]), 100*.0172402 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[11:18]))), 100*-.022121 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[6:10])), 100*.046056 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[19:34])), 100*.0875336 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[35:48])), 100*-.0572401  , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[2])), 100*.0672576, tolerance = 0.01)
#
#   # Total Reweighting Error
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_terms$Reweighting_error[1])),  100*-.0021358 , tolerance = 0.005)
#
#
#
#   ### Variance
# # decompose_variance  <- ddecompose::ob_decompose(formula = var_model,
# #                                  data = men8816_t4,
# #                                  weights = eweight,
# #                                  group = time,
# #                                  reference_0 = TRUE,
# #                                  rifreg_statistic = "variance",
# #                                  reweighting = TRUE,
# #                                  trimming = TRUE,
# #                                  trimming_threshold = 100)
# #
# #
# # # Overall
# # testthat::expect_equal(round(as.numeric(100*decompose_variance$variance$decomposition_term$Observed_difference[1]), 3),
# #                        4.205934 + 3.332824 + .3173928 - .0810136  , tolerance = 0.0001)
# # testthat::expect_equal(round(as.numeric(100*decompose_variance$variance$decomposition_term$Composition_effect[1]), 3), 4.205934, tolerance = 0.0001)
# # testthat::expect_equal(round(as.numeric(100*decompose_variance$variance$decomposition_term$Structure_effect[1]), 3),  3.332824, tolerance = 0.0001)
# #
# # # Composition Effects
# # testthat::expect_equal(as.numeric(100*decompose_variance$variance$decomposition_term$Composition_effect[3]), .7111088 , tolerance = 0.001)
# # testthat::expect_equal(as.numeric(100*(sum(decompose_variance$variance$decomposition_term$Composition_effect[4:5]) +
# #                                          sum(decompose_variance$variance$decomposition_term$Composition_effect[11:18]))), 1.005518, tolerance = 0.001)
# # testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Composition_effect[6:10])), .5995667 , tolerance = 0.001)
# # testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Composition_effect[19:34])), .733287, tolerance = 0.001)
# # testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Composition_effect[35:48])),  1.156454 , tolerance = 0.001)
# #
# # # Total Specification Error
# # testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Specification_error[1])),  .3173928 , tolerance = 0.001)
# #
# # # Wage Structure Effects
# # testthat::expect_equal(as.numeric(100*decompose_variance$variance$decomposition_term$Structure_effect[3]), .3310032 , tolerance = 0.001)
# # testthat::expect_equal(as.numeric(100*(sum(decompose_variance$variance$decomposition_term$Structure_effect[4:5]) +
# #                                          sum(decompose_variance$variance$decomposition_term$Structure_effect[11:18]))), -.8647259, tolerance = 0.001)
# # testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[6:10])), 2.36585, tolerance = 0.001)
# # testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[19:34])), 2.566117, tolerance = 0.001)
# # testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[35:48])), -3.508731, tolerance = 0.001)
# # testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[2])),  2.44331, tolerance = 0.001)
# #
# # # Total Reweighting Error
# # testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Reweighting_error[1])),  -.0810136, tolerance = 0.004)
# #
# #   ### Gini
# #
# #   gini_model <- as.formula(paste("exp(lwage2) ~ covered + nonwhite + nmarr +
# #                                 ed0 + ed1 + ed3 + ed4 + ed5 + ",
# #                                 paste(grep(paste0("^ex(", paste(c(1:4, 6:9), collapse = "|"), ")$"), names(men8816_t4), value = T), collapse = " + "), " + ",
# #                                 paste(grep(paste0("^occd(", paste(c(11:60, 80:91), collapse = "|"), ")$"), names(men8816_t4), value = T), collapse = " + "), " + ",
# #                                 paste(grep(paste0("^indd(", paste(c(1, 3:14), collapse = "|"), ")$"), names(men8816_t4), value = T), collapse = " + "), " + pub | ",
# #                                 paste("covered + nonwhite +",
# #                                       paste(grep("^marr", names(men8816_t4), value = TRUE), collapse = " + "), "+",
# #                                       paste(c("ed0", "ed1", "ed3", "ed4", "ed5"), collapse = " + "), "+",
# #                                       paste(grep("^ex[1-4]|^ex[6-9]", names(men8816_t4), value = TRUE), collapse = " + "), "+",
# #                                       paste(grep("^uned", names(men8816_t4), value = TRUE), collapse = " + "), "+",
# #                                       paste(grep("^unex", names(men8816_t4), value = TRUE), collapse = " + "), "+",
# #                                       paste(grep("^ex[1-9]ed", names(men8816_t4), value = TRUE), collapse = " + "), "+",
# #                                       paste(grep("^pub", names(men8816_t4), value = TRUE), collapse = " + "), "+",
# #                                       paste(grep("^indd(1|1e|[3-9]|10|11|13|14)(?!2)", names(men8816_t4), perl = TRUE, value = TRUE), collapse = " + "), "+",
# #                                       paste(grep("^occd", names(men8816_t4), value = TRUE), collapse = " + "))))
# #
# #
# #   decompose_gini  <- ddecompose::ob_decompose(formula = gini_model,
# #                                data = men8816_t4,
# #                                weights = eweight,
# #                                group = time,
# #                                rifreg_statistic = "gini",
# #                                reweighting = TRUE,
# #                                trimming = TRUE,
# #                                trimming_threshold = 100)
# #
# #
# #   # Overall
# #   testthat::expect_equal(round(as.numeric(100*decompose_gini$gini$decomposition_term$Observed_difference[1]), 3), 2.150751 + 4.448396, tolerance = 0.0001)
# #   testthat::expect_equal(round(as.numeric(100*decompose_gini$gini$decomposition_term$Composition_effect[1]), 3), 1.970835, tolerance = 0.0001)
# #   testthat::expect_equal(round(as.numeric(100*decompose_gini$gini$decomposition_term$Structure_effect[1]), 3), 4.489704 , tolerance = 0.0001)
# #
# #   # Composition Effects
# #   testthat::expect_equal(as.numeric(100*decompose_gini$gini$decomposition_term$Composition_effect[3]), .6364191, tolerance = 0.001)
# #   testthat::expect_equal(as.numeric(100*(sum(decompose_gini$gini$decomposition_term$Composition_effect[4:5]) +
# #                                            sum(decompose_gini$gini$decomposition_term$Composition_effect[11:18]))), .4801526 , tolerance = 0.0001)
# #   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Composition_effect[6:10])), .173072 , tolerance = 0.0001)
# #   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Composition_effect[19:34])), .1350762 , tolerance = 0.0001)
# #   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Composition_effect[35:48])),   .5461151, tolerance = 0.0001)
# #
# #   # Total Specification Error
# #   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Specification_error[1])),  .1799161, tolerance = 0.001)
# #
# #   # Wage Structure Effects
# #   testthat::expect_equal(as.numeric(100*decompose_gini$gini$decomposition_term$Structure_effect[3]), .2171314, tolerance = 0.001)
# #   testthat::expect_equal(as.numeric(100*(sum(decompose_gini$gini$decomposition_term$Structure_effect[4:5]) +
# #                                            sum(decompose_gini$gini$decomposition_term$Structure_effect[11:18]))), -.0680191, tolerance = 0.003)
# #   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[6:10])),  1.210563, tolerance = 0.001)
# #   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[19:34])), 1.30061 , tolerance = 0.001)
# #   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[35:48])),  -1.158205, tolerance = 0.001)
# #   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[2])), 2.987624 , tolerance = 0.001)
# #
# #   # Total Reweighting Error
# #   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Reweighting_error[1])),  -.0413075 , tolerance = 0.001)
#
#
# })
#
#
#
#
# test_that("ob_decompose() replicates Table 4 mit Bootstrap SE, p. 30, in FFL (2018)", {
#
#
# # browser()
# # set.seed(9283274)
# # sampled_indices <- sample(nrow(men8816_t4), size = 10000, replace = FALSE)
# # men8816_t4 <- men8816_t4[sampled_indices, ]
#
#
#   var_model <- as.formula(paste("lwage2 ~ covered + nonwhite + nmarr +
#                                 ed0 + ed1 + ed3 + ed4 + ed5 + ",
#                                 paste(grep(paste0("^ex(", paste(c(1:4, 6:9), collapse = "|"), ")$"), names(men8816_t4), value = T), collapse = " + "), " + ",
#                                 paste(grep(paste0("^occd(", paste(c(11:60, 80:91), collapse = "|"), ")$"), names(men8816_t4), value = T), collapse = " + "), " + ",
#                                 paste(grep(paste0("^indd(", paste(c(1, 3:14), collapse = "|"), ")$"), names(men8816_t4), value = T), collapse = " + "), " + pub | ",
#                                 paste("covered + nonwhite +",
#                                       paste(grep("^marr", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(c("ed0", "ed1", "ed3", "ed4", "ed5"), collapse = " + "), "+",
#                                       paste(grep("^ex[1-4]|^ex[6-9]", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^uned", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^unex", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^ex[1-9]ed", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^pub", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^indd(1|1e|[3-9]|10|11|13|14)(?!2)", names(men8816_t4), perl = TRUE, value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^occd", names(men8816_t4), value = TRUE), collapse = " + "))))
#
#
#
#
#
#
#   # # get cleaned Stata data
#   # men8816_t4 <- readstata13::read.dta13("data-raw/validation_data/FFL_2018_replication_code/usmen8816_t4.dta")
#   # men8816_t4 <- men8816_t4[men8816_t4$time <= 1,]
#   #
#   #
#   # men8816_sample <- readstata13::read.dta13("data-raw/validation_data/FFL_2018_replication_code/usmen8816_sample.dta")
#   # men8816_sample <- men8816_sample[men8816_sample$time <= 1,]
#
#   # IQR 90-10
#   decompose_90_10  <- ddecompose::ob_decompose(formula = var_model,
#                                 data = men8816_t4,
#                                 weights = eweight,
#                                 group = time,
#                                 reference_0 = TRUE,
#                                 rifreg_statistic = "interquantile_range",
#                                 rifreg_probs = c(0.9, 0.1),
#                                 reweighting_method = "fastglm",
#                                 bw = 0.065,
#                                 kernel = "epanechnikov",
#                                 reweighting = TRUE,
#                                 trimming = TRUE,
#                                 trimming_threshold = 10,
#                                 cores = 4,
#                                 bootstrap = TRUE,
#                                 bootstrap_iterations = 100)
#
#
#
#   # Overall
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Observed_difference[1]), 100*(.0923746  + .0328213) , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Composition_effect[1]), 100*.0900131  , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Structure_effect[1]), 100*.034831, tolerance = 0.017)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Composition_effect[3]), 100*.0162751, tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[11:18]))), 100*.0192389 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[6:10])), 100*.0072483, tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[19:34])), 100*.0207977 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Composition_effect[35:48])), 100*.0264531  , tolerance = 0.005)
#
#   # Total Specification Error
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Specification_error[1]),  100*.0023615  , tolerance = 0.04)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Structure_effect[3]), 100*.0121873, tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[11:18]))), 100*-.0481368 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[6:10])), 100*.0564411, tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[19:34])), 100*.0117771 , tolerance = 0.07)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[35:48])), 100*-.086405  , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_10$interquantile_range$decomposition_term$Structure_effect[2])), 100*.0889673, tolerance = 0.005)
#
#   # Total Reweighting Error
#   testthat::expect_equal(as.numeric(100*decompose_90_10$interquantile_range$decomposition_term$Reweighting_error[1]),  100*-.0020097, tolerance = 0.005)
#
#
#   #IQR 50-10
#   decompose_50_10  <- ddecompose::ob_decompose(formula = var_model,
#                                 data = men8816_t4,
#                                 weights = eweight,
#                                 group = time,
#                                 reference_0 = TRUE,
#                                 rifreg_statistic = "interquantile_range",
#                                 rifreg_probs = c(0.5, 0.1),
#                                 bw = 0.065,
#                                 kernel = "epanechnikov",
#                                 reweighting = TRUE,
#                                 trimming = TRUE,
#                                 trimming_threshold = 100,
#                                 bootstrap = TRUE,
#                                 bootstrap_iterations = 100)
#
#
#
#   # Overall
#   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Observed_difference[1]), 100*(.0283386  -.1037693)   , tolerance = 0.04)
#   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Composition_effect[1]), 100*.0380101   , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Structure_effect[1]), 100*-.1038954 , tolerance = 0.005)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Composition_effect[3]), 100*-.0187136 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[11:18]))), 100*.0085151 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[6:10])), 100*.0126624 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[19:34])), 100*.0226244  , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Composition_effect[35:48])), 100*.0129219  , tolerance = 0.005)
#
#   # Total Specification Error
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Specification_error[1])),  100*-.0096715   , tolerance = 0.3)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_50_10$interquantile_range$decomposition_term$Structure_effect[3]), 100*-.0050529 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[11:18]))), 100*-.0260159 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[6:10])), 100*.0103851 , tolerance = 0.03)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[19:34])), 100*-.0757565 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[35:48])), 100*-.0291649  , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Structure_effect[2])), 100*.0217097 , tolerance = 0.005)
#
#   # Total Reweighting Error
#   testthat::expect_equal(as.numeric(100*sum(decompose_50_10$interquantile_range$decomposition_term$Reweighting_error[1])),  100*.0001261, tolerance = 0.013)
#
#
#   #IQR 90-50
#   decompose_90_50  <- ddecompose::ob_decompose(formula = var_model,
#                                 data = men8816_t4,
#                                 weights = eweight,
#                                 group = time,
#                                 reference_0 = TRUE,
#                                 rifreg_statistic = "interquantile_range",
#                                 rifreg_probs = c(0.9, 0.5),
#                                 bw = 0.065,
#                                 kernel = "epanechnikov",
#                                 reweighting = TRUE,
#                                 trimming = TRUE,
#                                 trimming_threshold = 100,
#                                 bootstrap = TRUE,
#                                 bootstrap_iterations = 100)
#
#
#
#   # Overall
#   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Observed_difference[1]),100*(.0640361  + .1365906)   , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Composition_effect[1]), 100*.052003   , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Structure_effect[1]), 100*.1387263, tolerance = 0.005)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Composition_effect[3]), 100*.0349887 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[11:18]))), 100*.0107239  , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[6:10])), 100*-.0054142 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[19:34])), 100*-.0018267  , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Composition_effect[35:48])), 100*.0135312   , tolerance = 0.1)
#
#   # Total Specification Error
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Specification_error[1])), 100* .0120331, tolerance = 0.21)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_90_50$interquantile_range$decomposition_term$Structure_effect[3]), 100*.0172402 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[11:18]))), 100*-.022121 , tolerance = 0.01)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[6:10])), 100*.046056 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[19:34])), 100*.0875336 , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[35:48])), 100*-.0572401  , tolerance = 0.005)
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_term$Structure_effect[2])), 100*.0672576, tolerance = 0.01)
#
#   # Total Reweighting Error
#   testthat::expect_equal(as.numeric(100*sum(decompose_90_50$interquantile_range$decomposition_terms$Reweighting_error[1])),  100*-.0021358 , tolerance = 0.005)
#
#
#
#   ## Variance
#   decompose_variance  <- ddecompose::ob_decompose(formula = var_model,
#                                    data = men8816_t4,
#                                    weights = eweight,
#                                    group = time,
#                                    reference_0 = TRUE,
#                                    rifreg_statistic = "variance",
#                                    reweighting = TRUE,
#                                    trimming = TRUE,
#                                    trimming_threshold = 100,
#                                    bootstrap = TRUE,
#                                    bootstrap_iterations = 100)
#
#
#   # Overall
#   testthat::expect_equal(round(as.numeric(100*decompose_variance$variance$decomposition_term$Observed_difference[1]), 3),
#                          4.205934 + 3.332824 + .3173928 - .0810136  , tolerance = 0.0001)
#   testthat::expect_equal(round(as.numeric(100*decompose_variance$variance$decomposition_term$Composition_effect[1]), 3), 4.205934, tolerance = 0.0001)
#   testthat::expect_equal(round(as.numeric(100*decompose_variance$variance$decomposition_term$Structure_effect[1]), 3),  3.332824, tolerance = 0.0001)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_variance$variance$decomposition_term$Composition_effect[3]), .7111088 , tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_variance$variance$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_variance$variance$decomposition_term$Composition_effect[11:18]))), 1.005518, tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Composition_effect[6:10])), .5995667 , tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Composition_effect[19:34])), .733287, tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Composition_effect[35:48])),  1.156454 , tolerance = 0.001)
#
#   # Total Specification Error
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Specification_error[1])),  .3173928 , tolerance = 0.001)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_variance$variance$decomposition_term$Structure_effect[3]), .3310032 , tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_variance$variance$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_variance$variance$decomposition_term$Structure_effect[11:18]))), -.8647259, tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[6:10])), 2.36585, tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[19:34])), 2.566117, tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[35:48])), -3.508731, tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Structure_effect[2])),  2.44331, tolerance = 0.001)
#
#   # Total Reweighting Error
#   testthat::expect_equal(as.numeric(100*sum(decompose_variance$variance$decomposition_term$Reweighting_error[1])),  -.0810136, tolerance = 0.004)
#
#   ### Gini
#
#   gini_model <- as.formula(paste("exp(lwage2) ~ covered + nonwhite + nmarr +
#                                 ed0 + ed1 + ed3 + ed4 + ed5 + ",
#                                 paste(grep(paste0("^ex(", paste(c(1:4, 6:9), collapse = "|"), ")$"), names(men8816_t4), value = T), collapse = " + "), " + ",
#                                 paste(grep(paste0("^occd(", paste(c(11:60, 80:91), collapse = "|"), ")$"), names(men8816_t4), value = T), collapse = " + "), " + ",
#                                 paste(grep(paste0("^indd(", paste(c(1, 3:14), collapse = "|"), ")$"), names(men8816_t4), value = T), collapse = " + "), " + pub | ",
#                                 paste("covered + nonwhite +",
#                                       paste(grep("^marr", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(c("ed0", "ed1", "ed3", "ed4", "ed5"), collapse = " + "), "+",
#                                       paste(grep("^ex[1-4]|^ex[6-9]", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^uned", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^unex", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^ex[1-9]ed", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^pub", names(men8816_t4), value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^indd(1|1e|[3-9]|10|11|13|14)(?!2)", names(men8816_t4), perl = TRUE, value = TRUE), collapse = " + "), "+",
#                                       paste(grep("^occd", names(men8816_t4), value = TRUE), collapse = " + "))))
#
#
#   decompose_gini  <- ddecompose::ob_decompose(formula = gini_model,
#                                data = men8816_t4,
#                                weights = eweight,
#                                group = time,
#                                rifreg_statistic = "gini",
#                                reweighting = TRUE,
#                                trimming = TRUE,
#                                trimming_threshold = 100,
#                                bootstrap = TRUE,
#                                bootstrap_iterations = 100)
#
#
#
#   # Overall
#   testthat::expect_equal(round(as.numeric(100*decompose_gini$gini$decomposition_term$Observed_difference[1]), 3), 2.150751 + 4.448396, tolerance = 0.0001)
#   testthat::expect_equal(round(as.numeric(100*decompose_gini$gini$decomposition_term$Composition_effect[1]), 3), 1.970835, tolerance = 0.0001)
#   testthat::expect_equal(round(as.numeric(100*decompose_gini$gini$decomposition_term$Structure_effect[1]), 3), 4.489704 , tolerance = 0.0001)
#
#   # Composition Effects
#   testthat::expect_equal(as.numeric(100*decompose_gini$gini$decomposition_term$Composition_effect[3]), .6364191, tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_gini$gini$decomposition_term$Composition_effect[4:5]) +
#                                            sum(decompose_gini$gini$decomposition_term$Composition_effect[11:18]))), .4801526 , tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Composition_effect[6:10])), .173072 , tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Composition_effect[19:34])), .1350762 , tolerance = 0.0001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Composition_effect[35:48])),   .5461151, tolerance = 0.0001)
#
#   # Total Specification Error
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Specification_error[1])),  .1799161, tolerance = 0.001)
#
#   # Wage Structure Effects
#   testthat::expect_equal(as.numeric(100*decompose_gini$gini$decomposition_term$Structure_effect[3]), .2171314, tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*(sum(decompose_gini$gini$decomposition_term$Structure_effect[4:5]) +
#                                            sum(decompose_gini$gini$decomposition_term$Structure_effect[11:18]))), -.0680191, tolerance = 0.003)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[6:10])),  1.210563, tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[19:34])), 1.30061 , tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[35:48])),  -1.158205, tolerance = 0.001)
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Structure_effect[2])), 2.987624 , tolerance = 0.001)
#
#   # Total Reweighting Error
#   testthat::expect_equal(as.numeric(100*sum(decompose_gini$gini$decomposition_term$Reweighting_error[1])),  -.0413075 , tolerance = 0.001)
#
#
# })
#
#
#
# test_that("same results as in lecture 7, slide 29", {
#
#   # get and prepare data
#   #gsoep29 <- readstata13::read.dta13("data-raw/validation_data/gsoep29.dta")
#   total_n <- nrow(gsoep29)
#   gsoep29$age <- 2012 - gsoep29$bcgeburt
#   gsoep29 <- subset(gsoep29, age >=25 & age <= 55)
#   total_n - nrow(gsoep29)
#
#   gsoep29$wage <- with(gsoep29, ifelse(labgro12 > 0 & bctatzeit > 0, labgro12 / (bctatzeit * 4.3), NA))
#   gsoep29$lnwage <- log(gsoep29$wage)
#
#   gsoep29$schooling <- with(gsoep29, ifelse(bcbilzeit > 0, bcbilzeit, NA))
#   gsoep29$ft_experience <- with(gsoep29, ifelse(expft12 >= 0, expft12, NA))
#   gsoep29$ft_experience2 <- with(gsoep29, ifelse(expft12 >= 0, expft12^2, NA))
#
#   gsoep29$public <- ifelse(gsoep29$oeffd12 == "[1] Ja" | gsoep29$oeffd12 == "[2] Nein", as.integer(gsoep29$oeffd12 == "[1] Ja"), NA)
#
#   summary(gsoep29[c("lnwage", "schooling", "ft_experience", "ft_experience2")])
#
#
#   #summary(gsoep29[c("bcsex", "wage", "lnwage", "schooling", "ft_experience", "ft_experience2")])
#
#   # gsoep29$tenure <- with(gsoep29, ifelse(bcerwzeit >= 0, bcerwzeit, NA))
#   # gsoep29$ISEI <- with(gsoep29, ifelse(isei12 > 0, isei12, NA))
#
#   gsoep29 <- na.omit(gsoep29[, c("lnwage", "schooling", "ft_experience", "ft_experience2", "public")])
#
#
#   ## RIF Regression
#
#   # slide 7, p.29
#   rifreg_decompose <- ob_decompose(formula = lnwage ~ schooling + ft_experience + ft_experience2,
#                          data = gsoep29,
#                          group = public,
#                          reweighting = FALSE,
#                          rifreg_statistic = "variance",
#                          reference_0 = TRUE)
#
#   testthat::expect_equal(rifreg_decompose$variance$decomposition_terms$Observed_difference[1],
#                          -0.165342,
#                          tolerance = 0.0075)
#
#   testthat::expect_equal(rifreg_decompose$variance$decomposition_terms$Composition_effect[c(1, 3)],
#                          c(0.0289454, 0.025752),
#                          tolerance = 0.0075)
#
#   testthat::expect_equal(sum(rifreg_decompose$variance$decomposition_terms$Composition_effect[4:5]),
#                          0.0031934,
#                          tolerance = 0.0075)
#
#   testthat::expect_equal(rifreg_decompose$variance$decomposition_terms$Structure_effect[1:3],
#                          c(-0.1942874, 0.2323155, -0.34344 ),
#                          tolerance = 0.0075)
#
# })
#
#
# test_that("ob_decompose() returns the same results as R-pacakge oaxaca", {
#   set.seed(43825081)
#   library("oaxaca")
#   data("chicago")
#
#   oaxaca_results <- oaxaca(ln.real.wage ~ age + female | foreign.born,
#                              data = chicago, R = 100)
#
#   ddecompose_results <- ob_decompose(formula = ln.real.wage ~ age + female,
#                            data = chicago,
#                            group = foreign.born,
#                            bootstrap = TRUE,
#                            bootstrap_iterations = 100)
#
#   # no errors
#   testthat::expect_error(oaxaca_results, NA)
#   testthat::expect_error(ddecompose_results, NA)
#
#   # same regression results
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_0$coefficients,
#                          oaxaca_results$reg$reg.A$coefficients)
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_0$residuals,
#                          oaxaca_results$reg$reg.A$residuals)
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_0$fitted.values,
#                          oaxaca_results$reg$reg.A$fitted.values)
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_0$effects,
#                          oaxaca_results$reg$reg.A$effects)
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_0$rank,
#                          oaxaca_results$reg$reg.A$rank)
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_0$assign,
#                          oaxaca_results$reg$reg.A$assign)
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_0$df.residual,
#                          oaxaca_results$reg$reg.A$df.residual)
#
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_1$coefficients,
#                          oaxaca_results$reg$reg.B$coefficients)
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_1$residuals,
#                          oaxaca_results$reg$reg.B$residuals)
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_1$fitted.values,
#                          oaxaca_results$reg$reg.B$fitted.values)
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_1$effects,
#                          oaxaca_results$reg$reg.B$effects)
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_1$rank,
#                          oaxaca_results$reg$reg.B$rank)
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_1$assign,
#                          oaxaca_results$reg$reg.B$assign)
#   testthat::expect_equal(ddecompose_results$ob_decompose$model_fits$fit_group_1$df.residual,
#                          oaxaca_results$reg$reg.B$df.residual)
#
#
#   ## same decomposition results
#   # overall
#   testthat::expect_equal(ddecompose_results$ob_decompose$decomposition_terms$Composition_effect[1]*(-1), # we compute X1-X0
#                          unname(oaxaca_results$twofold$overall[2,2]))
#   testthat::expect_equal(ddecompose_results$ob_decompose$decomposition_terms$Structure_effect[1]*(-1),
#                          unname(oaxaca_results$twofold$overall[2,4]))
#
#   # detailed
#   testthat::expect_equal(ddecompose_results$ob_decompose$decomposition_terms$Composition_effect[2:4]*(-1),
#                          unname(oaxaca_results$twofold$variables[[2]][,2]))
#   testthat::expect_equal(ddecompose_results$ob_decompose$decomposition_terms$Structure_effect[2:4]*(-1),
#                          unname(oaxaca_results$twofold$variables[[2]][,4]))
#
#   # SE similar
#   testthat::expect_equal(ddecompose_results$ob_decompose$decomposition_vcov$decomposition_terms_se$Composition_effect[1],
#                          unname(oaxaca_results$twofold$overall[2,3]),
#                          tolerance = 0.03)
#   testthat::expect_equal(ddecompose_results$ob_decompose$decomposition_vcov$decomposition_terms_se$Structure_effect[1],
#                          unname(oaxaca_results$twofold$overall[2,5]),
#                          tolerance = 0.1)
#
#   testthat::expect_equal(ddecompose_results$ob_decompose$decomposition_vcov$decomposition_terms_se$Composition_effect[2:4],
#                          unname(oaxaca_results$twofold$variables[[2]][,3]),
#                          tolerance = 0.03)
#   testthat::expect_equal(ddecompose_results$ob_decompose$decomposition_vcov$decomposition_terms_se$Structure_effect[2:4],
#                          unname(oaxaca_results$twofold$variables[[2]][,5]),
#                          tolerance = 0.1)
#
#   # compare analytical se
#   ddecompose_results_analytical_se <- ob_decompose(formula = ln.real.wage ~ age + female,
#                                          data = chicago,
#                                          group = foreign.born)
#
#   testthat::expect_equal(ddecompose_results_analytical_se$ob_decompose$decomposition_vcov$decomposition_terms_se$Composition_effect[1],
#                          unname(oaxaca_results$twofold$overall[2,3]),
#                          tolerance = 0.03)
#   testthat::expect_equal(ddecompose_results_analytical_se$ob_decompose$decomposition_vcov$decomposition_terms_se$Structure_effect[1],
#                          unname(oaxaca_results$twofold$overall[2,5]),
#                          tolerance = 0.08)
#
#   testthat::expect_equal(ddecompose_results_analytical_se$ob_decompose$decomposition_vcov$decomposition_terms_se$Composition_effect[2:4],
#                          unname(oaxaca_results$twofold$variables[[2]][,3]),
#                          tolerance = 0.03)
#   testthat::expect_equal(ddecompose_results_analytical_se$ob_decompose$decomposition_vcov$decomposition_terms_se$Structure_effect[2:4],
#                          unname(oaxaca_results$twofold$variables[[2]][,5]),
#                          tolerance = 0.08)
# })

