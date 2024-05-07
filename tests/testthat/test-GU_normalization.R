testthat::test_that("Test GU normalization", {
  data("men8305")
  data0 <- subset(men8305, year == "1983-1985")
  data1 <- subset(men8305, year != "1983-1985")

  m0 <- mean(data0$wage)
  m1 <- mean(data1$wage)

  m0_union <- mean(subset(data0, union == "yes")$wage)
  m0_non_union <- mean(subset(data0, union != "yes")$wage)

  m1_union <- mean(subset(data1, union == "yes")$wage)
  m1_non_union <- mean(subset(data1, union != "yes")$wage)

  union_share0 <- mean(as.numeric(data0$union)) - 1
  union_share1 <- mean(as.numeric(data1$union)) - 1

  mC <- m1_union * (union_share0) + m1_non_union * (1 - union_share0)

  intercept_0 <- (m0_union + m0_non_union) / 2
  intercept_1 <- (m1_union + m1_non_union) / 2
  coef_union_0 <- m0_union - intercept_0
  coef_union_1 <- m1_union - intercept_1
  coef_non_union_0 <- m0_non_union - intercept_0
  coef_non_union_1 <- m1_non_union - intercept_1

  detailed_wage_structure_effect <- c(
    (intercept_1 - intercept_0),
    (coef_non_union_1 - coef_non_union_0) * (1 - union_share0),
    (coef_union_1 - coef_union_0) * union_share0
  )

  detailed_composition_effect <- c(
    0,
    coef_non_union_1 * ((1 - union_share1) - (1 - union_share0)),
    coef_union_1 * (union_share1 - union_share0)
  )

  expected_decompose <- data.frame(
    `Composition_effect` = detailed_composition_effect,
    `Structure_effect` = detailed_wage_structure_effect
  )
  rownames(expected_decompose) <- c("(Intercept)", "unionno", "unionyes")

  model_decompose <- wage ~ union
  decompose_union <- ob_decompose(
    formula = model_decompose,
    data = men8305,
    group = year,
    normalize_factors = TRUE,
    reference_0 = FALSE
  )

  estimated_decompose <-
    decompose_union$ob_decompose$decomposition_terms[
      which(rownames(decompose_union$ob_decompose$decomposition_terms) %in%
        rownames(expected_decompose)), names(expected_decompose)
    ]

  testthat::expect_equal(estimated_decompose,
    expected_decompose,
    tolerance = 0.0000000001
  )
})


# test_that("test in GU normalization yields the same results as in Ben Janns lecture notes", {
#
#   # See p. 27, Decomposition methods in the social sciences, Fall 2019, The transformation problem
#
#   # get and prepare data
#   gsoep29 <- readstata13::read.dta13("data-raw/validation_data/gsoep29.dta")
#   total_n <- nrow(gsoep29)
#   gsoep29$age <- 2012 - gsoep29$bcgeburt
#   gsoep29 <- subset(gsoep29, age >=25 & age <= 55)
#   n_2 <- nrow(gsoep29)
#   total_n - n_2
#
#   gsoep29$wage <- with(gsoep29, ifelse(labgro12 > 0 & bctatzeit > 0, labgro12 / (bctatzeit * 4.3), NA))
#   gsoep29$lnwage <- log(gsoep29$wage)
#
#   gsoep29$schooling <- with(gsoep29, ifelse(bcbilzeit > 0, bcbilzeit, NA))
#   gsoep29$ft_experience <- with(gsoep29, ifelse(expft12 >= 0, expft12, NA))
#   gsoep29$ft_experience2 <- with(gsoep29, ifelse(expft12 >= 0, expft12^2, NA))
#
#   gsoep29$female <- with(gsoep29, ifelse(bcsex == "[1] Maennlich", 0, 1))
#
#   summary(gsoep29[c("lnwage", "schooling", "ft_experience", "ft_experience2", "female")]) # means are identical to exercice
#
#   gsoep29$casmin4 <- dplyr::recode_factor( gsoep29$casmin12,
#                                           "[1]  (1a) inadequately completed"="low",
#                                           "[2]  (1b) general elementary school"="low",
#                                           "[4]  (2b) intermediate general qualification" = "medium general",
#                                           "[6]  (2c_gen) general maturity certificate" = "medium general",
#                                           "[3]  (1c) basic vocational qualification" = "medium vocational",
#                                           "[5]  (2a) intermediate vocational" = "medium vocational",
#                                           "[7]  (2c_voc) vocational maturity certificat" = "medium vocational",
#                                           "[8]  (3a) lower tertiary education" = "high",
#                                           "[9]  (3b) higher tertiary education" = "high",
#                                           .default = NA_character_)
#   levels(gsoep29$casmin4)
#   summary(gsoep29$casmin4)
#
#   # gsoep29 <- na.omit(gsoep29[, c("lnwage", "schooling", "ft_experience", "ft_experience2", "bcsex", "bcphrf", "female" )])
#   # n_3 <- nrow(gsoep29)
#   # n_2 - n_3
#
#
#   ### without GU normalization
#   ob_model_with_GU_decompose_1 <- ob_decompose(formula = lnwage ~ casmin4 + ft_experience + ft_experience2,
#                                data = gsoep29,
#                                group = female,
#                                subtract_1_from_0 = TRUE,
#                                normalize_factors = FALSE,
#                                reference_0 = TRUE)
#
#   res1 <- ob_model_with_GU_decompose_1$ob_decompose$decomposition_terms[3:5, 3:4]
#   res1_Jann <- data.frame(Composition_effect = c(.0002573, -.0002933, .0052819),
#                           Structure_effect = c(.0052416, .0240202, .0331881))
#   rownames(res1_Jann) <- rownames(res1)
#   testthat::expect_equal(as.matrix(res1),
#                          as.matrix(res1_Jann),
#                          tolerance = 0.0001)
#
#   ### with GU normalization
#   ob_model_with_GU_decompose_2 <- ob_decompose(formula = lnwage ~ casmin4 + ft_experience + ft_experience2,
#                                                data = gsoep29,
#                                                group = female,
#                                                subtract_1_from_0 = TRUE,
#                                                normalize_factors = TRUE,
#                                                reference_0 = TRUE)
#   res2 <- ob_model_with_GU_decompose_2$ob_decompose$decomposition_terms[3:6, 3:4]
#   res2_Jann <- data.frame(Composition_effect = c(.0022361, -.0001001, .0000367, .0030731),
#                           Structure_effect = c(-.0037775, .00204, -.0184644, .0147239))
#   rownames(res2_Jann) <- rownames(res2)
#   testthat::expect_equal(as.matrix(res2),
#                          as.matrix(res2_Jann),
#                          tolerance = 0.0001)
#
#   res2_se <- data.frame(Composition_effect = sqrt(diag(ob_model_with_GU_decompose_2$ob_decompose$decomposition_vcov$decomposition_terms_vcov$Composition_effect))[2:5],
#                         Structure_effect =  sqrt(diag(ob_model_with_GU_decompose_2$ob_decompose$decomposition_vcov$decomposition_terms_vcov$Structure_effect))[2:5])
#   res2_Jann_se <- data.frame(Composition_effect = c(.0016403, .0004385, .0003947, .0045529),
#                           Structure_effect = c(.0022963 , .002024, .0141663, .0068526))
#   rownames(res2_se) <- rownames(res2_Jann_se) <- rownames(res2)
#   testthat::expect_equal(as.matrix(res2_se),
#                          as.matrix(res2_Jann_se),
#                          tolerance = 0.01)
#
#   ob_model_with_GU_decompose_2bs <- ob_decompose(formula = lnwage ~ casmin4 + ft_experience + ft_experience2,
#                                                data = gsoep29,
#                                                group = female,
#                                                subtract_1_from_0 = TRUE,
#                                                normalize_factors = TRUE,
#                                                reference_0 = TRUE,
#                                                bootstrap = TRUE,
#                                                bootstrap_iterations = 200)
#   res2bs_se <- data.frame(Composition_effect = sqrt(diag(ob_model_with_GU_decompose_2bs$ob_decompose$decomposition_vcov$decomposition_terms_vcov$Composition_effect))[2:5],
#                            Structure_effect =  sqrt(diag(ob_model_with_GU_decompose_2bs$ob_decompose$decomposition_vcov$decomposition_terms_vcov$Structure_effect))[2:5])
#
# })
