# analysis runs without weights
test_that("Gives corrects results", {
  #without a dap
  no_dap_expected_output <- readRDS(testthat::test_path("fixtures", "results_create_analysis_no_dap_v1.RDS"))

  no_dap_test_design <- srvyr::as_survey(no_dap_expected_output$dataset)
  actual_output <- create_analysis(no_dap_test_design, group_var = "admin1")

  expect_equal(actual_output, no_dap_expected_output, ignore_attr = T)

  #with a dap #add ratios
  with_dap_expected_output <- readRDS(testthat::test_path("fixtures", "results_create_analysis_dap_v1.RDS"))

  with_dap_test_design <- srvyr::as_survey(with_dap_expected_output$dataset)
  with_dap_actual_output <- create_analysis(with_dap_test_design, dap = with_dap_expected_output$dap)

  expect_equal(with_dap_actual_output, with_dap_expected_output, ignore_attr = T)

  # with dap and no ratio
  no_ratio_dap <- with_dap_expected_output$dap %>%
    dplyr::filter(analysis_type != "ratio")

  no_ratio_dap_actual_output <- create_analysis(with_dap_test_design, no_ratio_dap)

  no_ratio_dap_expected_results_table <- with_dap_expected_output$results_table %>%
    dplyr::filter(analysis_type != "ratio")

  expect_equal(no_ratio_dap_actual_output$results_table, no_ratio_dap_expected_results_table, ignore_attr = T)

})


# errors in dap
test_that("Errors are caught correctly", {
  # Cannot identify design
  expect_error(create_analysis(analysistools_MSNA_template_data, no_ratio_dap),
               "It seems object design is not a design, did you use srvyr::as_survey ?")

  wrong_shape_dap1 <- data.frame(analysis = "mean", analysis_var = "a1", group_var = "b1", level = .95)
  wrong_shape_dap2 <- data.frame(analysis_type = "mean", var = "a1", group_var = "b1", level = .95)
  wrong_shape_dap3 <- data.frame(analysis_type = "mean", analysis_var = "a1", group = "b1", level = .95)

  expect_error(check_dap(dap = data.frame(wrong_shape_dap1)),
               "Make sure you have at least analysis_type, group_var, analysis_var in your dap")
  expect_error(check_dap(dap = data.frame(wrong_shape_dap2)),
               "Make sure you have at least analysis_type, group_var, analysis_var in your dap")
  expect_error(check_dap(dap = data.frame(wrong_shape_dap3)),
               "Make sure you have at least analysis_type, group_var, analysis_var in your dap")

  typo_dap <- data.frame(analysis_type = c("quantile", "crazyfunction", "mean"),
                         analysis_var = rep("a1",3),
                         group_var = rep("b1",3),
                         level = rep(.95,3))

  # If dap provided, one type of analysis is not implemented
  expect_error(check_dap(typo_dap),
               "The following analysis type are not yet implemented or check for typo: quantile, crazyfunction")

  # If dap provided, cannot identify a variable
  missing_var_dap1 <- data.frame(analysis_type = c("mean"),
                                 analysis_var = "wash_soap",
                                 group_var = "b1",
                                 level = .95)

  missing_var_dap2 <- data.frame(analysis_type = c("mean"),
                                 analysis_var = "a1",
                                 group_var = "wash_soap",
                                 level = .95)

  expect_error(check_dap(missing_var_dap1,
                         srvyr::as_survey(analysistools_MSNA_template_data)),
               "The following group variables are not present in the dataset: b1")
  expect_error(check_dap(missing_var_dap2,
                         srvyr::as_survey(analysistools_MSNA_template_data)),
               "The following analysis variables are not present in the dataset: a1")

  missing_var_dap3 <- data.frame(analysis_type = c("mean", "ratio"),
                                 analysis_var = c("wash_soap", NA),
                                 group_var = c("admin1", NA),
                                 level = rep(.95, 2),
                                 analysis_var_numerator = c(NA, "income_v1_salaried_work"),
                                 analysis_var_denominator = c(NA, "c1"),
                                 numerator_NA_to_0 = c(TRUE, TRUE),
                                 filter_denominator_0 = c(TRUE, TRUE))
  missing_var_dap4 <- data.frame(analysis_type = c("mean", "ratio"),
                                 analysis_var = c("wash_soap", NA),
                                 group_var = c("admin1", NA),
                                 level = rep(.95, 2),
                                 analysis_var_numerator = c(NA, "d1"),
                                 analysis_var_denominator = c(NA, "income_v1_salaried_work"),
                                 numerator_NA_to_0 = c(TRUE, TRUE),
                                 filter_denominator_0 = c(TRUE, TRUE))

  expect_error(check_dap(missing_var_dap3,
                         srvyr::as_survey(analysistools_MSNA_template_data)),
               "The following analysis denominator variables are not present in the dataset: c1")
  expect_error(check_dap(missing_var_dap4,
                         srvyr::as_survey(analysistools_MSNA_template_data)),
               "The following analysis numerator variables are not present in the dataset: d1")

})

test_that("If dap and group variable are provided, group_var will be ignored", {
  expected_output <- readRDS(testthat::test_path("fixtures", "results_create_analysis_dap_v1.RDS"))

  expect_warning(create_analysis(.design = srvyr::as_survey(expected_output$dataset),
                                 dap = expected_output$dap,
                                 group_var = "admin1"),
               "You have provided a data analysis plan and group variable, group variable will be ignored")

})

# create_dap with no grouping, 1 grouping, 2 grouping.
test_that("create_dap creates correctly with different grouping variables", {
  test_data <- data.frame(number_variable = sample(1:4, size = 5, replace = TRUE),
                          char_variable1 = sample(letters, size = 5),
                          char_variable2 = sample(LETTERS, size = 5))
  expected_dap <- data.frame(analysis_type = c("mean", "median", "prop_select_one", "prop_select_one"),
                             analysis_var = c("number_variable", "number_variable", "char_variable1", "char_variable2"),
                             group_var = rep(NA_character_, 4),
                             level = rep(.95, 4))

  expect_equal(create_dap(srvyr::as_survey(test_data)),
                          expected_dap)

  extenstion_dap2 <- data.frame(analysis_type = c("mean", "median", "prop_select_one"),
                                analysis_var = c("number_variable", "number_variable", "char_variable2"),
                                group_var = rep("char_variable1", 3),
                                level = rep(.95, 3))

  expected_dap2 <- rbind(expected_dap, extenstion_dap2)
  expect_equal(create_dap(srvyr::as_survey(test_data), group_var = "char_variable1"),
               expected_dap2)

  extenstion_dap3 <- data.frame(analysis_type = c("mean", "median"),
                                analysis_var = c("number_variable", "number_variable"),
                                group_var = rep("char_variable1, char_variable2", 2),
                                level = rep(.95, 2))

  expected_dap3 <- rbind(expected_dap2, extenstion_dap3)
  expect_equal(create_dap(srvyr::as_survey(test_data),
                          group_var = c("char_variable1", "char_variable1, char_variable2")),
               expected_dap3)
})


