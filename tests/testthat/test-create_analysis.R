# analysis runs without weights
test_that("Gives corrects results", {
  # without a loa
  no_loa_expected_output <- readRDS(testthat::test_path("fixtures", "results_create_analysis_no_loa_v1.RDS"))

  no_loa_test_design <- srvyr::as_survey(no_loa_expected_output$dataset)
  actual_output <- create_analysis(no_loa_test_design, group_var = "admin1")

  expect_equal(actual_output, no_loa_expected_output, ignore_attr = T)

  # with a loa #add ratios
  with_loa_expected_output <- readRDS(testthat::test_path("fixtures", "results_create_analysis_with_loa_v1.RDS"))

  with_loa_test_design <- srvyr::as_survey(with_loa_expected_output$dataset)
  with_loa_actual_output <- create_analysis(with_loa_test_design, loa = with_loa_expected_output$loa)

  expect_equal(with_loa_actual_output, with_loa_expected_output, ignore_attr = T)

  # with loa and no ratio
  no_ratio_loa <- with_loa_expected_output$loa %>%
    dplyr::filter(analysis_type != "ratio")

  no_ratio_loa_actual_output <- create_analysis(with_loa_test_design, no_ratio_loa)

  no_ratio_loa_expected_results_table <- with_loa_expected_output$results_table %>%
    dplyr::filter(analysis_type != "ratio")

  expect_equal(no_ratio_loa_actual_output$results_table, no_ratio_loa_expected_results_table, ignore_attr = T)
})


# errors in loa
test_that("Errors are caught correctly", {
  # Cannot identify design
  expect_error(
    create_analysis(analysistools_MSNA_template_data, no_ratio_loa),
    "It seems object design is not a design, did you use srvyr::as_survey ?"
  )

  wrong_shape_loa1 <- data.frame(analysis = "mean", analysis_var = "a1", group_var = "b1", level = .95)
  wrong_shape_loa2 <- data.frame(analysis_type = "mean", var = "a1", group_var = "b1", level = .95)
  wrong_shape_loa3 <- data.frame(analysis_type = "mean", analysis_var = "a1", group = "b1", level = .95)

  expect_error(
    check_loa(loa = data.frame(wrong_shape_loa1)),
    "Make sure you have at least analysis_type, group_var, analysis_var in your loa"
  )
  expect_error(
    check_loa(loa = data.frame(wrong_shape_loa2)),
    "Make sure you have at least analysis_type, group_var, analysis_var in your loa"
  )
  expect_error(
    check_loa(loa = data.frame(wrong_shape_loa3)),
    "Make sure you have at least analysis_type, group_var, analysis_var in your loa"
  )

  typo_loa <- data.frame(
    analysis_type = c("quantile", "crazyfunction", "mean"),
    analysis_var = rep("a1", 3),
    group_var = rep("b1", 3),
    level = rep(.95, 3)
  )

  # If loa provided, one type of analysis is not implemented
  expect_error(
    check_loa(typo_loa),
    "The following analysis type are not yet implemented or check for typo: quantile, crazyfunction"
  )

  # If loa provided, cannot identify a variable
  missing_var_loa1 <- data.frame(
    analysis_type = c("mean"),
    analysis_var = "wash_soap",
    group_var = "b1",
    level = .95
  )

  missing_var_loa2 <- data.frame(
    analysis_type = c("mean"),
    analysis_var = "a1",
    group_var = "wash_soap",
    level = .95
  )

  expect_error(
    check_loa(
      missing_var_loa1,
      srvyr::as_survey(analysistools_MSNA_template_data)
    ),
    "The following group variables are not present in the dataset: b1"
  )
  expect_error(
    check_loa(
      missing_var_loa2,
      srvyr::as_survey(analysistools_MSNA_template_data)
    ),
    "The following analysis variables are not present in the dataset: a1"
  )

  missing_var_loa3 <- data.frame(
    analysis_type = c("mean", "ratio"),
    analysis_var = c("wash_soap", NA),
    group_var = c("admin1", NA),
    level = rep(.95, 2),
    analysis_var_numerator = c(NA, "income_v1_salaried_work"),
    analysis_var_denominator = c(NA, "c1"),
    numerator_NA_to_0 = c(TRUE, TRUE),
    filter_denominator_0 = c(TRUE, TRUE)
  )
  missing_var_loa4 <- data.frame(
    analysis_type = c("mean", "ratio"),
    analysis_var = c("wash_soap", NA),
    group_var = c("admin1", NA),
    level = rep(.95, 2),
    analysis_var_numerator = c(NA, "d1"),
    analysis_var_denominator = c(NA, "income_v1_salaried_work"),
    numerator_NA_to_0 = c(TRUE, TRUE),
    filter_denominator_0 = c(TRUE, TRUE)
  )

  expect_error(
    check_loa(
      missing_var_loa3,
      srvyr::as_survey(analysistools_MSNA_template_data)
    ),
    "The following analysis denominator variables are not present in the dataset: c1"
  )
  expect_error(
    check_loa(
      missing_var_loa4,
      srvyr::as_survey(analysistools_MSNA_template_data)
    ),
    "The following analysis numerator variables are not present in the dataset: d1"
  )
})

test_that("If loa and group variable are provided, group_var will be ignored", {
  expected_output <- readRDS(testthat::test_path("fixtures", "results_create_analysis_with_loa_v1.RDS"))

  expect_warning(
    create_analysis(
      .design = srvyr::as_survey(expected_output$dataset),
      loa = expected_output$loa,
      group_var = "admin1"
    ),
    "You have provided a list of analysis and group variable, group variable will be ignored"
  )
})

# create_loa with no grouping, 1 grouping, 2 grouping.
test_that("create_loa creates correctly with different grouping variables", {
  test_data <- data.frame(
    number_variable = sample(1:4, size = 5, replace = TRUE),
    char_variable1 = sample(letters, size = 5),
    char_variable2 = sample(LETTERS, size = 5)
  )
  expected_loa <- data.frame(
    analysis_type = c("mean", "median", "prop_select_one", "prop_select_one"),
    analysis_var = c("number_variable", "number_variable", "char_variable1", "char_variable2"),
    group_var = rep(NA_character_, 4),
    level = rep(.95, 4)
  )

  expect_equal(
    create_loa(srvyr::as_survey(test_data)),
    expected_loa
  )

  extenstion_loa2 <- data.frame(
    analysis_type = c("mean", "median", "prop_select_one"),
    analysis_var = c("number_variable", "number_variable", "char_variable2"),
    group_var = rep("char_variable1", 3),
    level = rep(.95, 3)
  )

  expected_loa2 <- rbind(expected_loa, extenstion_loa2)
  expect_equal(
    create_loa(srvyr::as_survey(test_data), group_var = "char_variable1"),
    expected_loa2
  )

  extenstion_loa3 <- data.frame(
    analysis_type = c("mean", "median"),
    analysis_var = c("number_variable", "number_variable"),
    group_var = rep("char_variable1, char_variable2", 2),
    level = rep(.95, 2)
  )

  expected_loa3 <- rbind(expected_loa2, extenstion_loa3)
  expect_equal(
    create_loa(srvyr::as_survey(test_data),
      group_var = c("char_variable1", "char_variable1, char_variable2")
    ),
    expected_loa3
  )
})
