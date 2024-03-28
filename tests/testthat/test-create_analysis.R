# analysis runs without weights
test_that("Gives corrects results", {
  # without a loa
  no_loa_expected_output <- readRDS(testthat::test_path("fixtures", "results_create_analysis_no_loa_v3.RDS"))

  no_loa_test_design <- srvyr::as_survey(no_loa_expected_output$dataset)
  actual_output <- create_analysis(no_loa_test_design, group_var = "admin1", sm_separator = "/")

  expect_equal(actual_output, no_loa_expected_output, ignore_attr = T)

  # with loa without ratio
  test_design <- srvyr::as_survey(analysistools_MSNA_template_data)

  no_ratio_loa_actual_output <- create_analysis(test_design, analysistools_MSNA_template_loa, sm_separator = "/")

  expect_equal(no_ratio_loa_actual_output, analysistools_MSNA_template_no_ratio_results_table, ignore_attr = T)

  # with a loa with ratios
  with_loa_actual_output <- create_analysis(test_design, loa = analysistools_MSNA_template_loa_with_ratio, sm_separator = "/")

  expect_equal(with_loa_actual_output, analysistools_MSNA_template_with_ratio_results_table, ignore_attr = T)
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
    check_loa(loa = wrong_shape_loa1),
    "Make sure to have analysis_type in your loa"
  )
  expect_error(
    check_loa(loa = wrong_shape_loa2),
    "Make sure to have group_var, analysis_var in your loa"
  )
  expect_error(
    check_loa(loa = wrong_shape_loa3),
    "Make sure to have group_var, analysis_var in your loa"
  )

  #checks that a loa can only have ratios
  ratio_2groups_test_loa <- data.frame(analysis_type = "ratio",
                                       analysis_var_numerator = "income_v1_salaried_work",
                                       analysis_var_denominator = "expenditure_debt",
                                       group_var = "admin1",
                                       level = ".95",
                                       numerator_NA_to_0 = TRUE,
                                       filter_denominator_0 = TRUE
  )
  expect_equal(
    check_loa(loa = ratio_2groups_test_loa, srvyr::as_survey(analysistools_MSNA_template_data)),
    ratio_2groups_test_loa
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
  expect_warning(
    create_analysis(
      design = srvyr::as_survey(analysistools::analysistools_MSNA_template_data),
      loa = analysistools::analysistools_MSNA_template_loa_with_ratio,
      group_var = "admin1",
      sm_separator = "/"
    ),
    "You have provided a list of analysis and group variable, group variable will be ignored"
  )
})

# create_loa with no grouping, 1 grouping, 2 grouping.
test_that("create_loa creates correctly with different grouping variables", {
  test_data <- data.frame(
    number_variable = sample(1:4, size = 5, replace = TRUE),
    char_variable1 = sample(letters, size = 5),
    char_variable_sm.option1 = TRUE,
    char_variable_sm.option2 = sample(c(TRUE, FALSE), 5, TRUE),
    char_variable_sm.option3 = sample(c(TRUE, FALSE), 5, TRUE),
    char_variable2 = sample(LETTERS, size = 5)
  )
  test_data <- test_data %>%
    dplyr::mutate(uuid = letters[1:5], char_variable_sm = NA_character_) %>%
    cleaningtools::recreate_parent_column()
  test_data <- test_data$data_with_fix_concat %>%
    dplyr::select(number_variable, char_variable1, char_variable_sm, dplyr::starts_with("char_variable_sm."), char_variable2)
  expected_loa <- data.frame(
    analysis_type = c("mean", "median", "prop_select_one", "prop_select_multiple", "prop_select_one"),
    analysis_var = c("number_variable", "number_variable", "char_variable1", "char_variable_sm", "char_variable2"),
    group_var = rep(NA_character_, 5),
    level = rep(.95, 5)
  )

  expect_equal(
    create_loa(srvyr::as_survey(test_data)),
    expected_loa
  )

  extenstion_loa2 <- data.frame(
    analysis_type = c("mean", "median", "prop_select_multiple", "prop_select_one"),
    analysis_var = c("number_variable", "number_variable", "char_variable_sm", "char_variable2"),
    group_var = rep("char_variable1", 4),
    level = rep(.95, 4)
  )

  expected_loa2 <- rbind(expected_loa, extenstion_loa2)
  expect_equal(
    create_loa(srvyr::as_survey(test_data), group_var = "char_variable1"),
    expected_loa2
  )

  extenstion_loa3 <- data.frame(
    analysis_type = c("mean", "median", "prop_select_multiple"),
    analysis_var = c("number_variable", "number_variable", "char_variable_sm"),
    group_var = rep("char_variable1, char_variable2", 3),
    level = rep(.95, 3)
  )

  expected_loa3 <- rbind(expected_loa2, extenstion_loa3)
  expect_equal(
    create_loa(srvyr::as_survey(test_data),
      group_var = c("char_variable1", "char_variable1, char_variable2")
    ),
    expected_loa3
  )
})

# create_loa filters variables that starts with X_ and _
test_that("create_loa filters variables with X_ and _ and uuid", {
  test_data <- tibble::tibble("X_uuid" = c(1:3), "_uuid" = c(1:3), "hello" = c(1:3), "uuid" = c(1:3))

  expected_output <- data.frame(
    analysis_type = c("mean", "median"),
    analysis_var = rep("hello", 2),
    group_var = rep(NA_character_, 2),
    level = rep(.95, 2)
  )

  expect_equal(create_loa(srvyr::as_survey(test_data)), expected_output)
})


test_that("check_loa separates the grouping variables correclty", {
  loa_test <- data.frame(
    analysis_type = c("mean", "median"),
    analysis_var = rep("hh_number_girls", 2),
    group_var = rep("admin1, hoh_gender", 2),
    level = rep("0.95", 2)
  )

  expect_equal(check_loa(loa_test, srvyr::as_survey(analysistools_MSNA_template_data)), loa_test)
})

test_that("create_group_var split correctly the group_var for all analysis type", {
  no_space_loa <- analysistools_MSNA_template_loa_with_ratio %>%
    dplyr::mutate(group_var = "admin1,admin2,admin3") %>%
    dplyr::distinct() %>%
    dplyr::filter(analysis_var != "admin1" | is.na(analysis_var))
  no_space_results <- create_analysis(srvyr::as_survey(analysistools_MSNA_template_data),
                              loa = no_space_loa,
                              sm_separator = "/") %>% suppressWarnings()

  spaced_loa <- analysistools_MSNA_template_loa_with_ratio %>%
    dplyr::mutate(group_var = "admin1, admin2, admin3") %>%
    dplyr::distinct() %>%
    dplyr::filter(analysis_var != "admin1" | is.na(analysis_var))

  spaced_results <- create_analysis(srvyr::as_survey(analysistools_MSNA_template_data),
                              loa = spaced_loa,
                              sm_separator = "/") %>% suppressWarnings()
  expect_equal(no_space_results$results_table, spaced_results$results_table)
  }
)

test_that("check_loa does not break when no grouping variables", {
  expected_results <- data.frame(analysis_type = c("prop_select_one", "mean", "median", "mean", "median", "prop_select_one", "prop_select_multiple"),
                                 analysis_var = c("admin1", "income_v1_salaried_work", "income_v1_salaried_work", "expenditure_debt", "expenditure_debt", "wash_drinkingwatersource" , "edu_learning_conditions_reasons_v1"),
                                 group_var = NA_character_,
                                 level = .95)

  actual_results <- analysistools_MSNA_template_loa %>%
    dplyr::filter(is.na(group_var)) %>%
    check_loa(design = srvyr::as_survey(analysistools_MSNA_template_data))

  expect_equal(actual_results, expected_results, ignore_attr = T)
})
