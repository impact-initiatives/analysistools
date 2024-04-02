test_that("returns correct results", {
  results_test <- data.frame(analysis_key = c(
    "prop_select_one @/@ fcs_cat %/% low @/@ location %/% locationA -/- population %/% displaced",
    "mean @/@ expenditure_food %/% NA @/@ location %/% locationB"
  ))

  expected_output <- results_test %>%
    dplyr::mutate(
      analysis_type = c("prop_select_one", "mean"),
      analysis_var_1 = c("fcs_cat", "expenditure_food"),
      analysis_var_value_1 = c("low", "NA"),
      group_var_1 = c("location", "location"),
      group_var_value_1 = c("locationA", "locationB"),
      group_var_2 = c("population", NA),
      group_var_value_2 = c("displaced", NA),
      nb_analysis_var = c(1, 1),
      nb_group_var = c(2, 1)
    )

  actual_output <- create_analysis_key_table(results_test) %>%
    suppressWarnings()

  expect_equal(
    actual_output,
    expected_output,
    ignore_attr = TRUE
  )
})

test_that("if index not in the correct format, stops", {
  result_test <- analysistools_MSNA_template_with_ratio_results_table$results_table %>%
    head() %>%
    dplyr::mutate(analysis_key = gsub("@/@", "@@", analysis_key))

  expect_error(
    create_analysis_key_table(results_table = result_test),
    "Analysis keys does not seem to follow the correct format"
  )
})

test_that("unite_variables returns correct results", {
  results_test <- data.frame(analysis_key = c(
    "prop_select_one @/@ fcs_cat %/% low @/@ location %/% locationA -/- population %/% displaced",
    "mean @/@ expenditure_food %/% NA @/@ location %/% locationB"
  ))
  key_table_test <- create_analysis_key_table(results_test) %>%
    suppressWarnings()

  expected_output <- results_test %>%
    dplyr::mutate(
      analysis_type = c("prop_select_one", "mean"),
      analysis_var = c("fcs_cat", "expenditure_food"),
      analysis_var_value = c("low", "NA"),
      group_var = c("location %/% population", "location"),
      group_var_value = c("locationA %/% displaced", "locationB"),
      nb_analysis_var = c(1, 1),
      nb_group_var = c(2, 1)
    )

  expect_equal(unite_variables(key_table_test), expected_output, ignore_attr = TRUE)
})

test_that("if the format is not correct, returns an error", {
  analysis_key_test <- c("analysis_type @/@ analysis_var %/% analysis_var_value")
  expect_error(verify_analysis_key(analysis_key_test))
})


test_that("seperate_variable separate 1, 2, 3 group variable", {
  key_table <- data.frame(analysis_type = "mean",
                          analysis_var = "income %/% NA",
                          group_var = c("admin1 %/% adminA",
                                        "admin1 %/% adminA -/- population %/% populationA",
                                        "admin1 %/% adminA -/- admin2 %/% adminAA -/- population %/% populationA"),
                          nb_analysis_var = 1,
                          nb_group_var = c(1,2,3))

  expected_results <- data.frame(analysis_type = "mean",
                                 analysis_var = "income %/% NA",
                                 group_var_1 = "admin1",
                                 group_var_value_1 = c("adminA"),
                                 group_var_2 = c(NA, "population", "admin2"),
                                 group_var_value_2 = c(NA, "populationA", "adminAA"),
                                 group_var_3 = c(NA, NA, "population"),
                                 group_var_value_3 = c(NA, NA, "populationA"),
                                 nb_analysis_var = 1,
                                 nb_group_var = c(1,2,3))
  expect_equal(separate_variable(key_table, "group_var", "nb_group_var"),
               expected_results, ignore_attr = TRUE)
})
