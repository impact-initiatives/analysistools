test_that("char_to_vector returns the correct outputs", {
  expected_results <- list(
    result1 = c("groupa", "groupb", "groupc"),
    result2 = c("group_a", "group_b", "group_c"),
    result3 = c("groupa", "groupb", "groupc"),
    # result4 = c("groupa"),
    # result5 = c("group_a"),
    result4 = NA_character_
  )

  expect_equal(char_to_vector("groupa, groupb, groupc"), expected_results$result1)
  expect_equal(char_to_vector("group_a,group_b,group_c"), expected_results$result2)
  expect_equal(char_to_vector("groupa , groupb , groupc "), expected_results$result3)
  expect_equal(char_to_vector(""), expected_results$result4)

  expect_error(char_to_vector("groupa, "), "The group_var seems to have empty value, please check the inputs values")
  expect_error(char_to_vector("   ,group_a "), "The group_var seems to have empty value, please check the inputs values")
  expect_error(char_to_vector(c("group_a ", "group_b")), "The group_var to be turned into a vector is already a vector.")
})

test_that("create_group_var separates correctly", {
  expect_equal(create_group_var("groupa, groupb"), "groupa %/% groupb")
  expect_equal(create_group_var("groupa,groupb"), "groupa %/% groupb")
  expect_equal(create_group_var(NA), NA_character_)
})

test_that("correct_nan_numeric behaves correctly", {
  test_table <- data.frame(stat = c(NaN, 0, 1),
                           stat_low = c(NaN, 0, 1),
                           stat_upp = c(NaN, 0, 1),
                           n = c(0,0,1),
                           n_total = c(0,0,1),
                           n_w = c(0,0,1),
                           n_w_total = c(0,0,1))
  expected_output <- data.frame(stat = c(NaN, NaN, 1),
                                stat_low = c(NaN, NaN, 1),
                                stat_upp = c(NaN, NaN, 1),
                                n_total = c(0, 0,1),
                                n_w_total = c(NaN, NaN,1),
                                n_total = c(NaN, NaN,1),
                                n_w = c(NaN, NaN,1))

  expect_equal(correct_nan_total_is_0(test_table), expected_output, ignore_attr = T)

  expect_error(correct_nan_total_is_0(test_table, stat_columns = c("STATS")), "Cannot identify one column.")
  expect_error(correct_nan_total_is_0(test_table, total_column = c("TOTAL")), "Cannot identify one column.")
})


test_that("correct_nan_analysis_var_variable_is_na behaves correctly", {
  test_table <- data.frame(stat = c(NaN, 0, 1),
                           stat_low = c(NaN, 0, 1),
                           stat_upp = c(NaN, 0, 1),
                           n = c(5,5,1),
                           n_total = c(0,0,1),
                           n_w = c(0,0,1),
                           n_w_total = c(0,0,1),
                           analysis_var_value = c(NA,NA,"hello"))
  expected_output <- data.frame(stat = c(NaN, NaN, 1),
                                stat_low = c(NaN, NaN, 1),
                                stat_upp = c(NaN, NaN, 1),
                                n_total = c(5, 5,1),
                                n_w_total = c(NaN, NaN,1),
                                n_total = c(NaN, NaN,1),
                                n_w = c(NaN, NaN,1),
                                analysis_var_value = c(NA,NA,"hello"))

  expect_equal(correct_nan_analysis_var_variable_is_na(test_table), expected_output, ignore_attr = T)

  expect_error(correct_nan_analysis_var_variable_is_na(test_table, stat_columns = c("STATS")), "Cannot identify one column.")
  expect_error(correct_nan_analysis_var_variable_is_na(test_table, analysis_var_value_column = c("ANALYSIS_VAR_VALUE")), "Cannot identify one column.")
})


