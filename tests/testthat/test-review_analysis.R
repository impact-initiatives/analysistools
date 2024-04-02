test_that("review analysis works", {

  analysis_key_column <-  c("mean @/@ income %/% NA @/@ NA %/% NA",
                            "prop_select_one @/@ water_source %/% tap_water @/@ district %/% district_a",
                            "prop_select_one @/@ water_source %/% tap_water @/@ district %/% district_a -/- population %/% displaced",
                            "prop_select_multiple @/@ source_information %/% relatives @/@ NA %/% NA",
                            "ratio @/@ food_expenses %/% NA -/- total_expenses %/% NA @/@ NA %/% NA",
                            "prop_select_one @/@ water_source %/% tap_water @/@ population %/% displaced",
                            "ratio @/@ food_expenses %/% NA -/- total_expenses %/% NA @/@ district %/% district_a",
                            "prop_select_one @/@ water_source %/% tap_water @/@ population %/% returnees")
  test_analysis_results <- data.frame(
    test = c(
      "test equality",
      "test difference",
      "test Missing in y",
      "test Missing in x",
      "test equality rounding in x",
      "test equality rounding in y",
      "test difference rounding in x",
      "test difference rounding in y"
    ),
    stat_col.x = c(0, 1, 2, NA, 0.00019, 0.0002, 0.00035, 0.0003),
    upp_col.x = c(0, 1, 2, NA, 0.00019, 0.0002, 0.00035, 0.0003),
    stat_col.y = c(0, 2, NA, 3, 0.0002, 0.00019, 0.0003, 0.00035),
    upp_col.y = c(0, 2, NA, 3, 0.0002, 0.00019, 0.0003, 0.00035),
    analysis_key = analysis_key_column
  )

  review_check_columm <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)
  review_comment_column <- c(
    "Same results",
    "Different results",
    "Missing in stat_col.y",
    "Missing in stat_col.x",
    "Same results",
    "Same results",
    "Different results",
    "Different results"
  )
  analysis_type_column  <- c("mean", "prop_select_one", "prop_select_one", "prop_select_multiple",
                            "ratio", "prop_select_one", "ratio", "prop_select_one")

  analysis_var_column  <- c("income", "water_source", "water_source", "source_information",
                           "food_expenses %/% total_expenses", "water_source",
                           "food_expenses %/% total_expenses", "water_source")

  group_var_column <- c("NA", "district", "district %/% population", "NA",
                        "NA", "population", "district", "population")

  expected_results_table_1_stat <- test_analysis_results |>
    cbind(
      review_check_stat_col.x = review_check_columm,
      review_comment_stat_col.x = review_comment_column
    )
  expected_review_table_1_stat <- data.frame(
    analysis_key = analysis_key_column,
    stat = "stat_col.x",
    review_check = review_check_columm,
    review_comment = review_comment_column,
    analysis_type = analysis_type_column,
    analysis_var = analysis_var_column,
    group_var = group_var_column
  )

  expected_results_1_stat <- list(
    results_table = expected_results_table_1_stat,
    review_table = expected_review_table_1_stat
  )

  actual_review_table_1_stat <- review_analysis(test_analysis_results,
                                                stat_columns_to_review = "stat_col.x",
                                                stat_columns_to_compare_with = "stat_col.y")

  expect_equal(actual_review_table_1_stat, expected_results_1_stat)

  review_comment_upp_column <- c(
    "Same results",
    "Different results",
    "Missing in upp_col.y",
    "Missing in upp_col.x",
    "Same results",
    "Same results",
    "Different results",
    "Different results"
  )

  expected_results_table_2_stats <- test_analysis_results |>
    cbind(
      review_check_stat_col.x = review_check_columm,
      review_comment_stat_col.x = review_comment_column,
      review_check_upp_col.x = review_check_columm,
      review_comment_upp_col.x = review_comment_upp_column
    )

  expected_review_table_2_stats <- data.frame(
    analysis_key = rep(analysis_key_column, 2),
    stat = c(rep("stat_col.x", 8), rep("upp_col.x", 8)),
    review_check = rep(review_check_columm, 2),
    review_comment = c(review_comment_column,review_comment_upp_column),
    analysis_type = rep(analysis_type_column, 2),
    analysis_var = rep(analysis_var_column, 2),
    group_var = rep(group_var_column, 2)

  )
  expected_results_2_stats <- list(
    results_table = expected_results_table_2_stats,
    review_table = expected_review_table_2_stats
  )

  actual_review_table_2_stats <- review_analysis(test_analysis_results,
                                                 stat_columns_to_review = c("stat_col.x", "upp_col.x"),
                                                 stat_columns_to_compare_with = c("stat_col.y", "upp_col.y"))

  expect_equal(actual_review_table_2_stats, expected_results_2_stats)

})

