testthat::test_that("Error checks", {
  testthat::expect_error(add_weights(.dataset = analysistools::analysistools_clean_data,
                                     sample_data = analysistools::analysistools_sample_frame,
                                     strata_column_dataset = "sdsd",
                                     strata_column_sample = "Neighbourhood",
                                     population_column = "Total.no.of.HH"))
  testthat::expect_error(add_weights(.dataset = analysistools::analysistools_clean_data,
                                     sample_data = analysistools::analysistools_sample_frame,
                                     strata_column_dataset = "neighbourhood",
                                     strata_column_sample = "Neighbourd",
                                     population_column = "Total.no.of.HH"))
  testthat::expect_error(add_weights(.dataset = analysistools::analysistools_clean_data,
                                     sample_data = analysistools::analysistools_sample_frame,
                                     strata_column_dataset = "neighbourhood",
                                     strata_column_sample = "Neighbourhood",
                                     population_column = "Total"))
  test_data <- analysistools::analysistools_clean_data %>%
    dplyr::mutate(weight = 1)
  testthat::expect_error(add_weights(.dataset = test_data,
                                     sample_data = analysistools::analysistools_sample_frame,
                                     strata_column_dataset = "neighbourhood",
                                     strata_column_sample = "Neighbourhood",
                                     population_column = "Total.no.of.HH"))
  testthat::expect_no_error(add_weights(.dataset = analysistools::analysistools_clean_data,
                                     sample_data = analysistools::analysistools_sample_frame,
                                     strata_column_dataset = "neighbourhood",
                                     strata_column_sample = "Neighbourhood",
                                     population_column = "Total.no.of.HH"))
})

testthat::test_that("add_weights works", {
  test_clean_data <- data.frame(uuid = c(1,2,3,4,5,6,7,8),
                          strata = c("strata1","strata2","strata1",
                                     "strata2","strata1","strata2",
                                     "strata1","strata1"))
  test_sample <- data.frame(strata = c("strata1","strata2"),
                            population = c("30000","50000"))
  actual_output <- test_clean_data %>%
    add_weights(test_sample,
                strata_column_dataset = "strata",
                strata_column_sample = "strata",
                population_column = "population")
  expected_output <- data.frame(uuid = c(1,2,3,4,5,6,7,8),
                                strata = c("strata1","strata2","strata1",
                                           "strata2","strata1","strata2",
                                           "strata1","strata1"),
                                weight = c(0.60,1.67,0.60,1.67,0.60,1.67,0.60,0.60))
  testthat::expect_equal(actual_output,expected_output)
})
