testthat::test_that("Error checks", {
  test_clean_data <- data.frame(
    uuid = c(1, 2, 3, 4, 5, 6, 7, 8),
    strata = c(
      "strata1", "strata2", "strata1",
      "strata2", "strata1", "strata2",
      "strata1", "strata1"
    )
  )
  test_sample <- data.frame(
    strata = c("strata1", "strata2"),
    population = c("30000", "50000")
  )
  testthat::expect_error(add_weights(
    dataset = test_clean_data,
    sample_data = test_sample,
    strata_column_dataset = "STRATA",
    strata_column_sample = "strata",
    population_column = "population"
  ), "Cannot find the defined strata column in the provided dataset.")
  testthat::expect_error(add_weights(
    dataset = test_clean_data,
    sample_data = test_sample,
    strata_column_dataset = "strata",
    strata_column_sample = "STRATA",
    population_column = "population"
  ), "Cannot find the defined strata column in the provided sample frame.")
  testthat::expect_error(add_weights(
    dataset = test_clean_data,
    sample_data = test_sample,
    strata_column_dataset = "strata",
    strata_column_sample = "strata",
    population_column = "Total"
  ), "Cannot find the defined population_column column in the provided sample frame.")

  test_data <- test_clean_data %>%
    dplyr::mutate(weights = 1)
  testthat::expect_error(add_weights(
    dataset = test_data,
    sample_data = test_sample,
    strata_column_dataset = "strata",
    strata_column_sample = "strata",
    population_column = "population"
  ), "Weight column already exists in the dataset. Please input another weights column")
  testthat::expect_no_error(add_weights(
    dataset = test_clean_data,
    sample_data = test_sample,
    strata_column_dataset = "strata",
    strata_column_sample = "strata",
    population_column = "population"
  ))
  test_data <- test_clean_data
  test_data$strata[2] <- "not_applicable"
  testthat::expect_error(add_weights(
    dataset = test_data,
    sample_data = test_sample,
    strata_column_dataset = "strata",
    strata_column_sample = "strata",
    population_column = "population"
  ), "Not all strata from dataset are in sample frame")

  test_data <- test_clean_data %>%
    dplyr::filter(strata != "strata1")
  testthat::expect_error(add_weights(
    dataset = test_data,
    sample_data = test_sample,
    strata_column_dataset = "strata",
    strata_column_sample = "strata",
    population_column = "population"
  ), "Not all strata from sample frame are in dataset")
})

testthat::test_that("add_weights works", {
  test_clean_data <- data.frame(
    uuid = c(1, 2, 3, 4, 5, 6, 7, 8),
    strata = c(
      "strata1", "strata2", "strata1",
      "strata2", "strata1", "strata2",
      "strata1", "strata1"
    )
  )
  test_sample <- data.frame(
    strata = c("strata1", "strata2"),
    population = c("30000", "50000")
  )
  actual_output <- test_clean_data %>%
    add_weights(test_sample,
      strata_column_dataset = "strata",
      strata_column_sample = "strata",
      population_column = "population"
    )

  testthat::expect_equal(sum(actual_output$weight), nrow(test_clean_data))
  expected_output <- data.frame(
    uuid = c(1, 2, 3, 4, 5, 6, 7, 8),
    strata = c(
      "strata1", "strata2", "strata1",
      "strata2", "strata1", "strata2",
      "strata1", "strata1"
    ),
    weights = c(0.60, 1.67, 0.60, 1.67, 0.60, 1.67, 0.60, 0.60)
  )

  rounded_output <- actual_output %>%
    dplyr::mutate(weights = round(weights, 2))

  testthat::expect_equal(rounded_output, expected_output)
})
