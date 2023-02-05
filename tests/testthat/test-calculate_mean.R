# Test 1: Test that the function returns the correct output when no grouping variable is given
test_that("Test 1: calculate_mean returns correct output when no grouping variable is given", {
  somedata <- data.frame(group = rep("a", 100),
                         value = rnorm(100, mean = 50, sd = 10))
  calculated_ci <- (sd(somedata$value) / sqrt(nrow(somedata))) * qt(0.975, df = nrow(somedata) - 1)
  expected_output <- data.frame(stat = mean(somedata$value),
                                stat_low = mean(somedata$value) - calculated_ci,
                                stat_upp = mean(somedata$value) + calculated_ci,
                                group = NA_character_,
                                name = "value",
                                choice = NA,
                                analysis_type = "mean",
                                groupped_by = NA)
  dap <- c(group = NA,
           question_name = "value",
           level = 0.95)
  result <- calculate_mean(srvyr::as_survey(somedata), dap)
  expect_equal(result, expected_output, ignore_attr = T)
})
