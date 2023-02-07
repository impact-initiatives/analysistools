test_that("calculate_mean returns correct output, no weights", {
  somedata <- data.frame(groups = sample(c("a","b"),size = 100, replace = T),
                         value = rnorm(100, mean = 50, sd = 10))

  #no group
  dap <- c(group_var = NA,
           analysis_var = "value",
           level = 0.95)
  calculated_ci <- (sd(somedata$value) / sqrt(nrow(somedata))) * qt(0.975, df = nrow(somedata) - 1)
  expected_output <- data.frame(stat = mean(somedata$value),
                                stat_low = mean(somedata$value) - calculated_ci,
                                stat_upp = mean(somedata$value) + calculated_ci,
                                group_var = NA_character_,
                                analysis_var = "value",
                                analysis_var_value = NA_character_,
                                analysis_type = "mean",
                                group_var_value = NA_character_)

  expect_equal(calculate_mean(srvyr::as_survey(somedata), dap),
               expected_output,
               ignore_attr = T)

  #with 1 group
  one_group_dap <- c(group_var = "groups",
                     analysis_var = "value",
                     level = 0.95)
  one_group_expected_output <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::summarise(stat = mean(value, na.rm = T)) %>%
    dplyr::mutate(analysis_var = "value",
           analysis_type = "mean",
           analysis_var_value = NA_character_,
           group_var = "groups") %>%
    dplyr::rename(group_var_value = groups) %>%
    dplyr::select(group_var_value, stat, group_var, analysis_var, analysis_var_value, analysis_type)

  one_group_result <- calculate_mean(srvyr::as_survey(somedata), one_group_dap) %>%
    dplyr::select(-stat_low, -stat_upp)

  expect_equal(one_group_result,
               one_group_expected_output,
               ignore_attr = T)


})

test_that("calculate_mean handles NA", {
  #only NAs no grouping
  somedata <- data.frame(groups = rep(c("a","b"), 50),
                         value = rep(NA_integer_, 100))

  na_expected_output <- data.frame(stat = NaN,
                                 stat_low = NaN,
                                 stat_upp = NaN,
                                 group_var = NA_character_,
                                 analysis_var = "value",
                                 analysis_var_value = NA_character_,
                                 analysis_type = "mean",
                                 group_var_value = NA_character_)

  dap <- c(group_var = NA_character_,
           analysis_var = "value",
           level = 0.95)

  expect_warning(na_results <- calculate_mean(srvyr::as_survey(somedata), dap))

  expect_equal(na_results,
               na_expected_output,
               ignore_attr = T)

  #only NA with groupings
  one_group_dap <- c(group_var = "groups",
                     analysis_var = "value",
                     level = 0.95)


  na_one_group_expected_output <- data.frame(group_var_value = c("a", "b"),
                                             stat = rep(NaN, 2),
                                             stat_low = rep(NaN, 2),
                                             stat_upp = rep(NaN, 2),
                                             group_var = rep("groups", 2),
                                             analysis_var = rep("value", 2),
                                             analysis_var_value = rep(NA_character_, 2),
                                             analysis_type = rep("mean", 2))

  one_group_result <- calculate_mean(srvyr::as_survey(somedata), one_group_dap) %>%
    suppressWarnings()
  # expect_warning(one_group_result <- calculate_mean(srvyr::as_survey(somedata), one_group_dap))
  expect_equal(one_group_result,
               na_one_group_expected_output,
               ignore_attr = T)
  })


test_that("calculate_mean handles when only 1 value", {
  #99 NAs no group
  somedata <- data.frame(groups = rep(c("a","b"), 50),
                         value = c(rep(NA_integer_, 99), 1))

  one_value_expected_output <- data.frame(stat = 1,
                                    stat_low = NaN,
                                   stat_upp = NaN,
                                   group_var = NA_character_,
                                   analysis_var = "value",
                                   analysis_var_value = NA_character_,
                                   analysis_type = "mean",
                                   group_var_value = NA_character_)
  #
  dap <- c(group_var = NA_character_,
           analysis_var = "value",
           level = 0.95)

  expect_warning(one_value_results <- calculate_mean(srvyr::as_survey(somedata), dap))

  expect_equal(one_value_results,
               one_value_expected_output,
               ignore_attr = T)

  #one value with groupings
  one_value_one_group_dap <- c(group_var = "groups",
                     analysis_var = "value",
                     level = 0.95)


  one_value_one_group_expected_output <- data.frame(group_var_value = c("a", "b"),
                                                    stat = c(NaN, 1),
                                             stat_low = rep(NaN, 2),
                                             stat_upp = rep(NaN, 2),
                                             group_var = rep("groups", 2),
                                             analysis_var = rep("value", 2),
                                             analyis_var_choice = rep(NA_character_, 2),
                                             analysis_type = rep("mean", 2))

  one_value_one_group_results <- calculate_mean(srvyr::as_survey(somedata), one_value_one_group_dap) %>%
    suppressWarnings()
  expect_equal(one_value_one_group_results,
               one_value_one_group_expected_output,
               ignore_attr = T)

})

test_that("calculate_mean handles lonely PSU", {
  #1 group has 50 observation, 1 group has 1 observation.
  somedata <- data.frame(groups = c(rep("a", 50), "b"),
                         value = c(rnorm(50, mean = 50, sd = 10), sample(1:50,1)))

  lonely_psu_dap <- c(group_var = "groups",
                     analysis_var = "value",
                     level = 0.95)


  lonely_psu_expected_output <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::summarise(stat = mean(value, na.rm = T)) %>%
    dplyr::mutate(analysis_var = "value",
           analysis_type = "mean",
           analysis_var_value = NA_character_,
           group_var = "groups") %>%
    dplyr::rename(group_var_value = groups) %>%
    dplyr::select(group_var_value, stat, group_var, analysis_var, analysis_var_value,
                  analysis_type)

  lonely_psu_result <- calculate_mean(srvyr::as_survey(somedata), lonely_psu_dap) %>%
    dplyr::select(-stat_low, -stat_upp)
  expect_equal(lonely_psu_result,
               lonely_psu_expected_output,
               ignore_attr = T)
})

test_that("calculate_mean returns correct output, with weights", {
  somedata <- data.frame(groups = rep(c("a","b"),50),
                         value = rnorm(100, mean = 20, sd = 10))
  somedata[["weights"]] <- ifelse(somedata$groups == "a", 1.33,.67)

  dap <- c(group_var = NA,
           analysis_var = "value",
           level = 0.95)
  expected_output <- somedata %>%
    dplyr::mutate(value_w = value * weights) %>%
    dplyr::summarise(stat = mean(value_w)) %>%
    dplyr::mutate(group_var = NA_character_,
                  analysis_var = "value",
           analysis_var_value = NA_character_,
           analysis_type = "mean",
           group_var_value = NA_character_)

  results <- calculate_mean(srvyr::as_survey(somedata, weights = weights), dap) %>%
    dplyr::select(-stat_low, -stat_upp)
  expect_equal(results,
               expected_output,
               ignore_attr = T)

})

#write test for outputs with 2 grouping var
test_that("calculate_mean returns correct output with 3 grouping variable", {
  somedata <- data.frame(group_a = sample(c("a","b"),50, replace = T),
                         group_b = sample(c("aa", "bb"), 50, replace = T),
                         group_c = sample(c("aaa","bbb"), 50, replace = T),
                         value = rnorm(100, mean = 20, sd = 10))

  expected_output <- somedata %>%
    dplyr::group_by(group_a, group_b, group_c) %>%
    dplyr::summarise(stat = mean(value)) %>%
    dplyr::mutate(group_var = "group_a, group_b, group_c",
                  analysis_var = "value",
                  analysis_var_value = NA_character_,
                  analysis_type = "mean") %>%
    tidyr::unite(group_var_value, group_a, group_b, group_c, sep = " ~/~ ")

  dap <- c(group_var = "group_a, group_b, group_c",
           analysis_var = "value",
           level = 0.95)
  results <- calculate_mean(srvyr::as_survey(somedata), dap) %>%
    dplyr::select(-stat_upp, -stat_low)
  expect_equal(results, expected_output)
  })

