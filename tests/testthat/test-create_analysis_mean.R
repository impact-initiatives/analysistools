test_that("create_analysis_mean returns correct output, no weights", {
  somedata <-
    data.frame(
      groups = sample(c("a", "b"), size = 100, replace = T),
      value = rnorm(100, mean = 50, sd = 10)
    )

  # no group
  calculated_ci <-
    (sd(somedata$value) / sqrt(nrow(somedata))) * qt(0.975, df = nrow(somedata) - 1)
  expected_output <- data.frame(
    analysis_type = "mean",
    analysis_var = "value",
    analysis_var_value = NA_character_,
    group_var = NA_character_,
    group_var_value = NA_character_,
    stat = mean(somedata$value),
    stat_low = mean(somedata$value) - calculated_ci,
    stat_upp = mean(somedata$value) + calculated_ci,
    n = 100,
    n_total = 100,
    n_w = 100,
    n_w_total = 100,
    analysis_key = "mean @/@ value %/% NA @/@ NA %/% NA"
  )

  expect_equal(
    create_analysis_mean(srvyr::as_survey(somedata),
      group_var = NA,
      analysis_var = "value",
      level = 0.95
    ),
    expected_output,
    ignore_attr = T
  )

  # with 1 group
  one_group_expected_output <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::summarise(
      stat = mean(value, na.rm = T),
      n = dplyr::n(),
      n_total = dplyr::n(),
      n_w = dplyr::n(),
      n_w_total = dplyr::n()
    ) %>%
    dplyr::mutate(
      analysis_var = "value",
      analysis_type = "mean",
      analysis_var_value = NA_character_,
      group_var = "groups"
    ) %>%
    dplyr::rename(group_var_value = groups) %>%
    dplyr::mutate(
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        analysis_var,
        " %/% ",
        analysis_var_value,
        " @/@ ",
        group_var,
        " %/% ",
        group_var_value
      ),
    ) %>%
    dplyr::select(
      analysis_type,
      analysis_var,
      analysis_var_value,
      group_var,
      group_var_value,
      stat,
      n,
      n_total,
      n_w,
      n_w_total,
      analysis_key
    )

  one_group_result <-
    create_analysis_mean(srvyr::as_survey(somedata),
      group_var = "groups",
      analysis_var = "value",
      level = 0.95
    ) %>%
    dplyr::select(-stat_low, -stat_upp)

  expect_equal(one_group_result,
    one_group_expected_output,
    ignore_attr = T
  )
})

test_that("create_analysis_mean handles NA", {
  # only NAs no grouping
  somedata <- data.frame(
    groups = rep(c("a", "b"), 50),
    value = rep(NA_integer_, 100)
  )

  na_expected_output <- data.frame(
    analysis_type = "mean",
    analysis_var = "value",
    analysis_var_value = NA_character_,
    group_var = NA_character_,
    group_var_value = NA_character_,
    stat = NaN,
    stat_low = NaN,
    stat_upp = NaN,
    n = 0,
    n_total = NaN,
    n_w = NaN,
    n_w_total = NaN
  ) %>%
    dplyr::mutate(

      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        analysis_var,
        " %/% ",
        analysis_var_value,
        " @/@ ",
        group_var,
        " %/% ",
        group_var_value
      )
    )

  na_results <- create_analysis_mean(srvyr::as_survey(somedata),
    group_var = NA_character_,
    analysis_var = "value",
    level = 0.95
  ) %>%
    suppressWarnings() %>%
    suppressWarnings()

  expect_equal(na_results,
    na_expected_output,
    ignore_attr = T
  )

  # only NA with groupings
  na_one_group_expected_output <-
    data.frame(
      analysis_type = rep("mean", 2),
      analysis_var = rep("value", 2),
      analysis_var_value = rep(NA_character_, 2),
      group_var = rep("groups", 2),
      group_var_value = c("a", "b"),
      stat = rep(NaN, 2),
      stat_low = rep(NaN, 2),
      stat_upp = rep(NaN, 2),
      n = rep(0, 2),
      n_total = NaN,
      n_w = NaN,
      n_w_total = NaN
    ) %>%
    dplyr::mutate(
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        analysis_var,
        " %/% ",
        analysis_var_value,
        " @/@ ",
        group_var,
        " %/% ",
        group_var_value
      )
    )


  one_group_result <-
    create_analysis_mean(srvyr::as_survey(somedata),
      group_var = "groups",
      analysis_var = "value",
      level = 0.95
    ) %>%
    suppressWarnings()
  expect_equal(one_group_result,
    na_one_group_expected_output,
    ignore_attr = T
  )
})


test_that("create_analysis_mean handles when only 1 value", {
  # 99 NAs no group
  somedata <- data.frame(
    groups = rep(c("a", "b"), 50),
    value = c(rep(NA_integer_, 99), 1)
  )

  one_value_expected_output <- data.frame(
    analysis_type = "mean",
    analysis_var = "value",
    analysis_var_value = NA_character_,
    group_var = NA_character_,
    group_var_value = NA_character_,
    stat = 1,
    stat_low = NaN,
    stat_upp = NaN,
    n = 1,
    n_total = 1,
    n_w = 1,
    n_w_total = 1,
    analysis_key = "mean @/@ value %/% NA @/@ NA %/% NA"
  )

  one_value_results <- create_analysis_mean(srvyr::as_survey(somedata),
    group_var = NA_character_,
    analysis_var = "value",
    level = 0.95
  ) %>%
    suppressWarnings() %>%
    suppressWarnings()

  expect_equal(one_value_results,
    one_value_expected_output,
    ignore_attr = T
  )

  # one value with groupings
  one_value_one_group_expected_output <-
    data.frame(
      analysis_type = rep("mean", 2),
      analysis_var = rep("value", 2),
      analysis_var_value = rep(NA_character_, 2),
      group_var = rep("groups", 2),
      group_var_value = c("a", "b"),
      stat = c(NaN, 1),
      stat_low = rep(NaN, 2),
      stat_upp = rep(NaN, 2),
      n = c(0, 1),
      n_total = c(NaN, 1),
      n_w = c(NaN, 1),
      n_w_total = c(NaN, 1)
    ) %>%
    dplyr::mutate(
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        analysis_var,
        " %/% ",
        analysis_var_value,
        " @/@ ",
        group_var,
        " %/% ",
        group_var_value
      )
    )

  one_value_one_group_results <-
    create_analysis_mean(srvyr::as_survey(somedata),
      group_var = "groups",
      analysis_var = "value",
      level = 0.95
    ) %>%
    suppressWarnings()
  expect_equal(one_value_one_group_results,
    one_value_one_group_expected_output,
    ignore_attr = T
  )
})

test_that("create_analysis_mean handles lonely PSU", {
  # 1 group has 50 observation, 1 group has 1 observation.
  somedata <- data.frame(
    groups = c(rep("a", 50), "b"),
    value = c(rnorm(50, mean = 50, sd = 10), sample(1:50, 1))
  )

  lonely_psu_expected_output <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::summarise(stat = mean(value, na.rm = T)) %>%
    dplyr::mutate(
      analysis_var = "value",
      analysis_type = "mean",
      analysis_var_value = NA_character_,
      group_var = "groups"
    ) %>%
    dplyr::rename(group_var_value = groups) %>%
    dplyr::select(
      analysis_type,
      analysis_var,
      analysis_var_value,
      group_var,
      group_var_value,
      stat
    ) %>%
    dplyr::mutate(
      n = c(50, 1),
      n_total = c(50, 1),
      n_w = c(50, 1),
      n_w_total = c(50, 1),
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        analysis_var,
        " %/% ",
        analysis_var_value,
        " @/@ ",
        group_var,
        " %/% ",
        group_var_value
      )
    )

  lonely_psu_result <-
    create_analysis_mean(srvyr::as_survey(somedata),
      group_var = "groups",
      analysis_var = "value",
      level = 0.95
    ) %>%
    dplyr::select(-stat_low, -stat_upp)
  expect_equal(lonely_psu_result,
    lonely_psu_expected_output,
    ignore_attr = T
  )
})

test_that("create_analysis_mean returns correct output, with weights", {
  somedata <- data.frame(
    groups = rep(c("a", "b"), 50),
    value = rnorm(100, mean = 20, sd = 10)
  )
  somedata[["weights"]] <- ifelse(somedata$groups == "a", 1.33, .67)

  expected_output <- somedata %>%
    dplyr::mutate(value_w = value * weights) %>%
    dplyr::summarise(stat = mean(value_w)) %>%
    dplyr::mutate(
      group_var = NA_character_,
      analysis_var = "value",
      analysis_var_value = NA_character_,
      analysis_type = "mean",
      group_var_value = NA_character_
    ) %>%
    dplyr::select(
      analysis_type,
      analysis_var,
      analysis_var_value,
      group_var,
      group_var_value,
      stat
    ) %>%
    dplyr::mutate(
      n = 100,
      n_total = 100,
      n_w = 100,
      n_w_total = 100,
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        analysis_var,
        " %/% ",
        analysis_var_value,
        " @/@ ",
        group_var,
        " %/% ",
        group_var_value
      )
    )

  results <-
    create_analysis_mean(srvyr::as_survey(somedata, weights = weights),
      group_var = NA,
      analysis_var = "value",
      level = 0.95
    ) %>%
    dplyr::select(-stat_low, -stat_upp)
  expect_equal(results,
    expected_output,
    ignore_attr = T
  )
})
test_that("create_analysis_mean returns correct output with 3 grouping variables", {
  somedata <- data.frame(
    group_a = sample(c("group_value_a", "group_value_b"), 50, replace = T),
    group_b = sample(c(
      "group_value_2_aa", "group_value_2_bb"
    ), 50, replace = T),
    group_c = sample(c(
      "group_value_3_aaa", "group_value_3_bbb"
    ), 50, replace = T),
    value = rnorm(100, mean = 20, sd = 10)
  )

  expected_output <- somedata %>%
    dplyr::group_by(group_a, group_b, group_c) %>%
    dplyr::summarise(
      stat = mean(value),
      n = dplyr::n()
    ) %>%
    dplyr::mutate(
      group_var = "group_a %/% group_b %/% group_c",
      analysis_var = "value",
      analysis_var_value = NA_character_,
      analysis_type = "mean",
      n_total = n,
      n_w = n,
      n_w_total = n,
      analysis_key = "mean @/@ value %/% NA @/@"
    ) %>%
    tidyr::unite(group_var_value, group_a, group_b, group_c, sep = " %/% ")

  x <-
    expected_output$group_var %>% stringr::str_split(" %/% ")
  y <-
    expected_output$group_var_value %>% stringr::str_split(" %/% ")

  to_add <-
    purrr::map2(x, y, function(x, y) {
      paste(x, y, sep = " %/% ")
    }) %>%
    purrr::map(stringr::str_c, collapse = " -/- ") %>%
    do.call(c, .)


  expected_output <- expected_output %>%
    dplyr::mutate(analysis_key = paste(analysis_key, to_add)) %>%
    dplyr::select(
      analysis_type,
      analysis_var,
      analysis_var_value,
      group_var,
      group_var_value,
      stat,
      n,
      n_total,
      n_w,
      n_w_total,
      analysis_key
    )

  results <-
    create_analysis_mean(srvyr::as_survey(somedata),
      group_var = "group_a, group_b, group_c",
      analysis_var = "value",
      level = 0.95
    ) %>%
    dplyr::select(-stat_upp, -stat_low)
  expect_equal(results, expected_output)
})

test_that("create_analysis_mean returns correct output with 2 grouping variables and weighted", {
  set.seed(1222)
  somedata <- data.frame(
    group_a = sample(c("group_value_a", "group_value_b"), 50, replace = T),
    group_b = sample(c(
      "group_value_2_aa", "group_value_2_bb"
    ), 50, replace = T),
    group_c = sample(c(
      "group_value_3_aaa", "group_value_3_bbb"
    ), 50, replace = T),
    value = rnorm(100, mean = 20, sd = 10)
  )

  sample_frame <- expand.grid(
    group_b = c("group_value_2_aa", "group_value_2_bb"),
    group_c = c("group_value_3_aaa", "group_value_3_bbb")
  ) %>%
    dplyr::mutate(prop = c(20, 30, 15, 35))

  sample_frame <- somedata %>%
    dplyr::group_by(group_b, group_c) %>%
    dplyr::tally() %>%
    dplyr::left_join(sample_frame) %>%
    dplyr::mutate(weights = (prop / sum(prop)) / (n / sum(n))) %>%
    dplyr::ungroup() %>%
    dplyr::select(group_b, group_c, weights)

  somedata <- somedata %>%
    dplyr::left_join(sample_frame)

  expected_output <- somedata %>%
    dplyr::group_by(group_a, group_b) %>%
    dplyr::summarise(
      stat = weighted.mean(value, weights),
      n = dplyr::n(),
      n_w = sum(weights)
    ) %>%
    dplyr::mutate(
      group_var = "group_a %/% group_b",
      analysis_var = "value",
      analysis_var_value = NA_character_,
      analysis_type = "mean",
      n_total = n,
      n_w_total = n_w,
      analysis_key = "mean @/@ value %/% NA @/@"
    ) %>%
    tidyr::unite(group_var_value, group_a, group_b, sep = " %/% ")

  x <-
    expected_output$group_var %>% stringr::str_split(" %/% ")
  y <-
    expected_output$group_var_value %>% stringr::str_split(" %/% ")

  to_add <-
    purrr::map2(x, y, function(x, y) {
      paste(x, y, sep = " %/% ")
    }) %>%
    purrr::map(stringr::str_c, collapse = " -/- ") %>%
    do.call(c, .)

  expected_output <- expected_output %>%
    dplyr::mutate(analysis_key = paste(analysis_key, to_add)) %>%
    dplyr::select(
      analysis_type,
      analysis_var,
      analysis_var_value,
      group_var,
      group_var_value,
      stat,
      n,
      n_total,
      n_w,
      n_w_total,
      analysis_key
    )

  results <-
    create_analysis_mean(srvyr::as_survey(somedata, weights = weights),
      group_var = "group_a, group_b",
      analysis_var = "value",
      level = 0.95
    ) %>%
    dplyr::select(-stat_upp, -stat_low)
  expect_equal(results, expected_output)
})


test_that("stat is set to NaN when there is no value", {
  somedata <- data.frame(
    group = c(rep("group_value_a", 49), "group_value_b"),
    value = c(rep(NA,49),0)
  )

  results <- create_analysis_mean(srvyr::as_survey(somedata),
                                  group_var = "group",
                                  analysis_var = "value") %>%
    suppressWarnings()

  expected_output <- data.frame(analysis_type = rep("mean", 2),
                                analysis_var = rep("value", 2),
                                analysis_var_value = rep(NA_character_,2),
                                group_var = rep("group", 2),
                                group_var_value = c("group_value_a", "group_value_b"),
                                stat = c(NaN,0),
                                stat_low = rep(NaN,2),
                                stat_upp = rep(NaN,2),
                                n = c(0,1),
                                n_total = c(NaN,1),
                                n_w = c(NaN,1),
                                n_w_total = c(NaN,1),
                                analysis_key = c("mean @/@ value %/% NA @/@ group %/% group_value_a",
                                                 "mean @/@ value %/% NA @/@ group %/% group_value_b"))

  expect_equal(results, expected_output, ignore_attr = T)
})
