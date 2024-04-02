test_that("create_analysis_prop_select_one returns correct output, no weights", {
  somedata <- data.frame(
    groups = sample(c("group_a", "group_b"), size = 100, replace = T),
    value = sample(c("a", "b", "c"), size = 100, replace = T, prob = c(.6, .4, .1))
  )

  # no group

  expected_output <- somedata %>%
    dplyr::group_by(value) %>%
    dplyr::tally() %>%
    dplyr::mutate(
      stat = n / sum(n),
      analysis_type = "prop_select_one",
      analysis_var = "value",
      analysis_var_value = value,
      group_var = NA_character_,
      group_var_value = NA_character_,
      n_total = 100,
      n_w = n,
      n_w_total = 100,
      analysis_key = paste0("prop_select_one @/@ value %/% ", value, " @/@ NA %/% NA")
    ) %>%
    dplyr::select(
      analysis_type, analysis_var, analysis_var_value,
      group_var, group_var_value, stat,
      n, n_total, n_w, n_w_total, analysis_key
    )
  actual_output <- create_analysis_prop_select_one(srvyr::as_survey(somedata), group_var = NA, analysis_var = "value", level = 0.95) %>%
    dplyr::select(-stat_low, -stat_upp)
  expect_equal(actual_output,
    expected_output,
    ignore_attr = T
  )

  # with 1 group
  one_group_expected_output <- somedata %>%
    dplyr::group_by(groups, value) %>%
    dplyr::tally() %>%
    dplyr::mutate(
      stat = n / sum(n),
      analysis_type = "prop_select_one",
      analysis_var = "value",
      analysis_var_value = value,
      group_var = "groups",
      group_var_value = groups,
      n_total = sum(n),
      n_w = n,
      n_w_total = sum(n),
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
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      analysis_type, analysis_var, analysis_var_value,
      group_var, group_var_value, stat,
      n, n_total, n_w, n_w_total, analysis_key
    )

  one_group_result <-
    create_analysis_prop_select_one(srvyr::as_survey(somedata),
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

test_that("create_analysis_prop_select_one handles NA", {
  # only NAs no grouping
  somedata <- data.frame(
    groups = rep(c("a", "b"), 50),
    value = rep(NA_character_, 100)
  )

  na_expected_output <- data.frame(
    analysis_type = "prop_select_one",
    analysis_var = "value",
    analysis_var_value = NA_character_,
    group_var = NA_character_,
    group_var_value = NA_character_,
    stat = NaN,
    stat_low = NaN,
    stat_upp = NaN,
    n = 100,
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

  na_results <- create_analysis_prop_select_one(srvyr::as_survey(somedata),
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
      analysis_type = rep("prop_select_one", 2),
      analysis_var = rep("value", 2),
      analysis_var_value = rep(NA_character_, 2),
      group_var = rep("groups", 2),
      group_var_value = c("a", "b"),
      stat = rep(NaN, 2),
      stat_low = rep(NaN, 2),
      stat_upp = rep(NaN, 2),
      n = rep(50, 2),
      n_total = rep(NaN, 2),
      n_w = rep(NaN, 2),
      n_w_total = rep(NaN, 2)
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
    create_analysis_prop_select_one(srvyr::as_survey(somedata),
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

test_that("create_analysis_prop_select_one returns correct output, with weights", {
  somedata <- data.frame(
    groups = sample(c("group_a", "group_b"), size = 100, replace = T),
    value = sample(c("a", "b", "c"), size = 100, replace = T, prob = c(.5, .3, .2))
  )

  somedata[["weights"]] <- ifelse(somedata$groups == "group_a", 1.33, .67)

  expected_output <- somedata %>%
    dplyr::group_by(value) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_w = sum(weights)
    ) %>%
    dplyr::mutate(
      stat = n_w / sum(n_w),
      n_total = sum(n),
      n_w_total = sum(n_w),
      group_var = NA_character_,
      analysis_var = "value",
      analysis_var_value = value,
      analysis_type = "prop_select_one",
      group_var_value = NA_character_,
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

  results <-
    create_analysis_prop_select_one(srvyr::as_survey(somedata, weights = weights),
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

# test_that("create_analysis_prop_select_one handles when only 1 value", {
test_that("create_analysis_prop_select_one handles when only 1 value", {
  # 99 NAs no group
  somedata <- data.frame(
    groups = rep(c("group_a", "group_b"), 50),
    value = c(rep(NA_integer_, 99), "a")
  )

  one_value_expected_output <- data.frame(
    analysis_type = rep("prop_select_one", 2),
    analysis_var = rep("value", 2),
    analysis_var_value = c("a", NA_character_),
    group_var = rep(NA_character_, 2),
    group_var_value = rep(NA_character_, 2),
    stat = c(1, NaN),
    stat_low = rep(NaN, 2),
    stat_upp = rep(NaN, 2),
    n = c(1, 99),
    n_total = c(1, NaN),
    n_w = c(1, NaN),
    n_w_total = c(1, NaN)
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

  one_value_results <- create_analysis_prop_select_one(srvyr::as_survey(somedata),
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
      analysis_type = rep("prop_select_one", 3),
      analysis_var = rep("value", 3),
      analysis_var_value = c(NA_character_, "a", NA_character_),
      group_var = rep("groups", 3),
      group_var_value = c("group_a", "group_b", "group_b"),
      stat = c(NaN, 1, NaN),
      stat_low = rep(NaN, 3),
      stat_upp = rep(NaN, 3),
      n = c(50, 1, 49),
      n_total = c(NaN, 1, NaN),
      n_w = c(NaN, 1, NaN),
      n_w_total = c(NaN, 1, NaN)
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
    create_analysis_prop_select_one(srvyr::as_survey(somedata),
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
test_that("create_analysis_prop_select_one handles lonely PSU", {
  # 1 group has 50 observation, 1 group has 1 observation.
  somedata <- data.frame(
    groups = c(rep("group_a", 50), "group_b"),
    value = c(sample(c("a", "b", "c"), size = 50, replace = T, prob = c(.5, .3, .2)), sample(c("a", "b", "c"), 1))
  )

  lonely_psu_expected_output <- somedata %>%
    dplyr::group_by(groups, value) %>%
    dplyr::tally() %>%
    dplyr::mutate(
      stat = n / sum(n),
      analysis_type = "prop_select_one",
      analysis_var = "value",
      analysis_var_value = value,
      group_var = "groups",
      group_var_value = groups,
      n_total = sum(n),
      n_w = n,
      n_w_total = sum(n),
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
      stat_low = (stat - qnorm(0.975) * sqrt(stat * (1 - stat) / n_total)),
      stat_upp = (stat + qnorm(0.975) * sqrt(stat * (1 - stat) / n_total))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      analysis_type, analysis_var, analysis_var_value,
      group_var, group_var_value, stat,
      n, n_total, n_w, n_w_total, analysis_key
    )

  lonely_psu_result <-
    create_analysis_prop_select_one(srvyr::as_survey(somedata),
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

test_that("create_analysis_prop_select_one returns correct output with 3 grouping variable", {
  somedata <- data.frame(
    group_a = sample(c("male_hoh", "female_hoh"), 300, replace = T),
    group_b = sample(c(
      "district A", "district B"
    ), 300, replace = T),
    group_c = sample(c(
      "pop A", "pop B"
    ), 300, replace = T),
    value = sample(c("a", "b", "c"), size = 300, replace = T, prob = c(.5, .3, .2))
  )

  expected_output <- somedata %>%
    dplyr::group_by(group_a, group_b, group_c, value) %>%
    dplyr::tally() %>%
    dplyr::mutate(
      stat = n / sum(n),
      analysis_type = "prop_select_one",
      analysis_var = "value",
      analysis_var_value = value,
      group_var = "group_a %/% group_b %/% group_c",
      n_total = sum(n),
      n_w = n,
      n_w_total = sum(n),
      analysis_key = paste("prop_select_one @/@ value %/%", value, "@/@")
    ) %>%
    tidyr::unite(group_var_value, group_a, group_b, group_c, sep = " %/% ") %>%
    dplyr::ungroup()
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
    create_analysis_prop_select_one(srvyr::as_survey(somedata),
      group_var = "group_a, group_b, group_c",
      analysis_var = "value",
      level = 0.95
    ) %>%
    dplyr::select(-stat_upp, -stat_low)
  expect_equal(results, expected_output)
})

test_that("create_analysis_prop_select_one returns correct output with 2 grouping variables and weighted", {
  set.seed(1222)
  somedata <- data.frame(
    group_a = sample(c("male_hoh", "female_hoh"), 300, replace = T),
    group_b = sample(c(
      "district A", "district B"
    ), 300, replace = T),
    group_c = sample(c(
      "pop A", "pop B"
    ), 300, replace = T),
    value = sample(c("a", "b", "c"), size = 300, replace = T, prob = c(.5, .3, .2))
  )

  sample_frame <- expand.grid(
    group_b = c("district A", "district B"),
    group_c = c("pop A", "pop B")
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
    dplyr::group_by(group_a, group_b, value) %>%
    dplyr::summarise(
      n_w = sum(weights),
      n = dplyr::n()
    ) %>%
    dplyr::mutate(
      stat = n_w / sum(n_w),
      analysis_type = "prop_select_one",
      analysis_var = "value",
      analysis_var_value = value,
      group_var = "group_a %/% group_b",
      n_total = sum(n),
      n_w_total = sum(n_w),
      analysis_key = paste("prop_select_one @/@ value %/%", value, "@/@")
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
    create_analysis_prop_select_one(srvyr::as_survey(somedata, weights = weights),
      group_var = "group_a, group_b",
      analysis_var = "value",
      level = 0.95
    ) %>%
    dplyr::select(-stat_upp, -stat_low)
  expect_equal(results, expected_output)
})

test_that("When one option has never been selected, stats is 0 and not NaN", {
  test_data <- data.frame(uuid = letters[1:3],
                          so_question = c("option1", "option2", NA),
                          group = c("group_a", "group_b", "group_c"))

  expected_output <- data.frame(analysis_type = rep("prop_select_one", 3),
                                analysis_var = rep("so_question", 3),
                                analysis_var_value = c("option1", "option2", NA),
                                group_var = c("group", "group", "group"),
                                group_var_value = c("group_a", "group_b", "group_c"),
                                stat = c(1,1,NaN),
                                stat_low = c(1,1,NaN),
                                stat_upp = c(1,1,NaN),
                                n = c(1,1,1),
                                n_total = c(1,1,NaN),
                                n_w = c(1,1,NaN),
                                n_w_total = c(1,1,NaN),
                                analysis_key = c("prop_select_one @/@ so_question %/% option1 @/@ group %/% group_a",
                                                 "prop_select_one @/@ so_question %/% option2 @/@ group %/% group_b",
                                                 "prop_select_one @/@ so_question %/% NA @/@ group %/% group_c"))

  current_output <- create_analysis_prop_select_one(srvyr::as_survey(test_data),
                                                    group_var = "group",
                                                    analysis_var = "so_question") %>%
    suppressWarnings()

  expect_equal(current_output, expected_output, ignore_attr = T)
})

test_that("prop_select_one does not create a row for missing values that is empty, it returns the
          correct number of missing values", {
  set.seed(123)
  test_dataset <- data.frame(uuid = letters,
                             hoh = sample(c("no", "yes", "other", NA_character_),
                                          size = 26,
                                          replace = T,
                                          prob = c(0.44,0.44,0.1,0.1)))
  my_surveyx <- srvyr::as_survey(test_dataset)

  expected_output <- data.frame(analysis_type = "prop_select_one",
                                analysis_var = "hoh",
                                analysis_var_value = c("no", "other", "yes", NA_character_),
                                group_var = NA_character_,
                                group_var_value = NA_character_,
                                stat = c(0.273, 0.182, 0.545, NaN),
                                stat_low = c(0.0710, 0.007, 0.320, NaN),
                                stat_upp = c(0.474, 0.356, 0.771, NaN),
                                n = c(6,4,12, 4),
                                n_total = c(22, 22, 22, NaN),
                                n_w = c(6,4,12, NaN),
                                n_w_total = c(22, 22, 22, NaN),
                                analysis_key = c("prop_select_one @/@ hoh %/% no @/@ NA %/% NA",
                                                 "prop_select_one @/@ hoh %/% other @/@ NA %/% NA",
                                                 "prop_select_one @/@ hoh %/% yes @/@ NA %/% NA",
                                                 "prop_select_one @/@ hoh %/% NA @/@ NA %/% NA"))
  actual_output <- create_analysis_prop_select_one(my_surveyx,
                                                   analysis_var = "hoh")
  actual_output[, c("stat", "stat_low", "stat_upp")] <- actual_output[, c("stat", "stat_low", "stat_upp")] %>%
    round(3)
  expect_equal(actual_output, expected_output,
               ignore_attr = TRUE)
})
