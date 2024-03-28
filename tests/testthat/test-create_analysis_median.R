test_that("create_analysis_median returns correct output, no weights", {
  set.seed(135)
  somedata <-
    data.frame(
      groups = sample(c("a", "b"), size = 100, replace = T),
      value = rnorm(100, mean = 50, sd = 10)
    )

  # no group
  actual_output <- create_analysis_median(srvyr::as_survey(somedata),
    group_var = NA,
    analysis_var = "value",
    level = 0.95
  )

  svyquantile_results <- survey::svydesign(id = ~1, data = somedata) %>%
    survey::svyquantile(~value, design = ., quantiles = c(.5)) %>%
    suppressWarnings()

  expected_output <- svyquantile_results[["value"]] %>%
    data.frame() %>%
    `row.names<-`(NULL) %>%
    dplyr::mutate(
      analysis_type = "median",
      analysis_var = "value",
      analysis_var_value = NA_character_,
      group_var = NA_character_,
      group_var_value = NA_character_,
      n = 100,
      n_total = 100,
      n_w = 100,
      n_w_total = 100,
      analysis_key = "median @/@ value %/% NA @/@ NA %/% NA"
    ) %>%
    dplyr::rename(
      stat = quantile,
      stat_low = ci.2.5,
      stat_upp = ci.97.5
    ) %>%
    dplyr::select(dplyr::all_of(names(actual_output)))

  expect_equal(actual_output,
    expected_output,
    ignore_attr = T
  )

  # with 1 group
  one_group_expected_output <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::summarise(
      stat = median(value, na.rm = T),
      n = dplyr::n(),
      n_total = dplyr::n(),
      n_w = dplyr::n(),
      n_w_total = dplyr::n()
    ) %>%
    dplyr::mutate(
      analysis_var = "value",
      analysis_type = "median",
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
    create_analysis_median(srvyr::as_survey(somedata),
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

test_that("create_analysis_median handles NA", {
  # only NAs no grouping
  somedata <- data.frame(
    groups = rep(c("a", "b"), 50),
    value = rep(NA_integer_, 100)
  )

  na_expected_output <- data.frame(
    analysis_type = "median",
    analysis_var = "value",
    analysis_var_value = NA_character_,
    group_var = NA_character_,
    group_var_value = NA_character_,
    stat = NaN,
    stat_low = NaN,
    stat_upp = NaN
  ) %>%
    dplyr::mutate(
      n = 0,
      n_total = NaN,
      n_w = NaN,
      n_w_total = NaN,
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

  na_results <- create_analysis_median(srvyr::as_survey(somedata),
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
      analysis_type = rep("median", 2),
      analysis_var = rep("value", 2),
      analysis_var_value = rep(NA_character_, 2),
      group_var = rep("groups", 2),
      group_var_value = c("a", "b"),
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


  one_group_result <-
    create_analysis_median(srvyr::as_survey(somedata),
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

test_that("create_analysis_median handles when only 1 value", {
  # 99 NAs no group
  somedata <- data.frame(
    groups = rep(c("a", "b"), 50),
    value = c(rep(NA_integer_, 99), 1)
  )

  one_value_expected_output <- data.frame(
    analysis_type = "median",
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
    analysis_key = "median @/@ value %/% NA @/@ NA %/% NA"
  )

  one_value_results <- create_analysis_median(srvyr::as_survey(somedata),
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
      analysis_type = rep("median", 2),
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
    create_analysis_median(srvyr::as_survey(somedata),
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

test_that("create_analysis_median handles lonely PSU", {
  # 1 group has 50 observation, 1 group has 1 observation.
  somedata <- data.frame(
    groups = c(rep("a", 50), "b"),
    value = c(rnorm(50, mean = 50, sd = 10), sample(1:50, 1))
  )

  lonely_psu_result <-
    create_analysis_median(srvyr::as_survey(somedata),
      group_var = "groups",
      analysis_var = "value",
      level = 0.95
    ) %>%
    dplyr::select(-stat_low, -stat_upp) %>%
    suppressWarnings()

  svyquantile_results <- survey::svydesign(id = ~1, data = somedata) %>%
    survey::svyby(~value,
      ~groups,
      design = .,
      FUN = survey::svyquantile,
      quantiles = .5, vartype = "ci"
    ) %>%
    suppressWarnings()

  lonely_psu_expected_output_part1 <- svyquantile_results %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      stat = value,
      stat_low = ci_l,
      stat_upp = ci_u
    )
  lonely_psu_expected_output_part2 <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::mutate(
      analysis_type = "median",
      analysis_var = "value",
      analysis_var_value = NA_character_,
      group_var = "groups",
      group_var_value = c("a", "b"),
      n_total = n,
      n_w = n,
      n_w_total = n,
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
  lonely_psu_expected_output <- lonely_psu_expected_output_part1 %>%
    dplyr::left_join(lonely_psu_expected_output_part2, by = "groups") %>%
    dplyr::select(any_of(names(lonely_psu_result)))

  expect_equal(lonely_psu_result,
    lonely_psu_expected_output,
    ignore_attr = T
  )
})

test_that("create_analysis_median returns correct output, with weights", {
  set.seed(654645)
  somedata <- data.frame(
    groups = rep(c("a", "b"), 50),
    value = rnorm(100, mean = 20, sd = 10)
  )
  somedata[["weights"]] <- ifelse(somedata$groups == "a", 1.33, .67)

  results <-
    create_analysis_median(srvyr::as_survey(somedata, weights = weights),
      group_var = NA,
      analysis_var = "value",
      level = 0.95
    )

  svyquantile_results <- survey::svydesign(id = ~1, data = somedata, weights = ~weights) %>%
    survey::svyquantile(~value, design = ., quantiles = c(.5)) %>%
    suppressWarnings()

  expected_output <- svyquantile_results[["value"]] %>%
    data.frame() %>%
    `row.names<-`(NULL) %>%
    dplyr::mutate(
      analysis_type = "median",
      analysis_var = "value",
      analysis_var_value = NA_character_,
      group_var = NA_character_,
      group_var_value = NA_character_,
      n = 100,
      n_total = 100,
      n_w = 100,
      n_w_total = 100,
      analysis_key = "median @/@ value %/% NA @/@ NA %/% NA"
    ) %>%
    dplyr::rename(
      stat = quantile,
      stat_low = ci.2.5,
      stat_upp = ci.97.5
    ) %>%
    dplyr::select(dplyr::all_of(names(results)))

  expect_equal(results,
    expected_output,
    ignore_attr = T
  )
})

test_that("create_analysis_median returns correct output with 3 grouping variables", {
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

  results <-
    create_analysis_median(srvyr::as_survey(somedata),
      group_var = "group_a, group_b, group_c",
      analysis_var = "value",
      level = 0.95
    )

  svyquantile_results <- survey::svydesign(id = ~1, data = somedata) %>%
    survey::svyby(~value,
      ~ group_a + group_b + group_c,
      design = .,
      FUN = survey::svyquantile,
      quantiles = .5, vartype = "ci"
    ) %>%
    suppressWarnings()

  expected_output_part1 <- svyquantile_results %>%
    tibble::as_tibble() %>%
    `row.names<-`(NULL) %>%
    dplyr::rename(
      stat = value,
      stat_low = ci_l,
      stat_upp = ci_u
    )

  expected_output_part2 <- somedata %>%
    dplyr::group_by(group_a, group_b, group_c) %>%
    dplyr::summarise(
      n = dplyr::n()
    ) %>%
    dplyr::mutate(
      group_var = "group_a %/% group_b %/% group_c",
      analysis_var = "value",
      analysis_var_value = NA_character_,
      analysis_type = "median",
      n_total = n,
      n_w = n,
      n_w_total = n,
      analysis_key = "median @/@ value %/% NA @/@"
    ) %>%
    tidyr::unite(group_var_value, group_a, group_b, group_c, sep = " %/% ", remove = F)

  expected_output <- expected_output_part2 %>%
    dplyr::ungroup() %>%
    dplyr::left_join(expected_output_part1 %>% dplyr::ungroup()) %>%
    dplyr::select(dplyr::all_of(names(results)))

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
    dplyr::mutate(analysis_key = paste(analysis_key, to_add))

  expect_equal(results, expected_output)
})

test_that("create_analysis_median returns correct output with 2 grouping variables and weighted", {
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


  results <-
    create_analysis_median(srvyr::as_survey(somedata, weights = weights),
      group_var = "group_a, group_b",
      analysis_var = "value",
      level = 0.95
    )
  svyquantile_results <- survey::svydesign(id = ~1, data = somedata, weights = ~weights) %>%
    survey::svyby(~value,
      ~ group_a + group_b,
      design = .,
      FUN = survey::svyquantile,
      quantiles = .5, vartype = "ci"
    ) %>%
    suppressWarnings()

  expected_output_part1 <- svyquantile_results %>%
    tibble::as_tibble() %>%
    `row.names<-`(NULL) %>%
    dplyr::rename(
      stat = value,
      stat_low = ci_l,
      stat_upp = ci_u
    )

  expected_output_part2 <- somedata %>%
    dplyr::group_by(group_a, group_b) %>%
    dplyr::summarise(
      n = dplyr::n(),
      n_w = sum(weights)
    ) %>%
    dplyr::mutate(
      group_var = "group_a %/% group_b",
      analysis_var = "value",
      analysis_var_value = NA_character_,
      analysis_type = "median",
      n_total = n,
      n_w_total = n_w,
      analysis_key = "median @/@ value %/% NA @/@"
    ) %>%
    tidyr::unite(group_var_value, group_a, group_b, sep = " %/% ", remove = F)

  expected_output <- expected_output_part2 %>%
    dplyr::ungroup() %>%
    dplyr::left_join(expected_output_part1 %>% dplyr::ungroup()) %>%
    dplyr::select(dplyr::all_of(names(results)))

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
    dplyr::mutate(analysis_key = paste(analysis_key, to_add))

  expect_equal(
    results %>% dplyr::arrange(analysis_key),
    expected_output %>% dplyr::arrange(analysis_key)
  )
})

test_that("stat is set to NaN when there is no value", {
  somedata <- data.frame(
    group = c(rep("group_value_a", 49), "group_value_b"),
    value = c(rep(NA,49),0)
  )

  results <- create_analysis_median(srvyr::as_survey(somedata),
                                  group_var = "group",
                                  analysis_var = "value") %>%
    suppressWarnings()

  expected_output <- data.frame(analysis_type = rep("median", 2),
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
                                analysis_key = c("median @/@ value %/% NA @/@ group %/% group_value_a",
                                                 "median @/@ value %/% NA @/@ group %/% group_value_b"))

  expect_equal(results, expected_output, ignore_attr = T)
})
