test_that("create_analysis_ratio returns correct output, no weights", {
  somedata <-
    data.frame(
      groups = sample(c("a", "b"), size = 100, replace = T),
      children_518 = sample(0:5, 100, replace = TRUE),
      children_enrolled = sample(0:5, 100, replace = TRUE)
    ) %>%
    dplyr::mutate(children_enrolled = ifelse(children_enrolled > children_518,
      children_518,
      children_enrolled
    ))


  # no group
  expected_output <- data.frame(
    analysis_type = "ratio",
    analysis_var = "children_enrolled %/% children_518",
    analysis_var_value = "NA %/% NA",
    group_var = NA_character_,
    group_var_value = NA_character_,
    stat = sum(somedata$children_enrolled) / sum(somedata$children_518),
    n = sum(somedata$children_518 != 0),
    n_total = sum(somedata$children_518 != 0),
    n_w = sum(somedata$children_518 != 0),
    n_w_total = sum(somedata$children_518 != 0),
    analysis_key = "ratio @/@ children_enrolled %/% NA -/- children_518 %/% NA @/@ NA %/% NA"
  )
  actual_output <- create_analysis_ratio(srvyr::as_survey(somedata),
    group_var = NA,
    analysis_var_numerator = "children_enrolled",
    analysis_var_denominator = "children_518",
    level = 0.95
  ) %>%
    dplyr::select(-stat_low, -stat_upp)

  expect_equal(actual_output,
    expected_output,
    ignore_attr = T
  )

  # with 1 group
  one_group_expected_output <- somedata %>%
    dplyr::filter(children_518 != 0) %>%
    dplyr::group_by(groups) %>%
    dplyr::summarise(
      stat = sum(children_enrolled) / sum(children_518),
      n = dplyr::n(),
      n_total = dplyr::n(),
      n_w = dplyr::n(),
      n_w_total = dplyr::n()
    ) %>%
    dplyr::mutate(
      analysis_type = "ratio",
      analysis_var = "children_enrolled %/% children_518",
      analysis_var_value = "NA %/% NA",
      group_var = "groups"
    ) %>%
    dplyr::rename(group_var_value = groups) %>%
    dplyr::mutate(
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        "children_enrolled %/% NA",
        " -/- ",
        "children_518 %/% NA",
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

  one_group_result <- create_analysis_ratio(srvyr::as_survey(somedata),
    group_var = "groups",
    analysis_var_numerator = "children_enrolled",
    analysis_var_denominator = "children_518",
    level = 0.95
  ) %>%
    dplyr::select(-stat_low, -stat_upp)

  expect_equal(one_group_result,
    one_group_expected_output,
    ignore_attr = T
  )
})

test_that("create_analysis_ratio handles NA", {
  # only NAs no grouping
  somedata <-
    data.frame(
      groups = sample(c("a", "b"), size = 100, replace = T),
      children_518 = rep(NA_integer_, 100),
      children_enrolled = rep(NA_integer_, 100)
    )

  na_expected_output <- data.frame(
    analysis_type = "ratio",
    analysis_var = "children_enrolled %/% children_518",
    analysis_var_value = "NA %/% NA",
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
        "children_enrolled %/% NA",
        " -/- ",
        "children_518 %/% NA",
        " @/@ ",
        group_var,
        " %/% ",
        group_var_value
      )
    )

  na_results <- create_analysis_ratio(srvyr::as_survey(somedata),
    group_var = NA_character_,
    analysis_var_numerator = "children_enrolled",
    analysis_var_denominator = "children_518"
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
      analysis_type = rep("ratio", 2),
      analysis_var = "children_enrolled %/% children_518",
      analysis_var_value = rep("NA %/% NA", 2),
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
        "children_enrolled %/% NA",
        " -/- ",
        "children_518 %/% NA",
        " @/@ ",
        group_var,
        " %/% ",
        group_var_value
      )
    )

  one_group_result <- create_analysis_ratio(srvyr::as_survey(somedata),
    group_var = "groups",
    analysis_var_numerator = "children_enrolled",
    analysis_var_denominator = "children_518"
  ) %>%
    suppressWarnings()


  expect_equal(one_group_result,
    na_one_group_expected_output,
    ignore_attr = T
  )
})


test_that("create_analysis_ratio handles when only 1 value", {
  # 99 NAs no group
  set.seed(12344)
  somedata <-
    data.frame(
      groups = rep(c("a", "b"), 50),
      children_518 = c(rep(NA_integer_, 99), 1),
      children_enrolled = c(rep(NA_integer_, 99), 1)
    )

  one_value_expected_output <- data.frame(
    analysis_type = "ratio",
    analysis_var = "children_enrolled %/% children_518",
    analysis_var_value = "NA %/% NA",
    group_var = NA_character_,
    group_var_value = NA_character_,
    stat = 1,
    stat_low = NaN,
    stat_upp = NaN,
    n = 1,
    n_total = 1,
    n_w = 1,
    n_w_total = 1,
    analysis_key = "ratio @/@ children_enrolled %/% NA -/- children_518 %/% NA @/@ NA %/% NA"
  )

  one_value_results <- create_analysis_ratio(srvyr::as_survey(somedata),
    analysis_var_numerator = "children_enrolled",
    analysis_var_denominator = "children_518",
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
      analysis_type = rep("ratio", 2),
      analysis_var = "children_enrolled %/% children_518",
      analysis_var_value = rep("NA %/% NA", 2),
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
        "children_enrolled %/% NA",
        " -/- ",
        "children_518 %/% NA",
        " @/@ ",
        group_var,
        " %/% ",
        group_var_value
      )
    )

  one_value_one_group_results <-
    create_analysis_ratio(srvyr::as_survey(somedata),
      group_var = "groups",
      analysis_var_numerator = "children_enrolled",
      analysis_var_denominator = "children_518"
    ) %>%
    suppressWarnings() %>%
    suppressWarnings()

  expect_equal(one_value_one_group_results,
    one_value_one_group_expected_output,
    ignore_attr = T
  )
})

test_that("create_analysis_ratio handles lonely PSU", {
  # 1 group has 50 observation, 1 group has 1 observation.
  somedata <- data.frame(
    groups = c(rep("a", 99), "b"),
    children_518 = sample(0:5, 100, replace = TRUE),
    children_enrolled = sample(0:5, 100, replace = TRUE)
  ) %>%
    dplyr::mutate(children_enrolled = ifelse(children_enrolled > children_518,
      children_518,
      children_enrolled
    ))

  lonely_psu_expected_output <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::summarise(
      stat = sum(children_enrolled) / sum(children_518),
      n = sum(children_518 != 0)
    ) %>%
    dplyr::mutate(
      analysis_type = "ratio",
      analysis_var = "children_enrolled %/% children_518",
      analysis_var_value = "NA %/% NA",
      group_var = "groups",
    ) %>%
    dplyr::rename(group_var_value = groups) %>%
    dplyr::select(
      analysis_type,
      analysis_var,
      analysis_var_value,
      group_var,
      group_var_value,
      stat,
      n
    ) %>%
    dplyr::mutate(
      n_total = n,
      n_w = n,
      n_w_total = n,
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        "children_enrolled %/% NA",
        " -/- ",
        "children_518 %/% NA",
        " @/@ ",
        group_var,
        " %/% ",
        group_var_value
      )
    )

  lonely_psu_result <-
    create_analysis_ratio(srvyr::as_survey(somedata),
      group_var = "groups",
      analysis_var_numerator = "children_enrolled",
      analysis_var_denominator = "children_518",
      level = 0.95
    ) %>%
    dplyr::select(-stat_low, -stat_upp)
  expect_equal(lonely_psu_result,
    lonely_psu_expected_output,
    ignore_attr = T
  )
})

test_that("create_analysis_ratio returns correct output, with weights", {
  set.seed(8988)
  somedata <- data.frame(
    groups = rep(c("a", "b"), 50),
    children_518 = sample(0:5, 100, replace = TRUE),
    children_enrolled = sample(0:5, 100, replace = TRUE)
  ) %>%
    dplyr::mutate(children_enrolled = ifelse(children_enrolled > children_518,
      children_518,
      children_enrolled
    ))
  somedata[["weights"]] <- ifelse(somedata$groups == "a", 1.33, .67)

  results <-
    create_analysis_ratio(srvyr::as_survey(somedata, weights = weights),
      group_var = NA,
      analysis_var_numerator = "children_enrolled",
      analysis_var_denominator = "children_518",
      level = 0.95
    ) %>%
    dplyr::select(-stat_low, -stat_upp)

  expected_output <- somedata %>%
    dplyr::filter(!is.na(children_enrolled), !is.na(children_518), children_518 != 0) %>%
    dplyr::summarise(
      stat = sum(children_enrolled * weights) / sum(children_518 * weights),
      n = dplyr::n(),
      n_w = sum(weights)
    ) %>%
    dplyr::mutate(
      group_var = NA_character_,
      analysis_var = "children_enrolled %/% children_518",
      analysis_var_value = "NA %/% NA",
      analysis_type = "ratio",
      group_var_value = NA_character_
    ) %>%
    dplyr::select(
      analysis_type,
      analysis_var,
      analysis_var_value,
      group_var,
      group_var_value,
      stat,
      n,
      n_w
    ) %>%
    dplyr::mutate(
      n_total = n,
      n_w_total = n_w,
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        "children_enrolled %/% NA",
        " -/- ",
        "children_518 %/% NA",
        " @/@ ",
        group_var,
        " %/% ",
        group_var_value
      )
    ) %>%
    dplyr::select(all_of(names(results)))

  expect_equal(results,
    expected_output,
    ignore_attr = T
  )
})

test_that("create_analysis_ratio returns correct output with 3 grouping variables", {
  set.seed(35454)
  somedata <- data.frame(
    group_a = sample(c("group_value_a", "group_value_b"), 50, replace = T),
    group_b = sample(c(
      "group_value_2_aa", "group_value_2_bb"
    ), 50, replace = T),
    group_c = sample(c(
      "group_value_3_aaa", "group_value_3_bbb"
    ), 50, replace = T),
    children_518 = sample(0:5, 100, replace = TRUE),
    children_enrolled = sample(0:5, 100, replace = TRUE)
  ) %>%
    dplyr::mutate(children_enrolled = ifelse(children_enrolled > children_518,
      children_518,
      children_enrolled
    ))

  expected_output <- somedata %>%
    dplyr::group_by(group_a, group_b, group_c) %>%
    dplyr::summarise(
      stat = sum(children_enrolled) / sum(children_518),
      n = sum(children_518 != 0)
    ) %>%
    dplyr::mutate(
      group_var = "group_a %/% group_b %/% group_c",
      analysis_var = "children_enrolled %/% children_518",
      analysis_var_value = "NA %/% NA",
      analysis_type = "ratio",
      n_total = n,
      n_w = n,
      n_w_total = n,
      analysis_key = "ratio @/@ children_enrolled %/% NA -/- children_518 %/% NA @/@"
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
    create_analysis_ratio(srvyr::as_survey(somedata),
      group_var = "group_a, group_b, group_c",
      analysis_var_numerator = "children_enrolled",
      analysis_var_denominator = "children_518",
      level = 0.95
    ) %>%
    dplyr::select(-stat_upp, -stat_low)
  expect_equal(results, expected_output)
})

test_that("create_analysis_ratio returns correct output with 2 grouping variables and weighted", {
  set.seed(1222)
  somedata <- data.frame(
    group_a = sample(c("group_value_a", "group_value_b"), 50, replace = T),
    group_b = sample(c(
      "group_value_2_aa", "group_value_2_bb"
    ), 50, replace = T),
    group_c = sample(c(
      "group_value_3_aaa", "group_value_3_bbb"
    ), 50, replace = T),
    children_518 = sample(0:5, 100, replace = TRUE),
    children_enrolled = sample(0:5, 100, replace = TRUE)
  ) %>%
    dplyr::mutate(children_enrolled = ifelse(children_enrolled > children_518,
      children_518,
      children_enrolled
    ))

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
      stat = sum(children_enrolled * weights) / sum(children_518 * weights),
      n = sum(children_518 != 0),
      n_w = sum(weights[children_518 != 0])
    ) %>%
    dplyr::mutate(
      group_var = "group_a %/% group_b",
      analysis_var = "children_enrolled %/% children_518",
      analysis_var_value = "NA %/% NA",
      analysis_type = "ratio",
      n_total = n,
      n_w_total = n_w,
      analysis_key = "ratio @/@ children_enrolled %/% NA -/- children_518 %/% NA @/@"
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
    create_analysis_ratio(srvyr::as_survey(somedata, weights = weights),
      group_var = "group_a, group_b",
      analysis_var_numerator = "children_enrolled",
      analysis_var_denominator = "children_518",
      level = 0.95
    ) %>%
    dplyr::select(-stat_upp, -stat_low)
  expect_equal(results, expected_output)
})


test_that("if variables names are not found, it will stop", {
  somedata <-
    data.frame(
      groups = sample(c("a", "b"), size = 100, replace = T),
      children_518 = sample(0:5, 100, replace = TRUE),
      children_enrolled = sample(0:5, 100, replace = TRUE)
    ) %>%
    dplyr::mutate(children_enrolled = ifelse(children_enrolled > children_518,
      children_518,
      children_enrolled
    ))

  expect_error(
    create_analysis_ratio(srvyr::as_survey(somedata),
      analysis_var_numerator = "children",
      analysis_var_denominator = "children_518"
    ),
    "children is in the names of the dataset."
  )

  expect_error(
    create_analysis_ratio(srvyr::as_survey(somedata),
      analysis_var_numerator = "children_enrolled",
      analysis_var_denominator = "children"
    ),
    "children is in the names of the dataset."
  )
})

test_that("numerator_NA_to_0, it will return the correct number", {
  school_ex <- data.frame(
    hh = c("hh1", "hh2", "hh3"),
    num_child = c(3, 0, 2),
    num_enrolled = c(3, NA, 0),
    num_attend = c(1, NA, NA)
  )

  expected_output_TRUE <- tibble::tibble(
    analysis_type = "ratio",
    analysis_var = "num_attend %/% num_child",
    analysis_var_value = "NA %/% NA",
    group_var = NA_character_,
    group_var_value = NA_character_,
    stat = 1 / 5,
    n = 2,
    n_total = 2,
    n_w = 2,
    n_w_total = 2,
    analysis_key = "ratio @/@ num_attend %/% NA -/- num_child %/% NA @/@ NA %/% NA"
  )

  results_TRUE <- create_analysis_ratio(srvyr::as_survey(school_ex),
    analysis_var_numerator = "num_attend",
    analysis_var_denominator = "num_child",
    numerator_NA_to_0 = TRUE
  ) %>%
    dplyr::select(-stat_low, -stat_upp)

  expect_equal(
    results_TRUE,
    expected_output_TRUE
  )

  expected_output_FALSE <- tibble::tibble(
    analysis_type = "ratio",
    analysis_var = "num_attend %/% num_child",
    analysis_var_value = "NA %/% NA",
    group_var = NA_character_,
    group_var_value = NA_character_,
    stat = 1 / 3,
    n = 1,
    n_total = 1,
    n_w = 1,
    n_w_total = 1,
    analysis_key = "ratio @/@ num_attend %/% NA -/- num_child %/% NA @/@ NA %/% NA"
  )

  results_FALSE <- create_analysis_ratio(srvyr::as_survey(school_ex),
    analysis_var_numerator = "num_attend",
    analysis_var_denominator = "num_child",
    numerator_NA_to_0 = FALSE
  ) %>%
    suppressWarnings() %>%
    dplyr::select(-stat_low, -stat_upp)


  expect_equal(
    results_FALSE,
    expected_output_FALSE
  )

  set.seed(1223)
  school_ex_long <- data.frame(
    num_child = sample(c(0:4, NA), 100, TRUE, c(0.19, 0.19, .19, .19, .19, .05)),
    num_enrolled = sample(c(0:4, NA), 100, TRUE, c(0.19, 0.19, .19, .19, .19, .05)),
    num_attend = sample(c(0:4, NA), 100, TRUE, c(0.19, 0.19, .19, .19, .19, .05))
  )
  school_ex_long <- school_ex_long %>%
    dplyr::mutate(
      num_enrolled = ifelse(num_child == 0 | is.na(num_child), NA, num_enrolled),
      num_enrolled = ifelse(num_enrolled > num_child, num_child, num_enrolled),
      num_attend = ifelse(num_enrolled == 0 | is.na(num_enrolled), NA, num_attend),
      num_attend = ifelse(num_attend > num_enrolled, num_enrolled, num_attend)
    )

  long_results_TRUE <- create_analysis_ratio(srvyr::as_survey(school_ex_long),
    analysis_var_numerator = "num_attend",
    analysis_var_denominator = "num_child",
    numerator_NA_to_0 = TRUE
  ) %>%
    dplyr::select(-stat_low, -stat_upp)

  long_expected_output_TRUE <- school_ex_long %>%
    dplyr::filter(
      !is.na(num_child),
      num_child != 0
    ) %>%
    dplyr::summarise(
      stat = sum(num_attend, na.rm = T) / sum(num_child, na.rm = T),
      n = dplyr::n()
    ) %>%
    dplyr::mutate(
      analysis_type = "ratio",
      analysis_var = "num_attend %/% num_child",
      analysis_var_value = "NA %/% NA",
      group_var = NA_character_,
      group_var_value = NA_character_,
      n_total = n,
      n_w = n,
      n_w_total = n,
      analysis_key = "ratio @/@ num_attend %/% NA -/- num_child %/% NA @/@ NA %/% NA"
    ) %>%
    dplyr::select(
      all_of(names(long_results_TRUE))
    )

  expect_equal(long_results_TRUE,
    long_expected_output_TRUE,
    ignore_attr = T
  )

  long_results_FALSE <- create_analysis_ratio(srvyr::as_survey(school_ex_long),
    analysis_var_numerator = "num_attend",
    analysis_var_denominator = "num_child",
    numerator_NA_to_0 = FALSE
  ) %>%
    dplyr::select(-stat_low, -stat_upp)

  long_expected_output_FALSE <- school_ex_long %>%
    dplyr::filter(
      !is.na(num_child),
      num_child != 0,
      !is.na(num_attend)
    ) %>%
    dplyr::summarise(
      n = dplyr::n(),
      stat = sum(num_attend, na.rm = T) / sum(num_child, na.rm = T)
    ) %>%
    dplyr::mutate(
      analysis_type = "ratio",
      analysis_var = "num_attend %/% num_child",
      analysis_var_value = "NA %/% NA",
      group_var = NA_character_,
      group_var_value = NA_character_,
      n_total = n,
      n_w = n,
      n_w_total = n,
      analysis_key = "ratio @/@ num_attend %/% NA -/- num_child %/% NA @/@ NA %/% NA"
    ) %>%
    dplyr::select(names(long_results_FALSE))

  expect_equal(long_results_FALSE,
    long_expected_output_FALSE,
    ignore_attr = T
  )
})

# check for filter 0 to FALSE
test_that("denomiator is ok when filter_denominator_0 is set FALSE", {
  set.seed(1223)
  school_ex_long <- data.frame(
    num_child = sample(c(0:4, NA), 100, TRUE, c(0.19, 0.19, .19, .19, .19, .05)),
    num_enrolled = sample(c(0:4, NA), 100, TRUE, c(0.19, 0.19, .19, .19, .19, .05)),
    num_attend = sample(c(0:4, NA), 100, TRUE, c(0.19, 0.19, .19, .19, .19, .05))
  )
  long_results_FALSE <- create_analysis_ratio(srvyr::as_survey(school_ex_long),
    analysis_var_numerator = "num_attend",
    analysis_var_denominator = "num_child",
    filter_denominator_0 = FALSE
  )

  expect_equal(long_results_FALSE$n, sum(!is.na(school_ex_long$num_child)))
})

test_that("stat is set to NaN when there is no value", {
  somedata <- data.frame(
    group = c("group_value_a", "group_value_b", "group_value_c"),
    num_child = c(0, NA, 1),
    num_enrolled = c(0, NA, NA)
  )

  results <- create_analysis_ratio(srvyr::as_survey(somedata),
                                   analysis_var_numerator = "num_enrolled",
                                   analysis_var_denominator = "num_child",
                                  group_var = "group") %>%
    suppressWarnings()

  expected_output <- data.frame(analysis_type = rep("ratio", 3),
                                analysis_var = rep("num_enrolled %/% num_child", 3),
                                analysis_var_value = rep("NA %/% NA",3),
                                group_var = rep("group", 3),
                                group_var_value = c("group_value_a", "group_value_b", "group_value_c"),
                                stat = c(NaN, NaN, 0),
                                stat_low = rep(NaN,3),
                                stat_upp = rep(NaN,3),
                                n = c(0,0,1),
                                n_total = c(NaN,NaN,1),
                                n_w = c(NaN,NaN,1),
                                n_w_total = c(NaN,NaN,1),
                                analysis_key = c("ratio @/@ num_enrolled %/% NA -/- num_child %/% NA @/@ group %/% group_value_a",
                                                 "ratio @/@ num_enrolled %/% NA -/- num_child %/% NA @/@ group %/% group_value_b",
                                                 "ratio @/@ num_enrolled %/% NA -/- num_child %/% NA @/@ group %/% group_value_c"))

  expect_equal(results, expected_output, ignore_attr = T)
})
