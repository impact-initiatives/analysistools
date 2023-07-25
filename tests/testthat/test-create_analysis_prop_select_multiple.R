test_that("create_analysis_prop_select_multiple returns correct output, no weights", {
  somedata <- data.frame(
    groups = sample(c("group_a", "group_b"), size = 100, replace = T),
    smvar = rep(NA_character_, 100),
    smvar.option1 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.7, .3)),
    smvar.option2 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.6, .4)),
    smvar.option3 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.1, .9)),
    smvar.option4 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.8, .2)),
    uuid = 1:100 %>% as.character()
  ) %>%
    cleaningtools::recreate_parent_column(uuid = "uuid", sm_sep = ".")

  somedata <- somedata$data_with_fix_concat

  # no group
  expected_output <- somedata %>%
    dplyr::filter(!is.na(smvar)) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::starts_with("smvar."), .fns = list(stat = mean, n = sum))) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = c("analysis_info", "analysis_type"), values_to = "x", names_sep = "_"
    ) %>%
    tidyr::pivot_wider(id_cols = analysis_info, names_from = analysis_type, values_from = x) %>%
    tidyr::separate_wider_delim(analysis_info, delim = ".", names = c("analysis_var", "analysis_var_value")) %>%
    dplyr::mutate(
      analysis_type = "prop_select_multiple",
      group_var = NA_character_,
      group_var_value = NA_character_,
      n_total = sum(!is.na(somedata$smvar)),
      n_w = n,
      n_w_total = sum(!is.na(somedata$smvar)),
      analysis_key = paste0("prop_select_multiple @/@ smvar ~/~ ", analysis_var_value, " @/@ NA ~/~ NA")
    ) %>%
    dplyr::select(
      analysis_type, analysis_var, analysis_var_value,
      group_var, group_var_value, stat,
      n, n_total, n_w, n_w_total, analysis_key
    )
  actual_output <- create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
    group_var = NA,
    analysis_var = "smvar",
    level = 0.95
  ) %>%
    dplyr::select(-stat_low, -stat_upp)
  expect_equal(actual_output,
    expected_output,
    ignore_attr = T
  )

  # with 1 group
  groups_n <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::filter(!is.na(smvar)) %>%
    dplyr::tally(name = "n_total")
  one_group_expected_output <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::filter(!is.na(smvar)) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::starts_with("smvar."), .fns = list(stat = mean, n = sum))) %>%
    tidyr::pivot_longer(
      cols = -groups,
      names_to = c("analysis_info", "analysis_type"), values_to = "x", names_sep = "_"
    ) %>%
    tidyr::pivot_wider(id_cols = c(groups, analysis_info), names_from = analysis_type, values_from = x) %>%
    tidyr::separate_wider_delim(analysis_info, delim = ".", names = c("analysis_var", "analysis_var_value")) %>%
    dplyr::left_join(groups_n) %>%
    dplyr::mutate(
      analysis_type = "prop_select_multiple",
      group_var = "groups",
      group_var_value = groups,
      n_w = n,
      n_w_total = n_total,
      analysis_key = paste0("prop_select_multiple @/@ smvar ~/~ ", analysis_var_value, " @/@ groups ~/~ ", group_var_value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      analysis_type, analysis_var, analysis_var_value,
      group_var, group_var_value, stat,
      n, n_total, n_w, n_w_total, analysis_key
    )

  one_group_result <-
    create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
      group_var = "groups",
      analysis_var = "smvar",
      level = 0.95
    ) %>%
    dplyr::select(-stat_low, -stat_upp)

  expect_equal(one_group_result,
    one_group_expected_output,
    ignore_attr = T
  )
})


test_that("create_analysis_prop_select_multiple handles NA", {
  # only NAs no grouping
  somedata <- data.frame(
    groups = sample(c("group_a", "group_b"), size = 100, replace = T),
    smvar = rep(NA_character_, 100),
    smvar.option1 = rep(NA_character_, 100),
    smvar.option2 = rep(NA_character_, 100),
    smvar.option3 = rep(NA_character_, 100),
    smvar.option4 = rep(NA_character_, 100),
    uuid = 1:100 %>% as.character()
  ) %>%
    cleaningtools::recreate_parent_column(uuid = "uuid", sm_sep = ".")

  somedata <- somedata$data_with_fix_concat

  na_expected_output <- somedata %>%
    dplyr::summarise(dplyr::across(dplyr::starts_with("smvar."), mean)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), values_to = "stat") %>%
    tidyr::separate_wider_delim(name, delim = ".", names = c("analysis_var", "analysis_var_value")) %>%
    dplyr::mutate(
      analysis_type = "prop_select_multiple",
      group_var = NA_character_,
      group_var_value = NA_character_,
      stat_low = NaN,
      stat_upp = NaN
    ) %>%
    dplyr::mutate(
      n = 0,
      n_total = 0,
      n_w = 0,
      n_w_total = 0,
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        analysis_var,
        " ~/~ ",
        analysis_var_value,
        " @/@ ",
        group_var,
        " ~/~ ",
        group_var_value
      )
    ) %>%
    dplyr::select(
      analysis_type, analysis_var, analysis_var_value,
      group_var, group_var_value, stat, stat_low, stat_upp,
      n, n_total, n_w, n_w_total, analysis_key
    ) %>%
    suppressWarnings()

  na_results <- create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
    group_var = NA_character_,
    analysis_var = "smvar",
    level = 0.95
  ) %>%
    suppressWarnings() %>%
    suppressWarnings()

  expect_equal(na_results,
    na_expected_output,
    ignore_attr = T
  )

  # only NA with groupings
  groups_n <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::tally(name = "n_total")

  na_one_group_expected_output <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::summarise(dplyr::across(dplyr::starts_with("smvar."), mean)) %>%
    tidyr::pivot_longer(cols = -groups, values_to = "stat") %>%
    tidyr::separate_wider_delim(name, delim = ".", names = c("analysis_var", "analysis_var_value")) %>%
    dplyr::mutate(
      analysis_type = "prop_select_multiple",
      group_var = "groups",
      group_var_value = groups,
      stat_low = NaN,
      stat_upp = NaN
    ) %>%
    dplyr::mutate(
      n = 0,
      n_total = 0,
      n_w = 0,
      n_w_total = 0,
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        analysis_var,
        " ~/~ ",
        analysis_var_value,
        " @/@ ",
        group_var,
        " ~/~ ",
        group_var_value
      )
    ) %>%
    dplyr::select(
      analysis_type, analysis_var, analysis_var_value,
      group_var, group_var_value, stat, stat_low, stat_upp,
      n, n_total, n_w, n_w_total, analysis_key
    ) %>%
    suppressWarnings()

  one_group_result <-
    create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
      group_var = "groups",
      analysis_var = "smvar",
      level = 0.95
    ) %>%
    suppressWarnings()
  expect_equal(one_group_result,
    na_one_group_expected_output,
    ignore_attr = T
  )
})

# test_that("create_analysis_prop_select_multiple returns correct output, with weights", {
test_that("create_analysis_prop_select_multiple returns correct output, with weights", {
  somedata <- data.frame(
    groups = sample(c("group_a", "group_b"), size = 100, replace = T),
    smvar = rep(NA_character_, 100),
    smvar.option1 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.7, .3)),
    smvar.option2 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.6, .4)),
    smvar.option3 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.1, .9)),
    smvar.option4 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.8, .2)),
    uuid = 1:100 %>% as.character()
  ) %>%
    cleaningtools::recreate_parent_column(uuid = "uuid", sm_sep = ".")

  somedata <- somedata$data_with_fix_concat

  somedata[["weights"]] <- ifelse(somedata$groups == "group_a", 1.33, .67)

  expected_output <- somedata %>%
    dplyr::filter(!is.na(smvar)) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("smvar."),
      function(x) {
        x * weights
      },
      .names = "w_{.col}"
    )) %>%
    dplyr::summarise(
      n_w_total = sum(weights),
      n_total = dplyr::n(),
      dplyr::across(dplyr::starts_with("w_smvar."), sum),
      dplyr::across(dplyr::starts_with("smvar."), sum)
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("w_smvar."),
      function(x) {
        x / n_w_total
      },
      .names = "prop_{.col}"
    )) %>%
    dplyr::ungroup()

  expected_output <- expected_output %>%
    tidyr::pivot_longer(cols = -c(n_w_total, n_total)) %>%
    tidyr::separate_wider_delim(name, delim = ".", names = c("info", "analysis_var_value")) %>%
    dplyr::mutate(info = dplyr::case_when(
      info == "w_smvar" ~ "n_w",
      info == "smvar" ~ "n",
      info == "prop_w_smvar" ~ "stat"
    )) %>%
    tidyr::pivot_wider(
      id_cols = c(n_w_total, n_total, analysis_var_value),
      names_from = info,
      values_from = value
    ) %>%
    dplyr::mutate(
      group_var = NA_character_,
      analysis_var = "smvar",
      analysis_type = "prop_select_multiple",
      group_var_value = NA_character_,
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        analysis_var,
        " ~/~ ",
        analysis_var_value,
        " @/@ ",
        group_var,
        " ~/~ ",
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
    create_analysis_prop_select_multiple(srvyr::as_survey(somedata, weights = weights),
      group_var = NA,
      analysis_var = "smvar",
      level = 0.95
    ) %>%
    dplyr::select(-stat_low, -stat_upp)
  expect_equal(results,
    expected_output,
    ignore_attr = T
  )
})


# test_that("create_analysis_prop_select_multiple handles when only 1 value", {
test_that("create_analysis_prop_select_multiple handles when only 1 value", {
  # 99 NAs no group
  somedata <- data.frame(
    groups = rep(c("group_a", "group_b"), 50),
    smvar = rep(NA_character_, 100),
    smvar.option1 = c(rep(NA_integer_, 99), T),
    smvar.option2 = c(rep(NA_integer_, 99), F),
    smvar.option3 = c(rep(NA_integer_, 99), F),
    smvar.option4 = c(rep(NA_integer_, 99), T),
    uuid = 1:100 %>% as.character()
  ) %>%
    cleaningtools::recreate_parent_column(uuid = "uuid", sm_sep = ".")
  somedata <- somedata$data_with_fix_concat
  one_value_expected_output <- data.frame(
    analysis_type = rep("prop_select_multiple", 4),
    analysis_var = rep("smvar", 4),
    analysis_var_value = paste0("option", 1:4),
    group_var = rep(NA_character_, 4),
    group_var_value = rep(NA_character_, 4),
    stat = c(1, NaN, NaN, 1),
    stat_low = rep(NaN, 4),
    stat_upp = rep(NaN, 4),
    n = c(1, 0, 0, 1),
    n_total = rep(1, 4),
    n_w = c(1, 0, 0, 1),
    n_w_total = rep(1, 4)
  ) %>%
    dplyr::mutate(
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        analysis_var,
        " ~/~ ",
        analysis_var_value,
        " @/@ ",
        group_var,
        " ~/~ ",
        group_var_value
      )
    )

  one_value_results <- create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
    group_var = NA_character_,
    analysis_var = "smvar",
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
      analysis_type = rep("prop_select_multiple", 8),
      analysis_var = rep("smvar", 8),
      analysis_var_value = rep(paste0("option", 1:4), 2),
      group_var = rep("groups", 8),
      group_var_value = c(rep("group_a", 4), rep("group_b", 4)),
      stat = c(rep(NaN, 4), c(1, NaN, NaN, 1)),
      stat_low = rep(NaN, 8),
      stat_upp = rep(NaN, 8),
      n = c(rep(0, 4), c(1, 0, 0, 1)),
      n_total = c(rep(0, 4), rep(1, 4)),
      n_w = c(rep(0, 4), c(1, 0, 0, 1)),
      n_w_total = c(rep(0, 4), rep(1, 4))
    ) %>%
    dplyr::mutate(
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        analysis_var,
        " ~/~ ",
        analysis_var_value,
        " @/@ ",
        group_var,
        " ~/~ ",
        group_var_value
      )
    )

  one_value_one_group_results <-
    create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
      group_var = "groups",
      analysis_var = "smvar",
      level = 0.95
    ) %>%
    suppressWarnings()
  expect_equal(one_value_one_group_results,
    one_value_one_group_expected_output,
    ignore_attr = T
  )
})

test_that("create_analysis_prop_select_multiple handles lonely PSU", {
  # 1 group has 50 observation, 1 group has 1 observation.
  somedata <- data.frame(
    groups = c(rep("group_a", 50), "group_b"),
    smvar = rep(NA_character_, 51),
    smvar.option1 = sample(c(TRUE, FALSE), size = 51, replace = T, prob = c(.7, .3)),
    smvar.option2 = sample(c(TRUE, FALSE), size = 51, replace = T, prob = c(.6, .4)),
    smvar.option3 = sample(c(TRUE, FALSE), size = 51, replace = T, prob = c(.1, .9)),
    smvar.option4 = sample(c(TRUE, FALSE), size = 51, replace = T, prob = c(.8, .2)),
    uuid = 1:51 %>% as.character()
  ) %>%
    cleaningtools::recreate_parent_column(uuid = "uuid", sm_sep = ".")

  somedata <- somedata$data_with_fix_concat

  groups_n <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::filter(!is.na(smvar)) %>%
    dplyr::tally(name = "n_total")

  lonely_psu_expected_output <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::filter(!is.na(smvar)) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::starts_with("smvar."), .fns = list(stat = mean, n = sum))) %>%
    tidyr::pivot_longer(
      cols = -groups,
      names_to = c("analysis_info", "analysis_type"), values_to = "x", names_sep = "_"
    ) %>%
    tidyr::pivot_wider(id_cols = c(groups, analysis_info), names_from = analysis_type, values_from = x) %>%
    tidyr::separate_wider_delim(analysis_info, delim = ".", names = c("analysis_var", "analysis_var_value")) %>%
    dplyr::left_join(groups_n) %>%
    dplyr::mutate(
      analysis_type = "prop_select_multiple",
      group_var = "groups",
      group_var_value = groups,
      n_w = n,
      n_w_total = n_total,
      analysis_key = paste0("prop_select_multiple @/@ smvar ~/~ ", analysis_var_value, " @/@ groups ~/~ ", group_var_value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      analysis_type, analysis_var, analysis_var_value,
      group_var, group_var_value, stat,
      n, n_total, n_w, n_w_total, analysis_key
    )

  lonely_psu_result <-
    create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
      group_var = "groups",
      analysis_var = "smvar",
      level = 0.95
    ) %>%
    dplyr::select(-stat_low, -stat_upp)
  expect_equal(lonely_psu_result,
    lonely_psu_expected_output,
    ignore_attr = T
  )
})
test_that("create_analysis_prop_select_multiple returns correct output with 3 grouping variable", {
  somedata <- data.frame(
    group_a = sample(c("male_hoh", "female_hoh"), 300, replace = T),
    group_b = sample(c(
      "district A", "district B"
    ), 300, replace = T),
    group_c = sample(c(
      "pop A", "pop B"
    ), 300, replace = T),
    smvar = rep(NA_character_, 300),
    smvar.option1 = sample(c(TRUE, FALSE), size = 300, replace = T, prob = c(.7, .3)),
    smvar.option2 = sample(c(TRUE, FALSE), size = 300, replace = T, prob = c(.6, .4)),
    smvar.option3 = sample(c(TRUE, FALSE), size = 300, replace = T, prob = c(.1, .9)),
    smvar.option4 = sample(c(TRUE, FALSE), size = 300, replace = T, prob = c(.8, .2)),
    uuid = 1:300 %>% as.character()
  ) %>%
    cleaningtools::recreate_parent_column(uuid = "uuid", sm_sep = ".")

  somedata <- somedata$data_with_fix_concat

  groups_n <- somedata %>%
    dplyr::group_by(group_a, group_b, group_c) %>%
    dplyr::filter(!is.na(smvar)) %>%
    dplyr::tally(name = "n_total")

  expected_output <- somedata %>%
    dplyr::group_by(group_a, group_b, group_c) %>%
    dplyr::filter(!is.na(smvar)) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::starts_with("smvar."), .fns = list(stat = mean, n = sum))) %>%
    tidyr::pivot_longer(
      cols = -c(group_a, group_b, group_c),
      names_to = c("analysis_info", "analysis_type"), values_to = "x", names_sep = "_"
    ) %>%
    tidyr::pivot_wider(id_cols = c(group_a, group_b, group_c, analysis_info), names_from = analysis_type, values_from = x) %>%
    tidyr::separate_wider_delim(analysis_info, delim = ".", names = c("analysis_var", "analysis_var_value")) %>%
    dplyr::left_join(groups_n) %>%
    dplyr::mutate(
      analysis_type = "prop_select_multiple",
      group_var = "group_a ~/~ group_b ~/~ group_c",
      n_w = n,
      n_w_total = n_total,
      analysis_key = paste0("prop_select_multiple @/@ smvar ~/~ ", analysis_var_value, " @/@")
    ) %>%
    tidyr::unite(group_var_value, group_a, group_b, group_c, sep = " ~/~ ") %>%
    dplyr::ungroup()
  x <-
    expected_output$group_var %>% stringr::str_split(" ~/~ ")
  y <-
    expected_output$group_var_value %>% stringr::str_split(" ~/~ ")

  to_add <-
    purrr::map2(x, y, function(x, y) {
      paste(x, y, sep = " ~/~ ")
    }) %>%
    purrr::map(stringr::str_c, collapse = " ~/~ ") %>%
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
    create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
      group_var = "group_a, group_b, group_c",
      analysis_var = "smvar",
      level = 0.95
    ) %>%
    dplyr::select(-stat_upp, -stat_low)
  expect_equal(results, expected_output)
})

test_that("create_analysis_prop_select_multiple returns correct output with 2 grouping variables and weighted", {
  set.seed(1222)
  somedata <- data.frame(
    group_a = sample(c("male_hoh", "female_hoh"), 300, replace = T),
    group_b = sample(c(
      "district A", "district B"
    ), 300, replace = T),
    group_c = sample(c(
      "pop A", "pop B"
    ), 300, replace = T),
    smvar = rep(NA_character_, 300),
    smvar.option1 = sample(c(TRUE, FALSE), size = 300, replace = T, prob = c(.7, .3)),
    smvar.option2 = sample(c(TRUE, FALSE), size = 300, replace = T, prob = c(.6, .4)),
    smvar.option3 = sample(c(TRUE, FALSE), size = 300, replace = T, prob = c(.1, .9)),
    smvar.option4 = sample(c(TRUE, FALSE), size = 300, replace = T, prob = c(.8, .2)),
    uuid = 1:300 %>% as.character()
  ) %>%
    cleaningtools::recreate_parent_column(uuid = "uuid", sm_sep = ".")

  somedata <- somedata$data_with_fix_concat

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
    dplyr::group_by(group_a, group_b) %>%
    dplyr::filter(!is.na(smvar)) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("smvar."),
      function(x) {
        x * weights
      },
      .names = "w_{.col}"
    )) %>%
    dplyr::summarise(
      n_w_total = sum(weights),
      n_total = dplyr::n(),
      dplyr::across(dplyr::starts_with("w_smvar."), sum),
      dplyr::across(dplyr::starts_with("smvar."), sum)
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("w_smvar."),
      function(x) {
        x / n_w_total
      },
      .names = "prop_{.col}"
    )) %>%
    dplyr::ungroup()

  expected_output <- expected_output %>%
    tidyr::pivot_longer(cols = -c(group_a, group_b, n_w_total, n_total)) %>%
    tidyr::separate_wider_delim(name, delim = ".", names = c("info", "analysis_var_value")) %>%
    dplyr::mutate(info = dplyr::case_when(
      info == "w_smvar" ~ "n_w",
      info == "smvar" ~ "n",
      info == "prop_w_smvar" ~ "stat"
    )) %>%
    tidyr::pivot_wider(
      id_cols = c(group_a, group_b, n_w_total, n_total, analysis_var_value),
      names_from = info,
      values_from = value
    ) %>%
    dplyr::mutate(
      analysis_type = "prop_select_multiple",
      analysis_var = "smvar",
      group_var = "group_a ~/~ group_b",
      analysis_key = paste("prop_select_multiple @/@ smvar ~/~", analysis_var_value, "@/@")
    ) %>%
    tidyr::unite(group_var_value, group_a, group_b, sep = " ~/~ ")

  x <-
    expected_output$group_var %>% stringr::str_split(" ~/~ ")
  y <-
    expected_output$group_var_value %>% stringr::str_split(" ~/~ ")

  to_add <-
    purrr::map2(x, y, function(x, y) {
      paste(x, y, sep = " ~/~ ")
    }) %>%
    purrr::map(stringr::str_c, collapse = " ~/~ ") %>%
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
    create_analysis_prop_select_multiple(srvyr::as_survey(somedata, weights = weights),
      group_var = "group_a, group_b",
      analysis_var = "smvar",
      level = 0.95
    ) %>%
    dplyr::select(-stat_upp, -stat_low)
  expect_equal(results, expected_output)
})

test_that("create_analysis_prop_select_multiple handles NA in the dummy variables", {
  somedata <- data.frame(
    groups = sample(c("group_a", "group_b"), size = 100, replace = T),
    smvar = rep(NA_character_, 100),
    smvar.option1 = sample(c(TRUE, FALSE, NA), size = 100, replace = T, prob = c(.65, .25, .1)),
    smvar.option2 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.6, .4)),
    smvar.option3 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.1, .9)),
    smvar.option4 = sample(c(TRUE, FALSE), size = 100, replace = T, prob = c(.8, .2)),
    uuid = 1:100 %>% as.character()
  ) %>%
    cleaningtools::recreate_parent_column(uuid = "uuid", sm_sep = ".")

  somedata <- somedata$data_with_fix_concat

  # no group
  expected_output <- somedata %>%
    dplyr::filter(!is.na(smvar)) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::starts_with("smvar."),
                                   .fns = list(stat = ~mean(.x, na.rm=T),
                                               n = ~sum(.x, na.rm =T ),
                                               n_total = ~sum(!is.na(.x))),
                                   .names = "{.col}@ / @{.fn}")) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("smvar."),
      names_to = c("analysis_info", "analysis_type"), values_to = "x", names_sep = "@ / @"
    ) %>%
    tidyr::pivot_wider(id_cols = analysis_info, names_from = analysis_type, values_from = x) %>%
    tidyr::separate_wider_delim(analysis_info, delim = ".", names = c("analysis_var", "analysis_var_value")) %>%
    dplyr::mutate(
      analysis_type = "prop_select_multiple",
      group_var = NA_character_,
      group_var_value = NA_character_,
      n_w = n,
      n_w_total = n_total,
      analysis_key = paste0("prop_select_multiple @/@ smvar ~/~ ", analysis_var_value, " @/@ NA ~/~ NA")
    ) %>%
    dplyr::select(
      analysis_type, analysis_var, analysis_var_value,
      group_var, group_var_value, stat,
      n, n_total, n_w, n_w_total, analysis_key
    )
  actual_output <- create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
                                                        group_var = NA,
                                                        analysis_var = "smvar",
                                                        level = 0.95
  ) %>%
    dplyr::select(-stat_low, -stat_upp)
  expect_equal(actual_output,
               expected_output,
               ignore_attr = T
  )

  # with 1 group
  one_group_expected_output <- somedata %>%
    dplyr::group_by(groups) %>%
    dplyr::filter(!is.na(smvar)) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::starts_with("smvar."),
                                   .fns = list(stat = ~mean(.x, na.rm=T),
                                               n = ~sum(.x, na.rm =T ),
                                               n_total = ~sum(!is.na(.x))),
                                   .names = "{.col}@ / @{.fn}")) %>%
    tidyr::pivot_longer(
      cols = -groups,
      names_to = c("analysis_info", "analysis_type"), values_to = "x", names_sep = "@ / @"
    ) %>%
    tidyr::pivot_wider(id_cols = c(groups, analysis_info), names_from = analysis_type, values_from = x) %>%
    tidyr::separate_wider_delim(analysis_info, delim = ".", names = c("analysis_var", "analysis_var_value")) %>%
    dplyr::mutate(
      analysis_type = "prop_select_multiple",
      group_var = "groups",
      group_var_value = groups,
      n_w = n,
      n_w_total = n_total,
      analysis_key = paste0("prop_select_multiple @/@ smvar ~/~ ", analysis_var_value, " @/@ groups ~/~ ", group_var_value)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      analysis_type, analysis_var, analysis_var_value,
      group_var, group_var_value, stat,
      n, n_total, n_w, n_w_total, analysis_key
    )

  one_group_result <-
    create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
                                         group_var = "groups",
                                         analysis_var = "smvar",
                                         level = 0.95
    ) %>%
    dplyr::select(-stat_low, -stat_upp)

  expect_equal(one_group_result,
               one_group_expected_output,
               ignore_attr = T
  )
})

test_that("create_analysis_prop_select_multiple works with 0/1's instead of TRUE/FALSE", {
  somedata <- data.frame(
    groups = sample(c("group_a", "group_b"), size = 100, replace = T),
    smvar = rep(NA_character_, 100),
    smvar.option1 = sample(c(1, 0), size = 100, replace = T, prob = c(.7, .3)),
    smvar.option2 = sample(c(1, 0), size = 100, replace = T, prob = c(.6, .4)),
    smvar.option3 = sample(c(1, 0), size = 100, replace = T, prob = c(.1, .9)),
    smvar.option4 = sample(c(1, 0), size = 100, replace = T, prob = c(.8, .2)),
    uuid = 1:100 %>% as.character()
  ) %>%
    cleaningtools::recreate_parent_column(uuid = "uuid", sm_sep = ".")

  somedata <- somedata$data_with_fix_concat

  # no group
  expected_output <- somedata %>%
    dplyr::filter(!is.na(smvar)) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::starts_with("smvar."), .fns = list(stat = mean, n = sum))) %>%
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = c("analysis_info", "analysis_type"), values_to = "x", names_sep = "_"
    ) %>%
    tidyr::pivot_wider(id_cols = analysis_info, names_from = analysis_type, values_from = x) %>%
    tidyr::separate_wider_delim(analysis_info, delim = ".", names = c("analysis_var", "analysis_var_value")) %>%
    dplyr::mutate(
      analysis_type = "prop_select_multiple",
      group_var = NA_character_,
      group_var_value = NA_character_,
      n_total = sum(!is.na(somedata$smvar)),
      n_w = n,
      n_w_total = sum(!is.na(somedata$smvar)),
      analysis_key = paste0("prop_select_multiple @/@ smvar ~/~ ", analysis_var_value, " @/@ NA ~/~ NA")
    ) %>%
    dplyr::select(
      analysis_type, analysis_var, analysis_var_value,
      group_var, group_var_value, stat,
      n, n_total, n_w, n_w_total, analysis_key
    )
  actual_output <- create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
                                                        group_var = NA,
                                                        analysis_var = "smvar",
                                                        level = 0.95
  ) %>%
    dplyr::select(-stat_low, -stat_upp)
  expect_equal(actual_output,
               expected_output,
               ignore_attr = T
  )
})

