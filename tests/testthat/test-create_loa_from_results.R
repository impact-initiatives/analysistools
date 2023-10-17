test_that("loa is created correctly", {

  #no ratio, 1 grouping var
  expect_equal(create_loa_from_results(analysistools_MSNA_template_no_ratio_results_table$results_table),
               analysistools_MSNA_template_loa, ignore_attr = T)

  #with ratio, 1 grouping var
  expect_equal(create_loa_from_results(analysistools_MSNA_template_with_ratio_results_table$results_table),
               analysistools_MSNA_template_loa_with_ratio, ignore_attr = T)

  #no ratio, 2 grouping var
  no_ratio_2groups_test_loa <- data.frame(analysis_type = "prop_select_one",
                         analysis_var = "wash_drinkingwatersource",
                         group_var = "admin1, admin2",
                         level = .95
                         )
  no_ratio_2groups_test_input <- create_analysis(srvyr::as_survey(analysistools_MSNA_template_data),
                                loa = no_ratio_2groups_test_loa)
  expect_equal(create_loa_from_results(no_ratio_2groups_test_input$results_table),
               no_ratio_2groups_test_loa, ignore_attr = T)

  #with ratio, 2 grouping var
  ratio_2groups_test_loa <- data.frame(analysis_type = "ratio",
                                       group_var = "admin1, admin2",
                                       level = .95,
                                       analysis_var_numerator = "income_v1_salaried_work",
                                       analysis_var_denominator = "expenditure_debt",
                                       numerator_NA_to_0 = TRUE,
                                       filter_denominator_0 = TRUE
  )
  ratio_2groups_test_input <- create_analysis(srvyr::as_survey(analysistools_MSNA_template_data),
                                                 loa = ratio_2groups_test_loa)
  expect_equal(create_loa_from_results(ratio_2groups_test_input$results_table),
               ratio_2groups_test_loa, ignore_attr = T)


  #with ratio, 3 grouping var
  three_groups_test_loa <- data.frame(analysis_type = "prop_select_one",
                                          analysis_var = "wash_drinkingwatersource",
                                          group_var = "admin1, admin2, admin3",
                                          level = .95
  )
  three_groups_test_input <- create_analysis(srvyr::as_survey(analysistools_MSNA_template_data),
                                                 loa = three_groups_test_loa)
  expect_equal(create_loa_from_results(three_groups_test_input$results_table),
               three_groups_test_loa, ignore_attr = T)

  #2 and 3 grouping var no_space
  no_space_group_var_test_loa <- data.frame(analysis_type = "prop_select_one",
                                          analysis_var = "wash_drinkingwatersource",
                                          group_var = c("admin1,admin2", "admin1,admin2,admin3"),
                                          level = .95
  )
  no_space_group_var_input <- create_analysis(srvyr::as_survey(analysistools_MSNA_template_data),
                                             loa = no_space_group_var_test_loa)
  expect_equal(create_loa_from_results(no_space_group_var_input$results_table)$group_var,
               c("admin1, admin2", "admin1, admin2, admin3"))
})

