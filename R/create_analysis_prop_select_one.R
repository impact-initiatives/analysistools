#' Calculate proportion from a survey
#'
#' @param .dataset design survey
#' @param dap a vector containing the following information
#' - group_var : dependent variable(s), variable to group by. If no dependent variable, it should be NA
#' or empty string. If more than one variable, it should be one string with each variable separated by ,
#' - analysis_var : the independent variable, variable to summarise
#' - level : the confidence level to use to compute the confidence interval.
#'
#' @return a data frame with the proportion for each group and th analysis index.
#' @export
#'
#' @examples
#'somedata <- data.frame(groups = sample(c("group_a", "group_b"), size = 100,
#'                       replace = TRUE),
#'                       value = sample(c("a", "b", "c"), size = 100, replace = TRUE,
#'                       prob = c(.6,.4,.1)))
#'dap <- data.frame(group_var = c(NA, "groups"),
#'                  analysis_var = c("value", "value"),
#'                  level = c(0.95, 0.95))
#'create_analysis_prop_select_one(srvyr::as_survey(somedata, strata = groups), dap[1,])
#'create_analysis_prop_select_one(srvyr::as_survey(somedata, strata = groups), dap[2,])

create_analysis_prop_select_one <- function(.dataset, dap) {
  #check the grouping variable
  if(is.na(dap[["group_var"]])) {
    across_by <- c(dap[["analysis_var"]])
  } else {
    grouping_c <- dap[["group_var"]] %>%
      char_to_vector()
    across_by <- c(grouping_c,dap[["analysis_var"]])
  }

  #calculate
  results <- .dataset %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(across_by))) %>%
    dplyr::filter(!is.na(!!rlang::sym(dap[["analysis_var"]])), .preserve = T) %>%
    srvyr::summarise(srvyr::survey_prop(vartype = "ci", level = as.numeric(dap[["level"]])),
                     n = dplyr::n(),
                     n_w = srvyr::survey_total(
                       vartype = "ci",
                       level = as.numeric(dap[["level"]]),
                       na.rm = T
                     )
    ) %>%
    dplyr::mutate(
      group_var = dap[["group_var"]] %>% stringr::str_replace_all(",", " ~/~"),
      analysis_var = dap[["analysis_var"]],
      analysis_type = "prop_select_one",
      n_total = sum(n), #??
      n_w_total = sum(n_w) #??
    ) %>%
    dplyr::rename(stat = coef,
                  stat_low = `_low`,
                  stat_upp = `_upp`,
                  analysis_var_value = !!rlang::sym(dap[["analysis_var"]])) %>%
    dplyr::mutate(stat = dplyr::case_when(
      is.nan(stat_low) &
        is.nan(stat_upp) &
        stat == 0 ~ NaN,
      TRUE ~ stat
    ))

  #adding group_var_value
  results <- adding_group_var_value(results = results, dap = dap, grouping_vector = grouping_c)
  #adding analysis key
  results <- adding_analysis_key(results = results)
  #re-arranging the columns
  results %>%
    arranging_results_columns()
}