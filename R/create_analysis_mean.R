#' Calculate a mean from a survey
#'
#' @param design design survey
#' @param group_var dependent variable(s), variable to group by. If no dependent
#' variable, it should be NA or empty string. If more than one variable, it
#' should be one string with each variable separated by comma, e.g. "groupa, groupb"
#' to group for groupa and groupb.
#' NA is default for no grouping.
#' @param analysis_var the independent variable, variable to summarise
#' @param level the confidence level. 0.95 is default
#'
#' @return a data frame with the mean for each group
#' @export
#'
#' @examples
#' somedata <- data.frame(
#'   aa = 1:10,
#'   bb = rep(c("a", "b"), 5),
#'   weights = rep(c(.5, 1.5), 5)
#' )
#' me_design <- srvyr::as_survey(somedata)
#' create_analysis_mean(me_design, analysis_var = "aa")
#' create_analysis_mean(me_design, group_var = "bb", analysis_var = "aa")
#'
#' me_design_w <- srvyr::as_survey(somedata, weights = weights)
#' create_analysis_mean(me_design_w, analysis_var = "aa")
#' create_analysis_mean(me_design_w, group_var = "bb", analysis_var = "aa")
#'
create_analysis_mean <- function(design, group_var = NA, analysis_var, level = .95) {
  # check the grouping variable
  if (is.na(group_var)) {
    across_by <- c()
  } else {
    across_by <- group_var %>%
      char_to_vector()
  }

  # calculate
  results <- design %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(across_by))) %>%
    dplyr::filter(!is.na(!!rlang::sym(analysis_var)), .preserve = T) %>%
    srvyr::summarise(
      srvyr::survey_mean(
        !!rlang::sym(analysis_var),
        vartype = "ci",
        level = as.numeric(level),
        na.rm = T
      ),
      n = dplyr::n(),
      n_w = srvyr::survey_total(
        vartype = "ci",
        level = as.numeric(level),
        na.rm = T
      )
    ) %>%
    dplyr::mutate(
      group_var = create_group_var(group_var),
      analysis_var = analysis_var,
      analysis_var_value = NA_character_,
      analysis_type = "mean",
      n_total = n, # for mean we want the denominator
      n_w_total = n_w # for mean we want the denominator
    ) %>%
    dplyr::rename(
      stat = coef,
      stat_low = `_low`,
      stat_upp = `_upp`
    ) %>%
    correct_nan_total_is_0()

  # adding group_var_value
  results <- adding_group_var_value(results = results, group_var = group_var, grouping_vector = across_by)
  # adding analysis key
  results <- adding_analysis_key(results = results)
  # re-arranging the columns
  results %>%
    arranging_results_columns()
}
