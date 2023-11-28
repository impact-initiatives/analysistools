#' Calculate proportion from a survey
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
#' @return a data frame with the proportion for each group and th analysis index.
#' @export
#'
#' @examples
#' somedata <- data.frame(
#'   groups = sample(c("group_a", "group_b"),
#'     size = 100,
#'     replace = TRUE
#'   ),
#'   value = sample(c("a", "b", "c"),
#'     size = 100, replace = TRUE,
#'     prob = c(.6, .4, .1)
#'   )
#' )
#' create_analysis_prop_select_one(srvyr::as_survey(somedata, strata = groups),
#'   group_var = NA,
#'   analysis_var = "value",
#'   level = .95
#' )
#' create_analysis_prop_select_one(srvyr::as_survey(somedata, strata = groups),
#'   group_var = "groups",
#'   analysis_var = "value",
#'   level = .95
#' )
create_analysis_prop_select_one <- function(design, group_var = NA, analysis_var, level = .95) {
  # check the grouping variable
  if (is.na(group_var)) {
    across_by <- c(analysis_var)
  } else {
    grouping_c <- group_var %>%
      char_to_vector()
    across_by <- c(grouping_c, analysis_var)
  }

  # calculate
  pre_design <- design %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(across_by)))


  results <- pre_design %>%
    dplyr::filter(!is.na(!!rlang::sym(analysis_var)), .preserve = T) %>%
    srvyr::summarise(
      srvyr::survey_prop(
        vartype = "ci",
        level = as.numeric(level),
        proportion = FALSE
      ),
      n = dplyr::n(),
      n_w = srvyr::survey_total(
        vartype = "ci",
        level = as.numeric(level),
        na.rm = T
      )
    )  %>%
    dplyr::mutate(
      group_var = create_group_var(group_var),
      analysis_var = analysis_var,
      analysis_type = "prop_select_one",
      n_total = sum(n),
      n_w_total = sum(n_w)
    )

  NA_counts <- pre_design %>%
    dplyr::filter(is.na(!!rlang::sym(analysis_var)), .preserve = T) %>%
    dplyr::summarise(na_counts = dplyr::n())

  results <- results %>%
    dplyr::left_join(NA_counts) %>%
    dplyr::mutate(n = dplyr::if_else(na_counts > 0, na_counts, n))

  results <- results %>%
    dplyr::rename(
      stat = coef,
      stat_low = `_low`,
      stat_upp = `_upp`,
      analysis_var_value = !!rlang::sym(analysis_var)
    ) %>%
    correct_nan_analysis_var_variable_is_na()

  # adding group_var_value
  results <-
    adding_group_var_value(
      results = results,
      group_var = group_var,
      grouping_vector = grouping_c
    )
  # adding analysis key
  results <- adding_analysis_key(results = results)
  # re-arranging the columns
  results <- results %>%
    arranging_results_columns()

  return(results)
}
