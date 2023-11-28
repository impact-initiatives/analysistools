#' Calculate proportion from a survey design
#'
#' @param design design survey
#' @param group_var dependent variable(s), variable to group by. If no dependent
#' variable, it should be NA or empty string. If more than one variable, it
#' should be one string with each variable separated by comma, e.g. "groupa, groupb"
#' to group for groupa and groupb.
#' NA is default for no grouping.
#' @param analysis_var the independent variable, variable to summarise. It has to be the parent
#' select multiple
#' @param level the confidence level. 0.95 is default
#' @param sm_separator Select multiple separator. Default is "."
#'
#' @return a data frame with the proportion for each group and the analysis index.
#' @export
#'
#' @examples
#' somedata <- data.frame(
#'   groups = sample(c("group_a", "group_b"), size = 100, replace = TRUE),
#'   smvar = rep(NA_character_, 100),
#'   smvar.option1 = sample(c(TRUE, FALSE), size = 100, replace = TRUE, prob = c(.7, .3)),
#'   smvar.option2 = sample(c(TRUE, FALSE), size = 100, replace = TRUE, prob = c(.6, .4)),
#'   smvar.option3 = sample(c(TRUE, FALSE), size = 100, replace = TRUE, prob = c(.1, .9)),
#'   smvar.option4 = sample(c(TRUE, FALSE), size = 100, replace = TRUE, prob = c(.8, .2)),
#'   uuid = 1:100 %>% as.character()
#' ) %>%
#'   cleaningtools::recreate_parent_column(uuid = "uuid", sm_separator = ".")
#'
#' somedata <- somedata$data_with_fix_concat
#'
#' create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
#'   group_var = NA,
#'   analysis_var = "smvar",
#'   level = 0.95
#' )
#'
#' create_analysis_prop_select_multiple(srvyr::as_survey(somedata),
#'   group_var = "groups",
#'   analysis_var = "smvar",
#'   level = 0.95
#' )
create_analysis_prop_select_multiple <- function(design, group_var = NA, analysis_var, level = .95, sm_separator = ".") {
  # check the grouping variable
  if (is.na(group_var)) {
    across_by <- c()
  } else {
    across_by <- group_var %>%
      char_to_vector()
  }

  # calculate
  sm_var <- analysis_var
  sm_var_choice_start <- paste0(analysis_var, sm_separator)

  ## prepare the dataset
  predesign <- design %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with(sm_var_choice_start), as.numeric)) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(across_by)))

  ## get the stats
  results <- predesign %>%
    dplyr::filter(!is.na(!!rlang::sym(analysis_var)), .preserve = T) %>%
    srvyr::summarise(dplyr::across(dplyr::starts_with(sm_var_choice_start),
      .fns = list(
        stat = ~ srvyr::survey_mean(.x,
          vartype = "ci",
          na.rm = TRUE,
          level = level
        ),
        n_w = ~ srvyr::survey_total(.x,
          vartype = "ci",
          na.rm = TRUE,
          level = level
        ),
        n = ~ sum(.x, na.rm = TRUE),
        n_w_total = ~ srvyr::survey_total(!is.na(.x),
          vartype = "ci",
          na.rm = TRUE
        ),
        n_total = ~ sum(!is.na(.x), na.rm = TRUE)
      ),
      .names = "{.fn}...{.col}"
    ))

  n...sm_var_NA <- paste0("n...",sm_var_choice_start,"NA")
  NA_counts <- predesign %>%
    dplyr::filter(is.na(!!rlang::sym(analysis_var)), .preserve = T) %>%
    dplyr::summarise(!!rlang::sym(n...sm_var_NA) := dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyr::contains("n..."))

  results <- results %>% cbind(NA_counts)

  ## 1st manipulation to get all the information in a long format
  results <- results %>%
    dplyr::mutate(analysis_var = sm_var) %>%
    tidyr::pivot_longer(-c(analysis_var, dplyr::all_of(across_by)), names_to = "analysis_var_value", values_to = "stat") %>%
    tidyr::separate_wider_delim(analysis_var_value, delim = "...", names = c("type", "analysis_var_value")) %>%
    dplyr::mutate(
      analysis_var_value = gsub(sm_var_choice_start, "", analysis_var_value),
      type = dplyr::case_when(
        stringr::str_detect(analysis_var_value, "_low$") ~ paste0(type, "_low"),
        stringr::str_detect(analysis_var_value, "_upp$") ~ paste0(type, "_upp"),
        TRUE ~ type
      ),
      analysis_var_value = stringr::str_replace(analysis_var_value, "_low$|_upp$", ""),
      group_var = create_group_var(group_var),
      analysis_type = "prop_select_multiple"
    ) %>%
    dplyr::filter(type %in% c("stat", "stat_low", "stat_upp", "n_w", "n", "n_w_total", "n_total", "na_count"))
  ## 2nd manipulation to the wide format
  results <- results %>%
    tidyr::pivot_wider(
      id_cols = c(dplyr::all_of(across_by), group_var, analysis_var, analysis_var_value, analysis_type),
      names_from = type,
      values_from = stat
    )  %>%
    ## filter NA is 0 and correcting for NaN. To count NA for select_multiple, I need to keep the
    ## NA counts in the data manipulation above
    dplyr::filter(!(analysis_var_value == "NA" & n == 0)) %>%
    correct_nan_analysis_var_variable_is_na() %>%
    correct_nan_total_is_0()

  # adding group_var_value
  results <- adding_group_var_value(results = results, group_var = group_var, grouping_vector = across_by)
  # adding analysis key
  results <- adding_analysis_key(results = results)
  # re-arranging the columns
  results %>%
    arranging_results_columns()
}
