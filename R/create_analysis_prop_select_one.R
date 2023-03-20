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
#'somedata <- data.frame(groups = sample(c("group_a", "group_b"), size = 100, replace = T),
#'                       value = sample(c("a", "b", "c"), size = 100, replace = T,prob = c(.6,.4,.1)))
#'dap <- data.frame(group_var = c(NA, "groups"),
#'                  analysis_var = c("value", "value"),
#'                  level = c(0.95, 0.95))
#'create_analysis_prop_select_one(srvyr::as_survey(somedata, strata = groups), dap[1,])
#'create_analysis_prop_select_one(srvyr::as_survey(somedata, strata = groups), dap[2,])

create_analysis_prop_select_one <- function(.dataset, dap) {
    if(is.na(dap[["group_var"]])) {
      across_by <- c(dap[["analysis_var"]])
    } else {
      grouping_c <- dap[["group_var"]] %>%
        char_to_vector()
      across_by <- c(grouping_c,dap[["analysis_var"]])
    }
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


    if (is.na(dap[["group_var"]])) {
      results <- results %>%
        dplyr::mutate(group_var_value = NA_character_)
    } else {
      results <- results %>%
        tidyr::unite("group_var_value", dplyr::all_of(grouping_c), sep = " ~/~ ")
    }

    x <- results$group_var %>% stringr::str_split(" ~/~ ")
    y <- results$group_var_value %>% stringr::str_split(" ~/~ ")
    to_add <-
      purrr::map2(x, y, function(x, y) {
        paste(x, y, sep = " ~/~ ")
      }) %>%
      purrr::map(stringr::str_c, collapse = " ~/~ ") %>%
      do.call(c, .)

    results %>%
      dplyr::mutate(
        analysis_key = paste0(
          analysis_type,
          " @/@ ",
          analysis_var,
          " ~/~ ",
          analysis_var_value,
          " @/@"
        ),
        analysis_key = paste(analysis_key, to_add)
      ) %>%
      dplyr::select(
        analysis_type,
        analysis_var,
        analysis_var_value,
        group_var,
        group_var_value,
        stat,
        stat_low,
        stat_upp,
        n,
        n_total,
        n_w,
        n_w_total,
        analysis_key
      )
  }
