#' Calculate a mean from a survey
#'
#' @param .dataset : design survey
#' @param dap : a vector containing the following information
#' - group_var : dependent variable(s), variable to group by. If no dependent variable, it should be NA
#' or empty string. If more than one variable, it should be one string with each variable separated by ,
#' - analysis_var : the independent variable, variable to summarise
#' - level : the confidence level to use to compute the confidence interval.
#'
#' @return a data frame with the mean for each group
#' @export
#'
#' @examples
#' somedata <- data.frame(aa = 1:10,
#'                        bb = rep(c("a","b"),5),
#'                        weights = rep(c(.5,1.5),5))
#' dap_mean <- data.frame(group_var = c(NA, "bb"),
#'                        analysis_var = c("aa", "aa"),
#'                        level = c(.95, .95))
#' me_design <- srvyr::as_survey(somedata)
#' calculate_mean(me_design, dap_mean[1,])
#' calculate_mean(me_design, dap_mean[2,])
#'
#' me_design_w <- srvyr::as_survey(somedata, weights = weights)
#' calculate_mean(me_design_w, dap_mean[1,])
#' calculate_mean(me_design_w, dap_mean[2,])
#'
calculate_mean <- function(.dataset, dap) {
  if (is.na(dap[["group_var"]])) {
    across_by <- c()
  } else {
    across_by <- dap[["group_var"]] %>%
      stringr::str_split(",", simplify = T) %>%
      stringr::str_trim() %>% as.vector()
  }
  results <- .dataset %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(across_by))) %>%
    dplyr::filter(!is.na(!!rlang::sym(dap[["analysis_var"]])), .preserve = T) %>%
    srvyr::summarise(srvyr::survey_mean(
      !!rlang::sym(dap[["analysis_var"]]),
      vartype = "ci",
      level = as.numeric(dap[["level"]]),
      na.rm = T
    )) %>%
    dplyr::mutate(
      group_var = dap[["group_var"]] %>% stringr::str_replace_all(",", " ~/~"),
      analysis_var = dap[["analysis_var"]],
      analysis_var_value = NA_character_,
      analysis_type = "mean",
    ) %>%
    dplyr::rename(stat = coef,
                  stat_low = `_low`,
                  stat_upp = `_upp`) %>%
    dplyr::mutate(stat = dplyr::case_when(is.nan(stat_low) &
                                            is.nan(stat_upp) &
                                            stat == 0 ~ NaN,
                                          TRUE ~ stat))

  if (is.na(dap[["group_var"]])) {
    results <- results %>%
      dplyr::mutate(group_var_value = NA_character_)
  } else {
    results <- results %>%
      tidyr::unite("group_var_value", dplyr::all_of(across_by), sep =  " ~/~ ")
  }

  x <- results$group_var %>% stringr::str_split(" ~/~ ")
  y <- results$group_var_value %>% stringr::str_split(" ~/~ ")
  to_add <-
    purrr::map2(x, y, function(x, y)
      paste(x, y, sep = " ~/~ ")) %>% purrr::map(stringr::str_c, collapse = " ~/~ ") %>% do.call(c, .)

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
      analysis_key
    )
}
