#' Calculate a mean from a survey
#'
#' @param .dataset : design survey
#' @param dap : a vector containing the following information
#' - group : dependent variable(s), variable to group by. If no dependent variable, it should be NA
#' or empty string. If more than one variable, it should be one string with each variable separated by ,
#' - question_name : the independent variable, variable to summarise
#' - level : the confidence level to use to compute the confidence interval.
#'
#' @return a data frame with the mean for each group
#' @export
#'
#' @examples
#' somedata <- data.frame(aa = 1:10,
#'                        bb = rep(c("a","b"),5),
#'                        weights = rep(c(.5,1.5),5))
#' dap_mean <- data.frame(group = c(NA, "bb"),
#'                        question_name = c("aa", "aa"),
#'                        level = c(.95, .95))
#' me_design <- srvyr::as_survey(somedata)
#' calculate_mean(me_design, dap_mean[1,])
#' calculate_mean(me_design, dap_mean[2,])
#'
#' me_design_w <- srvyr::as_survey(somedata, weights = weights)
#' calculate_mean(me_design_w, dap_mean[1,])
#' calculate_mean(me_design_w, dap_mean[2,])
#'
#'#oneday
calculate_mean <- function(.dataset, dap) {
  if(is.na(dap[["group"]])) {
    across_by <- c()
  } else {
    #update multiple selection 06/09/2022
    # across_by <- dap[["group"]]
    #update 03/01/2023 adding the str_trim
    across_by <- dap[["group"]] %>%
      stringr:: str_split(",", simplify = T) %>%
      stringr::str_trim() %>% as.vector()
  }
  me_results <- .dataset %>%
    ####adding a filter 24082022
    dplyr::group_by(dplyr::across(dplyr::any_of(across_by))) %>%
    dplyr::filter(!is.na(!!rlang::sym(dap[["question_name"]]))) %>%
    srvyr::summarise(srvyr::survey_mean(!!rlang::sym(dap[["question_name"]]),
                                        vartype = "ci",
                                        level = as.numeric(dap[["level"]]))) %>%
    dplyr::mutate(group = dap[["group"]],
           name = dap[["question_name"]],
           choice = NA,
           analysis_type = "mean") %>%
    dplyr::rename(stat = coef,
           stat_low = `_low`,
           stat_upp = `_upp`)

  if(is.na(dap[["group"]])) {
    me_results %>%
      dplyr::mutate(groupped_by = NA) %>%
      return()
  } else {(
    #update multiple selection 06/09/2022
    # me_results %>%
    #   rename(groupped_by = !!sym(dap[["group"]]))
    me_results %>%
      tidyr::unite("groupped_by", dplyr::all_of(across_by), sep =  " ~/~ ")
  )}
}
