#' Create analysis from a design
#'
#' If not *data analysis plan (dap)* is provided, the analysis will run on the overall dataset, and all
#' the grouping variables set.
#'
#' The *dap* should contains the following columns :
#' - analysis_type: analysis type to be perform. At the moment mean, median, prop_select_one, and
#' ratio are available.
#' - analysis_var: analysis variable to be used as string.
#' - group_var: The grouping variable as string. NA if there is no grouping variable. If a combination
#' of grouping variable should be used together it should be 1 string character separated with a ",".
#' i.e. c("admin1", "population") and "admin1, population" are different.
#'    - c("admin1", "population") : will perform the analysis grouping once by admin1, and once by population
#'    - "admin1, population" : will perform the analysis grouping once by admin1 and admin 2
#' - level: confidence level to be used. If the column does not exists, .95 will be used.
#' It can also include a column **level**, if not provided .95 will be set as default.
#'
#' If ratios have to be performed, the *dap* should include the following columns as well:
#' - analysis_var_numerator analysis_var_denominator numerator_NA_to_0 filter_denominator_0
#'
#' @param .design Survey design object created with srvyr::as_survey or as_survey_design
#' @param dap Data Analysis plan: Default is NULL. If provided it will be used to create the analysis.
#' @param group_var Default is NULL. If provided, it will first create a Data Analysis Plan and then
#' will run the analysis. It should be a vector.
#'
#' @return A list with 3 items:
#'  - The results table in a long format with the analysis key
#'  - The dataset that was used
#'  - The data analysis plan that was used
#' @export
#'
#' @examples
#' create_analysis(.design = srvyr::as_survey(analysistools_MSNA_template_data),
#'                 dap = analysistools_dap)
#'
#' create_analysis(.design = srvyr::as_survey(analysistools_MSNA_template_data),
#'                 dap = analysistools_dap_with_ratio)
#'
#' shorter_df <- analysistools_MSNA_template_data[,c("admin1",
#'                                                   "expenditure_debt",
#'                                                   "wash_drinkingwatersource")]
#'
#' create_analysis(.design = srvyr::as_survey(shorter_df),
#'                 group_var = "admin1")
create_analysis <- function(.design,
                            dap = NULL,
                            group_var = NULL
                            ) {

  if(!"tbl_svy" %in% attributes(.design)$class) {
    stop("It seems object design is not a design, did you use srvyr::as_survey ?")
  }

  if(!is.null(dap) & !is.null(group_var)) {
    warning("You have provided a data analysis plan and group variable, group variable will be ignored")
  }

  if(!is.null(dap)) {
    dap <- check_dap(dap, .design)
  }

  if(is.null(dap)) {
    dap <- create_dap(.design = .design, group_var = group_var)
  }


  results_list <- dap %>%
         split(1:nrow(.)) %>%
    purrr::map(function(dap){
      if(dap[["analysis_type"]] == "mean") {
        return(create_analysis_mean(.design,
                                    group_var = dap[["group_var"]],
                                    analysis_var = dap[["analysis_var"]],
                                    level = dap[["level"]]))
      }
      if(dap[["analysis_type"]] == "median") {
        return(create_analysis_median(.design,
                                      group_var = dap[["group_var"]],
                                      analysis_var = dap[["analysis_var"]],
                                      level = dap[["level"]]))
      }
      if(dap[["analysis_type"]] == "prop_select_one") {
        return(create_analysis_prop_select_one(.design,
                                               group_var = dap[["group_var"]],
                                               analysis_var = dap[["analysis_var"]],
                                               level = dap[["level"]]))
      }
      if(dap[["analysis_type"]] == "ratio") {
        return(create_analysis_ratio(.design,
                                     group_var = dap[["group_var"]],
                                     analysis_var_numerator = dap[["analysis_var_numerator"]],
                                     analysis_var_denominator = dap[["analysis_var_denominator"]],
                                     numerator_NA_to_0 = dap[["numerator_NA_to_0"]],
                                     filter_denominator_0 = dap[["filter_denominator_0"]],
                                     level = dap[["level"]]))
      }
    }, .progress = T)


  results_table <- results_list  %>%
    do.call(rbind,.)

  return(list(results_table = results_table,
              dataset = .design$variables,
              dap = dap))

}

#' Create a data analysis plan from design and a grouping variable
#'
#' Helper for the create_analysis. It will take all the columns that are character, logical, double
#' or integers.
#'
#' @param .design Survey design object created with srvyr::as_survey or as_survey_design
#' @param group_var The grouping variable as string. If a combination of grouping variable should be
#'  used together it should be 1 string character separated with a ",". i.e. c("admin1", "population")
#'  and "admin1, population" are different:
#'    - c("admin1", "population") : will perform the analysis grouping once by admin1, and once by population
#'    - "admin1, population" : will perform the analysis grouping once by admin1 and admin 2
#'
#' @return a data analysis plan.
#' @export
#'
#' @examples
#' shorter_df <- analysistools_MSNA_template_data[,c("admin1",
#'                                                "expenditure_debt",
#'                                                "wash_drinkingwatersource")]
#'
#' create_dap(.design = srvyr::as_survey(shorter_df),
#'            group_var = "admin1")

create_dap <- function(.design,
                       group_var = NULL) {
  dap_dictionary <- data.frame(type = c("character", "double", "double", "logical", "integer", "integer"),
                               analysis_type = c("prop_select_one", "mean", "median", "prop_select_one", "mean", "median"))
  #select multiple is prop_select_one for the time being. the create_analysis_prop_select_multiple does not exist yet.

  dap <- .design$variables %>%
    sapply(typeof) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("analysis_var") %>%
    dplyr::rename(.,type = `.`) %>%
    dplyr::left_join(dap_dictionary) %>%
    dplyr::filter(!is.na(analysis_type))

  if(is.null(group_var)) {
    dap$group_var <- NA_character_
  } else {
    dap <- lapply(c(NA, group_var), function(x) {dap$group_var <- x; return(dap)}) %>%
      do.call(rbind,.)
    dap <- dap %>%
      dplyr::filter(is.na(group_var) | stringr::str_detect(group_var,analysis_var, negate = T))
  }
  dap %>%
    dplyr::select(analysis_type, analysis_var, group_var) %>%
    dplyr::mutate(level = .95)
}

#' Helper to check the data analysis plan format
#'
#' @param dap a *data analysis plan*
#' @param .design Survey design object created with srvyr::as_survey or as_survey_design
#'
#' @return The dap with default values added if missing or an error if something is not correct.
#' @export
#'
#' @examples
#' check_dap(.design = srvyr::as_survey(analysistools_MSNA_template_data),
#'           dap = analysistools_dap)
check_dap <- function(dap, .design) {

  # check relevant columns
  if(!all(c("analysis_type", "group_var", "analysis_var") %in% names(dap))) {
    stop("Make sure you have at least analysis_type, group_var, analysis_var in your dap")
  }
  # add some default if not exists
  if(!"level" %in% names(dap)) {
    dap[["level"]] <- 0.95
    warning("No column level identified, set to 0.95 as default value.")
  }

  # specific checks on columns for ratio
  if("ratio" %in% dap[["analysis_type"]]) {
    if(!all(c("analysis_var_numerator", "analysis_var_denominator") %in% names(dap))) {
      stop("You have ratio, you need analysis_var_numerator, analysis_var_denominator columns in your dap")
    }

    # specific default for ratio
  if(!"numerator_NA_to_0" %in% names(dap)) {
    dap <- dap %>%
      dplyr::mutate(numerator_NA_to_0 = dplyr::case_when(analysis_type == "ratio" ~ TRUE))
    warning("No column level numerator_NA_to_0, set to TRUE as default value.")
  }

  if(!"filter_denominator_0" %in% names(dap)) {
    dap <- dap %>%
      dplyr::mutate(filter_denominator_0 = dplyr::case_when(analysis_type == "ratio" ~ TRUE))
    warning("No column level filter_denominator_0, set to TRUE as default value.")
    }
  }

  # check for analysis type implemented
  analysis_type_dictionary <- c("prop_select_one", "mean", "median", "ratio")
  verify_if_AinB(dap[["analysis_type"]],
                 analysis_type_dictionary,
                 "The following analysis type are not yet implemented or check for typo: ")

  # check for variables.
  verify_if_AinB(dap[["group_var"]],
                 names(.design$variables),
                 "The following group variables are not present in the dataset: ")

  # check for variables.
  verify_if_AinB(dap[["analysis_var"]],
                 names(.design$variables),
                 "The following analysis variables are not present in the dataset: ")

  if("ratio" %in% dap[["analysis_type"]]) {
    # check for variables.
    verify_if_AinB(dap[["analysis_var_numerator"]],
                   names(.design$variables),
                   "The following analysis numerator variables are not present in the dataset: ")

    # check for variables.
    verify_if_AinB(dap[["analysis_var_denominator"]],
                   names(.design$variables),
                   "The following analysis denominator variables are not present in the dataset: ")
  }
  return(dap)
}



