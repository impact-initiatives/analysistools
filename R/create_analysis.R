#' Create analysis from a design
#'
#' If not *list of analysis (loa)* is provided, the analysis will run on the overall dataset, and all
#' the grouping variables set.
#'
#' The *loa* should contains the following columns :
#' - analysis_type: analysis type to be perform. At the moment mean, median, prop_select_one, and
#' ratio are available.
#' - analysis_var: analysis variable to be used as string.
#' - group_var: The grouping variable as string. NA if there is no grouping variable. If a combination
#' of grouping variable should be used together it should be 1 string character separated with a ",".
#' i.e. c("admin1", "admin2") and "admin1, admin2" are different.
#'    - c("admin1", "admin2") : will perform the analysis grouping once by admin1, and once by admin2
#'    - "admin1, admin2" : will perform the analysis grouping once by admin1 and admin2
#' - level: confidence level to be used. If the column does not exists, .95 will be used.
#' It can also include a column **level**, if not provided .95 will be set as default.
#'
#' If ratios have to be performed, the *loa* should include the following columns as well:
#' - analysis_var_numerator analysis_var_denominator numerator_NA_to_0 filter_denominator_0
#'
#' @param design Survey design object created with srvyr::as_survey or as_survey_design
#' @param loa list of analysis: Default is NULL. If provided it will be used to create the analysis.
#' @param group_var Default is NULL. If provided, it will first create a list of analysis and then
#' will run the analysis. It should be a vector.
#' @param sm_separator Separator for choice multiple questions. The default is "."
#'
#' @return A list with 3 items:
#'  - The results table in a long format with the analysis key
#'  - The dataset that was used
#'  - The list of analysis that was used
#' @export
#'
#' @examples
#' create_analysis(
#'   design = srvyr::as_survey(analysistools_MSNA_template_data),
#'   loa = analysistools_MSNA_template_loa,
#'   sm_separator = "/"
#' )
#' create_analysis(
#'   design = srvyr::as_survey(analysistools_MSNA_template_data),
#'   loa = analysistools_MSNA_template_loa_with_ratio,
#'   sm_separator = "/"
#' )
#' shorter_df <- analysistools_MSNA_template_data[, c(
#'   "admin1",
#'   "admin2",
#'   "expenditure_debt",
#'   "wash_drinkingwatersource"
#' )]
#' create_analysis(
#'   design = srvyr::as_survey(shorter_df),
#'   group_var = "admin1"
#' )
#' create_analysis(
#'   design = srvyr::as_survey(shorter_df),
#'   group_var = "admin1, admin2"
#' )
#' create_analysis(
#'   design = srvyr::as_survey(shorter_df),
#'   group_var = c("admin1", "admin2")
#' )
create_analysis <- function(design,
                            loa = NULL,
                            group_var = NULL,
                            sm_separator = ".") {
  if (!"tbl_svy" %in% attributes(design)$class) {
    stop("It seems object design is not a design, did you use srvyr::as_survey ?")
  }

  if (!is.null(loa) & !is.null(group_var)) {
    warning("You have provided a list of analysis and group variable, group variable will be ignored")
  }

  if (!is.null(loa)) {
    loa <- check_loa(loa, design)
  }

  if (is.null(loa)) {
    loa <- create_loa(design = design, group_var = group_var, sm_separator = sm_separator)
  }


  results_list <- loa %>%
    split(1:nrow(.)) %>%
    purrr::map(function(loa) {
      if (loa[["analysis_type"]] == "mean") {
        return(create_analysis_mean(design,
          group_var = loa[["group_var"]],
          analysis_var = loa[["analysis_var"]],
          level = loa[["level"]]
        ))
      }
      if (loa[["analysis_type"]] == "median") {
        return(create_analysis_median(design,
          group_var = loa[["group_var"]],
          analysis_var = loa[["analysis_var"]],
          level = loa[["level"]]
        ))
      }
      if (loa[["analysis_type"]] == "prop_select_one") {
        return(create_analysis_prop_select_one(design,
          group_var = loa[["group_var"]],
          analysis_var = loa[["analysis_var"]],
          level = loa[["level"]]
        ))
      }
      if (loa[["analysis_type"]] == "prop_select_multiple") {
        return(create_analysis_prop_select_multiple(design,
          group_var = loa[["group_var"]],
          analysis_var = loa[["analysis_var"]],
          level = loa[["level"]],
          sm_separator = sm_separator
        ))
      }
      if (loa[["analysis_type"]] == "ratio") {
        return(create_analysis_ratio(design,
          group_var = loa[["group_var"]],
          analysis_var_numerator = loa[["analysis_var_numerator"]],
          analysis_var_denominator = loa[["analysis_var_denominator"]],
          numerator_NA_to_0 = loa[["numerator_NA_to_0"]],
          filter_denominator_0 = loa[["filter_denominator_0"]],
          level = loa[["level"]]
        ))
      }
    }, .progress = TRUE)


  results_table <- results_list %>%
    do.call(rbind, .)

  return(list(
    results_table = results_table,
    dataset = design$variables,
    loa = loa
  ))
}

#' Create a list of analysis from design and a grouping variable
#'
#' Helper for the create_analysis. It will take all the columns that are character, logical, double
#' or integers.
#'
#' @param design Survey design object created with srvyr::as_survey or as_survey_design
#' @param group_var The grouping variable as string. If a combination of grouping variable should be
#'  used together it should be 1 string character separated with a ",". i.e. c("admin1", "population")
#'  and "admin1, population" are different:
#'    - c("admin1", "population") : will perform the analysis grouping once by admin1, and once by population
#'    - "admin1, population" : will perform the analysis grouping once by admin1 and admin 2
#' @param sm_separator Separator for choice multiple questions. The default is "."
#'
#' @return a list of analysis.
#' @export
#'
#' @examples
#' shorter_df <- analysistools_MSNA_template_data[, c(
#'   "admin1",
#'   "expenditure_debt",
#'   "wash_drinkingwatersource"
#' )]
#'
#' create_loa(
#'   design = srvyr::as_survey(shorter_df),
#'   group_var = "admin1"
#' )
create_loa <- function(design,
                       group_var = NULL,
                       sm_separator = ".") {
  loa_dictionary <- data.frame(
    type = c("character", "double", "double", "logical", "integer", "integer"),
    analysis_type = c("prop_select_one", "mean", "median", "prop_select_multiple", "mean", "median")
  )
  cols_to_remove <- c("start", "end", "today", "uuid")

  select_multiple_parents_columns <- design %>% cleaningtools::auto_detect_sm_parents(sm_separator = sm_separator)
  select_multiple_children_columns <- design %>%
    cleaningtools::auto_sm_parent_children(sm_separator = sm_separator) %>%
    dplyr::pull(sm_child)

  loa <- design$variables %>%
    sapply(typeof) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("analysis_var") %>%
    dplyr::rename(., type = `.`) %>%
    dplyr::left_join(loa_dictionary, relationship = "many-to-many") %>%
    dplyr::filter(!is.na(analysis_type)) %>%
    dplyr::filter(stringr::str_detect(analysis_var, "^(X_|_)", negate = TRUE)) %>%
    dplyr::filter(!analysis_var %in% cols_to_remove) %>%
    dplyr::filter(!analysis_var %in% select_multiple_children_columns) %>%
    dplyr::mutate(analysis_type = dplyr::case_when(
      analysis_var %in% select_multiple_parents_columns ~ "prop_select_multiple",
      TRUE ~ analysis_type
    ))

  if (is.null(group_var)) {
    loa$group_var <- NA_character_
  } else {
    loa <- lapply(c(NA, group_var), function(x) {
      loa$group_var <- x
      return(loa)
    }) %>%
      do.call(rbind, .)
    loa <- loa %>%
      dplyr::filter(is.na(group_var) | stringr::str_detect(group_var, analysis_var, negate = TRUE))
  }
  loa %>%
    dplyr::select(analysis_type, analysis_var, group_var) %>%
    dplyr::mutate(level = .95)
}

#' Helper to check the list of analysis format
#'
#' @param loa a *list of analysis*
#' @param design Survey design object created with srvyr::as_survey or as_survey_design
#'
#' @return The loa with default values added if missing or an error if something is not correct.
#' @export
#'
#' @examples
#' check_loa(
#'   design = srvyr::as_survey(analysistools_MSNA_template_data),
#'   loa = analysistools_MSNA_template_loa
#' )
check_loa <- function(loa, design) {
  analysis_type_dictionary <- c("prop_select_one", "prop_select_multiple", "mean", "median", "ratio")

  # check if analysis type exists
  if (!"analysis_type" %in% names(loa)) {
    stop("Make sure to have analysis_type in your loa")
  }

  # specific checks on columns for non-ratios
  if (any(analysis_type_dictionary[analysis_type_dictionary != "ratio"] %in% loa[["analysis_type"]])) {
    if (!all(c("group_var", "analysis_var") %in% names(loa))) {
      stop("Make sure to have group_var, analysis_var in your loa")
    }
  }

  # add some default if not exists
  if (!"level" %in% names(loa)) {
    loa[["level"]] <- 0.95
    warning("No column level identified, set to 0.95 as default value.")
  }

  # specific checks on columns for ratio
  if ("ratio" %in% loa[["analysis_type"]]) {
    if (!all(c("analysis_var_numerator", "analysis_var_denominator") %in% names(loa))) {
      stop("You have ratio, you need analysis_var_numerator, analysis_var_denominator columns in your loa")
    }

    # specific default for ratio
    if (!"numerator_NA_to_0" %in% names(loa)) {
      loa <- loa %>%
        dplyr::mutate(numerator_NA_to_0 = dplyr::case_when(analysis_type == "ratio" ~ TRUE))
      warning("No column level numerator_NA_to_0, set to TRUE as default value.")
    }

    if (!"filter_denominator_0" %in% names(loa)) {
      loa <- loa %>%
        dplyr::mutate(filter_denominator_0 = dplyr::case_when(analysis_type == "ratio" ~ TRUE))
      warning("No column level filter_denominator_0, set to TRUE as default value.")
    }
  }

  # check for analysis type implemented
  verify_if_AinB(
    loa[["analysis_type"]],
    analysis_type_dictionary,
    "The following analysis type are not yet implemented or check for typo: "
  )

  # check for variables.
  verify_if_AinB(
    loa[["group_var"]][!is.na(loa[["group_var"]])] %>%
      paste(collapse = ",") %>%
      char_to_vector(),
    names(design$variables),
    "The following group variables are not present in the dataset: "
  )

  # check for variables.
  verify_if_AinB(
    loa[["analysis_var"]],
    names(design$variables),
    "The following analysis variables are not present in the dataset: "
  )

  if ("ratio" %in% loa[["analysis_type"]]) {
    # check for variables.
    verify_if_AinB(
      loa[["analysis_var_numerator"]],
      names(design$variables),
      "The following analysis numerator variables are not present in the dataset: "
    )

    # check for variables.
    verify_if_AinB(
      loa[["analysis_var_denominator"]],
      names(design$variables),
      "The following analysis denominator variables are not present in the dataset: "
    )
  }
  return(loa)
}
