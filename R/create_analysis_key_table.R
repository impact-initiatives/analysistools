#' Create a table from the analysis_key and results object
#'
#'  Helper function to turn a analysis_key to a table. It will return a dataframe with
#'  analysis_var_x, analysis_var_value_x, group_var_x, group_var_value_x, nb_analysis_var,
#'  nb_group_var:
#'  - Where YY_var_x are the variable names for the dependent and
#'  independent variables.
#'  - Where YY_var_value_x are the values for the dependent and independent variables.
#'  - Where x is the number of the variable by order of grouping.
#'  - nb_xx is the number of variable used in the grouping.
#'
#' @param results_table result object with an analysis key
#' @param analysis_key_column character vector with the name of the analysis key column.
#' Default is "analysis key"
#'
#' @return a dataframe with the analysis key and analysis_var_x, analysis_var_value_x, group_var_x,
#' group_var_value_x, nb_analysis_var and nb_group_var
#' @export
#'
#' @examples
#' create_analysis_key_table(analysistools_MSNA_template_with_ratio_results_table$results_table)
create_analysis_key_table <- function(results_table, analysis_key_column = "analysis_key") {
  # check for @/@ format for the different types
  verify_analysis_key(results_table[[analysis_key_column]])

  key_table <- results_table %>%
    dplyr::select(dplyr::all_of(c(analysis_key_column))) %>%
    tidyr::separate(
      analysis_key_column,
      c("analysis_type", "analysis_var", "group_var"),
      sep = " @/@ ",
      remove = FALSE
    ) %>%
    dplyr::mutate(
      nb_analysis_var = stringr::str_count(analysis_var, "-/-") + 1,
      nb_group_var = stringr::str_count(group_var, "-/-") + 1
    )
  key_table <- separate_variable(key_table, "group_var", "nb_group_var") %>%
    separate_variable("analysis_var","nb_analysis_var")

  return(key_table)
}

separate_variable <- function(key_table, column_to_separate, number_column) {
  key_table2 <- key_table %>%
    tidyr::separate_wider_delim(cols = tidyr::all_of(column_to_separate),
                                delim = " -/- ",
                                names_sep = "_",
                                too_few = "align_start")
  key_table3 <- key_table2 %>%
    tidyr::separate_wider_delim(cols = tidyr::starts_with(column_to_separate),
                                delim = " %/% ",
                                names = c(column_to_separate, paste0(column_to_separate,"_value")),
                                too_few = "align_start", names_sep = "_")

  names_to_change <- grep(paste0("^", column_to_separate,"_\\d+"), names(key_table3), value = TRUE)

  max_number <- max(key_table[[number_column]])

  names_group_var <- paste0(c(column_to_separate, paste0(column_to_separate,"_value")),"_", rep(1:max_number, each =2))
  rename_map <- stats::setNames(names_to_change,names_group_var)

  dplyr::rename(key_table3, tidyr::all_of(rename_map))


}


#' Unite variable from the key_table
#'
#' Function will unite all analysis_var, analysis_var_value, group_var, and group_var_value from
#' an key table resulting from create_analysis_key_table.
#'
#' @param key_table a key table built with create_analysis_key_table
#'
#' @return a table with analysis_var, analysis_var_value, group_var, and group_var_value united and
#' with a %/% as separator
#' @export
#'
#' @examples
#' \dontrun{
#' unite_variables(key_table)
#' }
unite_variables <- function(key_table) {
  key_table %>%
    tidyr::unite(analysis_var,
      c(
        dplyr::starts_with("analysis_var") &
          !dplyr::contains("value")
      ),
      sep = " %/% "
    ) %>%
    tidyr::unite(analysis_var_value,
      c(dplyr::starts_with("analysis_var_value_")),
      sep = " %/% "
    ) %>%
    tidyr::unite(group_var, c(
      dplyr::starts_with("group_var_") &
        !dplyr::contains("value")
    ), sep = " %/% ") %>%
    tidyr::unite(group_var_value,
      c(dplyr::starts_with("group_var_value_")),
      sep = " %/% "
    ) %>%
    dplyr::mutate(dplyr::across(
      c(
        analysis_var,
        analysis_var_value,
        group_var,
        group_var_value
      ),
      ~ stringr::str_remove_all(.x, "( %/% NA)*$")
    ))
}

#' Helper to check the format of the analysis key
#'
#' @param analysis_key vector of analysis key (not the name of the column)
#'
#' @return It will stop the function if the analysis is not in the correct format
#' @export
#'
#' @examples
#' verify_analysis_key(analysistools_MSNA_template_no_ratio_results_table$results_table$analysis_key)
#'
verify_analysis_key <- function(analysis_key) {
  if (analysis_key %>%
      stringr::str_split(" @/@ ", simplify = TRUE) %>%
      dim() %>%
      `[[`(2) != 3) {
    stop("Analysis keys does not seem to follow the correct format")
  }
}
