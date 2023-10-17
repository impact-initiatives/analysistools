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
      nb_analysis_var = ceiling(stringr::str_count(analysis_var, "~/~") / 2),
      nb_group_var = ceiling(stringr::str_count(group_var, "~/~") / 2)
    )

  analysis_var_split <-
    paste0(rep(
      c("analysis_var_", "analysis_var_value_"),
      max(key_table$nb_analysis_var)
    ), rep(c(1:max(
      key_table$nb_analysis_var
    )), each = 2))
  group_var_split <-
    paste0(rep(
      c("group_var_", "group_var_value_"),
      max(key_table$nb_group_var)
    ), rep(c(1:max(
      key_table$nb_group_var
    )), each = 2))

  key_table <- key_table %>%
    tidyr::separate(analysis_var,
      analysis_var_split,
      sep = " ~/~ "
    ) %>%
    tidyr::separate(group_var,
      group_var_split,
      sep = " ~/~ "
    ) %>%
    suppressWarnings()

  return(key_table)
}

#' Unite variable from the key_table
#'
#' Function will unite all analysis_var, analysis_var_value, group_var, and group_var_value from
#' an key table resulting from create_analysis_key_table.
#'
#' @param key_table a key table built with create_analysis_key_table
#'
#' @return a table with analysis_var, analysis_var_value, group_var, and group_var_value united and
#' with a ~/~ as separator
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
      sep = " ~/~ "
    ) %>%
    tidyr::unite(analysis_var_value,
      c(dplyr::starts_with("analysis_var_value_")),
      sep = " ~/~ "
    ) %>%
    tidyr::unite(group_var, c(
      dplyr::starts_with("group_var_") &
        !dplyr::contains("value")
    ), sep = " ~/~ ") %>%
    tidyr::unite(group_var_value,
      c(dplyr::starts_with("group_var_value_")),
      sep = " ~/~ "
    ) %>%
    dplyr::mutate(dplyr::across(
      c(
        analysis_var,
        analysis_var_value,
        group_var,
        group_var_value
      ),
      ~ stringr::str_remove_all(.x, "( ~/~ NA)*$")
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
