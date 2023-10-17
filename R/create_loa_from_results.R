#' Create a list of analysis from a results table
#'
#' @param results_table Results table with an analysis key
#' @param analysis_key_column character vector with the name of the analysis key column.
#' Default is "analysis key"
#'
#' @return a list of analysis
#' @export
#'
#' @examples
#' create_loa_from_results(analysistools_MSNA_template_with_ratio_results_table$results_table)
create_loa_from_results <- function(results_table, analysis_key_column = "analysis_key") {
  analysis_key_table <- create_analysis_key_table(results_table, analysis_key_column = analysis_key_column)

  analysis_var_columns <- names(analysis_key_table) %>% stringr::str_subset("analysis_var_(?!val)")
  group_var_columns <- names(analysis_key_table) %>% stringr::str_subset("group_var_(?!val)")

  loa_to_return <- analysis_key_table %>%
    dplyr::select(dplyr::all_of(c("analysis_type", analysis_var_columns, group_var_columns))) %>%
    dplyr::distinct() %>%
    tidyr::unite("group_var", dplyr::all_of(group_var_columns), sep = ", ", na.rm = TRUE)

  loa_to_return <- loa_to_return %>%
    dplyr::rename(analysis_var = analysis_var_1) %>%
    dplyr::mutate(
      level = .95,
      group_var = dplyr::na_if(group_var, "NA")
    )

  if (any("ratio" %in% loa_to_return[["analysis_type"]])) {
    loa_to_return <- loa_to_return %>%
      dplyr::mutate(
        analysis_var_numerator = dplyr::if_else(analysis_type == "ratio", analysis_var, NA_character_),
        analysis_var_denominator = dplyr::if_else(analysis_type == "ratio", analysis_var_2, NA_character_),
        numerator_NA_to_0 = dplyr::if_else(analysis_type == "ratio", TRUE, NA),
        filter_denominator_0 = dplyr::if_else(analysis_type == "ratio", TRUE, NA),
        analysis_var = dplyr::if_else(analysis_type == "ratio", NA_character_, analysis_var)
      ) %>%
      dplyr::select(-analysis_var_2)
    if (all(loa_to_return[["analysis_type"]] %in% "ratio")) {
      loa_to_return <- loa_to_return %>%
        dplyr::select(-analysis_var)
    }
  }

  columns_order <- c(
    "analysis_type", "analysis_var", "group_var", "level",
    "analysis_var_numerator", "analysis_var_denominator",
    "numerator_NA_to_0", "filter_denominator_0"
  )
  loa_to_return <- loa_to_return %>%
    dplyr::select(dplyr::any_of(columns_order))
  return(loa_to_return)
}
