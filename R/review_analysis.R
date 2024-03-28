#' Review columns comparing it to another set of columns and spots differences
#'
#' Wrapper around addindicators::review_variables specific to review analysis.
#'
#' @param results_table Results table with an analysis key
#' @param stat_columns_to_review Vectors of columns to review (should be paired with
#' columns_to_compare_with). Default is "stat.x".
#' @param stat_columns_to_compare_with Vectors of columns to compare with (should be paired with
#' columns_to_review). Default is "stat.y"
#' @param analysis_key_column character vector with the name of the analysis key column.
#' Default is "analysis key"
#' @param prefix Prefix to be used for the review and comment column. Default is "review".
#'
#' @return A list with two objects:
#'   - the result table the review and comment columns
#'   - the review table
#'
#' @export
#'
#' @examples
#' analysis_key_column <-  c("mean @/@ income %/% NA @/@ NA %/% NA",
#'                           "prop_select_one @/@ water_source %/% tap_water @/@ district %/% district_a",
#'                           "prop_select_one @/@ water_source %/% tap_water @/@ district %/% district_a -/- population %/% displaced",
#'                           "prop_select_multiple @/@ source_information %/% relatives @/@ NA %/% NA",
#'                           "ratio @/@ food_expenses %/% NA -/- total_expenses %/% NA @/@ NA %/% NA",
#'                           "prop_select_one @/@ water_source %/% tap_water @/@ population %/% displaced",
#'                           "ratio @/@ food_expenses %/% NA -/- total_expenses %/% NA @/@ district %/% district_a",
#'                           "prop_select_one @/@ water_source %/% tap_water @/@ population %/% returnees")
#' test_analysis_results <- data.frame(
#'   test = c(
#'     "test equality",
#'     "test difference",
#'     "test Missing in y",
#'     "test Missing in x",
#'     "test equality rounding in x",
#'     "test equality rounding in y",
#'     "test difference rounding in x",
#'     "test difference rounding in y"
#'   ),
#'   stat_col.x = c(0, 1, 2, NA, 0.00019, 0.0002, 0.00035, 0.0003),
#'   upp_col.x = c(0, 1, 2, NA, 0.00019, 0.0002, 0.00035, 0.0003),
#'   stat_col.y = c(0, 2, NA, 3, 0.0002, 0.00019, 0.0003, 0.00035),
#'   upp_col.y = c(0, 2, NA, 3, 0.0002, 0.00019, 0.0003, 0.00035),
#'   analysis_key = analysis_key_column
#' )
#'
#' review_analysis(test_analysis_results,
#'                 stat_columns_to_review = "stat_col.x",
#'                 stat_columns_to_compare_with = "stat_col.y")
review_analysis <- function(results_table,
                            stat_columns_to_review = "stat.x",
                            stat_columns_to_compare_with = "stat.y",
                            analysis_key_column = "analysis_key",
                            prefix = "review") {
  results_review <- results_table %>%
    addindicators::review_variables(columns_to_review = stat_columns_to_review,
                                    columns_to_compare_with = stat_columns_to_compare_with,
                                    uuid_column = analysis_key_column)
  analysis_key_split_table <- results_review$review_table %>%
    create_analysis_key_table(analysis_key_column = analysis_key_column) %>%
    unite_variables()

  review_names <- c("analysis_type","analysis_var", "group_var")

  analysis_info_table <- analysis_key_split_table %>%
    dplyr::select(tidyselect::all_of(review_names))

  results_review$review_table <- results_review$review_table %>%
    dplyr::bind_cols(analysis_info_table)

  results_review$review_table <- results_review$review_table %>%
    dplyr::rename(stat = variable)

  names(results_review) <- c("results_table", "review_table")

  return(results_review)

}


