#' Turns a string separated by , into a vector
#'
#' This is a helper to avoid repeating the select in the 5 create_analysis_x functions.
#'
#' @param string a one length string
#'
#' @return a string vector
#' @export
#' @keywords internal
#'
#' @examples
#' char_to_vector("groupa, groupb, groupc")
char_to_vector <- function(string) {
  if (length(string) > 1) {
    stop("The group_var to be turned into a vector is already a vector.")
  }

  vector_to_return <- string %>%
    stringr::str_split(",", simplify = T) %>%
    stringr::str_trim() %>%
    as.vector()

  if ((any(vector_to_return == "") & length(vector_to_return) > 1) ) {
    stop("The group_var seems to have empty value, please check the inputs values")
  }

  if ((any(vector_to_return == "") & length(vector_to_return) == 1) ) {
    return(NA_character_)
  }
  return(vector_to_return)
}

#' Adds the group_var_value to the results table from a create_analysis_x
#'
#' This is a helper to avoid repeating the select in the 5 create_analysis_x functions.
#'
#' @param results a results table from the calculate section from create_analysis_x
#' @param group_var group_var from the create_analysis_x
#' @param grouping_vector vector with the variables used in the check the
#' grouping variable section of a create_analysis_x function
#'
#' @return a dataframe with the results and the group_var_value column added
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' adding_group_var_value(results = results, group_var = group_var, grouping_vector = grouping_c)
#' adding_group_var_value(results = results, group_var = group_var, grouping_vector = across_by)
#' }
adding_group_var_value <- function(results, group_var = group_var, grouping_vector) {
  if (is.na(group_var)) {
    results <- results %>%
      dplyr::mutate(group_var_value = NA_character_)
  } else {
    results <- results %>%
      tidyr::unite("group_var_value", dplyr::all_of(grouping_vector), sep = " %/% ")
  }
  return(results)
}

#' Adds the analysis key to the results table from a create_analysis_x
#'
#' This is a helper to avoid repeating the select in the 5 create_analysis_x functions.
#'
#' @param results a results table from the adding group_var_value section from create_analysis_x
#'
#' @return results with key
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' adding_analysis_key(results = results)
#' }
adding_analysis_key <- function(results) {
  x <- results$group_var %>% stringr::str_split(" %/% ")
  y <- results$group_var_value %>% stringr::str_split(" %/% ")
  to_add <-
    purrr::map2(x, y, function(x, y) {
      paste(x, y, sep = " %/% ")
    }) %>%
    purrr::map(stringr::str_c, collapse = " -/- ") %>%
    do.call(c, .)

  results %>%
    dplyr::mutate(
      analysis_key = paste0(
        analysis_type,
        " @/@ ",
        analysis_var,
        " %/% ",
        analysis_var_value,
        " @/@"
      ),
      analysis_key = paste(analysis_key, to_add)
    )
}

#' Adds the analysis key to the results table from a create_analysis_ratio
#'
#' This is a helper to avoid repeating the select in create_analysis_ratio functions. It differs
#' as there are 2 analysis variables, the numerator and denominator for the ratios.
#'
#' @param results a results table from the adding group_var_value section from create_analysis_x
#'
#' @return results with key
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' adding_analysis_key_ratio(results = results)
#' }
adding_analysis_key_ratio <- function(results) {
  x <- results$group_var %>% stringr::str_split(" %/% ")
  y <- results$group_var_value %>% stringr::str_split(" %/% ")
  to_add_group <-
    purrr::map2(x, y, function(x, y) {
      paste(x, y, sep = " %/% ")
    }) %>%
    purrr::map(stringr::str_c, collapse = " -/- ") %>%
    do.call(c, .)

  x <- results$analysis_var %>% stringr::str_split(" %/% ")
  y <- results$analysis_var_value %>% stringr::str_split(" %/% ")
  to_add_analysis <-
    purrr::map2(x, y, function(x, y) {
      paste(x, y, sep = " %/% ")
    }) %>%
    purrr::map(stringr::str_c, collapse = " -/- ") %>%
    do.call(c, .)

  results %>%
    dplyr::mutate(
      analysis_key = paste(analysis_type, "@/@", to_add_analysis, "@/@", to_add_group)
    )
}

#' Select columns in a specific order
#'
#' This is a helper to avoid repeating the select in the 5 create_analysis_x functions.
#'
#' @param results a results table from the adding analysis key section from create_analysis_x
#'
#' @return dataframe with the columns in a spefic order
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' arranging_results_columns(results)
#' }
arranging_results_columns <- function(results) {
  results %>%
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
#' Checks if all values of a vector is in another vector
#'
#' @param .A Vector to check
#' @param .B Reference vector
#' @param msg_error Message error to be shown if value are missing
#'
#' @return an error if one is missing
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' verify_if_AinB(
#'   c("admin1", "admin2"),
#'   c("admin1", "pop_group", "uuid", "admin2"),
#'   "Cannot identify: "
#' )
#' }
verify_if_AinB <- function(.A, .B, msg_error) {
  unique_A <- unique(.A)[!is.na(unique(.A))]
  unique_B <- unique(.B)[!is.na(unique(.B))]
  if (any(!unique_A %in% unique_B)) {
    A_missing <- unique_A[!unique_A %in% unique_B]
    msg <- glue::glue(msg_error, glue::glue_collapse(A_missing, ", "))
    stop(msg)
  }
}

#' Helper to reformat the group_var for the results table
#'
#' @param group_var one string with each variable separated by comma, e.g. "groupa, groupb"
#' to group for groupa and groupb
#'
#' @return group_var separated by " %/% " instead of a ","
#' @export
#' @keywords internal
#'
#' @examples
#' create_group_var("groupa, groupb")
#' create_group_var("groupa,groupb")
#' create_group_var(NA)
create_group_var <- function(group_var) {
  group_var %>%
    stringr::str_replace_all(",", " %/% ") %>%
    stringr::str_squish()
}

#' Helper to correct values to NaN when totals are 0's
#'
#' @param results Results to corrected
#' @param stat_columns Columns to be corrected
#' @param total_column Total column to be used
#'
#' @return The results table with the stats_columns turn to NaN if total column is 0.
#' @export
#' @keywords internal
#'
#' @examples
#' test_table <- data.frame(
#'   stat = c(NaN, 0, 1),
#'   stat_low = c(NaN, 0, 1),
#'   stat_upp = c(NaN, 0, 1),
#'   n = c(0, 0, 1),
#'   n_total = c(0, 0, 1),
#'   n_w = c(0, 0, 1),
#'   n_w_total = c(0, 0, 1)
#' )
#'
#' correct_nan_total_is_0(test_table)
correct_nan_total_is_0 <- function(results,
                                   stat_columns = c("stat", "stat_upp", "stat_low", "n_w_total", "n_total", "n_w"),
                                   total_column = "n_total") {
  if (!all(c(stat_columns, total_column) %in% names(results))) {
    stop("Cannot identify one column.")
  }
  results %>%
    dplyr::mutate(dplyr::across(
      tidyr::all_of(stat_columns),
      ~ dplyr::case_when(
        !!dplyr::sym(total_column) == 0 ~ NaN,
        TRUE ~ .x
      )
    ))
}

#' Helper to correct values to NaN when totals are 0's
#'
#' @param results Results to corrected
#' @param analysis_var_value_column Analysis variable value to be checked.
#' @param stat_columns Columns to be corrected
#'
#' @return The results table with the stats_columns turn to NaN if analysis_var_value_column is
#' missing.
#' @export
#' @keywords internal
#'
#' @examples
#' test_table <- data.frame(
#'   stat = c(NaN, 0, 1),
#'   stat_low = c(NaN, 0, 1),
#'   stat_upp = c(NaN, 0, 1),
#'   n = c(5, 5, 1),
#'   n_total = c(0, 0, 1),
#'   n_w = c(0, 0, 1),
#'   n_w_total = c(0, 0, 1),
#'   analysis_var_value = c(NA, NA, "hello")
#' )
#'
#' correct_nan_analysis_var_variable_is_na(test_table)
correct_nan_analysis_var_variable_is_na <- function(results,
                                                    stat_columns = c("stat", "stat_upp", "stat_low", "n_w_total", "n_total", "n_w"),
                                                    analysis_var_value_column = "analysis_var_value") {
  if (!all(c(stat_columns, analysis_var_value_column) %in% names(results))) {
    stop("Cannot identify one column.")
  }
  results %>%
    dplyr::mutate(dplyr::across(
      tidyr::all_of(stat_columns),
      ~ dplyr::case_when(
        is.na(!!dplyr::sym(analysis_var_value_column)) | dplyr::sym(analysis_var_value_column) == "NA" ~ NaN,
        TRUE ~ .x
      )
    ))
}
