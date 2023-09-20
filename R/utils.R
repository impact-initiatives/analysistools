#' Turns a string separated by , into a vector
#'
#' This is a helper to avoid repeating the select in the 5 create_analysis_x functions.
#'
#' @param string a one length string
#'
#' @return a string vector
#' @export
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

  if (any(vector_to_return == "")) {
    stop("The group_var seems to have empty value, please check the inputs values")
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
      tidyr::unite("group_var_value", dplyr::all_of(grouping_vector), sep = " ~/~ ")
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
#'
#'
#' @examples
#' \dontrun{
#' adding_analysis_key(results = results)
#' }
adding_analysis_key <- function(results) {
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
#'
#'
#' @examples
#' \dontrun{
#' adding_analysis_key_ratio(results = results)
#' }
adding_analysis_key_ratio <- function(results) {
  x <- results$group_var %>% stringr::str_split(" ~/~ ")
  y <- results$group_var_value %>% stringr::str_split(" ~/~ ")
  to_add_group <-
    purrr::map2(x, y, function(x, y) {
      paste(x, y, sep = " ~/~ ")
    }) %>%
    purrr::map(stringr::str_c, collapse = " ~/~ ") %>%
    do.call(c, .)

  x <- results$analysis_var %>% stringr::str_split(" ~/~ ")
  y <- results$analysis_var_value %>% stringr::str_split(" ~/~ ")
  to_add_analysis <-
    purrr::map2(x, y, function(x, y) {
      paste(x, y, sep = " ~/~ ")
    }) %>%
    purrr::map(stringr::str_c, collapse = " ~/~ ") %>%
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
