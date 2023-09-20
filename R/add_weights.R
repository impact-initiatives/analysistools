#' Add a weight variable using the sample frame
#'
#' @param dataset the clean dataframe
#' @param sample_data sample dataframe including poplution numbers and the strata
#' @param strata_column_dataset name of strata column in the clean dataframe
#' @param strata_column_sample name of strata column in the sample dataframe
#' @param population_column name of population column in the sample dataframe
#' @param weight_column name of the added weight column. By default "weights"
#'
#' @return The clean dataset with 1 new column: weight
#' @export
#'
#' @examples
#' clean_data <- data.frame(
#'   uuid = c(1, 2, 3, 4, 5, 6, 7, 8),
#'   strata = c(
#'     "strata1", "strata2", "strata1",
#'     "strata2", "strata1", "strata2",
#'     "strata1", "strata1"
#'   )
#' )
#' sample <- data.frame(
#'   strata = c("strata1", "strata2"),
#'   population = c(30000, 50000)
#' )
#'
#' clean_data_weighted <- clean_data %>%
#'   add_weights(sample,
#'     strata_column_dataset = "strata",
#'     strata_column_sample = "strata",
#'     population_column = "population"
#'   )
add_weights <- function(dataset,
                        sample_data,
                        strata_column_dataset = NULL,
                        strata_column_sample = NULL,
                        population_column = NULL,
                        weight_column = "weights") {
  # make dataset a dataframe
  dataset <- as.data.frame(dataset)

  # If strata_column do not exist in sample_data or dataset
  if (!strata_column_sample %in% names(sample_data)) {
    stop("Cannot find the defined strata column in the provided sample frame.")
  }
  if (!strata_column_dataset %in% names(dataset)) {
    stop("Cannot find the defined strata column in the provided dataset.")
  }

  # IF all strata from dataset not in sample frame
  if (!all(dataset[[strata_column_dataset]] %in% sample_data[[strata_column_sample]])) {
    stop("Not all strata from dataset are in sample frame")
  }

  if (!all(sample_data[[strata_column_sample]] %in% dataset[[strata_column_dataset]])) {
    stop("Not all strata from sample frame are in dataset")
  }

  # If population_column do not exist in sample_data
  if (!population_column %in% names(sample_data)) {
    stop("Cannot find the defined population_column column in the provided sample frame.")
  }

  # if weight column already exist in dataset
  if (weight_column %in% names(dataset)) {
    stop("Weight column already exists in the dataset. Please input another weights column")
  }

  # Count number of entries by strata
  count <- dataset %>%
    dplyr::group_by(!!rlang::sym(strata_column_dataset)) %>%
    dplyr::summarise(count = dplyr::n())

  # Create a weight table to left_join to the dataset
  weights <- sample_data %>%
    dplyr::rename(!!strata_column_dataset := !!rlang::sym(strata_column_sample)) %>%
    dplyr::group_by(!!rlang::sym(strata_column_dataset)) %>%
    dplyr::summarise(population = sum(as.numeric(!!rlang::sym(population_column)))) %>%
    dplyr::left_join(count, by = strata_column_dataset) %>%
    dplyr::mutate(
      !!rlang::sym(weight_column) := (as.numeric(population) / sum(as.numeric(population))) / (as.numeric(count) / sum(as.numeric(count)))
    ) %>%
    dplyr::select(dplyr::all_of(strata_column_dataset), dplyr::all_of(weight_column))

  # join to dataset
  dataset <- dataset %>%
    dplyr::left_join(weights, by = strata_column_dataset)

  if (any(is.na(dataset[[weight_column]]))) {
    stop("There are NA values in the weights column")
  }

  return(dataset)
}
