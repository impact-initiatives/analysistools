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
#' verify_if_AinB(c("admin1", "admin2"),
#'                c("admin1", "pop_group", "uuid", "admin2"),
#'                "Cannot identify: ")
#' }
verify_if_AinB <- function(.A, .B, msg_error) {
  unique_A <- unique(.A)[!is.na(unique(.A))]
  unique_B <- unique(.B)[!is.na(unique(.B))]
  if(any(!unique_A %in% unique_B)) {
    A_missing <- unique_A[!unique_A %in% unique_B]
    msg <- glue::glue(msg_error, glue::glue_collapse(A_missing, ", "))
    stop(msg)
  }
}
