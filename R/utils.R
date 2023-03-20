char_to_vector <- function(string) {
  string %>%
    stringr::str_split(",", simplify = T) %>%
    stringr::str_trim() %>%
    as.vector()
  }
