#' Remove rows with NA values
#'
#' This function removes all rows that contain any NA values from a data frame.
#'
#' @param data A data frame.
#' @return A cleaned data frame with rows containing NAs removed.
#' @examples
#' climate_cleaned <- remove_na_rows(climate_data)
#' @export
remove_na_rows <- function(data) {
  cleaned_data <- na.omit(data)
  return(cleaned_data)
}
