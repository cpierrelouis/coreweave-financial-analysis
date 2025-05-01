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


####################################


#' Summarize numeric columns in a dataset
#'
#' This function returns summary statistics (mean, median, min, max, and standard deviation)
#' for all numeric columns in a data frame.
#'
#' @param data A data frame.
#' @return A data frame with summary statistics for each numeric column.
#' @examples
#' summarize_data(climate_data)
#' @export
summarize_data <- function(data) {
  numeric_data <- dplyr::select_if(data, is.numeric)

  summary_df <- data.frame(
    Variable = names(numeric_data),
    Mean = round(sapply(numeric_data, mean, na.rm = TRUE), 2),
    Median = round(sapply(numeric_data, median, na.rm = TRUE), 2),
    Min = round(sapply(numeric_data, min, na.rm = TRUE), 2),
    Max = round(sapply(numeric_data, max, na.rm = TRUE), 2),
    SD = round(sapply(numeric_data, sd, na.rm = TRUE), 2)
  )

  return(summary_df)
}


#######################################################################################


#' Plot yearly average of a numeric column
#'
#' This function creates a line plot of the yearly average of a numeric column using ggplot2.
#'
#' @param data A data frame.
#' @param date_col Name of the column containing dates (as a string).
#' @param value_col Name of the numeric column to average and plot (as a string).
#' @return A ggplot2 line plot.
#' @examples
#' plot_column_by_year(climate_data, "created_at", "sentiment")
#' @export
plot_column_by_year <- function(data, date_col, value_col) {
  data[[date_col]] <- as.Date(data[[date_col]])
  data$year <- format(data[[date_col]], "%Y")

  yearly_avg <- dplyr::group_by(data, year) %>%
    dplyr::summarise(avg_value = mean(.data[[value_col]], na.rm = TRUE))

  yearly_avg$year <- as.numeric(yearly_avg$year)

  ggplot2::ggplot(yearly_avg, ggplot2::aes(x = year, y = avg_value)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_point(color = "darkblue") +
    ggplot2::labs(
      title = paste("Yearly Average of", value_col),
      x = "Year",
      y = paste("Average", value_col)
    ) +
    ggplot2::theme_minimal()
}

plot_column_by_year(climate_data, "created_at", "sentiment")
