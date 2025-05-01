#' Plot sentiment change by temperature anomaly
#'
#' This function bins temperature anomalies and calculates the average sentiment
#' within each bin to visualize how public sentiment shifts with climate extremes.
#'
#' @param data A data frame containing temperature anomaly and sentiment columns.
#' @param temp_col Name of the temperature anomaly column (as a string).
#' @param sentiment_col Name of the sentiment column (as a string).
#'
#' @return A ggplot2 bar plot showing average sentiment by temperature anomaly bins.
#' @examples
#' temp_vs_sentiment(climate_data, "temperature_avg", "sentiment")
#' @export
temp_vs_sentiment <- function(data, temp_col, sentiment_col) {
  data$bin <- cut(
    data[[temp_col]],
    breaks = c(-Inf, -1, 0, 1, Inf),
    labels = c("< -1", "-1 to 0", "0 to 1", "> 1")
  )

  summary_df <- dplyr::group_by(data, bin) %>%
    dplyr::summarise(
      avg_sentiment = mean(.data[[sentiment_col]], na.rm = TRUE),
      count = dplyr::n()
    )

  ggplot2::ggplot(summary_df, ggplot2::aes(x = bin, y = avg_sentiment, fill = bin)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::labs(
      title = "Sentiment by Temperature Anomaly Bin",
      x = "Temperature Anomaly (°C)",
      y = "Average Sentiment"
    ) +
    ggplot2::theme_minimal()
}


