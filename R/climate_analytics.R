#' Detect extreme temperature anomaly tweets
#'
#' Returns tweets in the top N% of temperature anomalies.
#'
#' @param data A data frame.
#' @param temp_col Name of the temperature anomaly column (as a string).
#' @param threshold A percentile cutoff (e.g., 0.95 for top 5%).
#' @return A data frame of extreme anomaly rows.
#' @examples
#' detect_extreme_anomalies(climate_data, "temperature_avg", threshold = 0.95)
#' @export
detect_extreme_anomalies <- function(data, temp_col, threshold = 0.95) {
  cutoff <- quantile(data[[temp_col]], probs = threshold, na.rm = TRUE)
  dplyr::filter(data, .data[[temp_col]] >= cutoff)
}




###########################################################

#' Classify sentiment trend over time
#'
#' Uses a linear model to classify the overall trend of sentiment over time.
#'
#' @param data A data frame.
#' @param date_col Name of the date column (as a string).
#' @param sentiment_col Name of the sentiment column (as a string).
#' @return A character string: "increasing", "decreasing", or "no clear trend".
#' @examples
#' classify_sentiment_trend(climate_data, "created_at", "sentiment")
#' @export
classify_sentiment_trend <- function(data, date_col, sentiment_col) {
  data[[date_col]] <- as.Date(data[[date_col]])
  lm_model <- lm(data[[sentiment_col]] ~ as.numeric(data[[date_col]]), data = data)
  slope <- coef(lm_model)[2]

  if (slope > 0.0005) {
    return("increasing")
  } else if (slope < -0.0005) {
    return("decreasing")
  } else {
    return("no clear trend")
  }
}




##############################################################

#' Find coordinates with most negative average sentiment
#'
#' Returns the latitude and longitude of the location with the lowest average sentiment score.
#'
#' @param data A data frame.
#' @param lat_col Name of the latitude column (as a string).
#' @param lon_col Name of the longitude column (as a string).
#' @param sentiment_col Name of the sentiment column (as a string).
#' @return A data frame with latitude and longitude of the most negative sentiment location.
#' @examples
#' region_with_negative_sentiment(climate_data, "lat", "lng", "sentiment")
#' @export
region_with_negative_sentiment <- function(data, lat_col, lon_col, sentiment_col) {
  summary_df <- dplyr::group_by(data, .data[[lat_col]], .data[[lon_col]]) %>%
    dplyr::summarise(avg_sentiment = mean(.data[[sentiment_col]], na.rm = TRUE)) %>%
    dplyr::arrange(avg_sentiment)

  result <- summary_df[1, c(lat_col, lon_col)]
  return(result)
}

