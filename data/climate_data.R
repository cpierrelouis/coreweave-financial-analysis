climate_data <- readr::read_csv("C:/Users/chris/OneDrive/Documents/Climate Tweets Proj/ClimateTweetStats/climate_sample.csv")

# Save it in the proper internal format
usethis::use_data(climate_data, overwrite = TRUE)
