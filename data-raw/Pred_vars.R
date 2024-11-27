### Create a Prediction Table for Months and Day Types (May 1st to Oct 31st)
library(dplyr)
Pred_vars <- DP2R::DayTypes %>%
  dplyr::mutate(
    year = lubridate::year(date),  # Extract the year
    month = lubridate::month(date)  # Extract the month
  ) %>%
  #dplyr::filter(month %in% c(5:10)) %>%  # COUld filter to specific moneths, but in order to keep general for ice-data removed
  dplyr::group_by(year, month, daytype) %>%  # Group by fixed effect predictors
  dplyr::summarize(
    ndays = dplyr::n_distinct(date),  # Unique days in each group
    hour = "12",  # Set reference hour
    weather_code = "UNK",  # Default weather code
    .groups = "drop"  # Avoids unnecessary ungrouping
  ) %>%
  droplevels()



usethis::use_data(Pred_vars, overwrite = TRUE)
