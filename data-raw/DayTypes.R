## code to prepare `DayTypes

# Script to generate a dataframe of dates and the assocaited DayTypes
#DayType categories (weekends, long weekends, etc.) are necessary in order to interpret the relative effort observed/expected from an instantaneous count on a given date/time

library(data.table)
library(lubridate)
library(timeDate)

DP2R::DP2R(Tables = "vwEffort")

# Get all unique years sorted
years <- sort(unique(vwEffort$assess_year))
years <- c(years, seq(max(years)+1,2045))

# Generate a sequence of dates from January 1 to December 31 for each year
# Using do.call to combine lists into a single vector
dates_list <- lapply(years, function(y) {
  # Create the start and end date as Date objects
  start_date <- as.Date(paste0(y, "-01-01"))
  end_date <- as.Date(paste0(y, "-12-31"))
  seq(from = start_date, to = end_date, by = "day")  # Create Date objects
})

# Combine the list of dates into a single Date vector
dates <- do.call(c, dates_list)

# Function to retrieve BC holidays for a given year
get_bc_holidays <- function(years) {
  holidays <- c("NewYearsDay", "CAFamilyDay", "GoodFriday", "Easter",
                "CAVictoriaDay", "CACanadaDay", "CACivicProvincialHoliday",
                "CALabourDay", "CAThanksgivingDay", "ChristmasDay", "BoxingDay")

  holiday_dates <- unique(unlist(lapply(years, function(year) {
    as.character(timeDate::holiday(year, holidays))  # Convert timeDate to character
  })))

  as.Date(holiday_dates)  # Convert to Date format
}


# Find holiday dates for years sampled (or to be sampled)
bc_holidays <- get_bc_holidays(years)  # Now returns Date objects

#Vector of holiday dates
isHoliday = dates%in% bc_holidays

#Vector of day of week corresponding to
day_of_week = wday(dates, label = TRUE) # Start weekday count on Monday

DayTypes = data.frame(date = dates, day_of_week, isHoliday)

setDT(DayTypes)# Convert to data.table for fast processing
# Check for long weekends (Friday holiday or Monday holiday)
DayTypes[, `:=` (
  #day_of_week = wday(Date),
  long_weekend = ((day_of_week == 'Fri' & (date - 1) %in% bc_holidays) |  # Holiday on Friday
                    (day_of_week == 'Sat' & (date - 2) %in% bc_holidays) |  # Saturday after Friday holiday
                    (day_of_week == 'Sun' & (date - 3) %in% bc_holidays) |  # Sunday after Friday holiday
                    (day_of_week == 'Sat' & (date + 2) %in% bc_holidays) |  # Saturday before Monday holiday
                    (day_of_week == 'Sun' & (date + 1) %in% bc_holidays) |  # Sunday before Monday holiday
                    (day_of_week == 'Mon' & date %in% bc_holidays)),        # Monday holiday
  weekend = day_of_week %in% c('Sat', 'Sun')  # Identify weekends
)]

# Assign Daytype labels
# Label weekday holidays (e.g. Canada Day) as WE
DayTypes[, daytype := fifelse(long_weekend, "LWE", fifelse(weekend|isHoliday, "WE", "WD"))]

usethis::use_data(DayTypes, overwrite = TRUE)
