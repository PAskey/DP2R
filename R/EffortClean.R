#' A function to bring Effort data into R from DataPond for analysis.
#' A temporary function to clean the Effort data which has duplicates and generally useless data.
#'
#'
#' @title EffortClean
#' @name EffortClean
#' @keywords SPDT; DataPond
#'
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom data.table ":="

EffortClean <- function() {

  Edata = DP2R::DP2R(Tables = "vwEffort")$vwEffort


  #Create simple date column without time. This facilitates other cleaning steps, and is used later in data analyses
  Edata$date = as.Date(Edata$assessed_dt)#Remove time portion for some functions

  ###Cleaning
  #Step 1: Address some small scale specific issues in the data. These could probably be cleaned by hand.

  #Yellow Lake camera data has 'open' for one camera when ground counts verify covered. The data also looks suspect that one camera has been misnamed later in season (same date and hour, with different counts). April is a problem for inconsistent ice-cover and like camera mis-namming, also data recorded in boats column (and supposedly open) when clear a covered period
  #### Edata[Edata$WBID == "01202SIML"&Edata$year == 2013&Edata$month %in%c(1:3),"ice_cover_code"]<-"COVERED"

  #Remove data from assess_event_id 20448 (Kathie Lake in 2012) where ice_cover is open. This April data duplicates dates in another assess_event, but this set has no observations and erroneous ice_cover_code for time of year and num_boats is filled with 0s.

  #### Edata = Edata[!(Edata$assess_event_id == 20448&Edata$ice_cover_code == "OPEN"),]

  #Dewar and Skulow have potential double entries for ground counts in 2004, but one entry has rounded time and mostly no weather data. Can't tell if they just repeated counts 15 min later so leave in for now. During analyses I will collapse all counts down in the same hour to a mean.


  ###Reformat to data.table to increase efficiency on following cleaning steps
  Edata_dt = data.table::as.data.table(Edata)

  Edata_dt[, `:=`(
    year = lubridate::year(date),
    month = lubridate::month(date),
    hour = lubridate::hour(as.POSIXct(Edata$assessed_dt, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
  )]

  # 2. Replace NAs for weather_code and ice_cover code within each lake_hr group using data.table. Since we know the weather is likely the same within the same hour and ice cover the same within a day. This also potentially helps with removing duplicates, if these were left blank for a duplicate row.
  weather_before = sum(Edata_dt[,!is.na(weather_code)])

  Edata_dt[, weather_code := if (any(!is.na(weather_code))) {
    data.table::fifelse(
      is.na(weather_code),
      # Replace NA with the most frequent non-NA weather_code
      as.character(stats::na.omit(weather_code)[which.max(table(na.omit(weather_code)))]),
      weather_code
    )
  } else NA_character_,
  by = .(WBID,date,hour)]
  weather_after = sum(Edata_dt[,!is.na(weather_code)])

  ice_before = sum(Edata_dt[,!is.na(ice_cover_code)])
  Edata_dt[, ice_cover_code := if (any(!is.na(ice_cover_code))) {
    data.table::fifelse(
      is.na(ice_cover_code),
      # Replace NA with the most frequent non-NA ice_cover_code
      as.character(stats::na.omit(ice_cover_code)[which.max(table(na.omit(ice_cover_code)))]),
      ice_cover_code
    )
  } else NA_character_,
  by = .(WBID,date)]
  ice_after = sum(Edata_dt[,!is.na(ice_cover_code)])

#Step 3. Remove all duplicates (About 15000).
N_before_dup = nrow(Edata_dt)
  Edata_dt <- Edata_dt[
    order(WBID, method, assessed_dt, num_shore_ice, num_spv, num_boat, num_ice_tent, view_location_name, assess_event_id, is.na(comment), fishing_effort_id),
    .SD[1],  # Keep the first occurrence (non-NA comment prioritized = ~150 more comments), and order assess_event_id so consistent in which assess_event_id can be removed.
    by = .(WBID, method, assessed_dt, num_shore_ice, num_spv, num_boat, num_ice_tent, view_location_name)
  ]
N_after_dup = nrow(Edata_dt)


  #After initial filters, we need a somewhat difficult data clean. There are cases in the data set where people entered rows of data, where no data was collected. For example, a flight may have been cancelled due to poor weather, but the rest of the lakes are entered with NA values for all counts. This is a problem, because in historical data there are also cases where NAs were entered instead of a 0. The following filter attempts to remove cases, where all count variables are NA and this can somewhat safely be assumed that the NA are due to poor or impossible counting conditions as opposed to actual 0s.
N_before_clean = nrow(Edata_dt)
  # 1. Define the count observation columns
  vcols <- c("num_shore_ice", "num_spv", "num_boat", "num_ice_tent")

  # 2. Create the 'allNA' column to mark rows where all count variables are NA 'allNA' or NA and 0s 'all0'
  Edata_dt[, allNA := rowSums(is.na(.SD)) == length(vcols), .SDcols = vcols]
  Edata_dt[, all0 := rowSums(.SD,na.rm =T) == 0, .SDcols = vcols]

  # 3. Define the keywords and pattern for invalid or unreliable comments
  keywords <- c("terminated", "thunderstorm", "visibility", "not observed",
                "not able", "lightning", "unable", "dark", "night", "low light", "glare",
                "lens", "twilight", "dawn", "fog", "blurry",
                "bluury", "fuzzy", "no image")
  pattern <- paste(keywords, collapse = "|")

  # 4. Identify rows to be removed: all count variables NA and keyword found in the comment
  remove_NAs <- Edata_dt[allNA & grepl(pattern, tolower(comment))]
  remove_night <- Edata_dt[all0 & hour %in% c(0:5,22:24) & grepl(pattern, tolower(comment))|hour %in% c(0:4,23:24)]

  # Combine all data to be removed into a vector of unique fishing_effort_id
  removals <- unique(c(remove_NAs$fishing_effort_id, remove_night$fishing_effort_id))

  # Perform anti-join to remove identified rows efficiently
  Edata_dt <- Edata_dt[!fishing_effort_id %in% removals]

  N_after_clean = nrow(Edata_dt)

  #REmove columns for NA and 0s
  Edata_dt <- Edata_dt[, c("allNA", "all0") := NULL]


# Add to the environment
return(Edata_dt)
#keepers = c("Edata_dt", "conn")
#rm(list = setdiff(ls(), keepers))
gc()

}
