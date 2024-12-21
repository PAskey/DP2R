#' A function to bring Effort data into R from DataPond for analysis.
#' It is important that you establish a personal password protected connection (and assign as 'conn' object before running this function. Use example code below and/or you may have it set up under Connections tab in RStudio after running it once.See: https://github.com/r-dbi/odbc/blob/main/README.md
#' Function only usable by FFSBC staff who have a direct or vpn connection to DataPond
#'
#'
#' @title Effort2R
#' @name Effort2R
#' @keywords SPDT; DataPond
#' @export
#'
#' @examples
#'   conn <- DBI::dbConnect(drv = odbc::odbc(),
#'   Driver = 'SQL Server',
#'   server = 'tcp:gofishbc.database.windows.net,1433',
#'   database = 'DataPond',
#'   uid = uid,
#'   pwd = pwd)
#'
#' DP2R::Effort2R()
#'
#' @importFrom magrittr "%>%"
#' @importFrom data.table ":="


Effort2R <- function() {


  if(!exists("conn")|!DBI::dbIsValid(conn)){stop("First you must establish a connection to DataPond or DataPond_STAGE in R and assign to 'conn' object. Use example code with your personal uid and pwd. Note this often fails on first attempt (some sort of timeout lag with Azure, but then works when you re-run the code
  conn <- DBI::dbConnect(drv = odbc::odbc(),
                         Driver = 'SQL Server'',
                         server = 'tcp:gofishbc.database.windows.net,1433',
                         database = 'DataPond',
                         uid = uid,
                         pwd = pwd")}

  #In future bring in vwEffort as well
  #DP2R::DP2R(Tables = c("vwWaterbodyLake"))

  # Currently Edata_dt is already a data.table object from EffortCLean() below
  #In future, once cleaning section can be removed, but add following:

  #Create simple date column without time.
  # vwEffort$date = as.Date(vwEffort$assessed_dt)#Remove time portion for some functions

  ###Reformat to data.table to increase efficiency on following steps
  # Edata_dt = data.table::as.data.table(vwEffort)

  #Edata_dt[, `:=`(
  #  month = lubridate::month(date),
  #  hour = lubridate::hour(as.POSIXct(Edata$assessed_dt, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
  #)]

  Edata_dt = DP2R::EffortClean()


  # Ensure DayTypes is a data.table object
  DayTypes <- data.table::as.data.table(DP2R::DayTypes)

  # Perform a left join with DayTypes on the 'date' column
  Edata_dt <- DayTypes[, .(date, daytype)][Edata_dt, on = "date"]

  # Filter to typical daylight hours, good visibility, and remove observations when lake in transition between frozen and open.
  Edata_dt <- Edata_dt[
    hour %in% 6:20 &
      (is.na(percent_visibility) | percent_visibility > 80) &
      !ice_cover_code %in% "PARTIAL"
  ]

  # Create 'lakeview_yr' group identifier using data.table grouping
  Edata_dt[, lakeview_yr := .GRP,
           by = .(WBID, method, view_location_name, year)]

  # Create 'lake_hr' group identifier
  Edata_dt[, lake_hr := .GRP,
           by = .(WBID, date, hour)]

  # Calculate 'OE' (total observed effort) as the sum across all different angling count categories
  Edata_dt[, OE := rowSums(.SD, na.rm = TRUE),
           .SDcols = num_shore_ice:num_ice_tent]

  # Define the minimum number of distinct sample days required to include in effort estimates
  min_days <- 12

  # Filter groups where the number of distinct 'date' values is at least 'min_days'
  Edata_dt <- Edata_dt[
    , if (data.table::uniqueN(date) >= min_days) .SD,
    by = lakeview_yr
  ]

  # Update weather_code column in-place, replacing NA values with "UNK"
  Edata_dt[, weather_code := data.table::fifelse(is.na(weather_code), "UNK", weather_code)]

  ###THis section reduces multi-counts per hour to the first count. A few cases have numerous counts from the same hour, which  would heavily weight that hour in the model fitting.

  # Define columns to group by
  group_cols <- c("region", "WBID", "gazetted_name", "method", "view_location_name", "date", "year", "month", "hour")

  # Define the desired column order after grouping columns
  desired_order <- c(
    group_cols,
    "ice_cover_code", "assessed_dt", "weather_code", "num_shore_ice", "num_spv",
    "num_boat", "num_ice_tent", "percent_visibility", "percent_lake_seen",
    "comment", "assess_event_id", "assess_event_name", "fishing_effort_id"
  )

  # Order the data and select the first row for each within hour group
  Edata_dt <- Edata_dt[order(region, WBID, gazetted_name, method, view_location_name, date, assessed_dt, year, month, hour)
  ][, .SD[1], by = group_cols]

  # Reorder the columns
  Edata_dt <- data.table::setcolorder(Edata_dt, desired_order)

  ##Add in camera expansion factor lakeview_yrs
  Edata_dt <- Cam_xdata_dt(Edata_dt)


  # Convert back to data.frame if necessary
  Edata <- as.data.frame(Edata_dt)
  #Lakes <- vwWaterbodyLake[vwWaterbodyLake$WBID %in% Edata_dt$WBID,]



  ###################################################################################
  #Final steps as data frame to create factors for analyses and separate ice fishing and open water data.
  #Potentially not frozen in Winter lakes: OKanangan, Kalamalka, etc.
  Nofreeze = c("00078OKAN", "00209OKAN", "01530OKAN", "101367OKAN", "01699OKAN", "00146BULL", "00672SHUL", "00418OKAN")

  #Create table for ice fishing and open water separately
  #OE (observed effort) column is the total effort counted at one time across all modes: ice, tents.
  Icedata = Edata%>%dplyr::filter(ice_cover_code %in% c("COVERED")&month%in%c(1:4,11:12)|#Consider only these months in ice period#Covered is confirmation frozen
                                  (is.na(ice_cover_code)&!region%in%c(1,2)&!WBID%in%Nofreeze&month%in%c(1:3,12))
                                  )%>%droplevels()
  #For shoulder months there must be confirmation the lake is actually iced over and no boats counted

#Separate open water and ice fish based on declared ice-cover codes and months
  Edata = suppressMessages(dplyr::anti_join(Edata, Icedata, by = NULL)%>%dplyr::filter(month%in%c(4:10)))

  #Put fac.data at very end to get rid of unused factor levels (months are different between ice and open water)
  #Convert categorical and character variables to factors for statistical modelling

  Edata = Edata%>%DP2R::fac.data(.,varlist = list("year" = NULL, "month" = "5", "hour" = "12", "daytype" = "WE", "weather_code" = "UNK", "lakeview_yr" = NULL, "lake_hr" = NULL))%>%droplevels()

  #Narrow up hours of day during ice-fishing
  Icedata = DP2R::fac.data(Icedata[Icedata$hour%in%c(7:18),], varlist = list("year" = NULL, "month" = "1", "hour" = "12", "daytype" = "WE", "weather_code" = "UNK", "lakeview_yr" = NULL, "lake_hr" = NULL))%>%droplevels()


  Edata <<- Edata
  Icedata <<- Icedata

  keepers = c("Edata","Icedata")
  rm(list = setdiff(ls(), keepers))
  gc()


}
