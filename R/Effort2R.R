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
  DP2R::DP2R(Tables = c("vwWaterbodyLake"))

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

  DP2R::EffortClean()



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


  #Use vwWAterbody lake to add region and surface area
 # vwWaterbodyLake_dt <- data.table::as.data.table(vwWaterbodyLake[])

  #Edata_dt <- merge(vwWaterbodyLake_dt[,c("region","WBID","gazetted_name","area_ha")], Edata_dt, by = c("WBID","gazetted_name"), all.y = TRUE)

  #Filter relevant lakes (only those present in Edata) using data.table
  #Lakes <- vwWaterbodyLake_dt[WBID %in% Edata_dt$WBID]

  # Convert back to data.frame if necessary
  Edata <- as.data.frame(Edata_dt)
  Lakes <- vwWaterbodyLake[vwWaterbodyLake$WBID %in% Edata_dt$WBID,]

  ###################################################################################
  #Final steps as data frame to create factors for analyses and separate ice fishing and open water data.

  #Create table for ice fishing and open water separately
  #Assume any ice counts in May to October are errors.
  #OE (observed effort) column is the total effort counted at one time across all modes: ice, tents.
  Icedata = Edata%>%dplyr::filter((month %in% c(1:3,12)&!ice_cover_code %in% c("OPEN","PARTIAL"))|
                    ice_cover_code %in% c("COVERED"),
                  is.na(num_boat)|num_boat == 0)%>%droplevels()
  #For shoulder months there must be confirmation the lake is actually iced over and no boats counted


  Edata = dplyr::anti_join(Edata, Icedata, by = NULL)%>%dplyr::filter(month%in%c(4:10))

  #Put fac.data at very end to get rid of unused factor levels (months are different between ice and open water)
  #Convert categorical and character variables to factors for statistical modelling

  Edata = Edata%>%DP2R::fac.data(.,varlist = list("year" = NULL, "month" = "5", "hour" = "12", "daytype" = "WE", "weather_code" = "UNK", "lakeview_yr" = NULL, "lake_hr" = NULL))%>%droplevels()

  Edata$month = droplevels(Edata$month)

  Icedata = DP2R::fac.data(Icedata, varlist = list("year" = NULL, "month" = "1", "hour" = "12", "daytype" = "WE", "weather_code" = "UNK", "lakeview_yr" = NULL, "lake_hr" = NULL))%>%droplevels()


  Edata <<- Edata
  Icedata <<- Icedata
  Lakes <<-Lakes


}
