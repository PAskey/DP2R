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


Effort2R <- function() {


  if(!exists("conn")){stop("First you must establish a connection to DataPond or DataPond_STAGE in R and assign to 'conn' object. Use example code with your personal uid and pwd. Note this often fails on first attempt (some sort of timeout lag with Azure, but then works when you re-run the code
  conn <- DBI::dbConnect(drv = odbc::odbc(),
                         Driver = 'SQL Server'',
                         server = 'tcp:gofishbc.database.windows.net,1433',
                         database = 'DataPond',
                         uid = uid,
                         pwd = pwd")}


DP2R::DP2R(Tables = c("vwEffort","vwWaterbodyLake"))



#Rename some variables, add some and filter to valid data
#Partial ice cover data was removed, as unclear how to assign between ice and open effort, and due to safety/access, should almost always be 0.
Edata = vwEffort%>%dplyr::mutate(date = as.Date(assessed_dt),
                                 month = lubridate::month(date),
                                 hour = lubridate::hour(assessed_dt))%>%
                   dplyr::filter(method!='TRL',
                                 hour %in% c(6:20),
                                 is.na(percent_visibility)|percent_visibility>75,
                                 !ice_cover_code %in% c("PARTIAL"),)%>%
                   dplyr::arrange(year, WBID, method)%>%
                   dplyr::select(-'assess_event_id')

#After initial filters, we need a somewhat difficult data clean. Take cases where all effort observations are NA (could be all 0s or not counted due to quality of data), and then remove the ones where it looks like the NAs were due to not observing at all or very poor observation conditions.
#Set of columns that must have at least one non-NA value for data to be useful
vcols = c("num_shore_ice","num_spv","num_boat","num_ice_tent")
Edata$allNA = rowSums(is.na(Edata[vcols])) == length(vcols)

# Define the keywords to filter out
keywords <- c("terminated","thunderstorm", "visibility", "not observed",
              "not able", "lightning", "unable", "dark", "low light","glare",
              "lens", "twilight", "dawn", "fog",
              "thawing", "blurry", "bluury","fuzzy","branch", "no image")

# Create a regex pattern to match any of the keywords
pattern <- stringr::str_c(keywords, collapse = "|")

# Filter the Edata dataframe.The logic in the filter() function now checks if allNA is FALSE (keeping those rows) or if allNA is TRUE and the comment does not contain any of the keywords (again, keeping those rows).
#Reduces data by ~75K records, but seems like worthwhile tradeoff for data quality.
Remove <- Edata %>%
  dplyr::filter((allNA & stringr::str_detect(tolower(comment), pattern)))

Edata = dplyr::anti_join(Edata, Remove, by = NULL)%>%
  dplyr::select(-allNA)



#join to DayType categories
#data(DayTypes)
Edata<-dplyr::left_join(Edata, DP2R::DayTypes[,c('date','daytype')], by = "date")

#Create a set of grouping variables for analyses.
#view_yr is the point of view, lake and year, and is the basic unit of total angler effort estimates (previous analyses called this cam_yr).
#lake_hr allows comparison of different instantaneous count methods of the same lake-hour.

Edata = Edata%>%dplyr::group_by(WBID, method, view_location_name, year)%>%
                dplyr::mutate(lakeview_yr = dplyr::cur_group_id())%>%
                dplyr::group_by(WBID, date, hour)%>%
                dplyr::mutate(lake_hr = dplyr::cur_group_id())%>%
                dplyr::ungroup()

#Remove data with less than a minimum sample size of counts per cam_yr
min_days = 12 #Even 12 count days would not be very reliable estimate, but in the interest of keeping as much data as possible. This cutoff keeps a lot of the data.

Edata <- Edata %>%
  dplyr::group_by(lakeview_yr) %>%
  dplyr::filter(dplyr::n_distinct(date) >= min_days) %>%
  dplyr::ungroup()

#Convert categorical and character variables to factors for statistical modelling
Edata = DP2R::fac.data(Edata, varlist = list("year" = NULL, "month" = "5", "hour" = "12", "daytype" = "WE", "lakeview_yr" = NULL, "lake_hr" = NULL))


#OE (observed effort) column is the total effort counted at one time across all modes: shore, boat spv.
Edata = Edata%>%dplyr::mutate(OE = rowSums(dplyr::across(c(num_shore_ice, num_spv, num_boat)),na.rm = TRUE))

###################################################################################
#REMOVE THIS SECTION ONCE VIEW IS UPDATED
Edata <- dplyr::left_join(
  Edata,
  vwWaterbodyLake[, c("WBID", "region", "gazetted_name", "area_ha")],
  by = "WBID"
)

Lakes = vwWaterbodyLake%>%dplyr::filter(WBID%in%Edata$WBID)
###################################################################################

#Create table for ice fishing and open water separately
#OE (observed effort) column is the total effort counted at one time across all modes: ice, tents.
Icedata = Edata%>%dplyr::filter(month %in% c(1:3,12),
                                hour %in% c(8:18),
                                !ice_cover_code %in% c("OPEN","PARTIAL"),
                                is.na(num_boat))%>%
                  dplyr::mutate(month = stats::relevel(factor(month), "1"),
                                OE = rowSums(dplyr::across(c(num_shore_ice, num_ice_tent)), na.rm = TRUE))%>%droplevels()

Edata = Edata%>%dplyr::filter(month %in% c(4:10), !ice_cover_code %in% c("COVERED"))%>%droplevels()

Edata <<- Edata
Icedata <<- Icedata
Lakes <<-Lakes


}

