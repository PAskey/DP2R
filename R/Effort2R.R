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


DP2R::DP2R(Tables = "vwEffort")

#Rename some variables, add some and filter to valid data
Edata = vwEffort%>%dplyr::rename(WBID = 'waterbody_identifier',
                                 method = 'fishing_effort_type',
                                 year = 'assess_year')%>%
                   dplyr::mutate(date = as.Date(assessed_dt),
                                 month = lubridate::month(date),
                                 hour = lubridate::hour(assessed_dt))%>%
                   dplyr::filter(method!='TRL',
                                 hour %in% c(6:20),
                                 is.na(percent_visibility)|percent_visibility>75)%>%
                   dplyr::arrange(year, WBID, method)%>%
                   dplyr::select(-'assess_event_id')



#join to DayType categories
#data(DayTypes)
Edata<-dplyr::left_join(Edata, DayTypes[,c('date','daytype')], by = "date")

#Create a grouping variable for the point of view, lake and year
#This is the scale of an effort estimate
Edata = Edata%>%dplyr::group_by(WBID, method, view_location_name, year)%>%
                dplyr::mutate(cam_yr = dplyr::cur_group_id())%>%
                dplyr::ungroup()

#Remove data with less than a minimum sample size of counts per cam_yr
min_days = 12 #Even 12 count days would not be very reliable estimate, but in the interest of keeping as much data as possible. This cutoff keeps a lot of the data.
tmp<- Edata %>% dplyr::group_by(cam_yr)%>%
  dplyr::summarize(days= length(unique(date)))%>%
  dplyr::filter(days >= min_days)%>%
  dplyr::pull(cam_yr)

Edata <-dplyr::filter(Edata, cam_yr %in% tmp)
rm(tmp)

#Convert categorical and character variables to factors for statistical modelling
Edata = Edata%>%dplyr::mutate(year = factor(year),
                              month = stats::relevel(factor(month), "5"),
                              hour = stats::relevel(factor(hour), "12"),
                              daytype = stats::relevel(factor(daytype),"WE"),
                              cam_yr = factor(cam_yr))

#Crate table for ice fishing and open water separately
Icedata = Edata%>%dplyr::filter(month %in% c(1:3,12),
                                hour %in% c(8:18),
                                !ice_cover_code %in% c("OPEN","PARTIAL"),
                                is.na(num_boat))%>%
                  dplyr::mutate(month = stats::relevel(factor(month), "1"))

Edata = Edata%>%dplyr::filter(month %in% c(4:10), !ice_cover_code %in% c("COVERED"))

Edata <<- Edata
Icedata <<- Icedata

}

