#' A function to create data subsets to estimate expansion factors for cameras where there has been ground counts to verify. Effort estimates from Camera data is compared to estimates from AIR/GRD with EffortEst function and the ratio of those is considered the expansion.
#'
#'
#' @title Cam_xdata
#' @name Cam_xdata
#' @keywords SPDT; DataPond
#' @export
#' @param data a data set to search for ground or air based verification counts of camera data. defaults to data, as this function is run within the Effort2R() function
#'
#' @examples
#'
#' DP2R::Cam_xdata()
#'
#' @importFrom magrittr "%>%"


Cam_xdata <- function(data = Edata) {

#CHatgpt helped with th esituation where more than one camera, so need to separate by lakeview_yr to compare to other methods.
  # Summarize AIR and GRD separately (ignoring lakeview_yr)
  air_grd_summary <- data %>%
    dplyr::filter(method %in% c("AIR", "GRD")) %>%
    dplyr::select(WBID, date, lake_hr, method, OE) %>%
    #dplyr::summarize(mucnt = mean(OE, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = method, values_from = OE)

  # Summarize CAM (keeping lakeview_yr for camera differentiation)
  cam_summary <- data %>%
    dplyr::filter(method == "CAM") %>%
    dplyr::select(WBID, view_location_name, year, lakeview_yr, date, lake_hr, OE)%>%
    rename(CAM = OE)
    #dplyr::summarize(CAM = mean(OE, na.rm = TRUE), .groups = "drop")#Should be just one observation anyways

  # Join CAM counts with AIR and GRD summaries
  contrasts <- cam_summary %>%
    dplyr::left_join(air_grd_summary, by = c("WBID", "date", "lake_hr")) %>%

    # Filter to keep rows where CAM is present
    dplyr::filter(!is.na(CAM)) %>%

    # Ensure at least one of AIR or GRD is not NA
    dplyr::filter(!(is.na(AIR) & is.na(GRD))) %>%

    # Ensure AIR and GRD match if both are present
    dplyr::filter((is.na(AIR) | is.na(GRD)) | (AIR == GRD)) #%>%

    # Pull lake_hr values
    #dplyr::pull(lake_hr)

###MAYBE CHANGE ABOVE TO PULL LAKEVIEW AND LAKE_HR COMBO?
  ##Might be able to verify sample sizes in the code black above.
  lklist <- contrasts%>%
    dplyr::group_by(WBID, year, lakeview_yr, view_location_name)%>%
    dplyr::summarize(ndays = length(unique(date)),
                     n = dplyr::n(),
                     sumcnts = sum(CAM))%>%
    dplyr::filter(!is.na(view_location_name),ndays>=12&sumcnts>=5)%>%
    dplyr::ungroup()%>%
    dplyr::select(WBID, year)%>%
    dplyr::distinct()

  #REduce to lake-year combos that meet sample size requirements
  ExpData = dplyr::inner_join(lklist, data, by = c('WBID','year'))

  # Filter only necessary columns first to reduce size
  ExpData <- ExpData %>%
    dplyr::filter(lake_hr %in% contrasts$lake_hr) #%>%


  # Ensure the final data frame matches the original column order
  ExpData <- ExpData %>%
    dplyr::select(dplyr::all_of(colnames(data)))


  allCamyrs<-unique(ExpData[ExpData$method == 'CAM',]$lakeview_yr)

  A<- NULL
  AC<-NULL
  C<-NULL
  GR<-NULL
  GRC<-NULL
  X<-data.frame()
  #Must make lakeview_yr character to matchup with expansion naming scheme
  data$lakeview_yr<-as.character(data$lakeview_yr)
  ExpData$lakeview_yr<-as.character(ExpData$lakeview_yr)

  for (i in allCamyrs){
    C <- ExpData%>%
      dplyr::filter(lakeview_yr == i)

    A <- ExpData[ExpData$method == 'AIR' & ExpData$lake_hr%in%C$lake_hr,]
    A$lakeview_yr[!is.na(A$lakeview_yr)] = paste(C$lakeview_yr[1], 'AIR', sep = '_')
    A$view_location_name[is.na(A$view_location_name)] = C$view_location_name[1]

    AC <- C%>%
      dplyr::filter(lake_hr%in%A$lake_hr)%>%
      dplyr::mutate(lakeview_yr = paste(C$lakeview_yr[1], view_location_name, 'C_AIR',sep = "_"))


    GR <- ExpData[ExpData$method == 'GRD' & ExpData$lake_hr%in%C$lake_hr,]
    GR$lakeview_yr[!is.na(GR$lakeview_yr)] = paste(C$lakeview_yr[1], 'GRD', sep = '_')
    GR$view_location_name[is.na(GR$view_location_name)]  = C$view_location_name[1]

    GRC <- C%>%
      dplyr::filter(lake_hr%in%GR$lake_hr)%>%
      dplyr::mutate(lakeview_yr = paste(C$lakeview_yr[1], view_location_name, 'C_GR',sep = "_"))

    X <- dplyr::bind_rows(X, A, GR, AC, GRC)#bind_rows faster than rbind

  }

  # Return the updated data with X appended
  # Combine the original data with X
  updated_data <- dplyr::bind_rows(data, X)

  # Return the updated data
  return(updated_data)
}
