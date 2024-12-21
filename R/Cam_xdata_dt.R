#' A function to create data subsets to estimate expansion factors for cameras where there has been ground/air counts to verify. Effort estimates from Camera data is compared to estimates from AIR/GRD with EffortEst function and the ratio of those is considered the expansion. The cam_xdata_dt is an improved version of Cam_xdata that used data.table and is much faster.
#'
#'
#' @title Cam_xdata_dt
#' @name Cam_xdata_dt
#' @keywords DP2R; SPDT; DataPond
#' @export
#' @param data a data.table object to search for ground or air based verification counts of camera data. defaults to data, as this function is run within the Effort2R() function
#' @param min_days the minimum number of unique days that verification counts were conducted
#' @param min_obs_E the minimum amount of angler effort (any of shore, boat, etc.) observed by the camera during the verification counts. It is important to have non-0 counts during verification counts. SOmetimes cameras are used in low effort areas, but obviously if 0 effort is observed by the camera during all of the verification counts then there is no way to expand to full lake effort.
#'
#'
#' @examples
#'
#' DP2R::Cam_xdata_dt(data = Edata_dt)
#'
#' @importFrom magrittr "%>%"


Cam_xdata_dt <- function(data = Edata_dt, min_days = 12, min_obs_E = 6) {

  # Summarize AIR and GRD methods
  air_grd_summary <- data[method %in% c("AIR", "GRD"),
                          .(WBID, date, lake_hr, method, OE)]

  # Pivot AIR and GRD into wide format
  air_grd_summary <- data.table::dcast(air_grd_summary, WBID + date + lake_hr ~ method, value.var = "OE")

  # Summarize CAM (keeping lakeview_yr for camera differentiation)
  cam_summary <- data[method == "CAM",
                      .(WBID, view_location_name, year, lakeview_yr, date, lake_hr, CAM = OE)]

  # Join CAM with AIR/GRD summaries
  contrasts <- cam_summary[air_grd_summary,
                           on = .(WBID, date, lake_hr)]  # Efficient join

  # Filter conditions that either AIR or GRD counts be present during the CAM counts, there are a four cases (Eena 2011, Irish and French(2) in 2013) where GRD counts were less than AIR. IN this case GRD counts must not see 100% of lake and underestimate total effort. IT is possible to add a condition that AIR=GRD as: & ((is.na(AIR) | is.na(GRD)) | (AIR == GRD))
  contrasts <- contrasts[!is.na(CAM) & !(is.na(AIR) & is.na(GRD))]

  # Summarize sample sizes for lake-year combinations
  lklist <- contrasts[, .(
    ndays = data.table::uniqueN(date),
    n = .N,
    sumcnts = sum(CAM, na.rm = TRUE)
  ), by = .(WBID, year, lakeview_yr, view_location_name)]

  # Filter lake-year combinations meeting conditions set my min_days and min_obs_E parameters
  lklist <- lklist[
    !is.na(view_location_name) & ndays >= min_days & sumcnts >= min_obs_E,
    .(WBID, year)
  ]

  # Remove duplicates from lklist to avoid unnecessary row inflation
  lklist <- unique(lklist, by = c("WBID", "year"))

  # Perform the join, ensuring only matching rows are retained
  ExpData <- data[lklist, on = .(WBID, year), nomatch = 0L]


  # Filter rows matching lake_hr values from contrasts
  ExpData <- ExpData[lake_hr %in% contrasts$lake_hr]

  # Ensure the final data frame matches the original column order
  ExpData <- ExpData[, colnames(data), with = FALSE]



  #Must make lakeview_yr character to match with expansion naming scheme
  data[, lakeview_yr := as.character(lakeview_yr)]
  ExpData[, lakeview_yr := as.character(lakeview_yr)]

  # Process each lakeview_yr (loop replacement with `rbindlist`)
  allCamyrs <- as.character(unique(ExpData[method == "CAM", lakeview_yr]))

  results <- data.table::rbindlist(lapply(allCamyrs, function(current_camyr) {
    C <- ExpData[lakeview_yr == current_camyr]

    A <- ExpData[method == "AIR" & lake_hr %in% C$lake_hr][
      , `:=`(
        lakeview_yr = paste(current_camyr, "AIR", sep = "_"),
        view_location_name = data.table::fifelse(is.na(view_location_name), C$view_location_name[1], view_location_name)
      )]

    AC <- C[lake_hr %in% A$lake_hr][
      , lakeview_yr := paste(current_camyr, view_location_name, "C_AIR", sep = "_")]

    GR <- ExpData[method == "GRD" & lake_hr %in% C$lake_hr][
      , `:=`(
        lakeview_yr = paste(current_camyr, "GRD", sep = "_"),
        view_location_name = data.table::fifelse(is.na(view_location_name), C$view_location_name[1], view_location_name)
      )]

    GRC <- C[lake_hr %in% GR$lake_hr][
      , lakeview_yr := paste(current_camyr, view_location_name, "C_GR", sep = "_")]

    data.table::rbindlist(list(A, GR, AC, GRC))
  }))

  # Combine original data with the results
  updated_data <- rbind(data, results, fill = TRUE)
}
