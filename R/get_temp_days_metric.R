#' Calculate Temperature-Based Growing Metrics from Climate Normals
#'
#' Computes temperature-based metrics for point locations using long-term
#' monthly mean air temperature normals from WorldClim.
#'
#' @description
#' This function calculates either:
#'
#' \itemize{
#'   \item \strong{Tally metric}: estimated number of days per year where
#'   mean air temperature is greater than or equal to a threshold.
#'   \item \strong{Accumulated metric}: estimated annual degree-days above a
#'   threshold.
#' }
#'
#' @details
#' Climate data are retrieved from the WorldClim v2.1 monthly climate normals:
#' \url{https://www.worldclim.org/data/worldclim21.html}
#'
#' Monthly mean air temperatures are interpolated to estimated daily values
#' before calculating the requested metric. This avoids tally values being
#' restricted to whole-month increments.
#'
#' The resulting values represent long-term average annual metrics, not
#' year-specific values.
#'
#' \strong{Important limitations:}
#' \itemize{
#'   \item Metrics are based on air temperature, not water temperature.
#'   \item Daily values are interpolated from monthly means, so true daily
#'   variability is not captured.
#'   \item These metrics should be interpreted as approximate thermal
#'   opportunity covariates.
#' }
#'
#' @param df A data frame containing latitude and longitude columns.
#' @param lat_col Character. Name of the latitude column. Default is
#'   \code{"latitude"}.
#' @param lon_col Character. Name of the longitude column. Default is
#'   \code{"longitude"}.
#' @param threshold Numeric. Temperature threshold in degrees Celsius.
#' @param days_metric Character. Type of metric to calculate. Must be one of:
#'   \itemize{
#'     \item \code{"tally"}: estimated days with mean air temperature >= threshold.
#'     \item \code{"sum"}: estimated accumulated degree-days above threshold.
#'   }
#' @param res Numeric. WorldClim spatial resolution in arc-minutes. Common
#'   values include 10, 5, 2.5, and 0.5. Default is 2.5.
#' @param path Character. Directory where WorldClim data should be downloaded
#'   or read from. Default is \code{tempdir()}.
#' @param output_col Character or NULL. Optional name for the output column.
#'   If NULL, a name is generated automatically.
#'
#' @return
#' The input data frame with one additional column containing the calculated
#' metric.
#'
#' @examples
#' \dontrun{
#' lakes_days10 <- get_temp_days_metric(
#'   Lakes,
#'   threshold = 10,
#'   days_metric = "tally"
#' )
#'
#' lakes_dd5 <- get_temp_days_metric(
#'   Lakes,
#'   threshold = 5,
#'   days_metric = "sum"
#' )
#' }
#'
#' @importFrom dplyr %>% mutate select left_join all_of row_number bind_cols
#' @export
get_temp_days_metric <- function(df,
                                 lat_col = "latitude",
                                 lon_col = "longitude",
                                 threshold = 10,
                                 days_metric = c("tally", "sum"),
                                 res = 2.5,
                                 path = tempdir(),
                                 output_col = NULL) {
  
  days_metric <- match.arg(days_metric)
  
  if (!lat_col %in% names(df)) {
    stop("Latitude column '", lat_col, "' not found in df.", call. = FALSE)
  }
  
  if (!lon_col %in% names(df)) {
    stop("Longitude column '", lon_col, "' not found in df.", call. = FALSE)
  }
  
  if (is.null(output_col)) {
    output_col <- paste0(
      ifelse(days_metric == "tally", "days_ge_", "dd_above_"),
      threshold,
      "c"
    )
  }
  
  tavg <- geodata::worldclim_global(
    var = "tavg",
    res = res,
    path = path
  )
  
  pts <- df %>%
    mutate(.row_id = row_number()) %>%
    select(
      .row_id,
      lon = all_of(lon_col),
      lat = all_of(lat_col)
    )
  
  pts_vect <- terra::vect(
    pts,
    geom = c("lon", "lat"),
    crs = "EPSG:4326"
  )
  
  vals <- terra::extract(tavg, pts_vect)
  vals <- as.data.frame(vals)
  vals$ID <- NULL
  
  # WorldClim temperature rasters may be returned as °C or °C * 10 depending
  # on source/version. This check avoids accidentally scaling twice.
  if (max(as.matrix(vals), na.rm = TRUE) > 60) {
    vals <- vals / 10
  }
  
  month_midpoints <- c(15.5, 45.125, 74.625, 105.125, 135.625, 166.125,
                       196.625, 227.625, 258.125, 288.625, 319.125, 349.625)
  
  daily_days <- 1:365
  
  estimate_metric <- function(monthly_temps) {
    monthly_temps <- as.numeric(monthly_temps)
    
    if (all(is.na(monthly_temps))) {
      return(NA_real_)
    }
    
    x <- c(month_midpoints - 365, month_midpoints, month_midpoints + 365)
    y <- c(monthly_temps, monthly_temps, monthly_temps)
    
    daily_temp <- stats::approx(
      x = x,
      y = y,
      xout = daily_days,
      rule = 2
    )$y
    
    if (days_metric == "tally") {
      sum(daily_temp >= threshold, na.rm = TRUE)
    } else {
      sum(pmax(daily_temp - threshold, 0), na.rm = TRUE)
    }
  }
  
  metric_vals <- data.frame(
    .row_id = pts$.row_id,
    metric_value = apply(vals, 1, estimate_metric)
  )
  
  names(metric_vals)[names(metric_vals) == "metric_value"] <- output_col
  
  df %>%
    mutate(.row_id = row_number()) %>%
    left_join(metric_vals, by = ".row_id") %>%
    select(-.row_id)
}
