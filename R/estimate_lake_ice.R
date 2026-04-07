#' Estimate typical lake ice-on and ice-off timing
#'
#' Estimates typical annual ice-on and ice-off timing for lakes using
#' latitude, elevation, surface area, and region code. The function is
#' intended to produce realistic approximate values for British Columbia
#' lakes when observed ice phenology data are unavailable.
#'
#' Two columns are added to the input data frame:
#' \itemize{
#'   \item `ice_on_day`: estimated ice-on day of year
#'   \item `ice_off_day`: estimated ice-off day of year
#' }
#'
#' Lakes that are unlikely to freeze in a typical year are assigned `NA`
#' for both output columns.
#'
#' @param x A data frame containing lake attributes.
#' @param lat_col Name of the latitude column. Default is `"latitude"`.
#' @param elevation_col Name of the elevation column in metres.
#'   Default is `"elevation_m"`.
#' @param area_col Name of the lake area column in hectares.
#'   Default is `"area_ha"`.
#' @param region_col Name of the region code column used to identify
#'   coastal lakes. Default is `"region_code"`.
#' @param ice_on_day_col Name of the output ice-on day-of-year column.
#'   Default is `"ice_on_day"`.
#' @param ice_off_day_col Name of the output ice-off day-of-year column.
#'   Default is `"ice_off_day"`.
#' @param overwrite Logical; if `FALSE`, existing non-missing values in
#'   output columns are preserved. Default is `TRUE`.
#'
#' @return The input data frame with ice phenology columns added.
#'
#' @details
#' This is a heuristic BC-focused approximation, not a calibrated model.
#' Colder, higher, and more northerly lakes freeze earlier and thaw later.
#' Larger lakes freeze later and thaw earlier.
#'
#' Coastal lakes are identified as rows where `region_code` is `1` or `2`.
#' Some low-elevation coastal lakes are treated as not typically freezing,
#' and are assigned `NA`.
#'
#' Estimated day-of-year values are constrained to plausible ranges:
#' \itemize{
#'   \item ice-on: day 300 to 360
#'   \item ice-off: day 75 to 180
#' }
#'
#' @examples
#' \dontrun{
#' Lakes <- estimate_lake_ice(Lakes)
#' }
#'
#' @export
estimate_lake_ice <- function(x,
                              lat_col = "latitude",
                              elevation_col = "elevation_m",
                              area_col = "area_ha",
                              region_col = "region_code",
                              ice_on_day_col = "ice_on_day",
                              ice_off_day_col = "ice_off_day",
                              overwrite = TRUE) {
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.", call. = FALSE)
  }

  required_cols <- c(lat_col, elevation_col)

  missing_cols <- required_cols[!required_cols %in% names(x)]
  if (length(missing_cols) > 0L) {
    stop(
      sprintf(
        "Required column(s) not found in `x`: %s",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!ice_on_day_col %in% names(x)) {
    x[[ice_on_day_col]] <- NA_integer_
  }
  if (!ice_off_day_col %in% names(x)) {
    x[[ice_off_day_col]] <- NA_integer_
  }

  lat <- x[[lat_col]]
  elev <- x[[elevation_col]]

  area <- rep(NA_real_, nrow(x))
  if (area_col %in% names(x)) {
    area <- x[[area_col]]
  }

  region <- rep(NA, nrow(x))
  if (region_col %in% names(x)) {
    region <- x[[region_col]]
  }

  coastal <- !is.na(region) & region %in% c(1, 2, "1", "2")

  valid_idx <- which(
    !is.na(lat) &
      !is.na(elev) &
      is.finite(lat) &
      is.finite(elev) &
      lat >= -90 & lat <= 90
  )

  if (!overwrite) {
    valid_idx <- valid_idx[
      is.na(x[[ice_on_day_col]][valid_idx]) |
        is.na(x[[ice_off_day_col]][valid_idx])
    ]
  }

  if (length(valid_idx) == 0L) {
    return(x)
  }

  lat_v <- lat[valid_idx]
  elev_v <- elev[valid_idx]
  area_v <- area[valid_idx]
  coastal_v <- coastal[valid_idx]

  # Identify lakes that typically do not freeze
  never_freeze <- coastal_v & (
    elev_v < 150 |
      (elev_v < 250 & lat_v < 50.5) |
      (elev_v < 100 & lat_v < 51.5)
  )

  freeze_idx <- which(!never_freeze)

  if (length(freeze_idx) == 0L) {
    return(x)
  }

  ice_on <- 315 + 2.2 * (lat_v[freeze_idx] - 49) + 0.0025 * elev_v[freeze_idx]
  ice_off <- 105 + 2.8 * (lat_v[freeze_idx] - 49) + 0.0035 * elev_v[freeze_idx]

  # Area adjustment
  area_adjust <- rep(0, length(freeze_idx))
  area_vals <- area_v[freeze_idx]
  area_ok <- !is.na(area_vals) & is.finite(area_vals) & area_vals >= 0
  area_adjust[area_ok] <- log10(area_vals[area_ok] + 1)

  ice_on <- ice_on + 3 * area_adjust
  ice_off <- ice_off - 3 * area_adjust

  # Coastal adjustment (for lakes that do freeze)
  coastal_freeze <- coastal_v[freeze_idx]
  ice_on[coastal_freeze] <- ice_on[coastal_freeze] + 20
  ice_off[coastal_freeze] <- ice_off[coastal_freeze] - 20

  # Clamp to realistic bounds
  ice_on <- pmin(pmax(ice_on, 300), 360)
  ice_off <- pmin(pmax(ice_off, 75), 180)

  ice_on <- as.integer(round(ice_on))
  ice_off <- as.integer(round(ice_off))

  out_idx <- valid_idx[freeze_idx]

  x[[ice_on_day_col]][out_idx] <- ice_on
  x[[ice_off_day_col]][out_idx] <- ice_off

  x
}
