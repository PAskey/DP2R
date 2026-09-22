# Create_Elev_ice_estimates.R
# -----------------------------------------------------------------------------
# Build the stored `Elev_ice_estimates` table: one row per lake (WBID) holding a
# best-available elevation and the estimated typical ice-on / ice-off timing.
#
# WHY THIS IS A STORED TABLE:
#   find_elevation() queries a remote elevation service (elevatr / AWS) for every
#   lake without a DataPond elevation. That is slow and the underlying terrain
#   data essentially never changes, so there is no reason to re-run it on every
#   Create_Lakes.R build. Instead we run it here ONCE (e.g. annually), store the
#   result, and Create_Lakes.R just joins it in.
#
# COLUMNS PRODUCED:
#   WBID          Character. Waterbody identifier (leading zeros preserved).
#   elevation_est Numeric. Best-available elevation in metres: the DataPond
#                 elevation_m where present, otherwise the find_elevation()
#                 estimate. This is the value used to drive the ice estimate.
#   ice_on_day    Integer. Estimated typical ice-on day of year (NA if the lake
#                 does not typically freeze).
#   ice_off_day   Integer. Estimated typical ice-off day of year.
#
# RUN CADENCE: once a year (or whenever the lake list / coordinates change).
# REQUIREMENTS: VPN/DataPond connection + internet (elevatr) + `elevatr` pkg.
# -----------------------------------------------------------------------------

library(dplyr)

## 1. Pull the waterbody attributes needed for elevation + ice estimation.
##    Mirrors the source join used in Create_Lakes.R so the WBID grain matches.
DP2R::DP2R(Tables = c("vwWaterbody"))

lake_base <- vwWaterbody%>%
  # One row per waterbody. Keep only the fields the two estimators read.
  dplyr::select(dplyr::any_of(c(
    "WBID", "waterbody_type", "region_code",
    "lake_lat", "lake_long", "area_ha", "elevation_m"
  ))) %>%
  dplyr::filter(!is.na(WBID), waterbody_type != "RV") %>%
  dplyr::distinct(WBID, .keep_all = TRUE)

## 2. Fill missing elevations from the terrain service.
##    overwrite = FALSE keeps any real DataPond elevation and only estimates the
##    gaps. Uses the lake point (lake_lat/lake_long; outlet or centroid) from
##    vwWaterbody. access_lat/access_long are missing for most lakes.
lake_base <- DP2R::find_elevation(lake_base, lon_col = "lake_long", lat_col = "lake_lat",
                                overwrite = FALSE)

## 3. Estimate ice-on / ice-off from latitude, elevation, area and region.
lake_base <- DP2R::estimate_lake_ice(lake_base, lat_col = "lake_lat")

## 4. Reduce to the stored schema. elevation_est == best-available elevation.
Elev_ice_estimates <- lake_base %>%
  dplyr::transmute(
    WBID,
    elevation_est = elevation_m,
    ice_on_day    = as.integer(ice_on_day),
    ice_off_day   = as.integer(ice_off_day)
  ) %>%
  dplyr::arrange(WBID)

## 5. Save as package data.
usethis::use_data(Elev_ice_estimates, overwrite = TRUE)
