#Create a stored Lakes file to quickly call into dplyr functions

DP2R::DP2R(Tables = c("vwWaterbody"))

#vwWaterbody includes rivers
Lakes = vwWaterbody%>%
  dplyr::filter(!is.na(WBID), waterbody_type != "RV")


cols_order =
  c("waterbody_type", "region_code", "WBID", "gazetted_name", "locale_name", "nearest_town",
  "area_ha", "lake_volume_m3", "max_depth_m", "mean_depth_m", "elevation_m",
  "perimeter", "littoral_area_ha", "littoral_area_percent",
  "num_outlets", "num_perm_inlets", "4wd",
  "hike_in", "boat_launch", "fishing_pier", "campsite",
  "washroom", "wheelchair", "directions", "access_comment", "access_lat", "access_long", "lake_lat", "lake_long", "allow_release",
  "comment", "waterbody_key","watershed_group_code", "waterbody_id", "locale_id",
  "feature_code","watershed_code", "ffsbc_waterbody_id", "active")

Lakes <- Lakes[, cols_order]

#res <- find_region_wmu(
#  lon  = Lakes$lake_long,
#  lat  = Lakes$lake_lat,
#  WBID = Lakes$WBID,
#  wbid_chunk_size = 400
#)

#Lakes <- dplyr::bind_cols(Lakes, res)

#Add elevations and estimated ice phenology from the stored Elev_ice_estimates table.
#These come from find_elevation() (slow remote terrain lookup) and
#estimate_lake_ice(), which are run once a year in data-raw/Create_Elev_ice_estimates.R
#rather than on every Lakes build. Fill a real DataPond elevation_m where present,
#otherwise use the stored estimate; bring the ice-on/off day-of-year estimates in.
Lakes <- Lakes |>
  dplyr::left_join(DP2R::Elev_ice_estimates, by = "WBID") |>
  dplyr::mutate(elevation_m = dplyr::coalesce(elevation_m, elevation_est)) |>
  dplyr::select(-elevation_est)

#If any lakes are new since Elev_ice_estimates was last built they will have NA
#elevation/ice. Re-run data-raw/Create_Elev_ice_estimates.R to refresh, or uncomment
#below to estimate just the gaps on the fly:
#gap <- is.na(Lakes$elevation_m) & !is.na(Lakes$waterbody_type)
#Lakes[gap, ] <- DP2R::find_elevation(Lakes[gap, ], lon_col = "lake_long", lat_col = "lake_lat")
#Lakes <- DP2R::estimate_lake_ice(Lakes, lat_col = "lake_lat", overwrite = FALSE)


#Add links to online bathymetric maps (BC FIDQ) by WBID.
#bathymetry_links.csv has one url per WBID (WBIDs are unique), so this left_join
#adds a `bathymetry_url` column without duplicating any Lakes rows. colClasses
#keeps WBID as character so leading zeros are preserved for the join.
bathymetry_links <- read.csv("data-raw/bathymetry_links.csv", colClasses = "character")
Lakes <- dplyr::left_join(Lakes, bathymetry_links, by = "WBID")


usethis::use_data(Lakes, overwrite = TRUE)
