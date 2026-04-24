#Create a stored Lakes file to quickly call into dplyr functions

DP2R::DP2R(Tables = c("vwWaterbodyLake","vwWaterbody"))

#vwWaterbody includes rivers
Lakes = dplyr::left_join(vwWaterbodyLake, vwWaterbody)


cols_order = c("waterbody_type", "region_code", "WBID", "gazetted_name", "alias", "locale_name", "nearest_town",  "area_ha", "lake_volume_m3", "max_depth_m", "mean_depth_m", "elevation_m",
  "perimeter", "littoral_area_ha", "littoral_area_percent", "max_water_level",
  "num_outlets", "num_perm_inlets", "4wd",
  "hike_in", "boat_launch", "fishing_pier", "campsite",
  "washroom", "wheelchair", "directions", "access_comment", "latitude", "longitude", "lake_latitude", "lake_longitude", "allow_release",
  "comment", "waterbody_key","watershed_group_code", "waterbody_id", "locale_id",
  "feature_code","watershed_code", "ffsbc_waterbody_id", "active")

Lakes <- Lakes[, cols_order]

#res <- find_region_wmu(
#  lon  = Lakes$longitude,
#  lat  = Lakes$latitude,
#  WBID = Lakes$WBID,
#  wbid_chunk_size = 400
#)

#Lakes <- dplyr::bind_cols(Lakes, res)

#Add elevations to lakes without this information
idx <- which(!is.na(Lakes$waterbody_type))
Lakes[idx, ] <- DP2R::find_elevation(Lakes[idx, ])

#Estimate ice-on or off based on elevation and region
idx <- which(!is.na(Lakes$elevation_m))
Lakes <- DP2R::estimate_lake_ice(Lakes[idx, ])


usethis::use_data(Lakes, overwrite = TRUE)
