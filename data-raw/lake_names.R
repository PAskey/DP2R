#A data set to consolidate gazetted names and alias into a single prefferred name for each lake (without the word "lake")

DP2R::DP2R(Tables = "vwWaterbodyLake")

lake_names = vwWaterbodyLake%>%
  dplyr::select(WBID, gazetted_name, alias)%>%
  mutate(gazetted_name = if_else(gazetted_name == "NA", NA_character_, gazetted_name))%>%
  dplyr::group_by(WBID)%>%
  dplyr::mutate(lake_name = if_else(is.na(gazetted_name),alias, gazetted_name))%>%
  dplyr::mutate(lake_name = gsub(" (LAKE|CREEK)$", "", lake_name),
                Lake_WBID = paste0(lake_name,"_",WBID))

usethis::use_data(lake_names, overwrite = TRUE)
