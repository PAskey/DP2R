#A data set to consolidate gazetted names and alias into a single prefferred name for each lake (without the word "lake")

DP2R::DP2R(Tables = "vwWaterbodyLake")
wb_alias <- vwWaterbodyLake%>%
  dplyr::transmute(WBID,
                   alias)

DP2R::DP2R(Tables = "vwWaterbody")

# Build a lookup with just what you need (and ensure WBID type is consistent)
wb_locale <- vwWaterbody %>%
  dplyr::filter(waterbody_type != 'RV')%>%
  dplyr::transmute(
    WBID,
    locale_name,
    gazetted_name
  )

lake_names = full_join(wb_locale, wb_alias, by = "WBID")%>%
  dplyr::mutate(locale_name = gsub(" (LAKE|CREEK)$", "", locale_name),
                Lake_WBID = paste0(locale_name,"_",WBID))


usethis::use_data(lake_names, overwrite = TRUE)
