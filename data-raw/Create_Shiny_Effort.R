#Run from the DP2R project folder (or via DP2Rdata_refresh(), which sets rerun_estimates itself).

library(DP2R)
library(dplyr)

#Builds three stored effort tables:
#  shinydata  - DataPond count-based estimates only (CAM, AIR, GRD), from EffortEst()
#  Effort_all - shinydata + questionnaire/survey estimates (Effort_external: QNR, SVY).
#               This is the single effort table the Angler_Effort app, lake report and
#               stocking metrics (effort_stocking(), stock_performance()) should use.
#  lakesum    - lake-by-lake summary of Effort_all
#Set rerun_estimates = FALSE to rebuild Effort_all/lakesum from the stored shinydata
#(e.g. after only re-running Create_Effort_external.R), which needs no DataPond connection.
if (!exists("rerun_estimates", inherits = FALSE)) rerun_estimates <- TRUE

if (rerun_estimates) {
  EffortEst(update.model = TRUE)

  shinydata = EffortEsts

  shinydata = dplyr::left_join(shinydata,
                               #vwWaterbody names are lake_lat/lake_long; keep the lake_latitude/lake_longitude
                               #names in shinydata/lakesum so the Shiny app is unaffected
                               dplyr::select(Lakes, WBID, lake_latitude = lake_lat, lake_longitude = lake_long),
                               by = "WBID")

  usethis::use_data(shinydata, overwrite = TRUE)
} else {
  shinydata = DP2R::shinydata
}

#______________________________________________________________________________
#Effort_all: add questionnaire and survey estimates
#Lake names, area, region and location all come from DP2R::Lakes so every method uses
#the same lake area (Angler_days_p_ha) and naming.
Lake_info = DP2R::Lakes %>%
  dplyr::distinct(WBID, .keep_all = TRUE) %>%
  dplyr::select(WBID, lake_region = region_code, locale_name, gazetted_name, area_ha,
                lake_latitude = lake_lat, lake_longitude = lake_long)

#Region for lakes missing from DP2R::Lakes
wbid_region = DP2R::Mgt_Region_Unit %>%
  dplyr::filter(!is.na(WBID), !is.na(region_code)) %>%
  dplyr::distinct(WBID, mgt_region = region_code) %>%
  dplyr::distinct(WBID, .keep_all = TRUE)

region_lookup <- DP2R::Mgt_Region_Unit %>%
  dplyr::filter(!is.na(region_code)) %>%
  dplyr::distinct(region_code, RegionName) %>%
  dplyr::distinct(region_code, .keep_all = TRUE)

#Fallback region for each questionnaire if the WBID is in neither lookup
source_region = c(VILQ = "1", R2LQ = "2")

External = DP2R::Effort_external %>%
  dplyr::left_join(dplyr::select(Lake_info, WBID, lake_region, area_ha), by = "WBID") %>%
  dplyr::left_join(wbid_region, by = "WBID") %>%
  dplyr::mutate(region_code = dplyr::coalesce(lake_region, mgt_region, unname(source_region[source])),
                Angler_days_p_ha = round(Angler_days/area_ha, 1)) %>%
  dplyr::select(region_code, WBID, year, method, source, Angler_days, Angler_days_p_ha, area_ha)

Effort_all = shinydata %>%
  dplyr::mutate(source = method) %>%
  dplyr::bind_rows(External) %>%
  dplyr::select(-dplyr::any_of(c("gazetted_name", "locale_name", "lake_latitude", "lake_longitude", "RegionName"))) %>%
  dplyr::left_join(region_lookup, by = "region_code") %>%
  dplyr::left_join(dplyr::select(Lake_info, WBID, locale_name, gazetted_name, lake_latitude, lake_longitude),
                   by = "WBID") %>%
  dplyr::relocate(RegionName, .after = region_code) %>%
  dplyr::relocate(source, .after = method) %>%
  dplyr::relocate(locale_name, gazetted_name, .after = WBID) %>%
  dplyr::arrange(WBID, year, method) %>%
  as.data.frame()

print(dplyr::count(Effort_all, method, source))

#Lake by lake summary of all effort data
lakesum <- Effort_all %>%
  dplyr::group_by(region_code, WBID, locale_name, gazetted_name) %>%
  dplyr::summarise(N_years = length(unique(year)),
                   min_year = min(year),
                   max_year = max(year),
                   Methods = paste0(unique(method),collapse = ","),
                   mean_AD = round(mean(Angler_days, na.rm = TRUE),1),
                   marker_size = max(mean_AD,1, na.rm = TRUE),
                   .groups = "drop" )%>%
  dplyr::mutate(AD_percentile = 100*round(dplyr::min_rank(mean_AD)/dplyr::n(),2)) %>%
  dplyr::left_join(dplyr::select(Lake_info, WBID, lake_latitude, lake_longitude), by = "WBID") %>%
  as.data.frame()

usethis::use_data(Effort_all, overwrite = TRUE)
usethis::use_data(lakesum, overwrite = TRUE)
