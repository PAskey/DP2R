setwd("C:/Users/paul.askey/OneDrive - Freshwater Fisheries Society of B.C/FFSBC work docs/Git_projects/DP2R")

library(DP2R)
library(dplyr)
EffortEst(update.model = TRUE)

shinydata = EffortEsts

shinydata = dplyr::left_join(shinydata,
                             Lakes[,c("WBID","lake_latitude","lake_longitude")],
                             by = "WBID")

#Lake by lake summary of effort data
lakesum <- shinydata %>%
  dplyr::group_by(region_code, WBID, gazetted_name, lake_latitude,lake_longitude) %>%
  dplyr::summarise(N_years = length(unique(year)),
                   min_year = min(year),
                   max_year = max(year),
                   Methods = paste0(unique(method),collapse = ","),
                   mean_AD = round(mean(Angler_days, na.rm = TRUE),1),
                   marker_size = max(mean_AD,1, na.rm = TRUE),
                   .groups = "drop" )%>%
  dplyr::mutate(AD_percentile = 100*round(dplyr::min_rank(mean_AD)/dplyr::n(),2))


save(shinydata, file = "data/shinydata.rda")
save(lakesum, file = "data/lakesum.rda")

