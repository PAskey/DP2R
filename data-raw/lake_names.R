#A data set to consolidate gazetted names and alias into a single prefferred name for each lake (without the word "lake")
library(dplyr)#needed for %>% and dplyr verbs when run on its own or by DP2Rdata_refresh()

DP2R::DP2R(Tables = "vwWaterbody")

#Aliases: vwWaterbody.aliases is an ntext column, which DP2R() skips, so query it directly,
#converted to nvarchar(max). Lakes with several vwWaterbody rows (e.g. one per access point)
#get their distinct alias entries combined into one string. If the query fails, keep the
#aliases already stored in DP2R::lake_names so they are not lost.
alias_sql <- paste("SELECT waterbody_identifier AS WBID, CAST(aliases AS nvarchar(max)) AS aliases",
                   "FROM dbo.vwWaterbody WHERE aliases IS NOT NULL")
wb_alias <- tryCatch(DBI::dbGetQuery(conn, alias_sql), error = function(e) {
  warning("Could not read vwWaterbody.aliases (", conditionMessage(e),
          "); keeping aliases from the stored DP2R::lake_names.", call. = FALSE)
  NULL
})
if (!is.null(wb_alias)) {
  wb_alias <- wb_alias %>%
    dplyr::mutate(aliases = trimws(aliases)) %>%
    dplyr::filter(!is.na(WBID), !is.na(aliases), aliases != "", aliases != "NA") %>%
    dplyr::group_by(WBID) %>%
    dplyr::summarise(alias = paste(unique(aliases), collapse = "; "), .groups = "drop")
} else {
  wb_alias <- DP2R::lake_names %>%
    dplyr::select(WBID, alias)
}

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
