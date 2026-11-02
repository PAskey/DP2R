#Build Effort_external: angler effort estimates that do NOT come from DataPond counts
#(questionnaires and surveys), standardized to one row per WBID x year x source.
#
#These are static, rarely-updated files, so the table is stored as package data and
#only rebuilt when a source file changes (same pattern as Elev_ice_estimates / FISS_Spp).
#Run Create_Shiny_Effort.R afterwards to fold it into Effort_all and lakesum.
#
#Sources (data-raw/effort_sources/):
#  VILQ_lakecode_data_master_2024.csv  Vancouver Island Lake Questionnaire (method QNR, source VILQ)
#  R2LQ_master.csv                     Region 2 Lake Questionnaire          (method QNR, source R2LQ)
#  iSEA_Effort.csv                     DFO iSEA angler survey, 2023-2024    (method SVY, source DFO)
#                                      Has lake names + Management Unit but almost no WBIDs,
#                                      so WBIDs are matched below (was Angler_Effort/data/Convert_iSEA_data.R)
#
#Duplicate WBID x year rows within a source (e.g. two VILQ lake codes mapped to one WBID)
#are SUMMED, so each source contributes one estimate per lake-year.

#Run from the DP2R project folder (or via DP2Rdata_refresh()).

library(DP2R)
library(dplyr)
library(stringr)
library(tidyr)

src <- "data-raw/effort_sources"
bad_wbid <- function(x) is.na(x) | x %in% c("", "NA", "UN")

#______________________________________________________________________________
#Vancouver Island Lake Questionnaire
VILQ <- read.csv(file.path(src, "VILQ_lakecode_data_master_2024.csv"),
                 fileEncoding = "UTF-8-BOM", colClasses = c(WBID = "character")) %>%
  dplyr::filter(!bad_wbid(WBID)) %>%
  dplyr::transmute(WBID, year = as.integer(YEAR), method = "QNR", source = "VILQ",
                   Angler_days = suppressWarnings(as.numeric(DAYS.FISHED)))

#______________________________________________________________________________
#Region 2 Lake Questionnaire
R2LQ <- read.csv(file.path(src, "R2LQ_master.csv"),
                 fileEncoding = "UTF-8-BOM", colClasses = c(WBID = "character")) %>%
  dplyr::filter(!bad_wbid(WBID)) %>%
  dplyr::transmute(WBID, year = as.integer(year), method = "QNR", source = "R2LQ",
                   Angler_days = suppressWarnings(as.numeric(DAYS.FISHED)))

#______________________________________________________________________________
#DFO iSEA survey: assign WBID using Mgt_Region_Unit
#Matching logic (in priority order):
#  1) DIRECT:       gazetted_name + Management_Unit (after "_" -> "-")
#  2) UNIQUE NAME:  gazetted_name only, when that name maps to exactly ONE WBID province-wide
#  3) NEIGHBOUR MU: gazetted_name exists and the iSEA MU is in that lake's neighbour_mus
iSEA <- read.csv(file.path(src, "iSEA_Effort.csv"), fileEncoding = "UTF-8-BOM")

clean_name <- function(x) stringr::str_squish(as.character(x))
clean_mu   <- function(x) stringr::str_squish(stringr::str_replace_all(as.character(x), "_", "-"))

mgt_key <- DP2R::Mgt_Region_Unit %>%
  dplyr::mutate(gazetted_name = clean_name(gazetted_name),
                Management_Unit = clean_mu(Management_Unit)) %>%
  dplyr::filter(!bad_wbid(WBID)) %>%
  dplyr::select(gazetted_name, Management_Unit, WBID, neighbour_mus)

unique_name_lookup <- mgt_key %>%
  dplyr::filter(!is.na(gazetted_name), gazetted_name != "") %>%
  dplyr::distinct(gazetted_name, WBID) %>%
  dplyr::group_by(gazetted_name) %>%
  dplyr::filter(dplyr::n() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::rename(unique_WBID = WBID)

mgt_neigh_long <- mgt_key %>%
  dplyr::mutate(neighbour_mus = stringr::str_replace_all(neighbour_mus, "\\s+", "")) %>%
  tidyr::separate_rows(neighbour_mus, sep = ",") %>%
  dplyr::mutate(neighbour_mu = clean_mu(neighbour_mus)) %>%
  dplyr::filter(!is.na(neighbour_mu), neighbour_mu != "") %>%
  dplyr::distinct(gazetted_name, neighbour_mu, WBID)

isea_prep <- iSEA %>%
  dplyr::mutate(.rid = dplyr::row_number(),
                gazetted_name = clean_name(gazetted_name),
                Management_Unit = clean_mu(Management_Unit))

direct <- isea_prep %>%
  dplyr::select(.rid, gazetted_name, Management_Unit) %>%
  dplyr::inner_join(dplyr::distinct(mgt_key, gazetted_name, Management_Unit, direct_WBID = WBID),
                    by = c("gazetted_name", "Management_Unit"), relationship = "many-to-many") %>%
  #Several lakes can share a name within an MU. The old version gave the full estimate to
  #EACH of them (double counting); assign it to the largest lake of that name instead.
  dplyr::left_join(dplyr::distinct(DP2R::Lakes, direct_WBID = WBID, .keep_all = TRUE) %>%
                     dplyr::select(direct_WBID, area_ha), by = "direct_WBID") %>%
  dplyr::group_by(.rid) %>%
  dplyr::mutate(n_direct = dplyr::n()) %>%
  dplyr::arrange(dplyr::desc(area_ha), .by_group = TRUE) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(.rid, direct_WBID, n_direct)
message(sum(direct$n_direct > 1), " iSEA records matched >1 lake by name + MU; assigned to the largest")

name_only <- isea_prep %>%
  dplyr::filter(!.rid %in% direct$.rid) %>%
  dplyr::select(.rid, gazetted_name) %>%
  dplyr::inner_join(unique_name_lookup, by = "gazetted_name") %>%
  dplyr::select(.rid, unique_WBID)

rescue_raw <- isea_prep %>%
  dplyr::filter(!.rid %in% c(direct$.rid, name_only$.rid)) %>%
  dplyr::select(.rid, gazetted_name, Management_Unit) %>%
  dplyr::inner_join(mgt_neigh_long, by = c("gazetted_name", "Management_Unit" = "neighbour_mu"),
                    relationship = "many-to-many") %>%
  dplyr::distinct(.rid, WBID)

rescue_ambiguous <- rescue_raw %>% dplyr::count(.rid) %>% dplyr::filter(n > 1)

rescue <- rescue_raw %>%
  dplyr::filter(!.rid %in% rescue_ambiguous$.rid) %>%
  dplyr::rename(rescue_WBID = WBID)

isea_matched <- isea_prep %>%
  dplyr::left_join(direct, by = ".rid") %>%
  dplyr::left_join(name_only, by = ".rid") %>%
  dplyr::left_join(rescue, by = ".rid") %>%
  dplyr::mutate(WBID_given = dplyr::if_else(bad_wbid(WBID), NA_character_, as.character(WBID)),
                WBID = dplyr::coalesce(WBID_given, direct_WBID, unique_WBID, rescue_WBID),
                match_type = dplyr::case_when(!is.na(WBID_given)  ~ "given",
                                              !is.na(direct_WBID) ~ "direct",
                                              !is.na(unique_WBID) ~ "unique_name",
                                              !is.na(rescue_WBID) ~ "neighbour_mu",
                                              TRUE ~ "unmatched"))

print(dplyr::count(isea_matched, match_type, sort = TRUE))
iSEA_unmatched <- isea_matched %>%
  dplyr::filter(match_type == "unmatched") %>%
  dplyr::distinct(gazetted_name, Management_Unit, RegionName)#inspect if needed

DFO <- isea_matched %>%
  dplyr::filter(!is.na(WBID)) %>%
  dplyr::transmute(WBID, year = as.integer(year), method = "SVY", source = "DFO",
                   Angler_days = as.numeric(Angler_days))

#______________________________________________________________________________
#Combine; one row per WBID x year x source (duplicates summed)
Effort_external <- dplyr::bind_rows(VILQ, R2LQ, DFO) %>%
  dplyr::group_by(WBID, year, method, source) %>%
  dplyr::summarise(N_records = dplyr::n(),
                   Angler_days = if (all(is.na(Angler_days))) NA_real_ else sum(Angler_days, na.rm = TRUE),
                   .groups = "drop") %>%
  dplyr::arrange(source, WBID, year) %>%
  as.data.frame()

print(dplyr::count(Effort_external, method, source))
message(sum(Effort_external$N_records > 1), " lake-years had duplicate records that were summed")

usethis::use_data(Effort_external, overwrite = TRUE)
