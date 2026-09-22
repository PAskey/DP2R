# Create_FISS_Spp.R
# -----------------------------------------------------------------------------
# Build the stored `FISS_Spp` table: a rawer, per-observation table of known fish
# from the provincial FISS "Known BC Fish Observations" point layer, restricted
# to lakes DP2R knows about. Kept unsummarised so it can be summarised different
# ways later (e.g. first/last year, method filters); linkClips() derives its own
# first_obs/last_obs from this.
#
# WHY THIS IS A STORED TABLE:
#   The FISS observation layer is a large, province-wide dataset served from the
#   BC Data Catalogue. Downloading it is slow and it changes only occasionally,
#   so we run this ONCE (e.g. annually) and store the result.
#
# COLUMNS PRODUCED (one row per distinct observation kept):
#   WBID         Character. Waterbody identifier (matches DataPond WBID, which is
#                the provincial waterbody_identifier).
#   species_code Character. FISS species code as published in the catalogue.
#   year         Integer. Calendar year extracted from OBSERVATION_DATE.
#   activity     Character. Activity / sampling method of the observation
#                (FISS ACTIVITY field, e.g. angling, netting, electrofishing).
#   agency_name  Character. Agency that reported the observation (AGENCY_NAME).
#
# ROW FILTER: only POINT_TYPE_CODE == "Observation" is kept. FISS "Summary" rows
#   are duplicates of Observation rows (per smnorris/bcfishobs), and dropping them
#   is also expected to omit summarised/stocking-style records -- we already have
#   stocking directly from DataPond Releases. NOTE: any stocking-origin records
#   still coded as Observation would remain; the retained `activity` column lets
#   you filter those out downstream if needed.
#   Also EXCLUDES AGENCY_NAME == "FFSBC": FISS records no ACTIVITY for stocked
#   fish but tags them agency FFSBC, and all FFSBC data (stocking or sampling)
#   is already in DataPond. This is filtered server-side to avoid downloading
#   those rows; null-agency rows are kept.
#   Rows with a missing year or a future year (bad dates) are also dropped.
#
# RUN CADENCE: once a year.
# REQUIREMENTS: internet + `bcdata` and `sf` packages, plus the already-built
#   DP2R::Lakes and DP2R::Species package data. No DataPond/VPN needed.
# SCOPE: restricted to WBIDs in DP2R::Lakes and species_codes in DP2R::Species.
#
# ---- FIELD NAMES (verified 2026-09-04 against ArcGIS MapServer layer 20) ------
#   Layer WHSE_FISH.FISS_FISH_OBSRVTN_PNT_SP. Confirmed fields used here:
#     WATERBODY_IDENTIFIER  (the WBID; NOT the numeric WBODY_ID)
#     SPECIES_CODE
#     OBSERVATION_DATE      (one date per record -> `year`)
#     POINT_TYPE_CODE       (filter to "Observation")
#     ACTIVITY              (sampling method/activity)
#     AGENCY_NAME
#   If the catalogue schema ever drifts, re-check with:
#     bcdata::bcdc_describe_feature(FISS_RECORD)
# -----------------------------------------------------------------------------

library(bcdata)
library(sf)
library(dplyr)

## Known BC Fish Observations and BC Fish Distributions.
## Prefer the permanent id; fall back to the slug / warehouse object name.
FISS_RECORD  <- "known-bc-fish-observations-and-bc-fish-distributions"
FISS_WHSE_FC <- "WHSE_FISH.FISS_FISH_OBSRVTN_PNT_SP"  # observation POINTS layer

## Exclude the stocking agency. FISS leaves ACTIVITY blank for stocked fish but
## tags AGENCY_NAME = "FFSBC"; any FFSBC records (stocking or sampling) are
## already in DataPond, so we drop them here -- filtered SERVER-SIDE so those
## rows are never downloaded. Keep rows with a NULL agency (non-FFSBC, unknown).
EXCLUDE_AGENCY  <- "FFSBC"
AGENCY_EXCL_CQL <- sprintf("(AGENCY_NAME IS NULL OR AGENCY_NAME <> '%s')", EXCLUDE_AGENCY)

## Map raw catalogue columns -> our stored schema, and keep only Observation rows.
## Central place to fix if the catalogue field names change.
standardise_fiss <- function(dat) {
  nm <- names(dat)
  pick <- function(...) {
    cand <- c(...)
    hit <- cand[cand %in% nm]
    if (length(hit) == 0L) {
      stop("None of these expected columns were found in the FISS layer: ",
           paste(cand, collapse = ", "),
           "\nColumns present: ", paste(nm, collapse = ", "),
           call. = FALSE)
    }
    hit[1]
  }
  wbid_col   <- pick("WATERBODY_IDENTIFIER", "WATERBODY_ID")
  spp_col    <- pick("SPECIES_CODE")
  date_col   <- pick("OBSERVATION_DATE", "OBSERVATION_DATE_TIME", "OBS_DATE")
  pt_col     <- pick("POINT_TYPE_CODE")
  act_col    <- pick("ACTIVITY", "ACTIVITY_CODE")
  agency_col <- pick("AGENCY_NAME")

  dat %>%
    dplyr::filter(trimws(as.character(.data[[pt_col]])) == "Observation") %>%
    dplyr::transmute(
      WBID         = trimws(as.character(.data[[wbid_col]])),
      species_code = trimws(as.character(.data[[spp_col]])),
      year         = as.integer(format(as.Date(.data[[date_col]]), "%Y")),
      activity     = trimws(as.character(.data[[act_col]])),
      agency_name  = trimws(as.character(.data[[agency_col]]))
    )
}

## 1. Restrict to WBIDs in the DP2R Lakes table, so we do not download the whole
##    province and so FISS_Spp only ever covers lakes DP2R knows about. Uses the
##    already-built package data (no live DataPond pull needed here). Server-side
##    CQL filtering is chunked to keep request sizes sane.
wbids <- unique(trimws(as.character(DP2R::Lakes$WBID)))
wbids <- wbids[!is.na(wbids) & wbids != ""]

fetch_fiss <- function(record, wbids = NULL, chunk_size = 200) {
  q <- tryCatch(
    bcdata::bcdc_query_geodata(record),
    error = function(e) bcdata::bcdc_query_geodata(FISS_WHSE_FC)
  )

  # No restriction: pull the whole layer (large) and standardise.
  if (is.null(wbids) || length(wbids) == 0L) {
    q <- q %>% dplyr::filter(bcdata::CQL(AGENCY_EXCL_CQL))
    return(standardise_fiss(sf::st_drop_geometry(dplyr::collect(q))))
  }

  # Restricted: filter server-side on WATERBODY_IDENTIFIER in chunks.
  chunks <- split(wbids, ceiling(seq_along(wbids) / chunk_size))
  res <- lapply(chunks, function(x) {
    x2     <- gsub("'", "''", x)
    in_sql <- paste0("'", x2, "'", collapse = ",")
    cql    <- sprintf("WATERBODY_IDENTIFIER IN (%s) AND %s", in_sql, AGENCY_EXCL_CQL)
    dat <- bcdata::bcdc_query_geodata(record) %>%
      dplyr::filter(bcdata::CQL(cql)) %>%
      dplyr::collect()
    if (identical(names(dat), "geometry") || nrow(dat) == 0L) return(NULL)
    standardise_fiss(sf::st_drop_geometry(dat))
  })
  dplyr::bind_rows(res)
}

fiss_raw <- fetch_fiss(FISS_RECORD, wbids = wbids)

## 2. Keep the per-observation rows (no year summarising). Drop rows with no WBID
##    or species, drop data-quality bad years (missing, or in the future), and
##    collapse exact duplicates on the retained columns.
this_year <- as.integer(format(Sys.Date(), "%Y"))
FISS_Spp <- fiss_raw %>%
  dplyr::filter(!is.na(WBID), WBID != "",
                !is.na(species_code), species_code != "") %>%
  # belt-and-braces: drop any FFSBC rows the server filter missed.
  dplyr::filter(is.na(agency_name) | agency_name != EXCLUDE_AGENCY) %>%
  # data quality: drop observations with no year, or a year in the future.
  dplyr::filter(!is.na(year), year <= this_year) %>%
  dplyr::distinct(WBID, species_code, year, activity, agency_name) %>%
  dplyr::arrange(WBID, species_code, year)

## 3. Keep only species and lakes that DP2R already recognises: species_code
##    present in DP2R::Species, and WBID present in DP2R::Lakes. (The download is
##    already restricted to Lakes WBIDs, so the WBID filter is a belt-and-braces
##    guard; the species filter drops FISS codes DP2R does not carry.)
valid_spp  <- unique(trimws(as.character(DP2R::Species$species_code)))
valid_wbid <- unique(trimws(as.character(DP2R::Lakes$WBID)))
n_before <- nrow(FISS_Spp)
FISS_Spp <- FISS_Spp %>%
  dplyr::filter(species_code %in% valid_spp, WBID %in% valid_wbid)
message(sprintf("FISS_Spp: kept %d of %d observation rows after filtering to DP2R::Species / DP2R::Lakes.",
                nrow(FISS_Spp), n_before))

## 4. Save as package data.
usethis::use_data(FISS_Spp, overwrite = TRUE)
