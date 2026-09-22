# data-raw/Update_bathymetry_links.R
# -----------------------------------------------------------------------------
# Refresh data-raw/bathymetry_links.csv from the authoritative BC open dataset:
#   "Bathymetric Maps - Open - Reference Table and Maps" (Open Government Licence)
#   https://catalogue.data.gov.bc.ca/dataset/1427d389-cd21-4fe2-8ed9-282d9bdcb7e2
#
# The reference table is keyed by WBID (WATERBODY_IDENTIFIER_WSA_50K) and lists
# every open-licence bathymetric map PDF in the province -- a superset of the
# original 468-lake file. Lakes with multiple map sheets appear as multiple rows;
# we collapse them to ONE row per WBID with the sheet URLs joined by "|" (pipe).
#
# Pipe is used because it never occurs inside a URL (commas collide with CSV
# fields; "&", "=", ";" can appear in query strings). In the Shiny app, split with:
#     urls <- strsplit(bathymetry_url, "|", fixed = TRUE)[[1]]
#
# This keeps bathymetry_links.csv 1:1 with WBID, so Create_Lakes.R still joins
# cleanly without duplicating Lakes rows.
# -----------------------------------------------------------------------------

library(dplyr)
library(readr)

# Resolve the current CSV download URL at runtime from the CKAN API using the
# dataset's PERMANENT id. This survives re-publishing (the resource id and dated
# filename in the direct download URL can change; the dataset id does not).
dataset_id <- "1427d389-cd21-4fe2-8ed9-282d9bdcb7e2"
pkg <- jsonlite::fromJSON(
  paste0("https://catalogue.data.gov.bc.ca/api/3/action/package_show?id=", dataset_id)
)
res <- pkg$result$resources
ref_url <- res$url[res$format == "CSV" &
                     grepl("Reference Table and Maps", res$name, ignore.case = TRUE)][1]
if (is.na(ref_url) || length(ref_url) == 0) {
  stop("Could not find the 'Reference Table and Maps' CSV resource; ",
       "check the dataset at https://catalogue.data.gov.bc.ca/dataset/", dataset_id)
}
message("Resolved reference table URL:\n  ", ref_url)

# Read everything as character so WBID leading zeros are preserved.
# (If your network needs a proxy, download.file(ref_url, tmp) first, then read tmp.)
bathy_raw <- readr::read_csv(ref_url, col_types = readr::cols(.default = "c"))
message("Columns in reference table:"); print(names(bathy_raw))

# --- Per-sheet table ---------------------------------------------------------
bathy_sheets <- bathy_raw %>%
  transmute(
    WBID         = WATERBODY_IDENTIFIER_WSA_50K,
    map_title    = MAP_TITLE,
    draft_date   = DRAFT_DATE,
    sheet_no     = suppressWarnings(as.integer(SHEET_NO)),
    pdf_filename = MAP_IMAGE_FILENAME_PDF,
    # Rebuild the a100 download URL to match your existing 468 links.
    # To use the catalogue's own published link instead, replace the next line with:
    #     url = PDF_FILE_URL
    url = paste0("https://a100.gov.bc.ca/pub/fidq/downloadBathymetricMap.do?filename=",
                 MAP_IMAGE_FILENAME_PDF)
  ) %>%
  filter(!is.na(WBID), !is.na(pdf_filename)) %>%
  arrange(WBID, sheet_no)

# --- Collapse to one row per WBID (all sheets, pipe-delimited) ----------------
bathymetry_links <- bathy_sheets %>%
  group_by(WBID) %>%
  summarise(
    n_sheets       = dplyr::n(),
    bathymetry_url = paste(url, collapse = "|"),
    .groups = "drop"
  )

# --- Coverage / sanity report ------------------------------------------------
old <- readr::read_csv("data-raw/bathymetry_links.csv",
                       col_types = readr::cols(.default = "c"))

cat("\n--- Coverage ---\n")
cat("WBIDs in new complete table :", nrow(bathymetry_links), "\n")
cat("WBIDs in your existing file :", nrow(old), "\n")
cat("New WBIDs gained            :", length(setdiff(bathymetry_links$WBID, old$WBID)), "\n")
cat("In existing but NOT in catalogue (worth checking):",
    length(setdiff(old$WBID, bathymetry_links$WBID)), "\n")
cat("Multi-sheet lakes           :", sum(bathymetry_links$n_sheets > 1), "\n")

# Coverage against your app lakes (uses the stored Lakes dataset)
if (requireNamespace("DP2R", quietly = TRUE)) {
  utils::data("Lakes", package = "DP2R", envir = environment())
  app_wbids <- unique(Lakes$WBID)
  have <- intersect(app_wbids, bathymetry_links$WBID)
  cat("\nApp lakes (Lakes$WBID)      :", length(app_wbids), "\n")
  cat("  with a bathymetric map    :", length(have),
      sprintf("(%.1f%%)\n", 100 * length(have) / length(app_wbids)))
  cat("  without a map (likely unsurveyed):",
      length(setdiff(app_wbids, bathymetry_links$WBID)), "\n")
}

# --- Write refreshed file ----------------------------------------------------
readr::write_csv(bathymetry_links %>% select(WBID, bathymetry_url),
                 "data-raw/bathymetry_links.csv")
cat("\nWrote data-raw/bathymetry_links.csv with", nrow(bathymetry_links), "lakes.\n")

# NOTE: if a lake has been re-surveyed, the table can contain sheets from more
# than one DRAFT_DATE. All are kept here (you wanted every sheet). To keep only
# the most recent survey per lake, filter bathy_sheets to max(draft_date) per
# WBID before the collapse.
