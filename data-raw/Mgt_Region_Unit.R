#Functions and code to map lakes to wildlife management units or other atributes

# ---- Packages ----
library(bcdata)
library(sf)
library(dplyr)

# ---- Helpers ----
get_wmu <- function() {
  # Try the record name first; fall back to warehouse FC name if needed
  tryCatch(
    bcdata::bcdc_query_geodata("wildlife-management-units") |>
      dplyr::collect(),
    error = function(e)
      bcdata::bcdc_query_geodata("WHSE_WILDLIFE_MANAGEMENT.WAA_WILDLIFE_MGMT_UNITS_SVW") |>
      dplyr::collect()
  ) |>
    sf::st_make_valid() |>
    sf::st_transform(3005)
}

get_fwa_lakes <- function() {
  bcdata::bcdc_query_geodata("freshwater-atlas-lakes") |>
    dplyr::collect() |>
    sf::st_make_valid() |>
    sf::st_transform(3005)
}

get_fwa_watersheds <- function() {
  bcdata::bcdc_query_geodata("freshwater-atlas-watersheds") |>
    dplyr::collect() |>
    sf::st_make_valid() |>
    sf::st_transform(3005)
}

# ---- Lookups ----

# A) WMU from lon/lat (WGS84)
wmu_from_coords <- function(lon, lat, wmu = NULL) {
  if (is.null(wmu)) wmu <- get_wmu()
  pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326) |>
    sf::st_transform(3005)
  res <- sf::st_join(
    sf::st_as_sf(data.frame(id = 1), geometry = pt),
    wmu,
    join = sf::st_within,
    left = FALSE
  )
  res |> sf::st_drop_geometry()
}

# B) WMU from Freshwater Atlas WATERBODY_POLY_ID (lake polygons)
wmu_from_waterbody_id <- function(waterbody_poly_id, wmu = NULL, lakes = NULL) {
  if (is.null(wmu))   wmu   <- get_wmu()
  if (is.null(lakes)) lakes <- get_fwa_lakes()

  lake <- lakes |> dplyr::filter(WATERBODY_POLY_ID == !!waterbody_poly_id)
  if (nrow(lake) == 0) base::stop("No lake found for WATERBODY_POLY_ID = ", waterbody_poly_id)

  # Use intersects to catch boundary-touching cases; switch to st_within for strict containment
  res <- sf::st_join(lake, wmu, join = sf::st_intersects, left = FALSE)
  res |> sf::st_drop_geometry()
}

# C) WMU from Freshwater Atlas WATERSHED_CODE (watershed polygons)
wmu_from_watershed_code <- function(watershed_code, wmu = NULL, watersheds = NULL) {
  if (is.null(wmu))        wmu        <- get_wmu()
  if (is.null(watersheds)) watersheds <- get_fwa_watersheds()

  ws <- watersheds |> dplyr::filter(WATERSHED_CODE == !!watershed_code)
  if (nrow(ws) == 0) base::stop("No watershed found for WATERSHED_CODE = ", watershed_code)

  res <- sf::st_join(ws, wmu, join = sf::st_intersects, left = FALSE)
  res |> sf::st_drop_geometry()
}

# ---- Data preparation ----
fwa_lakes <- get_fwa_lakes()
wmu <- get_wmu()
DP2R(Tables = "vwWaterbodyLake")

# Reduce down to lakes in DataPond and select minimal useful columns
lakes <- fwa_lakes |>
  dplyr::filter(WATERBODY_KEY_GROUP_CODE_50K %in% vwWaterbodyLake$WBID) |>
  dplyr::select(WATERBODY_POLY_ID, GNIS_NAME_1, GNIS_NAME_2, GNIS_NAME_3, WATERBODY_KEY_GROUP_CODE_50K, geometry) |>
  dplyr::rename(WBID = WATERBODY_KEY_GROUP_CODE_50K)

#Rename to match other tables
wmu <- wmu |>
  dplyr::select(WILDLIFE_MGMT_UNIT_ID, REGION_RESPONSIBLE_ID, REGION_RESPONSIBLE_NAME, OBJECTID, geometry) |>
  dplyr::rename(
    Management_Unit = WILDLIFE_MGMT_UNIT_ID,
    region = REGION_RESPONSIBLE_ID,
    RegionName = REGION_RESPONSIBLE_NAME
  )

Mgt_Region_Unit <- wmu_from_waterbody_id(
  waterbody_poly_id = lakes$WATERBODY_POLY_ID,
  wmu = wmu,
  lakes = lakes
)

#which lakes were omitted
missing = setdiff(vwWaterbodyLake$WBID, Mgt_Region_Unit$WBID)
stocked = Rel_sum$WBID
miss_stocked <- intersect(missing, stocked)
head(miss_stocked)
length(miss_stocked)


## --- 1) Build a per-MU neighbour map (text string) -------------------------
# Set line_only = TRUE if you want to exclude point-touch neighbours.
build_neighbour_map <- function(wmu_obj = wmu, line_only = FALSE) {
  # adjacency index list (integers per row)
  nb_list <- if (isTRUE(line_only)) {
    # boundary-line touches only (DE-9IM mask)
    rel <- sf::st_relate(wmu_obj, wmu_obj, pattern = "F***1****")
    rel
  } else {
    sf::st_touches(wmu_obj)
  }

  # character vector aligned to rows of wmu_obj
  neighbour_vec <- vapply(
    X = seq_len(nrow(wmu_obj)),
    FUN.VALUE = character(1),
    FUN = function(i) {
      idx <- nb_list[[i]]
      if (length(idx) == 0) return("")
      base::paste(base::sort(base::as.character(wmu_obj$Management_Unit[idx])), collapse = ",")
    }
  )

  # tidy map for joining
  dplyr::tibble(
    Management_Unit = wmu_obj$Management_Unit,
    neighbour_mus   = neighbour_vec
  )
}

neighbour_map <- build_neighbour_map(wmu, line_only = FALSE)

# --- 2) Add the neighbour_mus column to your table -------------------------
Mgt_Region_Unit <- dplyr::left_join(
  Mgt_Region_Unit,
  neighbour_map,
  by = "Management_Unit"
)

path = "C:/Users/paul.askey/OneDrive - Freshwater Fisheries Society of B.C/FFSBC work docs/Git_projects/DP2R/data"
save(Mgt_Region_Unit, file = file.path(path, "Mgt_Region_Unit.rda"))

