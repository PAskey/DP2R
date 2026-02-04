#' Find WMU, Region, and RegionName from coordinates with WBID fallback
#'
#' Efficiently populates Wildlife Management Unit (WMU) and associated region fields.
#' Uses WGS84 coordinates first (fast point-in-polygon), then fills remaining gaps using
#' WBID -> Freshwater Atlas lake polygons (server-side filtered) -> WMU.
#'
#' Designed for vectorized use on a whole column. Handles "fake" WBIDs by leaving
#' fallback results as NA (but still uses coords if present).
#'
#' @param lon Numeric vector of longitudes (EPSG:4326).
#' @param lat Numeric vector of latitudes (EPSG:4326).
#' @param WBID Character vector of WBIDs (expected to match FWA \code{WATERBODY_KEY_GROUP_CODE_50K}).
#' @param wmu Optional \code{sf} WMU polygon layer. If NULL, fetched via \pkg{bcdata}.
#' @param join Spatial predicate, one of \code{"intersects"} or \code{"within"}. Default \code{"intersects"}.
#' @param crs_in EPSG for input coords (default 4326).
#' @param crs_work EPSG for spatial work (default 3005).
#' @param lakes_record BC Data record for FWA lakes (default \code{"freshwater-atlas-lakes"}).
#' @param lakes_wbid_field Field in lakes layer containing WBID (default \code{"WATERBODY_KEY_GROUP_CODE_50K"}).
#' @param wbid_chunk_size Max WBIDs per WFS request (default 400; reduce to 100 if service is flaky).
#' @param wmu_id_field Column in \code{wmu} holding WMU id (default \code{"Management_Unit"}).
#' @param region_field Column in \code{wmu} holding region id (default \code{"region"}).
#' @param region_name_field Column in \code{wmu} holding region name (default \code{"RegionName"}).
#'
#' @return A \code{data.frame} with columns: \code{wmu}, \code{region}, \code{RegionName}.
#'   Same number of rows as the longest input among \code{lon/lat/WBID}.
#'
#' @export
find_region_wmu <- function(lon = NULL,
                            lat = NULL,
                            WBID = NULL,
                            wmu = NULL,
                            lakes_fetch = NULL,
                            join = c("intersects", "within"),
                            crs_in = 4326,
                            crs_work = 3005,
                            lakes_wbid_field = "WATERBODY_KEY_GROUP_CODE_50K",
                            wmu_id_field = "Management_Unit",
                            region_field = "region",
                            region_name_field = "RegionName") {

  join <- base::match.arg(join)
  join_fun <- if (join == "within") sf::st_within else sf::st_intersects
  crs_target <- sf::st_crs(crs_work)

  if (base::is.null(lakes_fetch)) {
    base::stop("Provide `lakes_fetch`, a function that takes WBID vector and returns an sf with attributes + geometry.")
  }

  # --- length + recycling ---
  n <- base::max(
    if (!base::is.null(lon)) base::length(lon) else 0L,
    if (!base::is.null(lat)) base::length(lat) else 0L,
    if (!base::is.null(WBID)) base::length(WBID) else 0L
  )
  if (n == 0L) base::stop("Provide at least one of: lon/lat, WBID")

  .recycle <- function(x) {
    if (base::is.null(x)) return(NULL)
    if (base::length(x) == 1L && n > 1L) return(base::rep(x, n))
    if (base::length(x) != n) base::stop("Inputs must have same length or be length 1.")
    x
  }
  lon <- .recycle(lon); lat <- .recycle(lat); WBID <- .recycle(WBID)

  # --- WMU load ---
  if (base::is.null(wmu)) {
    wmu <- base::tryCatch(
      bcdata::bcdc_query_geodata("wildlife-management-units") |>
        dplyr::collect(),
      error = function(e) {
        bcdata::bcdc_query_geodata("WHSE_WILDLIFE_MANAGEMENT.WAA_WILDLIFE_MGMT_UNITS_SVW") |>
          dplyr::collect()
      }
    ) |>
      sf::st_make_valid()
  }
  wmu <- sf::st_transform(wmu, crs_target)
  sf::st_crs(wmu) <- crs_target

  # resolve fields
  nm <- base::names(wmu)
  if (!wmu_id_field %in% nm && "WILDLIFE_MGMT_UNIT_ID" %in% nm) wmu_id_field <- "WILDLIFE_MGMT_UNIT_ID"
  if (!region_field %in% nm && "REGION_RESPONSIBLE_ID" %in% nm) region_field <- "REGION_RESPONSIBLE_ID"
  if (!region_name_field %in% nm && "REGION_RESPONSIBLE_NAME" %in% nm) region_name_field <- "REGION_RESPONSIBLE_NAME"

  wmu_df <- sf::st_drop_geometry(wmu)
  wmu_lut <- dplyr::tibble(
    wmu = base::as.character(wmu_df[[wmu_id_field]]),
    region = if (region_field %in% base::names(wmu_df)) base::as.character(wmu_df[[region_field]]) else NA_character_,
    RegionName = if (region_name_field %in% base::names(wmu_df)) base::as.character(wmu_df[[region_name_field]]) else NA_character_
  ) |>
    dplyr::distinct()

  out_wmu <- base::rep(NA_character_, n)

  # 1) coords -> WMU
  if (!base::is.null(lon) && !base::is.null(lat)) {
    ok <- !base::is.na(lon) & !base::is.na(lat)
    if (base::any(ok)) {
      pts <- sf::st_as_sf(
        data.frame(.row = base::which(ok), lon = lon[ok], lat = lat[ok]),
        coords = c("lon", "lat"),
        crs = sf::st_crs(crs_in),
        remove = FALSE
      ) |>
        sf::st_transform(crs_target)
      sf::st_crs(pts) <- crs_target

      j <- sf::st_join(pts, wmu, join = join_fun, left = TRUE) |>
        sf::st_drop_geometry() |>
        dplyr::group_by(.row) |>
        dplyr::slice(1) |>
        dplyr::ungroup()

      out_wmu[j$.row] <- base::as.character(j[[wmu_id_field]])
    }
  }

  # 2) WBID fallback for missing only
  need_wbid <- base::is.na(out_wmu) &
    !base::is.null(WBID) &
    !base::is.na(WBID) &
    base::trimws(base::as.character(WBID)) != ""

  if (base::any(need_wbid)) {
    wb_need <- base::unique(base::trimws(base::as.character(WBID[need_wbid])))

    lk <- lakes_fetch(wb_need)
    if (!is.null(lk) && nrow(lk) > 0) {
      lk <- sf::st_make_valid(lk) |> sf::st_transform(crs_target)
      sf::st_crs(lk) <- crs_target

      if (!lakes_wbid_field %in% base::names(lk)) {
        base::stop("lakes_fetch did not return WBID field '", lakes_wbid_field, "'. Returned: ",
                   base::paste(base::names(lk), collapse = ", "))
      }

      sub <- lk |>
        dplyr::transmute(.WBID = base::trimws(base::as.character(.data[[lakes_wbid_field]])), geometry)

      j <- sf::st_join(sub, wmu, join = join_fun, left = TRUE) |>
        sf::st_drop_geometry() |>
        dplyr::group_by(.WBID) |>
        dplyr::slice(1) |>
        dplyr::ungroup()

      map <- stats::setNames(base::as.character(j[[wmu_id_field]]), j$.WBID)

      out_wmu[need_wbid] <- map[base::trimws(base::as.character(WBID[need_wbid]))]
    }
  }

  out <- dplyr::tibble(wmu = out_wmu) |>
    dplyr::left_join(wmu_lut, by = "wmu")

  base::as.data.frame(out, stringsAsFactors = FALSE)
}
