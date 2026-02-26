#' Gillnet selectivity at fork length
#'
#' Returns relative gillnet selectivity (0--1) for one or more fish given
#' species code, fork length (mm), and either sample design code OR
#' Sample_event (which can have multiple sample_design_codes in the event).
#'
#' Curves are stored on a shared length grid \code{sel_classes}.
#' Species without direct curves are mapped to proxy species using \code{approx_select_spp}.
#'
#' @param species Species code(s).
#' @param length_mm Fork length(s) in mm.
#' @param sample_design_code Net/sample design code(s). Defaults to \code{"SGN7"}.
#' @param Sample_event Optional Sample_event id(s). If provided, curves are pulled from
#'   \code{sel_lookup_event} instead of \code{sel_lookup}.
#'
#' @return Numeric vector of selectivity values (0--1) or NA.
#'
#' @examples
#' GN_select("RB", 312, sample_design_code = "SGN7")
#' GN_select("RB", 312, Sample_event = "GN_00394CHIL_SON")
#'
#' @export
GN_select <- function(species,
                      length_mm,
                      sample_design_code = "SGN7",
                      Sample_event = NULL) {

  data("sel_lookup", "sel_lookup_event", "sel_classes", "approx_select_spp",
       envir = environment())

  sel_lookup       <- get("sel_lookup", envir = environment(), inherits = FALSE)
  sel_classes      <- get("sel_classes", envir = environment(), inherits = FALSE)
  approx_tbl       <- get("approx_select_spp", envir = environment(), inherits = FALSE)
  sel_lookup_event <- get("sel_lookup_event", envir = environment(), inherits = FALSE)


  sp_req <- as.character(species)
  L <- as.numeric(length_mm)

  # Choose lookup + key based on Sample_event presence
  use_event <- !is.null(Sample_event)

  if (use_event) {
    if (is.null(sel_lookup_event)) {
      stop("Sample_event was provided but sel_lookup_event is not available in package data.")
    }
    lookup_tbl <- sel_lookup_event
    key_name   <- "Sample_event"
    key_val    <- as.character(Sample_event)
    modeled    <- unique(as.character(sel_lookup_event$species_code))
  } else {
    lookup_tbl <- sel_lookup
    key_name   <- "sample_design_code"
    key_val    <- as.character(sample_design_code)
    modeled    <- unique(as.character(sel_lookup$species_code))
  }

  # ---- Approximate species only if not directly modeled (relative to chosen lookup) ----
  needs <- !(sp_req %in% modeled)

  map <- stats::setNames(as.character(approx_tbl$select_spp),
                         as.character(approx_tbl$species_code))

  sp_use <- sp_req
  sp_use[needs] <- dplyr::coalesce(unname(map[sp_req[needs]]), sp_req[needs])

  # ---- Join curves ----
  if (use_event) {
    q <- tibble::tibble(
      species_code = sp_use,
      Sample_event = key_val,
      length_mm = L
    ) %>%
      dplyr::left_join(lookup_tbl, by = c("species_code", "Sample_event"))
  } else {
    q <- tibble::tibble(
      species_code = sp_use,
      sample_design_code = key_val,
      length_mm = L
    ) %>%
      dplyr::left_join(lookup_tbl, by = c("species_code", "sample_design_code"))
  }

  out <- rep(NA_real_, nrow(q))

  # ---- Index into shared sel_classes ----
  classes_min <- as.integer(sel_classes[1])
  idx <- as.integer(round(q$length_mm)) - classes_min + 1L

  ncls <- length(sel_classes)
  ok <- !purrr::map_lgl(q$curve, is.null) &
    !is.na(idx) &
    idx >= 1L &
    idx <= ncls

  out[ok] <- purrr::map2_dbl(q$curve[ok], idx[ok], \(v, i) v[[i]])
  out
}
