#' Gillnet selectivity at fork length
#'
#' Returns relative gillnet selectivity (0--1) for one or more fish given
#' species code, fork length (mm), and sample design code.
#'
#' Selectivity curves are stored in \code{sel_lookup} on a shared length grid
#' \code{sel_classes}. Species without direct curves are automatically mapped
#' to proxy species using \code{approx_select_spp}.
#'
#' @param species Species code(s).
#' @param length_mm Fork length(s) in mm.
#' @param sample_design_code Net/sample design code(s). Defaults to \code{"SGN7"}.
#'
#' @return Numeric vector of selectivity values. Returns \code{NA_real_} when:
#' \itemize{
#'   \item \code{length_mm} is missing,
#'   \item length is outside \code{sel_classes},
#'   \item no curve exists for the (species, design) pair even after approximation.
#' }
#'
#' @examples
#' GN_select("RB", 312, "SGN7")
#' GN_select(c("RB","BT"), c(312, 580), "SGN7")
#'
#' @export
GN_select <- function(species,
                      length_mm,
                      sample_design_code = "SGN7") {

  data("sel_lookup", "sel_classes", "approx_select_spp", envir = environment())
  sel_lookup  <- get("sel_lookup", envir = environment(), inherits = FALSE)
  sel_classes <- get("sel_classes", envir = environment(), inherits = FALSE)
  approx_tbl  <- get("approx_select_spp", envir = environment(), inherits = FALSE)

  sp_req <- as.character(species)
  L <- as.numeric(length_mm)
  design <- as.character(sample_design_code)

  # ---- Approximate species only if not directly modeled ----
  modeled <- unique(as.character(sel_lookup$species_code))
  needs <- !(sp_req %in% modeled)

  map <- stats::setNames(as.character(approx_tbl$select_spp),
                         as.character(approx_tbl$species_code))

  sp_use <- sp_req
  sp_use[needs] <- dplyr::coalesce(unname(map[sp_req[needs]]), sp_req[needs])

  # ---- Join curves ----
  q <- tibble::tibble(
    species_code = sp_use,
    sample_design_code = design,
    length_mm = L
  ) %>%
    dplyr::left_join(sel_lookup, by = c("species_code", "sample_design_code"))

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
