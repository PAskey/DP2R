#' Add gillnet selectivity to Biological records
#'
#' Adds a column \code{select} to a Biological data frame using the
#' precomputed selectivity lookup table \code{sel_lookup}.
#'
#' For fish associated with a gillnet sample design present in
#' \code{sel_lookup}, selectivity is extracted based on species,
#' net design, and fish length. All other records (non-gillnet
#' methods or missing design information) receive a selectivity
#' value of 1.
#'
#' @param Biological A data frame of individual fish records. Must contain
#'   \code{fish_collection_id}, \code{species_code}, and \code{length_mm}.
#' @param Collections A data frame linking \code{fish_collection_id} to
#'   \code{sample_design_code} (and optionally method).
#' @param sel_lookup Optional selectivity lookup table. If \code{NULL},
#'   the function loads \code{sel_lookup} from package data.
#' @param classes_min Minimum length (mm) used in selectivity curves.
#' @param classes_max Maximum length (mm) used in selectivity curves.
#'
#' @return The input \code{Biological} data frame with an added
#'   numeric column \code{select}.
#'
#' @seealso \code{\link{predict_Millar}}, \code{\link{sel_lookup}}
#'
#' @export
add_selectivity <- function(Biological,
                            Collections,
                            sel_lookup = NULL,
                            classes_min = 75L,
                            classes_max = 900L) {

  if (is.null(sel_lookup)) {
    data("sel_lookup", envir = environment())
    sel_lookup <- get("sel_lookup", envir = environment(), inherits = FALSE)
  }

  # Join sample design onto Biological
  x <- Biological %>%
    dplyr::left_join(
      Collections %>%
        dplyr::select(fish_collection_id, sample_design_code),
      by = "fish_collection_id"
    ) %>%
    dplyr::left_join(
      sel_lookup,
      by = c("species_code", "sample_design_code")
    )

  # Default selectivity = 1
  x$select <- 1.0

  # Index into selectivity curves
  idx <- as.integer(round(x$length_mm)) - classes_min + 1L

  ok <- !purrr::map_lgl(x$curve, is.null) &
    !is.na(idx) &
    idx >= 1L &
    idx <= (classes_max - classes_min + 1L)

  x$select[ok] <- purrr::map2_dbl(
    x$curve[ok],
    idx[ok],
    \(v, i) v[[i]]
  )

  # Clean up
  x %>% dplyr::select(-curve)
}
