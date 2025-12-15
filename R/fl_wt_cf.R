#' Calculate condition factor, length, or weight
#'
#' CF = (W / L^3) * 100000   where W = weight in grams, L = fork length in mm
#'
#' Provide exactly TWO of: fl_mm, wt_g, cf.
#' The function returns the missing third value.
#'
#' @param fl_mm Numeric. Fish fork-length in millimetres.
#' @param wt_g Numeric. Fish weight in grams.
#' @param cf Numeric. Condition factor.
#'
#' @return Numeric value of the missing parameter.
#' @export
fl_wt_cf <- function(fl_mm = NULL,
                             wt_g = NULL,
                             cf = NULL) {

  n_provided <- sum(!vapply(list(fl_mm, wt_g, cf), is.null, logical(1)))

  if (n_provided != 2) {
    stop("Exactly two of fl_mm, wt_g, or cf must be provided.", call. = FALSE)
  }

  # helpers
  assert_pos <- function(x, name) {
    if (any(!is.finite(x), na.rm = TRUE)) stop(name, " must be finite.", call. = FALSE)
    if (any(x <= 0, na.rm = TRUE)) stop(name, " must be > 0.", call. = FALSE)
  }

  if (!is.null(fl_mm)) assert_pos(fl_mm, "fl_mm")
  if (!is.null(wt_g))  assert_pos(wt_g,  "wt_g")
  if (!is.null(cf))    assert_pos(cf,    "cf")

  # Calculate CF
  if (is.null(cf)) {
    return((wt_g / (fl_mm^3)) * 100000)
  }

  # Calculate weight
  if (is.null(wt_g)) {
    return((cf * (fl_mm^3)) / 100000)
  }

  # Calculate length
  if (is.null(fl_mm)) {
    return((wt_g * 100000 / cf)^(1/3))
  }

  stop("Unexpected input combination.", call. = FALSE)
}
