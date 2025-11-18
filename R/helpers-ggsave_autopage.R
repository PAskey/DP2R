# Internal helpers for ggsave_autopage_docx()
# These are intentionally unexported.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' @keywords internal
#' @noRd
.get_npanels <- function(p) {
  gb  <- ggplot2::ggplot_build(p)
  lay <- gb$layout$layout
  if (!"PANEL" %in% names(lay)) return(1L)
  nrow(lay)
}

# Extract facet_wrap spec (formula and scales) from a ggplot object
#' @keywords internal
#' @noRd
.get_wrap_spec <- function(p) {
  if (!inherits(p$facet, "FacetWrap")) {
    stop("This helper currently supports facet_wrap() plots only.", call. = FALSE)
  }
  fvars <- names(p$facet$params$facets)
  if (length(fvars) == 0L) {
    # no facets declared; treat as single panel
    return(list(formula = ~ 1, scales = "fixed"))
  }
  form  <- stats::as.formula(paste("~", paste(fvars, collapse = " + ")))
  scales <- p$facet$params$scales %||% "fixed"
  list(formula = form, scales = scales)
}

# Decide output ncol/nrow and number of pages, honoring max_col/max_row.
# If exactly one panel over capacity and squeeze_one = TRUE, allow one extra
# row or column to keep a single page.
#' @keywords internal
#' @noRd
.compute_layout <- function(n_total, max_col = 4, max_row = 5, squeeze_one = TRUE) {
  per_page <- max_col * max_row

  if (n_total <= per_page) {
    ncol <- min(max_col, n_total)
    nrow <- ceiling(n_total / ncol)
    return(list(ncol = ncol, nrow = nrow, pages = 1L))
  }

  if (squeeze_one && n_total == per_page + 1) {
    # Try adding one row (prefer rows first for readability), then one column.
    if (n_total <= (max_row + 1L) * max_col) {
      ncol <- max_col
      nrow <- ceiling(n_total / ncol)
      return(list(ncol = ncol, nrow = nrow, pages = 1L))
    }
    if (n_total <= max_row * (max_col + 1L)) {
      ncol <- max_col + 1L
      nrow <- ceiling(n_total / ncol)
      return(list(ncol = ncol, nrow = nrow, pages = 1L))
    }
  }

  ncol <- max_col
  nrow <- max_row
  pages <- ceiling(n_total / per_page)
  list(ncol = ncol, nrow = nrow, pages = pages)
}

# Compute fitted width/height inside a page box while preserving aspect ratio
#' @keywords internal
#' @noRd
.fit_to_page <- function(w0, h0, page_width, page_height, allow_scale_up = FALSE) {
  s <- min(page_width / w0, page_height / h0)
  if (!is.finite(s)) s <- 1
  if (!allow_scale_up) s <- min(s, 1)
  list(width = w0 * s, height = h0 * s, scale = s)
}
