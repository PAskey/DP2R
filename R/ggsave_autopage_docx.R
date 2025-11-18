#' Save a faceted ggplot with automatic pagination sized for Word/PDF pages
#'
#' @title Auto-paginated, page-fitting saver for \code{facet_wrap()} plots
#'
#' @description
#' Saves a faceted ggplot to one or more image files, automatically paginating
#' when the number of facets exceeds a per-page limit. Each saved page is sized
#' to fit within a specified page content box (e.g., Word/PowerPoint/LaTeX
#' printable area) while preserving aspect ratio.
#'
#' Works with \code{facet_wrap()} plots (not \code{facet_grid}). If the plot
#' has no facets, a single page is saved.
#'
#' @param plot A \code{ggplot} object that uses \code{facet_wrap()}.
#' @param file_stem Character scalar used as the filename stem. Files are saved
#'   as \code{paste0(file_stem, '_p\%02d.png')} by default.
#' @param w_per_facet Numeric, target width (inches) allocated per facet
#'   before page fitting. Default \code{2.6}.
#' @param h_per_facet Numeric, target height (inches) allocated per facet
#'   before page fitting. Default \code{2.4}.
#' @param page_width,page_height Numerics giving the maximum content box size
#'   (inches) to fit each page into. For US Letter with 1" margins, use
#'   \code{page_width = 6.5}, \code{page_height = 9.0}. Defaults are 6.5 and 9.0.
#' @param max_col,max_row Integers giving the maximum number of columns and rows
#'   per page. Defaults \code{max_col = 4}, \code{max_row = 5}.
#' @param squeeze_one Logical. If \code{TRUE}, and the total number of panels is
#'   exactly one more than the page capacity (\code{max_col * max_row + 1}),
#'   the function will first try to squeeze by adding one extra row or column
#'   to keep everything on a single page. Default \code{TRUE}.
#' @param dpi Numeric dots-per-inch passed to \code{ggplot2::ggsave()}. Default 300.
#' @param units Units for \code{ggplot2::ggsave()} size arguments. Default \code{"in"}.
#' @param device Graphics device function or name. Default \code{ragg::agg_png}
#'   for high-quality anti-aliased PNGs. Set \code{NULL} to let \code{ggsave()}
#'   choose based on filename.
#' @param overwrite Logical; if \code{FALSE}, stop if any target file exists.
#'   Default \code{TRUE}.
#' @param allow_scale_up Logical; if \code{FALSE}, the plot will only be shrunk
#'   to fit the page box, never enlarged. Default \code{FALSE}.
#'
#' @return (Invisibly) a list with details:
#' \itemize{
#'   \item \code{files}: character vector of saved file paths
#'   \item \code{npanels}: total number of panels in the original plot
#'   \item \code{pages}: number of pages saved
#'   \item \code{layout}: c(ncol, nrow) used per page
#'   \item \code{page_box}: c(width, height) page content box
#'   \item \code{natural_size}: c(width, height) before fit-to-page scaling
#'   \item \code{fitted_size}: c(width, height) passed to \code{ggsave()}
#'   \item \code{scale}: scalar scale factor applied to fit the page
#' }
#'
#' @section Notes:
#' - If the fitted scale factor drops below ~0.6 on a single page, a warning is
#'   issued (you may wish to paginate or reduce per-facet size).
#' - This function respects the plot's current \code{scales} setting
#'   (e.g., \code{"fixed"}, \code{"free_y"}), reusing it for pagination.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' p <- ggplot(mtcars, aes(disp, mpg)) +
#'   geom_point() +
#'   facet_wrap(~ cyl)
#'
#' ggsave_autopage_docx(
#'   plot = p,
#'   file_stem = "mtcars_facet",
#'   page_width = 6.5, page_height = 9.0,
#'   max_col = 4, max_row = 5
#' )
#' }
#'
#' @export
#' @importFrom ggplot2 ggsave ggplot_build
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom stats as.formula
ggsave_autopage_docx <- function(
    plot,
    file_stem,
    w_per_facet = 2.6,
    h_per_facet = 2.4,
    page_width  = 6.5,
    page_height = 9.0,
    max_col = 4,
    max_row = 5,
    squeeze_one = TRUE,
    dpi = 300,
    units = "in",
    device = ragg::agg_png,
    overwrite = TRUE,
    allow_scale_up = FALSE
) {
  if (!is.character(file_stem) || length(file_stem) != 1L || nchar(file_stem) == 0L) {
    stop("`file_stem` must be a non-empty character scalar.", call. = FALSE)
  }

  spec <- .get_wrap_spec(plot)
  npan <- .get_npanels(plot)

  layout <- .compute_layout(
    n_total    = npan,
    max_col    = max_col,
    max_row    = max_row,
    squeeze_one = squeeze_one
  )
  ncol_out <- layout$ncol
  nrow_out <- layout$nrow
  n_pages  <- layout$pages

  # natural size before page-fitting
  w0 <- w_per_facet * ncol_out
  h0 <- h_per_facet * nrow_out

  fitted <- .fit_to_page(
    w0 = w0, h0 = h0,
    page_width = page_width, page_height = page_height,
    allow_scale_up = allow_scale_up
  )
  width  <- fitted$width
  height <- fitted$height

  if (fitted$scale < 0.6 && n_pages == 1L) {
    warning(
      sprintf(
        "Plot scaled to %.0f%% of page area; consider pagination or smaller per-facet sizes.",
        100 * fitted$scale
      ),
      call. = FALSE
    )
  }

  paths <- character(n_pages)
  for (i in seq_len(n_pages)) {
    p_i <- plot +
      ggforce::facet_wrap_paginate(
        spec$formula,
        ncol   = ncol_out,
        nrow   = nrow_out,
        page   = i,
        scales = spec$scales
      )

    path <- sprintf("%s_p%02d.png", file_stem, i)
    if (!overwrite && file.exists(path)) {
      stop(sprintf("File exists: %s (set overwrite = TRUE to replace)", path), call. = FALSE)
    }

    ggplot2::ggsave(
      filename = path,
      plot     = p_i,
      width    = width,
      height   = height,
      units    = units,
      dpi      = dpi,
      device   = device
    )
    paths[i] <- path
  }

  invisible(list(
    files        = paths,
    npanels      = npan,
    pages        = n_pages,
    layout       = c(ncol = ncol_out, nrow = nrow_out),
    page_box     = c(width = page_width, height = page_height),
    natural_size = c(width = w0, height = h0),
    fitted_size  = c(width = width, height = height),
    scale        = fitted$scale
  ))
}
