#' Plot precomputed gillnet selectivity curves
#'
#' Plot relative gillnet selectivity curves from the precomputed lookup table
#' \code{sel_lookup}. Curves can be compared across species within a net design,
#' across net designs within a species, or across multiple species and designs.
#'
#' When faceting is used, the colour mapping is automatically forced to be
#' consistent across facets (e.g., if faceting by design, colours represent
#' species only; if faceting by species, colours represent designs only).
#'
#' @param sel_lookup A data frame like \code{sel_lookup} containing at least
#'   \code{species_code}, \code{sample_design_code}, and a list-column \code{curve}
#'   with numeric vectors of selectivity values.
#'   If \code{NULL}, the function loads \code{sel_lookup} from package data.
#' @param species Character vector of species codes to plot (e.g., \code{c("RB","EB")}).
#' @param design Character vector of sample design codes to plot
#'   (e.g., \code{c("SGN7","FWIN")}).
#' @param classes Integer/numeric vector of lengths (mm) corresponding to the curve
#'   indices. Defaults to \code{75:900}. Must match the length range used when
#'   building \code{sel_lookup}.
#' @param facet One of \code{"none"}, \code{"species"}, or \code{"design"} indicating
#'   whether to facet the plot.
#' @param colour_by One of \code{"auto"}, \code{"species"}, \code{"design"},
#'   or \code{"interaction"}. If \code{facet != "none"}, this argument is ignored
#'   and colour is mapped to the non-faceted variable for consistent legend/colours.
#'
#' @return A \code{ggplot} object.
#'
#' @details
#' The lookup table \code{sel_lookup} is typically built in \code{data-raw/} using
#' \code{\link{predict_Millar}} and saved to the package \code{data/} directory.
#' This plotting function does not recompute selectivity; it only visualizes
#' precomputed curves.
#'
#' @examples
#' \dontrun{
#' data(sel_lookup)
#'
#' # Compare species within a single net design
#' plot_selectivity(species = c("RB","EB"), design = "SGN7")
#'
#' # Compare net designs within a single species
#' plot_selectivity(species = "RB", design = c("SGN7","FWIN","SWIN"))
#'
#' # Facet by design, colours fixed by species
#' plot_selectivity(species = c("RB","EB"), design = c("SGN7","FWIN"), facet = "design")
#'
#' # Facet by species, colours fixed by design
#' plot_selectivity(species = c("RB","EB"), design = c("SGN7","FWIN"), facet = "species")
#' }
#'
#' @importFrom dplyr filter mutate case_when
#' @importFrom tidyr unnest_longer
#' @importFrom ggplot2 ggplot aes geom_line labs theme_bw facet_wrap
#' @export
plot_selectivity <- function(sel_lookup = NULL,
                             species,
                             design,
                             classes = 75:900,
                             facet = c("none", "species", "design"),
                             colour_by = c("auto", "species", "design", "interaction")) {

  facet <- match.arg(facet)
  colour_by <- match.arg(colour_by)

  if (is.null(sel_lookup)) {
    data("sel_lookup", envir = environment())
    sel_lookup <- get("sel_lookup", envir = environment(), inherits = FALSE)
  }

  stopifnot(is.numeric(classes), length(classes) > 1)

  df <- sel_lookup %>%
    dplyr::filter(species_code %in% species,
                  sample_design_code %in% design)

  if (nrow(df) == 0) stop("No curves found for requested species/design combination(s).")

  # If faceting, force colour to be the "other" variable for consistency
  if (facet == "design") colour_by <- "species"
  if (facet == "species") colour_by <- "design"

  # If not faceting, keep auto behaviour (unless user explicitly set colour_by)
  if (facet == "none" && colour_by == "auto") {
    if (length(unique(df$species_code)) > 1 && length(unique(df$sample_design_code)) == 1) {
      colour_by <- "species"
    } else if (length(unique(df$species_code)) == 1 && length(unique(df$sample_design_code)) > 1) {
      colour_by <- "design"
    } else {
      colour_by <- "interaction"
    }
  }

  df_long <- df %>%
    tidyr::unnest_longer(curve, indices_to = "idx") %>%
    dplyr::mutate(
      length_mm = classes[idx],
      species_code = as.character(species_code),
      sample_design_code = as.character(sample_design_code),
      group_id = dplyr::case_when(
        colour_by == "species" ~ species_code,
        colour_by == "design"  ~ sample_design_code,
        TRUE ~ paste(species_code, sample_design_code, sep = " | ")
      )
    )

  # legend title
  legend_title <- dplyr::case_when(
    colour_by == "species" ~ "Species",
    colour_by == "design" ~ "Net design",
    TRUE ~ "Species | Net"
  )

  p <- ggplot2::ggplot(df_long, ggplot2::aes(length_mm, curve, colour = group_id)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      x = "Fork length (mm)",
      y = "Relative selectivity",
      colour = legend_title
    ) +
    ggplot2::theme_bw()

  # Apply faceting
  if (facet == "species") {
    p <- p + ggplot2::facet_wrap(~ species_code)
  } else if (facet == "design") {
    p <- p + ggplot2::facet_wrap(~ sample_design_code)
  }

  p
}
