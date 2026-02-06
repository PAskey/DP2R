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
#' @param sel_lookup A data frame like \code{sel_lookup} containing at least
#'   \code{species_code}, \code{sample_design_code}, and a list-column \code{curve}
#'   with numeric vectors of selectivity values.
#' @param approx_select_spp A data frame like \code{approx_select_spp} that maps
#'   requested species codes to a proxy species code used for selectivity curves
#'   when a requested species is not present in \code{sel_lookup}. Must contain
#'   \code{species_code} (requested), \code{select_spp} (proxy), and the fork-length
#'   bounds \code{min_FL_gn} and \code{max_FL_gn} (used to highlight the observed
#'   range on the plot). If \code{NULL}, the function loads \code{approx_select_spp}
#'   from package data.
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
#' plot_selectivity(species = "RB", design = c("SGN7","FWIN","SGN6"))
#'
#' # Facet by design, colours fixed by species
#' plot_selectivity(species = c("RB","EB"), design = c("SGN7","FWIN"), facet = "design")
#'
#' # Facet by species for sportfish, standard design
#' plot_selectivity(species = c("BS","BT","CT","EB","KO", "LT","RB", "WCT","YP"), design = c("SGN7", "FWIN"), facet = "species")
#'
#' # Facet by species for non-sportfish, standard design
#' plot_selectivity(species = c("BNH","LKC", "LSU","PCC", "NSC","CSU", "RSC"), design = c("SGN7"), facet = "species", colour_by = "species")
#'
#' }
#'
#' @importFrom dplyr filter mutate case_when
#' @importFrom tidyr unnest_longer
#' @importFrom ggplot2 ggplot aes geom_line labs theme_bw facet_wrap
#' @export
plot_selectivity <- function(
                             species,
                             design ="SGN7",#Defaults to standard RIC net
                             classes = 75:900,
                             facet = c("none", "species", "design"),
                             colour_by = c("auto", "species", "design", "interaction"),
                             sel_lookup = NULL,
                             approx_select_spp = NULL
) {

  colour_by_user <- !missing(colour_by)
  facet <- match.arg(facet)
  colour_by <- match.arg(colour_by)

  if (is.null(sel_lookup)) {
    data("sel_lookup", envir = environment())
    sel_lookup <- get("sel_lookup", envir = environment(), inherits = FALSE)
  }

  if (is.null(approx_select_spp)) {
    data("approx_select_spp", envir = environment())
    approx_select_spp <- get("approx_select_spp", envir = environment(), inherits = FALSE)
  }

  stopifnot(is.numeric(classes), length(classes) > 1)

  # ---- Map requested species -> species actually used in sel_lookup ----
  sel_species <- unique(as.character(sel_lookup$species_code))
  species_req <- unique(as.character(species))

  needs_approx <- setdiff(species_req, sel_species)

  mapping <- tibble::tibble(species_req = species_req) %>%
    dplyr::left_join(
      approx_select_spp %>%
        dplyr::transmute(
          species_req  = as.character(species_code),
          species_use  = as.character(select_spp),
          min_FL       = min_FL_gn,
          max_FL       = max_FL_gn
        ),
      by = "species_req"
    ) %>%
    dplyr::mutate(
      species_use = dplyr::case_when(
        species_req %in% sel_species ~ species_req,
        TRUE ~ species_use
      )
    )


  # Warn about anything that required approximation
  if (length(needs_approx) > 0) {
    repl <- mapping %>%
      dplyr::filter(species_req %in% needs_approx) %>%
      dplyr::transmute(msg = paste0(species_req, " -> ", dplyr::if_else(is.na(species_use), "NA", species_use))) %>%
      dplyr::pull(msg)

    warning(
      "Some requested species were not found in sel_lookup and were approximated (or dropped if NA):\n  ",
      paste(repl, collapse = "\n  "),
      call. = FALSE
    )
  }

  # Drop those that still have no usable approximation
  mapping_ok <- mapping %>% dplyr::filter(!is.na(species_use))
  if (nrow(mapping_ok) == 0) {
    stop("None of the requested species exist in sel_lookup and no usable approximations were found.")
  }

  # Use the mapped species to pull curves, but keep requested codes for labels/facets
  df <- sel_lookup %>%
    dplyr::mutate(species_code = as.character(species_code),
                  sample_design_code = as.character(sample_design_code)) %>%
    dplyr::filter(species_code %in% mapping_ok$species_use,
                  sample_design_code %in% as.character(design))

  if (nrow(df) == 0) stop("No curves found for requested species/design combination(s).")

  # Attach "requested species" labels (handles many-to-one approximations)
  df <- df %>%
    dplyr::inner_join(
      mapping_ok,
      by = c("species_code" = "species_use"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      species_display = dplyr::if_else(
        species_req == species_code,
        species_req,
        paste0(species_req, " (≈", species_code, ")")
      )
    )



  # If faceting, only force colour when user did NOT explicitly choose
  if (!colour_by_user && colour_by == "auto") {
    if (facet == "design")  colour_by <- "species"
    if (facet == "species") colour_by <- "design"
  }

  # If not faceting, keep auto behaviour (unless user explicitly set colour_by)
  if (facet == "none" && colour_by == "auto") {
    if (length(unique(df$species_display)) > 1 && length(unique(df$sample_design_code)) == 1) {
      colour_by <- "species"
    } else if (length(unique(df$species_display)) == 1 && length(unique(df$sample_design_code)) > 1) {
      colour_by <- "design"
    } else {
      colour_by <- "interaction"
    }
  }

  df_long <- df %>%
    tidyr::unnest_longer(curve, indices_to = "idx") %>%
    dplyr::mutate(
      length_mm = classes[idx],

      # Always keep lines separate by species+design
      line_id = paste(species_display, sample_design_code, sep = " | "),

      # Colour mapping follows colour_by choice
      colour_id = dplyr::case_when(
        colour_by == "species" ~ species_display,
        colour_by == "design"  ~ sample_design_code,
        TRUE ~ paste(species_display, sample_design_code, sep = " | ")
      ),

      in_range = dplyr::if_else(
        is.na(min_FL) | is.na(max_FL),
        TRUE,
        length_mm >= min_FL & length_mm <= max_FL
      )
    ) %>%
    dplyr::arrange(species_display, sample_design_code, length_mm)



  legend_title <- dplyr::case_when(
    colour_by == "species" ~ "Species",
    colour_by == "design" ~ "Net design",
    TRUE ~ "Species | Net"
  )

  p <- ggplot2::ggplot(df_long, ggplot2::aes(length_mm, curve)) +

    # Full curve (grey)
    ggplot2::geom_line(
      ggplot2::aes(group = line_id),
      colour = "grey75",
      linewidth = 0.8
    ) +

    # Observed range (coloured per colour_by)
    ggplot2::geom_line(
      data = dplyr::filter(df_long, in_range),
      ggplot2::aes(group = line_id, colour = colour_id),
      linewidth = 1.3
    ) +

    ggplot2::labs(
      x = "Fork length (mm)",
      y = "Relative selectivity",
      colour = legend_title
    ) +
    ggplot2::theme_bw()


  # Apply faceting
  if (facet == "species") {
    p <- p + ggplot2::facet_wrap(~ species_display)
  } else if (facet == "design") {
    p <- p + ggplot2::facet_wrap(~ sample_design_code)
  }

  p
}
