#' Plot precomputed gillnet selectivity curves
#'
#' Plot relative gillnet selectivity curves from precomputed lookup tables in the
#' \pkg{DP2R} package.
#'
#' The function has two modes:
#' \itemize{
#'   \item \strong{Design mode} (default): if \code{Sample_event} is \code{NULL},
#'   curves are plotted from \code{sel_lookup} for the requested \code{design}(s).
#'   \item \strong{Event mode}: if \code{Sample_event} is provided, the overall
#'   event curve is plotted from \code{sel_lookup_event}. If
#'   \code{plot_sub_designs = TRUE}, design-level curves for all designs used in
#'   the event (from \code{sel_lookup_event$event_designs}) are also overlaid
#'   using \code{sel_lookup}.
#' }
#'
#' When faceting is used, the colour mapping is automatically forced to be
#' consistent across facets (e.g., if faceting by design, colours represent
#' species only; if faceting by species, colours represent designs only).
#'
#' @param species Character vector of species codes to plot (e.g., \code{c("RB","EB")}).
#' @param design Character vector of sample design codes to plot in design mode
#'   (e.g., \code{c("SGN7","FWIN")}). Ignored in event mode unless
#'   \code{plot_sub_designs = TRUE} and \code{sel_lookup_event$event_designs} is
#'   unavailable (in which case no sub-designs can be plotted).
#' @param Sample_event Optional character vector of Sample_event IDs. If provided,
#'   event mode is used.
#' @param plot_sub_designs Logical. In event mode, if \code{TRUE} (default),
#'   overlay design-level curves for the designs listed in
#'   \code{sel_lookup_event$event_designs} in addition to the overall event curve.
#'   If \code{FALSE}, only the overall event curve is plotted.
#' @param classes Integer/numeric vector of lengths (mm) corresponding to the curve
#'   indices. Defaults to \code{75:900}. Must match the length range used when
#'   building the lookup tables.
#' @param facet One of \code{"none"}, \code{"species"}, or \code{"design"} indicating
#'   whether to facet the plot.
#' @param colour_by One of \code{"auto"}, \code{"species"}, \code{"design"},
#'   or \code{"interaction"}. If \code{facet != "none"}, this argument is ignored
#'   and colour is mapped to the non-faceted variable for consistent legend/colours.
#' @param approx_select_spp A data frame like \code{approx_select_spp} that maps
#'   requested species codes to a proxy species code used for selectivity curves
#'   when a requested species is not present in \code{sel_lookup}. Must contain
#'   \code{species_code} (requested), \code{select_spp} (proxy), and the fork-length
#'   bounds \code{min_FL_gn} and \code{max_FL_gn} (used to highlight the observed
#'   range on the plot). If \code{NULL}, the function loads \code{approx_select_spp}
#'   from \pkg{DP2R} package data.
#'
#' @return A \code{ggplot} object.
#'
#' @details
#' This function does not recompute selectivity; it only visualizes precomputed
#' curves stored in \pkg{DP2R}. In event mode, the overall event curve comes from
#' \code{sel_lookup_event}. If \code{plot_sub_designs = TRUE}, design-level curves
#' are pulled from \code{sel_lookup} using the designs listed in the
#' \code{event_designs} list-column.
#'
#' @examples
#' \dontrun{
#' # Compare species within a single net design (design mode)
#' plot_selectivity(species = c("RB","EB"), design = "SGN7")
#'
#' # Compare net designs within a single species (design mode)
#' plot_selectivity(species = "RB", design = c("SGN7","FWIN","SGN6"))
#'
#' # Facet by design, colours fixed by species (design mode)
#' plot_selectivity(species = c("RB","EB"), design = c("SGN7","FWIN"), facet = "design")
#'
#' # Event mode: overall event curve only
#' plot_selectivity(species = c("RB","KO"),
#'                  Sample_event = "GN_00901LNIC_1990_SON",
#'                  plot_sub_designs = FALSE,
#'                  facet = "species")
#'
#' # Event mode: overlay sub-design curves + overall event curve
#' plot_selectivity(species = c("RB","KO"),
#'                  Sample_event = "GN_00901LNIC_1990_SON",
#'                  plot_sub_designs = TRUE,
#'                  facet = "species")
#' }
#'
#' @importFrom dplyr filter mutate case_when
#' @importFrom tidyr unnest_longer
#' @importFrom ggplot2 ggplot aes geom_line labs theme_bw facet_wrap
#' @export
plot_selectivity <- function(
    species,
    design = "SGN7",
    Sample_event = NULL,
    plot_sub_designs = TRUE,
    classes = 75:900,
    facet = c("none", "species", "design"),
    colour_by = c("auto", "species", "design", "interaction"),
    approx_select_spp = NULL
) {

  colour_by_user <- !missing(colour_by)
  facet <- match.arg(facet)
  colour_by <- match.arg(colour_by)

  stopifnot(is.numeric(classes), length(classes) > 1)

  # ---- Decide event mode ----
  ev <- character(0)
  if (!is.null(Sample_event)) {
    ev <- unique(as.character(Sample_event))
    ev <- ev[!is.na(ev) & ev != ""]
  }
  use_event <- length(ev) > 0

  if (!requireNamespace("DP2R", quietly = TRUE)) {
    stop("plot_selectivity(): package 'DP2R' is required but not installed.", call. = FALSE)
  }

  # ---- Load lookups from DP2R ----
  utils::data("sel_lookup", package = "DP2R", envir = environment())
  sel_lookup <- get("sel_lookup", envir = environment(), inherits = FALSE)

  if (use_event) {
    utils::data("sel_lookup_event", package = "DP2R", envir = environment())
    sel_lookup_event <- get("sel_lookup_event", envir = environment(), inherits = FALSE)

    req_event_cols <- c("Sample_event", "species_code", "curve")
    miss <- setdiff(req_event_cols, names(sel_lookup_event))
    if (length(miss) > 0) {
      stop("plot_selectivity(): sel_lookup_event is missing: ",
           paste(miss, collapse = ", "), call. = FALSE)
    }

    if (isTRUE(plot_sub_designs) && !("event_designs" %in% names(sel_lookup_event))) {
      stop("plot_selectivity(): plot_sub_designs=TRUE requires sel_lookup_event to have an 'event_designs' list-column.",
           call. = FALSE)
    }
  } else {
    req_cols <- c("species_code", "sample_design_code", "curve")
    miss <- setdiff(req_cols, names(sel_lookup))
    if (length(miss) > 0) {
      stop("plot_selectivity(): sel_lookup is missing: ",
           paste(miss, collapse = ", "), call. = FALSE)
    }
  }

  # ---- Load approx_select_spp (if not supplied) ----
  if (is.null(approx_select_spp)) {
    utils::data("approx_select_spp", package = "DP2R", envir = environment())
    approx_select_spp <- get("approx_select_spp", envir = environment(), inherits = FALSE)
  }

  # ---- Map requested species -> species actually used in sel_lookup ----
  sel_species <- unique(as.character(sel_lookup$species_code))
  species_req <- unique(as.character(species))

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

  mapping_ok <- mapping %>% dplyr::filter(!is.na(species_use))
  if (nrow(mapping_ok) == 0) {
    stop("None of the requested species exist in sel_lookup and no usable approximations were found.",
         call. = FALSE)
  }

  # ---- Build pieces to plot ----
  pieces <- list()

  if (use_event) {
    # Overall event curve(s)
    df_event <- sel_lookup_event %>%
      dplyr::mutate(
        Sample_event = as.character(.data$Sample_event),
        species_code = as.character(.data$species_code)
      ) %>%
      dplyr::filter(
        .data$Sample_event %in% ev,
        .data$species_code %in% mapping_ok$species_use
      ) %>%
      dplyr::mutate(sample_design_code = "Overall (event)")

    pieces[["event"]] <- df_event

    # Optional: sub-design curves from sel_lookup
    if (isTRUE(plot_sub_designs)) {

      # collect all designs for the requested event(s)
      dsgns <- sel_lookup_event %>%
        dplyr::filter(.data$Sample_event %in% ev) %>%
        dplyr::pull(.data$event_designs) %>%
        unlist(use.names = FALSE) %>%
        as.character()

      dsgns <- stringr::str_trim(dsgns)
      dsgns <- unique(dsgns[!is.na(dsgns) & dsgns != ""])

      if (length(dsgns) == 0) {
        warning("plot_selectivity(): plot_sub_designs=TRUE but no designs found in sel_lookup_event$event_designs for the requested Sample_event(s).",
                call. = FALSE)
      } else {
        df_design <- sel_lookup %>%
          dplyr::mutate(
            species_code = as.character(.data$species_code),
            sample_design_code = as.character(.data$sample_design_code)
          ) %>%
          dplyr::filter(
            .data$species_code %in% mapping_ok$species_use,
            .data$sample_design_code %in% dsgns
          )

        pieces[["design"]] <- df_design
      }
    }

  } else {
    # Design-only mode (original behaviour)
    df_design <- sel_lookup %>%
      dplyr::mutate(
        species_code = as.character(.data$species_code),
        sample_design_code = as.character(.data$sample_design_code)
      ) %>%
      dplyr::filter(
        .data$species_code %in% mapping_ok$species_use,
        .data$sample_design_code %in% as.character(design)
      )

    pieces[["design"]] <- df_design
  }

  df <- dplyr::bind_rows(pieces)
  if (nrow(df) == 0) stop("No curves found for requested filters.", call. = FALSE)

  # ---- Attach requested species labels ----
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

  # ---- Colour behaviour (unchanged) ----
  if (!colour_by_user && colour_by == "auto") {
    if (facet == "design")  colour_by <- "species"
    if (facet == "species") colour_by <- "design"
  }

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
      line_id   = paste(species_display, sample_design_code, sep = " | "),
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
    colour_by == "design"  ~ "Net design",
    TRUE ~ "Species | Net"
  )

  p <- ggplot2::ggplot(df_long, ggplot2::aes(length_mm, curve)) +
    ggplot2::geom_line(
      ggplot2::aes(group = line_id),
      colour = "grey75",
      linewidth = 0.8
    ) +
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

  if (facet == "species") {
    p <- p + ggplot2::facet_wrap(~ species_display)
  } else if (facet == "design") {
    p <- p + ggplot2::facet_wrap(~ sample_design_code)
  }

  p
}
