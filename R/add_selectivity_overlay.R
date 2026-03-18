#' Add selectivity-curve overlays to an existing ggplot object
#'
#' @description
#' Adds relative gillnet selectivity curves to an existing plot with
#' \code{length_mm} on the x-axis. This helper is intended for plots built from
#' fish-level or length-based data, especially \code{SPDTplot(..., Metric =
#' "FL_density")}, \code{"FL_hist"}, and \code{"FL_freq"}.
#'
#' The function inspects the supplied plot object, retrieves appropriate
#' selectivity curves with [selectivity_curves()], matches them to the plot's
#' faceting structure where possible, rescales selectivity to the y-range of
#' each panel, and returns the original plot with the overlay added.
#'
#' By default, if \code{species} and/or \code{Sample_event} are not supplied,
#' the function attempts to infer them from \code{p$data}.
#'
#' @param p A \pkg{ggplot2} plot object. The plot data must include
#'   \code{length_mm}. To infer species automatically, the plot data should also
#'   include \code{species_code}. To infer event-specific curves automatically,
#'   the plot data should include \code{Sample_event}.
#' @param species Optional character vector of species codes. If \code{NULL},
#'   unique non-missing values are taken from \code{p$data$species_code} when
#'   available. If multiple species are found, the first is used with a warning.
#' @param sample_design_code Character vector of design codes used when
#'   \code{Sample_event} is not supplied and cannot be inferred from the plot.
#' @param Sample_event Optional character vector of sample event IDs. If
#'   \code{NULL}, unique non-missing values are taken from \code{p$data} when a
#'   \code{Sample_event} column is present.
#' @param include_sub_designs Logical; if \code{TRUE} and event-specific curves
#'   are used, also include the sub-design curves listed in
#'   \code{sel_lookup_event$event_designs}.
#' @param overall_only Logical; if \code{TRUE}, only draw event-level overall
#'   curves when present.
#' @param highlight_in_range Logical; if \code{TRUE}, draw the full curve in
#'   grey and redraw the recommended approximation range in colour for
#'   design-level curves. Event-level overall curves are always drawn in black.
#' @param colour_by Colour grouping for non-overall design curves. One of
#'   \code{"auto"}, \code{"species"}, \code{"design"}, or
#'   \code{"interaction"}.
#' @param linewidth Base line width for the overlay.
#' @param alpha Line alpha for the overlay.
#' @param show.legend Logical; if \code{TRUE}, show the colour legend for
#'   non-overall design curves. Defaults to \code{FALSE}.
#' @param warn_missing Logical; if \code{TRUE}, warn when requested
#'   \code{Sample_event} values do not have matching selectivity curves.
#' @param x_trim How to trim the x-range of the overlay. One of:
#'   \itemize{
#'     \item \code{"auto"}: use panel-wise trimming when x-ranges vary by panel,
#'       otherwise use the common built plot x-range
#'     \item \code{"panel"}: trim to the observed \code{length_mm} range within
#'       each panel
#'     \item \code{"plot"}: trim to the built x-range of each panel, which is
#'       usually shared across panels when only y is free
#'     \item \code{"none"}: do not trim selectivity curves on x
#'   }
#'
#' @return
#' A \code{ggplot} object with selectivity overlay layers added.
#'
#' @details
#' Selectivity values are stored on a 0--1 scale. This function rescales them to
#' each panel's observed y-range in the supplied plot using the built ggplot
#' object. This makes the overlay suitable for density plots, histograms, and
#' similar displays, but the overlay remains visual rather than directly
#' quantitative unless the underlying y-scale is explicitly comparable to
#' relative selectivity.
#'
#' Faceting is handled by matching selectivity data to the facet variables used
#' in the plot, based on the built layout. If a facet variable is present in the
#' plot but not in the selectivity data, the function replicates the selectivity
#' curves across the observed facet combinations where possible.
#'
#' @examples
#' \dontrun{
#' p <- SPDTplot("FL_density", min_N = 30) +
#'   ggplot2::facet_wrap(~Sample_event + age, scales = "free", ncol = 4)
#'
#' add_selectivity_overlay(p)
#' add_selectivity_overlay(p, species = "KO")
#'
#' p2 <- SPDTplot("FL_hist", min_N = 30) +
#'   ggplot2::facet_wrap(~Sample_event + age, scales = "free_y", ncol = 4) +
#'   ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1))
#'
#' add_selectivity_overlay(p2, species = "KO", x_trim = "plot")
#' }
#'
#' @export
add_selectivity_overlay <- function(p,
                                    species = NULL,
                                    sample_design_code = "SGN7",
                                    Sample_event = NULL,
                                    include_sub_designs = FALSE,
                                    overall_only = TRUE,
                                    highlight_in_range = TRUE,
                                    colour_by = c("auto", "species", "design", "interaction"),
                                    linewidth = 1.1,
                                    alpha = 1,
                                    show.legend = FALSE,
                                    warn_missing = TRUE,
                                    x_trim = c("auto", "panel", "plot", "none")) {

  colour_by <- match.arg(colour_by)
  x_trim <- match.arg(x_trim)

  if (!inherits(p, "ggplot")) {
    stop("`p` must be a ggplot object.", call. = FALSE)
  }

  base_df <- p$data
  if (is.null(base_df) || nrow(base_df) == 0) {
    stop("`p$data` is empty; cannot infer overlay information from the plot.", call. = FALSE)
  }

  if (!("length_mm" %in% names(base_df))) {
    stop(
      "`p$data` must contain `length_mm`. ",
      "This helper is only appropriate for length-based plots.",
      call. = FALSE
    )
  }

  # infer species if not supplied
  if (is.null(species)) {
    if ("species_code" %in% names(base_df)) {
      species <- unique(as.character(base_df$species_code))
      species <- species[!is.na(species) & species != ""]
      if (length(species) > 1) {
        warning(
          "Multiple species found in `p$data`: ",
          paste(species, collapse = ", "),
          ". Using first: ", species[1],
          ". Supply `species` explicitly to override.",
          call. = FALSE
        )
        species <- species[1]
      }
    }
    if (is.null(species) || length(species) == 0) {
      stop(
        "Could not infer `species` from `p$data`. Please supply `species` explicitly.",
        call. = FALSE
      )
    }
  }

  # infer Sample_event if not supplied
  if (is.null(Sample_event) && "Sample_event" %in% names(base_df)) {
    Sample_event <- unique(as.character(base_df$Sample_event))
    Sample_event <- Sample_event[!is.na(Sample_event) & Sample_event != ""]
    if (length(Sample_event) == 0) {
      Sample_event <- NULL
    }
  }

  sel_df <- selectivity_curves(
    species = species,
    sample_design_code = sample_design_code,
    Sample_event = Sample_event,
    include_sub_designs = include_sub_designs
  )

  if (nrow(sel_df) == 0) {
    stop("No selectivity curves found for the requested plot/data combination.", call. = FALSE)
  }

  # warn about missing event-level curves
  if (!is.null(Sample_event) && warn_missing) {
    requested_events <- unique(as.character(Sample_event))
    requested_events <- requested_events[!is.na(requested_events) & requested_events != ""]

    returned_events <- unique(as.character(sel_df$Sample_event))
    returned_events <- returned_events[!is.na(returned_events) & returned_events != ""]

    missing_events <- setdiff(requested_events, returned_events)

    if (length(missing_events) > 0) {
      warning(
        "No selectivity curve found for: ",
        paste(missing_events, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # ensure common columns exist
  if (!("Sample_event" %in% names(sel_df))) sel_df$Sample_event <- NA_character_
  if (!("locale_name" %in% names(sel_df))) sel_df$locale_name <- NA_character_
  if (!("year" %in% names(sel_df))) sel_df$year <- NA_integer_

  gb <- ggplot2::ggplot_build(p)

  panel_layout <- tibble::as_tibble(gb$layout$layout)
  panel_layout$PANEL <- as.integer(as.character(panel_layout$PANEL))

  # keep only true facet variables that also exist in the plot data
  panel_vars <- intersect(names(panel_layout), names(base_df))
  panel_vars <- setdiff(panel_vars, c("PANEL", "ROW", "COL", "SCALE_X", "SCALE_Y"))

  panel_layout <- panel_layout |>
    dplyr::select(dplyr::all_of(c("PANEL", panel_vars))) |>
    dplyr::distinct()

  # panel y max from built layers
  layer_ymax <- lapply(gb$data, function(d) {
    d <- tibble::as_tibble(d)
    if (!all(c("PANEL", "y") %in% names(d))) return(NULL)

    d$PANEL <- as.integer(as.character(d$PANEL))

    d |>
      dplyr::filter(!is.na(.data$y)) |>
      dplyr::group_by(.data$PANEL) |>
      dplyr::summarise(panel_y_max = max(.data$y, na.rm = TRUE), .groups = "drop")
  })

  layer_ymax <- dplyr::bind_rows(layer_ymax)

  if (nrow(layer_ymax) == 0) {
    stop(
      "Could not determine panel y-ranges from the built plot.",
      call. = FALSE
    )
  }

  panel_ymax <- layer_ymax |>
    dplyr::group_by(.data$PANEL) |>
    dplyr::summarise(panel_y_max = max(.data$panel_y_max, na.rm = TRUE), .groups = "drop")
  panel_ymax$PANEL <- as.integer(panel_ymax$PANEL)

  # helper to extract built x-ranges from panel params across ggplot versions
  get_panel_xrange <- function(pp) {
    if (!is.null(pp$x.range)) {
      return(pp$x.range)
    }
    if (!is.null(pp$x$range$range)) {
      return(pp$x$range$range)
    }
    if (!is.null(pp$x_range)) {
      return(pp$x_range)
    }
    c(NA_real_, NA_real_)
  }

  built_xranges_list <- lapply(gb$layout$panel_params, get_panel_xrange)
  built_xranges <- tibble::tibble(
    PANEL = seq_along(built_xranges_list),
    built_x_min = vapply(built_xranges_list, function(z) z[1], numeric(1)),
    built_x_max = vapply(built_xranges_list, function(z) z[2], numeric(1))
  )
  built_xranges$PANEL <- as.integer(built_xranges$PANEL)

  x_varies_by_panel <- dplyr::n_distinct(
    paste(built_xranges$built_x_min, built_xranges$built_x_max)
  ) > 1

  if (x_trim == "auto") {
    x_trim <- if (x_varies_by_panel) "panel" else "plot"
  }

  if (x_trim == "panel") {
    if (length(panel_vars) > 0) {
      panel_xrange <- base_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(panel_vars))) |>
        dplyr::summarise(
          panel_x_min = min(.data$length_mm, na.rm = TRUE),
          panel_x_max = max(.data$length_mm, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::left_join(panel_layout, by = panel_vars)
    } else {
      panel_xrange <- tibble::tibble(
        PANEL = 1L,
        panel_x_min = min(base_df$length_mm, na.rm = TRUE),
        panel_x_max = max(base_df$length_mm, na.rm = TRUE)
      )
    }
  } else if (x_trim == "plot") {
    panel_xrange <- built_xranges |>
      dplyr::transmute(
        PANEL = .data$PANEL,
        panel_x_min = .data$built_x_min,
        panel_x_max = .data$built_x_max
      )
  } else {
    panel_xrange <- tibble::tibble(
      PANEL = as.integer(unique(panel_layout$PANEL)),
      panel_x_min = -Inf,
      panel_x_max = Inf
    )
  }
  panel_xrange$PANEL <- as.integer(panel_xrange$PANEL)

  # expand selectivity data across facets where needed
  if (length(panel_vars) > 0) {
    facet_key <- base_df |>
      dplyr::select(dplyr::all_of(intersect(panel_vars, names(base_df)))) |>
      dplyr::distinct()

    shared_vars <- intersect(panel_vars, intersect(names(facet_key), names(sel_df)))
    missing_vars <- setdiff(panel_vars, names(sel_df))

    if (length(missing_vars) > 0) {
      if (length(shared_vars) > 0) {
        sel_df <- sel_df |>
          dplyr::left_join(
            facet_key,
            by = shared_vars,
            relationship = "many-to-many"
          )
      } else {
        facet_key$.tmp_join_key <- 1L
        sel_df$.tmp_join_key <- 1L

        sel_df <- sel_df |>
          dplyr::left_join(
            facet_key,
            by = ".tmp_join_key",
            relationship = "many-to-many"
          ) |>
          dplyr::select(-.tmp_join_key)
      }
    }
  }

  # join panel info
  if (length(panel_vars) > 0) {
    sel_df <- sel_df |>
      dplyr::left_join(panel_layout, by = panel_vars) |>
      dplyr::mutate(PANEL = as.integer(.data$PANEL)) |>
      dplyr::left_join(panel_ymax, by = "PANEL") |>
      dplyr::left_join(
        panel_xrange |>
          dplyr::select(PANEL, panel_x_min, panel_x_max),
        by = "PANEL"
      )
  } else {
    sel_df$PANEL <- 1L
    sel_df <- sel_df |>
      dplyr::mutate(PANEL = as.integer(.data$PANEL)) |>
      dplyr::left_join(panel_ymax, by = "PANEL") |>
      dplyr::left_join(
        panel_xrange |>
          dplyr::select(PANEL, panel_x_min, panel_x_max),
        by = "PANEL"
      )
  }

  if (all(is.na(sel_df$panel_y_max))) {
    stop(
      "Could not match selectivity data to plot panels. ",
      "Try supplying `species` and/or `Sample_event` explicitly.",
      call. = FALSE
    )
  }

  # trim overlay to chosen x-range mode
  sel_df <- sel_df |>
    dplyr::filter(
      !is.na(.data$panel_x_min),
      !is.na(.data$panel_x_max),
      .data$length_mm >= .data$panel_x_min,
      .data$length_mm <= .data$panel_x_max
    )

  if (nrow(sel_df) == 0) {
    stop(
      "Selectivity curves were found, but none overlapped the chosen x-ranges of the plot panels.",
      call. = FALSE
    )
  }

  # colour grouping
  if (colour_by == "auto") {
    if (length(unique(sel_df$species_display)) > 1 &&
        length(unique(sel_df$sample_design_code)) == 1) {
      colour_by <- "species"
    } else if (length(unique(sel_df$species_display)) == 1 &&
               length(unique(sel_df$sample_design_code)) > 1) {
      colour_by <- "design"
    } else {
      colour_by <- "interaction"
    }
  }

  sel_df <- sel_df |>
    dplyr::mutate(
      plot_y = .data$selectivity * .data$panel_y_max,
      line_id = paste(.data$species_display, .data$sample_design_code, sep = " | "),
      in_range = dplyr::if_else(
        is.na(.data$min_FL) | is.na(.data$max_FL),
        TRUE,
        .data$length_mm >= .data$min_FL & .data$length_mm <= .data$max_FL
      )
    )

  sel_df$group_id <- interaction(
    dplyr::coalesce(sel_df$Sample_event, ""),
    sel_df$line_id,
    drop = TRUE
  )

  if (colour_by == "species") {
    sel_df$colour_id <- sel_df$species_display
  } else if (colour_by == "design") {
    sel_df$colour_id <- as.character(sel_df$sample_design_code)
  } else {
    sel_df$colour_id <- paste(sel_df$species_display, sel_df$sample_design_code, sep = " | ")
  }

  if (overall_only) {
    sel_df <- dplyr::filter(sel_df, .data$is_overall)
  }

  layers <- list()

  if (!highlight_in_range) {
    layers[[1]] <- ggplot2::geom_line(
      data = sel_df,
      ggplot2::aes(
        x = .data$length_mm,
        y = .data$plot_y,
        group = .data$group_id
      ),
      inherit.aes = FALSE,
      colour = "black",
      linewidth = linewidth,
      alpha = alpha,
      show.legend = FALSE
    )
  } else {
    layers[[length(layers) + 1]] <- ggplot2::geom_line(
      data = sel_df,
      ggplot2::aes(
        x = .data$length_mm,
        y = .data$plot_y,
        group = .data$group_id
      ),
      inherit.aes = FALSE,
      colour = "grey75",
      linewidth = linewidth * 0.8,
      alpha = alpha,
      show.legend = FALSE
    )

    if (any(!sel_df$is_overall, na.rm = TRUE)) {
      layers[[length(layers) + 1]] <- ggplot2::geom_line(
        data = dplyr::filter(sel_df, .data$in_range, !.data$is_overall),
        ggplot2::aes(
          x = .data$length_mm,
          y = .data$plot_y,
          group = .data$group_id,
          colour = .data$colour_id
        ),
        inherit.aes = FALSE,
        linewidth = linewidth,
        alpha = alpha,
        show.legend = show.legend
      )
    }

    if (any(sel_df$is_overall, na.rm = TRUE)) {
      layers[[length(layers) + 1]] <- ggplot2::geom_line(
        data = dplyr::filter(sel_df, .data$is_overall),
        ggplot2::aes(
          x = .data$length_mm,
          y = .data$plot_y,
          group = .data$group_id
        ),
        inherit.aes = FALSE,
        colour = "black",
        linewidth = linewidth * 1.25,
        alpha = alpha,
        show.legend = FALSE
      )
    }
  }

  p + layers
}
