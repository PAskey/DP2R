#' Plot relative selectivity curves by species, design, and/or sampling event
#'
#' @description
#' Creates a \pkg{ggplot2} visualization of relative selectivity curves from the
#' lookup tables in \pkg{DP2R}. Curves can be plotted by
#' \code{sample_design_code}, by \code{Sample_event}, and optionally include
#' event-level overall curves plus the sub-design curves associated with each
#' event.
#'
#' This function is a thin plotting wrapper around [selectivity_curves()] and
#' [prep_selectivity_plot_data()].
#'
#' @param species Character vector of requested species codes.
#' @param sample_design_code Character vector of design codes. Used only when
#'   \code{Sample_event} is \code{NULL}.
#' @param Sample_event Optional character vector of sample event IDs.
#' @param include_sub_designs Logical; if \code{TRUE} and \code{Sample_event} is
#'   supplied, also plot design-level curves listed in
#'   \code{sel_lookup_event$event_designs}.
#' @param classes Optional numeric vector of length classes corresponding to the
#'   stored selectivity curves. Defaults to package data \code{sel_classes}.
#' @param facet Faceting mode: \code{"none"}, \code{"species"}, \code{"design"},
#'   \code{"event"}, or \code{"locale_year"}.
#' @param colour_by Colour grouping: \code{"auto"}, \code{"species"},
#'   \code{"design"}, or \code{"interaction"}.
#'
#' @return A \code{ggplot} object.
#'
#' @export
plot_selectivity <- function(species,
                             sample_design_code = "SGN7",
                             Sample_event = NULL,
                             include_sub_designs = TRUE,
                             facet = c("none", "species", "design", "event", "locale_year"),
                             colour_by = c("auto", "species", "design", "interaction")) {

  facet <- match.arg(facet)
  colour_by <- match.arg(colour_by)

  df <- selectivity_curves(
    species = species,
    sample_design_code = sample_design_code,
    Sample_event = Sample_event,
    include_sub_designs = include_sub_designs
  )

  df_plot <- prep_selectivity_plot_data(
    x = df,
    colour_by = colour_by,
    facet = facet
  )

  use_event <- "Sample_event" %in% names(df_plot) &&
    any(!is.na(df_plot$Sample_event) & df_plot$Sample_event != "")

  legend_title <- unique(df_plot$legend_title)[1]

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(.data$length_mm, .data$plot_y)) +
    ggplot2::geom_line(
      ggplot2::aes(group = .data$group_id),
      colour = "grey75",
      linewidth = 0.8
    ) +
    ggplot2::geom_line(
      data = dplyr::filter(df_plot, .data$in_range, !.data$is_overall),
      ggplot2::aes(group = .data$group_id, colour = .data$colour_id),
      linewidth = 1.3
    ) +
    ggplot2::geom_line(
      data = dplyr::filter(df_plot, .data$is_overall),
      ggplot2::aes(group = .data$group_id),
      colour = "black",
      linewidth = 1.8
    ) +
    ggplot2::labs(
      x = "Fork length (mm)",
      y = "Relative selectivity",
      colour = legend_title
    ) +
    ggplot2::theme_bw()

  if (facet == "species") {
    p <- p + ggplot2::facet_wrap(~species_display)
  } else if (facet == "design") {
    p <- p + ggplot2::facet_wrap(~sample_design_code)
  } else if (facet == "event") {
    if (!use_event) {
      warning(
        "plot_selectivity(): `facet = \"event\"` requested but no `Sample_event` was supplied; plotting without faceting.",
        call. = FALSE
      )
    } else {
      p <- p + ggplot2::facet_wrap(~Sample_event)
    }
  } else if (facet == "locale_year") {
    if (!use_event) {
      warning(
        "plot_selectivity(): `facet = \"locale_year\"` requested but no `Sample_event` was supplied; plotting without faceting.",
        call. = FALSE
      )
    } else {
      req <- c("locale_name", "year")
      miss <- setdiff(req, names(df_plot))
      if (length(miss) > 0) {
        warning(
          "plot_selectivity(): `facet = \"locale_year\"` requested but plotting data are missing: ",
          paste(miss, collapse = ", "),
          ". Plotting without faceting.",
          call. = FALSE
        )
      } else {
        p <- p + ggplot2::facet_wrap(~locale_name + year)
      }
    }
  }

  p
}
