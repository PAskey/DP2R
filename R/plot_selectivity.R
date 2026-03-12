#' Plot relative selectivity curves by species, design, and/or sampling event
#'
#' @description
#' Creates selectivity curves from the \code{DP2R} package lookup tables
#' (\code{sel_lookup} and \code{sel_lookup_event}). You can plot by net
#' \code{design}, by \code{Sample_event}, and optionally include event-level
#' (overall) curves plus the sub-design curves associated with each event.
#'
#' This version adds:
#' \itemize{
#'   \item Faceting by \code{Sample_event} via \code{facet = "event"} (when \code{Sample_event} is supplied)
#'   \item Faceting by \code{locale_name + year} via \code{facet = "locale_year"} (when those columns exist in \code{sel_lookup_event})
#'   \item Correct replication of design-level curves to each \code{Sample_event} so faceting works
#'   \item Event-level \dQuote{Overall (event)} curve is always drawn first in ordering and drawn thicker in black
#'   \item Automatic use of \code{DP2R::approx_select_spp} when requested species are not present in \code{sel_lookup}
#' }
#'
#' @param species Character vector of requested species codes (e.g., \code{"KO"}).
#' @param design Character vector of net design codes (e.g., \code{"SGN7"}).
#'   Only used when \code{Sample_event} is \code{NULL}.
#' @param Sample_event Optional character vector of sample event IDs. If supplied,
#'   curves are sourced from \code{sel_lookup_event} (plus optional sub-design curves).
#' @param plot_sub_designs Logical; when \code{TRUE} and \code{Sample_event} is supplied,
#'   also plot the sub-design curves listed in \code{sel_lookup_event$event_designs}.
#' @param classes Numeric vector of fork-length classes (mm). Must align with curve length.
#' @param facet Faceting mode: \code{"none"}, \code{"species"}, \code{"design"},
#'   \code{"event"}, or \code{"locale_year"}.
#'   \code{"event"} facets by \code{Sample_event}; \code{"locale_year"} facets by
#'   \code{locale_name + year}. Event-based facets are only meaningful when
#'   \code{Sample_event} is supplied.
#' @param colour_by Colour grouping: \code{"auto"}, \code{"species"}, \code{"design"}, or \code{"interaction"}.
#'
#' @return A \code{ggplot} object.
#'
#' @export
plot_selectivity <- function(species,
                             design = "SGN7",
                             Sample_event = NULL,
                             plot_sub_designs = TRUE,
                             classes = 75:900,
                             facet = c("none", "species", "design", "event", "locale_year"),
                             colour_by = c("auto", "species", "design", "interaction")) {

  colour_by_user <- !missing(colour_by)
  facet <- match.arg(facet)
  colour_by <- match.arg(colour_by)

  stopifnot(is.numeric(classes), length(classes) > 1)

  ev <- character(0)
  if (!is.null(Sample_event)) {
    ev <- unique(as.character(Sample_event))
    ev <- ev[!is.na(ev) & ev != ""]
  }
  use_event <- length(ev) > 0

  if (!requireNamespace("DP2R", quietly = TRUE)) {
    stop("plot_selectivity(): package 'DP2R' is required but not installed.", call. = FALSE)
  }

  # Load lookups
  utils::data("sel_lookup", package = "DP2R", envir = environment())
  sel_lookup <- get("sel_lookup", envir = environment(), inherits = FALSE)

  if (use_event) {
    utils::data("sel_lookup_event", package = "DP2R", envir = environment())
    sel_lookup_event <- get("sel_lookup_event", envir = environment(), inherits = FALSE)

    req_event_cols <- c("Sample_event", "species_code", "curve")
    miss <- setdiff(req_event_cols, names(sel_lookup_event))
    if (length(miss) > 0) {
      stop("plot_selectivity(): sel_lookup_event is missing: ", paste(miss, collapse = ", "),
           call. = FALSE)
    }

    if (facet == "locale_year") {
      req_facet_cols <- c("locale_name", "year")
      miss_facet <- setdiff(req_facet_cols, names(sel_lookup_event))
      if (length(miss_facet) > 0) {
        stop("plot_selectivity(): facet='locale_year' requires sel_lookup_event columns: ",
             paste(miss_facet, collapse = ", "), call. = FALSE)
      }
    }

    if (isTRUE(plot_sub_designs) && !("event_designs" %in% names(sel_lookup_event))) {
      stop("plot_selectivity(): plot_sub_designs=TRUE requires sel_lookup_event to have an 'event_designs' list-column.",
           call. = FALSE)
    }
  } else {
    req_cols <- c("species_code", "sample_design_code", "curve")
    miss <- setdiff(req_cols, names(sel_lookup))
    if (length(miss) > 0) {
      stop("plot_selectivity(): sel_lookup is missing: ", paste(miss, collapse = ", "),
           call. = FALSE)
    }
  }

  # Always load approximation table internally
  utils::data("approx_select_spp", package = "DP2R", envir = environment())
  approx_select_spp <- get("approx_select_spp", envir = environment(), inherits = FALSE)

  # Species mapping / approximation logic
  sel_species <- unique(as.character(sel_lookup$species_code))
  species_req <- unique(as.character(species))

  mapping <- tibble::tibble(species_req = species_req) |>
    dplyr::left_join(
      approx_select_spp |>
        dplyr::transmute(
          species_req = as.character(species_code),
          species_use = as.character(select_spp),
          min_FL = min_FL_gn,
          max_FL = max_FL_gn
        ),
      by = "species_req"
    ) |>
    dplyr::mutate(
      species_use = dplyr::case_when(
        species_req %in% sel_species ~ species_req,
        TRUE ~ species_use
      )
    )

  mapping_ok <- mapping |>
    dplyr::filter(!is.na(species_use))

  if (nrow(mapping_ok) == 0) {
    stop("None of the requested species exist in sel_lookup and no usable approximations were found.",
         call. = FALSE)
  }

  pieces <- list()

  if (use_event) {
    # Event-level curves (already have Sample_event)
    df_event <- sel_lookup_event |>
      dplyr::mutate(
        Sample_event = as.character(Sample_event),
        species_code = as.character(species_code)
      ) |>
      dplyr::filter(
        Sample_event %in% ev,
        species_code %in% mapping_ok$species_use
      ) |>
      dplyr::mutate(sample_design_code = "Overall (event)")

    pieces[["event"]] <- df_event

    if (isTRUE(plot_sub_designs)) {
      # Preserve facetting columns so replicated design rows can facet correctly
      keep_cols <- c("Sample_event", "species_code", "event_designs")
      extra_cols <- intersect(c("locale_name", "year"), names(sel_lookup_event))

      event_design_key <- sel_lookup_event |>
        dplyr::mutate(
          Sample_event = as.character(Sample_event),
          species_code = as.character(species_code)
        ) |>
        dplyr::filter(Sample_event %in% ev) |>
        dplyr::select(dplyr::all_of(c(keep_cols, extra_cols))) |>
        tidyr::unnest_longer(event_designs, values_to = "sample_design_code") |>
        dplyr::mutate(
          sample_design_code = stringr::str_trim(as.character(sample_design_code))
        ) |>
        dplyr::filter(!is.na(sample_design_code), sample_design_code != "") |>
        dplyr::distinct()

      dsgns <- unique(event_design_key$sample_design_code)

      if (length(dsgns) == 0) {
        warning("plot_selectivity(): plot_sub_designs=TRUE but no designs found in sel_lookup_event$event_designs for the requested Sample_event(s).",
                call. = FALSE)
      } else {
        # Design curves from sel_lookup, replicated per Sample_event
        df_design <- sel_lookup |>
          dplyr::mutate(
            species_code = as.character(species_code),
            sample_design_code = as.character(sample_design_code)
          ) |>
          dplyr::filter(
            species_code %in% mapping_ok$species_use,
            sample_design_code %in% dsgns
          )

        df_design_ev <- event_design_key |>
          dplyr::inner_join(
            df_design,
            by = c("species_code" = "species_code",
                   "sample_design_code" = "sample_design_code")
          )

        pieces[["design"]] <- df_design_ev
      }
    }
  } else {
    # Design-only mode
    df_design <- sel_lookup |>
      dplyr::mutate(
        species_code = as.character(species_code),
        sample_design_code = as.character(sample_design_code)
      ) |>
      dplyr::filter(
        species_code %in% mapping_ok$species_use,
        sample_design_code %in% as.character(design)
      )
    pieces[["design"]] <- df_design
  }

  df <- dplyr::bind_rows(pieces)
  if (nrow(df) == 0) stop("No curves found for requested filters.", call. = FALSE)

  # Map back to requested species label
  df <- df |>
    dplyr::inner_join(mapping_ok, by = c("species_code" = "species_use"),
                      relationship = "many-to-many") |>
    dplyr::mutate(
      species_display = dplyr::if_else(
        species_req == species_code,
        species_req,
        paste0(species_req, " (≈", species_code, ")")
      )
    )

  # Auto colour choice
  if (!colour_by_user && colour_by == "auto") {
    if (facet == "design") colour_by <- "species"
    if (facet == "species") colour_by <- "design"
    if (facet == "event")   colour_by <- "design"
    if (facet == "locale_year") colour_by <- "design"
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

  # Force "Overall (event)" to be the first level
  other_levels <- sort(unique(as.character(df$sample_design_code[df$sample_design_code != "Overall (event)"])))
  df$sample_design_code <- factor(
    as.character(df$sample_design_code),
    levels = c("Overall (event)", other_levels)
  )

  # Long format
  df_long <- df |>
    tidyr::unnest_longer(curve, indices_to = "idx") |>
    dplyr::mutate(
      length_mm = classes[idx],
      is_overall = (as.character(sample_design_code) == "Overall (event)"),
      line_id = paste(species_display, sample_design_code, sep = " | ")
    ) |>
    dplyr::arrange(
      dplyr::coalesce(Sample_event, ""),
      species_display,
      sample_design_code,
      length_mm
    )

  # Guard against mismatch
  if (max(df_long$idx, na.rm = TRUE) > length(classes)) {
    stop("plot_selectivity(): 'classes' length (", length(classes),
         ") is shorter than curve index max (", max(df_long$idx, na.rm = TRUE), ").",
         call. = FALSE)
  }

  # Colour grouping
  if (colour_by == "species") {
    df_long$colour_id <- df_long$species_display
    legend_title <- "Species"
  } else if (colour_by == "design") {
    df_long$colour_id <- as.character(df_long$sample_design_code)
    legend_title <- "Net design"
  } else {
    df_long$colour_id <- paste(df_long$species_display, df_long$sample_design_code, sep = " | ")
    legend_title <- "Species | Net"
  }

  # Range highlighting
  df_long <- df_long |>
    dplyr::mutate(
      in_range = dplyr::if_else(
        is.na(min_FL) | is.na(max_FL),
        TRUE,
        length_mm >= min_FL & length_mm <= max_FL
      )
    )

  # Use base::interaction() to keep event-specific lines distinct
  df_long$group_id <- interaction(
    dplyr::coalesce(df_long$Sample_event, ""),
    df_long$line_id,
    drop = TRUE
  )

  # Plot
  p <- ggplot2::ggplot(df_long, ggplot2::aes(length_mm, curve)) +
    ggplot2::geom_line(
      ggplot2::aes(group = group_id),
      colour = "grey75",
      linewidth = 0.8
    ) +
    ggplot2::geom_line(
      data = dplyr::filter(df_long, in_range, !is_overall),
      ggplot2::aes(group = group_id, colour = colour_id),
      linewidth = 1.3
    ) +
    ggplot2::geom_line(
      data = dplyr::filter(df_long, is_overall),
      ggplot2::aes(group = group_id),
      colour = "black",
      linewidth = 1.8
    ) +
    ggplot2::labs(
      x = "Fork length (mm)",
      y = "Relative selectivity",
      colour = legend_title
    ) +
    ggplot2::theme_bw()

  # Faceting
  if (facet == "species") {
    p <- p + ggplot2::facet_wrap(~species_display)
  } else if (facet == "design") {
    p <- p + ggplot2::facet_wrap(~sample_design_code)
  } else if (facet == "event") {
    if (!use_event) {
      warning("plot_selectivity(): facet='event' requested but no Sample_event supplied; using facet='none'.",
              call. = FALSE)
    } else {
      p <- p + ggplot2::facet_wrap(~Sample_event)
    }
  } else if (facet == "locale_year") {
    if (!use_event) {
      warning("plot_selectivity(): facet='locale_year' requested but no Sample_event supplied; using facet='none'.",
              call. = FALSE)
    } else {
      p <- p + ggplot2::facet_wrap(~locale_name + year)
    }
  }

  p
}
