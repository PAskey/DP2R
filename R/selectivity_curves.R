#' Retrieve relative gillnet selectivity curves in tidy format
#'
#' @description
#' Returns stored relative selectivity curves from the \pkg{DP2R} lookup tables
#' as a tidy long-format data frame. Curves may be retrieved by
#' \code{sample_design_code}, by \code{Sample_event}, or both event-level
#' overall curves and their associated sub-design curves.
#'
#' This is a generic data-access function intended for reuse in plots, tables,
#' summaries, exports, and QA workflows. Unlike [plot_selectivity()], it does
#' not add plot-specific grouping, colouring, or faceting metadata.
#'
#' Requested species that do not have direct selectivity curves are mapped to
#' proxy species using \code{approx_select_spp}. The returned data retain both
#' the requested species code and the species code actually used to retrieve the
#' curve.
#'
#' @param species Character vector of requested species codes.
#' @param sample_design_code Character vector of design codes. Used only when
#'   \code{Sample_event} is \code{NULL}, or when
#'   \code{include_sub_designs = FALSE} and explicit design-level curves are
#'   desired.
#' @param Sample_event Optional character vector of sample event IDs. If
#'   supplied, event-level curves are retrieved from \code{sel_lookup_event}.
#' @param include_sub_designs Logical; if \code{TRUE} and \code{Sample_event} is
#'   supplied, also include the design-level curves listed in
#'   \code{sel_lookup_event$event_designs} for each event.
#' @param classes Optional numeric vector of length classes corresponding to the
#'   stored selectivity curves. Defaults to package data \code{sel_classes}.
#'
#' @return
#' A tibble with one row per length class per curve. Columns may include:
#' \itemize{
#'   \item \code{species_req}: requested species code
#'   \item \code{species_code}: species code used to retrieve the stored curve
#'   \item \code{species_display}: display label indicating proxy use where relevant
#'   \item \code{sample_design_code}: net design code, or \dQuote{Overall (event)}
#'     for event-level overall curves
#'   \item \code{Sample_event}: sample event ID, when applicable
#'   \item \code{curve_source}: \code{"event"} or \code{"design"}
#'   \item \code{is_overall}: logical flag for event-level overall curves
#'   \item \code{length_mm}: fork length class in mm
#'   \item \code{selectivity}: relative selectivity value on a 0--1 scale
#'   \item \code{min_FL}, \code{max_FL}: approximation guidance range, if available
#'   \item \code{locale_name}, \code{year}: retained when present in event lookup data
#' }
#'
#' @export
selectivity_curves <- function(species,
                               sample_design_code = "SGN7",
                               Sample_event = NULL,
                               include_sub_designs = TRUE) {

  if (!requireNamespace("DP2R", quietly = TRUE)) {
    stop("selectivity_curves(): package 'DP2R' is required but not installed.",
         call. = FALSE)
  }

  utils::data("sel_lookup", package = "DP2R", envir = environment())
  utils::data("sel_lookup_event", package = "DP2R", envir = environment())
  utils::data("sel_classes", package = "DP2R", envir = environment())
  utils::data("approx_select_spp", package = "DP2R", envir = environment())

  sel_lookup <- get("sel_lookup", envir = environment(), inherits = FALSE)
  sel_lookup_event <- get("sel_lookup_event", envir = environment(), inherits = FALSE)
  classes <- get("sel_classes", envir = environment(), inherits = FALSE)
  approx_select_spp <- get("approx_select_spp", envir = environment(), inherits = FALSE)

  species_req <- unique(as.character(species))
  species_req <- species_req[!is.na(species_req) & species_req != ""]
  if (length(species_req) == 0) {
    stop("At least one non-missing species code must be supplied.", call. = FALSE)
  }

  ev <- character(0)
  if (!is.null(Sample_event)) {
    ev <- unique(as.character(Sample_event))
    ev <- ev[!is.na(ev) & ev != ""]
  }
  use_event <- length(ev) > 0

  req_sel_cols <- c("species_code", "sample_design_code", "curve")
  miss_sel <- setdiff(req_sel_cols, names(sel_lookup))
  if (length(miss_sel) > 0) {
    stop(
      "selectivity_curves(): `sel_lookup` is missing required columns: ",
      paste(miss_sel, collapse = ", "),
      call. = FALSE
    )
  }

  if (use_event) {
    req_event_cols <- c("Sample_event", "species_code", "curve")
    miss_event <- setdiff(req_event_cols, names(sel_lookup_event))
    if (length(miss_event) > 0) {
      stop(
        "selectivity_curves(): `sel_lookup_event` is missing required columns: ",
        paste(miss_event, collapse = ", "),
        call. = FALSE
      )
    }

    if (isTRUE(include_sub_designs) && !("event_designs" %in% names(sel_lookup_event))) {
      stop(
        "selectivity_curves(): `include_sub_designs = TRUE` requires ",
        "`sel_lookup_event` to contain an `event_designs` list-column.",
        call. = FALSE
      )
    }
  }

  sel_species <- unique(as.character(sel_lookup$species_code))

  mapping <- tibble::tibble(species_req = species_req) |>
    dplyr::left_join(
      approx_select_spp |>
        dplyr::transmute(
          species_req = as.character(.data$species_code),
          species_code = as.character(.data$select_spp),
          min_FL = .data$min_FL_gn,
          max_FL = .data$max_FL_gn
        ),
      by = "species_req"
    ) |>
    dplyr::mutate(
      species_code = dplyr::if_else(
        .data$species_req %in% sel_species,
        .data$species_req,
        .data$species_code
      )
    ) |>
    dplyr::filter(!is.na(.data$species_code))

  if (nrow(mapping) == 0) {
    stop(
      "None of the requested species exist in `sel_lookup`, and no usable approximations were found.",
      call. = FALSE
    )
  }

  pieces <- list()

  if (use_event) {
    df_event <- sel_lookup_event |>
      dplyr::mutate(
        Sample_event = as.character(.data$Sample_event),
        species_code = as.character(.data$species_code)
      ) |>
      dplyr::filter(
        .data$Sample_event %in% ev,
        .data$species_code %in% mapping$species_code
      ) |>
      dplyr::mutate(
        sample_design_code = "Overall (event)",
        curve_source = "event",
        is_overall = TRUE
      )

    pieces[["event"]] <- df_event

    if (isTRUE(include_sub_designs)) {
      keep_cols <- c("Sample_event", "species_code", "event_designs")
      extra_cols <- intersect(c("locale_name", "year"), names(sel_lookup_event))

      event_design_key <- sel_lookup_event |>
        dplyr::mutate(
          Sample_event = as.character(.data$Sample_event),
          species_code = as.character(.data$species_code)
        ) |>
        dplyr::filter(.data$Sample_event %in% ev) |>
        dplyr::select(dplyr::all_of(c(keep_cols, extra_cols))) |>
        tidyr::unnest_longer(.data$event_designs, values_to = "sample_design_code") |>
        dplyr::mutate(
          sample_design_code = stringr::str_trim(as.character(.data$sample_design_code))
        ) |>
        dplyr::filter(!is.na(.data$sample_design_code), .data$sample_design_code != "") |>
        dplyr::distinct()

      dsgns <- unique(event_design_key$sample_design_code)

      if (length(dsgns) == 0) {
        warning(
          "selectivity_curves(): `include_sub_designs = TRUE` but no event sub-designs ",
          "were found for the requested `Sample_event` values.",
          call. = FALSE
        )
      } else {
        df_design <- sel_lookup |>
          dplyr::mutate(
            species_code = as.character(.data$species_code),
            sample_design_code = as.character(.data$sample_design_code)
          ) |>
          dplyr::filter(
            .data$species_code %in% mapping$species_code,
            .data$sample_design_code %in% dsgns
          ) |>
          dplyr::mutate(
            curve_source = "design",
            is_overall = FALSE
          )

        df_design_ev <- event_design_key |>
          dplyr::inner_join(df_design, by = c("species_code", "sample_design_code"))

        pieces[["design"]] <- df_design_ev
      }
    }
  } else {
    design_use <- unique(as.character(sample_design_code))
    design_use <- design_use[!is.na(design_use) & design_use != ""]
    if (length(design_use) == 0) {
      stop(
        "At least one non-missing `sample_design_code` must be supplied when `Sample_event` is NULL.",
        call. = FALSE
      )
    }

    df_design <- sel_lookup |>
      dplyr::mutate(
        species_code = as.character(.data$species_code),
        sample_design_code = as.character(.data$sample_design_code)
      ) |>
      dplyr::filter(
        .data$species_code %in% mapping$species_code,
        .data$sample_design_code %in% design_use
      ) |>
      dplyr::mutate(
        curve_source = "design",
        is_overall = FALSE
      )

    pieces[["design"]] <- df_design
  }

  df <- dplyr::bind_rows(pieces)
  if (nrow(df) == 0) {
    stop("No selectivity curves found for the requested filters.", call. = FALSE)
  }

  # Ensure Sample_event column exists for downstream consistency
  if (!"Sample_event" %in% names(df)) {
    df$Sample_event <- NA_character_
  }
  if (!"locale_name" %in% names(df)) df$locale_name <- NA_character_
  if (!"year" %in% names(df)) df$year <- NA_integer_

  df <- df |>
    dplyr::inner_join(
      mapping,
      by = "species_code",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      species_display = dplyr::if_else(
        .data$species_req == .data$species_code,
        .data$species_req,
        paste0(.data$species_req, " (≈", .data$species_code, ")")
      )
    )

  other_levels <- sort(unique(
    as.character(df$sample_design_code[as.character(df$sample_design_code) != "Overall (event)"])
  ))

  df$sample_design_code <- factor(
    as.character(df$sample_design_code),
    levels = c("Overall (event)", other_levels)
  )

  df_long <- df |>
    tidyr::unnest_longer(.data$curve, indices_to = "idx")

  if (max(df_long$idx, na.rm = TRUE) > length(classes)) {
    stop(
      "selectivity_curves(): `classes` length (", length(classes),
      ") is shorter than the maximum curve index (", max(df_long$idx, na.rm = TRUE), ").",
      call. = FALSE
    )
  }

  df_long <- df_long |>
    dplyr::mutate(
      length_mm = classes[.data$idx],
      selectivity = .data$curve
    ) |>
    dplyr::select(-curve, -idx)|>
    dplyr::arrange(
      dplyr::coalesce(.data$Sample_event, ""),
      .data$species_req,
      .data$sample_design_code,
      .data$length_mm
    )

  tibble::as_tibble(df_long)
}
