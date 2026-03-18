#' Prepare selectivity curve data for plotting
#'
#' @description
#' Adds plot-oriented grouping and colouring fields to the output of
#' [selectivity_curves()]. This helper is primarily intended for internal use by
#' [plot_selectivity()] and [selectivity_layer()], but may also be useful in
#' custom plotting workflows.
#'
#' @param x A data frame returned by [selectivity_curves()].
#' @param colour_by Colour grouping mode: \code{"auto"}, \code{"species"},
#'   \code{"design"}, or \code{"interaction"}.
#' @param facet Intended faceting mode used only to resolve
#'   \code{colour_by = "auto"}. One of \code{"none"}, \code{"species"},
#'   \code{"design"}, \code{"event"}, or \code{"locale_year"}.
#'
#' @return
#' A tibble with plot-specific columns added:
#' \itemize{
#'   \item \code{plot_y}: copy of \code{selectivity}
#'   \item \code{line_id}: human-readable line identifier
#'   \item \code{group_id}: grouping variable for line drawing
#'   \item \code{colour_id}: colour grouping variable
#'   \item \code{in_range}: logical flag based on \code{min_FL} and \code{max_FL}
#'   \item \code{legend_title}: legend title associated with \code{colour_id}
#' }
#'
#' @export
prep_selectivity_plot_data <- function(x,
                                       colour_by = c("auto", "species", "design", "interaction"),
                                       facet = c("none", "species", "design", "event", "locale_year")) {

  colour_by <- match.arg(colour_by)
  facet <- match.arg(facet)

  req <- c("species_display", "sample_design_code", "length_mm", "selectivity", "is_overall")
  miss <- setdiff(req, names(x))
  if (length(miss) > 0) {
    stop(
      "prep_selectivity_plot_data(): input is missing required columns: ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }

  df <- tibble::as_tibble(x)

  if (colour_by == "auto") {
    if (facet %in% c("design", "event", "locale_year")) {
      colour_by <- "species"
    } else if (facet == "species") {
      colour_by <- "design"
    } else {
      if (length(unique(df$species_display)) > 1 &&
          length(unique(df$sample_design_code)) == 1) {
        colour_by <- "species"
      } else if (length(unique(df$species_display)) == 1 &&
                 length(unique(df$sample_design_code)) > 1) {
        colour_by <- "design"
      } else {
        colour_by <- "interaction"
      }
    }
  }

  df <- df |>
    dplyr::mutate(
      plot_y = .data$selectivity,
      line_id = paste(.data$species_display, .data$sample_design_code, sep = " | "),
      in_range = dplyr::if_else(
        is.na(.data$min_FL) | is.na(.data$max_FL),
        TRUE,
        .data$length_mm >= .data$min_FL & .data$length_mm <= .data$max_FL
      )
    )

  df$group_id <- interaction(
    dplyr::coalesce(df$Sample_event, ""),
    df$line_id,
    drop = TRUE
  )

  if (colour_by == "species") {
    df$colour_id <- df$species_display
    legend_title <- "Species"
  } else if (colour_by == "design") {
    df$colour_id <- as.character(df$sample_design_code)
    legend_title <- "Net design"
  } else {
    df$colour_id <- paste(df$species_display, df$sample_design_code, sep = " | ")
    legend_title <- "Species | Net"
  }

  df$legend_title <- legend_title

  tibble::as_tibble(df)
}
