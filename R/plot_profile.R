#' Plot lake water profile summaries by month
#'
#' Creates faceted depth profile plots from a `Profile` table. Profile lines are
#' averaged by week and plotted in monthly facets, while habitat shading and
#' thermocline are based on month-year averages.
#'
#' @param Profile Data frame with profile summary data.
#' @param WBID Optional character vector of WBIDs to include.
#' @param months Optional numeric or character vector of months to include (1-12).
#' @param thermo_threshold Numeric threshold for thermocline detection.
#'
#' @return A ggplot object.
#' @export
plot_profile <- function(Profile,
                         WBID = NULL,
                         months = NULL,
                         thermo_threshold = 3.5) {

  required_cols <- c(
    "year", "month", "sampling_dt", "test_name",
    "depth", "Mean", "unit_code", "WBID"
  )
  missing_cols <- setdiff(required_cols, names(Profile))

  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Profile is missing required columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  if (!is.null(WBID)) {
    Profile <- Profile |>
      dplyr::filter(.data$WBID %in% .env$WBID)
  }

  if (!is.null(months)) {
    Profile <- Profile |>
      dplyr::filter(as.integer(.data$month) %in% as.integer(.env$months))
  }

  Profile <- Profile |>
    dplyr::filter(!unit_code %in% c("%", "% sat", "pH")) |>
    dplyr::mutate(
      WBID = as.character(WBID),
      sampling_dt = as.Date(sampling_dt),
      month = as.integer(month),
      year = as.integer(year),
      week = lubridate::floor_date(sampling_dt, unit = "week")
    )

  # Weekly lines
  weekly_profile <- Profile |>
    dplyr::group_by(WBID, year, month, week, test_name, unit_code, depth) |>
    dplyr::summarise(
      Mean = mean(Mean, na.rm = TRUE),
      .groups = "drop"
    )

  # Month-year averages for red blocks and thermocline
  monthly_profile <- Profile |>
    dplyr::group_by(WBID, year, month, test_name, unit_code, depth) |>
    dplyr::summarise(
      Mean = mean(Mean, na.rm = TRUE),
      .groups = "drop"
    )

  # Month-year thermocline
  thermo <- monthly_profile |>
    dplyr::filter(test_name == "Temperature") |>
    dplyr::arrange(WBID, year, month, depth) |>
    dplyr::group_by(WBID, year, month) |>
    dplyr::filter(dplyr::n() >= 3) |>
    dplyr::mutate(
      dT = dplyr::lead(Mean) - Mean,
      dz = dplyr::lead(depth) - depth,
      gradient = dT / dz,
      thermo_depth = depth + dz / 2
    ) |>
    dplyr::filter(
      !is.na(gradient),
      !is.na(thermo_depth),
      !is.infinite(gradient),
      !is.infinite(thermo_depth),
      !is.na(dz),
      dz != 0
    )

  thermocline_depths <- thermo |>
    dplyr::mutate(abs_grad = abs(gradient)) |>
    dplyr::group_by(WBID, year, month) |>
    dplyr::slice_max(abs_grad, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::filter(abs_grad >= thermo_threshold) |>
    dplyr::mutate(line_type = "Thermocline") |>
    dplyr::select(WBID, year, month, thermo_depth, line_type)

  # Month-year temperature stats for top red zone
  temp_stats <- monthly_profile |>
    dplyr::filter(test_name == "Temperature") |>
    dplyr::group_by(WBID, year, month) |>
    dplyr::summarise(
      shallowest_temp_depth = min(depth, na.rm = TRUE),
      shallowest_temp_value = Mean[which.min(depth)],
      top = suppressWarnings(min(depth[Mean < 24], na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      top = ifelse(is.infinite(top), NA_real_, top),
      observed_top_fail = !is.na(shallowest_temp_value) & shallowest_temp_value >= 24
    )

  # Month-year oxygen stats for bottom red zone
  oxy_stats <- monthly_profile |>
    dplyr::filter(test_name == "Dissolved Oxygen") |>
    dplyr::group_by(WBID, year, month) |>
    dplyr::summarise(
      deepest_oxy_depth = max(depth, na.rm = TRUE),
      deepest_oxy_value = Mean[which.max(depth)],
      bottom = suppressWarnings(max(depth[Mean >= 2], na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      bottom = ifelse(is.infinite(bottom), NA_real_, bottom),
      observed_bottom_fail = !is.na(deepest_oxy_value) & deepest_oxy_value < 2
    )

  band <- temp_stats |>
    dplyr::full_join(oxy_stats, by = c("WBID", "year", "month"))

  red_top <- band |>
    dplyr::filter(
      observed_top_fail,
      !is.na(top),
      top > shallowest_temp_depth
    ) |>
    dplyr::transmute(
      WBID, year, month,
      ymin = shallowest_temp_depth,
      ymax = top
    )

  red_bottom <- band |>
    dplyr::filter(
      observed_bottom_fail,
      !is.na(bottom),
      bottom < deepest_oxy_depth
    ) |>
    dplyr::transmute(
      WBID, year, month,
      ymin = bottom,
      ymax = deepest_oxy_depth
    )

  red_zones <- dplyr::bind_rows(red_top, red_bottom)

  weekly_plot <- weekly_profile |>
    dplyr::filter(test_name %in% c("Temperature", "Dissolved Oxygen")) |>
    dplyr::mutate(
      parameter = factor(
        dplyr::case_when(
          test_name == "Temperature" ~ "Temperature",
          test_name == "Dissolved Oxygen" ~ "Dissolved Oxygen"
        ),
        levels = c("Temperature", "Dissolved Oxygen")
      ),
      facet_month = factor(month, levels = 1:12, labels = month.abb)
    )

  red_zones <- red_zones |>
    dplyr::mutate(
      facet_month = factor(
        month,
        levels = 1:12,
        labels = month.abb
      )
    )

  thermocline_depths <- thermocline_depths |>
    dplyr::mutate(
      facet_month = factor(
        month,
        levels = 1:12,
        labels = month.abb
      )
    )

  ggplot2::ggplot(
    weekly_plot,
    ggplot2::aes(
      x = Mean,
      y = depth,
      colour = parameter,
      group = interaction(parameter, WBID, year, month, week)
    )
  ) +
    ggplot2::scale_y_reverse() +
    ggplot2::geom_rect(
      data = red_zones,
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = ymin,
        ymax = ymax
      ),
      inherit.aes = FALSE,
      fill = "red",
      alpha = 0.08
    ) +
    ggplot2::geom_path(alpha = 0.5, linewidth = 0.8) +
    ggplot2::geom_hline(
      data = thermocline_depths,
      ggplot2::aes(
        yintercept = thermo_depth,
        linetype = line_type
      ),
      colour = "black",
      linewidth = 0.6,
      show.legend = TRUE
    ) +
    ggplot2::facet_wrap(~facet_month) +
    ggplot2::scale_colour_manual(
      values = c(
        "Temperature" = viridisLite::viridis(11, option = "D")[2],
        "Dissolved Oxygen" = viridisLite::viridis(11, option = "D")[8]
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Thermocline" = "dashed"),
      name = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "mg/L or °C",
      y = "Depth (m)",
      colour = "Parameter"
    )+
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(
          colour = c("#440154FF", "#35B779FF"),  # explicitly set colours
          alpha = 1,
          linewidth = 1
        )
      )
    )
}
