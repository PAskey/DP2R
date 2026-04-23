#' Clean ambiguous sampling dates in lake water data
#'
#' Fixes cases where month/day may be reversed (MDY vs DMY) using
#' seasonal temperature rules with directional safeguards.
#'
#' Rules are applied only to ambiguous dates where day <= 12.
#' A flip is only allowed when:
#' \itemize{
#'   \item the flipped month lands in a different seasonal category
#'   \item the observed temperature is directionally consistent with that shift
#' }
#'
#' Seasonal categories are:
#' \itemize{
#'   \item winter = 12, 1, 2
#'   \item shoulder = 3, 4, 11
#'   \item warm = 5, 6, 7, 8, 9, 10
#' }
#'
#' When a sampling event is flagged, the same date reversal is applied to all
#' parameters collected for that `WBID` and `sampling_dt`.
#'
#' @param df Data frame with lake water data.
#'
#' @return Data frame with corrected `sampling_dt`, original parsed date in
#'   `sampling_dt_original`, and `flip_reason`.
#' @keywords internal
clean_sampling_dates <- function(df) {

  sampling_chr <- as.character(df$sampling_dt)

  dt_mdy <- suppressWarnings(lubridate::mdy_hm(sampling_chr))
  dt_dmy <- suppressWarnings(lubridate::dmy_hm(sampling_chr))

  month_mdy <- lubridate::month(dt_mdy)
  day_mdy <- lubridate::day(dt_mdy)
  month_dmy <- lubridate::month(dt_dmy)

  month_category <- function(x) {
    dplyr::case_when(
      x %in% c(12, 1, 2) ~ "winter",
      x %in% c(3, 4, 11) ~ "shoulder",
      x %in% c(5, 6, 7, 8, 9, 10) ~ "warm",
      TRUE ~ NA_character_
    )
  }

  category_rank <- function(x) {
    dplyr::case_when(
      x == "winter" ~ 1L,
      x == "shoulder" ~ 2L,
      x == "warm" ~ 3L,
      TRUE ~ NA_integer_
    )
  }

  df_flag <- df |>
    dplyr::mutate(
      sampling_chr = sampling_chr,
      dt_mdy = dt_mdy,
      dt_dmy = dt_dmy,
      month_mdy = month_mdy,
      day_mdy = day_mdy,
      month_dmy = month_dmy,
      category_mdy = month_category(month_mdy),
      category_dmy = month_category(month_dmy),
      rank_mdy = category_rank(category_mdy),
      rank_dmy = category_rank(category_dmy),
      ambiguous = day_mdy <= 12,
      different_category = !is.na(category_mdy) & !is.na(category_dmy) & category_mdy != category_dmy,
      moving_to_warmer = rank_dmy > rank_mdy,
      moving_to_colder = rank_dmy < rank_mdy
    )

  temp_df <- df_flag |>
    dplyr::filter(stringr::str_detect(test_name, "Temperature"))

  # Too warm for winter months: winter -> shoulder/warm
  rule_winter <- temp_df |>
    dplyr::filter(
      ambiguous,
      different_category,
      month_mdy %in% c(12, 1, 2),
      depth > 0,
      result >= 7,
      moving_to_warmer
    ) |>
    dplyr::distinct(WBID, sampling_chr) |>
    dplyr::mutate(flip_reason = "winter_rule")

  # Too warm for shoulder months: shoulder -> warm only
  # Excluded for region_code 1 and 2
  rule_shoulder_warm <- temp_df |>
    dplyr::filter(
      ambiguous,
      different_category,
      !(region_code %in% c(1, 2, "1", "2")),
      month_mdy %in% c(3, 4, 11),
      depth > 1,
      result >= 14,
      moving_to_warmer
    ) |>
    dplyr::distinct(WBID, sampling_chr) |>
    dplyr::mutate(flip_reason = "shoulder_rule_warm")

  # Too cold for warm months: warm -> shoulder/winter
  rule_warm_cold <- temp_df |>
    dplyr::filter(
      ambiguous,
      different_category,
      month_mdy %in% c(5, 6, 7, 8, 9, 10),
      depth %in% c(1,2),
      result <= 4,
      moving_to_colder
    ) |>
    dplyr::distinct(WBID, sampling_chr) |>
    dplyr::mutate(flip_reason = "warm_rule_cold")

  suspect_events <- dplyr::bind_rows(
    rule_winter,
    rule_shoulder_warm,
    rule_warm_cold
  ) |>
    dplyr::group_by(WBID, sampling_chr) |>
    dplyr::summarise(
      flip_reason = paste(unique(flip_reason), collapse = ";"),
      .groups = "drop"
    )

  flip_ids <- interaction(
    suspect_events$WBID,
    suspect_events$sampling_chr,
    drop = TRUE
  )

  df_clean <- df_flag |>
    dplyr::mutate(
      event_id = interaction(WBID, sampling_chr, drop = TRUE)
    ) |>
    dplyr::left_join(
      suspect_events,
      by = c("WBID", "sampling_chr")
    ) |>
    dplyr::mutate(
      needs_flip = event_id %in% flip_ids,
      sampling_dt_original = dt_mdy,
      sampling_dt = dplyr::if_else(needs_flip, dt_dmy, dt_mdy),
      flip_reason = dplyr::if_else(needs_flip, flip_reason, NA_character_)
    )

  n_events <- nrow(suspect_events)
  n_wbids <- dplyr::n_distinct(suspect_events$WBID)

  message(
    n_events, " sampling events corrected across ",
    n_wbids, " WBIDs."
  )

  df_clean |>
    dplyr::select(
      -sampling_chr, -dt_mdy, -dt_dmy,
      -month_mdy, -day_mdy, -month_dmy,
      -category_mdy, -category_dmy,
      -rank_mdy, -rank_dmy,
      -ambiguous, -different_category,
      -moving_to_warmer, -moving_to_colder,
      -event_id, -needs_flip
    )
}
