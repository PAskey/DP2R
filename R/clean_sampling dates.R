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

  # Expected format: YYYY-MM-DD HH:MM:SS
  dt_ymd <- suppressWarnings(lubridate::ymd_hms(sampling_chr))

  # Alternate interpretation if month/day were reversed:
  # YYYY-DD-MM HH:MM:SS
  dt_ydm <- suppressWarnings(lubridate::ydm_hms(sampling_chr))

  month_ymd <- lubridate::month(dt_ymd)
  day_ymd <- lubridate::day(dt_ymd)
  month_ydm <- lubridate::month(dt_ydm)

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
      dt_ymd = dt_ymd,
      dt_ydm = dt_ydm,
      month_ymd = month_ymd,
      day_ymd = day_ymd,
      month_ydm = month_ydm,
      category_ymd = month_category(month_ymd),
      category_ydm = month_category(month_ydm),
      rank_ymd = category_rank(category_ymd),
      rank_ydm = category_rank(category_ydm),

      # Ambiguous only if both month and day are <= 12
      ambiguous =
        !is.na(month_ymd) &
        !is.na(day_ymd) &
        month_ymd <= 12 &
        day_ymd <= 12,

      different_category =
        !is.na(category_ymd) &
        !is.na(category_ydm) &
        category_ymd != category_ydm,

      moving_to_warmer = rank_ydm > rank_ymd,
      moving_to_colder = rank_ydm < rank_ymd
    )

  temp_df <- df_flag |>
    dplyr::filter(stringr::str_detect(test_name, "Temperature"))

  # Too warm for winter months: winter -> shoulder/warm
  rule_winter <- temp_df |>
    dplyr::filter(
      ambiguous,
      different_category,
      month_ymd %in% c(12, 1, 2),
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
      month_ymd %in% c(3, 4, 11),
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
      month_ymd %in% c(5, 6, 7, 8, 9, 10),
      depth %in% c(1, 2),
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
      sampling_dt_original = dt_ymd,
      sampling_dt = dplyr::if_else(needs_flip, dt_ydm, dt_ymd),
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
      -sampling_chr,
      -dt_ymd,
      -dt_ydm,
      -month_ymd,
      -day_ymd,
      -month_ydm,
      -category_ymd,
      -category_ydm,
      -rank_ymd,
      -rank_ydm,
      -ambiguous,
      -different_category,
      -moving_to_warmer,
      -moving_to_colder,
      -event_id,
      -needs_flip
    )
}
