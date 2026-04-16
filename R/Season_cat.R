#' Categorize dates into custom seasons using season start dates
#'
#' Assigns seasonal categories based on four user-defined season start
#' boundaries expressed as month-day strings (for example `"12-01"` for
#' December 1). Inputs must be date-like values. The function assumes that
#' each supplied boundary marks the start of a season, and that seasons
#' proceed in calendar order until the next boundary.
#'
#' This is useful when seasons do not match meteorological definitions.
#' For example, winter can be defined as December 1 through March 31 by
#' setting `winter_start = "12-01"` and `spring_start = "04-01"`.
#'
#' @param x A vector of dates (`Date`, `POSIXct`, or `POSIXt`).
#' @param winter_start Character string giving the start of winter in `"MM-DD"`
#'   format. Default is `"12-01"`.
#' @param spring_start Character string giving the start of spring in `"MM-DD"`
#'   format. Default is `"04-01"`.
#' @param summer_start Character string giving the start of summer in `"MM-DD"`
#'   format. Default is `"06-01"`.
#' @param fall_start Character string giving the start of fall in `"MM-DD"`
#'   format. Default is `"09-01"`.
#' @param labels Character vector of length 4 giving the labels for the four
#'   seasons. Defaults to `c("WIN", "SPR", "SUM", "FAL")`.
#'
#' @return A character vector of seasonal categories with the same length as `x`.
#'
#' @details
#' The function compares only the month and day of each date, so season
#' assignment is stable across leap years and does not depend on day-of-year.
#'
#' Seasons are defined as:
#' - winter: `winter_start` through the day before `spring_start`
#' - spring: `spring_start` through the day before `summer_start`
#' - summer: `summer_start` through the day before `fall_start`
#' - fall: `fall_start` through the day before `winter_start`
#'
#' @examples
#' dates <- as.Date(c("2023-01-15", "2023-04-15", "2023-07-15", "2023-10-15"))
#' Season_cat(dates)
#'
#' Season_cat(
#'   dates,
#'   winter_start = "11-15",
#'   spring_start = "03-15",
#'   summer_start = "06-15",
#'   fall_start = "09-15"
#' )
#'
#' @export
Season_cat <- function(x,
                       winter_start = "12-01",
                       spring_start = "04-01",
                       summer_start = "06-01",
                       fall_start   = "09-01",
                       labels = c("WIN", "SPR", "SUM", "FAL")) {

  if (!inherits(x, c("Date", "POSIXct", "POSIXt"))) {
    stop("`x` must be a Date or date-time vector.")
  }

  if (length(labels) != 4) {
    stop("`labels` must have length 4.")
  }

  starts <- c(winter_start, spring_start, summer_start, fall_start)

  valid_md <- function(z) grepl("^[0-1][0-9]-[0-3][0-9]$", z)
  if (any(!valid_md(starts))) {
    stop("Season starts must be supplied in 'MM-DD' format.")
  }

  md <- format(as.Date(x), "%m-%d")

  dplyr::case_when(
    is.na(md) ~ NA_character_,
    md >= winter_start | md < spring_start ~ labels[1],
    md >= spring_start & md < summer_start ~ labels[2],
    md >= summer_start & md < fall_start   ~ labels[3],
    md >= fall_start   & md < winter_start ~ labels[4]
  )
}
