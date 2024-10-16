#' A function analyze Effort counts and estimate total annual effort in angler days.
#' This function builds on other DP2R functions to gather data and fit an effort model to observed data. THe final step is to predict effort in all the unobserved strata withthe effort model.
#'
#'
#' @title fac.data
#' @name fac.data
#' @keywords DP2R; DataPond
#' @export
#' @param data a data.frame object
#' @param varlist a list of variable names and their reference level to factor in the format "name" = "ref level". If no reference level is required then "name" = NULL must be specficied.
#'
#' @examples
#'
#' DP2R::DP2R(Tables = c("vwEffort"))
#' vwEffort = DP2R::fac.data(vwEffort, vars = list("year" = NULL, "month" = "5", "hour" = "12", "daytype" = "WE", "lakeview_yr" = NULL, "lake_hr" = NULL))
#'
#' @importFrom magrittr "%>%"

fac.data <- function(data, varlist = list()) {
  data <- data %>%
    dplyr::mutate(dplyr::across(
      .cols = names(varlist),
      .fns = ~ {
        ref_level <- varlist[[cur_column()]]  # Extract reference level if provided
        fct <- factor(.)
        if (!is.null(ref_level)) fct <- stats::relevel(fct, ref_level)  # Set reference level if given
        fct
      }
    ))
  return(data)
}
