#' A simple function to take a list of variables within a data set and convert to factors and optionally specified reference levels. This is useful when setting up data for anlaysis of categorical variables in glm or glmers for example.
#'
#'
#' @title fac.data
#' @name fac.data
#' @keywords DP2R; DataPond
#' @export
#' @param data a data.frame object
#' @param varlist a list of variable names and their reference level to factor in the format "name" = "ref level". If no reference level is required then "name" = NULL.
#'
#' @examples
#'
#' DP2R::DP2R(Tables = c("vwEffort"))
#' vwEffort = DP2R::fac.data(vwEffort, vars = list("year" = NULL, "month" = "5", "hour" = "12", "daytype" = "WE", "lakeview_yr" = NULL, "lake_hr" = NULL))
#'
#' @importFrom magrittr "%>%"

fac.data <- function(data, varlist = list()) {

  # Ensure all specified columns exist in the data
  missing_vars <- setdiff(names(varlist), names(data))
  if (length(missing_vars) > 0) {
    stop(paste("The following variables are not found in the data:",
               paste(missing_vars, collapse = ", ")))
  }

  # Apply factorization and optional releveling
  data <- data %>%
    dplyr::mutate(dplyr::across(
      .cols = names(varlist),
      .fns = ~ {
        ref_level <- varlist[[dplyr::cur_column()]]  # Extract reference level if provided
        fct <- factor(.)
        if (!is.null(ref_level)) fct <- stats::relevel(fct, ref_level)  # Set reference level if given
        fct <- droplevels(fct)
      }
    ))

  return(data)
}
