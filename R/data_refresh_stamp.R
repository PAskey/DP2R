# ----------------------------------------------------------------------
#' refresh_stamp Dataset
#'
#' Small DataPond signatures recorded by \code{DP2Rdata_refresh()} when the stored
#' data were last brought up to date: vwEffort row count, highest fishing_effort_id
#' and latest assessed_dt, and vwWaterbody row count and checksum. \code{DP2R()}
#' compares the current values with these once per session and prints a reminder
#' if DataPond has changed.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{check}{Name of the signature, e.g. "effort.max_id", "waterbody.chk"}
#'   \item{value}{Value when stamped, character}
#'   \item{stamped}{Time it was recorded, POSIXct}
#' }
#' @usage data(refresh_stamp)
#' @source Written by \code{DP2Rdata_refresh()}.
#' @docType data
#' @name refresh_stamp
NULL
