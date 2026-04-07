#' Find elevations from coordinates
#'
#' Looks up elevation for rows in a data frame using \pkg{elevatr} and writes
#' the results into an `elevation_m` column. Rows with missing or invalid
#' coordinates are left as `NA`.
#'
#' @param x A data frame containing longitude and latitude columns.
#' @param lon_col Name of the longitude column. Default is `"longitude"`.
#' @param lat_col Name of the latitude column. Default is `"latitude"`.
#' @param elevation_col Name of the output elevation column.
#'   Default is `"elevation_m"`.
#' @param prj Coordinate reference system passed to
#'   [elevatr::get_elev_point()]. Default is `"EPSG:4326"`.
#' @param src Elevation source passed to [elevatr::get_elev_point()].
#'   Default is `"aws"`.
#' @param batch_size Number of points to request per batch. Default is `500`.
#' @param overwrite Logical; if `FALSE`, existing non-missing values in
#'   `elevation_col` are preserved. Default is `FALSE`.
#' @param quiet Logical; if `FALSE`, prints progress. Default is `FALSE`.
#'
#' @return The input data frame with `elevation_col` filled for eligible rows.
#'
#' @details
#' `elevatr::get_elev_point()` does not allow missing coordinates, so this
#' function filters to valid rows first, then writes results back into the
#' original data frame. Invalid or missing coordinates are left unchanged.
#'
#' Coordinates are lightly validated before lookup:
#' \itemize{
#'   \item latitude must be between `-90` and `90`
#'   \item longitude must be between `-180` and `180`
#' }
#'
#' @examples
#' \dontrun{
#' Lakes <- find_elevation(Lakes)
#'
#' lakes_missing <- is.na(Lakes$elevation_m)
#' Lakes[lakes_missing, ] <- find_elevation(Lakes[lakes_missing, ])
#' }
#'
#' @export
find_elevation <- function(x,
                           lon_col = "longitude",
                           lat_col = "latitude",
                           elevation_col = "elevation_m",
                           prj = "EPSG:4326",
                           src = "aws",
                           batch_size = 500,
                           overwrite = FALSE,
                           quiet = FALSE) {
  if (!requireNamespace("elevatr", quietly = TRUE)) {
    stop("Package 'elevatr' is required.", call. = FALSE)
  }
  
  if (!is.data.frame(x)) {
    stop("`x` must be a data frame.", call. = FALSE)
  }
  
  if (!lon_col %in% names(x)) {
    stop("`lon_col` not found in `x`.", call. = FALSE)
  }
  
  if (!lat_col %in% names(x)) {
    stop("`lat_col` not found in `x`.", call. = FALSE)
  }
  
  if (!is.numeric(batch_size) || length(batch_size) != 1L ||
      is.na(batch_size) || batch_size < 1) {
    stop("`batch_size` must be a positive integer.", call. = FALSE)
  }
  batch_size <- as.integer(batch_size)
  
  if (!elevation_col %in% names(x)) {
    x[[elevation_col]] <- NA_real_
  }
  
  lon <- x[[lon_col]]
  lat <- x[[lat_col]]
  
  valid_idx <- which(
    !is.na(lon) &
      !is.na(lat) &
      is.finite(lon) &
      is.finite(lat) &
      lon >= -180 & lon <= 180 &
      lat >= -90 & lat <= 90
  )
  
  if (!overwrite) {
    valid_idx <- valid_idx[is.na(x[[elevation_col]][valid_idx])]
  }
  
  if (length(valid_idx) == 0L) {
    if (!quiet) {
      message("No eligible rows to process.")
    }
    return(x)
  }
  
  coords <- data.frame(
    x = lon[valid_idx],
    y = lat[valid_idx]
  )
  
  n_batches <- ceiling(nrow(coords) / batch_size)
  
  for (b in seq_len(n_batches)) {
    start <- (b - 1L) * batch_size + 1L
    end <- min(b * batch_size, nrow(coords))
    batch_rows <- start:end
    
    elev <- elevatr::get_elev_point(
      locations = coords[batch_rows, , drop = FALSE],
      prj = prj,
      src = src
    )$elevation
    
    x[[elevation_col]][valid_idx[batch_rows]] <- elev
    
    if (!quiet) {
      message(sprintf(
        "Processed batch %d/%d (%d rows)",
        b, n_batches, length(batch_rows)
      ))
    }
  }
  
  x
}