#' Add gillnet selectivity to a data frame
#'
#' Adds a selectivity column to a data frame using \code{\link{GN_select}}.
#'
#' Rules:
#' \itemize{
#'   \item If \code{method_col} is not \code{"GN"}, selectivity is \code{NA}.
#'   \item If \code{method_col} is \code{"GN"} and \code{design_col} is missing/NA,
#'         \code{default_design} is used.
#' }
#'
#' @param df A data frame containing fish records.
#' @param species_col Name of the species column. Defaults to \code{"species_code"}.
#' @param length_col Name of the fork length column (mm). Defaults to \code{"length_mm"}.
#' @param design_col Name of the sample/net design column. Defaults to \code{"sample_design_code"}.
#' @param method_col Name of the capture method column. Defaults to \code{"method"}.
#' @param out_col Name of the output column to add. Defaults to \code{"select"}.
#' @param default_design Design code used when \code{design_col} is missing/NA
#'   for \code{method == "GN"}. Defaults to \code{"SGN7"}.
#'
#' @return \code{df} with an added column \code{out_col}.
#'
#' @examples
#' idf <- add_selectivity(idf, out_col = "NetX")
#'
#'
#' @export
add_selectivity <- function(df,
                            species_col = "species_code",
                            length_col  = "length_mm",
                            design_col  = "sample_design_code",
                            method_col  = "method",
                            out_col     = "select",
                            default_design = "SGN7") {

  # Required columns
  req <- c(species_col, length_col, method_col)
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) {
    stop("add_selectivity(): df is missing required column(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }

  n <- nrow(df)
  out <- rep(NA_real_, n)

  method <- as.character(df[[method_col]])
  is_gn <- !is.na(method) & method == "GN"

  if (any(is_gn)) {
    # design: use default only for GN rows
    if (design_col %in% names(df)) {
      design <- as.character(df[[design_col]])
    } else {
      design <- rep(NA_character_, n)
    }
    design[is_gn] <- dplyr::if_else(is.na(design[is_gn]) | design[is_gn] == "",
                                    default_design,
                                    design[is_gn])

    out[is_gn] <- GN_select(
      species = df[[species_col]][is_gn],
      length_mm = df[[length_col]][is_gn],
      sample_design_code = design[is_gn]
    )
  }

  df[[out_col]] <- out
  df
}
