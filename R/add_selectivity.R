#' Add gillnet selectivity to a data frame
#'
#' Adds a selectivity column to a data frame using \code{\link{GN_select}}.
#'
#' Rules:
#' \itemize{
#'   \item If \code{method_col} is not \code{"GN"}, selectivity is \code{NA}.
#'   \item If \code{Sample_event} is present and non-missing, event-level
#'         selectivity is used.
#'   \item Otherwise, \code{sample_design_code} is used.
#'   \item If GN and design is missing/NA, \code{default_design} is used.
#' }
#'
#' @param df A data frame containing fish records.
#' @param species_col Name of the species column.
#' @param length_col Name of the fork length column (mm).
#' @param design_col Name of the sample/net design column.
#' @param sample_event_col Name of Sample_event column (optional).
#' @param method_col Name of the capture method column.
#' @param out_col Name of the output column to add.
#' @param default_design Design code used when design is missing.
#'
#' @export
add_selectivity <- function(df,
                            species_col = "species_code",
                            length_col  = "length_mm",
                            design_col  = "sample_design_code",
                            sample_event_col = "Sample_event",
                            method_col  = "method",
                            out_col     = "select",
                            default_design = "SGN7") {

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

  if (!any(is_gn)) {
    df[[out_col]] <- out
    return(df)
  }

  species_vec <- df[[species_col]][is_gn]
  length_vec  <- df[[length_col]][is_gn]

  # ---- Determine whether to use Sample_event ----
  use_event <- FALSE
  if (sample_event_col %in% names(df)) {
    event_vec_full <- df[[sample_event_col]]
    use_event <- any(!is.na(event_vec_full[is_gn]))
  }

  if (use_event) {

    event_vec <- as.character(df[[sample_event_col]][is_gn])

    out[is_gn] <- GN_select(
      species = species_vec,
      length_mm = length_vec,
      Sample_event = event_vec
    )

  } else {

    # Fallback to sample_design_code
    if (design_col %in% names(df)) {
      design <- as.character(df[[design_col]])
    } else {
      design <- rep(NA_character_, n)
    }

    design[is_gn] <- dplyr::if_else(
      is.na(design[is_gn]) | design[is_gn] == "",
      default_design,
      design[is_gn]
    )

    out[is_gn] <- GN_select(
      species = species_vec,
      length_mm = length_vec,
      sample_design_code = design[is_gn]
    )
  }

  df[[out_col]] <- out
  df
}
