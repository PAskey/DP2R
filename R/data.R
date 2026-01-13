#' DayTypes Dataset
#'
#' A dataset indicating different types of days used in the effort analysis.
#'
#' This dataset categorizes days as "Weekday", "Weekend", or "Holiday", which are
#' used for modeling effort variations in the DP2R package.
#'
#' @format A data frame with 3 columns:
#' \describe{
#'   \item{Date}{Date in "YYYY-MM-DD" format.}
#'   \item{DayType}{A factor with levels: "Weekday", "Weekend", "Holiday".}
#'   \item{Description}{A short description of the day type.}
#' }
#' @usage data(DayTypes)
#' @source Generated internally for the DP2R package.
#'
#' @examples
#' data(DayTypes)
#' head(DayTypes)
#'
#' @docType data
#' @name DayTypes
NULL

# ----------------------------------------------------------------------
#' Pred_vars Dataset
#'
#' A data frame summarizing frequency of variable categories for efficient model predictions used in the effort analysis.
#'
#' This dataset categorizes daytypes, weather, hour of unobserved counts to extrapolate effort estimates to a full season.Used within EffortEst function.
#'
#' @format A data frame with 6 columns:
#' \describe{
#'   \item{year}{year in numeric format.}
#'   \item{month}{month in numeric format.}
#'   \item{daytype}{A factor with levels: "WD", "WE", "LWE".}
#'   \item{ndays}{An integer describing the number of days for that daytype category in the given month}
#'   \item{hour}{hour in character format}
#'   \item{weather_code}{A character string describing the weather catgory as POOR, FAIR or GOOD.}
#' }
#' @usage data(Pred_vars)
#' @source Generated internally for the DP2R package.
#'
#' @examples
#' data(Pred_vars)
#' head(Pred_vars)
#'
#' @docType data
#' @name Pred_vars
NULL

# ----------------------------------------------------------------------
#' DP2R effort model (internal file)
#'
#' A generalized linear mixed-effects model (GLMM) used to estimate fishing
#' effort based on day type and other covariates.
#'
#' This model is stored as a compressed \code{.qs2} file in the package
#' \code{data/} directory and is not loaded automatically. It is only required
#' when updating or refitting effort estimates.
#'
#' @details
#' To load the model manually:
#' \preformatted{
#'   fit <- qs2::qs_read(system.file("data", "DP2R_Effort_Model.qs2", package = "DP2R"))
#' }
#'
#' @seealso \code{\link{EffortEst}}
#'
#' @name DP2R_Effort_Model
#' @keywords internal
NULL


# ----------------------------------------------------------------------
#' DP2R ice fishing effort model (internal file)
#'
#' A generalized linear mixed-effects model (GLMM) used to estimate ice fishing
#' effort.
#'
#' This model is stored as a compressed \code{.qs2} file in the package
#' \code{data/} directory and is not loaded automatically.
#'
#' @details
#' To load the model manually:
#' \preformatted{
#'   fit <- qs2::qs_read(system.file("data", "DP2R_Ice_Model.qs2", package = "DP2R"))
#' }
#'
#' @seealso \code{\link{EffortEst}}
#'
#' @name DP2R_Ice_Model
#' @keywords internal
NULL


# ----------------------------------------------------------------------
#' shinydata Dataset
#'
#' A data frame summarizing effort estimates for lakes for each year/fishing season assessed, which underlies the Shiny application used by provincial biologists.
#' Open water fishing season is standardized from May 1st to October 31st.
#'
#' @format A data frame with 18 columns:
#' \describe{
#'   \item{region}{managment region, character}
#'   \item{WBID}{Waterbody Identifie, character}
#'   \item{gazetted_name}{Gazetted waterbody name}
#'   \item{view_location_name}{A unique name given to a specific viewpoint of a lake by camera or ground}
#'   \item{year}{hour in character format}
#'   \item{method}{A character string describing the method used to count effort CAM, AIR, GRD}
#'   \item{N}{The number of observations for the specified year/fishing season a lake has been assessed, integer}
#'   \item{spv_obs}{The total number of single person vessels observed angling in year, integer}
#'   \item{boats_obs}{The total number of angling boats observed in year, integer}
#'   \item{shore_obs}{The total number of shore anglers observed in year, integer}
#'   \item{spv_AD}{The estimated total angler days from single person vessels in fishing season, integer}
#'   \item{boats_AD}{The estimated total angler days from angling boats in fishing season, integer}
#'   \item{shore_AD}{The estimated total angler days from shore anglers in fishing season, integer}
#'   \item{Angler_days}{The estimated total angler days in fishing season. WIll be NA for camera counts without verified Cam_Exp, integer}
#'   \item{Angler_days_p_ha}{The estimated total angler days per hectare of lake area in fishing season, numeric}
#'   \item{area_ha}{The lake surface area in hectares, numeric}
#'   \item{Cam_Exp}{The expansion factor used to expand the estimated effort seen from the view_locaiton_name to the entire lake, numeric}
#'   \item{Exp_N}{The numerb of verification counts used to estimate the Cam_Exp, numeric}
#' }
#' @usage data(shinydata)
#' @source Generated internally for the DP2R package.
#'
#' @examples
#' data(shinydata)
#' head(shinydata)
#'
#' @docType data
#' @name shinydata
NULL

# ----------------------------------------------------------------------
#' lakesum Dataset
#'
#' A data frame summarizing effort estimates for lakes over all years assessed, which underlies the Shiny application used by provincial biologists.
#'
#' @format A data frame with 10 columns:
#' \describe{
#'   \item{region}{managment region, character}
#'   \item{WBID}{Waterbody Identifie, character}
#'   \item{gazetted_name}{Gazetted waterbody name}
#'   \item{N_years}{the number of different calendar years with effort estimates, integer}
#'   \item{min_year}{the earliest year with effort estimates, integer}
#'   \item{max_year}{the nmost recent year effort estimates, integer}
#'   \item{Methods}{A character string describing the different methods used to count effort CAM, AIR, GRD}
#'   \item{mean_AD}{The average estimated total angler days across years assessed, numeric}
#'   \item{marker_size}{A value equal to mean_AD for plotting}
#'   \item{AD_percentile}{The percentile rank, province wide, of the average anglers days estiamted for a given lake.}
#'   \item{lake_latitude}{coordinate of lake, numeric}
#'   \item{lake_longitude}{coordinate of lake, numeric}
#' }
#' @usage data(lakesum)
#' @source Generated internally for the DP2R package.
#'
#' @examples
#' data(lakesum)
#' head(lakesum)
#'
#' @docType data
#' @name lakesum
NULL

# ----------------------------------------------------------------------
#' Species Dataset
#'
#' A data frame summarizing basic taxonomy and characteristics of fish species found in lakes of BC.
#'
#' @format A data frame with 23 columns:
#' @usage data(Species)
#' @source DataPond.
#'
#' @examples
#' data(Species)
#' head(Species)
#'
#' @docType data
#' @name Species
NULL

# ----------------------------------------------------------------------
#' Lake names Dataset
#'
#' A data frame summarizing lake names by using gazetted name where available and alias if not.Stirings ofr " Lake" or "Creek" are dropped.
#' Also has concatenated lake_name_WBID for quick reference.
#'
#' @format A data frame with 5 columns:
#' @usage data(lake_names)
#' @source DataPond.
#'
#' @examples
#' data(lake_names)
#' head(lake_names)
#'
#' @docType data
#' @name lake_names
NULL

# ----------------------------------------------------------------------
#' Mgt_Region_Unit Dataset
#'
#' A data frame regions and wildlife management units associated with lakes.
#' Data obtained from bcdata R package and code in raw-data folder.
#' Initial function of this data was to be able to match unique lakes by name and wmu to DFO survey data.
#'
#' @format A data frame with 10 columns:
#' \describe{
#'   \item{WATERBODY_POLY_ID}{unique identifier for the polygon describiing the lake, integer}
#'   \item{GNIS_NAME_1}{Gazetted waterbody name, character}
#'   \item{GNIS_NAME_2}{Alias, character}
#'   \item{GNIS_NAME_3}{Alias, character}
#'   \item{WBID}{Waterbody Identifie, character}
#'   \item{Management_Unit}{wildlife management unit, character}
#'   \item{region}{managment region code, character}
#'   \item{RegionName}{managment region full name, character}
#'   \item{OBJECTID}{unique identifier provinical database, integer}
#'   \item{neighbour_mus}{comma separated list of wildlife managment units that are adjacent to wmu in column 5, character}
#' }
#' @usage data(Mgt_Region_Unit)
#' @source Generated from bcdata R package for the DP2R package and code in data-raw folder.
#'
#' @examples
#' data(Mgt_Region_Unit)
#' head(Mgt_Region_Unit)
#'
#' @docType data
#' @name Mgt_Region_Unit
NULL

# ----------------------------------------------------------------------
#' Best-fitting selectivity models for each species
#'
#' A data frame where each row corresponds to a species and contains:
#' - the selected Millar model type (`rtype`)
#' - the fitted parameter vector (`theta`)
#' - the mesh sizes used for fitting (`meshSizes`)
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{species_code}{Species code (e.g., "KO", "RB")}
#'   \item{rtype}{Millar selectivity model type}
#'   \item{theta}{List column of parameter vectors}
#'   \item{meshSizes}{List column of mesh sizes (mm)}
#' }
#'
#' @docType data
#' @name best_models
NULL

# ----------------------------------------------------------------------
#' Precomputed gillnet selectivity lookup table
#'
#' A dataset containing precomputed relative gillnet selectivity curves
#' for combinations of fish species and gillnet sample designs.
#'
#' Each row corresponds to a unique combination of
#' \code{species_code} and \code{sample_design_code}.
#' The \code{curve} column is a numeric vector giving the relative
#' selectivity (scaled to a maximum of 1) for fish lengths from
#' 75 to 650 mm (inclusive), as predicted by \code{\link{predict_Millar}}.
#'
#' This lookup table is intended to support fast assignment of selectivity
#' values to individual fish records without repeatedly re-evaluating Millar
#' selectivity models.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{species_code}{Character. Species code (e.g., \code{"RB"}, \code{"EB"}).}
#'   \item{sample_design_code}{Character. Gillnet sample design code defining
#'     the mesh configuration of the net (e.g., \code{"SGN7"}, \code{"FWIN"}).}
#'   \item{curve}{List-column of numeric vectors. Each vector contains relative
#'     selectivity values for fish lengths 75--650 mm, in 1 mm increments.}
#' }
#'
#' @usage data(sel_lookup)
#' @source Generated internally for the DP2R package using \code{\link{predict_Millar}}.
#'
#' @examples
#' data(sel_lookup)
#' sel_lookup[1, ]
#'
#' @docType data
#' @name sel_lookup
NULL

# ----------------------------------------------------------------------
#' Approximate species mapping for gillnet selectivity
#'
#' A lookup table mapping observed fish \code{species_code} values to the
#' species selectivity model actually used when applying gillnet selectivity.
#'
#' This table is used when a species does not have a fitted selectivity model
#' in \code{\link{sel_lookup}}. In such cases, selectivity is approximated using
#' a proxy species selected from available models based on taxonomic similarity
#' and data availability.
#'
#' The proxy species (\code{select_spp}) is chosen using the following hierarchy:
#' \enumerate{
#'   \item Exact match on \code{species_code} (if available in \code{sel_lookup})
#'   \item Match on taxonomic \code{species} (from the \code{Species} table)
#'   \item Genus
#'   \item Subfamily
#'   \item Family
#'   \item Order
#' }
#'
#' When multiple candidate model species are available at the same taxonomic
#' level, the proxy species is selected as the one occurring most frequently
#' in the \code{Biological} dataset used to build the lookup table.
#'
#' This table is generated internally (typically once per year) and stored
#' in the package \code{data/} directory. It is not intended to be edited
#' manually.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{common_name}{Character. Species name observed in biological samples.}
#'   \item{species_code}{Character. Species code observed in biological samples.}
#'   \item{select_spp}{Character. Species code of the selectivity model to use
#'     when applying gillnet selectivity.}
#'   \item{match_level}{Character. Taxonomic level at which the proxy species
#'     was matched (e.g., \code{"species_code"}, \code{"species"},
#'     \code{"genus"}, \code{"family"}, \code{"order"}).}
#'   \item{min_FL}{number. Minimum fork length observed for species in gillnet across all DataPond biological samples.}
#'   \item{max_FL}{number. Maximum fork length observed for species in gillnet across all DataPond biological samples.}
#' }
#'
#' @details
#' The \code{approx_select_spp} table is used by
#' \code{\link{add_selectivity}} to determine which selectivity curve to apply
#' for each fish record. If no proxy species can be identified, selectivity
#' defaults to 1 (i.e., no size-based selectivity adjustment).
#'
#' @seealso
#' \code{\link{sel_lookup}},
#' \code{\link{add_selectivity}},
#' \code{\link{Species}}
#'
#' @docType data
#' @name approx_select_spp
#' @keywords internal
#' @usage data(approx_select_spp)
NULL
