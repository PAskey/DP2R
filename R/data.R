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
#' Lake name reference table
#'
#' A reference table linking waterbody identifiers (WBIDs) to known lake names.
#' The dataset includes local names, gazetted names, aliases, and a derived
#' combined lake identifier used for joins and reporting.
#'
#' @format A data frame with one row per lake and the following columns:
#' \describe{
#'   \item{WBID}{Character. Unique waterbody identifier.}
#'   \item{locale_name}{Character. Local or commonly used lake name, if available.}
#'   \item{gazetted_name}{Character. Official gazetted lake name, if available.}
#'   \item{alias}{Character. Alternative or informal lake name.}
#'   \item{Lake_WBID}{Character. Concatenated lake name and WBID identifier.}
#' }
#'
#' @usage data(lake_names)
#'
#' @source Derived from internal lake naming and waterbody reference sources.
#'
#' @examples
#' data(lake_names)
#' head(lake_names)
"lake_names"
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
#' Best-fitting gillnet selectivity models by species
#'
#' A data frame of fitted Millar selectivity models used by
#' \code{\link{predict_Millar}} and to build \code{\link{sel_lookup}}.
#'
#' Each row corresponds to one modeled species and stores the selected model
#' type and fitted parameters required to predict relative gillnet selectivity
#' across a set of mesh sizes.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{species_code}{Character. Species code (e.g., \code{"KO"}, \code{"RB"}).}
#'   \item{rtype}{Character. Millar selectivity model type.}
#'   \item{theta}{List-column. Fitted parameter vector for the selected model.}
#'   \item{meshSizes}{List-column. Mesh sizes (mm) used when fitting the model.}
#' }
#'
#' @usage data(best_models)
#' @source Generated internally for the DP2R package from DataPond biological data.
#'
#' @examples
#' data(best_models)
#' head(best_models)
#'
#' @docType data
#' @name best_models
NULL

# ----------------------------------------------------------------------
#' Precomputed gillnet selectivity lookup table
#'
#' A dataset containing precomputed relative gillnet selectivity curves for
#' combinations of fish species and gillnet sample designs.
#'
#' Each row corresponds to a unique combination of \code{species_code} and
#' \code{sample_design_code}. The \code{curve} column is a numeric vector of
#' relative selectivity values aligned to \code{\link{sel_classes}} (1 mm
#' increments). Curves are predicted using \code{\link{predict_Millar}} from
#' \code{\link{best_models}} and the mesh configuration for each sample design.
#'
#' This lookup table supports fast assignment of selectivity values to large
#' biological datasets without repeatedly re-evaluating Millar models.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{species_code}{Character. Modeled species code (e.g., \code{"RB"}, \code{"EB"}).}
#'   \item{sample_design_code}{Character. Gillnet sample design code defining
#'     the mesh configuration of the net (e.g., \code{"SGN7"}, \code{"FWIN"}).}
#'   \item{curve}{List-column of numeric vectors. Each vector has length
#'     \code{length(sel_classes)} and contains relative selectivity values
#'     aligned to \code{sel_classes}.}
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
#' selectivity model species actually used when applying gillnet selectivity.
#'
#' This table is used when a species does not have a fitted selectivity curve
#' in \code{\link{sel_lookup}}. In such cases, selectivity is approximated using
#' a proxy species (\code{select_spp}) that *does* have a curve.
#'
#' The table is generated internally and stored in the package \code{data/}
#' directory. It is not intended to be edited manually.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{species_code}{Character. Species code observed in biological samples.}
#'   \item{select_spp}{Character. Species code of the selectivity model to use
#'     when applying gillnet selectivity.}
#'   \item{match_level}{Character. Description of how the proxy species was chosen
#'     (e.g., taxonomic match level).}
#' }
#'
#' @details
#' \code{approx_select_spp} is used by \code{\link{GN_select}} to map species to
#' an available selectivity curve in \code{\link{sel_lookup}}.
#'
#' @seealso
#' \code{\link{sel_lookup}},
#' \code{\link{GN_select}}
#'
#' @docType data
#' @name approx_select_spp
#' @keywords internal
#' @usage data(approx_select_spp)
NULL


# ----------------------------------------------------------------------
#' Fork-length grid for gillnet selectivity curves
#'
#' A numeric vector defining the fork-length (mm) grid used for all selectivity
#' curves stored in \code{\link{sel_lookup}}.
#'
#' All curves in \code{sel_lookup$curve} are aligned to this shared grid.
#' This enables fast lookup of selectivity values without interpolation.
#'
#' @format A numeric vector of fork lengths (mm).
#'
#' @usage data(sel_classes)
#' @source Defined internally for the DP2R package when building \code{sel_lookup}.
#'
#' @examples
#' data(sel_classes)
#' head(sel_classes)
#'
#' @docType data
#' @name sel_classes
NULL
