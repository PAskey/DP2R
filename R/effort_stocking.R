#' Add stocking from the preceding years to each angler effort estimate
#'
#' For every angler effort estimate (one lake, one year, one method), sums the fish
#' and biomass stocked in that lake over the preceding \code{window} years and
#' converts them to an annual rate (total / \code{window}), so annual,
#' alternate-year (\code{window/2} stockings) and other stocking programs are
#' directly comparable. Angler days produced per fish and per kg stocked are then
#' calculated for that single effort estimate. This replaces the 4-year loop that
#' was in the lake report's Create_datafiles.R.
#'
#' Stocking years used for an effort estimate in \code{year} are
#' \code{year - 1} to \code{year - window} (default), or \code{year} to
#' \code{year - window + 1} when \code{include_current = TRUE}.
#'
#' @title effort_stocking
#' @name effort_stocking
#' @keywords DP2R; effort; releases; stocking
#' @export
#' @param Effort Effort estimates with at least WBID, year and Angler_days (and
#'   area_ha for Biom_ha). Default \code{DP2R::Effort_all}.
#' @param Releases Release records with WBID, release_year, species_code,
#'   lifestage_code and quantity/biomass (either vwRelease names \code{quantity},
#'   \code{biomass_kg}, or link_releases() names \code{Quantity}, \code{Biomass_kg}).
#'   If NULL, uses \code{Releases} or \code{vwRelease} from the global environment,
#'   otherwise pulls vwRelease with DP2R() (needs a DataPond connection).
#' @param window Number of years of stocking to consider (default 4).
#' @param include_current Logical; include releases in the same year as the effort
#'   estimate (default FALSE).
#' @param exclude_lifestages Lifestage codes left out of the fish and biomass
#'   totals. Default "EE" (eyed eggs), whose numbers swamp per-fish metrics.
#' @return \code{Effort} with added columns:
#' \describe{
#'   \item{Stock_events}{Number of years in the window with any stocking}
#'   \item{Stock_spp}{Species stocked in the window, comma separated}
#'   \item{Stock_n}{Fish stocked per year (window total / window)}
#'   \item{Stock_biom}{kg stocked per year (window total / window)}
#'   \item{Biom_ha}{Stock_biom per hectare}
#'   \item{AD_per_fish}{Angler_days / Stock_n; NA when nothing was stocked}
#'   \item{AD_per_kg}{Angler_days / Stock_biom; NA when nothing was stocked}
#'   \item{ln_EpK}{log(AD_per_kg)}
#' }
#' @examples
#' #Must be connected to VPN if working remotely
#' DP2R(Tables = "vwRelease")
#' Eff <- effort_stocking(DP2R::Effort_all, vwRelease)
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data

effort_stocking <- function(Effort = DP2R::Effort_all, Releases = NULL, window = 4,
                            include_current = FALSE, exclude_lifestages = "EE") {

  rel <- std_releases(Releases, exclude_lifestages)

  lags <- if (include_current) 0:(window - 1) else 1:window

  Rel_yr <- rel %>%
    dplyr::group_by(.data$WBID, release_year = .data$release_year) %>%
    dplyr::summarise(rel_n = sum(.data$quantity, na.rm = TRUE),
                     rel_kg = sum(.data$biomass_kg, na.rm = TRUE),
                     rel_spp = paste(sort(unique(.data$species_code)), collapse = ","),
                     .groups = "drop")

  Lake_yr <- Effort %>%
    dplyr::distinct(WBID = as.character(.data$WBID), year = as.integer(.data$year)) %>%
    tidyr::crossing(lag = lags) %>%
    dplyr::mutate(release_year = .data$year - .data$lag) %>%
    dplyr::left_join(Rel_yr, by = c("WBID", "release_year")) %>%
    dplyr::group_by(.data$WBID, .data$year) %>%
    dplyr::summarise(
      Stock_events = sum(!is.na(.data$rel_n) & .data$rel_n > 0),
      Stock_spp = {
        s <- unique(unlist(strsplit(stats::na.omit(.data$rel_spp), ",")))
        if (length(s)) paste(sort(s), collapse = ",") else NA_character_
      },
      Stock_n = round(sum(.data$rel_n, na.rm = TRUE) / window, 0),
      Stock_biom = round(sum(.data$rel_kg, na.rm = TRUE) / window, 1),
      .groups = "drop")

  # Drop any older versions of these columns before joining
  new_cols <- c("Stock_events", "Stock_spp", "Stock_n", "Stock_biom", "Biom_ha",
                "AD_per_fish", "AD_per_kg", "ln_EpK")

  out <- Effort %>%
    dplyr::select(-dplyr::any_of(new_cols)) %>%
    dplyr::mutate(WBID = as.character(.data$WBID), year = as.integer(.data$year)) %>%
    dplyr::left_join(Lake_yr, by = c("WBID", "year"))

  nz <- function(x) dplyr::if_else(is.finite(x) & x > 0, x, NA_real_)
  area <- if ("area_ha" %in% names(out)) out$area_ha else NA_real_

  out %>%
    dplyr::mutate(
      Biom_ha = round(.data$Stock_biom / area, 1),
      AD_per_fish = round(.data$Angler_days / nz(.data$Stock_n), 3),
      AD_per_kg = round(.data$Angler_days / nz(.data$Stock_biom), 2),
      ln_EpK = round(log(.data$AD_per_kg), 2),
      ln_EpK = dplyr::if_else(is.finite(.data$ln_EpK), .data$ln_EpK, NA_real_))
}


#Internal helper: get releases from argument or environment and standardize names
std_releases <- function(Releases = NULL, exclude_lifestages = "EE") {
  if (is.null(Releases)) {
    if (exists("Releases", envir = .GlobalEnv)) {
      Releases <- get("Releases", envir = .GlobalEnv)
    } else {
      if (!exists("vwRelease", envir = .GlobalEnv)) DP2R(Tables = "vwRelease")
      Releases <- get("vwRelease", envir = .GlobalEnv)
    }
  }
  pick <- function(df, opts) {
    hit <- opts[opts %in% names(df)]
    if (!length(hit)) stop("Releases needs one of these columns: ", paste(opts, collapse = ", "))
    df[[hit[1]]]
  }
  data.frame(
    WBID = as.character(pick(Releases, "WBID")),
    release_year = as.integer(pick(Releases, "release_year")),
    species_code = as.character(pick(Releases, "species_code")),
    lifestage_code = as.character(pick(Releases, "lifestage_code")),
    quantity = as.numeric(pick(Releases, c("quantity", "Quantity"))),
    biomass_kg = as.numeric(pick(Releases, c("biomass_kg", "Biomass_kg"))),
    stringsAsFactors = FALSE) %>%
    dplyr::filter(!(.data$lifestage_code %in% exclude_lifestages))
}
