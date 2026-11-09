#' Requested stocking per angler day for each lake
#'
#' A forward-looking, lake-level measure of how many fish (and kg) are committed
#' for each angler day a lake produces. Current standing requests are converted
#' to an annual rate (quantity / interval, where Annual = 1, Odd/Even Year = 2,
#' 3 Year = 3) and divided by the lake's average angler days over the last
#' \code{window} years, averaged across every effort method available
#' (counts, questionnaires, surveys).
#'
#' Average angler days: mean of estimates within each lake-year-method, then the
#' mean across methods within a year, then the mean across years, so each year
#' counts once regardless of how many methods measured it.
#'
#' Requested kg: if the request table has a weight or biomass column it is used.
#' Otherwise kg = requested fish x one province-wide average weight per species x
#' life stage, from releases over the last \code{weight_years} release years (see
#' \code{\link{release_weights}}). Strain, ploidy, hatchery and lake are deliberately
#' ignored so requests are comparable across lakes. Request life stages are
#' standardized with \code{\link{std_lifestage}} (e.g. CA1/CA2 -> CA) to match releases.
#' Column \code{kg_basis} records which was used ("request" or "prov_release").
#'
#' Lakes with effort but no active request get 0 requested fish (wild fisheries),
#' so Fish_req_per_AD is 0 rather than undefined.
#'
#' @title stock_performance
#' @name stock_performance
#' @keywords DP2R; effort; stocking; requests
#' @export
#' @param Requests Standing requests (vwStandingRequestsForBio). If NULL, taken from
#'   the global environment or pulled with DP2R() (needs a DataPond connection).
#' @param Effort Effort estimates with WBID, year, method, Angler_days.
#'   Default \code{DP2R::Effort_all}.
#' @param Releases Release records, used only to estimate kg when requests have no
#'   weight. Same options as \code{\link{effort_stocking}}.
#' @param window Number of years of effort to average (default 4).
#' @param weight_years Number of most recent release years used for average weights (default 5).
#' @param last_year Most recent effort year to include. Default is last calendar
#'   year, so the effort years are (last_year - window + 1):last_year.
#' @return One row per lake with requests and/or effort in the window:
#' \describe{
#'   \item{Req_spp, Req_stages}{Species and lifestages with active requests}
#'   \item{Req_fish_yr}{Requested fish per year}
#'   \item{Req_kg_yr}{Requested kg per year}
#'   \item{kg_basis}{How Req_kg_yr was obtained: "request", "prov_release", or mixed}
#'   \item{mean_AD}{Average angler days per year over the window}
#'   \item{N_effort_yrs, Effort_methods}{Number of years and methods behind mean_AD}
#'   \item{Fish_req_per_AD}{Req_fish_yr / mean_AD}
#'   \item{kg_req_per_AD}{Req_kg_yr / mean_AD}
#' }
#' @examples
#' #Must be connected to VPN if working remotely
#' DP2R(Tables = c("vwStandingRequestsForBio", "vwRelease"))
#' SP <- stock_performance(vwStandingRequestsForBio, Releases = vwRelease)
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data

stock_performance <- function(Requests = NULL, Effort = DP2R::Effort_all, Releases = NULL,
                              window = 4, last_year = as.integer(format(Sys.Date(), "%Y")) - 1,
                              weight_years = 5) {

  if (is.null(Requests)) {
    if (!exists("vwStandingRequestsForBio", envir = .GlobalEnv)) DP2R(Tables = "vwStandingRequestsForBio")
    Requests <- get("vwStandingRequestsForBio", envir = .GlobalEnv)
  }
  years <- (last_year - window + 1):last_year

  #___________________________________________________________________________
  #Requests -> fish (and kg) per year
  RQ <- Requests
  if ("active" %in% names(RQ)) RQ <- RQ[RQ$active %in% TRUE, ]
  #Same life stage rules as link_releases() (CA1/CA2 -> CA; EB/KO non-yearling YE -> FR)
  RQ$lifestage_code <- std_lifestage(RQ$species_code, RQ$lifestage_code,
                                     if ("release_year" %in% names(RQ)) RQ$release_year else NULL,
                                     if ("brood_year" %in% names(RQ)) RQ$brood_year else NULL)

  freq <- tolower(trimws(as.character(RQ$frequency)))
  RQ$interval <- dplyr::case_when(
    freq == "annual" ~ 1,
    grepl("odd|even", freq) ~ 2,
    grepl("^[0-9]+ ?y", freq) ~ suppressWarnings(as.numeric(sub("^([0-9]+).*", "\\1", freq))),
    TRUE ~ NA_real_)
  if (any(is.na(RQ$interval))) {
    warning("Unrecognized request frequency (dropped): ",
            paste(unique(RQ$frequency[is.na(RQ$interval)]), collapse = ", "))
    RQ <- RQ[!is.na(RQ$interval), ]
  }

  #Request weight, if the view carries one
  wt_g_col <- intersect(c("size_g", "weight_g", "avg_weight_g", "mean_weight_g", "target_weight_g"), names(RQ))
  kg_col <- intersect(c("biomass_kg", "kg"), names(RQ))
  RQ$req_kg <- if (length(kg_col)) as.numeric(RQ[[kg_col[1]]]) else
    if (length(wt_g_col)) as.numeric(RQ$quantity) * as.numeric(RQ[[wt_g_col[1]]]) / 1000 else NA_real_

  #One row per requested product (averaging duplicate request rows), as in the original analysis
  prod_keys <- intersect(c("WBID", "species_code", "lifestage_code", "strain", "strain_code", "interval"), names(RQ))
  Prod <- RQ %>%
    dplyr::mutate(WBID = as.character(.data$WBID)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(prod_keys))) %>%
    dplyr::summarise(N = mean(as.numeric(.data$quantity), na.rm = TRUE),
                     kg = mean(.data$req_kg, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(kg = dplyr::if_else(is.finite(.data$kg), .data$kg, NA_real_),
                  kg_basis = dplyr::if_else(is.na(.data$kg), NA_character_, "request"))

  #Estimate missing kg from the province-wide average weight per species x life stage
  if (any(is.na(Prod$kg))) {
    w_prov <- release_weights(Releases, n_years = weight_years, end_year = last_year + 1) %>%
      dplyr::transmute(species_code = .data$species_code, lifestage_code = .data$lifestage_code,
                       kg_fish_prov = .data$avg_weight_g / 1000)
    Prod <- Prod %>%
      dplyr::left_join(w_prov, by = c("species_code", "lifestage_code")) %>%
      dplyr::mutate(
        kg_basis = dplyr::case_when(!is.na(.data$kg) ~ .data$kg_basis,
                                    !is.na(.data$kg_fish_prov) ~ "prov_release",
                                    TRUE ~ NA_character_),
        kg = dplyr::coalesce(.data$kg, .data$N * .data$kg_fish_prov)) %>%
      dplyr::select(-"kg_fish_prov")
    if (any(is.na(Prod$kg)))
      warning(sum(is.na(Prod$kg)), " requested products have no release weights to estimate kg (",
              paste(unique(paste(Prod$species_code[is.na(Prod$kg)], Prod$lifestage_code[is.na(Prod$kg)])), collapse = ", "),
              "); their kg are left out of Req_kg_yr")
  }

  Req <- Prod %>%
    dplyr::group_by(.data$WBID) %>%
    dplyr::summarise(Req_spp = paste(sort(unique(.data$species_code)), collapse = ","),
                     Req_stages = paste(sort(unique(.data$lifestage_code)), collapse = ","),
                     Req_fish_yr = round(sum(.data$N / .data$interval, na.rm = TRUE), 0),
                     Req_kg_yr = round(sum(.data$kg / .data$interval, na.rm = TRUE), 1),
                     kg_basis = paste(sort(unique(stats::na.omit(.data$kg_basis))), collapse = ","),
                     .groups = "drop")

  #___________________________________________________________________________
  #Average angler days over the window, across all methods
  AD <- Effort %>%
    dplyr::mutate(WBID = as.character(.data$WBID), year = as.integer(.data$year)) %>%
    dplyr::filter(.data$year %in% years, is.finite(.data$Angler_days)) %>%
    dplyr::group_by(.data$WBID, .data$year, .data$method) %>%
    dplyr::summarise(AD = mean(.data$Angler_days), .groups = "drop") %>%
    dplyr::group_by(.data$WBID, .data$year) %>%
    dplyr::summarise(AD = mean(.data$AD), methods = paste(sort(unique(.data$method)), collapse = ","),
                     .groups = "drop") %>%
    dplyr::group_by(.data$WBID) %>%
    dplyr::summarise(mean_AD = round(mean(.data$AD), 1),
                     N_effort_yrs = dplyr::n(),
                     Effort_methods = paste(sort(unique(unlist(strsplit(.data$methods, ",")))), collapse = ","),
                     .groups = "drop")

  #___________________________________________________________________________
  out <- dplyr::full_join(Req, AD, by = "WBID") %>%
    dplyr::mutate(
      Req_fish_yr = dplyr::coalesce(.data$Req_fish_yr, 0),
      Req_kg_yr = dplyr::coalesce(.data$Req_kg_yr, 0),
      Fish_req_per_AD = dplyr::if_else(.data$mean_AD > 0, round(.data$Req_fish_yr / .data$mean_AD, 3), NA_real_),
      kg_req_per_AD = dplyr::if_else(.data$mean_AD > 0, round(.data$Req_kg_yr / .data$mean_AD, 4), NA_real_))

  Lake_info <- DP2R::Lakes %>%
    dplyr::distinct(.data$WBID, .keep_all = TRUE) %>%
    dplyr::select("WBID", "region_code", "locale_name", "area_ha")

  out %>%
    dplyr::left_join(Lake_info, by = "WBID") %>%
    dplyr::relocate("region_code", "locale_name", "area_ha", .after = "WBID") %>%
    dplyr::mutate(effort_years = paste0(min(years), "-", max(years))) %>%
    dplyr::arrange(.data$region_code, .data$WBID) %>%
    as.data.frame()
}


#' Average weight at release by species and life stage
#'
#' One province-wide average weight per species x life stage from recent releases,
#' used by \code{stock_performance()} to convert requested fish to kg. Strain, ploidy,
#' hatchery and lake are deliberately ignored (e.g. every RB yearling gets the same
#' weight), so requests are comparable across lakes. Life stages are standardized
#' with \code{\link{std_lifestage}}. Average weight is total biomass / total fish
#' released (fish-weighted), so expected kg = quantity x avg_weight_g / 1000.
#'
#' @title release_weights
#' @name release_weights
#' @keywords DP2R; releases; weight; requests
#' @export
#' @param Releases Release records (vwRelease or link_releases() output). If NULL, uses
#'   \code{Releases} or \code{vwRelease} from the global environment.
#' @param n_years Number of most recent release years to average over (default 5).
#' @param end_year Last release year included (default current year).
#' @return Data frame: species_code, lifestage_code, n_releases, avg_weight_g, median_g.
#' @examples
#' #Must be connected to VPN if working remotely
#' link_releases()
#' release_weights(Releases)
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
release_weights <- function(Releases = NULL, n_years = 5,
                            end_year = as.integer(format(Sys.Date(), "%Y"))) {
  std_releases(Releases, exclude_lifestages = character(0)) %>%
    dplyr::filter(.data$release_year > end_year - n_years, .data$release_year <= end_year,
                  is.finite(.data$quantity), .data$quantity > 0,
                  is.finite(.data$biomass_kg), .data$biomass_kg > 0) %>%
    dplyr::mutate(lifestage_code = std_lifestage(.data$species_code, .data$lifestage_code,
                                                 .data$release_year, .data$brood_year)) %>%
    dplyr::group_by(.data$species_code, .data$lifestage_code) %>%
    dplyr::summarise(n_releases   = dplyr::n(),
                     avg_weight_g = round(1000 * sum(.data$biomass_kg) / sum(.data$quantity), 1),
                     median_g     = round(stats::median(.data$size_g, na.rm = TRUE), 1),
                     .groups = "drop") %>%
    as.data.frame()
}
