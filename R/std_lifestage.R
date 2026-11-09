#' Standardize release / request life stage codes
#'
#' One set of life stage rules shared by \code{link_releases()} (release records) and
#' \code{stock_performance()} (standing requests), so the two tables always match:
#' \itemize{
#'   \item Catchables \code{CA1} and \code{CA2} become \code{CA}. The 2 codes only reflect
#'     that KTH (cold water) takes an extra year to grow fish to catchable size; age at
#'     release and hatchery are kept elsewhere.
#'   \item Fall spawners (EB, KO) spawn in the fall of brood_year but eggs do not hatch
#'     until the following year, so a genuine yearling is release_year - brood_year == 2.
#'     EB/KO coded \code{YE} that are not genuine yearlings (including when either year
#'     is missing) become \code{FR}.
#' }
#'
#' @title std_lifestage
#' @name std_lifestage
#' @keywords DP2R; releases; requests; lifestage
#' @export
#' @param species_code Species codes.
#' @param lifestage_code Life stage codes (same length).
#' @param release_year,brood_year Release and brood years (same length, or NULL / NA
#'   if not available).
#' @return Character vector of standardized life stage codes.
#' @examples
#' std_lifestage(c("RB","RB","KO"), c("CA1","CA2","YE"), c(2026,2026,2026), c(2025,2024,2025))
std_lifestage <- function(species_code, lifestage_code, release_year = NULL, brood_year = NULL) {
  ls <- as.character(lifestage_code)
  n <- length(ls)
  if (is.null(release_year)) release_year <- rep(NA_real_, n)
  if (is.null(brood_year)) brood_year <- rep(NA_real_, n)
  true_yearling <- dplyr::coalesce(as.numeric(release_year) - as.numeric(brood_year) == 2, FALSE)
  ls <- dplyr::if_else(ls %in% c("CA1", "CA2"), "CA", ls)
  dplyr::if_else(species_code %in% c("EB", "KO") & ls %in% "YE" & !true_yearling, "FR", ls)
}
