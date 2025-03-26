#' A function to convert sby_code (brood year) to age (Jan 1st birthday convention)
#'
#' This function calculates the age of fish based on  the species-specific spawn period and the brood year.
#'
#'
#' @title sby2age
#' @name sby2age
#' @keywords SPDT; age; brood year
#' @export
#' @param Spp_code an character string or vector describing a standard species code.
#' @param sby_code an integer value or vecotr descibing the brood year
#' @param Year an integer value or vecotr descibing the year of sample (to be aged)
#'
#' @examples
#' Must be connected to VPN if working remotely
#'
#'sby_code = 2005
#'Year = 2010
#'sby2age("RB", sby_code, Year)
#'sby2age("KO", sby_code, Year)
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'

sby2age<- function(Spp_code = NULL, sby_code = NULL, Year = NULL){

  # Ensure Spp_code is a vector to vectorize
  Spp_code <- as.vector(Spp_code)

  # Find the corresponding diff_by values (the difference between fertilization and hatch year for fall versus spring spawners) for the given species code(s)
  diff_bys <- DP2R::Species$brood_year_diff[match(Spp_code, DP2R::Species$species_code)]

  # Check if any species code is not found in the lookup table
  if (any(is.na(diff_bys))) {
    warning("NA introduced from invalid species code or brood year in data.")
  }

  # Calculate Age using the formula: Age = Year - sby_code + diff_by_values
  Age = Year - sby_code - diff_bys

  # If Age is less than 0 or sby_code is 0, set Age to NA
  Age[Age < 0 | sby_code < 1901] <- NA

  # Return the calculated Age
  return(Age)
}
