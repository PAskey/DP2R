#' A function to partition release sizes into discrete categories
#' 
#' This function takes a number (release size) and assigns it to another number category. 
#' This is useful since a difference of 1g between stocking groups is a big deal for a 1g fry release size, but a rounding error for large yearlings.
#' A default set of categories has been created. The function may be updated to offer options in the future.
#' A simple manual alternative is round(a/b)*a where a is value and b is the rounding increment.
#' 
#'   SAR_cat = dplyr::case_when(SAR<=5.5 ~ cround(SAR/1)*1,
#'                              SAR>5.5&SAR<=22.5 ~ cround(SAR/5)*5,
#'                              SAR>22.5&SAR<75 ~ cround(SAR/10)*10,
#'                              TRUE ~ cround(SAR/20)*20) 
#' 
#' Additional note is that R rounds 0.5 value up or down every second digit (e.g. 1.5 = 2, 2.5 = 2, 3.5 = 4, etc.).
#' Apparently, this is some sort of standard but is problematic when categorizing release sizes (1.5 and 2.5 are both put into 2.)
#' Therefore, a custom function to correct this to always round 0.5 up is used (cround()).
#' 
#' @title SAR_cat
#' @name SAR_cat
#' @keywords SPDT; gillnet; stocking, release size
#' @export
#' @param SAR a number or vector of fish release sizes (in g or other sizes)
#' @examples
#' Must be connected to VPN if working remotely
#' 
#'
#'#Create a vector of size-at-release values - SARs
#'SARs = c(1:100)
#'
#'#Re-categorize these into broader size bins depending on sizes. 
#'Default bins based on common release sizes and release size targets.
#'SAR_cat = SAR_cat(SARs)
#'SAR_cat
#'
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' 

SAR_cat = function(SAR){

  #new approach based on commerce rounding code from : https://andrewlandgraf.com/2012/06/15/rounding-in-r/ and here https://stackoverflow.com/questions/12688717/round-up-from-5
  
  cround = function(x, n=0) {
    posneg = sign(x)
    z = trunc(abs(x) * 10^n + 0.5 + sqrt(.Machine$double.eps)) / 10^n
    z * posneg
  }
 
#Make categorization bins relative to the size of the fish 
  bin = dplyr::case_when(
    SAR <= 5.5 ~ 1,
    SAR > 5.5 & SAR <= 22.5 ~ 5,
    SAR > 22.5 & SAR < 75 ~ 10,
    TRUE ~ 20
  ) 
  
  #ROund to nearest bin
  SAR_cat = cround(SAR/bin)*bin
  
  return(SAR_cat)
}