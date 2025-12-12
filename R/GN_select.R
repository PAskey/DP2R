#' A function to correct for gillnet selectivity
#'
#' This function estimates the relative size-dependent vulnerability of trout to the standard RIC gillnets.
#' The function is used to multiply observed catches by a multiplier proportional to the fish fork length.
#' In the future the selectivity function can be made more robust to account for non-standard combinations of gillnet panels.
#' Using a simple multiplier to correct for selectivity only works with good sample sizes, because 0 * anything = 0.
#'
#'
#' @title GN_select
#' @name GN_select
#' @keywords SPDT; gillnet; selectivity
# @export
#' @param FLengths an integer or vector of fork lengths in mm for which to calculate relative probability of capture by RIC gillnet.
#' @param Millar_model a TRUE/FALSE to indicate whether Millar model should be used for predictions
#' @param meshSizes_in a vecotr of mesh sizes in inches. Only applicable to Millar model and defaults to full RIC net
#'
#' @examples
#' Must be connected to VPN if working remotely
#'
#'
#'#Create a vector of fish lengths from 50 to 650mm
#'Fish_lengths = c(50:650)
#'
#'#Estimate the relative probability of capture for each of the fish lengths
#'pvals = RICselect(Fish_lengths)
#'
#'#Plot the selectivity function
#'plot(pvals~Fish_lengths)
#'
#'#However, you can access this selectivity function directly by simply typing
#'select_lookup
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'

GN_select<- function(FLengths_mm, Millar_model = FALSE, meshSizes_in = NULL){

 if(Millar_model == TRUE){

  #RIC_meshes <- c(1,1.25,1.5,2,2.5,3,3.5)
  if(is.null(meshSizes_in)) meshSizes_in <- SPDT::RIC_param$RIC_meshes
  meshSizes = meshSizes_in#25.4*meshSizes_in

  if(sum(sort(meshSizes)==meshSizes) != length(meshSizes))
    stop("Mesh size must be in ascending order!")



  #Use model 5 fit from Gillnet_Selectivity.RMD
  #theta = SPDT::RIC_param$theta
  #rel.power = SPDT::RIC_param$rel.power[match(meshSizes_in, SPDT::RIC_param$RIC_meshes)]
  #rel.power[is.na(rel.power)]<-1#For mesh sizes without data

  p = predict_Millar(rtype = "bilognorm", classes = FLengths_mm, meshSizes = meshSizes, theta = theta, rel.power = rel.power)


   }else{
  #Named vector approach causing issues
  #p = RIC_param$p_gam%>%dplyr::filter(Length_mm %in% FLengths_mm)%>%dplyr::pull(p)
  #p <- setNames(p, FLengths_mm)
   #  lens = c(100,100, 300, 200, 4000, 100, 200)
  #df = SPDT::RIC_param$p_gam
  #df$p[match(lens, df$Length_mm)]
  p = df$p[match(FLengths_mm, df$Length_mm)]
  #p = df$p[df$Length_mm %in% FLengths_mm]
  return(p)

  }


  return(p)
}
