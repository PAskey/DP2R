#' A function analyze Effort counts and estimate total annual effort in angler days.
#' It is important that you establish a personal password protected connection (and assign as 'conn' object before running this function. Use example code below and/or you may have it set up under Connections tab in RStudio after running it once.See: https://github.com/r-dbi/odbc/blob/main/README.md
#' Function only usable by FFSBC staff who have a direct or vpn connection to DataPond
#'
#'
#' @title Effortmodel
#' @name Effortmodel
#' @keywords SPDT; DataPond
#' @export
#' @param data a data.frame object of raw counts that wil be used to convert to annual effort estimates. Default value is Edata, which is generated form the DP2R functions Effort2R().
#' @param update_model a TRUE/FALSE to indicate whether to update the current effort model with new data (i.e. model fitting process builds on old model by fitting new data points and takes about 5min), or (FALSE) completely refit the model form scratch (takes about 20min to run.)
#' @param model_path path to current effort model to use for predicitons of unobserved time strata.
#'
#' @examples
#'
#' DP2R::Effort2R()
#' DP2R::Effortmodel()
#'
#' @importFrom magrittr "%>%"


Effortmodel <- function(data = NULL, update_model = TRUE, model_path  = "data/DP2R_Effort_Model.rda") {

  # Ensure the provided data is valid
  if (missing(data) || is.null(data)) {
    stop("Please provide a valid dataset.")
  }

    start_t <- Sys.time()

  if(!update_model){
    fit = lme4::glmer.nb(OE ~ 0 + daytype + hour + month + (0 + 1 | lakeview_yr) + (0 + 1 | weather_code), data = data, nAGQ=0, control = lme4::glmerControl(optimizer = "nloptwrap"), na.action = "na.pass")
  }else{
    load(file = model_path)
    fit = stats::update(fit, data = data)
    }

  end_t <- Sys.time()
  run_t <- end_t-start_t
  save(fit, file = model_path)#Save updated copy of model to update next time new data available
  message("Model run time: ", run_t)
}
