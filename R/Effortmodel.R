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
#' @param update_or_fit a character string "update" or "fit" to indicate whether to update the current effort model with new data (i.e. model fitting process builds on old model by fitting new data points and takes about 5min), or (fit") completely refit the model form scratch (takes about 20min to run.)
#' @param model_path path to current effort model to use for predictions of unobserved time strata. Two models exist, one for open water season: "data/DP2R_Effort_Model.qs2", and one for ice fishing "data/DP2R_Ice_Model.qs2"
#'
#' @examples
#'
#' DP2R::Effort2R()
#' DP2R::Effortmodel()
#'
#' @importFrom magrittr "%>%"


Effortmodel <- function(data = NULL, update_or_fit = "update", model_path  = "data/DP2R_Effort_Model.qs2", add_camX = TRUE) {

  # Ensure the provided data is valid
  if (missing(data) || is.null(data)) {
    stop("Please provide a valid dataset.")
  }

    start_t <- Sys.time()

  if(update_or_fit == "fit"){
    fit = lme4::glmer.nb(OE ~ 0 + daytype + hour + month + (0 + 1 | lakeview_yr) + (0 + 1 | weather_code),
                         data = data,
                         nAGQ=0,
                         control = lme4::glmerControl(optimizer = "nloptwrap"),
                         na.action = "na.pass")
  }else{
    fit = qs2::qs_read(file = model_path)
    fit = stats::update(fit,
                        data = data)
    }

  end_t <- Sys.time()
  run_t <- end_t-start_t
  #save(fit, file = model_path)#Save updated copy of model to update next time new data available
  message("Model run time: ", run_t)
  qs2::qs_save(fit,model_path)
}
