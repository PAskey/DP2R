#' A function analyze Effort counts and estimate total annual effort in angler days.
#' This function builds on other DP2R functions to gather data and fit an effort model to observed data. THe final step is to predict effort in all the unobserved strata withthe effort model.
#'
#'
#' @title EffortEst
#' @name EffortEst
#' @keywords SPDT; DataPond
#' @export
#' @param data a data.frame object of raw counts that will be used to convert to annual effort estimates. Default value is Edata, which is generated form the DP2R function Effort2R().
#' @param month_span a vector of the month values which the total effort estimate is calculated. Default is c(5:10), in which case the annual angler days are the sum over May through October. You cannot use a month span outside of the range of your data. Data may be different for ice fishing or open water seasons.
#' @param update.model a TRUE/FALSE to indicate whether to update the current effort model with new data or just load the current effort model (default)
#' @param model_path path to current effort model to use for predictions of unobserved time strata. Two models exist, one for open water season: "data/DP2R_Effort_Model.qs2", and one for ice fishing "data/DP2R_Ice_Model.qs2"
#'
#' @examples
#'
#' DP2R::EffortEst()
#'
#' @importFrom magrittr "%>%"


EffortEst <- function(data = NULL, month_span = c(5:10), update.model = FALSE, model_path = "data-raw/DP2R_Effort_Model.qs2") {

  ### 1. Load and/or Update the data and statistical model


  if (missing(data) || is.null(data)) {
    DP2R::Effort2R()
    data = Edata
  }
  data = data[data$month%in%month_span,]%>%droplevels()
  Lakes <- DP2R::DP2R(Tables = "vwWaterbodyLake")$vwWaterbodyLake#%>%dplyr::filter(WBID%in%data$WBID)


  if (update.model) {
    DP2R::Effortmodel(data = data)  # Update the model with new data if required. Autosaved to model_path location
  }


    fit = qs2::qs_read(model_path)  # Load the model from the specified path if not


  ### 2. Load a Prediction Table for Months and Day Types and match to the data set being predicted(e.g. open water is usually May 1st to Oct 31st)
 data(Pred_vars)
 Pred_vars = Pred_vars%>%dplyr::filter(year %in% data$year, month %in% as.numeric(as.character(data$month)))

  ### 3. Match Prediction Table to Model’s Reference Levels
  Pred_vars <- DP2R::fac.data(
    Pred_vars,
    varlist = list(
      "year" = NULL,
      "month" = levels(data$month)[1],  # Use model’s first month level (different for ice and open water)
      "daytype" = "WE",  # Weekend as default reference
      "hour" = "12",  # Reference hour
      "weather_code" = "UNK"
    )
  )

  ### 4. Calculate Day Expansion Factor
  # Get fixed effects from the model and compute full-day effort expansion
  fixefs <- lme4::fixef(fit)
  Day_expand <- sum(exp(c(0, fixefs[grepl("^hour", names(fixefs))])))
  # Adding exp(0) ensures reference hour (12 PM) is included

  ### 5. Summarize Effort by Lake
  Pred_lakes <- data %>%
    dplyr::group_by(
      region, WBID, gazetted_name, view_location_name,
      year, lakeview_yr, method
    ) %>%
    dplyr::summarize(
      N = dplyr::n(),  # Number of observations
      spv_obs = if (all(is.na(num_spv))) NA else sum(num_spv, na.rm = TRUE),
      boats_obs = if (all(is.na(num_boat))) NA else sum(num_boat, na.rm = TRUE),
      shore_obs = if (all(is.na(num_shore_ice))) NA else sum(num_shore_ice, na.rm = TRUE),
      OE = sum(OE),
      .groups = "drop"  # Avoids ungrouping later
    )

  ### 6. Generate Prediction category levels for Each Lake
  D <- unique(Pred_lakes$WBID) %>%
    purrr::map_df(function(wbid) {
      # Filter lake data and relevant prediction table rows
      L <- dplyr::filter(Pred_lakes[,c('WBID','year','lakeview_yr')], WBID == wbid)
      S <- dplyr::filter(Pred_vars, year %in% L$year)
      merge(S, L, all = TRUE)  # Merge prediction table with lake data
    })

  ### 7. Ensure Factor Levels Match Model's Fit and Generate Predictions and Compute Final Summary
  D <- D %>%
    dplyr::mutate(
      hour = factor(hour, levels = levels(fit@frame$hour)),
      weather_code = factor(weather_code, levels = levels(fit@frame$weather_code)),
      #preds = predict(fit, newdata = D, type = "response"),  # Generate predictions
      predx = predict(fit, newdata = D, type = "response") * Day_expand * ndays  # Generate predictions, Expand predictions to full-day effort
    )


  Pred.summary <- D %>%
    dplyr::group_by(lakeview_yr) %>%
    dplyr::summarize(Pred_E = round(sum(predx)), .groups = "drop")


  # Final effort calculations
  sum.pred <- Pred.summary %>%
    dplyr::left_join(Pred_lakes, by = "lakeview_yr") %>%
    dplyr::left_join(.,Lakes[,c("WBID","area_ha")], by = "WBID")%>%
    dplyr::mutate(area_ha = round(area_ha,1))

  # Define constants for angler-day calculations
  #Should update to have different number for when spv were counted or not. Lower avg if spv not separated
  calculate_angler_days <- function(df,
                                    Angs_p_boat = 1.83,
                                    Hrs_p_day = 3.15,
                                    Hrs_p_day_shore = 2.39
                                    ) {
    # Step 1: Calculate predicted boat and shore hours
    Pred_spv <- ifelse(df$OE == 0, 0, df$Pred_E * ((dplyr::coalesce(df$spv_obs, 0)) / df$OE))
    Pred_B <- ifelse(df$OE == 0, 0, round(df$Pred_E * ((dplyr::coalesce(df$boats_obs, 0)) / df$OE)))
    Pred_S <- ifelse(df$OE == 0, 0, round(df$Pred_E * (dplyr::coalesce(df$shore_obs, 0) / df$OE)))

    # Step 2: Calculate angler days for boat and shore
    spv_AD <- round(Pred_spv / Hrs_p_day)

    Boat_AD <- round(Pred_B * Angs_p_boat / Hrs_p_day)

    Shore_AD <- ifelse(df$shore_obs == 0, 0, round(Pred_S / Hrs_p_day_shore))

    # Step 3: Compute total angler days
    Angler_days <- rowSums(cbind(Shore_AD, spv_AD, Boat_AD), na.rm = TRUE)

    # Step 4: Replace 0 angler days with NA if observations exist
    Angler_days <- ifelse(Angler_days == 0 & (dplyr::coalesce(df$boats_obs, 0) > 0 | dplyr::coalesce(df$shore_obs, 0) > 0),
                          NA, Angler_days)

    Angler_days_p_ha <- ifelse(df$area_ha > 0, round(Angler_days / df$area_ha, 1), NA)

    # Return the relevant columns only
    calcs <- data.frame(
      spv_AD,
      Boat_AD,
      Shore_AD,
      Angler_days,
      Angler_days_p_ha
    )
    result = cbind(df,calcs)
    return(result)
  }

  sum.pred = calculate_angler_days(sum.pred)


  #Create a table of camera expansion factors when verification counts have been conducted
  #Step 1. Pull all of the lakeview_yrs that were crated specific for verification using the Cam_xdata_dt.R function within Effort2R()
  Xcam<-sum.pred%>%dplyr::filter(grepl('AIR|CAM|GR', lakeview_yr))%>%

    #Step 2. COpare the effort estimates between camera view and full lake using same sample days/hours
    dplyr::select(c(WBID:N,Angler_days))%>%
    tidyr::pivot_wider(names_from = method, values_from = c(Angler_days))%>%
    dplyr::rowwise()%>%
    dplyr::mutate(Exp = dplyr::if_else(CAM > 0, max(1, max(AIR, GRD, na.rm = TRUE) / CAM), NA_real_))%>%

    #Step 3. When more than one year or verifcation method is used for same camera view take a weighted mean
    dplyr::group_by(WBID, view_location_name)%>%
    dplyr::summarize(Exp_N = sum(N),
                     Exp = round(weighted.mean(Exp, N),2))%>%
    dplyr::ungroup()

##OK, now we can adjust estimates from cameras that have verification counts
  sum.pred = dplyr::left_join(sum.pred,Xcam, by = c("WBID","view_location_name"))%>%
    #and remove data that is just for Exp estimation
    dplyr::filter(!grepl('AIR|CAM|GR', lakeview_yr))

##Adjust camera data when verification available
  sum.pred <- sum.pred %>%
    dplyr::mutate(
      spv_AD = ifelse(method == "CAM", round(spv_AD * Exp,1), spv_AD),
      Boat_AD = ifelse(method == "CAM", round(Boat_AD * Exp,1), Boat_AD),
      Shore_AD = ifelse(method == "CAM", round(Shore_AD * Exp,1), Shore_AD),
      Angler_days = ifelse(method == "CAM", round(Angler_days * Exp,1), Angler_days),
      Angler_days_p_ha = ifelse(method == "CAM", round(Angler_days_p_ha * Exp,1), Angler_days_p_ha)
    )

  # Define column order
  cols <- c("region", "WBID", "gazetted_name", "view_location_name", "year", "method", "N", "spv_obs", "boats_obs","shore_obs","spv_AD","Boat_AD","Shore_AD","Angler_days","Angler_days_p_ha","area_ha","Exp","Exp_N")

  EffortEsts = sum.pred%>%
    dplyr::mutate(year = as.integer(as.character(year)))%>%#back to number format for shiny functionality
    dplyr::select(tidyr::all_of(cols))%>%
    dplyr::rename(CAM_Exp=Exp)

  EffortEsts<<-EffortEsts
Lakes<<-Lakes

}
