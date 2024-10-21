#' A function analyze Effort counts and estimate total annual effort in angler days.
#' This function builds on other DP2R functions to gather data and fit an effort model to observed data. THe final step is to predict effort in all the unobserved strata withthe effort model.
#'
#'
#' @title EffortEst
#' @name EffortEst
#' @keywords SPDT; DataPond
#' @export
#' @param data a data.frame object of raw counts that wil be used to convert to annual effort estimates. Default value is Edata, which is generated form the DP2R function Effort2R().
#' @param update.model a TRUE/FALSE to indicate whether to update the current effort model with new data or just load the current effort model (default)
#' @param model_path path to current effort model to use for predicitons of unobserved time strata.
#'
#' @examples
#'
#' DP2R::Effort2R()
#' DP2R::EffortEst()
#'
#' @importFrom magrittr "%>%"


EffortEst <- function(data = Edata, update.model = FALSE, model_path = "data/DP2R_Effort_Model.rda") {

  ### 1. Load or Update the Model
  if (update.model) {
    DP2R::Effortmodel()  # Load the updated model if required
  } else {
    load(file = model_path)  # Load the model from the specified path
  }

  ### 2. Create a Prediction Table for Months and Day Types (May 1st to Oct 31st)
  Pred_table <- DP2R::DayTypes %>%
    dplyr::mutate(
      year = lubridate::year(date),  # Extract the year
      month = lubridate::month(date)  # Extract the month
    ) %>%
    dplyr::filter(month %in% c(5:10)) %>%  # Filter by relevant months in Edata
    dplyr::group_by(year, month, daytype) %>%  # Group by fixed effect predictors
    dplyr::summarize(
      ndays = dplyr::n_distinct(date),  # Unique days in each group
      hour = "12",  # Set reference hour
      weather_code = "FAIR",  # Default weather code
      .groups = "drop"  # Avoids unnecessary ungrouping
    ) %>%
    droplevels()

  ### 3. Match Prediction Table to Model’s Reference Levels
  Pred_table <- DP2R::fac.data(
    Pred_table,
    varlist = list(
      "year" = NULL,
      "month" = levels(fit@frame$month)[1],  # Use model’s first month level
      "daytype" = "WE",  # Weekend as default reference
      "hour" = "12"  # Reference hour
    )
  )

  ### 4. Calculate Day Expansion Factor
  # Get fixed effects from the model and compute full-day effort expansion
  fixefs <- lme4::fixef(fit)
  Day_expand <- sum(exp(c(0, fixefs[grepl("^hour", names(fixefs))])))
  # Adding exp(0) ensures reference hour (12 PM) is included

  ### 5. Summarize Effort by Lake
  Pred.lakes <- Edata %>%
    dplyr::group_by(
      region, WBID, gazetted_name, area_ha, view_location_name,
      year, lakeview_yr, method
    ) %>%
    dplyr::summarize(
      N = dplyr::n(),  # Number of observations
      sum_spv_obs = if (all(is.na(num_spv))) NA else sum(num_spv, na.rm = TRUE),
      sum_boats_obs = if (all(is.na(num_boat))) NA else sum(num_boat, na.rm = TRUE),
      sum_shore_obs = if (all(is.na(num_shore_ice))) NA else sum(num_shore_ice, na.rm = TRUE),
      sum_OE = sum(OE),
      .groups = "drop"  # Avoids ungrouping later
    )

  ### 6. Generate Predictions for Each Lake
  D <- unique(Pred.lakes$WBID) %>%
    purrr::map_df(function(wbid) {
      # Filter lake data and relevant prediction table rows
      L <- dplyr::filter(Pred.lakes, WBID == wbid)
      S <- dplyr::filter(Pred_table, year %in% L$year)
      merge(S, L, all = TRUE)  # Merge prediction table with lake data
    })

  ### 7. Ensure Factor Levels Match Model's Fit and Generate Predictions and Compute Final Summary
  D <- D %>%
    dplyr::mutate(
      hour = factor(hour, levels = levels(fit@frame$hour)),
      weather_code = factor(weather_code, levels = levels(fit@frame$weather_code)),
      preds = predict(fit, newdata = D, type = "response"),  # Generate predictions
      predx = preds * Day_expand * ndays  # Expand predictions to full-day effort
    )


  Pred.summary <- D %>%
    dplyr::group_by(lakeview_yr) %>%
    dplyr::summarize(Pred_E = round(sum(predx)), .groups = "drop")

  # Define constants for angler-day calculations
  #Should have different number for when spv were counted or not.
  Angs_p_boat <- 1.83
  Hrs_p_day <- 3.15
  Hrs_p_day_shore <- 2.39

  # Final effort calculations
  sum.pred <- Pred.summary %>%
    dplyr::left_join(Pred.lakes, by = "lakeview_yr") %>%

    # Step 1: Calculate predicted boat and shore hours
    dplyr::mutate(
      Pred_B = ifelse(sum_OE == 0,
                      0,round(Pred_E * ((sum_OE - dplyr::coalesce(sum_shore_obs, 0)) / sum_OE))),
      Pred_S = ifelse(sum_OE == 0,
                      0,round(Pred_E * (dplyr::coalesce(sum_shore_obs, 0) / sum_OE))),
      Boat_hrs = round(Pred_B),
      Shore_hrs = round(Pred_S)
    ) %>%

    # Step 2: Calculate angler days for boat and shore
    dplyr::mutate(
      lakeview_angler_days = round(
        Pred_B * Angs_p_boat / Hrs_p_day +
          Pred_S / Hrs_p_day_shore
      ),
      Boat_angler_days = round(Boat_hrs * Angs_p_boat / Hrs_p_day),

      # Ensure Shore_angler_days is 0 when sum_shore_obs is 0, otherwise calculate normally
      Shore_angler_days = dplyr::case_when(
        sum_shore_obs == 0 ~ 0,
        !is.na(sum_shore_obs) ~ round(Shore_hrs / Hrs_p_day_shore),
        TRUE ~ NA_real_
      )
    ) %>%

    # Step 3: Handle edge cases with infinite or NaN values
  #  dplyr::mutate(
  #    Shore_angler_days = dplyr::if_else(
  #      is.infinite(Shore_hrs) & sum_boats_obs > 0,
  #      (dplyr::coalesce(sum_shore_obs, 0) / sum_boats_obs) * Boat_angler_days,
  #      Shore_angler_days
  #    ),
  #    Boat_angler_days = dplyr::na_if(
  #      replace(Boat_angler_days, is.infinite(Boat_angler_days) | is.nan(Boat_angler_days), NA),
  #      0
  #    ),
  #    Shore_angler_days = dplyr::na_if(
  #      replace(Shore_angler_days, is.infinite(Shore_angler_days) | is.nan(Shore_angler_days), NA),
  #      0
  #    )
  #  ) %>%

    # Step 4: Compute total angler days
    dplyr::rowwise() %>%
    dplyr::mutate(
      Angler_days = sum(c(Shore_angler_days, Boat_angler_days), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%

    # Step 5: Replace 0 angler days with NA if observations exist
    dplyr::mutate(
      Angler_days = dplyr::if_else(
        Angler_days == 0 & (dplyr::coalesce(sum_boats_obs, 0) > 0 | dplyr::coalesce(sum_shore_obs, 0) > 0),
        NA_real_,
        Angler_days
      ),
      Angler_days_p_ha = dplyr::if_else(area_ha>0, round(Angler_days/area_ha,1), NA_real_)
    )

  #Lake by lake summary of effort data
  Lake_sum <- sum.pred %>%
    dplyr::group_by(WBID, gazetted_name) %>%
    dplyr::summarise(N_years = length(unique(year)), Methods = paste0(unique(method),collapse = ","), mean_AD = round(mean(Angler_days, na.rm = TRUE),1), mean_lakeview= round(mean(lakeview_angler_days, na.rm = TRUE),1), marker_size = max(mean_AD,mean_lakeview, na.rm = TRUE),
                     .groups = "drop" )%>%
    dplyr::mutate(AD_percentile = 100*round(dplyr::min_rank(mean_AD)/dplyr::n(),2))

Lake_sum = dplyr::left_join(Lake_sum, Lakes[,c("WBID","lake_latitude","lake_longitude")], by = "WBID")

Shinydata <<- sum.pred
lakesum <<- Lake_sum

}
