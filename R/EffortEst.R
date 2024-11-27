#' A function analyze Effort counts and estimate total annual effort in angler days.
#' This function builds on other DP2R functions to gather data and fit an effort model to observed data. THe final step is to predict effort in all the unobserved strata withthe effort model.
#'
#'
#' @title EffortEst
#' @name EffortEst
#' @keywords SPDT; DataPond
#' @export
#' @param data a data.frame object of raw counts that will be used to convert to annual effort estimates. Default value is Edata, which is generated form the DP2R function Effort2R().
#' @param update.model a TRUE/FALSE to indicate whether to update the current effort model with new data or just load the current effort model (default)
#' @param model_path path to current effort model to use for predicitons of unobserved time strata.
#'
#' @examples
#'
#' DP2R::EffortEst()
#'
#' @importFrom magrittr "%>%"


EffortEst <- function(data = NULL, update.model = FALSE, model_path = "data/DP2R_Effort_Model.rda") {

  ### 1. Load and/or Update the data and statistical model


  if (missing(data) || is.null(data)) {
    DP2R::Effort2R()
  }


  if (update.model) {
    DP2R::Effortmodel(data = Edata)  # Update the model with new data if required
  } else {
    load(file = model_path)  # Load the model from the specified path if not
  }

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
      sum_spv_obs = if (all(is.na(num_spv))) NA else sum(num_spv, na.rm = TRUE),
      sum_boats_obs = if (all(is.na(num_boat))) NA else sum(num_boat, na.rm = TRUE),
      sum_shore_obs = if (all(is.na(num_shore_ice))) NA else sum(num_shore_ice, na.rm = TRUE),
      sum_OE = sum(OE),
      .groups = "drop"  # Avoids ungrouping later
    )

  ### 6. Generate Prediction category levels for Each Lake
  D <- unique(Pred_lakes$WBID) %>%
    purrr::map_df(function(wbid) {
      # Filter lake data and relevant prediction table rows
      L <- dplyr::filter(Pred_lakes, WBID == wbid)
      S <- dplyr::filter(Pred_vars, year %in% L$year)
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
    dplyr::left_join(Pred_lakes, by = "lakeview_yr") %>%
    dplyr::left_join(.,Lakes[,c("WBID","area_ha")], by = "WBID")%>%

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
