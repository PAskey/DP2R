#' Create lake water profile and surface water summary tables
#'
#' Downloads `vwLakeWater` using [DP2R::DP2R()] and creates two summary tables,
#' `Profile` and `Surface_Water`, in the target environment so they are available
#' for immediate analysis.
#'
#' @param envir Environment to assign `Profile` and `Surface_Water` into.
#'   Defaults to [globalenv()].
#'
#' @return Invisibly returns a named list containing `Profile` and `Surface_Water`.
#'
#' @examples
#' \dontrun{
#' Lakewater2R()
#' head(Profile)
#' head(Surface_Water)
#' }
#'
#' @export
Lakewater2R <- function(envir = globalenv(), keep.raw = FALSE) {

  target_env <- if (keep.raw) envir else NULL

  vwLakeWater <- DP2R::DP2R(
    Tables = "vwLakeWater",
    envir = target_env
  )[[1]]


  params = c(
    "Alkalinity - Low Level : Alkalinity (Total as CaCO3)",
    "Dissolved Oxygen",
    "Nitrogen (Total) : Total Nitrogen (N)",
    "pH",
    "Temperature",
    "Total Dissolved Solids",
    "Total Phosphorus Low Level Total : Total Phosphorus (P)"
  )

  vwLakeWater <- vwLakeWater %>%
    filter(test_name %in% params) %>%
    mutate(
      test_name = recode(
        test_name,
        "Alkalinity - Low Level : Alkalinity (Total as CaCO3)" = "Alkalinity",
        "Nitrogen (Total) : Total Nitrogen (N)" = "TN",
        "Total Dissolved Solids" = "TDS",
        "Total Phosphorus Low Level Total : Total Phosphorus (P)" = "TP"
      )
    )

  vwLakeWater <- clean_sampling_dates(vwLakeWater)

  #Change TN and TP to more readable units
  vwLakeWater <- vwLakeWater %>%
    mutate(
      result = if_else(test_name %in% c("TN", "TP") & unit_code == "mg/L",
                       result * 1000,
                       result),
      unit_code = if_else(test_name %in% c("TN", "TP") & unit_code == "mg/L",
                          "µg/L",
                          unit_code)
    )

  #Exclude any waterbodies with saltwater influence (high TDS) or bad data
  vwLakeWater <- vwLakeWater %>%
    group_by(WBID, sampling_dt) %>%
    filter(!any(test_name == "TDS" & result > 2000, na.rm = TRUE)) %>%
    ungroup()

  Surface_tests = c("Alkalinity", "TN",  "TP", "pH", "TDS" )

  Surface_Water <- vwLakeWater |>
    dplyr::filter(test_name %in% Surface_tests) |>
    dplyr::mutate(
      sampling_dt = lubridate::as_date(sampling_dt),
      year = format(sampling_dt, "%Y"),
      month = format(sampling_dt, "%m")
    ) |>
    dplyr::group_by(WBID, year, month, test_name, unit_code) |>
    dplyr::summarise(
      N = dplyr::n(),
      Mean = signif(mean(result, na.rm = TRUE), 2),
      .groups = "drop"
    )

  Profile_tests = c("Dissolved Oxygen","Temperature")

  Profile <- vwLakeWater |>
    dplyr::filter(
      water_analysis_type %in% c("TL", "PF"),
      test_name %in% Profile_tests
    ) |>
    dplyr::group_by(WBID, sampling_dt) |>
    dplyr::filter(dplyr::n_distinct(depth) >= 3) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      sampling_dt = lubridate::as_date(sampling_dt),
      year = format(sampling_dt, "%Y"),
      month = format(sampling_dt, "%m")
    ) |>
    dplyr::group_by(WBID, year, month, sampling_dt, test_name, unit_code, depth) |>
    dplyr::summarise(
      N = dplyr::n(),
      Mean = round(mean(result, na.rm = TRUE), 1),
      .groups = "drop"
    )

  assign("Profile", Profile, envir = envir)
  assign("Surface_Water", Surface_Water, envir = envir)

  invisible(list(
    Profile = Profile,
    Surface_Water = Surface_Water
  ))
}
