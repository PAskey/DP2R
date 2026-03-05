library(dplyr)
library(tidyr)
library(purrr)
library(DP2R)
DP2R()
names(vwFishCollection)

build_sel_lookup_event_observed <- function(vwFishCollection, vwIndividualFish, sel_lookup) {

  vwFishCollection<- vwFishCollection %>%
    dplyr::left_join(lake_names[,c("WBID","locale_name")], by = "WBID")%>%
    dplyr::relocate(locale_name, .after = WBID)

  # ---- 0) Event metadata (1 row per Sample_event) ----
  event_meta <- vwFishCollection %>%
    dplyr::filter(!is.na(Sample_event)) %>%
    dplyr::distinct(Sample_event, locale_name, year, Season)

  # ---- 1) Observed (Sample_event × species) ----
  spp_event <- vwIndividualFish %>%
    dplyr::filter(!is.na(Sample_event)) %>%
    dplyr::filter(!is.na(species_code)) %>%
    dplyr::distinct(Sample_event, species_code)

  if (nrow(spp_event) == 0) {
    return(tibble::tibble(
      Sample_event = character(),
      locale_name = character(),
      year = numeric(),
      Season = character(),
      species_code = character(),
      curve = list()
    ))
  }

  # ---- 2) GN design effort within Sample_event ----
  design_effort <- vwFishCollection %>%
    dplyr::filter(method == "GN") %>%
    dplyr::mutate(
      fishing_hours = dplyr::if_else(is.na(fishing_hours), 24, fishing_hours),
      sample_design_code = dplyr::if_else(
        (is.na(sample_design_code) & end_year > 2014) |
          stringr::str_detect(net_angler_id, "RIC|RISC"),
        "SGN7",
        sample_design_code
      )
    ) %>%
    dplyr::filter(!is.na(sample_design_code)) %>%
    dplyr::group_by(Sample_event, sample_design_code) %>%
    dplyr::summarise(
      fishing_hours = sum(fishing_hours, na.rm = TRUE),
      .groups = "drop"
    )

  if (nrow(design_effort) == 0) {
    return(tibble::tibble(
      Sample_event = character(),
      locale_name = character(),
      year = numeric(),
      Season = character(),
      species_code = character(),
      curve = list()
    ))
  }

  # ---- 3) Keep only events that actually have design-coded GN nets ----
  spp_event <- spp_event %>%
    dplyr::semi_join(design_effort %>% dplyr::distinct(Sample_event), by = "Sample_event")

  if (nrow(spp_event) == 0) {
    return(tibble::tibble(
      Sample_event = character(),
      locale_name = character(),
      year = numeric(),
      Season = character(),
      species_code = character(),
      curve = list()
    ))
  }

  # ---- 4) Expand across designs in event ----
  need <- spp_event %>%
    dplyr::inner_join(design_effort, by = "Sample_event",
                      relationship = "many-to-many")

  # ---- 5) Join to sel_lookup and compute weights ----
  joined <- need %>%
    dplyr::left_join(sel_lookup, by = c("species_code", "sample_design_code")) %>%
    dplyr::mutate(
      sample_design_code = stringr::str_squish(as.character(sample_design_code)),
      weight = panel_area_total * fishing_hours
    ) %>%
    dplyr::filter(!is.na(weight), weight > 0) %>%
    dplyr::filter(!purrr::map_lgl(curve, is.null))

  if (nrow(joined) == 0) {
    return(tibble::tibble(
      Sample_event = character(),
      locale_name = character(),
      year = numeric(),
      Season = character(),
      species_code = character(),
      curve = list()
    ))
  }

  # ---- 6) Weighted mixture curve per (Sample_event × species) ----
  sel_lookup_event <- joined %>%
    dplyr::group_by(Sample_event, species_code) %>%
    dplyr::summarise(
      curve = list({
        w <- weight
        curves <- curve
        num <- Reduce(`+`, Map(function(v, wi) v * wi, curves, w))
        num / sum(w)
      }),
      effort_total = sum(weight),
      n_designs = dplyr::n_distinct(sample_design_code),
      event_designs = list(unique(sample_design_code)),
      .groups = "drop"
    ) %>%
    # ---- 7) Attach event metadata ----
  dplyr::left_join(event_meta, by = "Sample_event") %>%
    dplyr::relocate(locale_name, year, Season, .after = Sample_event)

  sel_lookup_event
}

# Call it like this:
sel_lookup_event <- build_sel_lookup_event_observed(vwFishCollection, vwIndividualFish, sel_lookup)

usethis::use_data(sel_lookup_event, overwrite = TRUE)
