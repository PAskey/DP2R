library(dplyr)
library(tidyr)
library(purrr)
library(DP2R)
DP2R()

build_sel_lookup_event_observed <- function(vwFishCollection, vwIndividualFish, sel_lookup) {

  # ---- 1) Observed (Sample_event × species) ----
  spp_event <- vwIndividualFish %>%
    filter(!is.na(Sample_event)) %>%
    filter(!is.na(species_code)) %>%
    distinct(Sample_event, species_code)

  if (nrow(spp_event) == 0) {
    return(tibble::tibble(Sample_event = character(), species_code = character(), curve = list()))
  }

  # ---- 2) GN design effort within Sample_event ----
  design_effort <- vwFishCollection %>%
    filter(method == "GN") %>%
    mutate(
      fishing_hours = dplyr::if_else(is.na(fishing_hours), 24, fishing_hours),
      sample_design_code = dplyr::if_else(
          is.na(sample_design_code) &
            end_year > 2014 |
            stringr::str_detect(net_angler_id, "RIC|RISC"),
          "SGN7",
          sample_design_code
        )
    ) %>%
    filter(!is.na(sample_design_code)) %>%
    group_by(Sample_event, sample_design_code) %>%
    summarise(
      fishing_hours = sum(fishing_hours, na.rm = TRUE),
      .groups = "drop"
    )

  if (nrow(design_effort) == 0) {
    return(tibble::tibble(Sample_event = character(), species_code = character(), curve = list()))
  }

  # ---- 3) Keep only events that actually have design-coded GN nets ----
  spp_event <- spp_event %>%
    semi_join(design_effort %>% distinct(Sample_event), by = "Sample_event")

  if (nrow(spp_event) == 0) {
    return(tibble::tibble(Sample_event = character(), species_code = character(), curve = list()))
  }

  # ---- 4) Expand across designs in event ----
  need <- spp_event %>%
    inner_join(design_effort, by = "Sample_event",
               relationship = "many-to-many")

  # ---- 5) Join to sel_lookup and compute weights ----
  joined <- need %>%
    left_join(sel_lookup, by = c("species_code", "sample_design_code")) %>%
    mutate(
      sample_design_code = stringr::str_squish(as.character(sample_design_code)),
      weight = panel_area_total * fishing_hours) %>%
    filter(!is.na(weight), weight > 0) %>%
    filter(!purrr::map_lgl(curve, is.null))

  if (nrow(joined) == 0) {
    return(tibble::tibble(Sample_event = character(), species_code = character(), curve = list()))
  }

  # ---- 6) Weighted mixture curve per (Sample_event × species) ----
  sel_lookup_event <- joined %>%
    group_by(Sample_event, species_code) %>%
    summarise(
      curve = list({
        w <- weight
        curves <- curve
        num <- Reduce(`+`, Map(function(v, wi) v * wi, curves, w))
        num / sum(w)
      }),
      effort_total = sum(weight),
      n_designs = n_distinct(sample_design_code),
      event_designs = list(unique(sample_design_code)),
      .groups = "drop"
    )

  sel_lookup_event
}

# Call it like this:
sel_lookup_event <- build_sel_lookup_event_observed(vwFishCollection, vwIndividualFish, sel_lookup)

usethis::use_data(sel_lookup_event, overwrite = TRUE)
