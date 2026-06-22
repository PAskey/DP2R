#' A function to load, clean and standardize release data for analysis.
#' Function only usable with connection to DataPond.
#'
#'
#' This function simplifies code using the releases table to understand all possible stocked groups that could be in a lake at any given time.
#' In cases where clips are unique, then fields for age, strain, genotype are updated.
#' Previous version of this function was within SPDT as SPDTreleases
#' Ultimately, as upload filters and cleaning are improved in the main database, this function could become obsolete.
#'
#' @title link_releases
#' @name link_releases
#' @keywords DP2R; releases; stocked; clips
#' @export
#'
#' @examples
#' #' Must be connected to VPN if working remotely
#'
#' link_releases()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data



#_______________________________________________________________________________
#Open channel to SLD and download data
link_releases <- function(){

  #Check data has been loaded.
  if(!exists("vwLegacyRelease")|!exists("vwIndividualFish")|!exists("vwFishCollection")){DP2R()}

  #Step 1. Clean and standardize release data

  Releases <- vwLegacyRelease %>%
    dplyr::select(-area_ha) %>%#vwLegacyRelease is missing lots of surface area values
    dplyr::left_join(
      DP2R::Lakes %>% dplyr::filter(!is.na(area_ha))%>%dplyr::select(WBID, area_ha),
      by = "WBID"
    ) %>%
    dplyr::mutate(
      age = sby2age(species_code, brood_year, release_year),
      quantity_ha = quantity/area_ha,
      biom_ha = biomass/area_ha
    )


  Releases$Season = metR::season(Releases$release_dt)

  group_cols <- c("region_code", "locale_name","WBID", "area_ha" ,"species_code", "release_year","brood_year","age", "strain","ploidy","mark_code","lifestage_code", "Season", "trans_method_code")

  #First just group together cases of multiple relids for the same group type of fish to the same lake and time.Pretty slow function, so reduced to essential summary variables

  Releases <- Releases%>%
    #group by Spr or Fall in case 2 trips on different days of same fish
    dplyr::group_by(!!!rlang::syms(c(group_cols)))%>%
    dplyr::summarise(
      rel_Date = mean(as.Date(.data$release_dt,
                              format = "%Y-%m-%d"), na.rm = TRUE),
      size_g = round(sum(quantity*size_g)/sum(quantity),1),
      SAR_cat = SAR_cat(size_g),
      Quantity = sum(quantity),
      Biomass_kg = sum(biomass),
      Quantity_ha = sum(quantity_ha, na.rm = T),
      Biom_ha = sum(biom_ha, na.rm = T),
      .groups = "drop"
    )

  #Step 2. Find the list of sample events in the Collections that could be linked to releases.
  # event_start_dt = first sampling date in that Sample_event
  Sampled_events <- vwFishCollection %>%
    dplyr::group_by(WBID, year, Sample_event) %>%
    dplyr::summarise(
      event_start_dt = min(as.Date(start_dt), na.rm = TRUE),
      .groups = "drop"
    )

  #Use data on max age observed to see how far to span linkages to releases
  Observed_ages <- vwIndividualFish %>%
    dplyr::filter(age < 50) %>%
    dplyr::group_by(WBID, species_code) %>%
    dplyr::summarise(
      max_age_obs = max(age, na.rm = TRUE),
      .groups = "drop"
    )

  Sampled_events_species <- Sampled_events %>%
    dplyr::inner_join(
      Releases %>%
        dplyr::distinct(WBID, species_code),
      by = "WBID"
    ) %>%
    dplyr::left_join(
      Observed_ages,
      by = c("WBID", "species_code")
    )



  endage = 6# Set the max age you expect to retrieve stocked fish in a lake. This value is overwritten if fish were aged at an older age from a given lake at any point in time. So this is more like an average maximum age sampled.

#The sample events with maximum age to search back through releases.
  maxxages <- Sampled_events_species %>%
    dplyr::mutate(max_age = pmax(endage, max_age_obs, na.rm = TRUE)) %>%
    dplyr::group_by(WBID, species_code) %>%
    dplyr::mutate(max_age = max(max_age, na.rm = TRUE)) %>%   # lake-species max across events
    dplyr::ungroup()


  #Step 3. From those sampled lake years find the sequence of previous stocking years to that lake that could have fish in the lake at the time of sampling.
  #Search back to oldest age observed or the endage.

  Sampled <- maxxages %>%
    dplyr::filter(!is.na(year)) %>%
    dplyr::group_by(WBID, species_code, year, Sample_event, event_start_dt, max_age) %>%
    dplyr::mutate(YearSeq = purrr::map(year, ~ seq((. - max_age), .))) %>%
    tidyr::unnest(YearSeq) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      WBID, species_code,
      sample_year = year,
      Sample_event,
      event_start_dt,
      release_year = YearSeq
    )

  #Step 4. Go back to releases and filter down to the matching lake-year combinations


  #This is a much faster way to filter than using interaction()
  ##THIS TABLE IS ADDED TO ENVIRO_______________________________________________
  Rel_sampled <- Releases %>%
    dplyr::inner_join(
      Sampled,
      by = c("WBID", "species_code", "release_year"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(
      release_year < sample_year |
        (release_year == sample_year & dplyr::coalesce(rel_Date <= event_start_dt, TRUE))
    ) %>%
    dplyr::mutate(Rel_Age = age, age = (sample_year - release_year) + Rel_Age)


  #Step 5. Link individual fish back to their stocking records. If they are aged, the we can narrow down to release event(s) in one year to one lake.


  aged_in_lake <- Rel_sampled %>%
    dplyr::group_by(!!!rlang::syms(c(
      "Sample_event",
      "sample_year",
      setdiff(group_cols,
              c("release_year","brood_year","lifestage_code","Season","strain","ploidy", "trans_method_code"))
    ))) %>%
    dplyr::summarise(
      sby_rel    = dplyr::na_if(stringr::str_c(unique(na.omit(brood_year)), collapse=","), ""),
      Strain_rel = dplyr::na_if(stringr::str_c(unique(na.omit(strain)),     collapse=","), ""),
      Geno_rel   = dplyr::na_if(stringr::str_c(unique(na.omit(ploidy)),     collapse=","), ""),
      LS_rel     = dplyr::na_if(stringr::str_c(unique(na.omit(lifestage_code)), collapse=","), ""),
      AF = all(grepl("F", ploidy)),
      Sterile = all(grepl("3", ploidy)),
      wt_rel = round(sum(.data$size_g*.data$Quantity)/sum(.data$Quantity), 1),
      N_ha_rel = sum(Quantity_ha),
      avg_rel_date = dplyr::if_else(
        stringr::str_detect(sby_rel, ","),
        as.Date(NA),
        as.Date(mean(rel_Date))
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(Poss_Age = age)

  #If they are not aged, then in most cases there are multiple stocking events to a given lake that could have resulted in the sampled fish.
  #Unless it had a uniquely identifiable clip.

  #The second block builds off of the age-in-lake instead of Rel_sampled, so that the binning of N_ha_rel across strains is complete and not confounded as much across years..

  #This controls for not creating text NA values when collapsing lists.
  split_collapse <- function(x) {
    v <- unique(trimws(unlist(strsplit(x, ",", fixed = TRUE))))
    v <- v[!is.na(v) & v != ""]
    dplyr::na_if(stringr::str_c(v, collapse=","), "")
  }

  unaged_in_lake <- aged_in_lake %>%
    dplyr::group_by(!!!rlang::syms(c(
      "Sample_event",
      "sample_year",
      setdiff(group_cols,
              c("release_year","brood_year","age","lifestage_code","Season","strain","ploidy", "trans_method_code"))
    ))) %>%
    dplyr::summarise(
      sby_rel    = split_collapse(sby_rel),
      Strain_rel = split_collapse(Strain_rel),
      Geno_rel   = split_collapse(Geno_rel),
      AF = all(AF),
      Sterile = all(Sterile),
      LS_rel     = split_collapse(LS_rel),
      wt_rel = dplyr::if_else(
        dplyr::n()==1 | (sd(wt_rel)/mean(wt_rel)) < 0.5,
        mean(wt_rel),
        NA_real_
      ),
      N_ha_rel = dplyr::if_else(
        dplyr::n()==1 | (sd(N_ha_rel)/mean(N_ha_rel)) < 0.15,
        mean(N_ha_rel),
        NA_real_
      ),
      avg_rel_date = dplyr::if_else(
        stringr::str_detect(sby_rel, ","),
        as.Date(NA),
        as.Date(mean(avg_rel_date))
      ),
      Poss_Age = { v <- sort(unique(age)); v <- v[!is.na(v)]; dplyr::na_if(stringr::str_c(v, collapse=","), "") },
      .groups = "drop"
    ) %>%
    dplyr::mutate(age = NA_integer_)

  ##Add in a variable to document how many years the current stocking prescription has been stable (same as previous years)
  #Group based on a per lake-year stocking prescription across, species, strains, life-stages. Strain is an unknown impact and variable for species other than RB, so disregard for other species
#Build prescription by stocking year
  Stable_base <- Releases %>%
    dplyr::mutate(Strain_use = dplyr::if_else(species_code == "RB", strain, species_code)) %>%
    dplyr::group_by(WBID, release_year) %>%
    dplyr::summarise(
      Strain_rel     = paste(sort(unique(Strain_use)), collapse = ","),
      Geno_rel       = paste(sort(unique(ploidy)), collapse = ","),
      Total_Quantity = sum(Quantity),
      .groups = "drop"
    ) %>%
    dplyr::arrange(WBID, release_year)

  #Calculate stocking frequency and if quantity stable to within "limit"
  limit = 0.1
  Stable_base <- Stable_base %>%
    dplyr::group_by(WBID) %>%
    dplyr::mutate(
      gap = release_year - lag(release_year),
      Frequency = as.integer(stats::median(gap, na.rm = TRUE)),
      qty_stable = abs(Total_Quantity - lag(Total_Quantity)) /
        lag(Total_Quantity)<limit
    ) %>%
    dplyr::ungroup()

  #Create a prescription key
  Stable_base <- Stable_base %>%
    dplyr::mutate(
      presc_key = paste(Strain_rel, Geno_rel, qty_stable, Frequency, sep = "|")
    )

  #Expand over years where fish not released but potentially sampled
  Stable <- Stable_base %>%
    dplyr::group_by(WBID) %>%
    tidyr::complete(
      release_year = seq(min(release_year), max(release_year), 1)
    ) %>%
    dplyr::rename(sample_year = release_year)%>%
    dplyr::arrange(WBID, sample_year)%>%
    tidyr::fill(presc_key, Frequency, .direction = "down") %>%
    dplyr::ungroup()

  #Compute stable years across all years, which can then be added to linked releases
  Stable <- Stable %>%
    dplyr::group_by(WBID) %>%
    dplyr::mutate(
      change = presc_key != lag(presc_key),
      grp = cumsum(tidyr::replace_na(change, TRUE)),
      Stable_yrs = ave(sample_year, interaction(WBID, grp), FUN = seq_along)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(WBID, sample_year, Stable_yrs)

  ##THIS TABLE IS ADDED TO ENVIRO_______________________________________________
  Link_rel <- rbind(aged_in_lake, unaged_in_lake) %>%
    dplyr::left_join(Stable, by = c("WBID","sample_year")) %>%
    tidyr::replace_na(list(Stable_yrs = 0))

  Rel_sampled <-Rel_sampled%>%
    dplyr::left_join(Stable, by = c("WBID","sample_year")) %>%
    tidyr::replace_na(list(Stable_yrs = 0))

###SERIES OF ROUNDING EVENTS FOR BETTER DISPLAY
  Releases <- Releases %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("area_ha", "size_g", "wt_rel", "Quantity_ha", "N_ha_rel", "Biom_ha", "Biomass_kg")),
                    ~ round(.x, 1))
    )

  Rel_sampled <- Rel_sampled %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("area_ha", "size_g", "wt_rel", "Quantity_ha", "N_ha_rel", "Biom_ha", "Biomass_kg")),
                    ~ round(.x, 1))
    )

  Link_rel <- Link_rel %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("wt_rel", "N_ha_rel")), ~ round(.x, 1))
    )



  Releases <<-Releases#All releases in database cleaned and standardized
  Rel_sampled <<-Rel_sampled#Releases associated with sampled lakes in database cleaned and standardized
  Link_rel<<-Link_rel#Releases summarized into the in-lake observable and un-observable release characteristics, so that each individual fish can potentially be tied to a specific or group of releases into that lake


}
