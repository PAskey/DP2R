#' A function to load, clean and standardize release data for analysis.
#' Function only usable with connection to DataPond.
#'
#'
#' This is a non-exported funciton to simplify code using the releases table to understand all possible stocked groups that could be in a lake at any given time.
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
#' CLEANreleases()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data



#_______________________________________________________________________________
#Open channel to SLD and download data
link_releases <- function(){

  #Check data has been loaded.
  if(!exists("vwLegacyRelease")|!exists("vwIndividualFish")){DP2R()}

  #Step 1. Clean and standardize release data

  Releases = vwLegacyRelease%>%
    dplyr::mutate(age = sby2age(species_code, brood_year, release_year))


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

  #Step 2. Find the list of sampled lake-years in the vwIndividualFish table that could be linked to releases.
  # Step 2 (revised): sampled events (not just lake-years)
  # event_start_dt = first sampling date in that Sample_event
  Sampled_events <- vwIndividualFish %>%
    dplyr::semi_join(Releases, by = c("WBID", "species_code")) %>%
    dplyr::group_by(WBID, species_code, year, Sample_event) %>%
    dplyr::summarise(
      event_start_dt = min(as.Date(date_assessed), na.rm = TRUE),
      max_age_obs = suppressWarnings(max(age[age < 50], na.rm = TRUE)),
      .groups = "drop"
    )




  endage = 6# Set the max age you expect to retrieve stocked fish in a lake. This value is overwritten if fish were aged at an older age from a given lake at any point in time. So this is more like an average maximum age sampled.

  #The Sampled_only variable filters lakes and releases to cases where a lake has been assessed (data exists in the vwIndividualFish Table). However, some cases in the vwIndividualFish table are not true in-lake sampling events.We will remove these from here and all further analyses. If they are wanted use SLD2R() only.

  maxxages <- Sampled_events %>%
    dplyr::mutate(max = pmax(endage, max_age_obs, na.rm = TRUE)) %>%
    dplyr::group_by(WBID, species_code) %>%
    dplyr::mutate(max = max(max, na.rm = TRUE)) %>%   # lake-species max across events
    dplyr::ungroup()


  #Step 3. From those sampled lake years find the sequence of previous stocking years to that lake that could have fish in the lake at the time of sampling. Search back to oldest age observed or the endage.

  Sampled <- maxxages %>%
    dplyr::filter(!is.na(year)) %>%
    dplyr::group_by(WBID, species_code, year, Sample_event, event_start_dt, max) %>%
    dplyr::mutate(YearSeq = purrr::map(year, ~ seq((. - max), .))) %>%
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
        (release_year == sample_year & rel_Date <= event_start_dt)
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
      avg_rel_date = as.Date(mean(rel_Date)),
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
      avg_rel_date = as.Date(mean(avg_rel_date)),
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
      Total_Quantity = round(sum(Quantity), -2),
      .groups = "drop"
    ) %>%
    dplyr::arrange(WBID, release_year)

  #Calculate stocking frequency
  Stable_base <- Stable_base %>%
    dplyr::group_by(WBID) %>%
    dplyr::mutate(
      gap = release_year - lag(release_year)
    ) %>%
    dplyr::mutate(
      Frequency = as.integer(stats::median(gap, na.rm = TRUE))
    ) %>%
    dplyr::ungroup()

  #Create a prescription key
  Stable_base <- Stable_base %>%
    dplyr::mutate(
      presc_key = paste(Strain_rel, Geno_rel, Total_Quantity, Frequency, sep = "|")
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
