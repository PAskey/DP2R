#' A function brought over from the SPDT package and updated to work with DataPond. Created to load, clean and standardize SLD data for SPDT analysis.
#' Function only usable by FFSBC staff who have a direct or vpn connection to SLD.
#'
#'
#' This is the second data filtering and cleaning process after SLD2R() to reduce down to biological data that can be tied to releases.
#' Takes SLD2R() data and uses mark_code information to tie fish back to stocking event, age, strain, genotype where possible.
#' Make sure your VPN is running, so that the database can be accessed by the function.
#' The Biological record count remains the same, but includes information about the potential stocking event(s) tied to each fish.
#' In cases where clips are unique, then fields for age, strain, genotype are updated if black. Otherwise a list of possibilities can be found
#' in "clipAges, clipStrains, clipGenos.
#' All other data tables only include data that can be linked to the Biological data, either same Assessment_Key, or rel_id.
#' Any data Tables you have open as Assessments, Nets, Lakes, Biological, Releases will be replaced with versions from this function.
#' Lookup tables for integer ages and strain codes are included as part of package and can by called as Ages, Strain_code_LU
#' Ultimately, as upload filters and cleaning are improved in the main database, this function will become obsolete.
#'
#' @title linkClips
#' @name linkClips
#' @keywords DP2R; SPDT; clips
#' @export
#' @param Sampled_only a logical TRUE/FALSE indicating whether to reduce data tables to records associated with a sampling event with Biological data.
#' If true (default) then Assessments, Nets, Lakes and Releases tables are all reduced to Lake-years that can cross reference to Biological records.
#' @param Data_source a TRUE FALSE value to indicate whether to load data form the SLD, or just use data tables in the Environment.
#' @examples
#' #' Must be connected to VPN if working remotely
#'
#' linkClips()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data



#_______________________________________________________________________________
#Open channel to SLD and download data
linkClips <- function(Sampled_only = TRUE, Data_source = TRUE){

if(Data_source == TRUE){DP2R::DP2R()
                        DP2R::link_releases()
                        #DP2R::Releases2R()
                        #DP2R:::CLEANreleases()
                        }
if(!exists("Releases")){stop("Need to start with a data load from SLD (i.e. Data_source = TRUE) at least once to start")}

######################
  #A section to try and clean up if biologists record "NONE" or leave NULL for mark_code when not having a mark is an identifier.

#Number of unique clips potentially at large in a given year if the fish is not aged
Clipsrel = Link_rel%>%
  dplyr::group_by(WBID, sample_year, Sample_event, species_code)%>%
  dplyr::summarise(Nclips = sum(!is.na(mark_code)&is.na(age)), .groups = "drop")%>%
  dplyr::filter(Nclips>0)

#Add "NONE" to aged (or unaged?) fish where there are multiple clips at large but this group is not clipped (so unique)
#Unaged releases can be a unique product over many years.
#Also inconsistent use of NA and "NONE" in database, so need to duplicate release rows with NA as NONE to cover both
#Old attempt excluded any cases where Strain, Geno or Lifestage was not unique, but the other 2 might be, so updated.

Link_none = Link_rel%>%
  dplyr::filter(is.na(mark_code))%>%
  dplyr::mutate(mark_code = "NONE")


Link_rel = rbind(Link_rel, Link_none)%>%unique()


#Clean up NOREC and UNK mark_code entries in lake-years where no clips should be present anyways.
vwIndividualFish = vwIndividualFish%>%
  dplyr::mutate(mark_code = ifelse(
    (mark_code %in% c("UNK","NONE")&!(interaction(WBID,year,species_code)%in%interaction(Clipsrel$WBID, Clipsrel$sample_year,Clipsrel$species_code))),NA,mark_code))

vwIndividualFish <<- vwIndividualFish#Persist cleaned mark_code back to the global copy, consistent with the other tables updated below


##??POTENTIALLY ADD IN SECTION TO ADD NONE TO INDIVS IF LEFT AS NA?


#########################

#Join potential stocking events to vwIndividualFish to check for natural recruits in stocked species
join_cols = c("region_code","WBID","locale_name","year"="sample_year","Sample_event","species_code","age", "mark_code")
info_cols = c("sby_rel","AF","Sterile")
#This joins by age, so ageing errors can lead to false possible Natural Recruit
NR = dplyr::left_join(vwIndividualFish,
                      Link_rel[, names(Link_rel) %in% c(join_cols,info_cols)],
                      by = join_cols)%>%
  dplyr::mutate(Poss_NR = dplyr::case_when(
    is.na(mark_code)&is.na(sby_rel)~1,
    is.na(mark_code)&AF&species_code!="KO"&sex=="M"~1,
    is.na(mark_code)&Sterile&species_code!="KO"&maturity%in%c("M","MT","SP","SB","MR","R")~1,
    is.na(mark_code)&Sterile&sex == "F"&species_code=="KO"&maturity%in%c("M","MT","SP","SB","MR","R")~1,
    TRUE~NA_real_
  ))

#Create a NRT probability for each lake-species. Weight most recent assessment more heavily.
#Find Lake-years where at least 20 stocked species were sampled
SampleN = vwIndividualFish%>%
  dplyr::filter(species_code%in%c("CT","EB","KO","RB", "WCT"))%>%
  dplyr::count(WBID, year)%>%dplyr::filter(n>=20)

#Search for natural recruits in stocked lakes with stocked species by year
NR_sum = NR%>%
  dplyr::filter(interaction(WBID, year)%in%interaction(SampleN$WBID, SampleN$year), species_code%in%c("CT","EB","KO","RB", "WCT"))%>%
  dplyr::group_by(region_code, locale_name, WBID,year,species_code)%>%
  dplyr::summarise(N = dplyr::n(),
                   Nnr = sum(Poss_NR,na.rm = T),
                   pNR = round(Nnr/N, 2),
                   MeanFL = round(mean(length_mm, na.rm = T)),
                   MaxFL = max(length_mm, na.rm = T),
                   .groups = "drop")

#Specific lake-species combos where at least 30% of the fish could potentially be natural recruits
NR_lakes = NR_sum%>%
  dplyr::group_by(region_code, locale_name, WBID,species_code)%>%
  dplyr::reframe(pNR = round((mean(pNR)+pNR[year == max(year)])/2,2), N = sum(N))%>%
  dplyr::filter(pNR>0.29)%>%
  dplyr::ungroup()

#Now that established which lakes have a reasonable proportion of potential NR fish species, all non-clipped fish from those lake-species groups need to be treated as suspect.
Biological = dplyr::left_join(vwIndividualFish,dplyr::distinct(NR[,c("individual_fish_id","Poss_NR")]), by = "individual_fish_id")%>%
  dplyr::mutate(Poss_NR = ifelse((is.na(mark_code)&
                (interaction(WBID,species_code)%in%interaction(NR_lakes$WBID,NR_lakes$species_code))),1,Poss_NR),
                Lk_yr = paste0(WBID,"_",year))


##Now link stocking prescriptions to captured fish, first link everything without using age
#Select columns
info_cols = c(
  "species_code",
  "mark_code",
  "sby_rel",
  "Strain_rel",
  "Geno_rel",
  "LS_rel",
  "AF",
  "Sterile",
  "wt_rel",
  "N_ha_rel",
  "avg_rel_date",
  "Poss_Age"
)

#Perform 'Stocked_age' test to see if the entered age is within possible released ages.
Link_rel_noage = Link_rel%>%
  dplyr::filter(is.na(age))%>%
  dplyr::select(all_of(c("Sample_event",info_cols)))

#Is the observe age within possible ages Stocked_age == TRUE
#IS the obserevd age within +/-1 of the observed age = potentail ageing error
Biopossible <- dplyr::left_join(Biological,Link_rel_noage,
                                by = c("Sample_event","species_code","mark_code"))%>%
  #Vectorized replacement for the old rowwise() + per-row strsplit block, which
  #was the slowest step on the full biological table. Split Poss_Age once into a
  #list column, then iterate in compiled code via purrr::map2_lgl. NA handling is
  #identical to the original: age %in% NA -> FALSE, and any(abs(age - NA) <= 1) -> NA.
  dplyr::mutate(
    .poss_age = strsplit(Poss_Age, ",", fixed = TRUE),
    Stocked_age = purrr::map2_lgl(age, .poss_age, ~ .x %in% as.integer(.y)),
    Stock_age_close = purrr::map2_lgl(age, .poss_age, ~ any(abs(.x - as.integer(.y)) <= 1))
    )%>%
  dplyr::select(-.poss_age)


#If no, then leave the sby_rel possibilities as is (probably an ageing error or natural recruit or not stocked).
#However, remove everything else as clearly a natural recruit or data/clip error
Bioambig = Biopossible[!Biopossible$Stocked_age,]%>%
  dplyr::mutate(dplyr::across(dplyr::all_of(c("Strain_rel","Geno_rel","LS_rel","AF","Sterile","wt_rel","N_ha_rel","avg_rel_date")), ~ dplyr::if_else(Stock_age_close==FALSE, NA, .x)))


#Any fish that does not match a stocking age and doesn't have a clip is Poss_NR
Bioambig$Poss_NR[!is.na(Bioambig$age)&(is.na(Bioambig$mark_code)|Bioambig$mark_code=="NONE")]<-1

#If yes, then re-link releases to biological using age or brood year as a linking variable.
Bioaged = Biopossible[Biopossible$Stocked_age,]%>%dplyr::select(-dplyr::all_of(c("sby_rel","Strain_rel","Geno_rel","LS_rel","AF","Sterile","wt_rel","N_ha_rel","avg_rel_date","Poss_Age")))
Bioaged <- dplyr::left_join(Bioaged,Link_rel[!is.na(Link_rel$age),]%>%dplyr::select(all_of(c("Sample_event","age",info_cols))),
                            by = c("species_code","Sample_event","mark_code","age"))

#Bring back together and remove temporary files.
Biological = rbind(Bioambig,Bioaged)

rm(Clipsrel, NR, NR_lakes, Biopossible, Bioambig, Bioaged,  Link_rel_noage)

#Replace un-observable values associated with stocking events and based on clips.
replace_uni = function(var,uni, Poss_NR){
  target_class = class(var)[1]
  var = dplyr::case_when(!is.na(Poss_NR)|is.na(uni) ~ as.character(var),
                         grepl(",",uni) ~ as.character(var),
                         TRUE ~ uni)
  #Restore the ORIGINAL storage type. The old code did `class(var) <- class`,
  #which only set the class attribute and left numeric columns (e.g. sby_code)
  #stored as text, so downstream arithmetic in sby2age() was acting on strings.
  if (target_class %in% c("integer")) {
    var = suppressWarnings(as.integer(var))
  } else if (target_class %in% c("numeric","double")) {
    var = suppressWarnings(as.numeric(var))
  }
  #character (and any other class) is returned unchanged
  return(var)
}

Biological = Biological%>%
  dplyr::mutate(sby_code = brood_year)%>%#add a sby_code to alter if unique stocking ages possible and match to release names
  dplyr::mutate(
    Strain = replace_uni(strain, Strain_rel, Poss_NR),
    Genotype = replace_uni(ploidy, Geno_rel, Poss_NR),
    sby_code = replace_uni(sby_code, sby_rel, Poss_NR),
    age = dplyr::case_when(
      !is.na(sby_code)&is.na(age) ~ as.numeric(DP2R::sby2age(species_code, sby_code, year)),
      TRUE ~ as.numeric(age)  # Ensure age is numeric
    ),
    Dec.Age = round(.data$age+(lubridate::decimal_date(as.Date(.data$date_assessed)) - lubridate::year(as.Date(.data$date_assessed))),2)
  )

#IDENTIFY OUTLIERS
Biological <- Biological%>%
  dplyr::mutate(outlier = ifelse(is.na(.data$length_mm)|.data$length_mm>900,1,
                                 ifelse(!is.na(.data$weight_g)&.data$species_code %in% c('ACT','CT','CCT','WCT','CRS','RBCT','RB','KO','EB','CO','DV','BT','GB','TR','ST') &
                                          (!(.data$length_mm %in% c(60:1000)) | 0.65 > .data$condition_factor | 2.25 < .data$condition_factor),1,0)
  ))


#Summarize lake species presence from ALL available data sources. Each source
#becomes per-observation (WBID, species_code, year) rows tagged with:
#  assess -> TRUE if the species was actually detected (collection counts,
#            individual fish, FISS); FALSE for stocking releases only.
#  source -> "DataPond" for any DataPond table (captures, individual fish,
#            releases); "FISS" for FISS observations.
#Then reduce to one row per WBID x species:
#  first_obs   = earliest year across ANY source (species-specific)
#  last_obs    = latest   year across ANY source (species-specific)
#  last_source = "FISS" if that latest year came only from FISS, else "DataPond"
#  last_assess = latest ASSESSMENT year for the whole LAKE (max non-release year
#                across ALL species in the WBID) -- same value for every species
#                in a lake. Comparing a species' last_obs to the lake's
#                last_assess shows whether it went undetected in later sampling.

#1. Fish in collection counts (observation year from end_dt)
Cap_Spp = vwCollectCount %>%
  dplyr::transmute(WBID, species_code,
                   year = as.integer(lubridate::year(as.Date(end_dt))),
                   assess = TRUE, source = "DataPond")

#2. Individual fish records (year already present)
Ind_Spp = vwIndividualFish %>%
  dplyr::transmute(WBID, species_code, year = as.integer(year),
                   assess = TRUE, source = "DataPond")

#3. Stocking releases (release year) -- DataPond, but NOT an assessment.
Rel_Spp = Releases %>%
  dplyr::transmute(WBID, species_code, year = as.integer(release_year),
                   assess = FALSE, source = "DataPond")

#4. FISS known observations (raw per-observation table: WBID, species_code, year,
#activity, agency_name) -- assessment observations. Guarded so linkClips still
#runs if FISS_Spp is unavailable, but WARN rather than silently skip (usual cause:
#a stale DP2R namespace -- data/FISS_Spp.rda exists but the package was not
#rebuilt/reloaded).
FISS_rows = tryCatch(
  DP2R::FISS_Spp %>% dplyr::transmute(WBID, species_code, year = as.integer(year),
                                      assess = TRUE, source = "FISS"),
  error = function(e) {
    warning("linkClips(): FISS_Spp not found (", conditionMessage(e),
            "). Lake_Spp built WITHOUT FISS. If data/FISS_Spp.rda exists, rebuild/",
            "reload DP2R (devtools::load_all() or Install & Restart) so DP2R::FISS_Spp ",
            "is in the namespace, as DP2R::Species is.", call. = FALSE)
    Cap_Spp[0, ]
  }
)

all_rows = dplyr::bind_rows(Cap_Spp, Ind_Spp, Rel_Spp, FISS_rows) %>%
  dplyr::filter(!is.na(WBID), !is.na(species_code))

#Lake-level last assessment year: latest non-release (assessment) year anywhere
#in the lake, across all species. One value per WBID, joined onto every species.
lake_assess = all_rows %>%
  dplyr::filter(assess, !is.na(year)) %>%
  dplyr::group_by(WBID) %>%
  dplyr::summarise(last_assess = as.integer(max(year)), .groups = "drop")

#Species-level summary. dp_last / fiss_last are helpers to decide last_source:
#FISS only "wins" the latest record if it is strictly later than any DataPond year
#(so a tie, or any DataPond record at the latest year, reads as DataPond).
Lake_Spp = all_rows %>%
  dplyr::group_by(WBID, species_code) %>%
  dplyr::summarise(
    first_obs = suppressWarnings(min(year, na.rm = TRUE)),
    last_obs  = suppressWarnings(max(year, na.rm = TRUE)),
    dp_last   = suppressWarnings(max(year[source == "DataPond"], na.rm = TRUE)),
    fiss_last = suppressWarnings(max(year[source == "FISS"],     na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    first_obs   = dplyr::if_else(is.finite(first_obs), as.integer(first_obs), NA_integer_),
    last_obs    = dplyr::if_else(is.finite(last_obs),  as.integer(last_obs),  NA_integer_),
    last_source = dplyr::if_else(
      is.finite(fiss_last) & (!is.finite(dp_last) | fiss_last > dp_last),
      "FISS", "DataPond"
    )
  ) %>%
  dplyr::select(-dp_last, -fiss_last) %>%
  dplyr::left_join(lake_assess, by = "WBID")

#Attach species taxonomy (subfamily etc.) as before; downstream code (e.g.
#SPDTdata()) relies on WBID, species_code and subfamily being present.
Lake_Spp = dplyr::left_join(Lake_Spp, DP2R::Species, by = "species_code") %>%
  dplyr::relocate(first_obs, last_obs, last_assess, last_source, .after = species_code)

Biological <- DP2R::add_selectivity(Biological)%>%
                dplyr::mutate(NetX = 1/select)

vwFishCollection<<- vwFishCollection %>%
  dplyr::left_join(DP2R::lake_names[,c("WBID","locale_name")], by = "WBID")%>%
  dplyr::relocate(locale_name, .after = WBID)

vwCollectCount<<- vwCollectCount %>%
  dplyr::left_join(DP2R::lake_names[,c("WBID","locale_name")], by = "WBID")%>%
  dplyr::relocate(locale_name, .after = WBID)

vwWaterbody<<- vwWaterbody %>%
  dplyr::relocate(locale_name, .after = WBID)


Biological<<-Biological
NR_sum<<-NR_sum
Lake_Spp<<-Lake_Spp

}
