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
                        DP2R::Releases2R()
                        DP2R:::CLEANreleases()
                        }
if(!exists("Releases")){stop("Need to start with a data load from SLD (i.e. Data_source = TRUE) at least once to start")}

######################
  #A section to try and clean up if biologists record "NONE" or leave NULL for mark_code when not having a mark is an identifier.

#Number of unique clips potentially at large in a given year if the fish is not aged
Clipsrel = Link_rel%>%
  dplyr::group_by(WBID, year, species_code)%>%
  dplyr::summarise(Nclips = sum(!is.na(mark_code)&is.na(age)))%>%
  dplyr::filter(Nclips>0)

#Add "NONE" to aged (or unaged?) fish where there are multiple clips at large but this group is not clipped (so unique)
#Link_rel = Link_rel%>%
#  dplyr::mutate(mark_code = ifelse((is.na(mark_code)& !is.na(sby_rel) & !stringr::str_detect(sby_rel, ",")&(interaction(WBID,year,species_code)%in%interaction(Clipsrel$WBID, Clipsrel$year,Clipsrel$species_code))),"NONE",mark_code))

#Update for above when unaged releases area unique product over many years. Also inconsistent use of NA and "NONE" in database, so can duplicate to cover both
Link_none = Link_rel%>%
  dplyr::mutate(mark_code = ifelse((is.na(mark_code)&
                                      !is.na(sby_rel)&
                                      !if_any(c(Strain_rel, Geno_rel, LS_rel),
                                              ~ stringr::str_detect(.x, ","))&
                                      (interaction(WBID,year,species_code)%in%interaction(Clipsrel$WBID, Clipsrel$year,Clipsrel$species_code))),"NONE",mark_code))

Link_rel = rbind(Link_rel, Link_none)%>%unique()


#Clean up NOREC and UNK mark_code entries in lake-years where no clips should be present anyways.
vwIndividualFish = vwIndividualFish%>%
  dplyr::mutate(mark_code = ifelse((mark_code %in% c("UNK","NONE")&!(interaction(WBID,year,species_code)%in%interaction(Clipsrel$WBID, Clipsrel$year,Clipsrel$species_code))),NA,mark_code))

##??POTENTIALLY ADD IN SECTION TO ADD NONE TO INDIVS IF LEFT S NA?


#########################

#Join potential stocking events to vwIndividualFish to check for natural recruits in stocked species
#This joins by age, so ageing errors can lead to false possible Natural Recruit
NR = dplyr::left_join(vwIndividualFish,Link_rel, by = c(names(Link_rel)[1:5]))%>%
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
  dplyr::group_by(region, gazetted_name, WBID,year,species_code)%>%
  dplyr::summarise(N = dplyr::n(),
                   Nnr = sum(Poss_NR,na.rm = T),
                   pNR = round(Nnr/N, 2),
                   MeanFL = round(mean(length_mm, na.rm = T)),
                   MaxFL = max(length_mm, na.rm = T))

#Specific lake-species combos where at least 30% of the fish could potentially be natural recruits
NR_lakes = NR_sum%>%
  dplyr::group_by(region, gazetted_name, WBID,species_code)%>%
  dplyr::reframe(pNR = round((mean(pNR)+pNR[year == max(year)])/2,2), N = sum(N))%>%
  dplyr::filter(pNR>0.29)%>%
  dplyr::ungroup()

#Now that established which lakes have a reasonable proportion of potential NR fish species, all non-clipped fish from those lake-species groups need to be treated as suspect.
Biological = dplyr::left_join(vwIndividualFish,NR[,c("individual_fish_id","Poss_NR")], by = "individual_fish_id")%>%
  dplyr::mutate(Poss_NR = ifelse((is.na(mark_code)&(interaction(WBID,species_code)%in%interaction(NR_lakes$WBID,NR_lakes$species_code))),1,Poss_NR))


##DIFFERENT strategy, first link everything without using age
#Perform 'Stocked_age' test to see if the entered age is within possible released ages.
Link_rel_noage = Link_rel%>%dplyr::filter(is.na(age))%>%dplyr::select(-c(age))
Biopossible <- dplyr::left_join(Biological,Link_rel_noage, by = c("WBID","species_code","year","mark_code"))%>%
  dplyr::rowwise()%>%
  dplyr::mutate(Stocked_age = age%in%as.integer(strsplit(Poss_Age, ",")[[1]]))%>%
  dplyr::ungroup()


#If no, then leave the possibilities as is (probably an ageing error or natural recruit or not stocked).
Bioambig = Biopossible[!Biopossible$Stocked_age,]

#If yes, then re-link releases to biological using age or brood year as a linking variable.
Bioaged = Biopossible[Biopossible$Stocked_age,]%>%dplyr::select(-c(sby_rel:Stable_yrs))
Bioaged <- dplyr::left_join(Bioaged,Link_rel[!is.na(Link_rel$age),], by = c("WBID","species_code","year","Lk_yr","mark_code","age"))

#Bring back together and remove temporary files.
Biological = rbind(Bioambig,Bioaged)

rm(Clipsrel, NR, NR_lakes, Biopossible, Bioambig, Bioaged,  Link_rel_noage)

#Replace un-observable values associated with stocking events and based on clips.
replace_uni = function(var,uni, Poss_NR){
  class = class(var)
  var = as.character(var)
  var = dplyr::case_when(!is.na(Poss_NR)|is.na(uni) ~ var,
                         grepl(",",uni) ~ var,
                         TRUE ~ uni)
  class(var)<-class#in case replacing a numeric variable
  return(var)
}

Biological = Biological%>%
  dplyr::mutate(sby_code = accepted_brood_year)%>%#add a sby_code to alter if unique stocking ages possible and match to release names
  dplyr::mutate(
    Strain = replace_uni(strain, Strain_rel, Poss_NR),
    Genotype = replace_uni(ploidy, Geno_rel, Poss_NR),
    sby_code = replace_uni(sby_code, sby_rel, Poss_NR),
    age = dplyr::case_when(
      !is.na(sby_code)&is.na(age) ~ as.numeric(sby2age(species_code, sby_code, year)),
      TRUE ~ as.numeric(age)  # Ensure age is numeric
    ),
    Dec.Age = round(.data$age+(lubridate::decimal_date(as.Date(.data$date_assessed)) - lubridate::year(as.Date(.data$date_assessed))),2)
  )

#IDENTIFY OUTLIERS
Biological <- Biological%>%
  dplyr::mutate(outlier = ifelse(is.na(.data$length_mm)|.data$length_mm>900|.data$species_code%in%c("UNK","NFC","NFP"),1,
                                 ifelse(!is.na(.data$weight_g)&.data$species_code %in% c('ACT','CT','WCT','CRS','RBCT','RB','KO','EB','DV','BT','GB','TR','ST') &
                                          (!(.data$length_mm %in% c(60:1000)) | 0.65 > .data$condition_factor | 2.25 < .data$condition_factor),1,0)
  ))


#Summarize lake species composition information
Cap_Spp = vwCollectCount%>%
  dplyr::mutate(year = lubridate::year(as.Date(end_dt)))%>%
  dplyr::select( WBID, year, species_code)%>%
  unique()

Ind_Spp = vwIndividualFish%>%dplyr::select(WBID, year, species_code)%>%unique()

Lake_Spp = dplyr::full_join(Cap_Spp,Ind_Spp)

Lake_Spp = dplyr::left_join(Lake_Spp, Species, by = "species_code")%>%
  dplyr::select(WBID:stocked_species, subfamily)%>%
  dplyr::group_by(WBID, year)%>%
  dplyr::mutate(All_spp = paste(sort(unique(species_code)), collapse = ','),
                Spp_class = paste(sort(unique(subfamily)), collapse = ','),
                Non_salm = paste(sort(unique(species_code[.data$subfamily!="Salmoninae"])), collapse = ','))%>%dplyr::ungroup()


Biological<<-Biological
NR_sum<<-NR_sum
Lake_Spp<<-Lake_Spp

Collections = vwFishCollection%>%
  dplyr::select(region, WBID, gazetted_name, end_year, end_dt, method, net_angler_id, sample_design_code, gill_net_position_code, habitat_code, fishing_hours, comment, angling_rods,angling_method_code, terminal_gear_code, lake_lat,lake_long, shore_lat, shore_long)



#Not sur eif it is necessary to reduce to sampled only anymore....
#if(Sampled_only==TRUE){
#Assessments<<- Assessments[Assessments$Assessment_Key%in%Biological$Assessment_Key,]
#Lakes<<-Lakes[Lakes$WBID%in%Biological$WBID,]
#Nets<<-Nets[Nets$Assessment_Key%in%Biological$Assessment_Key,]
#Releases<<-Rel_sampled
#}

#if(Sampled_only==FALSE){
#  Assessments<<- Assessments
#  Lakes<<-Lakes
#  Nets<<-Nets
#  Releases<<-Releases
#}


}
