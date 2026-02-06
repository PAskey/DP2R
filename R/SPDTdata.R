#' A function transferred from SPDT package to filter and format DataPond data to cases with clipped experimental stocking groups.
#' Function only usable by FFSBC staff who have a direct connection to DataPond.
#'
#' This function returns the data tables from linkClips, plus new refined data sets.
#' "idf" is the individual level data frame, for all clipped fish meeting your filtering selections.
#' "gdf" is the grouped data frame, for all clipped groups meeting your filtering selections.
#' Our database will not likely ever have filtered summaries to this level, so this function will be used and updated in to the future.
#'
#'Several new columns are present. Anything prefixed by "NetX" means that variable has been expanded to account for gillnet selectivity.
#'SAR if for size-at-release in grams, and SAR_cat has categorized those release sizes.
#'The code creating categories for the SAR_cat has bigger categories as the size increases. See ?SAR_cat
#'
#' @title SPDTdata
#' @name SPDTdata
#' @keywords SPDT
#' @export
#' @param Spp an optional character string or character vector for BC species code (e.g. "RB" or c("KO", "EB"), etc.). This will filter data to only that species.
#' @param Contrast a required character string describing the experimental contrast, which must be a field in the SPDTdata (e.g. "species_code", "Strain_rel", "SAR_cat", "Geno_rel" are the 3 possibilities now).
#' Entering a value for contrast will filter to lake years that had fish present from a co-stocking event of groups varying in your contrast variable.
#' @param drop_controls an optional character string or vector to drop covariates from the potenital Contrast list that would be used as controls when filtering and grouping data by the selected Contrast. Default is the full list of potential contrasts are usd as controls (i.e. none are dropped). Using all controls may be restrictive for some contrasts (eg. Comparing species it would be difficult to control for size-at-release SAR_cat because they are released at different stage-sizes)
#' @param Strains an optional character string or character vector describing the strain code (SPDTdata format e.g. "RB" for Rainbow Trout) for source population. This will filter to only those strains listed
#' @param Genotypes an optional character string or character vector to filter data to specific genotypes (e.g. 2n or AF3n)
#' @param filters a vector of lake-years returned from the SPDTfilter() function. SPDTfilter() allows for filtering to various non-biological aspects to the data, lakes, years, regions, et.c See?SPDTfilter()
#' @param Data_source a TRUE FALSE value to indicate whether to load data form the SLD, or just use data tables in the Environment.
#' @examples
#' #Must be connected to VPN if working remotely
#'
#' #Download all data with clipped fish
#' SPDTdata()
#'
#' #Download only KO data
#' SPDTdata(Spp = "KO")
#'
#' #Download any data from RB, of either HF, CL or BW strain that were stocked as a strain comparison
#' SPDTdata(Spp = "RB", Strains = c("HF", "CL", "BW"), Contrast = "Strain")
#'
#' #Download all data from lake years that had a size-at-release comparison for RB
#' SPDTdata(Spp = "RB", Contrast = "SAR_cat")
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


SPDTdata <- function(Spp = NULL, Contrast = c("species_code", "Strain_rel", "SAR_cat", "Geno_rel"), drop_controls = NULL, Strains = NULL, Genotypes = NULL, filters = NULL, Data_source = TRUE){

  if (missing(Contrast)) {
    Contrast <- NULL
  } else {
    Contrast <- match.arg(Contrast)
  }

  if(is.null(Contrast)){stop("Must define a 'Contrast' for SPDTdata() function, see ?SPDTdata, for all data use SLD2R()  or linkClips() instead")}

  if(Data_source == TRUE){linkClips()}

  if(Data_source ==FALSE&(!exists("Biological")|!exists("Link_rel"))){stop("Need to start with a data load from SLD (i.e. Data_source = TRUE) at least once to start")}

#Create list of covariates, and those to be controlled in data summaries
  Covariates = c("species_code","Geno_rel","Strain_rel","SAR_cat")

  if(!is.null(drop_controls)){Covariates = Covariates[!Covariates %in% drop_controls]}

 #Always drop strain if comparing species
   if(Contrast == "species_code"){Covariates = Covariates[!Covariates == 'Strain_rel']}

  controls = dplyr::setdiff(Covariates, Contrast)

  warning(paste0("Note, the following controls were used in the data filter for the contrast:",
                  paste(controls, collapse = ", ")))

#Initial filters. Keep Clip == "NONE" because experimental fish in Yellow (KO) and maybe elsewhere were non-clips.

 #The Biological Table from the SLD through linkClips() is always the same regardless of other parameters, and be used as a raw data check.
  #Alternatively if Data_source is set to "FALSE" then analyst uses a Biological Table that is loaded in the RStudio environment, which could be from the SLD or a spreadsheet, etc.
idf = Biological
clipsdf = Link_rel#clipsum

if (!is.null(filters)) {
idf = dplyr::filter(idf, Lk_yr %in% filters)
clipsdf = dplyr::filter(clipsdf, Lk_yr %in% filters)
}

if (!is.null(Spp)) {
  idf = subset(idf, species_code %in% Spp)
  clipsdf = subset(clipsdf, species_code %in% Spp)
}

if (!is.null(Strains)) {
  idf = subset(idf, Strain_rel %in% Strains)
  clipsdf = subset(clipsdf, Strain_rel %in% Strains)
}

if (!is.null(Genotypes)) {
  idf = subset(idf, Geno_rel %in% Genotypes)
  clipsdf = subset(clipsdf, Geno_rel %in% Genotypes)
}

#Categorize release size to facilitate finding true contrasts and analyzing
#Code below create categories that increase in width as size increases.
idf <- idf%>%
  dplyr::mutate(SAR_cat = SAR_cat(wt_rel),
                Season = metR::season(date_assessed),
                #Season = dplyr::recode_factor(Season, MAM = "Spring", JJA = "Summer", SON = "Fall", DJF = "Winter"),
                NetX = ifelse(.data$method == "GN"&.data$length_mm>75&.data$length_mm<650&.data$species_code %in% c("CT","EB","KO","RB","WCT"),
                              1/SPDT::RICselect(FLengths_mm = .data$length_mm),1))%>%
  tidyr::replace_na(list(NetX = 1))


#Let's create a grouped data set
#Summarize mean values for growth and tallies for numbers, etc. EXCLUDES OUTLIERS
gdf <- idf%>%
  dplyr::group_by(.data$locale_name, .data$WBID, .data$year, .data$Lk_yr, .data$Season, .data$method, .data$species_code, .data$Strain_rel,.data$Geno_rel, .data$Poss_Age, .data$age, .data$sby_code, .data$mark_code, .data$N_ha_rel, .data$wt_rel, .data$SAR_cat)%>%
  dplyr::summarize( #Dec.Age = mean(.data$Dec.Age,na.rm = TRUE),
                    mean_FL = mean(.data$length_mm[.data$outlier%in%c(0,NA)], na.rm = TRUE),
                    sd_FL = sd(.data$length_mm[.data$outlier%in%c(0,NA)], na.rm = TRUE),
                    mean_wt = mean(weight_g[.data$outlier%in%c(0,NA)], na.rm = TRUE),
                    sd_wt = sd(weight_g[.data$outlier%in%c(0,NA)], na.rm = TRUE),
                    mean_K = mean(.data$condition_factor[.data$outlier%in%c(0,NA)], na.rm = TRUE),
                    N = dplyr::n(),
                    N_outliers = sum(.data$outlier, na.rm = TRUE),
                    NetX_FL = stats::weighted.mean(.data$length_mm[.data$outlier%in%c(0,NA)], .data$NetX[.data$outlier%in%c(0,NA)], na.rm = TRUE),
                    NetX_wt = stats::weighted.mean(.data$weight_g[.data$outlier%in%c(0,NA)], .data$NetX[.data$outlier%in%c(0,NA)], na.rm = TRUE),
                    NetXN = sum(NetX),
                    p_mat = sum(.data$maturity != 'IM'& .data$maturity != 'UNK', na.rm = TRUE)/sum(.data$maturity != 'UNK', na.rm = TRUE)
                  )%>%
  dplyr::ungroup()


#The only reason for this next section that merges clipsdf
#Is to track 0 counts for specific clips during sampling.
##MAYBE A BETTER WAY IS TO JSUT DO A LEFT JOIN OF LINK CLIPS AND IDF_GDF?


#Using clipsum instead of Xnew should keep release date and sample date and better cross reference when multiple release ids for one release group.
clipsdf<-clipsdf%>%
  dplyr::filter(!is.na(age)&sby_rel>0)%>%#Cases of a single defined cohort.
  dplyr::mutate(sby_rel = as.integer(sby_rel))

#Vector of unique sampling events by lake-year, season and method
#Can't add avg_sampling date within gdf, because will be different for each strain, age, etc.
idf$date_assessed = as.POSIXct(idf$date_assessed)
uni_events = idf%>%
  dplyr::group_by(Lk_yr, Season, method)%>%
  dplyr::summarize(avg_sample_date = round(mean(.data$date_assessed,na.rm = TRUE), units = "days"))%>%
  dplyr::ungroup()

#Gives same result with left or right join
#Join this with clipsdf, so all releases that should appear in a sampling event are tracked.
clipsdf = dplyr::left_join(uni_events[,c(1:3)], clipsdf, by = 'Lk_yr')%>%#Add sample date in below
          dplyr::filter(!is.na(species_code))#Remove cases that did not match a stocking event.
###################################################################################################################
gdf = dplyr::full_join(gdf, clipsdf[,c("WBID", "Lk_yr", "year", "Season", "method", "age", "species_code", "Strain_rel","Geno_rel", "sby_rel", "mark_code", "N_ha_rel","avg_rel_date", "wt_rel", "LS_rel")],
                       by = c("WBID", "Lk_yr", "year", "Season", "method", "age", "species_code", "Strain_rel","Geno_rel", "sby_code"="sby_rel", "mark_code", "N_ha_rel","wt_rel"))%>%
  dplyr::filter(Lk_yr%in%idf$Lk_yr)%>%#mark_code != "", Pulled this out to keep species comparisons
  dplyr::filter(dplyr::case_when(Contrast != "species_code"~ mark_code != "", TRUE ~ !is.na(N_ha_rel) ))%>%#, TRUE ~ mark_code %in% unique(mark_code)
  dplyr::mutate(N = replace(N, is.na(N), 0),
                NetXN = replace(NetXN, is.na(NetXN), 0),
                #avg_rel_date = pmax(avg_rel_date.x, avg_rel_date.y, na.rm = TRUE),
                SAR_cat = SAR_cat(wt_rel))



  # I guess we don't join by avg_rel_date, because does not appear in one or the other? Perhaps 0 obs cases.
#####################################################################################################################

#Have to add in average sample data at this point, otherwise it is NA for all cases of non-clips, etc.
gdf = dplyr::left_join(gdf, uni_events, by = c("Lk_yr", "Season", "method"))%>%
  dplyr::mutate(
    Dec.Age = round(.data$age+(lubridate::decimal_date(.data$avg_sample_date) - year),2),
    Delta_t = as.numeric(difftime(avg_sample_date,avg_rel_date, units = "days")))



exps <- gdf%>%
  dplyr::filter(!grepl(",",get(Contrast)))%>%#discount groups that included multiple levels within contrast (they are always separated by commas)
  dplyr::group_by(Lk_yr, age, !!!rlang::syms(controls))%>%
  dplyr::summarize(Ncontrasts = length(unique(na.omit(get(Contrast)))), Nclips = length(unique(na.omit(mark_code))))%>%
  dplyr::filter((Nclips>=Ncontrasts&Ncontrasts>1)|(Contrast == "species_code"&Ncontrasts>1))%>%
  droplevels()



idf<-subset(idf, Lk_yr%in%exps$Lk_yr)%>%dplyr::filter(!grepl(",",get(Contrast)),!is.na(get(Contrast)), get(Contrast)!="UNK")
gdf<-subset(gdf, Lk_yr%in%exps$Lk_yr)%>%dplyr::filter(!grepl(",",get(Contrast)),!is.na(get(Contrast)), get(Contrast)!="UNK")

######################################################################################################
##Effort section


#Let's try an effort table for standard gillnet data only for now. Data looks terrible quality.
#Assume 7 panels and overnight when not recorded but SGN indicated
#Nets = Nets%>%dplyr::filter(sample_design_code == "SGN")%>%
#  tidyr::replace_na(list(no_net_panels = 7, overnight_yn= "Y"))

#Capitalization
#Nets$overnight_yn = stringr::str_to_upper(Nets$overnight_yn)

#Net_effort = Nets%>%
#              dplyr::filter(Lk_yr %in% gdf$Lk_yr, method == "GN", sample_design_code == "SGN")%>%#This should be changed to "GN" to match other tables
#              dplyr::group_by(locale_name, Lk_yr, Start_Date, net_id, no_net_panels)%>%
#              dplyr::summarize(soak_hrs = mean(soak_hrs))#corrects for different soak times for same net with different species

#Have to count net panels and divide by 7 because standard nets were split into multiple sections in some cases.
#SGN_E = Net_effort%>%
#            dplyr::group_by(locale_name, Lk_yr)%>%
#            dplyr::summarize(Net_nights = sum(no_net_panels)/7, Net_hours = as.numeric(crossprod(no_net_panels,soak_hrs)/7))%>%
#            dplyr::ungroup()

########################################################################################################

#Create a wide format data set for comparing relative catch. May want to add sby_code back in as grouper? This messes up species comparisons.
#First only use groups that are recruited to gillnets (>150mm).
predf = gdf%>%
  #dplyr::filter(mean_FL>150)%>%
  dplyr::group_by(dplyr::across(dplyr::all_of(c(
    "Lk_yr","WBID","locale_name","year","Season","method","age",
    controls, Contrast   # Contrast is included via Covariates, but spelled out here for clarity
  ))))%>%#, sby_code
  dplyr::filter(!is.na(.data[[Contrast]]),
                !grepl(",",.data[[Contrast]]),
                !is.na(N_ha_rel))%>%#remove group that included multiple levels within contrast. Remove fish that do not link to stocking records
  dplyr::summarize(groups = dplyr::n(),
                   N = sum(N),
                   xN = sum(NetXN),
                   Nr = sum(N_ha_rel),
                   .groups = "drop_last")%>%
  # keep only key-groups that have >= 2 distinct Contrast levels
  dplyr::filter(dplyr::n_distinct(.data[[Contrast]]) >= 2) %>%
  dplyr::arrange(desc(.data[[Contrast]]))%>%
  dplyr::ungroup()


#The distinct categories found in the contrast
cats = predf%>%
  dplyr::pull(.data[[Contrast]])%>%
  unique()%>%
  sort(decreasing = TRUE)

cats <- cats[!is.na(cats)]   # remove any cases where the contrast is an NA value

#SAR_cat we want in numeric order. For strain or genotype we want 2N and BW at back end as they are the "base case" higher sample size and survival.
if(Contrast == "SAR_cat"){
  cats = sort(cats)
  predf = dplyr::arrange(predf, .data[[Contrast]])
  }

#i = 1
#j = 2
#calculations so that spreading an renaming columns works even when grouping columns change(i.e. for species groupings)
df = NULL
spread = c("xN", "Nr", "N", "groups")
Lspread = length(spread)
maxcols = as.integer(ncol(predf)-(Lspread-1)+(Lspread*2-1))#In the loop below this will be the max with of spread dataframe

for(i in 1:(length(cats)-1)){
  for(j in (i+1):length(cats)){
    Cons = c(i,j)
    Con = paste0(cats[i],"vs",cats[j])
    dfnew = predf%>%
      dplyr::filter(.data[[Contrast]] %in% cats[c(i,j)])%>%#Can switch to filter by delta
      dplyr::mutate(Comparison = Con)%>%
      tidyr::pivot_wider(names_from = !!rlang::sym(Contrast),
                         values_from = tidyselect::all_of(spread)
                         )%>%#, names_sort = TRUE
      dplyr::rename(a_xN = tidyselect::all_of(paste0("xN_", cats[i])),
                    b_xN = tidyselect::all_of(paste0("xN_", cats[j])),
                    a_Nr = tidyselect::all_of(paste0("Nr_", cats[i])),
                    b_Nr = tidyselect::all_of(paste0("Nr_", cats[j])),
                    a_N  = tidyselect::all_of(paste0("N_" , cats[i])),
                    b_N  = tidyselect::all_of(paste0("N_" , cats[j]))
                    )%>%
      dplyr::rowwise()%>%
      dplyr::filter(0<(sum(a_xN, b_xN)),
                    !is.na(sum(a_xN, b_xN, a_Nr, b_Nr)))%>%#Do not want na.rm = T becasue want to remove cases where there are NA in one of the categories
      dplyr::mutate(Recap_p = a_xN/(a_xN+b_xN),
                    Release_p = a_Nr/(a_Nr+b_Nr),
                    a = cats[i], b = cats[j],
                    N = sum(a_N, b_N, na.rm = TRUE))%>%
      dplyr::ungroup()
    df <- dplyr::bind_rows(df, dfnew)  # prefer bind_rows for tibbles


  }
}

if(nrow(df)>0){
wide_df = df%>%
  dplyr::rowwise()%>%
  dplyr::mutate(surv_diff=Recap_p/Release_p,
                LCI = stats::binom.test(round(a_xN,0),round((a_xN+b_xN),0), Release_p)$conf.int[1],
                UCI = stats::binom.test(round(a_xN,0),round((a_xN+b_xN),0), Release_p)$conf.int[2],
                Sig_p = 0.05>stats::binom.test(round(a_xN,0),round((a_xN+b_xN),0),Release_p)$p.value)#%>%
 # dplyr::mutate(LCI = LCI/(1-LCI),
 #               UCI = UCI/(1-UCI)) Relic from converting CI intervals to CI intervals of ratios and not proportions

#Add in average relative survival (log odds) for each comparison.
wide_df = wide_df%>%
  dplyr::group_by(Comparison, age)%>%
  dplyr::mutate(avg_surv = exp(mean(log(surv_diff))))%>%#Not sure if this is valid now that it is a ratio of proportions
  dplyr::ungroup()

#Add competitor species data as typically important to survival
Spp_comp =Lake_Spp%>%
  dplyr::select(WBID, species_code, subfamily)%>%
  dplyr::group_by(WBID)%>%
  dplyr::summarize(Non_salm = paste(sort(unique(species_code[.data$subfamily!="Salmoninae"])), collapse = ','))%>%
  dplyr::ungroup()

wide_df = dplyr::left_join(wide_df,Spp_comp, by = c("WBID"))
#Time varying species composition
#wide_df = left_join(wide_df,Lake_Spp[,c("WBID","year","Non_salm")], by = c("WBID","year"))

#Put into global environment. These only appear if Contrast is not NULL.
wide_df<<-wide_df
}
controls<<-controls

#}

#Removes cohorts years where nothing is observed> STILL WANT THIS?
#This coding should be used earlier to remove strain experiments that appear with size experiments
idf$Lk_yr_age = paste0(idf$Lk_yr, "_", idf$age)
gdf$Lk_yr_age = paste0(gdf$Lk_yr, "_", gdf$age)

gdf = gdf%>%
  dplyr::filter(Lk_yr_age %in% idf$Lk_yr_age)

#SUmmarize fish collections and counts
ids = unique(c(idf$Lk_yr, gdf$Lk_yr))
spp = unique(c(idf$species_code, gdf$species_code))

cdf <- Collections %>%
  dplyr::mutate(
    Lk_yr = paste0(WBID, "_", end_year),
    year = end_year,
    Season = {
      d <- as.Date(end_dt)                      # parse once
      ok <- !is.na(d)                           # keep non-missing
      out <- rep(NA_character_, dplyr::n())     # default NA
      out[ok] <- as.character(metR::season(d[ok]))
      out
    }
  ) %>%
  dplyr::filter(Lk_yr %in% ids)%>%
  dplyr::group_by(region, Lk_yr, WBID, locale_name, year, Season, method
                )%>%
  dplyr::summarize(Net_designs = paste0(unique(sample_design_code), collapse = ","),
                   Nets = length(unique(interaction(end_dt, net_angler_id))),
                   Hours = sum(fishing_hours,na.rm = T))


counts = vwCollectCount%>%
  dplyr::mutate(
    Lk_yr = paste0(WBID, "_", lubridate::year(as.Date(date_assessed))),
    Season = {
      d <- as.Date(date_assessed)                      # parse once
      ok <- !is.na(d)                           # keep non-missing
      out <- rep(NA_character_, dplyr::n())     # default NA
      out[ok] <- as.character(metR::season(d[ok]))
      out
    }
  )%>%
  dplyr::filter(Lk_yr %in% ids, species_code%in%spp)%>%
  dplyr::group_by(Lk_yr, Season, species_code)%>%
  dplyr::summarize( species = paste0(unique(na.omit(species_code)), collapse = ","),
                    Count = sum(species_count,na.rm = T))

cdf = cdf%>%
  dplyr::left_join(counts, by = c("Lk_yr", "Season"))#add species_code and species_count

idf_cnts = idf%>%
  dplyr::filter(species_code%in%spp)%>%
  dplyr::group_by(region, Lk_yr, WBID, locale_name, year, Season, method)%>%
  dplyr::summarize(sp_meas = paste0(unique(na.omit(species_code)), collapse = ","),
                    Measured = n())

cdf = cdf%>%
  dplyr::full_join(idf_cnts, by = c("region", "Lk_yr", "WBID", "locale_name", "year", "Season", "method")
)

#There are cases with fish, ut no netting details.
cdf <- cdf %>%
  rowwise() %>%
  mutate(
    species_code = {
      vals <- c(species_code, species, sp_meas)
      vals <- vals[!is.na(vals) & nzchar(trimws(vals)) & toupper(trimws(vals)) != "NA"]
      paste(unique(vals), collapse = ",")
    }
  ) %>%
  ungroup()%>%
  select(-c(species, sp_meas))


#Put data frames into the global environment
idf<<-idf
gdf<<-gdf
cdf<<-cdf




#Add function parameters to the global environment
Spp<<-Spp
Strains<<-Strains
Contrast<<-Contrast


if(!exists("wide_df")){message("WARNING: There were no controlled co-stocking events in the database for the specified contrast. Therefore, no wide_df exists, no survival plot can be constructed.")}

invisible(gc())
}

