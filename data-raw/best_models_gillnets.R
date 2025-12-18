##Code to fit all avaiable mesh specific gilllnet data to a species specific gillnet selectivity models that can be used to correct gillnet data.

library(dplyr)
library(tidyr)
library(readr)
library(TropFishR)
library(knitr)
library(DP2R)

DP2R(Tables = c("vwFishCollection", "vwIndividualFish"))
#fish collection events using RIC nets where mesh data collected (Add Dragon 2014 and maybe others where sample design is left NA)
sgn7s = vwFishCollection%>%filter(sample_design_code == "SGN7")%>%pull(fish_collection_id)
#Those collection events where at least 4 different meshes captured fish. Change to regroup by assess_event, because can be a net or two with few (but valuable fish, when lots of fish caught overall)
CIs = vwIndividualFish%>%
  filter(fish_collection_id%in%sgn7s, !is.na(mesh_size_code))%>%
  group_by(assess_event_name)%>%#(fish_collection_id)%>%
  mutate(N = length(unique(mesh_size_code)))%>%
  filter(N>3)%>%pull(fish_collection_id)%>%unique()

#only include species where at least 100 indivudals captured.
Spp = vwIndividualFish%>%filter(fish_collection_id%in%CIs, !is.na(mesh_size_code))%>%group_by(species_code)%>%summarize(N = n())%>%filter(N>100)%>%pull(species_code)

#Final data
Bio_data_w_meshes = vwIndividualFish%>%filter(fish_collection_id%in%CIs, !is.na(mesh_size_code), species_code%in%Spp)

###
#Reconfigure data with various categories to display and analyze data

df = Bio_data_w_meshes%>%
  dplyr::select(gazetted_name, year, species_code, strain, length_mm, mesh_size_code)%>%
  dplyr::filter(!is.na(length_mm))#drop headless fish

breaks = c(80,250,400)
names(breaks) = c("80-249","250-399",">=400")

df = df%>%
  mutate(Length_Bin = FSA::lencat(length_mm, 10),
         Big_bin = FSA::lencat(length_mm,breaks = breaks, as.fact = TRUE, use.names = TRUE, names=names(breaks)),
         Mid_length = Length_Bin+5)

df = df%>%group_by(species_code, Big_bin, Mid_length, mesh_size_code)%>%summarize(N = n())%>%ungroup()

df=df%>%group_by(species_code,mesh_size_code)%>%mutate(P_mesh = N/sum(N))%>%ungroup()
df=df%>%group_by(species_code,Mid_length)%>%mutate(N_len = sum(N), P_length = N/sum(N))%>%ungroup()
df=df%>%group_by(species_code,Big_bin)%>%mutate(N_bin = sum(N), P_bin = N/sum(N))%>%ungroup()

###
#Build  matrix of catch frequency by length bin and mesh size needed for fitting
.build_mesh_mat <- function(df, spp_code) {
  df_sp <- df %>%
    dplyr::filter(species_code == spp_code) %>%
    mutate(
      mesh_in = readr::parse_number(as.character(mesh_size_code)),
      mesh_mm = mesh_in * 25.4
    )

  select_mat <- df_sp[, c("Mid_length", "mesh_mm", "N")] %>%
    tidyr::pivot_wider(
      names_from  = mesh_mm,
      values_from = N
    )

  select_mat <- unname(data.matrix(select_mat))[ , -1]  # drop Mid_length col
  select_mat[is.na(select_mat)] <- 0

  midLengths <- sort(unique(df_sp$Mid_length)) * 1
  mesh_mm  <- sort(unique(df_sp$mesh_mm))

  list(
    midLengths      = midLengths,
    meshSizes       = mesh_mm,
    CatchPerNet_mat = select_mat
  )
}

###
#Some starting values needed for specific models

# Starting values for bimodal normal
x0_binorm <- c(95, 20, 333, 115, 0.5)

# Starting values for bimodal lognormal
x0_bilognorm <- c(4.6, 0.2, 5.9, 0.3, 0.5)

# (Optional) sanity check / translation to normal scale for bimodal lognormal
x <- x0_bilognorm
pars <- c(
  exp(x[1] - x[2]^2),
  sqrt(exp(2 * x[1] + x[2]^2) * (exp(x[2]^2) - 1)),
  exp(x[3] - x[4]^2),
  sqrt(exp(2 * x[3] + x[4]^2) * (exp(x[4]^2) - 1)),
  exp(x[5]) / (1 + exp(x[5]))
)
pars

#fit models for each species
fit_selectivity <- function(df, spp_code) {
  mesh_mat <- .build_mesh_mat(df, spp_code)

  #Clean up species cases were fish only caught in a couple meshes (e.g. RSC)
  # Drop mesh columns with zero total catch
  keep <- colSums(mesh_mat$CatchPerNet_mat, na.rm = TRUE) > 0
  mesh_mat$CatchPerNet_mat <- mesh_mat$CatchPerNet_mat[, keep, drop = FALSE]
  mesh_mat$meshSizes <- mesh_mat$meshSizes[keep]
  n_mesh <- length(mesh_mat$meshSizes)



  models <- list()
  models[[1]] <- select_Millar(mesh_mat, x0 = NULL, rtype = "norm.loc",   plot = FALSE)
  models[[2]] <- select_Millar(mesh_mat, x0 = NULL, rtype = "norm.sca",   plot = FALSE)
  models[[3]] <- select_Millar(mesh_mat, x0 = NULL, rtype = "lognorm",    plot = FALSE)
  if (n_mesh >= 4) models[[4]] <- select_Millar(mesh_mat, x0=x0_binorm,  rtype="binorm.sca", plot=FALSE)
  if (n_mesh >= 4 & spp_code!="BNH") models[[5]] <- select_Millar(mesh_mat, x0=x0_bilognorm, rtype="bilognorm",  plot=FALSE)

  models <- Filter(Negate(is.null), models)

  Model      <- vapply(models, function(x) x$rtype,                  character(1))
  value      <- vapply(models, function(x) round(x$value, 0),        numeric(1))
  Deviance   <- vapply(models, function(x) round(x$out["Deviance",], 0), numeric(1))
  Likelihood <- vapply(models, function(x) x$out["model.l", ],       numeric(1))
  k          <- vapply(models, function(x) length(x$par),            numeric(1))

  # N = number of observations (same for all models for this species)
  n_obs <- prod(dim(models[[1]]$CatchPerNet_mat))

  AIC_raw <- -2 * Likelihood + 2 * k
  BIC_raw <- -2 * Likelihood + k * log(n_obs)

  # Relative AIC/BIC within species (same as your approach)
  AIC <- round(AIC_raw - min(AIC_raw), 0)
  BIC <- round(BIC_raw - min(BIC_raw), 0)

  summary_df <- data.frame(
    species_code = spp_code,
    Model        = Model,
    value        = value,
    Deviance     = Deviance,
    AIC          = AIC,
    BIC          = BIC,
    stringsAsFactors = FALSE
  )

  # Choose "best" model – here by AIC, but you could change to BIC_raw
  best_idx <- which.min(AIC_raw)

  list(
    models     = models,
    summary    = summary_df,
    best_model = models[[best_idx]],
    best_rtype = Model[best_idx],
    best_theta = models[[best_idx]]$par,
    mesh_mm  = mesh_mat$meshSizes
  )
}

###Loop through all species.
species_vec <- unique(Bio_data_w_meshes$species_code)

fits_list <- lapply(species_vec, function(spp) {
  fit_selectivity(df, spp)
})
names(fits_list) <- species_vec

summary_all <- dplyr::bind_rows(lapply(fits_list, `[[`, "summary"))
kable(summary_all)

###Table of the best model fit for each species
best_models <- data.frame(
  species_code = species_vec,
  rtype        = sapply(fits_list, `[[`, "best_rtype"),
  stringsAsFactors = FALSE
)

best_models$theta     <- I(lapply(fits_list, `[[`, "best_theta"))
best_models$meshSizes <- I(lapply(fits_list, `[[`, "mesh_mm"))

best_models

save(best_models, file = "data/best_models.rda")



