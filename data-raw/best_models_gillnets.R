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
#Those events where at least 3 differnet meshes captured fish
CIs = vwIndividualFish%>%filter(fish_collection_id%in%sgn7s, !is.na(mesh_size_code))%>%group_by(fish_collection_id)%>%summarize(N = length(unique(mesh_size_code)))%>%filter(N>2)%>%pull(fish_collection_id)
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
  mutate(Length_Bin = lencat(length_mm, 10),
         Big_bin = lencat(length_mm,breaks = breaks, as.fact = TRUE, use.names = TRUE, names=names(breaks)),
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
      mesh_mm = parse_number(as.character(mesh_size_code)) * 25.4
    )

  select_mat <- df_sp[, c("Mid_length", "mesh_size_code", "N")] %>%
    tidyr::pivot_wider(
      names_from  = mesh_size_code,
      values_from = N
    )

  select_mat <- unname(data.matrix(select_mat))[ , -1]  # drop Mid_length col
  select_mat[is.na(select_mat)] <- 0

  midLengths <- sort(unique(df_sp$Mid_length)) * 1
  meshSizes  <- sort(unique(df_sp$mesh_mm))

  list(
    midLengths      = midLengths,
    meshSizes       = meshSizes,
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

  models <- list()
  models[[1]] <- select_Millar(mesh_mat, x0 = NULL,          rtype = "norm.loc",   plot = FALSE)
  models[[2]] <- select_Millar(mesh_mat, x0 = NULL,          rtype = "norm.sca",   plot = FALSE)
  models[[3]] <- select_Millar(mesh_mat, x0 = NULL,          rtype = "lognorm",    plot = FALSE)
  models[[4]] <- select_Millar(mesh_mat, x0 = x0_binorm,     rtype = "binorm.sca", plot = FALSE)
  models[[5]] <- select_Millar(mesh_mat, x0 = x0_bilognorm,  rtype = "bilognorm",  plot = FALSE)

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
    meshSizes  = mesh_mat$meshSizes
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
best_models$meshSizes <- I(lapply(fits_list, `[[`, "meshSizes"))

best_models




