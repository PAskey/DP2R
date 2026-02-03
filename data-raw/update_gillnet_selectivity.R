# data-raw/update_gillnet_selectivity.R
# Maintainer script: rebuild best_models, approx_select_spp, sel_lookup

library(DP2R)
library(dplyr)
library(tidyr)
library(purrr)

# 1) Pull fresh base tables needed for both models and ranges

# approx script expects Biological, and "vwFishCollection", "vwIndividualFish" are needed for models and range
if(!exists("Biological")){linkClips()}

# 2) Fit/update best_models (your existing workflow)
source("data-raw/best_models_gillnets.R")   # should create 'best_models'

# 3) Build/update approx_select_spp (your existing workflow)
#    IMPORTANT: modify Create_approx_select_spp.R to output min_FL_gn/max_FL_gn columns
source("data-raw/Create_approx_select_spp.R")  # should create 'approx_select_spp'

# 4) Build/update sel_lookup (modify to use true ranges & store classes_min/max)
source("data-raw/Create_sel_lookup.R")  # should create 'sel_lookup'

# 5) Save packaged datasets
usethis::use_data(best_models, approx_select_spp, sel_lookup, overwrite = TRUE)
