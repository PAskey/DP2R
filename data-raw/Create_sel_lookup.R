#Code to create the sel_lookup data set which is a list column of curves for each species and design_code
library(DP2R)
DP2R(Tables = "SampleDesign_MeshSizeCode")

mesh_lookup <- SampleDesign_MeshSizeCode %>%
  dplyr::mutate(mesh_size_code = as.numeric(mesh_size_code)) %>%
  dplyr::group_by(sample_design_code) %>%
  dplyr::summarise(meshVec = list(sort(mesh_size_code)), .groups = "drop")

all_classes = c(75:900)

build_sel_lookup <- function(best_models, mesh_lookup, classes = all_classes) {
  sp <- unique(best_models$species_code)

  tidyr::crossing(
    species_code = sp,
    sample_design_code = mesh_lookup$sample_design_code
  ) %>%
    dplyr::left_join(mesh_lookup, by = "sample_design_code") %>%
    dplyr::mutate(
      curve = purrr::map2(species_code, meshVec, \(sp, mv) {
        DP2R::predict_Millar(
          species = sp,
          classes = classes,
          meshSizes_in = mv,
          best_models = best_models
        )
      })
    ) %>%
    dplyr::select(species_code, sample_design_code, curve)
}

sel_lookup <- build_sel_lookup(best_models, mesh_lookup, classes = all_classes)
usethis::use_data(sel_lookup, overwrite = TRUE)



