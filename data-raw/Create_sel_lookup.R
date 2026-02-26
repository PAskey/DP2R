#Code to create the sel_lookup data set which is a list column of curves for each species and design_code
library(dplyr)
library(tidyr)
library(purrr)
library(DP2R)

DP2R(Tables = "SampleDesign_MeshSizeCode")

sel_classes <- 75:900

# Build mesh + power lookup per design, where rel.power is based on panel AREA by mesh
mesh_lookup <- SampleDesign_MeshSizeCode %>%
  dplyr::mutate(
    mesh_size_code = as.numeric(mesh_size_code),
    depth  = as.numeric(depth),
    length = as.numeric(length),
    panel_area = depth * length
  ) %>%
  # total area by (design, mesh)
  dplyr::group_by(sample_design_code, mesh_size_code) %>%
  dplyr::summarise(
    area_by_mesh = sum(panel_area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(sample_design_code) %>%
  dplyr::summarise(
    # unique, sorted meshes for predict_Millar()
    meshVec = list(sort(mesh_size_code)),

    # rel.power aligned with meshVec (same order)
    relPower = list(area_by_mesh[order(mesh_size_code)]),

    # single design-area metric for later event weighting
    panel_area_total = sum(area_by_mesh, na.rm = TRUE),

    # optional diagnostics
    n_unique_mesh = n(),
    .groups = "drop"
  )

build_sel_lookup <- function(best_models, mesh_lookup, classes = sel_classes,
                             normalize_relpower = TRUE) {
  sp <- unique(best_models$species_code)

  tidyr::crossing(
    species_code = sp,
    sample_design_code = mesh_lookup$sample_design_code
  ) %>%
    dplyr::left_join(mesh_lookup, by = "sample_design_code") %>%
    dplyr:: mutate(
      curve = purrr::pmap(
        list(species_code, meshVec, relPower),
        function(sp, mv, rp) {

          # optional: normalize so mean(rel.power)=1 (shape identical either way)
          if (normalize_relpower && all(is.finite(rp)) && sum(rp) > 0) {
            rp <- rp / mean(rp)
          }

          DP2R::predict_Millar(
            species = sp,
            classes = classes,
            meshSizes_in = mv,
            rel.power = rp,
            best_models = best_models
          )
        }
      )
    ) %>%
    dplyr::select(species_code, sample_design_code, curve, panel_area_total, meshVec, relPower)
}

sel_lookup <- build_sel_lookup(best_models, mesh_lookup, classes = sel_classes)

usethis::use_data(sel_lookup, overwrite = TRUE)
usethis::use_data(sel_classes, overwrite = TRUE)



