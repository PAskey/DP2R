#' Predict selectivity using Millar models by species
#' @export
#'
#' @param species  Species code (e.g. "RB", "KO").
#' @param classes  Vector of length classes (mm) to predict for. Default 75:650.
#' @param rtype    Optional: Millar curve type; if NULL, taken from best_models.
#' @param meshSizes_in Optional: mesh sizes (mm); if NULL, taken from best_models.
#' @param theta    Optional: parameter vector; if NULL, taken from best_models.
#' @param rel.power Optional: relative fishing power per mesh; default = 1 for all meshes.
#' @param theta_min_mesh This is a fixed parameter of the smallest mesh size used in fitting the original selectivity functions (1" mesh for nets fit in best_models), passed to rtypes_Millar(). This should not be changed for predicting nets without 1" mesh, only change if selectivity model is updated based on even smaller meshes.
#' @param best_models Optional: a data.frame like the DP2R best_models data frame object. If NULL, function will try to load it from package data.'best_models' must have columns: species_code, rtype, theta, meshSizes
#'
predict_Millar <- function(species,
                           classes = 75:750,
                           rtype = NULL,
                           meshSizes_in = NULL,
                           theta = NULL,
                           rel.power = NULL,
                           theta_min_mesh = 1,
                           best_models = NULL) {

  ## 1. Get best_models if not provided explicitly
  if (is.null(best_models)) {
    # Always load from package data into this function's environment
    data("best_models", envir = environment())
    best_models <- get("best_models", envir = environment(), inherits = FALSE)
  }

  # Now check structure
  if (!all(c("species_code", "rtype", "theta", "meshSizes") %in% names(best_models))) {
    stop("best_models must have columns: species_code, rtype, theta, meshSizes")
  }

  ## 2. Find the row for this species
  idx <- match(species, best_models$species_code)
  if (is.na(idx)) {
    stop("No best model found for species '", species,
         "'. Check species_code or update best_models.")
  }

  ## 3. Fill in defaults from best_models where user did not override
  if (is.null(rtype)) {
    rtype <- best_models$rtype[idx]
  }

  ##Set mesh sizes and convert inches to mm
  #Remove the theta_min_esh in future as that is a model paramter and should not be changed here.
  mesh_mm <- as.numeric(meshSizes_in) * 25.4
  theta_min_mesh = theta_min_mesh * 25.4

  #Consider moving this out of this function as a parameter, highly unlikely to change ever


  if (is.null(theta)) {
    theta <- best_models$theta[[idx]]
  }

  ## 4. Original logic from your previous predict_Millar

  # Input checks
  if (is.null(meshSizes_in)) {
      stop("meshSizes_in is NULL and no default.")
    }

  if (!isTRUE(all.equal(sort(mesh_mm), mesh_mm))) {
    stop("Mesh size must be in ascending order!")
  }

  if (is.null(rel.power)) {
    rel.power <- rep(1, length(meshSizes_in))
  }

  if (!is.null(rel.power) && length(rel.power) != length(mesh_mm)) {
    stop("Length of rel.power should match length meshSizes_in")
  }

  # Get selection curve function
  r <- DP2R:::rtypes_Millar(rtype, theta_min_mesh)

  all_classes <- 75:900  # Full range of fish sizes caught in gillnets in DataPond
  rmatrix     <- outer(all_classes, mesh_mm, r, theta)
  rmatrix     <- t(t(rmatrix) * rel.power)

  sum_class <- apply(rmatrix, 1, sum, na.rm = TRUE)
  p_full    <- sum_class / max(sum_class)

  df <- data.frame(Length_mm = all_classes, p = p_full)

  # Return predictions only for the requested classes
  p <- df$p[match(classes, df$Length_mm)]
  return(p)
}

