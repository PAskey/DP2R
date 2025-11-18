#' Predict selectivity using Millar models by species
#' @export
#'
#' @param species  Species code (e.g. "RB", "KO").
#' @param classes  Vector of length classes (mm) to predict for. Default 75:650.
#' @param rtype    Optional: Millar curve type; if NULL, taken from best_models.
#' @param meshSizes Optional: mesh sizes (mm); if NULL, taken from best_models.
#' @param theta    Optional: parameter vector; if NULL, taken from best_models.
#' @param rel.power Optional: relative fishing power per mesh; default = 1 for all meshes.
#' @param theta_min_mesh As in your original function, passed to rtypes_Millar().
#' @param best_models Optional: a data.frame like your best_models object. If NULL,
#'        function will try to load it from package data.
#'
predict_Millar <- function(species,
                           classes = 75:650,
                           rtype = NULL,
                           meshSizes = NULL,
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
  
  if (is.null(meshSizes)) {
    meshSizes <- best_models$meshSizes[[idx]]
  }
  
  if (is.null(theta)) {
    theta <- best_models$theta[[idx]]
  }
  
  ## 4. Original logic from your previous predict_Millar
  
  # Input checks
  if (is.null(meshSizes)) {
    # If you still want a global fallback:
    if (exists("RIC_param") && !is.null(RIC_param$RIC_meshes)) {
      meshSizes <- RIC_param$RIC_meshes
    } else {
      stop("meshSizes is NULL and no default (RIC_param$RIC_meshes) is available.")
    }
  }
  
  if (sum(sort(meshSizes) == meshSizes) != length(meshSizes)) {
    stop("Mesh size must be in ascending order!")
  }
  
  if (is.null(rel.power)) {
    rel.power <- rep(1, length(meshSizes))
  }
  
  if (!is.null(rel.power) && length(rel.power) != length(meshSizes)) {
    stop("Length of rel.power should match length meshSizes")
  }
  
  # Get selection curve function
  r <- rtypes_Millar(rtype, theta_min_mesh)
  
  all_classes <- 75:650  # Full range of fish sizes
  rmatrix     <- outer(all_classes, meshSizes, r, theta)
  rmatrix     <- t(t(rmatrix) * rel.power)
  
  sum_class <- apply(rmatrix, 1, sum, na.rm = TRUE)
  p_full    <- sum_class / max(sum_class)
  
  df <- data.frame(Length_mm = all_classes, p = p_full)
  
  # Return predictions only for the requested classes
  p <- df$p[match(classes, df$Length_mm)]
  return(p)
}

