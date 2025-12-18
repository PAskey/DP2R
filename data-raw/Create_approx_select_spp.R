# Build approx_select_spp lookup (species_code -> select_spp) using DP2R data objects only
# Matching priority:
#   1) species_code exists in sel_lookup -> identity
#   2) Species$species
#   3) genus
#   4) subfamily
#   5) family
#   6) order
# Tie-break when multiple candidate model species match:
#   - choose the candidate species_code that occurs most frequently in Biological
#   - if tied, choose alphabetical species_code (deterministic)
build_approx_select_spp <- function(species_codes) {

  # Require Biological in calling environment (annual build workflow)
  if (!exists("Biological", inherits = TRUE)) {
    stop("Object 'Biological' not found. Run this in an environment where Biological is loaded.")
  }
  Biological <- get("Biological", inherits = TRUE)

  # load DP2R package data objects
  data("Species", envir = environment())
  Species <- get("Species", envir = environment(), inherits = FALSE)

  data("sel_lookup", envir = environment())
  sel_lookup <- get("sel_lookup", envir = environment(), inherits = FALSE)

  stopifnot(is.character(species_codes) || is.factor(species_codes))
  species_codes <- unique(as.character(species_codes))

  stopifnot("species_code" %in% names(Species))
  stopifnot(all(c("order", "family", "subfamily", "genus", "species") %in% names(Species)))
  stopifnot("species_code" %in% names(sel_lookup))
  stopifnot(all(c("species_code") %in% names(Biological)))

  model_spp <- sort(unique(as.character(sel_lookup$species_code)))

  # Frequency of model species in Biological (tie-breaker)
  sp_rank <- Biological %>%
    dplyr::count(species_code, name = "n_in_bio") %>%
    dplyr::mutate(species_code = as.character(species_code))

  # Candidate model species with taxonomy + rank
  cand <- Species %>%
    dplyr::filter(species_code %in% model_spp) %>%
    dplyr::mutate(species_code = as.character(species_code)) %>%
    dplyr::left_join(sp_rank, by = "species_code") %>%
    dplyr::mutate(n_in_bio = dplyr::coalesce(n_in_bio, 0L))

  pick_best <- function(cands) {
    cands %>%
      dplyr::arrange(dplyr::desc(n_in_bio), species_code) %>%
      dplyr::slice(1) %>%
      dplyr::pull(species_code)
  }

  out <- lapply(species_codes, function(sp) {

    # Already modeled -> identity
    if (sp %in% model_spp) {
      return(data.frame(species_code = sp,
                        select_spp = sp,
                        match_level = "species_code",
                        stringsAsFactors = FALSE))
    }

    row <- Species %>% dplyr::filter(species_code == sp)
    if (nrow(row) == 0) {
      return(data.frame(species_code = sp,
                        select_spp = NA_character_,
                        match_level = NA_character_,
                        stringsAsFactors = FALSE))
    }

    # 1) match by Species$species (can match even if species_code differs)
    val <- row$species[1]
    if (!is.na(val) && nzchar(as.character(val))) {
      cands <- cand %>% dplyr::filter(.data$species == val)
      if (nrow(cands) > 0) {
        return(data.frame(species_code = sp,
                          select_spp = pick_best(cands),
                          match_level = "species",
                          stringsAsFactors = FALSE))
      }
    }

    # 2) genus -> subfamily -> family -> order
    for (lvl in c("genus", "subfamily", "family", "order")) {
      val <- row[[lvl]][1]
      if (is.na(val) || !nzchar(as.character(val))) next

      cands <- cand %>% dplyr::filter(.data[[lvl]] == val)
      if (nrow(cands) > 0) {
        return(data.frame(species_code = sp,
                          select_spp = pick_best(cands),
                          match_level = lvl,
                          stringsAsFactors = FALSE))
      }
    }

    # no match at any level
    data.frame(species_code = sp,
               select_spp = NA_character_,
               match_level = NA_character_,
               stringsAsFactors = FALSE)
  })

  dplyr::bind_rows(out)
}

spp <- unique(Biological$species_code)
approx_select_spp <- build_approx_select_spp(spp)

# save into your package data
usethis::use_data(approx_select_spp, overwrite = TRUE)

