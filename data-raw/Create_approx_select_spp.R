# Build approx_select_spp lookup (species_code -> select_spp) using DP2R data objects only
# This table is used to find a similar species for gillnet selectivity when mesh specific data is not available for the species of interest.
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
  stopifnot("common_name" %in% names(Species))

  stopifnot("species_code" %in% names(sel_lookup))
  stopifnot(all(c("species_code", "length_mm", "method") %in% names(Biological)))

  model_spp <- sort(unique(as.character(sel_lookup$species_code)))

  # Frequency of model species in Biological (tie-breaker)
  sp_rank <- Biological %>%
    dplyr::count(species_code, name = "n_in_bio") %>%
    dplyr::mutate(species_code = as.character(species_code))

  # min/max fork length (length_mm) by species_code
  # GN-only observed range
  fl_range_gn <- Biological %>%
    dplyr::filter(method == "GN") %>%
    dplyr::mutate(species_code = as.character(species_code)) %>%
    dplyr::summarise(
      min_FL_gn = suppressWarnings(min(length_mm, na.rm = TRUE)),
      max_FL_gn = suppressWarnings(max(length_mm, na.rm = TRUE)),
      .by = "species_code"
    ) %>%
    dplyr::mutate(
      min_FL_gn = dplyr::if_else(is.infinite(min_FL_gn), NA_real_, as.numeric(min_FL_gn)),
      max_FL_gn = dplyr::if_else(is.infinite(max_FL_gn), NA_real_, as.numeric(max_FL_gn))
    )

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
      return(data.frame(
        species_code = sp,
        select_spp = sp,
        match_level = "species_code",
        stringsAsFactors = FALSE
      ))
    }

    row <- Species %>% dplyr::filter(species_code == sp)
    if (nrow(row) == 0) {
      return(data.frame(
        species_code = sp,
        select_spp = NA_character_,
        match_level = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    # 1) match by Species$species
    val <- row$species[1]
    if (!is.na(val) && nzchar(as.character(val))) {
      cands <- cand %>% dplyr::filter(.data$species == val)
      if (nrow(cands) > 0) {
        return(data.frame(
          species_code = sp,
          select_spp = pick_best(cands),
          match_level = "species",
          stringsAsFactors = FALSE
        ))
      }
    }

    # 2) genus -> subfamily -> family -> order
    for (lvl in c("genus", "subfamily", "family", "order")) {
      val <- row[[lvl]][1]
      if (is.na(val) || !nzchar(as.character(val))) next

      cands <- cand %>% dplyr::filter(.data[[lvl]] == val)
      if (nrow(cands) > 0) {
        return(data.frame(
          species_code = sp,
          select_spp = pick_best(cands),
          match_level = lvl,
          stringsAsFactors = FALSE
        ))
      }
    }

    # no match at any level
    data.frame(
      species_code = sp,
      select_spp = NA_character_,
      match_level = NA_character_,
      stringsAsFactors = FALSE
    )
  })

  # Build final table + add common_name + FL ranges
  dplyr::bind_rows(out) %>%
    dplyr::left_join(
      Species %>%
        dplyr::transmute(
          species_code = as.character(species_code),
          common_name  = as.character(common_name)
        ),
      by = "species_code"
    ) %>%
    dplyr::left_join(fl_range_gn, by = "species_code") %>%
    dplyr::relocate(common_name, .before = species_code)
}

spp <- unique(Biological$species_code)
approx_select_spp <- build_approx_select_spp(spp)

# save into your package data
usethis::use_data(approx_select_spp, overwrite = TRUE)
