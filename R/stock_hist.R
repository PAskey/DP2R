#' Plot the stocking history of one or more lakes
#'
#' Draws a stacked bar per release year for each waterbody (one panel per WBID).
#' Each bar is split by stocked product and coded so the legend stays short:
#' \itemize{
#'   \item Fill colour = species (KO blue, EB orange, CCT green, other species a fallback colour).
#'     Strain is only shown for RB, where each strain gets its own shade from pink to red.
#'     RB strain shades are set from all RB strains in vwRelease, so they stay the same between plots.
#'   \item Border colour = lifestage (FR light grey, YE grey, CA (CA1 + CA2) black, any other lifestage brown).
#'     Egg releases are not plotted.
#'   \item Transparency = ploidy (2N/AF2N solid, 3N/AF3N half transparent).
#' }
#' Any mark_code is printed over the part of the bar it belongs to.
#' Data come from DataPond vwRelease. If vwRelease is not already loaded, it is
#' pulled with DP2R(), which needs a valid DataPond connection ('conn').
#'
#' @title stock_hist
#' @name stock_hist
#' @keywords DP2R; releases; stocking; plot
#' @export
#' @param WBID A character vector of waterbody identifiers (e.g. c("00607THOM", "01286STUR")).
#' @param min_year First release year to plot. Defaults to the earliest year on record for those lakes.
#' @param max_year Last release year to plot. Defaults to the latest year on record for those lakes.
#' @param measure What the bar height shows: "quantity" (number of fish, default),
#'   "quantity_ha" (fish per hectare) or "biomass_kg".
#' @param ncol Number of columns of lake panels (default 2).
#' @param label_size Text size of the mark_code labels.
#' @return A ggplot object, which can be modified with further ggplot layers.
#' @examples
#' #Must be connected to VPN if working remotely
#' stock_hist(c("00607THOM", "01286STUR"), 2000, 2025)
#'
#' #Fish per hectare instead of total numbers
#' stock_hist("00607THOM", 2010, 2025, measure = "quantity_ha")
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data

stock_hist <- function(WBID, min_year = NULL, max_year = NULL,
                       measure = c("quantity", "quantity_ha", "biomass_kg"),
                       ncol = 2, label_size = 2.5) {

  measure <- match.arg(measure)
  wbids <- unique(WBID)

  # Load release data if needed (DP2R renames waterbody_identifier -> WBID,
  # strain_code -> strain, ploidy_code -> ploidy)
  if (!exists("vwRelease", envir = .GlobalEnv)) DP2R(Tables = "vwRelease")
  rel <- get("vwRelease", envir = .GlobalEnv)

  needed <- c("WBID", "release_year", "species_code", "strain", "ploidy",
              "lifestage_code", "mark_code", "quantity", "biomass_kg")
  missing_cols <- setdiff(needed, names(rel))
  if (length(missing_cols) > 0) {
    stop("vwRelease is missing column(s): ", paste(missing_cols, collapse = ", "),
         ". Reload it with DP2R('vwRelease') (not as.raw = TRUE).")
  }

  # Guard against duplicate export rows in the view
  if ("export_id" %in% names(rel)) rel <- dplyr::distinct(rel, .data$export_id, .keep_all = TRUE)

  # RB strain colours are set from every RB strain in vwRelease (not just the
  # lakes plotted), so a strain keeps the same colour from one plot to the next.
  all_rb_strains <- sort(unique(stats::na.omit(rel$strain[rel$species_code == "RB"])))

  rel <- dplyr::filter(rel, .data$WBID %in% wbids)

  not_found <- setdiff(wbids, unique(rel$WBID))
  if (length(not_found) > 0) {
    warning("No releases found for WBID: ", paste(not_found, collapse = ", "), call. = FALSE)
  }
  if (nrow(rel) == 0) stop("No releases found for any of the supplied WBIDs.")

  if (is.null(min_year)) min_year <- min(rel$release_year, na.rm = TRUE)
  if (is.null(max_year)) max_year <- max(rel$release_year, na.rm = TRUE)

  rel <- dplyr::filter(rel, .data$release_year >= min_year, .data$release_year <= max_year)
  if (nrow(rel) == 0) stop("No releases for these WBIDs between ", min_year, " and ", max_year, ".")

  # Per-hectare values: use the view's area, filling gaps from DP2R::Lakes
  if (measure == "quantity_ha") {
    lake_area <- DP2R::Lakes %>%
      dplyr::filter(!is.na(.data$area_ha)) %>%
      dplyr::distinct(.data$WBID, .keep_all = TRUE) %>%
      dplyr::select("WBID", lake_area_ha = "area_ha")
    if (!"area_ha" %in% names(rel)) rel$area_ha <- NA_real_
    rel <- rel %>%
      dplyr::left_join(lake_area, by = "WBID") %>%
      dplyr::mutate(area_use = dplyr::coalesce(.data$area_ha, .data$lake_area_ha))
    no_area <- unique(rel$WBID[is.na(rel$area_use)])
    if (length(no_area) > 0) {
      warning("No surface area for WBID: ", paste(no_area, collapse = ", "),
              " - those releases are dropped from the per-hectare plot.", call. = FALSE)
    }
  }

  # Panel label: locale name without "Lake" (e.g. "BECKER LAKE" -> "Becker"),
  # falling back to WBID. Panels follow the order WBIDs were supplied.
  name_col <- intersect(c("locale_name", "waterbody_name", "gazetted_name"), names(rel))[1]
  lake_names <- vapply(wbids, function(id) {
    if (is.na(name_col)) return(id)
    nm <- stats::na.omit(rel[[name_col]][rel$WBID == id])
    if (length(nm) == 0 || nm[1] == "") return(id)
    nm <- trimws(gsub("\\s*\\blake\\b\\s*", " ", nm[1], ignore.case = TRUE))
    if (nm == "") id else tools::toTitleCase(tolower(nm))
  }, character(1))
  # Two lakes with the same name get their WBID added so panels stay separate
  dup_nm <- lake_names %in% lake_names[duplicated(lake_names)]
  lake_names[dup_nm] <- paste0(lake_names[dup_nm], " (", wbids[dup_nm], ")")
  names(lake_names) <- wbids

  na_dash <- function(x) ifelse(is.na(x) | x == "", "?", as.character(x))

  plot_df <- rel %>%
    dplyr::mutate(
      Lake = factor(unname(lake_names[.data$WBID]), levels = unname(lake_names)),
      # Fill = species; RB split by strain
      Stock = dplyr::if_else(.data$species_code %in% "RB",
                             paste0("RB ", na_dash(.data$strain)),
                             na_dash(.data$species_code)),
      # Border = lifestage (CA1/CA2 combined to CA)
      Lifestage = dplyr::if_else(.data$lifestage_code %in% c("CA1", "CA2"), "CA",
                                 na_dash(.data$lifestage_code)),
      # Transparency = ploidy (any triploid code, e.g. 3N/AF3N, is see-through)
      Ploidy = dplyr::if_else(grepl("3", .data$ploidy), "3N / AF3N", "2N / AF2N"),
      mark = dplyr::if_else(is.na(.data$mark_code) | .data$mark_code %in% c("NONE", ""),
                            "", as.character(.data$mark_code)),
      value = switch(measure,
                     quantity    = .data$quantity,
                     quantity_ha = .data$quantity / .data$area_use,
                     biomass_kg  = .data$biomass_kg)
    ) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    # Egg releases are not plotted
    dplyr::filter(!.data$Lifestage %in% c("EG", "EY", "EE")) %>%
    # One segment per lake-year-stock-lifestage-ploidy-mark
    dplyr::group_by(.data$Lake, .data$release_year, .data$Stock, .data$Lifestage,
                    .data$Ploidy, .data$mark) %>%
    dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
    dplyr::mutate(seg = interaction(.data$Stock, .data$Lifestage, .data$Ploidy, .data$mark,
                                    drop = TRUE, lex.order = TRUE)) %>%
    dplyr::arrange(.data$Lake, .data$release_year, .data$seg)

  # ---- Colour coding --------------------------------------------------------
  # Species colours (fixed). Species not listed get a fallback colour.
  spp_cols <- c(KO = "#1F5FBF",   # blue
                EB = "#F28C28",   # orange
                CCT = "#2E8B3D")  # green
  other_cols <- c("#7B4FA0", "#8C6D46", "#2BA6A6", "#C9B21C", "#5A6B7A", "#9ACD32", "#4B0082", "#A0522D")
  # RB strains: pink -> red ramp
  rb_lv <- unique(c(paste0("RB ", all_rb_strains), "RB ?"))
  rb_cols <- stats::setNames(grDevices::colorRampPalette(c("#F48FB1", "#E0457B", "#B0001E"))(length(rb_lv)), rb_lv)

  stocks <- unique(plot_df$Stock)
  other_spp <- setdiff(stocks[!startsWith(stocks, "RB ")], names(spp_cols))
  if (length(other_spp) > 0) {
    spp_cols <- c(spp_cols, stats::setNames(rep(other_cols, length.out = length(other_spp)), sort(other_spp)))
  }
  fill_vals <- c(spp_cols, rb_cols)
  # Legend order: species alphabetically, then RB strains
  fill_breaks <- c(sort(stocks[!startsWith(stocks, "RB ")]), rb_lv[rb_lv %in% stocks])

  # Lifestage borders. Other lifestages (e.g. adults) get a brown border.
  ls_cols <- c(FR = "grey85", YE = "grey50", CA = "black")
  other_ls <- setdiff(unique(plot_df$Lifestage), names(ls_cols))
  if (length(other_ls) > 0) ls_cols <- c(ls_cols, stats::setNames(rep("#8B4513", length(other_ls)), other_ls))
  ls_breaks <- c(intersect(c("FR", "YE", "CA"), unique(plot_df$Lifestage)), sort(other_ls))

  ploidy_alpha <- c("2N / AF2N" = 1, "3N / AF3N" = 0.5)
  ploidy_breaks <- intersect(names(ploidy_alpha), unique(plot_df$Ploidy))
  # Legends only list what was released in the lakes and years plotted
  n_fill <- length(fill_breaks)
  fill_nrow <- if (n_fill > 3) 2 else 1

  y_lab <- switch(measure,
                  quantity    = "Number released",
                  quantity_ha = "Number released per ha",
                  biomass_kg  = "Biomass released (kg)")

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$release_year, y = .data$value, group = .data$seg)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$Stock, colour = .data$Lifestage, alpha = .data$Ploidy),
                      width = 0.85, linewidth = 0.7,
                      position = ggplot2::position_stack()) +
    ggplot2::geom_text(ggplot2::aes(label = .data$mark),
                       position = ggplot2::position_stack(vjust = 0.5),
                       size = label_size, fontface = "bold") +
    ggplot2::facet_wrap(~ Lake, ncol = ncol, scales = "free_y", drop = TRUE,
                        strip.position = "right") +
    ggplot2::scale_fill_manual(values = fill_vals, limits = fill_breaks, breaks = fill_breaks, name = "Species /\nRB strain") +
    ggplot2::scale_colour_manual(values = ls_cols, limits = ls_breaks, breaks = ls_breaks, name = "Lifestage\n(border)") +
    ggplot2::scale_alpha_manual(values = ploidy_alpha, limits = ploidy_breaks, breaks = ploidy_breaks, name = "Ploidy") +
    ggplot2::guides(
      fill   = ggplot2::guide_legend(order = 1, nrow = fill_nrow, byrow = FALSE,
                                     override.aes = list(colour = "grey30", alpha = 1, linewidth = 0.3)),
      colour = ggplot2::guide_legend(order = 2, nrow = if (length(ls_breaks) > 2) 2 else 1, byrow = FALSE,
                                     override.aes = list(fill = "grey60", alpha = 1, linewidth = 1.2)),
      alpha  = ggplot2::guide_legend(order = 3, nrow = if (length(ploidy_breaks) > 1) 2 else 1,
                                     override.aes = list(fill = "grey30", colour = "grey30"))
    ) +
    ggplot2::scale_x_continuous(limits = c(min_year - 0.5, max_year + 0.5),
                                breaks = function(lims) {
                                  b <- pretty(lims)
                                  b[b == round(b)]
                                },
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(n.breaks = 4, labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE),
                                expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Release year", y = y_lab,
                  caption = "Text on bars = mark code") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   legend.box = "horizontal",
                   legend.title = ggplot2::element_text(size = 8, face = "bold"),
                   legend.text = ggplot2::element_text(size = 7),
                   legend.key.size = ggplot2::unit(0.9, "lines"),
                   legend.spacing.x = ggplot2::unit(0.2, "lines"),
                   legend.box.spacing = ggplot2::unit(0.3, "lines"),
                   legend.margin = ggplot2::margin(0, 6, 0, 6),
                   plot.caption = ggplot2::element_text(size = 7, colour = "grey40"),
                   strip.text.y.right = ggplot2::element_text(face = "bold", angle = -90),
                   strip.background = ggplot2::element_rect(fill = "grey92"),
                   panel.spacing = ggplot2::unit(0.4, "lines"),
                   panel.grid.minor = ggplot2::element_blank())
}
