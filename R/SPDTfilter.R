#' A function to facilitate rapid filtering SPDTdata to specific cases beyond the biological references (lakes, years, project)
#'
#'
#' This is convenience data filtering and cleaning process when doing exploratory analysis or plotting with SPDTdata.
#'
#'
#' @title SPDTfilter
#' @name SPDTfilter
#' @keywords SPDT
#' @export
#' @param minN an optional integer or vector describing the minimum acceptable catch within a sample_event for the data to be included
#' @param Lk_yrs an optional character string or character vector of the Lake-years to filter the data set to. Must be in format WBID_YYYY (e.g. "01100OKAN_2020" or for multiples c("01100OKAN_2020", "01598LNTH_2018"))
#' @param Lake_names an optional character string or character vector describing the lakes to include in data.
#' @param WBIDs an optional character string or character vector describing the WBIDs to include in data.
#' @param Years an optional integer or vector describing the sampling years to include in data.
#' @param Regions an optional character string or character vector describing the management regions to include in data.
#' @param Sample_events an optional character string or character vector describing the sampling events to include in data.
#' @examples
#' #Must be connected to VPN if working remotely
#'
#' #Download all data with clipped fish
#' SPDTdata(Contrast = "Strain")
#'
#' #Typically we would call this within the SPDTdata (or SPDTplot) call
#' SPDTdata(Contrast = "Strain", filters = SPDTfilter(Lake_names = "MATHEW"))
#
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


SPDTfilter <- function(
    Sample_events = NULL,
    Lk_yrs = NULL,
    Lake_names = NULL,
    WBIDs = NULL,
    Years = NULL,
    Regions = NULL,
    minN = NULL
) {

  # start with all events
  keep <- unique(Biological$Sample_event)

  if (!is.null(Sample_events)) {
    keep <- intersect(
      keep,
      Biological$Sample_event[Biological$Sample_event %in% Sample_events]
    )
  }

  if (!is.null(Lake_names)) {
    keep <- intersect(
      keep,
      Biological$Sample_event[Biological$locale_name %in% Lake_names]
    )
  }

  if (!is.null(WBIDs)) {
    keep <- intersect(
      keep,
      Biological$Sample_event[Biological$WBID %in% WBIDs]
    )
  }

  if (!is.null(Years)) {
    keep <- intersect(
      keep,
      Biological$Sample_event[Biological$Year %in% Years]
    )
  }

  if (!is.null(Regions)) {
    keep <- intersect(
      keep,
      Biological$Sample_event[Biological$Region %in% Regions]
    )
  }

  if (!is.null(minN)) {
    minNly <- Biological %>%
      dplyr::group_by(Sample_event) %>%
      dplyr::summarize(N = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(N >= minN) %>%
      dplyr::pull(Sample_event) %>%
      unique()

    keep <- intersect(keep, minNly)
  }

  return(keep)
}
