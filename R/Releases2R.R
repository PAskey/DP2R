#' Ultimately, as upload filters and cleaning are improved in the main database, this function will become obsolete.
#'
#' @title Releases2R
#' @name Releases2R
#' @keywords SPDT; DP2R
#' @export
#' @param Requests a True/False indicator on whether to upload request information as well
#' @examples
#' Releases2R()
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data



#_______________________________________________________________________________
#Open channel to SLD and download data. Make sure OpenVPN GUI is running
Releases2R <- function(Requests = FALSE){

  if(!exists("vwWaterbodyLake")){DP2R(Tables = "vwWaterbodyLake")}

  ch <- RODBC::odbcDriverConnect('driver={SQL Server};server=FFSBCSQL06;
                        DSN=SMALL_LAKES-TEST;DATABASE=SMALL_LAKES-TEST;
                        Trusted_Connection=TRUE')

  Releases <-RODBC::sqlFetch(ch,"ffsbc.vw_paris_releases", na.strings=c("","NA"))

  if(Requests){
  Requests <-RODBC::sqlFetch(ch,"ffsbc.vw_paris_requests", na.strings=c("","NA"))
              }

  close(ch)

  #Releases dates have been uploaded as factors without obvious errors
  #I believe very old records are accurate bass and lake whitefish stockings.
  #Releases$rel_Date<-as.POSIXct(Releases$rel_Date, format="%Y-%m-%d")

  ####################################################################################################################
  #Recode Genotypes and Strain


  Releases <- Releases%>%dplyr::rename(WBID = loc_msrm_waterbody_identifier,
                                       ploidy = stock_gtype_code,
                                       species_code = sp_code,
                                       mark_code = rel_fm_code)

  #_______________________________________________________________________________
  #RELEASES
  #Several minor adjustments

  #Remove stream releases as generally do not apply to SPDT type analyses
  Releases <- Releases%>%dplyr::filter(!(grepl("00000",.data$WBID))&.data$WBID!="")%>%droplevels()

  #Lake names were not exactly matching between releases and biological data, which required this code to use Biological Waterbody names
  Releases$gazetted_name = vwWaterbodyLake$gazetted_name[match(Releases$WBID, vwWaterbodyLake$WBID)]
  #Add region
  Releases$region = vwWaterbodyLake$region[match(Releases$WBID, vwWaterbodyLake$WBID)]

  correct_FV_sby = function(so_origin_code, sby_code){
    x = dplyr::case_when(so_origin_code != "BR"&sby_code<2013~sby_code-1,
                         TRUE ~ sby_code)
    return(x)
  }
  #Lookup strain codes and insert into releases to make consistent with Biological strain codes. Add Year variable for release year
  Releases<-Releases%>%
    dplyr::mutate(Strain = as.character(plyr::mapvalues(stock_strain_loc_name,
                                                        from=as.character(SPDT::Strain_code_LU$stock_strain_loc_name),
                                                        to=as.character(SPDT::Strain_code_LU$Strain),
                                                        warn_missing = FALSE)),
                  year = as.integer(format(as.Date(Releases$rel_Date,
                                                   format = "%Y-%m-%d"), "%Y")),
                  #sbycode = dplyr::if_else(Strain=="FV",correct_FV_sby(so_origin_code,sby_code),sby_code)
    )

  Releases<<-Releases

}
