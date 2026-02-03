#' A plot function for SPDT data when a contrast has been specified.
#' Transferred form teh depricated SPDT package
#' Function only usable by FFSBC staff who have a direct or vpn connection to SLD.
#'
#'
#'
#'
#'
#' @title SPDTplot
#' @name SPDTplot
#' @keywords SPDT, DP2R, SPDTplot, plot
#' @export
#' @param Metric a character string that describes the performance metric (dependent variable) to be plotted.This must be entered for function to work.
#' The specific options that can be stated are:
#' "survival" (standardized relative catch rate of one contrast group to another),
#' A series of plots that summarize all age classes captured in a given sampling event.
#' The plots show changes in size, numbers or maturation over time if multiple age classes were captured.
#'
#' "survival' (relative survival of each group, value of 1 equivalent, estimated relative survival shown by dotted line)
#'
#' "catch" (catch rates of each group - catch curve type data),
#'
#' "age_freq" (same data as above, but columns instead of points),
#'
#' "mu_growth_FL" (average length per age),
#'
#' "mu_growth_wt" (average weight per age),
#'
#' "growth_FL" (individual lengths per age),
#'
#' "growth_wt" (raw weights by age),
#'
#' "maturation" (the proportion mature per age),
#'
#' "maturation_by_sex" (partitions data used in plot above by sex)
#'
#' A series of plots looking at the overall size distribution of the entire population in a lake (all age classes)
#'
#' "FL_freq" (fork-length frequencies per lake-sample session as lines),
#'
#' "FL_density"(fork-length frequencies per lake-sample session as smoothed densities),
#'
#' "FL_hist"(fork-length frequencies per lake-sample session as histograms),
#'
#' In all cases the data will be grouped by the "Contrast" stated during the SPDT data call.
#' @param Method a character string describing the capture method. Defaults to "GN" (gillnet), but any other capture method code fo rmthe database is acceptable.
#' @param Ages a numeric vector or value of the ages to include in plotting
#' @param min_N an integer to set a minimum sample size to include in plots. This sample size applies to each contrast group.
#' In other words, even if one group meets the minimum sample size, if the gourp it is being contrasted against does not, then that contrast (Lk_yr_age) will be removed.
#' @param min_groups an integer to set a minimum number of contrast groups meeting the minimum sample size(min_N) within the grouping levels of the gdf data frame (lake, year, age) to include in plots.
#' @param filters a vector of lake-years returned from the SPDTfilter() function. SPDTfilter() allows for filtering to various non-biological aspects to the data, lakes, years, regions, et.c See?SPDTfilter()
#' @param save_pdf a logical TRUE/FALSE indicating whether a copy of the plot should be saved with the filename Metric.pdf
#' @examples
#' #Must be connected to VPN if working remotely
#'
#' #Download all data from lake years that had a genotype comparison for KO
#' SPDTdata(Spp = "KO", Contrast = "Genotype")
#'
#' Plot the relative survival of the contrast groups
#' SPDTplot(Metric = "survival")
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data


SPDTplot <- function(Metric = NULL, Method = "GN", Ages = c(0:100), min_N = 3, min_groups = 1, filters = NULL, save_pdf = FALSE){

  #If no specific contrast is given in SPDT data, then make defaults for colouring and shape schemes

  if (is.null(Contrast)){
    Contrast = "Geno_rel"
    controls = c("Strain_rel")
  }


  plot_wide = wide_df%>%dplyr::filter(method %in% Method, age %in% Ages, N >=min_N)

  #Some name simplification for plotting.
  plot_wide = plot_wide%>%
    dplyr::rowwise()%>%
    dplyr::mutate(short_name = substr(locale_name, 1, 4))#,
          # Other_spp = dplyr::if_else(grepl("NS",Spp_caught),"Pikeminnow",Spp_class))


  plot_idf = idf%>%dplyr::filter(method %in% Method,!is.na(N_ha_rel), age %in% Ages)#min_N is filtered by lake_yr later.
  plot_gdf = gdf%>%dplyr::filter(method %in% Method,!is.na(N_ha_rel), age %in% Ages, N >=min_N)


  if (!is.null(filters)) {
    plot_wide = dplyr::filter(wide_df, Lk_yr %in% filters)
    plot_idf = dplyr::filter(plot_idf, Lk_yr %in% filters)
    plot_gdf = dplyr::filter(plot_gdf, Lk_yr %in% filters)
  }


#The survival plot uses a different data set than all the other plots. min_N is less relevant at each strata because N = 0 is a valid observation for survival.


  if (Metric == "survival"){

 p =  ggplot2::ggplot(data = plot_wide, ggplot2::aes(x = .data$short_name, y = .data$Recap_p, group = age, shape = get(controls[1])))+
    ggplot2::geom_hline(yintercept = 0.5)+
    #ggplot2::geom_point(ggplot2::aes(y = .data$surv_diff, size = .data$N), stroke = 1, alpha = 0.7, position = ggplot2::position_dodge(width = 0.5))+
    ggplot2::geom_point(ggplot2::aes( size = .data$N, fill = Non_salm), stroke = 1, alpha = 0.7, position=ggplot2::position_dodge(width=.5))+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = LCI, ymax = UCI), lwd = .5, width = 0, position=ggplot2::position_dodge(width=0.5))+
    viridis::scale_fill_viridis(discrete = TRUE)+
    #ggplot2::geom_line(ggplot2::aes(y = .data$avg_surv), lty = 2, lwd = 1.2)+
    viridis::scale_colour_viridis(discrete = TRUE)+
    #ggplot2::scale_y_continuous(trans='log10')+#,expand=c(0.1,0.1))+
    ggplot2::labs(y = "Proportion top group in catch", x = "Lake", shape  = controls[1],  fill = "Non-salmoninae")+
    ggplot2::scale_shape_manual(values = rep(21:25, 5))+
    #ggplot2::facet_wrap(~.data$Comparison, scales = "free_x")+
    ggplot2::facet_grid(~.data$a+.data$b, scales = "free_x", space = "free")+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, hjust=1))+
    ggplot2::guides(fill=ggplot2::guide_legend(override.aes=list(shape=21)))+
    ggplot2::guides(size = "none")
  }



  #Filter to remove age classes that don't have releases for multiple levels of Contrast
  #Filter to min_N requirements at level of Lk_yr, age, season


  plot_gdf <- plot_gdf%>%
    dplyr::mutate(Year_Season = paste0(year,"_",Season),
                  SAR_cat = as.factor(SAR_cat))%>%
    dplyr::group_by(Lk_yr_age, Season)%>%
    dplyr::filter(min_groups<=length(unique(get(Contrast))))%>%#, sum(N)>min_N*length(unique(get(Contrast)))%>%#If min_N is ) still need at least one group to be > 0
    dplyr::ungroup()
  #The filter above should ensure there are at least 2 groups in contrast above the min_N

  #min_N for idf does not filter out individual age classes (because the x-axis is usually just length), and binned by Lk_yr and season. So min_N is by Lk_yr and season
  plot_idf <- plot_idf%>%
    dplyr::mutate(Year_Season = paste0(year,"_",Season),
                  SAR_cat = as.factor(SAR_cat))%>%
    dplyr::group_by(Lk_yr, Season)%>%
    dplyr::filter(paste0(Lk_yr, Season)%in% paste0(plot_gdf$Lk_yr, plot_gdf$Season))%>%#this is an option if want ot match plot_gdf data. Use Lk_yr_Age to b emost strict
    #dplyr::filter(dplyr::n()>min_N)%>%
    dplyr::ungroup()

  #Could add an additional filter [!is.na(plot_idf$length_mm),]
  #But it is better to get the warning I think. It still plots

  #Should add an option to include or eliminate outliers.
  #dplyr::filter(Outlier %in% c(0,NA))


  #Make colour and fill = to Contrast always.

#Base ggplot call for a plots based on group data with age as x-axis
pg = ggplot2::ggplot(data = plot_gdf, ggplot2::aes(x = .data$Dec.Age, fill = get(Contrast), colour = get(Contrast), shape = get(controls[1])))+#, y = .data$NetXN
    ggplot2::scale_shape_manual(values = rep(21:25, 5))+
    viridis::scale_fill_viridis(discrete = TRUE)+
    viridis::scale_colour_viridis(discrete = TRUE)+
    ggplot2::scale_y_continuous(limits = c(0,NA),expand=ggplot2::expansion(add=c(0,10))) +
    ggplot2::facet_wrap(~.data$locale_name+.data$Year_Season, scales = "fixed")+
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(1))+
    ggplot2::labs(x = "Age", fill = Contrast, colour = Contrast, shape = controls[1])+
    ggplot2::theme_bw()+
    ggplot2::theme(strip.text = ggplot2::element_text(size = 8, margin = ggplot2::margin(1, 1, 1, 1)))

#Catch plot additions.
if (Metric == "catch"){
p = pg +
    ggplot2::geom_point(ggplot2::aes(y = .data$NetXN),size = 3, alpha = 0.6, position = ggplot2::position_dodge(width = 0.2))+
    #ggplot2::scale_y_continuous(trans='log10')+
    ggplot2::labs(y = "Catch (selectivity adjusted)")+
  ggplot2::facet_wrap(~.data$locale_name+.data$Year_Season, scales = "free_y", ncol = 4)
}

#Avg growth plot additions.
if (Metric == "mu_growth_FL"){
  p = pg +
    ggplot2::geom_point(ggplot2::aes(y = .data$NetX_FL),size = 2.5, alpha = 0.7, position = ggplot2::position_dodge(width = 0.5))+
    ggplot2::geom_errorbar(ggplot2::aes(ymin=.data$NetX_FL-.data$sd_FL, ymax=.data$NetX_FL+.data$sd_FL), width=.2, position=ggplot2::position_dodge(.5))+
    ggplot2::scale_y_continuous(breaks = scales::breaks_width(50))+#, limits = c(100,NA))+
    ggplot2::labs(y = "Fork length (mm, selectivity adjusted)")
}

if (Metric == "mu_growth_wt"){
  p = pg +
    ggplot2::geom_point(ggplot2::aes(y = .data$NetX_wt),size = 2.5, alpha = 0.7, position = ggplot2::position_dodge(width = 0.5))+
    ggplot2::geom_errorbar(ggplot2::aes(ymin=.data$NetX_wt-.data$sd_wt, ymax=.data$NetX_wt+.data$sd_wt), width=.2, position=ggplot2::position_dodge(.5))+
    #ggplot2::scale_y_continuous(breaks = scales::breaks_width(100), limits = c(0,NA))+
    ggplot2::labs(y = "Weight (g, selectivity adjusted)")
}

if (Metric == "age_freq"){

  p = pg +
    ggplot2::geom_col(ggplot2::aes(y  =.data$NetXN),position = ggplot2::position_dodge2(preserve = "single"), alpha = 0.6)+
    ggplot2::facet_wrap(.data$locale_name~.data$Year_Season, scales = "free_y")+
    #ggplot2::scale_y_continuous(breaks = scales::breaks_width(10))+
    ggplot2::labs(y = "Catch (selectivity adjusted)")+
    ggplot2::theme_bw()
}

if (Metric == "maturation"){
  p = pg +
    ggplot2::geom_point(ggplot2::aes(x = .data$Dec.Age, y = .data$p_mat, size = log(.data$N)), alpha = 0.7, position = ggplot2::position_dodge(width = 0.2))+
    #ggplot2::facet_wrap(~get(controls[1]))+
    ggplot2::facet_wrap(~.data$locale_name)+
    ggplot2::ylim(0,1)+
    ggplot2::geom_smooth(se = FALSE, ggplot2::aes(y = .data$p_mat,colour = get(Contrast)), method = "glm", method.args = list(family = "binomial"))+
    ggplot2::theme_bw()+
    ggplot2::labs(x = "Age", y = "Proportion mature or maturing",
                  shape = controls[1], fill = Contrast, colour = Contrast)
}

##___________________________________________________________________________
##INDIVIDUAL LEVEL (raw data) PLOTS

  #plot_idf = plot_idf%>%filter(locale_name %in%c("NESS", "VIVIAN2", "WAPITI", "COBB", "FUSEE", "YELLOW", "TULIP"))
  #maxFL = max(plot_idf$length_mm, na.rm = T) + (20 - max(plot_idf$length_mm, na.rm = T) %% 20)

pi = ggplot2::ggplot(data = plot_idf, ggplot2::aes(colour = get(Contrast), fill = get(Contrast), shape = get(controls[1]), group=get(Contrast)))+
  ggplot2::scale_shape_manual(values = rep(21:25, 5))+
  viridis::scale_fill_viridis(discrete = TRUE)+
  viridis::scale_colour_viridis(discrete = TRUE)+
  ggplot2::facet_wrap(.data$locale_name~.data$Year_Season)+
  ggplot2::labs(fill = Contrast, colour = Contrast, shape = controls[1])+
  ggplot2::theme_bw()




if(Metric == "condition"){
  #plot_idf = plot_idf[0<plot_idf$weight_g,]

  #plot_idf$logW = log(plot_idf$weight_g)
  #plot_idf$logL = log(plot_idf$length_mm)
  #lm <- lm(data=plot_idf,logW ~ logL + as.factor(Lk_yr) + Strain)
  #ggiraphExtra::ggPredict(lm,se=TRUE,interactive=TRUE)
  #preds = ggeffects::ggpredict(lm, terms = c("Lk_yr", "logL"))
  #ggeffects::plot(preds)
  #plot(preds, add.data = TRUE, facet = TRUE)

p = pi +
  ggplot2::geom_point(ggplot2::aes(x = .data$length_mm, y = .data$weight_g), size = 3, alpha = 0.5)+
  ggplot2::scale_x_log10()+
  ggplot2::scale_y_log10()#+
  #ggplot2::facet_wrap(.data$locale_name~.data$Year_Season)#Don't want scales free so repeated facet_wrap line
}



 if (Metric == "growth_wt"){

p = pi +
     ggplot2::geom_point(ggplot2::aes(x = .data$Dec.Age, y = .data$weight_g), size = 2.5, alpha = 0.5, position = ggplot2::position_dodge(width = 0.3))+
     ggplot2::labs(x = "Age", y = "Weight (g)", fill = Contrast, colour = Contrast)+
     ggplot2::scale_y_continuous(breaks = scales::breaks_width(200))+#, limits = c(0,NA))+
     ggplot2::scale_x_continuous(breaks = scales::breaks_width(1))+
     ggplot2::theme_bw()#+
     #ggplot2::facet_wrap(.data$locale_name~.data$Year_Season, scales = "free_y")
 }

if (Metric == "growth_FL"){

  p = pi +
    ggplot2::geom_point(ggplot2::aes(x = .data$Dec.Age, y = .data$length_mm), size = 2.5, alpha = 0.5, position = ggplot2::position_dodge(width = 0.3))+
    ggplot2::labs(x = "Age", y = "Fork Length (mm)", fill = Contrast, colour = Contrast)+
    ggplot2::scale_y_continuous(breaks = scales::breaks_width(50))+#, limits = c(100,NA))+
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(1))+
    ggplot2::theme_bw()#+
    #ggplot2::facet_wrap(.data$locale_name~.data$Year_Season, scales = "free_y")
}



  if (Metric == "FL_age_facets"){
    p = pi+
      ggplot2::geom_point(ggplot2::aes(x = .data$locale_name, y = .data$length_mm),size = 2.5, alpha = 0.5, position = ggplot2::position_dodge(width = 0.75))+
      ggplot2::facet_wrap(~.data$Dec.Age, scales = "free")+
      ggplot2::labs(x = "", y = "Fork Length (mm)", fill = Contrast)+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, vjust = 1, hjust=1))
  }





  if (Metric == "FL_density"){
    if(min_N<20){warning("Min_N should be set >=20 for pop length frequency plots")}

p = pi+
    ggplot2::geom_density(ggplot2::aes(x = .data$length_mm), alpha = 0.4, lwd = 1, adjust = 1/2)+
    ggplot2::xlim(c(100,NA))+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.text.y=ggplot2::element_blank(), axis.ticks.y=ggplot2::element_blank())+
    ggplot2::labs( fill = Contrast, colour = Contrast)


  }

if (Metric == "FL_freq"){
  if(min_N<20){warning("Min_N should be set >=20 for pop length frequency plots")}

  p = pi+
    ggplot2::geom_freqpoly(ggplot2::aes(x = .data$length_mm), alpha = 0.9, lwd = 1, binwidth = 20)+
    ggplot2::xlim(c(100,NA))+
    ggplot2::theme_bw()+
    ggplot2::labs(colour = Contrast)+
    ggplot2::guides(colour=ggplot2::guide_legend(override.aes = list(line = 2)),fill="none")

}

if (Metric == "FL_hist"){
  if(min_N<20){warning("Min_N should be set >=20 for pop length frequency plots")}

  p = pi+
    ggplot2::geom_histogram(ggplot2::aes(x = .data$length_mm), alpha = 0.2, lwd = 1, position = "identity", binwidth = 20)+
    ggplot2::xlim(c(100,450))+
    ggplot2::theme_bw()+
    ggplot2::labs( fill = Contrast, colour = Contrast)

}

if (Metric == "wt_hist"){
  if(min_N<20){warning("Min_N should be set >=20 for pop length frequency plots")}

  p = pi+
    ggplot2::geom_histogram(ggplot2::aes(x = .data$weight_g), alpha = 0.2, lwd = 1, position = "identity", binwidth = 20)+
    ggplot2::xlim(c(100,450))+
    ggplot2::theme_bw()+
    ggplot2::labs( fill = Contrast, colour = Contrast)

}



  if (Metric == "maturation_by_sex"){
    if(min_N<5){warning("Min_N should be set >=5 for maturation plots")}
    mat_df = plot_idf%>%
              dplyr::filter(maturity != 'UNK', !is.na(sex), !grepl(",",Geno_rel))%>%
              dplyr::group_by(locale_name, WBID, year, species_code, Strain_rel, Geno_rel, age, Dec.Age, sex, SAR_cat)%>%
              dplyr::summarize(N = dplyr::n(),
                               Nkn = sum(.data$maturity != 'UNK', na.rm = TRUE),
                               Nm = sum(.data$maturity != 'IM'& .data$maturity != 'UNK', na.rm = TRUE),
                               p_mat = sum(.data$maturity != 'IM'& .data$maturity != 'UNK', na.rm = TRUE)/sum(.data$maturity != 'UNK', na.rm = TRUE))%>%
              dplyr::filter(N>=min_N)%>%
              dplyr::ungroup()


    p = ggplot2::ggplot(data = mat_df,
                        ggplot2::aes(x = .data$Dec.Age, y = .data$p_mat,
                                     fill = get(Contrast),
                                     colour = get(Contrast)))+
      ggplot2::geom_point(aes(size = log(.data$N)),alpha = 0.7, position = ggplot2::position_jitterdodge(  jitter.width = .1,
                                                                                            jitter.height = 0.02,
                                                                                            dodge.width = 0.3, seed = 1))+
      ggplot2::scale_shape_manual(values = 21)+
      viridis::scale_fill_viridis(discrete = TRUE)+
      viridis::scale_colour_viridis(discrete = TRUE)+
      ggplot2::facet_grid(.data$sex~.data$Geno_rel)+
      ggplot2::ylim(0,1)+
      ggplot2::scale_x_continuous(breaks = seq(min(mat_df$age, na.rm = T), max(mat_df$age, na.rm = T), by = 1))+
      ggplot2::geom_smooth(span = 1, se = FALSE, ggplot2::aes(colour = get(Contrast)), method = "glm", method.args = list(family = "binomial"))+
      ggplot2::theme_bw()+
      ggplot2::guides(fill=ggplot2::guide_legend(override.aes=list(shape=21)))+
      ggplot2::labs(x = "Age", y = "Proportion mature or maturing",
                    shape = "Sex", fill = Contrast, colour = Contrast)+
      ggplot2::guides(size = "none")
  }



#Save a .png of plot

if(save_pdf == TRUE){
  filename = paste0("plot_",Metric,".pdf")

  #In all cases above the Waterbody is used to facet, so the plot facets are arranged as:
  facets = 1
  if(class(p$facet)[1] != "FacetNull"){
    facets = length(unique(p$data$locale_name))
  }
  ncol <- ceiling(sqrt(facets))
  nrow <- ceiling(facets/ncol)


  ggplot2::ggsave(filename, p, dpi = "print", width = 7, height = 7*(nrow/ncol)*.95, units = "in", device = "pdf")

}

  #print(p)
  return(p)




}

##FUTURE CONSIDERATIONS







#FOr model plotting functions to work, need to convert factors outside of plotting formula
#idf$age = as.factor(idf$age)

#model = glm(data = idf, length_mm~Lk_yr+(age)*Genotype)
#summary(model)
#sjPlot::plot_model(model, type = "eff", terms = c("age", "Genotype"))

#jtools::effect_plot(model, pred = c(Genotype,age), plot.points = TRUE)+
#  ggplot2::facet_wrap(~age)

#model = glm(data = idf, weight_g~Lk_yr+as.factor(age)*Genotype)
#summary(model)
#sjPlot::plot_model(model, type = "pred", terms = c("age", "Genotype"))



# if (!is.null(Method)) {
#   #idf = subset(idf, method %in% Method)
#   plotdf = subset(gdf, method %in% Method)
## } else{
#   plotdf = gdf%>%dplyr::filter(!is.na(N_rel))
# }

#Sections to pull specific contrasts into wide format for survival comparisons.
#Using controls is risky becasue we rarely acheive the same SAR, etc.
#If a 1:1 stocking objective was achieved then the need for this disappears, and can just use glm

#f = paste("NetXN ~ Lk_yr + age*",Contrast)

#plot_gdf$age = as.factor(plot_gdf$age)

#model = stats::glm(as.formula(f), data = plot_gdf)
#plot(model)

#sjPlot::plot_model(model, type = "pred")
#jtools::effect_plot(model, pred = age, plot.points = TRUE, data = plot_gdf)
#jtools::effect_plot(model, pred = Genotype, plot.points = TRUE, data = plot_gdf)

#get(Contrast)


#if (!is.null(Contrast)) {

#  Contrast_possible = c("Genotype", "SAR_cat", "Strain")

#  controls = dplyr::setdiff(Contrast_possible, Contrast)
#}


#  surv_gdf = plot_gdf%>%
#    dplyr::group_by(locale_name, Lk_yr, sby_code, age, !!!rlang::syms(controls), !!!rlang::syms(Contrast))%>%
#   dplyr::summarize(groups = dplyr::n(), xN = sum(NetXN), Nr = sum(N_rel))%>%
#   tidyr::pivot_wider(names_from = Contrast, values_from = c(xN, Nr), names_sort = TRUE)



