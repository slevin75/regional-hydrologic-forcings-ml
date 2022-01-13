
calc_moving_window_metrics<-function(site_num, window_length,increment, min_yrs_in_window,clean_daily_flow,yearType, 
                                     drainArea_tab, NE_probs, digits = 3 ){
  ##subset clean_daily_flow, create a vector of starting years for a moving window analysis, and then
  ##call the calcFDC function for each starting year.
  data <- clean_daily_flow %>%
    filter(site_no == site_num)
  
  
  start_yrs<- seq(from = min(data$year_val), 
                  to= (max(data$year_val) - window_length+1),
                  by = increment)

  ##remove any starting years which would result in a window with no data (if there are large data gaps). 
  df_screen<-bind_rows(map(start_yrs,window_screen,data))
  start_yrs<-start_yrs[start_yrs %in% df_screen$start_yr[which(df_screen$years_in_window > min_yrs_in_window)]==TRUE]
  
  
  map_out<-map(start_yrs,calc_FDC_subset, 
               window_length, site_num=site_num, 
               clean_daily_flow=data,
               yearType = yearType,
               drainArea_tab=drainArea_tab,
               NE_probs=NE_probs,digits=3)
  data_out<-bind_rows(map_out)
 
  return(data_out)
}


plot_trend_summary<-function(moving_window_metrics,outdir){
  #normalize metrics and remove NAs (when there is only 1 moving window
  #for a site,the sd will be 0)
  df_norm<-moving_window_metrics %>%
    group_by(site_num,indice) %>%
    mutate(norm = (statistic - mean(statistic))/sd(statistic))%>%
    mutate(indice_grp=word(indice,start=1,sep="_") )%>%
    filter(!is.na(norm))
  
  indice_grp<-unique(df_norm$indice_grp) 
  ##map over indice_grp and produce a plot file for each group, with all the quantiles 
  map_out<-map(indice_grp,make_summary_plot,
          data=df_norm,
          outdir= outdir)%>%
    unlist()

  return(map_out)
}#end function


make_summary_plot<- function(grp,data, outdir){
  df_plot<-data %>%
    filter(indice_grp ==grp)
  
  p1<-ggplot(df_plot,aes(start_Year,norm)) +geom_point(size = .7,alpha = .2)+
    geom_smooth()+
    facet_wrap(~indice, ncol=3)
  
  filepath <- file.path(outdir, paste0("moving_window_summary_", grp, ".png"))
  
  ggsave(filename=filepath,
         plot= p1)
  return(filepath)
}




calc_FDC_subset<- function(start_yr, window_length,site_num, clean_daily_flow, yearType, 
                           drainArea_tab, NE_probs, digits = 3 ){

 ##subset the clean_daily_flow data

  sub_clean_daily_flow<- clean_daily_flow %>%
    filter((year_val >= start_yr) &(year_val < (start_yr + window_length)))
  
  df_FDC<- calc_FDCmetrics(site_num, 
                           clean_daily_flow=sub_clean_daily_flow,
                           yearType,drainArea_tab, NE_probs = NE_quants, 
                           digits = 3)

  ##append the start year and the number of years in the computation (in case there are missing yrs in 
  ##the data, can remove these later if there are too few years in the window)
  df_FDC$start_Year <- start_yr
  df_FDC$window_length <- window_length
  df_FDC$year_ct<-length(unique(sub_clean_daily_flow$year_val))
  return(df_FDC)
}

screen_plot_sites<-function(moving_window_metrics, min_windows){

  ##screen out any sites that do not have enough moving windows to plot
  ##using 
  metrics_screen<-moving_window_metrics %>% 
    group_by(site_num,indice) %>%
    count()
  "ok1"
  plot_sites<-metrics_screen %>%
    filter(n> min_windows) %>%
    pull(site_num)
  
  plot_sites<-unique(plot_sites)
}




make_plots_by_site<-function(site, moving_window_metrics, outdir){
  print(site)
  metrics<-moving_window_metrics %>%
    filter(site_num==site)
  if(nrow(metrics)> 0){
    metrics$index_grp<-word(metrics$indice,start=1,sep="_")
    metrics$quantile<-as.factor(word(metrics$indice,start=-1,sep="_q"))
  
    ##I tried a line plot here too but the loess plots were just easier to look at and
    ##get a general idea of the trend.  The line plots got very messy looking but sometimes
    ##the loessdoes weird things if there are lots of gaps in the data, so idk which is better.
    p1<-ggplot(metrics,aes(start_Year,statistic,color=quantile))+
      geom_point(size= .5) + facet_wrap(~index_grp,scales="free")+
      geom_smooth(se=FALSE)+
      theme(legend.position="bottom")
    filepath <- file.path(outdir, paste0(site, "_",window_length,"yr_moving_window_plots.png"))
    ggsave(filename=filepath,
           plot= p1)
    return(filepath)
  }
}

window_screen<-function(start_yr, data){
  ##screener to count the number years of data within a window.
  ##using this to select out any starting years that result in a window with 
  #no data or with too few years of data to be reliable
  
  x<-data %>% 
    filter((year_val >= start_yr) &(year_val < (start_yr + window_length)))
  df<-data.frame(start_yr=start_yr,years_in_window=length(unique(x$year_val)) )
  return(df) 
}
