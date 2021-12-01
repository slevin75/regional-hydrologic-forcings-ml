
get_reference_sites<-function(inpath,region){
  ref_gages<- read.csv(inpath,header=TRUE)
  ##subset gages to get all western mnt sites
  sites<- ref_gages %>% 
    filter(AggEco %in% region) %>%
    select(ID)%>%
    mutate(sites=substr(ID,start=2,stop=nchar(ID))) %>%
    pull(sites)
  ##turns out, not all the daily value sites have peak flow. So, 
  ##I am screening out the ones that don't have peak flow
  has_peaks<-whatNWISdata(siteNumber=sites,service="pk")
  has_dv<-whatNWISdata(siteNumber=sites, service="dv",parameterCd=NWIS_parameter)
  ###for CO basins, I'm removing the pacific northwest and california portion of the 
  ##ecoregion.  For DE basins, removing everything south of latitude 36
  if ("WestMnts" %in% region == TRUE){
    has_dv<-has_dv[which(has_dv$dec_long_va > -118),]
    sites<-sites[((sites %in% has_peaks$site_no)& (sites %in% has_dv$site_no))]
  } else {
    has_dv<-has_dv[which(has_dv$dec_lat_va > 36),]
    sites<-sites[((sites %in% has_peaks$site_no)& (sites %in% has_dv$site_no))]
  }
  ##also screening out the following sites which say they have daily data in 
  ##NWIS but actually don't have any daily discharge data. Not sure what is going on
  ##there.
  rm.sites<-c("03433637","03597210")
  sites<-sites[which (sites %in% rm.sites == FALSE)]
  return(sites)
}



get_nwis_daily_data<-function(site_num,outdir,parameterCd,startDate, endDate){
   ##read daily NWIS data, and save to csv file.
  data_out<-readNWISdv(site_num, parameterCd, startDate, endDate)
  ##renaming column names for discharge and discharge_cd
  names(data_out)[grep(pattern="X_.*\\cd",x=names(data_out))]<-"discharge_cd"
  names(data_out)[grep(pattern="X_",x=names(data_out))]<-"discharge"
  filepath<-file.path(outdir,paste0(site_num, "_dv.csv"))
  write_csv(data_out, file=filepath)
  return(filepath)
  
}

get_nwis_peak_data<-function(site_num,outdir,startDate, endDate){
  ##read NWIS peak data, and save to csv file.
  data_out<-readNWISpeak(site_num,  startDate, endDate)
  
  ##Removing any NA dates or peak_va values - dates are returned as NA if day of occurance is not known
  ## and peak_va is sometimes returned as NA if the gage height is known but not the discharge.
  data_out<-data_out[which(!is.na(data_out$peak_dt)),]
  data_out<-data_out[which(!is.na(data_out$peak_va)),]
  return(data_out)
  
}


screen_daily_data<-function(filename,yearType){
  ##screen for years with missing data
  data<-read.csv(filename)
  data$Date <- as.Date(data$Date)
  ###prior to screening, remove any provisional  or estimated data - this will be counted as 'no data'
  prov_data<- grep('P|e',data$discharge_cd)
  if(length(prov_data)>0){data<-data[-prov_data,]}
  
  if(yearType=="water"){
    water_year_start<-10
  }else{
      water_year_start <- 1
    }
  missing_data<-screen_flow_data(data.frame(site_no=data$site_no, Date=data$Date,Value=data$discharge),water_year_start=water_year_start)
  missing_data<-data.frame(site_num = unique(data$site_no),missing_data)
  return(missing_data)
}


clean_daily_data<-function(filename,missing_data, yearType ){
  ##remove nas from data and remove incomplete years of data
  ##Eflow stats requires the cleaned data to have dates in the first column and 
  ##discharge in the second column.
  data<-read.csv(filename)
  data$Date <- as.Date(data$Date)
  
  ##remove any missing flow data
  data<-data[which(!is.na(data$discharge)),]
  data<-addWaterYear(data)
  
  ##remove all data from years with data gaps
  keep_years<-missing_data$Year[which(missing_data$n_missing_Q==0)]
  if (yearType=="water"){
    data_sc<-data[which(data$waterYear %in% keep_years),]
  } else{
    data_sc<-data[which(year(data$Date)%in% keep_years),]
  }
  
  ###run EflowStats validation to produce clean, ready to process data
  clean_data<-validate_data(data_sc[,c("Date","discharge")],yearType=yearType)
  clean_data$site_no<-unique(data_sc$site_no)
  return(clean_data)
}  #end clean_daily_data function



calc_water_year<-function(dates){
  water_year <- ifelse(month(dates) > 9, year(dates)+1,year(dates) )
  return(water_year)
 
} #end calc_water_year

combine_flow_metrics<- function(...){
  flow_metrics_all_sites<-bind_rows(...)
  return(flow_metrics_all_sites)
  
}


##this function is taking the output of the screen_daily_data function and
##summarizing it into one line per gage
summarize_data_screen<-function(screen_daily_flow){
  minyr<-min(screen_daily_flow$Year)
  maxyr<-max(screen_daily_flow$Year)
  total_yrs<-length(screen_daily_flow$Year)
  complete_yrs<-length(which(screen_daily_flow$n_missing_Q==0))
  yrs_less_than_8_missing<-length(which(screen_daily_flow$n_missing_Q < 8))
  df<-screen_daily_flow %>%   
    rowwise() %>%
    mutate(missing_days_mar_nov = sum(c_across(cols=c(Mar_missing_Q,Apr_missing_Q,May_missing_Q,Jun_missing_Q,Jul_missing_Q,Aug_missing_Q,
                                                      Sep_missing_Q,Oct_missing_Q,Nov_missing_Q))))
 complete_yrs_mar_nov<-length(which(df$missing_days_mar_nov==0))
  ##now I'm looking at years in which no month has more than 5 days missing
 df<-screen_daily_flow %>%   
   rowwise() %>%
   mutate(max_monthly_missing =max(c_across(cols=c(Jan_missing_Q, Feb_missing_Q,Mar_missing_Q,Apr_missing_Q,May_missing_Q,Jun_missing_Q,
                                                   Jul_missing_Q,Aug_missing_Q,Sep_missing_Q,Oct_missing_Q))))
  
 lessthan_5_missing_permonth<-length(which(df$max_monthly_missing<6))
 
  df_out<-data.frame(site_num=unique(screen_daily_flow$site_num),minyr,maxyr,total_yrs,complete_yrs,yrs_less_than_8_missing,complete_yrs_mar_nov, lessthan_5_missing_permonth)
  return(df_out)
}



