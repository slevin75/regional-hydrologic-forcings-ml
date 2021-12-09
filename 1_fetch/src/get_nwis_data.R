
has_data_check<-function(site_nums,parameterCd){
  ##check to see if all sites actually have daily flow and peak flow data.  There are
  ##gages in gagesii that do not have one or the other, so screen these out before we try
  ##to download the data
  dv_screen<-whatNWISdata(siteNumber=site_nums,parameterCd=parameterCd,service="dv")
   pk_screen<-whatNWISdata(siteNumber=site_nums,service="pk")
  sites_with_data<-intersect(dv_screen$site_no,pk_screen$site_no)
}

filter_complete_years<-function(screen_daily_flow,complete_years){
  complete_yr_count<-screen_daily_flow %>% 
    group_by(site_no)%>%
    count()
  keep_sites<-complete_yr_count%>%
    filter(n >= complete_years)%>%
    pull(site_no)
  return(keep_sites)
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
  filepath<-file.path(outdir,paste0(site_num, "_pk.csv"))
  write_csv(data_out, file=filepath)
  return(filepath)

  
}


screen_daily_data<-function(filename,yearType){
  ##screen for years with missing data
  data<-read_csv(filename)
  data$Date <- as.Date(data$Date)
  data$site_no <- as.character(data$site_no)
  ###prior to screening, remove any provisional data - this will be counted as 'no data'
  prov_data<- grep('P|e',data$discharge_cd)
  if(length(prov_data)>0){data<-data[-prov_data,]}
  
  if(yearType=="water"){
    water_year_start<-10
  }else{
      water_year_start <- 1
    }
  missing_data<-screen_flow_data(data.frame(site_no=data$site_no, Date=data$Date,Value=data$discharge),water_year_start=water_year_start)
  complete_yrs<- missing_data %>%
    filter(n_missing_Q == 0)%>%
    select(Year)
  

  if(nrow(complete_yrs) > 0 ){
   data_out<-data.frame(site_no=unique(data$site_no),complete_yrs=complete_yrs$Year)
  }else {
   data_out<-data.frame(site_no=unique(data$site_no),complete_yrs=0)
  }
  
  return(data_out)
}




clean_daily_data<-function(site,filenames, screen_daily_flow,yearType ){
  ##remove nas from data and remove incomplete years of data
  ##Eflow stats requires the cleaned data to have dates in the first column and 
  ##discharge in the second column.
  filepath<-filenames[grep(site,filenames)]
  data<-read_csv(filepath)
  data$Date<-as_date(data$Date)
  data$site_no<-as.character(data$site_no)
  data<-addWaterYear(data)
  print(paste("raw data nrow = ", nrow(data)))
  ##remove all data from years with data gaps
  keep_years<-screen_daily_flow %>%
    filter(site_no ==site)%>%
    pull(complete_yrs)
  if (yearType=="water"){
    data_sc<-data[which(data$waterYear %in% keep_years),]
  } else{
    data_sc<-data[which(year(data$date)%in% keep_years),]
  }
  print(paste("nrow(data_sc", nrow(data_sc)))
  
  df<-data.frame(as.Date(data_sc$Date),data_sc$discharge)
  ###run EflowStats validation to produce clean, ready to process data
  clean_data<-validate_data(df,yearType=yearType)
  clean_data$site_no<-unique(data_sc$site_no)
  print(paste("nrow clean_data", nrow(clean_data)))
  return(clean_data)
}  #end clean_daily_data function


get_NWIS_drainArea<-function(site_num){
  data_out<- data.frame(site_no=as.character(site_num),
                        drainArea=readNWISsite(siteNumbers=site_num)$drain_area_va)
  return(data_out)
}


get_floodThreshold<-function(site_num,p1_clean_daily_flow,p1_peak_flow,perc,yearType){
  
  df_dv<- p1_clean_daily_flow %>%
    filter(site_no==site_num)%>%
    select(date,discharge)
  
  filepath<-p1_peak_flow[grep(site_num,p1_peak_flow)]
  peaks<-read_csv(filepath) 
  peaks$site_no<-as.character(peaks$site_no)
  df_pk<-data.frame(date=as.Date(peaks$peak_dt),peak=peaks$peak_va)
  floodThreshold<-get_peakThreshold(df_dv,df_pk,perc=perc,yearType=yearType)
  df_out<-data.frame(site_no=site_num,floodThreshold)
  return(df_out)
}




calc_water_year<-function(dates){
  water_year <- ifelse(month(dates) > 9, year(dates)+1,year(dates) )
  return(water_year)
 
} #end calc_water_year

combine_flow_metrics<- function(...){
  flow_metrics_all_sites<-bind_rows(...)
  return(flow_metrics_all_sites)
  
}



