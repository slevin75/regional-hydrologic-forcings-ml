
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
  ###prior to screening, remove any provisional data - this will be counted as 'no data'
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



