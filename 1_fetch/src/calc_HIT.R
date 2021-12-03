


calc_HITmetrics<-function(data,yearType,drainArea,floodThreshold){
  drainArea <- drainArea$DA_NWIS
  out_data<- suppressWarnings(calc_allHIT(x=data,yearType=yearType,
                                          drainArea=drainArea,
                                          floodThreshold=floodThreshold))
 
  out_data$site_num<-unique(data$site_no)
  return(out_data)
}