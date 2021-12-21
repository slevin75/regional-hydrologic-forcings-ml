#Add description
calc_HITmetrics <- function(site_num, clean_daily_flow, yearType, 
                            drainArea_tab, floodThreshold_tab, stat_vec){
  print(site_num)
  data <- clean_daily_flow %>%
    filter(site_no == site_num)

  drainArea <- drainArea_tab %>%
    filter(site_no == site_num) %>%
    pull(drainArea)
 
  floodThreshold <- floodThreshold_tab %>%
    filter(site_no == site_num) %>%
    pull(floodThreshold)

  out_data <- suppressWarnings(calc_allHIT(x=data, yearType=yearType,
                                          drainArea=drainArea,
                                          floodThreshold=floodThreshold, 
                                          stats = stat_vec))
  
  out_data$site_num <- unique(data$site_no)
  return(out_data)
}
