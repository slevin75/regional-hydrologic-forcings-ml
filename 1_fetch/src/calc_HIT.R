#Add description
calc_HITmetrics <- function(site_num, clean_daily_flow, yearType, 
                            drainArea_tab, floodThreshold_tab, stat_vec,
                            save_metrics = NULL, norm_DA = NULL, 
                            norm_med_DA = NULL, norm_ml17 = NULL){
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
  
  #Select only the metrics to save
  if (!is.null(save_metrics)){
    out_data <- filter(out_data, indice %in% save_metrics)
  }
  
  #Normalize metrics by drainage area
  if (!is.null(norm_DA)){
    out_data$statistic[out_data$indice %in% norm_DA] <- out_data$statistic[out_data$indice %in% norm_DA]/drainArea
  }
  
  #Normalize metrics by median
  if (!is.null(norm_med_DA)){
    med_flow <- median(data$discharge, na.rm = TRUE)
    out_data$statistic[out_data$indice %in% norm_med_DA] <- out_data$statistic[out_data$indice %in% norm_med_DA]/drainArea*med_flow
  }
  
  #Normalize ml17 by annual mean and drainage area
  if (!is.null(norm_ml17)){
    calc_bfibyyear <- dplyr::summarize(dplyr::group_by(data, year_val), 
                                       calc_bfi = calc_bfi(discharge)*mean(discharge))
    
    out_data$statistic[out_data$indice %in% 'ml17'] <- mean(calc_bfibyyear$calc_bfi)/drainArea
  }
  return(out_data)
}
