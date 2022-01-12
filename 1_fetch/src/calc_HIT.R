#Add description
calc_HITmetrics <- function(site_num, clean_daily_flow, yearType, 
                            drainArea_tab, floodThreshold_tab, stat_vec,
                            save_metrics = NULL, norm_DA = NULL, 
                            norm_med_DA = NULL, norm_ml17 = NULL,
                            digits = 3){
  message(paste('starting site', site_num))
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
                                          stats = stat_vec,
                                          digits = digits))
  
  out_data$site_num <- unique(data$site_no)
  
  #Select only the metrics to save
  if (!is.null(save_metrics)){
    out_data <- filter(out_data, indice %in% save_metrics)
  }
  
  #Normalize metrics by drainage area
  if (!is.null(norm_DA)){
    out_data <- out_data %>% mutate(statistic = case_when(
      (indice %in% norm_DA) ~ round(statistic/drainArea, digits),
      TRUE ~ statistic))
  }
  
  #Normalize metrics by drainage area and remove median normalization
  if (!is.null(norm_med_DA)){
    med_flow <- median(data$discharge, na.rm = TRUE)
    out_data <- out_data %>% mutate(statistic = case_when(
      (indice %in% norm_med_DA) ~ round(statistic/drainArea*med_flow, digits),
      TRUE ~ statistic))
  }
  
  #Normalize ml17 by drainage area and remove annual mean normalization
  if (!is.null(norm_ml17)){
    calc_bfibyyear <- dplyr::summarize(dplyr::group_by(data, year_val), 
                                       calc_bfi = calc_bfi(discharge)*mean(discharge))
    
    out_data <- out_data %>% mutate(statistic = case_when(
      (indice %in% 'ml17') ~ round(mean(calc_bfibyyear$calc_bfi)/drainArea, digits),
      TRUE ~ statistic))
    
    #Correct the CV of ml18 if it's one of the saved metrics
    if(!is.null(save_metrics) & ('ml18' %in% save_metrics)){
      out_data <- out_data %>% mutate(statistic = case_when(
        (indice %in% 'ml18') ~ round(sd(calc_bfibyyear$calc_bfi)/mean(calc_bfibyyear$calc_bfi) * 100, digits),
        TRUE ~ statistic))
    }
  }
  
  out_data <- pivot_wider(out_data, names_from = 'indice', values_from = 'statistic')
  
  return(out_data)
}
