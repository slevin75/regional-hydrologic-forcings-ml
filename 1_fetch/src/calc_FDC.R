#Function used to compute FDC-based metrics
calc_FDCmetrics <- function(site_num, clean_daily_flow, yearType, 
                            drainArea_tab, NE_probs, digits = 3){
  print(site_num)
  data <- clean_daily_flow %>%
    filter(site_no == site_num)
  
  drainArea <- drainArea_tab %>%
    filter(site_no == site_num) %>%
    pull(drainArea)
  
  #Validate using the same function as EflowStats
  data <- validate_data(data, yearType)
  
  #Get the non-exceedance flow values (mh15 style metrics)
  NE_flows <- quantile(data$discharge, probs = NE_probs, type = 6, names = F)
  
  #Compute metrics for each NE_flows threshold
  dhfdc <- fhfdc <- vhfdc1 <- vhfdc2 <- vector('numeric', length=length(NE_flows))
  for (i in 1:length(NE_flows)){
    #Get the duration of events above each of the NE_flows (dh17 style metrics)
    #We can use the trim=TRUE argument to not count events that start at the 
    #beginning or end of the record (these have unknown duration)
    dhfdc[i] <- find_eventDuration(data$discharge, threshold = NE_flows[i], aggType = "average")
    
    #Get the number of events above each of the NE_flows (fh5 style metrics)
    yearlyCounts <- dplyr::do(dplyr::group_by(data, year_val), 
                              {find_events(.$discharge, 
                                           threshold = NE_flows[i], type = "high")
                              })
    yearlyCounts <- na.omit(yearlyCounts)
    yearlyCounts <- dplyr::summarize(dplyr::group_by(yearlyCounts, year_val), 
                                     numEvents = max(event))
    fhfdc[i] <- mean(yearlyCounts$numEvents)
    
    #Get the flow volumes above the NE_flows threshold for events above each of the NE_flows (mh21 style metrics)
    lst <- find_events(data$discharge, threshold = NE_flows[i])
    eventData <- na.omit(lst)
    numEvents <- length(unique(eventData$event))
    totalFlow <- sum(eventData$flow - NE_flows[i])
    vhfdc1[i] <- totalFlow/numEvents
    
    #Get the average of the max flow for events above each of the NE_flows (mh24 style metrics)
    eventMax <- dplyr::group_by(lst[c("flow", "event")], event)
    eventMax <- dplyr::summarize(eventMax, maxQ = max(flow))
    eventMax <- na.omit(eventMax)
    vhfdc2[i] <- mean(eventMax$maxQ)
  }
  rm(i)
  
  #Compile metrics for output data
  #FDC magnitudes and volumes divided by drainage area
  #all values rounded
  mhfdc <- round(NE_flows/drainArea, digits)
  dhfdc <- round(dhfdc, digits)
  fhfdc <- round(fhfdc, digits)
  vhfdc1 <- round(vhfdc1/drainArea, digits)
  vhfdc2 <- round(vhfdc2/drainArea, digits)
  
  #Make a data.frame of values to match the format of the EflowStats metrics
  out_data <- data.frame(indice = c(paste0('mhfdc_q', NE_probs),
                                    paste0('dhfdc_q', NE_probs),
                                    paste0('fhfdc_q', NE_probs),
                                    paste0('vhfdc1_q', NE_probs),
                                    paste0('vhfdc2_q', NE_probs)),
                         statistic = c(mhfdc, dhfdc, fhfdc, vhfdc1, vhfdc2),
                         site_num = site_num)
  
  return(out_data)
}
