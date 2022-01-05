#Function used to compute FDC-based metrics
#seasonal = TRUE will compute each of the metrics annually and seasonally for the months specified in the season_months vector
#season_months is a numeric vector of months. Every 3 months are a season.
#stat_type is a character equal to 'POR' for period of record metrics or 'ATS' for annual timeseries metrics.
calc_FDCmetrics <- function(site_num, clean_daily_flow, yearType, 
                            drainArea_tab, NE_probs, digits = 3,
                            seasonal = FALSE, season_months = NULL,
                            stat_type = 'POR'){
  print(site_num)
  data <- clean_daily_flow %>%
    filter(site_no == site_num)
  
  #Validate using the same function as EflowStats
  data <- validate_data(data, yearType)
  
  #Add columns needed to compute seasonal information
  if(seasonal){
    if(is.null(season_months)){
      stop('season_months must be specified for seasonal FDC-based metrics')
    }
    #Add column for month and season
    data <- data %>% mutate(month_val = lubridate::month(date),
                            season = dplyr::case_when(month_val %in% season_months[1:3] ~ 1,
                                      month_val %in% season_months[4:6] ~ 2,
                                      month_val %in% season_months[7:9] ~ 3,
                                      month_val %in% season_months[10:12] ~ 4,
                                      TRUE ~ 0))
    
    if(any(data$season == 0)){
      stop(paste('some dates or months are misspecified for site', site_num))
    }
    if(!all(c(1,2,3,4) %in% unique(data$season))){
      stop(paste('some seasons are not present for site', site_num))
    }
  }
  
  drainArea <- drainArea_tab %>%
    filter(site_no == site_num) %>%
    pull(drainArea)
  
  #Get the non-exceedance flow values (mh15 style metrics)
  NE_flows <- quantile(data$discharge, probs = NE_probs, type = 6, names = F)
  
  #Compute metrics for each NE_flows threshold
  #dh, fh, and vh are for duration, frequency, and volume of high flows. 
  #fdc is for flow-duration curve. 
  #1 and 2 are two different volume metrics
  if(seasonal){
    dhfdc_s <- fhfdc_s <- vhfdc1_s <- vhfdc2_s <- vector('numeric', 
                                                         length=length(NE_flows)*4)
  }else{
    dhfdc <- fhfdc <- vhfdc1 <- vhfdc2 <- vector('numeric', length=length(NE_flows))
  }
  for (i in 1:length(NE_flows)){
    #Get the duration of events above each of the NE_flows (dh17 style metrics)
    # Seasonal durations will be underestimated because the event start date can 
    #  occur in the previous season, but the event is counted as a new event. 
    #  This may be okay because we're dividing by the total average annual duration.
    if(seasonal){
      seasonalDuration <- dplyr::do(dplyr::group_by(data, year_val, season), 
                                  duration = {find_eventDuration(.$discharge,
                                                      threshold = NE_flows[i],
                                                      aggType = "average")
                                  }) %>%
        mutate(duration = unlist(duration)) %>%
        dplyr::arrange(year_val, season)
      
      #Compute seasonal fractions using ATS or POR approach:
      #Computes annual fraction per season, then averages over all years
      if (stat_type == 'ATS'){
        seasonalDuration$year_frac <- 0
        for(j in 1:length(unique(seasonalDuration$year_val))){
          inds <- c((1+(4*(j-1))):(4*j))
          
          #There can be years with no events, so need to check for those first
          if(sum(seasonalDuration$duration[inds]) == 0){
            seasonalDuration$year_frac[inds] <- NA
          }else{
            seasonalDuration$year_frac[inds] <- seasonalDuration$duration[inds]/sum(
              seasonalDuration$duration[inds])
          }
        }
        rm(j)
        
        dhfdc_s[1+(i-1)*4] <- mean(seasonalDuration$year_frac[seasonalDuration$season == 1], 
                                   na.rm = TRUE)
        dhfdc_s[2+(i-1)*4] <- mean(seasonalDuration$year_frac[seasonalDuration$season == 2], 
                                   na.rm = TRUE)
        dhfdc_s[3+(i-1)*4] <- mean(seasonalDuration$year_frac[seasonalDuration$season == 3], 
                                   na.rm = TRUE)
        dhfdc_s[4+(i-1)*4] <- mean(seasonalDuration$year_frac[seasonalDuration$season == 4], 
                                   na.rm = TRUE)
      }else{
        #Computes seasonal average duration over all years, then fraction per season.
        seasonalDuration = dplyr::summarize(group_by(seasonalDuration, season),
                                     avg=mean(duration, na.rm=TRUE)) %>%
                                     arrange(season)
        if (sum(seasonalDuration) == 0){
          dhfdc_s[(1+(i-1)*4):(4+(i-1)*4)] <- NA
        }else{
          dhfdc_s[(1+(i-1)*4):(4+(i-1)*4)] <- seasonalDuration/sum(seasonalDuration)
        }
      }
    }else{
      dhfdc[i] <- find_eventDuration(data$discharge, threshold = NE_flows[i], 
                                     aggType = "average")
    }
    
    #Get the number of events above each of the NE_flows (fh5 style metrics)
    if(seasonal){
      seasonalCounts <- dplyr::do(dplyr::group_by(data, year_val, season), 
                                  {find_events(.$discharge, 
                                               threshold = NE_flows[i], type = "high")
                                  }) %>%
        dplyr::arrange(year_val, season)
      
      #changing NAs to 0s so that seasons with no events are counted as 0 
      #instead of NA
      seasonalCounts$event[is.na(seasonalCounts$event)] <- 0
      
      #This function double counts events when they overlap in season. 
      #That may be okay because we're dividing by the total number of events for the year
      seasonalCounts <- dplyr::summarize(dplyr::group_by(seasonalCounts, year_val, season), 
                                         numEvents = max(event), .groups = 'keep')
      
      #Compute seasonal fractions using ATS or POR approach:
      #Computes annual fraction per season, then averages over all years
      if (stat_type == 'ATS'){
        seasonalCounts$year_frac <- 0
        for(j in 1:length(unique(seasonalCounts$year_val))){
          inds <- c((1+(4*(j-1))):(4*j))
          #There can be years with no events, so need to check for those first
          if(sum(seasonalCounts$numEvents[inds]) == 0){
            seasonalCounts$year_frac[inds] <- NA
          }else{
            seasonalCounts$year_frac[inds] <- seasonalCounts$numEvents[inds]/sum(seasonalCounts$numEvents[inds])
          }
        }
        rm(j)
        
        fhfdc_s[1+(i-1)*4] <- mean(seasonalCounts$year_frac[seasonalCounts$season == 1],
                                   na.rm = TRUE)
        fhfdc_s[2+(i-1)*4] <- mean(seasonalCounts$year_frac[seasonalCounts$season == 2],
                                   na.rm = TRUE)
        fhfdc_s[3+(i-1)*4] <- mean(seasonalCounts$year_frac[seasonalCounts$season == 3],
                                   na.rm = TRUE)
        fhfdc_s[4+(i-1)*4] <- mean(seasonalCounts$year_frac[seasonalCounts$season == 4],
                                   na.rm = TRUE)
      }else{
        #Computes seasonal average duration over all years, then fraction per season.
        seasonalCounts = dplyr::summarize(group_by(seasonalCounts, season),
                                     avg=mean(numEvents, na.rm=TRUE)) %>%
                                     arrange(season)
        if (sum(seasonalCounts) == 0){
          fhfdc_s[(1+(i-1)*4):(4+(i-1)*4)] <- NA
        }else{
          fhfdc_s[(1+(i-1)*4):(4+(i-1)*4)] <- seasonalCounts/sum(seasonalCounts)
        }
      }
    }else{
      yearlyCounts <- dplyr::do(dplyr::group_by(data, year_val), 
                                {find_events(.$discharge, 
                                             threshold = NE_flows[i], type = "high")
                                })
      yearlyCounts <- na.omit(yearlyCounts)
      yearlyCounts <- dplyr::summarize(dplyr::group_by(yearlyCounts, year_val), 
                                       numEvents = max(event))
      fhfdc[i] <- mean(yearlyCounts$numEvents)
    }
    
    #Get the flow volume and average of the max flow for events above each of 
    #the NE_flows (mh21 style metrics for total flow instead of flow above threshold, 
    #and mh24 style metrics for max flow)
    if(seasonal){
      seasonalVolume <- dplyr::do(dplyr::group_by(data, year_val, season), 
                                  {find_events(.$discharge, 
                                               threshold = NE_flows[i], type = 'high')
                                  }) %>%
        dplyr::arrange(year_val, season)
      
      #Get the max flow for each event
      seasonalMaxQ <- dplyr::summarize(dplyr::group_by(seasonalVolume, year_val,
                                                      season, event),
                                       .groups = 'drop_last', 
                                      maxQ = max(flow)) %>%
        dplyr::arrange(year_val, season)
      
      #Some seasons do not have events, but we want those seasons to be reported 
      #as having 0 volume. So, set all flows to 0 in those seasons.
      seasonalVolume$event[is.na(seasonalVolume$event)] <- 0
      seasonalVolume$flow[seasonalVolume$event == 0] <- 0
      
      #Seasons with no events have flows reported. Want those to not be included 
      #in the calculation of average seasonal maximums
      seasonalMaxQ$maxQ[is.na(seasonalMaxQ$event)] <- NA
      
      #This function double counts events when they overlap in season. 
      #That may be okay because we're dividing by the total number of events for the year
      seasonalVolume <- dplyr::summarize(dplyr::group_by(seasonalVolume, year_val, season), 
                                         numEvents = max(event), 
                                         totalFlow = sum(flow),
                                         .groups = 'keep') %>%
        mutate(flow_event = totalFlow/numEvents)
      
      #Compute seasonal fractions using ATS or POR approach:
      #Computes annual fraction per season, then averages over all years
      if (stat_type == 'ATS'){
        seasonalVolume$year_frac <- 0
        for(j in 1:length(unique(seasonalVolume$year_val))){
          inds <- c((1+(4*(j-1))):(4*j))
          #There can be years with no events, so need to check for those first
          if(sum(seasonalVolume$numEvents[inds]) == 0){
            seasonalVolume$year_frac[inds] <- NA
          }else{
            seasonalVolume$year_frac[inds] <- seasonalVolume$flow_event[inds]/sum(seasonalVolume$flow_event[inds], na.rm = TRUE)
            #Set any NaN values to 0. These result from a season not having events
            seasonalVolume$year_frac[inds][is.nan(seasonalVolume$year_frac[inds])] <- 0
          }
        }
        rm(j)
        
        vhfdc1_s[1+(i-1)*4] <- mean(seasonalVolume$year_frac[seasonalVolume$season == 1],
                                    na.rm = TRUE)
        vhfdc1_s[2+(i-1)*4] <- mean(seasonalVolume$year_frac[seasonalVolume$season == 2],
                                    na.rm = TRUE)
        vhfdc1_s[3+(i-1)*4] <- mean(seasonalVolume$year_frac[seasonalVolume$season == 3],
                                    na.rm = TRUE)
        vhfdc1_s[4+(i-1)*4] <- mean(seasonalVolume$year_frac[seasonalVolume$season == 4],
                                    na.rm = TRUE)
        
        #Compute the average of the maximum flow in each season
        seasonalMaxQ <- dplyr::summarize(dplyr::group_by(seasonalMaxQ, year_val, season),
                                         season_max = suppressWarnings(
                                           max(maxQ, na.rm = TRUE)),
                                         .groups = 'keep')
        #Some seasons do not have events. Set the max to NA.
        seasonalMaxQ[seasonalMaxQ$season_max == -Inf,"season_max"] <- NA
        
        seasonalMaxQ$year_frac <- 0
        for(j in 1:length(unique(seasonalMaxQ$year_val))){
          inds <- c((1+(4*(j-1))):(4*j))
          #There can be years with no events, so need to check for those first
          if(all(is.na(seasonalMaxQ$season_max[inds]))){
            seasonalMaxQ$year_frac[inds] <- NA
          }else{
            seasonalMaxQ$year_frac[inds] <- seasonalMaxQ$season_max[inds]/sum(seasonalMaxQ$season_max[inds], na.rm = TRUE)
          }
        }
        rm(j)
        
        vhfdc2_s[1+(i-1)*4] <- mean(seasonalMaxQ$year_frac[seasonalMaxQ$season == 1],
                                    na.rm = TRUE)
        vhfdc2_s[2+(i-1)*4] <- mean(seasonalMaxQ$year_frac[seasonalMaxQ$season == 2],
                                    na.rm = TRUE)
        vhfdc2_s[3+(i-1)*4] <- mean(seasonalMaxQ$year_frac[seasonalMaxQ$season == 3],
                                    na.rm = TRUE)
        vhfdc2_s[4+(i-1)*4] <- mean(seasonalMaxQ$year_frac[seasonalMaxQ$season == 4],
                                    na.rm = TRUE)
      }else{
        #Computes seasonal average over all years, then fraction per season.
        seasonalVolume = dplyr::summarize(group_by(seasonalVolume, season),
                                   avg=mean(flow_event, na.rm=TRUE)) %>%
                                   arrange(season)
        if (sum(seasonalVolume) == 0){
          vhfdc1_s[(1+(i-1)*4):(4+(i-1)*4)] <- NA
        }else{
          vhfdc1_s[(1+(i-1)*4):(4+(i-1)*4)] <- seasonalVolume/sum(seasonalVolume)
        }
        
        #Compute the average of the maximum flow in each season
        seasonalMaxQ <- dplyr::summarize(group_by(seasonalMaxQ, season),
                                  avg=mean(maxQ, na.rm=TRUE)) %>%
                                  arrange(season)
        if (sum(seasonalMaxQ) == 0){
          vhfdc2_s[(1+(i-1)*4):(4+(i-1)*4)] <- NA
        }else{
          vhfdc2_s[(1+(i-1)*4):(4+(i-1)*4)] <- seasonalMaxQ/sum(seasonalMaxQ)
        }
      }
    }else{
      #volume
      lst <- find_events(data$discharge, threshold = NE_flows[i])
      eventData <- na.omit(lst)
      numEvents <- length(unique(eventData$event))
      totalFlow <- sum(eventData$flow)
      vhfdc1[i] <- totalFlow/numEvents
      
      #max flow 
      eventMax <- dplyr::group_by(lst[c("flow", "event")], event)
      eventMax <- dplyr::summarize(eventMax, maxQ = max(flow))
      eventMax <- na.omit(eventMax)
      vhfdc2[i] <- mean(eventMax$maxQ)
    }
  }
  rm(i)
  
  #Compile metrics for output data
  #non-seasonal FDC magnitudes and volumes divided by drainage area
  #all values rounded
  if(seasonal){
    dhfdc_s <- round(dhfdc_s, digits)
    fhfdc_s <- round(fhfdc_s, digits)
    vhfdc1_s <- round(vhfdc1_s, digits)
    vhfdc2_s <- round(vhfdc2_s, digits)
    season_names <- c(unlist(lapply(X = paste0('dhfdc_q', NE_probs, '_s'), 
                                    FUN = paste0, seq(1,4,1))),
                      unlist(lapply(X = paste0('fhfdc_q', NE_probs, '_s'), 
                                    FUN = paste0, seq(1,4,1))),
                      unlist(lapply(X = paste0('vhfdc1_q', NE_probs, '_s'), 
                                    FUN = paste0, seq(1,4,1))),
                      unlist(lapply(X = paste0('vhfdc2_q', NE_probs, '_s'), 
                                    FUN = paste0, seq(1,4,1))))
  }else{
    mhfdc <- round(NE_flows/drainArea, digits)
    dhfdc <- round(dhfdc, digits)
    fhfdc <- round(fhfdc, digits)
    vhfdc1 <- round(vhfdc1/drainArea, digits)
    vhfdc2 <- round(vhfdc2/drainArea, digits)
    annual_names <- c(paste0('mhfdc_q', NE_probs),
                      paste0('dhfdc_q', NE_probs),
                      paste0('fhfdc_q', NE_probs),
                      paste0('vhfdc1_q', NE_probs),
                      paste0('vhfdc2_q', NE_probs))
  }
  
  #Make a data.frame of values to match the format of the EflowStats metrics
  if(seasonal){
    out_data <- data.frame(indice = season_names,
                           statistic = c(dhfdc_s, fhfdc_s, vhfdc1_s, vhfdc2_s),
                           site_num = site_num)
  }else{
    out_data <- data.frame(indice = annual_names,
                           statistic = c(mhfdc, dhfdc, fhfdc, vhfdc1, vhfdc2),
                           site_num = site_num)
  }
  
  out_data <- pivot_wider(out_data, names_from = 'indice', values_from = 'statistic')
  
  return(out_data)
}
