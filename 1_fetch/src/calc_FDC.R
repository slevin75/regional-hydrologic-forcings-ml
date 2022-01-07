#Function used to compute FDC-based metrics
#inputs are the same as those for calc_HIT, except:
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
  
  #make groups of continuous years
  data <- data_groups(data)
  
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
  
  drain_area <- drainArea_tab %>%
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
    # Seasonal durations are relative to the total time in that season, not the
    # total duration of the event. On average, that should reflect the event
    # duration in that season.
    if(seasonal){
      #Find events within each group of continuous years
      lst <- data
      lst$event <- dplyr::do(dplyr::group_by(lst, groups), 
                       {find_events(.$discharge, threshold = NE_flows[i], 
                                    type = 'high')})$event
      
      #Trim events of unknown duration/volume
      lst <- trim_events(lst)
      
      #renumber the NA events and flows as 0. This helps for next renumbering step
      lst[is.na(lst$event), c('discharge', 'event')] <- 0
      
      #renumber the events to be monotonic. Gaps in event number are okay.
      lst <- monotonic_events(lst)
      
      event_durations <- dplyr::summarize(dplyr::group_by(lst, season, event), 
                                          duration = length(event),
                                          .groups = 'drop_last')
      
      #Set event 0 to NA duration. This ensures that all seasons in all years 
      #have data, even if they do not have events.
      event_durations$duration[event_durations$event == 0] <- NA
      
      #Compute seasonal fractions using ATS or POR approach:
      #Computes annual fraction per season, then averages over all years
      if (stat_type == 'ATS'){
        #Compute duration for each event and add to dataframe
        seasonal_durations <- dplyr::mutate(dplyr::group_by(lst, season, event), 
                                           duration = length(event))
        #get to a table of one entry per event
        seasonal_durations <- dplyr::summarize(dplyr::group_by(seasonal_durations,
                                                             year_val, season, 
                                                             event), 
                                avg = mean(duration, na.rm=TRUE), .groups = 'keep')
        
        seasonal_durations$avg[seasonal_durations$event == 0] <- NA
        
        #average for each season in each year
        seasonal_durations <- dplyr::summarize(dplyr::group_by(seasonal_durations, 
                                                               year_val, season), 
                                avg = mean(avg, na.rm=TRUE), .groups = 'keep')
        
        seasonal_durations$avg[is.na(seasonal_durations$avg)] <- 0
        
        seasonal_durations$year_frac <- 0
        for(j in 1:length(unique(seasonal_durations$year_val))){
          inds <- c((1+(4*(j-1))):(4*j))
          
          #There can be years with no events, so need to check for those first
          if(sum(seasonal_durations$avg[inds]) == 0){
            seasonal_durations$year_frac[inds] <- NA
          }else{
            seasonal_durations$year_frac[inds] <- seasonal_durations$avg[inds]/sum(
              seasonal_durations$avg[inds])
          }
        }
        rm(j, inds)
        
        dhfdc_s[(1+(i-1)*4):(4+(i-1)*4)] <- seasonal_mean(seasonal_durations)
      }else{
        seasonal_durations <- dplyr::summarize(dplyr::group_by(event_durations, season), 
                                             avg = mean(duration, na.rm=TRUE)) %>%
          arrange(season) %>%
          pull(avg)
        
        if (all(is.na(seasonal_durations))){
          #No events in any season
          dhfdc_s[(1+(i-1)*4):(4+(i-1)*4)] <- NA
        }else{
          #Change the NAs to 0s and compute the fractions per season
          seasonal_durations[is.na(seasonal_durations)] <- 0
          dhfdc_s[(1+(i-1)*4):(4+(i-1)*4)] <- seasonal_durations/sum(seasonal_durations)
        }
      }
    }else{
      #Find events within each group of continuous years
      lst <- dplyr::do(dplyr::group_by(data, groups), 
                       {find_events(.$discharge, threshold = NE_flows[i], 
                                    type = 'high')})
      
      #Trim events of unknown duration/volume
      lst <- trim_events(lst)
      
      #renumber the NA events and flows as 0. This helps for next renumbering step
      lst[is.na(lst$event), c('flow', 'event')] <- 0
      
      #renumber the events to be monotonic. Gaps in event number are okay.
      lst <- monotonic_events(lst)
      
      #drop event 0
      lst <- lst[-which(lst$event == 0),]
      
      if (nrow(lst) > 0){
        event_durations <- dplyr::summarize(dplyr::group_by(lst, event), 
                                            duration = length(event))
        dhfdc[i] <- mean(event_durations$duration)
      }else{
        dhfdc[i] <- NA
      }
    }
    
    #Get the number of events above each of the NE_flows (fh1 style metrics)
    if(seasonal){
      seasonal_counts <- dplyr::do(dplyr::group_by(data, year_val, season), 
                                  {find_events(.$discharge, 
                                               threshold = NE_flows[i], type = "high")
                                  }) %>%
        dplyr::arrange(year_val, season)
      
      #changing NAs to 0s so that seasons with no events are counted as 0 
      #instead of NA
      seasonal_counts$event[is.na(seasonal_counts$event)] <- 0
      
      #This function double counts events when they overlap in season.
      seasonal_counts <- dplyr::summarize(dplyr::group_by(seasonal_counts, year_val, season), 
                                         numEvents = max(event), .groups = 'keep')
      
      #Compute seasonal fractions using ATS or POR approach:
      #Computes annual fraction per season, then averages over all years
      if (stat_type == 'ATS'){
        seasonal_counts$year_frac <- 0
        for(j in 1:length(unique(seasonal_counts$year_val))){
          inds <- c((1+(4*(j-1))):(4*j))
          #There can be years with no events, so need to check for those first
          if(sum(seasonal_counts$numEvents[inds]) == 0){
            seasonal_counts$year_frac[inds] <- NA
          }else{
            seasonal_counts$year_frac[inds] <- seasonal_counts$numEvents[inds]/sum(seasonal_counts$numEvents[inds])
          }
        }
        rm(j, inds)
        
        fhfdc_s[(1+(i-1)*4):(4+(i-1)*4)] <- seasonal_mean(seasonal_counts)
      }else{
        #Computes seasonal average duration over all years, then fraction per season.
        seasonal_counts = dplyr::summarize(group_by(seasonal_counts, season),
                                          avg=mean(numEvents, na.rm=TRUE)) %>%
          arrange(season) %>%
          pull(avg)
        
        if (sum(seasonal_counts) == 0){
          fhfdc_s[(1+(i-1)*4):(4+(i-1)*4)] <- NA
        }else{
          fhfdc_s[(1+(i-1)*4):(4+(i-1)*4)] <- seasonal_counts/sum(seasonal_counts)
        }
      }
    }else{
      yearlyCounts <- dplyr::do(dplyr::group_by(data, year_val), 
                                {find_events(.$discharge, 
                                             threshold = NE_flows[i], type = "high")
                                })
      #changing NAs to 0s so that years with no events are counted as 0s 
      #instead of being omitted
      yearlyCounts$event[is.na(yearlyCounts$event)] <- 0
      yearlyCounts <- dplyr::summarize(dplyr::group_by(yearlyCounts, year_val), 
                                       numEvents = max(event))
      fhfdc[i] <- mean(yearlyCounts$numEvents)
    }
    
    #Get the flow volume and average of the max flow for events above each of 
    #the NE_flows (mh21 style metrics for total flow instead of flow above threshold, 
    #and mh24 style metrics for max flow)
    if(seasonal){
      #This function splits event volume over seasons when they overlap in season.
      seasonal_volumes <- dplyr::summarize(dplyr::group_by(lst, year_val, season), 
                                         numEvents = length(unique(event[event != 0])), 
                                         totalFlow = sum(discharge),
                                         .groups = 'keep') %>%
        mutate(flow_event = totalFlow/numEvents)
      
      #Get the max flow for each event
      seasonal_maxQ <- dplyr::summarize(dplyr::group_by(lst, year_val,
                                                      season, event),
                                       .groups = 'drop_last', 
                                      maxQ = max(discharge)) %>%
        dplyr::arrange(year_val, season)
      
      #When events overlap in season, this code assigns the max to the season
      #with the larger max for that event.
      for(d in 1:length(unique(seasonal_maxQ$event))){
        inds <- which(seasonal_maxQ$event == unique(seasonal_maxQ$event)[d])
        if(length(inds) > 1){
          #Find index of max flow
          ind_max <- which(seasonal_maxQ$maxQ[inds] == max(seasonal_maxQ$maxQ[inds]))
          if(length(ind_max) > 1){
            #Assign to the first season because that's when the event started
            #Other seasons get converted to NA (these are deleted later)
            seasonal_maxQ[inds,][-1, 'maxQ'] <- NA
          }else{
            #Assign to season in which the max occurred
            #Other season gets converted to NA (these are deleted later)
            seasonal_maxQ[inds,][-ind_max, 'maxQ'] <- NA
          }
          rm(ind_max)
        }
      }
      rm(d, inds)
      
      #Seasons with no events have flows reported. Want those to not be included 
      #in the calculation of average seasonal maximums
      seasonal_maxQ[seasonal_maxQ$event == 0, c('event', 'maxQ')] <- NA
      
      #Compute seasonal fractions using ATS or POR approach:
      #Computes annual fraction per season, then averages over all years
      if (stat_type == 'ATS'){
        seasonal_volumes$year_frac <- 0
        seasonal_volumes$flow_event[is.nan(seasonal_volumes$flow_event)] <- 0
        for(j in 1:length(unique(seasonal_volumes$year_val))){
          inds <- c((1+(4*(j-1))):(4*j))
          #There can be years with no events, so need to check for those first
          if(sum(seasonal_volumes$numEvents[inds]) == 0){
            seasonal_volumes$year_frac[inds] <- NA
          }else{
            seasonal_volumes$year_frac[inds] <- seasonal_volumes$flow_event[inds]/sum(seasonal_volumes$flow_event[inds], na.rm = TRUE)
          }
        }
        rm(j, inds)
        
        vhfdc1_s[(1+(i-1)*4):(4+(i-1)*4)] <- seasonal_mean(seasonal_volumes)
        
        #Compute the average of the maximum flow in each season
        seasonal_maxQ <- dplyr::summarize(dplyr::group_by(seasonal_maxQ, year_val, season),
                                         season_max = suppressWarnings(
                                           max(maxQ, na.rm = TRUE)),
                                         .groups = 'keep')
        #Some seasons do not have events. Set to NA
        seasonal_maxQ[seasonal_maxQ$season_max == -Inf, "season_max"] <- NA
        
        seasonal_maxQ$year_frac <- 0
        for(j in 1:length(unique(seasonal_maxQ$year_val))){
          inds <- c((1+(4*(j-1))):(4*j))
          #There can be years with no events, so need to check for those first
          if(all(is.na(seasonal_maxQ$season_max[inds]))){
            seasonal_maxQ$year_frac[inds] <- NA
          }else{
            #Set the NAs to 0 so the fractions are 0 for those seasons
            seasonal_maxQ$season_max[inds][is.na(seasonal_maxQ$season_max[inds])] <- 0
            seasonal_maxQ$year_frac[inds] <- seasonal_maxQ$season_max[inds]/sum(seasonal_maxQ$season_max[inds])
          }
        }
        rm(j, inds)
        
        vhfdc2_s[(1+(i-1)*4):(4+(i-1)*4)] <- seasonal_mean(seasonal_maxQ)
      }else{
        #Computes seasonal average over all years, then fraction per season.
        seasonal_volumes <- dplyr::summarize(group_by(seasonal_volumes, season),
                                          avg=mean(flow_event, na.rm=TRUE)) %>%
          arrange(season) %>%
          pull(avg)
        
        if (all(is.na(seasonal_volumes))){
          #No events in any season
          vhfdc1_s[(1+(i-1)*4):(4+(i-1)*4)] <- NA
        }else{
          #Change the NaNs to 0s and compute the fractions per season
          seasonal_volumes[is.na(seasonal_volumes)] <- 0
          vhfdc1_s[(1+(i-1)*4):(4+(i-1)*4)] <- seasonal_volumes/sum(seasonal_volumes)
        }
        
        #Compute the average of the maximum flow in each season
        seasonal_maxQ <- dplyr::summarize(group_by(seasonal_maxQ, season),
                                         avg=mean(maxQ, na.rm=TRUE)) %>%
          arrange(season) %>%
          pull(avg)
        
        if (all(is.na(seasonal_maxQ))){
          #No events in any season
          vhfdc2_s[(1+(i-1)*4):(4+(i-1)*4)] <- NA
        }else{
          #Change the NaNs to 0s and compute the fractions per season
          seasonal_maxQ[is.na(seasonal_maxQ)] <- 0
          vhfdc2_s[(1+(i-1)*4):(4+(i-1)*4)] <- seasonal_maxQ/sum(seasonal_maxQ)
        }
      }
    }else{
      #uses processed lst variable from duration calculations
      if (nrow(lst) > 0){
        #volume
        num_events <- length(unique(lst$event))
        total_flow <- sum(lst$flow)
        vhfdc1[i] <- total_flow/num_events
        
        #max flow 
        event_max <- dplyr::group_by(lst[c("flow", "event")], event)
        event_max <- dplyr::summarize(event_max, maxQ = max(flow))
        vhfdc2[i] <- mean(event_max$maxQ)
      }else{
        #no events
        vhfdc1[i] <- NA
        vhfdc2[i] <- NA
      }
    }
  }
  rm(i)
  
  #Compile metrics for output data
  #non-seasonal FDC magnitudes and volumes are divided by drainage area
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
    mhfdc <- round(NE_flows/drain_area, digits)
    dhfdc <- round(dhfdc, digits)
    fhfdc <- round(fhfdc, digits)
    vhfdc1 <- round(vhfdc1/drain_area, digits)
    vhfdc2 <- round(vhfdc2/drain_area, digits)
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

seasonal_mean <- function(seasonal_var){
  metric <- c(mean(seasonal_var$year_frac[seasonal_var$season == 1], na.rm = TRUE),
              mean(seasonal_var$year_frac[seasonal_var$season == 2], na.rm = TRUE),
              mean(seasonal_var$year_frac[seasonal_var$season == 3], na.rm = TRUE),
              mean(seasonal_var$year_frac[seasonal_var$season == 4], na.rm = TRUE))
  
  return(metric)
}

data_groups <- function(data){
  #The find_events function assumes the timeseries is continuous. 
  #Years do not have to be continuous, so that is handled here by making
  #groups of continuous years
  yr_srt <- sort(unique(data$year_val))
  yr_diff <- diff(yr_srt)
  if (any(yr_diff > 1)){
    #There are gaps in years.
    #Make numbers to be assigned to groups
    grp_nums <- seq(1,length(which(yr_diff > 1))+1,1)
    data$groups <- NA
    for (g in 1:length(grp_nums)){
      if (g == 1){
        grp_yrs <- yr_srt[yr_srt < yr_srt[which(yr_diff > 1)+1][g]]
      }else if (g == length(grp_nums)){
        grp_yrs <- yr_srt[yr_srt >= yr_srt[which(yr_diff > 1)+1][g-1]]
      }else{
        grp_yrs <- yr_srt[which((yr_srt < yr_srt[which(yr_diff > 1)+1][g]) & (yr_srt >= yr_srt[which(yr_diff > 1)+1][g-1]))]
      }
      data$groups[data$year_val %in% grp_yrs] <- grp_nums[g]
    }
    rm(g, grp_nums, grp_yrs)
  }else{
    #No gaps. Use one group
    data$groups <- 1
  }
  
  return(data)
}

trim_events <- function(data){
  #Trim events of unknown duration/volume
  for (g in 1:max(data$groups)){
    grp_evnts <- data$event[data$groups == g]
    if (!is.na(grp_evnts[1])){
      data$event[data$groups == g][which((grp_evnts == grp_evnts[1]) == TRUE)] <- NA
    }
    if (!is.na(grp_evnts[length(grp_evnts)])){
      data$event[data$groups == g][which((grp_evnts == grp_evnts[length(grp_evnts)]) == TRUE)] <- NA
    }
  }
  
  return(data)
}

monotonic_events <- function(data){
  if (max(data$groups) > 1){
    for (g in 2:max(data$groups)){
      inds <- which((data$groups == g) & (data$event > 0))
      data$event[inds] <- data$event[inds] + max(
        data$event[data$groups < g])
    }
  }
  
  return(data)
}
