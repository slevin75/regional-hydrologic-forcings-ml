
source("2_flow_metrics/src/calc_HIT.R")
source("2_flow_metrics/src/calc_FDC.R")

#####################
##p2_only  parameters

##statistics to compute within EflowStats
stats_HIT <- c("calc_magAverage", "calc_magLow", "calc_magHigh", 
               "calc_frequencyHigh", "calc_durationHigh", "calc_rateChange"
)

##EflowStats metrics to use
metrics <- c('ma1', 
             'ml17', 'ml18', 
             'mh20', 
             'fh2', 
             'dh1', 'dh6', 'dh15', 'dh16', 'dh23',
             'ra1', 'ra2', 'ra3', 'ra4'
)

##metrics to normalize by drainage area
metrics_DA <- c('ma1', 'dh1', 'ra1', 'ra3')

##metric ml17 to normalize by *annual mean/drainage area
metrics_ml17 <- c('ml17')

##metrics to normalize by *median/drainage area
metrics_med_DA <- NULL


p2_targets_list<- list(
  ##compute all HIT metrics for screened sites list
  tar_target(p2_HIT_metrics,
             calc_HITmetrics(site_num = p1_screened_site_list, 
                             clean_daily_flow = p1_clean_daily_flow, 
                             yearType = yearType,
                             drainArea_tab = p1_drainage_area,
                             floodThreshold_tab = p1_flood_threshold,
                             stat_vec = stats_HIT,
                             save_metrics = metrics,
                             norm_DA = metrics_DA,
                             norm_med_DA = metrics_med_DA,
                             norm_ml17 = metrics_ml17,
                             out_format = 'pivot'),
             map(p1_screened_site_list),
             deployment = 'worker'
  ),
  
  ##compute additional FDC-based metrics for screened sites list
  tar_target(p2_FDC_metrics,
             calc_FDCmetrics(site_num = p1_screened_site_list, 
                             clean_daily_flow = p1_clean_daily_flow, 
                             yearType = yearType,
                             drainArea_tab = p1_drainage_area,
                             NE_probs = NE_quants,
                             seasonal = FALSE,
                             year_start = year_start,
                             out_format = 'pivot',
                             allow_event_overlap = TRUE),
             map(p1_screened_site_list),
             deployment = 'worker'
  ),
  
  tar_target(p2_FDC_metrics_low,
             calc_FDCmetrics(site_num = p1_screened_site_list, 
                             clean_daily_flow = p1_clean_daily_flow, 
                             yearType = yearType,
                             drainArea_tab = p1_drainage_area,
                             NE_probs = NE_quants_low,
                             seasonal = FALSE,
                             year_start = year_start,
                             out_format = 'pivot',
                             threshold_type = 'low',
                             allow_event_overlap = TRUE),
             map(p1_screened_site_list),
             deployment = 'worker'
  ),
  
  ##combined metrics tables
  tar_target(p2_all_metrics,
             inner_join(p2_FDC_metrics,p2_HIT_metrics)
  ),
  #Metrics ml17 and 18 have NAs for some gages, so we will not predict them.
  #also dropping 0.98, 0.99, and 0.995 metrics
  tar_target(p2_all_metrics_predict,
             p2_all_metrics %>% 
               select(-ml17, -ml18, -contains('0.98'),
                      -contains('0.99'), -contains('0.995'))
  ),
  ##list of all the metrics names - for dynamic branching
  tar_target(p2_all_metrics_names,
             colnames(p2_all_metrics)[-1]
  ),
  tar_target(p2_all_metrics_names_predict,
             colnames(p2_all_metrics_predict)[-1]
  ),
  tar_target(p2_all_metrics_names_low,
             colnames(p2_FDC_metrics_low)[
               -c(1,
                  #Removing columns with NAs
                  grep(colnames(p2_FDC_metrics_low), pattern = 'q0.005'),
                  grep(colnames(p2_FDC_metrics_low), pattern = 'q0.01'))
             ]
  ),
  ##Compute FDC-based metrics using non-overlapping event definitions
  tar_target(p2_FDC_metrics_nonoverlapping,
             calc_FDCmetrics(site_num = p1_screened_site_list, 
                             clean_daily_flow = p1_clean_daily_flow, 
                             yearType = yearType,
                             drainArea_tab = p1_drainage_area,
                             NE_probs = c(0.5, 0.75,0.95),
                             seasonal = FALSE,
                             year_start = year_start,
                             out_format = 'pivot',
                             allow_event_overlap = FALSE),
             map(p1_screened_site_list),
             deployment = 'worker'
  ),
  
  tar_target(p2_FDC_metrics_low_nonoverlapping,
             calc_FDCmetrics(site_num = p1_screened_site_list, 
                             clean_daily_flow = p1_clean_daily_flow, 
                             yearType = yearType,
                             drainArea_tab = p1_drainage_area,
                             NE_probs = c(0.5, 0.25,0.05),
                             seasonal = FALSE,
                             year_start = year_start,
                             out_format = 'pivot',
                             threshold_type = 'low',
                             allow_event_overlap = FALSE),
             map(p1_screened_site_list),
             deployment = 'worker'
  ),
  
  ##compute seasonal FDC-based metrics using water year seasons
  tar_target(p2_FDC_metrics_season,
             calc_FDCmetrics(site_num = p1_screened_site_list_season, 
                             clean_daily_flow = p1_clean_daily_flow_season, 
                             yearType = yearType,
                             drainArea_tab = NULL,
                             NE_probs = NE_quants,
                             seasonal = TRUE,
                             season_months = season_months,
                             stat_type = 'POR',
                             year_start = season_year_start,
                             out_format = 'pivot',
                             allow_event_overlap = TRUE),
             map(p1_screened_site_list_season),
             deployment = 'worker'
  ),
  #Low flow
  tar_target(p2_FDC_metrics_season_low,
             calc_FDCmetrics(site_num = p1_screened_site_list_season, 
                             clean_daily_flow = p1_clean_daily_flow_season, 
                             yearType = yearType,
                             drainArea_tab = NULL,
                             NE_probs = NE_quants_low,
                             seasonal = TRUE,
                             season_months = season_months,
                             stat_type = 'POR',
                             year_start = season_year_start,
                             out_format = 'pivot',
                             threshold_type = 'low',
                             allow_event_overlap= TRUE),
             map(p1_screened_site_list_season),
             deployment = 'worker'
  ),
  
  ##compute seasonal FDC-based metrics using water year seasons and
  ##non-overlapping event definitions
  tar_target(p2_FDC_metrics_season_nonoverlapping,
             calc_FDCmetrics(site_num = p1_screened_site_list_season, 
                             clean_daily_flow = p1_clean_daily_flow_season, 
                             yearType = yearType,
                             drainArea_tab = NULL,
                             NE_probs = c(0.5,0.75,0.95),
                             seasonal = TRUE,
                             season_months = season_months,
                             stat_type = 'POR',
                             year_start = season_year_start,
                             out_format = 'pivot',
                             allow_event_overlap = FALSE),
             map(p1_screened_site_list_season),
             deployment = 'worker'
  ),
  #Low flow
  tar_target(p2_FDC_metrics_season_low_nonoverlapping,
             calc_FDCmetrics(site_num = p1_screened_site_list_season, 
                             clean_daily_flow = p1_clean_daily_flow_season, 
                             yearType = yearType,
                             drainArea_tab = NULL,
                             NE_probs = c(0.5,0.25,0.05),
                             seasonal = TRUE,
                             season_months = season_months,
                             stat_type = 'POR',
                             year_start = season_year_start,
                             out_format = 'pivot',
                             threshold_type = 'low',
                             allow_event_overlap= FALSE),
             map(p1_screened_site_list_season),
             deployment = 'worker'
  )
  
  ##compute seasonal FDC-based metrics using high flow seasons
  # tar_target(p2_FDC_metrics_season_high,
  #            calc_FDCmetrics(site_num = p1_screened_site_list_season_high, 
  #                            clean_daily_flow = p1_clean_daily_flow_season_high, 
  #                            yearType = yearType,
  #                            drainArea_tab = NULL,
  #                            NE_probs = NE_quants,
  #                            seasonal = TRUE,
  #                            season_months = season_months_high,
  #                            stat_type = 'POR',
  #                            year_start = season_year_start_high,
  #                            out_format = 'pivot'),
  #            map(p1_screened_site_list_season_high),
  #            deployment = 'worker'
  # )
  
  
)