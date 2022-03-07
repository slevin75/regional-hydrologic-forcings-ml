
calc_moving_window_metrics <- function(site_num, window_length, increment, 
                                     min_yrs_in_window, clean_daily_flow, 
                                     yearType, drainArea_tab, NE_probs, 
                                     digits = 3, seasonal = FALSE, 
                                     season_months = NULL, stat_type = 'POR', 
                                     year_start){
  ##subset clean_daily_flow, create a vector of starting years for a moving 
  ##window analysis, and then call the calcFDC function for each starting year.
  data <- clean_daily_flow %>%
    filter(site_no == site_num)
  
  start_yrs <- seq(from = min(data$year_val), 
                  to = (max(data$year_val) - window_length+1),
                  by = increment)

  ##remove starting years which would result in a window with no data 
  ##(if there are large data gaps, or the starting year is at the end of the record). 
  df_screen <- bind_rows(map(start_yrs, window_screen, data, window_length))
  start_yrs <- start_yrs[start_yrs %in% df_screen$start_yr[which(df_screen$years_in_window > min_yrs_in_window)] == TRUE]
  
  map_out <- map(start_yrs, calc_FDC_subset,
               window_length = window_length, site_num = site_num, 
               clean_daily_flow = data, yearType = yearType,
               drainArea_tab = drainArea_tab, NE_probs = NE_probs, 
               digits = digits, seasonal = seasonal, season_months = season_months, 
               stat_type = stat_type, year_start = year_start)
  data_out <- bind_rows(map_out)
 
  return(data_out)
}

plot_trend_summary <- function(moving_window_metrics, screened_plot_sites, 
                               by_cluster = FALSE, 
                               cluster_table = NULL, cluster_column = NULL,
                               outdir){
  if(by_cluster){
    if(is.null(cluster_table) | is.null(cluster_column)){
      stop('cluster_table and cluster_column must be specified to plot by cluster')
    }
    
    #make directory for saving the files
    outdir <- paste0(outdir, '/cluster', strsplit(cluster_column, split = 'k')[[1]] %>% last())
    dir.create(outdir, showWarnings = FALSE)
    
    #Select the specified column name
    cluster_table <- cluster_table[, c("ID", cluster_column)]
    colnames(cluster_table)[1] <- 'site_num'
    
    #Join the cluster info to the gages in the metric table
    moving_window_metrics <- dplyr::inner_join(x = moving_window_metrics, 
                                        y = cluster_table, by = 'site_num')
    colnames(moving_window_metrics)[ncol(moving_window_metrics)] <- 'cluster'
    
    #get the index from the cluster column name
    index <- strsplit(x = cluster_column, split = '_k')[[1]][1]
    
    #normalize metrics and remove NAs (when there is only 1 moving window
    #for a site,the sd will be 0)
    df_norm <- moving_window_metrics %>%
      filter(indice == index) %>%
      group_by(site_num, cluster) %>%
      filter(site_num %in% screened_plot_sites) %>%
      mutate(norm = (statistic - mean(statistic))/sd(statistic)) %>%
      mutate(indice_grp = paste0(indice, '_', cluster))
    
    ##map over indice_grp and produce a plot file for each group, with all the quantiles 
    map_out <- make_summary_plot_cluster(index = index, data = df_norm, outdir = outdir)
  }else{
    #normalize metrics and remove NAs (when there is only 1 moving window
    #for a site,the sd will be 0)
    df_norm <- moving_window_metrics %>%
      group_by(site_num,indice) %>%
      filter(site_num %in% screened_plot_sites) %>%
      mutate(norm = (statistic - mean(statistic))/sd(statistic)) %>%
      mutate(indice_grp=word(indice, start = 1, sep="_") ) 
    
    indice_grp <- unique(df_norm$indice_grp) 
    ##map over indice_grp and produce a plot file for each group, with all the quantiles 
    map_out <- map(indice_grp, make_summary_plot,
                   data = df_norm,
                   outdir = outdir) %>%
      unlist()
  }

  return(map_out)
}#end function

make_summary_plot <- function(grp, data, outdir){
  df_plot <- data %>%
    filter(indice_grp == grp)
  
  p1 <- ggplot(df_plot, aes(start_Year, norm)) + geom_point(size = .7, alpha = .2) +
    geom_smooth() +
    xlab('Start Year') +
    ylab('Normalized Metric Value') +
    facet_wrap(~indice, ncol = 3)
  

  filepath <- file.path(outdir, paste0("moving_window_summary_", grp, ".png"))
  
  ggsave(filename = filepath,
         plot = p1)
  return(filepath)
}

make_summary_plot_cluster <- function(index, data, outdir){
  #number of sites in each cluster
  num_sites <- group_by(data, indice_grp) %>% 
    summarize(length(unique(site_num))) %>% 
    pull('length(unique(site_num))')
  
  #number of observations in each cluster
  num_obs <- group_by(data, indice_grp) %>% 
    count() %>% 
    pull(n)
  
  #add the cluster number as the label order
  data$label_order <- 0
  
  #add these numbers to the label
  for (i in 1:length(num_obs)){
    #metric label_cluster number
    metric_lab <- unique(data$indice_grp)[i]
    #index for cluster
    clust_ind <- str_split(string = metric_lab, pattern = '_', simplify = T) %>% 
      last() %>% as.numeric()
    #plot label for this indice_grp
    lab <- paste0('Cluster ', clust_ind, ': ', num_sites[clust_ind], ' sites, ',
                  num_obs[clust_ind], ' obs')
    data$indice_grp[data$indice_grp == metric_lab] <- lab
    data$label_order[data$indice_grp == lab] <- clust_ind
  }
  
  
  p1 <- ggplot(data, aes(start_Year, norm)) + geom_point(size = .7, alpha = .2) +
    geom_smooth() +
    xlab('Start Year') +
    ylab('Normalized Metric Value') +
    ggtitle(paste0('Metric: ', data$indice[1])) +
    facet_wrap(~reorder(indice_grp, label_order))
  
  
  filepath <- file.path(outdir, paste0("moving_window_summary_", index, ".png"))
  
  ggsave(filename = filepath,
         plot = p1)
  return(filepath)
}

calc_FDC_subset <- function(start_yr, window_length, site_num, clean_daily_flow, 
                            yearType, drainArea_tab, NE_probs, digits = 3,
                            seasonal = FALSE, season_months = NULL, 
                            stat_type = 'POR', year_start){

 ##subset the clean_daily_flow data

  sub_clean_daily_flow <- clean_daily_flow %>%
    filter((year_val >= start_yr) & (year_val < (start_yr + window_length)))
  
  df_FDC<- calc_FDCmetrics(site_num = site_num,
                           clean_daily_flow = sub_clean_daily_flow, 
                           yearType = yearType, drainArea_tab = drainArea_tab, 
                           NE_probs = NE_probs, 
                           digits = digits, seasonal = seasonal, 
                           year_start = year_start, 
                           season_months = season_months, stat_type = stat_type)

  ##append the start year and the number of years in the computation 
  ##(in case there are missing yrs in the data, can remove these later if 
  ##there are too few years in the window)
  df_FDC$start_Year <- start_yr
  df_FDC$window_length <- window_length
  df_FDC$year_ct <- length(unique(sub_clean_daily_flow$year_val))
  
  return(df_FDC)
}

screen_plot_sites <- function(moving_window_metrics, min_windows){

  ##screen out any sites that do not have enough moving windows to plot
  metrics_screen <- moving_window_metrics %>% 
    group_by(site_num, indice) %>%
    count()

  plot_sites <- metrics_screen %>%
    filter(n > min_windows) %>%
    pull(site_num)
  
  plot_sites <- unique(plot_sites)
}

make_plots_by_site <- function(site, moving_window_metrics, window_length, outdir){
  message(site)
  metrics <- moving_window_metrics %>%
    filter(site_num == site)
  if(nrow(metrics) > 0){
    metrics$index_grp <- word(metrics$indice, start = 1, sep = "_")
    metrics$quantile <- as.factor(word(metrics$indice, start = -1, sep = "_q"))
  
    p1 <- ggplot(metrics, aes(start_Year, statistic, color = quantile)) +
      geom_point(size = .5) + facet_wrap(~index_grp, scales = "free") +
      geom_smooth(se = FALSE) +
      ggtitle(paste("USGS gage", site)) +
      theme(legend.position = "bottom")

    filepath <- file.path(outdir, paste0(site, "_", window_length, "yr_moving_window_plots.png"))
    ggsave(filename = filepath,
           plot = p1)
    
    return(filepath)
  }
}

window_screen <- function(start_yr, data, window_length){
  ##screener to count the number years of data within a window.
  ##using this to select out any starting years that result in a window with 
  #no data or with too few years of data to be reliable
  
  x <- data %>% 
    filter((year_val >= start_yr) & (year_val < (start_yr + window_length)))
  
  df <- data.frame(start_yr = start_yr, years_in_window = length(unique(x$year_val)))
  
  return(df) 
}
