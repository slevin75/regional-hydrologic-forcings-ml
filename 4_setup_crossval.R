source("4_setup_crossval/src/cross_validation_functions.R")

###############
##p4_params only

###moving window parameters
window_length <- 20  ##needs to be <= complete_years
increment <- 1
min_yrs_in_window<- 15  ##minimum number of years of data required within a window
min_windows <- 10  ##Must have this many windows available in order to plot 
###percentage of drainage area overlap between nested basins above which they will be grouped together

nested_threshold <- .5
##distance to search upstream for nested basins, in km.  note-the nhdplusTools function fails if this 
##value is 10000 or greater.
nav_distance_km <- 4500



p4_targets_list <- list(
  
  ########moving window nonstationarity stuff
  ##table with all the FDC metrics computed on a moving window. The parameter min_yrs_in_window
  ##screens out any moving windows for which there are too few years to be reliable. Can be an issue 
  ##when there are large gaps in the data record because a 20 year window might only have a few years of 
  ##actual data.The yr_ct column indicates how many complete years were in the window just to keep track of it.
  
 tar_target(p4_moving_window_metrics,
           calc_moving_window_metrics(site_num = p1_screened_site_list,
                                      window_length = window_length,
                                       increment = increment,
                                        min_yrs_in_window = min_yrs_in_window,
                                        clean_daily_flow = p1_clean_daily_flow,
                                      yearType = yearType,
                                       drainArea_tab = p1_drainage_area,
                                       NE_probs = NE_quants,
                                       digits = 3, seasonal = FALSE,
                                       year_start = year_start),
            map(p1_screened_site_list),
            deployment = 'worker'
 ),
  
  ##screen out any sites that don't have enough moving windows to plot (min_windows)
  ##using 10 for a default
  tar_target(p4_screened_plot_sites,
            screen_plot_sites(moving_window_metrics = p4_moving_window_metrics,
                              min_windows = min_windows),
            deployment = 'main'
 ),
  
 tar_target(p4_moving_window_plots_png,
            make_plots_by_site(site = p4_screened_plot_sites,
                               moving_window_metrics = p4_moving_window_metrics,
                               window_length = window_length,
                               outdir = "1_fetch/out/stationarity_plots"),
            map(p4_screened_plot_sites),
            deployment = 'worker',
            format = "file"
 ),
  
  #CONUS average
 tar_target(p4_moving_window_summary_plots_png,
            plot_trend_summary(moving_window_metrics = p4_moving_window_metrics,
                               screened_plot_sites = p4_screened_plot_sites,
                               by_cluster = FALSE,
                               outdir = "1_fetch/out/stationarity_plots"),
            deployment = 'worker',
            format = "file"
 ),
  
  #Cluster region average
 tar_target(p4_moving_window_summary_plots_cluster_png,
            plot_trend_summary(moving_window_metrics = p4_moving_window_metrics,
                               screened_plot_sites = p4_screened_plot_sites,
                               by_cluster = TRUE,
                               cluster_table = p3_gages_clusters,
                               cluster_column = p3_cluster_cols,
                               outdir = "1_fetch/out/stationarity_plots"),
            map(p3_cluster_cols),
            deployment = 'worker',
            format = "file"
 ),
  #This will not work because it doesn't know to split by quantile
  # tar_target(p4_moving_window_summary_plots_cluster_quants_png,
  #            plot_trend_summary(moving_window_metrics = p4_moving_window_metrics,
  #                               screened_plot_sites = p4_screened_plot_sites,
  #                               by_cluster = TRUE,
  #                               cluster_table = p3_gages_clusters_quants,
  #                               cluster_column = p3_cluster_cols_quants,
  #                               outdir = "1_fetch/out/stationarity_plots/by_quantiles"),
  #            map(p3_cluster_cols_quants),
  #            deployment = 'worker',
  #            format = "file"
  # ),
  
  #matrix of nested gages - proportion of overlapping area if column name gage is upstream of the row name gage, 0 otherwise
  tar_target(p4_nested_gages,
             get_nested_gages(sites_and_comids = p1_feature_vars_g2,
                              drainage_areas = p1_drainage_area,
                              nav_distance_km = nav_distance_km),
             deployment = 'worker'
  ),
  
  tar_target(p4_nested_groups,
             add_nested_group_id(p4_nested_gages, p1_drainage_area, nested_threshold))
  
  
)