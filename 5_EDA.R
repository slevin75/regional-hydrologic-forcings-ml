source("5_EDA/src/EDA_metric_plots.R")
source("5_EDA/src/EDA_feature_plots.R")
source("5_EDA/src/select_features.R")

p5_targets_list<- list(
  
  ###EDA plots
  ##maps and violin plots of all metrics by cluster.  k is the number of clusters to use in 
  ##the cluster table
  tar_target(p5_EDA_plots_metrics,
             make_EDA_metric_plots(metric = p2_all_metrics_names,
                                   k = 5, 
                                   cluster_table = p3_gages_clusters_quants_agg_selected,
                                   high_q_grep = '0.9', 
                                   low_q_grep = '0.5', 
                                   high_q_start = 0.75, 
                                   metrics_table = p2_all_metrics,
                                   gages = p1_feature_vars_g2_sf,
                                   out_dir = "5_EDA/out/metrics_plots"),
             map(p2_all_metrics_names),
             format="file"
  ),
  #Low flow
  tar_target(p5_EDA_plots_metrics_low_novhfdc3,
             make_EDA_metric_plots(metric = p2_all_metrics_names_low,
                                   k = 5,
                                   cluster_table = p3_gages_clusters_quants_agg_low_novhfdc3,
                                   high_q_grep = '0.4',
                                   low_q_grep = '0.1',
                                   high_q_start = 0.25,
                                   metrics_table = p2_FDC_metrics_low,
                                   gages = p1_feature_vars_g2_sf,
                                   out_dir = "5_EDA/out/metrics_plots_LowFlow"
             ),
             map(p2_all_metrics_names_low),
             format="file"
  ),
  
  #Down select features from full database
  tar_target(p5_screen_attr_g2,
             refine_features(nhdv2_attr = p1_feature_vars_g2, 
                             drop_columns = c('NO10AVE', 'NO200AVE', 'NO4AVE',
                                              'LAT', 'LON',
                                              # using ACC because CAT is highly correlated
                                              'CAT_PHYSIO',
                                              #CAT soils have NAs. Using TOT instead
                                              "CAT_HGA", "CAT_HGAC", "CAT_HGAD", "CAT_HGB", 
                                              "CAT_HGBC", "CAT_HGBD", "CAT_HGC", "CAT_HGCD",
                                              "CAT_HGD", "CAT_HGVAR", 
                                              #Duplicate with RF7100
                                              "RFACT",
                                              #Keeping shallow and deep soil info. Dropping middle 2.
                                              "SRL35AG", "SRL45AG",
                                              #Min elevation nearly identical for ACC and CAT
                                              "CAT_ELEV_MIN",
                                              #Canal ditch cndp better than ACC_CANALDITCH (no 0s)
                                              "TOT_CANALDITCH", "ACC_CANALDITCH",
                                              #storage available everywhere with NID and NORM STORAGE
                                              "strg",
                                              #CAT storage almost same for NID and NORM
                                              "CAT_NORM_STORAGE",
                                              #ACC and TOT correlations strange:
                                              'TOT_STRM_DENS', 
                                              #CAT hydrologic attributes very correlated with ACC
                                              'CAT_CWD', 'CAT_BFI', 'CAT_RF7100', 'CAT_SATOF', 
                                              'CAT_RH', 'CAT_WDANN', 'CAT_ET', 'CAT_PET', 'CAT_MINP6190', 
                                              'CAT_MAXP6190', 'CAT_FSTFZ6190', 'CAT_LSTFZ6190', 
                                              #5 odd TOT waterbody variables - using ACC instead
                                              'TOT_PLAYA', 'TOT_ICEMASS', 'TOT_LAKEPOND', 
                                              'TOT_RESERVOIR', 'TOT_SWAMPMARSH')),
             deployment = 'main'
  ),
  # remove TOT variables that are highly correlated with other variables (> 0.9)
  tar_target(p5_attr_g2,
             drop_high_corr_ACCTOT(features = p5_screen_attr_g2, 
                                   threshold_corr = 0.9,
                                   drop_var = 'TOT'),
             deployment = 'main'
  )
  
  
)