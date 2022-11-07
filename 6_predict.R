source("6_predict/src/train_models.R")
source("6_predict/src/plot_diagnostics.R")

#p_6 params only

#Random Forest Parameters
#maximum number of runs for Boruta feature screening algorithm
Boruta_runs <- 300
#number of trees
Boruta_trees <- 500
#number of cores
Boruta_cores <- 35
#Cross validation folds
cv_folds <- 5



p6_targets_list<- list(
  #########
  #Predict
  #Note - may be best to select features again using the local region databases
  #values can have different correlations in a region
  # Rain dominated region
  #Boruta screening
  # Not using the target map argument for now so that we get only the vhfdc1_q0.9 metric
  tar_target(p6_Boruta_rain,
             screen_Boruta(features = p5_attr_g2,
                           cluster_table = p3_gages_clusters_quants_agg_selected %>%
                             select(ID, contains('_k5')) %>%
                             rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                    high = '0.75,0.8,0.85,0.9,0.95_k5'),
                           metrics_table = p2_all_metrics_predict,
                           metric_name = 'vhfdc1_q0.9',
                           train_region = 'rain',
                           ncores = Boruta_cores, 
                           brf_runs = Boruta_runs, 
                           ntrees = Boruta_trees,
                           train_prop = 0.8,
                           nested_groups = p4_nested_groups
             ),
             #map(p2_all_metrics_names_predict),
             deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_rain,
             train_models_grid(brf_output = p6_Boruta_rain,
                               ncores = Boruta_cores,
                               v_folds = cv_folds, 
                               nested_groups = p4_nested_groups
             ),
             #map(p6_Boruta_rain),
             deployment = 'worker'
  ),
  #Test rain-dominated flood model in full rain-dominated region
  tar_target(p6_test_RF_rain_rain,
             predict_test_data(model_wf = p6_train_RF_rain$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  #Test rain-dominated flood model in snow-dominated region
  tar_target(p6_test_RF_rain_snow,
             predict_test_data(model_wf = p6_train_RF_rain$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow'),
             deployment = 'main'
  ),
  # Test on rain test region
  tar_target(p6_test_RF_rain_test_rain,
             predict_test_data_from_data(model_wf = p6_train_RF_rain$workflow,
                                         features = p5_attr_g2,
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'rain',
                                         test_gages = p6_Boruta_rain$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # Test on snow test region
  tar_target(p6_test_RF_rain_test_snow,
             predict_test_data_from_data(model_wf = p6_train_RF_rain$workflow,
                                         features = p5_attr_g2,
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'snow',
                                         test_gages = p6_Boruta_snow$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # Snow dominated region
  # Boruta screening
  tar_target(p6_Boruta_snow,
             screen_Boruta(features = p5_attr_g2,
                           cluster_table = p3_gages_clusters_quants_agg_selected %>%
                             select(ID, contains('_k5')) %>%
                             rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                    high = '0.75,0.8,0.85,0.9,0.95_k5'),
                           metrics_table = p2_all_metrics_predict,
                           metric_name = 'vhfdc1_q0.9',
                           train_region = 'snow',
                           ncores = Boruta_cores,
                           brf_runs = Boruta_runs,
                           ntrees = Boruta_trees,
                           train_prop = 0.8, 
                           nested_groups = p4_nested_groups
             ),
             #map(p2_all_metrics_names_predict),
             deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_snow,
             train_models_grid(brf_output = p6_Boruta_snow,
                               ncores = Boruta_cores,
                               v_folds = cv_folds,
                               nested_groups = p4_nested_groups
             ),
             #map(p6_Boruta_snow),
             deployment = 'worker'
  ),
  # Test on rain
  tar_target(p6_test_RF_snow_rain,
             predict_test_data(model_wf = p6_train_RF_snow$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  # Test on full snow region
  tar_target(p6_test_RF_snow_snow,
             predict_test_data(model_wf = p6_train_RF_snow$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow'),
             deployment = 'main'
  ),
  # Test on rain test region
  tar_target(p6_test_RF_snow_test_rain,
             predict_test_data_from_data(model_wf = p6_train_RF_snow$workflow,
                                         features = p5_attr_g2,
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'rain',
                                         test_gages = p6_Boruta_rain$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # Test on snow test region
  tar_target(p6_test_RF_snow_test_snow,
             predict_test_data_from_data(model_wf = p6_train_RF_snow$workflow,
                                         features = p5_attr_g2,
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'snow',
                                         test_gages = p6_Boruta_snow$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # All data in both regions
  # Boruta screening
  tar_target(p6_Boruta_rain_snow,
             screen_Boruta(features = p5_attr_g2,
                           cluster_table = p3_gages_clusters_quants_agg_selected %>%
                             select(ID, contains('_k5')) %>%
                             rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                    high = '0.75,0.8,0.85,0.9,0.95_k5'),
                           metrics_table = p2_all_metrics_predict,
                           metric_name = 'vhfdc1_q0.9',
                           train_region = c('rain', 'snow'),
                           ncores = Boruta_cores,
                           brf_runs = Boruta_runs,
                           ntrees = Boruta_trees,
                           train_prop = 0.8,
                           nested_groups = p4_nested_groups
             ),
             #map(p2_all_metrics_names_predict),
             deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_rain_snow,
             train_models_grid(brf_output = p6_Boruta_rain_snow,
                               ncores = Boruta_cores,
                               v_folds = cv_folds,
                               nested_groups = p4_nested_groups
             ),
             #map(p6_Boruta_rain_snow),
             deployment = 'worker'
  ),
  # Test on rain
  tar_target(p6_test_RF_rain_snow_rain,
             predict_test_data(model_wf = p6_train_RF_rain_snow$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  # Test on snow
  tar_target(p6_test_RF_rain_snow_snow,
             predict_test_data(model_wf = p6_train_RF_rain_snow$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow'),
             deployment = 'main'
  ),
  # Exact same testing data as rain and snow regions above
  # Boruta screening
  tar_target(p6_Boruta_rain_snow_exact,
             screen_Boruta_exact(features = p5_attr_g2,
                                 cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                   select(ID, contains('_k5')) %>%
                                   rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                          high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                 metrics_table = p2_all_metrics_predict,
                                 metric_name = 'vhfdc1_q0.9',
                                 train_region = c('rain', 'snow'),
                                 ncores = Boruta_cores,
                                 brf_runs = Boruta_runs,
                                 ntrees = Boruta_trees,
                                 train_prop = 0.8,
                                 exact_test_data = c(p6_Boruta_rain$input_data$testing$GAGES_ID,
                                                     p6_Boruta_snow$input_data$testing$GAGES_ID),
                                 nested_groups = p4_nested_groups
             ),
             #map(p2_all_metrics_names_predict),
             deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_rain_snow_exact,
             train_models_grid(brf_output = p6_Boruta_rain_snow_exact,
                               ncores = Boruta_cores,
                               v_folds = cv_folds,
                               nested_groups = p4_nested_groups),
             #map(p6_Boruta_rain_snow_exact),
             deployment = 'worker'
  ),
  # Test on full rain region
  tar_target(p6_test_RF_rain_snow_exact_rain,
             predict_test_data(model_wf = p6_train_RF_rain_snow_exact$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  # Test on full snow region
  tar_target(p6_test_RF_rain_snow_exact_snow,
             predict_test_data(model_wf = p6_train_RF_rain_snow_exact$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow'),
             deployment = 'main'
  ),
  # Test on rain test region
  tar_target(p6_test_RF_rain_snow_exact_test_rain,
             predict_test_data_from_data(model_wf = p6_train_RF_rain_snow_exact$workflow,
                                         features = p5_attr_g2,
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'rain',
                                         test_gages = p6_Boruta_rain$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # Test on snow test region
  tar_target(p6_test_RF_rain_snow_exact_test_snow,
             predict_test_data_from_data(model_wf = p6_train_RF_rain_snow_exact$workflow,
                                         features = p5_attr_g2,
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'snow',
                                         test_gages = p6_Boruta_snow$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # All data CONUS OOB error
  # Boruta screening
  tar_target(p6_Boruta_CONUS_g2,
             screen_Boruta(features = p5_attr_g2,
                           cluster_table = p3_gages_clusters_quants_agg_selected %>%
                             select(ID, contains('_k5')) %>%
                             rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                    high = '0.75,0.8,0.85,0.9,0.95_k5'),
                           metrics_table = p2_all_metrics_predict,
                           metric_name = 'vhfdc1_q0.9',
                           train_region = 'all',
                           ncores = Boruta_cores,
                           brf_runs = Boruta_runs,
                           ntrees = Boruta_trees,
                           train_prop = 0.8,
                           nested_groups = p4_nested_groups
             ),
             #map(p2_all_metrics_names_predict),
             deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_CONUS_g2,
             train_models_grid(brf_output = p6_Boruta_CONUS_g2,
                               ncores = Boruta_cores,
                               v_folds = cv_folds,
                               nested_groups = p4_nested_groups
             ),
             #map(p6_Boruta_CONUS_g2),
             deployment = 'worker'
  ),
  # Test on rain
  tar_target(p6_test_RF_CONUS_g2_rain,
             predict_test_data(model_wf = p6_train_RF_CONUS_g2$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  # Test on snow
  tar_target(p6_test_RF_CONUS_g2_snow,
             predict_test_data(model_wf = p6_train_RF_CONUS_g2$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow'),
             deployment = 'main'
  ),
  # Exact same testing data as rain and snow regions above
  # Boruta screening
  tar_target(p6_Boruta_CONUS_g2_exact,
             screen_Boruta_exact(features = p5_attr_g2,
                                 cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                   select(ID, contains('_k5')) %>%
                                   rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                          high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                 metrics_table = p2_all_metrics_predict,
                                 metric_name = 'vhfdc1_q0.9',
                                 train_region = c('all'),
                                 ncores = Boruta_cores,
                                 brf_runs = Boruta_runs,
                                 ntrees = Boruta_trees,
                                 train_prop = 0.8,
                                 exact_test_data = c(p6_Boruta_rain$input_data$testing$GAGES_ID,
                                                     p6_Boruta_snow$input_data$testing$GAGES_ID),
                                 nested_groups = p4_nested_groups
             ),
             #map(p2_all_metrics_names_predict),
             deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_CONUS_g2_exact,
             train_models_grid(brf_output = p6_Boruta_CONUS_g2_exact,
                               ncores = Boruta_cores,
                               v_folds = cv_folds,
                               nested_groups = p4_nested_groups),
             #map(p6_Boruta_CONUS_g2_exact),
             deployment = 'worker'
  ),
  # Test on full rain region
  tar_target(p6_test_RF_CONUS_g2_exact_rain,
             predict_test_data(model_wf = p6_train_RF_CONUS_g2_exact$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  # Test on full snow region
  tar_target(p6_test_RF_CONUS_g2_exact_snow,
             predict_test_data(model_wf = p6_train_RF_CONUS_g2_exact$workflow,
                               features = p5_attr_g2,
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow'),
             deployment = 'main'
  ),
  # Test on rain test region
  tar_target(p6_test_RF_CONUS_g2_exact_test_rain,
             predict_test_data_from_data(model_wf = p6_train_RF_CONUS_g2_exact$workflow,
                                         features = p5_attr_g2,
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'rain',
                                         test_gages = p6_Boruta_rain$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # Test on snow test region
  tar_target(p6_test_RF_CONUS_g2_exact_test_snow,
             predict_test_data_from_data(model_wf = p6_train_RF_CONUS_g2_exact$workflow,
                                         features = p5_attr_g2,
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'snow',
                                         test_gages = p6_Boruta_snow$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # With cluster regions as input feature
  # Boruta screening
  tar_target(p6_Boruta_CONUS_g2_exact_clust,
             screen_Boruta_exact(features = left_join(p5_attr_g2, p3_gages_clusters_quants_agg_selected %>% 
                                                        select(ID, '0.75,0.8,0.85,0.9,0.95_k5') %>%
                                                        rename(clusters = '0.75,0.8,0.85,0.9,0.95_k5'), 
                                                      by = c('GAGES_ID' = 'ID')) %>% 
                                   na.omit(),
                                 cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                   select(ID, contains('_k5')) %>%
                                   rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                          high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                 metrics_table = p2_all_metrics_predict,
                                 metric_name = 'vhfdc1_q0.9',
                                 train_region = c('all'),
                                 ncores = Boruta_cores,
                                 brf_runs = Boruta_runs,
                                 ntrees = Boruta_trees,
                                 train_prop = 0.8,
                                 exact_test_data = c(p6_Boruta_rain$input_data$testing$GAGES_ID,
                                                     p6_Boruta_snow$input_data$testing$GAGES_ID),
                                 nested_groups = p4_nested_groups
             ),
             #map(p2_all_metrics_names_predict),
             deployment = 'worker'
  ),
  #RF train
  tar_target(p6_train_RF_CONUS_g2_exact_clust,
             train_models_grid(brf_output = p6_Boruta_CONUS_g2_exact_clust,
                               ncores = Boruta_cores,
                               v_folds = cv_folds,
                               nested_groups = p4_nested_groups),
             #map(p6_Boruta_CONUS_g2_exact_clust),
             deployment = 'worker'
  ),
  # Test on full rain region
  tar_target(p6_test_RF_CONUS_g2_exact_clust_rain,
             predict_test_data(model_wf = p6_train_RF_CONUS_g2_exact_clust$workflow,
                               features = left_join(p5_attr_g2, p3_gages_clusters_quants_agg_selected %>% 
                                                      select(ID, '0.75,0.8,0.85,0.9,0.95_k5') %>%
                                                      rename(clusters = '0.75,0.8,0.85,0.9,0.95_k5'), 
                                                    by = c('GAGES_ID' = 'ID')) %>% 
                                 na.omit(),
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'rain'),
             deployment = 'main'
  ),
  # Test on full snow region
  tar_target(p6_test_RF_CONUS_g2_exact_clust_snow,
             predict_test_data(model_wf = p6_train_RF_CONUS_g2_exact_clust$workflow,
                               features = left_join(p5_attr_g2, p3_gages_clusters_quants_agg_selected %>% 
                                                      select(ID, '0.75,0.8,0.85,0.9,0.95_k5') %>%
                                                      rename(clusters = '0.75,0.8,0.85,0.9,0.95_k5'), 
                                                    by = c('GAGES_ID' = 'ID')) %>% 
                                 na.omit(),
                               cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                 select(ID, contains('_k5')) %>%
                                 rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                        high = '0.75,0.8,0.85,0.9,0.95_k5'),
                               metrics_table = p2_all_metrics_predict,
                               metric_name = 'vhfdc1_q0.9',
                               test_region = 'snow'),
             deployment = 'main'
  ),
  # Test on rain test region
  tar_target(p6_test_RF_CONUS_g2_exact_clust_test_rain,
             predict_test_data_from_data(model_wf = p6_train_RF_CONUS_g2_exact_clust$workflow,
                                         features = left_join(p5_attr_g2, p3_gages_clusters_quants_agg_selected %>% 
                                                                select(ID, '0.75,0.8,0.85,0.9,0.95_k5') %>%
                                                                rename(clusters = '0.75,0.8,0.85,0.9,0.95_k5'), 
                                                              by = c('GAGES_ID' = 'ID')) %>% 
                                           na.omit(),
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'rain',
                                         test_gages = p6_Boruta_rain$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  # Test on snow test region
  tar_target(p6_test_RF_CONUS_g2_exact_clust_test_snow,
             predict_test_data_from_data(model_wf = p6_train_RF_CONUS_g2_exact_clust$workflow,
                                         features = left_join(p5_attr_g2, p3_gages_clusters_quants_agg_selected %>% 
                                                                select(ID, '0.75,0.8,0.85,0.9,0.95_k5') %>%
                                                                rename(clusters = '0.75,0.8,0.85,0.9,0.95_k5'), 
                                                              by = c('GAGES_ID' = 'ID')) %>% 
                                           na.omit(),
                                         cluster_table = p3_gages_clusters_quants_agg_selected %>%
                                           select(ID, contains('_k5')) %>%
                                           rename(midhigh = '0.5,0.55,0.6,0.65,0.7_k5',
                                                  high = '0.75,0.8,0.85,0.9,0.95_k5'),
                                         metrics_table = p2_all_metrics_predict,
                                         metric_name = 'vhfdc1_q0.9',
                                         test_region = 'snow',
                                         test_gages = p6_Boruta_snow$input_data$testing$GAGES_ID),
             deployment = 'main'
  ),
  
  
  # Visualize Model Diagnostics:
  
  # Boruta screening
  tar_target(p6_Boruta_rain_png,
             plot_Boruta(p6_Boruta_rain$brf_All,
                         metric = p6_Boruta_rain$metric,
                         region = 'rain',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_Boruta_snow_png,
             plot_Boruta(p6_Boruta_snow$brf_All,
                         metric = p6_Boruta_snow$metric,
                         region = 'snow',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_Boruta_rain_snow_png,
             plot_Boruta(p6_Boruta_rain_snow$brf_All,
                         metric = p6_Boruta_rain_snow$metric,
                         region = 'rain_snow',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_Boruta_rain_snow_exact_png,
             plot_Boruta(p6_Boruta_rain_snow_exact$brf_All,
                         metric = p6_Boruta_rain_snow_exact$metric,
                         region = 'rain_snow_exact',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_Boruta_CONUS_g2_png,
             plot_Boruta(p6_Boruta_CONUS_g2$brf_All,
                         metric = p6_Boruta_CONUS_g2$metric,
                         region = 'CONUS_g2',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_Boruta_CONUS_g2_exact_png,
             plot_Boruta(p6_Boruta_CONUS_g2_exact$brf_All,
                         metric = p6_Boruta_CONUS_g2_exact$metric,
                         region = 'CONUS_g2_exact',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_Boruta_CONUS_g2_exact_clust_png,
             plot_Boruta(p6_Boruta_CONUS_g2_exact_clust$brf_All,
                         metric = p6_Boruta_CONUS_g2_exact_clust$metric,
                         region = 'CONUS_g2_exact_clust',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  
  # RF variable importance plot 
  #Should add error bars over X random seeds
  tar_target(p6_vip_rain_png,
             plot_vip(RF_model = p6_train_RF_rain$best_fit,
                      metric = p6_Boruta_rain$metric,
                      region = 'rain',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_vip_snow_png,
             plot_vip(RF_model = p6_train_RF_snow$best_fit,
                      metric = p6_Boruta_snow$metric,
                      region = 'snow',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_vip_rain_snow_png,
             plot_vip(RF_model = p6_train_RF_rain_snow$best_fit,
                      metric = p6_Boruta_rain_snow$metric,
                      region = 'rain_snow',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_vip_rain_snow_exact_png,
             plot_vip(RF_model = p6_train_RF_rain_snow_exact$best_fit,
                      metric = p6_Boruta_rain_snow_exact$metric,
                      region = 'rain_snow_exact',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_vip_CONUS_g2_png,
             plot_vip(RF_model = p6_train_RF_CONUS_g2$best_fit,
                      metric = p6_Boruta_CONUS_g2$metric,
                      region = 'CONUS_g2',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_vip_CONUS_g2_exact_png,
             plot_vip(RF_model = p6_train_RF_CONUS_g2_exact$best_fit,
                      metric = p6_Boruta_CONUS_g2_exact$metric,
                      region = 'CONUS_g2_exact',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_vip_CONUS_g2_exact_clust_png,
             plot_vip(RF_model = p6_train_RF_CONUS_g2_exact_clust$best_fit,
                      metric = p6_Boruta_CONUS_g2_exact_clust$metric,
                      region = 'CONUS_g2_exact_clust',
                      num_features = 10,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  
  # RF hyperparameter optimization
  tar_target(p6_hypopt_rain_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_rain$grid_params,
                                            metric = p6_Boruta_rain$metric,
                                            region = 'rain',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_snow_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_snow$grid_params,
                                            metric = p6_Boruta_snow$metric,
                                            region = 'snow',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_rain_snow_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_rain_snow$grid_params,
                                            metric = p6_Boruta_rain_snow$metric,
                                            region = 'rain_snow',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_rain_snow_exact_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_rain_snow_exact$grid_params,
                                            metric = p6_Boruta_rain_snow_exact$metric,
                                            region = 'rain_snow_exact',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_CONUS_g2_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_CONUS_g2$grid_params,
                                            metric = p6_Boruta_CONUS_g2$metric,
                                            region = 'CONUS_g2',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_CONUS_g2_exact_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_CONUS_g2_exact$grid_params,
                                            metric = p6_Boruta_CONUS_g2_exact$metric,
                                            region = 'CONUS_g2_exact',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_CONUS_g2_exact_clust_png,
             plot_hyperparam_opt_results_RF(p6_train_RF_CONUS_g2_exact_clust$grid_params,
                                            metric = p6_Boruta_CONUS_g2_exact_clust$metric,
                                            region = 'CONUS_g2_exact_clust',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  
  # Train test split boxplots for metric
  tar_target(p6_split_boxplot_rain_png,
             plot_metric_boxplot(data_split = p6_Boruta_rain$input_data,
                                 metric = p6_Boruta_rain$metric,
                                 region = 'rain',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_split_boxplot_snow_png,
             plot_metric_boxplot(data_split = p6_Boruta_snow$input_data,
                                 metric = p6_Boruta_snow$metric,
                                 region = 'snow',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_split_boxplot_rain_snow_png,
             plot_metric_boxplot(data_split = p6_Boruta_rain_snow$input_data,
                                 metric = p6_Boruta_rain_snow$metric,
                                 region = 'rain_snow',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_split_boxplot_rain_snow_exact_png,
             plot_metric_boxplot(data_split = p6_Boruta_rain_snow_exact$input_data,
                                 metric = p6_Boruta_rain_snow_exact$metric,
                                 region = 'rain_snow_exact',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_split_boxplot_CONUS_g2_png,
             plot_metric_boxplot(data_split = p6_Boruta_CONUS_g2$input_data,
                                 metric = p6_Boruta_CONUS_g2$metric,
                                 region = 'CONUS_g2',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_split_boxplot_CONUS_g2_exact_png,
             plot_metric_boxplot(data_split = p6_Boruta_CONUS_g2_exact$input_data,
                                 metric = p6_Boruta_CONUS_g2_exact$metric,
                                 region = 'CONUS_g2_exact',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_split_boxplot_CONUS_g2_exact_clust_png,
             plot_metric_boxplot(data_split = p6_Boruta_CONUS_g2_exact_clust$input_data,
                                 metric = p6_Boruta_CONUS_g2_exact_clust$metric,
                                 region = 'CONUS_g2_exact_clust',
                                 out_dir = '6_predict/out/split_boxplots'),
             deployment = 'main',
             format = 'file'
  ),
  
  # RF prediction vs. observed
  tar_target(p6_pred_obs_rain_png,
             plot_pred_obs(df_pred_obs = p6_test_RF_rain_rain$pred,
                           metric = p6_Boruta_rain$metric,
                           region = 'rain',
                           out_dir = '6_predict/out/pred_obs'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_pred_obs_snow_png,
             plot_pred_obs(df_pred_obs = p6_test_RF_snow_snow$pred,
                           metric = p6_Boruta_snow$metric,
                           region = 'snow',
                           out_dir = '6_predict/out/pred_obs'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_pred_obs_rain_snow_png,
             plot_pred_obs(df_pred_obs = NULL,
                           metric = p6_Boruta_rain_snow$metric,
                           region = 'rain_snow',
                           out_dir = '6_predict/out/pred_obs', 
                           from_predict = TRUE, 
                           model_wf = p6_train_RF_rain_snow$workflow,
                           pred_data = p6_Boruta_rain_snow$input_data$split$data),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_pred_obs_rain_snow_exact_png,
             plot_pred_obs(df_pred_obs = NULL,
                           metric = p6_Boruta_rain_snow_exact$metric,
                           region = 'rain_snow_exact',
                           out_dir = '6_predict/out/pred_obs', 
                           from_predict = TRUE, 
                           model_wf = p6_train_RF_rain_snow_exact$workflow,
                           pred_data = p6_Boruta_rain_snow_exact$input_data$split$data),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_pred_obs_CONUS_g2_png,
             plot_pred_obs(df_pred_obs = NULL,
                           metric = p6_Boruta_CONUS_g2$metric,
                           region = 'CONUS_g2',
                           out_dir = '6_predict/out/pred_obs', 
                           from_predict = TRUE, 
                           model_wf = p6_train_RF_CONUS_g2$workflow,
                           pred_data = p6_Boruta_CONUS_g2$input_data$split$data),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_pred_obs_CONUS_g2_exact_png,
             plot_pred_obs(df_pred_obs = NULL,
                           metric = p6_Boruta_CONUS_g2_exact$metric,
                           region = 'CONUS_g2_exact',
                           out_dir = '6_predict/out/pred_obs', 
                           from_predict = TRUE, 
                           model_wf = p6_train_RF_CONUS_g2_exact$workflow,
                           pred_data = p6_Boruta_CONUS_g2_exact$input_data$split$data),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_pred_obs_CONUS_g2_exact_clust_png,
             plot_pred_obs(df_pred_obs = NULL,
                           metric = p6_Boruta_CONUS_g2_exact_clust$metric,
                           region = 'CONUS_g2_exact_clust',
                           out_dir = '6_predict/out/pred_obs', 
                           from_predict = TRUE, 
                           model_wf = p6_train_RF_CONUS_g2_exact_clust$workflow,
                           pred_data = p6_Boruta_CONUS_g2_exact_clust$input_data$split$data),
             deployment = 'main',
             format = 'file'
  ),
  
  # RF residual vs. y 
  #Should be for the mean over X random seeds
  #May want to do some oversampling in extreme tails
  # plot(x = input_data %>% 
  #        filter(region == 'rain') %>% 
  #        pull({{metric_name}}), y = abs(rf$predictions - input_data %>% 
  #                                         filter(region == 'rain') %>% 
  #                                         pull({{metric_name}})))
  
  # Spatial residuals
  #Should be for the mean over X random seeds
  
  
  # Model RMSE comparison boxplots / barplots
  tar_target(p6_compare_RMSE_RF_png,
             barplot_compare_RF(rain_mod = p6_train_RF_rain, 
                                snow_mod = p6_train_RF_snow, 
                                rain_snow_mod = p6_train_RF_rain_snow, 
                                CONUS_mod = p6_train_RF_CONUS_g2,
                                test_rain_rain = p6_test_RF_rain_rain, 
                                test_rain_snow = p6_test_RF_rain_snow,
                                test_snow_rain = p6_test_RF_snow_rain, 
                                test_snow_snow = p6_test_RF_snow_snow, 
                                test_rain_snow_rain = p6_test_RF_rain_snow_rain, 
                                test_rain_snow_snow = p6_test_RF_rain_snow_snow, 
                                test_CONUS_rain = p6_test_RF_CONUS_g2_rain, 
                                test_CONUS_snow = p6_test_RF_CONUS_g2_snow,
                                flow_metric = 'vhfdc1_q0.9', 
                                perf_metric = 'rmse', 
                                out_dir = '6_predict/out'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_compare_RMSE_RF_exact_full_png,
             barplot_compare_RF(rain_mod = p6_train_RF_rain, 
                                snow_mod = p6_train_RF_snow, 
                                rain_snow_mod = p6_train_RF_rain_snow_exact, 
                                CONUS_mod = p6_train_RF_CONUS_g2_exact,
                                test_rain_rain = p6_test_RF_rain_rain, 
                                test_rain_snow = p6_test_RF_rain_snow,
                                test_snow_rain = p6_test_RF_snow_rain, 
                                test_snow_snow = p6_test_RF_snow_snow, 
                                test_rain_snow_rain = p6_test_RF_rain_snow_exact_rain, 
                                test_rain_snow_snow = p6_test_RF_rain_snow_exact_snow, 
                                test_CONUS_rain = p6_test_RF_CONUS_g2_exact_rain, 
                                test_CONUS_snow = p6_test_RF_CONUS_g2_exact_snow,
                                flow_metric = 'vhfdc1_q0.9_exact_full', 
                                perf_metric = 'rmse', 
                                out_dir = '6_predict/out'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_compare_RMSE_RF_exact_test_png,
             barplot_compare_RF(rain_mod = p6_train_RF_rain, 
                                snow_mod = p6_train_RF_snow, 
                                rain_snow_mod = p6_train_RF_rain_snow_exact, 
                                CONUS_mod = p6_train_RF_CONUS_g2_exact,
                                test_rain_rain = p6_test_RF_rain_test_rain, 
                                test_rain_snow = p6_test_RF_rain_test_snow,
                                test_snow_rain = p6_test_RF_snow_test_rain, 
                                test_snow_snow = p6_test_RF_snow_test_snow, 
                                test_rain_snow_rain = p6_test_RF_rain_snow_exact_test_rain, 
                                test_rain_snow_snow = p6_test_RF_rain_snow_exact_test_snow, 
                                test_CONUS_rain = p6_test_RF_CONUS_g2_exact_test_rain, 
                                test_CONUS_snow = p6_test_RF_CONUS_g2_exact_test_snow,
                                flow_metric = 'vhfdc1_q0.9_exact_test', 
                                perf_metric = 'rmse', 
                                out_dir = '6_predict/out'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_compare_RMSE_RF_exact_clust_test_png,
             barplot_compare_RF(rain_mod = p6_train_RF_rain, 
                                snow_mod = p6_train_RF_snow, 
                                rain_snow_mod = p6_train_RF_rain_snow_exact, 
                                CONUS_mod = p6_train_RF_CONUS_g2_exact_clust,
                                test_rain_rain = p6_test_RF_rain_test_rain, 
                                test_rain_snow = p6_test_RF_rain_test_snow,
                                test_snow_rain = p6_test_RF_snow_test_rain, 
                                test_snow_snow = p6_test_RF_snow_test_snow, 
                                test_rain_snow_rain = p6_test_RF_rain_snow_exact_test_rain, 
                                test_rain_snow_snow = p6_test_RF_rain_snow_exact_test_snow, 
                                test_CONUS_rain = p6_test_RF_CONUS_g2_exact_clust_test_rain, 
                                test_CONUS_snow = p6_test_RF_CONUS_g2_exact_clust_test_snow,
                                flow_metric = 'vhfdc1_q0.9_exact_clust_test', 
                                perf_metric = 'rmse', 
                                out_dir = '6_predict/out'),
             deployment = 'main',
             format = 'file'
  ),
  
  
  
  #####residual maps
  tar_target(p6_residual_map_RF_rain,
             make_residual_map(df_pred_obs = p6_test_RF_rain_rain$pred, 
                               sites = p1_feature_vars_g2_sf, 
                               metric = p6_test_RF_rain_rain$metric, 
                               pred_gage_ids = p6_test_RF_rain_rain$pred_gage_id, 
                               region = "rain", 
                               out_dir= "6_predict/out/pred_obs/")),
  
  tar_target(p6_residual_map_RF_snow,
             make_residual_map(df_pred_obs = p6_test_RF_snow_snow$pred, 
                               sites = p1_feature_vars_g2_sf, 
                               metric = p6_test_RF_snow_snow$metric, 
                               pred_gage_ids = p6_test_RF_snow_snow$pred_gage_id, 
                               region = "snow", 
                               out_dir= "6_predict/out/pred_obs/")),
  
  tar_target(p6_residual_map_RF_rain_snow,
             make_residual_map(df_pred_obs = NULL ,
                               sites = p1_feature_vars_g2_sf,
                               metric = p6_Boruta_rain_snow$metric,
                               pred_gage_ids = p6_Boruta_rain_snow$input_data$split$data$GAGES_ID,
                               region = "rain_snow",
                               out_dir = "6_predict/out/pred_obs/",
                               from_predict = TRUE,
                               model_wf = p6_train_RF_rain_snow$workflow,
                               pred_data = p6_Boruta_rain_snow$input_data$split$data)),
  tar_target(p6_residual_map_RF_rain_snow_exact,
             make_residual_map(df_pred_obs = NULL ,
                               sites = p1_feature_vars_g2_sf,
                               metric = p6_Boruta_rain_snow_exact$metric,
                               pred_gage_ids = p6_Boruta_rain_snow_exact$input_data$split$data$GAGES_ID,
                               region = "rain_snow_exact",
                               out_dir = "6_predict/out/pred_obs/",
                               from_predict = TRUE,
                               model_wf = p6_train_RF_rain_snow_exact$workflow,
                               pred_data = p6_Boruta_rain_snow_exact$input_data$split$data)),
  
  tar_target(p6_residual_map_RF_CONUS_g2,
             make_residual_map(df_pred_obs = NULL ,
                               sites = p1_feature_vars_g2_sf,
                               metric = p6_Boruta_CONUS_g2$metric,
                               pred_gage_ids = p6_Boruta_CONUS_g2$input_data$split$data$GAGES_ID,
                               region = "CONUS_g2",
                               out_dir = "6_predict/out/pred_obs/",
                               from_predict = TRUE,
                               model_wf = p6_train_RF_CONUS_g2$workflow,
                               pred_data = p6_Boruta_CONUS_g2$input_data$split$data)),
  
  tar_target(p6_residual_map_RF_CONUS_g2_exact,
             make_residual_map(df_pred_obs = NULL ,
                               sites = p1_feature_vars_g2_sf,
                               metric = p6_Boruta_CONUS_g2_exact$metric,
                               pred_gage_ids = p6_Boruta_CONUS_g2_exact$input_data$split$data$GAGES_ID,
                               region = "CONUS_g2_exact",
                               out_dir = "6_predict/out/pred_obs/",
                               from_predict = TRUE,
                               model_wf = p6_train_RF_CONUS_g2_exact$workflow,
                               pred_data = p6_Boruta_CONUS_g2_exact$input_data$split$data)),
  
  tar_target(p6_residual_map_RF_CONUS_g2_exact_clust,
             make_residual_map(df_pred_obs = NULL ,
                               sites = p1_feature_vars_g2_sf,
                               metric = p6_Boruta_CONUS_g2_exact_clust$metric,
                               pred_gage_ids = p6_Boruta_CONUS_g2_exact_clust$input_data$split$data$GAGES_ID,
                               region = "CONUS_g2_exact_clust",
                               out_dir = "6_predict/out/pred_obs/",
                               from_predict = TRUE,
                               model_wf = p6_train_RF_CONUS_g2_exact_clust$workflow,
                               pred_data = p6_Boruta_CONUS_g2_exact_clust$input_data$split$data)),
  
  #SHAP values and plots
  tar_target(p6_shap_multiclass,
    compute_shap(model = p4_train_RF_min_static$workflow,
                 data = p4_train_RF_min_static$best_fit$splits[[1]]$data %>%
                   select(-mean_value) %>%
                   as.data.frame(),
                 ncores = min(maxcores, SHAP_cores),
                 nsim = SHAP_nsim)
  ),
  #Global shap importance
  tar_target(p6_shap_importance_multiclass_png,
    plot_shap_global(shap = p6_shap_multiclass,
                     model_name = 'RF_static_full',
                     out_dir = "4_predict/out/random/shap/RF_static",
                     num_features = 40),
    format = "file"
  ),
  #shap dependence plots
  tar_target(p6_shap_dependence_multiclass_png,
    plot_shap_dependence(shap = p6_shap_multiclass,
                         data = p6_train_RF_min_static$best_fit$splits[[1]]$data %>%
                           select(-mean_value) %>%
                           as.data.frame(),
                     model_name = 'RF_min_static_full',
                     out_dir = "4_predict/out/random/shap/RF_min_static",
                     ncores = SHAP_cores),
    format = "file"
  ),
  
  
  #PDP and ICE plots - not ready yet
  tar_target(p6_pdp_multiclass_png,
    plot_pdp(shap = p6_shap_multiclass,
                         data = p4_train_RF_static$best_fit$splits[[1]]$data %>% 
                           select(-mean_value) %>% 
                           as.data.frame(),
                         model_name = 'RF_static_full',
                         out_dir = "4_predict/out/random/dependence/RF_static"),
    format = "file"
  )
)