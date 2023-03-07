source("6_predict/src/train_models.R")
source("6_predict/src/plot_diagnostics.R")
source("6_predict/src/train_multiclass_models.R")
source("6_predict/src/XAI.R")
source("6_predict/src/collect_model_attrs.R")

#p6 params only

#Random Forest Parameters
#maximum number of runs for Boruta feature screening algorithm
Boruta_runs <- 300
#number of trees
Boruta_trees <- 500
#number of cores
Boruta_cores <- 60
#Cross validation folds
cv_folds <- 5

#XAI parameters
SHAP_cores <- 35
SHAP_nsim <- 20
PDP_cores <- 35

#flow types to retain for maps of NHD reaches
retain_ftypes <- c("ArtificialPath", "StreamRiver", "Connector")


p6_targets_list <- list(
  #########
  #Predict
  #Note - may be best to select features again using the local region databases
  #values can have different correlations in a region
  # Rain dominated region
  #Boruta screening
  # Not using the target map argument for now so that we get only the vhfdc1_q0.9 metric
  tar_target(p6_Boruta_rain,
             screen_Boruta(features = p5_attr_g2 %>% 
                             select(-contains('PHYSIO')),
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
                               nested_groups = p4_nested_groups,
                               range_mtry = c(5,30), 
                               range_minn = c(2,10), 
                               range_trees = c(100,500),
                               gridsize = 50
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
             screen_Boruta(features = p5_attr_g2 %>% 
                             select(-contains('PHYSIO')),
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
                               nested_groups = p4_nested_groups,
                               range_mtry = c(5,30), 
                               range_minn = c(2,10), 
                               range_trees = c(100,500),
                               gridsize = 50
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
             screen_Boruta(features = p5_attr_g2 %>% 
                             select(-contains('PHYSIO')),
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
                               nested_groups = p4_nested_groups,
                               range_mtry = c(5,30), 
                               range_minn = c(2,10), 
                               range_trees = c(100,500),
                               gridsize = 50
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
             screen_Boruta_exact(features = p5_attr_g2 %>% 
                                   select(-contains('PHYSIO')),
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
                               nested_groups = p4_nested_groups,
                               range_mtry = c(5,30), 
                               range_minn = c(2,10), 
                               range_trees = c(100,500),
                               gridsize = 50),
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
             screen_Boruta(features = p5_attr_g2 %>% 
                             select(-contains('PHYSIO')),
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
                               nested_groups = p4_nested_groups,
                               range_mtry = c(5,30), 
                               range_minn = c(2,10), 
                               range_trees = c(100,500),
                               gridsize = 50
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
             screen_Boruta_exact(features = p5_attr_g2 %>% 
                                   select(-contains('PHYSIO')),
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
                               nested_groups = p4_nested_groups,
                               range_mtry = c(5,30), 
                               range_minn = c(2,10), 
                               range_trees = c(100,500),
                               gridsize = 50),
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
             screen_Boruta_exact(features = left_join(p5_attr_g2 %>% 
                                                        select(-contains('PHYSIO')), 
                                                      p3_gages_clusters_quants_agg_selected %>% 
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
                               nested_groups = p4_nested_groups,
                               range_mtry = c(5,30), 
                               range_minn = c(2,10), 
                               range_trees = c(100,500),
                               gridsize = 50),
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
  #marginals
  tar_target(p6_hypopt_marginals_rain_png,
             plot_hyperparam_opt_marginals(p6_train_RF_rain$grid_params,
                                            metric = p6_Boruta_rain$metric,
                                            region = 'rain',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_marginals_snow_png,
             plot_hyperparam_opt_marginals(p6_train_RF_snow$grid_params,
                                            metric = p6_Boruta_snow$metric,
                                            region = 'snow',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_marginals_rain_snow_png,
             plot_hyperparam_opt_marginals(p6_train_RF_rain_snow$grid_params,
                                            metric = p6_Boruta_rain_snow$metric,
                                            region = 'rain_snow',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_marginals_rain_snow_exact_png,
             plot_hyperparam_opt_marginals(p6_train_RF_rain_snow_exact$grid_params,
                                            metric = p6_Boruta_rain_snow_exact$metric,
                                            region = 'rain_snow_exact',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_marginals_CONUS_g2_png,
             plot_hyperparam_opt_marginals(p6_train_RF_CONUS_g2$grid_params,
                                            metric = p6_Boruta_CONUS_g2$metric,
                                            region = 'CONUS_g2',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_marginals_CONUS_g2_exact_png,
             plot_hyperparam_opt_marginals(p6_train_RF_CONUS_g2_exact$grid_params,
                                            metric = p6_Boruta_CONUS_g2_exact$metric,
                                            region = 'CONUS_g2_exact',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p6_hypopt_marginals_CONUS_g2_exact_clust_png,
             plot_hyperparam_opt_marginals(p6_train_RF_CONUS_g2_exact_clust$grid_params,
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
  
  
  #Multiclass prediction model training to predict cluster membership
  tar_target(p6_cluster_model_high,
             train_multiclass(InputData = left_join(p3_gages_clusters_quants_agg_selected,
                                                    p5_attr_g2 %>%
                                                      select(-COMID),
                                                    by = c('ID' = 'GAGES_ID')) %>%
                                na.omit(),
                              y_columns = 2:7,
                              GAGEID_column = 1,
                              #"-2" from removing COMID and the shared GAGES_ID column
                              x_columns = 8:(ncol(p5_attr_g2) + ncol(p3_gages_clusters_quants_agg_selected) - 2),
                              Val_Pct = 0.2,
                              bootstraps = 20,
                              num_features_retain = 40,
                              ranger_mtry = seq(5,40,5),
                              ranger_ntree = seq(100, 1100, 200),
                              file_prefix = '6_predict/out/multiclass/High/',
                              ranger_threads = Boruta_cores,
                              probability = TRUE, save_txt_files = FALSE)
             ),
  tar_target(p6_cluster_model_high_noPhysio,
             train_multiclass(InputData = left_join(p3_gages_clusters_quants_agg_selected,
                                                    p5_attr_g2 %>%
                                                      select(-COMID, -contains('PHYSIO')),
                                                    by = c('ID' = 'GAGES_ID')) %>%
                                na.omit(),
                              y_columns = 2:7,
                              GAGEID_column = 1,
                              #"-2" from removing COMID and the shared GAGES_ID column
                              x_columns = 8:(ncol(p5_attr_g2 %>% select(-contains('PHYSIO'))) + ncol(p3_gages_clusters_quants_agg_selected) - 2),
                              Val_Pct = 0.2,
                              bootstraps = 20,
                              num_features_retain = 40,
                              ranger_mtry = seq(5,40,5),
                              ranger_ntree = seq(100, 1100, 200),
                              file_prefix = '6_predict/out/multiclass/High/NoPhysio/',
                              ranger_threads = Boruta_cores,
                              probability = TRUE, save_txt_files = FALSE)
  ),
  #Raw metric values
  tar_target(p6_cluster_model_high_noPhysio_raw_metrics,
             train_multiclass(InputData = left_join(p3_gages_clusters_quants_agg_raw_metrics,
                                                    p5_attr_g2 %>%
                                                      select(-COMID, -contains('PHYSIO')),
                                                    by = c('ID' = 'GAGES_ID')) %>%
                                na.omit(),
                              y_columns = c(2:4, 9:11),
                              GAGEID_column = 1,
                              #"-2" from removing COMID and the shared GAGES_ID column
                              x_columns = 16:(ncol(p5_attr_g2 %>% select(-contains('PHYSIO'))) + ncol(p3_gages_clusters_quants_agg_raw_metrics) - 2),
                              Val_Pct = 0.2,
                              bootstraps = 20,
                              num_features_retain = 40,
                              ranger_mtry = seq(5,40,5),
                              ranger_ntree = seq(100, 1100, 200),
                              file_prefix = '6_predict/out/multiclass/High_Raw/NoPhysio/',
                              ranger_threads = Boruta_cores,
                              probability = TRUE, save_txt_files = FALSE)
  ),
  
  #Commenting out the low flows for now. Currently untested and out of scope.
  # tar_target(p6_cluster_model_low,
  #            train_multiclass(InputData = left_join(p3_gages_clusters_quants_agg_low,
  #                                                   p5_attr_g2 %>%
  #                                                     select(-COMID),
  #                                                   by = c('ID' = 'GAGES_ID')) %>%
  #                               na.omit(),
  #                             y_columns = c(seq(2,8,2), seq(9,15,2)),
  #                             GAGEID_column = 1,
  #                             x_columns = 16:(ncol(p5_attr_g2) + 13),
  #                             Val_Pct = 0.2,
  #                             bootstraps = 20,
  #                             num_features_retain = 40,
  #                             ranger_mtry = seq(5,40,5),
  #                             ranger_ntree = seq(100, 1100, 200),
  #                             file_prefix = '6_predict/out/multiclass/Low/')),
  
  tar_target(p6_EcoFlowsAttrs_csv,
             #EcoFlows_filepath,
             '6_predict/in/EcoFlowsAttrs.csv',
             deployment = 'main',
             format = 'file'),
  
  tar_target(p6_EcoFlowsAttrs,
             read_csv(p6_EcoFlowsAttrs_csv, show_col_types = FALSE) %>%
               #add leading 0s to gages
               mutate(ID = case_when(str_length(ID) == 7 ~ str_c('0', ID), 
                                     TRUE ~ as.character(ID))) %>%
               #Remove merged gage
               filter(ID != '01362198'),
             deployment = 'main'),
  
  tar_target(p6_cluster_model_high_EcoFlowsAttrs,
             train_multiclass(InputData = left_join(p3_gages_clusters_quants_agg %>%
                                                      #edit ID number for join
                                                      mutate(ID = case_when(ID == '03584000_03584020' ~ '03584000',
                                                                            ID == '12209500_12209490' ~ '12209500',
                                                                            TRUE ~ ID)), 
                                                    p6_EcoFlowsAttrs, 
                                                    by = 'ID') %>%
                                na.omit(), 
                              y_columns = c(2:4,6:8), 
                              GAGEID_column = 1,
                              x_columns = 10:(ncol(p6_EcoFlowsAttrs) - 1 + 9), 
                              Val_Pct = 0.1, 
                              bootstraps = 20, 
                              num_features_retain = 30, 
                              ranger_mtry = seq(5,30,5), 
                              ranger_ntree = seq(100, 1500, 200),
                              ranger_threads = Boruta_cores,
                              file_prefix = '6_predict/out/multiclass/EcoFlows_High/', 
                              omit_columns = c(10:44, 57:71, 73, 74, 77, 79:132, 156:268, 300),
                              probability = FALSE, save_txt_files = TRUE),
             cue = tar_cue('never')),
  
  tar_target(p6_cluster_model_low_EcoFlowsAttrs,
             train_multiclass(InputData = left_join(p3_gages_clusters_quants_agg_low_freq %>%
                                                      #edit ID number for join
                                                      mutate(ID = case_when(ID == '03584000_03584020' ~ '03584000',
                                                                            ID == '12209500_12209490' ~ '12209500',
                                                                            TRUE ~ ID)), 
                                                    p6_EcoFlowsAttrs, 
                                                    by = 'ID') %>%
                                na.omit(), 
                              y_columns = c(2,4,6,9,11,13), 
                              GAGEID_column = 1,
                              x_columns = 16:(ncol(p6_EcoFlowsAttrs) - 1 + 15), 
                              Val_Pct = 0.1, 
                              bootstraps = 20, 
                              num_features_retain = 30, 
                              ranger_mtry = seq(5,30,5), 
                              ranger_ntree = seq(100, 1500, 200),
                              ranger_threads = Boruta_cores,
                              file_prefix = '6_predict/out/multiclass/EcoFlows_Low/', 
                              omit_columns = c(16:50, 63:77, 79, 80, 83, 85:138, 162:274, 306),
                              probability = FALSE, save_txt_files = TRUE),
             cue = tar_cue('never')),
  
  #Make class predictions for CONUS reaches
  tar_target(p6_region_class_pred_midhigh_CONUS,
             predict_multiclass(model = filter(p6_cluster_model_high$RF_models, 
                                               HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                                  pull(model),
                                reach_attrs = p5_attr_g2 %>%
                                  mutate(ID = GAGES_ID))),
  tar_target(p6_region_class_pred_high_CONUS,
             predict_multiclass(model = filter(p6_cluster_model_high$RF_models, 
                                                   HM == "0.75,0.8,0.85,0.9,0.95_k5") %>% 
                                      pull(model),
                                    reach_attrs = p5_attr_g2 %>%
                                  mutate(ID = GAGES_ID))),
  #No Physio
  tar_target(p6_region_class_pred_midhigh_noPhysio_CONUS,
             predict_multiclass(model = filter(p6_cluster_model_high_noPhysio$RF_models, 
                                               HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                                  pull(model),
                                reach_attrs = p5_attr_g2 %>%
                                  mutate(ID = GAGES_ID))),
  tar_target(p6_region_class_pred_high_noPhysio_CONUS,
             predict_multiclass(model = filter(p6_cluster_model_high_noPhysio$RF_models, 
                                               HM == "0.75,0.8,0.85,0.9,0.95_k5") %>% 
                                  pull(model),
                                reach_attrs = p5_attr_g2 %>%
                                  mutate(ID = GAGES_ID))),
  #Raw metric Values
  tar_target(p6_region_class_pred_midhigh_noPhysio_raw_metrics_CONUS,
             predict_multiclass(model = filter(p6_cluster_model_high_noPhysio_raw_metrics$RF_models, 
                                               HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                                  pull(model),
                                reach_attrs = p5_attr_g2 %>%
                                  mutate(ID = GAGES_ID))),
  tar_target(p6_region_class_pred_high_noPhysio_raw_metrics_CONUS,
             predict_multiclass(model = filter(p6_cluster_model_high_noPhysio_raw_metrics$RF_models, 
                                               HM == "0.75,0.8,0.85,0.9,0.95_k5") %>% 
                                  pull(model),
                                reach_attrs = p5_attr_g2 %>%
                                  mutate(ID = GAGES_ID))),
  #CONUS NHD reaches
  tar_target(p6_region_class_pred_midhigh_noPhysio_CONUS_NHD,
             predict_multiclass(model = filter(p6_cluster_model_high_noPhysio$RF_models, 
                                               HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                                  pull(model),
                                reach_attrs = p1_feature_vars_conus %>%
                                  mutate(ID = COMID),
                                ncores = Boruta_cores)),
  tar_target(p6_region_class_pred_high_noPhysio_CONUS_NHD,
             predict_multiclass(model = filter(p6_cluster_model_high_noPhysio$RF_models, 
                                               HM == "0.75,0.8,0.85,0.9,0.95_k5") %>% 
                                  pull(model),
                                reach_attrs = p1_feature_vars_conus %>%
                                  mutate(ID = COMID),
                                ncores = Boruta_cores)),
  #Raw metric values
#  tar_target(p6_region_class_pred_midhigh_noPhysio_raw_metrics_CONUS_NHD,
#             predict_multiclass(model = filter(p6_cluster_model_high_noPhysio_raw_metrics$RF_models, 
#                                               HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
#                                  pull(model),
#                                reach_attrs = p1_feature_vars_conus %>%
#                                  mutate(ID = COMID),
#                                ncores = Boruta_cores)),
  tar_target(p6_region_class_pred_high_noPhysio_raw_metrics_CONUS_NHD,
             predict_multiclass(model = filter(p6_cluster_model_high_noPhysio_raw_metrics$RF_models, 
                                               HM == "0.75,0.8,0.85,0.9,0.95_k5") %>% 
                                  pull(model),
                                reach_attrs = p1_feature_vars_conus %>%
                                  mutate(ID = COMID),
                                ncores = Boruta_cores)),
  
  #Maps of the most likely cluster region class for CONUS
  #example for gage points instead of reaches
  tar_target(p6_region_class_pred_midhigh_CONUS_png,
             make_class_prediction_map(class_probs = p6_region_class_pred_midhigh_CONUS,
                                       reaches = p1_sites_g2_sf,
                                       out_dir = "6_predict/out/multiclass/High",
                                       plot_threshold = 0.05,
                                       model_name = 'Midhigh_k5'),
             format = "file"),
  tar_target(p6_region_class_pred_high_CONUS_png,
             make_class_prediction_map(class_probs = p6_region_class_pred_high_CONUS,
                                       reaches = p1_sites_g2_sf,
                                       out_dir = "6_predict/out/multiclass/High",
                                       plot_threshold = 0.05,
                                       model_name = 'High_k5'),
             format = "file"),
  #No Physio
  tar_target(p6_region_class_pred_midhigh_noPhysio_CONUS_png,
             make_class_prediction_map(class_probs = p6_region_class_pred_midhigh_noPhysio_CONUS,
                                       reaches = p1_sites_g2_sf,
                                       out_dir = "6_predict/out/multiclass/High/NoPhysio",
                                       plot_threshold = 0.05,
                                       model_name = 'Midhigh_NoPhysio_k5'),
             format = "file"),
  tar_target(p6_region_class_pred_high_noPhysio_CONUS_png,
             make_class_prediction_map(class_probs = p6_region_class_pred_high_noPhysio_CONUS,
                                       reaches = p1_sites_g2_sf,
                                       out_dir = "6_predict/out/multiclass/High/NoPhysio",
                                       plot_threshold = 0.05,
                                       model_name = 'High_NoPhysio_k5'),
             format = "file"),
  #Raw metric values
  tar_target(p6_region_class_pred_midhigh_noPhysio_raw_metrics_CONUS_png,
             make_class_prediction_map(class_probs = p6_region_class_pred_midhigh_noPhysio_raw_metrics_CONUS,
                                       reaches = p1_sites_g2_sf,
                                       out_dir = "6_predict/out/multiclass/High_Raw/NoPhysio",
                                       plot_threshold = 0.05,
                                       model_name = 'Midhigh_NoPhysio_Raw_k5'),
             format = "file"),
  tar_target(p6_region_class_pred_high_noPhysio_raw_metrics_CONUS_png,
             make_class_prediction_map(class_probs = p6_region_class_pred_high_noPhysio_raw_metrics_CONUS,
                                       reaches = p1_sites_g2_sf,
                                       out_dir = "6_predict/out/multiclass/High_Raw/NoPhysio",
                                       plot_threshold = 0.05,
                                       model_name = 'High_NoPhysio_Rawk5'),
             format = "file"),
  #CONUS NHD reaches
  tar_target(p6_region_class_pred_midhigh_noPhysio_CONUS_NHD_png,
             make_class_prediction_map(class_probs = p6_region_class_pred_midhigh_noPhysio_CONUS_NHD,
                                       reaches = p1_sites_conus_sf %>%
                                         mutate(ID = COMID) %>%
                                         filter(Tidal == 0, FTYPE %in% retain_ftypes),
                                       out_dir = "6_predict/out/multiclass/High/NoPhysio",
                                       plot_threshold = 0.05,
                                       model_name = 'Midhigh_NoPhysio_NHD_k5',
                                       ncores = 5,
                                       pt_size = 0.2),
             format = "file"),
  tar_target(p6_region_class_pred_high_noPhysio_CONUS_NHD_png,
             make_class_prediction_map(class_probs = p6_region_class_pred_high_noPhysio_CONUS_NHD,
                                       reaches = p1_sites_conus_sf %>%
                                         mutate(ID = COMID) %>%
                                         filter(Tidal == 0, FTYPE %in% retain_ftypes),
                                       out_dir = "6_predict/out/multiclass/High/NoPhysio",
                                       plot_threshold = 0.05,
                                       model_name = 'High_NoPhysio_NHD_k5',
                                       ncores = 5,
                                       pt_size = 0.2),
             format = "file"),
  tar_target(p6_region_class_pred_midhigh_noPhysio_CONUS_NHD_fine_png,
             make_class_prediction_map(class_probs = p6_region_class_pred_midhigh_noPhysio_CONUS_NHD,
                                       reaches = p1_sites_conus_sf %>%
                                         mutate(ID = COMID) %>%
                                         filter(Tidal == 0, FTYPE %in% retain_ftypes),
                                       out_dir = "6_predict/out/multiclass/High/NoPhysio",
                                       plot_threshold = 0.05,
                                       model_name = 'Midhigh_NoPhysio_NHD_fine_k5',
                                       ncores = 5,
                                       pt_size = 0.1),
             format = "file"),
  tar_target(p6_region_class_pred_high_noPhysio_CONUS_NHD_fine_png,
             make_class_prediction_map(class_probs = p6_region_class_pred_high_noPhysio_CONUS_NHD,
                                       reaches = p1_sites_conus_sf %>%
                                         mutate(ID = COMID) %>%
                                         filter(Tidal == 0, FTYPE %in% retain_ftypes),
                                       out_dir = "6_predict/out/multiclass/High/NoPhysio",
                                       plot_threshold = 0.05,
                                       model_name = 'High_NoPhysio_NHD_fine_k5',
                                       ncores = 5,
                                       pt_size = 0.1),
             format = "file"),

  #CONUS NHD "transition" reaches (reaches where predicted region is less certain)
  tar_target(p6_region_transition_high_noPhysio_CONUS_NHD_png, 
             make_transition_region_map(class_probs = p6_region_class_pred_high_noPhysio_CONUS_NHD, 
                                        reaches = p1_sites_conus_sf %>%
                                          mutate(ID = COMID) %>%
                                          filter(Tidal == 0, FTYPE %in% retain_ftypes), 
                                        threshold = 0.05,
                                        out_dir = "6_predict/out/multiclass/High/NoPhysio/",
                                        model_name = 'Transitions_High_NoPhysio_NHD_fine_k5',
                                        pt_size = 0.1), 
             format = "file"), 

  #CONUS NHD "probable" reaches (reaches colored by number of clusters above probability threshold)
  tar_target(p6_region_count_high_noPhysio_CONUS_NHD_png, 
             make_region_count_map(class_probs = p6_region_class_pred_high_noPhysio_CONUS_NHD, 
                                   reaches = p1_sites_conus_sf %>%
                                     mutate(ID = COMID) %>%
                                     filter(Tidal == 0, FTYPE %in% retain_ftypes), 
                                   threshold = 0.05,
                                   out_dir = "6_predict/out/multiclass/High/NoPhysio/",
                                   model_name = 'Region_Count_High_NoPhysio_NHD_fine_k5',
                                   pt_size = 0.01),
             format = "file"),

  #Raw metric values
#  tar_target(p6_region_class_pred_midhigh_noPhysio_raw_metrics_CONUS_NHD_png,
#             make_class_prediction_map(class_probs = p6_region_class_pred_midhigh_noPhysio_raw_metrics_CONUS_NHD,
#                                       reaches = p1_sites_conus_sf %>%
#                                         mutate(ID = COMID) %>%
#                                         filter(Tidal == 0, FTYPE %in% retain_ftypes),
#                                       out_dir = "6_predict/out/multiclass/High_Raw/NoPhysio",
#                                       plot_threshold = 0.05,
#                                       model_name = 'Midhigh_NoPhysio_Raw_NHD_k5',
#                                       ncores = 5,
#                                       pt_size = 0.2),
#             format = "file"),
  tar_target(p6_region_class_pred_high_noPhysio_raw_metrics_CONUS_NHD_png,
             make_class_prediction_map(class_probs = p6_region_class_pred_high_noPhysio_raw_metrics_CONUS_NHD,
                                       reaches = p1_sites_conus_sf %>%
                                         mutate(ID = COMID) %>%
                                         filter(Tidal == 0, FTYPE %in% retain_ftypes),
                                       out_dir = "6_predict/out/multiclass/High_Raw/NoPhysio",
                                       plot_threshold = 0.05,
                                       model_name = 'High_NoPhysio_Raw_NHD_k5',
                                       ncores = 5,
                                       pt_size = 0.2),
             format = "file"),
tar_target(p6_region_class_pred_high_noPhysio_raw_metrics_CONUS_NHD_panel_for_paper_png,
           make_class_prediction_map_panel_for_paper(class_probs = p6_region_class_pred_high_noPhysio_raw_metrics_CONUS_NHD,
                                     reaches = p1_sites_conus_sf %>%
                                       mutate(ID = COMID) %>%
                                       filter(Tidal == 0, FTYPE %in% retain_ftypes),
                                     plot_threshold = 0.05,
                                     model_name = 'High_NoPhysio_Raw_NHD_k5',
                                     ncores = 5,
                                     pt_size = 0.01,
                                     fname= "6_predict/out/multiclass/High_Raw/NoPhysio/FullYear_rank1-2.png",
                                     title_str = c("Full year: most likely region", "Full year: second most likely region"),
                                     color_pal = "berlin"),
           format = "file"),

tar_target(p6_region_class_pred_high_noPhysio_CONUS_NHD_panel_for_paper_png,
           make_class_prediction_map_panel_for_paper(class_probs = p6_region_class_pred_high_noPhysio_CONUS_NHD,
                                     reaches = p1_sites_conus_sf %>%
                                       mutate(ID = COMID) %>%
                                       filter(Tidal == 0, FTYPE %in% retain_ftypes),
                                     plot_threshold = 0.05,
                                     model_name = 'High_NoPhysio_NHD_k5',
                                     ncores = 5,
                                     pt_size = 0.01,
                                     fname= "6_predict/out/multiclass/High/NoPhysio/Seasonal_NHD_rank1-2.png",
                                     title_str = c("Seasonal: most likely region", "Seasonal: second most likely region"),
                                     color_pal = "batlow"),
           format = "file"),



#  tar_target(p6_region_class_pred_midhigh_noPhysio_raw_metrics_CONUS_NHD_fine_png,
#             make_class_prediction_map(class_probs = p6_region_class_pred_midhigh_noPhysio_raw_metrics_CONUS_NHD,
#                                       reaches = p1_sites_conus_sf %>%
#                                         mutate(ID = COMID) %>%
#                                         filter(Tidal == 0, FTYPE %in% retain_ftypes),
#                                       out_dir = "6_predict/out/multiclass/High_Raw/NoPhysio",
#                                       plot_threshold = 0.05,
#                                       model_name = 'Midhigh_NoPhysio_Raw_NHD_fine_k5',
#                                       ncores = 5,
#                                       pt_size = 0.1),
#             format = "file"),
  tar_target(p6_region_class_pred_high_noPhysio_raw_metrics_CONUS_NHD_fine_png,
             make_class_prediction_map(class_probs = p6_region_class_pred_high_noPhysio_raw_metrics_CONUS_NHD,
                                       reaches = p1_sites_conus_sf %>%
                                         mutate(ID = COMID) %>%
                                         filter(Tidal == 0, FTYPE %in% retain_ftypes),
                                       out_dir = "6_predict/out/multiclass/High_Raw/NoPhysio",
                                       plot_threshold = 0.05,
                                       model_name = 'High_NoPhysio_Raw_NHD_fine_k5',
                                       ncores = 5,
                                       pt_size = 0.1),
             format = "file"),
  
  
  #SHAP values and plots
  tar_target(p6_shap_multiclass_midhigh,
             #model is a list of models used to predict. 
             #Average SHAP values will be returned for the unique
             #features across all of the models (models do not need to have the
             #same features)
             #for probability model, SHAP values are returned for each class
             compute_shap(model = filter(p6_cluster_model_high$RF_models, 
                                         HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                            pull(model),
                          data = p5_attr_g2 %>%
                            select(-COMID, -GAGES_ID),
                          ncores = SHAP_cores,
                          nsim = SHAP_nsim,
                          predict_fxn = c(predict_shap_multiclass_1, 
                                          predict_shap_multiclass_2,
                                          predict_shap_multiclass_3,
                                          predict_shap_multiclass_4,
                                          predict_shap_multiclass_5))
  ),
  tar_target(p6_shap_multiclass_high,
             #model is a list of models used to predict. 
             #Average SHAP values will be returned for the unique
             #features across all of the models (models do not need to have the
             #same features)
             #for probability model, SHAP values are returned for each class
             compute_shap(model = filter(p6_cluster_model_high$RF_models, 
                                 HM == "0.75,0.8,0.85,0.9,0.95_k5") %>% 
                            pull(model),
                          data = p5_attr_g2 %>%
                            select(-COMID, -GAGES_ID),
                          ncores = SHAP_cores,
                          nsim = SHAP_nsim,
                          predict_fxn = c(predict_shap_multiclass_1, 
                                          predict_shap_multiclass_2,
                                          predict_shap_multiclass_3,
                                          predict_shap_multiclass_4,
                                          predict_shap_multiclass_5))
  ),
  #No Physio
  tar_target(p6_shap_multiclass_midhigh_noPhysio,
             #model is a list of models used to predict. 
             #Average SHAP values will be returned for the unique
             #features across all of the models (models do not need to have the
             #same features)
             #for probability model, SHAP values are returned for each class
             compute_shap(model = filter(p6_cluster_model_high_noPhysio$RF_models, 
                                         HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                            pull(model),
                          data = p5_attr_g2 %>%
                            select(-COMID, -GAGES_ID, -contains('PHYSIO')),
                          ncores = SHAP_cores,
                          nsim = SHAP_nsim,
                          predict_fxn = c(predict_shap_multiclass_1, 
                                          predict_shap_multiclass_2,
                                          predict_shap_multiclass_3,
                                          predict_shap_multiclass_4,
                                          predict_shap_multiclass_5))
  ),
  tar_target(p6_shap_multiclass_high_noPhysio,
             #model is a list of models used to predict. 
             #Average SHAP values will be returned for the unique
             #features across all of the models (models do not need to have the
             #same features)
             #for probability model, SHAP values are returned for each class
             compute_shap(model = filter(p6_cluster_model_high_noPhysio$RF_models, 
                                         HM == "0.75,0.8,0.85,0.9,0.95_k5") %>% 
                            pull(model),
                          data = p5_attr_g2 %>%
                            select(-COMID, -GAGES_ID, -contains('PHYSIO')),
                          ncores = SHAP_cores,
                          nsim = SHAP_nsim,
                          predict_fxn = c(predict_shap_multiclass_1, 
                                          predict_shap_multiclass_2,
                                          predict_shap_multiclass_3,
                                          predict_shap_multiclass_4,
                                          predict_shap_multiclass_5))
  ),
  #Raw metric values
  tar_target(p6_shap_multiclass_midhigh_noPhysio_raw_metrics,
             #model is a list of models used to predict. 
             #Average SHAP values will be returned for the unique
             #features across all of the models (models do not need to have the
             #same features)
             #for probability model, SHAP values are returned for each class
             compute_shap(model = filter(p6_cluster_model_high_noPhysio_raw_metrics$RF_models, 
                                         HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                            pull(model),
                          data = p5_attr_g2 %>%
                            select(-COMID, -GAGES_ID, -contains('PHYSIO')),
                          ncores = SHAP_cores,
                          nsim = SHAP_nsim,
                          predict_fxn = c(predict_shap_multiclass_1, 
                                          predict_shap_multiclass_2,
                                          predict_shap_multiclass_3,
                                          predict_shap_multiclass_4,
                                          predict_shap_multiclass_5))
  ),
  tar_target(p6_shap_multiclass_high_noPhysio_raw_metrics,
             #model is a list of models used to predict. 
             #Average SHAP values will be returned for the unique
             #features across all of the models (models do not need to have the
             #same features)
             #for probability model, SHAP values are returned for each class
             compute_shap(model = filter(p6_cluster_model_high_noPhysio_raw_metrics$RF_models, 
                                         HM == "0.75,0.8,0.85,0.9,0.95_k5") %>% 
                            pull(model),
                          data = p5_attr_g2 %>%
                            select(-COMID, -GAGES_ID, -contains('PHYSIO')),
                          ncores = SHAP_cores,
                          nsim = SHAP_nsim,
                          predict_fxn = c(predict_shap_multiclass_1, 
                                          predict_shap_multiclass_2,
                                          predict_shap_multiclass_3,
                                          predict_shap_multiclass_4,
                                          predict_shap_multiclass_5))
  ),
  
  #Global shap importance
  tar_target(p6_shap_importance_multiclass_midhigh_png,
             plot_shap_global_sv(shap = p6_shap_multiclass_midhigh,
                              model_name = 'RF_multiclass_midhigh',
                              out_dir = '6_predict/out/multiclass/High/shap/midhigh',
                              num_features = 20,
                              data = p5_attr_g2,
                              sv_kind = 'beeswarm'),
             format = "file"
  ),
  tar_target(p6_shap_importance_multiclass_high_png,
             plot_shap_global_sv(shap = p6_shap_multiclass_high,
                              model_name = 'RF_multiclass_high',
                              out_dir = '6_predict/out/multiclass/High/shap/high',
                              num_features = 20,
                              data = p5_attr_g2,
                              sv_kind = 'beeswarm'),
             format = "file"
  ),
  #No Physio
  tar_target(p6_shap_importance_multiclass_midhigh_noPhysio_png,
             plot_shap_global_sv(shap = p6_shap_multiclass_midhigh_noPhysio,
                                 model_name = 'RF_multiclass_midhigh_NoPhysio',
                                 out_dir = '6_predict/out/multiclass/High/NoPhysio/shap/midhigh',
                                 num_features = 20,
                                 data = p5_attr_g2,
                                 sv_kind = 'beeswarm'),
             format = "file"
  ),
  tar_target(p6_shap_importance_multiclass_high_noPhysio_png,
             plot_shap_global_sv(shap = p6_shap_multiclass_high_noPhysio,
                                 model_name = 'RF_multiclass_high_NoPhysio',
                                 out_dir = '6_predict/out/multiclass/High/NoPhysio/shap/high',
                                 num_features = 20,
                                 data = p5_attr_g2,
                                 sv_kind = 'beeswarm'),
             format = "file"
  ),
  tar_target(p6_shap_importance_multiclass_high_noPhysio_10vars_png,
             plot_shap_global_sv(shap = p6_shap_multiclass_high_noPhysio,
                                 model_name = 'RF_multiclass_high_NoPhysio',
                                 out_dir = '6_predict/out/multiclass/High/NoPhysio/shap/high',
                                 num_features = 10,
                                 data = p5_attr_g2,
                                 sv_kind = 'beeswarm'),
             format = "file"
  ),
  #Raw metric values
  tar_target(p6_shap_importance_multiclass_midhigh_noPhysio_raw_metrics_png,
             plot_shap_global_sv(shap = p6_shap_multiclass_midhigh_noPhysio_raw_metrics,
                                 model_name = 'RF_multiclass_midhigh_NoPhysio_Raw',
                                 out_dir = '6_predict/out/multiclass/High_Raw/NoPhysio/shap/midhigh',
                                 num_features = 20,
                                 data = p5_attr_g2,
                                 sv_kind = 'beeswarm'),
             format = "file"
  ),
  tar_target(p6_shap_importance_multiclass_high_noPhysio_raw_metrics_png,
             plot_shap_global_sv(shap = p6_shap_multiclass_high_noPhysio_raw_metrics,
                                 model_name = 'RF_multiclass_high_NoPhysio_Raw',
                                 out_dir = '6_predict/out/multiclass/High_Raw/NoPhysio/shap/high',
                                 num_features = 20,
                                 data = p5_attr_g2,
                                 sv_kind = 'beeswarm'),
             format = "file"
  ),
  tar_target(p6_shap_importance_multiclass_high_noPhysio_raw_metrics_10vars_png,
             plot_shap_global_sv(shap = p6_shap_multiclass_high_noPhysio_raw_metrics,
                                 model_name = 'RF_multiclass_high_NoPhysio_Raw',
                                 out_dir = '6_predict/out/multiclass/High_Raw/NoPhysio/shap/high',
                                 num_features = 10,
                                 data = p5_attr_g2,
                                 sv_kind = 'beeswarm'),
             format = "file"
  ),
  
  #shap dependence plots
  tar_target(p6_shap_dependence_multiclass_midhigh_png,
             plot_shap_dependence_sv(shap = p6_shap_multiclass_midhigh,
                                     data = p5_attr_g2,
                                     model_name = 'RF_multiclass_midhigh',
                                     out_dir = '6_predict/out/multiclass/High/shap/midhigh',
                                     ncores = SHAP_cores),
             format = "file"
  ),
  tar_target(p6_shap_dependence_multiclass_high_png,
             plot_shap_dependence_sv(shap = p6_shap_multiclass_high,
                                     data = p5_attr_g2,
                                     model_name = 'RF_multiclass_high',
                                     out_dir = '6_predict/out/multiclass/High/shap/high',
                                     ncores = SHAP_cores),
             format = "file"
  ),
  #No Physio
  tar_target(p6_shap_dependence_multiclass_midhigh_noPhysio_png,
             plot_shap_dependence_sv(shap = p6_shap_multiclass_midhigh_noPhysio,
                                     data = p5_attr_g2,
                                     model_name = 'RF_multiclass_midhigh_NoPhysio',
                                     out_dir = '6_predict/out/multiclass/High/NoPhysio/shap/midhigh',
                                     ncores = SHAP_cores),
             format = "file"
  ),
  tar_target(p6_shap_dependence_multiclass_high_noPhysio_png,
             plot_shap_dependence_sv(shap = p6_shap_multiclass_high_noPhysio,
                                     data = p5_attr_g2,
                                     model_name = 'RF_multiclass_high_NoPhysio',
                                     out_dir = '6_predict/out/multiclass/High/NoPhysio/shap/high',
                                     ncores = SHAP_cores),
             format = "file"
  ),
  #Raw metric values
  tar_target(p6_shap_dependence_multiclass_midhigh_noPhysio_raw_metrics_png,
             plot_shap_dependence_sv(shap = p6_shap_multiclass_midhigh_noPhysio_raw_metrics,
                                     data = p5_attr_g2,
                                     model_name = 'RF_multiclass_midhigh_NoPhysio_Raw',
                                     out_dir = '6_predict/out/multiclass/High_Raw/NoPhysio/shap/midhigh',
                                     ncores = SHAP_cores),
             format = "file"
  ),
  tar_target(p6_shap_dependence_multiclass_high_noPhysio_raw_metrics_png,
             plot_shap_dependence_sv(shap = p6_shap_multiclass_high_noPhysio_raw_metrics,
                                     data = p5_attr_g2,
                                     model_name = 'RF_multiclass_high_NoPhysio_Raw',
                                     out_dir = '6_predict/out/multiclass/High_Raw/NoPhysio/shap/high',
                                     ncores = SHAP_cores),
             format = "file"
  ),
  
  
  #PDP 
  tar_target(p6_pdp_multiclass_midhigh,
             compute_pdp(model = filter(p6_cluster_model_high$RF_models, 
                                        HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                           pull(model),
                         data = p5_attr_g2,
                         predict_fxn = predict_pdp_multiclass,
                         ice = FALSE,
                         ncores = PDP_cores,
                         avg_pred = TRUE)
  ),
  tar_target(p6_pdp_multiclass_midhigh_png,
             plot_pdp(partial = p6_pdp_multiclass_midhigh,
                      data = p5_attr_g2,
                      ncores = 1,
                      ice = FALSE,
                      model_name = 'RF_multiclass_midhigh',
                      out_dir = '6_predict/out/multiclass/High/dependence/midhigh'),
             format = "file"
  ),
  tar_target(p6_pdp_multiclass_midhigh_offset_png,
             plot_pdp(partial = p6_pdp_multiclass_midhigh,
                      data = p5_attr_g2,
                      ncores = 1,
                      ice = FALSE,
                      offset = TRUE,
                      model_name = 'RF_multiclass_midhigh',
                      out_dir = '6_predict/out/multiclass/High/dependence/midhigh'),
             format = "file"
  ),
  tar_target(p6_pdp_multiclass_high,
             compute_pdp(model = filter(p6_cluster_model_high$RF_models, 
                                        HM == "0.75,0.8,0.85,0.9,0.95_k5") %>% 
                           pull(model),
                         data = p5_attr_g2,
                         predict_fxn = predict_pdp_multiclass,
                         ice = FALSE,
                         ncores = PDP_cores,
                         avg_pred = TRUE)
  ),
  tar_target(p6_pdp_multiclass_high_png,
             plot_pdp(partial = p6_pdp_multiclass_high,
                      data = p5_attr_g2,
                      ncores = 1,
                      ice = FALSE,
                      model_name = 'RF_multiclass_high',
                      out_dir = '6_predict/out/multiclass/High/dependence/high'),
             format = "file"
  ),
  tar_target(p6_pdp_multiclass_high_offset_png,
             plot_pdp(partial = p6_pdp_multiclass_high,
                      data = p5_attr_g2,
                      ncores = 1,
                      ice = FALSE,
                      offset = TRUE,
                      model_name = 'RF_multiclass_high',
                      out_dir = '6_predict/out/multiclass/High/dependence/high'),
             format = "file"
  ),
  #No Physio
  tar_target(p6_pdp_multiclass_midhigh_noPhysio,
             compute_pdp(model = filter(p6_cluster_model_high_noPhysio$RF_models, 
                                        HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                           pull(model),
                         data = p5_attr_g2,
                         predict_fxn = predict_pdp_multiclass,
                         ice = FALSE,
                         ncores = PDP_cores,
                         avg_pred = TRUE)
  ),
  tar_target(p6_pdp_multiclass_midhigh_noPhysio_png,
             plot_pdp(partial = p6_pdp_multiclass_midhigh_noPhysio,
                      data = p5_attr_g2,
                      ncores = 1,
                      ice = FALSE,
                      offset = FALSE,
                      model_name = 'RF_multiclass_midhigh_NoPhysio',
                      out_dir = '6_predict/out/multiclass/High/NoPhysio/dependence/midhigh'),
             format = "file"
  ),
  tar_target(p6_pdp_multiclass_midhigh_noPhysio_offset_png,
             plot_pdp(partial = p6_pdp_multiclass_midhigh_noPhysio,
                      data = p5_attr_g2,
                      ncores = 1,
                      ice = FALSE,
                      offset = TRUE,
                      model_name = 'RF_multiclass_midhigh_NoPhysio',
                      out_dir = '6_predict/out/multiclass/High/NoPhysio/dependence/midhigh'),
             format = "file"
  ),
  tar_target(p6_pdp_multiclass_high_noPhysio,
             compute_pdp(model = filter(p6_cluster_model_high_noPhysio$RF_models, 
                                        HM == "0.75,0.8,0.85,0.9,0.95_k5") %>% 
                           pull(model),
                         data = p5_attr_g2,
                         predict_fxn = predict_pdp_multiclass,
                         ice = FALSE,
                         ncores = PDP_cores,
                         avg_pred = TRUE)
  ),
  tar_target(p6_pdp_multiclass_high_noPhysio_png,
             plot_pdp(partial = p6_pdp_multiclass_high_noPhysio,
                      data = p5_attr_g2,
                      ncores = 1,
                      ice = FALSE,
                      offset = FALSE,
                      model_name = 'RF_multiclass_high_NoPhysio',
                      out_dir = '6_predict/out/multiclass/High/NoPhysio/dependence/high'),
             format = "file"
  ),
  tar_target(p6_pdp_multiclass_high_noPhysio_offset_png,
             plot_pdp(partial = p6_pdp_multiclass_high_noPhysio,
                      data = p5_attr_g2,
                      ncores = 1,
                      ice = FALSE,
                      offset = TRUE,
                      model_name = 'RF_multiclass_high_NoPhysio',
                      out_dir = '6_predict/out/multiclass/High/NoPhysio/dependence/high'),
             format = "file"
  ),
  #panel plot for paper
  tar_target(p6_pdp_multiclass_high_noPhysio_offset_6panel_png,
             plot_pdp_panel(partial = p6_pdp_multiclass_high_noPhysio[names(p6_pdp_multiclass_high_noPhysio) %in%
                                                                  c('CAT_PPT_APR_avg', 'CAT_PPT_JUN_avg', 'ACC_PPT_AUG_avg',
                                                                    'ACC_WB5100_FEB', 'ACC_TAV_FEB_avg', 'ACC_LSTFZ6190')],
                      data = p5_attr_g2 %>%
                        select(COMID, GAGES_ID, CAT_PPT_APR_avg, CAT_PPT_JUN_avg, ACC_PPT_AUG_avg,
                               ACC_WB5100_FEB, ACC_TAV_FEB_avg, ACC_LSTFZ6190),
                      ice = FALSE,
                      offset = TRUE,
                      ymax_offset = 0.3,
                      model_name = 'RF_multiclass_high_NoPhysio',
                      out_dir = '6_predict/out/multiclass/High/NoPhysio/dependence/high'),
             format = "file"
  ),
  #Raw metric values
  tar_target(p6_pdp_multiclass_midhigh_noPhysio_raw_metrics,
             compute_pdp(model = filter(p6_cluster_model_high_noPhysio_raw_metrics$RF_models, 
                                        HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                           pull(model),
                         data = p5_attr_g2,
                         predict_fxn = predict_pdp_multiclass,
                         ice = FALSE,
                         ncores = PDP_cores,
                         avg_pred = TRUE)
  ),
  tar_target(p6_pdp_multiclass_midhigh_noPhysio_raw_metrics_png,
             plot_pdp(partial = p6_pdp_multiclass_midhigh_noPhysio_raw_metrics,
                      data = p5_attr_g2,
                      ncores = 1,
                      ice = FALSE,
                      offset = FALSE,
                      model_name = 'RF_multiclass_midhigh_NoPhysio_Raw',
                      out_dir = '6_predict/out/multiclass/High_Raw/NoPhysio/dependence/midhigh'),
             format = "file"
  ),
  tar_target(p6_pdp_multiclass_midhigh_noPhysio_raw_metrics_offset_png,
             plot_pdp(partial = p6_pdp_multiclass_midhigh_noPhysio_raw_metrics,
                      data = p5_attr_g2,
                      ncores = 1,
                      ice = FALSE,
                      offset = TRUE,
                      model_name = 'RF_multiclass_midhigh_NoPhysio_Raw',
                      out_dir = '6_predict/out/multiclass/High_Raw/NoPhysio/dependence/midhigh'),
             format = "file"
  ),
  tar_target(p6_pdp_multiclass_high_noPhysio_raw_metrics,
             compute_pdp(model = filter(p6_cluster_model_high_noPhysio_raw_metrics$RF_models, 
                                        HM == "0.75,0.8,0.85,0.9,0.95_k5") %>% 
                           pull(model),
                         data = p5_attr_g2,
                         predict_fxn = predict_pdp_multiclass,
                         ice = FALSE,
                         ncores = PDP_cores,
                         avg_pred = TRUE)
  ),
  tar_target(p6_pdp_multiclass_high_noPhysio_raw_metrics_png,
             plot_pdp(partial = p6_pdp_multiclass_high_noPhysio_raw_metrics,
                      data = p5_attr_g2,
                      ncores = 1,
                      ice = FALSE,
                      offset = FALSE,
                      model_name = 'RF_multiclass_high_NoPhysio_Raw',
                      out_dir = '6_predict/out/multiclass/High_Raw/NoPhysio/dependence/high'),
             format = "file"
  ),
  tar_target(p6_pdp_multiclass_high_noPhysio_raw_metrics_offset_png,
             plot_pdp(partial = p6_pdp_multiclass_high_noPhysio_raw_metrics,
                      data = p5_attr_g2,
                      ncores = 1,
                      ice = FALSE,
                      offset = TRUE,
                      model_name = 'RF_multiclass_high_NoPhysio_Raw',
                      out_dir = '6_predict/out/multiclass/High_Raw/NoPhysio/dependence/high'),
             format = "file"
  ),
  
  
  #Generate final datasets containing the minimum set of attributes used for models
  tar_target(p6_min_model_attrs_high_noPhysio_CONUS_NHD,
             collect_model_attrs(model = c(filter(p6_cluster_model_high_noPhysio$RF_models, 
                                                  HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                                             pull(model),
                                           filter(p6_cluster_model_high_noPhysio_raw_metrics$RF_models,
                                                  HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                                             pull(model)),
                                 data = p1_feature_vars_conus,
                                 col_id = 'COMID',
                                 outdir = '6_predict/out/multiclass/High/NoPhysio',
                                 filename = 'CONUS_NHD_attrs.csv'),
             format = "file"
  ),
  tar_target(p6_min_model_attrs_high_noPhysio_g2,
             collect_model_attrs(model = c(filter(p6_cluster_model_high_noPhysio$RF_models, 
                                                  HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                                             pull(model),
                                           filter(p6_cluster_model_high_noPhysio_raw_metrics$RF_models,
                                                  HM == "0.5,0.55,0.6,0.65,0.7_k5") %>% 
                                             pull(model)),
                                 data = p5_attr_g2,
                                 col_id = c('COMID', 'GAGES_ID'),
                                 outdir = '6_predict/out/multiclass/High/NoPhysio',
                                 filename = 'gages_attrs.csv'),
             format = "file"
  )
)