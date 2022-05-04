#Random split for now.
# Should add an argument to make train/test split based on nestedness matrix
# p4_nested_gages
split_data <- function(data){
  #' 
  #' @description splits the data into training and testing
  #'
  #' @param data table of gages (rows) and features (columns). Must include
  #' COMID and GAGES_ID columns.
  #' @param train_prop proportion of the data to use for training
  #' 
  #' @value Returns a list of the dataset split, the training dataset and 
  #' the testing dataset
  
  split <- initial_split(data, prop = 0.8)
  training <- training(split)
  testing <- testing(split)
  
  return(list(split = split, training = training, testing = testing))
}

screen_Boruta <- function(features, cluster_table, metrics_table, metric_name,
                          train_region, ncores, brf_runs, ntrees){
  #' 
  #' @description Applies Boruta screening to the features. Makes a train/test
  #' split before applying the screening.
  #'
  #' @param features table of gages (rows) and features (columns). Must include
  #' COMID and GAGES_ID columns.
  #' @param cluster_table table of gages (rows) and cluster columns. Must include 
  #' columns for gage ID, and regions named midhigh and high.
  #' @param metrics_table table of metrics computed for each gage. Must include
  #' site_num column.
  #' @param metric_name character string of the column name in metrics_table to use
  #' @param train_region vector of 'rain', 'snow', or both. Can use 'all' to 
  #' use all gages in the metrics_table.
  #' @param ncores number of cores to use
  #' @param brf_runs maximum number of RF runs
  #' @param ntrees number of trees to use
  #' 
  #' @value Returns a list of the metric name, all 3 brf models, and 
  #' the input dataset (IDs, features, metric) as a list with train/test splits
  #' as the elements of the list.
  
  
  #Get only the metric_name metric
  metrics_table <- select(metrics_table, site_num, .data[[metric_name]])
  
  #determine if the metric should use the high flow region or mid-high flow region
  if (metric_name %in% c('ma','ml17', 'ml18')){
    region <- 'midhigh'
  } else if (!grepl("_q", metric_name)){
    ##other metrics from HIT
    region <- 'high'
  }else{
    ##get quantile from FDC metric name
    metric_quantile <- as.numeric(str_split(metric_name,pattern="_q")[[1]][2])
    region <- ifelse(metric_quantile < 0.75, 'midhigh', 'high')
  }
  
  #Select training region
  if(train_region == 'all'){
    metrics_table$region = 'all'
  }else{
    #select only gages for high and mid-high model regions
    if (region == 'high'){
      metrics_table <- mutate(metrics_table,
                              region = case_when(site_num %in% cluster_table$ID[cluster_table$high == 5] ~ 'snow',
                                                 site_num %in% cluster_table$ID[cluster_table$high == 2] ~ 'rain')) %>%
        drop_na()
    }else{
      metrics_table <- mutate(metrics_table,
                              region = case_when(site_num %in% cluster_table$ID[cluster_table$midhigh == 5] ~ 'snow',
                                                 site_num %in% cluster_table$ID[cluster_table$midhigh == 3] ~ 'rain'))  %>%
        drop_na()
    }
  }
  
  #Select the features for these gages
  features <- filter(features, GAGES_ID %in% metrics_table$site_num) %>%
    left_join(metrics_table %>% select(site_num, region), c('GAGES_ID' = 'site_num')) %>%
    filter(region %in% train_region)
  
  #Detect variables that are all equal across the modeling domain and remove them
  unique_col_vals <- apply(features, 2, FUN = function(x) length(unique(x[!is.na(x)])))
  features <- features[, which(unique_col_vals > 1)]
  
  #scale all features using z transform
  #features[,-which(colnames(features) %in% c('COMID', 'GAGES_ID', 'region'))] <- 
  #  scale(features[,-which(colnames(features) %in% c('COMID', 'GAGES_ID', 'region'))], 
  #        center = TRUE, scale = TRUE)
  
  #Join dataframe to match the features to the metrics
  input_data <- left_join(features, metrics_table %>% select(-region), 
                          by = c('GAGES_ID' = 'site_num'))
  
  #Split into training and testing datasets
  input_data_split <- split_data(input_data)
  
  #Apply Boruta to down-select features
  #This is parallelized by default
  #Noticed that there were switches when applied 2 times, probably due to correlation
  #applying once to CAT+dev only, then ACC+dev only
  brf_noACC <- Boruta(x = input_data_split$training %>% 
                        select(-COMID, -GAGES_ID, -{{metric_name}}, -starts_with('ACC_')) %>%
                        as.data.frame(),
                      y = input_data_split$training %>% 
                        pull({{metric_name}}),
                      pValue = 0.01,
                      mcAdj = TRUE,
                      maxRuns = brf_runs,
                      doTrace = 0,
                      holdHistory = TRUE,
                      getImp = getImpRfZ,
                      num.trees = ntrees,
                      oob.error = TRUE,
                      num.threads = ncores)
  
  brf_noCAT <- Boruta(x = input_data_split$training %>% 
                        select(-COMID, -GAGES_ID, -{{metric_name}}, -starts_with('CAT_')) %>%
                        as.data.frame(),
                      y = input_data_split$training %>% 
                        pull({{metric_name}}),
                      pValue = 0.01,
                      mcAdj = TRUE,
                      maxRuns = brf_runs,
                      doTrace = 0,
                      holdHistory = TRUE,
                      getImp = getImpRfZ,
                      num.trees = ntrees,
                      oob.error = TRUE,
                      num.threads = ncores)
  
  brf_All <- Boruta(x = input_data_split$training %>%
                      select(-COMID, -GAGES_ID, -{{metric_name}}) %>%
                      as.data.frame(),
                    y = input_data_split$training %>%
                      pull({{metric_name}}),
                    pValue = 0.01,
                    mcAdj = TRUE,
                    maxRuns = brf_runs,
                    doTrace = 0,
                    holdHistory = TRUE,
                    getImp = getImpRfZ,
                    num.trees = ntrees,
                    oob.error = TRUE,
                    num.threads = ncores)
  
  #Select all features that were not rejected over these 3 screenings
  names_unique = unique(c(names(brf_All$finalDecision[brf_All$finalDecision != 'Rejected']),
                          names(brf_noACC$finalDecision[brf_noACC$finalDecision != 'Rejected']),
                          names(brf_noCAT$finalDecision[brf_noCAT$finalDecision != 'Rejected'])
  ))
  
  #Create modeling dataset
  #Make the class match the split object
  screened_input_data <- list(split = input_data_split$split,
                              training = input_data_split$training %>% 
                                select(COMID, GAGES_ID, all_of(names_unique), 
                                       {{metric_name}}),
                              testing = input_data_split$testing %>% 
                                select(COMID, GAGES_ID, all_of(names_unique), 
                                       {{metric_name}}))
  #correcting the split table separately so that the class of the split object
  #is correct.
  screened_input_data$split$data <- screened_input_data$split$data %>% 
    select(COMID, GAGES_ID, all_of(names_unique), {{metric_name}})
  
  # metric name, all 3 brf models and the input dataset (IDs, features, metric)
  return(list(metric = metric_name, brf_noCAT = brf_noCAT, brf_noACC = brf_noACC, 
              brf_All = brf_All, input_data = screened_input_data))
}

train_models_grid <- function(brf_output, v_folds, ncores){
  #' 
  #' @description optimizes hyperparameters using a grid search
  #'
  #' @param brf_output output of the screen_Boruta function
  #' @param v_folds number of cross validation folds to use
  #' @param ncores number of cores to use
  #' 
  #' @value Returns a list of the evaluated grid parameters, the 
  #' best fit parameters, and the workflow for those parameters.
  
  #Set the parameters to be tuned
  #Test with and without write.forest
  tune_spec <- rand_forest(mode = "regression",
                           mtry = tune(), 
                           min_n = tune(), 
                           trees = tune()) %>% 
    set_engine(engine = "ranger", 
               verbose = FALSE, importance = 'permutation', 
               probability = FALSE)
  
  #Set parameter ranges
  params <- parameters(list(mtry = mtry() %>% range_set(c(10,100)), 
                            min_n = min_n() %>% range_set(c(2,10)),
                            trees = trees() %>% range_set(c(500,2000))))
  
  #Space filled grid to search
  grid <- grid_max_entropy(params,
                           size = 100,
                           iter = 1000)
  
  #number of cross validation folds (v)
  cv_folds <- vfold_cv(data = brf_output$input_data$training, v = v_folds)
  
  #specify workflow to tune the grid
  wf <- workflow() %>%
    add_model(tune_spec) %>%
    add_variables(outcomes = contains(brf_output$metric),
                  predictors = (!(contains('COMID') | contains('GAGES_ID') | contains(brf_output$metric))))
  
  #Find best model with a grid search over hyperparameters
  cl = parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  #Send variables to worker environments
  parallel::clusterExport(cl = cl, varlist = c('brf_output'), 
                          envir = environment())
  
  grid_result <- tune_grid(wf, 
                           resamples = cv_folds, 
                           grid = grid, 
                           metrics = metric_set(rmse, mae, rsq),
                           control = control_grid(
                             verbose = TRUE,
                             allow_par = TRUE,
                             extract = NULL,
                             save_pred = FALSE,
                             pkgs = NULL,
                             save_workflow = FALSE,
                             event_level = "first",
                             parallel_over = "everything"
                           )
  )
  #refine hyperparameters with a Bayesian optimizaton
  # Seems to be an error in the tune_Bayes function when initial is a previous
  # model run (extremely slow to get to step of generating candidates, and also
  # slow on that step). So, not using this method for now.
  # Bayes_result <- tune_bayes(wf,
  #                            resamples = cv_folds,
  #                            iter = 100,
  #                            metrics = metric_set(rmse, mae, rsq),
  #                            initial = grid_result,
  #                            param_info = params,
  #                            objective = exp_improve(),
  #                            control = control_bayes(
  #                              verbose = TRUE,
  #                              no_improve = 10L,
  #                              uncertain = 5,
  #                              parallel_over = NULL
  #                            )
  # )
    
  best_grid_result <- select_best(grid_result, metric = "rmse")
  
  final_wf <- finalize_workflow(wf, best_grid_result)
  
  #Fit to all training data with best hyperparameters
  #test on testing dataset
  # would help to repeat over X random seeds to get model error
  final_fit <- last_fit(final_wf, 
                        split = brf_output$input_data$split,
                        metrics = metric_set(rmse, mae, rsq)) 
  
  #test set performance metrics
  #collect_metrics(final_fit)
  
  #test set predictions
  #collect_predictions(final_fit)
  
  parallel::stopCluster(cl)
  
  return(list(grid_params = grid_result, 
              best_fit = final_fit, 
              workflow = final_wf))
}

#Test with and without WB model variables
#slight improvement
# rf_noWB <- ranger(x = input_data %>%
#                select(all_of(names_unique), -contains('WB5100')) %>%
#                as.data.frame(),
#              y = input_data %>%
#                pull({{metric_name}}),
#              oob.error = TRUE,
#              num.threads = 20,
#              write.forest = FALSE,
#              replace = TRUE,
#              sample.fraction = 1,
#              holdout = FALSE,
#              importance = 'permutation',
#              num.trees = 500)

#Test with and without monthly climate variables
#slightly worse
# rf_noMonthlyClimate <- ranger(x = input_data %>%
#                     select(all_of(names_unique), -contains('_TAV_'), -contains('_PPT_')) %>%
#                     as.data.frame(),
#                   y = input_data %>%
#                     pull({{metric_name}}),
#                   oob.error = TRUE,
#                   num.threads = 20,
#                   write.forest = FALSE,
#                   replace = TRUE,
#                   sample.fraction = 1,
#                   holdout = FALSE,
#                   importance = 'permutation',
#                   num.trees = 500)