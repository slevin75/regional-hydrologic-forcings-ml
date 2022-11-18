#Random split for now.
# Should add an argument to make train/test split based on nestedness matrix
# p4_nested_groups
split_data <- function(data, train_prop, nested_groups){
  #' 
  #' @description splits the data into training and testing
  #'
  #' @param data table of gages (rows) and features (columns). Must include
  #' COMID and GAGES_ID columns.
  #' @param train_prop proportion of the data to use for training
  #' @param nested_groups is a data frame with gage ID and a unique nested group 
  #' ID.  Nested basins will have the same nested group id
  #' 
  #' @return Returns a list of the dataset split, the training dataset and 
  #' the testing dataset
  
  split <- initial_split(data, prop = train_prop)
  training <- training(split)
  testing <- testing(split)
  
  ##assign nested group ID to training and testing data sets
  training <- merge(training, nested_groups, by.x= "GAGES_ID", by.y = "ID")
  testing <- merge(testing, nested_groups, by.x = "GAGES_ID", by.y = "ID")
  

  regrouped <- regroup_split_data(training, testing, nested_groups)
  training <- regrouped[[1]]
  testing <- regrouped[[2]]
  

  ##replace split$in_id with re-grouped ids
  split$in_id <- which(data$GAGES_ID %in% training$GAGES_ID)
  return(list(split = split, training = training, testing = testing))
}

regroup_split_data<- function(training, testing, nested_groups){
  #'
  #' @description this function post identifies nested gages that are in both training
  #' and testing datasets and moves the gages from any split nested groups from the testing
  #' into the training. Used in post processing the initial_split function and cvfold function.
  #' @param training data frame that has GAGE_ID and nested_group_id.  Can have other attributes
  #' @param testing data frame of testing data with GAGE_ID and nested_group_id. Same attributes as training
  #' @nested_groupss data frame that lists the unique nested_gage_id for each streamgage ID.
  #'
  #' @return list containing the updated training and testing data frames
 
  ##find any nested group ids that are in both data sets
  intersection_ids <- intersect(training$nested_group_id, testing$nested_group_id)
  

  ##find all un-nested group ids
  unnested <- nested_groups %>%
    filter(nested_groups$ID %in% training$GAGES_ID | nested_groups$ID %in% testing$GAGES_ID) %>%
    group_by(nested_group_id) %>%
    count() %>%
    filter(n ==1) %>%
    select(nested_group_id)
  
  ##identify nested gages to move from testing to training and
  ##unnested replacements to move from training to testing
  move_gages<- testing %>%
    filter(nested_group_id %in% intersection_ids)
  
  replacement_gages<-training %>%
    filter(nested_group_id %in% unnested$nested_group_id) %>%
    sample_n(size = nrow(move_gages))
  
  ##remove move_gages from testing and add replacement gages
  ##remove nested_group_id column from data so it doesn't
  ##mess up subsequent functions
  testing <- testing %>%
    filter(!nested_group_id %in% intersection_ids) %>%
    bind_rows(replacement_gages) %>%
    select(-nested_group_id)
  
  
  ##remove replacement gages from training and add move_gages
  training <- training %>%
    filter(!nested_group_id %in% replacement_gages$nested_group_id) %>%
    bind_rows(move_gages) %>%
    select(-nested_group_id)

  return(list(training, testing))
  
}

screen_Boruta <- function(features, cluster_table, metrics_table, metric_name,
                          train_region, ncores, brf_runs, ntrees, train_prop, nested_groups){
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
  #' @param train_prop proportion of the data to use for training
  #' 
  #' @return Returns a list of the metric name, all 3 brf models, and 
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
  if('all' %in% train_region){
    metrics_table$region = 'all'
  }else{
    #select only gages for high and mid-high model regions
    if (region == 'high'){
      metrics_table <- mutate(metrics_table,
                              region = case_when(site_num %in% cluster_table$ID[cluster_table$high == 4] ~ 'snow',
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
  input_data_split <- split_data(input_data, train_prop = train_prop,nested_groups)


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

screen_Boruta_exact <- function(features, cluster_table, metrics_table, metric_name,
                          train_region, ncores, brf_runs, ntrees, train_prop,
                          exact_test_data, nested_groups){
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
  #' @param train_prop proportion of the data to use for training
  #' @param exact_test_data data to ensure is in the testing dataset
  #' 
  #' @return Returns a list of the metric name, all 3 brf models, and 
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
  if('all' %in% train_region){
    metrics_table$region = 'all'
  }else{
    #select only gages for high and mid-high model regions
    if (region == 'high'){
      metrics_table <- mutate(metrics_table,
                              region = case_when(site_num %in% cluster_table$ID[cluster_table$high == 4] ~ 'snow',
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
  
  #Join dataframe to match the features to the metrics
  input_data <- left_join(features, metrics_table %>% select(-region), 
                          by = c('GAGES_ID' = 'site_num'))
  
  #Split into training and testing datasets
  #use the exact_test_data:
  if (all(c('rain', 'snow') %in% train_region)){
    #use this to get a random split - ensures the classes are correct
    input_data_split <- split_data(input_data, train_prop = train_prop, nested_groups)
    #replace entries with the exact_test_data
    input_data_split$split$in_id <- which(!(input_data_split$split$data$GAGES_ID %in% 
                                            exact_test_data))
    input_data_split$training <- input_data_split$split$data[input_data_split$split$in_id,]
    input_data_split$testing <- input_data_split$split$data[-input_data_split$split$in_id,]
  }else{
    #add cluster information to the table
    input_data <- left_join(input_data, cluster_table %>% select(ID, {{region}}), 
                            by = c('GAGES_ID' = 'ID'))
    
    if(region == 'high'){
      #randomize for clusters 1, 3, and 5
      input_data_split3 <- split_data(input_data %>% filter(.data[[region]] %in% c(1,3,5)), 
                                        train_prop = train_prop,
                                      nested_groups = nested_groups)
    }else{
      #randomize for clusters 1, 2, and 4
      input_data_split3 <- split_data(input_data %>% filter(.data[[region]] %in% c(1,2,4)), 
                                        train_prop = train_prop,nested_groups = nested_groups)
    }
    
    #add on the test data from exact_test_data
    #include all other training data from rain and snow regions
    input_data_split <- input_data_split3
    input_data_split$split$data <- input_data
    input_data_split$testing <- rbind(input_data_split3$testing,
                                      input_data[which(input_data$GAGES_ID %in% exact_test_data),])
    input_data_split$training <- input_data[which(!(input_data$GAGES_ID %in% input_data_split$testing$GAGES_ID)),]
    input_data_split$split$in_id <- which(input_data_split$split$data$GAGES_ID %in% 
                                            input_data_split$training$GAGES_ID)
    
    #Remove the cluster column
    input_data_split$training <- select(input_data_split$training, -{{region}})
    input_data_split$testing <- select(input_data_split$testing, -{{region}})
    input_data_split$split$data <- select(input_data_split$split$data, -{{region}})
  }
  
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

train_models_grid <- function(brf_output, v_folds, ncores, nested_groups,
                              range_mtry, range_minn, range_trees,
                              gridsize){
  #' 
  #' @description optimizes hyperparameters using a grid search
  #'
  #' @param brf_output output of the screen_Boruta function
  #' @param v_folds number of cross validation folds to use
  #' @param ncores number of cores to use
  #' @param nested_groups matrix identifying nestedness from p4_nested_groups.
  #' @param range_mtry 2-element numeric vector for min and max values of mtry
  #' to use within ranger random forest
  #' @param range_minn 2-element numeric vector for min and max values of min_n
  #' to use within ranger random forest
  #' @param range_trees 2-element numeric vector for min and max values of trees
  #' to use within ranger random forest
  #' @param gridsize numeric number of points to evaluate within the 3D grid
  #' 
  #' @return Returns a list of the evaluated grid parameters, the 
  #' best fit parameters, and the workflow for those parameters.
  
  #faster parallelization for ranger is to use the built-in num.threads parameter.
  threads <- floor((ncores - v_folds)/v_folds)
  
  #Set the parameters to be tuned
  #Test with and without write.forest
  tune_spec <- rand_forest(mode = "regression",
                           mtry = tune(), 
                           min_n = tune(), 
                           trees = tune()) %>% 
    set_engine(engine = "ranger", num.threads = threads,
               verbose = FALSE, importance = 'permutation', 
               probability = FALSE, keep.inbag = TRUE, respect.unordered.factors = TRUE)
  
  #Set parameter ranges
  params <- parameters(list(mtry = mtry() %>% range_set(range_mtry), 
                            min_n = min_n() %>% range_set(range_minn),
                            trees = trees() %>% range_set(range_trees)))
  
  #Space filled grid to search
  grid <- grid_max_entropy(params,
                           size = gridsize,
                           iter = 1000)

  #number of cross validation folds (v)
  cv_folds <- vfold_cv(data = brf_output$input_data$training, v = v_folds)
  cv_folds <- post_process_cvfold(cv_folds, nested_groups)
  
  #specify workflow to tune the grid
  wf <- workflow() %>%
    add_model(tune_spec) %>%
    add_variables(outcomes = contains(brf_output$metric),
                  predictors = (!(contains('COMID') | contains('GAGES_ID') | contains(brf_output$metric))))
  
  #Find best model with a grid search over hyperparameters
  cl = parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  #Send variables to worker environments
  parallel::clusterExport(cl = cl, varlist = c('brf_output', 'threads'), 
                          envir = environment())
  
  grid_result <- tune_grid(wf, 
                           resamples = cv_folds, 
                           grid = grid, 
                           metrics = metric_set(rmse, mae, rsq),
                           control = control_grid(
                             verbose = FALSE,
                             allow_par = TRUE,
                             extract = NULL,
                             save_pred = FALSE,
                             pkgs = NULL,
                             save_workflow = FALSE,
                             event_level = "first",
                             parallel_over = "resamples"
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
  
  #extract workflow for best hyperparameters
  final_wf_trained <- extract_workflow(final_fit)
  
  parallel::stopCluster(cl)
  
  #remove data from grid_result to reduce file size
  grid_result$splits <- NULL
  
  return(list(grid_params = grid_result, 
              best_fit = final_fit, 
              workflow = final_wf_trained))
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


predict_test_data <- function(model_wf, features, cluster_table, metrics_table, 
                              metric_name, test_region){
  #'
  #' @description uses the provided model to predict on the test dataset and
  #' compute performance metrics
  #'
  #' @param model_wf model workflow containing a single model that will be used
  #' to predict on the test_data.
  #' @param features table of gages (rows) and features (columns). Must include
  #' COMID and GAGES_ID columns.
  #' @param cluster_table table of gages (rows) and cluster columns. Must include 
  #' columns for gage ID, and regions named midhigh and high.
  #' @param metrics_table table of metrics computed for each gage. Must include
  #' site_num column.
  #' @param metric_name character string of the column name in metrics_table to use
  #' @param test_region vector of 'rain', 'snow', or both. Can use 'all' to 
  #' use all gages in the metrics_table.
  #' 
  #' @return Returns the predictions and the performance metrics
  
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
    metric_quantile <- as.numeric(str_split(metric_name, pattern="_q")[[1]][2])
    region <- ifelse(metric_quantile < 0.75, 'midhigh', 'high')
  }
  
  #Select training region
  if(test_region == 'all'){
    metrics_table$region = 'all'
  }else{
    #select only gages for high and mid-high model regions
    if (region == 'high'){
      metrics_table <- mutate(metrics_table,
                              region = case_when(site_num %in% cluster_table$ID[cluster_table$high == 4] ~ 'snow',
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
    filter(region %in% test_region)
  
  #Join dataframe to match the features to the metrics
  test_data <- left_join(features, metrics_table %>% select(-region), 
                          by = c('GAGES_ID' = 'site_num')) %>%
    #select only the features in model_wf
    select(model_wf$fit$fit$fit$forest$independent.variable.names, contains(metric_name))
  
  test_preds <- predict(model_wf, new_data = test_data, type = 'numeric') %>%
    mutate(obs = test_data[[metric_name]])
  
  perf_metrics <- metrics(test_preds, truth = 'obs', estimate = '.pred')

  pred_gage_ids <- features$GAGES_ID
  
  return(list(metric = metric_name, pred = test_preds, metrics = perf_metrics, pred_gage_ids=pred_gage_ids))
}


predict_test_data_from_data <- function(model_wf, features, cluster_table, metrics_table, 
                              metric_name, test_region, test_gages){
  #'
  #' @description uses the provided model to predict on the test dataset and
  #' compute performance metrics
  #'
  #' @param model_wf model workflow containing a single model that will be used
  #' to predict on the test_data.
  #' @param features table of gages (rows) and features (columns). Must include
  #' COMID and GAGES_ID columns.
  #' @param cluster_table table of gages (rows) and cluster columns. Must include 
  #' columns for gage ID, and regions named midhigh and high.
  #' @param metrics_table table of metrics computed for each gage. Must include
  #' site_num column.
  #' @param metric_name character string of the column name in metrics_table to use
  #' @param test_region vector of 'rain', 'snow', or both. Can use 'all' to 
  #' use all gages in the metrics_table.
  #' @param test_gages vector of gage IDs to be used in testing
  #' 
  #' @return Returns the predictions and the performance metrics
  
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
    metric_quantile <- as.numeric(str_split(metric_name, pattern="_q")[[1]][2])
    region <- ifelse(metric_quantile < 0.75, 'midhigh', 'high')
  }
  
  #Select training region
  if(test_region == 'all'){
    metrics_table$region = 'all'
  }else{
    #select only gages for high and mid-high model regions
    if (region == 'high'){
      metrics_table <- mutate(metrics_table,
                              region = case_when(site_num %in% cluster_table$ID[cluster_table$high == 4] ~ 'snow',
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
    filter(region %in% test_region) %>%
    filter(GAGES_ID %in% test_gages)
  
  #Join dataframe to match the features to the metrics
  test_data <- left_join(features, metrics_table %>% select(-region), 
                         by = c('GAGES_ID' = 'site_num')) %>%
    #select only the features in model_wf
    select(model_wf$fit$fit$fit$forest$independent.variable.names, contains(metric_name))
  
  test_preds <- predict(model_wf, new_data = test_data, type = 'numeric') %>%
    mutate(obs = test_data[[metric_name]])
  
  perf_metrics <- metrics(test_preds, truth = 'obs', estimate = '.pred')
  
  return(list(metric = metric_name, pred = test_preds, metrics = perf_metrics))
}


post_process_cvfold<-function(cv_folds, nested_groups){
  #'
  #' @description this function post processes the vfold_cv function to 
  #' ensure that any nested gages are not separated between the in / out of sample data sets
  #' 
  #' @param cv_folds contains a list for each v_fold.  Each list has the full input data, in_id 
  #' which is the line number from the input data for selected in-samples, out_id which is null 
  #' and an id which is just the id of the v_fold.
  #' @param nested_groups data frame with a unique nested_group_id for each unnested gage, nested groups
  #' will have the same nested_group_id
  

  ##loop through number of folds (1:length(cv_folds$splits))

  for (i in 1: length(cv_folds$splits)){
    in_ids<-cv_folds$splits[[i]]$in_id
    
    in_sample_gages<-cv_folds$splits[[i]]$data[cv_folds$splits[[i]]$in_id,] %>%
      select(GAGES_ID) %>%
      mutate(in_id = row.names(.))%>%
      inner_join(nested_groups, by = c("GAGES_ID" = "ID"))
    
    out_sample_gages<-cv_folds$splits[[i]]$data %>%
      select(GAGES_ID) %>%
      mutate(in_id = row.names(.))%>%
      filter(!in_id %in% in_ids)%>%
      inner_join(nested_groups, by = c("GAGES_ID" = "ID"))
    
     regrouped <- regroup_split_data(in_sample_gages, out_sample_gages, nested_groups)
   
     in_sample_gages <- regrouped[[1]]
     out_sample_gages <- regrouped[[2]]
    ##replace the index numbers in cv_folds 
    cv_folds$splits[[i]]$in_id <- as.numeric(in_sample_gages$in_id)
  }##end i loop
  
  return(cv_folds)
  
}

