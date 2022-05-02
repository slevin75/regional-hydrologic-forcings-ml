screen_Boruta <- function(features, cluster_table, metrics_table, metric_name,
                          train_region, ncores, brf_runs, ntrees){
  #' 
  #' @description Applies Boruta screening to the features
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
  #' the input dataset (IDs, features, metric)
  
  
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
  features[,-which(colnames(features) %in% c('COMID', 'GAGES_ID', 'region'))] <- 
    scale(features[,-which(colnames(features) %in% c('COMID', 'GAGES_ID', 'region'))], 
          center = TRUE, scale = TRUE)
  
  #Join dataframe to match the features to the metrics
  input_data <- left_join(features, metrics_table %>% select(-region), 
                          by = c('GAGES_ID' = 'site_num'))
  
  #Apply Boruta to down-select features
  #This is parallelized by default
  #Noticed that there were switches when applied 2 times, probably due to correlation
  #applying once to CAT+ACC+dev only, then TOT+ACC+dev only
  brf_noTOT <- Boruta(x = input_data %>% 
                        select(-COMID, -GAGES_ID, -{{metric_name}}, -starts_with('TOT_')) %>%
                        as.data.frame(),
                      y = input_data %>% 
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
  
  brf_noCAT <- Boruta(x = input_data %>% 
                        select(-COMID, -GAGES_ID, -{{metric_name}}, -starts_with('CAT_')) %>%
                        as.data.frame(),
                      y = input_data %>% 
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
  
  brf_All <- Boruta(x = input_data %>%
                      select(-COMID, -GAGES_ID, -{{metric_name}}) %>%
                      as.data.frame(),
                    y = input_data %>%
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
                          names(brf_noTOT$finalDecision[brf_noTOT$finalDecision != 'Rejected']),
                          names(brf_noCAT$finalDecision[brf_noCAT$finalDecision != 'Rejected'])
  ))
  
  #Create modeling dataset
  input_data <- select(input_data, COMID, GAGES_ID, 
                       all_of(names_unique), {{metric_name}})
  
  # metric name, all 3 brf models and the input dataset (IDs, features, metric)
  return(list(metric = metric_name, brf_noCAT = brf_noCAT, brf_noTOT = brf_noTOT, 
              brf_All = brf_All, input_data = input_data))
}

train_models <- function(brf_output, ncores, ntrees){
  #Find best model with a grid search over hyperparameters
  # seed = 0? We set seed from targets, so it should be already set. But maybe not on parallel cores?,
  # max.depth = search,
  # min.node.size = search,
  # mtry = search,
  # num.trees = search)
  rf <- ranger(x = input_data %>%
                 select(all_of(names_unique)) %>%
                 as.data.frame(),
               y = input_data %>%
                 pull({{metric_name}}),
               oob.error = TRUE,
               num.threads = ncores,
               write.forest = FALSE,
               replace = TRUE,
               sample.fraction = 1,
               holdout = FALSE,
               importance = 'permutation',
               num.trees = 500,
               verbose = FALSE)
  
  #Predict with best hyperparameters over 100 random seeds to get model error
  
}


#Decide train/test split based on nestedness matrix
# - Can be random within algorithm for now
# p4_nested_gages

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