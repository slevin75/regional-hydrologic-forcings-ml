library(ranger)
library(Boruta)

#Features - all g2 sites
#Down select features from full database
#Dropping catchment soil groups becuase they have NAs
#Dropping some perfectly correlated attributes
tar_target(p6_attr_g2,
           {file_ind <- grep(p1_sb_data_g2_csv, pattern = "FAILS", invert = TRUE)
           refine_features(nhdv2_attr_path = p1_sb_data_g2_csv[file_ind], 
                drop_columns = c('NO10AVE', 'NO200AVE', 'NO4AVE', 'WRSE', 'IMPV',
                                 "ID.y", 'S1100', 'S1200', 'S1400', 'S1500', 
                                 'S1630', 'S1640', 'S1710', 'S1720', 'S1730', 
                                 'S1740', 'S1750', 'S1780', 'S1820', 'S1830', 
                                 'SOHL', 'NDAMS', 'NID', 'NORM', 'MAJOR', 'NLCD', 
                                 'ESTUARY', 'WILDFIRE', 'CNPY', 'RUN7100', '_PPT_', 
                                 '_TAV_', 'STANAME', 'LAT', 'LON', 
                                 'GRP', 'COMID.DUP', 'eco3', 'ecos3.NA', 'AggEco',
                                 '...892', '...908', 'wy', 'PHYSIO_AREA',
                                 "CAT_HGA", "CAT_HGAC", "CAT_HGAD", "CAT_HGB", 
                                 "CAT_HGBC", "CAT_HGBD", "CAT_HGC", "CAT_HGCD",
                                 "CAT_HGD", "CAT_HGVAR", 'ACC_PHYSIO_5', 
                                 'ACC_PHYSIO_6', 'ACC_PHYSIO_8', 'ACC_PHYSIO_11',
                                 'ACC_PHYSIO_14', 'ACC_PHYSIO_25', 'ACC_CWD', 
                                 'CAT_CWD', 'ACC_PIPELINE', 'ACC_ELEV_MIN', 'ACC_HGBC')) %>%
             rename(ID.x = GAGES_ID) %>%
             # add averages
             left_join(p1_avg_wildfire_g2, by = c('COMID' = 'COMID')) %>%
             left_join(p1_monthly_weather_g2, by = c('COMID' = 'COMID'))
           }
)

refine_features <- function(nhdv2_attr_path, drop_columns){
  #' 
  #' @description Function to reduce and refine the features for use in models.
  #' It drops features that have the same value for all gages.
  #' It drops columns specified in drop_columns
  #' It converts -9999 to NA
  #'
  #' @param nhdv2_attr_path the csv of static attributes (columns) for each COMID (rows)
  #' @param drop_columns character vector of column names to remove from nhdv2_attr
  #' 
  #' @value Returns a refined nhdv2_attr based on the columns to drop
  
  nhdv2_attr <- read_csv(nhdv2_attr_path, show_col_types = FALSE)
  
  #Convert -9999 to NA
  nhdv2_attr[nhdv2_attr == -9999] <- NA
  
  #Detect variables that are all equal across the modeling domain and remove them
  unique_col_vals <- apply(nhdv2_attr, 2, FUN = function(x) length(unique(x[!is.na(x)])))
  nhdv2_attr_refined <- nhdv2_attr[, which(unique_col_vals > 1)] %>%
    #Remove other columns
    select(!contains(drop_columns))

  return(nhdv2_attr_refined)
}

#Check highly correlated relationships and remove as needed
which(test >= 0.999, arr.ind = T)[-which(which(test >= 0.999, arr.ind = T)[,1] == which(test >= 0.999, arr.ind = T)[,2]),]

#Metrics to predict
p1_FDC_metrics
p1_HIT_metrics

#separate variables for high flow quantiles and mid-high flow quantiles
p6_metrics_high <-
p6_metrics_midhigh <- 


#select only gages for our model regions


#Decide train/test split based on nestedness matrix
# - Can be random within algorithm for now
p4_nested_gages

#Apply Boruta to down-select features
#Single dataframe of observations and features
input_data <- left_join(p2_SC_observations %>% 
                          drop_na() %>%
                          select(subsegid, mean_value), 
                        p2_nhdv2_attr_refined %>% 
                          select(-hru_segment), 
                        by = c('subsegid' = 'PRMS_segid')) %>% 
  select(-subsegid)

#Test if I can run in parallel within Boruta
#Test if result is different for oob.error = TRUE vs. FALSE
#Test with different max depth - time and results

rf <- ranger(data = as.data.frame(input_data), 
             dependent.variable.name = 'mean_value', 
             importance = 'permutation', 
             num.trees = 5000, 
             write.forest = FALSE, 
             replace = TRUE, 
             sample.fraction = 1, 
             holdout = FALSE)

#This is parallelized by default??
brf <- Boruta(x = as.data.frame(input_data[,-1]), 
              y = input_data$mean_value, 
              pValue = 0.01, 
              maxRuns = 11, 
              doTrace = 0, 
              holdHistory = TRUE, 
              getImp = getImpRfZ, 
              num.trees = 500,
              oob.error = FALSE, 
              num.threads = 15)

ranger(#seed = 0? We set seed from targets, so it should be already set. But maybe not on parallel cores?,
  num.threads = detectCores,
  oob.error = FALSE,
  verbose = FALSE,
  holdout = TRUE? test set based variable importance. ,
  max.depth = 5?,
  min.node.size = grid search)


#Find best model with a grid search over hyperparameters


#Predict with best model
