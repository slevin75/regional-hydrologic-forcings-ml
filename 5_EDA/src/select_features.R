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

drop_high_corr_ACC <- function(features, threshold_corr){
  #' 
  #' @description Function to remove ACC attributes that are highly correlated
  #' with other attributes.
  #'
  #' @param features table of static attributes (columns) for each gage. Must have
  #' columns for COMID and GAGES_ID
  #' @param threshold_corr correlation threshold used to drop ACC attributes
  #' 
  #' @value Returns features without highly correlated ACC attributes
  
  #Correlation matrix
  cor_mat <- cor(features %>% select(-COMID, -GAGES_ID))
  
  #Find all of the features that are highly correlated
  high_corr_features <- which(cor_mat >= threshold_corr, arr.ind = T)
  #Remove diagonal
  high_corr_features <- high_corr_features[
    -which(high_corr_features[,1] == high_corr_features[,2]),] %>%
    rownames()
  
  #Drop all of the ACC values - we'll use TOT instead
  features <- select(features, -grep(pattern = 'ACC_', x = high_corr_features, 
                                     value = TRUE))
  
  #other correlations were checked manually for CAT and TOT using
  #cor_mat <- cor(features %>% select(starts_with('TOT_')))
  #high_corr_features <- which(cor_mat >= 0.9, arr.ind = T)
  #high_corr_features <- rownames(high_corr_features[
  #  -which(high_corr_features[,1] == high_corr_features[,2]),])
  
  #Then the interaction of CAT and TOT for non-climate variables:
  #cor_mat <- cor(features[,-c(1,2)] %>% select(-contains('_PPT_'), 
  #-contains('_TAV_'), -contains('WB5100')))
  
  return(features)
}
