refine_features <- function(nhdv2_attr, drop_columns){
  #' 
  #' @description Function to reduce and refine the features for use in models.
  #' It drops features that have the same value for all gages.
  #' It drops columns specified in drop_columns
  #' It converts -9999 to NA
  #'
  #' @param nhdv2_attr the tbl of static attributes (columns) for each COMID (rows)
  #' @param drop_columns character vector of column names to remove from nhdv2_attr
  #' 
  #' @return Returns a refined nhdv2_attr based on the columns to drop
  
  #Convert -9999 to NA
  nhdv2_attr[nhdv2_attr == -9999] <- NA
  
  #Detect variables that are all equal across the modeling domain and remove them
  unique_col_vals <- apply(nhdv2_attr, 2, FUN = function(x) length(unique(x[!is.na(x)])))
  nhdv2_attr_refined <- nhdv2_attr[, which(unique_col_vals > 1)] %>%
    #Remove other columns
    select(!contains(drop_columns))
  
  return(nhdv2_attr_refined)
}

drop_high_corr_ACCTOT <- function(features, threshold_corr, drop_var){
  #' 
  #' @description Function to remove ACC or TOT attributes that are highly correlated
  #' with other attributes.
  #'
  #' @param features table of static attributes (columns) for each gage. Must have
  #' columns for COMID and GAGES_ID
  #' @param threshold_corr correlation threshold used to drop ACC attributes
  #' @param drop_var the variable prefix to drop. "ACC" or "TOT"
  #' 
  #' @return Returns features without highly correlated drop_var attributes
  
  #Correlation matrix
  cor_mat <- abs(cor(features %>% select(-COMID, -GAGES_ID)))
  
  #Find all of the features that are highly correlated
  high_corr_features <- which(cor_mat >= threshold_corr, arr.ind = T)
  #Remove diagonal
  high_corr_features <- high_corr_features[
    -which(high_corr_features[,1] == high_corr_features[,2]),]
  
  #Find where drop_var is highly correlated with other variables and remove
  names_cor <- colnames(cor_mat)
  remove_vars <- vector('character', length = 0L)
  for(i in 1:nrow(high_corr_features)){
    name <- rownames(high_corr_features)[i]
    if(substr(name, 1,3) == drop_var){
      #Check that the column is not a drop_var prefix
      if(substr(names_cor[high_corr_features[i,2]], 1,3) != drop_var){
        #add the drop_var name to vector for removal
        remove_vars <- c(remove_vars, name)
      }
    }
  }
  remove_vars <- unique(remove_vars)
  
  #Drop all of the remove_vars
  features <- select(features, -{{remove_vars}})
  
  #other correlations were checked manually using
  #cor_mat <- abs(cor(features %>% select(starts_with('CAT_'))))
  #high_corr_features <- which(cor_mat >= 0.9, arr.ind = T)
  #high_corr_features <- rownames(high_corr_features[
  #  -which(high_corr_features[,1] == high_corr_features[,2]),])
  
  #Then the interaction of CAT and ACC/TOT for non-climate variables:
  #cor_mat <- abs(cor(features[,-c(1,2)] %>% select(-contains('_PPT_'), 
  #-contains('_TAV_'), -contains('WB5100'))))
  
  return(features)
}
