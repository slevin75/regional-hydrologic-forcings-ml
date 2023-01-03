collect_model_attrs <- function(model, data, col_id, outdir, filename){
  #' 
  #' @description computes SHAP values
  #'
  #' @param model the list of models to be checked.
  #' @param data the dataset with predictor attribute values.
  #' @param col_id the column name within data that is the unique identifier.
  #' @param outdir where output figures are saved
  #' @param filepath name of the output file
  #' 
  #' @return filepath to the file containing the minimum set of attributes used
  
  #For all models in model, gather the attributes used
  attr_used <- vector('character', length = 0)
  for(i in 1:length(model)){
    attr_used <- unique(c(attr_used, model[[i]]$forest$independent.variable.names))
  }
  attr_used <- c(col_id, attr_used)
  data <- data[, which(colnames(data) %in% attr_used)] %>%
    select(order(colnames(.))) %>%
    select(all_of(col_id), everything())
  
  #save file
  fileout <- file.path(outdir, filename)
  write_csv(x = data, file = fileout)
  
  return(fileout)
}