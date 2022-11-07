#SHAP values
compute_shap <- function(model, data, ncores, nsim){
  #' 
  #' @description computes SHAP values
  #'
  #' @param model the model to be used
  #' @param data the dataset with predictor attributes
  #' @param ncores number of parallel cores to use
  #' @param nsim number of replicates to run (more = better SHAP estimates)
  #' 
  #' @return Returns a dataframe of SHAP values

  cl <- parallel::makeCluster(ncores, outfile = "")
  doParallel::registerDoParallel(cl)
  parallel::clusterExport(cl = cl, varlist = c('nsim'), 
                          envir = environment())
  parallel::clusterExport(cl = cl, varlist = c('predict_shap_data'))
  
  shap <- fastshap::explain(object = model, 
                            X = data, 
                            pred_wrapper = predict_shap_data, 
                            nsim = nsim, 
                            .parallel = TRUE, 
                            .inform = TRUE,
                            .paropts = list(.packages = c('tidyverse', 'tidymodels')))
  
  #For some reason, I cannot suppress the warnings of this function. It throws
  #an error with that tacked on. So, I'm allowing this warning to appear for now.
  #suppressing <anonymous>: ... may be used in an incorrect context: ‘.fun(piece, ...)’
  
  parallel::stopCluster(cl)
  
  return(shap)
}


predict_shap_data <- function(object, newdata){
  #' 
  #' @description uses the provided model to predict on the test dataset.
  #'
  #' @param object model workflow containing a single model that will be used
  #' to predict on the test_data.
  #' @param newdata dataset containing features and the metric to be predicted
  #' 
  #' @return Returns a vector of predictions
  
  preds <- predict(object, newdata, type = 'numeric') %>% 
    pull(.pred)
  
  return(preds)
}
#Need to make a separate predict function for the PDP plots (mean) because
# the function is only allowed to have 2 args.
predict_pdp_data <- function(object, newdata){
  #' 
  #' @description uses the provided model to predict on the test dataset. 
  #' Averages over all data to result in PDP
  #'
  #' @param object model workflow containing a single model that will be used
  #' to predict on the test_data.
  #' @param newdata dataset containing features and the metric to be predicted
  #' 
  #' @return Returns mean prediction
  
  preds <- predict(object, newdata, type = 'numeric') %>% 
    pull(.pred) %>%
    mean()
  
  return(preds)
}