#SHAP values
compute_shap <- function(model, data, ncores, nsim, predict_fxn,
                         avg_pred = TRUE){
  #' 
  #' @description computes SHAP values
  #'
  #' @param model the model to be used. Can be a list of models, in which case
  #' Average SHAP values will be returned for the unique features across all of 
  #' the models (models do not need to have the same features)
  #' @param data the dataset with predictor attributes. When model is a list, the
  #' predictors needed for each model are selected from data.
  #' @param ncores number of parallel cores to use
  #' @param nsim number of replicates to run (more = better SHAP estimates)
  #' @param predict_fxn the prediction function to use, unquoted. For multiclass
  #' models, a vector of prediction functions that return class probabilities 
  #' for each of the classes in separate functions.
  #' @param avg_pred logical indicating what the predict_fxn provides. When TRUE, it
  #' should provide an average over all models (bootstraps) and shap values
  #' correspond to that average. When FALSE, shap values are computed for each
  #' individual bootstrap.
  #' 
  #' @return Returns a dataframe of SHAP values
  #'
  #' @note For some reason, I cannot suppress the warnings of this function.
  #' So, I'm allowing this warning to appear for now.
  #' suppressing <anonymous>: ... may be used in an incorrect context: ?.fun(piece, ...)? 
  
  #Check if data is a tibble and convert to data frame if needed.
  if(is_tibble(data)){
    data <- as.data.frame(data)
  }
  
  #set up parallel computations
  cl <- parallel::makeCluster(ncores, outfile = "")
  doParallel::registerDoParallel(cl)
  parallel::clusterExport(cl = cl, varlist = c('nsim', 'predict_fxn'), 
                          envir = environment())
  parallel::clusterEvalQ(cl = cl, expr = options(tidyverse.quiet = TRUE))
  
  if(class(model) == 'AsIs'){
    shap <- list()
    
    if(avg_pred){
      #compute SHAP values for the average over all models
      # shap will be a list with length(predict_fxn) elements
      # each element will have the shap values for that class
      
      #Get only the attributes that are used within this list of models
      attr_used <- vector('character', length = 0)
      for(i in 1:length(model)){
        attr_used <- unique(c(attr_used, model[[i]]$forest$independent.variable.names))
      }
      data <- data[, which(colnames(data) %in% attr_used)]
      
      if(length(predict_fxn) > 1){
        #Compute SHAP values for each class
        for(j in 1:length(predict_fxn)){
          shap_j <- fastshap::explain(object = model, 
                                      X = data, 
                                      pred_wrapper = predict_fxn[[j]], 
                                      nsim = nsim, 
                                      .parallel = TRUE, 
                                      .inform = TRUE,
                                      .paropts = list(.packages = c('tidyverse', 'tidymodels', 'ranger')))
          
          shap <- c(shap, list(shap_j))
        }
        names(shap) <- paste0('class', seq(1:length(shap)))
      }else{
        #only 1 class
        shap <- fastshap::explain(object = model,
                                  X = data, 
                                  pred_wrapper = predict_fxn, 
                                  nsim = nsim, 
                                  .parallel = TRUE, 
                                  .inform = TRUE,
                                  .paropts = list(.packages = c('tidyverse', 'tidymodels', 'ranger')))
      }
    }else{
      #compute SHAP values for each model
      # shap will be a list with length(model) elements (bootstraps)
      #  each element will have a list of matrices with length(predict_fxn)
      #  averages and error values will be computed over the bootstraps
      for(i in 1:length(model)){
        #Get only the columns needed for the model
        data_i <- data[, which(colnames(data) %in% model[[i]]$forest$independent.variable.names)]
        
        if(length(predict_fxn) > 1){
          #Compute SHAP values for each class
          shap_i <- list()
          for(j in 1:length(predict_fxn)){
            shap_j <- fastshap::explain(object = model[[i]], 
                                        X = data_i, 
                                        pred_wrapper = predict_fxn[[j]], 
                                        nsim = nsim, 
                                        .parallel = TRUE, 
                                        .inform = TRUE,
                                        .paropts = list(.packages = c('tidyverse', 'tidymodels', 'ranger')))
            
            shap_i <- c(shap_i, list(shap_j))
          }
          names(shap_i) <- paste0('class', seq(1:length(shap_i)))
        }else{
          shap_i <- fastshap::explain(object = model[[i]], 
                                      X = data_i, 
                                      pred_wrapper = predict_fxn, 
                                      nsim = nsim, 
                                      .parallel = TRUE, 
                                      .inform = TRUE,
                                      .paropts = list(.packages = c('tidyverse', 'tidymodels', 'ranger')))
        }
        
        shap <- c(shap, list(shap_i))
      }
      names(shap) <- paste0('boot', seq(1:length(shap)))
      
      #Compute the average and 90% SHAP values over all bootstraps
      # need to consider that variables do not need to appear in all models
      # set SHAP to 0 for models without those variables
    }
  }else{
    #only 1 model for which to compute SHAP values
    
    if(length(predict_fxn) > 1){
      #Compute SHAP values for each class
      shap <- list()
      for(j in 1:length(predict_fxn)){
        shap_j <- fastshap::explain(object = model, 
                                    X = data, 
                                    pred_wrapper = predict_fxn[[j]], 
                                    nsim = nsim, 
                                    .parallel = TRUE, 
                                    .inform = TRUE,
                                    .paropts = list(.packages = c('tidyverse', 'tidymodels', 'ranger')))
        
        shap <- c(shap, list(shap_j))
      }
      names(shap) <- paste0('class', seq(1:length(shap)))
    }else{
      shap <- fastshap::explain(object = model, 
                                X = data, 
                                pred_wrapper = predict_fxn, 
                                nsim = nsim, 
                                .parallel = TRUE, 
                                .inform = TRUE,
                                .paropts = list(.packages = c('tidyverse', 'tidymodels', 'ranger')))
    }
  }
  
  parallel::stopCluster(cl)
  
  return(shap)
}

predict_shap_multiclass_1 <- function(object, newdata){
  #' @description provides class probability for class 1
  #' 
  #' @param object trained model(s) used to make predictions for each reach in newdata
  #' @param newdata dataframe of reaches (rows) and attributes (columns) for which
  #' predictions will be made. Must have a "ID" column.
  #' 
  #' @returns vector of predicted class 1 probabilities for each reach
  
  #loop over models within model to make predictions
  predictions <- array(data = NA, dim = c(length(object), nrow(newdata), ncol(object[[1]]$predictions)), 
  )
  for (i in 1:length(object)){
    #get the reach attrs used for this model
    attrs <- select(newdata, all_of(names(object[[i]]$variable.importance)))
    preds <- predict(object[[i]], attrs)$predictions
    
    predictions[i,,] <- preds
  }
  
  #compute the average predictions for each reach over all models
  avg_predications <- apply(X = predictions, MARGIN = c(2,3), FUN = mean)
  colnames(avg_predications) <- seq(1,ncol(avg_predications),1)
  avg_predications <- as.data.frame(avg_predications)
  
  return(avg_predications[,1])
}
predict_shap_multiclass_2 <- function(object, newdata){
  #' @description provides class probability for class 2
  #' 
  #' @param object trained model(s) used to make predictions for each reach in newdata
  #' @param newdata dataframe of reaches (rows) and attributes (columns) for which
  #' predictions will be made. Must have a "ID" column.
  #' 
  #' @returns vector of predicted class 1 probabilities for each reach
  
  #loop over models within model to make predictions
  predictions <- array(data = NA, dim = c(length(object), nrow(newdata), ncol(object[[1]]$predictions)), 
  )
  for (i in 1:length(object)){
    #get the reach attrs used for this model
    attrs <- select(newdata, all_of(names(object[[i]]$variable.importance)))
    preds <- predict(object[[i]], attrs)$predictions
    
    predictions[i,,] <- preds
  }
  
  #compute the average predictions for each reach over all models
  avg_predications <- apply(X = predictions, MARGIN = c(2,3), FUN = mean)
  colnames(avg_predications) <- seq(1,ncol(avg_predications),1)
  avg_predications <- as.data.frame(avg_predications)
  
  return(avg_predications[,2])
}
predict_shap_multiclass_3 <- function(object, newdata){
  #' @description provides class probability for class 3
  #' 
  #' @param object trained model(s) used to make predictions for each reach in newdata
  #' @param newdata dataframe of reaches (rows) and attributes (columns) for which
  #' predictions will be made. Must have a "ID" column.
  #' 
  #' @returns vector of predicted class 1 probabilities for each reach
  
  #loop over models within model to make predictions
  predictions <- array(data = NA, dim = c(length(object), nrow(newdata), ncol(object[[1]]$predictions)), 
  )
  for (i in 1:length(object)){
    #get the reach attrs used for this model
    attrs <- select(newdata, all_of(names(object[[i]]$variable.importance)))
    preds <- predict(object[[i]], attrs)$predictions
    
    predictions[i,,] <- preds
  }
  
  #compute the average predictions for each reach over all models
  avg_predications <- apply(X = predictions, MARGIN = c(2,3), FUN = mean)
  colnames(avg_predications) <- seq(1,ncol(avg_predications),1)
  avg_predications <- as.data.frame(avg_predications)
  
  return(avg_predications[,3])
}
predict_shap_multiclass_4 <- function(object, newdata){
  #' @description provides class probability for class 4
  #' 
  #' @param object trained model(s) used to make predictions for each reach in newdata
  #' @param newdata dataframe of reaches (rows) and attributes (columns) for which
  #' predictions will be made. Must have a "ID" column.
  #' 
  #' @returns vector of predicted class 1 probabilities for each reach
  
  #loop over models within model to make predictions
  predictions <- array(data = NA, dim = c(length(object), nrow(newdata), ncol(object[[1]]$predictions)), 
  )
  for (i in 1:length(object)){
    #get the reach attrs used for this model
    attrs <- select(newdata, all_of(names(object[[i]]$variable.importance)))
    preds <- predict(object[[i]], attrs)$predictions
    
    predictions[i,,] <- preds
  }
  
  #compute the average predictions for each reach over all models
  avg_predications <- apply(X = predictions, MARGIN = c(2,3), FUN = mean)
  colnames(avg_predications) <- seq(1,ncol(avg_predications),1)
  avg_predications <- as.data.frame(avg_predications)
  
  return(avg_predications[,4])
}
predict_shap_multiclass_5 <- function(object, newdata){
  #' @description provides class probability for class 5
  #' 
  #' @param object trained model(s) used to make predictions for each reach in newdata
  #' @param newdata dataframe of reaches (rows) and attributes (columns) for which
  #' predictions will be made. Must have a "ID" column.
  #' 
  #' @returns vector of predicted class 1 probabilities for each reach
  
  #loop over models within model to make predictions
  predictions <- array(data = NA, dim = c(length(object), nrow(newdata), ncol(object[[1]]$predictions)), 
  )
  for (i in 1:length(object)){
    #get the reach attrs used for this model
    attrs <- select(newdata, all_of(names(object[[i]]$variable.importance)))
    preds <- predict(object[[i]], attrs)$predictions
    
    predictions[i,,] <- preds
  }
  
  #compute the average predictions for each reach over all models
  avg_predications <- apply(X = predictions, MARGIN = c(2,3), FUN = mean)
  colnames(avg_predications) <- seq(1,ncol(avg_predications),1)
  avg_predications <- as.data.frame(avg_predications)
  
  return(avg_predications[,5])
}


predict_shap_ranger_1 <- function(object, newdata){
  #' 
  #' @description uses the provided ranger model to predict on the test dataset.
  #'
  #' @param object model workflow containing a single model that will be used
  #' to predict on the test_data.
  #' @param newdata dataset containing features and the metric to be predicted
  #' 
  #' @return Returns a vector of predictions
  
  preds <- predict(object, newdata)$predictions[,1]
  
  return(preds)
}
predict_shap_ranger_2 <- function(object, newdata){
  #' 
  #' @description uses the provided ranger model to predict on the test dataset.
  #'
  #' @param object model workflow containing a single model that will be used
  #' to predict on the test_data.
  #' @param newdata dataset containing features and the metric to be predicted
  #' 
  #' @return Returns a vector of predictions
  
  preds <- predict(object, newdata)$predictions[,2]
  
  return(preds)
}
predict_shap_ranger_3 <- function(object, newdata){
  #' 
  #' @description uses the provided ranger model to predict on the test dataset.
  #'
  #' @param object model workflow containing a single model that will be used
  #' to predict on the test_data.
  #' @param newdata dataset containing features and the metric to be predicted
  #' 
  #' @return Returns a vector of predictions
  
  preds <- predict(object, newdata)$predictions[,3]
  
  return(preds)
}
predict_shap_ranger_4 <- function(object, newdata){
  #' 
  #' @description uses the provided ranger model to predict on the test dataset.
  #'
  #' @param object model workflow containing a single model that will be used
  #' to predict on the test_data.
  #' @param newdata dataset containing features and the metric to be predicted
  #' 
  #' @return Returns a vector of predictions
  
  preds <- predict(object, newdata)$predictions[,4]
  
  return(preds)
}
predict_shap_ranger_5 <- function(object, newdata){
  #' 
  #' @description uses the provided ranger model to predict on the test dataset.
  #'
  #' @param object model workflow containing a single model that will be used
  #' to predict on the test_data.
  #' @param newdata dataset containing features and the metric to be predicted
  #' 
  #' @return Returns a vector of predictions
  
  preds <- predict(object, newdata)$predictions[,5]
  
  return(preds)
}


predict_shap_tm <- function(object, newdata){
  #' 
  #' @description uses the provided tidymodel (tm) to predict on the test dataset.
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


#PDP
compute_pdp <- function(model, data, ice = FALSE, ncores = 1, predict_fxn,
                        avg_pred = TRUE){
  #'
  #' @description Computes PDP or ICE values for each feature in data
  #'
  #' @param model the model to be used. Can be a list of models, in which case
  #' average values will be returned for the unique features across all of 
  #' the models (models do not need to have the same features)
  #' @param data the dataframe used to make model predictions within the workflow
  #' @param ice logical. if TRUE, computes ICE values.
  #' @param ncores number of cores to use for parallel calculations
  #' @param predict_fxn the prediction function to use, unquoted
  #' @param avg_pred logical indicating what the predict_fxn provides. When TRUE, it
  #' should provide an average over all models (bootstraps) and values
  #' correspond to that average. FALSE is currently not supported.
  #'
  #' @return Returns a list of length=ncol(data) with the pdp values for each feature
  
  cl = parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  parallel::clusterExport(cl = cl, varlist = 'predict_fxn',
                          envir = environment())
  parallel::clusterEvalQ(cl = cl, expr = options(tidyverse.quiet = TRUE))
  
  partial <- list()
  
  if(avg_pred){
    #compute values for the average over all models
    # partial will be a list with length(predict_fxn) elements
    # each element will have the PDP values for that class
    
    #Get only the attributes that are used within this list of models
    attr_used <- vector('character', length = 0)
    for(i in 1:length(model)){
      attr_used <- unique(c(attr_used, model[[i]]$forest$independent.variable.names))
    }
    data <- data[, which(colnames(data) %in% attr_used)]
    
    #Compute values for each feature
    for(j in 1:ncol(data)){
      partial_j <- pdp::partial(object = model,
                                pred.var = colnames(data)[j],
                                plot = FALSE,
                                ice = FALSE,
                                train = data,
                                type = 'classification',
                                prob = TRUE,
                                pred.fun = predict_fxn, 
                                grid.resolution = 25,
                                parallel = TRUE,
                                paropts = list(.packages = c('tidyverse', 'ranger')))
      
      partial <- c(partial, list(partial_j))
    }
    names(partial) <- colnames(data)
    
  }else{
    #compute values for each model
    # partial will be a list with length(model) elements (bootstraps)
    #  each element will have a list of matrices with length(predict_fxn)
    #  averages and error values will be computed over the bootstraps
    stop('function currently does not support returning values for each model.
         use avg_pred = TRUE instead.')
  }
  
  return(partial)
}

predict_pdp_multiclass <- function(object, newdata){
  #' @description provides mean class probability for all classes
  #' 
  #' @param object trained model(s) used to make predictions for each reach in newdata
  #' @param newdata dataframe of reaches (rows) and attributes (columns) for which
  #' predictions will be made. Must have a "ID" column.
  #' 
  #' @returns vector of predicted mean class probabilities
  
  #loop over models within model to make predictions
  predictions <- array(data = NA, dim = c(length(object), nrow(newdata), ncol(object[[1]]$predictions)), 
  )
  for (i in 1:length(object)){
    #get the reach attrs used for this model
    attrs <- select(newdata, all_of(names(object[[i]]$variable.importance)))
    preds <- predict(object[[i]], attrs)$predictions
    
    predictions[i,,] <- preds
  }
  
  #compute the average predictions for each reach over all models
  avg_predications <- apply(X = predictions, MARGIN = c(2,3), FUN = mean)
  colnames(avg_predications) <- seq(1,ncol(avg_predications),1)
  avg_predications <- as.data.frame(avg_predications)
  
  return(colMeans(avg_predications))
}
