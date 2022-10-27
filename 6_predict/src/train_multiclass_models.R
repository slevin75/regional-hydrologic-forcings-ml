train_multiclass <- function(InputData, y_columns, GAGEID_column, x_columns, 
                             omit_columns = NULL, Val_Pct, bootstraps, num_features_retain, 
                             ranger_threads = NULL, ranger_mtry, ranger_ntree,
                             file_prefix, save_txt_files = FALSE, probability = TRUE)
  {
  #' @description creates multiclass prediction models for provided y_columns
  #' 
  #' @param InputData dataframe containing columns for the response variables (y_columns),
  #' gage IDs (GAGEID_column), reach COMIDs (COMID_column), and features (x_columns)
  #' @param y_columns numeric column indices for the response variable columns.
  #' These variables must have numeric elements that are the labeled classes.
  #' @param GAGEID_column numeric column index for the gage ID column
  #' @param x_columns numeric column indices for the feature columns
  #' @param omit_columns numeric column indices for columns to omit from InputData.
  #' @param Val_Pct percent of data to use for validation. Note that this may be slightly 
  #' adjusted to balance class representation in the training and testing datasets
  #' @param bootstraps number of bootstrap replicates to use
  #' @param num_features_retain number of features to retain in final model
  #' @param ranger_threads number of threads to use to parallelize ranger. Defaults
  #' to the max of 1 and (number of cores - 4)
  #' @param ranger_mtry numeric vector to use in a grid search for mtry
  #' @param ranger_ntree numeric vector to use in a grid search for ntree
  #' @param file_prefix character prefix to start all filenames
  #' @param save_txt_files logical. If TRUE, then text files are saved with the gages
  #' that are in training and testing (these can also be found in the returned data,
  #' so it's not necessary to have these).
  #' @param probability logical. If TRUE, train a probability forest instead of a
  #' classification forest.
  #' 
  #' @example multiclass_prediction(InputData = "REF_LIST_Class_HF_CONUS_JSmithClusters.txt", 
  #' y_columns = 3:8, GAGEID_column = 2, x_columns = 12:612, 
  #' omit_columns = c(12:46, 59:73, 75, 76, 79, 81:134, 158:270, 302), Val_Pct = 0.1, 
  #' bootstraps = 2, num_features_retain = 20, ranger_threads = 12, ranger_mtry = seq(5,20,5), 
  #' ranger_ntree = seq(100, 1500, 200), file_prefix = 'Run1')
  #' 
  #' @return weighted macro F1 statistic
  
  #set threads to use if not specified
  if (is.null(ranger_threads)){
    ranger_threads <- max(parallel::detectCores() - 4, 1)
  }
  
  #Target variables for each model
  InputData_y <- InputData[1:nrow(InputData), y_columns]
  
  #Empty matrices to hold results of all classification models (HMs)(=abc) and bootstraps (=def)
  #optimal hyperparameters
  RF_opt_ntree <- matrix(0, ncol(InputData_y), bootstraps)
  RF_opt_mtry <- matrix(0, ncol(InputData_y), bootstraps)
  #F1 statistic
  RF_Wt_Macro_FStat <- matrix(0, ncol(InputData_y), bootstraps)
  #Confusion matrix
  RF_confusion <- data.frame(HM = integer(), boot = integer(), confusion = I(list()))
  #feature importance
  RF_importance <- data.frame(HM = integer(), boot = integer(), importance = I(list()))
  #Trained RF models
  RF_trained_models <- data.frame(HM = integer(), boot = integer(), model = I(list()))
  if(probability){
    #Brier score
    RF_Brier <- matrix(0, ncol(InputData_y), bootstraps)
  }
  
  #data frames for the grid of hyperparameters to try
  RF_Models_ntree <- ranger_ntree %>%
    as.data.frame()
  RF_Models_mtry <- ranger_mtry %>%
    as.data.frame()
  
  #Bootstraps
  for(def in 1:bootstraps){
    message(paste("bootstrap", def))
    #Classification models
    for(abc in 1:ncol(InputData_y)){
      message(paste("classification model column", abc))
      
      #Training and Testing set up
      #Number of classes
      num_classes <- length(unique(InputData_y[,abc]))
      #Loop over classes and randomly select test data by proportion of class data
      for (class in 1:num_classes){
        #Only the InputData with this class
        tmp_InputData <- InputData[InputData_y[,abc] == class,]
        #Random sample for test data
        sample_inds <- sample(nrow(tmp_InputData), ceiling(Val_Pct*nrow(tmp_InputData)), replace = FALSE)
        
        if(class == 1){
          TEST_SET <- tmp_InputData[sample_inds, ]
          TRAIN_SET <- tmp_InputData[-sample_inds, ]
        }else{
          TEST_SET <- rbind(TEST_SET, tmp_InputData[sample_inds, ])
          TRAIN_SET <- rbind(TRAIN_SET, tmp_InputData[-sample_inds, ])
        }
      }
      rm(tmp_InputData, class, sample_inds)
      
      Y <- TRAIN_SET[, y_columns] %>%
        as.data.frame()
      X <- TRAIN_SET[, x_columns] 
      ID <- TRAIN_SET[, GAGEID_column]
      #save column names for data name labels
      HM_names <- colnames(TRAIN_SET)[y_columns]
      
      Y_TEST <- TEST_SET[, y_columns] %>%
        as.data.frame()
      X_TEST <- TEST_SET[, x_columns]
      ID_TEST <- TEST_SET[, GAGEID_column]
      
      #select only the outcome data for this classification model
      Y <- Y[, abc]
      Y_TEST <- Y_TEST[, abc]
      
      #Saving the gage IDs that went into each test and train set for each bootstrap
      GAGEID_TRAIN <- ID
      GAGEID_TEST <- ID_TEST
      
      if(save_txt_files){
        #Setting up the filenames for the output files
        def_chr <- get_filename(def)
        filenameTRAIN <- paste0(file_prefix, "ListGagesTrain", def_chr, "HM", abc, ".txt")
        filenameTEST <- paste0(file_prefix, "ListGagesTest", def_chr, "HM", abc, ".txt")
        
        #Saving the list of gage ids to separate output text files
        write_delim(x = as.data.frame(GAGEID_TRAIN), file = filenameTRAIN, delim = ',', append = FALSE)
        write_delim(x = as.data.frame(GAGEID_TEST), file = filenameTEST, delim = ',', append = FALSE)
      }
      
      if(!is.null(omit_columns)){
        #Remove all desired omit_columns from feature matrix
        X_ML <- X[, -omit_columns]
        X_ML_TEST <- X_TEST[, -omit_columns]
        
        X_ML <- as.data.frame(X_ML)
        X_ML_TEST <- as.data.frame(X_ML_TEST)
      }else{
        X_ML <- as.data.frame(X)
        X_ML_TEST <- as.data.frame(X_TEST)
      }
      
      ###################################################################
      #MODEL TRAINING/VALIDATION
      ###################################################################
      #Random Forest-RF
      ###################################################################
      #Running Initial RF model to identify top features based on importance
      #uses default mtry value
      RF_init <- ranger::ranger(x = X_ML, y = Y, num.trees = 1000, importance = 'impurity', 
                                classification = TRUE, num.threads = ranger_threads, 
                                probability = probability)
      
      #importance
      imp_init <- as.data.frame(RF_init$variable.importance)
      colnames(imp_init) <- "IMP"
      
      if(save_txt_files){
        #Saving importance values
        filenameRFimp <- paste(file_prefix, "RF_Init_Imp_boot", def, "_HM", abc, ".txt", sep = "")
        write_delim(x = imp_init, file = filenameRFimp, delim = ',', append = FALSE)
      }
      
      #Training data with only the top num_features_retain attributes
      #transpose
      X_ML_T <- t(X_ML)
      #add feature importance
      X_ML_T_IMP <- cbind(imp_init, X_ML_T)
      #order by feature importance
      X_ML_T_IMP_ORD <- as.data.frame(X_ML_T_IMP)
      X_ML_T_IMP_ORD <- X_ML_T_IMP_ORD[order(X_ML_T_IMP_ORD$IMP, decreasing = TRUE),]
      #New matrix that holds the top num_features_retain watershed attributes
      X_ML_T_top_attrs <- as.data.frame(t(X_ML_T_IMP_ORD[1:num_features_retain, 2:ncol(X_ML_T_IMP_ORD)]))
      X_ML_T_top_attrs_TEST <- X_ML_TEST[,colnames(X_ML_T_top_attrs)]
      
      #Random forest tuning
      #Determining optimal parameter values for RF (mtry and ntree)
      #For CLASSIFICATION minimize on weighted MACRO F1Stat 
      #(the false pos and false negs are EQUALLY bad and the classes are imbalanced)
      RF_Wt_Macro_F1_Pre <- matrix(0, nrow(RF_Models_ntree), nrow(RF_Models_mtry))
      if(probability){
        RF_Brier_Pre <- matrix(0, nrow(RF_Models_ntree), nrow(RF_Models_mtry))
      }
      
      for(aaa in 1:nrow(RF_Models_ntree)){
        for(bbb in 1:nrow(RF_Models_mtry)){
          ntree_tune <- RF_Models_ntree[aaa,1]
          mtry_tune <- RF_Models_mtry[bbb,1]
          RF_Pre <- ranger::ranger(x = X_ML_T_top_attrs, y = Y, num.trees = ntree_tune, mtry = mtry_tune, 
                                   importance = 'impurity', classification = TRUE, num.threads = ranger_threads,
                                   probability = probability)
          YPRED_RF_Pre <- predict(RF_Pre, X_ML_T_top_attrs_TEST)
          
          if(probability){
            #Compute the predicted Y based on the maximum probability for each observation
            max_prob <- apply(X = YPRED_RF_Pre$predictions, MARGIN = 1, FUN = which.max)
            
            #compute confusion matrix for validation dataset
            CONFUSION_Pre <- calc_confusion_matrix(obs = Y_TEST, pred = max_prob)
            
            #Calculation of the Multiclass Brier Score
            colnames(YPRED_RF_Pre$predictions) <- seq(1:ncol(YPRED_RF_Pre$predictions))
            RF_Brier_Pre[aaa,bbb] <- measures::multiclass.Brier(probabilities = YPRED_RF_Pre$predictions, 
                                                                truth = Y_TEST)
          }else{
            #compute confusion matrix for validation dataset
            CONFUSION_Pre <- calc_confusion_matrix(obs = Y_TEST, pred = YPRED_RF_Pre$predictions)
          }
          
          #Calculation of the Weighted Macro F1 Statistic
          RF_Wt_Macro_F1_Pre[aaa,bbb] <- calc_weighted_F1(confusion = CONFUSION_Pre, 
                                                         max_class = max(Y_TEST))
          
          rm(ntree_tune, mtry_tune, RF_Pre, YPRED_RF_Pre, CONFUSION_Pre)
        } #bbb loop
      } #aaa loop
      
      #ID which row and col contain the highest/lowest perf metric 
      # (for classification = highest, probability = lowest)
      RF_FStat_Loc <- which(RF_Wt_Macro_F1_Pre == max(RF_Wt_Macro_F1_Pre), arr.ind = TRUE)
      if(probability){
        RF_Brier_Loc <- which(RF_Brier_Pre == min(RF_Brier_Pre), arr.ind = TRUE)
        
        RF_ntree_Opt <- RF_Models_ntree[RF_Brier_Loc[1,1], 1]
        RF_mtry_Opt <- RF_Models_mtry[RF_Brier_Loc[1,2], 1]
      }else{
        RF_ntree_Opt <- RF_Models_ntree[RF_FStat_Loc[1,1], 1]
        RF_mtry_Opt <- RF_Models_mtry[RF_FStat_Loc[1,2], 1]
      }
      RF_opt_ntree[abc,def] <- RF_ntree_Opt
      RF_opt_mtry[abc,def] <- RF_mtry_Opt
      
      #Forming optimal model since program does not save each RF model in tuning to save RAM
      RF <- ranger::ranger(x = X_ML_T_top_attrs, y = Y, num.trees = RF_ntree_Opt, mtry = RF_mtry_Opt, 
                           importance = 'impurity', classification = TRUE, num.threads = ranger_threads,
                           probability = probability)
      
      #importance
      imp_RF <- as.data.frame(RF$variable.importance)
      
      #predictions
      YPRED_RF <- predict(RF, X_ML_T_top_attrs_TEST)
      
      #Performance metrics for final model
      if(probability){
        #Compute the predicted Y based on the maximum probability for each observation
        max_prob <- apply(X = YPRED_RF$predictions, MARGIN = 1, FUN = which.max)
        
        #compute confusion matrix for validation dataset
        CONFUSION <- calc_confusion_matrix(obs = Y_TEST, pred = max_prob)
        
        #Calculation of the Multiclass Brier Score
        colnames(YPRED_RF$predictions) <- seq(1:ncol(YPRED_RF$predictions))
        RF_Brier[abc,def] <- measures::multiclass.Brier(probabilities = YPRED_RF$predictions, 
                                                        truth = Y_TEST)
      }else{
        #compute confusion matrix for validation dataset
        CONFUSION <- calc_confusion_matrix(obs = Y_TEST, pred = YPRED_RF$predictions)
      }
      
      #Calculation of the Weighted Macro F1 Statistic
      RF_Wt_Macro_FStat[abc,def] <- calc_weighted_F1(confusion = CONFUSION, 
                                                     max_class = max(Y_TEST))
      
      #Save confusion matrix and feature importance for best model
      RF_confusion <- rbind(RF_confusion, data.frame(HM = HM_names[abc], boot = def, confusion = I(list(CONFUSION))))
      RF_importance <- rbind(RF_importance, data.frame(HM = HM_names[abc], boot = def, importance = I(list(imp_RF))))
      
      #Save trained RF model so it can be used to predict on new data in other functions
      RF_trained_models <- rbind(RF_trained_models, data.frame(HM = HM_names[abc], boot = def, model = I(list(RF))))
      
      #observations and predictions for test dataset
      Y_Combined <- data.frame(as.data.frame(Y_TEST), 
                               as.data.frame(YPRED_RF$predictions))
      
      #Add column and row names to matrix
      colnames(CONFUSION) = paste0('Pred', seq(1,ncol(CONFUSION),1))
      rownames(CONFUSION) = paste0('Obs', seq(1,ncol(CONFUSION),1))
      
      #Saving the predicted and corresponding observed Y values from the test sets
      if(abc == 1){
        ALL_RES_RF <- list(cbind(GAGEID_TEST, Y_Combined))
      }else{
        ALL_RES_RF <- rlist::list.append(ALL_RES_RF, cbind(GAGEID_TEST,Y_Combined))
      }
      
      ###REMOVING certain data from the environment
      rm(TEST_SET, TRAIN_SET, X, ID, X_TEST, ID_TEST, Y, Y_TEST,
         GAGEID_TRAIN, GAGEID_TEST, X_ML, X_ML_TEST,
         RF_init, imp_init, X_ML_T, X_ML_T_IMP, X_ML_T_IMP_ORD, X_ML_T_top_attrs,
         X_ML_T_top_attrs_TEST, RF_Wt_Macro_F1_Pre,
         aaa, bbb, RF_FStat_Loc, RF_ntree_Opt, RF_mtry_Opt, RF, imp_RF, YPRED_RF,
         Y_Combined)
      if(probability){
        rm(RF_Brier_Pre)
      }
    } #abc loop
    
    #test gages and predictions to be saved later
    if(def == 1){
      ALL_RES_RF2 <- ALL_RES_RF
    }else{
      ALL_RES_RF2 <- rbind(ALL_RES_RF2, ALL_RES_RF)
    }
    
    ###REMOVING certain data from the environment
    rm(abc, ALL_RES_RF)
    
  } #def loop
  
  write.csv(RF_opt_ntree, file = paste0(file_prefix, "RF_opt_ntree_Class.csv"), row.names = FALSE)
  write.csv(RF_opt_mtry, file = paste0(file_prefix, "RF_opt_mtry_Class.csv"), row.names = FALSE)
  write.csv(RF_Wt_Macro_FStat, file = paste0(file_prefix, "RF_Wt_Macro_FStat_Class.csv"), row.names = FALSE)
  if(probability){
    write.csv(RF_Brier, file = paste0(file_prefix, "RF_Brier.csv"), row.names = FALSE)
  }
  
  #Warn if any of the optimal parameters are located at the bounds
  if (any(RF_opt_mtry == max(ranger_mtry))){
    message('Some of the optimal mtry are at the maximum value. 
            Try increasing the maximum and increasing the num_features_retained.')
  }
  if (any(RF_opt_mtry == min(ranger_mtry))){
    message('Some of the optimal mtry are at the minimum value. 
            Try decreasing the minimum value.')
  }
  if (any(RF_opt_ntree == max(ranger_ntree))){
    message('Some of the optimal ntree are at the maximum value. 
            Try increasing the maximum.')
  }
  if (any(RF_opt_ntree == min(ranger_ntree))){
    message('Some of the optimal ntree are at the minimum value. 
            Try decreasing the minimum value.')
  }
  
  #Loop over all HMs to write a combined file for each HM (contains all bootstraps)
  for (abc in 1:ncol(InputData_y)){
    if (bootstraps == 1){
      write.csv(ALL_RES_RF2[[abc]], 
                file = paste0(file_prefix, "ALL_RES_RF_Class_HM", abc,".csv"), row.names = FALSE)
    }else{
      write.csv(rlist::list.stack(ALL_RES_RF2[,abc]), 
                file = paste0(file_prefix, "ALL_RES_RF_Class_HM", abc,".csv"), row.names = FALSE)
    }
  }
  
  #Return list of info
  if(probability){
    return_list = list(Brier = RF_Brier, F1 = RF_Wt_Macro_FStat, confusion = RF_confusion, 
                       importance = RF_importance, opt_ntree = RF_opt_ntree, opt_mtry = RF_opt_mtry,
                       RF_models = RF_trained_models)
  }else{
    return_list = list(F1 = RF_Wt_Macro_FStat, confusion = RF_confusion, importance = RF_importance,
                       opt_ntree = RF_opt_ntree, opt_mtry = RF_opt_mtry, RF_models = RF_trained_models)
  }

  return(return_list)
}

calc_confusion_matrix <- function(obs, pred){
  #' @description Calculation of the confusion matrix
  #' 
  #' @param obs vector of observations
  #' @param pred vector of predictions
  #' 
  #' @returns confusion matrix with observed classes = rows & Predicted classes = cols
  
  pred <- as.data.frame(pred)
  obs <- as.data.frame(obs)
  
  #Constructing confusion matrix for validation data
  obs_pred <- cbind(obs, pred)
  #Calculate unstructured confusion matrix 
  #(TRUE POSITIVES, TRUE NEGATIVES, FALSE POSITIVES, FALSE NEGATIVES)
  confusion_pre <- as.data.frame(table(obs_pred)) 
  #Assigning the TP, TN, FP, FN from confusion_pre into the correct structured matrix
  confusion <- matrix(0, max(obs), max(obs))
  for(ccc in 1:nrow(confusion_pre)){
    #Observed classes = rows & Predicted classes = cols
    confusion[confusion_pre$obs[ccc], confusion_pre$pred[ccc]] = confusion_pre$Freq[ccc]
  }
  
  return(confusion)
}

calc_weighted_F1 <- function(confusion, max_class){
  #' @description Calculation of the Weighted Macro F1 Statistic
  #' 
  #' @param confusion the confusion matrix from calc_confusion_matrix()
  #' @param max_class the maximum class number (should equal total number of classes)
  #' 
  #' @returns weighted F1 statistic
  
  Precision_Pre <- vector('numeric', length = max_class)
  Recall_Pre <- vector('numeric', length = max_class)
  F1_Pre <- vector('numeric', length = max_class)
  Horiz_Sums_Pre <- vector('numeric', length = max_class)
  
  for(ddd in 1:nrow(confusion)){
    #uses False Positives, FP
    #FP=vertical sums (minus main diagonal)
    Precision_Pre[ddd] <- confusion[ddd,ddd]/(confusion[ddd,ddd] + (sum(confusion[,ddd]) - confusion[ddd,ddd]))
    #uses False Negatives, FN
    #FN=horizontal sums (minus main diagonal)
    Recall_Pre[ddd] <- confusion[ddd,ddd]/(confusion[ddd,ddd] + (sum(confusion[ddd,]) - confusion[ddd,ddd]))
    
    #compute F1
    if(Precision_Pre[ddd] == 0 || Recall_Pre[ddd] == 0){
      F1_Pre[ddd] <- 0
    }else{
      F1_Pre[ddd] <- 2*((Precision_Pre[ddd] * Recall_Pre[ddd])/(Precision_Pre[ddd] + Recall_Pre[ddd]))
    }
    
    #Each class' contribution to the weighted F1 statistic
    #Horizontal sums across the confusion matrix (Observed classes=rows)
    Horiz_Sums_Pre[ddd] <- F1_Pre[ddd] * (sum(confusion[ddd,])/sum(confusion))
  }
  
  #weighted F1
  Wt_F1 <- sum(Horiz_Sums_Pre)
  
  return(Wt_F1)
}


get_filename <- function(index){
  #' @description returns the filename to use based on the bootstrap index value
  #' 
  #' @param index the numeric bootstrap index
  #' 
  #' @returns character string with the bootstrap index precceded by 0s (if needed)
  
  if(index < 10){
    index_chr <- paste0('000', index)
  } else {
    if(index < 100){
      index_chr <- paste0('00', index)
    } else {
      if(index < 1000){
        index_chr <- paste0('0', index)
      } else{
        index_chr <- index
      }
    }
  }
  
  return(index_chr)
}


predict_multiclass <- function(model, reach_attrs){
  #' @description returns the filename to use based on the bootstrap index value
  #' 
  #' @param model trained model(s) used to make predictions for each reach in reach_attrs
  #' @param reach_attrs dataframe of reaches (rows) and attributes (columns) for which
  #' predictions will be made. Must have a "ID" column.
  #' 
  #' @returns matrix of predicted class (columns) probabilities (cells) for each reach (rows)
  
  #loop over models within model to make predictions
  predictions <- array(data = NA, dim = c(length(model), nrow(reach_attrs), ncol(model[[1]]$predictions)), 
  )
  for (i in 1:length(model)){
    #get the reach attrs used for this model
    attrs <- select(reach_attrs, all_of(names(model[[i]]$variable.importance)))
    preds <- predict(model[[i]], attrs)$predictions
    
    predictions[i,,] <- preds
  }
  
  #compute the average predictions for each reach over all models
  avg_predications <- apply(X = predictions, MARGIN = c(2,3), FUN = mean)
  colnames(avg_predications) <- seq(1,ncol(avg_predications),1)
  avg_predications <- as.data.frame(avg_predications)
  #Add identifier
  avg_predications$ID <- reach_attrs$ID
  
  return(avg_predications)
}
