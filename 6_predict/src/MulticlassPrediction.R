multiclass_prediction <- function(InputData, y_columns, GAGEID_column, COMID_column, x_columns, 
                                  omit_columns, Val_Pct, bootstraps, num_features_retain, 
                                  ranger_threads = NULL, ranger_mtry, ranger_ntree)
  {
  #' @description creates multiclass prediction models for provided y_columns
  #' 
  #' @param InputData dataframe containing columns for the response variables (y_columns),
  #' gage IDs (GAGEID_column), reach COMIDs (COMID_column), and features (x_columns)
  #' @param y_columns numeric column indices for the response variable columns.
  #' These variables must have numeric elements that are the labeled classes.
  #' @param GAGEID_column numeric column index for the gage ID column
  #' @param COMID_column numeric column index for the reach ID column
  #' @param x_columns numeric column indices for the feature columns
  #' @param omit_columns numeric column indices for features to omit
  #' @param Val_Pct percent of data to use for validation. Note that this may be slightly 
  #' adjusted to balance class representation in the training and testing datasets
  #' @param bootstraps number of bootstrap replicates to use
  #' @param num_features_retain number of features to retain in final model
  #' @param ranger_threads number of threads to use to parallelize ranger. Defaults
  #' to the max of 1 and (number of cores - 4)
  #' @param ranger_mtry numeric vector to use in a grid search for mtry
  #' @param ranger_ntree numeric vector to use in a grid search for ntree
  #' 
  #' @example multiclass_prediction(InputData = "REF_LIST_Class_HF_CONUS_JSmithClusters.txt", 
  #' y_columns = 3:8, GAGEID_column = 2, COMID_column = 11, x_columns = 12:612, 
  #' omit_columns = c(12:46, 59:73, 75, 76, 79, 81:134, 158:270, 302), Val_Pct = 0.1, 
  #' bootstraps = 2, num_features_retain = 20, ranger_threads = 12, ranger_mtry = seq(5,20,5), 
  #' ranger_ntree = seq(100, 1500, 200))
  #' 
  #' @return ...
  
  if (is.null(ranger_threads)){
    ranger_threads <- max(parallel::detectCores() - 4, 1)
  }
  
  #All data: Classes contained within cols 3 to 8 and features are in cols 12 to 612 (filename:XXXXX.xlsx)
  InputData <- read.delim(file = InputData, header = FALSE, sep = "\t")
  
  InputData_y <- InputData[1:nrow(InputData), y_columns]
  
  #Empty matrices to hold results of all HMs(=abc) and bootstraps (=def)
  #optimal hyperparameters
  RF_para_ntree <- matrix(0, ncol(InputData_y), bootstraps)
  RF_para_mtry <- matrix(0, ncol(InputData_y), bootstraps)
  #F1 statistic
  RF_Wt_Macro_FStat <- matrix(0, ncol(InputData_y), bootstraps)
  
  for(def in 1:bootstraps){
    print(paste("bootstrap", def))
    #For every class model
    for(abc in 1:ncol(InputData_y)){
      print(paste("HM", abc))
      
      #Validation Data set up
      #Number of classes
      num_classes <- length(unique(InputData_y[,abc]))
      #Loop over classes and randomly select test data
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
      
      YRED <- TRAIN_SET[, y_columns]
      YRED <- as.data.frame(YRED)
      XRED2 <- TRAIN_SET[, x_columns] 
      IDRED2 <- TRAIN_SET[, GAGEID_column]  
      COMIDRED2 <- TRAIN_SET[, COMID_column] 
      
      YRED_TEST <- TEST_SET[, y_columns]
      YRED_TEST<-as.data.frame(YRED_TEST)
      XRED2_TEST <- TEST_SET[, x_columns]
      IDRED2_TEST <- TEST_SET[, GAGEID_column]
      COMIDRED2_TEST <- TEST_SET[, COMID_column]
      
      YRED2 <- YRED[, abc]
      YRED2_TEST <- YRED_TEST[, abc]
      
      #Saving the gage IDs that went into each test and train set for each bootstrap
      GAGEID_TRAIN <- IDRED2
      GAGEID_TEST <- IDRED2_TEST
      
      #Setting up the filenames for the output files
      if(def < 10){
        def_chr <- paste0('000', def)
      } else {
        if(def < 100){
          def_chr <- paste0('00', def)
        } else {
          if(def < 1000){
            def_chr <- paste0('0', def)
          } else{
            def_chr <- def
          }
        }
      }
      filenameTRAIN <- paste0("ListGagesTrain", def_chr, "HM", abc, ".txt")
      filenameTEST <- paste0("ListGagesTest", def_chr, "HM", abc, ".txt")
      
      #Saving the list of gage ids to separate output text files
      out_TRAIN <- capture.output(GAGEID_TRAIN)
      out_TEST <- capture.output(GAGEID_TEST)
      cat("Train Gage List ", out_TRAIN, file = filenameTRAIN, sep = ",", append = FALSE)
      cat("Test Gage List ", out_TEST, file = filenameTEST, sep = ",", append = FALSE)
      
      #Remove all desired omit_columns from feature matrix
      XRED2_ML <- XRED2[, -omit_columns]
      XRED2_ML_TEST <- XRED2_TEST[, -omit_columns]
      
      XRED2_ML <- as.data.frame(XRED2_ML)
      XRED2_ML_TEST <- as.data.frame(XRED2_ML_TEST)
      colnames(XRED2_ML)[colnames(XRED2_ML) == "XRED2_ML"] <- "V1" #changing name of the 1st col
      colnames(XRED2_ML_TEST)[colnames(XRED2_ML_TEST) == "XRED2_ML_TEST"] <- "V1"
      
      ###################################################################
      #MODEL TRAINING/VALIDATION
      ###################################################################
      #Random Forest-RF
      ###################################################################
      #Running Initial RF model to identify top features based on importance
      RF_init <- ranger::ranger(x = XRED2_ML, y = YRED2, num.trees = 1000, importance = 'impurity', 
                                classification = TRUE, num.threads = ranger_threads) #uses default mtry value
      RF_init
      #RF_init$confusion.matrix
      imp_init <- as.data.frame(RF_init$variable.importance)
      colnames(imp_init)[colnames(imp_init) == "RF_init$variable.importance"] <- "IMP" #changing name of the 1st col
      
      #Saving random forest importance values
      out_imp <- capture.output(imp_init)
      filenameRFimp <- paste("RF_Init_Imp", def, "HM", abc, ".txt", sep = "")
      cat("RF_Init_Imp", out_imp, file = filenameRFimp, sep = ",", append = FALSE)
      
      #Training data
      XRED3 <- t(XRED2_ML) #transposes XRED2_ML
      XRED3_IMP <- cbind(imp_init, XRED3)
      XRED3_IMP_ORD <- as.data.frame(XRED3_IMP)
      XRED3_IMP_ORD <- XRED3_IMP_ORD[order(XRED3_IMP_ORD$IMP, decreasing = TRUE),]
      #New matrix that holds the top num_features_retain watershed attributes
      XRED3_20 <- as.data.frame(t(XRED3_IMP_ORD[1:num_features_retain, 2:ncol(XRED3_IMP_ORD)]))
      XRED3_20_TEST <- XRED2_ML_TEST[,colnames(XRED3_20)]
      
      #Random forest tuning
      #Determining optimal parameter values for RF (mtry and ntree)
      RF_Models_ntree <- ranger_ntree
      RF_Models_ntree <- as.data.frame(RF_Models_ntree)
      RF_Models_mtry <- ranger_mtry
      RF_Models_mtry <- as.data.frame(RF_Models_mtry)
      #For CLASSIFICATION minimize on weighted MACRO F1Stat (bc the false pos and false negs are EQUALLY bad and the classes are imbalanced)
      RF_Wt_Macro_F_Pre <- matrix(0, nrow(RF_Models_ntree), nrow(RF_Models_mtry))
      
      for(aaa in 1:nrow(RF_Models_ntree)){
        for(bbb in 1:nrow(RF_Models_mtry)){
          ntree_tune <- RF_Models_ntree[aaa,1]
          mtry_tune <- RF_Models_mtry[bbb,1]
          RF_Pre <- ranger::ranger(x = XRED3_20, y = YRED2, num.trees = ntree_tune, mtry = mtry_tune, 
                                   importance = 'impurity', classification = TRUE, num.threads = ranger_threads)
          YPRED_RF_Pre <- predict(RF_Pre, XRED3_20_TEST)
          YPRED_RF_Pre <- as.data.frame(YPRED_RF_Pre$predictions)
          YRED2_TEST_Comp <- as.data.frame(YRED2_TEST)
          
          #Constructing confusion matrix for validation data
          YRED2_TEST_Comp2 <- cbind(YRED2_TEST_Comp, YPRED_RF_Pre)
          #Calculates confusion matrix for the Validation data (TRUE POSITIVES, TRUE NEGATIVES, FALSE POSITIVES, FALSE NEGATIVES) BUT UNSTRUCTURED!
          TABG_Pre <- as.data.frame(table(YRED2_TEST_Comp2[,])) 
          #Ken suggests function starts here
          #Assigning the TP, TN, FP, FN from TABG_Pre into the correct structure matrix called CONFUSION_Pre
          CONFUSION_Pre <- matrix(0, max(YRED2_TEST), max(YRED2_TEST)) #Observed classes=rows & Predicted classes=cols
          for(ccc in 1:nrow(TABG_Pre)){
            CONFUSION_Pre[TABG_Pre$YRED2_TEST[ccc], TABG_Pre$YPRED_RF_Pre.predictions[ccc]] = TABG_Pre$Freq[ccc]
          }
          
          #Calculation of the Weighted Macro F1 Statistic
          Precision_Pre <- matrix(0, max(YRED2_TEST), 1) #uses False Positives, FP
          Recall_Pre <- matrix(0, max(YRED2_TEST), 1) #uses False Negatives, FN
          F1_Pre <- matrix(0, max(YRED2_TEST), 1)
          Horiz_Sums_Pre <- matrix(0, max(YRED2_TEST), 1) #Horizontal sums across the confusion matrix (bc Observed classes=rows)
          #FP=vertical sums (minus main diagonal)
          #FN=horizontal sums (minus main diagonal)
          for(ddd in 1:nrow(CONFUSION_Pre)){
            Precision_Pre[ddd,1] <- CONFUSION_Pre[ddd,ddd]/(CONFUSION_Pre[ddd,ddd] + (sum(CONFUSION_Pre[,ddd]) - CONFUSION_Pre[ddd,ddd]))
            Recall_Pre[ddd,1] <- CONFUSION_Pre[ddd,ddd]/(CONFUSION_Pre[ddd,ddd] + (sum(CONFUSION_Pre[ddd,]) - CONFUSION_Pre[ddd,ddd]))
            if(Precision_Pre[ddd,1] == 0 || Recall_Pre[ddd,1] == 0){
              F1_Pre[ddd,1] <- 0
            }else{
              F1_Pre[ddd,1] <- 2*((Precision_Pre[ddd,1] * Recall_Pre[ddd,1])/(Precision_Pre[ddd,1] + Recall_Pre[ddd,1]))
            }
            #Each classes weighted contribution to the Wt Macro F1 Stat
            Horiz_Sums_Pre[ddd,1] <- F1_Pre[ddd,1] * (sum(CONFUSION_Pre[ddd,])/sum(CONFUSION_Pre))
          }
          RF_Wt_Macro_F_Pre[aaa,bbb] <- sum(Horiz_Sums_Pre)
          
          rm(ntree_tune)
          rm(mtry_tune)
          rm(RF_Pre)
          rm(YPRED_RF_Pre)
          rm(YRED2_TEST_Comp)
          
          rm(YRED2_TEST_Comp2)
          rm(TABG_Pre)
          rm(CONFUSION_Pre)
          rm(ccc)
          
          rm(Precision_Pre)
          rm(Recall_Pre)
          rm(F1_Pre)
          rm(Horiz_Sums_Pre)
          
        } #bbb loop
      } #aaa loop
      
      #ID which row and col contain the highest/lowest perf metric (ie for regression=mse so lowest or NSE is max, for classification=  so highest)
      RF_FStat_Loc <- which(RF_Wt_Macro_F_Pre == max(RF_Wt_Macro_F_Pre), arr.ind = TRUE)
      RF_ntree_Opt <- RF_Models_ntree[RF_FStat_Loc[1,1], 1]
      RF_mtry_Opt <- RF_Models_mtry[RF_FStat_Loc[1,2], 1]
      RF_para_ntree[abc,def] <- RF_ntree_Opt
      RF_para_mtry[abc,def] <- RF_mtry_Opt
      
      #Forming optimal model since program does not save each RF model in tuning to save RAM
      RF <- ranger::ranger(x = XRED3_20, y = YRED2, num.trees = RF_ntree_Opt, mtry = RF_mtry_Opt, 
                           importance = 'impurity', classification = TRUE, num.threads = ranger_threads)
      RF
      #RF$confusion.matrix
      imp_RF <- as.data.frame(RF$variable.importance)
      YPRED_RF <- predict(RF, XRED3_20_TEST)
      YPRED_RF <- as.data.frame(YPRED_RF$predictions)
      
      #Performance metric for final classification model
      #Do not need to recalculate Wt Macro F1 stat bc max value assoc. with optimal values of ntree and mtry
      RF_Wt_Macro_FStat[abc,def] <- max(RF_Wt_Macro_F_Pre)
      
      YRED2_TEST_Comp3 <- as.data.frame(YRED2_TEST)
      Y_Combined <- data.frame(YRED2_TEST_Comp3, YPRED_RF)
      
      #Saving the predicted and corresponding observed Y values from the test sets
      #Setting up the filenames for the output files
      filename_RF <- paste("RF_Class_PredYObsY", def_chr, "HM", abc, ".csv", sep = "")
      
      #Saving the Y values to separate output text files
      out_RF <- write.csv(cbind(GAGEID_TEST, Y_Combined), file = filename_RF)
      
      if(abc == 1){
        ALL_RES_RF <- list(cbind(GAGEID_TEST, Y_Combined))
      }else{
        ALL_RES_RF <- rlist::list.append(ALL_RES_RF, cbind(GAGEID_TEST,Y_Combined))
      }
      
      ###REMOVING certain data from the environment use the 'rm(X,Y,Z)' at the end of the loop to zero out some matrices
      rm(TEST_SET)
      rm(TRAIN_SET)
      rm(XRED2)
      rm(IDRED2)
      rm(COMIDRED2)
      rm(XRED2_TEST)
      rm(IDRED2_TEST)
      rm(COMIDRED2_TEST)
      rm(YRED2)
      rm(YRED2_TEST)
      rm(GAGEID_TRAIN)
      rm(GAGEID_TEST)
      rm(filenameTRAIN)
      rm(filenameTEST)
      rm(out_TRAIN)
      rm(out_TEST)
      
      rm(XRED2_ML)
      rm(XRED2_ML_TEST)
      
      rm(RF_init)
      rm(imp_init)
      rm(XRED3)
      rm(XRED3_IMP)
      rm(XRED3_IMP_ORD)
      rm(XRED3_20)
      rm(XRED3_20_TEST)
      
      rm(RF_Models_ntree)
      rm(RF_Models_mtry)
      rm(RF_Wt_Macro_F_Pre)
      rm(aaa)
      rm(bbb)
      
      rm(RF_FStat_Loc)
      rm(RF_ntree_Opt)
      rm(RF_mtry_Opt)
      
      rm(RF)
      rm(imp_RF)
      rm(YPRED_RF)
      rm(YRED2_TEST_Comp3)
      
      rm(Y_Combined)
      rm(filename_RF)
      rm(out_RF)
      
    } #abc loop
    
    if(def == 1){
      ALL_RES_RF2 <- ALL_RES_RF
    }else{
      ALL_RES_RF2 <- rbind(ALL_RES_RF2, ALL_RES_RF)
    }
    
    ###REMOVING certain data from the environment use the 'rm(X,Y,Z)' at the end of the loop to zero out some matrices
    rm(abc)
    
    rm(ALL_RES_RF)
    
  } #def loop
  
  write.csv(RF_para_ntree, file = "RF_para_ntree_Class.csv")
  write.csv(RF_para_mtry, file = "RF_para_mtry_Class.csv")
  write.csv(RF_Wt_Macro_FStat, file = "RF_Wt_Macro_FStat_Class.csv")
  
  #Warn if any of the optimal parameters are located at the bounds
  if (any(RF_para_mtry == max(ranger_mtry))){
    message('Some of the optimal mtry are at the maximum value. 
            Try increasing the maximum and increasing the num_features_retained.')
  }
  if (any(RF_para_mtry == min(ranger_mtry))){
    message('Some of the optimal mtry are at the minimum value. 
            Try decreasing the minimum value.')
  }
  if (any(RF_para_ntree == max(ranger_ntree))){
    message('Some of the optimal ntree are at the maximum value. 
            Try increasing the maximum.')
  }
  if (any(RF_para_ntree == min(ranger_ntree))){
    message('Some of the optimal ntree are at the minimum value. 
            Try decreasing the minimum value.')
  }
  
  #Loop over all HMs to write a combined file for each HM (contains all bootstraps)
  for (abc in 1:ncol(InputData_y)){
    write.csv(rlist::list.stack(ALL_RES_RF2[,abc]), file = paste0("ALL_RES_RF_Class_HM", abc,".csv"))
  }
}
