plot_Boruta <- function(brf_model, metric, region, out_dir){
  #' 
  #' @description Plots the Boruta feature importance plot
  #'
  #' @param brf_model output of Boruta()
  #' @param metric metric name
  #' @param region region name for file name
  #' @param out_dir output directory
  #'
  #' @return filepath to resulting plot
  
  fileout <- file.path(out_dir, paste0('Boruta_', metric, '_', region, '.png'))
  
  png(fileout, width = 8, height = 4, units = 'in', res = 200)
  #plot without outlier points
  plot(brf_model, outpch = NA, show.names = FALSE, main = metric)
  dev.off()
  
  return(fileout)
}

plot_hyperparam_opt_results_RF <- function(opt_result, metric, region, out_dir){
  #' 
  #' @description Plots hyperparameter optimization results for RF models
  #'
  #' @param opt_result output of RF model hyperparameter optimization
  #' @param metric metric name
  #' @param region region name for file name
  #' @param out_dir output directory
  #'
  #' @return filepath to resulting plot
  
  fileout <- file.path(out_dir, paste0('hyperparam_diagnostic_', 
                                       metric, '_', region, '.png'))
  
  p1 <- opt_result %>% 
    collect_metrics() %>%
    ggplot(aes(mtry, mean, color = min_n)) +
    geom_line(size = 1.5, alpha = 0.6) +
    geom_point(size = 2) +
    facet_wrap(~ .metric, scales = "free", nrow = 2) +
    scale_color_viridis_c(option = "plasma", begin = .9, end = 0) +
    ggtitle(metric, subtitle = region)
  
  ggsave(filename = fileout, plot = p1, device = 'png')
  
  return(fileout)  
}

plot_hyperparam_opt_marginals <- function(opt_result, metric, region, 
                                          plt_type = "marginals",
                                          perf_metric = NULL, out_dir){
  #' @description Plots hyperparameter optimization results
  #'
  #' @param opt_result output of RF model hyperparameter optimization
  #' @param metric metric name
  #' @param region region name for file name
  #' @param plt_type passed to tune::autoplot type parameter
  #' @param perf_metric performance metric passed to tune::autoplot metric parameter
  #' Leave as NULL to plot all computed metrics.
  #' @param out_dir output directory
  #'
  #' @return filepath to resulting plot
  
  fileout <- file.path(out_dir, paste0('hyperparam_marginals_', 
                                       metric, '_', region, '.png'))
  
  p1 <- tune::autoplot(object = opt_result, type = plt_type, 
                       metric = perf_metric) +
    ggtitle(metric, subtitle = region)
  
  ggsave(filename = fileout, plot = p1, device = 'png')
  
  return(fileout)
}

plot_vip <- function(RF_model, metric, region, num_features, out_dir){
  #' 
  #' @description Plots the variable importance plot from a RF model
  #'
  #' @param RF_model workflow containing the best trained RF model, fit to all training data 
  #' @param metric metric name
  #' @param region region name for file name
  #' @param num_features select the top num_features number of features to plot
  #' @param out_dir output directory
  #' 
  #' @return filepath to resulting plot
  
  fileout <- file.path(out_dir, paste0('vip_', metric, '_', region, '.png'))
  
  p1 <- vip(RF_model %>% extract_fit_parsnip(), 
            num_features = num_features, aesthetics = list(width = 0.6)) + 
    ggtitle(metric, subtitle = region) +
    theme(axis.title.x = element_text(size = 18),
          axis.text.y = element_text(size = 18))
  
  ggsave(filename = fileout, plot = p1, device = 'png')
  
  return(fileout)
}

barplot_compare_RF <- function(rain_mod, snow_mod, rain_snow_mod, CONUS_mod,
                               test_rain_rain, test_rain_snow,
                               test_snow_rain, test_snow_snow, 
                               test_rain_snow_rain, test_rain_snow_snow, 
                               test_CONUS_rain, test_CONUS_snow,
                               flow_metric, perf_metric, out_dir){
  #'
  #' @description makes barplots of RMSEs for each of the supplied models
  #'
  #' @param rain_mod,snow_mod,rain_snow_mod,CONUS_mod best fit model evaluated 
  #' on the test dataset for that model. 
  #' example: rain_mod = p6_train_RF_rain
  #' @param test_rain_rain,test_rain_snow,test_snow_rain,test_snow_snow,test_rain_snow_rain,test_rain_snow_snow,test_CONUS_rain,test_CONUS_snow 
  #' These are the best fit model for each region evaluated in another region
  #' format is test_model-name_region-name. 
  #' example: test_snow_rain = p6_test_RF_snow_rain$perf_metrics
  #' @param flow_metric flow metric name
  #' @param perf_metric performance metric name
  #' @param out_dir output directory
  #'
  #' @return filepath to 3 resulting plots: 1. Validation and Testing performance
  #' 2. test performance in full rain region, 3. test performance in full snow region

  #3 plots:
  #1 validation and testing within each region
  #2 test in full rain region
  #3 test in full snow region
  fileout <- c(file.path(out_dir, paste0('compare_models_RF_', flow_metric, '_', perf_metric, '_CV.png')),
               file.path(out_dir, paste0('compare_models_RF_', flow_metric, '_', perf_metric, '_rain.png')),
               file.path(out_dir, paste0('compare_models_RF_', flow_metric, '_', perf_metric, '_snow.png')))
  
  #CV performances dataframe
  plt_df <- data.frame(perf = c(show_best(rain_mod$grid_params, n = 1, metric = perf_metric)$mean,
                                get_perf_metric(rain_mod$best_fit$.metrics[[1]], perf_metric = perf_metric),
                                show_best(snow_mod$grid_params, n = 1, metric = perf_metric)$mean,
                                get_perf_metric(snow_mod$best_fit$.metrics[[1]], perf_metric = perf_metric),
                                show_best(rain_snow_mod$grid_params, n = 1, metric = perf_metric)$mean,
                                get_perf_metric(rain_snow_mod$best_fit$.metrics[[1]], perf_metric = perf_metric),
                                show_best(CONUS_mod$grid_params, n = 1, metric = perf_metric)$mean,
                                get_perf_metric(CONUS_mod$best_fit$.metrics[[1]], perf_metric = perf_metric)),
                       sd = c(show_best(rain_mod$grid_params, n = 1, metric = perf_metric)$std_err,
                              NA,
                              show_best(snow_mod$grid_params, n = 1, metric = perf_metric)$std_err,
                              NA,
                              show_best(rain_snow_mod$grid_params, n = 1, metric = perf_metric)$std_err,
                              NA,
                              show_best(CONUS_mod$grid_params, n = 1, metric = perf_metric)$std_err,
                              NA),
                       Dataset = c('Val', 'Test', 
                                      'Val', 'Test',
                                      'Val', 'Test', 
                                      'Val', 'Test'),
                       grp = c("Rain","Rain","Snow","Snow",
                               "Rain+Snow","Rain+Snow","CONUS","CONUS"))
  
  p1 <- ggplot(data = plt_df, aes(x = grp, y = perf, fill = Dataset)) +
    geom_bar(stat="identity", position=position_dodge(), width = 0.6) +
    theme_bw() +
    scale_fill_brewer(palette="Paired") +
    geom_errorbar(aes(ymin = perf - 2*sd, ymax = perf + 2*sd), width = .2,
                  position = position_dodge(0.6)) +
    xlab('') +
    ylab('RMSE') + 
    scale_x_discrete(limits=c("Rain","Snow","Rain+Snow","CONUS")) +
    theme(axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 14)) +
    ggtitle(flow_metric)
  
  ggsave(filename = fileout[1], plot = p1, device = 'png')
  
  #Rain region test performance
  plt_df <- data.frame(perf = c(get_perf_metric(test_rain_rain$metrics, perf_metric = perf_metric),
                                get_perf_metric(test_snow_rain$metrics, perf_metric = perf_metric),
                                get_perf_metric(test_rain_snow_rain$metrics, perf_metric = perf_metric),
                                get_perf_metric(test_CONUS_rain$metrics, perf_metric = perf_metric)),
                       grp = c("Rain","Snow","Rain+Snow","CONUS"))
  
  p2 <- ggplot(data = plt_df, aes(x = grp, y = perf)) +
    geom_bar(stat="identity", width = 0.6) +
    theme_bw() +
    xlab('Training Region') +
    ylab('RMSE') +
    ggtitle('Testing Region: Rainfall-Dominated', subtitle = flow_metric) +
    scale_x_discrete(limits=c("Rain","Snow","Rain+Snow","CONUS")) +
    theme(axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.x = element_text(size = 18),
          plot.title = element_text(size = 20)) +
    geom_text(aes(label = round(perf,1)), vjust=-0.3, size=3.5)
  
  ggsave(filename = fileout[2], plot = p2, device = 'png')
  
  #Snow region performance
  plt_df <- data.frame(perf = c(get_perf_metric(test_rain_snow$metrics, perf_metric = perf_metric),
                                get_perf_metric(test_snow_snow$metrics, perf_metric = perf_metric),
                                get_perf_metric(test_rain_snow_snow$metrics, perf_metric = perf_metric),
                                get_perf_metric(test_CONUS_snow$metrics, perf_metric = perf_metric)),
                       grp = c("Rain","Snow","Rain+Snow","CONUS"))
  
  p3 <- ggplot(data = plt_df, aes(x = grp, y = perf)) +
    geom_bar(stat="identity", width = 0.6) +
    theme_bw() +
    xlab('Training Region') +
    ylab('RMSE') +
    ggtitle('Testing Region: Snowmelt-Dominated', subtitle = flow_metric) +
    scale_x_discrete(limits=c("Rain","Snow","Rain+Snow","CONUS")) +
    theme(axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.x = element_text(size = 18),
          plot.title = element_text(size = 20)) +
    geom_text(aes(label = round(perf,1)), vjust=-0.3, size=3.5)
  
  ggsave(filename = fileout[3], plot = p3, device = 'png')

  return(fileout)
}

get_perf_metric <- function(model_fit, perf_metric){
  #'
  #' @description returns the performance metric for the fitted model
  #'
  #' @param model_fit fitted model
  #' @param perf_metric performance metric name
  #'
  #' @return performance metric value
  
  model_fit$.estimate[model_fit$.metric == perf_metric]
}

plot_metric_boxplot <- function(data_split, metric, region, out_dir){
  #'
  #' @description returns boxplots comparing the training and testing splits
  #' for the metric.
  #'
  #' @param data_split the training and testing split. Example: p6_Boruta_CONUS_g2$input_data
  #' @param metric performance metric name
  #' @param region the modeling region
  #'
  #' @return filepath to the resulting plot
  
  fileout <- file.path(out_dir, paste0('train_test_boxplot_', metric, '_', region, '.png'))
  
  png(filename = fileout, width = 4, height = 4, units = 'in', res = 200)
  boxplot(data_split$training[[metric]],
          data_split$testing[[metric]], 
          names = c('Training', 'Testing'),
          main = paste0('Metric: ', metric, '\nRegion: ', region),
          cex.main = 0.8)
  dev.off()
  
  return(fileout)
}


plot_pred_obs <- function(df_pred_obs, metric, region, out_dir,
                          from_predict = FALSE, model_wf = NULL, pred_data = NULL){
  #'
  #' @description returns boxplots comparing the training and testing splits
  #' for the metric.
  #'
  #' @param df_pred_obs df with obs and .pred columns
  #' @param metric performance metric name
  #' @param region the modeling region
  #' @param from_predict logical stating if predictions should be made within
  #' this function using the provided model_wf and pred_data
  #' @param model_wf model workflow
  #' @param pred_data new_data for predict.workflow
  #'
  #' @return filepath to the resulting plot
  
  fileout <- file.path(out_dir, paste0('pred_obs_scatter', metric, '_', region, '.png'))
  
  if(from_predict){
    #predict from provided workflow and data
    df_pred_obs <- predict(model_wf, new_data = pred_data, type = 'numeric') %>%
      mutate(obs = pred_data[[metric]])
  }
  
  plt_lim <- max(c(df_pred_obs$obs, df_pred_obs$.pred))
  
  png(filename = fileout, width = 4, height = 4, units = 'in', res = 200)
  plot(df_pred_obs$obs, df_pred_obs$.pred,
       xlim = c(0,plt_lim), ylim = c(0,plt_lim),
       xlab = 'Observed', ylab = 'Predicted', cex = 0.4, pch = 16,
       main = paste0('Metric: ', metric, '\nRegion: ', region),
       cex.main = 0.8)
  lines(c(0,plt_lim), c(0,plt_lim), col = 'red')
  dev.off()
  
  return(fileout)
}


make_residual_map <- function(df_pred_obs, sites, metric, pred_gage_ids, region, out_dir,
                              from_predict = FALSE, model_wf = NULL, pred_data = NULL){
  #' @description this function creates maps of residuals (obs - predicted) and a
  #' variogram plot
  #' 
  #' @param df_pred_obs is a data frame with observed (obs) and predicted (.pred) from 
  #' the output of  predict_test_data function. if left null and from_predict is true, 
  #' it will predict it from the model_wf (copied these lines from the plot_pred_obs function)
  #' @param sites  gagesii spatial data object with LAT LON and ID
  #' @param metric  metric that is being predicted
  #' @param pred_gage_ids vector of gage ids that correspond to each row of the df_pred_obs
  #' @param region  region of the model/prediction
  #' @param directory where output figures are saved

  if(from_predict){
    #predict from provided workflow and data
    df_pred_obs <- predict(model_wf, new_data = pred_data, type = 'numeric') %>%
      mutate(obs = pred_data[[metric]])
  }
  
  lat_lons<- sites %>%
    rename(ID = GAGES_ID) %>%
    select(ID, LAT, LON) %>%
    filter(ID %in% pred_gage_ids)
  
  df<- bind_cols(lat_lons, df_pred_obs) %>%
    mutate(resid = obs - .pred)
  
  states <- map_data("state")
  limit <- quantile(df$resid, probs = c( 0.1, 0.9))
  p1<-ggplot(states, aes(x=long, y=lat, group=group)) +
    geom_polygon(fill="gray60", color="gray80") +
    geom_sf(data = df, inherit.aes = FALSE, 
            aes(color = .data[["resid"]]), 
            size = 0.5)+
    scale_color_scico(palette = 'roma',
                      midpoint = 0,
                      limits = limit,
                      oob = scales::squish)+
    theme(legend.position="bottom",
          legend.key.size=unit(.75,'cm'))+
    xlab('Longitude') + 
    ylab('Latitude')+
    ggtitle(paste("metric= ",metric,"    region = ", region ))
  
  p2 <- plot(variogram(resid ~1, df,
                       cutoff = 1000, 
                       width = 2,
                       cressie = TRUE),
             asp=1)
  
  
  fname<-paste0(out_dir, "/resid_map_", metric, "_", region, ".png")
  
  save_plot(filename = fname, 
            plot = plot_grid(p1,p2, scale = c(1,.8)),
            base_width = 10,
            bg="white")

  return(fname)
}


make_class_prediction_map <- function(class_probs, reaches, out_dir,
                                      plot_threshold = 0.05, model_name,
                                      ncores = 1, pt_size = 0.5){
  #' @description this function creates maps of predicted class probabilities for
  #' each reach
  #' 
  #' @param class_probs dataframe of predicted class probabilities for each reach.
  #' must have an "ID" column and columns for the class probabilities labeled with
  #' the name of the class. No other columns.
  #' @param reaches sf object containing the reaches to plot. Must have a "COMID" column
  #' @param out_dir where output figures are saved
  #' @param plot_threshold threshold below which sites / reaches are not plotted
  #' because the probability is too low.
  #' @param model_name name to add to the file name that describes this model
  #' @param ncores number of cores to use
  #' 
  #' @return file paths to maps
  
  #get number of ranks
  num_ranks <- ncol(class_probs) - 1
  
  cl = parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  
  #This method works for all cases except when there is only 1 class
  # whose probability is < prob_threshold. That class will still receive a rank
  # instead of an NA for plotting. I'm not sure of a quick way around that problem.
  inds_NA <- which(class_probs[,-which(colnames(class_probs) == "ID")] < plot_threshold, arr.ind = TRUE)
  class_probs[,-which(colnames(class_probs) == "ID")][inds_NA] <- -1
  
  #Convert the predicted probabilities into a rank of which class is most likely (1) 
  #to least likely (n = number of classes)
  rank_mat <- t(parApply(cl = cl, X = class_probs[, -which(colnames(class_probs) == "ID")], 
                      MARGIN = 1, FUN = decreasing_rank)) %>%
    as.data.frame()
  
  rank_mat$ID <- class_probs$ID
  colnames(rank_mat) <- colnames(class_probs)
  
  #Make new columns for the rank of the class (change the cells to column names)
  LikelyRanks <- matrix(NA, nrow = nrow(rank_mat), ncol = num_ranks)
  for(i in 1:ncol(LikelyRanks)){
    LikelyRanks[,i] <- parApply(cl = cl, X = rank_mat, MARGIN = 1, FUN = assign_max_rank, 
                             rank = i, rank_cols = colnames(rank_mat)[1:num_ranks]) 
  }
  LikelyRanks <- as.data.frame(LikelyRanks)
  colnames(LikelyRanks) <- paste0('LikelyRank', seq(1,ncol(LikelyRanks),1))
  #Change to characters
  LikelyRanks <- as.data.frame(apply(X = LikelyRanks, MARGIN = 2, FUN = as.character, simplify = FALSE))
  #Add ID
  LikelyRanks$ID <- rank_mat$ID
  
  #Join ranks to reaches for plotting
  reaches <- left_join(reaches, LikelyRanks, by = "ID")
  
  #Plot one map for the most likely class, second most likely, etc. to the nth class
  states <- map_data("state")
  
  #plot and return filenames
  fnames <- foreach(i = 1:num_ranks, .packages = c('ggplot2', 'scico', 'sf'),
                    .combine = c, .inorder = TRUE) %dopar% {
    col_name <- colnames(LikelyRanks)[i]
    fname <- file.path(out_dir, paste0(model_name, '_', col_name, '_map.png'))
    
    #Only plot if some data are not NA (< plotting threshold)
    if(!all(is.na(reaches[[col_name]]))){
      plot_sites <- reaches[!is.na(reaches[[col_name]]),]
      
      p1 <- ggplot(states, aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = "white", color = "gray80") +
        geom_sf(data = plot_sites, inherit.aes = FALSE, 
                aes(color = .data[[col_name]]), 
                size = pt_size) +
        scale_color_scico_d(palette = 'berlin') +
        theme(legend.position="bottom",
              legend.key.size=unit(2,'cm'),
              legend.text=element_text(size=16)) +
        guides(color = guide_legend(override.aes = list(size=10))) +
        xlab('Longitude') + 
        ylab('Latitude')
      
      ggsave(filename = fname, plot = p1, bg = "white")
    }
    
    fname
  }
  
  parallel::stopCluster(cl)
  
  return(fnames)
}

assign_max_rank <- function(rank_vec, rank, rank_cols){
  #' @description this function finds the index of rank within rank_vec
  #' 
  #' @param rank_vec the vector containing ranks
  #' @param rank the rank to search for in rank_vec
  #' @param rank_cols column names of rank_vec to search for rank
  #' 
  #' @return index of the rank within the rank_vec
  
  ind <- which(rank_vec[rank_cols] == rank)
  
  if((length(ind) > 1) | (length(ind) == 0)){
    #there are ties for this rank, which generally means the prob was 0.
    #return NA for the ind
    ind <- NA
  }
  
  return(ind)
}


decreasing_rank <- function(values){
  #' @description this function computes a decreasing rank for the values provided
  #' 
  #' @param values vector of values to be ranked
  #' 
  #' @return vector of ranks for those values
  
  rank_vals <- rank(-rank(values, ties.method = 'max'), ties.method = 'min')
  
  return(rank_vals)
}


#SHAP values
plot_shap_global <- function(shap, model_name, out_dir, num_features = 40){
  #' 
  #' @description Creates SHAP global importance plot
  #'
  #' @param shap SHAP value results from compute_SHAP
  #' @param model_name character string describing the model. Will be added 
  #' to the end of the filename before the file extension, and also be the plot title.
  #' @param out_dir output directory
  #'
  #' @return Returns the paths to png files of SHAP dependence plots for each feature
  
  fileout <- file.path(out_dir, 
                       paste0('SHAP_global_', model_name, '.png'))
  
  p1 <- autoplot(shap, type = "importance", num_features = num_features) +
    ggtitle(model_name) + 
    theme(axis.text.y = element_text(size = 5))
  
  ggsave(filename = fileout, plot = p1, device = 'png')
  
  return(fileout)
}
plot_shap_global_sv <- function(shap, data, model_name, out_dir, num_features = 40,
                                sv_kind = 'both'){
  #' 
  #' @description Creates SHAP global importance plot using the shapviz package
  #'
  #' @param shap SHAP value results from compute_SHAP
  #' @param data attributes data for the columns within shap
  #' @param model_name character string describing the model. Will be added 
  #' to the end of the filename before the file extension, and also be the plot title.
  #' @param out_dir output directory
  #' @param sv_kind kind of shapviz plot. bar, beeswarm, or both.
  #'
  #' @return Returns the paths to png files of SHAP dependence plots for each feature
  
  if(class(shap) == 'list'){
    #make plots for each list
    filesout <- vector('character', length = length(shap))
    
    for (i in 1:length(filesout)){
      filesout[i] <- file.path(out_dir, 
                           paste0('SHAP_global_', model_name, '_', names(shap)[i], 
                                  '_vars', num_features, '.png'))
      
      p1 <- sv_importance(shapviz(shap[[i]], X = data[,colnames(data) %in% colnames(shap[[i]])]), 
                    kind = sv_kind, max_display = num_features, fill = 'black',
                    alpha = 0.5) +
        ggtitle(model_name, subtitle = names(shap)[i])
      
      ggsave(filename = filesout[i], plot = p1, device = 'png')
    }
  }else{
    filesout <- file.path(out_dir, 
                         paste0('SHAP_global_', model_name, '_vars', num_features, '.png'))
    
    p1 <- sv_importance(shapviz(shap, X = data[,colnames(data) %in% colnames(shap)]), 
                        kind = sv_kind, max_display = num_features, fill = 'black',
                        alpha = 0.5) +
      ggtitle(model_name)
    
    ggsave(filename = filesout, plot = p1, device = 'png')
  }
  
  return(filesout)
}

plot_shap_dependence <- function(shap, data, model_name, out_dir, ncores = 1){
  #' 
  #' @description Creates SHAP dependence plots for each feature
  #'
  #' @param shap SHAP value results from compute_SHAP
  #' @param data the X dataframe used to compute SHAP values
  #' @param model_name character string describing the model. Will be added 
  #' to the end of the filename before the file extension, and also be the plot title.
  #' @param out_dir output directory
  #' @param ncores number of cores to use for parallel plot creation
  #'
  #' @return Returns the paths to png files of SHAP dependence plots for each feature
  
  #number of features to make plots for
  n_plts <- ncol(shap)
  
  cl = parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  
  filesout <- foreach(i = 1:n_plts, .inorder = TRUE, .combine = c, 
                      .packages = c('ggplot2', 'fastshap')) %dopar% {
                        fileout <- file.path(out_dir, 
                                             paste0('SHAP_dependence_', colnames(shap)[i], '_',
                                                    model_name, '.png'))
                        
                        p <- autoplot(shap, type = "dependence", feature = colnames(shap)[i], 
                                      X = data, 
                                      alpha = 0.5, smooth = TRUE, smooth_color = "black") +
                          ggtitle(model_name)
                        
                        ggsave(filename = fileout, plot = p, device = 'png')
                        
                        fileout
                      }
  
  parallel::stopCluster(cl)
  
  return(filesout)
}
plot_shap_dependence_sv <- function(shap, data, model_name, out_dir, ncores = 1){
  #' 
  #' @description Creates SHAP dependence plots for each feature using the shapviz package
  #'
  #' @param shap SHAP value results from compute_SHAP
  #' @param data the X dataframe used to compute SHAP values
  #' @param model_name character string describing the model. Will be added 
  #' to the end of the filename before the file extension, and also be the plot title.
  #' @param out_dir output directory
  #' @param ncores number of cores to use for parallel plot creation
  #'
  #' @return Returns the paths to png files of SHAP dependence plots for each feature
  
  cl = parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  
  if(class(shap) == 'list'){
    #make plots for each list
    
    #number of features to make plots for
    n_plts <- ncol(shap[[1]])
    
    filesout <- vector('character', length = 0)
    for (j in 1:length(shap)){
      filesout_j <- foreach(i = 1:n_plts, .inorder = TRUE, .combine = c, 
                          .packages = c('ggplot2', 'fastshap', 'shapviz')) %dopar% {
                            fileout <- file.path(out_dir, 
                                                 paste0(model_name, '_', 
                                                        names(shap)[j], 
                                                        '_SHAP_dependence_', 
                                                        colnames(shap[[j]])[i], 
                                                        '.png'))
                            
                            p <- sv_dependence(shapviz(shap[[j]], 
                                                       X = data[,colnames(data) %in% colnames(shap[[j]])]), 
                                               v = colnames(shap[[j]])[i],
                                               alpha = 0.5) +
                              geom_smooth(method = 'loess', se = FALSE, show.legend = FALSE) +
                              ggtitle(model_name, subtitle = names(shap)[j])
                            
                            ggsave(filename = fileout, plot = p, device = 'png')
                            
                            fileout
                          }
      
      filesout <- c(filesout, filesout_j)
    }
    
  }else{
    #number of features to make plots for
    n_plts <- ncol(shap)
    
    filesout <- foreach(i = 1:n_plts, .inorder = TRUE, .combine = c, 
                        .packages = c('ggplot2', 'fastshap', 'shapviz')) %dopar% {
                          fileout <- file.path(out_dir, 
                                               paste0(model_name, '_', 
                                                      '_SHAP_dependence_', 
                                                      colnames(shap)[i], 
                                                      '.png'))
                          
                          p <- sv_dependence(shapviz(shap, 
                                                     X = data[,colnames(data) %in% colnames(shap)]), 
                                             v = colnames(shap)[i],
                                             alpha = 0.5) +
                            geom_smooth(method = 'loess', se = FALSE, show.legend = FALSE) +
                            ggtitle(model_name)
                          
                          ggsave(filename = fileout, plot = p, device = 'png')
                          
                          fileout
                        }
  }
  
  parallel::stopCluster(cl)
  
  return(filesout)
}

plot_shap_individual <- function(shap, data, reach, model_name, out_dir,
                                 num_features = 40){
  #' 
  #' @description Creates a SHAP contribution plot for an individual prediction index.
  #'
  #' @param shap SHAP value results from compute_SHAP
  #' @param data dataframe with COMID column with rows in the
  #' same order as shap
  #' @param reach COMID of observation to plot
  #' @param model_name character string describing the model. Will be added 
  #' to the end of the filename before the file extension, and also be the plot title.
  #' @param out_dir output directory
  #'
  #' @return Returns the path to the png file of feature contributions to the index prediction
  
  #row index for which to compute plot
  ind_plt <- which(data$COMID == reach)
  
  fileout <- file.path(out_dir, paste0('SHAP_individual_', model_name, 
                                       '_reach-', reach, '_vars', num_features, '.png'))
  
  p1 <- autoplot(shap[ind_plt,], type = "contribution", num_features = num_features) +
    ggtitle(model_name, subtitle = paste0('reach ', reach)) +
    theme(axis.text.y = element_text(size = 5))
  
  ggsave(filename = fileout, plot = p1, device = 'png')
  
  return(fileout)
}


#PDP and ICE
plot_pdp <- function(partial, data, model_name, out_dir, 
                     ncores = 1, ice = FALSE, offset = FALSE){
  #'
  #' @description Creates PDP plots for each feature
  #'
  #' @param partial result from compute_pdp
  #' @param data the dataframe used to make model predictions
  #' @param model_name character string describing the model. Will be added
  #' to the end of the filename before the file extension, and also be the plot title.
  #' @param out_dir output directory
  #' @param ncores number of cores to use for parallel plot creation
  #' @param ice logical. if TRUE, plots ICE values.
  #' @param offset logical. if TRUE, offset first pdp element to 0.
  #'
  #' @return Returns the paths to png files of PDP for each feature
  
  cl = parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  
  #number of features to make plots for
  n_plts <- length(partial)
  
  filesout <- foreach(i = 1:n_plts, .inorder = TRUE, .combine = c, 
          .packages = c('ggplot2', 'pdp', 'tidyverse', 'scico'), .export = 'offset_partial') %dopar% {
            #Convert class ID to character to plot correctly
            partial[[i]]$yhat.id <- as.character(partial[[i]]$yhat.id) 
            
            if(offset){
              #offset each yhat.id such that it starts at 0
              partial[[i]]$yhat <- offset_partial(partial[[i]])
              
              #used for a figure. get rid of classes 4 and 5 but retain color scheme
              #partial[[i]][,1][partial[[i]]$yhat.id > 3] <- min(partial[[i]][,1])
              #partial[[i]][,1][partial[[i]]$yhat > 3] <- 0
              
              #add y=-0.5 for rug
              data$y0 <- -0.5
              
              fileout <- file.path(out_dir,
                                   paste0(ifelse(ice, 'ICE_offset_', 'PDP_offset_'), names(partial)[i], '_',
                                          model_name, '.png'))
              
              p <- ggplot(data = as.data.frame(partial[[i]]), 
                          mapping = aes(x = as.data.frame(partial[[i]])[,1], y = yhat, color = yhat.id)) + 
                geom_line(size = 3) + 
                #add rug to indicate observation loactions
                geom_point(data = data, mapping = aes(x = as.data.frame(data[,colnames(partial[[i]])[1]])[,1],
                                                      y = y0, 
                                                      color = NA),
                           shape = '|',
                           show.legend = FALSE) +
                ylim(-0.5,0.5) +
                ggtitle(model_name) +
                ylab('Centered Region Probability') +
                xlab(colnames(as.data.frame(partial[[i]]))[1]) +
                theme_classic() +
                scale_color_scico_d(palette = 'batlow') +
                labs(color = "Region")
              
            }else{
              #add y=0 for rug
              data$y0 <- 0
              
              fileout <- file.path(out_dir,
                                   paste0(ifelse(ice, 'ICE_', 'PDP_'), names(partial)[i], '_',
                                          model_name, '.png'))
              
              p <- ggplot(data = as.data.frame(partial[[i]]), 
                          mapping = aes(x = as.data.frame(partial[[i]])[,1], y = yhat, color = yhat.id)) + 
                geom_line(size = 3) + 
                #add rug to indicate observation loactions
                geom_point(data = data, mapping = aes(x = as.data.frame(data[,colnames(partial[[i]])[1]])[,1],
                                                      y = y0, 
                                                      color = NA),
                           shape = '|',
                           show.legend = FALSE) +
                ylim(0,1) +
                ggtitle(model_name) +
                ylab('Average Class Probability') +
                xlab(colnames(as.data.frame(partial[[i]]))[1]) +
                theme_classic() +
                scale_color_scico_d(palette = 'batlow') +
                labs(color = "Region")
            }
            
            ggsave(filename = fileout, plot = p, device = 'png')
            
            fileout
          }
  
  parallel::stopCluster(cl)
  
  return(filesout)
}


offset_partial <- function(partial_data){
  #'
  #' @description Creates an offset yhat value in which the first element is 0
  #' and all other elements are their original value minus the original first
  #' element value.
  #'
  #' @param partial_data data.frame with columns yhat (numeric) and yhat.id (character)
  #'
  #' @return Returns the offset yhat vector
  
  for(i in 1:length(unique(partial_data$yhat.id))){
    inds_i <- which(partial_data$yhat.id == as.character(i))
    partial_data$yhat[inds_i] <- partial_data$yhat[inds_i] - partial_data$yhat[inds_i][1]
  }
  
  return(partial_data$yhat)
}



make_class_prediction_map_panel_for_paper <- function(class_probs, reaches, 
                                                      plot_threshold = 0.05, model_name,
                                                      ncores = 1, pt_size = 0.5, fname,
                                                      title_str, color_pal){
  #' @description this function is a modified form of the make_class_prediction_map
  #' to make figure 2 for the regions paper. 
  #' 
  #' @param class_probs dataframe of predicted class probabilities for each reach.
  #' must have an "ID" column and columns for the class probabilities labeled with
  #' the name of the class. No other columns.
  #' @param reaches sf object containing the reaches to plot. Must have a "COMID" column
  #' @param out_dir where output figures are saved
  #' @param plot_threshold threshold below which sites / reaches are not plotted
  #' because the probability is too low.
  #' @param model_name name to add to the file name that describes this model
  #' @param ncores number of cores to use
  #' 
  #' @return file paths to map
  
  #get number of ranks
  num_ranks <- ncol(class_probs) - 1
  
  cl = parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  
  #This method works for all cases except when there is only 1 class
  # whose probability is < prob_threshold. That class will still receive a rank
  # instead of an NA for plotting. I'm not sure of a quick way around that problem.
  inds_NA <- which(class_probs[,-which(colnames(class_probs) == "ID")] < plot_threshold, arr.ind = TRUE)
  class_probs[,-which(colnames(class_probs) == "ID")][inds_NA] <- -1
  
  #Convert the predicted probabilities into a rank of which class is most likely (1) 
  #to least likely (n = number of classes)
  rank_mat <- t(parApply(cl = cl, X = class_probs[, -which(colnames(class_probs) == "ID")], 
                         MARGIN = 1, FUN = decreasing_rank)) %>%
    as.data.frame()

  rank_mat$ID <- class_probs$ID
  colnames(rank_mat) <- colnames(class_probs)

  
  #Make new columns for the rank of the class (change the cells to column names)
  LikelyRanks <- matrix(NA, nrow = nrow(rank_mat), ncol = num_ranks)
  for(i in 1:ncol(LikelyRanks)){
    LikelyRanks[,i] <- parApply(cl = cl, X = rank_mat, MARGIN = 1, FUN = assign_max_rank, 
                                rank = i, rank_cols = colnames(rank_mat)[1:num_ranks]) 
  }
  LikelyRanks <- as.data.frame(LikelyRanks)
  colnames(LikelyRanks) <- paste0('LikelyRank', seq(1,ncol(LikelyRanks),1))
  #Change to characters
  LikelyRanks <- as.data.frame(apply(X = LikelyRanks, MARGIN = 2, FUN = as.character, simplify = FALSE))
  #Add ID
  LikelyRanks$ID <- rank_mat$ID
  
  #Join ranks to reaches for plotting
  reaches <- left_join(reaches, LikelyRanks, by = "ID")
  
  #Plot one map for the most likely class, second most likely, etc. to the nth class
  states <- map_data("state")
  parallel::stopCluster(cl)
  p<- list()

  
  for (i in 1:2){

    col_name <- colnames(LikelyRanks)[i]
    if(!all(is.na(reaches[[col_name]]))){
      plot_sites <- reaches[!is.na(reaches[[col_name]]),]
      
      p[[i]] <- ggplot(states, aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = "white", color = "gray80") +
        geom_sf(data = plot_sites, inherit.aes = FALSE, 
                aes(color = .data[[col_name]]), 
                size = pt_size) +
        scale_color_scico_d(palette = color_pal) +
        theme(legend.position="bottom",
              legend.key.size=unit(1,'cm'),
              legend.key= element_blank(),
              legend.text=element_text(size=10)) +
        guides(color = guide_legend(override.aes = list(size=5))) +
        xlab('Longitude') + 
        ylab('Latitude')+
        labs(color = "Region")+
        ggtitle(title_str[i])
    } #endif
  } #end for i
  
  
  ggarrange(p[[1]], p[[2]], ncol = 1,common.legend = TRUE, legend = "bottom")

  ggsave(filename = fname, bg = "white")

 # parallel::stopCluster(cl)
  
  return(fname)
}