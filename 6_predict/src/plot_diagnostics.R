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
    geom_polygon(fill="gray60", colour="gray80") +
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


#Residual error boxplots
# boxplot(p6_test_RF_snow_snow$pred$.pred - p6_test_RF_snow_snow$pred$obs,
#                    p6_test_RF_rain_snow_snow$pred$.pred - p6_test_RF_rain_snow_snow$pred$obs,
#                    p6_test_RF_CONUS_g2_snow$pred$.pred - p6_test_RF_CONUS_g2_snow$pred$obs, names = c('Rain', 'Rain+Snow', 'CONUS'))
# boxplot(p6_test_RF_snow_snow$pred$.pred - p6_test_RF_snow_snow$pred$obs,
#                    p6_test_RF_rain_snow_snow$pred$.pred - p6_test_RF_rain_snow_snow$pred$obs,
#                    p6_test_RF_CONUS_g2_snow$pred$.pred - p6_test_RF_CONUS_g2_snow$pred$obs, names = c('Snow', 'Rain+Snow', 'CONUS'))