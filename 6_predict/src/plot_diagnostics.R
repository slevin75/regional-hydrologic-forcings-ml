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
  #' 
  
  fileout <- file.path(out_dir, paste0('Boruta_', metric, '_', region, '.png'))
  
  png(fileout, width = 8, height = 4, units = 'in', res = 200)
  #plot without outlier points
  plot(brf_model, outpch = NA, show.names = FALSE)
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
  #' 
  
  fileout <- file.path(out_dir, paste0('hyperparam_diagnostic_', 
                                       metric, '_', region, '.png'))
  
  p1 <- opt_result %>% 
    collect_metrics() %>%
    ggplot(aes(mtry, mean, color = min_n)) +
    geom_line(size = 1.5, alpha = 0.6) +
    geom_point(size = 2) +
    facet_wrap(~ .metric, scales = "free", nrow = 2) +
    scale_color_viridis_c(option = "plasma", begin = .9, end = 0) +
    ggtitle(metric)
  
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
  #' 
  
  fileout <- file.path(out_dir, paste0('vip_', metric, '_', region, '.png'))
  
  p1 <- vip(RF_model %>% extract_fit_parsnip(), 
            num_features = num_features) + 
    ggtitle(metric)
  
  ggsave(filename = fileout, plot = p1, device = 'png')
  
  return(fileout)
}

barplot_compare_RF <- function(rain_mod, snow_mod, rain_snow_mod, CONUS_mod,
                          test_rain_snow, test_snow_rain, test_rain_snow_rain,
                          test_rain_snow_snow, test_CONUS_rain, test_CONUS_snow,
                          metric, out_dir){
  #' 
  #' @description makes barplots of RMSEs for each of the supplied models
  #'
  #' @param 
  #' @param metric metric name
  #' @param out_dir output directory
  #' 
  #' @return filepath to resulting plot
  #' 
  
  #3 plots:
  #only rain region
  #only snow region
  #all regions
  fileout <- c(file.path(out_dir, paste0('compare_models_RF_', metric, '_rain.png')),
               file.path(out_dir, paste0('compare_models_RF_', metric, '_snow.png')),
               file.path(out_dir, paste0('compare_models_RF_', metric, '_rain+snow.png')))
  
  #Rain region performance
  #rain_mod, test_snow_rain, test_rain_snow_rain, test_CONUS_rain
  
  #Snow region performance
  #snow_mod, test_rain_snow, test_rain_snow_snow, test_CONUS_snow
  
  #Overall performance, also showing rain and snow performance
  #rain_mod, snow_mod, rain_snow_mod, CONUS_mod,
  #test_rain_snow, test_snow_rain, test_rain_snow_rain,
  #test_rain_snow_snow, test_CONUS_rain, test_CONUS_snow
  
  return(fileout)
}