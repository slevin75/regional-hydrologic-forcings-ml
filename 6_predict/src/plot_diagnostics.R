plot_Boruta <- function(brf_model, metric, out_dir){
  #' 
  #' @description Plots the Boruta feature importance plot
  #'
  #' @param brf_model output of Boruta()
  #' 
  #' @value filepath to resulting plot
  #' 
  
  fileout <- file.path(out_dir, paste0('Boruta_', metric, '.png'))
  
  png(fileout, width = 8, height = 4, units = 'in', res = 200)
  plot(brf_model)
  dev.off()
  
  return(fileout)
}