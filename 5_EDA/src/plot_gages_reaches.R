make_gages_map <- function(gages, out_dir){
  #' @description this function creates a map of gage locations
  #' 
  #' @param reaches sf object containing the gages to plot
  #' @param out_dir where output figure is saved
  #' 
  #' @return file path to map
  
  states <- map_data("state")
  
  fname <- file.path(out_dir, 'gage_location_map.png')

  p1 <- ggplot(states, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "white", color = "gray80") +
    geom_sf(data = gages, inherit.aes = FALSE, 
            size = 0.5) +
    xlab('Longitude') + 
    ylab('Latitude')
    
  ggsave(filename = fname, plot = p1, bg = "white")
  
  return(fname)
}
