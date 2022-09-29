plot_data_coverage<- function(clean_daily_flow, cluster_table, dir_out){
  
  #' @description Function to plot the date range for which there is data for each gage. Gages
  #' are grouped by cluster group.  Will produce a file for each quantile group of clusters.
  #' 
  #' @param clean_daily_flow p1_clean_daily_flow target. Includes date, daily discharge
  #' year, day, and site_no
  #' @param cluster_table data frame with ID (gage ID), and any number of columns containing
  #' cluster id for different groups of quantiles
  #' @param dir_out directory to save plots
  
  

  years_with_data<- clean_daily_flow %>%
    select(site_no, year_val) %>%
    distinct(.) %>%
    rename(ID = site_no) %>%
    left_join(.,cluster_table, by= "ID") 
  
  
  plot_cols <- names(years_with_data[,-c(1:2)])
  plot_list<- purrr::map(plot_cols, make_por_plot, years_with_data, dir_out)
  return(plot_list)
  
}


make_por_plot<- function(plot_col, years_with_data, dir_out){
  
  df <- years_with_data  %>%
    rename(cluster = plot_col) %>%
    select(ID, year_val, cluster) %>%
    mutate(cluster_ID = paste0(cluster, "_", ID))
  
  cluster_name<- paste(plot_col, "cluster group")
  
  p1<- ggplot(df, aes(x= year_val, y = cluster_ID, fill = as.factor(cluster))) + geom_tile() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())+
    ylab(cluster_name)+
    scale_fill_discrete(name = cluster_name) +
    scale_x_continuous(breaks = seq(1900, 2020, 20))
  
  fileout<- file.path(dir_out, paste0(plot_col, "quants_data_coverage.png"))
  ggsave(filename = fileout,
         plot = p1,
         device = png)
  
  
   return(fileout)
  
}

 

  

  


