
plot_data_coverage <- function(screened_site_list, cluster_table, dv_data_dir, dir_out, estimated_data){
  #' @description Function to plot the date range for which there is data for each gage. Gages
  #' are grouped by cluster group.  Will produce a file for each quantile group of clusters.
  #' 
  #' @param screened_site_list p1_screened_site_list target. 
  #' @param cluster_table data frame with ID (gage ID), and any number of columns containing
  #' cluster id for different groups of quantiles
  #' @param dv_data_dir directory where daily NWIS streamflow data were saved (currently ./1_fetch/out")
  #' @param dir_out directory to save plots
  #' @param estimated_data whether or not to use estimated data in the plots
  
  
  ##there are some sites with more than one discharge field - I think this might have something
  ##to do with the edits Charlie made when there were more than 1 gage on a single streamline.  For
  ##now I just removed them - there aren't many and it was breaking my head to try to combine them.
  
  df_all <- data.frame()
  file_list <- list.files(dv_data_dir, pattern = "_dv.csv")
  screened_site_list <-paste0 (screened_site_list, "_dv.csv") 
  screened_site_list <- screened_site_list[which(screened_site_list %in% file_list)]
  
  df_tally_allgages<-data.frame()
  for (i in 1:length(screened_site_list)){
    print(i)
    
    fname<- file.path(dv_data_dir,screened_site_list[i])
    station.ID <- substr(screened_site_list[i], start = 1, stop = 8)
    df<-read_csv(fname, col_types = cols() ) %>%
      addWaterYear()
    
    if("discharge" %in% names(df)==TRUE ){
      df_screened <- df %>%
        filter(!is.na(discharge)) %>%
        rename(ID = site_no)
    if (estimated_data == FALSE ) {
      prov_data <- grep(pattern= 'P | e', x=df_screened$discharge_cd)
    } else {
      prov_data <- grep(pattern= 'P', x=df_screened$discharge_cd)
    }
     
    
      if(length(prov_data) > 0){  df_screened <-   df_screened[-prov_data, ]}
      tally_obs<- df_screened %>%
        group_by(waterYear) %>%
        count() %>%
        mutate(ID = station.ID)
      df_tally_allgages<-rbind(df_tally_allgages, tally_obs) 
    }
    
  }  #end for
  
  
  df_tally_allgages<- df_tally_allgages %>%
    left_join(.,cluster_table, by= "ID")
  plot_cols <- names(df_tally_allgages[,-c(1:3)])
  
  
  plot_paths<- purrr::map(plot_cols, make_por_plot, df_tally_allgages, dir_out, estimated_data) 
  
  
}  


make_por_plot<- function(plot_col, tally_allgages, dir_out,estimated_data ){
  #' @description helper function for plot_data_coverage creates the plots for a 
  #' single cluster and returns it to plot_data_coverage
  
  
  df <- tally_allgages  %>%
    rename(cluster = plot_col) %>%
    select(ID, waterYear, n,cluster) %>%
    mutate(cluster_ID = paste0(cluster, "_", ID))
  
  cluster_name<- paste(plot_col, "cluster group")
  
  p1<-ggplot(df, aes(x= waterYear, y = ID, fill = n)) + geom_tile() +
    scale_fill_viridis_c(option = "plasma")+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())+
    scale_x_continuous(breaks = seq(1900, 2020, 20))+
    facet_grid(cluster~., scales="free", space="free")+
    ggtitle(paste("data coverage for " , cluster_name))
  if (estimated_data == FALSE) {
    fileout<- file.path(dir_out, paste0(plot_col, "quants_data_coverage-no_estimated_data.png"))
  } else {
    fileout<- file.path(dir_out, paste0(plot_col, "quants_data_coverage- with_estimated_data.png"))
  }
 
  ggsave(filename = fileout,
         plot = p1,
         device = png)
  
  
  return(fileout)
  
}


estimated_data_quantiles <- function(screened_site_list, cluster_table,dv_data_dir,dir_out){
  #' @description Function to determine and plot the quantiles of the estimated data by cluster
  #' 
  #' @param screened_site_list p1_screened_site_list target. 
  #' @param cluster_table data frame with ID (gage ID), and any number of columns containing
  #' cluster id for different groups of quantiles
  #' @param dv_data_dir directory where daily NWIS streamflow data were saved (currently ./1_fetch/out")
  #' @param dir_out directory to save plots
  
  
  gage_list<- unique(cluster_table$ID) 
  file_list <- paste0(dv_data_dir, "/", gage_list, "_dv.csv") 
  #removes the data files for the few gages that have been combined
  file_list <- file_list[which(file.exists(file_list))]
  estimated_data_allgages <- purrr::map_df(file_list, get_estimated_data)
  
  df_plot<-estimated_data_allgages %>%
    left_join(.,cluster_table, by="ID")%>%
    rename(midhigh_cluster = midhigh, high_cluster = high)
  
  
  p1 <- ggplot(df_plot, aes(quantile)) + geom_histogram() + 
    facet_wrap(~midhigh_cluster, scales= "free") +
    ggtitle("Quantiles of estimated data for midhigh clusters")
  
  p2 <- ggplot(df_plot, aes(quantile)) + geom_histogram() + 
    facet_wrap(~high_cluster, scales= "free") +
    ggtitle("Quantiles of estimated data for high clusters")
  
  
  fname <- paste0("Estimated_data_quantiles.png")
  fileout <- file.path(dir_out, fname)  
  ggsave(filename = fileout, 
         plot =   plot_grid(p1,p2, ncol = 1, label_size = 8))
  
  return(fileout)
  
}



get_estimated_data<- function(fname){
  #' @description helper function for estimated_data_quantile to read in daily data for a single file
  #' @param fname is the file path to the dialy csv file
  
  station.ID <- substr(fname, start = 15, stop = 22)
  df<- read_csv(fname, col_types = cols()) 
  if("discharge" %in% names(df)){
    df <- df %>% 
      mutate(ID = station.ID) %>%
      mutate(quantile = ecdf(discharge)(discharge))
    
    df_estimated  <- df %>%
      filter(., grepl("e", discharge_cd)) %>%
      select(ID, Date, discharge, discharge_cd, quantile)
    return(df_estimated)   
    
  } #end if
  
}


