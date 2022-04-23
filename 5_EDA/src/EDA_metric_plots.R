

##function to make maps and violin plots for all metrics by cluster
make_EDA_metric_plots <- function(metric,k,cluster_table, metrics_table, gages,out_dir){
  

  
  ##get the high and low quantile cluster columns for the specified number of clusters
  ##from the cluster_table.  This will throw an error if cluster table does not 
  ##have the specified number of clusters.
  high_quant_cluster_col<-intersect(grep(paste0("k",k), names(cluster_table)),
                                    grep("0.9", names(cluster_table)))
  low_quant_cluster_col<-intersect(grep(paste0("k",k), names(cluster_table)),
                                   grep("0.5", names(cluster_table)))
  
  
  ##find the appropriate cluster column for the current metric
  if (metric %in% c('ma','ml17', 'ml18')){
    cluster_col <- low_quant_cluster_col
  } else if (!grepl("_q", metric)){
    ##other metrics from HIT
    cluster_col<-high_quant_cluster_col
  }else{
    ##get quantile from FDC metric name
    metric_quantile <- as.numeric(str_split(metric,pattern="_q")[[1]][2])
    cluster_col<-ifelse(metric_quantile< 0.75,low_quant_cluster_col,high_quant_cluster_col)
  } ##end if
  
  
  ##get the column index of the metric
  metric_col<- which(names(metrics_table)==metric)
  ##renaming the first column to match the cluster and gages ID field
  names(metrics_table)[1] <- "ID"
  
  
  ##create data frame for plots
  df_plot<- 
    inner_join(gages[,c("ID","LAT","LON")],cluster_table[,c(1,cluster_col)]) %>%
    left_join(.,metrics_table[c(1,metric_col)])
  


  ##loop through clusters and make a map and violin plot for each cluster
  states <- map_data("state")
  fileout_all <- vector()
  for (cl in 1:k){
    
    df<-df_plot %>% 
      filter(.data[[colnames(df_plot)[5]]]==cl)
    
    p_violin<- ggplot(df, aes(x=1,y = .data[[colnames(df)[6]]]))+
      geom_violin(draw_quantiles = c(0.5))+
      geom_jitter(height=0,color = "steelblue",alpha=0.5,width=0.2)+
      xlab(paste("cluster" , cl))
    
    p_map<- ggplot(states, aes(x=long, y=lat, group=group)) +
      geom_polygon(fill="white", colour="gray") +
      geom_sf(data = df, inherit.aes = FALSE, 
              aes(color = .data[[colnames(df)[6]]]), 
              size = 0.75)+
      scale_color_gradientn(colors=rainbow(5))+
      theme(legend.position="bottom",
            legend.key.size=unit(.5,'cm'))+
      xlab('Longitude') + 
      ylab('Latitude') +
      ggtitle(paste("cluster",cl))

     fname<- paste0(metric,"_k",k,"_cluster",cl,".png")
     fileout<-file.path(out_dir, fname)  
     save_plot(filename = fileout, base_height = 5, base_width = 7, 
               plot = plot_grid(p_map, p_violin,scale=c(1,.6)))
    
     fileout_all<-append(fileout_all,fileout)
  }
  return(fileout_all)

}#end function
  
