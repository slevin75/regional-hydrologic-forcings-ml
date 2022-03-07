#Function to compute the clusters for seasonal metrics
# written to be branched over the metric
#metric_mat - p1_FDC_metrics_season. rows are gauges, columns are seasonal metrics
#metric - the name of the metric without the _s season at the end
#dist_method - the distance computation for dist()
seasonal_metric_cluster <- function(metric_mat, metric, 
                                    dist_method = 'euclidean'
                                    ){
  #Select all of the seasonal columns for this metric
  metric_mat <- metric_mat[, c(1,grep(x = colnames(metric_mat), 
                                      pattern = paste0(metric,'_')))]
  
  #Scaling of metrics should not be necessary because the metrics are on [0,1]
  
  #Compute the distance matrix between all sites
  dists <- dist(metric_mat[,-1], method = dist_method)
  
  #Compute clusters using different methods
  clust_methods_hclust <- c( "average", "single", "complete", "ward.D2")
  names(clust_methods_hclust) <- clust_methods_hclust
  
  #compute clusters using hclust function instead of agnes becuase it's faster
  clusts <- purrr::map(.x = clust_methods_hclust,
                      .f = hclust, 
                      d = dists, members = NULL)
  #add metric name
  #add agglomeration coefficient
  for (i in 1:length(clusts)){
    clusts[[i]]$ac = round(coef.hclust(clusts[[i]]), 3)
    clusts[[i]]$metric = metric
  }
  
  return(clusts)
}

#Function to extract the best cluster method for each metric
#clusts is the list output from seasonal_metric_cluster
select_cluster_method <- function(clusts){
  #data.frame of the metric, method, and ac value
  df <- matrix(nrow = length(clusts), ncol = 3, data = '')
  for (i in 1:nrow(df)){
    df[i,] <- as.character(clusts[[i]][c('metric', 'method', 'ac')])
  }
  df <- as.data.frame(df)
  colnames(df) <- c('metric', 'method', 'ac')
  df$ac <- as.numeric(df$ac)
  
  #df of the clustering method with the max ac value for each metric
  df_max <- df[1:length(unique(df$metric)),]
  df_max[,] <- NA
  for (i in 1:nrow(df_max)){
    a <- df[df$metric == unique(df$metric)[i],]
    df_max[i,] <- a[a$ac == max(a$ac),]
  }
  
  return(df_max)
}

#Function to compute cluster diagnostics
#kmin, kmax - min and max number of clusters to use
#alpha - significance level
#boot - number of bootstrap replicates
#index - the NbClust index to compute. 'all' computes all except those with long compute times.
compute_cluster_diagnostics <- function(clusts, metric_mat,
                                        kmin, kmax, alpha, boot = 50,
                                        index = 'all',
                                        dist_method = 'euclidean',
                                        clust_method = 'ward.D2'
                                        ){
  clusts <- clusts[[clust_method]]
  
  #Select all of the seasonal columns for this metric
  metric_mat <- metric_mat[, grep(x = colnames(metric_mat), 
                                  pattern = paste0(clusts$metric,'_'))]
  
  #Compute NbClust cluster diagnostics
  nbclust_metrics <- NbClust::NbClust(data = metric_mat, diss = NULL, 
                                      distance = dist_method, 
                                      min.nc = kmin, max.nc = kmax, 
                                      method = clust_method, index = index, 
                                      alphaBeale = alpha)
  
  #Compute gap statistic
  gap_stat <- cluster::clusGap(as.matrix(metric_mat), FUNcluster = hcut,
                               K.max = kmax, B = boot, d.power = 2,
                               hc_func = 'hclust', hc_method = clust_method,
                               hc_metric = dist_method, verbose = FALSE)
  
  return(list(flow_metric = clusts$metric, 
              #dropping the suggested best cluster partition to save space
              nbclust_metrics = nbclust_metrics[-4], 
              gap_stat = gap_stat))
}

#Function to make cluster diagnostic panel plot
plot_cluster_diagnostics <- function(clusts, metric_mat, nbclust_metrics,
                                     dist_method = 'euclidean',
                                     clust_method = 'ward.D2',
                                     dir_out){
  clusts <- list(clusts[[clust_method]])
  
  fileout <- vector('character', length = length(clusts))
  
  for(cl in 1:length(clusts)){
    fileout[cl] <- file.path(dir_out, 
                             paste0(clusts[[cl]]$metric, '_', 
                                    clust_method, '_diagnostics.png'))
    
    #Select all of the seasonal columns for this metric
    metric_mat <- metric_mat[, c(1,grep(x = colnames(metric_mat), pattern = paste0(clusts[[cl]]$metric,'_')))]
    
    #dendrogram
    p1 <- ggplot(dendextend::as.ggdend(as.dendrogram(clusts[[cl]]))) +
      labs(title = paste0("Dendrogram of ", clusts[[cl]]$metric, " with\n", 
                          clust_method, ' Clustering. AC = ', clusts[[cl]]$ac))
    
    #WSS
    p2 <- fviz_nbclust(x = as.matrix(metric_mat[,-1]), FUNcluster = hcut, method = 'wss', 
                       k.max = 20, hc_func = 'hclust', hc_method = clust_method, 
                       hc_metric = dist_method) +
      labs(title = paste0('WSS for Metric: ', clusts[[cl]]$metric,
                          ',\nCluster Method: ', clust_method))
    
    #histogram of optimal number of clusters
    p3 <- ggplot(data = as.data.frame(t(nbclust_metrics$nbclust_metrics$Best.nc)), 
                 aes(Number_clusters)) + 
      geom_histogram(bins = 20, binwidth = 0.5) +
      labs(title = paste0('Suggested Optimal Number of Clusters from 26 Metrics\nMetric: ', 
                          clusts[[cl]]$metric, ', Cluster Method: ', clust_method)) +
      xlab("Suggested Optimal Number of Clusters") +
      ylab("Count")
    
    #gap statistic
    p4 <- fviz_gap_stat(nbclust_metrics$gap_stat, maxSE = list(method = 'globalmax')) +
      labs(title = paste0('Gap Statistic for Metric: ', clusts[[cl]]$metric,
                          ',\nCluster Method: ', clust_method))
    
    save_plot(filename = fileout[cl], base_height = 8, base_width = 8, 
              plot = plot_grid(p1, p2, p3, p4, nrow = 2, ncol = 2))
  }
  
  return(fileout)
}

#Function to add the cluster numbers to gages
add_cluster_to_gages <- function(gages, screened_sites, clusts, best_clust,
                                 min_clusts, max_clusts, by_clusts){
  #Select the gages that have clusters computed
  gages_clusts <- gages[gages$ID %in% screened_sites, "ID"]
  
  #add columns with cluster numbers
  clust_nums <- seq(min_clusts, max_clusts, by_clusts)
  for(i in 1:length(clusts)){
    #find only the best cluster methods
    if(clusts[[i]]$method == best_clust$method[best_clust$metric == clusts[[i]]$metric]){
      #get clusters from the best cluster method
      for (k in clust_nums){
        gages_clusts <- cbind(gages_clusts, cutree(clusts[[i]], k = k))
      }
      colnames(gages_clusts)[(ncol(gages_clusts) - length(clust_nums) 
                              + 1):ncol(gages_clusts)] <- paste0(clusts[[i]]$metric, 
                                                                 '_k', clust_nums)
      
    }
  }
  
  return(gages_clusts)
}

#Function to plot the average seasonal distribution for all sites, or
#plot the average seasonal distribution for sites in the cluster
plot_seasonal_barplot <- function(metric_mat, metric, 
                                  season_months,
                                  by_cluster = FALSE,
                                  cluster_table = NULL,
                                  panel_plot = NULL,
                                  dir_out)
  {
  if(by_cluster & is.null(cluster_table)){
    stop('cluster_table must be supplied to plot by clusters.')
  }
  
  #Select all of the column names used for this metric
  metric_mat <- metric_mat[, c(1,grep(x = colnames(metric_mat), 
                                      pattern = paste0(metric,'_')))]
  
  #get the month labels
  month_chars <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
  season_months <- stringi::stri_join(month_chars[season_months][c(1,4,7,10)],
                                      month_chars[season_months][c(2,5,8,11)],
                                      month_chars[season_months][c(3,6,9,12)])
  
  if (by_cluster){
    #Select all of the column names used for this metric
    cluster_table <- cluster_table[, c(1,grep(x = colnames(cluster_table), 
                                              pattern = paste0(metric,'_')))]
    
    #Get the total number of clusters in all of the columns. 
    #There will be 2 elements after splitting
    k <- unlist(strsplit(colnames(cluster_table[,-1]), 
                         split = '_k'))[seq(2,ncol(cluster_table[,-1])*2,2)] %>%
      as.numeric()
    
    #get all directories to create based on the number of clusters
    dir_out <- file.path(dir_out, paste0('cluster', k))
    
    if(panel_plot){
      fileout <- vector('character', length = length(k))
    }else{
      fileout <- vector('character', length = sum(k))
    }
    
    for (i in 1:length(k)){
      dir.create(dir_out[i], showWarnings = FALSE)
      if(panel_plot){
        #create matrix of colmeans as rows to plot with facet_wrap
        metric_mat_c <- as.data.frame(matrix(nrow = k[i]*4, ncol = 6))
        num_sites <- vector('numeric', length = k[i])
        for (cl in 1:k[i]){
          full_mat <- filter(metric_mat, 
                             site_num %in% cluster_table$ID[cluster_table[,i+1] == cl]) %>%
            select(-site_num)
          
          num_sites[cl] <- nrow(full_mat)
          
          metric_mat_c[(1+(cl-1)*4):(4*cl), ] <- data.frame(data = full_mat %>% colMeans(), 
                                           season = season_months, 
                                           cluster = paste0('Cluster ', cl, ', ', num_sites[cl], ' sites'), 
                                           ymin = as.numeric(apply(X = full_mat, MARGIN = 2, 
                                                                   FUN = quantile, probs = 0.05)), 
                                           ymax = as.numeric(apply(X = full_mat, MARGIN = 2, 
                                                                   FUN = quantile, probs = 0.95)),
                                           label_order = cl)
        }
        colnames(metric_mat_c) <- c('data', 'season', 'cluster', 'ymin', 'ymax', 'label_order')
        
        #file index
        fileout[i] <- file.path(dir_out[i], paste0('SeasonalBarplot_', 
                             colnames(cluster_table)[i+1], '.png'))
        
        plt <- ggplot(metric_mat_c) + 
            ylim(0,1) +
            xlab('Season Months') + 
            ylab('Seasonal Fraction') +
            ggtitle(paste0('Metric ', metric)) +
            geom_col(aes(season, data)) + 
            scale_x_discrete(limits=season_months) + 
            geom_errorbar(aes(x = season, 
                              ymin = ymin, 
                              ymax = ymax),
                          width = 0.4) +
          facet_wrap(~reorder(cluster, label_order))
        ggsave(filename = fileout[i], plot = plt, device = 'png')
      }else{
        for (cl in 1:k[i]){
          #metric matrix for gages in cluster
          metric_mat_c <- filter(metric_mat, 
                                 site_num %in% cluster_table$ID[cluster_table[,i+1] == cl]) %>%
            select(-site_num)
          
          #file index
          ind_f <- ifelse(test = i > 1, cl + cumsum(k)[i-1], cl)
          
          fileout[ind_f] <- file.path(dir_out[i], 
                                   paste0('SeasonalBarplot_', colnames(cluster_table)[i+1], 
                                   '_c', cl, '.png'))
          
          png(filename = fileout[ind_f], width = 5, height = 5, units = 'in', res = 200)
          barplot(height = colMeans(metric_mat_c), width = 1, 
                  names.arg = season_months, 
                  xlim = c(0,4), ylim = c(0,1),
                  space = 0, main = paste0('Metric ', metric, ', k = ', k[i],
                                           ',\nCluster ', cl, ', ', nrow(metric_mat_c), ' sites'), 
                  xlab = 'Season Months', ylab = 'Seasonal Fraction')
          #add error bars as 5th - 95th percentiles
          arrows(x0 = c(0.5,1.5,2.5,3.5), 
                 y0 = as.numeric(apply(X = metric_mat_c, MARGIN = 2, FUN = quantile, 
                                       probs = 0.05)),
                 x1 = c(0.5,1.5,2.5,3.5), 
                 y1 = as.numeric(apply(X = metric_mat_c, MARGIN = 2, FUN = quantile, 
                                       probs = 0.95)),
                 angle = 90, length = 0.1, code = 3)
          dev.off()
        }
      }
    }
  }else{
    fileout <- file.path(dir_out, paste0('SeasonalBarplot_', metric, '.png'))
    png(filename = fileout, width = 5, height = 5, units = 'in', res = 200)
    barplot(height = colMeans(metric_mat[,-1]), width = 1, 
            names.arg = season_months, 
            xlim = c(0,4), ylim = c(0,1),
            space = 0, main = paste0('Metric ', metric, ', ', nrow(metric_mat), ' sites'),
            xlab = 'Season Months', ylab = 'Seasonal Fraction')
    #add error bars as 5th - 95th percentiles
    arrows(x0 = c(0.5,1.5,2.5,3.5), 
           y0 = as.numeric(apply(X = metric_mat[,-1], MARGIN = 2, FUN = quantile, 
                                 probs = 0.05)),
           x1 = c(0.5,1.5,2.5,3.5), 
           y1 = as.numeric(apply(X = metric_mat[,-1], MARGIN = 2, FUN = quantile, 
                                 probs = 0.95)),
           angle = 90, length = 0.1, code = 3)
    dev.off()
  }
  
  return(fileout)
}


#Function to plot the cuts in trees from kmin to kmax clusters
#clusts is the output from hclust
plot_cuttree <- function(clusts, kmin, kmax, seq_by, dir_out){
  for (k in seq(kmin, kmax, seq_by)){
    fileout <- file.path(dir_out, paste0(clusts$metric, '_', clusts$method, '_', k,'.png'))
    png(fileout, res = 300, units = 'in', width = 7, height = 5)
    plot(clusts, cex = 0.6, hang = -1, 
         main = paste0("Dendrogram of ", clusts$metric, " with ", clusts$method,
                       ' Clustering,\nCut at ', k, ' Clusters'))
    rect.hclust(clusts, k = k, border = rainbow(45, alpha = 1))
    dev.off()
  }
  
  return(fileout)
}


#Function to make a map of the resulting clusters
#can add bounding boxes for clusters as argument
plot_cluster_map <- function(gages, cluster_table, screened_sites, dir_out){
  ncol_gages <- ncol(gages)
  
  #get only sites with metrics computed
  gages <- gages[which(gages$ID %in% screened_sites),]
  
  #Add the cluster_table to gages by ID join
  gages <- cbind(gages, cluster_table)
  
  fileout <- vector('character', length = ncol(cluster_table)-1)
  
  for(i in 1:(ncol(cluster_table)-1)){
    fileout[i] <- file.path(dir_out, paste0(colnames(cluster_table)[i+1], '_map.png'))
    
    #number of clusters fro the column name
    k <- as.numeric(str_split(string = str_split(string = colnames(cluster_table)[i+1], 
                                      pattern = '_')[[1]] %>% last(), 
                   pattern = 'k')[[1]] %>% last())
    
    png(filename = fileout[i], width = 8, height = 5, units = 'in', res = 200)
    #plot gage locations, colored by their cluster
    plot(gages[ncol_gages+i], pch = 16, cex = 0.4, axes = TRUE,
         main = colnames(cluster_table)[i+1], 
         breaks = seq(0,k,1),
         pal = scico(n = k, palette = 'batlow'),
         key.width = 0.15, key.length = 0.75, key.pos = 1)
    #add legend
    dev.off()
  }
  
  return(fileout)
}
