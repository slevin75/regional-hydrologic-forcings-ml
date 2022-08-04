
get_nested_gages<-function(sites_and_comids, drainage_areas, nav_distance_km) {
  
  #' 
  #' @description This function determines the area of overlap between each
  #' gage in the gagesii data.
  #' 
  #'  @param gagesii gagesii database of streamgages
  #'  @param nav_distance_km distance to search upstream to look for nested basins
  #'  @param screened_site_list vector of streamgage ids that has been screened 
  #'  
  #'  @return Returns a matrix with proportion of overlapping area. Column name 
  #'  gage is downstream of the row name gage.
  
  data <- sites_and_comids %>%
    select(GAGES_ID, COMID) %>%
    rename(site_no = GAGES_ID) %>%
    left_join(drainage_areas)

  #column names are the downstream gages, rows with non-zero values are upstream gages
  upstream_df <- data.frame(matrix(ncol = nrow(data), nrow = nrow(data), data=0))
  colnames(upstream_df) <- rownames(upstream_df) <- as.character(data$site_no)

  for (i in 1:nrow(data)){
    downstream_da<-data$drainArea[i]
    flowline <- navigate_nldi(list(featureSource = "comid", 
                                       featureID = data$COMID[i]), 
                                  mode = "upstreamTributaries",
                                  distance_km=nav_distance_km)
    ##list of all upstream COMIDs
    upstream_list <- flowline$UT_flowlines$nhdplus_comid
    
    ##gages located on any upstream COMID (includes the COMID of the downstream gage)
    upstream_gages <- data$site_no[which(data$COMID %in% upstream_list)]
    
    ##if there are more upstream gages than there are unique COMIDs, then 
    ##there are more than one gage on a COMID.  If this happens, remove any
    ##gage from the upstream gages list that has a larger drainage area than the
    ##downstream_da.
    # if (length(upstream_gages) >
    #   length(unique(COMIDs[which(COMIDs %in% upstream_list)]))){
    #     
    #     upstream_das<-drainage_areas[which(drainage_areas$gage_IDs %in% upstream_gages),] 
    #     keep_gages<-upstream_das$gage_IDs[which(upstream_das$drainArea <= downstream_da)]
    #     upstream_gages<-upstream_gages[which(upstream_gages %in% keep_gages)]
    #   }
    upstream_das <- data[which(data$site_no %in% upstream_gages), 3]
    upstream_df[which(row.names(upstream_df) %in% upstream_gages), i] <- round(upstream_das$drainArea / downstream_da,3)
    message(data$site_no[i])
  }


  
  return(upstream_df)
}


add_nested_group_id <- function(nested_gages, drainage_area, nested_threshold){
  #' 
  #' @description this function assigns a unique group numbers to each un-nested 
  #' gage and groups of gages that are nested with a minimum overlap threshold.
  #' 
  #' @param nested_gages is a matrix of the percentage of overlap between each pair
  #' of gages. Column name gages are the downstream gage and row name is the upstream gage
  #' @param drainage_area data frame containing the drainage area of each streamgage
  #' @param nested_threshold is the minimum percentage of overlap needed to be considered
  #' nested
  #' 
  #' @return Returns a data frame with a unique group id for each pair of nested gages.  Unnested
  #' gages get their own unique nested_group_id.
  
  ##replace any number equal or greater than the nested_threshold to 1
  nested_gages<- as.data.frame(ifelse(nested_gages >= nested_threshold, 1, 0))
    
  ##get column sums, add drainage area and sort by descending DA so that
  ##we get the largest ones first and will work our way upstream
  
  nested_count<- data.frame(site_no = rownames(nested_gages), n = colSums(nested_gages)) %>%
    inner_join(x=., y=drainage_area, by="site_no") %>%
    arrange(desc(drainage_area))%>%
    add_column(nested_group_id=NA)
  
  ##starting with the gage with the highest number of overlaps, to ensure we always do the largest, downstream
  #gage within a group first, assign a unique number to each gage/group
  
  gp<-1
  for (i in 1: nrow(nested_count)){
    #check to make sure a group has not already been assigned
    if (is.na(nested_count$nested_group_id[i])){
    gage<-nested_count$site_no[i]
    nested_group<-rownames(nested_gages)[which(nested_gages[, gage]==1)]
    nested_count$nested_group_id[which(nested_count$site_no %in% nested_group)] <- gp
    gp <- gp + 1
    }#end if
  } #end for
  
  df_out<- data.frame(ID = nested_count$site_no, nested_group_id = nested_count$nested_group_id)
}




