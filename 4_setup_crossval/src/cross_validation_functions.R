
get_nested_gages<-function(gagesii,nav_distance_km, screened_site_list){
  gage_IDs <- as.character(gagesii$ID[gagesii$ID %in% screened_site_list])
  COMIDs <- as.character(gagesii$COMID[gagesii$ID %in% screened_site_list])
  drainage_areas<-data.frame(gage_IDs=gage_IDs,
                             drainArea=readNWISsite(siteNumbers=gage_IDs)$drain_area_va)

   #column names are the downstream gages, rows with non-zero values are upstream gages
  upstream_df <- data.frame(matrix(ncol = length(gage_IDs), nrow = length(gage_IDs),data=0))
  colnames(upstream_df) <- rownames(upstream_df) <- gage_IDs

  for (i in 1:length(gage_IDs)){
    downstream_da<-drainage_areas$drainArea[i]
    flowline <- navigate_nldi(list(featureSource = "comid", 
                                       featureID = COMIDs[i]), 
                                  mode = "upstreamTributaries",
                                  distance_km=nav_distance_km)
    ##list of all upstream COMIDs
    upstream_list <- flowline$UT_flowlines$nhdplus_comid
    
    ##gages located on any upstream COMID (includes the COMID of the downstream gage)
    upstream_gages<-gage_IDs[which(COMIDs %in% upstream_list)]
    
    ##if there are more upstream gages than there are unique COMIDs, then 
    ##there are more than one gage on a COMID.  If this happens, remove any
    ##gage from the upstream gages list that has a larger drainage area than the
    ##downstream_da.
    if (length(upstream_gages) >
      length(unique(COMIDs[which(COMIDs %in% upstream_list)]))){
        
        upstream_das<-drainage_areas[which(drainage_areas$gage_IDs %in% upstream_gages),] 
        keep_gages<-upstream_das$gage_IDs[which(upstream_das$drainArea <= downstream_da)]
        upstream_gages<-upstream_gages[which(upstream_gages %in% keep_gages)]
      }
    upstream_das<-drainage_areas[which(gage_IDs %in% upstream_gages),]
    upstream_df[which(row.names(upstream_df) %in% upstream_gages), i] <- round(upstream_das$drainArea / downstream_da,3)
    message(gage_IDs[i])
  }


  
  return(upstream_df)
}


add_nested_group_id <- function(nested_gages, drainage_area, nested_threshold){
  
  ##this function will take the matrix of nested gages, and output a dataframe
  ##assigning unique group numbers to each un-nested gage and groups of gages 
  ##that are nested with a minimum overlap equal to the nested_threshold.
  
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




