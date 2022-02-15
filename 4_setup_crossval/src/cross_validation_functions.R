
get_nested_gages<-function(gagesii,nav_distance_km){
  gage_IDs <- as.character(gagesii$ID)
  COMIDs <- as.character(gagesii$COMID)
  drainage_areas<-data.frame(gage_IDs=gage_IDs,
                             drainArea=readNWISsite(siteNumbers=gage_IDs)$drain_area_va)
  ##remove any gages that do not have a drainage area in NWIS
  rm_gages<-which(is.na(drainage_areas$drainArea))
  gage_IDs<-gage_IDs[-rm_gages]
  COMIDs<-COMIDs[-rm_gages]
  drainage_areas<-drainage_areas[-rm_gages,]
  
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
