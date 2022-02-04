
get_nested_gages<-function(gagesii,nav_distance_km){
  gage_IDs <- as.character(gagesii$ID)
  COMIDs <- as.character(gagesii$COMID)
  drainage_areas<-data.frame(gage_IDs=gage_IDs,
                             drainArea=readNWISsite(siteNumbers=gage_IDs)$drain_area_va)
  
   #assigns a 1 if the row name gage is upstream of the column name gage
  upstream_df <- data.frame(matrix(ncol = length(gage_IDs), nrow = length(gage_IDs),data=0))
  colnames(upstream_df) <- rownames(upstream_df) <- gage_IDs

  for (i in 1:length(gage_IDs)){
    downstream_da<-drainage_areas$drainArea[i]
    flowline <- navigate_nldi(list(featureSource = "comid", 
                                       featureID = COMIDs[i]), 
                                  mode = "upstreamTributaries",
                                  distance_km=nav_distance_km)
                    

    upstream_list <- flowline$UT_flowlines$nhdplus_comid
    upstream_da<-drainage_areas$drainArea[which(COMIDs %in% upstream_list)]
    upstream_df[which(COMIDs %in% upstream_list), i] <- round(upstream_da / downstream_da,3)
    message(gage_IDs[i])
  }
  
  return(upstream_df)
}
