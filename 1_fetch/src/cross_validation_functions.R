
get_nested_gages<-function(gagesii,nav_distance_km){
  gage_IDs <- as.character(gagesii$ID)
  COMIDs <- as.character(gagesii$COMID)
  
   #assigns a 1 if the row name gage is upstream of the column name gage
  upstream_df <- data.frame(matrix(ncol = length(gage_IDs), nrow = length(gage_IDs)))
  colnames(upstream_df) <- gage_IDs; rownames(upstream_df) <- gage_IDs
  upstream_df[,]<-0
  for (i in 1:length(gage_IDs)){
    flowline <- navigate_nldi(list(featureSource = "comid", 
                                       featureID = COMIDs[i]), 
                                  mode = "upstreamTributaries",
                                  distance_km=nav_distance_km)
    upstream_list <- flowline$UT_flowlines$nhdplus_comid
    upstream_df[which(COMIDs %in% upstream_list), i] <- 1
    message(gage_IDs[i])
  }
  
  return(upstream_df)
}
