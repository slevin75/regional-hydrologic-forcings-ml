
get_sb_data <- function(sites, sb_var_ids, dldir, workdir, outdir, out_file_name) {
  data_at_sites <- tibble(COMID = sites$COMID)
  itemfails <- "1st"
  item <- sb_var_ids[[1]]
  name <- sb_var_ids[[2]]
  message(paste('starting SB ID', item, name))
  item_file_download(item, dest_dir = dldir, overwrite_file = TRUE)
  files <- list.files(path = dldir, pattern = NULL, full.names = TRUE)
  for (file in files)
  {
    last3 <- str_sub(file, -3, -1)
    last3 <- tolower(last3)
    if (last3 == "xml")
    {
      file.remove(file)
    }
    if (last3 == "zip")
    {
      unzip(file, exdir=workdir)
      file.remove(file)
    }
    if (last3 == "txt")
    {
      file.copy(file, workdir)
      file.remove(file)
    }
    filestomerge <- c(list.files(workdir, ".txt", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), 
                      list.files(workdir, ".csv", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), 
                      list.files(workdir, ".TXT", recursive = TRUE, full.names = TRUE, include.dirs = TRUE), 
                      list.files(workdir, ".CSV", recursive = TRUE, full.names = TRUE, include.dirs = TRUE))
    for (filem in filestomerge)
    {
      print(filem)
      success = "yes"
      tryCatch(tempfile <- read_delim(filem, delim = ",", show_col_types = FALSE), 
               error = function(e) {success <<- "no"})
      names(tempfile) <- toupper(names(tempfile))
      if (!"COMID" %in% colnames(tempfile))
      {
        success <- "no"
      }
      if (success == "no")
      {
        itemfails <- c(itemfails, filem)
        print("data table error")
      }
      if (success == "yes")
      {
        print("We're merging")
        try(tempfile <- subset(tempfile, select =-c(NODATA)), silent = TRUE)
        try(tempfile <- subset(tempfile, select =-c(CAT_NODATA)), silent = TRUE)
        try(tempfile <- subset(tempfile, select =-c(ACC_NODATA)), silent = TRUE)
        try(tempfile <- subset(tempfile, select =-c(TOT_NODATA)), silent = TRUE)
        data_at_sites <- merge(data_at_sites, tempfile, by = "COMID", all.x = TRUE)
      }
      file.remove(filem)
    }
    f <- list.files(workdir)
    unlink(file.path(workdir,f), recursive=TRUE)
  }
  
  if (length(itemfails) > 0)
  {
    failist <- as.data.frame(itemfails[-1])
    colnames(failist) <- c('filename')
    rownames(failist) <- NULL
  }
  
  itemfails <- as.data.frame(itemfails)
  
  filepath1 <- paste0(outdir, "/", out_file_name, item, ".csv")
  write_csv(data_at_sites, filepath1)
  filepath2 <- paste0(outdir, "/", out_file_name, item, "_FAILS.csv")
  write_csv(itemfails, filepath2)
  return_df = c(filepath1, filepath2)
  return(return_df)
}

get_sb_data_log_g2 <- function(sb_var_ids, file_out) {
  
  sb_log <- tibble(sb_id = character(), 
                   last_update = character())
  for (i in 1:nrow(sb_var_ids)) {
    sb_id <- as.character(sb_var_ids[i,1])
    last_update <- item_get_fields(sb_id, "provenance")$lastUpdated
    log_item <- tibble(sb_id, last_update)
    sb_log <- bind_rows(sb_log, log_item)
  }
  
  write_csv(sb_log, file_out)
  return(file_out)
  
}

prep_feature_vars <- function(sb_var_data, sites) {
  
  data <- sites %>%
    select(COMID, LAT, LON, npdes, fwwd, strg, devl, cndp)
  
  for (i in 1:length(sb_var_data)) {
    filepath <- sb_var_data[[i]][1]
    data_temp <- read_csv(filepath, show_col_types = FALSE) %>%
      group_by(COMID) %>%
      summarise_all(mean)
    data <- left_join(data, data_temp, by = "COMID")
  }
  
  data$npdes <- as.numeric(data$npdes)
  data$fwwd <- as.numeric(data$fwwd)
  data$strg <- as.numeric(data$strg)
  data$devl <- as.numeric(data$devl)
  data$cndp <- as.numeric(data$cndp)
  
  data <- data %>% 
    group_by(COMID) %>%
    summarise_all(mean)
  
  land_cover <- data %>%
    select(COMID, 
           contains("SOHL40"), contains("SOHL50"), contains("SOHL60"),
           contains("SOHL70"), contains("SOHL80"), contains("SOHL90"), 
           contains("SOHL00")) %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           year = if_else(str_sub(name, 9, 10) == "00", 2000, 
                          as.numeric(paste0("19", str_sub(name, 9, 10)))), 
           class = as.numeric(str_sub(name, 12))) %>%
    group_by(COMID, unit, year, class) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    mutate(new_class = case_when(class == 1 ~ "WATER", class == 2 ~ "DEVELOPED", 
                                 class == 3 ~ "FOREST", class == 4 ~ "FOREST", 
                                 class == 5 ~ "FOREST", class == 6 ~ "MINING", 
                                 class == 7 ~ "BARREN", class == 8 ~ "FOREST", 
                                 class == 9 ~ "FOREST", class == 10 ~ "FOREST", 
                                 class == 11 ~ "GRASSLAND", class == 12 ~ "SHRUBLAND", 
                                 class == 13 ~ "CROPLAND", class == 14 ~ "HAY/PASTURE", 
                                 class == 15 ~ "WETLAND", class == 16 ~ "WETLAND", 
                                 class == 17 ~ "ICE/SNOW")) %>%
    group_by(COMID, unit, year, new_class) %>%
    summarise(value = sum(value), .groups = "drop")
  
  comid_out_conus <- land_cover %>%
    group_by(COMID, unit, year) %>%
    summarise(lc_sum = sum(value), .groups = "drop") %>%
    filter(lc_sum < 90 | lc_sum > 101)
  comid_out_conus <- unique(comid_out_conus$COMID)
  data <- data %>%
    subset(!(COMID %in% comid_out_conus))
  
  land_cover <- land_cover %>%
    group_by(COMID, unit, new_class) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    mutate(label = paste0(unit, "_SOHL_", new_class)) %>%
    select(COMID, label, value) %>%
    pivot_wider(names_from = "label", values_from = "value")
  land_cover <- land_cover %>%
    subset(!(COMID %in% comid_out_conus))
  
  dams <- data %>%
    select(COMID, contains("NDAMS"), contains("STORAGE"), contains("MAJOR")) %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           feature = str_sub(name, 5, -5),
           year = str_sub(name, -4)) %>%
    filter(year <= 2010) %>%
    group_by(COMID, unit, feature, year) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    group_by(COMID, unit, feature) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    mutate(label = paste0(unit, "_", feature)) %>%
    select(COMID, label, value) %>%
    pivot_wider(names_from = label, values_from = value)
 
  weather <- data %>%
    select(COMID, contains("_PPT_"), contains("_TAV_")) %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           type = str_sub(name, 5, 7),
           month_name = str_sub(name, 9, 11)) %>%
    group_by(COMID, unit, type, month_name) %>%
    summarise(avg_monthly = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(month_num = case_when(month_name == "JAN" ~ 1, month_name == "FEB" ~ 2, 
                                 month_name == "MAR" ~ 3, month_name == "APR" ~ 4, 
                                 month_name == "MAY" ~ 5, month_name == "JUN" ~ 6, 
                                 month_name == "JUL" ~ 7, month_name == "AUG" ~ 8, 
                                 month_name == "SEP" ~ 9, month_name == "OCT" ~ 10, 
                                 month_name == "NOV" ~ 11, month_name == "DEC" ~ 12),
           label = paste0(unit, "_", type, "_", month_name)) %>%
    arrange(COMID, type, unit, month_num) %>%
    select(COMID, label, avg_monthly) %>%
    pivot_wider(names_from = "label", values_from = "avg_monthly")
  
  wildfire <- data %>%
    select(COMID, contains("_WILDFIRE_")) %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           year = str_sub(name, 14, 17)) %>%
    group_by(COMID, unit) %>%
    summarise(avg_annual = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(label = paste0(unit, "_AVG_WILDFIRE")) %>%
    arrange(COMID, label) %>%
    select(COMID, label, avg_annual) %>%
    pivot_wider(names_from = "label", values_from = "avg_annual")
  
  data <- data %>%
    select(COMID:TOT_PIPELINE.x, ACC_OLSON_K.x:TOT_RDX, 
           contains("TOTAL_ROAD_DENS"), SINUOSITY:ACC_PHYSIO_25, 
           CAT_PHYSIO_0:CAT_PHYSIO_25, TOT_PHYSIO_0:TOT_PHYSIO_25, 
           TOT_OLSON_K:TOT_WB5100_DEC, CAT_FSTFZ6190:TOT_LSTFZ6190, 
           CAT_MAXP6190:TOT_ET, ACC_PLAYA:TOT_ESTUARY) %>%
    left_join(land_cover) %>%
    left_join(dams) %>%
    left_join(weather) %>%
    left_join(wildfire)
  
  return(data)
}
