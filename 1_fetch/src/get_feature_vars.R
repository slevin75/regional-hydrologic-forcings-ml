finalize_vars_conus <- function(prep1_conus, prep2_conus, drop_attr) {
  
  #'@description combines outputs from prep1 and prep2 conus functions to provide
  #'full set of feature variables for all comids in conus
  #'
  #'@param prep1_conus filepaths for all variables except
  #'land cover variables (from prep1_vars_conus)
  #'@param prep2_conus filepath for land cover variables (from prep2_vars_conus)
  #'@param drop_attr character vector of attributes to drop from
  #' prep1_conus tables before joining
  #'
  #'@return data frame including all conus-wide comids and their feature variables
  
  data <- read_csv(prep2_conus, show_col_types = FALSE)
  for (i in prep1_conus) {
    data_temp <- read_csv(i, show_col_types = FALSE) %>%
      select(-all_of(drop_attr))
    data <- left_join(data, data_temp, by = "COMID")
  }
  
  return(data)
}

get_nhd_conus_gdb <- function(outdir, seven_zip) {
  
  #'@description uses nhdPlusTools package to download nhd geodatabase
  #'
  #'@param outdir filepath for final data downloads
  #'@param seven_zip filepath (full, not relative) to 7zip executable
  #'
  #'@return unzipped nhd geodatabase
  
  download_nhdplusv2(
    outdir,
    url = paste0("https://edap-ow-data-commons.s3.amazonaws.com/NHDPlusV21/",
                 "Data/NationalData/NHDPlusV21_NationalData_Seamless", 
                 "_Geodatabase_Lower48_07.7z"),
    progress = TRUE
  )
  
  system(paste0(seven_zip, " -o", outdir, " x ", outdir, 
                "NHDPlusV21_NationalData_Seamless_Geodatabase_Lower48_07.7z"))
  
  filepath <- paste0(outdir, "NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb")
  return(filepath)
}


get_sb_data <- function(sites, sb_var_ids, dldir, workdir, outdir, out_file_label) {
  
  #'@description recursively downloads and saves feature variable values from ScienceBase
  #'
  #'@param sites data frame with a COMID column including all reaches of interest
  #'@param sb_var_ids data frame with ScienceBase identifier column
  #'@param dldir filepath for downloads
  #'@param workdir filepath for unzipping, joining, etc.
  #'@param outdir filepath for final data downloads
  #'@param out_file_label name to append to the file name containing the downloaded data
  #'
  #'@return series of .csv files with ScienceBase feature variable values
  #'joined to a list of COMIDs of interest
  
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
  
  filepath1 <- paste0(outdir, "/sb_", item, "_", out_file_label, ".csv")
  write_csv(data_at_sites, filepath1)
  filepath2 <- paste0(outdir, "/sb_", item, "_", out_file_label, "_FAILS.csv")
  write_csv(itemfails, filepath2)
  return_df = c(filepath1, filepath2)
  return(return_df)
}


get_sb_data_log <- function(sb_var_ids, file_out) {
  
  #'@description generates a log file to track changes to ScienceBase data
  #'
  #'@param sb_var_ids data frame with ScienceBase identifier column
  #'@param file_out filepath for log file
  #'
  #'@return log file documenting dates of changes to ScienceBase feature variable values
  
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


prep_comid_conus <- function(nhd_conus_gdb, attrib_to_keep, ftype_to_keep, outdir) {
  
  #'@description Selects comids of interest for conus-wide predictions
  #'
  #'@param nhd_conus_gdb target from p1_nhd_conus_gdb (raw unzipped nhd geodatabase)
  #'@param attrib_to_keep character list of attributes to keep (columns in nhd_conus_gdb)
  #'@param ftype_to_keep character list of flowline types to retain (of the following:
  #'ArtificialPath, StreamRiver, Connector, CanalDitch, Coastline, Pipeline)
  #'@param outdir filepath to save final csv
  #'
  #'@return non-tidal conus-wide comids of specified ftype(s)
  #'with specified attributes retained from geodatabase (class 'sf')
  
  nhd_full <- st_read(nhd_conus_gdb, layer = 'NHDFlowline_Network')
  comid_conus <- nhd_full %>%
    select(all_of(attrib_to_keep)) %>%
    filter(FTYPE %in% all_of(ftype_to_keep)) %>%
    filter(Tidal == 0)
  
  return(comid_conus)
}


prep_feature_vars_g2 <- function(sb_var_data, sites_all, sites_screened, 
                                 combine_gages, years_by_site, retain_vars) {
  
  #'@description joins all data downloaded from ScienceBase with sites of interest
  #'
  #'@param sb_var_data target generated from the 'get_sb_data()' function mapped over the 
  #''p1_sb_var_ids' targets; the 'p1_sb_var_ids' target reads in a .csv of ScienceBase
  #'identifiers and names, and the 'get_sb_data()' function generates a list of .csv file
  #'locations containing the feature variable data from each ScienceBase identifier
  #'joined with the COMIDs contained in the 'sites' parameter
  #'@param sites_all data frame with a COMID column including all reaches of interest
  #'@param sites_screened list of sites that passed screening functions
  #'@param retain_vars character strings of additional column headers to keep in the 
  #'sites data frame
  #'
  #'@return data frame with COMID column appended by all feature variables of interest; 
  #'time-varying features converted to long-term averages where applicable
  
  # Identify all combinations of sites and their complete years (for weighted avgs)
  complete_years <- years_by_site %>% 
    group_by(year_val, site_no) %>%
    summarise(.groups = "drop") %>%
    arrange(site_no) %>%
    mutate(site_comid_match = if_else(grepl("_", site_no), 
                                      sub("_.*", "", site_no), site_no))
  
  # Identify screened sites and split the combined sites
  screened_site_list <- c()
  for (i in sites_screened) {
    if (grepl("_", i)) {
      site_1 <- str_split_fixed(i, pattern = "_", n = 2)[1]
      site_2 <- str_split_fixed(i, pattern = "_", n = 2)[2]
      sites <- c(site_1, site_2)
    } else {
      sites <- i
    }
    screened_site_list <- c(screened_site_list, sites)
  }
  
  # Only keep retained feature variables and screened gages 
  data <- sites_all %>%
    select(COMID, all_of(retain_vars)) %>%
    filter(ID %in% screened_site_list) %>%
    mutate(across(where(is.character) & !starts_with('ID'), as.numeric))
  
  if("ID" %in% retain_vars) {
    data <- rename(data, GAGES_ID = ID)
  }
  
  #join all sciencebase data to single data frame with comids
  for (i in 1:length(sb_var_data)) {
    filepath <- sb_var_data[[i]][1]
    data_temp <- read_csv(filepath, show_col_types = FALSE) %>%
      group_by(COMID) %>%
      summarise_all(mean)
    data <- left_join(data, data_temp, by = "COMID")
  }
  
  #handles any duplicated COMIDs
  if(!("ID" %in% retain_vars)) {
    data <- data %>%
      group_by(COMID) %>%
      summarise_all(mean) 
  }
  
  #reclassify land use data for consistency across time periods
  land_cover <- data %>%
    select(COMID, 
           contains("SOHL40"), contains("SOHL50"), contains("SOHL60"),
           contains("SOHL70"), contains("SOHL80"), contains("SOHL90"), 
           contains("SOHL00")) %>%
    distinct() %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           year = if_else(str_sub(name, 9, 10) == "00", 2000, 
                          as.numeric(paste0("19", str_sub(name, 9, 10)))), 
           class = as.numeric(str_sub(name, 12))) %>%
    mutate(new_class = case_when(class == 1 ~ "WATER", class == 2 ~ "DEVELOPED", 
                                 class == 3 ~ "FOREST", class == 4 ~ "FOREST", 
                                 class == 5 ~ "FOREST", class == 6 ~ "MINING", 
                                 class == 7 ~ "BARREN", class == 8 ~ "FOREST", 
                                 class == 9 ~ "FOREST", class == 10 ~ "FOREST", 
                                 class == 11 ~ "GRASSLAND", class == 12 ~ "SHRUBLAND", 
                                 class == 13 ~ "CROPLAND", class == 14 ~ "HAYPASTURE", 
                                 class == 15 ~ "WETLAND", class == 16 ~ "WETLAND", 
                                 class == 17 ~ "ICESNOW")) %>%
    group_by(COMID, unit, year, new_class) %>%
    summarise(value = sum(value)) %>%
    mutate(lc_sum = sum(value)) %>%
    ungroup()
  
  #identify comids with substantial area outside of CONUS and remove
  comid_out_conus <- land_cover %>%
    filter(lc_sum < 90 | lc_sum > 101)
  comid_out_conus <- unique(comid_out_conus$COMID)
  data <- data %>%
    subset(!(COMID %in% comid_out_conus))
  
  #convert decadal land use to long-term average land use
  land_cover_longterm_avg <- land_cover %>%
    subset(!(COMID %in% comid_out_conus)) %>%
    group_by(COMID, unit, year, new_class) %>%
    mutate(lc_adj = (value / lc_sum) * 100) %>%
    ungroup() %>%
    select(COMID, unit, new_class, lc_adj) %>%
    group_by(COMID, unit, new_class) %>%
    summarise(value = mean(lc_adj), .groups = "drop") %>%
    mutate(label = paste0(unit, "_SOHL_", new_class, "_longterm_avg")) %>%
    select(COMID, label, value) %>%
    pivot_wider(names_from = "label", values_from = "value")
  
  #convert decadal land use to weighted average land use by year
  land_cover_weighted_avg <- data %>%
    select(COMID, site_comid_match = GAGES_ID) %>%
    left_join(complete_years, by = "site_comid_match") %>%
    select(COMID, year_w_data = year_val) %>%
    drop_na() %>%
    left_join(land_cover, by = "COMID") %>%
    select(-lc_sum) %>%
    mutate(lc_by_year = case_when( 
      #assign LC each year based on nearest decade reported
      #decades 1940 - 2000 available; mid-decade (1945, 1955, etc.) round down
      #assign 1940 to all years before 1940, 2000 to all years after 2000
      year_w_data <= 1945 & year == 1940 ~ value, 
      year_w_data > 1945 & year_w_data <= 1955 & year == 1950 ~ value, 
      year_w_data > 1955 & year_w_data <= 1965 & year == 1960 ~ value, 
      year_w_data > 1965 & year_w_data <= 1975 & year == 1970 ~ value, 
      year_w_data > 1975 & year_w_data <= 1985 & year == 1980 ~ value, 
      year_w_data > 1985 & year_w_data <= 1995 & year == 1990 ~ value, 
      year_w_data > 1995 & year == 2000 ~ value)) %>%
    drop_na() %>%
    select(COMID, unit, new_class, year_w_data, lc_by_year) %>%
    group_by(COMID, unit, year_w_data) %>%
    mutate(lc_sum_by_year = sum(lc_by_year)) %>%
    mutate(lc_adj_by_year = (lc_by_year / lc_sum_by_year) * 100) %>%
    ungroup() %>%
    group_by(COMID, unit, new_class) %>%
    summarise(value = mean(lc_adj_by_year), .groups = "drop") %>%
    mutate(label = paste0(unit, "_SOHL_", new_class, "_weighted_avg")) %>%
    select(COMID, label, value) %>%
    pivot_wider(names_from = "label", values_from = "value")

  #convert decadal dam information to long-term average dam information
  dams_longterm_avg <- data %>%
    select(COMID, contains("NDAMS"), contains("STORAGE"), contains("MAJOR")) %>%
    distinct() %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           feature = str_sub(name, 5, -5),
           year = str_sub(name, -4)) %>%
    filter(year <= 2010) %>%
    group_by(COMID, unit, feature, year) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    group_by(COMID, unit, feature) %>%
    summarise(value = mean(value), .groups = "drop") %>%
    mutate(label = paste0(unit, "_", feature, "_longterm_avg")) %>%
    select(COMID, label, value) %>%
    pivot_wider(names_from = label, values_from = value)
  
  #convert decadal dam information to weighted average dam information by year
  dams_weighted_avg <- data %>%
    select(COMID, site_comid_match = GAGES_ID) %>%
    left_join(complete_years, by = "site_comid_match") %>%
    select(COMID, year_w_data = year_val) %>%
    drop_na() %>%
    left_join(data, by = "COMID") %>%
    select(COMID, year_w_data, 
           contains("NDAMS"), contains("STORAGE"), contains("MAJOR")) %>%
    distinct() %>%
    pivot_longer(cols = -c(COMID, year_w_data), 
                 names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           feature = str_sub(name, 5, -5), 
           year = str_sub(name, -4)) %>%
    filter(year <= 2010) %>%
    mutate(dams_by_year = case_when(
      #assign dam info each year based on nearest decade reported
      #decades 1930 - 2010 available (and 2013...); mid-decade (1945, 1955, etc.) round down
      #assign 1930 to all years before 1930, 2010 to all years after 2010
      year_w_data <= 1935 & year == 1930 ~ value, 
      year_w_data > 1935 & year_w_data <= 1945 & year == 1940 ~ value, 
      year_w_data > 1945 & year_w_data < 1955 & year == 1950 ~ value, 
      year_w_data > 1955 & year_w_data < 1965 & year == 1960 ~ value, 
      year_w_data > 1965 & year_w_data < 1975 & year == 1970 ~ value, 
      year_w_data > 1975 & year_w_data < 1985 & year == 1980 ~ value, 
      year_w_data > 1985 & year_w_data < 1995 & year == 1990 ~ value, 
      year_w_data > 1995 & year_w_data < 2005 & year == 2000 ~ value, 
      year_w_data > 2005 & year == 2010 ~ value)) %>%
    drop_na() %>%
    select(COMID, unit, feature, year_w_data, dams_by_year) %>%
    group_by(COMID, unit, feature) %>%
    summarise(value = mean(dams_by_year), .groups = "drop") %>%
    mutate(label = paste0(unit, "_", feature, "_weighted_avg")) %>%
    select(COMID, label, value) %>%
    pivot_wider(names_from = label, values_from = value)
 
  #convert annual monthly weather data to long-term average monthly weather data
  weather_longterm_avg <- data %>%
    select(COMID, contains("_PPT_"), contains("_TAV_")) %>%
    distinct() %>%
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
           label = paste0(unit, "_", type, "_", month_name, "_longterm_avg")) %>%
    arrange(COMID, type, unit, month_num) %>%
    select(COMID, label, avg_monthly) %>%
    pivot_wider(names_from = "label", values_from = "avg_monthly")
  
  #convert annual monthly weather data to weighted average monthly weather data by year
  #long-term avgs needed to backfill for years outside of range of weather data
  weather_weighted_avg_temp <- data %>%
    select(COMID, contains("_PPT_"), contains("_TAV_")) %>%
    distinct() %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           type = str_sub(name, 5, 7),
           month_name = str_sub(name, 9, 11)) %>%
    group_by(COMID, unit, type, month_name) %>%
    summarise(avg_monthly = mean(value, na.rm = TRUE), .groups = "drop")
  
  weather_weighted_avg <- data %>%
    select(COMID, site_comid_match = GAGES_ID) %>%
    left_join(complete_years, by = "site_comid_match") %>%
    select(COMID, year_w_data = year_val) %>%
    drop_na() %>%
    left_join(data, by = "COMID") %>%
    select(COMID, year_w_data, contains("_PPT_"), contains("_TAV_")) %>%
    distinct() %>%
    pivot_longer(cols = -c(COMID, year_w_data), 
                 names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           type = str_sub(name, 5, 7),
           year = str_sub(name, 12, 15),
           month_name = str_sub(name, 9, 11)) %>%
    left_join(weather_weighted_avg_temp, by = c("COMID", "unit", "type", "month_name")) %>%
    mutate(month_num = case_when(month_name == "JAN" ~ 1, month_name == "FEB" ~ 2, 
                                 month_name == "MAR" ~ 3, month_name == "APR" ~ 4, 
                                 month_name == "MAY" ~ 5, month_name == "JUN" ~ 6, 
                                 month_name == "JUL" ~ 7, month_name == "AUG" ~ 8, 
                                 month_name == "SEP" ~ 9, month_name == "OCT" ~ 10, 
                                 month_name == "NOV" ~ 11, month_name == "DEC" ~ 12)) %>%
    mutate(weather_by_year = case_when(
      #assign monthly weather each year based on year reported
      #weather data reported by calendar year, weighted averages based on water year
      #years 1945 - 2015 available
      #assign long-term average to all years before 1945 and after 2015
      year_w_data <= 1944 & year == 1945 ~ avg_monthly, 
      year_w_data == 1945 & month_num >= 10 & year == 1945 ~ avg_monthly, 
      year_w_data == 1945 & month_num < 10 & year == 1945 ~ value, 
      year_w_data >= 1946 & year_w_data <= 2015 & month_num >= 10 & year == (year_w_data-1) ~ value, 
      year_w_data >= 1946 & year_w_data <= 2015 & month_num < 10 & year == year_w_data ~ value, 
      year_w_data == 2016 & month_num >= 10 & year == 2015 ~ value, 
      year_w_data == 2016 & month_num < 10 & year == 2015 ~ avg_monthly, 
      year_w_data > 2016 & year == 2015 ~ avg_monthly)) %>%
    drop_na() %>%
    select(COMID, unit, type, month_name, month_num, year_w_data, weather_by_year) %>%
    group_by(COMID, unit, type, month_name) %>%
    summarise(value = mean(weather_by_year), 
              month_num = mean(month_num),
              .groups = "drop") %>%
    mutate(label = paste0(unit, "_", type, "_", month_name, "_weighted_avg")) %>%
    arrange(COMID, type, unit, month_num) %>%
    select(COMID, label, value) %>%
    pivot_wider(names_from = label, values_from = value)
  
  #convert annual wildfire data to long-term average wildfire data
  wildfire_shortterm_avg <- data %>%
    select(COMID, contains("_WILDFIRE_")) %>%
    group_by(COMID) %>%
    summarise_all(mean) %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           year = str_sub(name, 14, 17)) %>%
    group_by(COMID, unit) %>%
    summarise(avg_annual = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(label = paste0(unit, "_WILDFIRE_shortterm_avg")) %>%
    arrange(COMID, label) %>%
    select(COMID, label, avg_annual) %>%
    pivot_wider(names_from = "label", values_from = "avg_annual")
  
  #determine dominant physiographic region
  phys_region <- data %>%
    select(COMID, contains("_PHYSIO_")) %>%
    select(-contains("AREA")) %>%
    group_by(COMID) %>%
    summarise_all(mean) %>%
    pivot_longer(!COMID, names_to = "name", values_to = "value") %>%
    mutate(unit = str_sub(name, 1, 3), 
           region = str_sub(name, 12))
  
  dom_phys_reg <- tibble()
  for (i in unique(phys_region$COMID)) {
    phys_reg_comid <- filter(phys_region, COMID == i)
    
    dom_phys_reg_comid <- tibble()
    for (j in unique(phys_reg_comid$unit)) {
      phys_reg_comid_unit_ties <- filter(phys_reg_comid, unit == j) %>%
        arrange(desc(value))
      
      #if tie for dominant phyiographic region, default to CAT value
      if (phys_reg_comid_unit_ties[[1,3]] > phys_reg_comid_unit_ties[[2,3]]) {
        phys_reg_comid_unit <- phys_reg_comid_unit_ties[1, ]
      } else {
        phys_reg_comid_unit_tied <- phys_reg_comid %>%
          filter(unit == "CAT")
        phys_reg_comid_unit <- 
          phys_reg_comid_unit_tied[which.max(phys_reg_comid_unit_tied$value), ]
        phys_reg_comid_unit <- mutate(phys_reg_comid_unit, unit == j)
      }
      dom_phys_reg_comid <- bind_rows(dom_phys_reg_comid, phys_reg_comid_unit)
    }
    dom_phys_reg <- bind_rows(dom_phys_reg, dom_phys_reg_comid)
  }
  
  dom_phys_reg$region <- as.factor(dom_phys_reg$region)
  phys_region <- dom_phys_reg %>%
    mutate(label = paste0(unit, "_PHYSIO")) %>%
    arrange(COMID, label) %>%
    select(COMID, label, region) %>%
    pivot_wider(names_from = "label", values_from = "region")
  
  #remove unwanted feature variables and re-join long-term average datasets
  data <- data %>%
    select(-ends_with(".y"), -ID, -starts_with("..."), -contains("_S1"), 
           -contains("_PHYSIO_"), -contains("SOHL"), -contains("NDAMS"), 
           -contains("STORAGE"), -contains("MAJOR"), -contains("_TAV_"), 
           -contains("_PPT_"), -contains("WILDFIRE")) %>%
    left_join(phys_region, by = "COMID") %>%
    left_join(land_cover_longterm_avg, by = "COMID") %>%
    left_join(land_cover_weighted_avg, by = "COMID") %>%
    left_join(dams_longterm_avg, by = "COMID") %>%
    left_join(dams_weighted_avg, by = "COMID") %>%
    left_join(weather_longterm_avg, by = "COMID") %>%
    left_join(weather_weighted_avg, by = "COMID") %>%
    left_join(wildfire_shortterm_avg, by = "COMID")
  
  #rename duplicated headers (if any) and replace headers containing "/" with "-"
  rename_dup_headers <- list()
  for (i in 1:ncol(data)) {
    header <- names(data)[i]
    if (str_sub(header, -2) == ".x") {
      new_header <- str_sub(header, 1, -3)
    } else if (str_detect(header, "/")) {
      new_header <- str_replace(header, "/", "-")
    } else {
      new_header <- header
    }
    rename_dup_headers[[i]] <- new_header
  }
  rename_dup_headers <- as.character(rename_dup_headers)
  names(data) <- rename_dup_headers
  
  #renumber gages that were combined on same comid
  final_data <- data %>%
    filter(!(GAGES_ID %in% unlist(combine_gages)))
  for (i in 1:length(unlist(combine_gages$to_be_combined))) {
    combined <- data %>%
      filter(GAGES_ID == unlist(combine_gages$to_be_combined)[[i]]) %>%
      mutate(GAGES_ID = paste0(combine_gages$to_be_combined[[i]], "_", 
                               combine_gages$assigned_rep[[i]]))
    final_data <- bind_rows(final_data, combined)
  }
  
  return(final_data)
}


prep1_vars_conus <- function(sb_var_data, sites, retain_attr, outdir, out_file_label) {
  
  #'@description joins all data downloaded from ScienceBase with sites of interest
  #'
  #'@param sb_var_data target generated from the 'get_sb_data()' function mapped over the 
  #''p1_sb_var_ids' targets; the 'p1_sb_var_ids' target reads in a .csv of ScienceBase
  #'identifiers and names, and the 'get_sb_data()' function generates a list of .csv file
  #'locations containing the feature variable data from each ScienceBase identifier
  #'joined with the COMIDs contained in the 'sites' parameter
  #'@param sites data frame with a COMID column including all reaches of interest
  #'@param retain_attr character strings of additional column headers (attributes) 
  #'to keep in the sites data frame
  #'@param out_dir output file directory
  #'@param out_file_label naming convention to label output files from this function
  #'
  #'@return data frame with COMID column appended by all feature variables of interest
  
  # Only keep retained feature variables and screened gages 
  data <- sites %>%
    select(COMID, all_of(retain_attr)) %>%
    mutate(across(where(is.character), as.numeric))
  
  #read in sb data
  filepath <- sb_var_data
  filepath <- unlist(filepath)[1]
  id <- str_sub(filepath, 26, -15)
  data_temp <- read_csv(filepath, show_col_types = FALSE)
  
  if (str_detect(names(data_temp[2]), "SOHL")) {
    
    data <- data
    
  } else if (str_detect(names(data_temp[2]), "NDAMS")) {
    
    unit <- c("CAT", "ACC", "TOT")
    type <- c("NDAMS", "NID_STORAGE", "NORM_STORAGE", "MAJOR")
    
    data_all <- tibble(COMID = data_temp$COMID)
    for (i in unit) {
      data_by_unit <- data_temp %>%
        select(COMID, contains(i))
      data_all_units <- tibble(COMID = data_temp$COMID)
      for (j in type) {
        data_by_type <- data_by_unit %>%
          select(COMID, contains(j)) %>%
          mutate(longterm_avg = rowMeans(select(., -COMID))) %>%
          select(COMID, longterm_avg)
        name <- paste0(i, "_", j, "_longterm_avg")
        names(data_by_type) <- c("COMID", name)
        data_all_units <- left_join(data_all_units, data_by_type, by = "COMID")
      }
      data_all <- left_join(data_all, data_all_units, by = "COMID")
    }
    
    #re-join long-term average dam data
    data <- data %>%
      left_join(data_all, by = "COMID")
    
  } else if (str_sub(names(data_temp)[2], 5, 7) == "TAV" |
             str_sub(names(data_temp)[2], 5, 7) == "PPT") {
    
    unit <- str_sub(names(data_temp)[2], 1, 3)
    type <- str_sub(names(data_temp)[2], 5, 7)
    months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
                "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
    
    data_all <- tibble(COMID = data_temp$COMID)
    for (i in months) {
      data_by_month <- data_temp %>%
        select(COMID, contains(i)) %>%
        mutate(longterm_avg = rowMeans(select(., -COMID))) %>%
        select(COMID, longterm_avg)
      name <- paste0(unit, "_", type, "_", i, "_longterm_avg")
      names(data_by_month) <- c("COMID", name)
      data_all <- left_join(data_all, data_by_month, by = "COMID")
    }
    
    #re-join long-term average weather data
    data <- data %>%
      left_join(data_all, by = "COMID")
    
  } else if (str_detect(names(data_temp[2]), "WILDFIRE")) {
    
    units <- c("CAT", "ACC", "TOT")
    
    data_all <- tibble(COMID = data_temp$COMID)
    for (i in units) {
      data_by_unit <- data_temp %>%
        select(COMID, contains(i)) %>%
        mutate(shortterm_avg = rowMeans(select(., -COMID))) %>%
        select(COMID, shortterm_avg)
      name <- paste0(i, "_WILDFIRE_shortterm_avg")
      names(data_by_unit) <- c("COMID", name)
      data_all <- left_join(data_all, data_by_unit, by = "COMID")
    }
    
    #re-join short-term average wildfire data
    data <- data %>%
      left_join(data_all, by = "COMID")
    
  } else if (str_detect(names(data_temp[2]), "PHYSIO")) {
    
    units <- c("CAT", "ACC", "TOT")
    
    data_all <- tibble(COMID = data_temp$COMID)
    for (i in units) {
      data_by_unit <- data_temp %>%
        select(COMID, contains(i), -contains("AREA")) %>%
        mutate(PHYSIO_name = names(select(., -COMID))[max.col(select(., -COMID))]) %>%
        mutate(PHYSIO_reg = str_sub(PHYSIO_name, 12)) %>%
        select(COMID, PHYSIO_reg)
      data_by_unit$PHYSIO_reg <- as.factor(data_by_unit$PHYSIO_reg)
      name <- paste0(i, "_PHYSIO")
      names(data_by_unit) <- c("COMID", name)
      data_all <- left_join(data_all, data_by_unit, by = "COMID")
    }
    
    #re-join dominant physiographic region
    data <- data %>%
      left_join(data_all, by = "COMID")
    
  } else if (str_detect(names(data_temp[2]), "ACC_BASIN_AREA")) {
    
    data <- data %>%
      left_join(data_temp, by = "COMID") %>%
      select(-ends_with(".y"), -ID, -starts_with("..."), -contains("_S1"))
    
  } else {
    
    data <- data %>%
      left_join(data_temp, by = "COMID")
    
  }
  
  #rename duplicated headers (if any) and replace headers containing "/" with "-"
  rename_dup_headers <- list()
  for (i in 1:ncol(data)) {
    header <- names(data)[i]
    if (str_sub(header, -2) == ".x") {
      new_header <- str_sub(header, 1, -3)
    } else if (str_detect(header, "/")) {
      new_header <- str_replace(header, "/", "-")
    } else {
      new_header <- header
    }
    rename_dup_headers[[i]] <- new_header
  }
  rename_dup_headers <- as.character(rename_dup_headers)
  names(data) <- rename_dup_headers
  
  file_out <- paste0(outdir, "/sb_", id, "_", out_file_label, ".csv")
  write_csv(data, file_out)
  return(file_out)
}


prep2_vars_conus <- function(sohl_early, sohl_late, outdir, out_file_label) {
  
  #'@description joins all data downloaded from ScienceBase with sites of interest
  #'
  #'@param prep1_vars target generated from the 'prep1_vars_conus()' function
  #'@param outdir output directory
  #'@param out_file_label naming convention to label output files from this function
  #'
  #'@return data frame with COMID column appended by all feature variables of interest
  
  #read in sb data
  early <- read_csv(paste0(outdir, "/sb_", sohl_early, "_raw_conus.csv"), 
                    show_col_types = FALSE)
  late <- read_csv(paste0(outdir, "/sb_", sohl_late, "_raw_conus.csv"), 
                   show_col_types = FALSE)
  late <- late %>%
    select(COMID, contains("SOHL00"))
  sohl <- left_join(early, late, by = "COMID")
  sohl[sohl == -9999] <- NA_real_
  
  unit <- c("CAT", "ACC", "TOT")
  category <- as.character(1:17)
  
  data_all <- tibble(COMID = sohl$COMID)
  comid_out_conus_all <- tibble()
  for (i in unit) {
    data_by_unit <- sohl %>%
      select(COMID, contains(i))
    data_all_cats <- tibble(COMID = sohl$COMID)
    for (j in category) {
      data_by_category <- data_by_unit %>%
        select(COMID, ends_with(paste0("_", j))) %>%
        mutate(longterm_avg = rowMeans(select(., -COMID))) %>%
        select(COMID, longterm_avg)
      name <- paste0("SOHL", j)
      names(data_by_category) <- c("COMID", name)
      data_all_cats <- left_join(data_all_cats, data_by_category, by = "COMID")
    }
    data_reclass <- data_all_cats %>%
      mutate(WATER = SOHL1, 
             DEVELOPED = SOHL2, 
             FOREST = rowSums(select(., SOHL3, SOHL4, SOHL5, SOHL8, SOHL9, SOHL10), 
                                na.rm = TRUE),
             MINING = SOHL6, 
             BARREN = SOHL7, 
             GRASSLAND = SOHL11, 
             SHRUBLAND = SOHL12, 
             CROPLAND = SOHL13, 
             HAYPASTURE = SOHL14, 
             WETLAND = rowSums(select(., SOHL15, SOHL16),
                                 na.rm = TRUE), 
             ICESNOW = SOHL17) %>%
      select(COMID, WATER, DEVELOPED, FOREST, MINING, BARREN, GRASSLAND, 
             SHRUBLAND, CROPLAND, HAYPASTURE, WETLAND, ICESNOW) %>%
      mutate(lc_sum = rowSums(select(., -COMID)))
                
    comid_out_conus_unit <- data_reclass %>%
      filter(lc_sum < 90 | lc_sum > 101 | is.na(lc_sum)) %>%
      select(COMID)
    comid_out_conus_all <- bind_rows(comid_out_conus_all, comid_out_conus_unit)
    
    data_reclass <- data_reclass %>%
      mutate(WATER = (WATER/lc_sum)*100, 
             DEVELOPED = (DEVELOPED/lc_sum)*100, 
             FOREST = (FOREST/lc_sum)*100,
             MINING = (MINING/lc_sum)*100,
             BARREN = (BARREN/lc_sum)*100, 
             GRASSLAND = (GRASSLAND/lc_sum)*100, 
             SHRUBLAND = (SHRUBLAND/lc_sum)*100, 
             CROPLAND = (CROPLAND/lc_sum)*100, 
             HAYPASTURE = (HAYPASTURE/lc_sum)*100, 
             WETLAND = (WETLAND/lc_sum)*100, 
             ICESNOW = (ICESNOW/lc_sum)*100) %>%
      select(-lc_sum) %>%
      rename_with(~ paste0(i, "_SOHL_", ., "_longterm_avg"), -COMID)
    data_all <- left_join(data_all, data_reclass, by = "COMID")
  }
  
  data_all <- data_all %>%
    subset(!(COMID %in% unique(comid_out_conus_all$COMID)))
  
  file_out <- paste0(outdir, "/sb_SOHL_", out_file_label, ".csv")
  write_csv(data_all, file_out)
  return(file_out)
}

