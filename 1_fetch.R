
source("1_fetch/src/get_nwis_data.R")
source("1_fetch/src/get_feature_vars.R")
source("1_fetch/src/moving_window_functions.R")

##p1- only parameters
gagesii_path <- "Gages2.1_RefSiteList.xlsx"
NWIS_parameter <- '00060'
startDate <- as.Date("1900-10-01") 
endDate <- as.Date("2022-09-30")
##number of complete years we require for a site to be used
complete_years <- 20
##percentile for flood threshold in EflowStats. 0.6 is the default
perc <- 0.6
#Drop the following gages from the dataset because they are not representative
#pipeline, ditch, duplicate comids in gages2.1, spring, etc.
drop_gages <- c('02084557', '09406300', '09512200', '10143500', '10172200', 
                '01349711', '01362198', '01362380', '02322698', '04127918', 
                '10336674', '10336675', '06190540')

#Combine the following gages from the dataset because they are located on same
#comid with unique periods or record
combine_gages <- list(c('03584000', '06037000', '12209500'), 
                      c('03584020', '06037100', '12209490'))
names(combine_gages) <- c("to_be_combined", "assigned_rep")
# list of science base identifiers containing feature variables of interest
sb_var_ids_path <- "1_fetch/in/sb_var_ids.csv"




p1_targets_list <- list(
  #file target for gagesii (g2) sites 
  tar_target(p1_sites_g2_xlsx,
             gagesii_path,
             deployment = 'main',
             format = "file"
  ),
  #all gagesii (g2) sites 
  tar_target(p1_sites_g2,
             read_xlsx(p1_sites_g2_xlsx) %>% 
               mutate(ID = substr(ID, start=2, stop=nchar(ID))) %>%
               #drop 5 sites that are not representative (ditch, pipeline)
               #and 7 sites that are duplicates on same comid
               filter(!(ID %in% drop_gages)),
             deployment = 'main'
  ),
  
  #create a spatial object 
  tar_target(p1_sites_g2_sf,
             st_as_sf(x = p1_sites_g2, coords = c('LON', 'LAT'), 
                      crs = st_crs(4326),remove = FALSE, dim = 'XY', na.fail = TRUE),
             deployment = 'main'
  ),
  #ID numbers for sites to use
  tar_target(
    p1_sites_list,
    p1_sites_g2$ID,
    deployment = 'main'
  ),
  
  ##check to make sure peak and daily flow are actually available for all sites
  tar_target(p1_has_data,
             has_data_check(p1_sites_list, NWIS_parameter, endDate),
             deployment = 'main'
  ),
  
  ##fetch daily streamflow
  #this is deployed on main to avoid overloading the NWIS server with download requests
  #note that the download occasionally randomly hangs even with the timeout.
  #you can stop and restart the pipeline when this happens.
  tar_target(p1_daily_flow_csv, 
             get_nwis_daily_data(p1_has_data, outdir="./1_fetch/out", 
                                 NWIS_parameter, startDate, endDate, 
                                 timeout = 60),
             map(p1_has_data),
             deployment = 'main',
             format = "file"
  ),
  ##generate log file to track changes to dataRetrieval daily flow request
  tar_target(p1_daily_flow_log, 
             get_daily_flow_log(files_in = p1_daily_flow_csv, 
                                file_out = "./1_fetch/out/logs/daily_flow_log.csv"),
             deployment = 'main',
             format = "file"
  ),
  
  ##prescreen data to remove provisional data and handle odd column names, and
  ##combine records from gages with unique periods of record on same comid
  tar_target(p1_prescreen_daily_data, 
             prescreen_daily_data(p1_daily_flow_csv, prov_rm = TRUE),
             map(p1_daily_flow_csv),
             deployment = 'worker'
  ),
  
  ##compute the number of complete years based on when the year starts
  #These are being run on main because parallel processing is taking too long.
  #Likely because the mapped branches build quickly and the prescreen data are large
  tar_target(p1_screen_daily_flow,
             screen_daily_data(p1_has_data, p1_prescreen_daily_data, year_start),
             map(p1_has_data),
             deployment = 'main'
  ),
  
  ##For seasonal analysis
  tar_target(p1_screen_daily_flow_season,
             screen_daily_data(p1_has_data, p1_prescreen_daily_data, season_year_start),
             map(p1_has_data),
             deployment = 'main'
  ),
  # tar_target(p1_screen_daily_flow_season_high,
  #            screen_daily_data(p1_has_data, p1_prescreen_daily_data, season_year_start_high),
  #            map(p1_has_data),
  #            deployment = 'main'
  # ),
  
  ##select sites with enough complete years
  tar_target(p1_screened_site_list,
             filter_complete_years(p1_screen_daily_flow, combine_gages, complete_years),
             deployment = 'main'
  ),
  ##seasonal
  tar_target(p1_screened_site_list_season,
             filter_complete_years(p1_screen_daily_flow_season, combine_gages, complete_years),
             deployment = 'main'
  ),
  # tar_target(p1_screened_site_list_season_high,
  #            filter_complete_years(p1_screen_daily_flow_season_high, combine_gages, complete_years),
  #            deployment = 'main'
  # ),
  
  ##clean and format daily data so it can be used in EflowStats 
  tar_target(p1_clean_daily_flow,
             clean_daily_data(p1_screened_site_list, p1_prescreen_daily_data, 
                              p1_screen_daily_flow, yearType, year_start),
             map(p1_screened_site_list),
             deployment = 'main'
  ),
  ##seasonal
  tar_target(p1_clean_daily_flow_season,
             clean_daily_data(p1_screened_site_list_season, p1_prescreen_daily_data, 
                              p1_screen_daily_flow_season, yearType, season_year_start),
             map(p1_screened_site_list_season),
             deployment = 'main'
  ),
  # tar_target(p1_clean_daily_flow_season_high,
  #            clean_daily_data(p1_screened_site_list_season_high, p1_prescreen_daily_data, 
  #                             p1_screen_daily_flow_season_high, yearType, 
  #                             season_year_start_high),
  #            map(p1_screened_site_list_season_high),
  #            deployment = 'main'
  # ),
  
  #get drainage area from NWIS
  #this is deployed on main to avoid overloading the NWIS server with download requests
  tar_target(p1_drainage_area,
             get_NWIS_drainArea(p1_screened_site_list),
             map(p1_screened_site_list),
             deployment = 'main'
  ),
  ##generate log file to track changes to dataRetrieval drainage area request
  tar_target(p1_drainage_area_log, 
             get_drainage_area_log(file_in = p1_drainage_area, 
                                   file_out = "./1_fetch/out/logs/drainage_area_log.csv"),
             deployment = 'main',
             format = "file"
  ),
  
  ##get and save as file peak flow from NWIS for eflowstats
  #this is deployed on main to avoid overloading the NWIS server with download requests
  tar_target(p1_peak_flow_csv,
             get_nwis_peak_data(p1_screened_site_list, outdir="./1_fetch/out",
                                startDate, endDate, timeout = 60),
             map(p1_screened_site_list),
             deployment = 'main',
             format="file"
  ),
  ##generate log file to track changes to dataRetrieval peak flow request
  tar_target(p1_peak_flow_log, 
             get_peak_flow_log(files_in = p1_peak_flow_csv, 
                               file_out = "./1_fetch/out/logs/peak_flow_log.csv"),
             deployment = 'main',
             format = "file"
  ),
  
  #file target for sciencebase variable list csv
  tar_target(p1_sb_var_ids_csv,
             sb_var_ids_path,
             deployment = 'main',
             format = "file"
  ),
  
  #read in sciencebase variable list csv
  tar_target(p1_sb_var_ids,
             read_csv(file = p1_sb_var_ids_csv, show_col_types = FALSE),
             deployment = 'main'
  ),
  
  ##generate tables of feature variables from gagesii list  
  tar_target(p1_sb_data_g2_csv,
             get_sb_data(sites = p1_sites_g2, 
                         sb_var_ids = p1_sb_var_ids,
                         dldir = "./1_fetch/out/sb/dldir", 
                         workdir = "./1_fetch/out/sb/workdir",
                         outdir = "./1_fetch/out/sb/data",
                         out_file_name = "sb_data_g2_"),
             map(p1_sb_var_ids),
             iteration = "list",
             deployment = 'main',
             format = "file"
  ),
  
  ##downloads nhd geodatabase
  tar_target(p1_nhd_conus, 
             get_nhd_conus(outdir = "./1_fetch/out/nhd_plus/", 
                           seven_zip = "/caldera/projects/usgs/water/impd/fhwa/seven_zip/7zz"), 
             deployment = 'main',
             format = "file"
  ),
  
  ##generate log file to track updates to sb variables
  tar_target(p1_sb_data_g2_log,
             get_sb_data_log(sb_var_ids = p1_sb_var_ids,
                             file_out = "1_fetch/out/logs/sb_update_log.csv"),
             deployment = "main",
             format = "file"
  ),
  
  ##merge and select feature variables from gagesii list
  tar_target(p1_feature_vars_g2, 
             prep_feature_vars(sb_var_data = p1_sb_data_g2_csv, 
                               sites_all = p1_sites_g2, 
                               sites_screened = p1_screened_site_list, 
                               combine_gages = combine_gages,
                               years_by_site = p1_clean_daily_flow,
                               retain_vars = c("ID", "LAT", "LON",
                                               "npdes", "fwwd", "strg", "devl", "cndp")), 
             deployment = "main"
  ),
  
  #create a spatial object for remaining feature variables
  tar_target(p1_feature_vars_g2_sf,
             st_as_sf(x = p1_feature_vars_g2, coords = c('LON', 'LAT'), 
                      remove = FALSE, dim = 'XY', na.fail = TRUE),
             deployment = 'main'
  ),
  
  ##get flood threshold from NWIS for eflowstats
  #this is deployed on main to avoid overloading the NWIS server with download requests
  tar_target(p1_flood_threshold,
             get_floodThreshold(p1_screened_site_list, p1_clean_daily_flow,
                                p1_peak_flow_csv, perc, yearType),
             map(p1_screened_site_list),
             deployment = 'main'
  )
  
  
  
)
