library(targets)
library(tarchetypes)
library(readxl)
suppressPackageStartupMessages(library(tidyverse))
options(tidyverse.quiet = TRUE)

##Load libraries for use in computing targets
tar_option_set(packages = c("fasstr", "EflowStats", "dataRetrieval",
                            "lubridate"))

##Create output file directories
dir.create('1_fetch/out', showWarnings=FALSE)

##Load user defined functions
source("./1_fetch/src/get_nwis_data.R")
source("./1_fetch/src/calc_HIT.R")

###Define parameters
NWIS_parameter <- '00060'
startDate <- as.Date("1900-10-01") 
endDate <- as.Date("2020-09-30")
##water year or calendar year.
yearType <- "water"
##number of complete years we require for a site to be used
complete_years <- 20
##percentile for flood threshold in EflowStats. 0.6 is the default
perc <- 0.6
##statistics to compute within EflowStats
stats_HIT <- c("calc_magAverage", "calc_magLow", "calc_magHigh", 
               "calc_frequencyHigh", "calc_durationHigh", "calc_rateChange")
##EflowStats metrics to use
metrics <- c('ma1', 'ma2', 
             'ml17', 'ml18', 
             'mh15', 'mh16', 'mh17', 'mh20', 'mh21', 'mh24', 'mh27', 
             'fh1', 'fh2', 'fh5', 
             'dh1', 'dh6', 'dh15', 'dh16', 'dh17', 'dh20',
             'ra1', 'ra2', 'ra3', 'ra4'
)
##metrics to normalize by drainage area
metrics_DA <- c('ma1', 'ma2', 'dh1', 'ra1', 'ra3')
##metric ml17 to normalize by *annual mean/drainage area
metrics_ml17 <- c('ml17')
##metrics to normalize by *median/drainage area
metrics_med_DA <- c('mh15', 'mh16', 'mh17', 'mh21', 'mh24', 'mh27')

###gages2.1 ref site list - not sure how to get this right from sharepoint, so the
##filepath is currently to onedrive.
gagesii_path <- "C:/Users/jsmith/OneDrive - DOI/Shared Documents - FHWA/General/Data/Gages2.1_RefSiteList.xlsx"
gagesii <- read_xlsx(gagesii_path)
gagesii$ID <- substr(gagesii$ID, start=2, stop=nchar(gagesii$ID))


## not sure yet how we'll be selecting gages so I'm not putting this in a function yet.
##since there is no state attribution in the gagesii list, for East River, I am taking 
##AggEco==WestMnts and LON > -117 which cuts off the pacific northwest and cA areas

#p1_sites_list <- gagesii %>%
#  filter(AggEco == "WestMnts") %>%
#  filter(LON > -117) %>%
#  pull(ID)

##DE - just pulling a bounding box of sites here
p1_sites_list <- gagesii %>%
  filter(LAT < 42) %>%
  filter(LON > -76) %>%
  pull(ID)


##targets
list(
  ##check to make sure peak and daily flow are actually available for all sites
  tar_target(p1_has_data,
             has_data_check(p1_sites_list, NWIS_parameter)),
  
  ##fetch daily streamflow
  tar_target(p1_daily_flow_csv, 
             get_nwis_daily_data(p1_has_data, outdir="./1_fetch/out", 
                                 NWIS_parameter, startDate, endDate),
             map(p1_has_data),
             format="file"),
  
  ##compute the number of complete years
  tar_target(p1_screen_daily_flow,
             screen_daily_data(p1_daily_flow_csv, yearType),
             map(p1_daily_flow_csv)),
  
  ##select out sites with enough complete years
  tar_target(p1_screened_site_list,
             filter_complete_years(p1_screen_daily_flow, complete_years)),
  
  ##clean and format daily data so it can be used in EflowStats 
  tar_target(p1_clean_daily_flow,
             clean_daily_data(p1_screened_site_list, p1_daily_flow_csv, 
                              p1_screen_daily_flow, yearType),
             map(p1_screened_site_list)),
  
  #get drainage area from NWIS
  tar_target(p1_drainage_area,
             get_NWIS_drainArea(p1_screened_site_list),
             map(p1_screened_site_list)),
  
  ##get and save as file peak flow from NWIS
  tar_target(p1_peak_flow_csv,
             get_nwis_peak_data(p1_screened_site_list, outdir="./1_fetch/out",
                                startDate, endDate),
             map(p1_screened_site_list),
             format="file"),
  
  ##get flood threshold for eflowstats
  tar_target(p1_flood_threshold,
             get_floodThreshold(p1_screened_site_list, p1_clean_daily_flow,
                                p1_peak_flow_csv, perc, yearType),
             map(p1_screened_site_list)),
  
  ##compute all HIT metrics for screened sites list
  tar_target(p1_HIT_metrics,
             calc_HITmetrics(site_num = p1_screened_site_list, 
                             clean_daily_flow = p1_clean_daily_flow, 
                             yearType = yearType,
                             drainArea_tab = p1_drainage_area,
                             floodThreshold_tab = p1_flood_threshold,
                             stat_vec = stats_HIT,
                             save_metrics = metrics,
                             norm_DA = metrics_DA,
                             norm_med_DA = metrics_med_DA,
                             norm_ml17 = metrics_ml17),
             map(p1_screened_site_list))
) #end list
