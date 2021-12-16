library(targets)
library(tarchetypes)
library(readxl)

dir.create('1_fetch/out',showWarnings=FALSE)
source("./1_fetch/src/get_nwis_data.R")
source("./1_fetch/src/calc_HIT.R")

tar_option_set(packages = c("fasstr","EflowStats","dataRetrieval",
                            "lubridate"))
suppressPackageStartupMessages(library(tidyverse))
options(tidyverse.quiet = TRUE)


###parameters  
NWIS_parameter <- '00060'
startDate<-as.Date("1900-10-01") 
endDate<-as.Date("2020-09-30")
##water year or calendar year.  I assume we are doing this on water year but
##Eflow stats can do either one and it needs to be specified 
yearType<-"water"
##number of complete years we require for a site to be
complete_years <- 20
#precentile for flood threshold in Eflowstats.  0.6 is the default
perc<-0.6

###gages2.1 ref site list - not sure how to get this right from sharepoint, so the
##filepath is currently to my onedrive.
gagesii_path<-"C:/Users/slevin/OneDrive - DOI/FWA_bridgeScour/Data/Gages2.1_RefSiteList.xlsx"
gagesii<-read_xlsx(gagesii_path)
gagesii$ID<- substr(gagesii$ID,start=2,stop=nchar(gagesii$ID))
##

## not sure yet how we'll be selecting gages so I'm not putting this in a function yet.
##since there is no state attribution in the gagesii list, for East River, I am taking 
##AggEco==WestMnts and LON > -117 which cuts off the pacific northwest and cA areas


#p1_sites_list<- c("06036805" ,"06036905", "06037500","06043500","06073500","06078500","06090500",
#"06092500","06109800","06115500" ,"06137570" ,"06154410","06188000","06190540")
#note- 

#p1_sites_list<-gagesii %>%
#  filter(AggEco=="WestMnts") %>%
#  filter(LON > -117)%>%
#  pull(ID)

##DE - just pulling a bounding box of sites here
p1_sites_list<-gagesii %>%
  filter(LAT<42) %>%
  filter(LON > -76)%>%
  pull(ID)



##targets
list(
  ##check to make sure peak and daily flow are actually available for all sites
  tar_target(p1_has_data,
             has_data_check(p1_sites_list,NWIS_parameter)),
 ##fetch daily streamflow
 tar_target(p1_daily_flow, 
             get_nwis_daily_data(p1_has_data,outdir="./1_fetch/out",NWIS_parameter,startDate,endDate),
             map(p1_has_data),
             format="file"),
 ##compute the number of complete years
 tar_target(p1_screen_daily_flow,
             screen_daily_data(p1_daily_flow,yearType),
             map(p1_daily_flow)),
  ##select out sites with enough complete years
 tar_target(p1_screened_site_list,
            filter_complete_years(p1_screen_daily_flow,complete_years)),
  ##clean and format daily data so it can be used in eflostats 
 tar_target(p1_clean_daily_flow,
            clean_daily_data(p1_screened_site_list,p1_daily_flow,p1_screen_daily_flow,yearType),
            map(p1_screened_site_list)),
 #get drainage area from NWIS
 tar_target(p1_drainage_area,
            get_NWIS_drainArea(p1_screened_site_list),
            map(p1_screened_site_list)),
 ##get and save as file peak flow from NWIS
 tar_target(p1_peak_flow,
               get_nwis_peak_data(p1_screened_site_list,outdir="./1_fetch/out",startDate,endDate),
            map(p1_screened_site_list),
            format="file"),
 ##get flood threshold for eflowstats
 tar_target(p1_flood_threshold,
               get_floodThreshold(p1_screened_site_list, p1_clean_daily_flow,
                                  p1_peak_flow,perc,yearType),
            map(p1_screened_site_list)),
 ##compute all HIT metrics for screened sites list
 tar_target(p1_HIT_metrics,
                 calc_HITmetrics(p1_screened_site_list,p1_clean_daily_flow,yearType,
                                 drainArea_tab=p1_drainage_area,floodThreshold_tab=p1_flood_threshold),
            map(p1_screened_site_list)) 

 
) #end list
