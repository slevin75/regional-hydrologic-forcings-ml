library(targets)
library(tarchetypes)
library(dataRetrieval)

dir.create('1_fetch/out',showWarnings=FALSE)
source("./1_fetch/src/get_nwis_data.R")
source("./1_fetch/src/calc_HIT.R")

# true confession - I don't really know what these three lines do, I just copied them from 
#the pipelines training
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

###for east river region, I am selecting the WestMnts region in gagesii and cutting off
##anything west of the Washington/Idaho border.  For DE, I used the 

region<-c("EastHghlnds","SECstPlain") 
#region <- "WestMnts"

gagesii_path<-"./1_fetch/in/Gages2.1_RefSiteList.csv"
region<-"WestMnts"

p1_sites_list<- get_reference_sites(gagesii_path,region)

##note-  We'll probably need to add another screening function somehwere that checks the 
##number of complete water years.  Some of the gages have a long enough period of record, but don't have complete years. 
##Right now the screening function I put in just removes all the incomplete years but it doesn't 
##check to make sure the remaining period of record is long enough.

p1_flow_metrics<- tar_map(
  values= tibble(sites=p1_sites_list),
  ##download raw daily data
  tar_target(p1_daily_flow, 
             get_nwis_daily_data(sites,outdir="./1_fetch/out",NWIS_parameter,startDate,endDate),
             format="file"),
  ##screen for incomplete years of data
  tar_target(p1_screen_daily_flow,
             screen_daily_data(p1_daily_flow,yearType)),
  ##summarize the daily data screening df
  tar_target(p1_data_screen_summary,
             summarize_data_screen(p1_screen_daily_flow)),
  
  ##clean data and reformat for EflowStats 
  tar_target(p1_clean_daily_flow,
             clean_daily_data(p1_daily_flow,p1_screen_daily_flow,yearType)),
  
  ##get drainage area (needed for EflowStats - can change this later if DA is part of the
  ## basin characteristics file)
  tar_target(p1_drainage_area,
             readNWISsite(siteNumbers=sites)$drain_area_va),
  
  ##get peak flow data (also needed for Eflow Stats)
  ##note - in this function, dataRetreival gives an NA for date any time the day of occurance is unknown in NWIS,
  ##I just removed any peak that didn't have a date, but if we want, I can fill in the date so we don't lose that 
  ##peak value.  
  tar_target(p1_peak_flow,
             get_nwis_peak_data(sites,outdir="./1_fetch/out",startDate,endDate)),
  
  ##flood threshold is used in eFlowStats for some hi flow duration and frequency stats
  tar_target(p1_flood_threshold,
             get_peakThreshold(p1_clean_daily_flow[c("date","discharge")],
                               peakValues=p1_peak_flow[c("peak_dt","peak_va")],yearType=yearType))
  
  ###compute all 171 HIT metrics
 # tar_target(p1_HIT_metrics,
  #           calc_HITmetrics(p1_clean_daily_flow,yearType,drainArea=p1_drainage_area,
   #                      floodThreshold=p1_flood_threshold))
  
)


##targets
list(
  p1_flow_metrics,
  tar_combine(p1_combined_data_screen_summary,
              p1_flow_metrics$p1_data_screen_summary,
              command=combine_flow_metrics(!!!.x))
  ##combine flow metrics from all sites into one dataframe - not sure if that is 
  ##the format we want for this or if it would be better as a list?
  #tar_combine(p1_combined_HIT_metrics,
  #            p1_flow_metrics$p1_HIT_metrics,
  #            command=combine_flow_metrics(!!!.x))
 
) #end list




#################This takes a long time to run the first time through, so I am just summarizing 
##that final combined summary table for data completeness.

#for CO - there were 297 sites total, 190 sites with at least 20 complete years (not necessarily consecutive),
# and 196 sites that had at least 20 complete "mar-november years".  Also, there were 197 sites that had
## at least 20 years in which no month had more than 5 missing (thinking these would be fill-able)

##for delaware, there were










