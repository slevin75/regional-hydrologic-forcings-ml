library(targets)
library(tarchetypes)
library(readxl)
options(clustermq.scheduler = "multiprocess")
library(clustermq)
getOption("clustermq.data.warning", 1000) #MB
options(tidyverse.quiet = TRUE)
library(tidyverse)

##Load libraries for use in computing targets
tar_option_set(packages = c("fasstr", "EflowStats", "dataRetrieval",
                            "lubridate", "cluster", "factoextra", "NbClust",
                            "sf", "cowplot", "gridGraphics", "stringi",
                            "dendextend", "scico", "tidyverse", "nhdplusTools",
                            "sbtools", "maps", "mapproj", "ranger", "Boruta",
                            "tidymodels", "doParallel", "vip", "gstat", "rlist", 
                            "measures", "fastshap", "shapviz", "pdp"),
               imports = c("fasstr", "EflowStats", "dataRetrieval", 
                           "cluster","factoextra", "NbClust", "dendextend",
                           "tidyverse", "ranger", "Boruta", "tidymodels",
                           "fastshap", "pdp"))

##Create output file directories
dir.create('1_fetch/out', showWarnings = FALSE)
dir.create('1_fetch/out/stationarity_plots', showWarnings = FALSE)
dir.create('1_fetch/out/stationarity_plots/by_quantiles', showWarnings = FALSE)
dir.create('1_fetch/out/stationarity_plots/by_agg_quantiles', showWarnings = FALSE)
dir.create('1_fetch/out/logs', showWarnings = FALSE)
dir.create('1_fetch/out/nhd_plus', showWarnings = FALSE)
dir.create('1_fetch/out/sb', showWarnings = FALSE)
dir.create('1_fetch/out/sb/workdir', showWarnings = FALSE)
dir.create('1_fetch/out/sb/dldir', showWarnings = FALSE)
dir.create('1_fetch/out/sb/data', showWarnings = FALSE)
dir.create('3_cluster/out', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/barplots', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/barplots/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/barplots/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/barplots/by_all_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/barplots/CONUS', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/diagnostics', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/diagnostics/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/diagnostics/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/diagnostics/by_all_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/maps', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/maps/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/maps/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots/maps/by_all_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow/barplots', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow/barplots/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow/barplots/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow/barplots/CONUS', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow/diagnostics', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow/diagnostics/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow/diagnostics/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow/maps', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow/maps/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow/maps/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_noHighVolume', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_noHighVolume/barplots', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_noHighVolume/barplots/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_noHighVolume/barplots/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_noHighVolume/barplots/CONUS', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_noHighVolume/diagnostics', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_noHighVolume/diagnostics/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_noHighVolume/diagnostics/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_noHighVolume/maps', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_noHighVolume/maps/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_noHighVolume/maps/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_freq', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_freq/barplots', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_freq/barplots/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_freq/barplots/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_freq/barplots/CONUS', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_freq/diagnostics', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_freq/diagnostics/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_freq/diagnostics/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_freq/maps', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_freq/maps/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/seasonal_plots_LowFlow_freq/maps/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/raw_metric_plots', showWarnings = FALSE)
dir.create('3_cluster/out/raw_metric_plots/barplots', showWarnings = FALSE)
dir.create('3_cluster/out/raw_metric_plots/barplots/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/raw_metric_plots/barplots/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/raw_metric_plots/diagnostics', showWarnings = FALSE)
dir.create('3_cluster/out/raw_metric_plots/diagnostics/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/raw_metric_plots/diagnostics/by_agg_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/raw_metric_plots/maps', showWarnings = FALSE)
dir.create('3_cluster/out/raw_metric_plots/maps/by_quantiles', showWarnings = FALSE)
dir.create('3_cluster/out/raw_metric_plots/maps/by_agg_quantiles', showWarnings = FALSE)
dir.create('5_EDA/out', showWarnings = FALSE)
dir.create('5_EDA/out/metrics_plots', showWarnings = FALSE)
dir.create('5_EDA/out/feature_plots', showWarnings = FALSE)
dir.create('5_EDA/out/metrics_plots_LowFlow', showWarnings = FALSE)
dir.create('5_EDA/out/metrics_plots_RawClusts', showWarnings = FALSE)
dir.create('6_predict/out', showWarnings = FALSE)
dir.create('6_predict/out/Boruta', showWarnings = FALSE)
dir.create('6_predict/out/vip', showWarnings = FALSE)
dir.create('6_predict/out/hypopt', showWarnings = FALSE)
dir.create('6_predict/out/split_boxplots', showWarnings = FALSE)
dir.create('6_predict/out/pred_obs', showWarnings = FALSE)
dir.create('6_predict/out/multiclass', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High/shap', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High/shap/high', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High/shap/midhigh', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High/dependence', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High/dependence/high', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High/dependence/midhigh', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/Low', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/EcoFlows_High', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/EcoFlows_Low', showWarnings = FALSE)
#No physio predictor
dir.create('6_predict/out/multiclass/High/NoPhysio', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High/NoPhysio/shap', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High/NoPhysio/shap/high', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High/NoPhysio/shap/midhigh', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High/NoPhysio/dependence', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High/NoPhysio/dependence/high', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High/NoPhysio/dependence/midhigh', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High_Raw/NoPhysio', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High_Raw/NoPhysio/shap', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High_Raw/NoPhysio/shap/high', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High_Raw/NoPhysio/shap/midhigh', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High_Raw/NoPhysio/dependence', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High_Raw/NoPhysio/dependence/high', showWarnings = FALSE)
dir.create('6_predict/out/multiclass/High_Raw/NoPhysio/dependence/midhigh', showWarnings = FALSE)

##Load user defined functions
source("1_fetch.R")
source("2_flow_metrics.R")
source("3_cluster.R")
source("4_setup_crossval.R")
source("5_EDA.R")
source("6_predict.R")


###Define parameters common to more than one script

##water year or calendar year.
yearType <- "water"
year_start <- 10

#non-exceedance quantiles for additional metrics - daily flows
NE_quants <- c(seq(0.5, 0.95, 0.05), 0.98, 0.99, 0.995)
NE_quants_low <- c(0.005, 0.01, 0.02, seq(0.05, 0.5, 0.05))

#Seasons to use in season analysis
# matches water year
season_months <- c(10, 11, 12, seq(1, 9, 1))
season_year_start <- season_months[1]
# suggested by Ken for high flows
season_months_high <- c(12, seq(1, 11, 1))
season_year_start_high <- season_months_high[1]



c(p1_targets_list, p2_targets_list, p3_targets_list, p4_targets_list, p5_targets_list, p6_targets_list)
