library(tidyverse)
library(readxl)

setwd("C:/Users/jsmith/OneDrive - DOI/Shared Documents - FHWA/General/Data")

#all currently available SB IDs
sb_table <- read_csv(file = 'sbdownloadtable.csv', col_types = 'cccT')

#variables of interest
NHD_vars <- read_excel(path = 'FHWA-NHDVariableList.xlsx', sheet = 'FY22-FHWA')
#gather the SB IDs listed in NHD_vars
sbid <- vector('character')
for (i in 1:nrow(NHD_vars)){
  sbid[i] <- strsplit(x = NHD_vars$Science.Base.Link[i], split = 'item/')[[1]] %>% 
    last()
}
rm(i)

#add ID to NHD variable table
NHD_vars$sbid <- sbid

#Reduce to only unique links to check which are missing
sbid <- unique(sbid)

#get sb IDs that are not in the full list - these have changed to other links
missing <- sbid[!(sbid %in% sb_table$id)]
missing_vars <- NHD_vars[NHD_vars$sbid %in% missing,]

#additional IDs to get missing values
#monthly precip - 3 - I think we'll need to use Lauren's processing script to get the monthly averages.
#monthly temp - 3 - I think we'll need to use Lauren's processing script to get the monthly averages.
retain_ids <- c("5734acafe4b0dae0d5de622d", 
               "5730f062e4b0dae0d5db1fbe", 
               "573362bce4b0dae0d5dd6193", 
               "574ddc8ce4b07e28b66901b3",
               "574f3e86e4b0ee97d51abf31",
               "574f238fe4b0ee97d51a8916"
)


retain_sb_table <- sb_table[sb_table$id %in% c(sbid, retain_ids),]

write_csv(retain_sb_table, file = 'sbdownloadtable_reduced.csv')
