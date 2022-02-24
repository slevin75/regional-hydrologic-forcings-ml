#this script uses the download table from the makedltable script to download all the data
#from that list of ids.
#
#it also merges the downloaded data to a list of user inputed comids (possibly gage locations or a study area)
#the script downloads each file to the download directory, copies to work directory (unzips if necessary),
#deletes the download, merges data to list of comids, and deletes the copied files.
#it does take some time due to size of downloads.
#there's a lot of handling of different file types. each download item can be different (.txt, .zips, zips of folders)
#that part is a good chunk of the code
#
#if using mike's database, i highly suggest removing rows from the makedltable result to speed up
#processing time. it takes a while (8 hours) to download everything.
#
#by andrew sekellick
#2/18/2022

library(beepr)
library(sbtools)
library(stringr)

starttime <- Sys.time()

#import list of gages
gages <- read.csv("E://FHWA//Rarea//rsbtest//gages.csv", header = TRUE)

#making a copy to work with
data_at_gages <- gages

#set download and work area
dldir <- "E://FHWA//Rarea//downloadarea"
workdir <- "E://FHWA//Rarea//workdir"

#the main loop.
#for each item in the download table, we download everything, then loop through everything we downloaded
#first we cehck the extension of what we downloaded. toss the xml (for now, maybe could use that later).
#copy or unzip what we need to the work area.
#we have another loop to go through what we copied.
#that loop does handle situations where unzipped a folder containing more files using list.files with recursive
#and also things like .txt vs .TXT
#then we read it into R, get rid of the nodata columns, merge to gage list, and delete the working file and directory.

itemfails <- "1st"

#for (row in 1:nrow(tabledl))
#the line below this one is for testing a smaller amount of ids
for (row in 20:21)
{
  item <- tabledl[row, 'id']
  print(row)
  print(item)
  item_file_download(item, dest_dir = dldir, overwrite_file = TRUE)
  files <- list.files(path = dldir, pattern=NULL, full.names = TRUE)
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
  }
  filestomerge <- c(list.files(workdir, ".txt", recursive=TRUE, full.names=TRUE, include.dirs=TRUE),list.files(workdir, ".csv", recursive=TRUE, full.names=TRUE, include.dirs=TRUE),list.files(workdir, ".TXT", recursive=TRUE, full.names=TRUE, include.dirs=TRUE),list.files(workdir, ".CSV", recursive=TRUE, full.names=TRUE, include.dirs=TRUE))
  for (filem in filestomerge)
  {
    print(filem)
    success = "yes"
    tryCatch(tempfile <- read.csv(filem, header = TRUE), error = function(e) {success <<- "no"})
    print(success)
    names(tempfile) <- toupper(names(tempfile))
    if(!"COMID" %in% colnames(tempfile))
    {
      success == "no"
    }
    if (success == "no")
    {
      itemfails <- c(itemfails, filem)
    }
    if (success == "yes")
    {
    try(tempfile <- subset(tempfile, select =-c(NODATA)), silent = TRUE)
    try(tempfile <- subset(tempfile, select =-c(CAT_NODATA)), silent = TRUE)
    try(tempfile <- subset(tempfile, select =-c(ACC_NODATA)), silent = TRUE)
    try(tempfile <- subset(tempfile, select =-c(TOT_NODATA)), silent = TRUE)
    data_at_gages <- merge(data_at_gages, tempfile, by = "COMID", all.x = TRUE)
    }
    file.remove(filem)
  }
  f <- list.files(workdir)
  unlink(file.path(workdir,f), recursive=TRUE)
}

colnames(itemfails) <- c('filename')
itemfails <- itemfails[-1]

endtime <- Sys.time()

#how long did it all take.
print(endtime - starttime)

#beep beep
beep(2)