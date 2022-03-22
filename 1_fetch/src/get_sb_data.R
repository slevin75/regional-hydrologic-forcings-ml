get_babies <- function(idin) {
  listall <- item_list_children(idin, limit = 999)
  listall2 <- lapply(listall, `[`, c('id'))
  ids <- unlist(listall2)
  return(ids)
}

make_dl_table <- function(idtostart, outdir) {
  
  #start with doing the top level id so we have something to work with.
  idlist <- get_babies(idtostart)
  
  #these are our 2 working lists for our loop
  #the = "blank" thing is maybe a little lazy, but i take care of it later.
  parlist <- idlist
  dllist <- "blank"
  
  #getting ready for our loop
  end <- FALSE
  count <- 1
  
  #the main stuff
  #the first if just ends the loop when you go past the end of the parlist
  #otherwise, we test the id in parlist. We use item_get_fields to check if the id has children.
  #if yes, we get the ids of those children with our get_babies function
  #and we add them to the end of parlist so we can check those ids later in our loop
  #if the id has no children, that means its an id with download links.
  #and we add them to our dllist.
  while (end == FALSE)
  {
    if (is.na(parlist[count]) == TRUE)
    {
      end <- TRUE
      print("we are done")
    }
    if (is.na(parlist[count]) == FALSE)
    {
      idN <- parlist[count]
      print(paste("trying", idN))
      test <- item_get_fields(idN,'hasChildren')
      if (test == TRUE)
      {
        templist <- get_babies(idN)
        parlist <- c(parlist, templist)
        print("adding to parlist")
      }
      if (test == FALSE)
      {
        print("adding one to dllist")
        dllist <- c(dllist, idN)
      }
      count <- count + 1
    }
  }
  
  #this gets rid of our first "blank" entry
  dllist <- dllist[-1]
  
  #resetting count for the next loop
  count <- 1
  
  #this loop just gets a parentid, title, and last modified date for each download id to add back to our dllist
  #just uses item_get_fields
  #i made this loop in a way that did not need my lazy "blank" first entry
  for (x in dllist)
  {
    parentid <- item_get_fields(x, 'parentId')
    parenttitle <- item_get_fields(parentid, 'title')
    dllisttitle <- item_get_fields(x, 'title')
    dllistdate <- item_get_fields(x, 'provenance')
    if (count == 1)
    {
      tabledl <- c(x, parenttitle, dllisttitle, dllistdate$lastUpdated)
    }
    if (count == 2)
    {
      tableadd <- c(x, parenttitle, dllisttitle, dllistdate$lastUpdated)
      tabledl <- rbind(tabledl, tableadd)
    }
    count <- 2
  }
  
  #renaming some columns and getting rid of that ugly first column
  colnames(tabledl) <- c('id', 'parent_title', 'title', 'lastUpdated')
  rownames(tabledl) <- NULL
  
  #if you would like to export the table as csv
  filepath <- file.path(outdir, paste0("sb_dl_table.csv"))
  write.csv(tabledl,filepath, row.names = FALSE)
  return(filepath)
}

download_children <-function(sites, dldir, workdir, outdir, out_file_name, table_sb_dl) {
  #making a copy to work with
  data_at_sites <- sites
  itemfails <- "1st"
  table_sb_dl <- read_csv(table_sb_dl, 
                          col_types = cols(id = col_character(), 
                                           parent_title = col_character(), 
                                           title = col_character(), 
                                           lastUpdated = col_character()))
  
  for (row in 1:nrow(table_sb_dl))
  {
    item <- as.character(table_sb_dl[row, 'id'])
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
  filepath1 <- file.path(outdir, paste0("sb_landscapedata_", out_file_name))
  write.csv(data_at_sites, filepath1, row.names = FALSE)
  filepath2 <- file.path(outdir, paste0("sb_itemfails_", out_file_name))
  write.csv(itemfails, filepath2, row.names = FALSE)
  return_df = c(filepath1, filepath2)
  return(return_df)
}