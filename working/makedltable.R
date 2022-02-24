#this script makes a table of all lower level (subfolder) sciencebase items with downloads from 1 parent sciencebase id
#table includes title, last updated date, sciencebase id, and parent title
#would be nice to also add things like theme (geology, land use, etc) and 
#possibly other info, but that's for another time
#
#by andrew sekellick
#2/18/2022

library(beepr)
library(sbtools)
library(stringr)

starttime <- Sys.time()

#this is the top level Science Base ID of Mike's database but this should work with any ID
idtostart <- '5669a79ee4b08895842a1d47'

#i did this step as a function to keep things cleaner.
#getbabies returns a list of the next level down of ids from a parent id.
#the "lapply" line keeps only the id from the list of lists returned from item_list_children
#we unlist to make it a vector
getbabies <- function(idin) 
{
  listall <- item_list_children(idin)
  listall2 <- lapply(listall, `[`, c('id'))
  ids <- unlist(listall2)
  return(ids)
}

#start with doing the top level id so we have something to work with.
idlist <- getbabies(idtostart)

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
#if yes, we get the ids of those children with our getbabies function
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
      templist <- getbabies(idN)
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


endtime <- Sys.time()

#how long did it all take.
print(endtime - starttime)

#beep beep
beep(2)