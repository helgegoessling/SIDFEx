sidfex.fcst.search.rda2csv <- function(rTab.in, headerlines, indexTable.path=NULL){
  # create and open the file connection
  datafile <- file(paste0(indexTable.path, "indexTable.csv"), open = 'wt')
  # if a header is defined, write it to the file 
  writeLines(headerlines,con=datafile)
  # write the file using the defined function and required addition arguments  
  write.csv(rTab.in, file=datafile, row.names = FALSE)
  close(datafile)
}