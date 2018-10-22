sidfex.load.index <- function(indexTable.path=NULL) {

  rTab = sidfex.fcst.search.extractFromTable(indexTable.path = indexTable.path, return.dataframe = TRUE)

  return(rTab)

}
