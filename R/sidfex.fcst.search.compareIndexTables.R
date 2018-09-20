sidfex.fcst.search.compareIndexTables <-
  function(index.1=NULL,index.2=NULL,return.dataframe=TRUE,verbose=FALSE) {

    if (is.character(index.1)) {
      if (exists("rTab")) {rm(rTab)}
      load(index.1)
      index.1 = rTab
      rm(rTab)
    }
    if (!is.data.frame(index.1)) {stop("index.1 must be a data frame or the path to a saved data.frame object named rTab .")}
    index.1 = index.1[order(index.1$File),]

    if (is.character(index.2)) {
      if (exists("rTab")) {rm(rTab)}
      load(index.2)
      index.2 = rTab
      rm(rTab)
    }
    if (!is.data.frame(index.2)) {stop("index.2 must be a data frame or the path to a saved data.frame object named rTab .")}
    index.2 = index.2[order(index.2$File),]

    only.1 = index.1[!(index.1$File %in% index.2$File),]
    only.2 = index.2[!(index.2$File %in% index.1$File),]

    both.1 = index.1[index.1$File %in% index.2$File,]
    both.2 = index.2[index.2$File %in% index.1$File,]

    both.diffcount = rowSums(both.1 != both.2)
    both.ident = both.1[both.diffcount == 0,]
    both.diff.1 = both.1[both.diffcount > 0,]
    both.diff.2 = both.2[both.diffcount > 0,]

    if (verbose) {
      print(paste0("Entries only in index.1: ",nrow(only.1)))
      print(paste0("Entries only in index.2: ",nrow(only.2)))
      print(paste0("Identical entries: ",nrow(both.ident)))
      print(paste0("Entries for same files with differences: ",nrow(both.diff.1)))
    }

    # return lists of files or corresponding data frames
    if (!return.dataframe) {
      return(list(only.1=only.1$File,only.2=only.2$File,both.ident=both.ident$File,both.diff.1=both.diff.1$File,both.diff.2=both.diff.2$File))
    } else {
      return(list(only.1=only.1,only.2=only.2,both.ident=both.ident,both.diff.1=both.diff.1,both.diff.2=both.diff.2))
    }

  }
