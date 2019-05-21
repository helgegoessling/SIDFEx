sidfex.fcst.search.createIndexTable <-
  function(indexTable.path = NULL, data.path=NULL, do.fromScratch=FALSE, do.saveAddCSV=TRUE,do.print.less=T, checkfileformat=TRUE){

    warning("Function sidfex.fcst.search.createIndexTable is deprecated; use sidfex.fcst.search.createIndex instead")

    indexNames = c("File", "GroupID", "MethodID", "TargetID", "InitYear", "InitDayOfYear", "EnsMemNum", "SubmitYear", "SubmitDayOfYear",
                   "ProcessedYear", "ProcessedDayOfYear", "Delay", "nTimeSteps", "FirstTimeStepYear", "FirstTimeStepDayOfYear", "LastTimeStepYear",
                   "LastTimeStepDayOfYear", "FcstTime")
    # check if specific directory for the fcst data is given, otherwise use a default
    if (is.null(data.path)) {
      no.data.path.fcst=TRUE
      if (file.exists(file.path("~",".SIDFEx"))) {
        source(file.path("~",".SIDFEx"))
        if (exists("data.path.fcst")) {no.data.path.fcst=FALSE
        print(indexTable.path)}
      }
      if (no.data.path.fcst) {
        stop(paste0("With data.path=NULL , data.path.fcst must be specified in a file ~/.SIDFEx as a line like data.path.fcst=..."))
      }
    } else {
      data.path.fcst = data.path
    }
    # check if specific directory for indexList is given, otherwise use a default
    if (is.null(indexTable.path)) {
      no.indexTable.path=TRUE
      if (file.exists(file.path("~",".SIDFEx"))) {
        source(file.path("~",".SIDFEx"))
        if (exists("indexTable.path.in")) {no.indexTable.path=FALSE}
      }
      if (no.indexTable.path) {
        stop(paste0("With indexTable.path=NULL , indexTable.path.in must be specified in a file ~/.SIDFEx as a line like indexTable.path.in=..."))
      }
    } else {
      indexTable.path.in = indexTable.path
    }

    nRowsOld=0  # init. number of rows in indexTable

    # if do.fromScratch==TRUE, we remove the previous version of the indexFile (permanently)
    if (do.fromScratch){
      if (file.exists(file.path(indexTable.path.in, "indexTable.rda"))) {
        print("Delete existing file and rebuild it from scratch!")
        file.remove(file.path(indexTable.path.in, "indexTable.rda"))
      }
    }

    # load if file exists, otherwise create a new one
    if (file.exists(file.path(indexTable.path.in, "indexTable.rda"))) {
      load(file.path(indexTable.path.in, "indexTable.rda"))
      nRowsOld = nrow(rTab)
      rm.frst = FALSE
      headerlines[2] = paste0("Last update on: ", Sys.time())
    }
    else {
      rTab = data.frame(matrix(ncol=length(indexNames), nrow=0))
      colnames(rTab) <- indexNames
      rTab[1,] = indexNames
      rm.frst = TRUE
    }

    # iterate through directories to create entries in indexFile for each submitted forecast
    gid.dirs = list.files(data.path.fcst)
    for (gid in gid.dirs) {
      fdir = paste0(data.path.fcst, "/", gid)
      for (item in list.files(fdir, pattern=".txt")){
        rTab = sidfex.fcst.search.addTableItem(item, data.path = paste0(fdir, "/"), is.open.rTab = TRUE, rTab.in = rTab,
                                               checkfileformat = checkfileformat, do.print.less=do.print.less)
      }
      if (do.print.less) {
        print(paste0("... now done with group ", gid, " ..."))
      }
    }

    if (rm.frst){
      rTab <- rTab[-1, ] # just a remainder of bad R knowledge with respect to the initialization of dataframes:
      headerlines <- c(paste0("File created on: ", Sys.time()), paste0("Last update on: ", Sys.time()))
    }

    # save the file-- maybe we should save this more frequently during the process or
    # at least keep an old version of the indexTable until this one is written on disk...?
    save(rTab, headerlines, file=file.path(indexTable.path.in, "indexTable.rda"))

    # and convert file to csv if desired
    if (do.saveAddCSV) {sidfex.fcst.search.rda2csv(rTab.in = rTab, headerlines = headerlines, indexTable.path = indexTable.path.in)}

    print(paste0("The file now contains entries for ", nrow(rTab), " submitted forecasts, with ", nrow(rTab)-nRowsOld, " item(s) added. See ya!"))

    return(1)
  }
