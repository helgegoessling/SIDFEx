indexNames = c("File", "GroupID", "MethodID", "TargetID", "InitYear", "InitDayOfYear", "EnsMemNum", "SubmitYear", "SubmitDayOfYear",
               "ProcessedYear", "ProcessedDayOfYear", "Delay", "nTimeSteps", "FirstTimeStepYear", "FirstTimeStepDayOfYear", "LastTimeStepYear", 
               "LastTimeStepDayOfYear", "FcstTime")

sidfex.fcst.search.addTableItem <-
  function (filename, data.path=NULL, indexTable.path=NULL, is.open.rTab=FALSE, rTab.in=NULL) {
    # check if specific directory for forecasts is given, otherwise use default 
    if (is.null(data.path)) {
      no.data.path.fcst=TRUE
      if (file.exists(file.path("~",".SIDFEx"))) {
        source(file.path("~",".SIDFEx"))
        if (exists("data.path.fcst")) {no.data.path.fcst=FALSE}
      }
      if (no.data.path.fcst) {
        stop(paste0("With data.path=NULL , data.path.fcst must be specified in a file ~/.SIDFEx as a line like data.path.fcst=..."))
      }
    } else {
      data.path.fcst = data.path
    }
    
    # check if specific directory for indexList is given, otherwise use default 
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
    
    # read forecast data    
    fcst = sidfex.read.fcst(filepathnames = paste0(data.path.fcst, filename))
    dat = fcst$res.list[[1]]

    # generate items for table 
    fid <- substring(filename, 1, nchar(filename)-4)
    # contained in filename
    gid = dat$GroupID 
    mid = dat$MethodID
    tid = dat$TargetID
    iy  = dat$InitYear
    idoy= dat$InitDayOfYear
    emn = dat$EnsMemNum
    
    # contained in header
    sy  = dat$SubmitYear
    sdoy= dat$SubmitDayOfYear
    py  = dat$ProcessedYear
    pdoy= dat$ProcessedDayOfYear
    
    # derived from data
    nt  = dat$Ntimesteps
    fy  = dat$FirstYear
    fdoy= dat$FirstDayOfYear
    ly  = dat$LastYear
    ldoy= dat$LastDayOfYear
    per = dat$DaysForecastLength
    del = as.double(as.Date(sdoy-1, origin = paste0(as.character(sy),"-01-01")) - 
                      as.Date(idoy-1, origin = paste0(as.character(iy),"-01-01")))     # calculating the delay (difference submit time <-> initial time) in days
    
    # create the row that will be added to the dataframe    
    outvec <- list(fid, gid, mid, tid, iy, idoy, emn, sy, sdoy, py, pdoy, del, nt, fy, fdoy, ly, ldoy, per)
    names(outvec) <- indexNames
    
    filenameInd = paste0(indexTable.path.in, "indexTable.rda")
    
    # open table(s) if exists, otherwise create a new one and insert first row
    if (file.exists(paste0(indexTable.path.in, "indexTable.rda"))) {
      
      # for the case that the table is not input from an overarching function that uses this function here,
      # load the table and headerlines from the given directory
      if (!is.open.rTab) {load(paste0(indexTable.path.in, "indexTable.rda"))} else {rTab = rTab.in}
      
      if (fid %in% rTab$File) {
        print(paste0("Entry for ", filename, " already exists in indexFile, aborted procedure."))
      } else {
        rTab = rbind(rTab, outvec)
        print(paste0("Appended data of file ", filename, " to indexFile."))
        if (!is.open.rTab) {
          headerlines[2] = paste0("Last update on: ", Sys.time())
          save(rTab, headerlines, file=filenameInd)
        }
      }
    }
    # if a new indexfile is created, we have to create a new header as well
    else {
      print("Warning, creating a new indexFile, ... if you didn't intend to do that, well, you just did.")
      rTab = data.frame(matrix(ncol=length(indexNames), nrow=0))
      
      headerlines <- c(paste0("File created on: ", Sys.time()), paste0("Last update on: ", Sys.time())) 
      
      colnames(rTab) <- indexNames
      rTab[1,] = outvec
      print(paste0("Appended data of file ", filename, " to indexFile."))
      
      save(rTab, headerlines, file=filenameInd)
    }
    
    if(is.open.rTab){return(rTab)}
    else {return(0)}
  }