sidfex.fcst.search.createIndex <-
  function(indexTable.path = NULL, data.path=NULL, do.fromScratch=FALSE, do.checktime=TRUE, do.saveAddCSV=TRUE,do.print.less=T, checkfileformat=TRUE, do.manipulate=TRUE, add.LatLon=FALSE, not.save.but.return.index=FALSE, allow.unprocessed=FALSE){
    indexNames = c("File", "GroupID", "MethodID", "TargetID", "InitYear", "InitDayOfYear", "EnsMemNum", "SubmitYear", "SubmitDayOfYear",
                   "ProcessedYear", "ProcessedDayOfYear", "Delay", "nTimeSteps", "FirstTimeStepYear", "FirstTimeStepDayOfYear", "LastTimeStepYear",
                   "LastTimeStepDayOfYear", "FcstTime", "EnsParentFile", "EnsSize")
    if (add.LatLon) {
      indexNames = c(indexNames,"InitLat","InitLon","FirstTimeStepLat","FirstTimeStepLon","LastTimeStepLat","LastTimeStepLon")
    }
    # check if specific directory for the fcst data is given, otherwise use a default
    if (is.null(data.path)) {
      no.data.path.fcst=TRUE
      if (file.exists(file.path("~",".SIDFEx"))) {
        source(file.path("~",".SIDFEx"))
        if (exists("data.path.fcst")) {
          no.data.path.fcst=FALSE
          #print(indexTable.path)
          }
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

    # load existing index if !do.fromScratch and file exists, otherwise create a new one
    if (!do.fromScratch && file.exists(file.path(indexTable.path.in, "indexTable.rda"))) {
      load(file.path(indexTable.path.in, "indexTable.rda"))
      nRowsOld = nrow(rTab)
      missng = rep(TRUE,nRowsOld)
    } else {
      rTab = data.frame(matrix(ncol=length(indexNames), nrow=0))
      colnames(rTab) = indexNames
      do.fromScratch = TRUE
    }

    nRows = nRowsOld
    nModified = 0

    # iterate through directories to create entries in indexFile for each submitted forecast
    gid.dirs = list.files(data.path.fcst)
    for (gid in gid.dirs) {
      fdir = paste0(data.path.fcst, "/", gid)
      for (item in list.files(fdir, pattern=".txt")){

        fid <- substring(item, 1, nchar(item)-4)
        update.row = FALSE
        if (!do.fromScratch) {
          match.ind = match(fid, rTab$File)
          if (!is.na(match.ind)) {
            missng[match.ind] = FALSE
            if (do.checktime) {
              readtime.res = scan(file.path(fdir, item),n=4,what="character",quiet=TRUE)
              if (length(readtime.res) > 3 && as.numeric(readtime.res[2]) == rTab$SubmitYear[match.ind] && as.numeric(readtime.res[4]) == rTab$SubmitDayOfYear[match.ind]) {
                next
              } else {
                update.row = TRUE
                nModified = nModified + 1
              }
            } else {
              next
            }
          }
        }

        dat = sidfex.read.fcst(files = file.path(fdir, item), checkfileformat = checkfileformat, ens.merge=FALSE)$res.list[[1]]
        if (is.character(dat) && dat[1] != "No file format violations found.") {
          print(paste0("File format violation in ",file.path(fdir, item)," :"))
          print(dat)
          stop("Trying to add item from file with invalid format.")
        }

        ### generate items for table
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
        if (is.null(sy)) {
          if (allow.unprocessed) {
            sy  = NA
            sdoy= NA
            py  = NA
            pdoy= NA
            del = NA
          } else {
            stop(paste0("file ",fid," has not been processed; consider running sidfex.fcst.search.createIndex() with allow.unprocessed=TRUE"))
          }
        } else {
          del = as.double(as.Date(sdoy-1, origin = paste0(as.character(sy),"-01-01")) -
                          as.Date(idoy-1, origin = paste0(as.character(iy),"-01-01")))     # calculating the delay (difference submit time <-> initial time) in days
        }
        # ensemble parent and size, to be added later by analysing the whole index
        epf = "NA"
        es = NA
        if (add.LatLon) {
          ila = dat$InitLat
          ilo = dat$InitLon
          fla = dat$FirstLat
          flo = dat$FirstLon
          lla = dat$LastLat
          llo = dat$LastLon
        }

        # add row to the dataframe
        if (add.LatLon) {
          add.row = list(fid, gid, mid, tid, iy, idoy, emn, sy, sdoy, py, pdoy, del, nt,
                         fy, fdoy, ly, ldoy, per, epf, es, ila, ilo, fla, flo, lla, llo)
        } else {
          add.row = list(fid, gid, mid, tid, iy, idoy, emn, sy, sdoy, py, pdoy, del, nt,
                         fy, fdoy, ly, ldoy, per, epf, es)
        }
        if (update.row) {
          rTab[match.ind,] = add.row
        } else {
          nRows = nRows + 1
          rTab[nRows,] = add.row
        }

      }
      if (do.print.less) {
        print(paste0("... now done with group ", gid, " ..."))
      }
    }

    # remove previous entries with no corresponding file
    if (!do.fromScratch) {
      nAdded = nRows - nRowsOld
      nRemoved = sum(missng)
      rTab = rTab[c(!missng,rep(TRUE,nAdded)),]
      nRows = nrow(rTab)
    }

    # order index
    rTab = rTab[order(rTab$GroupID,rTab$MethodID,rTab$TargetID,rTab$InitYear,rTab$InitDayOfYear,rTab$EnsMemNum),]

    # derive ensemble information
    FileWithoutEMN = paste(rTab$GroupID,rTab$MethodID,rTab$TargetID,paste0(rTab$InitYear,"-",rTab$InitDayOfYear),sep="_")
    for (parentX in FileWithoutEMN) {
      inds = which(FileWithoutEMN == parentX)
      rTab$EnsSize[inds] = length(inds)
      rTab$EnsParentFile[inds] = rTab$File[inds][which.max(rTab$nTimeSteps[inds])]
    }

    # manipulate index to account for special cases; currently only for nrl001 lagged-initial-time ensembles
    if (do.manipulate) {
      rTab = sidfex.fcst.search.manipulateIndex(rTab)
    }

    if (do.fromScratch) {
      headerlines <- c(paste0("File created on: ", Sys.time()), paste0("Last update on: ", Sys.time()))
    } else {
      headerlines[2] = paste0("Last update on: ", Sys.time())
    }

    if (not.save.but.return.index) {

      if (do.fromScratch) {
        print(paste0("The index now contains entries for ", nRows, " submitted forecasts."))
      } else {
        print(paste0("The index now contains entries for ", nRows, " submitted forecasts. Compared to the previous index, ",nAdded,
                     " item(s) have been added, ",nModified," item(s) have been modified, and ",nRemoved," item(s) have been removed."))
      }
      return(rTab)

    } else {

      save(rTab, headerlines, file=file.path(indexTable.path.in, "indexTable.rda"))
      if (do.saveAddCSV) {sidfex.fcst.search.rda2csv(rTab.in = rTab, headerlines = headerlines, indexTable.path = indexTable.path.in)}
      if (do.fromScratch) {
        print(paste0("The file now contains entries for ", nRows, " submitted forecasts."))
      } else {
        print(paste0("The file now contains entries for ", nRows, " submitted forecasts. Compared to the previous index, ",nAdded,
                     " item(s) have been added, ",nModified," item(s) have been modified, and ",nRemoved," item(s) have been removed."))
      }
      return(1)

    }

  }
