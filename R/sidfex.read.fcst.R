sidfex.read.fcst <- function(files=NULL,data.path=NULL,GroupID=NULL,MethodID=NULL,TargetID=NULL,InitYear=NULL,InitDayOfYear=NULL,EnsMemNum=NULL,ens.merge=TRUE,checkfileformat=TRUE,verbose=TRUE,speed.fix=TRUE,speed.fix.max=80,timedupli.fix=TRUE) {

  if (is.null(data.path)) {
    no.data.path.fcst=TRUE
    if (file.exists(file.path("~",".SIDFEx"))) {
      source(file.path("~",".SIDFEx"))
      if (exists("data.path.fcst")) {no.data.path.fcst=FALSE}
    }
    if (no.data.path.fcst) {
      warning(paste0("Depending on whether or not the type of input given in 'files' needs an additional forecast data path, with data.path=NULL , data.path.fcst might need to be specified in a file ~/.SIDFEx as a line like data.path.fcst=..."))
    }
  } else {
    data.path.fcst = data.path
  }

  indexBased = FALSE
  index = NULL
  if (is.null(files)) {
    if (is.null(GroupID)) {GroupID = "*"}
    if (is.null(MethodID)) {MethodID = "*"}
    if (is.null(TargetID)) {TargetID = "*"}
    if (is.null(InitYear)) {InitYear = "*"}
    if (is.null(InitDayOfYear)) {InitDayOfYear = "*"}
    if (is.null(EnsMemNum)) {EnsMemNum = "*"}
    for (gid in GroupID) {
      for (mid in MethodID) {
        for (tid in TargetID) {
          for (iy in InitYear) {
            for (idoy in InitDayOfYear) {
              for (emn in EnsMemNum) {
                filestr = paste0(paste0(gid,"_",mid,"_",tid,"_",iy,"-",idoy,"_",emn),".txt")
                files = c(files,system(paste0("find ",file.path(data.path.fcst,GroupID)," -name ",filestr),intern=TRUE))
              }
            }
          }
        }
      }
    }
  } else {
    if (is.character(files)) {
      if (length(files) == 1 && dir.exists(files)) {
        lastchar = substr(files,nchar(files),nchar(files))
        if (lastchar != "/") {files = paste0(files,"/")}
        files = paste0(files,system(paste0("ls ",files),intern=TRUE))
      } else {
        fl1 = files[1]
        if (nchar(fl1) < 4) {stop("The strings in 'files' are too short!")}
        sbstr = substr(fl1,start=nchar(fl1)-3,stop=nchar(fl1))
        if (sbstr != ".txt") {
          gids = strsplit(files,split="_")
          gids = as.character(as.data.frame(gids,stringsAsFactors=FALSE)[1,])
          files = file.path(data.path.fcst,gids,paste0(files,".txt"))
        }
      }
    } else {
      indexBased = TRUE
      index = files
      files = file.path(data.path.fcst,files$GroupID,paste0(index$File,".txt"))
    }
  }

  if (ens.merge && !indexBased) {
    stop("'ens.merge=TRUE' works only if 'files' is a SIDFEx forecast (sub-)index.")
  }

  N = length(files)
  if (N == 0) {
    warning("No files found.")
    return(NULL)
  }
  res.list = list()

  for (i in 1:N) {

    fl = files[[i]]
    res = list()
    res$fl = fl

    if (!file.exists(fl)) {
      res[[2]] = "File does not exist."
      res.list[[i]] = res
      next
    }

    if (checkfileformat) {
      checkres = sidfex.checkfileformat(filepathnames=fl)
      res$checkfileformat.result = checkres
      if (checkres != "No file format violations found.") {
        res.list[[i]] = checkres
        next
      }
    }

    filecont = scan(fl,sep="\n",what="character",quiet=TRUE)
    Nr = length(filecont)

    nrGroupID = 0
    GroupIDfound = FALSE
    while (nrGroupID < Nr && !GroupIDfound) {
      nrGroupID = nrGroupID + 1
      rowcont = filecont[nrGroupID]
      if (substr(rowcont,1,7) == "GroupID") {
        GroupIDfound = TRUE
        break
      }
    }

    if (nrGroupID < 6) {
      res$SubmitYear = NULL
      res$SubmitDayOfYear = NULL
      res$ProcessedYear = NULL
      res$ProcessedDayOfYear = NULL
    } else {
      row.flds = strsplit(filecont[nrGroupID-5],split=" ",fixed=TRUE)[[1]]
      res$SubmitYear = as.integer(row.flds[row.flds != ""][2])
      row.flds = strsplit(filecont[nrGroupID-4],split=" ",fixed=TRUE)[[1]]
      res$SubmitDayOfYear = as.numeric(row.flds[row.flds != ""][2])
      row.flds = strsplit(filecont[nrGroupID-3],split=" ",fixed=TRUE)[[1]]
      res$ProcessedYear = as.integer(row.flds[row.flds != ""][2])
      row.flds = strsplit(filecont[nrGroupID-2],split=" ",fixed=TRUE)[[1]]
      res$ProcessedDayOfYear = as.numeric(row.flds[row.flds != ""][2])
    }

    row.flds = strsplit(filecont[nrGroupID+0],split=" ",fixed=TRUE)[[1]]
    res$GroupID = row.flds[row.flds != ""][2]
    row.flds = strsplit(filecont[nrGroupID+1],split=" ",fixed=TRUE)[[1]]
    res$MethodID = row.flds[row.flds != ""][2]
    row.flds = strsplit(filecont[nrGroupID+2],split=" ",fixed=TRUE)[[1]]
    res$TargetID = row.flds[row.flds != ""][2]
    row.flds = strsplit(filecont[nrGroupID+3],split=" ",fixed=TRUE)[[1]]
    res$InitYear = as.integer(row.flds[row.flds != ""][2])
    row.flds = strsplit(filecont[nrGroupID+4],split=" ",fixed=TRUE)[[1]]
    res$InitDayOfYear = as.numeric(row.flds[row.flds != ""][2])
    row.flds = strsplit(filecont[nrGroupID+5],split=" ",fixed=TRUE)[[1]]
    res$InitLat = as.numeric(row.flds[row.flds != ""][2])
    row.flds = strsplit(filecont[nrGroupID+6],split=" ",fixed=TRUE)[[1]]
    res$InitLon = as.numeric(row.flds[row.flds != ""][2])
    row.flds = strsplit(filecont[nrGroupID+7],split=" ",fixed=TRUE)[[1]]
    res$EnsMemNum = as.integer(row.flds[row.flds != ""][2])

    dat = read.table(fl,header=TRUE,skip=nrGroupID+8)
    dat$DaysLeadTime = sidfex.ydoy2reltime(dat$Year,dat$DayOfYear,res$InitYear,res$InitDayOfYear)

    if (timedupli.fix) {
      time.dupli = duplicated(dat$DaysLeadTime)
      if (any(time.dupli)) {
        warning(paste0("There are ",sum(time.dupli)," time duplicates in ",fl," , removing duplicates without checking coordinates!"))
        dat = dat[which(!time.dupli),]
      }
    }

    if (speed.fix) {
      lola = sidfex.trajectory.fix(reltime = dat$DaysLeadTime, lon = dat$Lon, lat = dat$Lat, speed.max = speed.fix.max)
      dat$Lon = lola$lon
      dat$Lat = lola$lat
    }

    Nt = nrow(dat)
    res$Ntimesteps = Nt
    res$FirstYear = dat[1,1]
    res$FirstDayOfYear = dat[1,2]
    res$FirstLat = dat[1,3]
    res$FirstLon = dat[1,4]
    res$LastYear = dat[Nt,1]
    res$LastDayOfYear = dat[Nt,2]
    res$LastLat = dat[Nt,3]
    res$LastLon = dat[Nt,4]
    res$DaysForecastLength = dat$DaysLeadTime[Nt]
    res$data = dat

    res.list[[i]] = res

  }

  if (ens.merge) {

    require(spheRlab)

    index.merged = index[index$File==index$EnsParentFile,]
    if (length(unique(index$EnsParentFile)) != length(unique(index.merged$EnsParentFile)) ||
        any(unique(index$EnsParentFile) != unique(index.merged$EnsParentFile))) {
      warning("One or more ensembles do not contain their parent; defining new parent based on lastest initial time, then largest number of time steps, then lowest EnsMemNum")
      for (parentX in unique(index$EnsParentFile)) {
        inds = which(index$EnsParentFile == parentX)
        index$EnsParentFile[inds] = index$File[inds][order(-1*index$InitYear[inds],-1*index$InitDayOfYear[inds],-1*index$nTimeSteps[inds])[1]]
      }
      index.merged = index[index$File==index$EnsParentFile,]
    }

    nMerged = nrow(index.merged)
    index.merged$MergedEnsSize = as.integer(rep(NA,nMerged))
    index.merged$MergedEnsMemNum = as.character(rep(NA,nMerged))

    rl.merged = res.list[index$File==index$EnsParentFile]

    warn.remaptime = FALSE
    for (i in 1:nMerged) {

      inds = which(index$EnsParentFile==index.merged$EnsParentFile[i])
      es = length(inds)
      index.merged$MergedEnsSize[i] = es
      index.merged$MergedEnsMemNum[i] = paste(index$EnsMemNum[inds],collapse=",")

      if (es <= 1) {next}

      rl.merged[[i]]$fl = unlist(lapply(res.list[inds], function(x) x$fl))
      if (checkfileformat) {
        rl.merged[[i]]$checkfileformat.result = unlist(lapply(res.list[inds], function(x) x$checkfileformat.result))
      }
      rl.merged[[i]]$EnsMemNum = unlist(lapply(res.list[inds], function(x) x$EnsMemNum))

      dat = matrix(nrow=nrow(rl.merged[[i]]$data), ncol=2*es)
      dat.colnames = as.character(rep(NA,2*es))

      for (ie in 1:es) {

        Nts.min = min(rl.merged[[i]]$Ntimesteps, res.list[[inds[ie]]]$Ntimesteps)
        child.init.Year = res.list[[inds[ie]]]$InitYear
        child.init.DayOfYear = res.list[[inds[ie]]]$InitDayOfYear
        parent.init.Year = rl.merged[[i]]$InitYear
        parent.init.DayOfYear = rl.merged[[i]]$InitDayOfYear
        if (any(res.list[[inds[ie]]]$data$Year[1:Nts.min] != rl.merged[[i]]$data$Year[1:Nts.min] ||
                res.list[[inds[ie]]]$data$DayOfYear[1:Nts.min] != rl.merged[[i]]$data$DayOfYear[1:Nts.min]) ||
            child.init.Year != parent.init.Year || child.init.DayOfYear != parent.init.DayOfYear) {
          warn.remaptime = TRUE
          child.init.rt = sidfex.ydoy2reltime(child.init.Year,child.init.DayOfYear,
                                              parent.init.Year,parent.init.DayOfYear)
          child.rt = sidfex.ydoy2reltime(res.list[[inds[ie]]]$data$Year,res.list[[inds[ie]]]$data$DayOfYear,
                                              parent.init.Year,parent.init.DayOfYear)
          child.lat = res.list[[inds[ie]]]$data$Lat
          child.lon = res.list[[inds[ie]]]$data$Lon
          if (child.rt[1] > child.init.rt) {
            child.rt = c(child.init.rt,child.rt)
            child.lat = c(res.list[[inds[ie]]]$InitLat,child.lat)
            child.lon = c(res.list[[inds[ie]]]$InitLon,child.lon)
          } else if (child.rt[1] < child.init.rt) {
            stop(paste0("First time of data in ",res.list[[inds[ie]]]$fl," earlier than corresponding initial time."))
          }
          parent.rt = sidfex.ydoy2reltime(rl.merged[[i]]$data$Year,rl.merged[[i]]$data$DayOfYear,
                                          parent.init.Year,parent.init.DayOfYear)
          remap.res = sl.trajectory.remaptime(child.rt,child.lat,child.lon,parent.rt,verbose=verbose)
          dat[,2*ie-1] = remap.res$Lat
          dat[,2*ie] = remap.res$Lon
          remap.init.res = sl.trajectory.remaptime(child.rt,child.lat,child.lon,0,verbose=verbose)
          rl.merged[[i]]$MergedInitLat[ie+1] = remap.init.res$Lat
          rl.merged[[i]]$MergedInitLon[ie+1] = remap.init.res$Lon
        } else {
          dat[1:Nts.min,2*ie-1] = res.list[[inds[ie]]]$data$Lat[1:Nts.min]
          dat[1:Nts.min,2*ie] = res.list[[inds[ie]]]$data$Lon[1:Nts.min]
          rl.merged[[i]]$MergedInitLat[ie+1] = res.list[[inds[ie]]]$InitLat
          rl.merged[[i]]$MergedInitLon[ie+1] = res.list[[inds[ie]]]$InitLon
        }
        dat.colnames[(2*ie-1):(2*ie)] = paste0(c("Lat","Lon"),res.list[[inds[ie]]]$EnsMemNum)

      }

      ensmeans.lat = rep(NA,nrow(dat))
      ensmeans.lon = rep(NA,nrow(dat))
      notna = which(rowSums(!is.na(dat)) > 0)
      for (j in notna) {
        em = sl.barycenter(lon = dat[j,seq(2,2*es,2)], lat = dat[j,seq(1,2*es,2)])
        ensmeans.lat[j] = em$lat
        ensmeans.lon[j] = em$lon
      }

      datX = as.data.frame(matrix(ncol=2*es+5, nrow=nrow(dat)))
      colnames(datX) = c(colnames(rl.merged[[i]]$data),dat.colnames)
      datX[,c(1,2,5)] = rl.merged[[i]]$data[,c(1,2,5)]
      datX[,3] = ensmeans.lat
      datX[,4] = ensmeans.lon
      datX[,5+(1:(2*es))] = dat

      rl.merged[[i]]$data = datX

      rl.merged[[i]]$FirstLat = ensmeans.lat[1]
      rl.merged[[i]]$FirstLon = ensmeans.lon[1]
      rl.merged[[i]]$LastYear = rl.merged[[i]]$data$Year[tail(notna,1)]
      rl.merged[[i]]$LastDayOfYear = rl.merged[[i]]$data$DayOfYear[tail(notna,1)]
      rl.merged[[i]]$LastLat = rl.merged[[i]]$data$Lat[tail(notna,1)]
      rl.merged[[i]]$LastLon = rl.merged[[i]]$data$Lon[tail(notna,1)]

      em.init = sl.barycenter(lon=rl.merged[[i]]$MergedInitLon[2:(es+1)],
                              lat=rl.merged[[i]]$MergedInitLat[2:(es+1)])
      rl.merged[[i]]$MergedInitLat[1] = em.init$lat
      rl.merged[[i]]$MergedInitLon[1] = em.init$lon

    }

    if (warn.remaptime) {
      warning("Some forecast ensemble members have been remapped temporally to match the parent forecast time axis")
    }

    res.list = rl.merged
    index = index.merged

  }

  return(list(ens.merge=ens.merge,res.list=res.list,index=index))

}
