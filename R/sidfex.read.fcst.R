sidfex.read.fcst <- function(filepathnames=NULL,data.path=NULL,GroupID=NULL,MethodID=NULL,TargetID=NULL,InitYear=NULL,InitDayOfYear=NULL,EnsMemNum=NULL,ens.merge=TRUE,checkfileformat=TRUE) {

  if (is.null(filepathnames)) {
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
                filepathnames = c(filepathnames,system(paste0("find ",file.path(data.path.fcst,GroupID)," -name ",filestr),intern=TRUE))
              }
            }
          }
        }
      }
    }
  } else {
    if (length(filepathnames) == 1 && dir.exists(filepathnames)) {
      lastchar = substr(filepathnames,nchar(filepathnames),nchar(filepathnames))
      if (lastchar != "/") {filepathnames = paste0(filepathnames,"/")}
      filepathnames = paste0(filepathnames,system(paste0("ls ",filepathnames),intern=TRUE))
    }
  }

  N = length(filepathnames)
  if (N == 0) {
    warning("No files found.")
    return(NULL)
  }
  res.list = list()

  for (i in 1:N) {

    filepathname = filepathnames[[i]]
    res = list()
    res$filepathname = filepathname

    if (!file.exists(filepathname)) {
      res[[2]] = "File does not exist."
      res.list[[i]] = res
      next
    }

    if (checkfileformat) {
      checkres = sidfex.checkfileformat(filepathnames=filepathname)
      res$checkfileformat.result = checkres
      if (checkres != "No file format violations found.") {
        res.list[[i]] = checkres
        next
      }
    }

    filecont = scan(filepathname,sep="\n",what="character")
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

    dat = read.table(filepathname,header=TRUE,skip=nrGroupID+8)
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
    DaysLeadTime = dat$DayOfYear - res$InitDayOfYear
    if (res$LastYear > res$InitYear) {
      for (yr in res$InitYear:(res$LastYear-1)) {
        DaysLeadTime[dat$Year > yr] = DaysLeadTime[dat$Year > yr] + 365 + as.integer(((yr %% 4 == 0) & (yr %% 100 != 0)) | (yr %% 400 == 0))
      }
    }
    res$DaysForecastLength = DaysLeadTime[Nt]
    dat$DaysLeadTime = DaysLeadTime
    res$data = dat

    res.list[[i]] = res

  }

  if (ens.merge) {
    #res.list = sidfex.ensmerge(res.list)
    warning("ens.merge not yet implemented. Returning forecast objects without merging.")
    ens.merge=FALSE
  }
  return(list(ens.merge=ens.merge,res.list=res.list))

}
