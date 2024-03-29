sidfex.read.obs <- function(index=NULL,TargetID=NULL,data.path=NULL,NA_values=-999,unique_POS_time=TRUE,add_RelTimeDay_Ref="firstobs") {

  if (is.null(data.path)) {
    no.data.path.obs=TRUE
    if (file.exists(file.path("~",".SIDFEx"))) {
      source(file.path("~",".SIDFEx"))
      if (exists("data.path.obs")) {no.data.path.obs=FALSE}
    }
    if (no.data.path.obs) {
      stop(paste0("With data.path=NULL , data.path.obs must be specified in a file ~/.SIDFEx as a line like data.path.obs=..."))
    }
  } else {
    data.path.obs = data.path
  }

  from.index = FALSE
  if (is.null(TargetID)) {
    if (is.null(index)) {stop("Either 'index' or 'TargetID' must be specified")}
    from.index = TRUE
    TargetID = unique(index$TargetID)
  }

  res.list = list()
  i = 0
  for (tid in TargetID) {
    i = i + 1

    ifile = file.path(data.path.obs,paste0(tid,".txt"))

    if (!file.exists(ifile)) {
      warning(paste0("file '",ifile,"' does not exist, returning empty data element"))
      res.list[[i]] = list(filename=ifile,TargetID=tid,data=NULL)
      next
    }

    res = read.table(file=ifile,header=TRUE)
    if (names(res)[1] == "BuoyID") {
      res = res[,2:ncol(res)]
    }

    if (!("POS_DOY" %in% names(res))) {
      if (!("DOY" %in% names(res))) {stop(paste0("Neither 'POS_DOY' nor 'DOY' column contained in ",ifile))}
      warning(paste0("No 'POS_DOY' column contained in ",ifile,", copying 'DOY' column to 'POS_DOY' column"))
      res$POS_DOY = res$DOY
    }

    if (!is.null(NA_values)) {
      res = res[!(res$Year%in%NA_values | res$POS_DOY%in%NA_values |
                  res$Lon%in%NA_values | res$Lat%in%NA_values), ]
    }

    if (!is.null(add_RelTimeDay_Ref)) {
      if (is.character(add_RelTimeDay_Ref)) {
        if (add_RelTimeDay_Ref != "firstobs") {
          stop("'add_RelTimeDay_Ref' must be 'firstobs', NULL, or a 2-element numeric vector")
        }
        add_RelTimeDay_Ref = c(min(res$Year),min(res$POS_DOY[res$Year==min(res$Year)]))
      }
      doy_cont = sidfex.ydoy2reltime(res$Year,res$POS_DOY,add_RelTimeDay_Ref[1],
                                     add_RelTimeDay_Ref[2])
      res$RelTimeDay = doy_cont
    }

    if (unique_POS_time) {
      if (is.null(add_RelTimeDay_Ref)) {
        RelTimeDay_Ref = c(min(res$Year),min(res$POS_DOY[res$Year==min(res$Year)]))
        doy_cont = sidfex.ydoy2reltime(res$Year,res$POS_DOY,RelTimeDay_Ref[1],RelTimeDay_Ref[2])
      }
      Nobs = length(doy_cont)
      keep.unique = which(!duplicated(doy_cont,fromLast = TRUE))
      keep.unique.ordered = keep.unique[order(doy_cont[keep.unique])]
      if (length(keep.unique.ordered) != Nobs) {warning("duplicates removed; using only respective last values")}
      if (any(diff(keep.unique.ordered)<0)) {warning("data time stamps not always ascending; reordered")}
      res = res[keep.unique.ordered,]
    }

    if (!is.null(add_RelTimeDay_Ref)) {
      res.list[[i]] = list(filename=ifile,TargetID=tid,RelTime_RefYearDOY=add_RelTimeDay_Ref,data=res)
    } else {
      res.list[[i]] = list(filename=ifile,TargetID=tid,data=res)
    }


  }

  if (length(TargetID) > 1) {
    names(res.list) = TargetID
    if (from.index) {
      index.map = rep(NA,nrow(index))
      for (i in 1:length(TargetID)) {
        index.map[index$TargetID == TargetID[i]] = i
      }
      return(list(res.list=res.list,index.map=index.map))
    } else {
      return(res.list)
    }
  } else {
    #return(list(filename=res.list[[1]]$filename,TargetID=res.list[[1]]$TargetID,data=res.list[[1]]$data))
    return(res.list[[1]])
  }

}
