sidfex.read.obs <- function(index=NULL,TargetID=NULL,data.path=NULL) {

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

    res = read.table(file=ifile,header=TRUE)
    res = res[,2:ncol(res)]

    res.list[[i]] = list(filename=ifile,TargetID=tid,data=res)

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
    return(list(filename=res.list[[1]]$filename,TargetID=res.list[[1]]$TargetID,data=res.list[[1]]$data))
  }

}
