sidfex.read.obs <- function(data.path=NULL,TargetType="IABP",TargetID) {

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
  ifile = file.path(data.path.obs,paste0(TargetID,".txt"))

  if (TargetType == "IABP") {
    res = read.table(file=ifile,header=TRUE)
    res = res[,2:ncol(res)]
  } else {
    stop("No TargetType other than 'IABP' implemented so far.")
  }

  return(list(filename=ifile,TargetType=TargetType,TargetID=TargetID,data=res))

}
