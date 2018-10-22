sidfex.download.obs <- function(index=NULL,TargetID=NULL,data.path=NULL) {

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

  if (!dir.exists(data.path.obs)) {
    print(paste0("Creating directory ",data.path.obs))
    system(paste0("mkdir -p ",data.path.obs))
  }

  wd = getwd()
  setwd(data.path.obs)
  print("Starting download ...")
  for (tid in TargetID) {
    download.file(url=paste0("http://iabp.apl.washington.edu/WebData/",tid,".dat"),destfile=paste0(tid,".txt"))
  }
  print("Download done.")
  setwd(wd)

  return(TargetID)

}
