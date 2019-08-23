sidfex.download.obs <- function(index=NULL,TargetID=NULL,data.path=NULL,baseurl=NULL) {

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
  i = 0
  for (tid in TargetID) {
    i = i + 1
    if (is.null(baseurl)) {
      if (substr(tid, start=1, stop=5) %in% c("FIXED","POLAR","DISTN")) {
        baseurlx = "https://swift.dkrz.de/v1/dkrz_0262ea1f00e34439850f3f1d71817205/SIDFEx_index/observations/"
      } else {
        baseurlx = "http://iabp.apl.washington.edu/WebData/"
      }
    } else if (length(baseurl) > 1) {
      baseurlx = baseurl[i]
    } else {
      baseurlx = baseurl
    }
    suf = ".txt"
    if (baseurlx == "http://iabp.apl.washington.edu/WebData/") {suf = ".dat"}
    download.file(url=paste0(baseurlx,tid,suf),destfile=paste0(tid,".txt"))
  }
  print("Download done.")
  setwd(wd)

  return(TargetID)

}
