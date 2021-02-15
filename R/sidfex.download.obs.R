sidfex.download.obs <- function(index=NULL,TargetID=NULL,data.path=NULL,baseurl=NULL,try.N=30,try.timeout=300) {

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

  if (is.null(TargetID)) {
    tt = sidfex.targettable.update(download.obs = FALSE, data.path = data.path)
    if (is.null(index)) {
      TargetID = tt$TargetID
    } else {
      TargetID = unique(index$TargetID)
      if (any(!(TargetID %in% tt$TargetID))) {
        warning("'index' contains TargetIDs not contained in the SIDFEx target table. Consider updating the target table.")
        print(paste0("The following TargetIDs are not downloaded:",TargetID[!(TargetID %in% tt$TargetID)]))
        TargetID = TargetID[TargetID %in% tt$TargetID]
      }
    }
  }

  if (!dir.exists(data.path.obs)) {
    print(paste0("Creating directory ",data.path.obs))
    system(paste0("mkdir -p ",data.path.obs))
  }

  try_with_timeout <- function(expr, cpu = Inf, elapsed = Inf) {
    return(
      tryCatch(
        {
          expr <- substitute(expr)
          envir <- parent.frame()
          setTimeLimit(cpu = cpu, elapsed = elapsed, transient = TRUE)
          on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
          eval(expr, envir = envir)
        },
        error=function(x){
          return(NA)
        }
      )
    )
  }

  wd = getwd()
  setwd(data.path.obs)
  print("Starting download ...")
  i = 0
  for (tid in TargetID) {
    i = i + 1
    if (is.null(baseurl)) {
      if (substr(tid, start=1, stop=5) %in% c("FIXED","POLAR","DISTN","CENTR")) {
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
    try.i = 0
    repeat {
      if (baseurlx == "http://iabp.apl.washington.edu/WebData/") {
        res = try_with_timeout(download.file(url=paste0(baseurlx,tid,suf),destfile=paste0(tid,".txt"),quiet=TRUE,
                                             method="wget",extra="--no-check-certificate"),elapsed=try.timeout)
      } else {
        res = try_with_timeout(download.file(url=paste0(baseurlx,tid,suf),destfile=paste0(tid,".txt"),quiet=TRUE),elapsed=try.timeout)
      }
      if (!is.na(res)) {break}
      try.i = try.i + 1
      if (try.i > try.N) {stop(paste0("Download of file ",baseurlx,tid,suf," failed."))}
    }
  }
  print("Download done.")
  setwd(wd)

  return(TargetID)

}
