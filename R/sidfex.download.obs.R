sidfex.download.obs <- function(index=NULL,TargetID=NULL,data.path=NULL,baseurl=NULL,try.N=30,try.timeout=300,check.tt=TRUE,GoogleDrive=FALSE) {

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

  if (is.null(TargetID) && is.null(index)) {check.tt = TRUE}
  if (check.tt) {
    tt = sidfex.targettable.update(download.obs = FALSE, data.path = data.path)
    if (is.null(TargetID)) {
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
    } else {
      if (any(!(TargetID %in% tt$TargetID))) {
        warning("one or more entries of TargetID not contained in the SIDFEx target table. Consider updating the target table.")
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
        use.IABP = !(GoogleDrive)
        if (GoogleDrive) {
          tids.GD = c("300234065495020","300234065498190",
                      "300234066030330","300234067527540",
                      "300234067527560","300234067939910",
                      "300234068312210","900120",
                      "900121","300234066890280",
                      "300234067428260","300234067529520",
                      "300234062881810","204762",
                      "145951")
          if (!(tid %in% tids.GD)) {
            warning(paste0("Target ID ",tid," not registered for download from Google Drive; using usual IABP server"))
            use.IABP = TRUE
          } else {
            GD.pref = "https://docs.google.com/uc?export=download&id="
            tids.GD.IDs = c("18gIY_7bxrnp98NhhaCJvXUkbHZUorWF1","17mEqBe_mTfSGCbq0NwAuapXhe6CBFk4W",
                            "188bNxVU9z6YMBdJo5KvX_EbDCXZt6N2j","16yuoBxFbjzvZqjZU8w6Kt2yGW9Ll6K0a",
                            "191NZFgWPDyUv_llRZ0_nAIQGe3sLq8EM","188edR_0bhSsRQld4RgCBXn2bq3pDT3bw",
                            "17RUOMhfwbSFN_jygqZHvwPCJiv3k5vTb","18dfeTFi27FOeawTlF3Fb-7GDBWYYILC5",
                            "18iTzMJYEgAtBhSuAGFZDoqXsmo916JK3","1ECaeqhb0X1SoPnYYg6t2fArr7-zI_RN4",
                            "1Du9x8p2KUfu_1ojB6hBDIEoKk9wwAMlk","1EAtEajBJHtcdgumV7dX1BSyUVaqSk0AG",
                            "1E8C04zacRctUkBrdySULvdpTpBNUxCXs","1DyacCv9C8vPS_LlfBdkmFS7RQE7eltdb",
                            "1EGKZTl2zDxxm51oI3LFc4gY4plnWthp7")
            destfile = paste0(tid,".txt")
            res = try_with_timeout(download.file(url=paste0(GD.pref,tids.GD.IDs[tids.GD==tid]),destfile=paste0(tid,".txt"),quiet=TRUE,
                                                 method="wget",extra="--no-check-certificate"),elapsed=try.timeout)
            fl.raw = readLines(destfile)
            cat(gsub(";","     ",fl.raw[fl.raw!=""],fixed=TRUE),file=destfile,sep="\n",append=FALSE)
          }
        }
        if (use.IABP) {
          res = try_with_timeout(download.file(url=paste0(baseurlx,tid,suf),destfile=paste0(tid,".txt"),quiet=TRUE,
                                               method="wget",extra="--no-check-certificate"),elapsed=try.timeout)
        }
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
