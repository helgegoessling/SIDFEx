sidfex.download.fcst <- function(data.path=NULL,baseurl="https://swiftbrowser.dkrz.de/public/dkrz_0262ea1f00e34439850f3f1d71817205/SIDFEx_processed/") {

  if (is.null(data.path)) {
    no.data.path.fcst=TRUE
    if (file.exists(file.path("~",".SIDFEx"))) {
      source(file.path("~",".SIDFEx"))
      if (exists("data.path.fcst")) {no.data.path.fcst=FALSE}
    }
    if (no.data.path.fcst) {
      stop(paste0("With data.path.fcst=NULL , data.path.fcst must be specified in a file ~/.SIDFEx as a line like data.path.fcst=..."))
    }
  } else {
    data.path.fcst = data.path
  }

  if (!dir.exists(data.path.fcst)) {
    print(paste0("Creating directory ",data.path.fcst))
    system(paste0("mkdir -p ",data.path.fcst))
  }

  wd = getwd()
  setwd(data.path.fcst)
  print("Starting download ...")
  system(paste0("wget -r -H -N --cut-dirs=3 --include-directories=\"/v1/\" \"",baseurl,"?show_all\""))
  print("Download done.")
  system("rm -rf swiftbrowser.dkrz.de")
  system("mv swift.dkrz.de/* .")
  system("rm -rf swift.dkrz.de")
  setwd(wd)

}
