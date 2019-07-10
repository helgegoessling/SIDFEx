sidfex.download.fcst <- function(comparison.mode=FALSE, from.scratch=TRUE, data.path=NULL, indexTable.path=NULL, baseurl="https://swift.dkrz.de/v1/dkrz_0262ea1f00e34439850f3f1d71817205/", verbose = FALSE) {

  dataurl = paste0(baseurl,"SIDFEx_processed/")
  indexurl = paste0(baseurl,"SIDFEx_index/")

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

  if (is.null(indexTable.path)) {
    no.indexTable.path=TRUE
    if (file.exists(file.path("~",".SIDFEx"))) {
      data.path.fcst.x = data.path.fcst
      source(file.path("~",".SIDFEx"))
      data.path.fcst = data.path.fcst.x
      rm(data.path.fcst.x)
      if (exists("indexTable.path.in")) {no.indexTable.path=FALSE}
    }
    if (no.indexTable.path) {
      stop(paste0("With indexTable.path=NULL , indexTable.path.in must be specified in a file ~/.SIDFEx as a line like indexTable.path.in=..."))
    }
  } else {
    indexTable.path.in = indexTable.path
  }

  if (from.scratch && dir.exists(data.path.fcst)) {
    if (comparison.mode) {
      print(paste0("Be aware that all previous data in ",data.path.fcst," is removed if you set comparison.mode=FALSE with from.scratch=TRUE ."))
    } else {
      res = system(paste0("rm -rf ",data.path.fcst))
    }
  }
  if (!comparison.mode && !dir.exists(data.path.fcst)) {
    print(paste0("Creating directory ",data.path.fcst))
    res = system(paste0("mkdir -p ",data.path.fcst))
  }

  wd = getwd()

  if (!dir.exists(indexTable.path.in)) {
    print(paste0("Creating directory ",indexTable.path.in))
    res = system(paste0("mkdir -p ",indexTable.path.in))
  }
  setwd(indexTable.path.in)
  print("Index download ...")
  res = download.file(url=paste0(indexurl,"indexTable.rda"),destfile="indexTable_remote.rda")
  print("Index download done.")

  if (!from.scratch) {

    if (!file.exists("indexTable.rda")) {
      stop(paste0("No local index ",file.path(indexTable.path.in,"indexTable.rda"),
                   " exists. Rerun with from.scratch=TRUE or, if data exists,",
                   " generate index with sidfex.fcst.search.createIndexTable."))
    }

    index.diff = sidfex.fcst.search.compareIndexTables("indexTable.rda","indexTable_remote.rda")
    print(paste0("Entries only in local index = obsolete files to be deleted: ",nrow(index.diff$only.1)))
    print(paste0("Entries only in remote index = new files to be downloaded: ",nrow(index.diff$only.2)))
    print(paste0("Identical entries = local files already up-to-date: ",nrow(index.diff$both.ident)))
    print(paste0("Entries for same files with differences = files to be downloaded and to replace local ones: ",nrow(index.diff$both.diff.2)))

    index.download = rbind(index.diff$only.2,index.diff$both.diff.2)

    if (comparison.mode) {print("To execute these downloads and other changes (if present as indicated above), resubmit command with comparison.mode=FALSE .")}

  } else {

    df.env = new.env()
    load(file.path(indexTable.path.in,"indexTable_remote.rda"),envir=df.env)
    index.download = df.env$rTab

  }

  if (!comparison.mode) {

    setwd(data.path.fcst)

    print("Data download ...")
    if (from.scratch) {
      res = download.file(url=paste0(indexurl,"data.tar.gz"),destfile=file.path(data.path.fcst,"data.tar.gz"))
      if (verbose)
        {res = system(paste0("tar -zxvf ",file.path(data.path.fcst,"data.tar.gz")))} else
        {res = system(paste0("tar -zxf ",file.path(data.path.fcst,"data.tar.gz")))}
      res = system(paste0("mv ",file.path(data.path.fcst,"data/*")," ",data.path.fcst))
      res = system(paste0("rm -rf ",file.path(data.path.fcst,"data")))
      res = system(paste0("rm -rf ",file.path(data.path.fcst,"data.tar.gz")))
    } else {
      for (fi in 1:nrow(index.download)) {
        gid = index.download$GroupID[fi]
        fl = index.download$File[fi]
        if (!dir.exists(gid)) {dir.create(gid)}
        res = download.file(url=paste0(dataurl,gid,"/",fl,".txt"),destfile=paste0(file.path(gid,fl),".txt"))
      }
    }
    print("Data download done.")

    if (!from.scratch && nrow(index.diff$only.1) > 0) {
      print("Removing obsolete data present only locally ...")
      for (fi in 1:nrow(index.diff$only.1)) {
        gid = index.diff$only.1$GroupID[fi]
        fl = index.diff$only.1$File[fi]
        system(paste0("rm -f ",file.path(gid,fl),".txt"))
      }
      print("Removing obsolete data done.")
    }

    res = system(paste0("mv ",file.path(indexTable.path.in,"indexTable_remote.rda")," ",file.path(indexTable.path.in,"indexTable.rda")),intern=TRUE)

    print("Returning index of files that have been downloaded, and (if not from.scratch) the more detailed result of sidfex.fcst.search.compareIndexTables")

  } else {

    print("Returning index of files that would be downloaded with comparison.mode=FALSE, and (if not from.scratch) the more detailed result of sidfex.fcst.search.compareIndexTables")

  }

  setwd(wd)

  if (!from.scratch) {
    warning("Note that, with from.scratch=FALSE, it can not be excluded that the local data differs from the remote data in aspects that are not captured by the index. But that's very unlikely.")
    return(list(index.download=index.download,index.diff=index.diff))
  } else {
    return(list(index.download=index.download))
  }

}
