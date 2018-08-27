sidfex.lagged2synced <- function(data.path=NULL,GroupID=NULL) {

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

  if (is.null(GroupID)) {
    GroupID = system(paste0("ls ",data.path.fcst))
  }

  for (gid in GroupID) {

    if (gid == "nrl001") {

      stop("implementation pending")

    } else {
      print(paste0("No lagged2synced conversion implemented for GroupID ",gid,"."))
    }

  }

}
