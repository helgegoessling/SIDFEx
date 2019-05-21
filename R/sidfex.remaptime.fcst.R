sidfex.remaptime.fcst <- function (fcst,newtime.DaysLeadTime=NULL,newtime.HourOfDay=NULL,newtime.DayOfYear=NULL,newtime.Year=NULL,method="linear",extrapolate=FALSE,return.remapinfo=FALSE,data.path=NULL) {

  require(spheRlab)

  if (method != "nearestneighbour" && method != "linear") {
    stop("'method' must be one of 'nearestneighbour' and 'linear'.")
  }

  if (!is.null(newtime.DaysLeadTime)) {
    if (!(is.null(newtime.HourOfDay) && is.null(newtime.DayOfYear) && is.null(newtime.Year))) {
      warning("Using argument 'newtime.DaysLeadTime'. Other arguments starting 'newtime.' are ignored.")
    }
    newtime = newtime.DaysLeadTime
  } else {
    stop("arguments for new time axis other than 'newtime.DaysLeadTime' not yet implemented")
    if (!is.null(newtime.HourOfDay)) {
      if (!(is.null(newtime.DayOfYear) && is.null(newtime.Year))) {
        warning("Using argument 'newtime.HourOfDay'. Other arguments starting 'newtime.' are ignored.")
      }
    } else {
      if (!is.null(newtime.Year)) {
        if (is.null(newtime.DayOfYear)) {
          stop("With 'newtime.Year' specified, 'newtime.DayOfYear' must be specified, too.")
        }
      } else {
        if (is.null(newtime.DayOfYear)) {
          stop("At least one of the arguments starting 'newtime.' must be specified.")
        }
      }
    }
  }

  rl = fcst$res.list

  for (irl in 1:length(rl)) {

    if (newtime[1] < 0) {stop("First time of new time axis before the forecast initial time")}

    nms = names(fcst$res.list[[irl]]$data)
    LatCols = which(substr(nms, start = 1, stop = 3) == "Lat")
    LonCols = which(substr(nms, start = 1, stop = 3) == "Lon")
    oldlat = fcst$res.list[[irl]]$data[,LatCols]
    oldlon = fcst$res.list[[irl]]$data[,LonCols]
    oldtime = sidfex.ydoy2reltime(Year=rl[[irl]]$data$Year, DayOfYear = rl[[irl]]$data$DayOfYear,
                                  RefYear = rl[[irl]]$InitYear, RefDayOfYear = rl[[irl]]$InitDayOfYear)

    if (newtime[1] < oldtime[1]) {
      oldtime = c(0,oldtime)
      if (fcst$ens.merge && !is.vector(oldlat)) {
        oldlat0 = matrix(nrow=nrow(oldlat)+1,ncol=ncol(oldlat))
        oldlat0[1,] = rl[[irl]]$MergedInitLat
        oldlat0[2:nrow(oldlat0),] = as.matrix(oldlat)
        oldlat = oldlat0
        oldlon0 = matrix(nrow=nrow(oldlon)+1,ncol=ncol(oldlon))
        oldlon0[1,] = rl[[irl]]$MergedInitLon
        oldlon0[2:nrow(oldlon0),] = as.matrix(oldlon)
        oldlon = oldlon0
      } else {
        oldlat = c(rl[[irl]]$InitLat, oldlat)
        oldlon = c(rl[[irl]]$InitLon, oldlat)
      }
    }

    remap.res = sl.trajectory.remaptime(oldtime = oldtime, oldlat = oldlat,
                                        oldlon = oldlon, newtime = newtime,
                                        extrapolate = FALSE, method = "linear",
                                        return.remapinfo = return.remapinfo)
    newtime.ydoy = sidfex.reltime2ydoy(reltime = newtime, RefYear = rl[[irl]]$InitYear,
                                       RefDayOfYear = rl[[irl]]$InitDayOfYear)

    rl[[irl]]$data = data.frame(matrix(ncol=length(nms),nrow=length(newtime)))
    names(rl[[irl]]$data) = nms
    rl[[irl]]$data$Year = newtime.ydoy$Year
    rl[[irl]]$data$DayOfYear = newtime.ydoy$DayOfYear
    rl[[irl]]$data[,LatCols] = remap.res$Lat
    rl[[irl]]$data[,LonCols] = remap.res$Lon
    rl[[irl]]$data$DaysLeadTime = newtime
    if (return.remapinfo) {
      rl[[irl]]$data$weights.left.ind = weights.left.ind
      rl[[irl]]$data$weights.left = weights.left
      rl[[irl]]$oldtime = oldtime
    }

  }

  return(list(ens.merge=fcst$ens.merge, remaptime.fcst=list(method=method,extrapolate=extrapolate), res.list=rl))

}
