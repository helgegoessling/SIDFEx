sidfex.remaptime.fcst <- function (fcst,newtime.DaysLeadTime=NULL,newtime.FractionOfDay=NULL,newtime.YearDayOfYear=NULL,method="linear",extrapolate=FALSE,extrapolate.maxspeed=Inf,return.remapinfo=FALSE,verbose=TRUE) {

  require(spheRlab)

  if (method != "nearestneighbour" && method != "linear") {
    stop("'method' must be one of 'nearestneighbour' and 'linear'.")
  }

  if (!is.null(newtime.DaysLeadTime)) {
    if (!(is.null(newtime.FractionOfDay) && is.null(newtime.YearDayOfYear))) {
      warning("Using argument 'newtime.DaysLeadTime'. Other arguments starting 'newtime.' are ignored.")
    }
    newtime = newtime.DaysLeadTime
  } else {
    if (is.null(newtime.FractionOfDay)) {stop("Either 'newtime.DaysLeadTime' or 'newtime.FractionOfDay' must be defined.")}
    if (newtime.FractionOfDay < 0 || newtime.FractionOfDay >= 1) {stop("'newtime.FractionOfDay' must be within [0,1)")}
  }

  if ("res.list" %in% names(fcst)) {
    rl = fcst$res.list
    single.element = FALSE
  } else if ("data" %in% names(fcst)) {
    rl = list(fcst)
    single.element = TRUE
  } else {
    stop("format of 'fcst' not recognised.")
  }

  for (irl in 1:length(rl)) {

    if (is.null(newtime.DaysLeadTime)) {
      fulldayrange = sidfex.ydoy2reltime(Year=c(rl[[irl]]$InitYear,rl[[irl]]$LastYear),
                                         DayOfYear=c(floor(rl[[irl]]$InitDayOfYear),floor(rl[[irl]]$LastDayOfYear)),
                                         RefYear=rl[[irl]]$InitYear,RefDayOfYear=rl[[irl]]$InitDayOfYear)
      newtime = seq(fulldayrange[1],fulldayrange[2])
      if (!is.null(newtime.YearDayOfYear)) {
        newtime.keep = sidfex.ydoy2reltime(Year=newtime.YearDayOfYear[,1],DayOfYear=newtime.YearDayOfYear[,2],
                                             RefYear=rl[[irl]]$InitYear,RefDayOfYear=rl[[irl]]$InitDayOfYear)
        newtime = newtime[newtime %in% newtime.keep]
      }
      newtime = rep(newtime,each=length(newtime.FractionOfDay)) + rep(newtime.FractionOfDay,length(newtime))
      lastleadtime = sidfex.ydoy2reltime(Year=rl[[irl]]$LastYear,DayOfYear=rl[[irl]]$LastDayOfYear,
                                         RefYear=rl[[irl]]$InitYear,RefDayOfYear=rl[[irl]]$InitDayOfYear)
      newtime = newtime[newtime >= 0 & newtime <= lastleadtime]
      if (length(newtime) < 1) {
        warning("None of the times specified is within the original lead time range, remapping not possible")
        rl[[irl]] = "Time remapping failed: no times within lead time range"
        next
      }
    }

    if (newtime[1] < 0) {
      warning("First time of new time axis before the forecast initial time, remapping not possible")
      rl[[irl]] = "Time remapping failed"
      next
    }

    nms = names(rl[[irl]]$data)
    LatCols = which(substr(nms, start = 1, stop = 3) == "Lat")
    LonCols = which(substr(nms, start = 1, stop = 3) == "Lon")
    oldlat = rl[[irl]]$data[,LatCols]
    oldlon = rl[[irl]]$data[,LonCols]
    oldtime = sidfex.ydoy2reltime(Year=rl[[irl]]$data$Year, DayOfYear = rl[[irl]]$data$DayOfYear,
                                  RefYear = rl[[irl]]$InitYear, RefDayOfYear = rl[[irl]]$InitDayOfYear)
    if (length(oldtime) > 1) {
      oldtime.diff = oldtime[2:length(oldtime)] - oldtime[1:(length(oldtime)-1)]
      if (any(oldtime.diff <= 0)) {
        if (any(oldtime.diff < 0)) {
          stop('old time axis must increase monotonously')
        } else {
          warning('removing duplicate times in old time axis')
          inds = c(TRUE, oldtime.diff > 0)
          oldtime = oldtime[inds]
          if (is.null(dim(oldlat))) {
            oldlat = oldlat[inds]
            oldlon = oldlon[inds]
          } else {
            oldlat = oldlat[inds,]
            oldlon = oldlon[inds,]
          }
        }
      }
    }

    if (newtime[1] < oldtime[1]) {
      oldtime = c(0,oldtime)
      if (!single.element && fcst$ens.merge && !is.vector(oldlat)) {
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
        oldlon = c(rl[[irl]]$InitLon, oldlon)
      }
    }

    remap.res = sl.trajectory.remaptime(oldtime = oldtime, oldlat = oldlat,
                                        oldlon = oldlon, newtime = newtime,
                                        extrapolate = extrapolate, extrapolate.maxspeed = extrapolate.maxspeed, method = "linear",
                                        return.remapinfo = return.remapinfo, verbose = verbose)
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

  if (single.element) {
    return(rl[[1]])
  } else {
    return(list(ens.merge=fcst$ens.merge, remaptime.fcst=list(method=method,extrapolate=extrapolate), res.list=rl))
  }

}
