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

    if (is.null(newtime.DaysLeadTime)) {
      stop("arguments for new time axis other than 'newtime.DaysLeadTime' not yet implemented")
      #if (!is.null(newtime.HourOfDay)) {
      #  b1.y = fc$InitYear
      #  b1.doy = fc$InitDayOfYear
      #  b2.y = tail(fc$data$Year, n = 1)
      #  b2.doy = tail(fc$data$DayOfYear, n = 1)
      #  for (yr in b1.y:b2.y) {
      #    newtime.Year = bla
      #  }
      #} else {
      #  bli = blubb
      #}
    }

    if (newtime[1] < 0) {stop("First time of new time axis before the forecast initial time")}

    olddata = rl[[irl]]$data[,3:ncol(rl[[irl]]$data)]
    oldtime = sidfex.ydoy2reltime(Year=rl[[irl]]$data$Year, DayOfYear = rl[[irl]]$data$DayOfYear,
                                  RefYear = rl[[irl]]$InitYear, RefDayOfYear = rl[[irl]]$InitDayOfYear)

    old.N = length(oldtime)
    new.N = length(newtime)

    if (newtime[1] < oldtime[1]) {
      oldtime = c(0,oldtime)
      old.N = old.N + 1
      olddata = rbind(data.frame(Lat=rl[[irl]]$InitLat,Lon=rl[[irl]]$InitLon,DaysLeadTime=0),olddata)   # this needs to be extended once ensemble merging will have been implemented
    }

    if (newtime[new.N] > oldtime[old.N]) {
      if (newtime[1] > oldtime[old.N]) {
        warning("New time axis completely out of original lead time range.")
        if (extrapolate) {
          warning("All values will be extrapolated.")
        } else {
          warning("All values will be 'NA'.")
        }
      } else {
        warning("New time axis exceeds original lead time range.")
        if (extrapolate) {
          warning("Values after original lead time range will be extrapolated.")
        } else {
          warning("Values after original lead time range will be 'NA'.")
        }
      }
    }

    new.N.x = new.N
    newtime.x = newtime    #c(0,newtime)
    weights.left = rep(NA,new.N.x)
    weights.left.ind = rep(NA,new.N.x)
    i.old = 1
    for (i.new in 1:new.N.x) {
      while (i.old < (old.N-1) && newtime[i.new] > oldtime[i.old+1]) {
        i.old = i.old + 1
      }
      weights.left.ind[i.new] = i.old
      weights.left[i.new] = (oldtime[i.old+1] - newtime.x[i.new]) / (oldtime[i.old+1] - oldtime[i.old])
      if (weights.left[i.new] > 1) {
        stop("interpolation weight larger than one, something is wrong")}
    }
    if (!extrapolate) {
      weights.left[weights.left < 0 | weights.left > 1] = NA
    }
    if (method == "nearestneighbour") {
      weights.left[weights.left > .5] = 1
      weights.left[weights.left <= .5] = 0
    }

    #weights.left.ind = weights.left.ind[2:new.N.x]
    #weights.left = weights.left[2:new.N.x]
    new.Lat = rep(NA,new.N)
    new.Lon = rep(NA,new.N)
    for (i in 1:new.N) {
      new.LonLat = sl.p2p(lon1=olddata$Lon[weights.left.ind[i]],lat1=olddata$Lat[weights.left.ind[i]],lon2=olddata$Lon[weights.left.ind[i]+1],lat2=olddata$Lat[weights.left.ind[i]+1],frac=1-weights.left[i])
      new.Lat[i] = new.LonLat$lat
      new.Lon[i] = new.LonLat$lon
    }

    rl[[irl]]$data = data.frame(Year=rep(NA,new.N))
    rl[[irl]]$data$DayOfYear = rep(NA,new.N)
    rl[[irl]]$data$Lat = new.Lat
    rl[[irl]]$data$Lon = new.Lon
    rl[[irl]]$data$DaysLeadTime = newtime
    if (return.remapinfo) {
      rl[[irl]]$data$weights.left.ind = weights.left.ind
      rl[[irl]]$data$weights.left = weights.left
      rl[[irl]]$oldtime = oldtime
    }

  }

  return(list(ens.merge=fcst$ens.merge, remaptime.fcst=list(method=method,extrapolate=extrapolate), res.list=rl))

}
