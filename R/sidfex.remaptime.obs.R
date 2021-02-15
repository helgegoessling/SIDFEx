sidfex.remaptime.obs <- function (obs,newtime.RelTimeDay=NULL,newtime.FractionOfDay=NULL,newtime.YearDayOfYear=NULL,method="linear",extrapolate=FALSE,extrapolate.maxspeed=Inf,return.remapinfo=FALSE,verbose=TRUE) {

  require(spheRlab)

  if (method != "nearestneighbour" && method != "linear") {
    stop("'method' must be one of 'nearestneighbour' and 'linear'.")
  }

  if (!is.null(newtime.RelTimeDay)) {
    if (!(is.null(newtime.FractionOfDay) && is.null(newtime.YearDayOfYear))) {
      warning("Using argument 'newtime.RelTimeDay'. Other arguments starting 'newtime.' are ignored.")
    }
    newtime = newtime.RelTimeDay
  } else {
    if (is.null(newtime.FractionOfDay)) {
      if (is.null(newtime.YearDayOfYear)) {
        stop("One of 'newtime.RelTimeDay', 'newtime.FractionOfDay', and 'newtime.YearDayOfYear' must be defined.")
      }
    } else {
      if (!is.null(newtime.YearDayOfYear)) {
        if (!identical(as.numeric(newtime.YearDayOfYear$DayOfYear), floor(newtime.YearDayOfYear$DayOfYear))) {
          stop("When 'newtime.FractionOfDay' is defined, 'newtime.YearDayOfYear$DayOfYear' must be an integer vector")
        }
      }
      if (min(newtime.FractionOfDay) < 0 || max(newtime.FractionOfDay) >= 1) {stop("values in 'newtime.FractionOfDay' must be within [0,1)")}
    }
  }

  single.element = FALSE
  obs.frommultid = FALSE
  if ("res.list" %in% names(obs)) {
    rl = obs$res.list
  } else if ("data" %in% names(obs)) {
    rl = list(obs)
    single.element = TRUE
  } else {
    #stop("format of 'obs' not recognised.")
    rl = obs
    obs.frommultid = TRUE
  }

  for (irl in 1:length(rl)) {

    Ndat = nrow(rl[[irl]]$data)

    if (is.null(newtime.RelTimeDay)) {
      if (is.null(newtime.FractionOfDay)) {
        newtime = sidfex.ydoy2reltime(Year = newtime.YearDayOfYear$Year, DayOfYear = newtime.YearDayOfYear$DayOfYear,
                                      RefYear = rl[[irl]]$RelTime_RefYearDOY[1], RefDayOfYear = rl[[irl]]$RelTime_RefYearDOY[2])
      } else {
        fulldayrange = sidfex.ydoy2reltime(Year=rl[[irl]]$data$Year[c(1,Ndat)],
                                           DayOfYear=floor(rl[[irl]]$data$POS_DOY[c(1,Ndat)]),
                                           RefYear=rl[[irl]]$RelTime_RefYearDOY[1],RefDayOfYear=rl[[irl]]$RelTime_RefYearDOY[2])
        newtime = seq(fulldayrange[1],fulldayrange[2])
        if (!is.null(newtime.YearDayOfYear)) {
          newtime.keep = sidfex.ydoy2reltime(Year=newtime.YearDayOfYear[,1],DayOfYear=newtime.YearDayOfYear[,2],
                                             RefYear=rl[[irl]]$RelTime_RefYearDOY[1],RefDayOfYear=rl[[irl]]$RelTime_RefYearDOY[2])
          newtime = newtime[newtime %in% newtime.keep]
        }
        newtime = rep(newtime,each=length(newtime.FractionOfDay)) + rep(newtime.FractionOfDay,length(newtime))
        lastreltime = sidfex.ydoy2reltime(Year=rl[[irl]]$data$Year[Ndat],DayOfYear=rl[[irl]]$data$POS_DOY[Ndat],
                                          RefYear=rl[[irl]]$data$Year[1],RefDayOfYear=rl[[irl]]$data$POS_DOY[1])
        newtime = newtime[newtime >= 0 & newtime <= lastreltime]
      }
      if (length(newtime) < 1) {
        warning("None of the times specified is within the original lead time range, remapping not possible")
        rl[[irl]] = "Time remapping failed: no times within lead time range"
        next
      }
    }

    #if (newtime[1] < 0) {
    #  warning("First time of new time axis before 'RelTime_RefYearDOY' (first observed time), remapping not possible")
    #  rl[[irl]] = "Time remapping failed"
    #  next
    #}

    oldlat = rl[[irl]]$data$Lat
    oldlon = rl[[irl]]$data$Lon
    oldtime = sidfex.ydoy2reltime(Year=rl[[irl]]$data$Year, DayOfYear = rl[[irl]]$data$POS_DOY,
                                  RefYear = rl[[irl]]$RelTime_RefYearDOY[1], RefDayOfYear = rl[[irl]]$RelTime_RefYearDOY[2])
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

    remap.res = sl.trajectory.remaptime(oldtime = oldtime, oldlat = oldlat,
                                        oldlon = oldlon, newtime = newtime,
                                        extrapolate = extrapolate, extrapolate.maxspeed = extrapolate.maxspeed, method = "linear",
                                        return.remapinfo = TRUE, verbose = verbose)
    newtime.ydoy = sidfex.reltime2ydoy(reltime = newtime, RefYear = rl[[irl]]$RelTime_RefYearDOY[1],
                                       RefDayOfYear = rl[[irl]]$RelTime_RefYearDOY[2])

    data.new = data.frame(matrix(ncol=ncol(rl[[irl]]$data),nrow=length(newtime)))
    names(data.new) = names(rl[[irl]]$data)
    data.new$Year = newtime.ydoy$Year
    data.new$POS_DOY = newtime.ydoy$DayOfYear
    data.new$Lat = remap.res$Lat
    data.new$Lon = remap.res$Lon
    data.new$RelTimeDay = newtime

    more.cols = which(!(names(data.new) %in% c("Year","POS_DOY","Lat","Lon","RelTimeDay")))
    if (length(more.cols) > 0 || return.remapinfo) {
      weights.left.ind = remap.res$remapinfo$weights.left.ind
      weights.left = remap.res$remapinfo$weights.left
    }
    if (length(more.cols) > 0) {
      for (icol in more.cols) {
        colname = names(data.new)[icol]
        if (colname %in% c("Hour","hour")) {
          data.new[,icol] = floor(data.new$POS_DOY %% 1 * 24)
        } else if (colname %in% c("Min","min")) {
          data.new[,icol] = floor((data.new$POS_DOY %% 1 * 24) %% 1 * 60)
        } else if (colname %in% c("Sec","sec")) {
          data.new[,icol] = floor(((data.new$POS_DOY %% 1 * 24) %% 1 * 60) %% 1 * 60)
        } else if (colname == "DOY") {
          warning("copying column 'POS_DOY' to column 'DOY' to avoid remapping issues at year boundaries")
          data.new[,icol] = data.new$POS_DOY
        } else {
          if (is.numeric(rl[[irl]]$data[,icol])) {
            data.new[,icol] = (rl[[irl]]$data[weights.left.ind,icol] * weights.left +
                                 rl[[irl]]$data[weights.left.ind+1,icol] * (1 - weights.left))
          } else {
            if (length(unique(rl[[irl]]$data[,icol])) <= 1) {
              data.new[,icol] = rl[[irl]]$data[1,icol]
            } else {
              warning(paste0("data column '",colname,"' is not numeric and not uniform and can't be remapped; returning NAs"))
            }
          }
        }
      }
    }

    rl[[irl]]$data = data.new

    if (return.remapinfo) {
      rl[[irl]]$data$weights.left.ind = weights.left.ind
      rl[[irl]]$data$weights.left = weights.left
      rl[[irl]]$oldtime = oldtime
    }

  }

  if (single.element) {
    return(rl[[1]])
  } else {
    if (obs.frommultid) {
      return(rl)
    } else {
      return(list(res.list=rl, index.map=obs$index.map, remaptime.obs=list(method=method,extrapolate=extrapolate)))
    }
  }

}
