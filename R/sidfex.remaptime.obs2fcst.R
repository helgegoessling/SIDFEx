sidfex.remaptime.obs2fcst <- function (obs,fcst,method="linear",extrapolate=FALSE,return.remapinfo=FALSE) {

  if (method != "nearestneighbour" && method != "linear") {
    stop("'method' must be one of 'nearestneighbour' and 'linear'.")
  }

  obs.yrs = obs$data$Year
  obs.N = length(obs.yrs)
  obs.DaysSinceStart = obs$data$POS_DOY - obs$data$POS_DOY[1]
  if (obs.yrs[obs.N] > obs.yrs[1]) {
    for (yr in obs.yrs[1]:(obs.yrs[obs.N]-1)) {
      obs.DaysSinceStart[obs.yrs > yr] = obs.DaysSinceStart[obs.yrs > yr] + 365 + as.integer(((yr %% 4 == 0) & (yr %% 100 != 0)) | (yr %% 400 == 0))
    }
  }

  rl = list()

  for (irl in 1:length(fcst$res.list)) {

    fcst.N = fcst$res.list[[irl]]$Ntimesteps
    fcst.InitDaysOffset = fcst$res.list[[irl]]$InitDayOfYear - obs$data$POS_DOY[1]
    fcst.InitYear = fcst$res.list[[irl]]$InitYear
    if (fcst.InitYear > obs.yrs[1]) {
      for (yr in obs.yrs[1]:(fcst.InitYear-1)) {
        fcst.InitDaysOffset = fcst.InitDaysOffset + 365 + as.integer(((yr %% 4 == 0) & (yr %% 100 != 0)) | (yr %% 400 == 0))
      }
    } else if (fcst.InitYear < obs.yrs[1]) {
      for (yr in (obs.yrs[1]-1):fcst.InitYear) {
        fcst.InitDaysOffset = fcst.InitDaysOffset - 365 - as.integer(((yr %% 4 == 0) & (yr %% 100 != 0)) | (yr %% 400 == 0))
      }
    }

    fcst.DaysLeadTime = fcst$res.list[[irl]]$data$DaysLeadTime
    obs.DaysLeadTime = obs.DaysSinceStart - fcst.InitDaysOffset

    if (obs.DaysLeadTime[1] > 0) {
      if (obs.DaysLeadTime[1] > fcst.DaysLeadTime[fcst.N]) {
        warning("Forecast time range completely out of observational time range.")
        if (extrapolate) {
          warning("All forecast values will be extrapolated.")
        } else {
          warning("All forecast values will be 'NA'.")
        }
      } else {
        warning("Forecast initial time before first observation.")
        if (extrapolate) {
          warning("Forecast values ahead of the observational time range will be extrapolated.")
        } else {
          warning("Forecast values ahead of the observational time range will be 'NA'.")
        }
      }
    }
    if (obs.DaysLeadTime[obs.N] < fcst.DaysLeadTime[fcst.N]) {
      if (obs.DaysLeadTime[obs.N] < 0) {
        warning("Forecast time range completely out of observational time range.")
        if (extrapolate) {
          warning("All forecast values will be extrapolated.")
        } else {
          warning("All forecast values will be 'NA'.")
        }
      } else {
        warning("Last forecast time after last observation.")
        if (extrapolate) {
          warning("Forecast values after the observational time range will be extrapolated.")
        } else {
          warning("Forecast values after the observational time range will be 'NA'.")
        }
      }
    }

    fcst.N.x = fcst.N + 1
    fcst.DaysLeadTime.x = c(0,fcst.DaysLeadTime)
    weights.left = rep(NA,fcst.N.x)
    weights.left.ind = rep(NA,fcst.N.x)
    i.obs = 1
    for (i.fcst in 1:fcst.N.x) {
      while (i.obs < (obs.N-1) && fcst.DaysLeadTime.x[i.fcst] > obs.DaysLeadTime[i.obs+1]) {
        i.obs = i.obs + 1
      }
      weights.left.ind[i.fcst] = i.obs
      weights.left[i.fcst] = (obs.DaysLeadTime[i.obs+1] - fcst.DaysLeadTime.x[i.fcst]) / (obs.DaysLeadTime[i.obs+1] - obs.DaysLeadTime[i.obs])
      if (weights.left[i.fcst] > 1) {return()}
    }
    if (!extrapolate) {
      weights.left[weights.left < 0 | weights.left > 1] = NA
    }
    if (method == "nearestneighbour") {
      weights.left[weights.left > .5] = 1
      weights.left[weights.left <= .5] = 0
    }

    obs.InitLonLat = sl.p2p(lon1=obs$data$Lon[weights.left.ind[1]],lat1=obs$data$Lat[weights.left.ind[1]],lon2=obs$data$Lon[weights.left.ind[1]+1],lat2=obs$data$Lat[weights.left.ind[1]+1],frac=1-weights.left[1])
    weights.left.ind = weights.left.ind[2:fcst.N.x]
    weights.left = weights.left[2:fcst.N.x]
    obs.Lat = rep(NA,fcst.N)
    obs.Lon = rep(NA,fcst.N)
    if (!is.null(obs$data$BP)) {obs.BP = rep(NA,fcst.N)}
    if (!is.null(obs$data$Ts)) {obs.Ts = rep(NA,fcst.N)}
    if (!is.null(obs$data$Ta)) {obs.Ta = rep(NA,fcst.N)}
    for (i in 1:fcst.N) {
      obs.LonLat = sl.p2p(lon1=obs$data$Lon[weights.left.ind[i]],lat1=obs$data$Lat[weights.left.ind[i]],lon2=obs$data$Lon[weights.left.ind[i]+1],lat2=obs$data$Lat[weights.left.ind[i]+1],frac=1-weights.left[i])
      obs.Lat[i] = obs.LonLat$lat
      obs.Lon[i] = obs.LonLat$lon
      if (!is.null(obs$data$BP)) {obs.BP[i] = weights.left[i]*obs$data$BP[weights.left.ind[i]] + (1-weights.left[i])*obs$data$BP[weights.left.ind[i]+1]}
      if (!is.null(obs$data$Ts)) {obs.Ts[i] = weights.left[i]*obs$data$Ts[weights.left.ind[i]] + (1-weights.left[i])*obs$data$Ts[weights.left.ind[i]+1]}
      if (!is.null(obs$data$Ta)) {obs.Ta[i] = weights.left[i]*obs$data$Ta[weights.left.ind[i]] + (1-weights.left[i])*obs$data$Ta[weights.left.ind[i]+1]}
    }

    rl[[irl]] = list()
    rl[[irl]]$filename = obs$filename
    rl[[irl]]$TargetType = obs$TargetType
    rl[[irl]]$TargetID = obs$TargetID
    rl[[irl]]$InitYear = fcst$res.list[[irl]]$InitYear
    rl[[irl]]$InitDayOfYear = fcst$res.list[[irl]]$InitDayOfYear
    rl[[irl]]$InitLat = obs.InitLonLat$lat
    rl[[irl]]$InitLon = obs.InitLonLat$lon
    rl[[irl]]$data = data.frame(Year=fcst$res.list[[irl]]$data$Year)
    rl[[irl]]$data$DayOfYear = fcst$res.list[[irl]]$data$DayOfYear
    rl[[irl]]$data$DaysLeadTime = fcst$res.list[[irl]]$data$DaysLeadTime
    rl[[irl]]$data$Lat = obs.Lat
    rl[[irl]]$data$Lon = obs.Lon
    if (!is.null(obs$data$BP)) {rl[[irl]]$data$BP = obs.BP}
    if (!is.null(obs$data$Ts)) {rl[[irl]]$data$Ts = obs.Ts}
    if (!is.null(obs$data$Ta)) {rl[[irl]]$data$Ta = obs.Ta}
    if (return.remapinfo) {
      rl[[irl]]$data$weights.left.ind = weights.left.ind
      rl[[irl]]$data$weights.left = weights.left
      rl[[irl]]$data$weights.left
      rl[[irl]]$obs.DaysLeadTime = obs.DaysLeadTime
    }
  }

  return(list(remaptime.obs2fcst=list(method=method,extrapolate=extrapolate),res.list=rl))

}
