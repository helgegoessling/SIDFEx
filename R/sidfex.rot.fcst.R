sidfex.rot.fcst <- function (obs=NULL,fcst,obsref.DaysLeadTime=0,obsref.Year=NULL,obsref.DayOfYear=NULL,lonlat.orig=NULL,lonlat.new=NULL,rot.InitX=TRUE,ensmean.based=FALSE,data.path=NULL) {

  require(spheRlab)

  # check whether fcst is a complete list of forecasts as returned from sidfex.read.fcst, or just
  # one element from such a list; in the latter case; then, define rl from fcst consistently
  if ("res.list" %in% names(fcst)) {
    rl = fcst$res.list
    single.element = FALSE
  } else if ("data" %in% names(fcst)) {
    rl = list(fcst)
    single.element = TRUE
  } else {
    stop("format of 'fcst' not recognised.")
  }

  if (is.null(obs) && is.null(lonlat.orig)) {
    obs = sidfex.read.obs(TargetID = unique(sapply(fcst$res.list,"[[","TargetID")),data.path=data.path)
  }

  if (is.null(lonlat.orig)) {
    timebased = TRUE
    if (!is.null(obsref.Year)) {
      if (is.null(obsref.DayOfYear)) {stop("If 'obsref.Year' is specified, 'obsref.DayOfYear' must be specified as well")}
      ydoybased = TRUE
      fcst.ref = sidfex.remaptime.fcst(fcst, newtime.FractionOfDay = obsref.DayOfYear - floor(obsref.DayOfYear),
                                       newtime.YearDayOfYear = data.frame(obsref.Year,floor(obsref.DayOfYear)))
    } else {
      ydoybased = FALSE
      fcst.ref = sidfex.remaptime.fcst(fcst, newtime.DaysLeadTime = obsref.DaysLeadTime)
    }
    obs.reltime = sidfex.ydoy2reltime(Year = obs$data$Year, DayOfYear = obs$data$POS_DOY,
                                      RefYear = fcst.ref$res.list[[1]]$data$Year, RefDayOfYear = fcst.ref$res.list[[1]]$data$DayOfYear)
    if (obs.reltime[1] > 0 || tail(obs.reltime,1) < 0) {
      stop("reference time outside the observational time range")
    }
    obs.ref = sidfex.remaptime.obs2fcst(obs=obs, fcst=fcst.ref, extrapolate = TRUE, method = "linear")
  } else {
    timebased = FALSE
    # determine rotation parameters based on lonlat.orig and lonlat.new so that they lie on the equator of the rotation
    xyz.orig = sl.lonlat2xyz(lonlat.orig)
    xyz.new = sl.lonlat2xyz(lonlat.new)
    dist.angle = sl.gc.dist(lon=c(lonlat.orig[1], lonlat.new[1]), lat=c(lonlat.orig[2], lonlat.new[2]))*360/2/pi
    pole.lonlat = sl.xyz2lonlat(sl.crossvec(xyz.orig,xyz.new))
    abg.1 = sl.lonlatrot2abg(lonlatrot = c(pole.lonlat,-dist.angle))
    abg.2 = sl.lonlatrot2abg(lonlatrot = c(pole.lonlat,0))
  }

  for (irl in 1:length(rl)) {

    LatCols = which(substr(names(rl[[irl]]$data), start = 1, stop = 3) == "Lat")
    LonCols = LatCols + 1
    Nens = length(LatCols)

    if (timebased) {
      lonlat.orig = c(fcst.ref$res.list[[irl]]$data$Lon[1], fcst.ref$res.list[[irl]]$data$Lat[1])
      lonlat.new = c(obs.ref$res.list[[irl]]$data$Lon[1], obs.ref$res.list[[irl]]$data$Lat[1])
      # determine rotation parameters based on lonlat.orig and lonlat.new so that they lie on the equator of the rotation
      xyz.orig = sl.lonlat2xyz(lonlat.orig)
      xyz.new = sl.lonlat2xyz(lonlat.new)
      dist.angle = sl.gc.dist(lon=c(lonlat.orig[1], lonlat.new[1]), lat=c(lonlat.orig[2], lonlat.new[2]))*360/2/pi
      pole.lonlat = sl.xyz2lonlat(sl.crossvec(xyz.orig,xyz.new))
      abg.1 = sl.lonlatrot2abg(lonlatrot = c(pole.lonlat,-dist.angle))
      abg.2 = sl.lonlatrot2abg(lonlatrot = c(pole.lonlat,0))
    }

    if (rot.InitX) {
      init.rot.tmp = sl.rot(lon = rl[[irl]]$InitLon, lat = rl[[irl]]$InitLat, alpha = abg.1[1], beta = abg.1[2], gamma = abg.1[3])
      init.rot = sl.rot(lon = init.rot.tmp$lon, lat = init.rot.tmp$lat, alpha = abg.2[1], beta = abg.2[2], gamma = abg.2[3], invert = TRUE)
      rl[[irl]]$InitLat = init.rot$lat
      rl[[irl]]$InitLon = init.rot$lon
    }

    for (iens in 1:Nens) {

      if (timebased && iens > 1 && !ensmean.based) {
        lonlat.orig = c(fcst.ref$res.list[[irl]]$data[,LonCols[iens]][1], fcst.ref$res.list[[irl]]$data[,LatCols[iens]][1])
        # determine rotation parameters based on lonlat.orig and lonlat.new so that they lie on the equator of the rotation
        xyz.orig = sl.lonlat2xyz(lonlat.orig)
        xyz.new = sl.lonlat2xyz(lonlat.new)
        dist.angle = sl.gc.dist(lon=c(lonlat.orig[1], lonlat.new[1]), lat=c(lonlat.orig[2], lonlat.new[2]))*360/2/pi
        pole.lonlat = sl.xyz2lonlat(sl.crossvec(xyz.orig,xyz.new))
        abg.1 = sl.lonlatrot2abg(lonlatrot = c(pole.lonlat,-dist.angle))
        abg.2 = sl.lonlatrot2abg(lonlatrot = c(pole.lonlat,0))
      }

      # do the rotation
      rot.tmp = sl.rot(lon = rl[[irl]]$data[,LonCols[iens]], lat = rl[[irl]]$data[,LatCols[iens]], alpha = abg.1[1], beta = abg.1[2], gamma = abg.1[3])
      rot = sl.rot(lon = rot.tmp$lon, lat = rot.tmp$lat, alpha = abg.2[1], beta = abg.2[2], gamma = abg.2[3], invert = TRUE)
      rl[[irl]]$data[,LatCols[iens]] = rot$lat
      rl[[irl]]$data[,LonCols[iens]] = rot$lon

      if (rot.InitX && !is.null(rl[[irl]]$MergedInitLat)) {
        init.rot.tmp = sl.rot(lon = rl[[irl]]$MergedInitLon[iens], lat = rl[[irl]]$MergedInitLat[iens], alpha = abg.1[1], beta = abg.1[2], gamma = abg.1[3])
        init.rot = sl.rot(lon = init.rot.tmp$lon, lat = init.rot.tmp$lat, alpha = abg.2[1], beta = abg.2[2], gamma = abg.2[3], invert = TRUE)
        rl[[irl]]$MergedInitLat[iens] = init.rot$lat
        rl[[irl]]$MergedInitLon[iens] = init.rot$lon
      }

    }

  }

  if (single.element) {
    return(rl[[1]])
  } else {
    return(list(ens.merge=fcst$ens.merge, rot.fcst.arguments=list(obsref.DaysLeadTime=obsref.DaysLeadTime,obsref.Year=obsref.Year,
                                                                obsref.DayOfYear=obsref.DayOfYear,lonlat.orig=lonlat.orig,
                                                                lonlat.new=lonlat.new,rot.InitX=rot.InitX,
                                                                ensmean.based=ensmean.based),
              res.list=rl))
  }

}
