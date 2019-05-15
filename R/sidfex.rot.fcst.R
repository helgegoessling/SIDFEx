sidfex.rot.fcst <- function (obs=NULL,fcst,lonlat.orig=NULL,lonlat.new=NULL,rot.InitX=FALSE,data.path=NULL) {

  require(spheRlab)

  rl = fcst$res.list

  if (is.null(obs)) {
    obs = sidfex.read.obs(TargetID = unique(sapply(fcst$res.list,"[[","TargetID")),data.path=data.path)
  }

  if (is.null(lonlat.orig)) {
    initial.correction = TRUE
    fcst.initial = sidfex.remaptime.fcst(fcst, newtime.DaysLeadTime = 0)
    obs.initial = sidfex.remaptime.obs2fcst(obs=obs, fcst=fcst.initial)
    rot.fcst.string = "Rotation based on initial forecast position difference from observation"
  } else {
    initial.correction = FALSE
    rot.fcst.string = paste0("Rotation based on difference between pre-defined points: lonlat.orig=",
                             lonlat.orig, ", lonlat.new=", lonlat.new)
  }

  for (irl in 1:length(rl)) {

    if (initial.correction) {
      #lonlat.orig = c(fcst$res.list[[irl]]$InitLon, fcst$res.list[[irl]]$InitLat)
      lonlat.orig = c(fcst.initial$res.list[[irl]]$data$Lon[1], fcst.initial$res.list[[irl]]$data$Lat[1])
      lonlat.new = c(obs.initial$res.list[[irl]]$data$Lon[1], obs.initial$res.list[[irl]]$data$Lat[1])
    }

    xyz.orig = sl.lonlat2xyz(lonlat.orig)
    xyz.new = sl.lonlat2xyz(lonlat.new)
    dist.angle = sl.gc.dist(lon=c(lonlat.orig[1], lonlat.new[1]), lat=c(lonlat.orig[2], lonlat.new[2]))*360/2/pi
    pole.lonlat = sl.xyz2lonlat(sl.crossvec(xyz.orig,xyz.new))
    abg.1 = sl.lonlatrot2abg(lonlatrot = c(pole.lonlat,-dist.angle))
    abg.2 = sl.lonlatrot2abg(lonlatrot = c(pole.lonlat,0))

    rot.tmp = sl.rot(lon = rl[[irl]]$data$Lon, lat = rl[[irl]]$data$Lat, alpha = abg.1[1], beta = abg.1[2], gamma = abg.1[3])
    rot = sl.rot(lon = rot.tmp$lon, lat = rot.tmp$lat, alpha = abg.2[1], beta = abg.2[2], gamma = abg.2[3], invert = TRUE)
    rl[[irl]]$data$Lat = rot$lat
    rl[[irl]]$data$Lon = rot$lon

    if (rot.InitX) {
      init.rot.tmp = sl.rot(lon = rl[[irl]]$InitLon, lat = rl[[irl]]$InitLat, alpha = abg.1[1], beta = abg.1[2], gamma = abg.1[3])
      init.rot = sl.rot(lon = init.rot.tmp$lon, lat = init.rot.tmp$lat, alpha = abg.2[1], beta = abg.2[2], gamma = abg.2[3], invert = TRUE)
      rl[[irl]]$InitLat = init.rot$lat
      rl[[irl]]$InitLon = init.rot$lon
    }

  }

  return(list(ens.merge=fcst$ens.merge, rot.fcst=rot.fcst.string, res.list=rl))

}
