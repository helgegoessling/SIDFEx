sidfex.evaluate <- function (obs=NULL,fcst,do.speedangle=TRUE,ens.stats.na.rm=TRUE,do.multifcst.stats=TRUE,multifcst.stats.na.rm=TRUE,data.path=NULL,verbose=TRUE) {

  require(spheRlab)

  if (is.null(obs)) {
    obs = sidfex.read.obs(TargetID = unique(sapply(fcst$res.list,"[[","TargetID")),data.path=data.path)
  }
  if ("remaptime.obs2fcst" %in% names(obs)) {
    if (length(obs$res.list) != length(fcst$res.list)) {
      stop("'obs' is seemingly an output from 'sidfex.remaptime.obs2fcst()', but number of 'fcst' and 'obs' objects is inconsistent")
    }
    if (verbose) {warning("'obs' is seemingly an output from 'sidfex.remaptime.obs2fcst()'; assuming that it is completely consistent with 'fcst'")}
    do.obs2fcst = FALSE
    obs2fcst = obs
  } else {
    do.obs2fcst = TRUE
    obs2fcst = sidfex.remaptime.obs2fcst(obs,fcst,data.path=data.path,verbose=verbose)
  }

  # check whether fcst is a complete list of forecasts as returned from sidfex.read.fcst, or just
  # one element from such a list; in the latter case; then, define rl from fcst consistently
  if ("res.list" %in% names(fcst)) {
    rl.orig = fcst$res.list
    rl = list()
    obs.rl = obs2fcst$res.list
    single.element = FALSE
  } else if ("data" %in% names(fcst)) {
    rl.orig = list(fcst)
    rl = list()
    obs.rl = list(obs2fcst)
    single.element = TRUE
  } else {
    stop("format of 'fcst' not recognised.")
  }

  for (irl in 1:length(rl.orig)) {

    rl[[irl]] = list()

    LatCols = which(substr(names(rl.orig[[irl]]$data), start = 1, stop = 3) == "Lat")
    LonCols = LatCols + 1
    Nens = length(LatCols)

    obs.lat = obs.rl[[irl]]$data$Lat
    obs.lon = obs.rl[[irl]]$data$Lon

    nTimeSteps = length(obs.lat)
    if (nTimeSteps != nrow(rl.orig[[irl]]$data) || any(obs.rl[[irl]]$data$Year != rl.orig[[irl]]$data$Year |
                                                  obs.rl[[irl]]$data$DayOfYear != rl.orig[[irl]]$data$DayOfYear)) {
      stop("time axis mismatch between 'obs2fcst' and 'fcst', consider setting 'obs2fcst=NULL'")
    }

    rl[[irl]]$ens.mean.gc.dist = as.numeric(rep(NA,nTimeSteps))
    rl[[irl]]$ens.mean.lat.err = as.numeric(rep(NA,nTimeSteps))
    rl[[irl]]$ens.mean.lon.err = as.numeric(rep(NA,nTimeSteps))
    if (Nens > 1) {
      rl[[irl]]$ens.individual.gc.dist = matrix(rep(NA,nTimeSteps*(Nens-1)),ncol=(Nens-1))
      rl[[irl]]$ens.individual.lat.err = matrix(rep(NA,nTimeSteps*(Nens-1)),ncol=(Nens-1))
      rl[[irl]]$ens.individual.lon.err = matrix(rep(NA,nTimeSteps*(Nens-1)),ncol=(Nens-1))
      rl[[irl]]$ens.individual.gc.dist.mean = as.numeric(rep(NA,nTimeSteps))
      rl[[irl]]$ens.individual.lat.err.mean = as.numeric(rep(NA,nTimeSteps))
      rl[[irl]]$ens.individual.lat.err.meanabs = as.numeric(rep(NA,nTimeSteps))
      rl[[irl]]$ens.individual.lon.err.mean = as.numeric(rep(NA,nTimeSteps))
      rl[[irl]]$ens.individual.lon.err.meanabs = as.numeric(rep(NA,nTimeSteps))
      rl[[irl]]$ens.spread.gc.dist = as.numeric(rep(NA,nTimeSteps))
      rl[[irl]]$ens.spread.lat = as.numeric(rep(NA,nTimeSteps))
      rl[[irl]]$ens.spread.lon = as.numeric(rep(NA,nTimeSteps))
    }

    for (its in 1:nTimeSteps) {

      gc.dist = sl.gc.dist(lon=unlist(c(obs.lon[its],rl.orig[[irl]]$data[its,LonCols])),
                            lat=unlist(c(obs.lat[its],rl.orig[[irl]]$data[its,LatCols])),
                            sequential=FALSE,Rsphere=6371000)

      rl[[irl]]$ens.mean.gc.dist[its] = gc.dist[1]

      lat.err = unlist(rl.orig[[irl]]$data[its,LatCols] - obs.lat[its])
      rl[[irl]]$ens.mean.lat.err[its] = lat.err[1]

      lon.err = unlist(rl.orig[[irl]]$data[its,LonCols] - obs.lon[its])
      toosmall = (lon.err<=-180)
      toosmall[is.na(toosmall)] = FALSE
      lon.err[toosmall] = lon.err[toosmall] + 360
      toolarge = (lon.err>180)
      toolarge[is.na(toolarge)] = FALSE
      lon.err[toolarge] = lon.err[toolarge] - 360
      rl[[irl]]$ens.mean.lon.err[its] = lon.err[1]

      if (Nens > 1) {
        rl[[irl]]$ens.individual.gc.dist[its,] = gc.dist[2:Nens]
        rl[[irl]]$ens.individual.lat.err[its,] = lat.err[2:Nens]
        rl[[irl]]$ens.individual.lon.err[its,] = lon.err[2:Nens]
        rl[[irl]]$ens.individual.gc.dist.mean[its] = mean(gc.dist[2:Nens], na.rm = ens.stats.na.rm)
        rl[[irl]]$ens.individual.lat.err.mean[its] = mean(lat.err[2:Nens], na.rm = ens.stats.na.rm)
        rl[[irl]]$ens.individual.lat.err.meanabs[its] = mean(abs(lat.err[2:Nens]), na.rm = ens.stats.na.rm)
        rl[[irl]]$ens.individual.lon.err.mean[its] = mean(lon.err[2:Nens], na.rm = ens.stats.na.rm)
        rl[[irl]]$ens.individual.lon.err.meanabs[its] = mean(abs(lon.err[2:Nens]), na.rm = ens.stats.na.rm)
        spread.gc.dist = sl.gc.dist(lon=unlist(rl.orig[[irl]]$data[its,LonCols]),
                                    lat=unlist(rl.orig[[irl]]$data[its,LatCols]),
                                    sequential=FALSE,Rsphere=6371000)
        spread.lat = unlist(rl.orig[[irl]]$data[its,LatCols[2:Nens]] - rl.orig[[irl]]$data[its,LatCols[1]])
        spread.lon = unlist(rl.orig[[irl]]$data[its,LonCols[2:Nens]] - rl.orig[[irl]]$data[its,LonCols[1]])
        toosmall = (spread.lon<=-180)
        toosmall[is.na(toosmall)] = FALSE
        spread.lon[toosmall] = spread.lon[toosmall] + 360
        toolarge = (spread.lon>180)
        toolarge[is.na(toolarge)] = FALSE
        spread.lon[toolarge] = spread.lon[toolarge] - 360
        rl[[irl]]$ens.spread.gc.dist[its] = mean(spread.gc.dist, na.rm = ens.stats.na.rm)
        rl[[irl]]$ens.spread.lat[its] = mean(spread.lat, na.rm = ens.stats.na.rm)
        rl[[irl]]$ens.spread.lon[its] = mean(spread.lon, na.rm = ens.stats.na.rm)
      }

    }

    if (do.speedangle) {

      rl[[irl]]$ens.mean.relspeed = as.numeric(rep(NA,nTimeSteps))
      rl[[irl]]$ens.mean.angle = as.numeric(rep(NA,nTimeSteps))
      if (Nens > 1) {
        rl[[irl]]$ens.individual.relspeed = matrix(rep(NA,nTimeSteps*(Nens-1)),ncol=(Nens-1))
        rl[[irl]]$ens.individual.angle = matrix(rep(NA,nTimeSteps*(Nens-1)),ncol=(Nens-1))
        rl[[irl]]$ens.individual.relspeed.mean = as.numeric(rep(NA,nTimeSteps))
        rl[[irl]]$ens.individual.angle.mean = as.numeric(rep(NA,nTimeSteps))
        rl[[irl]]$ens.individual.angle.meanabs = as.numeric(rep(NA,nTimeSteps))
      }

      fcst.init = sidfex.remaptime.fcst(fcst=rl.orig[[irl]],newtime.DaysLeadTime=0,verbose=verbose)
      if (do.obs2fcst) {
        obs.init = sidfex.remaptime.obs2fcst(obs = obs, fcst = fcst.init, verbose = verbose)
      } else {
        #obs.init = sidfex.remaptime.fcst(fcst = obs2fcst, newtime.DaysLeadTime = 0)
        obs.init = sidfex.remaptime.fcst(fcst = obs.rl[[irl]], newtime.DaysLeadTime = 0)
      }

      abg = sl.lonlatrot2abg(lonlatrot=c(obs.init$data$Lon,obs.init$data$Lat,0))
      obs.rot = sl.rot(lon=obs.lon,lat=obs.lat,alpha=abg[1],beta=abg[2],gamma=abg[3])
      latx = (90-obs.rot$lat)

      # ensemble mean
      fcst.rot = sl.rot(lon=rl.orig[[irl]]$data$Lon,lat=rl.orig[[irl]]$data$Lat,alpha=abg[1],beta=abg[2],gamma=abg[3])
      rl[[irl]]$ens.mean.relspeed = (90-fcst.rot$lat) / latx
      ang = fcst.rot$lon - obs.rot$lon
      ang[latx==0] = NA
      toosmall = (ang<=-180)
      toosmall[is.na(toosmall)] = FALSE
      ang[toosmall] = ang[toosmall] + 360
      toolarge = (ang>180)
      toolarge[is.na(toolarge)] = FALSE
      ang[toolarge] = ang[toolarge] - 360
      rl[[irl]]$ens.mean.angle = ang

      if (Nens > 1) {

        for (iens in 2:Nens) {
          fcst.rot = sl.rot(lon=rl.orig[[irl]]$data[,LonCols[iens]],lat=rl.orig[[irl]]$data[,LatCols[iens]],alpha=abg[1],beta=abg[2],gamma=abg[3])
          rl[[irl]]$ens.individual.relspeed[,iens-1] = (90-fcst.rot$lat) / latx
          ang = fcst.rot$lon - obs.rot$lon
          ang[latx==0] = NA
          toosmall = (ang<=-180)
          toosmall[is.na(toosmall)] = FALSE
          ang[toosmall] = ang[toosmall] + 360
          toolarge = (ang>180)
          toolarge[is.na(toolarge)] = FALSE
          ang[toolarge] = ang[toolarge] - 360
          rl[[irl]]$ens.individual.angle[,iens-1] = ang
        }

        rl[[irl]]$ens.individual.relspeed.mean[] = rowMeans(rl[[irl]]$ens.individual.relspeed,na.rm = ens.stats.na.rm)
        rl[[irl]]$ens.individual.angle.mean[] = rowMeans(rl[[irl]]$ens.individual.angle,na.rm = ens.stats.na.rm)
        rl[[irl]]$ens.individual.angle.meanabs[] = rowMeans(abs(rl[[irl]]$ens.individual.angle),na.rm = ens.stats.na.rm)

      }

    }

  }

  multifcst.stats = NULL
  if (!single.element && do.multifcst.stats && length(rl)>1) {

    multifcst.stats.names = c("ens.mean.gc.dist","ens.mean.lat.err","ens.mean.lon.err","ens.individual.gc.dist.mean",
                              "ens.individual.lat.err.mean","ens.individual.lat.err.meanabs",
                              "ens.individual.lon.err.mean","ens.individual.lon.err.meanabs",
                              "ens.spread.gc.dist","ens.spread.lat","ens.spread.lon","ens.mean.relspeed","ens.mean.angle",
                              "ens.individual.relspeed.mean","ens.individual.angle.mean","ens.individual.angle.meanabs")
    multifcst.stats.names.present = NULL
    for (i in 1:length(rl)) {
      multifcst.stats.names.present = c(multifcst.stats.names.present,names(rl[[i]]))
    }
    multifcst.stats.names.present = unique(multifcst.stats.names.present)
    multifcst.stats.names = multifcst.stats.names[multifcst.stats.names %in% multifcst.stats.names.present]
    nTimeSteps = length(rl[[1]]$ens.mean.gc.dist)
    multifcst.stats.mats = list()

    for (irl in 1:length(rl)) {
      if (length(rl[[irl]]$ens.mean.gc.dist) != nTimeSteps ||
          any(rl.orig[[irl]]$data$DaysLeadTime != rl.orig[[1]]$data$DaysLeadTime)) {
        warning(paste("Multi-forecast statistics not possible, forecasts have inconsistent relative time axes.",
                      "Consider using sidfex.remaptime.fcst() first."))
        break
      }
      for (err.name in multifcst.stats.names) {
        add.vec = rl[[irl]][[err.name]]
        if (is.null(add.vec)) {
          multifcst.stats.mats[[err.name]] = cbind(multifcst.stats.mats[[err.name]], rep(NA,nTimeSteps))
        } else {
          multifcst.stats.mats[[err.name]] = cbind(multifcst.stats.mats[[err.name]], add.vec)
        }
      }
    }

    multifcst.stats = list()
    for (err.name in multifcst.stats.names) {
      multifcst.stats[[err.name]] = data.frame(
        mean = apply(multifcst.stats.mats[[err.name]], 1, function (x) {mean(x, na.rm=multifcst.stats.na.rm)}),
        st.dev = apply(multifcst.stats.mats[[err.name]], 1, function (x) {sd(x, na.rm=multifcst.stats.na.rm)}),
        st.err = apply(multifcst.stats.mats[[err.name]], 1, function (x) {sd(x, na.rm=multifcst.stats.na.rm)/sqrt(sum(!is.na(x)))}),
        median = apply(multifcst.stats.mats[[err.name]], 1, function (x) {median(x, na.rm=multifcst.stats.na.rm)})
      )
    }

  }

  if (single.element) {
    return(rl[[1]])
  } else {
    return(list(ens.merge=fcst$ens.merge,
                evaluate.arguments=list(do.speedangle=do.speedangle,do.multifcst.stats=do.multifcst.stats),
                res.list=rl,
                multifcst.stats=multifcst.stats))
  }

}
