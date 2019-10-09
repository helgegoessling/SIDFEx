sidfex.consensus <- function (TargetID = "POLARSTERN01",
                              init.ydoy = NULL,
                              shortterm.include = c("ukmo001v1_cplNWPv1",
                                                    "ukmo001v1_FOAMv1",
                                                    "ukmo001v1_cplNWP-HRv1",
                                                    "nrl001_gofs3.1-shortrange",
                                                    "eccc001_giops",
                                                    "esrl001_SeaIceVelocity",
                                                    "metno001_RK2",
                                                    "ncep001_freedrift-ensmean",
                                                    "dmi001_Forecast5d"),
                              shortterm.age.max = 3,
                              shortterm.remainrange.min = 3,
                              shortterm.replicate.max = 13,
                              #subseas.replicate.max = 6,
                              leadtimes = seq(0,124),
                              submitted.within = Inf,
                              verbose=TRUE) {

  ##########################################

  now = unclass(as.POSIXlt(Sys.time(),tz="GMT"))
  now.dayfrac = (now$hour + (now$min + now$sec/60)/60)/24
  now.doy = now$yday + 1 + now.dayfrac
  now.year = now$year + 1900
  if (is.null(init.ydoy)) {
    init.doy = now.doy
    init.year = now.year
  } else {
    if (any(!(c("Year","DayOfYear") %in% names(init.ydoy)))) {
      stop("argument 'init.ydoy' must be a list with elements 'Year' and 'DayOfYear', or NULL")
    }
    init.doy = init.ydoy$DayOfYear
    init.year = init.ydoy$Year
  }

  obs = sidfex.read.obs(TargetID = TargetID)
  Nobs = nrow(obs$data)
  obs.last.year = obs$data$Year[Nobs]
  obs.last.doy = obs$data$POS_DOY[Nobs]
  if (obs.last.year < init.year || (obs.last.year == init.year && obs.last.doy < init.doy)) {
    if (is.null(init.ydoy)) {
      if (verbose) {print(paste0("setting initial date to latest observation time (",obs.last.year,"-",obs.last.doy,")"))}
      init.year = obs.last.year
      init.doy = obs.last.doy
    } else {
      stop(paste0("latest observation (",obs.last.year,"-",obs.last.doy,") older than initial date (",
                  init.year,"-",init.doy,") for consensus forecast"))
    }
  }

  indx = sidfex.load.index()
  indx = indx[indx$TargetID == TargetID, ]
  indx_gidmid = paste0(indx$GroupID,"_",indx$MethodID)

  ##############################################
  # SELECT AND READ SEASONAL FORECAST
  indx.po.seas = sidfex.fcst.search.extractFromTable(gid = "ecmwf001", mid = "SEAS5", tid = TargetID, EnsParentOnly = TRUE)
  reltime.init.seas = sidfex.ydoy2reltime(indx.po.seas$InitYear, indx.po.seas$InitDayOfYear, init.year, init.doy)
  reltime.submit.seas = sidfex.ydoy2reltime(indx.po.seas$SubmitYear, indx.po.seas$SubmitDayOfYear, init.year, init.doy)
  if (any(reltime.init.seas > 0)) {
    if (verbose) {print("ignoring seasonal forecast(s) with initial time later than requested initial time")}
  }
  include.seas = TRUE
  if (!any(reltime.init.seas <= 0 & reltime.submit.seas <= submitted.within)) {
    warning("no seasonal (ecmwf_SEAS5) forecasts with initial time before requested initial time and submitted in time available")
  }
  reltime.fcst.init.seas = max(reltime.init.seas[reltime.init.seas <= 0 & reltime.submit.seas <= submitted.within])
  indx.po.seas = indx.po.seas[reltime.init.seas == reltime.fcst.init.seas, ]
  if (nrow(indx.po.seas) != 1) {
    stop("something is wrong, not exactly one seasonal (ecmwf) forecast (ensemble) left in index ...")
  }
  indx.seas = indx[indx$EnsParentFile == indx.po.seas$File, ]
  N.em = nrow(indx.seas)
  fcst.seas = sidfex.read.fcst(files = indx.seas, ens.merge = TRUE)
  fcst.seas.em.template = fcst.seas
  fcst.seas.em.template$res.list[[1]]$data = fcst.seas.em.template$res.list[[1]]$data[,1:5]
  fcst.seas.rot = sidfex.rot.fcst(obs = obs, fcst = fcst.seas, obsref.Year = init.year, obsref.DayOfYear = init.doy)
  fcst.seas.remaptime = sidfex.remaptime.fcst(fcst = fcst.seas.rot,
                                              newtime.DaysLeadTime = leadtimes - reltime.fcst.init.seas,
                                              extrapolate=TRUE)

  fcst.cons = fcst.seas.remaptime
  fcst.cons.data = fcst.seas.remaptime$res.list[[1]]$data
  fcst.cons.data$DaysLeadTime = leadtimes

  ##############################################
  # SELECT AND READ SHORT-TERM FORECASTS
  indx.st.list = NULL
  reltime.fcst.init.st.list = NULL
  N.st = 0
  include.actual = NULL
  include.actual.age = NULL
  include.actual.remainrange = NULL
  for (i in 1:length(shortterm.include)) {
    gm = shortterm.include[i]
    indx.st.i = indx[indx_gidmid == gm, ]
    if (nrow(indx.st.i) == 0) {
      if (verbose) {print(paste0("no ",gm," forecasts for this target available at all"))}
      next
    }
    reltime.init.st = sidfex.ydoy2reltime(indx.st.i$InitYear, indx.st.i$InitDayOfYear, init.year, init.doy)
    reltime.submit.st = sidfex.ydoy2reltime(indx.st.i$SubmitYear, indx.st.i$SubmitDayOfYear, init.year, init.doy)
    if (any(reltime.init.st > 0)) {
      if (verbose) {print(paste0("ignoring ",gm," forecast(s) with initial time later than requested initial time"))}
    }
    if (!any(reltime.init.st <= 0 & reltime.submit.st <= submitted.within & reltime.init.st >= -shortterm.age.max)) {
      if (verbose) {print(paste0("no ",gm," forecasts with initial time before requested initial time younger than ",
                                 shortterm.age.max,"d available"))}
      next
    }
    reltime.fcst.init.st = max(reltime.init.st[reltime.init.st <= 0 & reltime.submit.st <= submitted.within])
    indx.st = indx.st.i[reltime.init.st == reltime.fcst.init.st, ]
    if (nrow(indx.st) != 1) {
      stop("something is wrong, not exactly one ",gm," forecast left in index ...")
    }
    if (indx.st$FcstTime + reltime.fcst.init.st < shortterm.remainrange.min) {
      if (verbose) {print(paste0("insufficient range of latest ",gm," forecast"))}
      next
    }
    ### here one might read the forecast already once to check for NAs and, if NAs are included, omit the forecast...
    N.st = N.st + 1
    reltime.fcst.init.st.list[[N.st]] = reltime.fcst.init.st
    indx.st.list[[N.st]] = indx.st
    include.actual = c(include.actual, gm)
    include.actual.age = c(include.actual.age, -reltime.init.st)
    include.actual.remainrange = c(include.actual.remainrange, indx.st$FcstTime + reltime.fcst.init.st)
  }

  if (N.st == 0) {
    warning("no short-term forecasts matching the criteria found; consensus will be based only on longer-term (possibly non-NRT) forecasts")
  } else {
    if (verbose) {print(paste0("forecast initially using ",N.st," short-term forecasts"))}
    if (N.st*shortterm.replicate.max < N.em) {
      warning(paste0("low number of short-term forecasts (",N.st,
                     "); some ensemble members of longer-term forecasts are used from the beginning to avoid overconfidence"))
    }

    for (i in 1:N.st) {

      fcst.st = sidfex.read.fcst(files = indx.st.list[[i]], ens.merge = TRUE)
      fcst.st.rot = sidfex.rot.fcst(obs = obs, fcst = fcst.st, obsref.Year = init.year, obsref.DayOfYear = init.doy)
      fcst.st.remaptime = sidfex.remaptime.fcst(fcst = fcst.st.rot,
                                                  newtime.DaysLeadTime = leadtimes - reltime.fcst.init.st.list[[i]],
                                                  extrapolate=FALSE)
      fcst.st.remainrange = indx.st.list[[i]]$FcstTime + reltime.fcst.init.st.list[[i]]

      ensmems = seq(i,min(N.em,i+N.st*(shortterm.replicate.max-1)),by=N.st)

      for (em in ensmems) {

        fcst.cons.data[,2*em+c(4,5)] = fcst.st.remaptime$res.list[[1]]$data[,c(3,4)]
        if (fcst.st.remainrange < max(leadtimes)) {
          fcst.seas.em = fcst.seas.em.template
          fcst.seas.em$res.list[[1]]$data[,c(3,4)] = fcst.seas$res.list[[1]]$data[,2*em+c(4,5)]
          lonlat.new = c(tail(fcst.st.rot$res.list[[1]]$data$Lon,1), tail(fcst.st.rot$res.list[[1]]$data$Lat,1))
          lonlat.orig.fcst = sidfex.remaptime.fcst(fcst = fcst.seas.em,
                                                  newtime.DaysLeadTime = fcst.st.remainrange - reltime.fcst.init.seas,
                                                  extrapolate=TRUE)
          lonlat.orig = c(lonlat.orig.fcst$res.list[[1]]$data[1,4], lonlat.orig.fcst$res.list[[1]]$data[1,3])
          fcst.seas.em.rot = sidfex.rot.fcst(fcst = fcst.seas.em, lonlat.orig = lonlat.orig, lonlat.new = lonlat.new)
          fcst.seas.em.remaptime = sidfex.remaptime.fcst(fcst = fcst.seas.em.rot,
                                                    newtime.DaysLeadTime = leadtimes - reltime.fcst.init.seas,
                                                    extrapolate=TRUE)
          which.fill = which(leadtimes > fcst.st.remainrange)
          fcst.cons.data[which.fill, 2*em+c(4,5)] = fcst.seas.em.remaptime$res.list[[1]]$data[which.fill, c(3,4)]
        }

      }

    }

    for (nldt in 1:length(leadtimes)) {
      baryc = sl.barycenter(lon = as.numeric(fcst.cons.data[nldt,2*(1:N.em)+5]),
                            lat = as.numeric(fcst.cons.data[nldt,2*(1:N.em)+4]),
                            rm.na = TRUE)
      fcst.cons.data[nldt, 3] = baryc$lat
      fcst.cons.data[nldt, 4] = baryc$lon
    }

  }

  fcst.cons$res.list[[1]]$data = fcst.cons.data
  fcst.cons$res.list[[1]]$fl = "CONSENSUS"
  fcst.cons$res.list[[1]]$checkfileformat.result = "CONSENSUS"
  fcst.cons$res.list[[1]]$SubmitYear = "CONSENSUS"
  fcst.cons$res.list[[1]]$SubmitDayOfYear = "CONSENSUS"
  fcst.cons$res.list[[1]]$ProcessedYear = "CONSENSUS"
  fcst.cons$res.list[[1]]$ProcessedDayOfYear = "CONSENSUS"
  fcst.cons$res.list[[1]]$GroupID = "CONSENSUS"
  fcst.cons$res.list[[1]]$MethodID = "CONSENSUS"
  fcst.cons$res.list[[1]]$InitYear = init.year
  fcst.cons$res.list[[1]]$InitDayOfYear = init.doy
  obs.init.fcst = sidfex.remaptime.fcst(fcst = fcst.seas.em.template, newtime.DaysLeadTime = -reltime.fcst.init.seas, extrapolate=FALSE)
  obs.init = sidfex.remaptime.obs2fcst(fcst = obs.init.fcst)
  fcst.cons$res.list[[1]]$InitLat = obs.init$res.list[[1]]$data$Lat[1]
  fcst.cons$res.list[[1]]$InitLon = obs.init$res.list[[1]]$data$Lon[1]
  fcst.cons$res.list[[1]]$Ntimesteps = nrow(fcst.cons$res.list[[1]]$data)
  fcst.cons$res.list[[1]]$FirstYear = fcst.cons$res.list[[1]]$data$Year[1]
  fcst.cons$res.list[[1]]$FirstDayOfYear = fcst.cons$res.list[[1]]$data$DayOfYear[1]
  fcst.cons$res.list[[1]]$FirstLat = fcst.cons$res.list[[1]]$data$Lat[1]
  fcst.cons$res.list[[1]]$FirstLon = fcst.cons$res.list[[1]]$data$Lon[1]
  fcst.cons$res.list[[1]]$LastYear = tail(fcst.cons$res.list[[1]]$data$Year,1)
  fcst.cons$res.list[[1]]$LastDayOfYear = tail(fcst.cons$res.list[[1]]$data$DayOfYear,1)
  fcst.cons$res.list[[1]]$LastLat = tail(fcst.cons$res.list[[1]]$data$Lat,1)
  fcst.cons$res.list[[1]]$LastLon = tail(fcst.cons$res.list[[1]]$data$Lon,1)
  fcst.cons$res.list[[1]]$MergedInitLat = rep(obs.init$res.list[[1]]$data$Lat[1], N.em+1)
  fcst.cons$res.list[[1]]$MergedInitLon = rep(obs.init$res.list[[1]]$data$Lon[1], N.em+1)

  consensus.info = list()
  consensus.info$Version = "20191009v0"
  consensus.info$GeneratedYear = now.year
  consensus.info$GeneratedDayOfYear = now.doy
  consensus.info$arguments = list()
  consensus.info$arguments$shortterm.include = shortterm.include
  consensus.info$arguments$shortterm.age.max = shortterm.age.max
  consensus.info$arguments$shortterm.remainrange.min = shortterm.remainrange.min
  consensus.info$arguments$shortterm.replicate.max = shortterm.replicate.max
  consensus.info$arguments$submitted.within = submitted.within
  consensus.info$stats = list()
  consensus.info$stats$include.actual = include.actual
  consensus.info$stats$include.actual.age = include.actual.age
  consensus.info$stats$include.actual.remainrange = include.actual.remainrange

  return(list(ens.merge = fcst.cons$ens.merge, remaptime.fcst = fcst.cons$remaptime.fcst,
         consensus.info = consensus.info, res.list = fcst.cons$res.list))

}
