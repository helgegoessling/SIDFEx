sidfex.consensus <- function (TargetID = "POLARSTERN01",
                              init.ydoy = NULL,
                              shortterm.include = c("ukmo001v1_cplNWPv1",
                                                    "ukmo001v1_FOAMv1",
                                                    "ukmo001v1_cplNWP-HRv1",
                                                    "nrl001_gofs3.1-shortrange",
                                                    "eccc001_giops",
                                                    "esrl001_SeaIceVelocity",
                                                    "metno001_RK2"),
                                                    #"ncep001_freedrift-ensmean",
                                                    #"dmi001_Forecast5d"),
                              shortterm.age.max = 3,
                              shortterm.remainrange.min = 3,
                              shortterm.replicate.max = 17,
                              #subseas.replicate.max = 6,
                              leadtimes = seq(0,124),
                              submitted.within = Inf,
                              verbose=TRUE,
                              data.path.obs = NULL,
                              data.path.index = NULL,
                              data.path.fcst = NULL,
                              extrapolate.maxspeed = 10/6371,
                              read.obs.res = NULL) {

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

  if (is.null(read.obs.res)) {
    obs = sidfex.read.obs(TargetID = TargetID, data.path = data.path.obs)
  } else {
    obs = read.obs.res
  }
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

  indx = sidfex.load.index(indexTable.path = data.path.index)
  indx = indx[indx$TargetID == TargetID, ]
  indx_gidmid = paste0(indx$GroupID,"_",indx$MethodID)

  ##############################################
  # SELECT AND READ SEASONAL FORECAST
  indx.po.seas = sidfex.fcst.search.extractFromTable(gid = "ecmwf001", mid = "SEAS5", tid = TargetID, EnsParentOnly = TRUE, indexTable.path = data.path.index)
  seas.missing = FALSE
  if (nrow(indx.po.seas) == 0) {
    seas.missing = TRUE
    warning("no ecmwf001_SEAS5 forecast available for the specified target")
  } else {
    reltime.init.seas = sidfex.ydoy2reltime(indx.po.seas$InitYear, indx.po.seas$InitDayOfYear, init.year, init.doy)
    reltime.submit.seas = sidfex.ydoy2reltime(indx.po.seas$SubmitYear, indx.po.seas$SubmitDayOfYear, init.year, init.doy)
    if (any(reltime.init.seas > 0)) {
      if (verbose) {print("ignoring seasonal forecast(s) with initial time later than requested initial time")}
    }
    if (!any(reltime.init.seas <= 0 & reltime.submit.seas <= submitted.within)) {
      seas.missing = TRUE
      warning("no seasonal (ecmwf_SEAS5) forecasts with initial time before requested initial time and submitted in time available")
    }
  }

  if (!seas.missing) {
    reltime.fcst.init.seas = max(reltime.init.seas[reltime.init.seas <= 0 & reltime.submit.seas <= submitted.within])
    indx.po.seas = indx.po.seas[reltime.init.seas == reltime.fcst.init.seas, ]
    if (nrow(indx.po.seas) != 1) {
      return("something is wrong, not exactly one seasonal (ecmwf) forecast (ensemble) left in index ...")
    }
    indx.seas = indx[indx$EnsParentFile == indx.po.seas$File, ]
    N.em = nrow(indx.seas)
    fcst.seas = sidfex.read.fcst(files = indx.seas, ens.merge = TRUE, data.path = data.path.fcst)
  } else {
    warning("using dummy seasonal forecast")
    reltime.fcst.init.seas = 0
    N.em = 51
    fcst.seas = list()
    fcst.seas$ens.merge = TRUE
    fcst.seas$res.list = list()
    fcst.seas$res.list[[1]] = list()
    fcst.seas$res.list[[1]]$fl = "no file available, this is a dummy forecast"
    fcst.seas$res.list[[1]]$checkfileformat = "no file format checked, this is a dummy forecast"
    fcst.seas$res.list[[1]]$SubmitYear = NA
    fcst.seas$res.list[[1]]$SubmitDayOfYear = NA
    fcst.seas$res.list[[1]]$ProcessedYear = NA
    fcst.seas$res.list[[1]]$ProcessedDayOfYear = NA
    fcst.seas$res.list[[1]]$GroupID = NA
    fcst.seas$res.list[[1]]$MethodID = NA
    fcst.seas$res.list[[1]]$TargetID = TargetID
    fcst.seas$res.list[[1]]$InitYear = init.year
    fcst.seas$res.list[[1]]$InitDayOfYear = init.doy
    fcst.seas$res.list[[1]]$InitLat = 0
    fcst.seas$res.list[[1]]$InitLon = 0
    fcst.seas$res.list[[1]]$EnsMemNum = 1:51
    fcst.seas$res.list[[1]]$Ntimesteps = 125
    fcst.seas$res.list[[1]]$FirstYear = init.year
    fcst.seas$res.list[[1]]$FirstDayOfYear = init.doy
    fcst.seas$res.list[[1]]$FirstLat = 0
    fcst.seas$res.list[[1]]$FirstLon = 0
    seas.last.ydoy = sidfex.reltime2ydoy(reltime=124,RefYear=init.year,RefDayOfYear=init.doy)
    fcst.seas$res.list[[1]]$LastYear = seas.last.ydoy$Year
    fcst.seas$res.list[[1]]$LastDayOfYear = seas.last.ydoy$DayOfYear
    fcst.seas$res.list[[1]]$LastLat = 0
    fcst.seas$res.list[[1]]$LastLon = 0
    fcst.seas$res.list[[1]]$DaysForecastLength = 124
    seas.data = as.data.frame(matrix(rep(0,(5+2*51)*125),nrow=125))
    names(seas.data) = c("Year","DayOfYear","Lat","Lon","DaysLeadTime",paste0(rep(c("Lat","Lon"),51),rep(1:51,each=2)))
    seas.data.ydoy = sidfex.reltime2ydoy(reltime=0:124,RefYear=init.year,RefDayOfYear=init.doy)
    seas.data$Year = seas.data.ydoy$Year
    seas.data$DayOfYear = seas.data.ydoy$DayOfYear
    seas.data$DaysLeadTime = 0:124
    fcst.seas$res.list[[1]]$data = seas.data
    fcst.seas$res.list[[1]]$MergedInitLat = rep(0,52)
    fcst.seas$res.list[[1]]$MergedInitLon = rep(0,52)
    fcst.seas$index = "no index available, this is a dummy forecast"
  }

  for (i in 1:N.em) {
    lon.col = 5 + 2*i
    lat.col = lon.col - 1
    lat.dat = fcst.seas$res.list[[1]]$data[,lat.col]
    lon.dat = fcst.seas$res.list[[1]]$data[,lon.col]
    update.ensmean.dummy = FALSE
    if (anyNA(lat.dat) | anyNA(lon.dat)) {
      warning(paste0("NAs contained in ensemble member ",i," of the ecmwf_SEAS5 forecast, repeating last valid position"))
      update.ensmean.dummy = TRUE
      first.na = min(which(is.na(lat.dat) | is.na(lon.dat)))
      Nstp = length(lat.dat)
      if (first.na == 1) {
        if (is.na(fcst.seas$res.list[[1]]$InitLat) || is.na(fcst.seas$res.list[[1]]$InitLon)) {
          stop("no valid initial location found to fill NA-only ensemble members")
        }
        lat.dat = rep(fcst.seas$res.list[[1]]$InitLat,Nstp)
        lon.dat = rep(fcst.seas$res.list[[1]]$InitLon,Nstp)
      } else {
        lat.dat[first.na:Nstp] = lat.dat[first.na-1]
        lon.dat[first.na:Nstp] = lon.dat[first.na-1]
      }
      fcst.seas$res.list[[1]]$data[,lat.col] = lat.dat
      fcst.seas$res.list[[1]]$data[,lon.col] = lon.dat
    }
  }
  if (update.ensmean.dummy) {
    for (nldt in 1:nrow(fcst.seas$res.list[[1]]$data)) {
      baryc = sl.barycenter(lon = as.numeric(fcst.seas$res.list[[1]]$data[nldt,2*(1:N.em)+5]),
                            lat = as.numeric(fcst.seas$res.list[[1]]$data[nldt,2*(1:N.em)+4]),
                            rm.na = TRUE)
      fcst.seas$res.list[[1]]$data[nldt, 3] = baryc$lat
      fcst.seas$res.list[[1]]$data[nldt, 4] = baryc$lon
    }
    fcst.seas$res.list[[1]]$LastYear = tail(fcst.seas$res.list[[1]]$data$Year,1)
    fcst.seas$res.list[[1]]$LastDayOfYear = tail(fcst.seas$res.list[[1]]$data$DayOfYear,1)
  }

  fcst.seas.em.template = fcst.seas
  fcst.seas.em.template$res.list[[1]]$data = fcst.seas.em.template$res.list[[1]]$data[,1:5]
  fcst.seas.rot = sidfex.rot.fcst(obs = obs, fcst = fcst.seas, obsref.Year = init.year, obsref.DayOfYear = init.doy)
  fcst.seas.remaptime = sidfex.remaptime.fcst(fcst = fcst.seas.rot,
                                              newtime.DaysLeadTime = leadtimes - reltime.fcst.init.seas,
                                              extrapolate=TRUE, extrapolate.maxspeed = extrapolate.maxspeed)
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
    fcst.st.test = sidfex.read.fcst(files = indx.st, ens.merge = TRUE, data.path = data.path.fcst)
    if (anyNA(fcst.st.test$res.list[[1]]$data)) {
      if (verbose) {print(paste0("latest ",gm," forecast contains NA value(s)"))}
      next
    }
    N.st = N.st + 1
    reltime.fcst.init.st.list[[N.st]] = reltime.fcst.init.st
    indx.st.list[[N.st]] = indx.st
    include.actual = c(include.actual, gm)
    include.actual.age = c(include.actual.age, -reltime.fcst.init.st)
    include.actual.remainrange = c(include.actual.remainrange, indx.st$FcstTime + reltime.fcst.init.st)
  }
  include.actual = c(include.actual, "ecmwf001_SEAS5")
  include.actual.age = c(include.actual.age, -reltime.fcst.init.seas)
  include.actual.remainrange = c(include.actual.remainrange, fcst.seas$res.list[[1]]$DaysForecastLength + reltime.fcst.init.seas[1])

  if (N.st == 0) {
    warning("no short-term forecasts matching the criteria found; consensus will be based only on longer-term (possibly non-NRT) forecasts")
  } else {
    if (verbose) {print(paste0("forecast initially using ",N.st," short-term forecasts"))}
    if (N.st*shortterm.replicate.max < N.em) {
      warning(paste0("low number of short-term forecasts (",N.st,
                     "); some ensemble members of longer-term forecasts are used from the beginning to avoid overconfidence"))
    }

    for (i in 1:N.st) {

      fcst.st = sidfex.read.fcst(files = indx.st.list[[i]], ens.merge = TRUE, data.path = data.path.fcst)
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
                                                  extrapolate=TRUE, extrapolate.maxspeed = extrapolate.maxspeed)
          lonlat.orig = c(lonlat.orig.fcst$res.list[[1]]$data[1,4], lonlat.orig.fcst$res.list[[1]]$data[1,3])
          fcst.seas.em.rot = sidfex.rot.fcst(fcst = fcst.seas.em, lonlat.orig = lonlat.orig, lonlat.new = lonlat.new)
          fcst.seas.em.remaptime = sidfex.remaptime.fcst(fcst = fcst.seas.em.rot,
                                                    newtime.DaysLeadTime = leadtimes - reltime.fcst.init.seas,
                                                    extrapolate=TRUE, extrapolate.maxspeed = extrapolate.maxspeed)
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
  obs.init = sidfex.remaptime.obs2fcst(fcst = obs.init.fcst, obs = obs, extrapolate = TRUE, method = "linear")
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
  consensus.info$Version = "20200221v1"
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
