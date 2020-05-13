sidfex.reduce.obs <- function(TargetID=NULL, obs.out.path=NULL, obs.old.freq.days=1, obs.old.dayfracs=0, obs.fullfreq.days=2, truncate.with.targettable=TRUE, targettable.manual=NULL, download.obs=TRUE, data.path=NULL, keep.vars=NULL) {

  if (truncate.with.targettable && is.null(targettable.manual)) {
    targettable.manual = system.file("extdata","targettable_manual.txt",package="SIDFEx")
    warning("Make sure that your version of the SIDFEx package is up-to-date, otherwise the manual target table used as
            input might be outdated (new targets and/or information on discarded targets might be missing)!")
  }

  if (obs.old.freq.days != ceiling(obs.old.freq.days)) {
    warning("rounded 'obs.old.freq.days' up (must be an integer)")
    obs.old.freq.days = ceiling(obs.old.freq.days)
  }
  if (obs.old.freq.days > 1 && length(obs.old.dayfracs) != 1) {
    warning("using only first element of 'obs.old.dayfracs' because 'obs.old.freq.days'>1")
    obs.old.dayfracs = obs.old.dayfracs[1]
  }

  now = unclass(as.POSIXlt(Sys.time(),tz="GMT"))
  dayfrac = (now$hour + (now$min + now$sec/60)/60)/24
  doy = now$yday + 1 + dayfrac
  RefYear = now$year+1900
  RefDayOfYear = doy

  tt_in = read.table(file = targettable.manual, skip = 19, colClasses = "character", header = TRUE)
  if (is.null(TargetID)) {
    tt = tt_in
  } else {
    if (!any(tt_in$TargetID %in% TargetID)) {
      print(tt_in$TargetID)
      print(TargetID)
      stop("'TargetID'(s) not found in the target table")
    }
    tt = tt_in[tt_in$TargetID %in% TargetID, ]
  }
  TargetID = tt$TargetID

  out.list = list()

  for (i in 1:nrow(tt)) {

    tid = TargetID[i]
    if (download.obs) {sidfex.download.obs(TargetID = tid, data.path = data.path)}
    obs = sidfex.read.obs(TargetID = tid, data.path = data.path)$data
    Nobs = nrow(obs)
    obs = obs[, names(obs) %in% c("Year","POS_DOY","Lat","Lon",keep.vars)]

    # contruct new time axis
    reltime.orig = sidfex.ydoy2reltime(obs$Year, obs$POS_DOY, RefYear, RefDayOfYear)
    keep.nodouble = c(1,which(reltime.orig[2:Nobs] > reltime.orig[1:(Nobs-1)])+1)
    while (length(keep.nodouble) < Nobs) {
      Nobs = length(keep.nodouble)
      reltime.orig = reltime.orig[keep.nodouble]
      obs = obs[keep.nodouble,]
      keep.nodouble = c(1,which(reltime.orig[2:Nobs] > reltime.orig[1:(Nobs-1)])+1)
      warning("removed non-monotonically increasing time entries from original observations")
    }
    reltime.new.firstday = sidfex.ydoy2reltime(Year = obs$Year[1], DayOfYear = floor(obs$POS_DOY[1]),
                                            RefYear = RefYear, RefDayOfYear = RefDayOfYear)
    reltime.new.lastday = sidfex.ydoy2reltime(Year = tail(obs$Year,1), DayOfYear = floor(tail(obs$POS_DOY,1)),
                                               RefYear = RefYear, RefDayOfYear = RefDayOfYear)
    reltime.new.days = seq(reltime.new.firstday,max(0,reltime.new.lastday),obs.old.freq.days)
    reltime.new = c(rep(reltime.new.days, each=length(obs.old.dayfracs)) + rep(obs.old.dayfracs, length(reltime.new.days)))
    reltime.new = reltime.new[reltime.new >= reltime.orig[1] & reltime.new <= -obs.fullfreq.days]
    if (length(reltime.new) == 0 || tail(reltime.orig,1) > tail(reltime.new,1)) {
      if (tail(reltime.orig,1) > -obs.fullfreq.days) {
        reltime.new = c(reltime.new,reltime.orig[reltime.orig > -obs.fullfreq.days])
      } else {
        reltime.new = c(reltime.new,tail(reltime.orig,1))
      }
    }
    if (truncate.with.targettable) {
      tt.first = sidfex.ydoy2reltime(tt$SIDFEx_First_Year[i], tt$SIDFEx_First_DayOfYear[i], RefYear, RefDayOfYear)
      if (is.na(as.numeric(tt$SIDFEx_Last_DayOfYear[i]))) {
        reltime.new = reltime.new[reltime.new >= tt.first]
      } else {
        tt.last = sidfex.ydoy2reltime(tt$SIDFEx_Last_Year[i], tt$SIDFEx_Last_DayOfYear[i], RefYear, RefDayOfYear)
        reltime.new = reltime.new[reltime.new >= tt.first & reltime.new <= tt.last]
      }
    }
    obs.out.remap = sl.trajectory.remaptime(reltime.orig, obs$Lat, obs$Lon, reltime.new, return.remapinfo = TRUE)
    obs.out.ydoy = sidfex.reltime2ydoy(reltime.new, RefYear, RefDayOfYear)
    obs.out = data.frame(Year = obs.out.ydoy$Year,
                         POS_DOY = round(obs.out.ydoy$DayOfYear,digits = 3),
                         Lat = round(obs.out.remap$Lat, digits = 5),
                         Lon = round(obs.out.remap$Lon, digits = 5))
    if (!is.null(keep.vars) && any(names(obs) %in% keep.vars)) {
      warning("'keep.vars' not yet implemented (too lazy)")
      #for (var in which(names(obs) %in% keep.vars)) {
      #
      #  obs.out = cbind(obs.out,
    }

    if (!is.null(obs.out.path)) {
      write.table(obs.out, file = file.path(obs.out.path,paste0(tid,".txt")), append = FALSE, quote = FALSE, col.names = TRUE, row.names = FALSE)
    }

    out.list[[i]] = obs.out

  }

  if (length(out.list) > 1) {
    names(out.list) = TargetID
    return(out.list)
  } else {
    return(out.list[[1]])
  }

}
