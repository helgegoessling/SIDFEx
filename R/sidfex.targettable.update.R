sidfex.targettable.update <- function(targettable.manual=NULL, targettable.out=NULL, download.obs=TRUE, data.path=NULL) {

  if (is.null(targettable.manual)) {
    targettable.manual = system.file("extdata","targettable_manual.txt",package="SIDFEx")
    warning("Make sure that your version of the SIDFEx package is up-to-date, otherwise the manual target table used as
            input might be outdated (new targets and/or information on discarded targets might be missing)!")
  }

  tt_header = scan(targettable.manual,n = 19,what="character", sep="\n")

  now = unclass(as.POSIXlt(Sys.time(),tz="GMT"))
  dayfrac = (now$hour + (now$min + now$sec/60)/60)/24
  doy = now$yday + 1 + dayfrac
  tt_header[3] = paste0("Last Automatic Update (Year-DayOfYear): ",now$year+1900,"-",sprintf("%.3f",doy))

  tt_in = read.table(file = targettable.manual, skip = 19, colClasses = "character", header = TRUE)
  tt_out = tt_in

  if (download.obs) {sidfex.download.obs(TargetID = tt_in$TargetID, data.path = data.path)}

  for (i in 1:nrow(tt_in)) {
    obs = sidfex.read.obs(TargetID = tt_in$TargetID[i], data.path = data.path)
    Nobs = nrow(obs$data)
    tt_out$Latest_Position_Year[i] = obs$data$Year[Nobs]
    tt_out$Latest_Position_DayOfYear[i] = obs$data$POS_DOY[Nobs]
    tt_out$Latest_Position_Lat[i] = obs$data$Lat[Nobs]
    tt_out$Latest_Position_Lon[i] = obs$data$Lon[Nobs]
  }

  if (!is.null(targettable.out)) {
    write(tt_header, file = targettable.out, append = FALSE)
    write.table(tt_out, file = targettable.out, append = TRUE, quote = FALSE, col.names = TRUE, row.names = FALSE)
  }

  return(tt_out)

}