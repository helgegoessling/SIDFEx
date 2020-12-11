sidfex.trajectory.fix <- function (reltime, lon, lat, speed.max = 80, fill=TRUE, buffer=0, verbose=TRUE) {

  require(spheRlab)

  N = length(reltime)
  if (N < 2) {
    warning("'reltime' (and 'lon' and 'lat') must contain at least 2 elements each. Returning lon and lat unchanged.")
    return(list(lon=lon, lat=lat))
  }

  dst = sl.gc.dist(lon, lat, Rsphere = 6371)
  spd = dst / (reltime[2:N] - reltime[1:(N-1)])

  while (max(spd,na.rm = TRUE) > speed.max) {

    if (verbose) {print(paste0("max speed ",max(spd,na.rm = TRUE),"km/day > ",speed.max,"km/day"))}
    lon[spd > speed.max] = NA
    lon[which(spd > speed.max)+1] = NA
    lat[spd > speed.max] = NA
    lat[which(spd > speed.max)+1] = NA

    if (fill) {
      na.start = which(!is.na(lon[1:(N-1)]) & is.na(lon[2:N]))
      na.end = which(is.na(lon[1:(N-1)]) & !is.na(lon[2:N])) + 1
      if (length(na.start) > 0 && length(na.end) > 0) {
        if (na.start[1] < tail(na.end,1)) {
          # there is at least one segment of NAs that can be filled
          if (na.end[1] <= na.start[1]) {na.end = na.end[2:length(na.end)]}
          na.start = na.start[1:length(na.end)]
          for (i in 1:length(na.end)) {
            i1 = na.start[i]
            i2 = na.end[i]
            lola = sl.trajectory.remaptime(oldtime = reltime[c(i1,i2)], oldlat = lat[c(i1,i2)], oldlon = lon[c(i1,i2)],
                                           newtime = reltime[(i1+1):(i2-1)])
            lat[(i1+1):(i2-1)] = lola$Lat
            lon[(i1+1):(i2-1)] = lola$Lon
          }
        }
      }
    }

    dst = sl.gc.dist(lon, lat, Rsphere = 6371)
    spd = dst / (reltime[2:N] - reltime[1:(N-1)])

    if (sum(!is.na(spd)) == 0) {break}

  }

  return(list(lon=lon, lat=lat))

}
