sidfex.obs.snapshot <- function (obs,Year,DayOfYear,method="nearestneighbour") {

  if (method != "nearestneighbour") {
    stop("Only method='nearestneighbour' implemented so far.")
  }

  yrs = obs$data$Year
  N = length(yrs)
  DaysSinceStart = obs$data$POS_DOY - obs$data$POS_DOY[1]
  snapshot.DaysSinceStart = DayOfYear - obs$data$POS_DOY[1]
  print(snapshot.DaysSinceStart)
  if (yrs[N] > yrs[1]) {
    for (yr in yrs[1]:(yrs[N]-1)) {
      DaysSinceStart[yrs > yr] = DaysSinceStart[yrs > yr] + 365 + as.integer(((yr %% 4 == 0) & (yr %% 100 != 0)) | (yr %% 400 == 0))
    }
  }
  if (Year < yrs[1]) {stop("Date out of range.")}
  if (Year > yrs[1]) {
    for (yr in yrs[1]:(Year-1)) {
      snapshot.DaysSinceStart = snapshot.DaysSinceStart + 365 + as.integer(((yr %% 4 == 0) & (yr %% 100 != 0)) | (yr %% 400 == 0))
    }
  }
  print(snapshot.DaysSinceStart)
  if (snapshot.DaysSinceStart < 0) {stop("Date out of range.")}
  if (snapshot.DaysSinceStart > max(DaysSinceStart)) {stop("Date out of range.")}
  ind = max(which(DaysSinceStart <= snapshot.DaysSinceStart))
  if (ind != N) {
    if (abs(DaysSinceStart[ind+1] - snapshot.DaysSinceStart) <= abs(DaysSinceStart[ind] - snapshot.DaysSinceStart)) {
      ind = ind + 1
    }
  }

  return(list(index=ind,data=obs$data[ind,]))

}
