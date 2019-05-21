sidfex.reltime2ydoy <- function(reltime,RefYear=2017,RefDayOfYear=1.0) {

  if (!is.numeric(reltime)) {reltime = as.numeric(reltime)}
  if (!is.numeric(RefYear)) {RefYear = as.integer(RefYear)}
  if (!is.numeric(RefDayOfYear)) {RefDayOfYear = as.numeric(RefDayOfYear)}

  if (!any(!is.na(reltime))) {stop("'reltime' must contain at least one non-NA numeric time")}
  if (is.na(RefYear)) {stop("'RefYear' must be numeric or coercible to numeric")}
  if (is.na(RefDayOfYear)) {stop("'RefDayOfYear' must be numeric or coercible to numeric")}

  N = length(reltime)
  minrt = min(reltime)
  maxrt = max(reltime)
  Year = as.numeric(rep(NA,N))
  DayOfYear = as.numeric(rep(NA,N))

  yearBreak.before = 1 - RefDayOfYear
  yearBreak.after = 366 - RefDayOfYear + as.integer(((yr %% 4 == 0) & (yr %% 100 != 0)) | (yr %% 400 == 0))
  inds = which(reltime >= yearBreak.before & reltime < yearBreak.after)
  if (!is.null(inds)) {
    Year[inds] = RefYear
    DayOfYear[inds] = reltime[inds] - yearBreak.before + 1
  }

  yrb.before = yearBreak.before
  yr = RefYear
  while (minrt < yrb.before) {
    yr = yr - 1
    yrb.after = yrb.before
    yrb.before = yrb.before - 365 - as.integer(((yr %% 4 == 0) & (yr %% 100 != 0)) | (yr %% 400 == 0))
    inds = which(reltime >= yrb.before & reltime < yrb.after)
    if (!is.null(inds)) {
      Year[inds] = yr
      DayOfYear[inds] = reltime[inds] - yrb.before + 1
    }
  }

  yrb.after = yearBreak.after
  yr = RefYear
  while (maxrt >= yrb.after) {
    yr = yr + 1
    yrb.before = yrb.after
    yrb.after = yrb.after + 365 + as.integer(((yr %% 4 == 0) & (yr %% 100 != 0)) | (yr %% 400 == 0))
    inds = which(reltime >= yrb.before & reltime < yrb.after)
    if (!is.null(inds)) {
      Year[inds] = yr
      DayOfYear[inds] = reltime[inds] - yrb.before + 1
    }
  }

  return(list(Year=Year, DayOfYear=DayOfYear))

}
