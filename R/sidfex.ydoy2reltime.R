sidfex.ydoy2reltime <- function(Year,DayOfYear,RefYear=2017,RefDayOfYear=1.0) {

  if (!is.numeric(Year)) {Year = as.integer(Year)}
  if (!is.numeric(DayOfYear)) {DayOfYear = as.numeric(DayOfYear)}
  if (!is.numeric(RefYear)) {RefYear = as.integer(RefYear)}
  if (!is.numeric(RefDayOfYear)) {RefDayOfYear = as.numeric(RefDayOfYear)}

  if (sum(!is.na(Year) & !is.na(DayOfYear)) < 1) {stop("'Year' and 'DayOfYear' must contain at least one non-NA numeric data pair")}
  if (is.na(RefYear)) {stop("'RefYear' must be numeric or coercible to numeric")}
  if (is.na(RefDayOfYear)) {stop("'RefDayOfYear' must be numeric or coercible to numeric")}

  if (length(Year) == 1 && length(DayOfYear) > 1) {Year = rep(Year,length(DayOfYear))}
  if (length(Year) != length(DayOfYear)) {stop("'Year' and 'DayOfYear' must have the same length, or 'Year' must have length 1")}

  DaysSinceRefX = DayOfYear - RefDayOfYear
  notna = !is.na(DaysSinceRefX)
  DaysSinceRef = DaysSinceRefX[notna]
  Year = Year[notna]

  maxy = max(Year,na.rm=TRUE)
  miny = min(Year,na.rm=TRUE)

  if (miny < RefYear) {
    for (yr in RefYear:(miny+1)) {
      DaysSinceRef[Year < yr] = DaysSinceRef[Year < yr] - 365 - as.integer((((yr-1) %% 4 == 0) & ((yr-1) %% 100 != 0)) | ((yr-1) %% 400 == 0))
    }
  }
  if (maxy > RefYear) {
    for (yr in RefYear:(maxy-1)) {
      DaysSinceRef[Year > yr] = DaysSinceRef[Year > yr] + 365 + as.integer(((yr %% 4 == 0) & (yr %% 100 != 0)) | (yr %% 400 == 0))
    }
  }

  DaysSinceRefX[notna] = DaysSinceRef
  return(DaysSinceRefX)

}
