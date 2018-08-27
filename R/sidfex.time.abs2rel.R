sidfex.time.abs2rel <- function(Year=NULL,DayOfYear=NULL,Ref.Year=2000,Ref.DayOfYear=1,Units="days") {

  if (length(Year) != length(DayOfYear)) {
    stop("Year and DayOfYear must have the same length")
  }

  if (units != "days") {

  }

  return(list(Ref.Time=list(Ref.Year=Ref.Year)))

}
