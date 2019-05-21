\name{sidfex.ydoy2reltime}
\alias{sidfex.ydoy2reltime}
\title{Convert Year and DayOfYear to Days Since a Reference Time}
\usage{
sidfex.ydoy2reltime(Year, DayOfYear, RefYear = 2017, RefDayOfYear = 1.0)
}
\description{
Convert Year and DayOfYear to days since a reference time based on the Gregorian calendar
}
\arguments{
\item{Year}{
an integer scalar or vector specifying the year. Can also be character which will be coerced with \code{\link{as.integer}}.
}
\item{DayOfYear}{
a numeric scalar or vector specifying the day of the year. Can also be character which will be coerced with \code{\link{as.numeric}}.
}
\item{RefYear}{
an integer scalar specifying the reference year. Can also be character which will be coerced with \code{\link{as.integer}}.
}
\item{RefDayOfYear}{
a numeric scalar specifying the reference day of year. Can also be character which will be coerced with \code{\link{as.numeric}}.
}
}
\value{
a numeric vector of the same length as \code{DayOfYear}.
}
\note{
If \code{Year} has length 1 and \code{DayOfYear} has length >1, the same \code{Year} will be used for all values.

The convention for the day of the year is that January 1st begins with \code{DayOfYear=1.0}, that is, \code{DayOfYear=1.5} corresponds to January 1st 12:00.
}
\author{
Helge Goessling
}
\examples{
y = c(2015,2015,2015,2016,2016,2016,2017,2017,2017)
doy = rep(c(1,180,365),3)
DaysSinceRef = sidfex.ydoy2reltime(Year=y,DayOfYear=doy,RefYear=2016,RefDayOfYear=1)
print(DaysSinceRef)
}
