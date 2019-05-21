\name{sidfex.reltime2ydoy}
\alias{sidfex.reltime2ydoy}
\title{Convert Days Since a Reference Time to Year and DayOfYear}
\usage{
sidfex.reltime2ydoy(reltime, RefYear = 2017, RefDayOfYear = 1.0)
}
\description{
Convert days since a reference time to Year and DayOfYear based on the Gregorian calendar
}
\arguments{
\item{reltime}{
a numeric vector specifying the time in days relative to the reference time provided by \code{RefYear} and \code{RefDayOfYear}. Can also be character which will be coerced with \code{\link{as.numeric}}.
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

\item{Year}{
an integer vector providing the year.
}
\item{DayOfYear}{
a numeric vector providing the day of the year.
}

}
\note{
The convention for the day of the year is that January 1st begins with \code{DayOfYear=1.0}, that is, \code{DayOfYear=1.5} corresponds to January 1st 12:00.
}
\author{
Helge Goessling
}
\examples{
reltime = seq(-100,400,pi*10)
ydoy = sidfex.reltime2ydoy(reltime=reltime,RefYear=2016,RefDayOfYear=1)
print(ydoy)
}
