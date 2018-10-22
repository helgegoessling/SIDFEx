\name{sidfex.remaptime.obs2fcst}
\alias{sidfex.remaptime.obs2fcst}
\title{Remap SIDFEx Observations to Forecasts in Time}
\usage{
sidfex.remaptime.obs2fcst(obs=NULL,fcst,method="linear",extrapolate=FALSE,return.remapinfo=FALSE,data.path=NULL)
}
\description{
Remap SIDFEx observations temporally to forecast times, with linear or nearest-neighbour interpolation.
}
\arguments{
  \item{obs}{
a list with SIDFEx observations as returned by \code{\link{sidfex.read.obs}}, or \code{NULL} (default). In the latter case, \code{\link{sidfex.read.obs}} is used to read the required observations.
}
  \item{fcst}{
a list with SIDFEx forecasts as returned by \code{\link{sidfex.read.fcst}}.
}
  \item{method}{
a character specifying which method to use for interpolation (and extrapolation). Valid methods are 'linear' (default) and 'nearestneighbour'.
}
  \item{extrapolate}{
a logical value specifying whether or not to extrapolate observations to times outside of the observational time range. Default is \code{FALSE}.
}
  \item{return.remapinfo}{
a logical value specifying whether or not to include information on the time remapping (indices and weights of respective earlier and later neighbour points, plus the vector \code{obs.DaysLeadTime}) in the return value. Mainly for debugging; default is \code{FALSE}.
}
  \item{data.path}{
\code{NULL} or a character specifying the directory of the forecast data. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{data.path.fcst} needs to be defined.
}
}
\value{
A list with two entries, the first of which named 'remaptime.obs2fcst' returns the arguments provided for 'method' and 'extrapolate'. The second entry named 'res.list' is a list with one entry for each forecast provided in 'fcst$res.list', with observed coordinates and, if provided, values (BP, Ts, TA, ...) interpolated to forecast times.
}
\note{
This function is used to evaluate SIDFEx forecasts, e.g., in \code{\link{sidfex.plot.speedangle}}.

Would be good to add parameters \code{interpolate.maxdist} and \code{extrapolate.maxdist} to determine how far (in time) interpolation and extrapolation are done, so that, if the time difference is larger, the resulting data points will be NA.
}
\author{
Helge Goessling
}
\seealso{
\code{\link{sidfex.read.fcst}}, \code{\link{sidfex.read.obs}}, \code{\link{sidfex.plot.speedangle}}
}
