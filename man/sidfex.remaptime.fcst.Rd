\name{sidfex.remaptime.fcst}
\alias{sidfex.remaptime.fcst}
\title{Remap SIDFEx Forecasts}
\usage{
sidfex.remaptime.fcst(fcst, newtime.DaysLeadTime = NULL, newtime.FractionOfDay = 0,
newtime.YearDayOfYear = NULL, method = "linear", extrapolate = FALSE, return.remapinfo = FALSE)
}
\description{
Remap SIDFEx forecast data temporally to specific lead times (or, not yet implemented, FractionOfDay,YearDayOfYear combinations).
}
\arguments{
  \item{fcst}{
a list with SIDFEx forecasts as returned by \code{\link{sidfex.read.fcst}} (or a single element from the \code{res.list} element of such a list).
}
  \item{newtime.DaysLeadTime}{
a numeric vector (or scalar) specifying the new time axis in days relative to the forecast initial time. If specified, all other arguments starting with \code{newtime} are ignored.
}
  \item{newtime.FractionOfDay}{
a numeric vector (or scalar) specifying the day fractions of the new time axis. Default is \code{0}, meaning that values will be provided at 00:00 UTC. If \code{newtime.YearDayOfYear=NULL}, all such times within the original forecast time range will be provided. Used only if \code{newtime.DaysLeadTime=NULL}.
}
  \item{newtime.YearDayOfYear}{
an \code{Nx2} integer matrix specifying the years and full days of year of the new time axis. Default is \code{NULL}, meaning that values will be provided for all days within the original forecast range. Used only if \code{newtime.DaysLeadTime=NULL}.
}
  \item{method}{
  a character specifying the interpolation method. Must be one of \code{"linear"} (default) and \code{"nearestneighbour"}.
}
  \item{extrapolate}{
  a logical value specifying whether or not to extrapolate to times outside the original forecast range. Relevant only if the new time axis is defined via \code{newtime.DaysLeadTime}. Default is \code{FALSE} which implies that corresponding locations will be \code{NA}. If \code{TRUE}, the method specified in the argument \code{method} will be used also for the extrapolation.
}
  \item{return.remapinfo}{
  a logical value specifying whether or not to return information on the remapping (nearest-neighbour-before indices and weights); if \code{TRUE}, the information will be added as extra columns to the trajectories contained in \code{res.list[[i]]$data}. Default is \code{FALSE}.
}
}
\value{
a list which is identical with the input \code{fcst}, except that the trajectories have been remapped (and possibly with extra columns for the remapping information). See the documentation of \code{\link{sidfex.read.fcst}} for details.
}
\note{
Based on the \code{\link{spheRlab}} function \code{\link{sl.trajectory.remaptime}}.
}
\author{
Helge Goessling
}
\seealso{
\code{\link{sidfex.read.fcst}}, \code{\link{sl.trajectory.remaptime}}
}
\examples{
# Load the forecast data index
index = sidfex.load.index()

# Show the first rows of the index to see which variables can be used to subselect the index
head(index)

# Select a subset containing one specific forecast ensemble
subind = sidfex.fcst.search.extractFromTable(gid = "ecmwf001" , tid = "300234063991680", iy = 2019, idoy = 1, return.dataframe = TRUE)

# Read the forecasts
fcst = sidfex.read.fcst(subind)

# Remap these seasonal-range forecasts to the first ten days only
fcst.remapped = sidfex.remaptime.fcst(fcst,newtime.DaysLeadTime=0:10)

# Plot latitude against forecast lead time for the individual ensemble members and the ensemble mean
dat = fcst.remapped$res.list[[1]]$data
times = dat$DaysLeadTime
lats = dat[,substr(names(dat),1,3)=="Lat"]
plot(NA,xlim=range(times),ylim=range(lats),xlab="Forecast Lead Time (Days)",ylab="Latitude (Degrees North)")
for (i in 2:ncol(lats)) {lines(times,lats[,i], col="grey")}
lines(times,lats[,1],col="black",lwd=2)

# Load and plot the corresponding observations
obs = sidfex.read.obs(subind)
obs.remapped = sidfex.remaptime.obs2fcst(obs,fcst.remapped)
lines(times,obs.remapped$res.list[[1]]$data$Lat,col="red",lwd=2)

# You see that the initial location of the forecast ensemble is slightly offset from the true initial location (due to some subtle intricacies of the tracing method used in the ecmwf001 forecasts). So let's correct the initial location of the forecast ensemble to match the initial observed position:
fcst.corrected = sidfex.rot.fcst(obs = obs, fcst = fcst.remapped)
dat.corrected = fcst.corrected$res.list[[1]]$data
lats.corrected = dat.corrected[,substr(names(dat.corrected),1,3)=="Lat"]
for (i in 2:ncol(lats.corrected)) {lines(times,lats.corrected[,i], col="cyan")}
lines(times,lats.corrected[,1],col="blue",lwd=2)

# Now let's imagine a real-time scenario where the forecast is already 3 days old (current date is 2019 January 4th, 00:00UTC), but we still want to use the forecast to estimate the future trajectory. We can do so by rotating the forecast (and all ensemble members) based on the current observed position:
fcst.realtime = sidfex.rot.fcst(obs = obs, fcst = fcst.remapped, obsref.Year=2019, obsref.DayOfYear=4)
dat.realtime = fcst.realtime$res.list[[1]]$data
lats.realtime = dat.realtime[,substr(names(dat.realtime),1,3)=="Lat"]
for (i in 2:ncol(lats.realtime)) {lines(times,lats.realtime[,i], col="orange")}
lines(times,lats.realtime[,1],col="brown",lwd=2)

# Let's replot the observations to make it visible again
lines(times,obs.remapped$res.list[[1]]$data$Lat,col="red",lwd=2)

}
