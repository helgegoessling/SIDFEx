\name{sidfex.rot.fcst}
\alias{sidfex.rot.fcst}
\title{Rotate (Adjust) SIDFEx Forecasts}
\usage{
sidfex.rot.fcst(obs = NULL, fcst, obsref.DaysLeadTime = 0, obsref.Year = NULL, obsref.DayOfYear = NULL,
lonlat.orig = NULL, lonlat.new = NULL, rot.InitX = FALSE, ensmean.based = FALSE, data.path = NULL)
}
\description{
Rotate SIDFEx forecasts, typically to correct their initial position or to adjust them based on the most recent observed position.
}
\arguments{
  \item{obs}{
a list with SIDFEx observations as returned by \code{\link{sidfex.read.obs}}, or \code{NULL} (default). Used to determine the rotation parameters based on observed positions only if \code{lonlat.orig} and \code{lonlat.new} are not provided. If \code{sidfex.read.obs=NULL}, \code{\link{sidfex.read.obs}} is used to read the required observations.
}
  \item{fcst}{
a list with SIDFEx forecasts as returned by \code{\link{sidfex.read.fcst}} (or a single element from the \code{res.list} element of such a list).
}
  \item{obsref.DaysLeadTime}{
a numeric scalar specifying the time in days relative to the respective forecast initial time to use to adjust the forecast trajectories based on the corresponding observed position. Default is \code{0}. Not used if \code{obsref.Year} and \code{obsref.DayOfYear} are provided.
}
  \item{obsref.Year}{
an integer specifying the year of the time to use to adjust the forecast trajectories based on the corresponding observed position. Default is \code{NULL}, in which case the rotation parameters are determined based either on \code{obsref.DaysLeadTime} or on \code{lonlat.orig} and \code{lonlat.new}.
}
  \item{obsref.DayOfYear}{
a numeric scalar specifying the day of year of the time to use to adjust the forecast trajectories based on the corresponding observed position. Used only in combination with \code{obsref.Year}.
}
  \item{lonlat.orig}{
a 2-element (longitude-latitude) vector specifying the original position of a reference point. Used only in combination with \code{lonlat.new}, which is the new position of the reference point after the rotation. The latter is performed such that \code{lonlat.orig} and \code{lonlat.new} are located on the equator of the rotation. Default is \code{NULL}, in which case these points are determined based on deviations from observed positions, using either \code{obsref.DaysLeadTime} or \code{obsref.Year} and \code{obsref.DayOfYear}.
}
  \item{lonlat.new}{
a 2-element (longitude-latitude) vector specifying the new position of a reference point after the rotation. Used only in combination with \code{lonlat.orig}.
}
  \item{rot.InitX}{
a logical value specifying whether also to rotate variables outside the \code{data} matrix corresponding to initial locations. Default is \code{TRUE}.
}
  \item{ensmean.based}{
a logical value specifying whether individual ensemble member shall be rotated using the same rotation parameters as for the rotation of the ensemble mean. Default is \code{FALSE}, in which case each member is adjusted individually.
}
  \item{data.path}{
\code{NULL} or a character specifying the directory of the forecast (and observational) data. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{data.path.fcst} (and \code{data.path.obs}) needs to be defined.
}
}
\value{
a list which is identical with the input \code{fcst}, except that the trajectories have been rotated. See the documentation of \code{\link{sidfex.read.fcst}} for details.
}
\note{
Based on several \code{\link{spheRlab}} functions.
}
\author{
Helge Goessling
}
\seealso{
\code{\link{sidfex.read.fcst}}, \code{\link{sidfex.read.obs}}
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
