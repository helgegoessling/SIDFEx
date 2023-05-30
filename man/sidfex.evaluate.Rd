\name{sidfex.evaluate}
\alias{sidfex.evaluate}
\title{Evaluate SIDFEx Forecasts Against Observations}
\usage{
sidfex.evaluate(obs = NULL, fcst, do.speedangle = TRUE, ens.stats.na.rm = TRUE, do.multifcst.stats = TRUE, multifcst.stats.na.rm = TRUE, data.path = NULL, verbose = TRUE)
}
\description{
Compute various types of errors of SIDFEx forecasts relative to observations and, if applicable, ensemble spread.
}
\arguments{
  \item{obs}{
a list with SIDFEx observations as returned by \code{\link{sidfex.read.obs}}, or \code{NULL} (default). If \code{NULL}, \code{\link{sidfex.read.obs}} is used to read the required observations.
}
  \item{fcst}{
a list with SIDFEx forecasts as returned by \code{\link{sidfex.read.fcst}} (or a single element from the \code{res.list} element of such a list).
}
  \item{do.speedangle}{
a logical value specifying whether relative speed and angle errors shall be computed in addition to great-circle distance and lon-lat errors.
}
  \item{ens.stats.na.rm}{
a logical value specifying whether to remove NAs before statistics are computed across ensemble members.
}
  \item{do.multifcst.stats}{
a logical value specifying whether statistics across all elements of \code{fcst$res.list} shall be computed (if \code{length(fcst$res.list)>1} and \code{fcst} is not a single element of such a list). Will result in an error if the forecast elements do not have identical time axes in terms of forecast lead time. This can be achieved by applying \code{\link{sidfex.remaptime.fcst}} with the argument \code{newtime.DaysLeadTime} first and providing the result as argument `fcst` to \code{sidfex.evaluate} (this function).
}
  \item{multifcst.stats.na.rm}{
a logical value specifying whether to remove NAs before statistics are computed across forecast elements.
}
  \item{data.path}{
\code{NULL} or a character specifying the directory of the observational data. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{data.path.obs} needs to be defined (used only if \code{obs=NULL}).
}
  \item{verbose}{
a logical value specifying whether to produce warnings in \code{sidfex.remaptime.fcst} and \code{sidfex.remaptime.obs2fcst}, in particular when new time axes reach outside original time axes. Default is \code{TRUE}.
}
}
\value{
If \code{fcst} is a list with SIDFEx forecasts as returned by \code{\link{sidfex.read.fcst}}, the return value is a list with elements \code{ens.merge} (inherited from \code{fcst}), \code{evaluate.arguments} (to document the logical values specified for \code{do.speedangle} and \code{do.multifcst.stats}), \code{res.list}, and optionally \code{multifcst.stats} if \code{do.multifcst.stats=TRUE} and \code{length(fcst$res.list)>1}. The element \code{res.list} holds one element for each corresponding element in \code{fcst$res.list}, with various types of forecast errors listed below. If \code{fcst} is a single forecast element, only a single list with the types of forecast errors is returned.

Forecast errors are computed for all \code{Nts} timesteps in \code{fcst} and, if the respective element of \code{fcst} is an ensemble, for all \code{Nens} ensemble members individually. If provided, \code{multifcst.stats} has four elements per element of \code{res.list} (except results for individual members) with the \code{mean}, \code{st.dev} (standard deviation), \code{st.err} (standard error), and \code{median} across the elements of \code{res.list}.

The errors provided are listed in the following. Those with 'individual' or 'spread' in their names are provided only for forecast elements composed of an ensemble.
\item{ens.mean.gc.dist}{
  a numeric vector of length \code{Nts} with the great-circle distance of the ensemble-mean position from the observed position
}
\item{ens.mean.lat.err}{
  a numeric vector of length \code{Nts} with the latitudinal error of the ensemble-mean position relative to the observed position
}
\item{ens.mean.lon.err}{
  a numeric vector of length \code{Nts} with the longitudinal error of the ensemble-mean position relative to the observed position
}
\item{ens.individual.gc.dist}{
  an \code{Nts x Nens} matrix with the great-circle distance of the individual ensemble-member positions from the observed position
}
\item{ens.individual.lat.err}{
  an \code{Nts x Nens} matrix with the latitudinal error of the individual ensemble-member positions from the observed position
}
\item{ens.individual.lon.err}{
  an \code{Nts x Nens} matrix with the longitudinal error of the individual ensemble-member positions from the observed position
}
\item{ens.individual.gc.dist.mean}{
  a numeric vector of length \code{Nts} with the mean great-circle distance of ensemble-member positions from the observed position, averaged over all ensemble members
}
\item{ens.individual.lat.err.mean}{
  a numeric vector of length \code{Nts} with the mean latitudinal error of ensemble-member positions from the observed position, averaged over all ensemble members
}
\item{ens.individual.lat.err.meanabs}{
  a numeric vector of length \code{Nts} with the mean absolute latitudinal error of ensemble-member positions from the observed position, averaged over all ensemble members
}
\item{ens.individual.lon.err.mean}{
  a numeric vector of length \code{Nts} with the mean longitudinal error of ensemble-member positions from the observed position, averaged over all ensemble members
}
\item{ens.individual.lon.err.meanabs}{
  a numeric vector of length \code{Nts} with the mean absolute longitudinal error of ensemble-member positions from the observed position, averaged over all ensemble members
}
\item{ens.spread.gc.dist}{
  a numeric vector of length \code{Nts} with the mean great-circle distance of ensemble-member positions from the ensemble-mean position, averaged over all ensemble members
}
\item{ens.spread.lat}{
  a numeric vector of length \code{Nts} with the mean latitudinal distance of ensemble-member positions from the ensemble-mean position, averaged over all ensemble members
}
\item{ens.spread.lon}{
  a numeric vector of length \code{Nts} with the mean longitudinal distance of ensemble-member positions from the ensemble-mean position, averaged over all ensemble members
}

If \code{do.speedangle}, additionally the following types are included:
\item{ens.mean.relspeed}{
  a numeric vector of length \code{Nts} with the relative integrated speed of the drift since the initial time/location (distance from the initial location) of the ensemble-mean position relative to the observed speed of the drift since the initial time/location
}
\item{ens.mean.angle}{
  a numeric vector of length \code{Nts} with the angle between the drift since the initial time/location of the ensemble-mean position relative to the observed drift since the initial time/location
}
\item{ens.individual.relspeed}{
  an \code{Nts x Nens} matrix with relative speed errors for each ensemble member (compare \code{ens.mean.relspeed})
}
\item{ens.individual.angle}{
  an \code{Nts x Nens} matrix with angle errors for each ensemble member (compare \code{ens.mean.angle})
}
\item{ens.individual.relspeed.mean}{
  a numeric vector of length \code{Nts} with the mean relative drift speed error, averaged over all ensemble members
}
\item{ens.individual.angle.mean}{
  a numeric vector of length \code{Nts} with the mean angle error, averaged over all ensemble members
}
\item{ens.individual.angle.meanabs}{
  a numeric vector of length \code{Nts} with the mean absolute angle error, averaged over all ensemble members
}
}
\note{
Based on several \code{\link{spheRlab}} and \code{\link{SIDFEx}} functions.

To recompute multi-forecast statistics for a subset of the forecasts already evaluated with \code{\link{sidfex.evaluate}} (this function), use \code{\link{sidfex.evaluate.subset}} afterwards.
}
\author{
Helge Goessling
}
\seealso{
\code{\link{sidfex.read.fcst}}, \code{\link{sidfex.read.obs}}, \code{\link{sidfex.evaluate.subset}}
}
\examples{
# Load the forecast data index
index = sidfex.load.index()

# Select a subset containing one specific forecast ensemble
subind = sidfex.fcst.search.extractFromTable(gid = "ecmwf001" , tid = "300234063991680", iy = 2019, idoy = c(1,65))

# Read the forecasts
fcst = sidfex.read.fcst(subind)

# Remap these seasonal-range forecasts to the first ten days only
fcst.remapped = sidfex.remaptime.fcst(fcst,newtime.DaysLeadTime=0:10)

# Compute forecast errors
errs = sidfex.evaluate(fcst = fcst.remapped)

# Show structure of errs
str(errs)

# Recompute multi-forecast statistics for small subset of forecasts
errs.sub = sidfex.evaluate.subset(eval.res = errs, keep = c(1,3))

# Show structure of errs.sub
str(errs.sub)
}
