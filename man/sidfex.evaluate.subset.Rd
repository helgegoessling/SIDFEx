\name{sidfex.evaluate.subset}
\alias{sidfex.evaluate.subset}
\title{Recompute Multi-Forecast Statistics for a Subset of Evaluated SIDFEx Forecasts}
\usage{
sidfex.evaluate.subset(eval.res = NULL, keep = NULL, do.multifcst.stats = TRUE, multifcst.stats.na.rm = TRUE)
}
\description{
Recompute multi-forecast statistics for a subset of already evaluated SIDFEx forecasts (various types of errors relative to observations and, if applicable, ensemble spread).
}
\arguments{
  \item{eval.res}{
a list with SIDFEx evaluation results as returned by \code{\link{sidfex.evaluate}}.
}
  \item{keep}{
a numeric vector with indices (length >= 2 and <= \code{length(eval.res$res.list)}) or a logical vector (length = \code{length(eval.res$res.list)}) specifying the subset of forecasts for which to recompute the multi-forecast statistics.
}
  \item{do.multifcst.stats}{
a logical value specifying whether statistics across all elements of \code{fcst$res.list} shall be computed. Will result in an error if the elements of \code{eval.res$res.list} do not have identical numbers of time steps. If \code{FALSE}, the subset of individual evaluation results is just extracted. See documentation of \code{sidfex.evaluate} for details.
}
  \item{multifcst.stats.na.rm}{
a logical value specifying whether to remove NAs before statistics are computed across forecast elements.
}
}
\value{
a list with elements \code{ens.merge} (inherited from \code{eval.res}), \code{evaluate.arguments} (to document the logical values specified for \code{do.speedangle} (inherited from \code{eval.res}) and \code{do.multifcst.stats}), \code{res.list}, and optionally \code{multifcst.stats} if \code{do.multifcst.stats=TRUE} and \code{length(fcst$res.list)>1}. The element \code{res.list} holds one element for each corresponding element in \code{fcst$res.list}, with various types of forecast errors listed below. If \code{fcst} is a single forecast element, only a single list with the types of forecast errors is returned.

See \code{\link{sidfex.evaluate}} for details, including all error types.
}
\note{
Based on several \code{\link{spheRlab}} and \code{\link{SIDFEx}} functions.
}
\author{
Helge Goessling
}
\seealso{
\code{\link{sidfex.evaluate}}, \code{\link{sidfex.read.fcst}}, \code{\link{sidfex.read.obs}}
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
