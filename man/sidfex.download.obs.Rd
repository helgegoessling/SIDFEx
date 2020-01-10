\name{sidfex.download.obs}
\alias{sidfex.download.obs}
\title{Download SIDFEx Observations}
\usage{
sidfex.download.obs(index=NULL,TargetID=NULL,data.path=NULL,baseurl=NULL,try.N=30,try.timeout=20)
}
\description{
Download observations corresponding to one or more SIDFEx target(s), or from other IABP buoys.
}
\arguments{
  \item{index}{
an index data frame specifying the SIDFEx forecasts for which the corresponding observations are to be downloaded. Used only if \code{TargetID=NULL}.
}
  \item{TargetID}{
a character or character vector specifying which observations are to be downloaded. If \code{TargetID} is specified, \code{index} is ignored.
}
  \item{data.path}{
\code{NULL} or a character specifying the directory where the observational data are to be stored. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{data.path.obs} needs to be defined.
}
  \item{baseurl}{
\code{NULL} or a character or character vector specifying the URL from where to fetch the data. If \code{NULL} (default), different default locations are used for different targets (IABP versus non-IABP).
}
  \item{try.N}{
an integer that specifies how often the download of each individual file shall be tried. Default is \code{10}.
}
  \item{try.timeout}{
a numeric value that specifies a time limit for each try of a file download in seconds. Default is \code{30}.
}
}
\value{
A character vector with the TargetIDs for which observations have been downloaded.
}
%\note{
%}
\author{
Helge Goessling
}
\seealso{
\code{\link{sidfex.read.obs}}, \code{\link{sidfex.download.fcst}}
}
\examples{
# Download observations corresponding to an IABP buoy with IMEI 300234063991680
res = sidfex.download.obs(TargetID="300234063991680")
# Read the data
dat = sidfex.read.obs(TargetID="300234063991680")
# Show the data header
head(dat$data)
# Make a line plot for the latitude against the relative time axis
plot(dat$data$RelTimeDay,dat$data$Lat,type="l")
# Note that for this buoy, the plot reveals spurious data points at the beginning where the buoy hadn't been deployed yet

# To download forecasts corresponding to an index which in turn corresponds to SIDFEx forecast data, you need to download the forecast data first using sidfex.download.fcst(). Otherwise the subsequent line will fail. Note that the second line downloads data for all buoys for which any forecast data have been downloaded (which is documented in the index).
# First load the index that refelcts the currently downloaded forecast data.
index = sidfex.load.index()
# Show for which TargetIDs forecast data exist
unique(index$TargetID)
res = sidfex.download.obs(index=index)
# (the same can be achieved with sidfex.download.obs(TargetID=unique(index$TargetID)) )
}
