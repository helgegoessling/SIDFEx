\name{sidfex.download.fcst}
\alias{sidfex.download.fcst}
\title{Download SIDFEx Forecasts}
\usage{
sidfex.download.fcst(data.path = NULL, baseurl = "https://swiftbrowser.dkrz.de/public/dkrz_0262ea1f00e34439850f3f1d71817205/SIDFEx_processed/")
}
\description{
Download all SIDFEx forecasts from the DKRZ cloud service.
}
\arguments{
  \item{data.path}{
\code{NULL} or a character specifying the directory where to put the forecast data locally. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{data.path.fcst} needs to be defined.
}
  \item{baseurl}{
a character specifying the URL where the SIDFEx data are located remotely. Should not need to be changed from the default value.
}
\note{
It might be convenient to add an argument \code{GroupID} so one does not always need to download all data.
}
}
\examples{
sidfex.download.fcst()
# Downloads all SIDFEx forecast data to the directory specified in \code{~/.SIDFEx}.
}
