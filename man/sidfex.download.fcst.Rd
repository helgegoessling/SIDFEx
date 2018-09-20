\name{sidfex.download.fcst}
\alias{sidfex.download.fcst}
\title{Download SIDFEx Forecasts}
\usage{
sidfex.download.fcst(test.mode=TRUE, from.scratch=FALSE, data.path=NULL, indexTable.path=NULL, baseurl="https://swift.dkrz.de/v1/dkrz_0262ea1f00e34439850f3f1d71817205/")
}
\description{
Download SIDFEx forecasts from the DKRZ cloud service, either incrementally or from scratch. A test mode allows to diagnose differences between the local and the remote data index first. Unfortunately, the download is quite slow if many files are downloaded as each file is loaded separately which takes about 0.2 seconds each.
}
\arguments{
\item{test.mode}{
a logical value specifying whether to diagnose only, which new data would be downloaded, which obsolete data would be deleted, and which existing data would be replaced because they have changed between the local copy and the remote reference. If \code{test.mode=FALSE}, these modifications are executed. Default is \code{test.mode=TRUE}.
}
\item{from.scratch}{
a logical value specifying whether or not to update the local data from scratch. If \code{from.scratch=FALSE} (default), which is much faster if most of the data has already been downloaded previously, changes are made incrementally to achieve the same state locally as remotely. See the note below regarding possible complications.
}
  \item{data.path}{
\code{NULL} or a character specifying the directory where to put the forecast data locally. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{data.path.fcst} needs to be defined.
}
  \item{indexTable.path}{
    \code{NULL} or a character specifying the directory where to find the local index file and where to move the updated one. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{indexTable.path.in} needs to be defined.
  }
  \item{baseurl}{
a character specifying the URL where the SIDFEx data and index are located remotely. Should not need to be changed from the default value.
}
}
\value{
If \code{from.scratch=TRUE}, a list with just one element named 'index.download' which is the index of all data that would be or have been downloaded.
If \code{from.scratch=FALSE}, a list with two elements, one named 'index.download' which is the index of all data that would be or have been downloaded, and one named 'index.diff' which provides details about the index comparison as returned from \code{\link{sidfex.fcst.search.compareIndexTables}}.
}
\note{
With \code{from.scratch=FALSE} it can not be excluded that the local data differs from the remote data in aspects that are not captured by the index. However, that's highly unlikely, in particular because the data submission and processing times are documented in the index.

Unfortunately, the download is quite slow if many files are downloaded as each file is loaded separately which takes about 0.2 seconds each.
}
\author{
Helge Goessling
}
\examples{
res = sidfex.download.fcst(test.mode=FALSE, from.scratch=TRUE)
# Downloads all SIDFEx forecast data to the directory specified in \code{~/.SIDFEx}. If present, any previous data is removed first.

res = sidfex.download.fcst(test.mode=TRUE, from.scratch=FALSE)
# Compares the local index of SIDFEx forecast data with the most recent remote one and reports which new files would be downloaded, which files would be be downloaded to replace altered existing ones, and which obsolete local files would be deleted. Details are provided in the return value (here 'res').
}
