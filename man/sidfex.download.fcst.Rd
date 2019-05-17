\name{sidfex.download.fcst}
\alias{sidfex.download.fcst}
\title{Download SIDFEx Forecasts}
\usage{
sidfex.download.fcst(comparison.mode=FALSE, from.scratch=TRUE, data.path=NULL, indexTable.path=NULL, baseurl="https://swift.dkrz.de/v1/dkrz_0262ea1f00e34439850f3f1d71817205/")
}
\description{
Download SIDFEx forecasts from the DKRZ cloud service, either from scratch through a tar file or incrementally through individual files. A comparison mode allows to diagnose differences between the local and the remote data index first. The incremental  download of individual files is quite slow, so this option is not recommended if the last data download is long ago and thus many files need to be downloaded.
}
\arguments{
\item{comparison.mode}{
a logical value specifying whether to diagnose only which new data would be downloaded, which obsolete data would be deleted, and which existing data would be replaced because they have changed between the local copy and the remote reference. If \code{comparison.mode=FALSE} (default), these modifications are executed.
}
\item{from.scratch}{
a logical value specifying whether or not to update the local data from scratch. If \code{from.scratch=FALSE}, changes are made incrementally to achieve the same state locally as remotely; see the note below regarding possible complications in this case. However, \code{from.scratch=TRUE} is much faster if many files need to be downloaded because then a single tar.gz-file is downloaded.
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
  \item{verbose}{
a logical value specifying whether print output of the 'tar' command will be displayed or not.
}
}
\value{
If \code{from.scratch=TRUE}, a list with just one element named 'index.download' which is the index of all data that would be or have been downloaded.
If \code{from.scratch=FALSE}, a list with two elements, one named 'index.download' which is the index of all data that would be or have been downloaded, and one named 'index.diff' which provides details about the index comparison as returned from \code{\link{sidfex.fcst.search.compareIndexTables}}.
}
\note{
With \code{from.scratch=FALSE} it can not be excluded that the local data differs from the remote data in aspects that are not captured by the index. However, that's highly unlikely, in particular because the data submission and processing times are documented in the index.

The download of individual files is quite slow if many files are downloaded, so \code{from.scratch=TRUE} is more efficient in most cases.
}
\author{
Helge Goessling
}
\examples{
# You can specify 'data.path' explicitly, but that's not recommended. Rather, make sure you have a file '~/.SIDFEx' with for example the following three rows (without the comment signs of course):
# data.path.fcst = "~/SIDFEx/data/forecasts"
# data.path.obs = "~/SIDFEx/data/observations"
# indexTable.path.in = "~/SIDFEx/index"
# In this example, the directory 'SIDFEx' in your home would need to exist

# Download all SIDFEx forecasts to be sure everything is up to date
res = sidfex.download.fcst()
# Load the forecast data index
index = sidfex.load.index()
# Show the first rows of the index to see which variables can be used to subselect the index
head(index)

# Make an index subset for one specific forecast ensemble
subind = sidfex.fcst.search.extractFromTable(gid = "awi001" , tid = "300234063991680", iy = 2018, idoy = 30, return.dataframe = TRUE)
# Read the forecasts
fcst = sidfex.read.fcst(index=subind)
# Now plot latitude against forecast lead time for the whole ensemble
plot(fcst$res.list[[1]]$data$DaysLeadTime,fcst$res.list[[1]]$data$Lat,type="l",ylim=c(74,80),xlim=c(0,600))
for (i in 2:length(fcst$res.list)) {lines(fcst$res.list[[i]]$data$DaysLeadTime,fcst$res.list[[i]]$data$Lat)}
# If you are surprised that the ensemble members have different length, that's because the climatological method applied to generate these forecasts discards trajectories when the sea-ice concentration in the corresponding years dropped below some threshold.

# Another example:
res = sidfex.download.fcst(comparison.mode=TRUE, from.scratch=FALSE)
# Compares the local index of SIDFEx forecast data with the most recent remote one and reports which new files exist. Details are provided in the return value (here 'res').
}
