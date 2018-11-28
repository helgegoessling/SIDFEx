\name{sidfex.read.fcst}
\alias{sidfex.read.fcst}
\title{Read SIDFEx Forecasts}
\usage{
sidfex.read.fcst(files=NULL,data.path=NULL,GroupID=NULL,MethodID=NULL,TargetID=NULL,InitYear=NULL,InitDayOfYear=NULL,EnsMemNum=NULL,ens.merge=TRUE,checkfileformat=TRUE)
}
\description{
Read one or more SIDFEx forecasts.
}
\arguments{
  \item{files}{
a character, character vector, or index data frame specifying the SIDFEx forecast file(s) to be read. If a directory is provided, all files in that directory are read.
}
  \item{data.path}{
\code{NULL} or a character specifying the directory of the forecast data. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{data.path.fcst} needs to be defined.
}
  \item{ens.merge}{
a logical value specifying whether or not to merge ensemble members from individual lists (corresponding to individual files) into merged lists with shared meta data. NOT YET IMPLEMENTED!
}
  \item{checkfileformat}{
a logical value specifying whether or not to check if the file format complies with the SIDFEx conventions, using \code{\link{sidfex.checkfileformat}}.
}
}
\value{
If \code{ens.merge = FALSE}, a list with the two elements \code{ens.merge} (set \code{FALSE}) and \code{res.list}. The latter contains one list for each file that has been read, including separate lists for each ensemble member in case the files contain such.
If \code{ens.merge = TRUE} (NOT YET IMPLEMENTED!), a list with the two elements \code{ens.merge} (set \code{TRUE}) and \code{res.list}. The latter contains one list for each ensemble that has been read. That is, if the files contain ensemble members, those are merged into lists with shared meta data.
}
\note{
The 'Value' section in this documentation should be extended to explain what the elements of \code{res.list} are (with and without \code{ens.merge})
}
\author{
Helge Goessling
}
\seealso{
\code{\link{sidfex.download.fcst}}, \code{\link{sidfex.write.fcst}}, \code{\link{sidfex.fcst.ensmerge}}, \code{\link{sidfex.checkfileformat}}
}
\examples{
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
}
