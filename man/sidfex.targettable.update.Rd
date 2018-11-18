\name{sidfex.targettable.update}
\alias{sidfex.targettable.update}
\title{Update Position Information in SIDFEx Target Table}
\usage{
sidfex.targettable.update(targettable.manual=NULL, targettable.out=NULL, download.obs=TRUE, data.path=NULL)
}
\description{
Update position information in SIDFEx target table, using either a manual version or the file where recent position data is missing or an earlier version including (possibly outdated) position data.
}
\arguments{
\item{targettable.manual}{
\code{NULL} or a character specifying the path and name of the text file for which an updated version shall be generated. Can be a manual version or the file where recent position data is missing or an earlier version including (possibly outdated) position data. If \code{NULL} (default), the manual version of the target table delivered with the SIDFEx package is used. In this case, make sure that you have the latest version of the SIDFEx package. Otherwise, the manual target table used as input might be outdated, that is, new targets and/or information on discarded targets might be missing.
}
\item{targettable.out}{
\code{NULL} or a character specifying the path and name of the output text file. If \code{NULL}, no file will be written and only the updated data table will be returned by the function.
}
\item{download.obs}{
a logical value specifying whether or not to download the latest versions of the observational data first. Default is \code{TRUE}. If \code{FALSE}, be aware that the local data used to extract the latest position data might be outdated.
}
\item{data.path}{
\code{NULL} or a character specifying the local directory of the observational data. Usually, \code{NULL} (default) should be appropriate. For details, see the same argument in \code{\link{sidfex.download.obs}}.
}
}
\value{
a data frame with the updated SIDFEx target table.
}
\note{
If run with \code{targettable.manual=NULL}, make sure that you have the latest version of the SIDFEx package. Otherwise, the manual target table used as input might be outdated, that is, new targets and/or information on discarded targets might be missing.

Be aware that the data used to extract the latest position data might be outdated if you run this function with \code{download.obs=FALSE}.
}
\author{
Helge Goessling
}
\examples{
targettable = sidfex.targettable.update(targettable.out="~/SIDFEX_targettable_autoupdate.txt", download.obs=FALSE)
# the argument 'download.obs' has been set 'FALSE' to make sure that the
# example works without internet conncetion; should usually be 'TRUE'.
}
