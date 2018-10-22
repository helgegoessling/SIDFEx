\name{sidfex.download.obs}
\alias{sidfex.download.obs}
\title{Download SIDFEx Observations}
\usage{
sidfex.download.obs(index=NULL,TargetID=NULL,data.path=NULL)
}
\description{
Read observations corresponding to one or more SIDFEx targets.
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
