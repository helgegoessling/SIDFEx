\name{sidfex.read.obs}
\alias{sidfex.read.obs}
\title{Read SIDFEx Observations}
\usage{
sidfex.read.obs(index=NULL,TargetID=NULL,data.path=NULL)
}
\description{
Read observations corresponding to one or more SIDFEx targets.
}
\arguments{
  \item{index}{
an index data frame specifying the SIDFEx forecasts for which the corresponding observations are to be read. Used only if \code{TargetID=NULL}.
}
  \item{TargetID}{
a character or character vector specifying which observations are to be read. If \code{TargetID} is specified, \code{index} is ignored.
}
  \item{data.path}{
\code{NULL} or a character specifying the directory of the observational data. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{data.path.obs} needs to be defined.
}
}
\value{
If observations for more than one \code{TargetID} are read based on the argument \code{index}, a list with the two elements: \code{index.map} maps the rows in \code{index} to the Target IDs, and \code{res.list} is a list with one entry (a list with the entries \code{filename}, \code{TargetID}, and \code{data}) for each Target ID.
If observations for more than one \code{TargetID} are read based on the argument \code{TargetID}, a list with one entry (a list with the entries \code{filename}, \code{TargetID}, and \code{data}) for each Target ID.
If observations for just one \code{TargetID}, not matter if based on which argument, a list with the entries \code{filename}, \code{TargetID}, and \code{data}.
}
%\note{
%}
\author{
Helge Goessling
}
\seealso{
\code{\link{sidfex.download.obs}}, \code{\link{sidfex.read.fcst}}
}
