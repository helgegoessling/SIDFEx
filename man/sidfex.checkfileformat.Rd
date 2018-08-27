\name{sidfex.checkfileformat}
\alias{sidfex.checkfileformat}
\title{Check SIDFEx Forecast Format}
\usage{
sidfex.checkfileformat(filepathnames)
}
\description{
Check if one or more files comply with the SIDFEx conventions.
}
\arguments{
  \item{filepathnames}{
a character or character vector specifying the file(s) path(s) and name(s) to check. If a directory is provided, all files in that directory are checked.
}
}
\value{
In case of a single file, either the character 'No file format violations found.' or one or more characters expressing what type of violations are found.
}
\details{
This function can be used to check forecast files before submitting them to SIDFEx (see details on submission procedure at \link{http://www.polarprediction.net/yopp-activities/sidfex/}). The same function is used automatically by the SIDFEx system to check any incoming files.
}
\author{
Helge Goessling
}
\seealso{
\code{\link{sidfex.download.fcst}}, \code{\link{sidfex.read.fcst}}, \code{\link{sidfex.write.fcst}}
}
