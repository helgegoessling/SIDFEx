\name{sidfex.load.index}
\alias{sidfex.load.index}
\title{Load the SIDFEx Forecast Index}
\usage{
  sidfex.load.index(indexTable.path=NULL)
}
\description{
  Simple function to load the whole SIDFEx forecast index using \code{\link{sidfex.fcst.search.extractFromTable}}.
}
\arguments{
  \item{indexTable.path}{
    \code{NULL} or a character specifying the directory where to find the index file. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{indexTable.path.in} needs to be defined.
  }
}
\value{
  A dataframe holding the whole SIDFEx forecast index.
}
%\note{
%}
\author{
Helge Goessling
}

\examples{
rTab = sidfex.load.index()
}

\seealso{
\code{\link{sidfex.fcst.search.extractFromTable}}
}
