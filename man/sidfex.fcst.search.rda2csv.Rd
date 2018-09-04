\name{sidfex.fcst.search.rda2csv}
\alias{sidfex.fcst.search.rda2csv}
\title{Save R data frame of search index to CSV file}
\usage{
  sidfex.fcst.search.rda2csv (rTab.in, headerlines, indexTable.path=NULL)
}
\description{
  This is a small function that will mainly be called by \code{\link{sidfex.fcst.search.createIndexTable}} but can also be used as a standalone function    (even if I can't see any big advantages other than testing frameworks) to save the data frame of the index file in R to a CSV file.
  
}
\arguments{
  \item{rTab.in}{
    a data frame to be written in the CSV file
  }
  \item{headerlines}{
    some lines of characters that will appear in the CSV file above the data
  }
  \item{indexTable.path}{
    \code{NULL} or a character specifying the directory where to find or create the index file. If \code{NULL}, it is attempted to source a file named       \code{.SIDFEx} in the home directory where a character variable \code{data.path.indexTable.in} needs to be defined.
  }

}

\note{ 
  To be honest, your life will most probably not change essentially after the use of this function. 
}
\author{
Simon Reifenberg
}
\seealso{
\code{\link{sidfex.fcst.search.createIndexTable}}, \code{\link{sidfex.fcst.search.extractFromTable}}, \code{\link{sidfex.fcst.search.addTableItem}}
}
