\name{sidfex.fcst.search.compareIndexTables}
\alias{sidfex.fcst.search.compareIndexTables}
\title{Compare Two SIDFEx Forecast Data Index Tables}
\usage{
sidfex.fcst.search.compareIndexTables(index.1=NULL,index.2=NULL,return.dataframe=TRUE,verbose=FALSE)
}
\description{
Used primarily to download SIDFEx forecasts from the DKRZ cloud service incrementally with \code{\link{sidfex.download.fcst}}.
}
\arguments{
\item{index.1}{
either a character specifying the local path and filename where index 1 is located as \code{R} data file, or an already loaded data frame containing index 1.
}
\item{index.2}{
either a character specifying the local path and filename where index 2 is located as \code{R} data file, or an already loaded data frame containing index 2.
}
\item{return.dataframe}{
a logical value specifying whether the diff results (which files are only in index 1, which only in index 2, which in both and identical, which in both but different) are returned as separate index data frames. If \code{return.dataframe=FALSE}, only corresponding filenames are returned. Default is \code{return.dataframe=TRUE}.
}
  \item{verbose}{
a logical value specifying whether a summary of the results (how many files are only in index 1, how many only in index 2, how many in both and identical, how many in both but different) shall be printed. Default is \code{verbose=FALSE}.
}
}
\value{
A list with index data frames (if \code{return.dataframe=TRUE}) or file name character vectors (if \code{return.dataframe=FALSE}) which correspond to subgroups resulting from the index comparison:
\item{only.1}{
files only present in in index.1
}
\item{only.2}{
files only present in in index.2
}
\item{both.ident}{
files present in both index.1 and index.2 and identical
}
\item{both.diff.1}{
files present in both index.1 and index.2 but with different index entries; if \code{return.dataframe=TRUE}, the index entries correspond to the values in index.1
}
\item{both.diff.2}{
files present in both index.1 and index.2 but with different index entries; if \code{return.dataframe=TRUE}, the index entries correspond to the values in index.2
}
}
\note{
As only the index entries are compared, it can not be excluded that the data corresponding to \code{both.ident} differ in aspects that are not captured by the index. However, that's highly unlikely, in particular because the data submission and processing times are documented in the index.

This function is used by \code{\link{sidfex.download.fcst}} to compared the local index with the remote index to determine which files need to be downloaded.
}

\author{
Helge Goessling
}
\examples{
res = sidfex.fcst.search.compareIndexTables(index.1="PATH-TO-INDEX1/index1.rda",index.2="PATH-TO-INDEX2/index2.rda")

res = sidfex.download.fcst(test.mode=TRUE, from.scratch=FALSE)
# Compares the local index of SIDFEx forecast data with the most recent remote one and reports which new files would be downloaded, which files would be be downloaded to replace altered existing ones, and which obsolete local files would be deleted. Details are provided in the return value (here 'res').
}
\seealso{
\code{\link{sidfex.download.fcst}}, \code{\link{sidfex.fcst.search.createIndex}}, \code{\link{sidfex.fcst.search.rda2csv}}
}
