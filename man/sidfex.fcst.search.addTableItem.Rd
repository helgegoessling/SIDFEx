\name{sidfex.fcst.search.addTableItem}
\alias{sidfex.fcst.search.addTableItem}
\title{Add single row to search index file}
\usage{
  sidfex.fcst.search.addTableItem (filename, data.path=NULL, indexTable.path=NULL, is.open.rTab=FALSE, rTab.in=NULL, checkfileformat=TRUE, do.print.less=TRUE)
}
\description{
  Read one or SIDFEx forecast and add some key quantities and information to the (previously existing or not) index file "indexTable.rda". If the           specified index file does not exist, a now one is created. The data will be stored in .rda-format.
}
\arguments{
  \item{filename}{
    a character specifying the file to read. If the filename is provided with a leading filepath, set \code{data.path = ""}.
  }
  \item{data.path}{
    \code{NULL} or a character specifying the directory where to find the forecast data locally. If \code{NULL}, it is attempted to source a file named         \code{.SIDFEx} in the home directory where a character variable \code{data.path.fcst} needs to be defined.
  }
  \item{indexTable.path}{
    \code{NULL} or a character specifying the directory where to find or create the index file. If \code{NULL}, it is attempted to source a file named       \code{.SIDFEx} in the home directory where a character variable \code{data.path.indexTable.in} needs to be defined.
  }
  \item{is.open.rTab}{
    a boolen value, default \code{FALSE}. The function can and will be used by other functions to add multiple rows one by one. To save I/O operations,
    it is possible to hand over the opened table by the overarching function. In that case set \code{is.open.rTab = TRUE} AND don't forget to deliver that     table (a data frame) in \code{rTab.in} (see below).
  }
  \item{rTab.in}{
    a data frame which will be returned with one row appended if \code{is.open.rTab = TRUE}. \code{rTab.in} needs to be a data frame containing 18 columns,
    the respective column names are given below.
  }
  \item{checkfileformat}{
a logical value specifying whether or not to check if the file format complies with the SIDFEx conventions, using \code{\link{sidfex.checkfileformat}}.
}
  \item{do.print.less}{
a logical value specifying whether or not to print a statement for every dataset while adding it to the index file (to keep console output down).
}
}
\value{
  If \code{is.open.rTab = FALSE}, this function returns \code{0} (old C++ habit of author and sometimes useful). If \code{is.open.rTab = TRUE} and (thus)          \code{rTab.in} given, the function returns a copy of the input data frame with one appended row (if the row did not exist before).
}
\note{ The above-mentioned columns of the data frame should be (exactly) this vector:

  \code{indexNames = c("File", "GroupID", "MethodID", "TargetID", "InitYear", "InitDayOfYear", "EnsMemNum", "SubmitYear", "SubmitDayOfYear",
               "ProcessedYear", "ProcessedDayOfYear", "Delay", "nTimeSteps", "FirstTimeStepYear", "FirstTimeStepDayOfYear", "LastTimeStepYear",
               "LastTimeStepDayOfYear", "FcstTime")}

  It is strongly suggested not to create the index file by hand, use the given functions instead to avoid problems or inconsistencies.
}
\author{
Simon Reifenberg
}
\seealso{
\code{\link{sidfex.fcst.search.createIndexTable}}, \code{\link{sidfex.fcst.search.extractFromTable}}, \code{\link{sidfex.fcst.search.rda2csv}}
}
