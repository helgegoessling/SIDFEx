\name{sidfex.fcst.search.createIndexTable}
\alias{sidfex.fcst.search.createIndexTable}
\title{Create search index table for whole forecast directory}
\usage{
  sidfex.fcst.search.createIndexTable (indexTable.path = NULL, data.path = NULL, do.fromScratch = FALSE, do.saveAddCSV = TRUE, checkfileformat = TRUE, do.print.less = TRUE)
}
\description{
  Loop through the downloaded SIDFEx forecasts and add some predefined key quantities and information to the (previously existing or not) index file "indexTable.rda". If the specified index file does not exist, a new one is created. The data will be stored in .rda-format.
}
\arguments{
  \item{indexTable.path}{
    \code{NULL} or a character specifying the directory where to find or create the index file. If \code{NULL}, it is attempted to source a file named        \code{.SIDFEx} in the home directory where a character variable \code{data.path.indexTable.in} needs to be defined.
    }

  \item{data.path}{
    \code{NULL} or a character specifying the directory where to find the forecast data locally. If \code{NULL}, it is attempted to source a file named         \code{.SIDFEx} in the home directory where a character variable \code{data.path.fcst} needs to be defined.
  }


  \item{do.fromScratch}{
    a boolen value, default \code{FALSE}. If set to \code{FALSE}, an existing index file will be extended by all files that have not been added before. If     set to \code{TRUE}, the existing index file will be removed and a new one is created from scratch.
  }
  \item{do.saveAddCSV}{
    a boolean value, default \code{TRUE}. If so, a CSV version of the data frame will be stored in addition to the .rda-file in the same directory.
  }
  \item{checkfileformat}{
a logical value specifying whether or not to check if the file format complies with the SIDFEx conventions, using \code{\link{sidfex.checkfileformat}}.
}
  \item{do.print.less}{
a logical value specifying whether or not to print a statement for every dataset while adding it to the index file (to keep console output down). Will be also passed on to \code{sidfex.fcst.search.addTableItem}. 
}
}
\value{
  This function returns \code{1} if it ran successfully.
}
\note{
  Use the option \code{do.fromScratch} carefully, as (for now) the existing file is removed before the new one is created. If there's some time for that    task in the future, a backup copy of the old file will be stored until the new one is written on disk - implementation pending.

  The optionally produced CSV file is mainly generated for users not familiar with R. As it is only a copy of the .rda file and won't be altered or         affected by changes in the .rda file in general, it is recommended to work with the .rda file whenever possible.
}
\author{
Simon Reifenberg
}
\seealso{
\code{\link{sidfex.fcst.search.addTableItem}}, \code{\link{sidfex.fcst.search.extractFromTable}}, \code{\link{sidfex.fcst.search.rda2csv}}
}
