\name{sidfex.fcst.search.createIndex}
\alias{sidfex.fcst.search.createIndex}
\title{Create or update SIDFEx forecast index}
\usage{
  sidfex.fcst.search.createIndex(indexTable.path = NULL, data.path = NULL, do.fromScratch = FALSE, do.checktime = TRUE, do.saveAddCSV = TRUE, do.print.less = TRUE, checkfileformat = TRUE, do.manipulate = TRUE, add.LatLon=TRUE, not.save.but.return.index=FALSE)
}
\description{
  Loop through the downloaded SIDFEx forecasts and add some predefined key quantities and information to the (previously existing or not) index file "indexTable.rda". If the specified index file does not exist, a new one is created. The data will be stored in .rda-format.
}
\arguments{
  \item{indexTable.path}{
    \code{NULL} or a character specifying the directory where to find or create the index file. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{data.path.indexTable.in} needs to be defined.
    }

  \item{data.path}{
    \code{NULL} or a character specifying the directory where to find the forecast data locally. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{data.path.fcst} needs to be defined.
  }


  \item{do.fromScratch}{
    a boolen value, default \code{FALSE}. If set to \code{FALSE}, an existing index file will be extended by all files that have not been added before (and, optionally, by all files with changed submission time). If set to \code{TRUE}, the existing index file will be removed and a new one is created from scratch.
  }
  \item{do.checktime}{
    a boolen value, default \code{TRUE}. Used only if \code{do.fromScratch} is \code{FALSE}. If \code{do.checktime} is \code{TRUE}, the first lines of the forecast file headers are read to check whether the submission time has changed; only if that's the case the corresponding row in the index table is updated. If \code{do.checktime} (and \code{do.fromScratch} is \code{FALSE}), previously existing entries will NOT be updated, which saves some time.
  }
  \item{do.saveAddCSV}{
    a boolean value, default \code{TRUE}. If so, a CSV version of the data frame will be stored in addition to the .rda-file in the same directory.
  }
  \item{do.print.less}{
a logical value specifying whether or not to print a statement for every dataset while adding it to the index file (to keep console output down). Will be also passed on to \code{sidfex.fcst.search.addTableItem}.
}
  \item{checkfileformat}{
a logical value specifying whether or not to check if the file format complies with the SIDFEx conventions, using \code{\link{sidfex.checkfileformat}}.
}
  \item{do.manipulate}{
a logical value specifying whether or not to manipulate the automatically derived ensemble information, using \code{sidfex.fcst.search.manipulateIndex} (an undocumented function); required for lagged-initial-time ensembles.
}
  \item{add.LatLon}{
a logical value specifying whether or not to add latitude and longitude information (initial, first and last timesteps) to the index. Default is \code{TRUE}.
}
  \item{not.save.but.return.index}{
a logical value specifying whether to return the index instead of saving as an R data file (and possibly also as CSV). Default is \code{FALSE}.
}
}
\value{
  This function returns \code{1} if it ran successfully, except if \code{not.save.but.return.index=FALSE} in which case the new index is returned.
}
\note{
  The optionally produced CSV file is mainly generated for users not familiar with R. As it is only a copy of the .rda file and won't be altered or affected by changes in the .rda file in general, it is recommended to work with the .rda file whenever possible.
}
\author{
Simon Reifenberg, update by Helge Goessling
}
\seealso{
\code{\link{sidfex.fcst.search.extractFromTable}}, \code{\link{sidfex.fcst.search.rda2csv}}
}
