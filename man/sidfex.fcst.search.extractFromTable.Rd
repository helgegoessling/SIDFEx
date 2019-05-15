\name{sidfex.fcst.search.extractFromTable}
\alias{sidfex.fcst.search.extractFromTable}
\title{Extract specific row(s) of search index table}
\usage{
  sidfex.fcst.search.extractFromTable (indexTable.path = NULL, return.dataframe=FALSE, gid=NULL, mid=NULL, tid=NULL,
           iy=NULL, idoy=NULL, emn=NULL, sy=NULL, sdoy=NULL, py=NULL, pdoy=NULL,
           del=NULL, nt=NULL, fy=NULL, fdoy=NULL, ly=NULL, ldoy=NULL, per=NULL, fcstrange=NULL,
           es=NULL, EnsParentOnly=FALSE, InheritFromParent=FALSE)
}
\description{
  This function is more or less the core of the upcoming "Shiny App" for SIDFEx. It allows a user to quickly find the filename of forecast datasets based on a useful set of search parameters. These filenames can then be easily used to load the forecast of interest for further processing and/or analysis. Please note the note about date inputs.
}
\arguments{
  \item{indexTable.path}{
    \code{NULL} or a character specifying the directory where to find or create the index file. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{indexTable.path.in} needs to be defined.
  }
  \item{return.dataframe}{
    a logical value specifying whether to return the whole data frame. If \code{FALSE} (default), only the file names will be returned.
  }
  \item{gid}{
    a character (or list/vector of characters) for the GroupID(s) of interest
  }
  \item{mid}{
    a character (or list/vector of characters) for the MethodID(s) of interest
  }
  \item{tid}{
    a character (or list/vector of characters) TargetID(s) of interest
  }
  \item{iy}{
    a numeric value (or list/vector of such) for the initial year
  }
  \item{idoy}{
    a numeric value (or list/vector of such) for the initial day of year
  }
  \item{emn}{
    an integer (or list/vector of inters) for the ensemble member number of interest
  }
  \item{sy}{
    a numeric value (or list/vector of such) for the year that the respective file was submitted in
  }
  \item{sdoy}{
    a numeric value (or list/vector of such) for the day of year that the respective file was submitted
  }
  \item{py}{
    a numeric value (or list/vector of such) for the year that the respective file was processed
  }
  \item{pdoy}{
    a numeric value (or list/vector of such) for the day of year that the respective file was processed
  }
  \item{del}{
    a numeric value (or list/vector of such) for the submitting delay (difference of submit time and initial time of forecast)
  }
  \item{nt}{
    a numeric value (or list/vector of such) for the number of timesteps of the forecast
  }
  \item{fy}{
    a numeric value (or list/vector of such) for the year of the first timestep
  }
  \item{fdoy}{
    a numeric value (or list/vector of such) for the day of year of the first timestep
  }
  \item{ly}{
    a numeric value (or list/vector of such) for the year of the last timestep
  }
  \item{ldoy}{
    a numeric value (or list/vector of such) for the day of year of the last timestep
  }
  \item{per}{
    a numeric value (or list/vector of such) for the forecast time in days, i.e. the time difference between first and last timestep
  }
  \item{fcstrange}{
    a vector of length 2, containing two date objects spanning a range. If a forecast has at least one timestep in that range, it will be selected.
  }
  \item{es}{
    an integer (or list/vector of inters) for the ensemble size
  }
  \item{EnsParentOnly}{
    a logical value specifying whether only ensemble parents (typically ensemble member 1) shall be returned. Default is \code{FALSE}
  }
  \item{InheritFromParent}{
    a logical value specifying whether ensemble members inherit all properties except \code{File} and \code{EnsMemNum} from their parents (typically ensemble member 1) before the other criteria are applied. Default is \code{FALSE}
  }
}
\value{
  If \code{return.dataframe=FALSE}, returns a list of filenames (without extension) or NULL, if no files fit to search parameters. Otherwise, the whole corresponding data frame is returned.
}
\note{
  As all search parameters are \code{NULL} by default, currently unimportant parameters can just be omitted, see example below.

  If you want to use this function outside the Shiny App, it is advised to look at the code of this function to understand how it works with special respect to date inputs, as strange/unwanted behavior might occur without throwing error messages. As a rule of thumb: Use either point queries here (e.g. iy=2018, idoy=14, sdoy=20, ..) or vectors of length 2 (e.g. iy=c(2017, 2018)
  and idoy=c(300, 5)). In that specific case, the function returns all datasets that lie in between 2017, DOY 300 and 2018, DOY 5.

    If you loaded the data frame from the .rda file (will appear as "rTab" in environment) in R, you can use the output list of this function like in the example below to obtain a reduced list (this selection procedure is basically equivalent to horizontal slicing of the index table).

}
\author{
Simon Reifenberg, updated by Helge Goessling
}

\examples{
# get file names of all avaiable data from the index table
rTab_filenames = sidfex.fcst.search.extractFromTable()

# get whole index table as a data frame
rTab = sidfex.fcst.search.extractFromTable(return.dataframe=TRUE)

# extract the filenames of all forecasts with GroupID awi003, initialized in 2017 and ensemble member number 1
rTab_selected_filenames = sidfex.fcst.search.extractFromTable(gid="awi003", iy=2017, emn = 1)

# extract a reduced data frame for all forecasts with GroupID awi003, initialized in 2017 and ensemble member number 1
rTab_selected = sidfex.fcst.search.extractFromTable(return.dataframe=TRUE, gid="awi003", iy=2017, emn = 1)
}

\seealso{
\code{\link{sidfex.fcst.search.createIndexTable}}, \code{\link{sidfex.fcst.search.addTableItem}}, \code{\link{sidfex.fcst.search.rda2csv}}
}
