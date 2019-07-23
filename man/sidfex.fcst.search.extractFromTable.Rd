\name{sidfex.fcst.search.extractFromTable}
\alias{sidfex.fcst.search.extractFromTable}
\title{Extract specific row(s) of search index table}
\usage{
  sidfex.fcst.search.extractFromTable (index = NULL, indexTable.path = NULL, return.dataframe=TRUE,
           gid=NULL, mid=NULL, tid=NULL, iy=NULL, idoy=NULL, emn=NULL, sy=NULL, sdoy=NULL, py=NULL,
           pdoy=NULL, del=NULL, nt=NULL, fy=NULL, fdoy=NULL, ly=NULL, ldoy=NULL, per=NULL,
           fcstrange=NULL, es=NULL, EnsParentOnly=FALSE, InheritFromParent=FALSE,
           ila=NULL, ilo=NULL, fla=NULL, flo=NULL, lla=NULL, llo=NULL)
}
\description{
  This function allows one to make of subselection of the SIDFEx forecast index based on a useful set of search parameters. The resulting subindex can then be easily used to load the forecasts of interest for further processing and/or analysis. Please note the note about date inputs.
}
\arguments{
  \item{index}{
    \code{NULL} or a SIDFEx forecast index (data frame) as returned by \code{\link{sidfex.load.index}}. If \code{NULL} (default), it will be attempted to load the complete index from its default location (see argument \code{indexTable.path}).
  }
  \item{indexTable.path}{
    \code{NULL} or a character specifying the directory where to find the index file. Used only if \code{index=NULL}. If \code{indexTable.path=NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{indexTable.path.in} needs to be defined.
  }
  \item{return.dataframe}{
    a logical value specifying whether to return the whole data frame. If \code{FALSE}, only the file names will be returned. Default is \code{TRUE}.
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
    a numeric value (or a length-2 list/vector) for the submitting delay (difference of submit time and initial time of forecast). If a length-2 vector is provided, the values will be interpreted as upper and lower boundaries. If only a single number is given, the function will try to match the exact value.
  }
  \item{nt}{
    a numeric value (or a length-2 list/vector) for the number of timesteps of the forecast. If a length-2 vector is provided, the values will be interpreted as upper and lower boundaries. If only a single number is given, the function will try to match the exact value.
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
    a numeric value (or a length-2 list/vector) for the forecast period in days, i.e. the time difference between first and last timestep. If a length-2 vector is provided, the values will be interpreted as upper and lower boundaries. If only a single number is given, the function will try to match the exact value.
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
  \item{ila}{
    a numeric value (or a length-2 list/vector) for the initial latitude. If a length-2 vector is provided, the values will be interpreted as upper and lower boundaries. If only a single number is given, the function will try to match the exact value. Used only if the index contains the corresponding (optional) column 'InitLat'.
  }
  \item{ilo}{
    a numeric value (or a length-2 list/vector) for the initial longitude. If a length-2 vector is provided, the values will be interpreted as upper and lower boundaries. If only a single number is given, the function will try to match the exact value. Used only if the index contains the corresponding (optional) column 'InitLon'.
  }
  \item{fla}{
    a numeric value (or a length-2 list/vector) for the first latitude of the trajectory data table. If a length-2 vector is provided, the values will be interpreted as upper and lower boundaries. If only a single number is given, the function will try to match the exact value. Used only if the index contains the corresponding (optional) column 'FirstTimeStepLat'.
  }
  \item{flo}{
    a numeric value (or a length-2 list/vector) for the first longitude of the trajectory data table. If a length-2 vector is provided, the values will be interpreted as upper and lower boundaries. If only a single number is given, the function will try to match the exact value. Used only if the index contains the corresponding (optional) column 'FirstTimeStepLon'.
  }
  \item{lla}{
    a numeric value (or a length-2 list/vector) for the last latitude of the trajectory data table. If a length-2 vector is provided, the values will be interpreted as upper and lower boundaries. If only a single number is given, the function will try to match the exact value. Used only if the index contains the corresponding (optional) column 'LastTimeStepLat'.
  }
  \item{llo}{
    a numeric value (or a length-2 list/vector) for the last longitude of the trajectory data table. If a length-2 vector is provided, the values will be interpreted as upper and lower boundaries. If only a single number is given, the function will try to match the exact value. Used only if the index contains the corresponding (optional) column 'LastTimeStepLon'.
  }
}
\value{
  If \code{return.dataframe=FALSE}, returns a list of filenames (without extension) or NULL, if no files fit to search parameters. Otherwise, the whole corresponding data frame is returned.
}
\note{
  As all search parameters are \code{NULL} by default, currently unimportant parameters can just be omitted, see examples below.

  If you want to use this function outside the Shiny App, it is advised to look at the code of this function to understand how it works with special respect to date inputs, as strange/unwanted behavior might occur without throwing error messages. As a rule of thumb: Use either point queries here (e.g. iy=2018, idoy=14, sdoy=20, ..) or vectors of length 2 (e.g. iy=c(2017, 2018)
  and idoy=c(300, 5)). In that specific case, the function returns all datasets that lie in between 2017, DOY 300 and 2018, DOY 5.

    If you loaded the data frame from the .rda file (will appear as "rTab" in environment) in R, you can use the output list of this function like in the example below to obtain a reduced list (this selection procedure is basically equivalent to horizontal slicing of the index table).

}
\author{
Simon Reifenberg, updated by Helge Goessling
}

\examples{
# extract a reduced index data frame for all forecasts with GroupID awi003,
# initialized in 2017 and with ensemble member number 3:
subindex1 = sidfex.fcst.search.extractFromTable(gid="awi003", iy=2017, emn = 3)

# extract a reduced index data frame for all forecasts with GroupID nrl001, MethodID navyespc-subseasonal,
# TargetID 300234063991680, initialized between March 1st 2018, 12:00 UTC and February 15th 2019, 00:00UTC;
# return only the parent of each ensemble:
subindex2 = sidfex.fcst.search.extractFromTable(gid="nrl001", mid="navyespc-subseasonal", tid="300234063991680",
                                                iy=c(2018,2019), idoy=c(62.5,48.0), EnsParentOnly=TRUE)
}

\seealso{
\code{\link{sidfex.fcst.search.createIndex}}
}
