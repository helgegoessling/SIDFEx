\name{sidfex.read.fcst}
\alias{sidfex.read.fcst}
\title{Read SIDFEx Forecasts}
\usage{
sidfex.read.fcst(files=NULL,data.path=NULL,GroupID=NULL,MethodID=NULL,TargetID=NULL,InitYear=NULL,InitDayOfYear=NULL,EnsMemNum=NULL,ens.merge=TRUE,checkfileformat=TRUE,verbose=TRUE,speed.fix=TRUE,speed.fix.max=80,timedupli.fix=TRUE,nrl001_xxx.ini.fix=TRUE)
}
\description{
Read SIDFEx forecast data and (optionally) merge ensembles.
}
\arguments{
  \item{files}{
a character, character vector, or index data frame specifying the SIDFEx forecast file(s) to be read. If a directory is provided, all files in that directory are read. Note that \code{ens.merge} works only if \code{files} is an index data frame.
}
  \item{data.path}{
\code{NULL} or a character specifying the directory of the forecast data. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{data.path.fcst} needs to be defined.
}
  \item{ens.merge}{
a logical value specifying whether or not to merge ensemble members from individual lists (corresponding to individual files) into merged lists with shared meta data. If \code{TRUE}, also ensemble means are computed and, if required, ensembles members are temporally regridded to the time axis of the parent ensemble member (required e.g. to handle lagged-initial-date ensembles).
}
  \item{checkfileformat}{
a logical value specifying whether or not to check if the file format complies with the SIDFEx conventions, using \code{\link{sidfex.checkfileformat}}.
}
  \item{verbose}{
a logical value specifying whether to produce warnings in \code{sl.trajectory.remaptime} (only used when lagged-initial-time ensembles are merged), in particular when new time axes reach outside original time axes. Default is \code{TRUE}.
}
  \item{speed.fix}{
a logical value specifying whether to limit the drift speed to certain bounds. Trajectory segments with unrealistically high speeds (as set by \code{speed.fix.max}) are replaced by linearly interpolated values between adjacent data points based on \code{sl.trajectory.remaptime()}. Default is \code{TRUE}.
}
  \item{speed.fix.max}{
a numerical value specifying the speed threshold used to 'fix' trajectory segments with unrealistically high speeds when \code{speed.fix=TRUE}. Default is \code{80}.
}
  \item{timedupli.fix}{
a logical value specifying whether to remove trajectory data entries with identical times. The respective first values are kept. Note that the consistency or inconsistency of the associated coordinates are not considered. Default is \code{TRUE}.
}
  \item{nrl001_xxx.ini.fix}{
a logical value specifying whether to fix the InitYear and InitDayOfYear of some of the nrl001 MethodIDs where the true initial times correspond to the first time entry in the data table rather than the values in the file name and header. Default is \code{TRUE}. As sidfex.fcst.search.createIndex() uses sidfex.fcst.read(), this fix is also reflected in the SIDFEx index.
}
}
\value{
\item{ens.merge}{
a logical value indicating whether ensembles have been merged or not (see corresponding argument).
}
\item{res.list}{
a list with one element for each individual forecast or forecast ensemble that has been read. The following subelements are included:}
  \item{...$fl}{
    a character with the corresponding file path and name. A vector if \code{ens.merge=TRUE} and the ensemble size is larger than 1.
  }
  \item{...$checkfileformat.result}{
    a character, provided only if \code{checkfileformat=TRUE}), with the output of \code{sidfex.checkfileformat}. A vector if \code{ens.merge=TRUE} and the ensemble size is larger than 1.
  }
  \item{...$SubmitYear}{
    Year of submission of the forecast to the DKRZ cloud. Inherited from the ensemble parent if \code{ens.merge=TRUE} and the ensemble size is larger than 1.
  }
  \item{...$SubmitDayOfYear}{
    DayOfYear of submission of the forecast to the DKRZ cloud. Inherited from the ensemble parent if \code{ens.merge=TRUE} and the ensemble size is larger than 1.
  }
  \item{...$ProcessedYear}{
    Year of processing of the forecast on the DKRZ cloud. Inherited from the ensemble parent if \code{ens.merge=TRUE} and the ensemble size is larger than 1.
  }
  \item{...$ProcessedDayOfYear}{
    DayOfYear of processing of the forecast on the DKRZ cloud. Inherited from the ensemble parent if \code{ens.merge=TRUE} and the ensemble size is larger than 1.
  }
  \item{...$GroupID}{
    the Group ID.
  }
  \item{...$MethodID}{
    the Method ID.
  }
  \item{...$TargetID}{
    the Target ID.
  }
  \item{...$InitYear}{
    Year of the initialisation time. Inherited from the ensemble parent if \code{ens.merge=TRUE} and the ensemble size is larger than 1.
  }
  \item{...$InitDayOfYear}{
    DayOfYear of the initialisation time. Inherited from the ensemble parent if \code{ens.merge=TRUE} and the ensemble size is larger than 1.
  }
  \item{...$InitLat}{
    initial latitude. Inherited from the ensemble parent if \code{ens.merge=TRUE} and the ensemble size is larger than 1. See also \code{MergedInitLat}.
  }
  \item{...$InitLon}{
    initial longitude. Inherited from the ensemble parent if \code{ens.merge=TRUE} and the ensemble size is larger than 1. See also \code{MergedInitLon}.
  }
  \item{...$EnsMemNum}{
    the ensemble member number. A vector if \code{ens.merge=TRUE} and the ensemble size is larger than 1.
  }
  \item{...$Ntimesteps}{
    number of timesteps. Inherited from the ensemble parent if \code{ens.merge=TRUE} and the ensemble size is larger than 1.
  }
  \item{...$FirstYear}{
    Year of the first data point in \code{data}. May or may not differ from \code{InitYear}.
  }
  \item{...$FirstDayOfYear}{
    DayOfYear of the first data point in \code{data}. May or may not differ from \code{InitDayOfYear}.
  }
  \item{...$FirstLat}{
    latitude of the first data point in \code{data}. May or may not differ from \code{InitLat}. If \code{ens.merge=TRUE} and the ensemble size is larger than 1 and initial locations differ, this is the ensemble mean whereas \code{InitLat} is inherited from the parent.
  }
  \item{...$FirstLon}{
    lonitude of the first data point in \code{data}. May or may not differ from \code{InitLon}. If \code{ens.merge=TRUE} and the ensemble size is larger than 1 and initial locations differ, this is the ensemble mean whereas \code{InitLon} is inherited from the parent.
  }
  \item{...$LastYear}{
    Year of the last data point in \code{data}.
  }
  \item{...$LastDayOfYear}{
    DayOfYear of the last data point in \code{data}.
  }
  \item{...$LastLat}{
    latitude of the last data point in \code{data}. If \code{ens.merge=TRUE} and the ensemble size is larger than 1, this is the ensemble mean.
  }
  \item{...$LastLon}{
    longitude of the last data point in \code{data}. If \code{ens.merge=TRUE} and the ensemble size is larger than 1, this is the ensemble mean.
  }
  \item{...$DaysForecastLength}{
    the time difference between the last and initial time of the forecast in days.
  }
  \item{...$data}{
    a data frame holding the actual trajectory data with one point in each row and the following columns: \code{Year}, \code{DayOfYear}, \code{Lat}, \code{Lon}, and \code{DaysLeadTime}. The latter is the time in days relative to the initial time. If \code{ens.merge=TRUE} and the ensemble size is larger than 1, columns 3 and 4 correspond to the ensemble-mean (barycenter) locations and additional pairs of columns with names \code{LatX} and \code{LonX} are added for each ensemble member, where \code{X} is the ensemble member number.
  }
  \item{...$MergedInitLat}{
    a vector, provided only if \code{ens.merge=TRUE} and the ensemble size is larger than 1, with the first value providing the initial ensemble-mean (barycenter) latitude and the subsequent values providing the initial latitudes of the individual forecasts.
  }
  \item{...$MergedInitLon}{
    a vector, provided only if \code{ens.merge=TRUE} and the ensemble size is larger than 1, with the first value providing the initial ensemble-mean (barycenter) longitude and the subsequent values providing the initial longitudes of the individual forecasts.
  }
\item{index}{
the index data frame corresponding to the data read.
}
}
%\note{
%The 'Value' section in this documentation should be extended to explain what the elements of \code{res.list} are (with and without \code{ens.merge})
%}
\author{
Helge Goessling
}
\seealso{
\code{\link{sidfex.download.fcst}}, \code{\link{sidfex.write.fcst}}, \code{\link{sidfex.checkfileformat}}
}
\examples{
# Load the forecast data index
index = sidfex.load.index()
# Show the first rows of the index to see which variables can be used to subselect the index
head(index)

# Make an index subset for one specific forecast ensemble
subind = sidfex.fcst.search.extractFromTable(gid = "awi001" , tid = "300234063991680", iy = 2018, idoy = 30, return.dataframe = TRUE, ens.merge = FALSE)
# Read the forecasts
fcst = sidfex.read.fcst(index=subind)
# Now plot latitude against forecast lead time for the whole ensemble
plot(fcst$res.list[[1]]$data$DaysLeadTime,fcst$res.list[[1]]$data$Lat,type="l",ylim=c(74,80),xlim=c(0,600))
for (i in 2:length(fcst$res.list)) {lines(fcst$res.list[[i]]$data$DaysLeadTime,fcst$res.list[[i]]$data$Lat)}
# If you are surprised that the ensemble members have different length, that's because the climatological method applied to generate these forecasts discards trajectories when the sea-ice concentration in the corresponding years dropped below some threshold.
}
