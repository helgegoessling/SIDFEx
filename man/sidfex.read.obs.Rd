\name{sidfex.read.obs}
\alias{sidfex.read.obs}
\title{Read SIDFEx Observations}
\usage{
sidfex.read.obs(index = NULL, TargetID = NULL, data.path = NULL, NA_values = -999,
                unique_POS_time = TRUE, add_RelTimeDay_Ref = "firstobs")
}
\description{
Read observations corresponding to one or more SIDFEx targets. Optionally (default), in case of multiple data points (rows) valid for the same position time and coordinates, only the first is used, and data points with NA_values in any of the columns relevant for the trajectories (time and loaction) will be removed.
}
\arguments{
  \item{index}{
an index data frame specifying the SIDFEx forecasts for which the corresponding observations are to be read. Used only if \code{TargetID=NULL}.
}
  \item{TargetID}{
a character or character vector specifying which observations are to be read. If \code{TargetID} is specified, \code{index} is ignored.
}
  \item{data.path}{
\code{NULL} or a character specifying the directory of the observational data. If \code{NULL}, it is attempted to source a file named \code{.SIDFEx} in the home directory where a character variable \code{data.path.obs} needs to be defined.
}
  \item{NA_values}{
a numeric scalar or vector specifying one or more values to be considered \code{NA}; rows with such value in any of the columns \code{Year}, \code{POS_DOY}, \code{Lat}, or \code{Lon} will be removed. If \code{NULL}, no values will be considered \code{NA}. The default is \code{-999} as this value is used as \code{NA} at least in some buoy data.
}
  \item{unique_POS_time}{
a logical value specifying whether or not in case of multiple data points (rows) valid for the same position time and coordinates, all rows expect the first shall be removed. Default is \code{TRUE}.
}
  \item{add_RelTimeDay_Ref}{
this argument specifies whether a column with a relative time axis (days since ...) shall be added. Valid values are \code{NULL} (no axis is added), \code{'firstobs'} (default; the first data time is used as reference), and a 2-element numeric vector specifying the Year and the DayOfYear of the reference time. Note that the DayOfYear definition for SIDFEx (adopted from IABP) is that January 1st start at \code{DOY=1.000}, meaning that noon on January first is \code{DOY=1.500}. To get the same in the relative time axis for a particular year, one must set \code{add_RelTimeDay_Ref=c(YYYY,365)} where \code{YYYY} is the previous (non-leap-)year (the second element would be \code{366} in leap years).
}
}
\value{
If observations for more than one \code{TargetID} are read based on the argument \code{index}, a list with the two elements: \code{index.map} maps the rows in \code{index} to the Target IDs, and \code{res.list} is a list with one entry (a list with the entries \code{filename}, \code{TargetID}, \code{RelTime_RefYearDOY}, and \code{data}) for each Target ID.
If observations for more than one \code{TargetID} are read based on the argument \code{TargetID}, a list with one entry (a list with the entries \code{filename}, \code{TargetID}, \code{RelTime_RefYearDOY}, and \code{data}) for each Target ID.
If observations for just one \code{TargetID}, not matter if based on which argument, a list with the entries \code{filename}, \code{TargetID}, and \code{data}.
}
%\note{
%}
\author{
Helge Goessling
}
\seealso{
\code{\link{sidfex.download.obs}}, \code{\link{sidfex.read.fcst}}
}
\examples{
# Download observations corresponding to an IABP buoy with IMEI 300234063991680
res = sidfex.download.obs(TargetID="300234063991680")
# Read the data
dat = sidfex.read.obs(TargetID="300234063991680")
# Show the data header
head(dat$data)
# Make a line plot for the latitude against the relative time axis
plot(dat$data$RelTimeDay,dat$data$Lat,type="l")
# Note that for this buoy, the plot reveals spurious data points at the beginning where the buoy hadn't been deployed yet
}
