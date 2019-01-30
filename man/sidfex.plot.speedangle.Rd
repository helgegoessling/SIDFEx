\name{sidfex.plot.speedangle}
\alias{sidfex.plot.speedangle}
\title{Plot Speed-vs-Angle Errors for SIDFEx Data}
\usage{
sidfex.plot.speedangle(index=NULL,read.fcst.res=NULL,read.obs.res=NULL,remap.res=NULL,col.by="DaysLeadTime",colbar=sl.colbar.redgreyblue_256,colbar.breaks=NULL,colbar.breaks.log=FALSE,points.type="p",device="pdf",file=paste0("~/sidfex.plot.speedangle.",device),width=NULL,do.init.device=TRUE,do.close.device=TRUE,labels=TRUE,labels.cex=1,bg.lines.lwd=labels.cex,...)
}
\description{
Generate a so-called speed-angle-plot for SIDFEx forecast data. Speed and angle errors are always based on the integrated displacement of the target (e.g., buoy) from its initial position.  If only a (sub-)index of the forecasts is provided, all relevant forecast and observational data are read, remapped to a common time axis, analysed, and subsequently plotted; if intermediate results (that must be consistent with the index) are provided, the corresponding steps are skipped.
}
\arguments{
  \item{index}{
a data frame specifying the SIDFEx forecasts to be read/analysed/plotted, or \code{NULL} (default). In the latter case, \code{read.fcst.res} must be provided and a corresponding index is derived from that.
}
  \item{read.fcst.res}{
a list with SIDFEx forecasts as returned by \code{\link{sidfex.read.fcst}}, or \code{NULL} (default). In the latter case, \code{\link{sidfex.read.fcst}} is used to read the required forecasts based on \code{index}.
}
  \item{read.obs.res}{
a list with SIDFEx observations as returned by \code{\link{sidfex.read.obs}}, or \code{NULL} (default). In the latter case, \code{\link{sidfex.read.obs}} is used to read the required observations (based on \code{index} or \code{read.fcst.res}). Providing this argument can save time. Ignored if \code{remap.res} is provided, which saves even more time.
}
  \item{remap.res}{
a list with remapped SIDFEx observations as returned by \code{\link{sidfex.remaptime.obs2fcst}}, or \code{NULL} (default). In the latter case, \code{\link{sidfex.remaptime.obs2fcst}} is used to remap observations temporally to times at which the forecasts are valid. Providing this argument can save time.
}
  \item{col.by}{
a character specifiying by what property data points shall be coloured. Can be either any column of \code{index} (except \code{File}): \code{File}, \code{GroupID}, \code{MethodID}, \code{TargetID}, \code{InitYear}, \code{InitDayOfYear}, \code{EnsMemNum}, \code{SubmitYear}, \code{SubmitDayOfYear}, \code{ProcessedYear}, \code{ProcessedDayOfYear}, \code{Delay}, \code{nTimeSteps}, \code{FirstTimeStepYear}, \code{FirstTimeStepDayOfYear}, \code{LastTimeStepYear}, \code{LastTimeStepDayOfYear}, and \code{FcstTime}. Or, it can be a column of the forecast data table: \code{Year}, \code{DayOfYear}, \code{Lat}, \code{Lon}, and \code{DaysLeadTime}. Some of these are interpreted in a categorical way (e.g., \code{GroupID}, \code{EnsMemNum}, and \code{Year}), and some as continuous numerical values (e.g., \code{Delay}, \code{Lat}, and \code{DaysLeadTime}, which is the default).
}
  \item{colbar}{
  a spheRlab colourbar as returned by \code{\link{sl.colbar}}). If \code{col.by} is categorical, \code{colbar} must contain at least as many colours as there are categories; if \code{colbar} has more colours, the colourbar is thinned out.
}
  \item{colbar.breaks}{
  an optional numeric vector specifying the breaks between colours. Ignored if \code{col.by} is categorical.
}
  \item{colbar.breaks.log}{
  a logical value indicating whether breaks for the colourbar shall be placed in logarithmic steps. Default is \code{breaks.log=FALSE}. Ignored if \code{col.by} is categorical or if \code{breaks} is provided.
}
  \item{points.type}{
  a character specifying whether only points ('p'), only lines ('l'), or both ('b') shall be plotted.
}
  \item{device}{
  a character specifying which graphics device to use, or \code{NULL} for the default device. Default is \code{device='pdf'}.
}
  \item{file}{
  a character specifying the output file (if \code{device!=NULL}). Default is \code{file=paste0("~/sidfex.plot.speedangle.",device)}.
}
  \item{width}{
  a scalar specifying the width of the resulting figure. The height will be equal. If \code{NULL} (default), a (hopefully) useful value depending on \code{device} is used.
}
  \item{labels}{
  a logical value specifying whether or not labels shall be plotted. Default is \code{TRUE}.
}
  \item{labels.cex}{
  a numerical value specifying the amount by which labels shall be magnified relative to the default.
}
  \item{bg.lines.lwd}{
  a numerical value specifying the width of the background lines (marking the 'coordinate system').
}
  \item{...}{
  Further graphical parameters to be forwarded to \code{\link{points}} (see documentation of \code{\link{points}} for details).
}
}
\value{
A list with the elements \code{col.by}, \code{colbar}, \code{categorical}, \code{breaks}, and \code{labels}, which can be used to plot the corresponding colourbar with \code{\link{sl.plot.colbar}}.
}
%\note{
%
%}
\author{
Helge Goessling
}
\seealso{
\code{\link{sidfex.read.fcst}}, \code{\link{sidfex.read.obs}}, \code{\link{sidfex.remap.obs2fcst}}
}
\examples{
# load whole SIDFEx forecast index
index = sidfex.load.index()

# extract subindex with all forecasts that match some filters
subindex = index[ index$TargetID == "300234063991680" &
                  index$GroupID \%in\% c("metno001","esrl001","ukmo001","eccc001") &
                  index$InitYear == 2018 &
                  index$InitDayOfYear > 210 &
                  index$InitDayOfYear < 215, ]

# Generate a speed-angle-plot for all forecast data corresponding to the subindex, with colours according to the GroupID, as well as the corresponding colourbar
colbar = sidfex.plot.speedangle(subindex,col.by="GroupID",points.type="l",colbar=sl.colbar(cols = c("violet","blue","green","yellow","orange","red"),N=256),device="pdf",file="~/speedangle_example_1.pdf")
sl.plot.colbar(colbar,file.name="~/speedangle_example_1_colbar.pdf")

# Generate another speed-angle-plot for the same data, but with colours according to DaysLeadTime
colbar = sidfex.plot.speedangle(subindex,col.by="DaysLeadTime",points.type="l",colbar=sl.colbar(cols = c("violet","blue","green","yellow","orange","red"),N=256),device="pdf",file="~/speedangle_example_2.pdf")
sl.plot.colbar(colbar,file.name="~/speedangle_example_2_colbar.pdf")
}
