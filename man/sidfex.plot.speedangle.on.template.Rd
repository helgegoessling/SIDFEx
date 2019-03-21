\name{sidfex.plot.speedangle.on.template}
\alias{sidfex.plot.speedangle.on.template}
\title{Plot Speed-vs-Angle data on existing template}
\usage{
sidfex.plot.speedangle.on.template(index=NULL,read.fcst.res=NULL,read.obs.res=NULL,remap.res=NULL,
                                   col.by="DaysLeadTime",colbar=sl.colbar.redgreyblue_256,
                                   colbar.breaks=NULL,colbar.breaks.log=FALSE,points.type="p",out.device="pdf", in.device="png",
                                   file=paste0("~/sidfex.plot.speedangle.",out.device),width=NULL,outer.plot=F
                                   templatefile = NULL,...) 
}
\description{
Does mainly the same thing as \code{\link{sidfex.plot.speedangle}} but uses an input file for the background axis and labels.
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
  \item{in.device}{
  a character specifying the graphics device of the template (\code{templatefile}), or \code{NULL} for the default device. Default is \code{device='png'}.
}
  \item{out.device}{
  a character specifying which graphics device to use, or \code{NULL} for the default device. Default is \code{device='pdf'}.
}
  \item{file}{
  a character specifying the output file (if \code{device!=NULL}). Default is \code{file=paste0("~/sidfex.plot.speedangle.",out.device)}.
}
  \item{width}{
  a scalar specifying the width of the resulting output figure. The height will be equal. If \code{NULL} (default), a (hopefully) useful value depending on \code{device} is used.
}
  \item{outer.plot}{
  a boolean (default \code{FALSE}) specifying whether there is already a new plot created (e.g. calling \code{par)()} and \code{plot()}) outside this function or not. If \code{TRUE}, the calls of \code{par()} and \code{plot()} inside the function will be suppressed, which basically avoids plotting unnecessary axes and keeps the aspect ratio correctly. The default should be sufficient for almost all cases.  
  }
  \item{templatefile}{
  the image on which the data will be plotted. For best results use a png-file created by \code{\link{sidfex.plot.speedangle.template}}. 
}
  \item{...}{
  Further graphical parameters to be forwarded to \code{\link{points}} (see documentation of \code{\link{points}} for details).
}
}
\value{
A list with the elements \code{col.by}, \code{colbar}, \code{categorical}, \code{breaks}, and \code{labels}, which can be used to plot the corresponding colourbar with \code{\link{sl.plot.colbar}}.
}
\note{
So far (and very certainly it will stay like this) only 'png' and 'jpeg' are intended and implemented as template types (\code{in.device}). We recommend the usage of 'png', as 'jpeg' could lead to data points covering the labels, which can be solved by using transparend templates - a feature, that 'png' easily can provide. Aside from this limitation, the output files can be any supported graphics device.

As this function is almost identical with \code{\link{sidfex.plot.speedangle}} now, the authors will soon discuss the necessity of keeping both.
}
\author{
Simon Reifenberg, Helge Goessling
}
\seealso{
\code{\link{sidfex.plot.speedangle.template}}, \code{\link{sidfex.plot.speedangle}}
}
\examples{

# load whole SIDFEx forecast index
index = sidfex.load.index()
 
# extract subindex with all forecasts that match some filters
subindex = index[ index$TargetID == "300234063991680" &
                   index$GroupID %in% c("metno001","esrl001","ukmo001","eccc001") &
                   index$InitYear == 2018 &
                   index$InitDayOfYear > 210 &
                   index$InitDayOfYear < 215, ]
                   
# create the template
fn_template = "~/speedangle.template.png"
sidfex.plot.speedangle.template(device = "png", file = fn_template, width = 700)

# generate a speed-angle-plot for all forecast data corresponding to the subindex, with colours according to the GroupID
cb = sidfex.plot.speedangle.on.template(subindex,col.by="GroupID",points.type="p",colbar=sl.colbar(cols = c("violet","blue","green","yellow","orange","red"),N=256),
                                   out.device="png",file="~/speedangle_example_plot2template.png", 
                                   templatefile = fn_template, width = 800)

}
