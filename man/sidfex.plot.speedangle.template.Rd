\name{sidfex.plot.speedangle.template}
\alias{sidfex.plot.speedangle.template}
\title{Create clean speed-angle-plot for later use}
\usage{
  sidfex.plot.speedangle.template(device="pdf", file=paste0("~/sidfex.plot.speedangle.template.",device), width=NULL,
                                   labels=TRUE,labels.cex=1,bg.lines.lwd=labels.cex,...) 
}
\description{This function creates an empty speed-angle-plot, i.e. a template for adding lines or point clusters later.}

\arguments{
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
 The function returns nothing but saves the template to the specified destination.
}

\note{
  This function will be modified in a way that it can be called in \code{\link{sidfex.plot.speedangle}}, i.e. it will provide the template directly without the need for saving and reloading the template. This is work in progress.
  
  In addition to that, there will be a new function that takes the output file of this function and plots data on it. The combination of that and the current function will mimic and probably replace \code{\link{sidfex.plot.speedangle}} (or just simplify the latter a lot).
}

\author{
  Simon Reifenberg
}
\seealso{
  \code{\link{sidfex.plot.speedangle}} 
}
\examples{
  # plot a template for speed-angle-plot in jpeg format
  sidfex.plot.speedangle.template(device = "jpeg")
}
