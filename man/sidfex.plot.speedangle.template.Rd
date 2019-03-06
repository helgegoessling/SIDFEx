\name{sidfex.plot.speedangle.template}
\alias{sidfex.plot.speedangle.template}
\title{Create clean speed-angle-plot for later use}
\usage{
  sidfex.plot.speedangle.template(device="pdf", file=paste0("~/sidfex.plot.speedangle.template.",device), width=NULL,
                                   labels=TRUE,labels.cex=1,bg.lines.lwd=labels.cex,...) 
}
\description{This function creates an empty speed-angle-plot in a specified format ("device"), i.e. a template for adding lines or point clusters later.}

\arguments{
  \item{device}{
    a character specifying which graphics device to use, or \code{NULL} for the default device. Default is \code{device='jpeg'}.
  }
  \item{file}{
    a character specifying the output file (if \code{device!=NULL}). Default is \code{file=paste0("~/sidfex.plot.speedangle.",device)}.
  }
  \item{width}{
    a scalar specifying the width of the resulting figure. The height will be equal. If \code{NULL} (default), a (hopefully) useful value depending on \code{device} is used. It is recommended to to use values around \code{600} to \code{800}. Greater values are also possible, but then you have to adjust the scales of labels as well. 
    
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
  PLEASE NOTE: If you use the saved image and want to plot data on it without (!) using \code{\link{sidfex.plot.speedangle2template}}, you have to multiply the x- and y-coordinates by \code{1/1.08} right before calling \code{\link{points}} or \code{\link{lines}}; OR you rescale the image after loading it in R during your call of rasterImage (then you need to multiply xlim and ylim by 1.08). This might appear cumbersome, but it is necessary as R automatically adds a \code{4\%}-margin around the actual axes in each direction when saving, such that plotting the image into axes with presumably the same range (usually -2 to 2 in both directions) will shrink the image a little - which might remain unnoticed in the worst case. This is why we highly recommend using \code{\link{sidfex.plot.speedangle2template}}, then you don't have to think about this issue at all!
  
  So far, there is no functionality to read an PDF image for further plotting, it is still possible to create them with this function though.

  Also, this function will be modified in a way that it can be called inside \code{\link{sidfex.plot.speedangle}}, i.e. it will provide the template directly without the need for saving and reloading an image. This is work in progress.
}

\author{
  Simon Reifenberg
}
\seealso{
  \code{\link{sidfex.plot.speedangle}}, \code{\link{sidfex.plot.speedangle2template}}
}
\examples{
  # plot a template for speed-angle-plot in jpeg format
  sidfex.plot.speedangle.template(device = "jpeg")
}
