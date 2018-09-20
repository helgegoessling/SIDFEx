\name{sidfex.osisaf.SIC2dev}
\alias{sidfex.osisaf.SIC2dev}
\title{Plot processed OSI-SAF data in specific device for later use}
\usage{
  sidfex.osisaf.SIC2dev (sic.sic, sic.lonlati, projection="polar", polar.latbound=75, device="jpeg", 
                                width=1600, sic.cols=c("steelblue", "white"), col.N=100, coast.col="red", 
                                lon.distance=30, lat.distance=5,pole.hole=TRUE,grid.labels=TRUE,grid.labels.cex=2,
                                filename = paste0("~/sl.plot.",device))
}
\description{
  This function creates a plot of the processed OSI SAF sea-ice concentration (see \code{\link{sidfex.osisaf.nc2rda}} for more information on processing) with some input plot specifications. The plot is sent to a specific device (default: jpeg) and can be used later. The need for this came up in considerations on time saving in the plotting routines of the upcoming Shiny App for SIDFEx.
}
\arguments{
  \item{indexTable.path}{
    \code{NULL} or a character specifying the directory where to find or create the index file. If \code{NULL}, it is attempted to source a file named       \code{.SIDFEx} in the home directory where a character variable \code{data.path.indexTable.in} needs to be defined.
  }

  \item{sic.sic}{
    a 2D array of sea-ice concentration data
  }  
  \item{sic.lonlati}{
    output of \code{\link{sl.lonlat2D.c2i}}, see spheRlab documentation for further information. (NB: if sic.sic has shape (n, m), then sic.lonlati$<item> has shape (n+1,m+1)).
  }  
  \item{projection}{
    a character specifing the plot projection type. Currently possible projection is only \code{"polar"} (orthographic), to be extended. 
  }  
  \item{polar.latbound}{
    a scalar in the range [0,90) specifying the latitude (post-rotation) at which the plot shall be truncated. Default is \code{polar.latbound=0}, in which case the whole hemisphere is plotted.  Only used if \code{projection="polar"}.
  }    
  \item{device}{
    a character specifying which graphics device to use. Default is \code{device='jpeg'}.
  }  
  \item{width}{
    a scalar giving the width of the resulting figure. The height will be derived according to the chosen projection and other relevant plotting parameters. Only used if \code{do.init.device=TRUE}. Default is \code{width=12}.
  }  
  \item{sic.cols}{
    either a list of (at least 2) RGB(alpha) tuples (length-3 or length-4 vectors in the range [0,1]) or a character vector of named colours and/or hexadecimal-coded RGB colours. Defines the basis colours of the colourbar.
  }  
  \item{col.N}{
    number of colours the resulting colourbar shall contain.
  }    
  \item{coast.col}{
    a colour or vector of colours to be used for any lines.
  }    
  \item{lon.distance}{
    a scalar specifying the distance (degrees) between adjacent longitudes.
  }    
  \item{lat.distance}{
    a scalar specifying the distance (degrees) between adjacent latitudes.
  }    
  \item{pole.hole}{
    a logical value indicating whether or not to let longitudinal lines end at the last latitude(s) before the pole(s). 
  }    
  \item{grid.labels}{
    a logical value indicating whether or not to add longitude and latitude labels. Note that the placement of labels is determined automatically based on the lon-lat-grid, with some arguments to control the behaviour. Latitude labels are placed between lines of longitudes and vice versa. For more flexibility, use \code{sl.plot.lonlatlabels} directly.
  }    
  \item{grid.labels.cex}{
    numeric character expansion factor for the labels; multiplied by \code{par("cex")} yields the final character size. \code{NULL} and \code{NA} are equivalent to \code{1.0}.
  }    
  \item{filename}{
    a character specifying the output file. Default is \code{file.name=paste0('~/sl.plot.',device)}.
  }
}
\value{
  Returns nothing, but creates the plot in the specified device.
}
\note{ 
  It is strongly advised to use the output of \code{\link{sidfex.osisaf.nc2rda}} or the saved .rda file from  \code{\link{sidfex.osisaf.download.latestNhSIC}} only as input for \code{sic.sic} and \code{sic.lonlati}, as these functions were developed together for the single purpose of automation and performance-enhancing of sea-ice concentration visualization in the SIDFEx Shiny App. 
  
  Also note that this function depends on the spheRlab package. 
}
\author{
Simon Reifenberg, Helge Goessling
}

\examples{
  # if output of sidfex.osisaf.nc2rda exists (will be a variable called "dat"):
  sidfex.osisaf.SIC2dev(sic.sic=dat[[1]], sic.lonlati = dat[[2]], polar.latbound = 72)
}

\seealso{
  \code{\link{sidfex.osisaf.nc2rda}}, \code{\link{sidfex.osisaf.download.latestNhSIC}}, \code{\link{sl.lonlat2D.c2i}}
}
