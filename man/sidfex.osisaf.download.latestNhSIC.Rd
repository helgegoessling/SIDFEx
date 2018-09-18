\name{sidfex.osisaf.download.latestNhSIC}
\alias{sidfex.osisaf.download.latestNhSIC}
\title{Download and process OSI-SAF Arctic sea-ice concentration data}
\usage{
  sidfex.osisaf.download.latestNhSIC(url = "ftp://osisaf.met.no/prod/ice/conc/", outdir="", do.calc.lonlati = T, do.del.nc = T))
  }
\description{
  This function will download the most recent Arctic sea-ice concentration data from the OSI-SAF ftp server and process it using \code{\link{sidfex.osisaf.SIC2dev}} and  \code{\link{sidfex.osisaf.nc2rda}}}. 
}
\arguments{
  \item{url}{
    URL of OSI-SAF ftp server
  }
  \item{outdir}{
    character for output directory (current working directory as default); downloaded .nc file, processed .rda file and small log file will be written in this directory.
  }  
  \item{do.calc.lonlati}{
    boolean value, if \code{TRUE} (default), the function applies \code{\link{sl.lonlat2D.c2i}} on grid, otherwise (set to \code{FALSE}) only the .nc file is downloaded
  }  
  \item{do.del.nc}{
    boolean value, if \code{TRUE} (default), the function deletes downloaded .nc file at the end of the procedure to save space and keeps the file if set to \code{FALSE}.
  }  
}

\value{
  Returns \code{TRUE} if the whole procedure came to an end and \code{FALSE} if there is already the currently available NetCDF file in the specified output directory. 
}

\note{
  This function is intended for automated updates of the daily sea-ice concentration and writes a small log file in output directory to keep track of the current state, as the processed files ("current_SIC.rda") will be overwritten in every use of this function. 
}

\author{
Simon Reifenberg
}

\seealso{
\code{\link{sidfex.osisaf.SIC2dev}}, \code{\link{sidfex.osisaf.nc2rda}}}, \code{\link{sl.lonlat2D.c2i}}
}
