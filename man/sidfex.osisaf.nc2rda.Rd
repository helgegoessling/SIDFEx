\name{sidfex.osisaf.nc2rda}
\alias{sidfex.osisaf.nc2rda}
\title{Load OSI-SAF data from NetCDF file and process sea-ice concentration and underlying grid}
\usage{
  sidfex.osisaf.nc2rda(filename)
}
\description{
  This function reads the grid information and sea-ice concentration from the frequently downloaded NetCDF file. It then applies \code{sl.lonlat2D.c2i} from spheRlab and returns the sea-ice contration together with the ouput of the spheRlab routine.
}
\arguments{
  \item{filename}{
    the path and filename of the OSI-SAF datafile (must be the same or comparable to the available .nc files on OSI-SAF ftp server: "ftp://osisaf.met.no/prod/ice/conc/").
  }
}
\value{
  Returns sea-ice concentration and processed grid data, also sets all datapoints without sea ice to \code{NA}.
}

\note{
  The output can be further processed with \code{\link{sidfex.osisaf.SIC2dev}}.
}

\author{
Simon Reifenberg, Helge Goessling
}

\seealso{
\code{\link{sidfex.osisaf.SIC2dev}}, \code{\link{sidfex.osisaf.download.latestNhSIC}}, \code{\link{sl.lonlat2D.c2i}}
}
