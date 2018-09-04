\name{sidfex.fcst.search.extractFromTable}
\alias{sidfex.fcst.search.extractFromTable}
\title{Extract specific row(s) of search index table}
\usage{
  sidfex.fcst.search.extractFromTable (indexTable.path = NULL, gid=NULL, mid=NULL, tid=NULL, 
           iy=NULL, idoy=NULL, emn=NULL, sy=NULL, sdoy=NULL, py=NULL, pdoy=NULL, 
           del=NULL, nt=NULL, fy=NULL, fdoy=NULL, ly=NULL, ldoy=NULL, per=NULL)
}
\description{
  This function is more or less the core of the upcoming "Shiny App" for SIDFEx. It allows a user to quickly find the filename of forecast datasets based on a useful set of search parameters. These filenames can then be easily used to load the forecast of interest for further processing and/or analysis.
}
\arguments{
  \item{indexTable.path}{
    \code{NULL} or a character specifying the directory where to find or create the index file. If \code{NULL}, it is attempted to source a file named       \code{.SIDFEx} in the home directory where a character variable \code{data.path.indexTable.in} needs to be defined.
  }

  \item{gid}{
    a character for the GroupID of interest (point query)
  }  
  \item{mid}{
    a character for the MethodID of interest (point query)
  }  
  \item{tid}{
    a character for the TargetID of interest (point query, implementation of range query in progress)
  }  
  \item{iy}{
    a numeric value (or list/vector of such) for the initial year (point query or range query)
  }  
  \item{idoy}{
    a numeric value (or list/vector of such) for the initial day of year (point query or range query)
  }  
  \item{emn}{
    a integer (or list/vector of inters) for the ensemble member number of interest (point query or range query)
  }  
  \item{sy}{
    a numeric value (or list/vector of such) for the year that the respective file was submitted in (point query or range query)
  }  
  \item{sdoy}{
    a numeric value (or list/vector of such) for the day of year that the respective file was submitted (point query or range query)
  }  
  \item{py}{
    a numeric value (or list/vector of such) for the year that the respective file was processed (point query or range query)
  }  
  \item{pdoy}{
    a numeric value (or list/vector of such) for the day of year that the respective file was processed (point query or range query)
  }  
  \item{del}{
    a numeric value (or list/vector of such) for the submitting delay (difference of submit time and initial time of forecast), (point query or range query)
  }  
  \item{nt}{
    a numeric value (or list/vector of such) for the number of timesteps of the forecast (point query or range query)
  }  
  \item{fy}{
    a numeric value (or list/vector of such) for the year of the first timestep (point query or range query)
  }  
  \item{fdoy}{
    a numeric value (or list/vector of such) for the day of year of the first timestep (point query or range query)
  }  
  \item{ly}{
    a numeric value (or list/vector of such) for the year of the last timestep  (point query or range query)
  }  
  \item{ldoy}{
    a numeric value (or list/vector of such) for the day of year of the last timestep  (point query or range query)
  }  
  \item{per}{
    a numeric value (or list/vector of such) for the forecast time in days, i.e. the time difference between first and last timestep (point query or range query)
  }  

}
\value{
  Returns a list of filenames (without extension) or NULL, if no files fit to search parameters. 
}
\note{ 
  As all search parameters are \code{NULL} by default, currently unimportant parameters can just be omitted, see example below. 

  If you loaded the data frame from the .rda file (will appear as "rTab" in environment) in R, you can use the output list of this function like in the example below to obtain a reduced list (this selection procedure is basically equivalent to horizontal slicing of the index table).
  
}
\author{
Simon Reifenberg
}

\examples{
# get relevant path of indexTable
source(file.path("~",".SIDFEx")

# load index table rTab (will be a R data frame)
load(paste0(indexTable.path.in, "indexTable.rda"))

# use the function to extract the filenames of all forecasts with GroupID awi003, initialized in 2017 and ensemble member number 1
selected_files = sidfex.fcst.search.extractFromTable(indexTable.path.in, gid="awi003", iy=2017, emn = 1)

# use the just generated list to filter rTab and store the filtered data frame in a new variable for later use
rTab_reduced = rTab[rTab$File \%in\% selected_files, ]
}

\seealso{
\code{\link{sidfex.fcst.search.createIndexTable}}, \code{\link{sidfex.fcst.search.addTableItem}}, \code{\link{sidfex.fcst.search.rda2csv}}
}
