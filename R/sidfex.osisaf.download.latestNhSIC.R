# this is a script that will download the most recent SIC data from osisaf, process it and save it as .rda file as well as jpeg
# for later use

library(RCurl)
require(spheRlab)
require(ncdf4)
require(stringi)
require(tictoc)

# the following variables will probably input parameters for upcoming function :)
do.del.nc <- T                           # delete existing sea ice .nc if exists 
do.calc.lonlati <- !T                      # do we want to use tis sl.lonlat2D.c2i() function or did we already? T/F
url <- "ftp://osisaf.met.no/prod/ice/conc/"   # ftp server with data
outdir <- "/home/csys/sreifenb/Documents/Rstuff/SIDFEX/SIDFEx_goes_shiny/data/"

sidfex.osisaf.download.latestNhSIC <- function(url = "ftp://osisaf.met.no/prod/ice/conc/", outdir="/home/csys/sreifenb/Documents/Rstuff/SIDFEX/SIDFEx_goes_shiny/data/", do.calc.lonlati = T, do.del.nc = T)
{
  if (do.del.nc & !do.calc.lonlati) {
    print("You're about to download a file and delete it without processing. You hereby have been warned.")
  }
  
  # from ftp to nc in folder
  files <- as.vector(strsplit(getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE) , '\n'))[[1]]
  file <- tail(files[stringi::stri_startswith(files, fixed="ice_conc_nh_")], n=1)
  if (!file.exists(paste(outdir, file, sep = ""))){
    download.file(paste(url, file, sep = ""), paste(outdir, file, sep = ""))
  } else {
      print(paste0("File ", paste(outdir, file, sep = "")," already exists."))
      return(FALSE)
  }
  
  # from nc to processed .rda
if (do.calc.lonlati){
    source("/home/csys/sreifenb/Documents/Rstuff/SIDFEX/SIDFEx_goes_shiny/sidfex.osisaf.nc2rda.R")
    dat = sidfex.osisaf.nc2rda(paste0(outdir, file))
    save(dat, file=paste0(outdir, "current_SIC.rda"))
    toc()
  }
  
  # save a short text file with information on current data (for automatic headers in shiny?)
  if (!file.exists(paste0(outdir, "SICdwnld_log.txt"))) {
    fileConn<-file(paste0(outdir, "SICdwnld_log.txt"))
    writeLines(c("This is a very basic log file to keep track of the latest SIC download",
                 paste0(file, " was downloaded: ", Sys.time())), fileConn)
    close(fileConn)
  } else {
    write(paste0(file, " was downloaded: ", Sys.time()),file=paste0(outdir, "SICdwnld_log.txt"),append=TRUE)
  }
  
  write(paste0(file, " was also processed? ", do.calc.lonlati),file=paste0(outdir, "SICdwnld_log.txt"),append=TRUE)
  
  # optionally delete nc file to save space (default: do deletion)
  if(do.del.nc) {
    system(paste0("rm ", outdir, file))
  }
  
  write(paste0(file, " was deleted by download function? ", do.del.nc),file=paste0(outdir, "SICdwnld_log.txt"),append=TRUE)
  
  return(TRUE)
}

sidfex.osisaf.download.latestNhSIC()