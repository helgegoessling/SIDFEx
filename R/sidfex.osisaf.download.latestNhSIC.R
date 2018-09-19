sidfex.osisaf.download.latestNhSIC <- function(url = "ftp://osisaf.met.no/prod/ice/conc/", outdir="", do.calc.lonlati = T, do.del.nc = T)
{
  require(RCurl)
  require(spheRlab)
  require(ncdf4)
  require(stringi)
  require(tictoc)
  if(any(grepl("package:SVGAnnotation", search()))) detach("package:SVGAnnotation") 
  
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