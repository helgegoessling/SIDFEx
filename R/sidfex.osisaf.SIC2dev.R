sidfex.osisaf.SIC2dev<-function(sic.sic, sic.lonlati, projection="polar", polar.latbound=75, device="jpeg", 
                                width=1600, sic.cols=c("steelblue", "white"), col.N=100, coast.col="red", 
                                lon.distance=30, lat.distance=5,pole.hole=TRUE,grid.labels=TRUE,grid.labels.cex=2,
                                filename = paste0("~/sl.plot.",device))
{
  require(spheRlab)  
  if(any(grepl("package:SVGAnnotation", search()))) detach("package:SVGAnnotation") 
  sic.sic[sic.sic==0] = NA
  pir = sl.plot.init(projection=projection,polar.latbound=polar.latbound,device=device ,width=width, file.name = filename)
  cbar = sl.colbar(cols = sic.cols, N=col.N)
  cb = sl.plot.fld.curvilin(pir,vals=sic.sic,lon.i=sic.lonlati$lon.i,lat.i=sic.lonlati$lat.i,colbar=cbar,colbar.breaks=seq(1,col.N-1))
  ne = sl.plot.naturalearth(pir,lines.col=coast.col)
  sl.plot.lonlatgrid(pir,lon.distance=lon.distance,lat.distance=lat.distance,pole.hole=pole.hole,labels=grid.labels,labels.cex=grid.labels.cex)
  sl.plot.end(pir)
}


load("/home/csys/sreifenb/Documents/Rstuff/SIDFEX/SIDFEx_goes_shiny/data/current_SIC.rda")
sidfex.osisaf.SIC2dev(sic.sic=dat[[1]], sic.lonlati = dat[[2]], polar.latbound = 72, filename = "/home/csys/sreifenb/Documents/Rstuff/SIDFEX/SIDFEx_goes_shiny/www/sic.jpeg")