sidfex.osisaf.nc2rda <- function(filename){
  if(any(grepl("package:SVGAnnotation", search()))) detach("package:SVGAnnotation") 
  sic_fl = nc_open(filename)
  sic = ncvar_get(sic_fl, "ice_conc")
  sic_lon = ncvar_get(sic_fl, "lon")
  sic_lat = ncvar_get(sic_fl, "lat")
  nc_close(sic_fl)
  lonlati = sl.lonlat2D.c2i(lon.c=sic_lon,lat.c=sic_lat)
  out = list(sic, lonlati)
  return(out)
}