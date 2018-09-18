sidfex.osisaf.nc2rda <- function(filename){
  sic_fl = nc_open(filename)
  sic = ncvar_get(sic_fl, "ice_conc")
  sic[sic==0] = NA
  sic_lon = ncvar_get(sic_fl, "lon")
  sic_lat = ncvar_get(sic_fl, "lat")
  nc_close(sic_fl)
  lonlati = sl.lonlat2D.c2i(lon.c=sic_lon,lat.c=sic_lat)
  out = list(sic, lonlati)
  return(out)
}

#dat = sidfex.osisaf.nc2rda("/home/csys/sreifenb/Documents/Rstuff/SIDFEX/SIDFEx_goes_shiny/data/ice_conc_nh_polstere-100_multi_201809161200.nc"
                                          #)

