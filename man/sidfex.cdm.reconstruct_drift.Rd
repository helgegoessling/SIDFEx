\name{sidfex.cdm.reconstruct_drift}
\alias{sidfex.cdm.reconstruct_drift}
\title{
Auxiliary function for reconstructing ice drift based on fitted parameters from complex demodulation.
}
\description{
This function takes the parameters from the optimization performed in \code{\link{sidfex.cdm.comlex_demodulation}} and generates time series of demodulated ice drift (background, semidiurnal oscillations, dirunal oscillations) for a given input time grid. Between the fitted parameters, linear interpolation will be used. 

}
\usage{
sidfex.cdm.reconstruct_drift(time_grid_out, demodulation_output)
}
\arguments{
  \item{observed_time}{vector of time variable for which the demodulated drift will be evaluated resp. interpolated based on the fitted parameters (unit should be days!)}
  \item{demodulation_output}{list, output of \code{\link{sidfex.cdm.comlex_demodulation}}}
  
}
\value{ 
  \item{time}{Time vector of demodulated drift. As the function input time vector will produce leading or trailing NAsif it is partly outside the range of the applied moving window (centers), this vector might be shorter than the input time vector.}
  \item{background}{Time series of (complex) 'background' velocity.}
  \item{diurnal}{Time series of complex velocity from diurnal oscillations.}
  \item{semidiurnal}{Time series of complex velocity from semidiurnal oscillations.}
Note that the sum of the output velocity time series should closely resemble the original drift.
}
\references{
McPhee, M. G. (1988). "Analysis and Prediction of Short-Term Ice Drift." ASME. J. Offshore Mech. Arct. Eng. February 1988; 110(1): 94–100. https://doi.org/10.1115/1.3257130
}
\author{
Simon F. Reifenberg
}

\seealso{
\code{\link{sidfex.cdm.comlex_demodulation}}
}
\examples{

  # NOTE: THIS EXAMPLE NEEDS SIDFEx OBSERVATIONS OF TARGET 300534062174040
  # You might want to run: sidfex.download.obs(TargetID="300534062174040")

  library(SIDFEx)
  library(spheRlab)
  abg = sl.lonlatrot2abg(c(5,81.5,0))
  Rearth = 6371
  
  
  # read data
  tarID = "300534062174040"
  obs = sidfex.read.obs(TargetID=tarID)
  
  reltime = obs$data$RelTimeDay
  lat = obs$data$Lat
  lon = obs$data$Lon
  
  # get velocity time series by transformation in x-y coordinates and differencing
  xyz = sl.rot(lon, lat, abg[1], abg[2], abg[3], return.xyz = TRUE)
  xice = xyz$x*Rearth # in km
  yice = xyz$y*Rearth # in km
  
  uice = diff(xice)/diff(reltime) / 86.4    # in m/s
  vice = diff(yice)/diff(reltime) / 86.4    # in m/s
  tice = 0.5*(reltime[2:length(reltime)]+reltime[1:(length(reltime)-1)]) # in days
  
  # throw out values before and after deployment
  i_good = which((tice > 2.3) & (tice < 20))
  
  tice = tice[i_good]
  uice = uice[i_good]
  vice = vice[i_good]
  
  # complex velocity 
  cv = uice + 1i*vice
  
  # demodulation
  nwin = 32; dwin = 4
  re = sidfex.cdm.comlex_demodulation(tice, cv, nwin, dwin)
  
  # reconstruct
  dt_out = 1/48
  time_grid = seq(0,17.5, dt_out)
  cdm_rec = sidfex.cdm.reconstruct_drift(time_grid, re)
  
  cv_rec_osc = cdm_rec$diurnal + cdm_rec$semidiurnal
  cv_rec_bgd = cdm_rec$background 
  tcdm = (cdm_rec$time) 
  
  u_osc = Re(cv_rec_osc)  # oscillating part (u-component)
  v_osc = Im(cv_rec_osc)  # oscillating part (v-component)
  u_bgd = Re(cv_rec_bgd)  # 'leftover' drift (u-component)
  v_bgd = Im(cv_rec_bgd)  # 'leftover' drift (v-component)
  
  
  # let's check weather the sum of the demodulated parts reproduces the old drift
  plot(tice, uice, type="o", col="firebrick", cex=.6, pch=20, lwd=.7)
  lines(tcdm, u_osc+u_bgd)
  
  plot(tice, vice, type="o", col="steelblue", cex=.6, pch=20, lwd=.7)
  lines(tcdm, v_osc+v_bgd)

  ### for getting the trajectory back, you might do the following things:
  
  # pick an initial position (integration constant), a good choice should be this:
  it = which.min(abs(tice-tcdm[1])) # nearest value to first demodulated timestamp
  x0 = xice[it]; y0 = yice[it]
  
  # integrate the velocity time series:
  x_rec = x0 + c(0,cumsum(u_bgd+u_osc)*dt_out*86.4)  # in km
  y_rec = y0 + c(0,cumsum(v_bgd+v_osc)*dt_out*86.4)  # in km
  
  # note: if we had used cumsum(u_bgd) instead, we would have obtained the background trajectory.
  # which is quite likely mostly wind-driven
  
  # rotate back to lon-lat
  lola   = sl.xyz2lonlat(x=x_rec, y=y_rec, z = sqrt(Rearth^2-x_rec^2-y_rec^2))
  lolaro = sl.rot(lola$lon, lola$lat, abg[1], abg[2], abg[3], invert = TRUE)
  
  # done.
  
  # very quick look:
  plot(lolaro$lon, lolaro$lat)    # reconstructed
  lines(lon, lat, col="red")      # original 
  
  # x-y plane:
  # plot(x_rec, y_rec)
  # lines(xice, yice, lwd=2, col="red")

}

