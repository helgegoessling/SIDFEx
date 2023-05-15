sidfex.cdm.reconstruct_drift <- function(time_grid_out, demodulation_output){
  #   Returns a list with demodulated velocity components.
  #   Input: desired output time grid, time and parameters from demodulation + used frequencies
  
  demod_time = demodulation_output$time
  demod_params = demodulation_output$fitted_parameters
  
  freqS = demodulation_output$freqS
  freqD = demodulation_output$freqD
  
  # de-nan vectors for interpolation
  ninan = which(!is.na(demod_time))
  xint = demod_time[ninan]
  yint = demod_params[ninan,]
  
  # interpolate onto specified output grid
  params_interpolated = t(apply(yint, 2, function(y) approx(xint, y, time_grid_out)$y))
  
  # reconstruct drift component-wise
  CV_MEAN = params_interpolated[1,] + 1i*params_interpolated[ 2,]
  S_CW    = params_interpolated[3,] + 1i*params_interpolated[ 4,]
  S_CCW   = params_interpolated[5,] + 1i*params_interpolated[ 6,]
  D_CW    = params_interpolated[7,] + 1i*params_interpolated[ 8,]
  D_CCW   = params_interpolated[9,] + 1i*params_interpolated[10,]
  
  
  CV_SEMI = S_CW*exp(- 1i * freqS * time_grid_out) +
    S_CCW*exp(+ 1i * freqS * time_grid_out)
  CV_DIUR = D_CW*exp(- 1i * freqD * time_grid_out) + 
    D_CCW*exp(+ 1i * freqD * time_grid_out)
  
  ninan = which(!is.na(CV_MEAN))
  
  CV_MEAN = CV_MEAN[ninan]
  CV_SEMI = CV_SEMI[ninan]
  CV_DIUR = CV_DIUR[ninan]
  
  TIME = time_grid_out[ninan]
  
  return(list(time=TIME, background=CV_MEAN, diurnal=CV_DIUR, semidiurnal=CV_SEMI))
}