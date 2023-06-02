sidfex.cdm.complex_demodulation <- function(observed_time, observed_drift, NWIN, DWIN, freqS=NA, freqD=NA){
  if (length(observed_time) != length(observed_drift)){
    warning("Length of 'observed_time' and 'observed_drift' must be equal, return NA.")
    return(NA)
  }
  
  # a function that takes 2M real parameters and returns M complex corresponding ones
  parameters_real2complex <- function(parameters_real){
    N = length(parameters_real)
    return(parameters_real[seq(1,N,2)] + 1i * parameters_real[seq(2,N,2)])
  }
  
  # a drift model with five complex parameters
  drift_model <- function(parameters_complex, tgr, freqS, freqD){
    
    V_0   = parameters_complex[1] 
    S_CW  = parameters_complex[2];  S_CCW = parameters_complex[3]
    D_CW  = parameters_complex[4];  D_CCW = parameters_complex[5]
    
    osci_S = S_CW*exp(- 1i * freqS * tgr) + S_CCW*exp(+ 1i * freqS * tgr)
    osci_D = D_CW*exp(- 1i * freqD * tgr) + D_CCW*exp(+ 1i * freqD * tgr)
    
    return(V_0 + osci_D + osci_S)
  }
  
  # a function that calculates residuals of modeled and observed drift (for set of params)
  fit_resid <- function(parameters_real, observed_time, observed_drift, freqS=NA,freqD=NA){
    
    parameters_complex = parameters_real2complex(parameters_real)
    resid = drift_model(parameters_complex, observed_time, freqS, freqD) - observed_drift
    
    return( sum(abs(resid)) ) # return sum of absolute of residuals (this is minimized)
  }
  
  # use M2 and K1 as default values for freq1 and freq2
  if (is.na(freqS)){
    freqS = 24 / 12.42 * 2 * pi
  }
  if (is.na(freqD)){
    freqD =  24 / 23.93 * 2 * pi
  }
  
  NOBS = length(observed_time)
  
  # make arrays too large and weed out later
  demod_params = array(dim=c(NOBS, 10)) 
  demod_time =  rep(NA,NOBS) 
  
  # loop for moving window of length NWIN, moving by DWIN each step
  for (i in (1:NOBS)-1){
    i0 = 1 + DWIN*i
    i1 = NWIN + DWIN*i
    if (i1 > NOBS){break}
    
    twin = observed_time[i0:i1]   # time in days
    vwin = observed_drift[i0:i1]  # complex velocity in m/s
    
    if(any(is.na(vwin))){next}
    
    # the minimization happens here
    optim_out = optim(rep(0, 10), fit_resid, observed_drift=vwin, 
                      observed_time=twin, freqS=freqS, freqD=freqD, method="BFGS")
    params_fit_real = optim_out$par
    
    if (sum(params_fit_real)==0){
      params_fit_real = rep(NA,10)
      twin = twin*NA
    }
    demod_params[i,1:10] = params_fit_real
    demod_time[i] = mean(twin, na.rm=TRUE)
  }
  return(list(time=demod_time, fitted_parameters=demod_params, freqS=freqS, freqD=freqD))
}