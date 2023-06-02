\name{sidfex.cdm.complex_demodulation}
\alias{sidfex.cdm.complex_demodulation}
\title{
Function to seperate observed ice drift trajectories into oscillating parts and 'background' drift.
}
\description{
This function takes Lagrangian ice-drift velocities and seperates it into oscillating parts (with two adjustable frequencies) and a background drift. It follows the approach of McPhee (1988), with technical adjustments regarding the minimization. It can be used, for instance, for removing tidal and inertial oscillations from observed trajectories.

Note that the temporal resolution of the observed drift should be high enough for resolving said oscillations, 1h time steps should be sufficient for most applications. 

An exemplary use case is provided here: \code{\link{sidfex.cdm.reconstruct_drift}}.
}
\usage{
sidfex.cdm.complex_demodulation(observed_time, observed_drift, NWIN, DWIN, freqS=NA, freqD=NA)
}
\arguments{
  \item{observed_time}{Vector of equidistant time variable (unit should be days!)}
  \item{observed_drift}{Vector of complex drift velocity, \code{u+1i*v}, (unit in meter per second)}
  \item{NWIN}{Window length (scalar) for the complex demodulation. Should cover the time period of the slowest oscillation, or at least not significantly less. With equidistant spacing \code{dt}, the running window will cover the time period \code{(NWIN-1)*dt}.}
  \item{DWIN}{Scalar index by which the moving window is shifted in each iteration. A value of \code{DWIN=NWIN/2} creates half-overlapping windows. }
  \item{freqS}{OPTIONAL, frequency in cycles per day (cpd) of the semidiurnal oscillations. If left to \code{NA}, the M2 tidal frequency is used. The coriolis parameter or simply 2 are also sensible choices.}
  \item{freqD}{OPTIONAL, frequency in cycles per day (cpd) of the diurnal oscillations. If left to \code{NA}, the K1 tidal frequency is used. A value of 1 is also a sensible choice.}
}
\details{
This function fits the complex velocity in each window of length \code{NWIN} to a linear combination of four phasors of type \code{A*exp(1i*omega*t)} (with an amplitude \code{A} and a frequency \code{omega}) and a non-oscillating "background" drift \code{V0}.

The five fitted complex parameters are then \code{V0} (background drift),  \code{S_CW} (semidiurnal clockwise amplitude),  \code{S_CCW} (semidiurnal counterclockwise amplitude),  \code{D_CW} (diurnal clockwise amplitude) , and \code{D_CCW} (diurnal clockwise amplitude). The model for the demodulation then looks like this:

\code{osci_S = S_CW*exp(- 1i * freqS * tgr) + S_CCW*exp(+ 1i * freqS * tgr)  }
    
\code{osci_D = D_CW*exp(- 1i * freqD * tgr) + D_CCW*exp(+ 1i * freqD * tgr)  }
    
\code{drift_model = V_0 + osci_D + osci_S  }

The applied optimization minimizes the residuals of observed and modeled drift. For technical reasons, the five complex fit parameters are converted to ten real-valued parameters internally. 

The (demodulated) drift trajectory, given an initial position, can be obtained by integration of the (demodulated) velocity. This is done in the example of \code{\link{sidfex.cdm.reconstruct_drift}}.

}
\value{
  \item{time}{Time vector for fitted parameters, taken as the bin center of the moving window}
  \item{fitted_parameters}{Array containing the time series of the fitted parameters (split into real and imaginary part, pairwise, in the same order they appear in the model equation above); time series of the complex parameters are obtained via \code{V0 = fitted_paramters[:,1] + 1i*fitted_paramters[:,2]} and so on.}
  \item{freqS}{Frequency in cycles per day (cpd) of the semidiurnal oscillations used in drift model. }
  \item{freqD}{Frequency in cycles per day (cpd) of the diurnal oscillations used in drift model.}
}
\note{
  The two input frequencies do not have to be diurnal/semidiurnal. However, this is probably the most common case. In high latitudes, the inertial frequency will be close to the one of the M2 tide, which makes the separation of those two a bit difficult.
}
\references{
McPhee, M. G. (1988). "Analysis and Prediction of Short-Term Ice Drift." ASME. J. Offshore Mech. Arct. Eng. February 1988; 110(1): 94–100. https://doi.org/10.1115/1.3257130
}
\author{
Simon F. Reifenberg
}

\seealso{
\code{\link{sidfex.cdm.reconstruct_drift}}
}


