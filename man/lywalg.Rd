\name{lywalg}
\alias{lywalg}
\title{ W-algorithm for M-estimate of location }
\description{
  Robust location estimate with simultaneous estimation of the scale parameter
}
\usage{
lywalg(y, lambda, psp0 = psp(0), expsi = psi, exchi = chi, exrho = rho, 
       sigmai, tol = .dFvGet()$tlo, gam = .dFvGet()$gma, 
       isigma = .dFvGet()$isg, maxit = .dFvGet()$mxt, maxis = .dFvGet()$mxs, 
       nitmon = .dFvGet()$ntm)
}
\arguments{
  \item{y}{ Vector containing the observations }
  \item{lambda}{ Initial solution of the location parameter }
  \item{psp0}{ Value of psp(0) (first derivative of the psi function)}
  \item{expsi}{ User supplied psi function }
  \item{exchi}{ User supplied chi function }
  \item{exrho}{ User supplied rho function }
  \item{sigmai}{ Initial estimate of the scale parameter }
  \item{tol}{ Relative precision for the convergence criterion }
  \item{gam}{ Relaxation factor. Set 0 < gam < 2. }
  \item{isigma}{ If isigma<0, the value of sigma is not changed during the first 
                 iteration.
   If isigma=0, bypasss iteration on sigma (sigmaf=sigmai). If |isigma|>0, sigma 
   is updated using the robeth function rysigm.}
  \item{maxit}{ Maximum number of cycles }
  \item{maxis}{ Maximum number of iterations for the scale step }
  \item{nitmon}{ If nitmon>0 and the  iteration counter is a multiple of nitmon,
   the current value of sigma, theta and delta are printed. If no iteration
   monitoring is required, set nitmon equal to 0. }
}
\details{
  The .dFv variables for the default values must be created by a call to the
  \code{dfvals()} function of the robeth package. To see if this variable is 
  available in your R session, type  ls(all.names=TRUE). The parameters for psi, 
  chi and rho functions must also be set by a preliminary call to the \code{dfcomn} 
  function of the robeth package. 
}

\value{
  \item{lambda }{Final value of the location estimate }
  \item{nit }{Reached number of cycles}
  \item{sigmaf }{Final estimate of sigma}
  \item{rs }{The residual vector}
}
\references{
 Marazzi A. (1993), \emph{Algorithm, Routines, and S functions 
 for Robust Statistics}, Wadsworth & Brooks/cole, 
 Pacific Grove, California. p.30 and p.83 .
 }
\keyword{ robust }
\keyword{ univar }

