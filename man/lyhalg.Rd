\name{lyhalg}
\alias{lyhalg}
\title{ M-estimate of location with simultaneous estimation of scale }
\description{
 See Marazzi A. (1993), p.30}
\usage{
lyhalg(y, expsi = psi, expsp = psp, exchi = chi, theta, sigmai, 
       tol = .dFvGet()$tlo, gam = .dFvGet()$gma, isigma = .dFvGet()$isg, 
       maxit = .dFvGet()$mxt, maxis = .dFvGet()$mxs)
}
\arguments{
  \item{y}{ See reference}
  \item{expsi}{ See reference}
  \item{expsp}{ See reference}
  \item{exchi}{ See reference}
  \item{theta}{ See reference}
  \item{sigmai}{ See reference}
  \item{tol}{ See reference}
  \item{gam}{ See reference}
  \item{isigma}{ See reference}
  \item{maxit}{ See reference}
  \item{maxis}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.30
}
\keyword{robust}

