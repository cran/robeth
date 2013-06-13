\name{rywalg}
\alias{rywalg}
\title{ W-algorithm for M-estimates }
\description{
 See Marazzi A. (1993), p.87}
\usage{
rywalg(x, y, theta, wgt, cov, psp0 = psp(0), expsi = psi, exchi = chi, 
       exrho = rho, sigmai, tol = .dFvGet()$tlo, gam = .dFvGet()$gma, 
       tau = .dFvGet()$tua, itype = .dFvGet()$ite, isigma = .dFvGet()$isg, 
       icnv = .dFvGet()$icn, maxit = .dFvGet()$mxt, maxis = .dFvGet()$mxs, 
       nitmon = .dFvGet()$ntm)
}
\arguments{
  \item{x}{ See reference}
  \item{y}{ See reference}
  \item{theta}{ See reference}
  \item{wgt}{ See reference}
  \item{cov}{ See reference}
  \item{psp0}{ See reference}
  \item{expsi}{ See reference}
  \item{exchi}{ See reference}
  \item{exrho}{ See reference}
  \item{sigmai}{ See reference}
  \item{tol}{ See reference}
  \item{gam}{ See reference}
  \item{tau}{ See reference}
  \item{itype}{ See reference}
  \item{isigma}{ See reference}
  \item{icnv}{ See reference}
  \item{maxit}{ See reference}
  \item{maxis}{ See reference}
  \item{nitmon}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.87
}
\keyword{robust}
