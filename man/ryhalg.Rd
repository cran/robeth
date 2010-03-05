\name{ryhalg}
\alias{ryhalg}
\title{ H-algorithm for M-estimates }
\description{
 See Marazzi A. (1993), p.98}
\usage{
ryhalg(x, y, theta, wgt, cov, expsi = psi, exchi = chi, exrho = rho, 
       sigmai, k = np, tol = .dFvGet()$tlo, gam = .dFvGet()$gma, 
       tau = .dFvGet()$tua, itype = .dFvGet()$ite, ix = .dFvGet()$ix1, 
       iy = .dFvGet()$iy1, ic = .dFvGet()$ic1, isigma = .dFvGet()$isg, 
       icnv = .dFvGet()$icn, maxit = .dFvGet()$mxt, maxis = .dFvGet()$mxs, 
       nitmon = .dFvGet()$ntm, sf, sg, sh, ip)
}
\arguments{
  \item{x}{ See reference}
  \item{y}{ See reference}
  \item{theta}{ See reference}
  \item{wgt}{ See reference}
  \item{cov}{ See reference}
  \item{expsi}{ See reference}
  \item{exchi}{ See reference}
  \item{exrho}{ See reference}
  \item{sigmai}{ See reference}
  \item{k}{ See reference}
  \item{tol}{ See reference}
  \item{gam}{ See reference}
  \item{tau}{ See reference}
  \item{itype}{ See reference}
  \item{ix}{ See reference}
  \item{iy}{ See reference}
  \item{ic}{ See reference}
  \item{isigma}{ See reference}
  \item{icnv}{ See reference}
  \item{maxit}{ See reference}
  \item{maxis}{ See reference}
  \item{nitmon}{ See reference}
  \item{sf}{ See reference}
  \item{sg}{ See reference}
  \item{sh}{ See reference}
  \item{ip}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.98
}
\keyword{robust}
