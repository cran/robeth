\name{rynalg}
\alias{rynalg}
\title{ Newton algorithm with adaptive steps for M-estimates}
\description{
 See Marazzi A. (1993), p.73}
\usage{
rynalg(x, y, theta, wgt, cov, expsi = psi, expsp = psp, exchi = chi, 
       exrho = rho, sigmai, gam = .dFvGet()$gma, tol = .dFvGet()$tlo, 
       tau = .dFvGet()$tua, itype = .dFvGet()$ite, iopt = .dFvGet()$iop, 
       isigma = .dFvGet()$isg, icnv = .dFvGet()$icn, maxit = .dFvGet()$mxt, 
       maxis = .dFvGet()$mxs, nitmon = .dFvGet()$ntm)
}
\arguments{
  \item{x}{ See reference}
  \item{y}{ See reference}
  \item{theta}{ See reference}
  \item{wgt}{ See reference}
  \item{cov}{ See reference}
  \item{expsi}{ See reference}
  \item{expsp}{ See reference}
  \item{exchi}{ See reference}
  \item{exrho}{ See reference}
  \item{sigmai}{ See reference}
  \item{gam}{ See reference}
  \item{tol}{ See reference}
  \item{tau}{ See reference}
  \item{itype}{ See reference}
  \item{iopt}{ See reference}
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
California. p.73
}
\keyword{robust}
