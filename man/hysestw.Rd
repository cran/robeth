\name{hysestw}
\alias{hysestw}
\title{ Resampling algorithm for the computation of weighted S-estimates}
\description{
 See Marazzi A. (1993), p.216}
\usage{
hysestw(x, y, wgt, nq = np, iopt = .dFvGet()$ipt, intch = .dFvGet()$ich, 
        nrep, tols = .dFvGet()$tls, tolr = .dFvGet()$tlr, tau = .dFvGet()$tua, 
        gam = .dFvGet()$gma, maxit = .dFvGet()$mxt, maxs1 = .dFvGet()$msx, 
        maxs2 = .dFvGet()$mxs, expsi = psi, expsp = psp, exchi = chi, 
        iseed = .dFvGet()$isd)
}
\arguments{
  \item{x}{ See reference}
  \item{y}{ See reference}
  \item{wgt}{ See reference}
  \item{nq}{ See reference}
  \item{iopt}{ See reference}
  \item{intch}{ See reference}
  \item{nrep}{ See reference}
  \item{tols}{ See reference}
  \item{tolr}{ See reference}
  \item{tau}{ See reference}
  \item{gam}{ See reference}
  \item{maxit}{ See reference}
  \item{maxs1}{ See reference}
  \item{maxs2}{ See reference}
  \item{expsi}{ See reference}
  \item{expsp}{ See reference}
  \item{exchi}{ See reference}
  \item{iseed}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.216
}
\keyword{robust}

