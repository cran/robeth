\name{gymain}
\alias{gymain}
\title{ Main algorithm }
\description{
 See Marazzi A. (1993), p.304}
\usage{
gymain(x, y, ni, cov, a, theta, oi = 0, b = 1.1 * sqrt(np), 
       gam = .dFvGet()$gma, tau = .dFvGet()$tua, icase = .dFvGet()$ics, 
       iugl = .dFvGet()$iug, iopt = .dFvGet()$ipo, ialg = .dFvGet()$ilg, 
       icnvt = .dFvGet()$icn, icnva = .dFvGet()$icv, maxit = .dFvGet()$mxx, 
       maxtt = .dFvGet()$mxt, maxta = .dFvGet()$mxf, maxtc = .dFvGet()$mxt, 
       nitmnt = .dFvGet()$ntm, nitmna = .dFvGet()$ntm, tol = .dFvGet()$tlo, 
       tolt = .dFvGet()$tlo * 10, tola = .dFvGet()$tlo * 10, 
       tolc = .dFvGet()$tlo * 10)
}
\arguments{
  \item{x}{ See reference}
  \item{y}{ See reference}
  \item{ni}{ See reference}
  \item{cov}{ See reference}
  \item{a}{ See reference}
  \item{theta}{ See reference}
  \item{oi}{ See reference}
  \item{b}{ See reference}
  \item{gam}{ See reference}
  \item{tau}{ See reference}
  \item{icase}{ See reference}
  \item{iugl}{ See reference}
  \item{iopt}{ See reference}
  \item{ialg}{ See reference}
  \item{icnvt}{ See reference}
  \item{icnva}{ See reference}
  \item{maxit}{ See reference}
  \item{maxtt}{ See reference}
  \item{maxta}{ See reference}
  \item{maxtc}{ See reference}
  \item{nitmnt}{ See reference}
  \item{nitmna}{ See reference}
  \item{tol}{ See reference}
  \item{tolt}{ See reference}
  \item{tola}{ See reference}
  \item{tolc}{ See reference}
}
\value{
  See reference}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.304
}
\keyword{robust}

