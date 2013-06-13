\name{gytstp}
\alias{gytstp}
\title{ Newton-type algorithm for the theta-step }
\description{
 See Marazzi A. (1993), p.295}

\usage{
gytstp(x, y, ci, theta, wa, cov, ni, oi = 0, gam = .dFvGet()$gma, 
       tol = .dFvGet()$tlo, tau = .dFvGet()$tua, iopt = .dFvGet()$ipo, 
       icase = .dFvGet()$ics, icnv = .dFvGet()$icn, maxit = .dFvGet()$mxt, 
       nitmon = .dFvGet()$ntm)
}
\arguments{
  \item{x}{ See reference}
  \item{y}{ See reference}
  \item{ci}{ See reference}
  \item{theta}{ See reference}
  \item{wa}{ See reference}
  \item{cov}{ See reference}
  \item{ni}{ See reference}
  \item{oi}{ See reference}
  \item{gam}{ See reference}
  \item{tol}{ See reference}
  \item{tau}{ See reference}
  \item{iopt}{ See reference}
  \item{icase}{ See reference}
  \item{icnv}{ See reference}
  \item{maxit}{ See reference}
  \item{nitmon}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.295
}
\keyword{robust}

