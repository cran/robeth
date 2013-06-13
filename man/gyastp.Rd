\name{gyastp}
\alias{gyastp}
\title{ Fixed-point algorithm for the A-step }
\description{
 See Marazzi A. (1993), p.301}
\usage{
gyastp(x, y, ni, vtheta, ci, a, oi = 0, b = 1.1 * sqrt(nvar), 
       iugl = .dFvGet()$iug, icase = .dFvGet()$ics, tau = .dFvGet()$tua, 
       maxit = .dFvGet()$mxf, nitmon = .dFvGet()$ntm, icnv = .dFvGet()$icv, 
       tol = .dFvGet()$tlo)
}
\arguments{
  \item{x}{ See reference}
  \item{y}{ See reference}
  \item{ni}{ See reference}
  \item{vtheta}{ See reference}
  \item{ci}{ See reference}
  \item{a}{ See reference}
  \item{oi}{ See reference}
  \item{b}{ See reference}
  \item{iugl}{ See reference}
  \item{icase}{ See reference}
  \item{tau}{ See reference}
  \item{maxit}{ See reference}
  \item{nitmon}{ See reference}
  \item{icnv}{ See reference}
  \item{tol}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.301
}
\keyword{robust}

