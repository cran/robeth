\name{cygalg}
\alias{cygalg}
\title{ Conjugate gradient algorithm  for the computation of an
        M-estimate of multivariate location and scatter }
\description{
 See Marazzi A. (1993), p.238}
\usage{
cygalg(x, a, t, exu = ucv, exup = upcv, exv = vcv, exw = wcv, exwp = wpcv, nobs = nrow(x), maxit = .dFvGet()$mxg, nitmon = .dFvGet()$ntm, iloc = .dFvGet()$ilc, icnv = .dFvGet()$icv, tol = .dFvGet()$tlo, xfud = .dFvGet()$xfd)
}
\arguments{
  \item{x}{ See reference}
  \item{a}{ See reference}
  \item{t}{ See reference}
  \item{exu}{ See reference}
  \item{exup}{ See reference}
  \item{exv}{ See reference}
  \item{exw}{ See reference}
  \item{exwp}{ See reference}
  \item{nobs}{ See reference}
  \item{maxit}{ See reference}
  \item{nitmon}{ See reference}
  \item{iloc}{ See reference}
  \item{icnv}{ See reference}
  \item{tol}{ See reference}
  \item{xfud}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.238
}
\keyword{robust}

