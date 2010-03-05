\name{cyfalg}
\alias{cyfalg}
\title{ Fixed-point algorithm for the computation of an
        M-estimate of multivariate location and scatter }
\description{
 See Marazzi A. (1993), p.232}
\usage{
cyfalg(x, a, t, exu = ucv, exv = vcv, exw = wcv, nobs = nrow(x), tau = .dFvGet()$tua, maxit = .dFvGet()$mxf, nitmon = .dFvGet()$ntm, iloc = .dFvGet()$ilc, icnv = .dFvGet()$icv, tol = .dFvGet()$tlo)
}
\arguments{
  \item{x}{ See reference}
  \item{a}{ See reference}
  \item{t}{ See reference}
  \item{exu}{ See reference}
  \item{exv}{ See reference}
  \item{exw}{ See reference}
  \item{nobs}{ See reference}
  \item{tau}{ See reference}
  \item{maxit}{ See reference}
  \item{nitmon}{ See reference}
  \item{iloc}{ See reference}
  \item{icnv}{ See reference}
  \item{tol}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.232
}
\keyword{robust}

