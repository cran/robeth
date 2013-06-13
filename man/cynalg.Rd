\name{cynalg}
\alias{cynalg}
\title{ Newton-type algorithm  for the computation of an 
        M-estimate of multivariate location and scatter  }
\description{
 See Marazzi A. (1993), p.235}
\usage{
cynalg(x, a, t, exu = ucv, exup = upcv, exv = vcv, exvp = vpcv, 
       exw = wcv, exwp = wpcv, nobs = nrow(x), maxit = .dFvGet()$mxn, 
       nitmon = .dFvGet()$ntm, iloc = .dFvGet()$ilc, icnv = .dFvGet()$icv, 
       tol = .dFvGet()$tlo, xfud = .dFvGet()$xfd)
}
\arguments{
  \item{x}{ See reference}
  \item{a}{ See reference}
  \item{t}{ See reference}
  \item{exu}{ See reference}
  \item{exup}{ See reference}
  \item{exv}{ See reference}
  \item{exvp}{ See reference}
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
California. p.235
}
\keyword{robust}

