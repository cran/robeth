\name{wyfalg}
\alias{wyfalg}
\title{ Fixed-point algorithm for the computation of the matrix A }
\description{
 See Marazzi A. (1993), p.121}
\usage{
wyfalg(x, a, gwt, exu = ucv, nobs = nrow(x), nvarq = 0, 
       tau = .dFvGet()$tua, maxit = .dFvGet()$mxf, nitmon = .dFvGet()$ntm, 
       icnv = .dFvGet()$icv, itypw = .dFvGet()$itw, igwt = 0, 
       tol = .dFvGet()$tlo)
}
\arguments{
  \item{x}{ See reference}
  \item{a}{ See reference}
  \item{gwt}{ See reference}
  \item{exu}{ See reference}
  \item{nobs}{ See reference}
  \item{nvarq}{ See reference}
  \item{tau}{ See reference}
  \item{maxit}{ See reference}
  \item{nitmon}{ See reference}
  \item{icnv}{ See reference}
  \item{itypw}{ See reference}
  \item{igwt}{ See reference}
  \item{tol}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.121
}
\keyword{robust}

