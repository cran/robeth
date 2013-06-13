\name{wygalg}
\alias{wygalg}
\title{ Conjugate gradient algorithm for the computation of
        the lower-triangular matrix A in the standardized case }
\description{
 See Marazzi A. (1993), p.127}
\usage{
wygalg(x, a, exu = ucv, exup = upcv, nobs = nrow(x), 
       maxit = .dFvGet()$mxg, nitmon = .dFvGet()$ntm, 
       icnv = .dFvGet()$icv, tol = .dFvGet()$tlo, 
       xfud = .dFvGet()$xfd)
}
\arguments{
  \item{x}{ See reference}
  \item{a}{ See reference}
  \item{exu}{ See reference}
  \item{exup}{ See reference}
  \item{nobs}{ See reference}
  \item{maxit}{ See reference}
  \item{nitmon}{ See reference}
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
California. p.127
}
\keyword{robust}

