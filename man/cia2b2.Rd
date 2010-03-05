\name{cia2b2}
\alias{cia2b2}
\title{Determination of the parameters a2 and b2 of the Huber weight function from the
        proportion eps of contamination}
\description{
 See Marazzi A. (1993), p.244}
\usage{
cia2b2(eps = .dFvGet()$esp, nvar, tol = .dFvGet()$tlo, maxit = .dFvGet()$mxt)
}
\arguments{
  \item{eps}{ See reference}
  \item{nvar}{ See reference}
  \item{tol}{ See reference}
  \item{maxit}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.244
}
\keyword{robust}

