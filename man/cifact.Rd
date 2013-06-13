\name{cifact}
\alias{cifact}
\title{ 
Determination of the correction factor for the M-estimate based on Huber 
weight function }
\description{
 See Marazzi A. (1993), p.246}
\usage{
cifact(a2 = .dFvGet()$aa2, b2 = .dFvGet()$bb2, nvar, tol = .dFvGet()$tlo, 
       maxit = .dFvGet()$mxt)
}
\arguments{
  \item{a2}{ See reference}
  \item{b2}{ See reference}
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
California. p.246
}
\keyword{robust}

