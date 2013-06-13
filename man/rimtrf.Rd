\name{rimtrf}
\alias{rimtrf}
\title{ Upper triangularization (QR-decomposition) of the design 
        matrix and determination of its pseudorank}
\description{
 See Marazzi A. (1993), p.64}
\usage{
rimtrf(x, n = nrow(x), intch = .dFvGet()$ith, tau = .dFvGet()$tua)
}
\arguments{
  \item{x}{ See reference}
  \item{n}{ See reference}
  \item{intch}{ See reference}
  \item{tau}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.64
}
\keyword{algebra}
