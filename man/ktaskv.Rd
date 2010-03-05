\name{ktaskv}
\alias{ktaskv}
\title{ Covariance matrix of the coefficient estimates of the
        form f.inv(XT X) }
\description{
 See Marazzi A. (1993), p.147}
\usage{
ktaskv(x, n = nrow(x), tau = .dFvGet()$tua, f = .dFvGet()$fff)
}
\arguments{
  \item{x}{ See reference}
  \item{n}{ See reference}
  \item{tau}{ See reference}
  \item{f}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.147
}
\keyword{robust}

