\name{rimtrd}
\alias{rimtrd}
\title{Double precision version of RIMTRF}
\description{
 See Marazzi A. (1993), p.64}
\usage{
rimtrd(x, n = nrow(x), intch = .dFvGet()$ith, tau = .dFvGet()$tua)
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

\seealso{\code{\link{rimtrf}}}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.64
}
\keyword{algebra}
