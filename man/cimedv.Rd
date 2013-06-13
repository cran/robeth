\name{cimedv}
\alias{cimedv}
\title{Initial values for the iterative algorithms implemented in 
       CYFALG, CYNALG, and CYGALG }
\description{
 See Marazzi A. (1993), p.230}
\usage{
cimedv(x, nobs = nrow(x), nfirst = nobs, iloc = .dFvGet()$ilc, t)
}
\arguments{
  \item{x}{ See reference}
  \item{nobs}{ See reference}
  \item{nfirst}{ See reference}
  \item{iloc}{ See reference}
  \item{t}{ See reference}
}

\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.230
}
\keyword{robust}

