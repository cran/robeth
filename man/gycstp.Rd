\name{gycstp}
\alias{gycstp}
\title{ Newton-type algorithm for the c-step}
\description{
 See Marazzi A. (1993), p.299}
\usage{
gycstp(icase = .dFvGet()$ics, ialg = .dFvGet()$ilg, ni, a, e, 
        tol = .dFvGet()$tlo, maxit = .dFvGet()$mxt, t)
}
\arguments{
  \item{icase}{ See reference}
  \item{ialg}{ See reference}
  \item{ni}{ See reference}
  \item{a}{ See reference}
  \item{e}{ See reference}
  \item{tol}{ See reference}
  \item{maxit}{ See reference}
  \item{t}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.299
}
\keyword{robust}

