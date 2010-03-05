\name{ktaskw}
\alias{ktaskw}
\title{ Covariance matrix of the coefficient estimates of the 
        form f.inv(S1) S2 inv(S1) }
\description{
 See Marazzi A. (1993), p.148}
\usage{
ktaskw(x, d, e, tau = .dFvGet()$tua, ia = .dFvGet()$ia1, f = .dFvGet()$fff, 
       f1 = .dFvGet()$ff1, iainv = .dFvGet()$ia2, a)
}
\arguments{
  \item{x}{ See reference}
  \item{d}{ See reference}
  \item{e}{ See reference}
  \item{tau}{ See reference}
  \item{ia}{ See reference}
  \item{f}{ See reference}
  \item{f1}{ See reference}
  \item{iainv}{ See reference}
  \item{a}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.148
}
\keyword{robust}

