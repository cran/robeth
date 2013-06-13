\name{mirtsr}
\alias{mirtsr}
\title{Computation of (robust) t-statistics for t-directed search}
\description{
 See Marazzi A. (1993), p.262}
\usage{
mirtsr(x, y, itype = .dFvGet()$ite, c = .dFvGet()$ccc, 
       d = .dFvGet()$ddd, tol = .dFvGet()$tlo, 
       gam = .dFvGet()$gma,  maxit = .dFvGet()$mxt, 
       maxis = .dFvGet()$mxs, tau = .dFvGet()$tua)
}
\arguments{
  \item{x}{ See reference}
  \item{y}{ See reference}
  \item{itype}{ See reference}
  \item{c}{ See reference}
  \item{d}{ See reference}
  \item{tol}{ See reference}
  \item{gam}{ See reference}
  \item{maxit}{ See reference}
  \item{maxis}{ See reference}
  \item{tau}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.262
}
\keyword{robust}

