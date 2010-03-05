\name{rybifr}
\alias{rybifr}
\title{ Bounded influence regression }
\description{
 See Marazzi A. (1993), p.410}
\usage{
rybifr(x, y, np, nthet = np + 1, itype = 2, icoll = 0, isigma = 1, 
       ch = 1.345, ck = 1.05 * sqrt(nthet), bm = 1.05 * sqrt(nthet), 
       tol = 0.001, tau = 1e-06, maxitt = 50, maxitw = 80)
}
\arguments{
  \item{x}{ See reference}
  \item{y}{ See reference}
  \item{np}{ See reference}
  \item{nthet}{ See reference}
  \item{itype}{ See reference}
  \item{icoll}{ See reference}
  \item{isigma}{ See reference}
  \item{ch}{ See reference}
  \item{ck}{ See reference}
  \item{bm}{ See reference}
  \item{tol}{ See reference}
  \item{tau}{ See reference}
  \item{maxitt}{ See reference}
  \item{maxitw}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.410
}
\keyword{robust}
