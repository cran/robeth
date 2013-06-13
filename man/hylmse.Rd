\name{hylmse}
\alias{hylmse}
\title{ Resampling algorithm for the computation of the LMS estimate }
\description{
 See Marazzi A. (1993), p.208}

\usage{
hylmse(x, y, nq = np, ik = .dFvGet()$ik1, iopt = .dFvGet()$ipt, 
       intch = .dFvGet()$ich, nrep, tol = .dFvGet()$tlo, tau = .dFvGet()$tua, 
       iseed = .dFvGet()$isd)
}
\arguments{
  \item{x}{ See reference}
  \item{y}{ See reference}
  \item{nq}{ See reference}
  \item{ik}{ See reference}
  \item{iopt}{ See reference}
  \item{intch}{ See reference}
  \item{nrep}{ See reference}
  \item{tol}{ See reference}
  \item{tau}{ See reference}
  \item{iseed}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.208
}
\keyword{robust}

