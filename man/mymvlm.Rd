\name{mymvlm}
\alias{mymvlm}
\title{ Simultaneous computation of the MVE and LMS estimates }
\description{
 See Marazzi A. (1993), p.265}
\usage{
mymvlm(x, y, ilms = .dFvGet()$ilm, iopt = .dFvGet()$ipt, 
       intch = .dFvGet()$ich, nrep, tolv = .dFvGet()$tlv, 
       tolm = .dFvGet()$tlm, tau = .dFvGet()$tua, 
       iseed = .dFvGet()$isd)
}
\arguments{
  \item{x}{ See reference}
  \item{y}{ See reference}
  \item{ilms}{ See reference}
  \item{iopt}{ See reference}
  \item{intch}{ See reference}
  \item{nrep}{ See reference}
  \item{tolv}{ See reference}
  \item{tolm}{ See reference}
  \item{tau}{ See reference}
  \item{iseed}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.265
}
\keyword{robust}

