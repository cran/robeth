\name{rysigm}
\alias{rysigm}
\title{ Iterative algorithm for the computation of an M-estimate
        of the scale parameter when the residuals are given}
\description{
 See Marazzi A. (1993), p.94}
\usage{
rysigm(rs, wgt, exchi = chi, sigmai, np, tol = .dFvGet()$tlo, 
       itype = .dFvGet()$ite, isigma = .dFvGet()$isg, 
       maxis = .dFvGet()$mxt)
}
\arguments{
  \item{rs}{ See reference}
  \item{wgt}{ See reference}
  \item{exchi}{ See reference}
  \item{sigmai}{ See reference}
  \item{np}{ See reference}
  \item{tol}{ See reference}
  \item{itype}{ See reference}
  \item{isigma}{ See reference}
  \item{maxis}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.94
}
\keyword{robust}
