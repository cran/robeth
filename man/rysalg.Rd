\name{rysalg}
\alias{rysalg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ S-algorithm for M-estimates}
\description{
 See Marazzi A. (1993), p.87}
\usage{
rysalg(x, y, theta, wgt, cov, sigmai, tol = .dFvGet()$tlo, 
       tau = .dFvGet()$tua, itype = .dFvGet()$ite, 
       isigma = .dFvGet()$isg, icnv = .dFvGet()$icn, 
       maxit = .dFvGet()$mxt, maxis = .dFvGet()$mxs, 
       nitmon = .dFvGet()$ntm)
}
\arguments{
  \item{x}{ See reference}
  \item{y}{ See reference}
  \item{theta}{ See reference}
  \item{wgt}{ See reference}
  \item{cov}{ See reference}
  \item{sigmai}{ See reference}
  \item{tol}{ See reference}
  \item{tau}{ See reference}
  \item{itype}{ See reference}
  \item{isigma}{ See reference}
  \item{icnv}{ See reference}
  \item{maxit}{ See reference}
  \item{maxis}{ See reference}
  \item{nitmon}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.87
}
\keyword{robust}
