\name{wyfcol}
\alias{wyfcol}
\title{Modified fixed-point algorithm for collinear 
       data in the standardized case }
\description{
 See Marazzi A. (1993), p.87}
\usage{
wyfcol(x, exu = ucv, nobs = nrow(x), iwgt = .dFvGet()$iwg, 
       apar = .dFvGet()$apr, tau = .dFvGet()$tua, 
       tol = .dFvGet()$tlo, maxit = .dFvGet()$mxf, 
       nitmon = .dFvGet()$ntm, icnv = .dFvGet()$icv)
}
\arguments{
  \item{x}{ See reference}
  \item{exu}{ See reference}
  \item{nobs}{ See reference}
  \item{iwgt}{ See reference}
  \item{apar}{ See reference}
  \item{tau}{ See reference}
  \item{tol}{ See reference}
  \item{maxit}{ See reference}
  \item{nitmon}{ See reference}
  \item{icnv}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.8
}
\keyword{robust}

