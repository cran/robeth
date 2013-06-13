\name{gintac}
\alias{gintac}
\title{ Initial values of theta, A and c_i,...,c_n }
\description{
 See Marazzi A. (1993), p.292}
\usage{
gintac(x, y, ni, oi = 0, icase = .dFvGet()$ics, maxtt = .dFvGet()$mxt, 
       maxta = .dFvGet()$mxf, tolt = .dFvGet()$tlo, tola = .dFvGet()$tlo, 
       b = 1.1 * sqrt(np), c = 1.345)
}
\arguments{
  \item{x}{ See reference}
  \item{y}{ See reference}
  \item{ni}{ See reference}
  \item{oi}{ See reference}
  \item{icase}{ See reference}
  \item{maxtt}{ See reference}
  \item{maxta}{ See reference}
  \item{tolt}{ See reference}
  \item{tola}{ See reference}
  \item{b}{ See reference}
  \item{c}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.292
}
\keyword{robust}

