\name{kiascv}
\alias{kiascv}
\title{ Covariance matrix of the coefficient estimates of the form 
        f.inv(XT X) in the transformed coordinate system}
\description{
 See Marazzi A. (1993), p.150}
\usage{
kiascv(xt, k = np, mdx = nrow(xt), fu = .dFvGet()$fu1, fb = .dFvGet()$fb1)
}
\arguments{
  \item{xt}{ See reference}
  \item{k}{ See reference}
  \item{mdx}{ See reference}
  \item{fu}{ See reference}
  \item{fb}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.150
}
\keyword{robust}

