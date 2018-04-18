\name{lrfctd}
\alias{lrfctd}

\title{Computation of Li, li and lip 
}
\description{
 See Marazzi A. (1993), p.282-286 and p.297-298
}
\usage{
lrfctd(icase, y, ci, vtheta, offset, wa, ni, i0, i1, i2)
}
\arguments{
  \item{icase}{ Integer: 1 for Bernouilli, 2 for binomial and 3 for Poisson.}
  \item{y}{ The y vector.}
  \item{ci}{ The c_i vector.}
  \item{vtheta}{ The x by theta vector.}
  \item{offset}{ The offset vector.}
  \item{wa}{ The a_i vector.}
  \item{ni}{ The integer n_i vector.}
  \item{i0}{ Integer: 1 to compute Li otherwise 0.}
  \item{i1}{ Integer: 1 to compute li otherwise 0.}
  \item{i2}{ Integer: 1 to compute lip otherwise 0.}
}
\value{
  List with the following components :
  \item{f0}{ NULL if i0=0 else Li.}
  \item{i1}{ NULL if i1=0 else li , derivative of Li.}
  \item{i2}{ NULL if i2=0 else lip, derivative of li.}
  \item{sf0}{ NULL if i0=0 else sum of the Li components.}
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.282-286 and p.297-298
}
\keyword{ robust }


