\name{cirock}
\alias{cirock}
\title{Initial values for the Rocke estimates of covariance}
\description{
 See Marazzi A. (1993), p.223}

\usage{
cirock(nvar, em = .dFvGet()$em, cr = .dFvGet()$cr, iopt = 1)
}
\arguments{
  \item{nvar}{ See the description of \code{nvar} as indicated above }
  \item{em}{ See the description of \code{em} as indicated above }
  \item{cr}{ See the description of \code{cr} as indicated above }
  \item{iopt}{ See the description of \code{iopt} as indicated above }
}

\references{\url{http://www.stat.ubc.ca/research/techreports/176html/node11.html}
- Marazzi A. (1993), \emph{Algorithm, Routines, and S functions 
for Robust Statistics}, Wadsworth & Brooks/cole, Pacific Grove,
California, p.223
}
\author{Rocke and Downs (1981)}

\keyword{robust}

