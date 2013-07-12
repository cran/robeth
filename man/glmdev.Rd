\name{glmdev}
\alias{glmdev}
\title{
The total deviance of the fitted generalizrd linear model
}
\description{
Computes the sum of the vector deviance and other intermediate results
}
\usage{
glmdev(y, ni, ci, wa, vtheta, offset = 0, icase = .dFvGet()$ics)
}
\arguments{
  \item{y}{The vector of observations
}
  \item{ni}{
The number of trial at xi in the binomial case (ics=2). Otherwise  ni=1 for each xi.
}
  \item{ci}{
The constants ci
}
  \item{wa}{
The vector of ai=b/|Axi|
}
  \item{vtheta}{
The vector of xi^T %*% theta
}
  \item{offset}{Optional offset added to the linear predictor. 
}
  \item{icase}{Set ics=1 for Bernoulli case, ics=2 for Binomial case and ics=3 for Poisson
     case
}
}

\value{
A list with the following components: 
  \item{dev }{2*sum_i abs(Li-Ti)}
  \item{thetas }{The estimates of theta_i}
  \item{li }{The values of  Li }
  \item{sc }{The alues of Ti}

}
\references{
Kuensch, H.R., Stefanski L.A., Carroll R.J. (1989).
Conditionally unbiased bounded-influence estimation in general regression models,
with application to generalized linear models. 
Journal of the American Statistical Association, 84, 460-466.

Marazzi, A. (1993).
Algorithms, Routines, and S-functions for robust Statistics.
Chapman and Hall, New York.

Marazzi A. (1997).
Object oriented S-plus functions for robust discrete generalized linear
models available in the doc folder of this package.
}

\keyword{stats }

