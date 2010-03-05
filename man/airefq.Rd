\name{airefq}
\alias{airefq}
\title{ Asymptotic relative efficiency of a general M-estimate for a model 
        with mu quantitative and nu qualitative covariates}
                       
\description{
 See Marazzi A. (1993), p.170}
\usage{
airefq(t, expsi = psi, exu = ucv, exw = www, itype = .dFvGet()$ite, mu, sigmx = 1, upper = .dFvGet()$upr, til = .dFvGet()$tli, tau = .dFvGet()$tua, nobs = nrow(t), maxit = .dFvGet()$mxe, tol = .dFvGet()$tlo, init = .dFvGet()$ini, nitmon = .dFvGet()$ntm)
}
\arguments{
  \item{t}{ See reference}
  \item{expsi}{ See reference}
  \item{exu}{ See reference}
  \item{exw}{ See reference}
  \item{itype}{ See reference}
  \item{mu}{ See reference}
  \item{sigmx}{ See reference}
  \item{upper}{ See reference}
  \item{til}{ See reference}
  \item{tau}{ See reference}
  \item{nobs}{ See reference}
  \item{maxit}{ See reference}
  \item{tol}{ See reference}
  \item{init}{ See reference}
  \item{nitmon}{ See reference}
}
\value{
  See reference
}
\references{
  Marazzi A. (1993) \emph{Algorithm, Routines, and S functions 
for Robust Statistics}. Wadsworth & Brooks/cole, Pacific Grove,
California. p.170
}
\keyword{robust}

