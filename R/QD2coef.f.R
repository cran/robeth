"QD2coef.f" <- function(lambda,yc,delta,muI,sigmaI,zero=1e-4) {
n   <- length(yc); P   <- ppoints(n)
qn  <- rep(0,n) 
tol <- 0.001; maxit <- 20
f.res <- .Fortran("qd2func",
lambda=to.double(lambda),yc=to.double(yc),delta=to.single(delta),n=to.integer(n),mui=to.double(muI),
sigmai=to.double(sigmaI),zero=to.double(zero),tol=to.double(tol),
maxit=to.integer(maxit),p=to.double(P),qn=to.double(qn))
ql   <- qloggamma(P,lambda)
z    <- lsfit(ql,f.res$qn)
mu   <- as.numeric(z$coef[1])
sig  <- as.numeric(z$coef[2])
list(lambda=lambda,mu=mu,sig=sig)}

