"Qn.Exp.f" <- function(p,yc,delta,mu,sigma,lambda,zero=1e-4) {
n  <-length(yc)
a <- -10; b <- 6; tol <- 0.001; maxit <- 20
f.res <- .Fortran("qnexp",
p=to.double(p),yc=to.double(yc),delta=to.single(delta),n=to.integer(n),mu=to.double(mu),
sigma=to.double(sigma),lambda=to.double(lambda),zero=to.double(zero),a=to.double(a),
b=to.double(b),tol=to.double(tol),maxit=to.integer(maxit),qj=double(1),itr=integer(1),
iterm=integer(1))
f.res$qj
}


