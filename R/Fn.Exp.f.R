"Fn.Exp.f" <- function(z,y,delta,mu,sigma,lambda,zero=1e-4) {
n  <-length(y)
f.res <- .Fortran("fnexp",
z=to.double(z),y=to.double(y),delta=to.single(delta),n=to.integer(n),mu=to.double(mu),
sigma=to.double(sigma),lambda=to.double(lambda),zero=to.double(zero),res=double(1))
f.res$res
}


