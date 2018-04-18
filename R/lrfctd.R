lrfctd <- function(icase,y,ci,vtheta,offset,wa,ni,i0,i1,i2)
{
   n <- length(y)
   if (length(offset)==1) offset <- rep(0,n) 
   f.res <- .Fortran("lrfctd",
             icase = to.integer(icase),
             y = to.single(y),
             ci = to.single(ci),
             vtheta = to.single(vtheta),
             oi = to.single(offset),
             wa = to.single(wa),
             ni = to.integer(ni),
             n = to.integer(n),
             i0 = to.integer(i0),
             i1 = to.integer(i1),
             i2 = to.integer(i2),
             f0 = double(n),
             f1 = double(n),
             f2 = double(n),
             sf0 = double(1))
  if (i0==0) f0 <- NULL else f0 <- f.res$f0
  if (i1==0) f1 <- NULL else f1 <- f.res$f1
  if (i2==0) f2 <- NULL else f2 <- f.res$f2
  if (i0==0) sf0 <- NULL else sf0 <- f.res$f0
  list(f0=f0, f1=f1, f2=f2, sf0=sf0)}


