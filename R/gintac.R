"gintac" <-
function(x,y,ni,oi=0,icase=.dFvGet()$ics,maxtt=.dFvGet()$mxt,maxta=.dFvGet()$mxf,
tolt=.dFvGet()$tlo,tola=.dFvGet()$tlo,b=1.1*sqrt(np),c=1.345) {
if (missing(x)) messagena("x")
if (missing(y)) messagena("y")
if (missing(ni)) messagena("ni")
mdx <- nrow(x)
n <- length(y)
np <- ncol(x)
mdt <- n
ncov <- np*(np+1)/2
nitt <- integer(1)
nita <- integer(1)
sigma <- single(1)
a <- double(ncov)
if (length(oi)==1) oi <- rep(0,n)
theta <- single(mdt)
ci <- single(n)
dist <- single(n)
f.res <- .Fortran("gintacz",
x=to.single(x),
y=to.single(y),
ni=to.integer(ni),
oi=to.single(oi),
mdx=to.integer(mdx),
mdt=to.integer(mdt),
n=to.integer(n),
np=to.integer(np),
ncov=to.integer(ncov),
icase=to.integer(icase),
maxtt=to.integer(maxtt),
maxta=to.integer(maxta),
tolt=to.single(tolt),
tola=to.single(tola),
b=to.single(b),
c=to.single(c),
nitt=to.integer(nitt),
nita=to.integer(nita),
sigma=to.single(sigma),
a=as.double(a),
theta=to.single(theta),
ci=to.single(ci),
dist=to.single(dist))
list(nitt=f.res$nitt,nita=f.res$nita,sigma=f.res$sigma,a=f.res$a,
theta=f.res$theta,ci=f.res$ci,dist=f.res$dist)
}
