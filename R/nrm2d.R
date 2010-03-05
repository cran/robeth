"nrm2d" <-
function(x,n=nrow(x),incx=1) {
if (missing(x)) messagena("x")
mdx <- length(x)
xnrm <- double(1)
f.res <- .Fortran("nrm2d",
x=to.double(x),
n=to.integer(n),
incx=to.integer(incx),
mdx=to.integer(mdx),
xnrm=to.double(xnrm))
list(xnrm=f.res$xnrm)
}

