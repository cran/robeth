"mtt2d" <-
function(a,n) {
if (missing(a)) messagena("a")
if (missing(n)) messagena("n")
nn <- length(a)
b <- double(nn)
f.res <- .Fortran("mtt2d",
a=to.double(a),
b=to.double(b),
n=to.integer(n),
nn=to.integer(nn))
list(b=f.res$b)
}

