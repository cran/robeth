"mtt3d" <-
function(a,b,n) {
if (missing(a)) messagena("a")
if (missing(b)) messagena("b")
if (missing(n)) messagena("n")
nn <- length(a)
c <- double(nn)
f.res <- .Fortran("mtt3d",
a=to.double(a),
b=to.double(b),
c=to.double(c),
n=to.integer(n),
nn=to.integer(nn))
list(c=f.res$c)
}

