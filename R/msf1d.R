"msf1d" <-
function(a,b) {
if (missing(a)) messagena("a")
if (missing(b)) messagena("b")
n <- ncol(b)
nn <- length(a)
mdb <- nrow(b)
c <- double(nn)
f.res <- .Fortran("msf1d",
a=to.double(a),
b=to.double(b),
c=to.double(c),
n=to.integer(n),
nn=to.integer(nn),
mdb=to.integer(mdb))
list(c=f.res$c)
}

