"xsyd" <-
function(x,y,s) {
if (missing(x)) messagena("x")
if (missing(y)) messagena("y")
if (missing(s)) messagena("s")
n <- length(x)
nn <- length(s)
result <- double(1)
f.res <- .Fortran("xsyd",
x=to.double(x),
y=to.double(y),
s=to.double(s),
n=to.integer(n),
nn=to.integer(nn),
result=to.double(result))
list(result=f.res$result)
}

