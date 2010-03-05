"cerfd" <-
function(x) {
if (missing(x)) messagena("x")
f <- double(1)
f.res <- .Fortran("cerfd",
x=to.double(x),
f=to.double(f))
list(f=f.res$f)
}

