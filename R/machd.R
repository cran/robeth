"machd" <-
function(i) {
if (missing(i)) messagena("i")
x <- double(1)
f.res <- .Fortran("machd",
i=to.integer(i),
x=to.double(x))
list(x=f.res$x)
}

