"mach" <-
function(i) {
if (missing(i)) messagena("i")
x <- single(1)
f.res <- .Fortran("machz",
i=to.integer(i),
x=to.single(x))
list(x=f.res$x)
}

