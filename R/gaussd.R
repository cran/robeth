"gaussd" <-
function(kode=1,x) {
if (missing(x)) messagena("x")
p <- double(1)
f.res <- .Fortran("gaussd",
kode=to.integer(kode),
x=to.double(x),
p=to.double(p))
list(p=f.res$p)
}

