"chisq" <-
function(kode=1,ifn,x) {
if (missing(ifn)) messagena("ifn")
if (missing(x)) messagena("x")
p <- single(1)
f.res <- .Fortran("chisqz",
kode=to.integer(kode),
ifn=to.integer(ifn),
x=to.single(x),
p=to.single(p))
list(p=f.res$p)
}

