"precd" <-
function() {
prec <- double(1)
f.res <- .Fortran("precdz",
prec=as.double(prec))
list(prec=f.res$prec)
}

