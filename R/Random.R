"Random" <-
function(iseed=.dFvGet()$isd) {
rn <- single(1)
f.res <- .Fortran("randow",
iseed=to.integer(iseed),
rn=to.single(rn))
zdf <- .dFvGet()
zdf$isd <- f.res$iseed

.dFvSet(zdf)
f.res$rn
}
