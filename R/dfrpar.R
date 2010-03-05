"dfrpar" <-
function(x, etype, upar=-1, psipar=-1)
{
        if(missing(x))
                messagena("x")
        if(missing(etype))
                messagena("etype")
        np <- ncol(x)
        mdx <- nrow(x)
        n <- mdx
        tabl <- list(c("HUBER", "Huber", "huber", "HUB", "hub", "Hub"), c(
                "MAL-STD", "Mal-Std", "Mal-std", "mal-std"), c("KRA-WEL",
                "Kra-Wel", "kra-wel", "Kra-wel"), c("MAL-HAM", "Mal-Ham",
                "mal-ham", "Mal-ham"), c("OLS", "Ols", "ols"), c("LAR",
                "Lar", "lar"), c("HAM-KRA", "Ham-Kra", "Ham-kra", "ham-kra"),
                c("MAL-UNS", "Mal-Uns", "mal-uns", "Mal-uns"), c("MAL-TAU",
                "Mal-Tau", "mal-tau", "Mal-tau"), c("SCH-TAU","Sch-Tau",
                "sch-tau", "Sch-tau"), c("LMS", "Lms", "lms"),
                c("LTS", "Lts", "lts"), c("S", "s"),c('ROCKE1','Rocke1','rocke1'),
                c('ROCKE2','Rocke2','rocke2'))
        for(i in 1:15) {
                j <- match(etype, tabl[[i]], nomatch = 0)
                if (j == 0)
                        next
                rtype <- i
                break
        }
        if(j == 0) rtype <- 0
        itypw <- integer(1)
        itype <- integer(1)
        isigma <- integer(1)
        f.res <- .Fortran("rpardf",
                x = to.single(x),
                n = to.integer(n),
                np = to.integer(np),
                mdx = to.integer(mdx),
                rtype = to.integer(rtype),
                upar = to.single(upar),
                psipar = to.single(psipar),
                itypw = to.integer(itypw),
                itype = to.integer(itype),
                isigma = to.integer(isigma))
         .def <- .dFvGet()
        .def$itw <- f.res$itypw
        .def$ite <- f.res$itype
        .def$isg <- f.res$isigma
        z   <-  comval()
        if (z$c > 0) .def$ccc <- z$c else .def$ccc <- 1.345
        if (z$d > 0) .def$ddd <- z$d else .def$ddd <- .def$ccc

        .dFvSet(.def)

        list(itypw = .dFvGet()$itw, itype = .dFvGet()$ite, isigma = .dFvGet()$isg)
}
