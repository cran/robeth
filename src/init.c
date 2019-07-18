#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* How was this made?   AR 04/2018
Within the top level of the package source:
tools::package_native_routine_registration_skeleton('c:/data/R/R-3.4.3/library/robeth',,,FALSE)
Copy all text results to file init.c

When the command : Rcmd check --as-cran robeth is executed,
the line : useDynLib(robeth, .registration=TRUE)
in the NAMESPACE file, displays a warning for each exported R function such as:

 libeth <- function(d) {
    f.res <- .Fortran("libeth", d = to.single(d), bta = single(1))
    list(bta = f.res$bta)
 }

The warning is : libeth symbol is already in use in the table.
The only solution is to rename  the Fortran subroutine libeth. The R team proposal 
is to add a prefix but we apply a suffix "z" and the libeth function becomes: 

 libeth <- function(d) {
    f.res <- .Fortran("libethz", d = to.single(d), bta = single(1))
    list(bta = f.res$bta)
 }
   
*/

/* .Fortran calls */
extern void F77_NAME(addcol)(float*, int*, int*, int*, int*, int*, float*, int*, float*);
extern void F77_NAME(chia)(int*, float*, float*);
extern void F77_NAME(dbinom)(float*, float*, float*, float*, int*, float*, float*, int*, float*, float*);
extern void F77_NAME(fnexp)(double*, double*, float*, int*, double*, double*, double*, double*, double*);

extern void F77_NAME(binprdz)(int*, int*, float*, float*, float*);
extern void F77_NAME(cerfz)(float*, float*);
extern void F77_NAME(cerfzd)(double*, double*);
extern void F77_NAME(cfrcovz)(double*, int*, int*, float*, float*, float*, float*); 
extern void F77_NAME(chisqz)(int*, int*, float*, float*);
extern void F77_NAME(cia2b2z)(float*, int*, float*, int*, float*, float*);
extern void F77_NAME(cibeatz)(float*, float*, int*, float*);
extern void F77_NAME(ciclocz)(float*, float*, float*);
extern void F77_NAME(cifactz)(float*, float*, int*, float*, int*, float*);
extern void F77_NAME(cimedvz)(float*, int*, int*, int*, int*, int*, int*, double*, float*, float*);
extern void F77_NAME(cirockz)(float*, float*, int*, int*, float*);
extern void F77_NAME(comvalz)(int*, float*, float*, float*, float*, float*, float*, float*, float*, int*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, int*, float*, float*, int*);
extern void F77_NAME(cquantz)(float*, int*, float*, int*, float*);
extern void F77_NAME(dfcomnz)(int*, float*, float*, float*, float*, float*, float*, float*, float*, int*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, int*, float*, float*, int*);
extern void F77_NAME(dotpz)(float*, float*, int*, int*, int*, int*, int*, float*);
extern void F77_NAME(dotpzd)(double*, double*, int*, int*, int*, int*, int*, double*);
extern void F77_NAME(dpoissz)(float*, float*, float*, float*, float*, float*, int*, float*, float*);
extern void F77_NAME(exchz)(float*, int*, int*, int*, int*);
extern void F77_NAME(exchzd)(double*, int*, int*, int*, int*);
extern void F77_NAME(fcumz)(int*, int*, float*, float*, int*);
extern void F77_NAME(fstordz)(float*, int*, int*, float*);
extern void F77_NAME(gaussz)(int*, float*, float*);
extern void F77_NAME(gausszd)(int*, double*, double*);
extern void F77_NAME(gfedcaz)(float*, float*, float*, int*, float*, int*, int*, float*, float*);
extern void F77_NAME(gicstpz)(int*, int*, int*, float*, float*, float*, int*, float*, int*, float*);
extern void F77_NAME(gintacz)(float*, float*, int*, float*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, int*, int*, float*, double*, float*, float*, float*);
extern void F77_NAME(glmdevz)(float*, int*, float*, float*, float*, float*, int*, int*, double*, double*, double*, double*);
extern void F77_NAME(gyastpz)(float*, float*, int*, float*, float*, double*, float*, float*, int*, int*, int*, int*, int*, int*, float*, int*, int*, int*, float*, int*, float*, double*, double*, double*, double*);
extern void F77_NAME(gycstpz)(int*, int*, int*, float*, float*, float*, int*, float*);
extern void F77_NAME(gymainz)(float*, float*, int*, float*, double*, float*, float*, int*, int*, int*, int*, float*, float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, int*, float*, float*, float*, float*, float*, float*, float*, float*, int*, double*);
extern void F77_NAME(gytstpz)(float*, float*, float*, float*, float*, float*, int*, float*, int*, int*, int*, int*, float*, float*, float*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, int*);
extern void F77_NAME(h12z)(int*, int*, int*, int*, float*, int*,float*, float*, int*, int*, int*, int*);
extern void F77_NAME(h12zd)(int*, int*, int*, int*, double*, int*, double*, double*, int*, int*, int*, int*);
extern void F77_NAME(hylmsez)(float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, int*, int*, float*, float*, float*, int*, float*, int*);
extern void F77_NAME(hyltsez)(float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, int*, int*, float*, float*, float*, int*, float*, int*);
extern void F77_NAME(ingamaz)(float*, float*, float*);

extern void F77_NAME(int0)(int*, int*, int*, int*, int*, int*, float*, float*, float*, int*, float*, int*, float*, float*, float*);
extern void F77_NAME(int10)(float*, double*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, int*, float*, double*, double*, double*, double*, double*, double*);
extern void F77_NAME(int16)(float*, double*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, int*, float*, double*, double*, float*, double*, double*, double*, double*, double*, double*, double*);
extern void F77_NAME(int21)(float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, int*, float*, float*, int*);
extern void F77_NAME(int21w)(float*, float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, int*, float*, float*, int*);
extern void F77_NAME(int24)(float*, int*, int*, int*, float*, float*, float*, float*, float*);
extern void F77_NAME(int25)(float*, int*, int*, int*, int*, float*, float*);
extern void F77_NAME(int27)(float*, float*, int*, int*, int*, float*, int*, float*, float*);
extern void F77_NAME(int29)(float*, float*, int*, int*, int*, float*, int*, float*, float*);
extern void F77_NAME(int3)(float*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, int*, int*, int*, int*, int*, float*, int*, int*, int*, float*, float*, double*, double*, double*, double*, double*, float*, float*, float*, float*, float*);
extern void F77_NAME(int31)(int*, float*, float*, float*, float*);
extern void F77_NAME(int32)(int*, float*, float*, float*, float*, float*);
extern void F77_NAME(int33)(float*, int*, int*, int*, float*, float*, int*, float*, float*, int*, int*, int*, int*, float*, float*, float*, float*);
extern void F77_NAME(int36)(float*, int*, int*, int*, int*, int*, int*, int*, float*, float*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, int*);
extern void F77_NAME(int40)(float*, int*, int*, int*, float*, float*, float*, float*);
extern void F77_NAME(int41)(float*, float*, float*, float*, float*, int*, int*, int*, float*, int*, int*, int*, int*, int*, int*, float*, float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*, float*, float*, int*);
extern void F77_NAME(int44)(float*, float*, float*, float*, float*, float*, int*, int*, int*, float*, int*, int*, int*, int*, int*, float*, float*, float*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*, int*, float*, float*);
extern void F77_NAME(int47)(float*, float*, float*, float*, float*, int*, int*, int*, int*, float*, int*, int*, int*, int*, int*, float*, float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, int*, float*);
extern void F77_NAME(int51)(float*, float*, int*, float*, int*, int*, float*, int*, int*, int*, int*, float*, float*, float*);
extern void F77_NAME(int52)(float*, float*, float*, int*, int*, int*, int*, float*, int*, float*, float*, float*);
extern void F77_NAME(int53)(float*, double*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, int*, float*, double*, double*, double*, double*, double*, double*);
extern void F77_NAME(int55)(float*, double*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, int*, float*, double*, double*, float*, double*, double*, double*, double*, double*, double*);
extern void F77_NAME(int57)(float*, double*, float*, int*, int*, int*, int*, int*, int*, float*, int*, int*, int*, int*, int*, float*, int*, float*, double*, double*, double*, double*, double*);
extern void F77_NAME(int58)(double*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, int*, int*, int*, int*, int*, float*, double*, double*, double*, double*, double*, double*, double*, int*, double*, double*);
extern void F77_NAME(int59)(float*, float*);
extern void F77_NAME(int6)(int*, float*, float*, float*, float*, int*, float*, int*);
extern void F77_NAME(int60)(float*, float*);
extern void F77_NAME(int61)(float*, float*);
extern void F77_NAME(int62)(float*, float*);
extern void F77_NAME(int63)(float*, double*);
extern void F77_NAME(int64)(float*, double*);
extern void F77_NAME(int65)(float*, double*);
extern void F77_NAME(int66)(float*, double*);
extern void F77_NAME(int67)(float*, double*);
extern void F77_NAME(int68)(float*, double*);
extern void F77_NAME(int69)(float*, double*);
extern void F77_NAME(int7)(float*, double*, float*, int*, int*, int*, int*, int*, int*, int*, float*, int*, int*, int*, int*, float*, int*, float*, double*, double*, double*, double*);
extern void F77_NAME(int70)(float*, int*, float*, double*);
extern void F77_NAME(int9)(float*, double*, float*, int*, int*, int*, int*, int*, int*, int*, float*, int*, int*, int*, int*, float*, int*, float*, double*, double*, double*, double*);
extern void F77_NAME(int92)(float*, float*, float*, int*, int*, int*, float*, int*, float*, float*, int*, int*, int*, int*, int*, float*, float*, float*);

extern void F77_NAME(kfascvz)(float*, float*, int*, int*, int*, int*, float*, float*, float*, int*);
extern void F77_NAME(kiascvz)(float*, int*, int*, int*, int*, float*, float*, float*);
extern void F77_NAME(kiedchz)(float*, int*, float*, int*, float*, float*);
extern void F77_NAME(ktaskvz)(float*, int*, int*, int*, int*, float*, float*, float*, float*);
extern void F77_NAME(ktaskwz)(float*, float*, float*, int*, int*, int*, int*, int*, float*, int*, float*, float*, int*, float*, float*, float*, float*, float*, float*);
extern void F77_NAME(lgamaz)(float*, float*);
extern void F77_NAME(libet0z)(float*);
extern void F77_NAME(libethz)(float*, float*);
extern void F77_NAME(licllsz)(float*, int*, float*, float*, float*, float*);
extern void F77_NAME(liepshz)(float*, float*, float*);
extern void F77_NAME(liindhz)(float*, int*, int*, float*);
extern void F77_NAME(liindsz)(float*, int*, int*, float*);
extern void F77_NAME(liindwz)(float*, int*, int*, int*, float*);
extern void F77_NAME(lilarsz)(float*, int*, int*, float*, float*, float*, float*, float*);
extern void F77_NAME(littstz)(float*, float*, int*, int*, float*, float*, float*, float*, float*, float*, float*, float*);
extern void F77_NAME(lmddz)(float*, float*, int*, int*, float*, float*, float*);
extern void F77_NAME(lrfctdz)(int*, float*, float*, float*, float*, float*, int*, int*, int*, int*, int*, double*, double*, double*, double*);
extern void F77_NAME(lrfnctz)(int*, float*, float*, float*, float*, float*, int*, int*, int*, int*, int*, float*, float*, float*, float*);
extern void F77_NAME(lyhdlez)(float*, int*, int*, int*, float*, int*, int*, float*);
extern void F77_NAME(lymnwtz)(float*, float*, int*, int*, int*, int*, float*, int*, int*, float*);
extern void F77_NAME(machz)(int*, float*);
extern void F77_NAME(machzd)(int*, double*);
extern void F77_NAME(mchlz)(float*, int*, int*, int*);
extern void F77_NAME(mchlzd)(double*, int*, int*, int*);
extern void F77_NAME(mffz)(float*, float*, float*, int*, int*, int*, int*, int*, int*);
extern void F77_NAME(mffzd)(double*, double*, double*, int*, int*, int*, int*, int*, int*);
extern void F77_NAME(mfragrz)(float*, float*, float*, int*, int*, int*, int*, int*, int*, float*, float*, float*, int*, float*, float*, float*, float*, int*, int*, float*, float*, float*, float*, float*, float*, float*, int*, int*, int*);
extern void F77_NAME(mfyz)(float*, float*, float*, int*, int*, int*, int*, int*, int*, int*);
extern void F77_NAME(mfyzd)(double*, double*, double*, int*, int*, int*, int*, int*, int*, int*);
extern void F77_NAME(mhatz)(float*, int*, int*, int*, int*, float*, float*, float*);
extern void F77_NAME(minvz)(float*, int*, int*, float*, int*);
extern void F77_NAME(minvzd)(double*, int*, int*, float*, int*);
extern void F77_NAME(mirtsrz)(float*, float*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, int*, int*, float*, int*, int*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float *, int*);
extern void F77_NAME(mlyz)(float*, float*, int*, int*, int*, int*);
extern void F77_NAME(mlyzd)(double*, double*, int*, int*, int*, int*);
extern void F77_NAME(msfz)(float*, float*, float*, int*, int*, int*, int*, int*);
extern void F77_NAME(msf1z)(float*, float*, float*, int*, int*, int*);
extern void F77_NAME(msf1zd)(double*, double*, double*, int*, int*, int*);
extern void F77_NAME(msfzd)(double*, double*, double*, int*, int*, int*, int*, int*);
extern void F77_NAME(mssz)(float*, float*, float*, int*, int*, int*);
extern void F77_NAME(msszd)(double*, double*, double*, int*, int*, int*);
extern void F77_NAME(mtt1z)(float*, float*, int*, int*);
extern void F77_NAME(mtt1zd)(double*, double*, int*, int*);
extern void F77_NAME(mtt2z)(float*, float*, int*, int*);
extern void F77_NAME(mtt2zd)(double*, double*, int*, int*);
extern void F77_NAME(mtt3z)(float*, float*, float*, int*, int*);
extern void F77_NAME(mtt3zd)(double*, double*, double*, int*, int*);
extern void F77_NAME(mtyz)(float*, float*, int*, int*, int*, int*);
extern void F77_NAME(mtyzd)(double*, double*, int*, int*, int*, int*);
extern void F77_NAME(myhbhez)(float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*, int*, float*, float*, int*);
extern void F77_NAME(mymvlmz)(float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, int*, int*, float*, float*, float*, float*, float*, float*, float*, int*, int*, float*, int*);
extern void F77_NAME(nlgmz)(int*, float*);
extern void F77_NAME(nrm2z)(float*, int*, int*, int*, float*);
extern void F77_NAME(nrm2zd)(double*, int*, int*, int*, double*);
extern void F77_NAME(permcz)(float*, int*, int*, int*, int*, int*);
extern void F77_NAME(permvz)(float*, int*, int*, int*);
extern void F77_NAME(poissnz)(float*, int*, float*, float*);
extern void F77_NAME(precdz)(double*);
extern void F77_NAME(precsz)(float*);
extern void F77_NAME(probinz)(int*, int*, double*, void*, double*);
extern void F77_NAME(probstz)(float*, int*, float*);
extern void F77_NAME(prpoisz)(double*, int*, int*, double*);
extern void F77_NAME(ribet0z)(float*, int*, int*, int*, float*, float*);
extern void F77_NAME(ribethz)(float*, int*, float*, int*, float*);
extern void F77_NAME(ricllsz)(float*, float*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*, float*, int*);
extern void F77_NAME(rilarsz)(float*, float*, int*, int*, int*, int*, float*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*);
extern void F77_NAME(rimtrdz)(double*, int*, int*, int*, int*, float*, int*, double*, double*, double*, int*);
extern void F77_NAME(rimtrfz)(float*, int*, int*, int*, int*, float*, int*, float*, float*, float*, int*);
extern void F77_NAME(rmvcz)(float*, int*, int*, int*, int*, int*, float*, int*, float*);
extern void F77_NAME(rubenz)(float*, float*, int*, int*, float*, float*, int*, float*, float*, float*, int*, float*, float*, float*, float*);
extern void F77_NAME(rybifrz)(float*, float*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, float*, int*, int*, float*, float*, float*, float*, float*);
extern void F77_NAME(rysalgz)(float*, float*, float*, float*, float*, float*, int*, int*, int*, int*, int*, float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, int*, float*, float*);
extern void F77_NAME(scalz)(float*, float*, int*, int*, int*);
extern void F77_NAME(scalzd)(double*, double*, int*, int*, int*);
extern void F77_NAME(srt1z)(float*, int*, int*, int*);
extern void F77_NAME(srt2z)(float*, float*, int*, int*, int*);
extern void F77_NAME(stplrgz)(int*, float*, float*, float*, float*, float*, int*, float*, float*, float*, int*, float*, int*, int*, int*, float*, float*, float*, float*, float*, float*);
extern void F77_NAME(swapz)(float*, float*, int*, int*, int*, int*, int*);
extern void F77_NAME(swapzd)(double*, double*, int*, int*, int*, int*, int*);
extern void F77_NAME(tauarez)(int*, int*, int*, float*, float*, float*, float*, float*, float*, int*, float*, float*);
extern void F77_NAME(tfrn2tz)(float*, float*, int*, int*, int*, int*, float*, float*, float*);
extern void F77_NAME(tisrtcz)(float*, int*, int*, int*, int*, int*, int*, int*);
extern void F77_NAME(tquantz)(float*, int*, float*);
extern void F77_NAME(ttasktz)(float*, float*, int*, int*, int*, int*, float*, float*, float*, float*);
extern void F77_NAME(tteignz)(float*, int*, int*, int*, float*, int*, float*);
extern void F77_NAME(wfshatz)(float*, int*, int*, int*, float*, float*, float*);
extern void F77_NAME(wimedvz)(float*, int*, int*, int*, int*, int*, int*, int*, double*, float*);
extern void F77_NAME(xerfz)(int*, float*, float*);
extern void F77_NAME(xerpz)(int*, float*, float*, float*);
extern void F77_NAME(xsyz)(float*, float*, float*, int*, int*, float*);
extern void F77_NAME(xsyzd)(double*, double*, double*, int*, int*, double*);
extern void F77_NAME(zemllz)(float*, float*, float*, int*, int*, int*, float*, float*);

extern void F77_NAME(nquant)(float*, float*);
extern void F77_NAME(psia)(int*, float*, float*);
extern void F77_NAME(pspa)(int*, float*, float*);
extern void F77_NAME(qd2func)(double*, double*, float*, int*, double*, double*, double*, double*, int*, double*, double*);
extern void F77_NAME(qnexp)(double*, double*, float*, int*, double*, double*, double*, double*, double*, double*, double*, int*, double*, int*, int*);
extern void F77_NAME(randow)(int*, float*);
extern void F77_NAME(regtau)(double*, double*, int*, int*, float*, float*, float*, float*, float*, int*, double*, double*, double*, double*, double*, double*, double*, float*, float*);
extern void F77_NAME(regtauw)(double*, double*, double*, int*, int*, float*, float*, float*, float*, float*, int*, double*, double*, double*, double*, double*, double*, double*, float*, float*);
extern void F77_NAME(rhoa)(int*, float*, float*);
extern void F77_NAME(rpardf)(float*, int*, int*, int*, int*, float*, float*, int*, int*, int*);
extern void F77_NAME(ucva)(int*, float*, double*);
extern void F77_NAME(upcva)(int*, float*, double*);
extern void F77_NAME(wcva)(int*, float*, double*);
extern void F77_NAME(wpcva)(int*, float*, double*);
extern void F77_NAME(wwwa)(int*, float*, double*);
extern void F77_NAME(zdfvals)(int*, float*);

static const R_FortranMethodDef FortranEntries[] = {
    {"addcol",  (DL_FUNC) &F77_NAME(addcol),   9},
    {"chia",    (DL_FUNC) &F77_NAME(chia),     3},
    {"dbinom",  (DL_FUNC) &F77_NAME(dbinom),  10},
    {"fnexp",   (DL_FUNC) &F77_NAME(fnexp),    9},

    {"binprdz", (DL_FUNC) &F77_NAME(binprdz),  5},
    {"cerfz",   (DL_FUNC) &F77_NAME(cerfz),    2},
    {"cerfzd",  (DL_FUNC) &F77_NAME(cerfzd),   2},
    {"cfrcovz", (DL_FUNC) &F77_NAME(cfrcovz),  7},
    {"chisqz",  (DL_FUNC) &F77_NAME(chisqz),   4},
    {"cia2b2z", (DL_FUNC) &F77_NAME(cia2b2z),  6},
    {"cibeatz", (DL_FUNC) &F77_NAME(cibeatz),  4},
    {"ciclocz", (DL_FUNC) &F77_NAME(ciclocz),  3},
    {"cifactz", (DL_FUNC) &F77_NAME(cifactz),  6},
    {"cimedvz", (DL_FUNC) &F77_NAME(cimedvz), 10},
    {"cirockz", (DL_FUNC) &F77_NAME(cirockz),  5},
    {"comvalz", (DL_FUNC) &F77_NAME(comvalz), 24},
    {"cquantz", (DL_FUNC) &F77_NAME(cquantz),  5},
    {"dfcomnz", (DL_FUNC) &F77_NAME(dfcomnz), 24},
    {"dotpz",   (DL_FUNC) &F77_NAME(dotpz),    8},
    {"dotpzd",  (DL_FUNC) &F77_NAME(dotpzd),   8},
    {"dpoissz", (DL_FUNC) &F77_NAME(dpoissz),  9},
    {"exchz",   (DL_FUNC) &F77_NAME(exchz),    5},
    {"exchzd",  (DL_FUNC) &F77_NAME(exchzd),   5},
    {"fcumz",   (DL_FUNC) &F77_NAME(fcumz),    5},
    {"fstordz", (DL_FUNC) &F77_NAME(fstordz),  4},
    {"gaussz",  (DL_FUNC) &F77_NAME(gaussz),   3},
    {"gausszd", (DL_FUNC) &F77_NAME(gausszd),  3},
    {"gfedcaz", (DL_FUNC) &F77_NAME(gfedcaz),  9},
    {"gicstpz", (DL_FUNC) &F77_NAME(gicstpz), 10},
    {"gintacz", (DL_FUNC) &F77_NAME(gintacz), 23},
    {"glmdevz", (DL_FUNC) &F77_NAME(glmdevz), 12},
    {"gyastpz", (DL_FUNC) &F77_NAME(gyastpz), 25},
    {"gycstpz", (DL_FUNC) &F77_NAME(gycstpz),  8},
    {"gymainz", (DL_FUNC) &F77_NAME(gymainz), 41},
    {"gytstpz", (DL_FUNC) &F77_NAME(gytstpz), 32},
    {"h12z",    (DL_FUNC) &F77_NAME(h12z),    12},
    {"h12zd",   (DL_FUNC) &F77_NAME(h12zd),   12},
    {"hylmsez", (DL_FUNC) &F77_NAME(hylmsez), 22},
    {"hyltsez", (DL_FUNC) &F77_NAME(hyltsez), 22},
    {"ingamaz", (DL_FUNC) &F77_NAME(ingamaz),  3},

    {"int0",    (DL_FUNC) &F77_NAME(int0),    15},
    {"int10",   (DL_FUNC) &F77_NAME(int10),   27},
    {"int16",   (DL_FUNC) &F77_NAME(int16),   31},
    {"int21",   (DL_FUNC) &F77_NAME(int21),   31},
    {"int21w",  (DL_FUNC) &F77_NAME(int21w),  32},
    {"int24",   (DL_FUNC) &F77_NAME(int24),    9},
    {"int25",   (DL_FUNC) &F77_NAME(int25),    7},
    {"int27",   (DL_FUNC) &F77_NAME(int27),    9},
    {"int29",   (DL_FUNC) &F77_NAME(int29),    9},
    {"int3",    (DL_FUNC) &F77_NAME(int3),    32},
    {"int31",   (DL_FUNC) &F77_NAME(int31),    5},
    {"int32",   (DL_FUNC) &F77_NAME(int32),    6},
    {"int33",   (DL_FUNC) &F77_NAME(int33),   17},
    {"int36",   (DL_FUNC) &F77_NAME(int36),   25},
    {"int40",   (DL_FUNC) &F77_NAME(int40),    8},
    {"int41",   (DL_FUNC) &F77_NAME(int41),   38},
    {"int44",   (DL_FUNC) &F77_NAME(int44),   35},
    {"int47",   (DL_FUNC) &F77_NAME(int47),   39},
    {"int51",   (DL_FUNC) &F77_NAME(int51),   14},
    {"int52",   (DL_FUNC) &F77_NAME(int52),   12},
    {"int53",   (DL_FUNC) &F77_NAME(int53),   21},
    {"int55",   (DL_FUNC) &F77_NAME(int55),   25},
    {"int57",   (DL_FUNC) &F77_NAME(int57),   23},
    {"int58",   (DL_FUNC) &F77_NAME(int58),   28},
    {"int59",   (DL_FUNC) &F77_NAME(int59),    2},
    {"int6",    (DL_FUNC) &F77_NAME(int6),     8},
    {"int60",   (DL_FUNC) &F77_NAME(int60),    2},
    {"int61",   (DL_FUNC) &F77_NAME(int61),    2},
    {"int62",   (DL_FUNC) &F77_NAME(int62),    2},
    {"int63",   (DL_FUNC) &F77_NAME(int63),    2},
    {"int64",   (DL_FUNC) &F77_NAME(int64),    2},
    {"int65",   (DL_FUNC) &F77_NAME(int65),    2},
    {"int66",   (DL_FUNC) &F77_NAME(int66),    2},
    {"int67",   (DL_FUNC) &F77_NAME(int67),    2},
    {"int68",   (DL_FUNC) &F77_NAME(int68),    2},
    {"int69",   (DL_FUNC) &F77_NAME(int69),    2},
    {"int7",    (DL_FUNC) &F77_NAME(int7),    22},
    {"int70",   (DL_FUNC) &F77_NAME(int70),    4},
    {"int9",    (DL_FUNC) &F77_NAME(int9),    22},
    {"int92",   (DL_FUNC) &F77_NAME(int92),   18},

    {"kfascvz", (DL_FUNC) &F77_NAME(kfascvz), 10},
    {"kiascvz", (DL_FUNC) &F77_NAME(kiascvz),  8},
    {"kiedchz", (DL_FUNC) &F77_NAME(kiedchz),  6},
    {"ktaskvz", (DL_FUNC) &F77_NAME(ktaskvz),  9},
    {"ktaskwz", (DL_FUNC) &F77_NAME(ktaskwz), 19},
    {"lgamaz",  (DL_FUNC) &F77_NAME(lgamaz),   2},
    {"libet0z", (DL_FUNC) &F77_NAME(libet0z),  1},
    {"libethz", (DL_FUNC) &F77_NAME(libethz),  2},
    {"licllsz", (DL_FUNC) &F77_NAME(licllsz),  6},
    {"liepshz", (DL_FUNC) &F77_NAME(liepshz),  3},
    {"liindhz", (DL_FUNC) &F77_NAME(liindhz),  4},
    {"liindsz", (DL_FUNC) &F77_NAME(liindsz),  4},
    {"liindwz", (DL_FUNC) &F77_NAME(liindwz),  5},
    {"lilarsz", (DL_FUNC) &F77_NAME(lilarsz),  8},
    {"littstz", (DL_FUNC) &F77_NAME(littstz), 12},
    {"lmddz",   (DL_FUNC) &F77_NAME(lmddz),    7},
    {"lrfctdz", (DL_FUNC) &F77_NAME(lrfctdz), 15},
    {"lrfnctz", (DL_FUNC) &F77_NAME(lrfnctz), 15},
    {"lyhdlez", (DL_FUNC) &F77_NAME(lyhdlez),  8},
    {"lymnwtz", (DL_FUNC) &F77_NAME(lymnwtz), 10},
    {"machz",   (DL_FUNC) &F77_NAME(machz),    2},
    {"machzd",  (DL_FUNC) &F77_NAME(machzd),   2},
    {"mchlz",   (DL_FUNC) &F77_NAME(mchlz),    4},
    {"mchlzd",  (DL_FUNC) &F77_NAME(mchlzd),   4},
    {"mffz",    (DL_FUNC) &F77_NAME(mffz),     9},
    {"mffzd",   (DL_FUNC) &F77_NAME(mffzd),    9},
    {"mfragrz", (DL_FUNC) &F77_NAME(mfragrz), 29},
    {"mfyz",    (DL_FUNC) &F77_NAME(mfyz),    10},
    {"mfyzd",   (DL_FUNC) &F77_NAME(mfyzd),   10},
    {"mhatz",   (DL_FUNC) &F77_NAME(mhatz),    8},
    {"minvz",   (DL_FUNC) &F77_NAME(minvz),    5},
    {"minvzd",  (DL_FUNC) &F77_NAME(minvzd),   5},
    {"mirtsrz", (DL_FUNC) &F77_NAME(mirtsrz), 29},
    {"mlyz",    (DL_FUNC) &F77_NAME(mlyz),     6},
    {"mlyzd",   (DL_FUNC) &F77_NAME(mlyzd),    6},
    {"msfz",    (DL_FUNC) &F77_NAME(msfz),     8},
    {"msf1z",   (DL_FUNC) &F77_NAME(msf1z),    6},
    {"msf1zd",  (DL_FUNC) &F77_NAME(msf1zd),   6},
    {"msfzd",   (DL_FUNC) &F77_NAME(msfzd),    8},
    {"mssz",    (DL_FUNC) &F77_NAME(mssz),     6},
    {"msszd",   (DL_FUNC) &F77_NAME(msszd),    6},
    {"mtt1z",   (DL_FUNC) &F77_NAME(mtt1z),    4},
    {"mtt1zd",  (DL_FUNC) &F77_NAME(mtt1zd),   4},
    {"mtt2z",   (DL_FUNC) &F77_NAME(mtt2z),    4},
    {"mtt2zd",  (DL_FUNC) &F77_NAME(mtt2zd),   4},
    {"mtt3z",   (DL_FUNC) &F77_NAME(mtt3z),    5},
    {"mtt3zd",  (DL_FUNC) &F77_NAME(mtt3zd),   5},
    {"mtyz",    (DL_FUNC) &F77_NAME(mtyz),     6},
    {"mtyzd",   (DL_FUNC) &F77_NAME(mtyzd),    6},
    {"myhbhez", (DL_FUNC) &F77_NAME(myhbhez), 21},
    {"mymvlmz", (DL_FUNC) &F77_NAME(mymvlmz), 29},
    {"nlgmz",   (DL_FUNC) &F77_NAME(nlgmz),    2},
    {"nrm2z",   (DL_FUNC) &F77_NAME(nrm2z),    5},
    {"nrm2zd",  (DL_FUNC) &F77_NAME(nrm2zd),   5},
    {"permcz",  (DL_FUNC) &F77_NAME(permcz),   6},
    {"permvz",  (DL_FUNC) &F77_NAME(permvz),   4},
    {"poissnz", (DL_FUNC) &F77_NAME(poissnz),  4},
    {"precdz",  (DL_FUNC) &F77_NAME(precdz),   1},
    {"precsz",  (DL_FUNC) &F77_NAME(precsz),   1},
    {"probinz", (DL_FUNC) &F77_NAME(probinz),  5},
    {"probstz", (DL_FUNC) &F77_NAME(probstz),  3},
    {"prpoisz", (DL_FUNC) &F77_NAME(prpoisz),  4},
    {"ribet0z", (DL_FUNC) &F77_NAME(ribet0z),  6},
    {"ribethz", (DL_FUNC) &F77_NAME(ribethz),  5},
    {"ricllsz", (DL_FUNC) &F77_NAME(ricllsz), 18},
    {"rilarsz", (DL_FUNC) &F77_NAME(rilarsz), 17},
    {"rimtrdz", (DL_FUNC) &F77_NAME(rimtrdz), 11},
    {"rimtrfz", (DL_FUNC) &F77_NAME(rimtrfz), 11},
    {"rmvcz",   (DL_FUNC) &F77_NAME(rmvcz),    9},
    {"rubenz",  (DL_FUNC) &F77_NAME(rubenz),  15},
    {"rybifrz", (DL_FUNC) &F77_NAME(rybifrz), 21},
    {"rysalgz", (DL_FUNC) &F77_NAME(rysalgz), 34},
    {"scalz",   (DL_FUNC) &F77_NAME(scalz),    5},
    {"scalzd",  (DL_FUNC) &F77_NAME(scalzd),   5},
    {"srt1z",   (DL_FUNC) &F77_NAME(srt1z),    4},
    {"srt2z",   (DL_FUNC) &F77_NAME(srt2z),    5},
    {"stplrgz", (DL_FUNC) &F77_NAME(stplrgz), 21},
    {"swapz",   (DL_FUNC) &F77_NAME(swapz),    7},
    {"swapzd",  (DL_FUNC) &F77_NAME(swapzd),   7},
    {"tauarez", (DL_FUNC) &F77_NAME(tauarez), 12},
    {"tfrn2tz", (DL_FUNC) &F77_NAME(tfrn2tz),  9},
    {"tisrtcz", (DL_FUNC) &F77_NAME(tisrtcz),  8},
    {"tquantz", (DL_FUNC) &F77_NAME(tquantz),  3},
    {"ttasktz", (DL_FUNC) &F77_NAME(ttasktz), 10},
    {"tteignz", (DL_FUNC) &F77_NAME(tteignz),  7},
    {"wfshatz", (DL_FUNC) &F77_NAME(wfshatz),  7},
    {"wimedvz", (DL_FUNC) &F77_NAME(wimedvz), 10},
    {"xerfz",   (DL_FUNC) &F77_NAME(xerfz),    3},
    {"xerpz",   (DL_FUNC) &F77_NAME(xerpz),    4},
    {"xsyz",    (DL_FUNC) &F77_NAME(xsyz),     6},
    {"xsyzd",   (DL_FUNC) &F77_NAME(xsyzd),    6},
    {"zemllz",  (DL_FUNC) &F77_NAME(zemllz),   8}, 

    {"nquant",  (DL_FUNC) &F77_NAME(nquant),   2},
    {"psia",    (DL_FUNC) &F77_NAME(psia),     3},
    {"pspa",    (DL_FUNC) &F77_NAME(pspa),     3},
    {"qd2func", (DL_FUNC) &F77_NAME(qd2func), 11},
    {"qnexp",   (DL_FUNC) &F77_NAME(qnexp),   15},
    {"randow",  (DL_FUNC) &F77_NAME(randow),   2},
    {"regtau",  (DL_FUNC) &F77_NAME(regtau),  19},
    {"regtauw", (DL_FUNC) &F77_NAME(regtauw), 20},
    {"rhoa",    (DL_FUNC) &F77_NAME(rhoa),     3},
    {"rpardf",  (DL_FUNC) &F77_NAME(rpardf),  10},
    {"ucva",    (DL_FUNC) &F77_NAME(ucva),     3},
    {"upcva",   (DL_FUNC) &F77_NAME(upcva),    3},
    {"wcva",    (DL_FUNC) &F77_NAME(wcva),     3},
    {"wpcva",   (DL_FUNC) &F77_NAME(wpcva),    3},
    {"wwwa",    (DL_FUNC) &F77_NAME(wwwa),     3},
    {"zdfvals", (DL_FUNC) &F77_NAME(zdfvals),  2}, //
    {NULL, NULL, 0}
};

void R_init_robeth(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}


