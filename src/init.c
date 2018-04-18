#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* How was this made?   AR 04/2018
Within the top level of the package source:
tools::package_native_routine_registration_skeleton('c:/data/R/R-3.4.3/library/robeth',,,FALSE)
Copy all text results to file init.c

Then add to NAMESPACE file: useDynLib(robeth, .registration=TRUE)
*/

/* .Fortran calls */
extern void F77_NAME(addcol)(float*, int*, int*, int*, int*, int*, float*, int*, float*);
extern void F77_NAME(binprd)(int*, int*, float*, float*, float*);
extern void F77_NAME(cerf)(float*, float*);
extern void F77_NAME(cerfd)(double*, double*);
extern void F77_NAME(cfrcov)(double*, int*, int*, float*, float*, float*, float*);
extern void F77_NAME(chia)(int*, float*, float*);
extern void F77_NAME(chisq)(int*, int*, float*, float*);
extern void F77_NAME(cia2b2)(float*, int*, float*, int*, float*, float*);
extern void F77_NAME(cibeat)(float*, float*, int*, float*);
extern void F77_NAME(cicloc)(float*, float*, float*);
extern void F77_NAME(cifact)(float*, float*, int*, float*, int*, float*);
extern void F77_NAME(cimedv)(float*, int*, int*, int*, int*, int*, int*, float*, float*, float*);
extern void F77_NAME(cirock)(float*, float*, int*, int*, float*);
extern void F77_NAME(comval)(int*, float*, float*, float*, float*, float*, float*, float*, float*, int*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, int*);
extern void F77_NAME(cquant)(float*, int*, float*, int*, float*);
extern void F77_NAME(dbinom)(float*, float*, float*, float*, int*, float*, float*, int*, float*, float*);
extern void F77_NAME(dfcomn)(int*, float*, float*, float*, float*, float*, float*, float*, float*, int*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, int*);
extern void F77_NAME(dotp)(float*, float*, int*, int*, int*, int*, int*, float*);
extern void F77_NAME(dotpd)(double*, double*, int*, int*, int*, int*, int*, double*);
extern void F77_NAME(dpoiss)(float*, float*, float*, float*, float*, float*, int*, float*, float*);
extern void F77_NAME(exch)(float*, int*, int*, int*, int*);
extern void F77_NAME(exchd)(double*, int*, int*, int*, int*);
extern void F77_NAME(fcum)(int*, int*, float*, float*, int*);
extern void F77_NAME(fnexp)(double*, double*, float*, int*, double*, double*, double*, double*, double*);
extern void F77_NAME(fstord)(float*, int*, int*, float*);
extern void F77_NAME(gauss)(int*, float*, float*);
extern void F77_NAME(gaussd)(int*, double*, double*);
extern void F77_NAME(gfedca)(float*, float*, float*, int*, float*, int*, int*, float*, float*);
extern void F77_NAME(gicstp)(int*, int*, int*, float*, float*, float*, int*, float*, int*, float*);
extern void F77_NAME(gintac)(float*, float*, int*, float*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, int*, int*, float*, double*, float*, float*, float*, float*, float*, int*, double*);
extern void F77_NAME(glmdev)(float*, int*, float*, float*, float*, float*, int*, int*, double*, double*, double*, double*);
extern void F77_NAME(gyastp)(float*, float*, int*, float*, float*, double*, float*, float*, int*, int*, int*, int*, int*, int*, float*, int*, int*, int*, float*, int*, float*, double*, double*, double*, double*);
extern void F77_NAME(gycstp)(int*, int*, int*, float*, float*, float*, int*, float*);
extern void F77_NAME(gymain)(float*, float*, int*, float*, double*, float*, float*, int*, int*, int*, int*, float*, float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, int*, float*, float*, float*, float*, float*, float*, float*, float*, int*, double*);
extern void F77_NAME(gytstp)(float*, float*, float*, float*, float*, float*, int*, float*, int*, int*, int*, int*, float*, float*, float*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, int*);
extern void F77_NAME(h12)(int*, int*, int*, int*, float*, int*,float*, float*, int*, int*, int*, int*);
extern void F77_NAME(h12d)(int*, int*, int*, int*, double*, int*, double*, double*, int*, int*, int*, int*);
extern void F77_NAME(hylmse)(float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, int*, int*, float*, float*, float*, int*, float*, int*);
extern void F77_NAME(hyltse)(float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, int*, int*, float*, float*, float*, int*, float*, int*);
extern void F77_NAME(ingama)(float*, float*, float*);
extern void F77_NAME(int0)(int*, int*, int*, int*, int*, int*, float*, float*, float*, int*, float*, int*, float*, float*, float*);
extern void F77_NAME(int10)(float*, double*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, int*, float*, double*, double*, double*, double*, double*, double*);
extern void F77_NAME(int16)(float*, double*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, int*, float*, double*, double*, double*, double*, double*, double*, double*, double*, double*, double*);
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
extern void F77_NAME(int70)(float*, float*, float*, double*);
extern void F77_NAME(int9)(float*, double*, float*, int*, int*, int*, int*, int*, int*, int*, float*, int*, int*, int*, int*, float*, int*, float*, double*, double*, double*, double*);
extern void F77_NAME(int92)(int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*);
extern void F77_NAME(kfascv)(float*, float*, int*, int*, int*, int*, float*, float*, float*, int*);
extern void F77_NAME(kiascv)(float*, int*, int*, int*, int*, float*, float*, float*);
extern void F77_NAME(kiedch)(float*, int*, float*, int*, float*, float*);
extern void F77_NAME(ktaskv)(float*, int*, int*, int*, int*, float*, float*, float*, float*);
extern void F77_NAME(ktaskw)(float*, float*, float*, int*, int*, int*, int*, int*, float*, int*, float*, float*, int*, float*, float*, float*, float*, float*, float*);
extern void F77_NAME(lgama)(float*, float*);
extern void F77_NAME(libet0)(float*);
extern void F77_NAME(libeth)(float*, float*);
extern void F77_NAME(liclls)(float*, int*, float*, float*, float*, float*);
extern void F77_NAME(liepsh)(float*, float*, float*);
extern void F77_NAME(liindh)(float*, int*, int*, float*);
extern void F77_NAME(liinds)(float*, int*, int*, float*);
extern void F77_NAME(liindw)(float*, int*, int*, int*, float*);
extern void F77_NAME(lilars)(float*, int*, int*, float*, float*, float*, float*, float*);
extern void F77_NAME(littst)(float*, float*, int*, int*, float*, float*, float*, float*, float*, float*, float*, float*);
extern void F77_NAME(lmdd)(float*, float*, int*, int*, float*, float*, float*);
extern void F77_NAME(lrfctd)(int*, float*, float*, float*, float*, float*, int*, int*, int*, int*, int*, double*, double*, double*, double*);
extern void F77_NAME(lrfnct)(int*, float*, float*, float*, float*, float*, int*, int*, int*, int*, int*, float*, float*, float*, float*);
extern void F77_NAME(lyhdle)(float*, int*, int*, int*, float*, int*, int*, float*);
extern void F77_NAME(lymnwt)(float*, float*, int*, int*, int*, int*, float*, int*, int*, float*);
extern void F77_NAME(mach)(int*, float*);
extern void F77_NAME(machd)(int*, double*);
extern void F77_NAME(mchl)(float*, int*, int*, int*);
extern void F77_NAME(mchld)(double*, int*, int*, int*);
extern void F77_NAME(mff)(float*, float*, float*, int*, int*, int*, int*, int*, int*);
extern void F77_NAME(mffd)(double*, double*, double*, int*, int*, int*, int*, int*, int*);
extern void F77_NAME(mfragr)(float*, float*, float*, int*, int*, int*, int*, int*, int*, float*, float*, float*, int*, float*, float*, float*, float*, int*, int*, float*, float*, float*, float*, float*, float*, float*, int*, int*, int*);
extern void F77_NAME(mfy)(float*, float*, float*, int*, int*, int*, int*, int*, int*, int*);
extern void F77_NAME(mfyd)(double*, double*, double*, int*, int*, int*, int*, int*, int*, int*);
extern void F77_NAME(mhat)(float*, int*, int*, int*, int*, float*, float*, float*);
extern void F77_NAME(minv)(float*, int*, int*, float*, int*);
extern void F77_NAME(minvd)(double*, int*, int*, float*, int*);
extern void F77_NAME(mirtsr)(float*, float*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, int*, int*, float*, int*, int*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float *, int*);
extern void F77_NAME(mly)(float*, float*, int*, int*, int*, int*);
extern void F77_NAME(mlyd)(double*, double*, int*, int*, int*, int*);
extern void F77_NAME(msf)(float*, float*, float*, int*, int*, int*, int*, int*);
extern void F77_NAME(msf1)(float*, float*, float*, int*, int*, int*);
extern void F77_NAME(msf1d)(double*, double*, double*, int*, int*, int*);
extern void F77_NAME(msfd)(double*, double*, double*, int*, int*, int*, int*, int*);
extern void F77_NAME(mss)(float*, float*, float*, int*, int*, int*);
extern void F77_NAME(mssd)(double*, double*, double*, int*, int*, int*);
extern void F77_NAME(mtt1)(float*, float*, int*, int*);
extern void F77_NAME(mtt1d)(double*, double*, int*, int*);
extern void F77_NAME(mtt2)(float*, float*, int*, int*);
extern void F77_NAME(mtt2d)(double*, double*, int*, int*);
extern void F77_NAME(mtt3)(float*, float*, float*, int*, int*);
extern void F77_NAME(mtt3d)(double*, double*, double*, int*, int*);
extern void F77_NAME(mty)(float*, float*, int*, int*, int*, int*);
extern void F77_NAME(mtyd)(double*, double*, int*, int*, int*, int*);
extern void F77_NAME(myhbhe)(float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*, int*, float*, float*, int*);
extern void F77_NAME(mymvlm)(float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, int*, int*, float*, float*, float*, float*, float*, float*, float*, int*, int*, float*, int*);
extern void F77_NAME(nlgm)(int*, float*);
extern void F77_NAME(nquant)(float*, float*);
extern void F77_NAME(nrm2)(float*, int*, int*, int*, float*);
extern void F77_NAME(nrm2d)(double*, int*, int*, int*, double*);
extern void F77_NAME(permc)(float*, int*, int*, int*, int*, int*);
extern void F77_NAME(permv)(float*, int*, int*, int*);
extern void F77_NAME(poissn)(float*, int*, float*, float*);
extern void F77_NAME(precd)(double*);
extern void F77_NAME(precs)(float*);
extern void F77_NAME(probin)(int*, int*, double*, void*, double*);
extern void F77_NAME(probst)(float*, int*, float*);
extern void F77_NAME(prpois)(double*, int*, int*, double*);
extern void F77_NAME(psia)(int*, float*, float*);
extern void F77_NAME(pspa)(int*, float*, float*);
extern void F77_NAME(qd2func)(double*, double*, float*, int*, double*, double*, double*, double*, int*, double*, double*);
extern void F77_NAME(qnexp)(double*, double*, float*, int*, double*, double*, double*, double*, double*, double*, double*, int*, double*, int*, int*);
extern void F77_NAME(randow)(int*, float*);
extern void F77_NAME(regtau)(double*, double*, int*, int*, float*, float*, float*, float*, float*, int*, double*, double*, double*, double*, double*, double*, double*, float*, float*);
extern void F77_NAME(regtauw)(double*, double*, double*, int*, int*, float*, float*, float*, float*, float*, int*, double*, double*, double*, double*, double*, double*, double*, float*, float*);
extern void F77_NAME(rhoa)(int*, float*, float*);
extern void F77_NAME(ribet0)(float*, int*, int*, int*, float*, float*);
extern void F77_NAME(ribeth)(float*, int*, float*, int*, float*);
extern void F77_NAME(riclls)(float*, float*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*, float*, int*);
extern void F77_NAME(rilars)(float*, float*, int*, int*, int*, int*, float*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*);
extern void F77_NAME(rimtrd)(double*, int*, int*, int*, int*, double*, float*, double*, double*, double*, int*);
extern void F77_NAME(rimtrf)(float*, int*, int*, int*, int*, float*, int*, float*, float*, float*, int*);
extern void F77_NAME(rmvc)(float*, int*, int*, int*, int*, int*, float*, int*, float*);
extern void F77_NAME(rpardf)(float*, int*, int*, int*, int*, float*, float*, int*, int*, int*);
extern void F77_NAME(ruben)(float*, float*, int*, int*, float*, float*, int*, float*, float*, float*, int*, float*, float*, float*, float*);
extern void F77_NAME(rysalg)(float*, float*, float*, float*, float*, float*, int*, int*, int*, int*, int*, float*, float*, int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, float*, float*, float*, float*, float*, float*, float*, float*, int*, float*, float*);
extern void F77_NAME(scal)(float*, float*, int*, int*, int*);
extern void F77_NAME(scald)(double*, double*, int*, int*, int*);
extern void F77_NAME(srt1)(float*, int*, int*, int*);
extern void F77_NAME(srt2)(float*, float*, int*, int*, int*);
extern void F77_NAME(stplrg)(int*, float*, float*, float*, float*, float*, int*, float*, float*, float*, int*, float*, int*, int*, int*, float*, float*, float*, float*, float*, float*);
extern void F77_NAME(swap)(float*, float*, int*, int*, int*, int*, int*);
extern void F77_NAME(swapd)(double*, double*, int*, int*, int*, int*, int*);
extern void F77_NAME(tauare)(int*, int*, int*, float*, float*, float*, float*, float*, float*, int*, float*, int*);
extern void F77_NAME(tfrn2t)(float*, float*, int*, int*, int*, int*, float*, float*, float*);
extern void F77_NAME(tisrtc)(float*, int*, int*, int*, int*, int*, int*, int*);
extern void F77_NAME(tquant)(float*, int*, float*);
extern void F77_NAME(ttaskt)(float*, float*, int*, int*, int*, int*, float*, float*, float*, float*);
extern void F77_NAME(tteign)(float*, int*, int*, int*, float*, int*, float*);
extern void F77_NAME(ucva)(int*, float*, float*);
extern void F77_NAME(upcva)(int*, float*, float*);
extern void F77_NAME(wcva)(int*, float*, float*);
extern void F77_NAME(wfshat)(float*, int*, int*, int*, float*, float*, float*);
extern void F77_NAME(wimedv)(int*, int*, int*, int*, int*, int*, int*, int*, int*, int*);
extern void F77_NAME(wpcva)(int*, float*, float*);
extern void F77_NAME(wwwa)(int*, float*, float*);
extern void F77_NAME(xerf)(int*, float*, float*);
extern void F77_NAME(xerp)(int*, float*, float*, float*);
extern void F77_NAME(xsy)(float*, float*, float*, int*, int*, float*);
extern void F77_NAME(xsyd)(double*, double*, double*, int*, int*, double*);
extern void F77_NAME(zdfvals)(int*, float*);
extern void F77_NAME(zemll)(float*, float*, float*, int*, int*, int*, float*, float*);

static const R_FortranMethodDef FortranEntries[] = {
    {"addcol",  (DL_FUNC) &F77_NAME(addcol),   9},
    {"binprd",  (DL_FUNC) &F77_NAME(binprd),   5},
    {"cerf",    (DL_FUNC) &F77_NAME(cerf),     2},
    {"cerfd",   (DL_FUNC) &F77_NAME(cerfd),    2},
    {"cfrcov",  (DL_FUNC) &F77_NAME(cfrcov),   7},
    {"chia",    (DL_FUNC) &F77_NAME(chia),     3},
    {"chisq",   (DL_FUNC) &F77_NAME(chisq),    4},
    {"cia2b2",  (DL_FUNC) &F77_NAME(cia2b2),   6},
    {"cibeat",  (DL_FUNC) &F77_NAME(cibeat),   4},
    {"cicloc",  (DL_FUNC) &F77_NAME(cicloc),   3},
    {"cifact",  (DL_FUNC) &F77_NAME(cifact),   6},
    {"cimedv",  (DL_FUNC) &F77_NAME(cimedv),  10},
    {"cirock",  (DL_FUNC) &F77_NAME(cirock),   5},
    {"comval",  (DL_FUNC) &F77_NAME(comval),  24},
    {"cquant",  (DL_FUNC) &F77_NAME(cquant),   5},
    {"dbinom",  (DL_FUNC) &F77_NAME(dbinom),  10},
    {"dfcomn",  (DL_FUNC) &F77_NAME(dfcomn),  24},
    {"dotp",    (DL_FUNC) &F77_NAME(dotp),     8},
    {"dotpd",   (DL_FUNC) &F77_NAME(dotpd),    8},
    {"dpoiss",  (DL_FUNC) &F77_NAME(dpoiss),   9},
    {"exch",    (DL_FUNC) &F77_NAME(exch),     5},
    {"exchd",   (DL_FUNC) &F77_NAME(exchd),    5},
    {"fcum",    (DL_FUNC) &F77_NAME(fcum),     5},
    {"fnexp",   (DL_FUNC) &F77_NAME(fnexp),    9},
    {"fstord",  (DL_FUNC) &F77_NAME(fstord),   4},
    {"gauss",   (DL_FUNC) &F77_NAME(gauss),    3},
    {"gaussd",  (DL_FUNC) &F77_NAME(gaussd),   3},
    {"gfedca",  (DL_FUNC) &F77_NAME(gfedca),   9},
    {"gicstp",  (DL_FUNC) &F77_NAME(gicstp),  10},
    {"gintac",  (DL_FUNC) &F77_NAME(gintac),  27},
    {"glmdev",  (DL_FUNC) &F77_NAME(glmdev),  12},
    {"gyastp",  (DL_FUNC) &F77_NAME(gyastp),  25},
    {"gycstp",  (DL_FUNC) &F77_NAME(gycstp),   8},
    {"gymain",  (DL_FUNC) &F77_NAME(gymain),  41},
    {"gytstp",  (DL_FUNC) &F77_NAME(gytstp),  32},
    {"h12",     (DL_FUNC) &F77_NAME(h12),     12},
    {"h12d",    (DL_FUNC) &F77_NAME(h12d),    12},
    {"hylmse",  (DL_FUNC) &F77_NAME(hylmse),  22},
    {"hyltse",  (DL_FUNC) &F77_NAME(hyltse),  22},
    {"ingama",  (DL_FUNC) &F77_NAME(ingama),   3},
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
    {"kfascv",  (DL_FUNC) &F77_NAME(kfascv),  10},
    {"kiascv",  (DL_FUNC) &F77_NAME(kiascv),   8},
    {"kiedch",  (DL_FUNC) &F77_NAME(kiedch),   6},
    {"ktaskv",  (DL_FUNC) &F77_NAME(ktaskv),   9},
    {"ktaskw",  (DL_FUNC) &F77_NAME(ktaskw),  19},
    {"lgama",   (DL_FUNC) &F77_NAME(lgama),    2},
    {"libet0",  (DL_FUNC) &F77_NAME(libet0),   1},
    {"libeth",  (DL_FUNC) &F77_NAME(libeth),   2},
    {"liclls",  (DL_FUNC) &F77_NAME(liclls),   6},
    {"liepsh",  (DL_FUNC) &F77_NAME(liepsh),   3},
    {"liindh",  (DL_FUNC) &F77_NAME(liindh),   4},
    {"liinds",  (DL_FUNC) &F77_NAME(liinds),   4},
    {"liindw",  (DL_FUNC) &F77_NAME(liindw),   5},
    {"lilars",  (DL_FUNC) &F77_NAME(lilars),   8},
    {"littst",  (DL_FUNC) &F77_NAME(littst),  12},
    {"lmdd",    (DL_FUNC) &F77_NAME(lmdd),     7},
    {"lrfctd",  (DL_FUNC) &F77_NAME(lrfctd),  15},
    {"lrfnct",  (DL_FUNC) &F77_NAME(lrfnct),  15},
    {"lyhdle",  (DL_FUNC) &F77_NAME(lyhdle),   8},
    {"lymnwt",  (DL_FUNC) &F77_NAME(lymnwt),  10},
    {"mach",    (DL_FUNC) &F77_NAME(mach),     2},
    {"machd",   (DL_FUNC) &F77_NAME(machd),    2},
    {"mchl",    (DL_FUNC) &F77_NAME(mchl),     4},
    {"mchld",   (DL_FUNC) &F77_NAME(mchld),    4},
    {"mff",     (DL_FUNC) &F77_NAME(mff),      9},
    {"mffd",    (DL_FUNC) &F77_NAME(mffd),     9},
    {"mfragr",  (DL_FUNC) &F77_NAME(mfragr),  29},
    {"mfy",     (DL_FUNC) &F77_NAME(mfy),     10},
    {"mfyd",    (DL_FUNC) &F77_NAME(mfyd),    10},
    {"mhat",    (DL_FUNC) &F77_NAME(mhat),     8},
    {"minv",    (DL_FUNC) &F77_NAME(minv),     5},
    {"minvd",   (DL_FUNC) &F77_NAME(minvd),    5},
    {"mirtsr",  (DL_FUNC) &F77_NAME(mirtsr),  29},
    {"mly",     (DL_FUNC) &F77_NAME(mly),      6},
    {"mlyd",    (DL_FUNC) &F77_NAME(mlyd),     6},
    {"msf",     (DL_FUNC) &F77_NAME(msf),      8},
    {"msf1",    (DL_FUNC) &F77_NAME(msf1),     6},
    {"msf1d",   (DL_FUNC) &F77_NAME(msf1d),    6},
    {"msfd",    (DL_FUNC) &F77_NAME(msfd),     8},
    {"mss",     (DL_FUNC) &F77_NAME(mss),      6},
    {"mssd",    (DL_FUNC) &F77_NAME(mssd),     6},
    {"mtt1",    (DL_FUNC) &F77_NAME(mtt1),     4},
    {"mtt1d",   (DL_FUNC) &F77_NAME(mtt1d),    4},
    {"mtt2",    (DL_FUNC) &F77_NAME(mtt2),     4},
    {"mtt2d",   (DL_FUNC) &F77_NAME(mtt2d),    4},
    {"mtt3",    (DL_FUNC) &F77_NAME(mtt3),     5},
    {"mtt3d",   (DL_FUNC) &F77_NAME(mtt3d),    5},
    {"mty",     (DL_FUNC) &F77_NAME(mty),      6},
    {"mtyd",    (DL_FUNC) &F77_NAME(mtyd),     6},
    {"myhbhe",  (DL_FUNC) &F77_NAME(myhbhe),  21},
    {"mymvlm",  (DL_FUNC) &F77_NAME(mymvlm),  29},
    {"nlgm",    (DL_FUNC) &F77_NAME(nlgm),     2},
    {"nquant",  (DL_FUNC) &F77_NAME(nquant),   2},
    {"nrm2",    (DL_FUNC) &F77_NAME(nrm2),     5},
    {"nrm2d",   (DL_FUNC) &F77_NAME(nrm2d),    5},
    {"permc",   (DL_FUNC) &F77_NAME(permc),    6},
    {"permv",   (DL_FUNC) &F77_NAME(permv),    4},
    {"poissn",  (DL_FUNC) &F77_NAME(poissn),   4},
    {"precd",   (DL_FUNC) &F77_NAME(precd),    1},
    {"precs",   (DL_FUNC) &F77_NAME(precs),    1},
    {"probin",  (DL_FUNC) &F77_NAME(probin),   5},
    {"probst",  (DL_FUNC) &F77_NAME(probst),   3},
    {"prpois",  (DL_FUNC) &F77_NAME(prpois),   4},
    {"psia",    (DL_FUNC) &F77_NAME(psia),     3},
    {"pspa",    (DL_FUNC) &F77_NAME(pspa),     3},
    {"qd2func", (DL_FUNC) &F77_NAME(qd2func), 11},
    {"qnexp",   (DL_FUNC) &F77_NAME(qnexp),   15},
    {"randow",  (DL_FUNC) &F77_NAME(randow),   2},
    {"regtau",  (DL_FUNC) &F77_NAME(regtau),  19},
    {"regtauw", (DL_FUNC) &F77_NAME(regtauw), 20},
    {"rhoa",    (DL_FUNC) &F77_NAME(rhoa),     3},
    {"ribet0",  (DL_FUNC) &F77_NAME(ribet0),   6},
    {"ribeth",  (DL_FUNC) &F77_NAME(ribeth),   5},
    {"riclls",  (DL_FUNC) &F77_NAME(riclls),  18},
    {"rilars",  (DL_FUNC) &F77_NAME(rilars),  17},
    {"rimtrd",  (DL_FUNC) &F77_NAME(rimtrd),  11},
    {"rimtrf",  (DL_FUNC) &F77_NAME(rimtrf),  11},
    {"rmvc",    (DL_FUNC) &F77_NAME(rmvc),     9},
    {"rpardf",  (DL_FUNC) &F77_NAME(rpardf),  10},
    {"ruben",   (DL_FUNC) &F77_NAME(ruben),   15},
    {"rysalg",  (DL_FUNC) &F77_NAME(rysalg),  34},
    {"scal",    (DL_FUNC) &F77_NAME(scal),     5},
    {"scald",   (DL_FUNC) &F77_NAME(scald),    5},
    {"srt1",    (DL_FUNC) &F77_NAME(srt1),     4},
    {"srt2",    (DL_FUNC) &F77_NAME(srt2),     5},
    {"stplrg",  (DL_FUNC) &F77_NAME(stplrg),  21},
    {"swap",    (DL_FUNC) &F77_NAME(swap),     7},
    {"swapd",   (DL_FUNC) &F77_NAME(swapd),    7},
    {"tauare",  (DL_FUNC) &F77_NAME(tauare),  12},
    {"tfrn2t",  (DL_FUNC) &F77_NAME(tfrn2t),   9},
    {"tisrtc",  (DL_FUNC) &F77_NAME(tisrtc),   8},
    {"tquant",  (DL_FUNC) &F77_NAME(tquant),   3},
    {"ttaskt",  (DL_FUNC) &F77_NAME(ttaskt),  10},
    {"tteign",  (DL_FUNC) &F77_NAME(tteign),   7},
    {"ucva",    (DL_FUNC) &F77_NAME(ucva),     3},
    {"upcva",   (DL_FUNC) &F77_NAME(upcva),    3},
    {"wcva",    (DL_FUNC) &F77_NAME(wcva),     3},
    {"wfshat",  (DL_FUNC) &F77_NAME(wfshat),   7},
    {"wimedv",  (DL_FUNC) &F77_NAME(wimedv),  10},
    {"wpcva",   (DL_FUNC) &F77_NAME(wpcva),    3},
    {"wwwa",    (DL_FUNC) &F77_NAME(wwwa),     3},
    {"xerf",    (DL_FUNC) &F77_NAME(xerf),     3},
    {"xerp",    (DL_FUNC) &F77_NAME(xerp),     4},
    {"xsy",     (DL_FUNC) &F77_NAME(xsy),      6},
    {"xsyd",    (DL_FUNC) &F77_NAME(xsyd),     6},
    {"zdfvals", (DL_FUNC) &F77_NAME(zdfvals),  2},
    {"zemll",   (DL_FUNC) &F77_NAME(zemll),    8}, //
    {NULL, NULL, 0}
};

void R_init_robeth(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}


