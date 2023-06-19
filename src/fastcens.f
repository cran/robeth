
      SUBROUTINE INGAMD(X,P,G)
C.......................................................................
C
C   AUTHOR :     G. P. BHATTACHARJEE (1970)
C                ALGORITHM AS 32 "THE INCOMPLETE GAMA INTEGRAL"
C                APPLIED STATISTICS, VOL.19.
C                REPRINT FROM PP.285-287 WITH THE PERMISSION OF 
C                BLACKWELL PUBLISHERS.
C                ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION X,PN(6),P,G,GP,GIN,OFLO,FACTOR,RN,TERM,TOL,
     1                 A,B,AN,DIF,XEXPD
      EXTERNAL XEXPD
      DATA TOL/1.0D-8/
C
      G=0.D0
      IF (X.EQ.0.D0) RETURN
      IF (X.LT.0.D0.OR.P.LE.0.D0) CALL MESSGE(500,'INGAMD',1)
      CALL MACHZD(6,OFLO)
      OFLO=OFLO*1.D-15
      CALL LGAMAD(P,GP)
      GIN=0.D0
      FACTOR=XEXPD(P*DLOG(X)-X-GP)
      IF (X.GT.1.D0.AND.X.GE.P) GOTO 30
C
C  CALCULATION BY SERIES EXPANSION
C
      GIN=1.D0
      TERM=1.D0
      RN=P
   20 RN=RN+1.D0
      TERM=TERM*X/RN
      GIN=GIN+TERM
      IF (TERM.GT.TOL) GOTO 20
      GIN=GIN*FACTOR/P
      GOTO 50
C
C  CALCULATION BY CONTINUED FRACTION
C
   30 A=1.D0-P
      B=A+X+1.D0
      TERM=0.D0
      PN(1)=1.D0
      PN(2)=X
      PN(3)=X+1.D0
      PN(4)=X*B
      GIN=PN(3)/PN(4)
   32 A=A+1.D0
      B=B+2.D0
      TERM=TERM+1.D0
      AN=A*TERM
      DO 33 I=1,2
      PN(I+4)=B*PN(I+2)-AN*PN(I)
   33 CONTINUE 
      IF (PN(6).EQ.0.D0) GOTO 35
      RN=PN(5)/PN(6)
      DIF=DABS(GIN-RN)
      IF (DIF.GT.TOL) GOTO 34
      IF (DIF.LE.TOL*RN) GOTO 42
   34 GIN=RN
   35 DO 36 I=1,4
      PN(I)=PN(I+2)
   36 CONTINUE 
      IF (DABS(PN(5)).LT.OFLO) GOTO 32
      DO 41 I=1,4
      PN(I)=PN(I)/OFLO
   41 CONTINUE 
      GOTO 32
   42 GIN=1.D0-FACTOR*GIN
   50 G=GIN
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LGAMAD(X,GL)
C.......................................................................
C
C   AUTHORS :     M.C. PIKE AND I.D. HILL (1966)
C                 ALGORITHM 291: LOGARITHM OF GAMMA FUNCTION.
C                 COMMUNICATIONS OF THE ACM, VOL.9, P 684.
C                 ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION X,GL,V,F,Z
      IF (X.LE.0.D0) CALL MESSGE(500,'LGAMAD',1)
      V=X
      F=0.D0
      IF (X.GE.7.D0) GOTO 300
      F=1.D0
      Z=X-1.D0
  100 Z=Z+1.D0
      IF (Z.GE.7.D0) GOTO 200
      V=Z
      F=F*Z
      GOTO 100
  200 V=V+1.D0
      F=-DLOG(F)
  300 Z=1.D0/V**2
      GL=F+(V-0.5D0)*DLOG(V)-V+.9189385332D0+(((-.000595238D0*Z+
     +   .0007936507D0)*Z - .0027777778D0)*Z+.0833333333D0)/V
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PLOGAMA(X,LAMBDA,ZERO,RES)
      DOUBLE PRECISION X,LAMBDA,ZERO,ALPHA,RES,TMP,XEXPD
      EXTERNAL XEXPD
      IF (DABS(LAMBDA).GT.ZERO) THEN
        ALPHA=1.D0/(LAMBDA**2) 
        TMP=LAMBDA*X      
        TMP=ALPHA*XEXPD(TMP)
        CALL INGAMD(TMP,ALPHA,RES)
      ELSE
        CALL GAUSSZD(1,X,RES)
      ENDIF
      IF (LAMBDA.LT.-ZERO) RES=1.D0-RES
      RETURN
      END
C
C-----------------------------------------------------------------------
C
C  Fn.Exp <- function(z,y,delta,mu,sigma,lambda){
C  # parametric estimate of survival cdf
C  n   <- length(y)
C  r   <- (y-mu)/sigma
C  Iz  <- 1*(r <= z)
C  Izc <- Iz[delta==0]
C  Izu <- Iz[delta==1]
C  rc  <-  r[delta==0]
C  ss  <- sum(Izu) + sum( (ploggamma(z,lambda)-ploggamma(rc,lambda))/(1-ploggamma(rc,lambda))*Izc )
C  ss/n}
C
      SUBROUTINE FNEXP(Z,Y,DELTA,N,MU,SIGMA,LAMBDA,ZERO,RES)
      DOUBLE PRECISION Z,Y(N),SS,MU,SIGMA,LAMBDA,RI,NUM,DEN,TMP,RES,
     +       ZERO,PLGZ
      REAL DELTA(N)
C
      SS=0.D0
      CALL PLOGAMA(Z,LAMBDA,ZERO,PLGZ)      
      DO 300 I=1,N
      RI=(Y(I)-MU)/SIGMA
      IF (DELTA(I).EQ.1.0) THEN
        IF (RI.LE.Z) SS=SS+1.D0
      ELSE
        IF (RI.GT.Z) GOTO 300
        CALL PLOGAMA(RI,LAMBDA,ZERO,TMP)
        NUM=PLGZ-TMP
        DEN=1.D0-TMP
        SS=SS+NUM/DEN
      ENDIF
  300 CONTINUE
      RES=SS/DBLE(N)
      RETURN
      END
C
C  Qn.Exp <- function(p,yc,delta,mu,sigma,lambda){
C  zqj    <- regfal(Fn.Exp,p,lower=-10,upper=6,nint=50,tol=0.001,maxit=20,yc,delta,mu,sigma,lambda)
C  qj     <- zqj$solution
C  qj}
C
      SUBROUTINE QNEXP(P,YC,DELTA,N,MU,SIGMA,LAMBDA,ZERO,
     +           A,B,TOL,MAXIT,QJ,ITR,ITERM)
      DOUBLE PRECISION P,YC(N),MU,SIGMA,LAMBDA,A,B,XN,FA,FB,FN,TOL,QJ,
     +                 ZERO,AA,BB,PAS
      REAL DELTA(N)
C
C  INITIALIZE
C
c     TL=DMIN1(1.D-10,0.1D0*TOL)
      ITR=1
      MESS=0
      CALL FNEXP(A,YC,DELTA,N,MU,SIGMA,LAMBDA,ZERO,FA)
      FA=FA-P
      CALL FNEXP(B,YC,DELTA,N,MU,SIGMA,LAMBDA,ZERO,FB)
      FB=FB-P
      IF (FA*FB.GT.0.D0.OR.B-A.GT.3.D0) THEN
        PAS=(B-A)/50.D0
        AA=A
        DO 10 I=2,51
        BB=AA+PAS
        CALL FNEXP(BB,YC,DELTA,N,MU,SIGMA,LAMBDA,ZERO,FB)
        FB=FB-P
        IF (FA*FB.LE.0.D0) GOTO 20
        AA=BB
        FA=FB
   10   CONTINUE
        CALL MESSGE(400,'QNEXP ',0)
        QJ=-999.D0 
        ITR=-1
        ITERM=0
        RETURN
   20   A=AA
        B=BB
      ENDIF
C
C  REGULA FALSI ITERATION
C
   30 XN=(A*FB-B*FA)/(FB-FA)
      CALL FNEXP(XN,YC,DELTA,N,MU,SIGMA,LAMBDA,ZERO,FN)
      FN=FN-P
C
C  TEST TO SEE IF MAXIMUM NUMBER OF ITERATIONS HAS BEEN EXECUTED
C
      IF (ITR.GE.MAXIT) GOTO 60
C
C  TEST TO SEE IF ROOT HAS BEEN FOUND
C
      IF (DABS(FN).LT.TOL) GOTO 70
      IF (FA*FN.LE.0.D0) GOTO 40
      A=XN
      FA=FN
      GOTO 50
   40 B=XN
      FB=FN
C
C  INCREMENT ITERATION COUNTER
C
   50 ITR=ITR+1
      GOTO 30
C
   60 ITERM=2
      QJ=XN
      RETURN
   70 ITERM=1
      QJ=XN
      RETURN
      END
C
C  QD2funC <- function(lambda,yc,delta,muI,sigmaI){
C  n   <- length(yc); P <- ppoints(n)
C  Qn  <- apply(as.matrix(P),1,Qn.Exp,yc,delta,muI,sigmaI,lambda)
C  ql  <- qloggamma(P,lambda)
C  res <- lsfit(ql,Qn)$residuals
C  sum(res^2)}
C
C  ppoints <- function (n, a = ifelse(n <= 10, 3/8, 1/2)) {
C   if (length(n) > 1) n <- length(n)
C   if (n > 0) 
C      (1:n - a)/(n + 1 - 2 * a)
C   else numeric(0)}
C
      SUBROUTINE QD2FUNC(LAMBDA,YC,DELTA,N,MUI,SIGMAI,ZERO,
     +           TOL,MAXIT,P,QN)
      DOUBLE PRECISION YC(N),P(N),QN(N),MUI,SIGMAI,LAMBDA,A,B,FA,FB,
     +                 ZERO,TOL,QJ
      REAL DELTA(N)
      A=-10.D0
      B=6.D0
      K=2
      ITERM=0
      DO 100 I=1,N
      IF (I.NE.1) THEN
        IF (ITERM.NE.2) A=QN(I-1)
        FA=P(I-1)-P(I)
        B=A
  10    B=B+0.25D0
        CALL FNEXP(B,YC,DELTA,N,MUI,SIGMAI,LAMBDA,ZERO,FB)
        FB=FB-P(I)
        IF (FB.LT.0.D0) GOTO 10
      ENDIF
      CALL QNEXP(P(I),YC,DELTA,N,MUI,SIGMAI,LAMBDA,ZERO,
     +           A,B,TOL,MAXIT,QJ,ITR,ITERM)      
      QN(I)=QJ
C     QL(1,I)=1.0
C     QL(2,I)=qloggamma(P,lambda)
  100 CONTINUE
      RETURN
      END
C
      SUBROUTINE ZEMLLZ(B,X,YO,DO,N,NP,R,EMLL)
      REAL B(NP),X(N,NP),YO(N),R(N),AN(2)
      INTEGER DO(N) 
      IJ=0
      NU=0
      NN=N*N
      DO 100 I=1,N
      NU=NU+DO(I)
  100 CONTINUE
      NC=N-NU
      S1=0.0
      S2=0.0
      O1=0.0
      I1=0
      I0=N+1
      SD1=0.0
      SD0=0.0
      S2D1=0.0
      S2D0=0.0
      DO 300 I=1,N
      XB=0.0
      DO 200 J=1,NP
      XB=XB+X(I,J)*B(J)
  200 CONTINUE
      R(I)=YO(I)-XB
      RI2=R(I)*R(I)
      IF (DO(I).EQ.1) THEN
        SD1=SD1+R(I)
        S2D1=S2D1+RI2
        O1=O1-XB-R(I)
      ELSE
        SD0=SD0+R(I)
        S2D0=S2D0+RI2
      ENDIF
  300 CONTINUE
      IF (NU.GT.1) THEN
        SD1=SD1/FLOAT(NU)
        S1=(S2D1-FLOAT(NU)*(SD1**2))/FLOAT(NU-1)
        S1=SQRT(S1)
      ENDIF
      IF (NC.GT.1) THEN
       SD0=SD0/FLOAT(NC)
       S2=(S2D0-FLOAT(NC)*(SD0**2))/FLOAT(NC-1)
       S2=SQRT(S2)
      ENDIF
      IF (S2.EQ.0.0) S2=S1
      AN(1) = ((8.0*SQRT(2.0)/3.0)**0.2)*S1*(FLOAT(N)**(-0.2))
      AN(2) = (4.0**(1.0/3.0))*S2*(FLOAT(N)**(-1.0/3.0))
      O2=0.0
      O3=0.0
      DO 600 I=1,N
      IF (DO(I).EQ.0) GOTO 600
      ARG2=0.0
      ARG3=0.0
      IA=1
      IF (DO(I).EQ.0) IA=2
      DO 500 J=1,N
      Z=(R(J)-R(I))/AN(IA)
      CALL GAUSSZ(1,Z,PNRMZ)
      ARG3=ARG3+PNRMZ
      IF (DO(J).EQ.0) GOTO 500
      CALL XERFZ(2,Z,DNRMZ)
      ARG2=ARG2+DNRMZ/AN(IA)
 500  CONTINUE
      ARG2=ARG2/FLOAT(N)
      ARG3=ARG3/FLOAT(N)
      IF (ARG2.EQ.0.0) ARG2=0.0001
      IF (ARG3.EQ.0.0) ARG3=0.0001
      O2=O2+ALOG(ARG2)
      O3=O3+ALOG(ARG3)      
 600  CONTINUE
c      call realpr('O1',2,O1,1)
c      call realpr('O2',2,O2,1)
c      call realpr('O3',2,O3,1)
      EMLL= -(O1+O2-O3)/FLOAT(N)
      RETURN
      END

