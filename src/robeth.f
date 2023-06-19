C
C-----------------------------------------------------------------------
C                 R O B E T H  FORTRAN Source
C
C  File COMUTL.F  Utility subroutines of Chapter 12
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MACHZ(I,X)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C     SINGLE PRECISION VERSION (DOUBLE PRECISION VERSION FOLLOWS)
C     ***********************************************************
C
C  MACHINE PARAMETERS : TO  ALTER  THIS  SUBROUTINE  FOR  A PARTICULAR 
C  ++++++++++++++++++   ENVIRONMENT, THE DESIRED SET OF DATA STATEMENT
C  SHOULD BE ACTIVATED BY REMOVING THE "C" FROM COLUMN ONE  AND ADDING
C  THE "C" FOR THE TWO LINES AFTER "... VAX FORTRAN (V5) compiler".
C
C  RADIX IS ALWAYS EQUAL TO 2
C  PREC CAN BE FOUND BY CALLING THE ROBETH SUBROUTINE "PRECS"
C  EPMACH IS APPROXIMATELY EQUAL TO THE EXPONENT PART OF PREC
C  EXMIN, XLGMN, YLGMN AND XBIG CAN BE FOUND BY TRIAL AND ERROR
C
C  for VAX AND DEC-ALPHA FORTRAN (V5) compiler
C     DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C    *     /2.,5.960465E-8,-88.722,0.2939E-38,-88.7227,1.7E38,1.0E-7/
C  for IBM-PC F77L compiler
C     DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C    *     /2.,5.43E-20,-87.4,0.118E-37,-87.3327,3.4E38,1.0E-19/
C  for IBM-PC MICROSOFT FORTRAN compiler
C     DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C    *     /2.,5.47522E-18,-103.972,0.701E-45,-103.279,.E.,1.0E-17/
C  for WATCOM F77 compiler (32 bits version)
C     DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C    *     /2.,6.020E-8,-87.336,0.0588E-37,-88.029,34.02E37,1.0E-7/
C  for ULTRIX DEC and ALPHA/OSF1 FORTRAN-77 compiler
      DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
     *     /2.,6.02007E-8,-87.336,0.1176E-37,-87.3361,3.401E38,1.0E-7/
C  for SUN FORTRAN Compiler 
c      DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
c    *     /2.,6.02007E-8,-103.972,0.1401E-44,-103.279,3.402E38,1.0E-7/
C  for SILICON GRAPHICS MIPS FORTRAN 77 Compiler 
C      DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C    *     /2.,6.02007E-8,-102.88,4.757E-43,-88.0297,3.395E38,1.0E-7/
C  for HP-UX FORTRAN 77 Compiler 
C      DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C    *     /2.,6.02007E-8,-87.33,0.989E-42,-88.0296,3.401E38,1.0E-7/
C
      IF (I.EQ.1) X=RADIX
      IF (I.EQ.2) X=PREC
      IF (I.EQ.3) X=EXMIN
      IF (I.EQ.4) X=XLGMN
      IF (I.EQ.5) X=YLGMN
      IF (I.EQ.6) X=XBIG
      IF (I.EQ.7) X=EPMACH
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MACHZD(I,X)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C                   DOUBLE PRECISION VERSION
C                   ************************
C
C  MACHINE PARAMETERS : TO  ALTER  THIS  SUBROUTINE  FOR  A PARTICULAR 
C  ++++++++++++++++++   ENVIRONMENT, THE DESIRED SET OF DATA STATEMENT
C  SHOULD BE ACTIVATED BY REMOVING THE "C" FROM COLUMN ONE  AND ADDING
C  THE "C" FOR THE TWO LINES AFTER "... VAX FORTRAN (V5) compiler".
C
C  RADIX IS ALWAYS EQUAL TO 2.D0
C  PREC CAN BE FOUND BY CALLING THE ROBETH SUBROUTINE "PRECD"
C  EPMACH IS APPROXIMATELY EQUAL TO THE EXPONENT PART OF PREC
C  EXMIN, XLGMN, YLGMN AND XBIG CAN BE FOUND BY TRIAL AND ERROR
C
       DOUBLE PRECISION X,RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C  for VAX FORTRAN (V5) compiler (VMS)
C      DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C    *  /2.D0,1.38778D-17,-88.722D0,2.939D-39,-88.7227D0,1.7D38,1.D-17/
C  for IBM-PC F77L compiler
C      DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C    *  /2.D0,5.422D-20,-708.D0,1.D-307,-706.591D0,1.D308,1.D-19/
C  for IBM-PC MICROSOFT FORTRAN compiler
C      DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C    *  /2.D0,5.47522D-18,-745.133D0,0.9D-48,-110.629D0,1.D308,1.D-17/
C  for WATCOM F77 Compiler (32 bits version)
C      DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C    */2.,0.1121D-15,-709.782D0,0.974D-312,-718.433D0,1.797D308,1.0D-17/
C  for ULTRIX DEC and ALPHA/OSF1 FORTRAN-77 compiler
      DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
     */2.,1.12133D-16,-707.9D0,2.226D-308,-708.396D0,1.796D308,1.0D-17/
C  for SUN FORTRAN compiler
c     DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
c    */2.,1.12133D-16,-745.13D0,0.494D-323,-744.44D0,1.797D308,1.0D-17/
C  for SILICON GRAPHICS MIPS FORTRAN 77 Compiler
C      DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C    */2.,1.12133D-16,-744.04D0,0.758D-323,-743.75D0,1.797D308,1.0D-17/
C  for HP-UX FORTRAN 77 Compiler
C      DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C    */2.,1.12133D-16,-708.396D0,0.1D-308,-709.09D0,1.797D308,1.0D-17/
C  for DEC-ALPHA FORTRAN Compiler (OpenVMS)
C      DATA RADIX,PREC,EXMIN,XLGMN,YLGMN,XBIG,EPMACH
C    */2.,1.1102D-16,-709.782D0,0.1057D-45,-105.863D0,0.898D307,1.0D-17/
C
       IF (I.EQ.1) X=RADIX
       IF (I.EQ.2) X=PREC
       IF (I.EQ.3) X=EXMIN
       IF (I.EQ.4) X=XLGMN
       IF (I.EQ.5) X=YLGMN
       IF (I.EQ.6) X=XBIG
       IF (I.EQ.7) X=EPMACH
       RETURN
       END
C
C-----------------------------------------------------------------------
C
      FUNCTION XLOG(X,XMIN,YMIN)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  EXTENDED NATURAL LOGARITHM FUNCTION
C
      IF (X.LE.0.) THEN
        XLOG=0.
      ELSEIF (X.LE.XMIN) THEN
        XLOG=YMIN
      ELSE
        XLOG=ALOG(X)
      ENDIF
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION XEXP(X)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
      DATA NCALL,ZMIN,DMAX,XBIG/0,0.,0.,0./
      IF (NCALL.EQ.0) THEN
        CALL MACHZ(3,ZMIN)
        CALL MACHZ(6,XBIG)
        XBIG=XBIG/10.
        DMAX=ALOG(XBIG)
        NCALL=1
      ENDIF
C
C  EXTENDED EXPONENTIAL FUNCTION
C
      IF (X.LE.ZMIN) THEN
        XEXP=0.
      ELSEIF (X.GE.DMAX) THEN
        XEXP=XBIG
      ELSE
        XEXP=EXP(X)
      ENDIF
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DXLOG(X,XMIN,YMIN)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
      DOUBLE PRECISION X,XMIN,YMIN
C
C  EXTENDED NATURAL LOGARITHM FUNCTION
C
      IF (X.LE.0.D0) THEN
        DXLOG=0.D0
      ELSEIF (X.LE.XMIN) THEN
        DXLOG=YMIN
      ELSE
        DXLOG=DLOG(X)
      ENDIF
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION XEXPD(X)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
      DOUBLE PRECISION X,ZMIN,DMAX,XBIG
      DATA ZMIN,DMAX,XBIG,NCALL/0.D0,0.D0,0.D0,0/
      IF (NCALL.EQ.0) THEN
        CALL MACHZD(3,ZMIN)
        CALL MACHZD(6,XBIG)
        XBIG=XBIG/10.D0
        DMAX=DLOG(XBIG)
        NCALL=1
      ENDIF
C
C  EXTENDED EXPONENTIAL FUNCTION
C
      IF (X.LE.ZMIN) THEN
        XEXPD=0.D0
      ELSEIF (X.GE.DMAX) THEN
        XEXPD=XBIG
      ELSE
        XEXPD=DEXP(X)
      ENDIF
      RETURN
      END
C
C-----------------------------------------------------------------------
C
c     SUBROUTINE ADDC(X,N,NP,MDX,L,J,SH,IP,SX)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. CAMPBELL / A. MARAZZI
C.......................................................................
C
c     REAL X(MDX,NP),SH(NP),SX(N)
c     INTEGER IP(NP)
c     LOGICAL NPRCHK
c     NPRCHK=MDX.GE.N.AND.NP.LE.N.AND.J.LE.NP.AND.J.GT.L.AND.L.GE.0
c     IF (.NOT.NPRCHK) CALL MESSGE(500,'ADDC  ',1)
c     CALL ADDCOL(X,N,NP,MDX,L,J,SH,IP,SX)
c     RETURN
c     END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE ADDCOL(X,N,NP,MDX,L,J,SH,IP,SX)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. CAMPBELL / A. MARAZZI
C.......................................................................
C
      REAL X(MDX,NP),SH(NP),SX(N)
      INTEGER IP(NP)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=MDX.GE.N.AND.NP.LE.N.AND.J.LE.NP.AND.J.GT.L.AND.L.GE.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'ADDCOL',1)
C
C  EXCHANGE COLUMN J WITH COLUMN L+1
C
      LP1=L+1
      IF (J.EQ.LP1) GOTO 20
      CALL SWAPZ(X(1,J),X(1,L+1),N,1,1,MDX,MDX)
      TMP=SH(J)
      SH(J)=SH(L+1)
      SH(L+1)=TMP
      ITMP=IP(J)
      IP(J)=IP(L+1)
      IP(L+1)=ITMP
   20 CONTINUE
C
C  APPLY H.T. DEFINED BY COLS 1...L TO COLUMN L+1
C
      IF (L.EQ.0) GOTO 40
      DO 30 II=1,L
      I=II
      CALL H12Z(2,I,I+1,N,X(1,I),1,SH(I),X(1,L+1),1,N,1,N)
   30 CONTINUE 
   40 CONTINUE
C
C  COMPUTE H.T. FOR COLUMNN L+1
C
      CALL H12Z(1,L+1,L+2,N,X(1,L+1),1,SH(L+1),SX,1,N,0,N)
      L=L+1
      RETURN
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE RMVCZ(X,N,NP,MDX,L,J,SH,IP,SX)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. CAMPBELL / A. MARAZZI
C.......................................................................
C
      REAL X(MDX,NP),SH(NP),SX(N)
      INTEGER IP(NP)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.MDX.GE.N.AND.NP.LE.N
     1       .AND.L.GE.1.AND.J.GE.0.AND.J.LE.L.AND.L.LE.NP
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RMVCz ',1)
      IF (J.EQ.0) RETURN
C
C  APPLY ALREADY COMPUTED H.T. DEFINED BY COLUMN L TO SAME
C  COLUMN L OF THE TRANSFORMED MATRIX (TEMPORARILY STORING
C  THE RESULT IN SX)
C
      DO 20 I=1,L
      SX(I)=X(I,L)
   20 CONTINUE
      LP1=L+1
      DO 30 I=LP1,N
      SX(I)=0.
   30 CONTINUE 
      CALL H12Z(2,L,L+1,N,X(1,L),1,SH(L),SX,1,N,1,N)
      DO 40 I=1,N
      X(I,L)=SX(I)
   40 CONTINUE 
C
C  APPLY ALREADY COMPUTED H.T. DEFINED BY COLS L-1 DOWN TO J
C  TO SUBSEQUENT COLS
C
      IF (J.EQ.L) GOTO 90
      LMJP1=L-J+1
      DO 80 IN=2,LMJP1
      J1=L-IN+1
      CALL H12Z(2,J1,J1+1,N,X(1,J1),1,SH(J1),X(1,J1+1),1,MDX,
     1L-J1,(L-J1)*MDX)
C
C  APPLY ALREADY COMPUTED H.T. DEFINED BY COLS L-1 DOWN TO J
C  TO SAME COLS (TEMPORARILY STORING THE RESULT IN SX)
C
      DO 50 I=1,L
      SX(I)=X(I,J1)
   50 CONTINUE 
      J1P1=J1+1
      DO 60 I=J1P1,N
      SX(I)=0.
   60 CONTINUE
      CALL H12Z(2,J1,J1+1,N,X(1,J1),1,SH(J1),SX,1,N,1,N)
      DO 70 I=1,N
      X(I,J1)=SX(I)
   70 CONTINUE
   80 CONTINUE
C
C  MULTIPLY J-TH COLUMN BY H.T. STORED IN COLS
C  J-1 DOWN TO 1 SO THAT DATA COLUMN IS REGENERATED
C
   90 CONTINUE
      IF (J.EQ.1) GOTO 110
      JM1=J-1
      DO 100 II=1,JM1
      I=J-II
      CALL H12Z(2,I,I+1,N,X(1,I),1,SH(I),X(1,J),1,MDX,1,MDX)
  100 CONTINUE
  110 CONTINUE
      IF (J.EQ.L) GOTO 140
C
C  COLS J+1 UP TO L NOW CONTAIN S, WHICH IS NOW USED
C  TO COMPUTE THE NEW H.T. AND TO PLACE THESE TRANSFORMED COLS IN
C  COLS J UP TO L-1 OF THE ARRAY X
C
      JP1=J+1
      DO 120 II=JP1,L
      I=II
      IF (L-I.GT.0)
     1CALL H12Z(1,I-1,I,N,X(1,I),1,SH(I),X(1,I+1),1,MDX,L-I,(L-I)*MDX)
      IF (L-I.EQ.0)
     1CALL H12Z(1,I-1,I,N,X(1,I),1,SH(I),SX,1,N,0,N)
  120 CONTINUE
      DO 130 I=JP1,L
      CALL SWAPZ(X(1,I-1),X(1,I),N,1,1,MDX,MDX)
      TMP=SH(I-1)
      SH(I-1)=SH(I)
      SH(I)=TMP
      ITMP=IP(I-1)
      IP(I-1)=IP(I)
      IP(I)=ITMP
  130 CONTINUE
  140 L=L-1
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE EXCHZ(S,N,NN,H,K)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL S(NN)
      INTEGER H
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      MM=N*(N+1)/2
      NPRCHK=N.GT.0.AND.H.GT.0.AND.K.GT.H.AND.K.LE.N.AND.MM.EQ.NN
      IF (.NOT.NPRCHK) CALL MESSGE(500,'EXCHz ',1)
C
      LH=H*(H+1)/2
      LK=K*(K+1)/2
      T=S(LH)
      S(LH)=S(LK)
      S(LK)=T
      LH=LH-H
      LK=LK-K
      M=H-1
      IF (M.EQ.0) GOTO 15
      DO 10 I=1,M
      LH=LH+1
      LK=LK+1
      T=S(LH)
      S(LH)=S(LK)
      S(LK)=T
   10 CONTINUE
   15 LH=LH+1
      LK=LK+1
      M=K-H-1
      IF (M.EQ.0) GOTO 30
      DO 20 I=1,M
      LH=LH+H-1+I
      LK=LK+1
      T=S(LH)
      S(LH)=S(LK)
      S(LK)=T
   20 CONTINUE
   30 LH=LH+K-1
      LK=LK+1
      M=N-K
      IF (M.EQ.0) GOTO 45
      DO 40 I=1,M
      LH=LH+K+I-1
      LK=LK+K+I-1
      T=S(LH)
      S(LH)=S(LK)
      S(LK)=T
   40 CONTINUE
   45 RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE EXCHZD(S,N,NN,H,K)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION S(NN),T
      INTEGER H
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      MM=N*(N+1)/2
      NPRCHK=N.GT.0.AND.H.GT.0.AND.K.GT.H.AND.K.LE.N.AND.NN.EQ.MM
      IF (.NOT.NPRCHK) CALL MESSGE(500,'EXCHzD',1)
C
      LH=H*(H+1)/2
      LK=K*(K+1)/2
      T=S(LH)
      S(LH)=S(LK)
      S(LK)=T
      LH=LH-H
      LK=LK-K
      M=H-1
      IF (M.EQ.0) GOTO 15
      DO 10 I=1,M
      LH=LH+1
      LK=LK+1
      T=S(LH)
      S(LH)=S(LK)
      S(LK)=T
   10 CONTINUE
   15 LH=LH+1
      LK=LK+1
      M=K-H-1
      IF (M.EQ.0) GOTO 30
      DO 20 I=1,M
      LH=LH+H-1+I
      LK=LK+1
      T=S(LH)
      S(LH)=S(LK)
      S(LK)=T
   20 CONTINUE
   30 LH=LH+K-1
      LK=LK+1
      M=N-K
      IF (M.EQ.0) GOTO 45
      DO 40 I=1,M
      LH=LH+K+I-1
      LK=LK+K+I-1
      T=S(LH)
      S(LH)=S(LK)
      S(LK)=T
   40 CONTINUE
   45 RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE H12Z(MODE,LPIVOT,L1,M,U,IUE,UP,C,ICE,ICV,NCV,
     1               MDC)
C.......................................................................
C
C   AUTHORS :     CH.L. LAWSON & R.J. HANSON (1974)
C                 SOLVING LEAST SQUARES PROBLEMS 
C                 REPRINT FROM PP.290-291,308 BY PERMISSION OF 
C                 PRENTICE HALL, ENGLEWOOD CLIFFS, NEW JERSEY.
C                 ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      REAL U(IUE,M),C(MDC)
      DOUBLE PRECISION SM,B
      ONE=1.
C
      IF (0.GE.LPIVOT.OR.LPIVOT.GE.L1.OR.L1.GT.M) RETURN
      CL=ABS(U(1,LPIVOT))
      IF (MODE.EQ.2) GOTO 60
C
C  CONSTRUCT THE TRANSFORMATION
C
      DO 10 J=L1,M
      CL=AMAX1(ABS(U(1,J)),CL)
   10 CONTINUE
      IF (CL.LE.0.0) GOTO 130
      CLINV=ONE/CL
      SM=(DBLE(U(1,LPIVOT))*CLINV)**2
      DO 30 J=L1,M
      SM=SM+(DBLE(U(1,J))*CLINV)**2
   30 CONTINUE
C
C  CONVERT DBLE. PRE. SM TO SNGL. PREC. SM1
C
      SM1=SNGL(SM)
      CL=CL*SQRT(SM1)
      IF (U(1,LPIVOT).LE.0.0) GOTO 50
      CL=-CL
   50 UP=U(1,LPIVOT)-CL
      U(1,LPIVOT)=CL
      GOTO 70
C
C  APPLY THE TRANSFORMATION I+U*(U**T)/B TO C
C
   60 IF (CL.LE.0.0) GOTO 130
   70 IF (NCV.LE.0) RETURN
      B=DBLE(UP*U(1,LPIVOT))
C
C  B MUST BE NONPOSITIVE HERE. IF B=0., RETURN.
C
      IF (B.GE.0.D0) GOTO 130
      B=DBLE(ONE)/B
      I2=1-ICV+ICE*(LPIVOT-1)
      INCR=ICE*(L1-LPIVOT)
      DO 120 J=1,NCV
      I2=I2+ICV
      I3=I2+INCR
      I4=I3
      SM=DBLE(C(I2)*UP)
      DO 90 I=L1,M
      SM=SM+DBLE(C(I3)*U(1,I))
      I3=I3+ICE
   90 CONTINUE
      IF (SM.EQ.0.D0) GOTO 120
      SM=SM*B
      C(I2)=C(I2)+SNGL(SM*DBLE(UP))
      DO 110 I=L1,M
      C(I4)=C(I4)+SNGL(SM*DBLE(U(1,I)))
      I4=I4+ICE
  110 CONTINUE
  120 CONTINUE
  130 RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE H12ZD(MODE,LPIVOT,L1,M,U,IUE,UP,C,ICE,ICV,NCV,
     1               MDC)
C.......................................................................
C
C   AUTHORS :     CH.L. LAWSON & R.J. HANSON (1974)
C                 SOLVING LEAST SQUARES PROBLEMS
C                 REPRINT FROM PP.290-291,308 BY PERMISSION OF 
C                 PRENTICE HALL, ENGLEWOOD CLIFFS, NEW JERSEY.
C                 ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      DOUBLE PRECISION U(IUE,M),C(MDC)
      DOUBLE PRECISION SM,B,UP,ONE,CL,CLINV
      ONE=1.D0
C
      IF (0.GE.LPIVOT.OR.LPIVOT.GE.L1.OR.L1.GT.M) RETURN
      CL=DABS(U(1,LPIVOT))
      IF (MODE.EQ.2) GOTO 60
C
C  CONSTRUCT THE TRANSFORMATION
C
      DO 10 J=L1,M
      CL=DMAX1(DABS(U(1,J)),CL)
   10 CONTINUE
      IF (SNGL(CL).LE.0.0) GOTO 130
      CLINV=ONE/CL
      SM=(U(1,LPIVOT)*CLINV)**2
      DO 30 J=L1,M
      SM=SM+(U(1,J)*CLINV)**2
   30 CONTINUE
C
C  CONVERT DBLE. PRE. SM TO SNGL. PREC. SM1
C
C     SM1=SM
      CL=CL*DSQRT(SM)
      IF (SNGL(U(1,LPIVOT)).LE.0.0) GOTO 50
      CL=-CL
   50 UP=U(1,LPIVOT)-CL
      U(1,LPIVOT)=CL
      GOTO 70
C
C  APPLY THE TRANSFORMATION I+U*(U**T)/B TO C
C
   60 IF (SNGL(CL).LE.0.0) GOTO 130
   70 IF (NCV.LE.0) RETURN
      B=UP*U(1,LPIVOT)
C
C  B MUST BE NONPOSITIVE HERE. IF B=0., RETURN.
C
      IF (SNGL(B).GE.0.0) GOTO 130
      B=ONE/B
      I2=1-ICV+ICE*(LPIVOT-1)
      INCR=ICE*(L1-LPIVOT)
      DO 120 J=1,NCV
      I2=I2+ICV
      I3=I2+INCR
      I4=I3
      SM=C(I2)*UP
      DO 90 I=L1,M
      SM=SM+C(I3)*U(1,I)
      I3=I3+ICE
   90 CONTINUE
      IF (SM.EQ.0.D0) GOTO 120
      SM=SM*B
      C(I2)=C(I2)+SM*UP
      DO 110 I=L1,M
      C(I4)=C(I4)+SM*U(1,I)
      I4=I4+ICE
  110 CONTINUE
  120 CONTINUE
  130 RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MHATZ(X,N,NP,K,MDX,HAT,SH,SC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL X(MDX,NP),SH(NP),SC(N),HAT(N)
      LOGICAL NPRCHK
      DOUBLE PRECISION SM,DZERO
C
C  PARAMETER CHECK AND INITIALIZATION
C
      DZERO=0.D0
      NPRCHK=K.GT.0.AND.K.LE.NP.AND.NP.LE.N.AND.MDX.GE.N
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MHATz ',1)
C
      DO 100 I=1,N
      DO  20 J=1,N
      SC(J)=0.
   20 CONTINUE
      SC(I)=1.
      DO 50 JJ=1,NP
      J=JJ
      CALL H12Z(2,J,J+1,N,X(1,J),1,SH(J),SC,1,N,1,N)
   50 CONTINUE
      SM=DZERO
      DO 70 J=1,K
      SM=SM+DBLE(SC(J)*SC(J))
   70 CONTINUE
      HAT(I)=SNGL(SM)
  100 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MINVZ(R,N,NN,TAU,ISING)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL R(NN)
      DOUBLE PRECISION SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2).AND.TAU.GE.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MINVz ',1)
C
      DZERO=0.D0
      ISING=0
      I1=0
      DO 10 I=1,N
      I1=I1+I
      IF (ABS(R(I1)).LE.TAU) GOTO 900
      R(I1)=1./R(I1)
   10 CONTINUE
      IF (N.EQ.1) RETURN
      I1=0
      NM1=N-1
      DO 40 I=1,NM1
      I1=I1+I
      J1=I1+I
      IP1=I+1
      DO 30 J=IP1,N
      SM=DZERO
      IL=I1
      LJ=J1
      JM1=J-1
      DO 20 L=I,JM1
      SM=SM+DBLE(R(IL)*R(LJ))
      LJ=LJ+1
      IL=IL+L
   20 CONTINUE
      R(J1)=-R(LJ)*SNGL(SM)
      J1=J1+J
   30 CONTINUE
   40 CONTINUE
      RETURN
  900 ISING=1
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MINVZD(R,N,NN,TAU,ISING)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION R(NN),SM,DZERO,DTAU
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2).AND.TAU.GE.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MINVzD',1)
C
      DZERO=0.D0
      DTAU=DBLE(TAU)
      ISING=0
      I1=0
      DO 10 I=1,N
      I1=I1+I
      IF (DABS(R(I1)).LE.DTAU) GOTO 900
      R(I1)=1.D0/R(I1)
   10 CONTINUE
      IF (N.EQ.1) RETURN
      I1=0
      NM1=N-1
      DO 40 I=1,NM1
      I1=I1+I
      J1=I1+I
      IP1=I+1
      DO 30 J=IP1,N
      SM=DZERO
      IL=I1
      LJ=J1
      JM1=J-1
      DO 20 L=I,JM1
      SM=SM+R(IL)*R(LJ)
      LJ=LJ+1
      IL=IL+L
   20 CONTINUE
      R(J1)=-R(LJ)*SM
      J1=J1+J
   30 CONTINUE
   40 CONTINUE
      RETURN
  900 ISING=1
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MCHLZ(A,N,NN,INFO)
C.......................................................................
C
C   COPYRIGHT 1979 SOCIETY FOR INDUSTRIAL AND APPLIED MATHEMATICS.
C   ALL RIGHTS RESERVED.
C
C   AUTHOR :     LINPACK (SUBROUTINE SPPFA)
C                REPRINTED WITH PERMISSION FROM 
C                LINPACK USER'S GUIDE.
C                ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      REAL A(NN)
      DOUBLE PRECISION S
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MCHLz ',1)
C
      JJ=0
      DO 30 J=1,N
      INFO=J
      S=0.D0
      JM1=J-1
      KJ=JJ
      KK=0
      IF (JM1.LT.1) GOTO 20
      DO 10 K=1,JM1
      KJ=KJ+1
      CALL DOTPZ(A(KK+1),A(JJ+1),K-1,1,1,NN-KK,NN-JJ,DTP)
      T=A(KJ)-DTP
      KK=KK+K
      T=T/A(KK)
      A(KJ)=T
      S=S+T*DBLE(T)
   10 CONTINUE
   20 CONTINUE
      JJ=JJ+J
      S=DBLE(A(JJ))-S
      IF (S.LE.0.D0) GOTO 40
      A(JJ)=SNGL(DSQRT(S))
   30 CONTINUE
      INFO=0
   40 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MCHLZD(A,N,NN,INFO)
C.......................................................................
C
C   COPYRIGHT 1979 SOCIETY FOR INDUSTRIAL AND APPLIED MATHEMATICS.
C   ALL RIGHTS RESERVED.
C
C   AUTHOR :     LINPACK (SUBROUTINE SPPFA)
C                REPRINTED WITH PERMISSION FROM 
C                LINPACK USER'S GUIDE.
C                ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      DOUBLE PRECISION A(NN),S,T,DTP
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MCHLzD',1)
C
      JJ=0
      DO 30 J=1,N
      INFO=J
      S=0.D0
      JM1=J-1
      KJ=JJ
      KK=0
      IF (JM1.LT.1) GOTO 20
      DO 10 K=1,JM1
      KJ=KJ+1
      CALL DOTPZD(A(KK+1),A(JJ+1),K-1,1,1,NN-KK,NN-JJ,DTP)
      T=A(KJ)-DTP
      KK=KK+K
      T=T/A(KK)
      A(KJ)=T
      S=S+T*T
   10 CONTINUE
   20 CONTINUE
      JJ=JJ+J
      S=A(JJ)-S
      IF (S.LE.0.D0) GOTO 40
      A(JJ)=DSQRT(S)
   30 CONTINUE
      INFO=0
   40 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MFYZ(A,Y,Z,M,N,MDA,NY,IYE,NZ,IZE)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL A(MDA,N),Y(NY),Z(NZ)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      LY=1+(N-1)*IYE
      LZ=1+(M-1)*IZE
      NPRCHK=M.GT.0.AND.N.GT.0.AND.MDA.GE.M.AND.NY.GE.LY
     1       .AND.NZ.GE.LZ.AND.IYE.GE.1.AND.IZE.GE.1
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MFYz  ',1)
      NA1=(N-1)*MDA+1
C
      IZ=-IZE+1
      DO 20 I=1,M
      IZ=IZ+IZE
      CALL DOTPZ(A(I,1),Y,N,MDA,IYE,NA1,NY,R)
      Z(IZ)=R
   20 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MFYZD(A,Y,Z,M,N,MDA,NY,IYE,NZ,IZE)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION A(MDA,N),Y(NY),Z(NZ),R
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      LY=1+(N-1)*IYE
      LZ=1+(M-1)*IZE
      NPRCHK=M.GT.0.AND.N.GT.0.AND.MDA.GE.M.AND.NY.GE.LY
     1       .AND.NZ.GE.LZ.AND.IYE.GE.1.AND.IZE.GE.1
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MFYzD ',1)
      NA1=(N-1)*MDA+1
C
      IZ=-IZE+1
      DO 20 I=1,M
      IZ=IZ+IZE
      CALL DOTPZD(A(I,1),Y,N,MDA,IYE,NA1,NY,R)
      Z(IZ)=R
   20 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MFFZ(A,B,C,M,K,N,MDA,MDB,MDC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL A(MDA,K),B(MDB,N),C(MDC,N)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=M.GT.0.AND.N.GT.0.AND.K.GT.0.AND.MDA.GE.M.AND.MDB.GE.K
     1       .AND.MDC.GE.M
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MFFz  ',1)
      NA1=(K-1)*MDA+1
C
      DO 30 I=1,M
      DO 20 J=1,N
      CALL DOTPZ(A(I,1),B(1,J),K,MDA,1,NA1,K,R)
      C(I,J)=R
   20 CONTINUE
   30 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MFFZD(A,B,C,M,K,N,MDA,MDB,MDC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION A(MDA,K),B(MDB,N),C(MDC,N),R
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=M.GT.0.AND.N.GT.0.AND.K.GT.0.AND.MDA.GE.M.AND.MDB.GE.K
     1       .AND.MDC.GE.M
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MFFzD ',1)
      NA1=(K-1)*MDA+1
C
      DO 30 I=1,M
      DO 20 J=1,N
      CALL DOTPZD(A(I,1),B(1,J),K,MDA,1,NA1,K,R)
      C(I,J)=R
   20 CONTINUE
   30 CONTINUE
      RETURN
      END
C
C------------------------------------------------------------------------
C
      SUBROUTINE MSFZ(A,B,C,N,NN,M,MDB,MDC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL A(NN),B(MDB,M),C(MDC,M)
      DOUBLE PRECISION SM,DZERO
      INTEGER H
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      N2=N*(N+1)/2
      NPRCHK=N.GT.0.AND.M.GT.0.AND.MDB.GE.N.AND.MDC.GE.N.AND.NN.EQ.N2
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MSFz  ',1)
C
      DZERO=0.D0
      DO 20 J=1,M
      H=1
      DO 15 I=1,N
      L=H
      KK=1
      SM=DZERO
      DO 10 K=1,N
      SM=SM+DBLE(A(L))*B(K,J)
      IF (K.GE.I) KK=K
      L=L+KK
   10 CONTINUE
      C(I,J)=SNGL(SM)
      H=H+I
   15 CONTINUE
   20 CONTINUE
      RETURN
      END
C
C------------------------------------------------------------------------
C
      SUBROUTINE MSFZD(A,B,C,N,NN,M,MDB,MDC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION A(NN),B(MDB,M),C(MDC,M),SM,DZERO
      INTEGER H
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      N2=N*(N+1)/2
      NPRCHK=N.GT.0.AND.M.GT.0.AND.MDB.GE.N.AND.MDC.GE.N.AND.NN.EQ.N2
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MSFzD ',1)
C
      DZERO=0.D0
      DO 20 J=1,M
      H=1
      DO 15 I=1,N
      L=H
      KK=1
      SM=DZERO
      DO 10 K=1,N
      SM=SM+A(L)*B(K,J)
      IF (K.GE.I) KK=K
      L=L+KK
   10 CONTINUE
      C(I,J)=SM
      H=H+I
   15 CONTINUE
   20 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MSF1Z(A,B,C,N,NN,MDB)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL A(NN),B(MDB,N),C(NN)
      LOGICAL NPRCHK
      DOUBLE PRECISION SM,DZERO
C
C  PARAMETER CHECK
C
      N2=N*(N+1)/2
      NPRCHK=N.GT.0.AND.MDB.GE.N.AND.NN.EQ.N2
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MSF1z ',1)
C
      DZERO=0.D0
      LC=1
      DO 15 J=1,N
      IBEG=1
      DO 10 I=1,J
      L=IBEG
      KK=1
      SM=DZERO
      DO 5 K=1,N
      SM=SM+DBLE(A(L))*B(K,J)
      IF (K.GE.I) KK=K
      L=L+KK
    5 CONTINUE
      C(LC)=SNGL(SM)
      LC=LC+1
      IBEG=IBEG+I
   10 CONTINUE
   15 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MSF1ZD(A,B,C,N,NN,MDB)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION A(NN),B(MDB,N),C(NN),SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      N2=N*(N+1)/2
      NPRCHK=N.GT.0.AND.MDB.GE.N.AND.NN.EQ.N2
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MSF1zD',1)
C
      DZERO=0.D0
      LC=1
      DO 15 J=1,N
      IBEG=1
      DO 10 I=1,J
      L=IBEG
      KK=1
      SM=DZERO
      DO 5 K=1,N
      SM=SM+A(L)*B(K,J)
      IF (K.GE.I) KK=K
      L=L+KK
    5 CONTINUE
      C(LC)=SM
      LC=LC+1
      IBEG=IBEG+I
   10 CONTINUE
   15 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MSSZ(A,B,C,N,NN,MDC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL A(NN),B(NN),C(MDC,N)
      DOUBLE PRECISION SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      N2=N*(N+1)/2
      NPRCHK=N.GT.0.AND.MDC.GE.N.AND.NN.EQ.N2
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MSSz  ',1)
C
      DZERO=0.D0
      LI=1
      DO 60 IR=1,N
      LJ=1
      DO 50 JC=1,N
      I=LI
      J=LJ
      SM=DZERO
      DO 40 K=1,N
      SM=DBLE(A(I))*B(J)+SM
      IF (K.LT.IR) GOTO 10
      I=I+K
      GOTO 20
   10 I=I+1
   20 IF (K.LT.JC) GOTO 30
      J=J+K
      GOTO 40
   30 J=J+1
   40 CONTINUE
      C(IR,JC)=SNGL(SM)
      LJ=LJ+JC
   50 CONTINUE
      LI=LI+IR
   60 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MSSZD(A,B,C,N,NN,MDC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION A(NN),B(NN),C(MDC,N),SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      N2=N*(N+1)/2
      NPRCHK=N.GT.0.AND.MDC.GE.N.AND.NN.EQ.N2
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MSSzD ',1)
C
      DZERO=0.D0
      LI=1
      DO 60 IR=1,N
      LJ=1
      DO 50 JC=1,N
      I=LI
      J=LJ
      SM=DZERO
      DO 40 K=1,N
      SM=A(I)*B(J)+SM
      IF (K.LT.IR) GOTO 10
      I=I+K
      GOTO 20
   10 I=I+1
   20 IF (K.LT.JC) GOTO 30
      J=J+K
      GOTO 40
   30 J=J+1
   40 CONTINUE
      C(IR,JC)=SM
      LJ=LJ+JC
   50 CONTINUE
      LI=LI+IR
   60 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MTT1Z(A,B,N,NN)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / R. DUTTER
C.......................................................................
C
      REAL A(NN),B(NN)
      DOUBLE PRECISION SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MTT1Z ',1)
C
      DZERO=0.D0
      IJ=0
      JJ=0
      DO 30 J=1,N
      DO 20 I=1,J
      IJ=IJ+1
      SM=DZERO
      IL=JJ+I
      JL=JJ+J
      DO 10 L=J,N
      SM=SM+A(IL)*DBLE(A(JL))
      IL=IL+L
      JL=JL+L
   10 CONTINUE 
      B(IJ)=SNGL(SM)
   20 CONTINUE
      JJ=JJ+J
   30 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MTT1ZD(A,B,N,NN)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / R. DUTTER / A.RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION  A(NN),B(NN),SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MTT1zD',1)
C
      DZERO=0.D0
      IJ=0
      JJ=0
      DO 30 J=1,N
      DO 20 I=1,J
      IJ=IJ+1
      SM=DZERO
      IL=JJ+I
      JL=JJ+J
      DO 10 L=J,N
      SM=SM+A(IL)*A(JL)
      IL=IL+L
      JL=JL+L
   10 CONTINUE
      B(IJ)=SM
   20 CONTINUE
      JJ=JJ+J
   30 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MTT2Z(A,B,N,NN)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / R. DUTTER
C.......................................................................
C
      REAL A(NN),B(NN)
      DOUBLE PRECISION SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MTT2Z ',1)
C
      DZERO=0.D0
      JJ=NN+N+1
      DO 30 J=N,1,-1
      JJ=JJ-(J+1)
      IAT=JJ+1
      DO 20 I=1,J
      IB=JJ+1-I
      IA=IB+1
      SM=DZERO
      DO 10 L=1,(J-I+1)
      IA=IA-1
      IAT=IAT-1
      SM=SM+A(IA)*DBLE(A(IAT))
   10 CONTINUE
      B(IB)=SNGL(SM)
   20 CONTINUE
   30 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MTT2ZD(A,B,N,NN)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / R. DUTTER / A.RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION A(NN),B(NN),SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MTT2zD',1)
C
      DZERO=0.D0
      JJ=NN+N+1
      DO 30 J=N,1,-1
      JJ=JJ-(J+1)
      IAT=JJ+1
      DO 20 I=1,J
      IB=JJ+1-I
      IA=IB+1
      SM=DZERO
      DO 10 L=1,(J-I+1)
      IA=IA-1
      IAT=IAT-1
      SM=SM+A(IA)*A(IAT)
   10 CONTINUE
      B(IB)=SM
   20 CONTINUE
   30 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MTT3Z(A,B,C,N,NN)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / R. DUTTER
C.......................................................................
C
      REAL A(NN),B(NN),C(NN)
      DOUBLE PRECISION SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MTT3Z ',1)
C
      DZERO=0.D0
      IC=0
      JJ=0
      DO 30 J=1,N
      II=0
      DO 20 I=1,J
      II=II+I
      IL=II
      IC=IC+1
      SM=DZERO
      DO 10 L=I,J
      JL=JJ+L
      SM=SM+A(IL)*DBLE(B(JL))
      IL=IL+L
   10 CONTINUE
      C(IC)=SNGL(SM)
   20 CONTINUE
      JJ=JJ+J
   30 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MTT3ZD(A,B,C,N,NN)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / R. DUTTER / A.RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION  A(NN),B(NN),C(NN),SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MTT3ZD',1)
C
      DZERO=0.D0
      IC=0
      JJ=0
      DO 30 J=1,N
      II=0
      DO 20 I=1,J
      II=II+I
      IL=II
      IC=IC+1
      SM=DZERO
      DO 10 L=I,J
      JL=JJ+L
      SM=SM+A(IL)*B(JL)
      IL=IL+L
   10 CONTINUE
      C(IC)=SM
   20 CONTINUE
      JJ=JJ+J
   30 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MTYZ(A,Y,N,NN,NY,IYE)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL A(NN),Y(NY)
      DOUBLE PRECISION SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
      NPRCHK=NPRCHK.AND.IYE.GT.0.AND.(NY.GE.IYE*(N-1)+1)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MTYz  ',1)
C
      DZERO=0.D0
      IA1=0
      IY1=-IYE+1
      DO 20 J=1,N
      IA=IA1+1
      IY=IY1
      SM=DZERO
      DO 10 I=J,N
      IA=IA+I-1
      IY=IY+IYE
      SM=SM+A(IA)*DBLE(Y(IY))
   10 CONTINUE
      IA1=IA1+J
      IY1=IY1+IYE
      Y(IY1)=SNGL(SM)
   20 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MTYZD(A,Y,N,NN,NY,IYE)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION A(NN),Y(NY),SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
      NPRCHK=NPRCHK.AND.IYE.GT.0.AND.(NY.GE.IYE*(N-1)+1)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MTYzD ',1)
C
      DZERO=0.D0
      IA1=0
      IY1=-IYE+1
      DO 20 J=1,N
      IA=IA1+1
      IY=IY1
      SM=DZERO
      DO 10 I=J,N
      IA=IA+I-1
      IY=IY+IYE
      SM=SM+A(IA)*Y(IY)
   10 CONTINUE
      IA1=IA1+J
      IY1=IY1+IYE
      Y(IY1)=SM
   20 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MLYZ(A,Y,N,NN,NY,IYE)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL A(NN),Y(NY)
      LOGICAL NPRCHK
      DOUBLE PRECISION SM,DZERO
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
     1       .AND.IYE.GT.0.AND.(NY.GE.IYE*(N-1)+1)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MLYz  ',1)
C
      DZERO=0.D0
      IA=NN
      IY1=N*IYE+1
      DO 20 J1=1,N
      J=N-J1+1
      IY1=IY1-IYE
      IY=IY1
      SM=DZERO
      DO 10 I=1,J
      SM=SM+A(IA)*DBLE(Y(IY))
      IA=IA-1
      IY=IY-IYE
   10 CONTINUE
      Y(IY1)=SNGL(SM)
   20 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MLYZD(A,Y,N,NN,NY,IYE)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION A(NN),Y(NY),SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.NN.EQ.(N*(N+1)/2)
     1       .AND.IYE.GT.0.AND.(NY.GE.IYE*(N-1)+1)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MLYzD ',1)
C
      DZERO=0.D0
      IA=NN
      IY1=N*IYE+1
      DO 20 J1=1,N
      J=N-J1+1
      IY1=IY1-IYE
      IY=IY1
      SM=DZERO
      DO 10 I=1,J
      SM=SM+A(IA)*Y(IY)
      IA=IA-1
      IY=IY-IYE
   10 CONTINUE
      Y(IY1)=SM
   20 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DOTPZ(X,Y,N,INCX,INCY,NX,NY,RESULT)
C.......................................................................
C
C   COPYRIGHT 1979 SOCIETY FOR INDUSTRIAL AND APPLIED MATHEMATICS.
C   ALL RIGHTS RESERVED.
C
C   AUTHOR :     LINPACK (SUBROUTINE SDOT)
C                REPRINTED WITH PERMISSION FROM 
C                LINPACK USER'S GUIDE.
C                ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      REAL X(NX),Y(NY)
      DOUBLE PRECISION DTEMP
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=INCX.NE.0.AND.IABS(INCX)*(N-1)+1.LE.NX
     1       .AND.INCY.NE.0.AND.IABS(INCY)*(N-1)+1.LE.NY
      IF (.NOT.NPRCHK) CALL MESSGE(500,'DOTPz ',1)
C
      DTEMP=0.D0
      RESULT=0.
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1.AND.INCY.EQ.1) GOTO 20
C
C  CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL TO 1
C
      IX=1
      IY=1
      IF (INCX.LT.0) IX=(-N+1)*INCX+1
      IF (INCY.LT.0) IY=(-N+1)*INCY+1
      DO 10 I=1,N
      DTEMP=DTEMP+DBLE(X(IX)*Y(IY))
      IX=IX+INCX
      IY=IY+INCY
   10 CONTINUE
      RESULT=SNGL(DTEMP)
      RETURN
C
C  CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 M=MOD(N,5)
      IF (M.EQ.0) GOTO 40
      DO 30 I=1,M
      DTEMP=DTEMP+X(I)*DBLE(Y(I))
   30 CONTINUE
      IF (N.LT.5) GOTO 60
   40 MP1=M+1
      DO 50 I=MP1,N,5
      DTEMP=DTEMP+X(I)*DBLE(Y(I))+X(I+1)*DBLE(Y(I+1))+
     1      X(I+2)*DBLE(Y(I+2))+X(I+3)*DBLE(Y(I+3))+
     1      X(I+4)*DBLE(Y(I+4))
   50 CONTINUE
   60 RESULT=SNGL(DTEMP)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DOTPZD(X,Y,N,INCX,INCY,NX,NY,RESULT)
C.......................................................................
C
C   COPYRIGHT 1979 SOCIETY FOR INDUSTRIAL AND APPLIED MATHEMATICS.
C   ALL RIGHTS RESERVED.
C
C   AUTHOR :     LINPACK (SUBROUTINE SDOT)
C                REPRINTED WITH PERMISSION FROM 
C                LINPACK USER'S GUIDE.
C                ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      DOUBLE PRECISION X(NX),Y(NY),RESULT,DTEMP
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=INCX.NE.0.AND.IABS(INCX)*(N-1)+1.LE.NX
     1       .AND.INCY.NE.0.AND.IABS(INCY)*(N-1)+1.LE.NY
      IF (.NOT.NPRCHK) CALL MESSGE(500,'DOTPzD',1)
C
      DTEMP=0.D0
      RESULT=0.D0
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1.AND.INCY.EQ.1) GOTO 20
C
C  CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL TO 1
C
      IX=1
      IY=1
      IF (INCX.LT.0) IX=(-N+1)*INCX+1
      IF (INCY.LT.0) IY=(-N+1)*INCY+1
      DO 10 I=1,N
      DTEMP=DTEMP+X(IX)*Y(IY)
      IX=IX+INCX
      IY=IY+INCY
   10 CONTINUE
      RESULT=DTEMP
      RETURN
C
C  CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 M=MOD(N,5)
      IF (M.EQ.0) GOTO 40
      DO 30 I=1,M
      DTEMP=DTEMP+X(I)*Y(I)
   30 CONTINUE
      IF (N.LT.5) GOTO 60
   40 MP1=M+1
      DO 50 I=MP1,N,5
      DTEMP=DTEMP+X(I)*Y(I)+X(I+1)*Y(I+1)+
     1      X(I+2)*Y(I+2)+X(I+3)*Y(I+3)+X(I+4)*Y(I+4)
   50 CONTINUE
   60 RESULT=DTEMP
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE NRM2Z(X,N,INCX,MDX,XNRM)
C.......................................................................
C
C   COPYRIGHT 1979 SOCIETY FOR INDUSTRIAL AND APPLIED MATHEMATICS.
C   ALL RIGHTS RESERVED.
C
C   AUTHOR :     LINPACK (SUBROUTINE SNRM2)
C                REPRINTED WITH PERMISSION FROM 
C                LINPACK USER'S GUIDE.
C                ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      LOGICAL NPRCHK
      REAL X(MDX)
      DOUBLE PRECISION SUM,ZERO,ONE,XMAX,DXI
C     DATA ZERO,ONE/0.0D0,1.0D0/,CUTLO,CUTHI/4.441E-16,1.304E19/
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,CUTLO=4.441E-16,CUTHI=1.304E19)
C
C  PARAMETER CHECK
C
      NPRCHK=INCX.GT.0.AND.INCX*(N-1)+1.LE.MDX
      IF (.NOT.NPRCHK) CALL MESSGE(500,'NRM2z ',1)
C
      IF (N.GT.0) GOTO 10
      XNRM=0.
      GOTO 300
C
   10 NEXT=1  !ASSIGN 30 TO NEXT
      SUM=ZERO
      NN=N*INCX
C
C  BEGIN MAIN LOOP
C
      XMAX=ZERO
      I=1
   20 DXI=DBLE(X(I))
C     GOTO (30,50,70,110) NEXT
      IF (NEXT.EQ.2) GOTO 50
      IF (NEXT.EQ.3) GOTO 70
      IF (NEXT.EQ.4) GOTO 110
      IF (ABS(X(I)).GT.CUTLO) GOTO 85
      NEXT=2  !ASSIGN 50 TO NEXT
      XMAX=ZERO
C
C  PHASE1.  SUM IS ZERO
C
   50 IF (X(I).EQ.0.) GOTO 200
      IF (ABS(X(I)).GT.CUTLO) GOTO 85
C
C  PREPARE FOR PHASE 2.
C
      NEXT=3   !ASSIGN 70 TO NEXT
      GOTO 105
C
C  PREPARE FOR PHASE 4.
C
  100 I=J
      DXI=DBLE(X(I))
      NEXT=4   !ASSIGN 110 TO NEXT
      SUM=(SUM/DXI)/DXI
  105 XMAX=DABS(DXI)
      GOTO 115
C
C  PHASE 2.  SUM IS SMALL. SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
C
   70 IF (ABS(X(I)).GT.CUTLO) GOTO 75
C
C  COMMON CODE FOR PHASE 2 AND 4.
C  IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
C
  110 IF (DABS(DXI).LE.XMAX) GOTO 115
      SUM=ONE+SUM*(XMAX/DXI)**2
      XMAX=DABS(DXI)
      GOTO 200
C
  115 SUM=SUM+(DXI/XMAX)**2
      GOTO 200
C
C  PREPARE FOR PHASE 3.
C
   75 SUM=(SUM*XMAX)*XMAX
C
C  SET HITEST=CUTHI/N
C
   85 HITEST=CUTHI/FLOAT(N)
C
C  PHASE3.  SUM IS MID-RANGE.  NO SCALING.
C
      DO 95 J=I,NN,INCX
      IF (ABS(X(J)).GE.HITEST) GOTO 100
      SUM=SUM+X(J)*DBLE(X(J))
   95 CONTINUE
      XNRM=SNGL(DSQRT(SUM))
      GOTO 300
C
  200 CONTINUE
      I=I+INCX
      IF (I.LE.NN) GOTO 20
C
C  END MAIN LOOP
C
      XNRM=SNGL(XMAX*DSQRT(SUM))
  300 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE NRM2ZD(X,N,INCX,MDX,XNRM)
C.......................................................................
C
C   COPYRIGHT 1979 SOCIETY FOR INDUSTRIAL AND APPLIED MATHEMATICS.
C   ALL RIGHTS RESERVED.
C
C   AUTHOR :     LINPACK (SUBROUTINE SNRM2Z)
C                REPRINTED WITH PERMISSION FROM 
C                LINPACK USER'S GUIDE.
C                ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      LOGICAL NPRCHK
      DOUBLE PRECISION X(MDX),XNRM,SUM,ZERO,ONE,XMAX,DXI,CUTLO,CUTHI,
     +                 HITEST
C     DATA ZERO,ONE/0.0D0,1.0D0/,CUTLO,CUTHI/4.441D-16,1.304D19/
      PARAMETER (ZERO=0.0D0,ONE=1.0D0,CUTLO=4.441D-16,CUTHI=1.304D19)
C
C  PARAMETER CHECK
C
      NPRCHK=INCX.GT.0.AND.INCX*(N-1)+1.LE.MDX
      IF (.NOT.NPRCHK) CALL MESSGE(500,'NRM2zD ',1)
C
      IF (N.GT.0) GOTO 10
      XNRM=0.D0
      GOTO 300
C
   10 NEXT=1  !ASSIGN 30 TO NEXT
      SUM=ZERO
      NN=N*INCX
C
C  BEGIN MAIN LOOP
C
      XMAX=ZERO
      I=1
   20 DXI=X(I)
C     GOTO (30,50,70,110) NEXT
      IF (NEXT.EQ.2) GOTO 50
      IF (NEXT.EQ.3) GOTO 70
      IF (NEXT.EQ.4) GOTO 110
      IF (DABS(X(I)).GT.CUTLO) GOTO 85
      NEXT=2    !ASSIGN 50 TO NEXT
      XMAX=ZERO
C
C  PHASE1.  SUM IS ZERO
C
   50 IF (X(I).EQ.ZERO) GOTO 200
      IF (DABS(X(I)).GT.CUTLO) GOTO 85
C
C  PREPARE FOR PHASE 2.
C
      NEXT=3   !ASSIGN 70 TO NEXT
      GOTO 105
C
C  PREPARE FOR PHASE 4.
C
  100 I=J
      DXI=X(I)
      NEXT=4    !ASSIGN 110 TO NEXT
      SUM=(SUM/DXI)/DXI
  105 XMAX=DABS(DXI)
      GOTO 115
C
C  PHASE 2.  SUM IS SMALL. SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
C
   70 IF (DABS(X(I)).GT.CUTLO) GOTO 75
C
C  COMMON CODE FOR PHASE 2 AND 4.
C  IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
C
  110 IF (DABS(DXI).LE.XMAX) GOTO 115
      SUM=ONE+SUM*(XMAX/DXI)**2
      XMAX=DABS(DXI)
      GOTO 200
C
  115 SUM=SUM+(DXI/XMAX)**2
      GOTO 200
C
C  PREPARE FOR PHASE 3.
C
   75 SUM=(SUM*XMAX)*XMAX
C
C  SET HITEST=CUTHI/N
C
   85 HITEST=CUTHI/DBLE(N)
C
C  PHASE3.  SUM IS MID-RANGE.  NO SCALING.
C
      DO 95 J=I,NN,INCX
      IF (DABS(X(J)).GE.HITEST) GOTO 100
      SUM=SUM+X(J)*X(J)
   95 CONTINUE
      XNRM=DSQRT(SUM)
      GOTO 300
C
  200 CONTINUE
      I=I+INCX
      IF (I.LE.NN) GOTO 20
C
C  END MAIN LOOP
C
      XNRM=XMAX*DSQRT(SUM)
  300 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE XSYZ(X,Y,S,N,NN,RESULT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL X(N),Y(N),S(NN)
      DOUBLE PRECISION SM
C
C  PARAMETER CHECK
C
      NS=N*(N+1)/2
      IF (N.LE.0.OR.NN.NE.NS) CALL MESSGE(500,'XSYz  ',1)
C
      SM=0.D0
      L=0
      DO 30 I=1,N
      L=L+I
      L1=L-I+1
      K=0
      DO 20 J=L1,L
      K=K+1
      IF (J.EQ.L) THEN
        SM=SM+DBLE(S(J))*X(I)*Y(I)
      ELSE
        SM=SM+DBLE(S(J))*(X(I)*Y(K)+X(K)*Y(I))
      ENDIF
   20 CONTINUE
   30 CONTINUE
      RESULT=SNGL(SM)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE XSYZD(X,Y,S,N,NN,RESULT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION X(N),Y(N),S(NN),RESULT,SM
C
C  PARAMETER CHECK
C
      NS=N*(N+1)/2
      IF (N.LE.0.OR.NN.NE.NS) CALL MESSGE(500,'XSYzD ',1)
C
      SM=0.D0
      L=0
      DO 30 I=1,N
      L=L+I
      L1=L-I+1
      K=0
      DO 20 J=L1,L
      K=K+1
      IF (J.EQ.L) THEN
        SM=SM+S(J)*X(I)*Y(I)
      ELSE
        SM=SM+S(J)*(X(I)*Y(K)+X(K)*Y(I))
      ENDIF
   20 CONTINUE
   30 CONTINUE
      RESULT=SM
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SCALZ(X,SA,N,INCX,MDX)
C.......................................................................
C
C   COPYRIGHT 1979 SOCIETY FOR INDUSTRIAL AND APPLIED MATHEMATICS.
C   ALL RIGHTS RESERVED.
C
C   AUTHOR :     LINPACK (SUBROUTINE SSCAL)
C                REPRINTED WITH PERMISSION FROM 
C                LINPACK USER'S GUIDE.
C                ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      REAL X(MDX)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=INCX.GT.0.AND.N.GE.0.AND.INCX*(N-1)+1.LE.MDX
      IF (.NOT.NPRCHK) CALL MESSGE(500,'SCALz ',1)
C
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1) GOTO 20
C
C  CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX=N*INCX
      DO 10 I=1,NINCX,INCX
      X(I)=SA*X(I)
   10 CONTINUE
      RETURN
C
C  CODE FOR INCREMENT EQUAL TO 1
C
   20 M=MOD(N,5)
      IF (M.EQ.0) GOTO 40
      DO 30 I=1,M
      X(I)=SA*X(I)
   30 CONTINUE
      IF (N.LT.5) RETURN
   40 MP1=M+1
      DO 50 I=MP1,N,5
      X(I)=SA*X(I)
      X(I+1)=SA*X(I+1)
      X(I+2)=SA*X(I+2)
      X(I+3)=SA*X(I+3)
      X(I+4)=SA*X(I+4)
   50 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SCALZD(X,SA,N,INCX,MDX)
C.......................................................................
C
C   COPYRIGHT 1979 SOCIETY FOR INDUSTRIAL AND APPLIED MATHEMATICS.
C   ALL RIGHTS RESERVED.
C
C   AUTHOR :     LINPACK (SUBROUTINE SSCAL)
C                REPRINTED WITH PERMISSION FROM 
C                LINPACK USER'S GUIDE.
C                ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      DOUBLE PRECISION X(MDX),SA
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=INCX.GT.0.AND.N.GE.0.AND.INCX*(N-1)+1.LE.MDX
      IF (.NOT.NPRCHK) CALL MESSGE(500,'SCALzD',1)
C
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1) GOTO 20
C
C  CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX=N*INCX
      DO 10 I=1,NINCX,INCX
      X(I)=SA*X(I)
   10 CONTINUE
      RETURN
C
C  CODE FOR INCREMENT EQUAL TO 1
C
   20 M=MOD(N,5)
      IF (M.EQ.0) GOTO 40
      DO 30 I=1,M
      X(I)=SA*X(I)
   30 CONTINUE
      IF (N.LT.5) RETURN
   40 MP1=M+1
      DO 50 I=MP1,N,5
      X(I)=SA*X(I)
      X(I+1)=SA*X(I+1)
      X(I+2)=SA*X(I+2)
      X(I+3)=SA*X(I+3)
      X(I+4)=SA*X(I+4)
   50 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SWAPZ(X,Y,N,INCX,INCY,MDX,MDY)
C.......................................................................
C
C   COPYRIGHT 1979 SOCIETY FOR INDUSTRIAL AND APPLIED MATHEMATICS.
C   ALL RIGHTS RESERVED.
C
C   AUTHOR :     LINPACK (SUBROUTINE SSWAP)
C                REPRINTED WITH PERMISSION FROM 
C                LINPACK USER'S GUIDE.
C                ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      REAL X(MDX),Y(MDY)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GE.0.AND.INCX.NE.0.AND.IABS(INCX)*(N-1)+1.LE.MDX
     1       .AND.INCY.NE.0.AND.IABS(INCY)*(N-1)+1.LE.MDY
      IF (.NOT.NPRCHK) CALL MESSGE(500,'SWAPz ',1)
C
      IF (N.EQ.0) RETURN
      IF (INCX.EQ.1.AND.INCY.EQ.1) GOTO 20
C
C  CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT
C  EQUAL TO 1
C
      IX=1
      IY=1
      IF (INCX.LT.0) IX=(-N+1)*INCX+1
      IF (INCY.LT.0) IY=(-N+1)*INCY+1
      DO 10 I=1,N
      TEMP=X(IX)
      X(IX)=Y(IY)
      Y(IY)=TEMP
      IX=IX+INCX
      IY=IY+INCY
   10 CONTINUE
      RETURN
C
C  CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 M=MOD(N,3)
      IF (M.EQ.0) GOTO 40
      DO 30 I=1,M
      TEMP=X(I)
      X(I)=Y(I)
      Y(I)=TEMP
   30 CONTINUE
      IF (N.LT.3) RETURN
   40 MP1=M+1
      DO 50 I=MP1,N,3
      TEMP=X(I)
      X(I)=Y(I)
      Y(I)=TEMP
      TEMP=X(I+1)
      X(I+1)=Y(I+1)
      Y(I+1)=TEMP
      TEMP=X(I+2)
      X(I+2)=Y(I+2)
      Y(I+2)=TEMP
   50 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SWAPZD(X,Y,N,INCX,INCY,MDX,MDY)
C.......................................................................
C
C   COPYRIGHT 1979 SOCIETY FOR INDUSTRIAL AND APPLIED MATHEMATICS.
C   ALL RIGHTS RESERVED.
C
C   AUTHOR :     LINPACK (SUBROUTINE SSWAP)
C                REPRINTED WITH PERMISSION FROM 
C                LINPACK USER'S GUIDE.
C                ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      DOUBLE PRECISION X(MDX),Y(MDY),TEMP
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GE.0.AND.INCX.NE.0.AND.IABS(INCX)*(N-1)+1.LE.MDX
     1       .AND.INCY.NE.0.AND.IABS(INCY)*(N-1)+1.LE.MDY
      IF (.NOT.NPRCHK) CALL MESSGE(500,'SWAPzD',1)
C
      IF (N.EQ.0) RETURN
      IF (INCX.EQ.1.AND.INCY.EQ.1) GOTO 20
C
C  CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT
C  EQUAL TO 1
C
      IX=1
      IY=1
      IF (INCX.LT.0) IX=(-N+1)*INCX+1
      IF (INCY.LT.0) IY=(-N+1)*INCY+1
      DO 10 I=1,N
      TEMP=X(IX)
      X(IX)=Y(IY)
      Y(IY)=TEMP
      IX=IX+INCX
      IY=IY+INCY
   10 CONTINUE
      RETURN
C
C  CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 M=MOD(N,3)
      IF (M.EQ.0) GOTO 40
      DO 30 I=1,M
      TEMP=X(I)
      X(I)=Y(I)
      Y(I)=TEMP
   30 CONTINUE
      IF (N.LT.3) RETURN
   40 MP1=M+1
      DO 50 I=MP1,N,3
      TEMP=X(I)
      X(I)=Y(I)
      Y(I)=TEMP
      TEMP=X(I+1)
      X(I+1)=Y(I+1)
      Y(I+1)=TEMP
      TEMP=X(I+2)
      X(I+2)=Y(I+2)
      Y(I+2)=TEMP
   50 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PERMCZ(X,IT,N,NP,MDX,IOPT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : J. JOSS / A. RANDRIAMIHARISOA
C.......................................................................
C
      INTEGER IT(NP)
      REAL X(MDX,NP)
      IF (N.LE.0.OR.NP.LE.0.OR.MDX.LT.N.OR.(IOPT.NE.1.AND.IOPT.NE.2))
     + CALL MESSGE(500,'PERMCz',1)
      IF (IOPT.EQ.2) GOTO 400
      DO 200 I=1,NP
      IF (IT(I).LT.0) THEN
        IT(I)=-IT(I)
      ELSEIF (IT(I).NE.I) THEN
        J=I
        K=IT(J)
  100   CALL SWAPZ(X(1,J),X(1,K),N,1,1,MDX,MDX)
        J=K
        K=IT(J)
        IT(J)=-K
        IF (K.NE.I) GOTO 100
      ENDIF
  200 CONTINUE
      RETURN
  400 DO 700 I=1,NP
        IF (IT(I).LT.0) THEN
          IT(I)=-IT(I)
        ELSEIF (IT(I).NE.I) THEN
          J=I
          IK=IT(J)
  500     K=IK
          IK=IT(K)
          IT(K)=J
          J=K
          IF (IK.NE.I) GOTO 500
          IJ=IT(K)
          IT(K)=-I
  600     J=IJ
          CALL SWAPZ(X(1,J),X(1,K),N,1,1,MDX,MDX)
          IJ=IT(J)
          IT(J)=-K
          K=J
          IF (J.NE.I) GOTO 600
          IT(I)=-IT(I)
        ENDIF
  700 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PERMVZ(Y,IT,NP,IOPT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : J. JOSS / A. RANDRIAMIHARISOA
C.......................................................................
C
      INTEGER IT(NP)
      REAL Y(NP)
      IF (NP.LE.0.OR.(IOPT.NE.1.AND.IOPT.NE.2))
     + CALL MESSGE(500,'PERMVz',1)
      IF (IOPT.EQ.2) GOTO 400
      DO 200 I=1,NP
      IF (IT(I).LT.0) THEN
        IT(I)=-IT(I)
      ELSEIF (IT(I).NE.I) THEN
        J=I
        K=IT(J)
  100   TEMP=Y(J)
        Y(J)=Y(K)
        Y(K)=TEMP
        J=K
        K=IT(J)
        IT(J)=-K
        IF (K.NE.I) GOTO 100
      ENDIF
  200 CONTINUE
      RETURN
  400 DO 700 I=1,NP
        IF (IT(I).LT.0) THEN
          IT(I)=-IT(I)
        ELSEIF (IT(I).NE.I) THEN
          J=I
          IK=IT(J)
  500     K=IK
          IK=IT(K)
          IT(K)=J
          J=K
          IF (IK.NE.I) GOTO 500
          IJ=IT(K)
          IT(K)=-I
  600     J=IJ
          TEMP=Y(J)
          Y(J)=Y(K)
          Y(K)=TEMP
          IJ=IT(J)
          IT(J)=-K
          K=J
          IF (J.NE.I) GOTO 600
          IT(I)=-IT(I)
        ENDIF
  700 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CHISQZ(KODE,IFN,X,P)
C.......................................................................
C
C   AUTHORS :     I.D. HILL AND M.C. PIKE (1967)
C                 ALGORITHM 299: CHI-SQUARED INTEGRAL
C                 COMMUNICATION OF THE ACM, VOL.10, PP.243-244.
C                 ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
C
C REMARK:   IF (X.LE.0.OR.IFN.LT.1).AND.(KODE.EQ.1) P=0. + MESSAGE 400
C ------    IF (X.LE.0.OR.IFN.LT.1).AND.(KODE.EQ.2) P=1. + MESSAGE 400
C
      LOGICAL EVEN,BIGX,ODD,SMLX
      DATA XLSPI,YLSPI/0.572364942925,0.564189583548/
C
      IF (KODE.NE.1.AND.KODE.NE.2) CALL MESSGE(500,'CHISQZ',1)
      S=1.
      FN=FLOAT(IFN)
      IF (X.GT.0..AND.FN.GE.1.) GOTO 5
      CALL MESSGE(400,'CHISQZ',0)
      GOTO 99
    5 NU=IFIX(FN+.5)
      CALL MACHZ(3,EXMIN)
      A=0.5*X
      BIGX=.FALSE.
      IF (-A.LE.EXMIN) BIGX=.TRUE.
      SMLX=.NOT.BIGX
      EVEN=(2*(NU/2).EQ.NU)
      ODD=.NOT.EVEN
      IF ((EVEN.OR.NU.GT.2).AND.SMLX) S=EXP(-A)
      IF (BIGX) S=0.
      Y=S
      IF (EVEN) GOTO 10
      SX=-SQRT(X)
      CALL GAUSSZ(1,SX,ANS)
      S=2.0*ANS
C
C  NU.LE.2
C
   10 IF (NU.LE.2) GOTO 99
C
C  NU.GT.2
C
      X1=0.5*(FN-1.0)
      IF (EVEN) Z=1.0
      IF (ODD ) Z=0.5
      IF (SMLX) GOTO 30
      IF (EVEN) E=0.0
      IF (ODD ) E=XLSPI
      C=ALOG(A)
   20 E=ALOG(Z)+E
      IF (C*Z-A-E.GT.EXMIN) S=EXP(C*Z-A-E)+S
      Z=Z+1.0
      IF (Z.LE.X1) GOTO 20
      GOTO 99
   30 IF (EVEN) E=1.0
      IF (ODD ) E=YLSPI/SQRT(A)
      C=0.0
   40 E=E*A/Z
      C=C+E
      Z=Z+1.0
      IF (Z.LE.X1) GOTO 40
      S=C*Y+S
   99 P=S
      IF (KODE.EQ.1) P=1.0-P
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE NLGMZ(N,GL)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DATA PI/3.1415926535898/
C
      GL2=ALOG(2.)
      GL=0.
      K=N-2
   20 IF (K.LE.1) GOTO 30
      GL=GL+ALOG(FLOAT(K))-GL2
      K=K-2
      GOTO 20
   30 IF (K.EQ.1) GL=GL+ALOG(SQRT(PI))-GL2
      IF (N.EQ.1) GL=ALOG(SQRT(PI))
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LGAMAZ(X,GL)
C.......................................................................
C
C   AUTHORS :     M.C. PIKE AND I.D. HILL (1966)
C                 ALGORITHM 291: LOGARITHM OF GAMMA FUNCTION.
C                 COMMUNICATIONS OF THE ACM, VOL.9, P 684.
C                 ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      IF (X.LE.0.) CALL MESSGE(500,'LGAMAz',1)
      V=X
      F=0.0
      IF (X.GE.7.0) GOTO 300
      F=1.0
      Z=X-1.0
  100 Z=Z+1.0
      IF (Z.GE.7.0) GOTO 200
      V=Z
      F=F*Z
      GOTO 100
  200 V=V+1.0
      F=-ALOG(F)
  300 Z=1.0/V**2
      GL=F+(V-0.5)*ALOG(V)-V+.9189385332+(((-.000595238*Z+.0007936507)
     +   *Z - .0027777778)*Z+.0833333333)/V
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CQUANTZ(P,IFN,TOL,MAXIT,X)
C.......................................................................
C
C   AUTHORS :     D.J. BEST & D.E. ROBERTS (1975)
C                 ALGORITHM AS 91 "THE PERCENTAGE POINTS OF THE X
C                 DISTRIBUTION" APPLIED STATISTICS, VOL.19.
C                 REPRINT FROM PP.285-287 WITH THE PERMISSION OF 
C                 BLACKWELL PUBLISHERS.
C                 ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      X=-1.0
      IF (IFN.LE.0..OR.P.GT.0.999998.OR.P.LT.0.000002.OR.TOL.LE.0..OR.
     + MAXIT.LE.1) CALL MESSGE(500,'CQUANT',1)
C  This subroutine allows a positive real as degrees of freedom.
      DF=FLOAT(IFN)
      XX=0.5*DF
      CALL LGAMAZ(XX,G)
      AA=0.6931471805
      CC=XX-1.0
      NIT=0
C
C  STARTING APPROXIMATION FOR SMALL CHI-SQUARED
C
      IF (DF.GE.-1.24*ALOG(P)) GOTO 10
      CH=(P*XX*EXP(G+XX*AA))**(1.0/XX)
      IF ((CH-TOL).GE.0.0) GOTO 40
      GOTO 50
C
C  STARTING APPROXIMATION FOR DF LESS THAN OR EQUAL TO 0.32
C
   10 IF (DF.GT.0.32) GOTO 30
      CH=0.4
      A=ALOG(1.0-P)
   20 Q=CH
      P1=1.0+CH*(4.67+CH)
      P2=CH*(6.73+CH*(6.66+CH))
      T=-0.5+(4.67+2.0*CH)/P1 - (6.73+CH*(13.32+3.0*CH))/P2
      CH=CH-(1.0-EXP(A+G+0.5*CH+CC*AA)*P2/P1) / T
      IF ((ABS(Q/CH - 1.0)-0.01) .LE. 0.0) GOTO 40
      GOTO 20
   30 CALL NQUANT(P,XP)
C
C  STARTING APPROXIMATION USING WILSON AND HILFERTY ESTIMATE
C
      P1=0.222222/DF
      CH=DF*(XP*SQRT(P1)+1.0-P1)**3
C
C  STARTING APPROXIMATION FOR P TENDING TO 1
C
      IF (CH.GT.2.2*DF+6.0) CH=-2.0*(ALOG(1.0-P)-CC*ALOG(0.5*CH)+G)
   40 NIT=NIT+1
      IF (NIT.EQ.MAXIT) GOTO 50
      Q=CH
      P1=0.5*CH
      CALL INGAMAZ(P1,XX,GQ)
      P2=P-GQ
      T=P2*EXP(XX*AA+G+P1-CC*ALOG(CH))
      B=T/CH
      A=0.5*T-B*X
      S1=(210.0+A*(140.0+A*(105.0+A*(84.0+A*(70.0+60.0*A)))))/420.0
      S2=(420.0+A*(735.0+A*(966.0+A*(1141.0+1278.0*A))))/2520.0
      S3=(210.0+A*(462.0+A*(707.0+932.0*A)))/2520.0
      S4=(252.0+A*(672.0+1182.0*A)+CC*(294.0+A*(889.0+1740.0*A)))/5040.0
      S5=(84.0+264.0*A+CC*(175.0+606.0*A))/2520.0
      S6=(120.0+CC*(346.0+127.0*CC))/5040.0
      CH=CH+T*(1.0+0.5*T*S1-B*CC*(S1-B*(S2-B*(S3-B*(S4-B*(S5-B*S6))))))
      IF (ABS(Q/CH-1.0) .GT. TOL) GOTO 40
   50 X=CH
      IF (NIT.EQ.MAXIT) CALL MESSGE(300,'CQUANT',0)
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE INGAMAZ(X,P,G)
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
      DIMENSION PN(6)
      EXTERNAL XEXP
      DATA TOL/1.0E-7/
C
      G=0.0
      IF (X.EQ.0.) RETURN
      IF (X.LT.0..OR.P.LE.0.) CALL MESSGE(500,'INGAMA',1)
      CALL MACHZ(6,OFLO)
      OFLO=OFLO*1.E-15
      CALL LGAMAZ(P,GP)
      GIN=0.0
      FACTOR=XEXP(P*ALOG(X)-X-GP)
      IF (X.GT.1.0.AND.X.GE.P) GOTO 30
C
C  CALCULATION BY SERIES EXPANSION
C
      GIN=1.0
      TERM=1.0
      RN=P
   20 RN=RN+1.0
      TERM=TERM*X/RN
      GIN=GIN+TERM
      IF (TERM.GT.TOL) GOTO 20
      GIN=GIN*FACTOR/P
      GOTO 50
C
C  CALCULATION BY CONTINUED FRACTION
C
   30 A=1.0-P
      B=A+X+1.0
      TERM=0.0
      PN(1)=1.0
      PN(2)=X
      PN(3)=X+1.0
      PN(4)=X*B
      GIN=PN(3)/PN(4)
   32 A=A+1.0
      B=B+2.0
      TERM=TERM+1.0
      AN=A*TERM
      DO 33 I=1,2
      PN(I+4)=B*PN(I+2)-AN*PN(I)
   33 CONTINUE
      IF (PN(6).EQ.0.0) GOTO 35
      RN=PN(5)/PN(6)
      DIF=ABS(GIN-RN)
      IF (DIF.GT.TOL) GOTO 34
      IF (DIF.LE.TOL*RN) GOTO 42
   34 GIN=RN
   35 DO 36 I=1,4
      PN(I)=PN(I+2)
   36 CONTINUE
      IF (ABS(PN(5)).LT.OFLO) GOTO 32
      DO 41 I=1,4
      PN(I)=PN(I)/OFLO
   41 CONTINUE
      GOTO 32
   42 GIN=1.0-FACTOR*GIN
   50 G=GIN
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GAUSSZ(KODE,X,P)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C                
C.......................................................................
C
      REAL               P,X,SQR1D2
      DATA               SQR1D2/.7071068/
C
      IF (KODE.NE.1.AND.KODE.NE.2) CALL MESSGE(500,'GAUSSz',1)
      CALL CERFZ(-X*SQR1D2,C)
      P = .5 * C
      IF (KODE.EQ.2) P=1.-P
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GAUSSZD (KODE,X,P)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C                
C.......................................................................
C
      DOUBLE PRECISION   P,X,SQR1D2,CD
      DATA               SQR1D2/.7071067811865475D0/
C
      IF (KODE.NE.1.AND.KODE.NE.2) CALL MESSGE(500,'GAUSSD',1)
      CALL CERFZD(-X*SQR1D2,CD)
      P = .5D0 * CD
      IF (KODE.EQ.2) P=1.D0-P
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CERFZ(X,F)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C                
C.......................................................................
C
      REAL               X,F
      INTEGER            ISW,I
      DIMENSION          P(3),Q(2),P1(5),Q1(4),P2(3),Q2(2)
      REAL               P,Q,P1,Q1,P2,Q2,XMIN,XLARGE,SQRPI,XX,
     *                   RES,XSQ,XNUM,XDEN,XI,XBIG
      EXTERNAL XEXP
      DATA               P(1)/.3166529/,P(2)/1.722276/,
     *                   P(3)/21.38533/
      DATA               Q(1)/7.843746/,Q(2)/18.95226/
      DATA               P1(1)/.5631696/,P1(2)/3.031799/,
     *                   P1(3)/6.865018/,P1(4)/7.373888/,
     *                   P1(5)/4.318779E-5/
      DATA               Q1(1)/5.354217/,Q1(2)/12.79553/,
     *                   Q1(3)/15.18491/,Q1(4)/7.373961/
      DATA               P2(1)/-5.168823E-2/,P2(2)/-.1960690/,
     *                   P2(3)/-4.257996E-2/
      DATA               Q2(1)/.9214524/,Q2(2)/.1509421/
      DATA               XMIN/1.0E-5/,XLARGE/4.1875E0/
      DATA               XBIG/9.0/
      DATA               SQRPI/.5641896/
C
      Y=X
      XX = Y
      ISW = 1
      IF (XX.GE.0.0E0) GO TO 5
      ISW = -1
      XX = -XX
    5 IF (XX.LT..477E0) GO TO 10
      IF (XX.LE.4.0E0) GO TO 25
      IF (ISW .GT. 0) GO TO 35
      IF (XX.LT.XLARGE) GO TO 40
      RES = 2.0E0
      GO TO 55
   10 IF (XX.LT.XMIN) GO TO 15
      XSQ = XX*XX
      XNUM = (P(1)*XSQ+P(2))*XSQ+P(3)
      XDEN = (XSQ+Q(1))*XSQ+Q(2)
      RES = XX*XNUM/XDEN
      GO TO 20
   15 RES = XX*P(3)/Q(2)
   20 IF (ISW.EQ.-1) RES = -RES
      RES = 1.0E0-RES
      GO TO 55
   25 XSQ = XX*XX
      XNUM = P1(5)*XX+P1(1)
      XDEN = XX+Q1(1)
      DO 30 I=2,4
         XNUM = XNUM*XX+P1(I)
         XDEN = XDEN*XX+Q1(I)
   30 CONTINUE
      RES = XNUM/XDEN
      GO TO 45
   35 IF (XX.GT.XBIG) GO TO 50
   40 XSQ = XX*XX
      XI = 1.0E0/XSQ
      XNUM = (P2(1)*XI+P2(2))*XI+P2(3)
      XDEN = (XI+Q2(1))*XI+Q2(2)
      RES = (SQRPI+XI*XNUM/XDEN)/XX
   45 RES = RES*XEXP(-XSQ)
      IF (ISW.EQ.-1) RES = 2.0E0-RES
      GO TO 55
   50 RES = 0.0E0
   55 F = RES
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CERFZD(X,F)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C                
C.......................................................................
C
      DOUBLE PRECISION   F,X,XEXPD
      DIMENSION          P(5),Q(4),P1(9),Q1(8),P2(6),Q2(5)
      DOUBLE PRECISION   P,Q,P1,Q1,P2,Q2,XMIN,XLARGE,SQRPI,XX,
     *                   RES,XSQ,XNUM,XDEN,XI,XBIG
      INTEGER            ISW,I
      EXTERNAL XEXPD
C                                  COEFFICIENTS FOR 0.0 .LE. Y .LT.
C                                  .477
      DATA               P(1)/113.8641541510502D0/,
     *                   P(2)/377.4852376853020D0/,
     *                   P(3)/3209.377589138469D0/,
     *                   P(4)/.1857777061846032D0/,
     *                   P(5)/3.161123743870566D0/
      DATA               Q(1)/244.0246379344442D0/,
     *                   Q(2)/1282.616526077372D0/,
     *                   Q(3)/2844.236833439171D0/,
     *                   Q(4)/23.60129095234412D0/
C                                  COEFFICIENTS FOR .477 .LE. Y
C                                  .LE. 4.0
      DATA               P1(1)/8.883149794388376D0/,
     *                   P1(2)/66.11919063714163D0/,
     *                   P1(3)/298.6351381974001D0/,
     *                   P1(4)/881.9522212417691D0/,
     *                   P1(5)/1712.047612634071D0/,
     *                   P1(6)/2051.078377826071D0/,
     *                   P1(7)/1230.339354797997D0/,
     *                   P1(8)/2.153115354744038D-8/,
     *                   P1(9)/.5641884969886701D0/
      DATA               Q1(1)/117.6939508913125D0/,
     *                   Q1(2)/537.1811018620099D0/,
     *                   Q1(3)/1621.389574566690D0/,
     *                   Q1(4)/3290.799235733460D0/,
     *                   Q1(5)/4362.619090143247D0/,
     *                   Q1(6)/3439.367674143722D0/,
     *                   Q1(7)/1230.339354803749D0/,
     *                   Q1(8)/15.74492611070983D0/
C                                  COEFFICIENTS FOR 4.0 .LT. Y
      DATA               P2(1)/-3.603448999498044D-01/,
     *                   P2(2)/-1.257817261112292D-01/,
     *                   P2(3)/-1.608378514874228D-02/,
     *                   P2(4)/-6.587491615298378D-04/,
     *                   P2(5)/-1.631538713730210D-02/,
     *                   P2(6)/-3.053266349612323D-01/
      DATA               Q2(1)/1.872952849923460D0/,
     *                   Q2(2)/5.279051029514284D-01/,
     *                   Q2(3)/6.051834131244132D-02/,
     *                   Q2(4)/2.335204976268692D-03/,
     *                   Q2(5)/2.568520192289822D0/
C                                  CONSTANTS
      DATA               XMIN/1.0D-10/,XLARGE/6.375D0/
C                                  CERFD(XBIG) .APPROX. DETAP
      DATA               XBIG/13.3D0/
      DATA               SQRPI/.5641895835477563D0/
C
      Y=SNGL(X)
      XX = Y
      ISW = 1
      IF (XX.GE.0.0D0) GO TO 5
      ISW = -1
      XX = -XX
    5 IF (XX.LT..477D0) GO TO 10
      IF (XX.LE.4.0D0) GO TO 30
      IF (ISW .GT. 0) GO TO 40
      IF (XX.LT.XLARGE) GO TO 45
      RES = 2.0D0
      GO TO 70
C                                  ABS(Y) .LT. .477, EVALUATE
C                                  APPROXIMATION FOR CERFD
   10 IF (XX.LT.XMIN) GO TO 20
      XSQ = XX*XX
      XNUM = P(4)*XSQ+P(5)
      XDEN = XSQ+Q(4)
      DO 15 I = 1,3
         XNUM = XNUM*XSQ+P(I)
         XDEN = XDEN*XSQ+Q(I)
   15 CONTINUE
      RES = XX*XNUM/XDEN
      GO TO 25
   20 RES = XX*P(3)/Q(3)
   25 IF (ISW.EQ.-1) RES = -RES
      RES = 1.0D0-RES
      GO TO 70
C                                  .477 .LE. ABS(Y) .LE. 4.0
C                                  EVALUATE APPROXIMATION FOR CERFD
   30 XSQ = XX*XX
      XNUM = P1(8)*XX+P1(9)
      XDEN = XX+Q1(8)
      DO 35 I=1,7
         XNUM = XNUM*XX+P1(I)
         XDEN = XDEN*XX+Q1(I)
   35 CONTINUE
      RES = XNUM/XDEN
      GO TO 60
C                                  4.0 .LT. ABS(Y), EVALUATE
C                                  MINIMAX APPROXIMATION FOR CERFD
   40 IF (XX.GT.XBIG) GO TO 65
   45 XSQ = XX*XX
      XI = 1.0D0/XSQ
      XNUM= P2(5)*XI+P2(6)
      XDEN = XI+Q2(5)
      DO 50 I = 1,4
         XNUM = XNUM*XI+P2(I)
         XDEN = XDEN*XI+Q2(I)
   50 CONTINUE
      RES = (SQRPI+XI*XNUM/XDEN)/XX
   60 RES = RES*XEXPD(-XSQ)
      IF (ISW.EQ.-1) RES = 2.0D0-RES
      GO TO 70
   65 RES = 0.0D0
   70 F = RES
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE NQUANT(P,X)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL C(6)
      DATA C(1),C(2),C(3),C(4),C(5),C(6)/
     1     2.515517,0.802853,0.010328,1.432788,0.189269,0.001308/
C
      IF (P.GT.1..OR.P.LT.0.)  CALL MESSGE(500,'NQUANT',1)
      P1=P
      IF (P.GT.0.5) P1=1.-P
      T=SQRT(-2.*ALOG(P1))
      XZ=(C(3)*T+C(2))*T+C(1)
      XN=((C(6)*T+C(5))*T+C(4))*T+1.
      X=T-XZ/XN
      IF (P.LT.0.5) X=-X
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE XERFZ(KODE,X,P)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      EXTERNAL XEXP
      DATA SPI/2.506628274631/
C
C  EXMIN IS A MACHINE DEPENDENT PARAMETER SPECIFYING THE LARGEST NEGATIVE
C  REAL VALUE SUCH THAT EXP(EXMIN) CAN BE SUCCESSFULLY EVALUATED WITHOUT
C  UNDERFLOW.
C
      IF (KODE.NE.1.AND.KODE.NE.2) CALL MESSGE(500,'XERFz ',1)
      X2=-X*X/2.
      P=XEXP(X2)
      IF (KODE.EQ.2) P=P/SPI
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE XERPZ(IP,XLCNST,S,F)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : B. MARTIN-BERAN / A. MARAZZI
C.......................................................................
C
      EXTERNAL XEXP
      DATA CMIN/-.2257913526/
C
C  CIMIN=ALOG(SQRT(2/PI)) IS THE MINIMAL VALUE OF XLCNST.
C  EXMIN IS A MACHINE DEPENDENT PARAMETER SPECIFYING THE LARGEST NEGATIVE
C  REAL VALUE SUCH THAT EXP(EXMIN) CAN BE SUCCESSFULLY EVALUATED WITHOUT
C  UNDERFLOW.
C
      IF (IP.LE.0.OR.S.LT.0.) CALL MESSGE(500,'XERPz ',1)
      S2=-S*S/2.
      PP=FLOAT(IP)
      IF (XLCNST.GT.CMIN.OR.XLCNST.EQ.0.) GOTO 30
      CALL NLGMZ(IP,XLGM)
      XLCNST=(1.-PP/2.)*ALOG(2.)-XLGM
   30 F=0.
      IF (S.LE.0.) RETURN
      XLP=(PP-1.)*ALOG(S)+S2+XLCNST
      F=XEXP(XLP)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RUBENZ(XLMBDA,DELTA,MULT,N,X,XMODE,MAXIT,EPS,
     1               DNSTY,CUMDF,IFAULT,SG,ST,SA,SB)
C.......................................................................
C
C   AUTHOR :     R.W. FAREBROTHER (1984)
C                ALGORITHM AS 204, "THE DISTRIBUTION OF A POSITIVE
C                LINEAR COMBINATION OF X RANDOM VARIABLES"
C                APPLIED STATISTICS, VOL 24.
C                REPRINT FROM PP.385-388 WITH THE PERMISSION OF
C                BLACKWELL PUBLISHERS.
C                ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      DIMENSION XLMBDA(N),DELTA(N),MULT(N),
     1          SG(N),ST(N),SA(MAXIT),SB(MAXIT)
      INTEGER MULT
      REAL AO,AOINV,Z,BETA,EPS2,HOLD,HOLD2,SUM,SUM1,DANS,LANS,PANS,
     1     PRBTY,TOL
      LOGICAL NPRCHK
      EXTERNAL XEXP
C
C  PARAMETER CHECK
C
      NPRCHK=N.GE.1.AND.X.GT.0..AND.MAXIT.GE.1.AND.EPS.GT.0.0
      IF (NPRCHK) GOTO 10
      CUMDF=-2.0
      IFAULT=2
      GOTO 400
   10 CONTINUE
C
C  TOL IS A MACHINE DEPENDENT PARAMETER AND SHOULD BE SET AT A VALUE
C  RATHER LARGER THAN THE LOGARITHM OF THE SMALLEST POSITIVE REAL NUMBER.
C
      CALL MACHZ(5,YLGMN)
      TOL=YLGMN+10.
C
C  PRELIMINARIES
C
      BETA=XLMBDA(1)
      SUM=XLMBDA(1)
      DO 20 I=1,N
      HOLD=XLMBDA(I)
      IF (HOLD.GT.0..AND.MULT(I).GE.1.AND.DELTA(I).GE.0.) GOTO 25
      CUMDF=-7.0
      IFAULT=-I
      GOTO 500
   25 IF (BETA.GT.HOLD) BETA=HOLD
      IF (SUM.LT.HOLD) SUM=HOLD
   20 CONTINUE
      IF (XMODE.GT.0.0) BETA=XMODE*BETA
      IF (XMODE.LE.0.0) BETA=2./(1./BETA+1./SUM)
      K=0
      SUM=1.0
      SUM1=0.0
      DO 30 I=1,N
      HOLD=BETA/XLMBDA(I)
      SG(I)=1.0-HOLD
      SUM=SUM*HOLD**MULT(I)
      SUM1=SUM1+DELTA(I)
      K=K+MULT(I)
      ST(I)=1.0
   30 CONTINUE
      AO=XEXP(0.5*(ALOG(SUM)-SUM1))
      IF (AO.GT.0.) GOTO 40
      CUMDF=0.
      DNSTY=0.
      IFAULT=1
      GOTO 400
   40 CONTINUE
C
C  EVALUATE PROBABILITY AND DENSITY OF X2(K)
C
      Z=X/BETA
      IF (K.NE.(K/2)*2) GOTO 50
      I=2
      LANS=-0.5*Z
      DANS=XEXP(LANS)
      PANS=1.0-DANS
      GOTO 60
   50 I=1
      LANS=(-0.5*(Z+ALOG(Z))-0.22579135264473)
      DANS=XEXP(LANS)
      SQZ=SQRT(Z)
      CALL GAUSSZ(1,SQZ,PANS)
      PANS=2*PANS-1.
   60 CONTINUE
      K=K-2
      DO 75 II=I,K,2
      IF (LANS.LT.TOL) GOTO 65
      DANS=DANS*Z/FLOAT(II)
      GOTO 70
   65 LANS=LANS+ALOG(Z/FLOAT(II))
      DANS=XEXP(LANS)
   70 PANS=PANS-DANS
   75 CONTINUE
C
C  EVALUATE SUCCESSIVE TERMS OF EXPANSION
C
      PRBTY=PANS
      DNSTY=DANS
      EPS2=EPS/AO
      AOINV=1.0/AO
      SUM=AOINV-1.0
      DO 130 M=1,MAXIT
      SUM1=0.0
      DO 80 I=1,N
      HOLD=ST(I)
      ST(I)=HOLD*SG(I)
      HOLD2=ST(I)
      SUM1=SUM1+HOLD2*MULT(I)+M*DELTA(I)*(HOLD-HOLD2)
   80 CONTINUE
      SUM1=0.5*SUM1
      SB(M)=SUM1
      MM1=M-1
      IF (MM1.LE.0) GOTO 95
      DO 90 I=MM1,1,-1
      SUM1=SUM1+SB(I)*SA(M-I)
   90 CONTINUE
   95 SUM1=SUM1/FLOAT(M)
      SA(M)=SUM1
      K=K+2
      IF (LANS.LT.TOL) GOTO 100
      DANS=DANS*Z/FLOAT(K)
      GOTO 110
  100 LANS=LANS+ALOG(Z/FLOAT(K))
      DANS=XEXP(LANS)
  110 CONTINUE
      PANS=PANS-DANS
      SUM=SUM-SUM1
      DNSTY=DNSTY+DANS*SUM1
      SUM1=PANS*SUM1
      PRBTY=PRBTY+SUM1
      IF (PRBTY.GE.(-AOINV)) GOTO 120
      CUMDF=-3.0
      IFAULT=3
      GOTO 400
  120 IF (ABS(PANS*SUM).GE.EPS2) GOTO 130
      IF (ABS(SUM1).GE.EPS2) GOTO 130
      IFAULT=0
      GOTO 140
  130 CONTINUE
      IFAULT=4
  140 DNSTY=AO*DNSTY/(BETA+BETA)
      PRBTY=AO*PRBTY
      IF (PRBTY.GE.0.0.AND.PRBTY.LE.1.0) GOTO 150
      IFAULT=IFAULT+5
      GOTO 160
  150 IF (DNSTY.LT.0.0) IFAULT=IFAULT+6
  160 CUMDF=PRBTY
  400 IF (IFAULT.GT.0) CALL MESSGE(400+IFAULT,'RUBENz',0)
      RETURN
  500 IF (IFAULT.LT.0) CALL MESSGE(500-IFAULT,'RUBENz',0)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE FCUMZ(N1,N2,X,P,IER)
C.......................................................................
C
C   AUTHORS :     W.J. KENNEDY JR & J.E. GENTLE (1980) 
C                 STATISTICAL COMPUTING
C                 REPRINT FROM PAGES 114-115 (SUBROUTINE PF)
C                 BY COURTESY OF MARCEL DEKKER. INC.
C                 ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      REAL X,P
C
      Z=DBLE(X)
      IER=0
      IF (Z.GT.0.0D0) GOTO 5
      POFF=0.0D0
      IER=1
      P=SNGL(POFF)
      RETURN
    5 IF (N1.GT.0.AND.N2.GT.0) GOTO 10
      IER=2
      POFF=0.0D0
      P=SNGL(POFF)
      CALL MESSGE(402,'FCUMz ',0)
      RETURN
   10 CONTINUE
      CALL PRECDZ(EPS)
      AN1=DBLE(N1)
      AN2=DBLE(N2)
      A=AN1*Z/(AN1*Z+AN2)
      A1=1.D0-A
      IF (A1.LT.EPS) A1=EPS
      D1=AN1*0.5D0
      D2=AN2*0.5D0
      D3=D1+D2-1.D0
      R=0.D0
      S1=0.D0
      S2=0.D0
      DEL=1.D0
      XM=1.D0
      XK=1.D0
      C=0.25D0
      PI=3.141592653589793D0
      N=N2
C
C  NOTE BEGINNING OF MAJOR LOOP
C
   15 CONTINUE
C
C  TO SEE IF DEGREES OF FREEDOM ARE ODD OR EVEN
C
      M=IDINT(D2)
      M=2*M
      IF (M.NE.N) GOTO 30
      N=IDINT(D2)-1
C
C  IF DEGREES OF FREEDOM ARE EVEN
C  N=D.F./2-1
C
      IF (N.EQ.0) GOTO 25
      DO 20 I=1,N
      S1=DEL+S1*R
      D2=D2-1.D0
      D3=D3-1.D0
      TEM=A1/D2
      R=D3*TEM
      S2=(R+TEM)*S2
   20 CONTINUE
   25 S1=DEL+S1*R
      DEL=0.D0
      T=-1.D0
      D3=-1.D0
      S2=A*S2
      C=C+0.5D0
      GOTO 45
C
C  IF DEGREES OF FREEDOM ARE ODD
C  N=(D.F.-1)/2
C
   30 N=IDINT(D2)
C
C  IF DEGREES OF FREEDOM EQUAL 1.
C  DO NOT EXIT LOOP.
C
      IF (N.EQ.0) GOTO 40
      DO 35 I=1,N
      S1=DEL+S1*R
      D2=D2-1.D0
      D3=D3-1.D0
      TEM=A1/D2
      R=D3*TEM
      S2=(R+TEM)*S2
   35 CONTINUE
   40 S1=XK*S1
      S2=XK*S2
      ART=DSQRT(A1)
      XM=XM*ART
      T=(XM-ART)/A1
      D3=-0.5D0
      XK=2.D0/PI
      C=C*2.0D0
   45 IF (C.GT.0.875D0) GOTO 50
      D2=D1
      D3=D2+D3
      S2=S1
      S1=0.D0
      A1=A
      IF (A1.LT.EPS) A1=EPS
      N=N1
      GOTO 15
   50 IF (C.LT.1.125D0) DEL=4.D0/PI*DATAN(T)
      POFF=XM*(S2-S1)-DEL
      P=SNGL(POFF)
      IF (0.0D0.LE.POFF.AND.1.0D0.GE.POFF) RETURN
      IF (POFF.LT.0.0D0) POFF=0.0D0
      IF (POFF.GT.1.0D0) POFF=1.0D0
      IER=3
      P=SNGL(POFF)
      CALL MESSGE(403,'FCUMz ',0)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PRECSZ(PREC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      S=0.5
   10 S=0.5*S
      IF ((1.+S).GT.1.) GOTO 10
      T=1.
      S0=S
   20 S=(1.+T/100.)*S0
      T=T+1.
      IF ((1.+S).EQ.1.) GOTO 20
      PREC=S
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PRECDZ(PREC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DOUBLE PRECISION PREC,S,S0,T
C
      S=0.5D0
   10 S=0.5D0*S
      IF ((1.D0+S).GT.1.D0) GOTO 10
      T=1.D0
      S0=S
   20 S=(1.D0+T/100.)*S0
      T=T+1.D0
      IF ((1.D0+S).EQ.1.D0) GOTO 20
      PREC=S
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SRT1Z(A,N,K1,K2)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL A(N)
      LOGICAL NPRCHK
C
      NPRCHK=K1.GE.1.AND.K2.GT.K1.AND.K2.LE.N
      IF (.NOT.NPRCHK) CALL MESSGE(500,'SRT1z ',1)
      N1=K2-K1+1
c      I=1
c   10 I=I+I
c      IF (I.LE.N1) GOTO 10
      M=N1
   20 M=M/2
      IF (M.EQ.0) GOTO 90
      K=N1-M
      DO 40 J=1,K
      L=J
   50 IF (L.LT.1) GOTO 40
      LPM=L+M
      LPM1=LPM+K1-1
      L1=L+K1-1
      IF (A(LPM1).GE.A(L1)) GOTO 40
      X=A(LPM1)
      A(LPM1)=A(L1)
      A(L1)=X
      L=L-M
      GOTO 50
   40 CONTINUE
      GOTO 20
   90 CONTINUE
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SRT2Z(A,B,N,K1,K2)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      REAL A(N),B(N)
      LOGICAL NPRCHK
C
      NPRCHK=N.GT.0.AND.K1.GE.1.AND.K2.GE.K1.AND.K2.LE.N
      IF (.NOT.NPRCHK) CALL MESSGE(500,'SRT2z ',1)
      N1=K2-K1+1
c      I=1
c   10 I=I+I
c      IF (I.LE.N1) GOTO 10
      M=N1
   20 M=M/2
      IF (M.EQ.0) GOTO 90
      K=N1-M
      DO 40 J=1,K
      L=J
   50 IF (L.LT.1) GOTO 40
      LPM=L+M
      LPM1=LPM+K1-1
      L1=L+K1-1
      IF (A(LPM1).GE.A(L1)) GOTO 40
      X=A(LPM1)
      Y=B(LPM1)
      A(LPM1)=A(L1)
      B(LPM1)=B(L1)
      A(L1)=X
      B(L1)=Y
      L=L-M
      GOTO 50
   40 CONTINUE
      GOTO 20
   90 CONTINUE
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE FSTORDZ(Y,N,J,YJ)
C.......................................................................
C
C   AUTHOR :     P.J. ROUSSEEUW & A.M. LEROY
C                PROGRESS PACKAGE (SUBROUTINE PULL)
C                ADAPTED FOR ROBETH BY J. JOSS / A. RANDRIAMIHARISOA
C.......................................................................
C
C  FSTORDZ SEARCHES THE J-TH VALUE IN ORDER OF MAGNITUDE IN A VECTOR
C  OF LENGTH N.
C
      DIMENSION Y(N)
      IF (J.LE.0.OR.J.GT.N) CALL MESSGE(500,'FSTORD',1)
      L=1
      LR=N
   20 IF (L.GE.LR) GOTO 90
      AX=Y(J)
      JNC=L
      JJ=LR
   30 IF(JNC.GT.JJ) GOTO 80
   40 IF (Y(JNC).GE.AX) GOTO 50
      JNC=JNC+1
      GOTO 40
   50 IF(Y(JJ).LE.AX) GOTO 60
      JJ=JJ-1
      GOTO 50
   60 IF(JNC.GT.JJ) GOTO 70
      WA=Y(JNC)
      Y(JNC)=Y(JJ)
      Y(JJ)=WA
      JNC=JNC+1
      JJ=JJ-1
   70 GOTO 30
   80 IF(JJ.LT.J) L=JNC
      IF(J.LT.JNC) LR=JJ
      GOTO 20
   90 YJ=Y(J)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LMDDZ(X,Y,N,ISORT,XME,XMD,XSD)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : W. STAHEL / A. MARAZZI
C.......................................................................
C
      REAL X(N),Y(N)
C
      KM=(N+1)/2
      DO 20 I=1,N
      Y(I)=X(I)
   20 CONTINUE
      IF (ISORT.NE.0) CALL SRT1Z(Y,N,1,N)
      XME=Y(KM)
      IF (KM*2.EQ.N) XME=(XME+Y(KM+1))/2.
      K=0
      K1=KM
      K2=KM
      X1=0.
      X2=0.
   30 IF (K.GE.KM) GOTO 50
      K=K+1
      IF (X1.GT.X2) GOTO 40
      K1=K1-1
      IF (K1.EQ.0) GOTO 50
      X1=XME-Y(K1)
      GOTO 30
   40 K2=K2+1
      IF (K2.GT.N) GOTO 50
      X2=Y(K2)-XME
      GOTO 30
   50 XMD=AMIN1(X1,X2)
      XSD=XMD/.6745
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RGFL(F,Y,A,B,TOL,MAXIT,X,ITERM)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      EXTERNAL F
      LOGICAL NPRCHK
      DATA TL/1.E-10/
C
C  PARAMETER CHECK
C
      NPRCHK=A.LE.B.AND.TOL.GT.0..AND.MAXIT.GT.1
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RGFL  ',1)
C
C  INITIALIZE
C
      ITR=1
      FA=F(A)-Y
      FB=F(B)-Y
C
C  REGULA FALSI ITERATION
C
   20 IF (ABS(FA-FB).GT.TL) GOTO 30
      CALL MESSGE(401,'RGFL  ',0)
      RETURN
   30 XN=(A*FB-B*FA)/(FB-FA)
      FN=F(XN)-Y
C
C  TEST TO SEE IF MAXIMUM NUMBER OF ITERATIONS HAS BEEN EXECUTED
C
      IF (ITR.GE.MAXIT) GOTO 60
C
C  TEST TO SEE IF ROOT HAS BEEN FOUND
C
      IF (ABS(FN).LT.TOL) GOTO 70
      IF (FA*FN.LE.0.) GOTO 40
      A=XN
      FA=FN
      GOTO 50
   40 B=XN
      FB=FN
C
C  INCREMENT ITERATION COUNTER
C
   50 ITR=ITR+1
      GOTO 20
C
   60 ITERM=2
      X=XN
      RETURN
   70 ITERM=1
      X=XN
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TQUANTZ(P,IFN,X)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      DATA HALFPI/1.5707963268/
      X=0.
      FN=FLOAT(IFN)
      IF (FN.LT.1..OR.P.GT.1..OR.P.LT.0.) CALL MESSGE(500,'TQUANT',1)
      SIGN=-1.
      IF (P.GE.0.5) SIGN=1.
      P2=P*2
      IF (P.GT.0.5) P2=2.-P2
      IF (FN.NE.2.) GOTO 10
      X=SIGN*SQRT(2./(P2*(2.-P2))-2.)
      RETURN
   10 IF (FN.NE.1.) GOTO 20
      X=SIGN*COS(P2*HALFPI)/SIN(P2*HALFPI)
      RETURN
   20 A=1./(FN-0.5)
      B=48./(A**2)
      C=((20700.*A/B-98.)*A-16.)*A + 96.36
      D=((94.5/(B+C)-3.0)/B+1.0)*SQRT(A*HALFPI)*FN
      Z=D*P2
      Y=Z**(2./FN)
      IF (Y.LE.0.05+A) GOTO 30
C
C ASYMPTOTIC INVERSE EXPANSION ABOUT NORMAL
C
      CALL NQUANT(P2*.5,Z)
      Y=Z**2
      IF (FN.LT.5) C=C+0.3*(FN-4.5)*(Z+0.6)
      C=(((0.05*D*Z-5.0)*Z-7.)*Z-2.)*Z + B + C
      Y=(((((0.4*Y+6.3)*Y+36.)*Y+94.5)/C-Y-3.)/B+1.)*Z
      Y=A*Y**2
      IF (Y.GT.0.002) Y=XEXP(Y)-1.0
      IF (Y.LE.0.002) Y=.5*Y**2 + Y
      GOTO 40
   30 Y=((1./(((FN+6.0)/(FN*Y)-0.089*D-0.8222)*(FN+2.)*3.)+
     +  0.5/(FN+4.))*Y-1.)*(FN+1.)/(FN+2.)+1.0/Y
   40 X=SQRT(FN*Y)*SIGN
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PROBSTZ(X,IFN,P)
C.......................................................................
C
C   AUTHOR:     D.B. OWEN (1965)
C               A SPECIAL CASE OF A BIVARIATE NON-CENTRAL T-DISTRIBUTION
C               BIOMETRIKA, VOL.52, PP.437-446
C               ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      DATA G1/0.3183098862/
      P=0.
      IF (IFN.LE.0) CALL MESSGE(500,'PROBST',1)
      F=FLOAT(IFN)
      A=X/SQRT(F)
      B=F/(F+X**2)
      IM2=IFN-2
      IOE=IFN-2*(IFN/2)
      S=1.0
      C=1.0
      KS=2+IOE
      FK=KS
      IF (IM2-2.LT.0) GOTO 6
      DO 7 K=KS,IM2,2
        C=C*B*(FK-1.0)/FK
        S=S+C
        FK=FK+2.0
   7  CONTINUE
   6  IF (IOE.GT.0) GOTO 2
      P=0.5+0.5*A*SQRT(B)*S
      GOTO 3
   2  IF (IFN-1.GT.0) GOTO 5
      S=0.0
   5  P=0.5+(A*B*S+ATAN(A))*G1
   3  RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE BINPRDZ(K,N,P,PS,PK)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C                
C.......................................................................
C
      INTEGER K,N,K1,ICNT,J
      REAL    P,PS,PK,P1,Q1,XN,XX,ALQN,QP,XJ,SML,ALSML
      LOGICAL NPRCHK
      EXTERNAL XEXP
C
      PK = 0.
      PS = 0.
      NPRCHK=(K .LE. N .AND. K .GE. 0).AND.
     +       (P .LE. 1. .AND. P .GE. 0.)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'BINPRD',1)
      CALL MACHZ(4,SML)
      CALL MACHZ(5,ALSML)
      IF (P .NE. 0.) GO TO 15
      PS = 1.
      IF (K .NE. 0) GO TO 900
      PK = 1.
      GO TO 900
   15 IF (P .NE. 1.) GO TO 20
      IF (K .NE. N) GO TO 900
      PK = 1.
      PS = 1.
      GO TO 900
   20 P1 = P
      Q1 = 1.0-P
      K1 = K
      XN = N
      XX = XN*P
      IF (K .LE. XX) GO TO 25
      P1 = Q1
      Q1 = P
      K1 = N-K
   25 ALQN = XN*ALOG(Q1)
      ICNT =INT(ALQN/ALSML)
      ALQN = ALQN-ICNT*ALSML
      PK = XEXP(ALQN)
      IF (K1 .EQ. 0) GO TO 35
      QP = P1/Q1
      XJ = 0.0
      XN = XN+1.0
      DO 30 J = 1,K1
         IF (ICNT .EQ. 0) PS = PS+PK
         XJ = XJ+1.0
         PK = PK*(QP*(XN-XJ))
         IF (PK .LT. XJ) GO TO 30
         PK = PK*SML
         ICNT = ICNT-1
         PK = PK/XJ
   30 CONTINUE
   35 IF (ICNT .NE. 0) PK = 0.0
      IF (K .GT. XX) GO TO 40
      PS = PS+PK
      GO TO 900
   40 PS = 1.0-PS
  900 RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE RANDOW(ISEED,RN)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS 
C.......................................................................
C
C  RANDOM NUMBER GENERATOR ACCORDING TO THE LINEAR CONGRUENT SCHEME
C                  ISEED=ISEED*5761+999 MODULO 65536
C  IMPROVED AFTER MACLAREN-MARSAGLIA
C
      DIMENSION T(128)
      DATA INIT,T/0,128*0./
      IF (INIT.EQ.0.OR.INIT.NE.ISEED) THEN
        ISEED=MOD(ISEED,65536)
        DO 100 I=1,128
        ISEED=ISEED*5761+999
        ISEED=MOD(ISEED,65536)
        T(I)=FLOAT(ISEED)/65536.0
  100   CONTINUE
      ENDIF
      ISEED=ISEED*5761+999
      ISEED=MOD(ISEED,65536)
      I=128*ISEED/65536
      RN=T(I+1)
      ISEED=ISEED*5761+999
      ISEED=MOD(ISEED,65536)
      T(I+1)=FLOAT(ISEED)/65536.0
      INIT=ISEED
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE POISSNZ(LAMBDA,K,PS,PK)
C.......................................................................
C
C   AUTHOR  :    L. KNUSEL (1986)
C                COMPUTATION OF THE CHI-SQUARE AND POISSON DISTRIBUTION,
C                SIAM J. SCI. STATIST. COMPUT. 7, PP 1022-1036.
C                ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      INTEGER K
      REAL    LAMBDA,PS,PK,A,LAMAX,IAX,JAX,LPK,MEDIAN
      LOGICAL NPRCHK
      EXTERNAL XEXP
      DATA LAMAX/1.E6/
C
      PS = 0.
      PGT = 0.
      PK = 0.
      NPRCHK=LAMBDA.GT.0. .AND. LAMBDA.LE.LAMAX .AND. K.GE.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'POISSN',1)
      CALL MACHZ(3,EXMIN)
      CALL MACHZ(4,XLGMN)
C
C     Returns the lower tail and point probabilities
C     associated with a Poisson distribution.
C
C     Let Z denote a random variable having a Poisson distribution with
C     parameters N and P. The routine computes for given LAMBDA and K:
C
C     PS = Prob (Z .LE. K)
C     PK = Prob (Z .EQ. K)
C
      IF (K.GT.1100000) THEN
C       For LAMBDA.LE.1E6 and K.GT.1100000 the probability
C       PK is smaller than 1E-2000.
        PS = 1.0
        PK = 0.0
        RETURN
      ELSEIF (LAMBDA.LT.SQRT(XLGMN)) THEN
C       This case is treated here in order to reduce underflow
C       problems
        PS = 1.0
        PK = 0.0
        IF (K.EQ.0) PK = 1.0
        IF (K.EQ.1) PK = LAMBDA
        RETURN
      ENDIF
      A = FLOAT(K+1)
      I2A=2*(K+1)
      X=LAMBDA
C
C     Computation of PK
C
      IF (A.EQ.1.0) THEN
         LPK = -X
      ELSE
         CALL NLGMZ(I2A,GL)
         LPK=-X+(A-1.)*ALOG(X)-GL
      ENDIF
      PK = XEXP(LPK)
      MEDIAN = A - 0.33
      IF (X.LE.MEDIAN) THEN
C
C       Compute PS = 1 - PK*IAX
C
        IF (LPK.GE.EXMIN) THEN
          CALL INTGM0(X,A,IAX)
          PS = 1.0 - PK*IAX
        ELSE IF (2*X.LE.A) THEN
          PS = 1.0
        ELSE
          Q = X/A
          ARG = LPK + LOG(Q/(1.0-Q))
          IF (ARG.LE.EXMIN) THEN
            PS = 1.0
          ELSE
            CALL INTGM0(X,A,IAX)
            ARG = LPK + LOG(IAX)
            PS = 1.0 - XEXP(ARG)
          ENDIF
        ENDIF
      ELSE
C
C       Compute PS = PK*JAX
C
        IF (LPK.GE.EXMIN) THEN
          CALL INTGM1(X,A,JAX)
          PS = PK*JAX
        ELSE
          Q = (A-1.0)/X
          ARG = LPK - LOG(1.0-Q)
          IF (ARG.LE.EXMIN) THEN
            PS = 0.0
          ELSE
            CALL INTGM1(X,A,JAX)
            ARG = LPK + LOG(JAX)
            PS = XEXP(ARG)
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE INTGM0(X,A,IAX)
C
C     Computes I(A,X) = Integral from t=0 to t=X of
C                        (t/X)**(A-1) * EXP(X-t)
C     0 .LT. X .LE. A .LE. 1E6
C
      REAL  X,A,IAX,EPS
      DATA EPS/0.5E-6/
C
C     Determine NSTEP, the number of steps in the backward recursion:
C
      NSTEP = 0
      FAC = 1.0
      B = A
   20 CONTINUE
      FAC = FAC*X/B
      B = B + 1.0
      NSTEP = NSTEP + 1
      IF (FAC.GT.EPS) GO TO 20
C
C     Backward recursion with NSTEP iteration steps:
C
      IAX = 0.0
      DO 40 I = 1, NSTEP
         B = B - 1.0
         IAX = (1.0+IAX)*X/B
   40 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE INTGM1(X,A,JAX)
C
C     Computes J(A,X) = Integral from t=X to t=infinity of
C                        (t/X)**(A-1) * EXP(X-t)
C     0 .LT. A .LE. X .LE. 1E6 (A VALUE IS INTEGER)
C
      REAL  X,A,JAX,EPS
      DATA EPS/0.5E-6/
C
C     Determine NSTEP, the number of steps in the forward recursion:
C
      NSTEP = 0
      FAC = 1.0
      B = A
   20 CONTINUE
      B = B - 1.0
      FAC = FAC*B/X
      NSTEP = NSTEP + 1
      IF (FAC.GT.EPS) GO TO 20
C
C     Forward recursion with NSTEP iteration steps:
C
      JAX = 1.0
      DO 40 I = 2, NSTEP
         B = B + 1.0
         JAX = 1.0 + JAX*B/X
   40 CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
C
C                 R O B E T H  FORTRAN Source
C
C  File AEAUXI.F  Auxiliary subroutines of Chapter 5
C
C-----------------------------------------------------------------------
C
      SUBROUTINE AIFALG(X,SA,EXU,EXUP,N,NP,NCOV,MDX,
     1                  TAU,MAXIT,ICNV,TOL,NIT,SN,SA0,SU1,SD)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : C. RUFFIEUX / A.MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
C  FIXED POINT ALGORITHM FOR THE COMPUTATION OF THE MATRIX SA
C  (STANDARDIZED CASE, SA LOWER TRIANGULAR)
C
      DIMENSION X(MDX,NP),SN(N)
      DOUBLE PRECISION SA0(NCOV),SU1(NCOV),SA(NCOV),SD(NP),EXU,EXUP
      EXTERNAL EXU,EXUP,ICNVA
      LOGICAL NPRCHK
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NP*(NP+1)/2
      NPRCHK=NP.GT.0
     1       .AND.N.GE.NP.AND.NCOV.EQ.NN
     2       .AND.MDX.GE.N.AND.TAU.GE.0.0
     3       .AND.(ICNV.EQ.1.OR.ICNV.EQ.2)
     4       .AND.TOL.GT.0.0.AND.MAXIT.GT.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'AIFALG',1)
C
C  STEP 0 : INITIALIZATION
C  -----
      NIT=0
      IF (ICNV.EQ.1) THEN
        L=0
        DO 20 I=1,NP
        DO 10 J=1,I
        L=L+1
        SA0(L)=0.D0
        IF (I.EQ.J) SA0(L)=-1.D0
   10   CONTINUE
   20   CONTINUE
      ENDIF
      DO 30 L=1,N
      SN(L)=0.0
   30 CONTINUE
C
C  STEP 1: COMPUTE WEIGHTED COVARIANCE (SU1) AND AUXILIARY VALUES
C  ------
  100 CALL AIUCOW(X,SA,SU1,EXU,EXUP,N,NP,NCOV,
     1          MDX,ICNV,NIT,ZMAX,SN,SD)
C
C  STEP 2: CHECK CONVERGENCE
C  ------
      IF (NIT.EQ.MAXIT.OR.ICNVA(NCOV,ZMAX,SA,SA0,TOL,ICNV).EQ.1)GOTO 500
C
C  STEP 3: FIND IMPROVEMENT MATRIX SS=I-SU1 FOR SA
C  ------
      INFO=0
      CALL PRSCF0(SU1,NP,NCOV,TAU,INFO)
      IF (INFO.NE.0) CALL MESSGE(400+INFO,'AIFALG',0)
C
C  STEP 4: SET SA0:=SA AND SA:=(I-SS)*SA0
C  -------
      DO 410 IJ=1,NCOV
      SA0(IJ)=SA(IJ)
  410 CONTINUE
      CALL MTT3ZD(SA0,SU1,SA,NP,NCOV)
      NIT=NIT+1
      GOTO 100
C
C  STEP 5: STOP
C  -------
  500 RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE AIUCOW(X,SA,SU1,EXU,EXUP,N,NP,NCOV,
     1           MDX,ICNV,NIT,ZMAX,SN,SD)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : C. RUFFIEUX / A.MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
C  COMPUTE WEIGHTED COVARIANCE MATRIX;
C  STORE OBSERVATION NORMS IN SN;
C  IF (IALG.NE.0) STORE EXU VALUES IN SU, EXUP VALUES IN SUP;
C  EXUP IS NOT USED IF IALG=0;
C
      DIMENSION X(MDX,NP),SN(N)
      DOUBLE PRECISION SA(NCOV),SU1(NCOV),SD(NP),U,XN,ZNR
      DOUBLE PRECISION EXU,EXUP
      EXTERNAL         EXU,EXUP
      DATA XN/0.D0/
C
      IF (NIT.GT.1) GOTO 10
      XN=DBLE(N)
   10 ZMAX=0.0
      DO 50 IJ=1,NCOV
      SU1(IJ)=0.D0
   50 CONTINUE
      DO 100 L=1,N
      DO  60 J=1,NP
      SD(J)=DBLE(X(L,J))
   60 CONTINUE
      CALL MLYZD(SA,SD,NP,NCOV,NP,1)
      CALL NRM2ZD(SD,NP,1,NP,ZNR)
      SNL=SNGL(ZNR)
      IF (ICNV.NE.1) ZMAX=AMAX1(ZMAX,ABS(SNL-SN(L)))
      SN(L)=SNL
      U=EXU(SNL,EXUP)
      IJ=0
      DO 90 I=1,NP
      DO 80 J=1,I
      IJ=IJ+1
      SU1(IJ)=SU1(IJ)+(SD(I)*U)*SD(J)
   80 CONTINUE
   90 CONTINUE
  100 CONTINUE
      DO 110 IJ=1,NCOV
      SU1(IJ)=SU1(IJ)/XN
  110 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION UZED(DS,WGT,N,EXU,PSY)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : C. RUFFIEUX / A.MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
C  UZED(S)=U(SQRT(ZBAR2+BET2*S^2))*dG(S)
C
      DIMENSION WGT(N)
      DOUBLE PRECISION EXU,DS
      EXTERNAL EXU,PSY
      COMMON/ALBEC/ZBAR2,BET2,IPP,ITP,XLCNST,SIGM
      DATA NCALL,PSY1/0,0.0/
      IF (NCALL.EQ.1) PSY1=PSY(1.0)
      S=SNGL(DS)
      IF (IPP.GT.0) GOTO 5
      Z=SQRT(ZBAR2)
      ANS=1.+0*WGT(1)
      GOTO 10
    5 SBAR=S/SIGM
      CALL XERPZ(IPP,XLCNST,SBAR,ANS)
      ANS=ANS/SIGM
      Z=SQRT(ZBAR2+BET2*S*S)
   10 UZED=EXU(Z)*DBLE(ANS)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION ESPUK(S,EXU)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : C. RUFFIEUX / A.MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
C  ESPUK(S)=E[U(|Z|)]=INT(U(SQRT(ZBAR2+BET2*S**2))DG(S)
C
      DIMENSION WGT(1),IWORK(40)
      DOUBLE PRECISION UZED,ERRSTD,UPERD,TILD,RES,WORK(80),EXU,DS
      EXTERNAL UZED,EXU,PSY
      COMMON/ALBEC/ZBAR2,BET2,IPP,ITP,XLCNST,SIGM
      COMMON/INTEG/UUPER,TTIL,IWORK,WORK,IER1,ERRST1
      UPERD=DBLE(UUPER)
      DS=DBLE(S)
      TILD=DBLE(TTIL)
      ZBAR2=S*S
      IF (IPP.GT.0) GOTO 5
      RES=UZED(DS,WGT,1,EXU,PSY)
      GOTO 10
   5  LIMIT=20
      KEY=1
      CALL INTGRD(UZED,WGT,1,EXU,PSY,0.D0,UPERD,TILD,0.D0,KEY,
     *           LIMIT,RES,ERRSTD,NEVAL,IER,WORK,IWORK)
      IER1=MAX(IER,IER1)
      ERRST=SNGL(ERRSTD)
      ERRST1=MAX(ERRST1,ERRST)
  10  ESPUK=RES
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION UZED2(DS,WGT,N,EXU,PSY)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : C. RUFFIEUX / A.MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
C  UZED2(S)=AVE{U(SQRT(ZBAR2+BET2*S**2))*S**2*dG(S)}
C
      DOUBLE PRECISION EXU,DS,U,XN
      DIMENSION WGT(N)
      EXTERNAL EXU,PSY
      COMMON/ALBEC/ZBAR2,BET2,IPP,ITP,XLCNST,SIGM
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,BT,CW
      DATA NCALL,PSY1/0,0.0/
      IF (NCALL.EQ.1) PSY1=PSY(1.0)
      U=0.D0
      S=SNGL(DS)
      DO 10 L=1,N
      ZBAR2=WGT(L)*WGT(L)
      Z=SQRT(ZBAR2+BET2*S*S)
      U=U+EXU(Z)
   10 CONTINUE
      SBAR=S/SIGM
      CALL XERPZ(IPP,XLCNST,SBAR,ANS)
      XN=DBLE(N)*SIGM
      UZED2=(U/XN)*DS*DS*DBLE(ANS)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION INS1(DS,WGT,N,EXW,EXPSI)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : C. RUFFIEUX / A.MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
C  INS1(S)=E[ETA'(|ZI|)]*dG(S)
C  SERVES TO COMPUTE S1 WHEN OF THE FORM B1
C
      DIMENSION WGT(N),IWRK1(20),IWORK(40),WRK1(80),WORK(160)
      DOUBLE PRECISION DS,EXW
      EXTERNAL EXPSI,PSPPHI,EXW
      COMMON/ALBEC/ZBAR2,BET2,IPP,ITP,XLCNST,SIGM
      COMMON/INTPAR/ITYPE,I,NEVAL,LIMIT,KEY
      COMMON/INTEG/UUPER,TTIL,IWORK,WORK,IER1,ERRST1
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      ANS=1.
      S=SNGL(DS)
      Z=SQRT(ZBAR2+BET2*S*S)
      WGT(I)=SNGL(EXW(Z))
      INS1=0.D0
C
C  WEIGHTS FOR SCHWEPPE ESTIMATORS WHEN FUNCTION PSI IS NOT FROM HUBER TYP
C
      IF (IPSI.EQ.1) GOTO 15
      ITYPE=ITP
      LIMIT=20
      KEY=1
      CALL INTGRS(PSPPHI,WGT,N,EXPSI,EXPSI,0.,UUPER,TTIL,0.0,KEY,
     *          LIMIT,RES,ERREST,NEVAL,IER,WRK1,IWRK1)
      IER1=MAX(IER1,IER)
      ERRST1=MAX(ERREST,ERRST1)
      RES1=2*RES*WGT(I)
      GOTO 10
C
C  WEIGHTS FOR SCHWEPPE ESTIMATORS WHEN FUNCTION PSI IS FROM HUBER TYP
C
   15 C0=C*WGT(I)
      CALL LIEPSHZ(C0,EPSI2,RES1)
   10 IF(IPP.GT.0) THEN
       SBAR=S/SIGM
       CALL XERPZ(IPP,XLCNST,SBAR,ANS)
       ANS=ANS/SIGM
      ENDIF
C
C    FOR MALLOWS AND SCHWEPPE CASES, E[ETA'(|ZI|)]dG(S)
C
      IF (ITP.LT.3) INS1=WGT(I)*DBLE(ANS)
      IF (ITP.EQ.3) INS1=RES1*DBLE(ANS)
      WGT(I)=ZBAR2
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION INS2(DS,WGT,N,EXW,EXPSI)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : C. RUFFIEUX / A.MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
C  INS2(S)=E[ETA**2(|ZI|)]*dG(S)
C  SERVES TO COMPUTE S2 WHEN OF THE FORM B1
C
      DIMENSION WGT(N),IWRK1(20),IWORK(40),WORK(160),WRK1(80)
      DOUBLE PRECISION EXW,DS
      EXTERNAL EXPSI,PS2PHI,EXW
      COMMON/ALBEC/ZBAR2,BET2,IPP,ITP,XLCNST,SIGM
      COMMON/INTPAR/ITYPE,I,NEVAL,LIMIT,KEY
      COMMON/INTEG/UUPER,TTIL,IWORK,WORK,IER1,ERRST1
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      ANS=1.
      S=SNGL(DS)
      Z=SQRT(ZBAR2+BET2*S*S)
      WGT(I)=SNGL(EXW(Z))
      INS2=0.D0
C
C  WEIGHTS FOR SCHWEPPE ESTIMATORS (FUNCTION PSI IS NOT FROM HUBER TYPE)
C
      IF (IPSI.EQ.1) GOTO 15
      ITYPE=ITP
      LIMIT=20
      KEY=1
      CALL INTGRS(PS2PHI,WGT,N,EXPSI,EXPSI,0.,UUPER,TTIL,0.0,KEY,
     *          LIMIT,RES,ERREST,NEVAL,IER,WRK1,IWRK1)
      IER1=MAX(IER1,IER)
      ERRST1=MAX(ERRST1,ERREST)
      RES1=2*RES*WGT(I)*WGT(I)
      GOTO 10
C
C  WEIGHTS FOR SCHWEPPE ESTIMATORS (FUNCTION PSI IS FROM HUBER TYPE)
C
   15 C1=C*WGT(I)
      CALL LIEPSHZ(C1,RES1,EPSIP)
C
C  MALLOWS et SCHWEPPE E[ETA**2]*dG(S)
C
   10 IF (IPP.GT.0) THEN
       SBAR=S/SIGM
       CALL XERPZ(IPP,XLCNST,SBAR,ANS)
       ANS=ANS/SIGM
      ENDIF
      IF (ITP.LT.3) INS2=WGT(I)*WGT(I)*DBLE(ANS)
      IF (ITP.EQ.3) INS2=RES1*DBLE(ANS)
      WGT(I)=ZBAR2
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION INS3(DS,WGT,N,EXW,EXPSI)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : C. RUFFIEUX / A.MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
C  INS3(S)=1/n*SUM{E[ETA'(|ZI|)]}*dG(S)
C  TO COMPUTE S1 WHEN OF THE FORM B2
C
      DIMENSION WGT(N)
      DOUBLE PRECISION INS1,EXW,DS,SUM
      EXTERNAL INS1,EXPSI,EXW
      COMMON/INTPAR/ITYPE,I,NEVAL,LIMIT,KEY
      COMMON/ALBEC/ZBAR2,BET2,IPP,ITP,XLCNST,SIGM
      SUM=0.D0
      DO 10 J=1,N
      I=J
      ZBAR2=WGT(I)
      SUM=SUM+INS1(DS,WGT,N,EXW,EXPSI)
   10 CONTINUE
      INS3=SUM*DS*DS/DBLE(N)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION INS4(DS,WGT,N,EXW,EXPSI)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : C. RUFFIEUX / A.MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
C  INS4(S)=1/n*SUM{E[ETA**2(|ZI|)]}*dG(S)
C  TO COMPUTE S2 WHEN OF THE FORM B2
C
      DIMENSION WGT(N)
      DOUBLE PRECISION INS2,EXW,DS,SUM
      EXTERNAL INS2,EXPSI,EXW
      COMMON/INTPAR/ITYPE,I,NEVAL,LIMIT,KEY
      COMMON/ALBEC/ZBAR2,BET2,IPP,ITP,XLCNST,SIGM
      SUM=0.D0
      DO 10 J=1,N
      I=J
      ZBAR2=WGT(I)
      SUM=SUM+INS2(DS,WGT,N,EXW,EXPSI)
   10 CONTINUE
      INS4=SUM*DS*DS/DBLE(N)
      RETURN
      END
C-----------------------------------------------------------------------
C
C                 R O B E T H  FORTRAN Source
C
C  File AEMAIN.F  Main subroutines of Chapter 5
C
C-----------------------------------------------------------------------
C
      SUBROUTINE AIREF0(EXPSI,EXU,EXW,ITYPE,MU,IALFA,SIGMX,
     1           UPPER,TIL,MAXIT,TOL,NIT,ALFA,BETA,REFF)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / C. RUFFIEUX
C.......................................................................
C
      EXTERNAL UZED,UZED2,EXPSI,EXU,EXW,INS1,INS2,INS3,INS4
      REAL WGT(1)
      INTEGER IWORK(40)
      DOUBLE PRECISION EXU,EXW,UZED,UZED2,INS1,INS2,INS3,INS4,
     *       UPERD,TILD,ERRSTD,ANS1,ANS2,ANS3,ANS4,DS,WORK(80)
      LOGICAL NPRCHK
      COMMON/ALBEC/ALF2,BET2,IPP,ITP,XLCNST,SIGM
      COMMON/INTEG/UUPER,TTIL,IWORK,WORK,IER1,ERRST1
      COMMON/INTPAR/ITYP,I,NEVAL,LIMIT,KEY
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
C
C  PARAMETER CHECK AND INITIALISATION
C
       NPRCHK=MAXIT.GT.0.AND.TOL.GT.0..AND.UPPER.GT.0.
     *         .AND.TIL.GT.0..AND.SIGMX.GT.0..AND.
     *         (ITYPE.GE.1.AND.ITYPE.LE.3)
          IF (.NOT.NPRCHK) CALL MESSGE(500,'AIREF0',1)
       NPRCHK=((IALFA.EQ.0.OR.IALFA.EQ.1).AND.MU.GT.0)
     *     .OR.(IALFA.EQ.1.AND.MU.EQ.0)
          IF (.NOT.NPRCHK) CALL MESSGE(501,'AIREF0',1)
       ITYP=ITYPE
        NIT=0
        ALFA=0.
        BETA=0.
C
C  SPECIAL FOR ITYPE.EQ.1.OR.(MU.EQ.0.AND.ITYPE.EQ.2)
C           Compute Epsi**2/Epsi'
C
       IF (ITYP.EQ.3) GOTO 5
       IF (IPSI.EQ.1) THEN
        IF (C.LE.0.) C=1.345
        CALL LIEPSHZ(C,G1,G0)
       ELSE
        CALL LIEPSU(EXPSI,UPPER,TIL,ERREST,G1,G0)
       ENDIF
       IF (ITYP.NE.1.AND.(MU.GT.0.OR.ITYP.NE.2)) GOTO 5
       REFF=(G0**2)/G1
       RETURN
    5  CONTINUE
       P=FLOAT(MU)
       Q=FLOAT(IALFA)
       SIGM=SIGMX
       LIMIT=20
       KEY=1
        IPP=MU
        ITP=ITYPE
        I=1
        UPERD=DBLE(UPPER)
        UUPER=UPPER
        TILD=DBLE(TIL)
        TTIL=TIL
        XLCNST=-1.
C
C  COVARIANCE LS-ESTIMATOR
C
      TRCVLS=Q+P/(SIGMX**2)
C
C  COMPUTATION OF ALFA AND BETA (ITERATIVE ALGORITHM)
C
        DS=1.D0
        DALF=0.
        DBET=0.
C
C  STEP 0: INITIALIZATION
C  ------
        NIT=1
        ALF2=1.
        BET2=1.
        IF (IALFA.EQ.0) ALF2=0.
        IF (MU.EQ.0) BET2=0.
        S=SNGL(DS)
   10 IF (IALFA.EQ.0) GOTO 20
C
C  STEP 1: SOLVE FOR ALFA
C -------
      IF (MU.GT.0) CALL INTGRD(UZED,WGT,1,EXU,EXPSI,0.D0,UPERD,TILD,
     *             0.D0,KEY,LIMIT,ANS1,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'AIREF0',0)
      IF (MU.EQ.0) ANS1=UZED(DS,WGT,1,EXU,EXPSI)
      ALF2=1./SNGL(ANS1)
      ALF=SQRT(ALF2)
      DALF=ABS(ALFA-ALF)
      ALFA=ALF
   20 IF (MU.EQ.0) GOTO 30
C
C  STEP 2: SOLVE FOR BETA
C  ------
      WGT(1)=SQRT(ALF2)
      CALL INTGRD(UZED2,WGT,1,EXU,EXPSI,0.D0,UPERD,TILD,0.D0,KEY,
     *     LIMIT,ANS2,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'AIREF0',0)
      BET2=P/SNGL(ANS2)
      BET=SQRT(BET2)
      DBET=ABS(BETA-BET)
      BETA=BET
C
C STEP 3: CHECK CONVERGENCE
C ------
   30 IF ((DALF.LT.TOL.AND.DBET.LT.TOL).OR.(NIT.EQ.MAXIT)) GOTO 40
      NIT=NIT+1
      GOTO 10
   40 IF (NIT.EQ.MAXIT) CALL MESSGE (200,'AIREF0',0)
C
C  COVARIANCE M-ESTIMATORS
C
      TRCOV=0.
      IF (IALFA.EQ.0.AND.MU.GT.0) GOTO 50
      IF (IALFA.EQ.1.AND.MU.EQ.0) GOTO 60
C
C  STEP 4: COMPUTE S1 AND S2 FOR THE QUALITATIVE COVARIATE
C  ------
        CALL INTGRD(INS2,WGT,1,EXW,EXPSI,0.D0,UPERD,TILD,0.D0,KEY,
     *     LIMIT,ANS2,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'AIREF0',0)
        IF (IER1.GT.0) CALL MESSGE (300+IER1,'INS2  ',0)
        IER1=0
        ERRST1=0.
        CALL INTGRD(INS1,WGT,1,EXW,EXPSI,0.D0,UPERD,TILD,0.D0,KEY,
     *     LIMIT,ANS1,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'AIREF0',0)
        IF (IER1.GT.0) CALL MESSGE (300+IER1,'INS1  ',0)
        IER1=0
        ERRST1=0.
       TRCOV=SNGL(ANS2/ANS1**2)
C
C  COMPUTE S1 AND S2 FOR THE QUANTITATIVE COVARIATES
C
   50   CALL INTGRD(INS3,WGT,1,EXW,EXPSI,0.D0,UPERD,TILD,0.D0,KEY,
     *     LIMIT,ANS3,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'AIREF0',0)
        IF (IER1.GT.0) CALL MESSGE (300+IER1,'INS3  ',0)
        IER1=0
        ERRST1=0.
        CALL INTGRD(INS4,WGT,1,EXW,EXPSI,0.D0,UPERD,TILD,0.D0,KEY,
     *     LIMIT,ANS4,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'AIREF0',0)
        IF (IER1.GT.0) CALL MESSGE (300+IER1,'INS4  ',0)
        IER1=0
        ERRST1=0.
C
C  STEP 5: COMPUTE ARE1
C  ------
         FONCT=TRCOV+P*P*SNGL(ANS4/ANS3**2)
         REFF=(TRCVLS)/FONCT
      GOTO 70
   60 ANS2=INS2(DS,WGT,1,EXW,EXPSI)
      ANS1=INS1(DS,WGT,1,EXW,EXPSI)
      FONCT=SNGL(ANS2/ANS1**2 )
         REFF=1./FONCT
   70 IF (ITYP.EQ.3) RETURN
         REFF=REFF*(G0**2)/G1
       RETURN
       END
C
C
C----------------------------------------------------------------------
C
      SUBROUTINE AIREFQ(T,EXPSI,EXU,EXW,ITYPE,NU,MU,SIGMX,
     *           UPPER,TIL,TAU,NOBS,NCOV,MDX,MDZ,MAXIT,TOL,
     *           INIT,NITMON,NIT,BETA,REFF,A,
     *           SA,SU1,SA0,SD,SS,WGT,DL,EL,SZ)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / C. RUFFIEUX
C.......................................................................
C
      EXTERNAL UZED2,EXPSI,EXU,EXW,ESPUK,INS1,INS2,INS3,INS4,ICNVA
      REAL T(MDX,NU),SZ(MDZ,NU),DL(NOBS),EL(NOBS),WGT(NOBS),SS(5*NCOV)
      INTEGER IWORK(40)
      DOUBLE PRECISION EXU,EXW,UZED2,ESPUK,INS1,INS2,INS3,INS4,
     *      UPERD,TILD,ERRSTD,ANS1,ANS2,ANS3,ANS4,DS,
     *      A(NCOV),SA(NCOV),SA0(NCOV),SU1(NCOV),SD(NU),WORK(80)
      LOGICAL NPRCHK
      COMMON/ALBEC/ZBAR2,BET2,IPP,ITP,XLCNST,SIGM
      COMMON/INTEG/UUPER,TTIL,IWORK,WORK,IER1,ERRST1
      COMMON/INTPAR/ITYP,I,NEVAL,LIMIT,KEY
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
C
C       PARAMETER CHECK AND INITIALISATION
C
      NPRCHK=MAXIT.GT.0.AND.TOL.GT.0.AND.TAU.GE.0.AND.UPPER.GT.0.
     *         .AND.TIL.GT.0..AND.SIGMX.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'AIREFQ',1)
      NN=NU*(NU+1)/2
      NPRCHK=NU.GE.1.AND.MU.GE.0.AND.NOBS.GT.0.AND.NN.EQ.NCOV
     *         .AND.MDX.GE.NOBS.AND.MDZ.GE.NU.AND.ITYPE.GE.1
     *         .AND.ITYPE.LE.3
      IF (.NOT.NPRCHK) CALL MESSGE(501,'AIREFQ',1)
      P=FLOAT(MU)
      Q=FLOAT(NU)
      FH=1.0
      SIGM=SIGMX
      ITYP=ITYPE
      LIMIT=20
      KEY=1
C
C   STEP 0: INITIALIZATIONS
C   ------
      NIT=0
      BETA=0.
C
C   SPECIAL FOR ITYPE.EQ.1.OR.(MU.EQ.0.AND.ITYPE.EQ.2)
C           Compute Epsi**2/Epsi'
C
       IF (ITYP.EQ.3) GOTO 5
       IF (IPSI.EQ.1) THEN
        IF (C.LE.0.) C=1.345
        CALL LIEPSHZ(C,G1,G0)
       ELSE
        CALL LIEPSU(EXPSI,UPPER,TIL,ERREST,G1,G0)
       ENDIF
       FH=(G0**2)/G1
       IF (ITYP.NE.1) GOTO 5
       REFF=(G0**2)/G1
       RETURN
    5  CONTINUE
      ISS2=NCOV+1
      ISS3=ISS2+NCOV
      ISS4=ISS3+NCOV
      ICOV=ISS4+NCOV
      ZBAR2=0.
      BET2=1.
      IF (MU.EQ.0) BET2=0.
      IPP=MU
      ITP=ITYP
      UPERD=DBLE(UPPER)
      UUPER=UPPER
      TILD=DBLE(TIL)
      TTIL=TIL
      XLCNST=-1.
C
C   COVARIANCE LS-ESTIMATOR (unscaled)
C
      FATT=FLOAT(NOBS)
      CALL KTASKVZ(T,NOBS,NU,MDX,NCOV,TAU,FATT,SS,SS(ICOV))
      TRCVLS=0.
      DO 155 J0=1,NU
      J1=J0*(J0+1)/2
      TRCVLS=TRCVLS+SS(ICOV+J1-1)
  155 CONTINUE
      TRCVLS=(TRCVLS)+P/(SIGMX*SIGMX)
C
C   COMPUTATION OF ALFA AND BETA (ITERATIVE ALGORITHM)
C
      DS=0.D0
      DALF=0.
      DBET=0.
      NIT=1
      CALL WIMEDVZ(T,NOBS,NU,NCOV,MDX,1,INIT,NOBS,A,DL)
      DO 10 L=1,NCOV
      SA(L)=A(L)
   10 CONTINUE
C
C  STEP 1: SOLVE FOR A
C  ------
   15 CALL AIFALG(T,SA,ESPUK,EXU,NOBS,NU,NCOV,MDX,TAU,
     *   MAXIT,1,TOL,NITW,WGT,SA0,SU1,SD)
C
C  STEP 2: SOLVE FOR BETA
C  ------
      IF (MU.EQ.0) GOTO 30
      CALL INTGRD(UZED2,WGT,NOBS,EXU,EXPSI,0.D0,UPERD,TILD,0.D0,KEY,
     *     LIMIT,ANS2,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'AIREFQ',0)
      BET2=P/SNGL(ANS2)
      BET=SQRT(BET2)
      DBET=ABS(BETA-BET)
      BETA=BET
   30 IF (NITMON.GT.0.AND.MOD(NIT,NITMON).EQ.0)
     *    CALL MONITA(NIT,NU,NCOV,BETA,SA,DBET,DALF)
C
C  STEP 3: CHECK CONVERGENCE
C  ------
      I=ICNVA(NCOV,DALF,SA,A,TOL,1)
      DO 25 L=1,NCOV
      A(L)=SA(L)
   25 CONTINUE
      IF ((DALF.LT.TOL.AND.DBET.LT.TOL).OR.(NIT.GE.MAXIT)) GOTO 40
      NIT=NIT+1
      GOTO 15
   40 IF (NIT.GE.MAXIT) CALL MESSGE(200,'AIREFQ',0)
        IF (IER1.GT.0) THEN
          CALL MESSGE (300+IER1,'ESPUK ',0)
          IER1=0
          ERRST1=0.
        ENDIF
C
C  STEP 4: COMPUTE S1 AND S2 (FOR THE QUALITATIVE COVARIATES)
C  ------
      DO 50 L=1,NOBS
      I=L
      ZBAR2=WGT(L)*WGT(L)
      IF (MU.EQ.0) THEN
        DS=0.D0
        ANS2=INS2(DS,WGT,NOBS,EXW,EXPSI)
      ELSE
        CALL INTGRD(INS2,WGT,NOBS,EXW,EXPSI,0.D0,UPERD,TILD,0.D0,KEY,
     *  LIMIT,ANS2,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'AIREFQ',0)
      ENDIF
      EL(L)=SNGL(ANS2)
      IF (MU.EQ.0) THEN
        DS=0.D0
        ANS1=INS1(DS,WGT,NOBS,EXW,EXPSI)
      ELSE
        CALL INTGRD(INS1,WGT,NOBS,EXW,EXPSI,0.D0,UPERD,TILD,0.D0,KEY,
     *       LIMIT,ANS1,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'AIREFQ',0)
      ENDIF
      DL(L)=SNGL(ANS1)
   50 CONTINUE
      FACT=1.
      JAINV=1
      CALL KTASKWZ(T,DL,EL,NOBS,NU,MDX,MDZ,NCOV,TAU,1,FACT,0.,JAINV,
     *            SS,SS(ISS2),SS(ISS3),SS(ISS4),SS(ICOV),SZ)
      TRCOV=0.
      DO 55 J0=1,NU
      J1=J0*(J0+1)/2
      TRCOV=TRCOV+SS(ICOV+J1-1)
   55 CONTINUE
      IF (MU.EQ.0) THEN
       REFF=TRCVLS/TRCOV
       GOTO 70
      ENDIF
C
C  COMPUTE S1 AND S2 FOR THE QUANTITATIVE COVARIATES
C
        CALL INTGRD(INS3,WGT,NOBS,EXW,EXPSI,0.D0,UPERD,TILD,0.D0,KEY,
     *     LIMIT,ANS3,ERRSTD,NEVAL,IER,WORK,IWORK)
        CALL INTGRD(INS4,WGT,NOBS,EXW,EXPSI,0.D0,UPERD,TILD,0.D0,KEY,
     *     LIMIT,ANS4,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'AIREFQ',0)
        IF (IER1.GT.0) THEN
         CALL MESSGE (300+IER1,'INS1/2',0)
         IER1=0
         ERRST1=0.
        ENDIF
C
C  STEP 5: COMPUTE ARE1
C  ------
      FONCT=P*P*SNGL(ANS4/(ANS3**2))
      TRCOV=TRCOV+FONCT
      REFF=(TRCVLS)/TRCOV
   70 IF (ITYP.EQ.3) RETURN
      REFF=REFF*FH
C
C  STEP 6: EXIT
C  ------
      RETURN
      END
C***********************************************************************
C***************************  C V A U X I  *****************************
C
      FUNCTION EXPU(TAU2)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : A. MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  COMPUTATION OF THE EXPECTED VALUE OF U(TAU*NORM(X))*(NORM(TAU*X)**2)
C  WHERE X IS A STANDARD P-VARIATE NORMAL VECTOR AND U IS THE
C  HUBER WEIGHT FUNCTION COMPUTED BY THE FUNCTION HBWT.
C
      COMMON/EXPUPR/XP,A21,B21
      A2=A21
      B2=B21
      IF (TAU2.GT.0.) GOTO 10
      EXPU=A2
      RETURN
   10 AT=A2/TAU2
      BT=B2/TAU2
      IFN=INT(XP)
      IF (AT.LE.0.) THEN
        P1=0.
        P4=0.
      ELSE
        P1=0.
        IF (AT.GT.0..AND.IFN.GE.1) CALL CHISQZ(1,IFN,AT,P1)
        P4=0.
        IF (AT.GT.0..AND.IFN.GE.-1) CALL CHISQZ(1,IFN+2,AT,P4)
      ENDIF
      P2=0.
      IF (BT.GT.0..AND.IFN.GE.1) CALL CHISQZ(1,IFN,BT,P2)
      P3=0.
      IF (BT.GT.0..AND.IFN.GE.-1) CALL CHISQZ(1,IFN+2,BT,P3)
      EXPU=A2*P1+B2*(1.-P2)+TAU2*XP*(P3-P4)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION EPSC(CAP)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : A. MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  AUXILIARY SUBROUTINE FOR CIA2B2.
C
      COMMON/EPSCPR/IP,TL
C
C  IF CAP=0 SET EPSC EQUAL A POSITIVE NUMBER
C
      IF (CAP.GT.0.) GOTO 10
      EPSC=1000.
      RETURN
   10 CONTINUE
C
C  COMPUTE AUXILIARY QUANTITIES
C
      XP=FLOAT(IP)
      A2=AMAX1(XP-CAP,0.)
      B2=XP+CAP
      A=SQRT(A2)
      B=SQRT(B2)
      PA=0.
      IF (A2.GT.0..AND.IP.GE.1) CALL CHISQZ(1,IP,A2,PA)
      PB=0.
      IF (B2.GT.0..AND.IP.GE.1) CALL CHISQZ(1,IP,B2,PB)
      CALL NLGMZ(IP,XLGM)
      XLCP=(1.-XP/2.)*ALOG(2.)-XLGM
C
C  COMPUTE INTEGRAL PARTS AND EPSC
C
      XI1=0.
      XI3=0.
      XI2=PB-PA
      IF (A.GT.0.) XI1=EXP(-A2/2.+XP*ALOG(A)-ALOG(XP-A2)+XLCP)
      IF (XI2.LT.1.-TL)
     1   XI3=EXP(-B2/2.+XP*ALOG(B)-ALOG(B2-XP)+XLCP)
      EPSC=XI1+XI2+XI3
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE UVCOW(X,SA,T,ST,EXU,EXUP,EXV,EXVP,EXW,EXWP,N,NP,NCOV,
     1           MDX,MDZ,NU,IALG,ICNV,ILOC,TL,DELTA,DIST,S1,S1P,S2,SR,
     2           SU,SUP,SZ,SD)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  COMPUTE WEIGHTED COVARIANCE MATRIX;
C  IF (IALG.NE.1) STORE EXU VALUES IN SU AND EXUP VALUES IN SUP;
C  EXUP, EXVP AND EXWP ARE NOT USED IF IALG=1;
C  SZ IS NOT USED IF IALG.LE.2
C  EXV(Z)=CONSTANT AND EXVP(Z)=0 IF IALG.GT.2.
C
      DIMENSION X(MDX,NP),T(NP),DIST(N),SZ(MDZ,NP)
      DOUBLE PRECISION SA(NCOV),ST(NCOV),SR(NP),SU(NU),SUP(NU),SD(NP),
     +       EXU,EXUP,EXV,EXVP,EXW,EXWP,S1,S2,S1P,DEN,U,UP,W,ZNR,TL
      EXTERNAL EXU,EXUP,EXV,EXVP,EXW,EXWP
C
      XN=FLOAT(N)
      XP=FLOAT(NP)
      DELTA=0.0
      S1=0.D0
      S1P=0.D0
      S2=0.D0
      W=0.D0
      DO 10 I=1,NP
      SR(I)=0.D0
   10 CONTINUE
      DO 20 IJ=1,NCOV
      ST(IJ)=0.D0
   20 CONTINUE
      DO 100 L=1,N
      DO  30 J=1,NP
      SD(J)=DBLE(X(L,J)-T(J))
   30 CONTINUE
      CALL MLYZD(SA,SD,NP,NCOV,NP,1)
      CALL NRM2ZD(SD,NP,1,NP,ZNR)
      DISTL=SNGL(ZNR)
      IF (ICNV.EQ.2) DELTA=AMAX1(DELTA,ABS(DISTL-DIST(L)))
      DIST(L)=DISTL
      U=EXU(DISTL)
      S1=S1+EXV(DISTL)
      IF (ILOC.EQ.0) GOTO 40
      W=EXW(DISTL)
      S2=S2+W
   40 IF (IALG.EQ.1) GOTO 60
      UP=EXUP(DISTL)
      IF (ILOC.EQ.1) S2=S2+EXWP(DISTL)*ZNR/DBLE(XP)
      IF (IALG.EQ.2) THEN
        S1P=S1P+EXVP(DISTL)*ZNR
        GOTO 60
      ENDIF
      DO 50 I=1,NP
      SZ(L,I)=SNGL(SD(I))
   50 CONTINUE
   60 IJ=0
      DO 90 I=1,NP
      IF (ILOC.EQ.1) SR(I)=SR(I)+DBLE(X(L,I)-T(I))*W
      DO 80 J=1,I
      IJ=IJ+1
      ST(IJ)=ST(IJ)+(SD(I)*U)*SD(J)
   80 CONTINUE
   90 CONTINUE
      IF (IALG.EQ.1) GOTO 100
      SU(L)=U
      SUP(L)=UP
  100 CONTINUE
      DEN=DBLE(XN)
      IF (IALG.NE.2.AND.DABS(S1).GT.TL) DEN=S1
      DO 110 IJ=1,NCOV
      ST(IJ)=ST(IJ)/DEN
  110 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PRSCF0(SU1,NP,NCOV,TAU,INFO)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PRESCRIPTION F0 (TO BE USED WITH IALG=1)
C
      DOUBLE PRECISION SU1(NCOV)
      CALL MCHLZD(SU1,NP,NCOV,INFO)
      IF (INFO.EQ.0) GOTO 100
      INFO=1
      RETURN
  100 CALL MINVZD(SU1,NP,NCOV,TAU,INFO)
      IF (INFO.NE.0) INFO=2
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PRSCNH(SU1,SS,DIST,SU,SUP,SV,SVPZ,N,NP,NCOV)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  MODIFIED NEWTON PRESCRIPTION
C  IALG=2 FOR PRESCRIPTION NH;
C
      DIMENSION DIST(N)
      DOUBLE PRECISION SU1(NCOV),SS(NCOV),SU(N),SUP(N),SV,SVPZ,A,B
      DATA TL/1.E-10/
      XN=FLOAT(N)
      XP=FLOAT(NP)
      C1=SNGL(SVPZ)/XN
      D1=SNGL(SV)/XN
      A=0.D0
      B=0.D0
      GOTO 20
   10 XPDEN=1.0
      BTMD=-D1
      GOTO 40
   20 DO 30 L=1,N
        ZNR=DIST(L)
        ZNR2=ZNR*ZNR
        A=A+SU(L)*DBLE(ZNR2)
        B=B+SUP(L)*DBLE(ZNR*ZNR2)
   30 CONTINUE
      A1=SNGL(A)/XN
      B1=SNGL(B)/XN/(XP+2.0)
      DEN=A1+B1
      IF (ABS(DEN).LT.TL) GOTO 10
      TDEN=2.0*DEN+XP*(B1-C1)
      IF (ABS(TDEN).LT.TL) GOTO 10
      T=(D1*XP-A1)/TDEN
      XPDEN=XP/DEN
      BTMD=(B1-C1)*T-D1
   40 IJ=0
      DO 70 I=1,NP
      IF (I.EQ.1) GOTO 60
      I1=I-1
      DO 50 J=1,I1
      IJ=IJ+1
      SS(IJ)=SU1(IJ)*DBLE(XPDEN)
   50 CONTINUE
   60 IJ=IJ+1
      SS(IJ)=(SU1(IJ)+DBLE(BTMD))*DBLE(XPDEN/2.)
   70 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PRSCCG(SU1,SU2,SS,N,NP,NCOV,MDZ,NIT,TAU,SV,
     1                  DIST,SU,SUP,SY1,SY2,SZ)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PRESCRIPTION CG (TO BE USED WITH IALG=3)
C
      DIMENSION DIST(N),SZ(MDZ,NP)
      DOUBLE PRECISION SU1(NCOV),SU2(NCOV),SS(NCOV),SY1(NP),SY2(NP)
      DOUBLE PRECISION SU(N),SUP(N),U,UP,SV,BETA1,BETA2,ALFA11,ALFA12,
     1   ALFA21,ALFA22,DSZ,S1,S2,S3,S4,S5,T1,T2,T11,T12,T22,TAU,DEN
      DATA GAM/1.E-3/
C
      II=0
      DO 10 I=1,NP
      II=II+I
      SU1(II)=SU1(II)-1.D0
   10 CONTINUE
      IF (NIT.EQ.0) THEN
        DO 20 IJ=1,NCOV
        SU2(IJ)=0.D0
   20 CONTINUE
      ELSE
        DO 30 IJ=1,NCOV
        SU2(IJ)=GAM*SS(IJ)
   30 CONTINUE
      ENDIF
C
C COMPUTE BETA1,BETA2,ALFA11,ALFA12,ALFA21,ALFA22
C
      BETA1=0.D0
      BETA2=0.D0
      ALFA11=0.D0
      ALFA12=0.D0
      ALFA21=0.D0
      ALFA22=0.D0
      DO 300 L=1,N
      DO 230 J=1,NP
      DSZ=DBLE(SZ(L,J))
      SS(J)=DSZ
      SY2(J)=DSZ
      SY1(J)=DSZ
  230 CONTINUE
      CALL MLYZD(SU1,SY1,NP,NCOV,NP,1)
      CALL DOTPZD(SY2,SY1,NP,1,1,NP,NP,S1)
      CALL DOTPZD(SY1,SY1,NP,1,1,NP,NP,S3)
      ZNR=DIST(L)
      U=SU(L)
      IF (ZNR.GE.GAM) GOTO 250
      CALL MESSGE(201,'PRSCCG',0)
      ZNR=GAM
  250 UP=SUP(L)/DBLE(ZNR)
      BETA1=BETA1+U*S1
      ALFA11=ALFA11+(UP*S1)*S1+U*S3
      IF (NIT.EQ.0) GOTO 300
      CALL MLYZD(SU2,SY2,NP,NCOV,NP,1)
      CALL DOTPZD(SS,SY2,NP,1,1,NP,NP,S2)
      CALL DOTPZD(SY2,SY2,NP,1,1,NP,NP,S4)
      CALL DOTPZD(SY1,SY2,NP,1,1,NP,NP,S5)
      BETA2=BETA2+U*S2
      ALFA22=ALFA22+(UP*S2)*S2+U*S4
      ALFA12=ALFA12+(UP*S1)*S2+U*S5
  300 CONTINUE
C
      T1=0.D0
      T2=0.D0
      T11=0.D0
      T12=0.D0
      T22=0.D0
      IJ=0
      DO 310 I=1,NP
      IJ=IJ+I
      T1=T1+SU1(IJ)
      T11=T11+SU1(IJ)*SU1(IJ)
      IF (NIT.EQ.0) GOTO 310
      T2=T2+SU2(IJ)
      T12=T12+SU1(IJ)*SU2(IJ)
      T22=T22+SU2(IJ)*SU2(IJ)
  310 CONTINUE
C
      BETA1=-T1+BETA1/SV
      ALFA11=T11+ALFA11/SV
      IF (NIT.EQ.0) GOTO 320
      BETA2=-T2+BETA2/SV
      ALFA22=T22+ALFA22/SV
      ALFA12=T12+ALFA12/SV
      ALFA21=ALFA12
  320 CONTINUE
C
C  FIND IMPROVEMENT STEP
C
      S1=BETA1/ALFA11
      S2=0.0
      DEN=ALFA21*ALFA12-ALFA22*ALFA11
      IF (DABS(DEN).LT.TAU) CALL MESSGE(150,'PRSCCG',0)
      IF (NIT.EQ.0.OR.DABS(DEN).LE.TAU) GOTO 420
      S1=(BETA2*ALFA12-BETA1*ALFA22)/DEN
      S2=(BETA1*ALFA21-BETA2*ALFA11)/DEN
  420 CONTINUE
C
C  FIND IMPROVEMENT MATRIX
C
      IJ=0
      DO 520 I=1,NP
      DO 510 J=1,I
      IJ=IJ+1
      SS(IJ)=S1*SU1(IJ)+S2*SU2(IJ)
  510 CONTINUE
  520 CONTINUE
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE FUDGE(SS,NP,NCOV,XKAP,GAMMA)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  COMPUTE FUDGE FACTOR GAMMA
C
      DOUBLE PRECISION SS(NCOV)
      E=0.0
      DO 10 I=1,NP
      II=I*(I+1)/2
      SII=SNGL(SS(II))
      E=AMAX1(E,ABS(SII))
   10 CONTINUE
      GAMMA=1.0/AMAX1(1.0,XKAP*E)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE UPDATA(SS,SA0,SA,GAMMA,NP,NCOV)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION SS(NCOV),SA0(NCOV),SA(NCOV),GAMD
      IJ=0
      GAMD=DBLE(-GAMMA)
      DO 20 I=1,NP
      DO 10 J=1,I
      IJ=IJ+1
      SA(IJ)=GAMD*SS(IJ)
      IF (I.EQ.J) SA(IJ)=1.D0+SA(IJ)
   10 CONTINUE
   20 CONTINUE
      CALL MTT3ZD(SA0,SA,SA,NP,NCOV)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION ICNVA(NCOV,DELTA,SA,SA0,TOL,ICNV)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION SA(NCOV),SA0(NCOV),SDMAX
      ICNVA=0
      IF (ICNV.EQ.1) THEN
        DO 10 IJ=1,NCOV
        SA0(IJ)=SA(IJ)-SA0(IJ)
   10   CONTINUE
        CALL NRM2ZD(SA0,NCOV,1,NCOV,SDMAX)
        DELTA=SNGL(SDMAX)
      ENDIF
      IF (DELTA.LT.TOL) ICNVA=1
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION ICNVH(NVAR,HMAX,H,TOL,ICNV)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION H(NVAR),HDMAX
      ICNVH=0
      IF (ICNV.EQ.1) THEN
        CALL NRM2ZD(H,NVAR,1,NVAR,HDMAX)
        HMAX=SNGL(HDMAX)
      ENDIF
      IF (HMAX.LT.TOL) ICNVH=1
      RETURN
      END
C-----------------------------------------------------------------------
C
C                 R O B E T H  FORTRAN Source
C
C  File CVMAIN.F  Main subroutines of Chapter 8
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CIMEDVZ(X,NOBS,NVAR,NCOV,MDX,NFIRST,ILOC,A,T,SC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C  INITIAL VALUES FOR A AND T
C
      DIMENSION X(MDX,NVAR),T(NVAR),SC(NFIRST)
      DOUBLE PRECISION A(NCOV)
      LOGICAL NPRCHK
      DATA TL/1.E-10/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=NVAR.GT.0.AND.NOBS.GE.NVAR.AND.NN.EQ.NCOV.AND.NFIRST.GT.0
     1       .AND.MDX.GE.NOBS.AND.(ILOC.EQ.0.OR.ILOC.EQ.1)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'CIMEDV',1)
C
C  COMPUTE INITIAL VALUES FOR A AND T
C
      N0=MIN0(NFIRST,NOBS)
      DO 20 I=1,NCOV
      A(I)=0.D0
   20 CONTINUE
      DO 50 J=1,NVAR
      CALL LMDDZ(X(1,J),SC,N0,1,XME,XMD,XSD)
      IF (ILOC.EQ.1) T(J)=XME
      SQDEV2=SQRT(XSD**2+(XME-T(J))**2)
      JJ=(J*J+J)/2
      IF (SQDEV2.GT.TL) GOTO 45
      CALL MESSGE(301,'CIMEDV',0)
      A(JJ)=9999.D0
      GOTO 50
   45 A(JJ)=1.D0/DBLE(SQDEV2)
   50 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CYFALG(X,A,T,EXU,EXV,EXW,NOBS,NVAR,NCOV,MDX,
     1                  TAU,MAXIT,NITMON,ILOC,ICNV,TOL,NIT,DIST,
     2                  SA,ST,SR,SD)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C
C  FIXED POINT ALGORITHM FOR ROBUST AFFINE INVARIANT COVARIANCES
C
      DIMENSION X(MDX,NVAR),T(NVAR),DIST(NOBS)
      DOUBLE PRECISION A(NCOV),SA(NCOV),SR(NVAR),ST(NCOV),
     +       SU(1),SUP(1),SD(NVAR),SV,SW,TL,EXU,EXV,EXW
      EXTERNAL EXU,EXV,EXW,ICNVA,ICNVH
      LOGICAL NPRCHK
      DATA TL/1.D-7/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=NVAR.GT.0.AND.NOBS.GE.NVAR.AND.NCOV.EQ.NN
     1       .AND.MDX.GE.NOBS.AND.(ILOC.EQ.0.OR.ILOC.EQ.1)
     2       .AND.TAU.GE.0.0.AND.(ICNV.EQ.1.OR.ICNV.EQ.2)
     3       .AND.TOL.GT.0.0.AND.MAXIT.GT.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'CYFALG',1)
C
C  STEP 0 : INITIALIZATION
C  ------
      IALG=1
      NU=1
      NIT=0
      HMAX=10.*TOL
      DO 10 I=1,NVAR
      SR(I)=DBLE(HMAX)
   10 CONTINUE
      IF (ICNV.EQ.1) THEN
        L=0
        DO 30 I=1,NVAR
        DO 20 J=1,I
        L=L+1
        SA(L)=0.D0
        IF (I.EQ.J) SA(L)=-1.D0
   20   CONTINUE
   30   CONTINUE
      ENDIF
      DO 40 L=1,NOBS
      DIST(L)=-1.0
   40 CONTINUE
C
C  STEP 1: COMPUTE WEIGHTED COVARIANCE (ST) AND AUXILIARY VALUES
C  ------
  100 CALL UVCOW(X,A,T,ST,EXU,EXU,EXV,EXV,EXW,EXW,NOBS,NVAR,NCOV,MDX,
     1     MDX,NU,IALG,ICNV,ILOC,TL,DELTA,DIST,SV,SV,SW,SR,SU,SUP,X,SD)
      IF (DABS(SV).LE.TL) CALL MESSGE(401,'CYFALG',0)
C
C  STEP 2: CHECK CONVERGENCE
C  ------
      IF (NIT.EQ.MAXIT) GOTO 600
      IF (ICNVA(NCOV,DELTA,A,SA,TOL,ICNV).EQ.1) THEN
         IF (ILOC.EQ.0) GOTO 600
         IF (ICNVH(NVAR,HMAX,SR,TOL,ICNV).EQ.1) GOTO 600
      ENDIF
C
C  STEP 3: FIND IMPROVEMENT MATRIX I-SS FOR A
C  ------
      INFO=0
      CALL PRSCF0(ST,NVAR,NCOV,TAU,INFO)
      IF (INFO.NE.0) CALL MESSGE(410+INFO,'CYFALG',0)
C
C  STEP 4: FIND AN IMPROVMENT VECTOR H FOR T
C  ------
      IF (ILOC.EQ.0) GOTO 500
      IF (DABS(SW).LE.TL) CALL MESSGE(402,'CYFALG',0)
      IF (DABS(SV).LE.TL.OR.DABS(SW).LE.TL) RETURN
      HMAX=0.
      DO 400 I=1,NVAR
        SR(I)=SR(I)/SW
        SRI=SNGL(SR(I))
        HMAX=AMAX1(HMAX,ABS(SRI))
        T(I)=T(I)+SRI
  400 CONTINUE
C
C  STEP 5: SET SA:=A AND A:=(I-SS)*SA
C  ------
  500 DO 510 IJ=1,NCOV
      SA(IJ)=A(IJ)
  510 CONTINUE
      CALL MTT3ZD(SA,ST,A,NVAR,NCOV)
      NIT=NIT+1
C
C  STEP 5A: ITERATION MONITORING
C  -------
      IF (NITMON.LE.0) GOTO 100
      IF (MOD(NIT,NITMON).EQ.0)CALL MONITC(NIT,NVAR,NCOV,T,A,HMAX,DELTA)
      GOTO 100
C
C  STEP 6: EXIT
C  ------
  600 RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE CYNALG(X,A,T,EXU,EXUP,EXV,EXVP,EXW,EXWP,NOBS,NVAR,
     1                  NCOV,MDX,MAXIT,NITMON,ILOC,ICNV,TOL,XFUD,NIT,
     2                  DIST,SA,SS,SU,SUP,ST,SD)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / R. DUTTER / A. RANDRIAMIHARISOA
C.......................................................................
C
C
C  NEWTON-HUBER ALGORITHM FOR ROBUST AFFINE INVARIANT COVARIANCES
C
      DIMENSION X(MDX,NVAR),T(NVAR),DIST(NOBS)
      DOUBLE PRECISION A(NCOV),SU(NOBS),SUP(NOBS),SA(NCOV),SS(NCOV),
     +                 SD(NVAR),ST(NCOV),SV,SVP,SW,TL,
     +                 EXU,EXUP,EXV,EXVP,EXW,EXWP
      EXTERNAL EXU,EXUP,EXV,EXVP,EXW,EXWP,ICNVA,ICNVH
      LOGICAL NPRCHK
      DATA TL/1.D-7/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=NVAR.GT.0.AND.NOBS.GE.NVAR.AND.NCOV.EQ.NN
     1       .AND.MDX.GE.NOBS.AND.(ILOC.EQ.0.OR.ILOC.EQ.1)
     2       .AND.(ICNV.EQ.1.OR.ICNV.EQ.2)
     3       .AND.TOL.GT.0.0.AND.MAXIT.GT.0.AND.XFUD.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'CYNALG',1)
C
C  STEP 0 : INITIALIZATION
C  ------
      IALG=2
      NU=NOBS
      NIT=0
      HMAX=10.*TOL
      DO 10 I=1,NVAR
      SD(I)=DBLE(HMAX)
   10 CONTINUE
      IF (ICNV.EQ.1) THEN
        L=0
        DO 30 I=1,NVAR
        DO 20 J=1,I
        L=L+1
        SA(L)=0.D0
        IF (I.EQ.J) SA(L)=-1.D0
   20   CONTINUE
   30   CONTINUE
      ENDIF
      DO 40 L=1,NOBS
      DIST(L)=-1.0
   40 CONTINUE
C
C  STEP 1: COMPUTE WEIGHTED COVARIANCE (ST) AND AUXILIARY VALUES
C  ------
  100 CALL UVCOW(X,A,T,ST,EXU,EXUP,EXV,EXVP,EXW,EXWP,NOBS,NVAR,NCOV,MDX,
     1     MDX,NU,IALG,ICNV,ILOC,TL,DELTA,DIST,SV,SVP,SW,SD,SU,SUP,X,SS)
C
C  STEP 2: CHECK CONVERGENCE
C  ------
      IF (NIT.EQ.MAXIT) GOTO 600
      IF (ICNVA(NCOV,DELTA,A,SA,TOL,ICNV).EQ.1) THEN
         IF (ILOC.EQ.0) GOTO 600
         IF (ICNVH(NVAR,HMAX,SD,TOL,ICNV).EQ.1) GOTO 600
      ENDIF
C
C  STEP 3: FIND IMPROVEMENT MATRIX SS FOR A
C  ------
      CALL PRSCNH(ST,SS,DIST,SU,SUP,SV,SVP,NOBS,NVAR,NCOV)
C
C  STEP 4: FIND IMPROVMENT VECTOR H FOR T
C  ------
      IF (ILOC.EQ.0) GOTO 500
      IF (DABS(SW).LE.TL) THEN
        CALL MESSGE(402,'CYNALG',0)
        RETURN
      ENDIF
      HMAX=0.
      DO 400 I=1,NVAR
        SD(I)=SD(I)/SW
        SDI=SNGL(SD(I))
        HMAX=AMAX1(HMAX,ABS(SDI))
        T(I)=T(I)+SDI
  400 CONTINUE
C
C  STEP 5: Compute GAM0, Set SA:=A and A:=(I-GAM0*SS)*SA
C  -------
  500 DO 510 IJ=1,NCOV
      SA(IJ)=A(IJ)
  510 CONTINUE
      CALL FUDGE(SS,NVAR,NCOV,XFUD,GAM0)
      CALL UPDATA(SS,SA,A,GAM0,NVAR,NCOV)
      NIT=NIT+1
C
C  STEP 5A: ITERATION MONITORING
C  -------
      IF (NITMON.LE.0) GOTO 100
      IF (MOD(NIT,NITMON).EQ.0)CALL MONITC(NIT,NVAR,NCOV,T,A,HMAX,DELTA)
      GOTO 100
C
C  STEP 6: EXIT
C  ------
  600 RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CYGALG(X,A,T,EXU,EXUP,EXV,EXW,EXWP,NOBS,NVAR,NCOV,
     1                  MDX,MDZ,MAXIT,NITMON,ILOC,ICNV,TOL,XFUD,NIT,
     2                  DIST,SA,SS,SZ,SU,SUP,SY1,SY2,SD,ST,ST1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C
C  CONJUGATE GRADIENT ALGORITHM FOR ROBUST AFFINE INVARIANT COVARIANCES
C
      DIMENSION X(MDX,NVAR),T(NVAR),SZ(MDZ,NVAR),DIST(NOBS)
      DOUBLE PRECISION A(NCOV),SA(NCOV),SS(NCOV),SU(NOBS),SUP(NOBS),
     1       SY1(NVAR),SY2(NVAR),SD(NVAR),ST(NCOV),ST1(NCOV),SV,SW,TL,
     2       EXU,EXUP,EXV,EXW,EXWP
      EXTERNAL EXU,EXUP,EXV,EXW,EXWP,ICNVA,ICNVH
      LOGICAL NPRCHK
      DATA TL/1.D-7/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=NVAR.GT.0.AND.NOBS.GE.NVAR.AND.NCOV.EQ.NN
     1       .AND.MDX.GE.NOBS.AND.MDZ.GE.NOBS.AND.(ILOC.EQ.0
     2       .OR.ILOC.EQ.1).AND.(ICNV.EQ.1.OR.ICNV.EQ.2)
     3       .AND.TOL.GT.0.0.AND.MAXIT.GT.0.AND.XFUD.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'CYGALG',1)
C
C  STEP 0 : INITIALIZATION
C  ------
      IALG=3
      NU=NOBS
      NIT=0
      HMAX=10.*TOL
      DO 10 I=1,NVAR
      SD(I)=DBLE(HMAX)
   10 CONTINUE
      IF (ICNV.EQ.1) THEN
        L=0
        DO 30 I=1,NVAR
        DO 20 J=1,I
        L=L+1
        SA(L)=0.D0
        IF (I.EQ.J) SA(L)=-1.D0
   20   CONTINUE
   30   CONTINUE
      ENDIF
      DO 40 L=1,NOBS
      DIST(L)=-1.0
   40 CONTINUE
C
C  STEP 1: COMPUTE WEIGHTED COVARIANCE (ST) AND AUXILIARY VALUES
C  ------
  100 CALL UVCOW(X,A,T,ST,EXU,EXUP,EXV,EXV,EXW,EXWP,NOBS,NVAR,NCOV,MDX,
     1   MDZ,NU,IALG,ICNV,ILOC,TL,DELTA,DIST,SV,SV,SW,SD,SU,SUP,SZ,SY2)
      IF (DABS(SV).LE.TL) CALL MESSGE(401,'CYGALG',0)
C
C  STEP 2: CHECK CONVERGENCE
C  ------
      IF (NIT.EQ.MAXIT) GOTO 600
      IF (ICNVA(NCOV,DELTA,A,SA,TOL,ICNV).EQ.1) THEN
         IF (ILOC.EQ.0) GOTO 600
         IF (ICNVH(NVAR,HMAX,SD,TOL,ICNV).EQ.1) GOTO 600
      ENDIF
C
C  STEP 3: FIND IMPROVEMENT MATRIX SS FOR A
C  ------
      CALL PRSCCG(ST,ST1,SS,NOBS,NVAR,NCOV,MDZ,NIT,TL,SV,
     1            DIST,SU,SUP,SY1,SY2,SZ)
C
C  STEP 4: FIND IMPROVEMENT VECTOR H FOR T
C  ------
      IF (ILOC.EQ.0) GOTO 500
      IF (DABS(SW).LE.TL) CALL MESSGE(402,'CYGALG',0)
      IF (DABS(SV).LE.TL.OR.DABS(SW).LE.TL) RETURN
      HMAX=0.
      DO 400 I=1,NVAR
        SD(I)=SD(I)/SW
        SDI=SNGL(SD(I))
        HMAX=AMAX1(HMAX,ABS(SDI))
        T(I)=T(I)+SDI
  400 CONTINUE
C
C  STEP 5: Compute GAM0, Set SA:=A and A:=(I-GAM0*SS)*SA
C  -------
  500 DO 510 IJ=1,NCOV
      SA(IJ)=A(IJ)
  510 CONTINUE
      CALL FUDGE(SS,NVAR,NCOV,XFUD,GAM0)
      CALL UPDATA(SS,SA,A,GAM0,NVAR,NCOV)
      NIT=NIT+1
C
C  STEP 5A: ITERATION MONITORING
C  -------
      IF (NITMON.LE.0) GOTO 100
      IF (MOD(NIT,NITMON).EQ.0)CALL MONITC(NIT,NVAR,NCOV,T,A,HMAX,DELTA)
      GOTO 100
C
C  STEP 6: EXIT
C  ------
  600 RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE CFRCOVZ(A,NVAR,NCOV,FC,TAU,AINV,COV)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DIMENSION AINV(NCOV),COV(NCOV)
      DOUBLE PRECISION A(NCOV)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=NVAR.GT.0.AND.NCOV.EQ.NN.AND.FC.GT.0..AND.TAU.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'CFRCOV',0)
C
C  COMPUTE INVERSE OF A
C
      DO 20 I=1,NCOV
      AINV(I)=SNGL(A(I))
   20 CONTINUE
      CALL MINVZ(AINV,NVAR,NCOV,TAU,ISING)
      IF (ISING.NE.1) GOTO 30
      CALL MESSGE(401,'CFRCOV',0)
      RETURN
   30 CONTINUE
C
C  COMPUTE COVARIANCE MATRIX
C
      CALL MTT2Z(AINV,COV,NVAR,NCOV)
      CALL SCALZ(COV,FC,NCOV,1,NCOV)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CICLOCZ(EPS,TOL,C)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=EPS.GT.0..AND.EPS.LT.1..AND.TOL.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'CICLOC',1)
C
      CONST=(EPS-2.)/(1.-EPS)/2.
      C=0.
   20 CALL XERFZ(2,C,EX)
      CALL GAUSSZ(1,C,PH)
      F=EX+C*(PH+CONST)
      IF (ABS(F).LT.TOL) GOTO 30
      FP=PH+CONST
      C=C-F/FP
      GOTO 20
   30 RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CIA2B2Z(EPS,NVAR,TOL,MAXIT,A2,B2)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      COMMON/EPSCPR/IP,TL
      EXTERNAL EPSC
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=EPS.GT.0..AND.EPS.LT.1..AND.TOL.GT.0..AND.NVAR.GT.0
     1       .AND.MAXIT.GT.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'CIA2B2',1)
C
      IP=NVAR
      XP=FLOAT(IP)
      EP=1./(1.-EPS)
      TL=TOL
      A=1.
      B=1.
   20 FB=EPSC(B)-EP
      IF (FB.LT.0.) GOTO 30
      A=B
      B=B+1.
      GOTO 20
   30 FA=EPSC(A)-EP
      IF (FA.GT.0.) GOTO 40
      B=A
      A=A/2.
      GOTO 30
   40 CALL RGFL(EPSC,EP,A,B,TOL,MAXIT,ROOT,ITERM)
      IF (ITERM.EQ.1) GOTO 50
      CALL MESSGE(101,'CIA2B2',0)
   50 CAP=ROOT
      A2=AMAX1(XP-CAP,0.)
      B2=XP+CAP
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CIFACTZ(A2,B2,NVAR,TOL,MAXIT,FC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      COMMON/EXPUPR/XP,A21,B21
      LOGICAL NPRCHK
      EXTERNAL EXPU
C
C  PARAMETER CHECK
C
      XP=FLOAT(NVAR)
      NPRCHK=A2.GE.0..AND.A2.LT.XP.AND.B2.GT.XP.AND.
     1       TOL.GT.0..AND.MAXIT.GT.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'CIFACT',1)
C
      A21=A2
      B21=B2
      A=0.
      B=1.
   20 FB=EXPU(B)-XP
      IF (FB.GT.0.) GOTO 30
      A=B
      B=B+1.
      GOTO 20
   30 CALL RGFL(EXPU,XP,A,B,TOL,MAXIT,ROOT,ITERM)
      IF (ITERM.EQ.1) GOTO 40
      CALL MESSGE(101,'CIFACT',0)
   40 FC=ROOT
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CIBEATZ(A2,B2,NVAR,D)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C
      COMMON/EXPUPR/XP,A21,B21
      LOGICAL NPRCHK
      EXTERNAL EXPU
C
C  PARAMETER CHECK
C
      XP=FLOAT(NVAR)
      NPRCHK=A2.GE.0..AND.A2.LT.XP.AND.B2.GT.XP
      IF (.NOT.NPRCHK) CALL MESSGE(500,'CIBEAT',1)
C
      A21=A2
      B21=B2
      TAU2=1.
      D=EXPU(TAU2)/XP
      RETURN
      END
C
C==========================================================================
C
      DOUBLE PRECISION FUNCTION USSANS(DX,WGT,N,EXU,EXV)
      DIMENSION WGT(N)
      DOUBLE PRECISION EXU,DX,XX
      EXTERNAL EXU,EXV
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
      DATA NCALL,XLCNST/0,0./
      I=INT(WGT(1))
      S=SNGL(DX)
      IF (NCALL.NE.NP) THEN
        XX=EXU(0.)
        P=EXV(0.)
        CALL NLGMZ(NP,GL)
        P=FLOAT(NP)
        XLCNST=(1.0-P/2.)*ALOG(2.)-GL
        NCALL=NP
      ENDIF
      CALL XERPZ(NP,XLCNST,S,ANS)
C     GOTO (10,20,30,40) I
      IF (I.EQ.2) GOTO 20
      IF (I.EQ.3) GOTO 30
      IF (I.EQ.4) GOTO 40
      USSANS=(DX**2)*DBLE(ANS)
      RETURN
   20 ZED=S*(1.0-((S-EM)/CR)**2)
      XX=DBLE(ZED)**2
      USSANS=XX*DBLE(ANS)
      RETURN
   30 USSANS=0.D0
      RETURN
   40 ZED=(S**2)/(ENU+S)
      USSANS=DBLE(ZED)*DBLE(ANS)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE INTUSS(X,NREP,IWGT,TIL,SUM)
      DOUBLE PRECISION USSANS,TILD,ERRSTD,WORK,UCV,LO,HI,SS(2)
      REAL WGT(1),X(NREP)
      EXTERNAL UCV,USSANS,PSY
      COMMON/INTEGN/AINTEG(4),IWORK(80),WORK(320)
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
C
C     INITIALISATION
C
      LIMIT=80
      KEY=1
      HI=0.D0
      SUM=0.
      TILD=DBLE(TIL)
      I=0
  100 I=I+1
      WGT(1)=IWGT+FLOAT(I)
      LO=HI
      IF (I.GT.NREP) GOTO 200
      HI=DBLE(X(I))
      IF (LO.GE.HI) THEN
        SS(I)=0.D0
        GOTO 100
      ENDIF
      CALL INTGRD(USSANS,WGT,1,UCV,PSY,LO,HI,TILD,TILD,
     1            KEY,LIMIT,SS(I),ERRSTD,NEVAL,IER,WORK,IWORK)
      SUM=SUM+SNGL(SS(I))
      GOTO 100
  200 SUM=SUM/FLOAT(NP)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE CITYLR(ENU,NVAR,VD)
C.......................................................................
C
C   COPYRIGHT 2000 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      REAL X(1),NU
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,BT,CW
      COMMON/UCV56/PM,PCR,PK,NNP,NU,V7
      LOGICAL NPRCHK
      EXTERNAL EXPU
C
C  PARAMETER CHECK
C
      XP=FLOAT(NVAR)
      NPRCHK=ENU.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'CITYLR',1)
C
      NNP=NVAR
      NU=ENU
      IUCV=7
      TOL=1.E-4
      X(1)=10.0
      CALL INTUSS(X,1,3,TOL,VD)
      V7=VD
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE CIROCKZ(EM,CR,NVAR,IOPT,VK)
C.......................................................................
C
C   COPYRIGHT 2000 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      REAL X(2),NU
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,BT,CW
      COMMON/UCV56/PM,PCR,PK,NNP,NU,V7
      LOGICAL NPRCHK
      EXTERNAL EXPU
C
C  PARAMETER CHECK
C
      XP=FLOAT(NVAR)
      NPRCHK=EM.GT.0..AND.CR.GT.0.AND.(IOPT.EQ.1.OR.IOPT.EQ.2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'CIROCK',1)
C
      PM=EM
      PCR=CR
      NNP=NVAR
      IUCV=5
      IF (IOPT.EQ.2) IUCV=6
      TOL=1.E-4
      X(1)=EM
      X(2)=EM+CR
      CALL INTUSS(X,2,0,TOL,VK)
      PK=VK
      RETURN
      END

C-----------------------------------------------------------------------
C
C                 R O B E T H  FORTRAN Source
C
C  File DFCOMN.F  Routines for default values
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DFRPAR(X,N,NP,MDX,ETYPE,UPAR,PSIPAR,
     1                  ITYPW,ITYPE,ISIGMA)
C.......................................................................
C
C   COPYRIGHT  1992  Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  SET THE PARAMETER VALUES OF THE COMMON/UCVPR/, /WWWPR/
C  AND /PSIPR/ ACCORDING TO THE ESTIMATOR TYPE
C
      CHARACTER*(7) ETYPE,CP*7,CC*1
      REAL X(MDX,NP)
      DOUBLE PRECISION SUMNRM
      LOGICAL NPRCHK
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,BT,CW
      COMMON/UCV56/EM,CR,VK,NNP,ENU,V7
      COMMON/ESTIM/IEST
      COMMON/WWWPR/IWWW
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      DATA K0,L0,L1,CP/-1,0,0,'       '/
      IF (K0.NE.-1) GOTO 5
      K0=ICHAR('A')
      L0=ICHAR('a')
      L1=L0+25
      IUCV=0
      IWWW=0
      IPSI=0
      A2=0.
      B2=0.
      CHK=0.
      CKW=0.
      BB=0.
      BT=0.
      CW=0.
      EM=0.
      CR=0.
      VK=0.
      NNP=NP
      C=0.
      H1=0.
      H2=0.
      H3=0.
      XK=0.
      D=0.
C
    5 NPRCHK=N.GT.0.AND.NP.GT.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'DFRPAR',1)
      CP=ETYPE
      DO 7 I=1,7
       CC=CP(I:I)
       J=ICHAR(CC)
       IF ((L0.LE.J).AND.(J.LE.L1)) THEN
         K=J-L0+K0
         CC=CHAR(K)
         CP(I:I)=CC
       ENDIF
    7 CONTINUE
      P=FLOAT(NP)
      C=0.
      IEST=5
      IF (CP(1:3).EQ.'OLS') GOTO 10
      IEST=6
      IF (CP(1:3).EQ.'LAR') GOTO 10
      IEST=1
      IF (CP(1:5).EQ.'HUBER') GOTO 15
      IEST=2
      IF (CP.EQ.'MAL-STD') GOTO 20
      IEST=3
      IF (CP.EQ.'KRA-WEL') GOTO 25
      IEST=4
      IF (CP.EQ.'MAL-HAM') GOTO 30
      IEST=7
      IF (CP.EQ.'HAM-KRA') GOTO 35
      IEST=8
      IF (CP.EQ.'MAL-UNS') GOTO 40
      IEST=9
      IF (CP.EQ.'MAL-TAU') GOTO 45
      IEST=10
      IF (CP.EQ.'SCH-TAU') GOTO 50
      IEST=11
      IF (CP(1:3).EQ.'LMS') GOTO 55
      IEST=12
      IF (CP(1:3).EQ.'LTS') GOTO 60
      IEST=13
      IF (CP(1:1).EQ.'S') GOTO 65
      IEST=14
      IF (CP.EQ.'ROCKE1') GOTO 70
      IEST=15
      IF (CP.EQ.'ROCKE2') GOTO 75
      IEST=0
      CALL MESSGE(500,'DFRPAR',1)
C
C ORDINARY LEAST SQUARES
C
   10 IUCV=0
      IWWW=0
      RETURN
C
C LEAST ABSOLUTE RESIDUALS
C
C     GOTO 10
C
C HUBER CASE
C
   15 IPSI=1
      IF (PSIPAR.LT.0.) THEN
        C=1.345
        D=1.345
      ELSE
        C=PSIPAR
        D=C
      ENDIF
      ITYPE=1
      ISIGMA=1
      IUCV=0
      IWWW=0
      RETURN
C
C MALLOWS STANDARD
C
   20 IWWW=3
   21 IUCV=1
      A2=0.
      IF (UPAR.LE.P) B2=1.05*1.05*P
      IF (UPAR.GT.P) B2=UPAR
      IPSI=1
      IF (PSIPAR.LT.0.) C=1.345
      IF (PSIPAR.GE.0.) C=PSIPAR
      ITYPW=1
      ITYPE=2
      ISIGMA=2
      RETURN
C
C KRASKER-WELSH
C
   25 IUCV=3
      IF (UPAR.LT.0.) UPAR=PSIPAR
      IF (UPAR.LE.SQRT(P)) CKW=1.05*SQRT(P)
      IF (UPAR.GT.SQRT(P)) CKW=UPAR
      IWWW=1
      IPSI=1
      C=CKW
      ITYPW=1
      ITYPE=3
      ISIGMA=2
      RETURN
C
C MALLOWS-HAMPEL
C
   30 IWWW=2
      GOTO 21
C
C HAMPEL-KRASKER
C
   35 IUCV=2
      IF (UPAR.LT.0.) UPAR=PSIPAR
      IF (UPAR.GE.0.) THEN
        CHK=UPAR
      ELSE
        SUMNRM=0.D0
        DO 36 I=1,N
        CALL NRM2Z(X(I,1),N,MDX,MDX*(NP-1)+1,XNRM)
        SUMNRM=SUMNRM+DBLE(XNRM)
   36   CONTINUE
        CHK=1.05*FLOAT(NP)*SQRT(1.5707963)
        CHK=CHK/(SNGL(SUMNRM)/FLOAT(N))
      ENDIF
      IWWW=1
      C=CHK
      IPSI=1
      ITYPW=2
      ITYPE=3
      ISIGMA=2
      RETURN
C
C MALLOWS : UNSTANDARDIZED CASE
C
   40 IUCV=4
      IF (UPAR.GE.0.) THEN
        BB=UPAR
      ELSE
        SUMNRM=0.D0
        DO 41 I=1,N
        CALL NRM2Z(X(I,1),N,MDX,MDX*(NP-1)+1,XNRM)
        SUMNRM=SUMNRM+DBLE(XNRM)
   41   CONTINUE
        BB=1.05*FLOAT(NP)
        BB=BB/(SNGL(SUMNRM)/FLOAT(N))
      ENDIF
      IWWW=2
      IPSI=1
      IF (PSIPAR.LT.0.) C=1.345
      IF (PSIPAR.GE.0.) C=PSIPAR
      ITYPW=2
      ITYPE=3
      ISIGMA=2
      RETURN
C
C MALLOWS ESTIMATOR IN THE TAU-TEST
C
   45 IUCV=4
      IF (UPAR.LE.0.) BB=9.999
      IF (UPAR.GT.0.) BB=UPAR
      IWWW=2
      IPSI=1
      IF (PSIPAR.LT.0.) C=1.345
      IF (PSIPAR.GE.0.) C=PSIPAR
      ITYPW=1
      ITYPE=2
      ISIGMA=2
      RETURN
C
C SCHWEPPE ESTIMATOR IN THE TAU-TEST
C
   50 IUCV=2
      IF (UPAR.LT.0.) UPAR=PSIPAR
      IF (UPAR.LE.0.) CHK=9.999
      IF (UPAR.GT.0.) CHK=UPAR
      IWWW=1
      IPSI=1
      C=CHK
      ITYPW=1
      ITYPE=3
      ISIGMA=2
      RETURN
C
C LMS-ESTIMATOR
C
   55 RETURN
C
C LTS-ESTIMATOR
C
   60 RETURN
C
C S-ESTIMATOR
C
   65 IPSI=4
      ITYPE=1
      IF (PSIPAR.LT.0.) XK=1.548
      IF (PSIPAR.GT.0.) XK=PSIPAR
      RETURN
C
C ROCKE ESTIMATOR 
C
   70 IUCV=5
      GOTO 76
   75 IUCV=6
   76 EM=PSIPAR
      IF (PSIPAR.LE.0.) EM=1.345
      CR=2
      IF (UPAR.GT.0.) CR=UPAR
      IWWW=2
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RPARDF(X,N,NP,MDX,RTYPE,UPAR,PSIPAR,
     1                  ITYPW,ITYPE,ISIGMA)
C.......................................................................
C
C   COPYRIGHT  1992  Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  SET THE PARAMETER VALUES OF THE COMMON/UCVPR/, /WWWPR/
C  AND /PSIPR/ ACCORDING TO THE ESTIMATOR TYPE
C
      INTEGER RTYPE
      REAL X(MDX,NP)
      DOUBLE PRECISION SUMNRM
      LOGICAL NPRCHK
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,BT,CW
      COMMON/UCV56/EM,CR,VK,NNP,ENU,V7
      COMMON/ESTIM/IEST
      COMMON/WWWPR/IWWW
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      IUCV=0
      IWWW=0
      IPSI=0
      A2=0.
      B2=0.
      CHK=0.
      CKW=0.
      BB=0.
      BT=0.
      CW=0.
      EM=0.
      CR=0.
      VK=0.
      NNP=NP
      C=0.
      H1=0.
      H2=0.
      H3=0.
      XK=0.
      D=0.
C
      NPRCHK=N.GT.0.AND.NP.GT.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'DFRPAR',1)
      P=FLOAT(NP)
      C=0.
      IEST=5
      IF (RTYPE.EQ.5) GOTO 10
      IEST=6
      IF (RTYPE.EQ.6) GOTO 10
      IEST=1
      IF (RTYPE.EQ.1) GOTO 15
      IEST=2
      IF (RTYPE.EQ.2) GOTO 20
      IEST=3
      IF (RTYPE.EQ.3) GOTO 25
      IEST=4
      IF (RTYPE.EQ.4) GOTO 30
      IEST=7
      IF (RTYPE.EQ.7) GOTO 35
      IEST=8
      IF (RTYPE.EQ.8) GOTO 40
      IEST=9
      IF (RTYPE.EQ.9) GOTO 45
      IEST=10
      IF (RTYPE.EQ.10) GOTO 50
      IEST=11
      IF (RTYPE.EQ.11) GOTO 55
      IEST=12
      IF (RTYPE.EQ.12) GOTO 60
      IEST=13
      IF (RTYPE.EQ.13) GOTO 65
      IEST=14
      IF (RTYPE.EQ.14) GOTO 70
      IEST=15
      IF (RTYPE.EQ.15) GOTO 75
      IEST=0
      CALL MESSGE(500,'DFRPAR',1)
C
C ORDINARY LEAST SQUARES
C
   10 IUCV=0
      IWWW=0
      RETURN
C
C LEAST ABSOLUTE RESIDUALS
C
C     GOTO 10
C
C HUBER CASE
C
   15 IPSI=1
      IF (PSIPAR.LT.0.) THEN
        C=1.345
        D=1.345
      ELSE
        C=PSIPAR
        D=C
      ENDIF
      ITYPE=1
      ISIGMA=1
      IUCV=0
      IWWW=0
      RETURN
C
C MALLOWS STANDARD
C
   20 IWWW=3
   21 IUCV=1
      A2=0.
      IF (UPAR.LE.P) B2=1.05*1.05*P
      IF (UPAR.GT.P) B2=UPAR
      IPSI=1
      IF (PSIPAR.LT.0.) C=1.345
      IF (PSIPAR.GE.0.) C=PSIPAR
      ITYPW=1
      ITYPE=2
      ISIGMA=2
      RETURN
C
C KRASKER-WELSH
C
   25 IUCV=3
      IF (UPAR.LT.0.) UPAR=PSIPAR
      IF (UPAR.LE.SQRT(P)) CKW=1.05*SQRT(P)
      IF (UPAR.GT.SQRT(P)) CKW=UPAR
      IWWW=1
      IPSI=1
      C=CKW
      ITYPW=1
      ITYPE=3
      ISIGMA=2
      RETURN
C
C MALLOWS-HAMPEL
C
   30 IWWW=2
      GOTO 21
C
C HAMPEL-KRASKER
C
   35 IUCV=2
      IF (UPAR.LT.0.) UPAR=PSIPAR
      IF (UPAR.GE.0.) THEN
        CHK=UPAR
      ELSE
        SUMNRM=0.D0
        DO 36 I=1,N
        CALL NRM2Z(X(I,1),N,MDX,MDX*(NP-1)+1,XNRM)
        SUMNRM=SUMNRM+DBLE(XNRM)
   36   CONTINUE
        CHK=1.05*FLOAT(NP)*SQRT(1.5707963)
        CHK=CHK/(SNGL(SUMNRM)/FLOAT(N))
      ENDIF
      IWWW=1
      C=CHK
      IPSI=1
      ITYPW=2
      ITYPE=3
      ISIGMA=2
      RETURN
C
C MALLOWS : UNSTANDARDIZED CASE
C
   40 IUCV=4
      IF (UPAR.GE.0.) THEN
        BB=UPAR
      ELSE
        SUMNRM=0.D0
        DO 41 I=1,N
        CALL NRM2Z(X(I,1),N,MDX,MDX*(NP-1)+1,XNRM)
        SUMNRM=SUMNRM+DBLE(XNRM)
   41   CONTINUE
        BB=1.05*FLOAT(NP)
        BB=BB/(SNGL(SUMNRM)/FLOAT(N))
      ENDIF
      IWWW=2
      IPSI=1
      IF (PSIPAR.LT.0.) C=1.345
      IF (PSIPAR.GE.0.) C=PSIPAR
      ITYPW=2
      ITYPE=3
      ISIGMA=2
      RETURN
C
C MALLOWS ESTIMATOR IN THE TAU-TEST
C
   45 IUCV=4
      IF (UPAR.LE.0.) BB=9.999
      IF (UPAR.GT.0.) BB=UPAR
      IWWW=2
      IPSI=1
      IF (PSIPAR.LT.0.) C=1.345
      IF (PSIPAR.GE.0.) C=PSIPAR
      ITYPW=1
      ITYPE=2
      ISIGMA=2
      RETURN
C
C SCHWEPPE ESTIMATOR IN THE TAU-TEST
C
   50 IUCV=2
      IF (UPAR.LT.0.) UPAR=PSIPAR
      IF (UPAR.LE.0.) CHK=9.999
      IF (UPAR.GT.0.) CHK=UPAR
      IWWW=1
      IPSI=1
      C=CHK
      ITYPW=1
      ITYPE=3
      ISIGMA=2
      RETURN
C
C LMS-ESTIMATOR
C
   55 RETURN
C
C LTS-ESTIMATOR
C
   60 RETURN
C
C S-ESTIMATOR
C
   65 IPSI=4
      ITYPE=1
      IF (PSIPAR.LT.0.) XK=1.548
      IF (PSIPAR.GT.0.) XK=PSIPAR
      RETURN
C
C ROCKE ESTIMATOR 
C
   70 IUCV=5
      GOTO 76
   75 IUCV=6
   76 EM=PSIPAR
      IF (PSIPAR.LE.0.) EM=1.345
      CR=2
      IF (UPAR.GT.0.) CR=UPAR
      IWWW=2
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE ZDFVALS(IO,DFV)
C.......................................................................

CC   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      REAL DFV(66),VALS(66),VALZ(66)
      DATA VALS(1:66)/1.e-3, 1., 1., 30., 0., 1.e-6, 1.e-11, 2., 1., 1., 
     +     1., 2., 1., 1., 1., 0.025, 1.345, 10.0, 1.e-4, 1., 1., 1., 
     +     0., 150., 50., 50., 2., 1.25, 1., 1., 1., 1., 0.0, 0.0 , 1.0,
     +     1., 1., 150., 0.0 , 0.0, 1., 150., 1.e-3, 1.e-3, 30., 2. ,
     +     1., 1313., 1., 0.1, 1., 0., 9., 1.345, 2., 1., -6.9078, 
     +     1.e-3, 1. , 1. , 1.345, 1., 50., 1. , 1., 1./
      DATA VALZ(1:66)/1.e-3, 1., 1., 30., 0., 1.e-6, 1.e-11, 2., 1., 1., 
     +     1., 2., 1., 1., 1., 0.025, 1.345, 10.0, 1.e-4, 1., 1., 1., 
     +     0., 150., 50., 50., 2., 1.25, 1., 1., 1., 1., 0.0, 0.0 , 1.0,
     +     1., 1., 150., 0.0 , 0.0, 1., 150., 1.e-3, 1.e-3, 30., 2. ,
     +     1., 1313., 1., 0.1, 1., 0., 9., 1.345, 2., 1., -6.9078, 
     +     1.e-3, 1. , 1. , 1.345, 1., 50., 1. , 1., 1./

      IF (IO.EQ.0) THEN
       DO 100 I=1,66
       DFV(I)=VALS(I)
  100  CONTINUE
      ELSEIF (IO.EQ.1) THEN
       DO 200 I=1,66
       VALS(I)=DFV(I)
  200  CONTINUE
      ELSE
       DO 300 I=1,66
       VALS(I)=VALZ(I)
  300  CONTINUE
      ENDIF    
      RETURN 
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DFCOMNZ(IPSI,C,H1,H2,H3,XK,D,BTA,BT0,IUCV,A2,B2,CHK,
     +                   CKW,BB,BT,CW,EM,CR,VK,NP,ENU,V7,IWWW)
C.......................................................................
C
C   COPYRIGHT  1992  Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      COMMON/UCVPR/JUCV,AA,AB,PHK,PKW,PBB,PBT,PW
      COMMON/UCV56/PM,PCR,PK,NNP,PNU,P7
      COMMON/PSIPR/JPSI,PC,PH1,PH2,PH3,PXK,PD
      COMMON/WWWPR/JWWW
      COMMON/BETA/BETA,BET0
      IF (IPSI.GE.-5) JPSI=IPSI
      IF (C .GE.0.)  PC=C
      IF (H1.GE.0.)  PH1=H1
      IF (IPSI.EQ.10) PH1=H1
      IF (H2.GE.0.)  PH2=H2
      IF (H3.GE.0.)  PH3=H3
      IF (XK.GE.0.)  PXK=XK
      IF (D .GE.0.)  PD=D
      IF (BTA.GE.0.) BETA=BTA
      IF (BT0.GE.0.) BET0=BT0
      IF (IUCV.GE.0) JUCV=IUCV
      IF (A2.GE.0.)  AA=A2
      IF (B2.GE.0.)  AB=B2
      IF (CHK.GE.0.) PHK=CHK
      IF (CKW.GE.0.) PKW=CKW
      IF (BB.GE.0.)  PBB=BB
      IF (BT.GE.0.)  PBT=BT
      IF (CW.GE.0.)  PW=CW
      IF (EM.GT.0.)  PM=EM
      IF (CR.GT.0.)  PCR=CR
      IF (VK.GT.0.)  PK=VK
      IF (NP.GT.0)   NNP=NP
      IF (ENU.GT.0.) PNU=ENU
      IF (V7.GT.0.)  P7=V7
      IF (IWWW.GE.0) JWWW=IWWW
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE COMVALZ(IPSI,C,H1,H2,H3,XK,D,BTA,BT0,IUCV,A2,B2,CHK,
     +                  CKW,BB,BT,CW,EM,CR,VK,NP,ENU,V7,IWWW)
C.......................................................................
C
C   COPYRIGHT  1992  Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      COMMON/UCVPR/JUCV,AA,AB,PHK,PKW,PBB,PBT,PW
      COMMON/UCV56/PM,PCR,PK,NNP,PNU,P7
      COMMON/PSIPR/JPSI,PC,PH1,PH2,PH3,PXK,PD
      COMMON/WWWPR/JWWW
      COMMON/BETA/BETA,BET0
      IPSI=JPSI
      C=PC
      H1=PH1
      H2=PH2
      H3=PH3
      XK=PXK
      D=PD
      BTA=BETA
      BT0=BET0
      IUCV=JUCV
      A2=AA
      B2=AB
      CHK=PHK
      CKW=PKW
      BB=PBB
      BT=PBT
      CW=PW
      EM=PM
      CR=PCR
      VK=PK
      NP=NNP
      ENU=PNU
      V7=P7
      IWWW=JWWW
      RETURN
      END
C-----------------------------------------------------------------------
C
C                 R O B E T H  FORTRAN Source
C
C  File GLAUXI.F  Auxiliary subroutines of Chapter 10
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TS12BI(YI,AI,ENI,OI,XMIN,YMIN,T1,S1,T2,S2)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
      DOUBLE PRECISION DNI,XMIN,YMIN,T1,S1,T2,S2,OI,DXLOG  
      EXTERNAL DXLOG
C
C  AUXILIARY SUBROUTINE FOR DBINOM (BINOMIAL CASE).
C
      DNI=DBLE(ENI) 
      T1=DBLE(YI-AI)/DBLE(ENI-YI+AI)
      T1=DXLOG(T1,XMIN,YMIN)-OI
      S1=DNI/DBLE(ENI-YI+AI)
      S1=DXLOG(S1,XMIN,YMIN)
      S1=-DBLE(YI-AI)*T1+ENI*S1
      T2=DBLE(YI+AI)/DBLE(ENI-YI-AI)
      T2=DXLOG(T2,XMIN,YMIN)-OI
      S2=DNI/DBLE(ENI-YI-AI)
      S2=DXLOG(S2,XMIN,YMIN)
      S2=-DBLE(YI+AI)*T2+DNI*S2
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE BIGGBI(X,DNI,ZMIN,ZMAX,Y)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
      DOUBLE PRECISION DNI,ZMIN,ZMAX,X,Y  
C
C  AUXILIARY SUBROUTINE FOR DBINOM (BINOMIAL CASE).
C
      IF (X.LE.ZMIN) THEN
        Y=0.D0 
      ELSEIF (X.GE.ZMAX) THEN
        Y=DNI*X
      ELSE
        Y=DNI*DLOG(1.D0+DEXP(X))
      ENDIF
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TS12PO(YI,AI,OI,XMIN,YMIN,T1,S1,T2,S2)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
      DOUBLE PRECISION XMIN,YMIN,T1,S1,T2,S2,OI,DXLOG  
      EXTERNAL DXLOG
C
C  AUXILIARY SUBROUTINE FOR DPOISS (POISSON CASE).
C
      T1=DXLOG(DBLE(YI-AI),XMIN,YMIN)-OI
      S1=-DBLE(YI-AI)*T1+DBLE(YI-AI)
      T2=DXLOG(DBLE(YI+AI),XMIN,YMIN)-OI
      S2=-DBLE(YI+AI)*T2+DBLE(YI+AI)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LRFNCTZ(ICASE,Y,C,VTHETA,OI,WA,NN,N,I0,I1,I2,F0,F1,F2,
     +                  SF0)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  FUNCTIONS L, L' and L" FOR THE THETA STEP
C
      DIMENSION Y(N),C(N),VTHETA(N),OI(N),WA(N),NN(N),F0(N),F1(N),F2(N)
      DOUBLE PRECISION GFUN,DXLOG,EXPGI,F0I,XBIG   
      DOUBLE PRECISION GI,OF,DNI,S1,S2,T1,T2,SUM,ZMIN,XMIN,YMIN,ZMAX 
      EXTERNAL GFUN,DXLOG
      DATA ZMIN,XMIN,YMIN,ZMAX,NCALL/0.D0,0.D0,0.D0,0.D0,0/
C
      IF (NCALL.EQ.1) GOTO 10
      CALL MACHZD(3,ZMIN)
      CALL MACHZD(4,XMIN)
      CALL MACHZD(5,YMIN)
      CALL MACHZD(6,XBIG)
      XBIG=XBIG/10.D0 
      ZMAX=DLOG(XBIG)
      NCALL=1
   10 SUM=0.D0
      DO 500 I=1,N
        GI=DBLE(VTHETA(I))
        OF=DBLE(OI(I))
        GO=SNGL(GI+OF) 
        YI=Y(I)-C(I)
        AI=WA(I)
        NI=1
        IF (ICASE.EQ.2) NI=NN(I)
        ENI=FLOAT(NI)
        DNI=DBLE(ENI) 
        IF (-YI.GE.AI) THEN
          S2=0.D0 
          GOTO 200
        ENDIF
        IF (ICASE.EQ.3) GOTO 30
C==>    BERNOUILLI OR BINOMIAL CASE
        IF (-YI.LT.-AI) THEN
          IF (-YI+ENI.LE.-AI) THEN
            S1=0.D0 
            GOTO 100
          ENDIF
          T1=DBLE(YI-AI)/DBLE(ENI-YI+AI)
          T1=DXLOG(T1,XMIN,YMIN)-OF
          IF (GI.LT.T1) THEN
            S1=DNI/DBLE(ENI-YI+AI)
            S1=DXLOG(S1,XMIN,YMIN)
            S1=-DBLE(YI-AI)*T1+DNI*S1
            GOTO 100
          ENDIF
          GOTO 50
        ELSE
C**     IF (-YI.LT.AI) THEN
          GOTO 50
        ENDIF
   30   IF (-YI.LT.-AI) THEN
C==>    POISSON CASE
          T1=DBLE(YI-AI) 
          T2=DBLE(YI+AI) 
          T1=DXLOG(T1,XMIN,YMIN)-OF
          T2=DXLOG(T2,XMIN,YMIN)-OF
          IF (GI.LT.T1) THEN
            S1=-DBLE(YI-AI)*T1+DBLE(YI-AI) 
            GOTO 100
          ELSEIF (GI.GT.T2) THEN
            S2=-DBLE(YI+AI)*T2+DBLE(YI+AI) 
            GOTO 200
          ELSE
            GOTO 400
          ENDIF
        ELSE
C**     IF (-YI.LT.AI) THEN
          T2=DBLE(YI+AI) 
          T2=DXLOG(T2,XMIN,YMIN)-OF
          IF (GI.LE.T2) GOTO 400
          S2=-DBLE(YI+AI)*T2+DBLE(YI+AI) 
          GOTO 200
        ENDIF
   50   IF (-YI+ENI.LE.AI) GOTO 300
        T2=DBLE(YI+AI)/DBLE(ENI-YI-AI)
        T2=DXLOG(T2,XMIN,YMIN)-OF
        IF (GI.LE.T2) GOTO 300
        S2=DNI/DBLE(ENI-YI-AI)
        S2=DXLOG(S2,XMIN,YMIN)
        S2=-DBLE(YI+AI)*T2+DNI*S2
        GOTO 200
  100   F0I=-DBLE(AI)*GI+S1  
        IF (I0.NE.0) F0(I)=SNGL(F0I)           
        IF (I1.NE.0) F1(I)=-AI
        IF (I2.NE.0) F2(I)=0.
        GOTO 450
  200   F0I=DBLE(AI)*GI+S2
        IF (I0.NE.0) F0(I)=SNGL(F0I) 
        IF (I1.NE.0) F1(I)=AI
        IF (I2.NE.0) F2(I)=0.
        GOTO 450
  300   IF (GI+OF.LE.ZMIN) THEN
          S1=0.D0 
        ELSEIF (GI+OF.GE.ZMAX) THEN
          S1=DNI*(GI+OF)
        ELSE
          S1=DNI*DLOG(1.D0+DEXP(GI+OF))
        ENDIF
        S2=GFUN(ICASE,1,GO)
        F0I=-DBLE(YI)*GI+S1 
        IF (I0.NE.0) F0(I)=SNGL(F0I)
        IF (I1.NE.0) F1(I)=-YI+SNGL(DNI*S2)
        IF (I2.NE.0) F2(I)=SNGL(DNI*S2*(1.D0-S2))
        GOTO 450
  400   EXPGI=GFUN(ICASE,1,GO)
        F0I=-DBLE(YI)*GI+EXPGI 
        IF (I0.NE.0) F0(I)=SNGL(F0I)
        IF (I1.NE.0) F1(I)=-YI+SNGL(EXPGI)
        IF (I2.NE.0) F2(I)=SNGL(EXPGI)
  450   SUM=SUM+F0I  
  500 CONTINUE
      SF0=SNGL(SUM) 
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LRFCTDZ(ICASE,Y,C,VTHETA,OI,WA,NN,N,I0,I1,I2,F0,F1,F2,
     +                  SF0)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  FUNCTIONS L, L' and L" FOR THE THETA STEP
C
      DIMENSION Y(N),C(N),VTHETA(N),OI(N),WA(N),NN(N)
      DOUBLE PRECISION GFUN,DXLOG,EXPGI,XBIG,YY,F0(N),F1(N),F2(N),   
     +       F0I,GI,OF,DNI,S1,S2,T1,T2,SUM,SF0,ZMIN,XMIN,YMIN,ZMAX 
      EXTERNAL GFUN,DXLOG
      DATA ZMIN,XMIN,YMIN,ZMAX,NCALL/0.D0,0.D0,0.D0,0.D0,0/
C
      IF (NCALL.EQ.1) GOTO 10
      CALL MACHZD(3,ZMIN)
      CALL MACHZD(4,XMIN)
      CALL MACHZD(5,YMIN)
      CALL MACHZD(6,XBIG)
      XBIG=XBIG/10.D0 
      ZMAX=DLOG(XBIG)
      NCALL=1
   10 SUM=0.D0
      DO 500 I=1,N
        GI=DBLE(VTHETA(I))
        OF=DBLE(OI(I))
        GO=SNGL(GI+OF) 
        YI=Y(I)-C(I)
        YY=DBLE(YI)
        AI=WA(I)
        NI=1
        IF (ICASE.EQ.2) NI=NN(I)
        ENI=FLOAT(NI)
        DNI=DBLE(ENI) 
        IF (-YI.GE.AI) THEN
          S2=0.D0 
          GOTO 200
        ENDIF
        IF (ICASE.EQ.3) GOTO 30
C==>    BERNOUILLI OR BINOMIAL CASE
        IF (-YI.LT.-AI) THEN
          IF (-YI+ENI.LE.-AI) THEN
            S1=0.D0 
            GOTO 100
          ENDIF
          T1=DBLE(YI-AI)/DBLE(ENI-YI+AI)
          T1=DXLOG(T1,XMIN,YMIN)-OF
          IF (GI.LT.T1) THEN
            S1=DNI/DBLE(ENI-YI+AI)
            S1=DXLOG(S1,XMIN,YMIN)
            S1=-DBLE(YI-AI)*T1+DNI*S1
            GOTO 100
          ENDIF
          GOTO 50
        ELSE
C**     IF (-YI.LT.AI) THEN
          GOTO 50
        ENDIF
   30   IF (-YI.LT.-AI) THEN
C==>    POISSON CASE
          T1=DBLE(YI-AI) 
          T2=DBLE(YI+AI) 
          T1=DXLOG(T1,XMIN,YMIN)-OF
          T2=DXLOG(T2,XMIN,YMIN)-OF
          IF (GI.LT.T1) THEN
            S1=-DBLE(YI-AI)*T1+DBLE(YI-AI) 
            GOTO 100
          ELSEIF (GI.GT.T2) THEN
            S2=-DBLE(YI+AI)*T2+DBLE(YI+AI) 
            GOTO 200
          ELSE
            GOTO 400
          ENDIF
        ELSE
C**     IF (-YI.LT.AI) THEN
          T2=DBLE(YI+AI) 
          T2=DXLOG(T2,XMIN,YMIN)-OF
          IF (GI.LE.T2) GOTO 400
          S2=-DBLE(YI+AI)*T2+DBLE(YI+AI) 
          GOTO 200
        ENDIF
   50   IF (-YI+ENI.LE.AI) GOTO 300
        T2=DBLE(YI+AI)/DBLE(ENI-YI-AI)
        T2=DXLOG(T2,XMIN,YMIN)-OF
        IF (GI.LE.T2) GOTO 300
        S2=DNI/DBLE(ENI-YI-AI)
        S2=DXLOG(S2,XMIN,YMIN)
        S2=-DBLE(YI+AI)*T2+DNI*S2
        GOTO 200
  100   F0I=-DBLE(AI)*GI+S1  
        IF (I0.NE.0) F0(I)=F0I           
        IF (I1.NE.0) F1(I)=-DBLE(AI)
        IF (I2.NE.0) F2(I)=0.D0
        GOTO 450
  200   F0I=DBLE(AI)*GI+S2
        IF (I0.NE.0) F0(I)=F0I
        IF (I1.NE.0) F1(I)=DBLE(AI)
        IF (I2.NE.0) F2(I)=0.D0
        GOTO 450
  300   IF (GI+OF.LE.ZMIN) THEN
          S1=0.D0 
        ELSEIF (GI+OF.GE.ZMAX) THEN
          S1=DNI*(GI+OF)
        ELSE
          S1=DNI*DLOG(1.D0+DEXP(GI+OF))
        ENDIF
        S2=GFUN(ICASE,1,GO)
        F0I=-YY*GI+S1 
        IF (I0.NE.0) F0(I)=F0I
        IF (I1.NE.0) F1(I)=-YY+(DNI*S2)
        IF (I2.NE.0) F2(I)=DNI*S2*(1.D0-S2)
        GOTO 450
  400   EXPGI=GFUN(ICASE,1,GO)
        F0I=-YY*GI+EXPGI 
        IF (I0.NE.0) F0(I)=F0I
        IF (I1.NE.0) F1(I)=-YY+EXPGI
        IF (I2.NE.0) F2(I)=EXPGI
  450   SUM=SUM+F0I  
  500 CONTINUE
      SF0=SUM
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE UCOWJ(X,Y,NI,VTHETA,OI,CI,EXUL,SA,ST,N,NP,NCOV,MDX,
     1                  ICNT,NIT,ZMAX,DIST,SU,SD)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  COMPUTE WEIGHTED COVARIANCE MATRIX FOR LOGISTIC REGRESSION;
C  (STORE EXUL VALUES IN SU)
C
      DIMENSION X(MDX,NP),Y(N),CI(N),VTHETA(N),OI(N),DIST(N),UARR(4)
      DOUBLE PRECISION SA(NCOV),ST(NCOV),SU(N),SD(NP),XN,ZNR,U,EXUL
      INTEGER NI(N)
      EXTERNAL EXUL
      COMMON/UGLPR/IUGL,ICASE,B
      DATA XN/0.D0/
C
      IF (NIT.GT.1) GOTO 10
      XN=DBLE(N)
   10 ZMAX=0.0
      DO 20 IJ=1,NCOV
      ST(IJ)=0.D0
   20 CONTINUE
      NL=1
      DO 100 L=1,N
      DO  50 J=1,NP
      SD(J)=DBLE(X(L,J))
   50 CONTINUE
      CALL MLYZD(SA,SD,NP,NCOV,NP,1)
      CALL NRM2ZD(SD,NP,1,NP,ZNR)
      DISTL=SNGL(ZNR)
      IF (ICNT.EQ.2) ZMAX=AMAX1(ZMAX,ABS(DISTL-DIST(L)))
      DIST(L)=DISTL
      GL=VTHETA(L)+OI(L)
      IF (ICASE.EQ.2) NL=NI(L)
      UARR(1)=Y(L)
      UARR(2)=FLOAT(NL)
      UARR(3)=GL
      UARR(4)=CI(L)
      U=EXUL(UARR,4,DISTL)
      SU(L)=U
      IJ=0
      DO 90 I=1,NP
      DO 80 J=1,I
      IJ=IJ+1
      ST(IJ)=ST(IJ)+(SD(I)*U)*SD(J)
   80 CONTINUE
   90 CONTINUE
  100 CONTINUE
      DO 110 IJ=1,NCOV
      ST(IJ)=ST(IJ)/XN
  110 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION GFUN(ICASE,NI,G)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  G-FUNCTION FOR LOGISTIC REGRESSION
C
      DOUBLE PRECISION GI,EXGI,ZMIN,ZMAX,XBIG
      DATA ZMIN,ZMAX,XBIG,NCALL/0.D0,0.D0,0.D0,0/
C
      IF (NCALL.EQ.1) GOTO 10
c     CALL MACHZD(3,ZMIN)
c     CALL MACHZD(6,XBIG)
c     XBIG=XBIG/10.D0
      ZMIN=-35.D0
      XBIG=1.D6
      ZMAX=DLOG(XBIG)
      NCALL=1
   10 GI=DBLE(G)  
      IF (ICASE.LE.2) THEN
C==>    LOGISTIC BERNOUILLI OR BINOMIAL
        IF (GI.LE.ZMIN) THEN
          GFUN=0.D0
        ELSEIF (GI.GE.ZMAX) THEN
          GFUN=DBLE(NI)
        ELSE
          EXGI=DEXP(GI)
          GFUN=DBLE(NI)*EXGI/(1.D0+EXGI)
        ENDIF
      ELSE
C==>    LOGISTIC POISSON (ICASE=3)
        IF (GI.LE.ZMIN) THEN
          GFUN=DEXP(ZMIN)
        ELSEIF (GI.GE.ZMAX) THEN
          GFUN=XBIG
        ELSE
          GFUN=DEXP(GI)
        ENDIF
      ENDIF
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE STEPLR(ICASE,X,Y,C,OI,THETA,DELTA,WA,NI,GRAD,GRAD1,
     1                   N,NP,MDX,SF0,SF1,GAM,ST,F0,F1,F2,VTHETA)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  STEP LENGTH ALGORITHM BY CUBIC INTERPOLATION.
C
      DIMENSION X(MDX,NP),Y(N),C(N),THETA(NP),DELTA(NP),WA(N),NI(N),
     1   GRAD(NP),GRAD1(NP),ST(NP),F0(N),F1(N),F2(N),VTHETA(N),OI(N)

      CALL DOTPZ(DELTA,GRAD,NP,1,1,NP,NP,S0)
C     S0=-S0
      ETA=AMIN1(1.0,-2.*SF0/S0)
      DO 10 J=1,NP
      ST(J)=THETA(J)+ETA*DELTA(J)
   10 CONTINUE
      CALL MFYZ(X,ST,VTHETA,N,NP,MDX,NP,1,N,1)
      CALL LRFNCTZ(ICASE,Y,C,VTHETA,OI,WA,NI,N,1,1,0,F0,F1,F2,SF1)
      CALL GRADNT(X,F1,N,NP,MDX,GRAD1)
      CALL DOTPZ(DELTA,GRAD1,NP,1,1,NP,NP,S1)
C     S1=-S1
      Z=(3./ETA)*(SF0-SF1)+S0+S1
      A=Z*Z-S0*S1
      W=0.
      IF (A.GT.0.) W=SQRT(A)
      GAM=ETA*(1.-(S1+W-Z)/(S1-S0+2.*W))
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE STPLRGZ(ICASE,X,Y,C,OI,ZETA,IQ,THETA,DELTA,WA,NI,GRAD,
     1                  N,NP,MDX,SF0,SF1,GAM,ST,F0,VTHETA)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  ARMIJO-GOLDSTEIN STEP LENGTH ALGORITHM
C
      DIMENSION X(MDX,NP),Y(N),C(N),THETA(NP),DELTA(NP),WA(N),NI(N),
     1          GRAD(NP),ST(NP),F0(N),VTHETA(N),OI(N)
C
      CALL DOTPZ(DELTA,GRAD,NP,1,1,NP,NP,S0)
      IF (ABS(S0).GT.1.E-5) GOTO 10
      ETA=1.
      DO 450 NLOOP=1,IQ
      ETA=ETA*0.5
      DO 400 J=1,NP
      ST(J)=THETA(J)+ETA*DELTA(J)
  400 CONTINUE
      CALL MFYZ(X,ST,VTHETA,N,NP,MDX,NP,1,N,1)
      CALL LRFNCTZ(ICASE,Y,C,VTHETA,OI,WA,NI,N,1,0,0,F0,F0,F0,SF1)
      IF (SF1.LT.SF0) GOTO 500
  450 CONTINUE
      GOTO 300
   10 IP=0
      ETA=2.
  100 IF (IP.EQ.IQ) GOTO 300
      ETA=0.5**IP
      DO 200 J=1,NP
      ST(J)=THETA(J)+ETA*DELTA(J)
  200 CONTINUE
      CALL MFYZ(X,ST,VTHETA,N,NP,MDX,NP,1,N,1)
      CALL LRFNCTZ(ICASE,Y,C,VTHETA,OI,WA,NI,N,1,0,0,F0,F0,F0,SF1)
      IF (SF1.LT.SF0) GOTO 500
      IF ((SF1-SF0)/ETA/S0.GT.ZETA) GOTO 500
      IP=IP+1
      GOTO 100
  300 CALL MESSGE(450,'STPLRG',0)
  500 GAM=ETA
      DO 600 J=1,NP
      DELTA(J)=DELTA(J)*ETA
      THETA(J)=ST(J)
  600 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GICSTPZ(ICASE,IALG,NN,VTHETA,WA,OI,N,TOL,MAXIT,CI)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  H-,W-,S-ALGORITHM FOR ROBUST LOGISTIC REGRESSION : C-STEP solution
C
      DIMENSION VTHETA(N),OI(N),WA(N),CI(N)
      DOUBLE PRECISION PP,GFUN 
      INTEGER NN(N)
      LOGICAL NPRCHK
      EXTERNAL GFUN
      COMMON/UGLPR/IUGL,ICS,B
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.TOL.GT.0..AND.MAXIT.GT.0.AND.(ICASE.EQ.1
     1       .OR.ICASE.EQ.2.OR.ICASE.EQ.3).AND.(IALG.EQ.-2.OR.
     2       IALG.EQ.-1.OR.IALG.EQ.1.OR.IALG.EQ.2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'GICSTP',1)
C
      ICS=ICASE
      DO 500 I=1,N
      GI=VTHETA(I)+OI(I)
      A=WA(I)
      NI=1
      IF (ICASE.EQ.2) NI=NN(I) 
      PP=GFUN(ICASE,NI,GI)
      E=SNGL(PP) 
      T=CI(I)+E
      CALL GYCSTPZ(ICASE,IALG,NI,A,E,TOL,MAXIT,T)  
      CI(I)=T-E
  500 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PROBINZ(K,N,P,ILG,PK)
      DOUBLE PRECISION P,PK,PL,LPL,EMIN,SML,ALSML,P1,Q1,ALQ,ALP,
     +       XN,XX,XK,ICNT,PAR
      INTEGER K,N,ILG,KL
      LOGICAL NPRCHK
      DATA ALP,ALQ,LPL,EMIN,SML,ALSML/0.D0,0.D0,0.D0,0.D0,0.D0,0.D0/
      DATA NCALL,KL/0,0/
C
      PK=0.D0 
      NPRCHK=(K .LE. N .AND. K .GE. 0).AND.
     +       (P .LE. 1. .AND. P .GE. 0.). AND.
     +       (ILG .EQ. 0. .OR. ILG .EQ. 1)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'PROBIN',1)
      IF (NCALL.EQ.0) THEN
        CALL MACHZD(3,EMIN)
        CALL MACHZD(4,SML)
        CALL MACHZD(5,ALSML)
        NCALL=1
        ALP = ALSML
        IF (P.GT.SML) ALP = DLOG(P)
        ALQ = ALSML
        P1=1.D0-P
        IF (P1.GT.SML) ALQ = DLOG(P1)
        KL=K
      ENDIF
c     XLMN=SML
      IF (ALP.GE.0.D0) CALL MESSGE(499,'PROBIN',0)
      PAR = ALSML
      IF (P.GT.SML) PAR = DLOG(P)
      IF (DABS(PAR-ALP).GT.1.D-5) THEN
        KL=K
        ALP=PAR
        ALQ = ALSML
        P1=1.D0-P
        IF (P1.GT.SML) ALQ = DLOG(P1)
      ENDIF
      IF (P .NE. 0.D0) GO TO 115
      PL = 1.D0
      IF (K .NE. 0) PL=0.D0 
      GOTO 900
  115 IF (P .NE. 1.D0) GO TO 120
      PL = 1.D0
      IF (K .NE. N) PL=0.D0  
      GO TO 900
  120 IF (K.EQ.0) THEN
c       Q1=1.D0-P
c       ALQ=ALSML
c       IF (Q1.GT.SML) ALQ=DLOG(Q1)
c       ALP=ALSML
c       P1=P
c       IF (P1.GT.SML) ALP=DLOG(P1)
        PL=0.D0
        LPL=DBLE(N)*ALQ
        IF (LPL.GT.EMIN) PL=DEXP(LPL)
      ELSEIF (KL+1.NE.K.OR.LPL.LE.ALSML) THEN 
c       CALL BINPRDZ(K,N,P,S1,PK)
        P1 = P
        Q1 = 1.D0-P
        K1 = K
        XN = DBLE(N)
        XX = XN*P
        LPL = 0.D0
        IF (DBLE(K) .LE. XX) GO TO 25
        P1 = Q1
        Q1 = P
        K1 = N-K
   25   ALQ = ALSML
        IF (Q1.GT.SML) ALQ=DLOG(Q1)
        XK=DBLE(K1)
        ICNT = 1.D0
        ALP = ALSML
        IF (P1.GT.SML) ALP=DLOG(P1)
        IF (K1 .EQ. 0 .OR. K1 .EQ. N) GO TO 35
        DO 30 J = 1,K1
         ICNT=ICNT*DBLE(N-J+1)/DBLE(J)
   30   CONTINUE
   35   LPL = DLOG(ICNT) + XK*ALP + (XN-XK)*ALQ 
        PL=0.D0
        IF (LPL.GT.EMIN) PL=DEXP(LPL)
        GOTO 950 
      ELSE
        LPL=LPL+DLOG(DBLE(N-K+1))+ALP-DLOG(DBLE(K))-ALQ
        PL=0.D0
        IF (LPL.GT.EMIN) PL=DEXP(LPL)
      ENDIF
      GOTO 950
  900 LPL=ALSML
      IF (PL.GT.SML) LPL=DLOG(PL)   
  950 PK=PL
      IF (ILG.EQ.1) PK=LPL
      KL=K
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PRPOISZ(E,K,ILG,PK)
      DOUBLE PRECISION E,PK,LPL,LGE,ESML,XLMN,YLMN,PAR 
      INTEGER K,ILG
      LOGICAL NPRCHK
      DATA LPL,LGE,ESML,XLMN,YLMN/0.D0,0.D0,0.D0,0.D0,0.D0/
      DATA NCALL,KL/0,0/
C
      PK=0.D0
      NPRCHK=(E.GT.0.D0 .AND. E.LE.1.D6 .AND. K .GE. 0 .AND. 
     +       (ILG.EQ.0 .OR. ILG.EQ.1)) 
      IF (.NOT.NPRCHK) CALL MESSGE(500,'PRPOIS',1)
      IF (NCALL.EQ.0) THEN
        CALL MACHZD(3,ESML)
        CALL MACHZD(4,XLMN)
        CALL MACHZD(5,YLMN)
        NCALL=1
        KL=K
        LGE = YLMN
        IF (E.GT.XLMN) LGE = DLOG(E)
      ENDIF
      PAR = YLMN
      IF (E.GT.XLMN) PAR = DLOG(E)
      IF (DABS(PAR-LGE).GT.1.D-5) THEN
        KL=K
        LGE=PAR
      ENDIF
      IF (K.GT.1100000) THEN
C       For E.LE.1E6 and K.GT.1100000 the probability
C       PK is smaller than 1E-2000.
        LPL = YLMN 
        PK=0.D0
        GOTO 950
      ELSEIF (E.LT.DSQRT(XLMN)) THEN
C       This case is treated here in order to reduce underflow
C       problems
        LPL = YLMN
        PK = 0.D0
        IF (K.EQ.0) PK = 1.D0
        IF (K.EQ.1) PK = E
        IF (K.GT.1) THEN
          LPL=DBLE(K)*LGE
          GOTO 700
        ENDIF
        IF (PK.GT.0.D0.AND.PK.GT.XLMN) LPL = DLOG(PK) 
        GOTO 950 
      ENDIF
      IF (K.EQ.0) THEN
        LPL = -E
      ELSEIF (KL+1.NE.K) THEN
C       CALL POISSNZ(E,K,S1,PK) 
        LGE = YLMN
        IF (E.GT.XLMN) LGE = DLOG(E)
        LPL=DBLE(K)*LGE - E
        GOTO 700
      ELSE
c       IF (K.LE.4) CALL DBLEPR('LPL',3,LPL,1)
c       IF (K.LE.4) CALL DBLEPR('LGE',3,LGE,1)
        LPL=LPL+LGE-DLOG(DBLE(K))
      ENDIF
      GOTO 800
  700 DO 750 I=1,K
        LPL=LPL-DLOG(DBLE(I))
  750 CONTINUE    
  800 PK=0.
      IF (LPL.GT.ESML) PK = DEXP(LPL)
c     GOTO 950
c 900 LPL=YLMN 
c     IF (PK.GT.XLMN) LPL=DLOG(PK) 
  950 KL=K
      IF (ILG.EQ.1) PK=LPL
      RETURN
      END
C-----------------------------------------------------------------------
C
C                 R O B E T H  FORTRAN Source
C
C  File GLMDEV.F  Subroutines for the computation of deviance in GLM 
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GLMDEVZ(Y,NI,CI,WA,VTHETA,OI,N,ICASE,DEV,THETAS,LI,SC)
C.......................................................................
C
C   COPYRIGHT 1996 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI 
C.......................................................................
C
C  GLM DEVIANCE COMPUTATION 
C   
      REAL  Y(N),CI(N),WA(N),VTHETA(N),OI(N)
      DOUBLE PRECISION DEV,THETAS(N),ENI,YI,ENYI,LI(N),SC(N),Q,QS,
     +       TMP,FLINK
      INTEGER NI(N)
      LOGICAL NPRCHK 
      EXTERNAL FLINK
      NPRCHK=(ICASE.EQ.1.OR.ICASE.EQ.2.OR.ICASE.EQ.3)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'GLMDEV',1)
      CALL LRFCTDZ(ICASE,Y,CI,VTHETA,OI,WA,NI,N,1,0,0,LI,LI,LI,Q)
      DO 700 I=1,N
      TMP=DBLE(Y(I)-CI(I))/DBLE(NI(I))
      THETAS(I)=FLINK(ICASE,TMP)-DBLE(OI(I))
  700 CONTINUE 
c     CALL LRFNCTZ(ICASE,Y,CI,THETAS,OI,WA,NI,N,1,0,0,SC,WA,WA,QS)
      QS=0.D0
      DO 800 I=1,N
      ENI=DBLE(NI(I))
      YI=DBLE(Y(I))
      IF (ICASE.LE.2) THEN
        TMP=ENI*DLOG(ENI)
        IF (YI.GT.0.D0) TMP=TMP-YI*DLOG(YI)
        ENYI=ENI-YI
        IF (ENYI.GT.0.D0) TMP=TMP-ENYI*DLOG(ENYI)
      ELSE
        TMP=YI
        IF (YI.GT.0.D0) TMP=TMP-YI*DLOG(YI)
      ENDIF  
      QS=QS+TMP
      SC(I)=TMP
  800 CONTINUE 
      DEV=2.D0*DABS(Q-QS)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION FLINK(ICASE,EM)
C.......................................................................
C
C   COPYRIGHT 1996 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  LINK FUNCTION FOR LOGISTIC REGRESSION
      DOUBLE PRECISION EM,EM1,XMIN,YMIN,TT,TMP
C
      DATA XMIN,YMIN,NCALL/0,0.D0,0.D0/
C
      IF (NCALL.EQ.1) GOTO 10
      CALL MACHZD(4,XMIN)
      CALL MACHZD(5,YMIN)
      NCALL=1
      IF (ICASE.NE.3) GOTO 10
      FLINK=DLOG(0.5D0)
      IF (EM.EQ.0.D0) RETURN
   10 FLINK=-9999.D0
      IF (EM.LE.0.D0) RETURN 
      TT=YMIN
      IF (EM.GT.XMIN) TT=DLOG(EM)
      TMP=0.D0
      IF (ICASE.EQ.3) GOTO 20
      EM1=1.D0-EM 
      IF (EM1.LE.0.D0) RETURN 
      TMP=YMIN
      IF (EM1.GT.XMIN) TMP=DLOG(EM1)
   20 FLINK=TT-TMP 
      RETURN
      END
C-----------------------------------------------------------------------
C
C                 R O B E T H  FORTRAN Source
C
C  File GLMAIN.F  Main subroutines of Chapter 10
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GYMAINZ(X,Y,NI,COV,A,THETA,OI,MDX,N,NP,NCOV,B,GAM,TAU,
     +       ICASE,IUGL,IOPT,IALG,ICNVT,ICNVA,MAXIT,MAXTT,MAXTA,MAXTC,
     +       NITMNT,NITMNA,TOL,TOLT,TOLA,TOLC,NIT,CI,WA,VTHETA,
     +       DELTA,GRAD,HESSNV,RW1,RW2,IW1,DW1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI 
C.......................................................................
C
C  MAIN ALGORITHM FOR THE LOGISTIC REGRESSION
C   
      DIMENSION X(MDX,NP),Y(N),COV(NCOV),THETA(NP),CI(N),VTHETA(N),
     1          WA(N),DELTA(NP),GRAD(NP),HESSNV(NCOV),RW2(MDX,NP),
     2          RW1(5*NCOV+3*N),OI(N)
      DOUBLE PRECISION A(NCOV),DW1(2*NCOV+NP+N)
      INTEGER IW1(NP),NI(N)  
      LOGICAL NPRCHK
      DATA ZMIN/0.001/
      NN=NP*(NP+1)/2
      NPRCHK=NP.GT.0.AND.NP.LE.N.AND.MDX.GE.N.AND.NN.EQ.NCOV.AND.
     1       (ICASE.EQ.1.OR.ICASE.EQ.2.OR.ICASE.EQ.3).AND.MAXIT.GT.0
     2       .AND.MAXTT.GT.0.AND.MAXTA.GT.0.AND.MAXTC.GT.0.AND.
     3       B.GT.SQRT(FLOAT(NP)).AND.GAM.GT.0..AND.TAU.GE.0..AND.
     4       TOL.GT.0..AND.TOLT.GT.0..AND.TOLA.GT.0..AND.TOLC.GT.0.
     5       .AND.(IOPT.EQ.1.OR.IOPT.EQ.2).AND.(IUGL.EQ.1.OR.
     6       IUGL.EQ.2).AND.(ICNVT.EQ.1.OR.ICNVT.EQ.2.OR.ICNVT.EQ.3)
     7       .AND.(ICNVA.EQ.1.OR.ICNVA.EQ.2).AND.(IALG.EQ.-1.OR.
     8       IALG.EQ.-2.OR.IALG.EQ.1.OR.IALG.EQ.2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'GYMAIN',1)
      IF1=N+1
      IF2=IF1+N
      ISC=IF2+N
      ISE=ISC+NCOV
      ISF=ISE+NCOV
      ISG=ISF+NCOV
      ISH=ISG+NCOV
      IST=NCOV+1
      ISD=IST+NCOV
      ISU=ISD+NP
      CALL GMAIN2(X,Y,NI,COV,A,THETA,OI,MDX,N,NP,NCOV,B,GAM,TAU,ICASE,
     +       IUGL,IOPT,IALG,ICNVT,ICNVA,MAXIT,MAXTT,MAXTA,MAXTC,
     +       NITMNT,NITMNA,TOL,TOLT,TOLA,TOLC,ZMIN,NIT,CI,WA,VTHETA,
     +       DELTA,GRAD,HESSNV,RW1,RW1(IF1),RW1(IF2),RW1(ISC),RW1(ISE),
     +       RW1(ISF),RW1(ISG),RW1(ISH),RW2,IW1,DW1,DW1(IST),DW1(ISD),
     +       DW1(ISU))
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GMAIN2(X,Y,NI,COV,A,THETA,OI,MDX,N,NP,NCOV,B,GAM,TAU,
     +    ICASE,IUGL,IOPT,IALG,ICNVT,ICNVA,MAXIT,MAXTT,MAXTA,MAXTC,
     +    NITMNT,NITMNA,TOL,TOLT,TOLA,TOLC,ZMIN,NIT,CI,WA,VTHETA,
     +    DELTA,GRAD,HESSNV,F0,F1,F2,SC,SE,SF,SG,SH,SX,SP,SA,ST,SD,SU)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI 
C.......................................................................
C
C  MAIN ALGORITHM FOR THE LOGISTIC REGRESSION
C   
      DIMENSION X(MDX,NP),Y(N),WA(N),F0(N),F1(N),F2(N),CI(N),VTHETA(N),
     1          THETA(NP),DELTA(NP),GRAD(NP),COV(NCOV),HESSNV(NCOV),
     2          SC(NCOV),SE(NCOV),SF(NP),SG(NP),SH(NP),SX(MDX,NP),OI(N)
      DOUBLE PRECISION A(NCOV),SA(NCOV),SD(NP),ST(NCOV),SU(N),Z
      INTEGER SP(NP),NI(N)    !,nta(2) 
      LOGICAL NPRCHK
      EXTERNAL ICTHET
      NN=NP*(NP+1)/2
      NPRCHK=NP.GT.0.AND.NP.LE.N.AND.MDX.GE.N.AND.NN.EQ.NCOV.AND.
     1       (ICASE.EQ.1.OR.ICASE.EQ.2.OR.ICASE.EQ.3).AND.MAXIT.GT.0
     2       .AND.MAXTT.GT.0.AND.MAXTA.GT.0.AND.MAXTC.GT.0.AND.
     3       B.GT.SQRT(FLOAT(NP)).AND.GAM.GT.0..AND.TAU.GE.0..AND.
     4       TOL.GT.0..AND.TOLT.GT.0..AND.TOLA.GT.0..AND.TOLC.GT.0.
     5       .AND.(IOPT.EQ.1.OR.IOPT.EQ.2).AND.(IUGL.EQ.1.OR.
     6       IUGL.EQ.2).AND.(ICNVT.EQ.1.OR.ICNVT.EQ.2.OR.ICNVT.EQ.3)
     7       .AND.(ICNVA.EQ.1.OR.ICNVA.EQ.2).AND.(IALG.EQ.-1.OR.
     8       IALG.EQ.-2.OR.IALG.EQ.1.OR.IALG.EQ.2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'GMAIN2',1)
C
C  STEP 0 : INITIALIZATIONS
C  ------
      NIT=1
      DO 10 I=1,N
      CI(I)=0.
   10 CONTINUE
      DO 40 I=1,N
      DO 20 J=1,NP
      SD(J)=DBLE(X(I,J))
   20 CONTINUE
      CALL MLYZD(A,SD,NP,NCOV,NP,1)
      CALL NRM2ZD(SD,NP,1,NP,Z)
      ZNR=SNGL(Z)
      IF (ZNR.GT.ZMIN) GOTO 30
      CALL MESSGE(201,'GYMAIN',0)
      ZNR=ZMIN
   30 WA(I)=B/ZNR
   40 CONTINUE
      INI=1
      QMIN=1.E10
C
C  STEP 1 : COMPUTE THETA
C  ------
  100 DO 150 I=1,NP
      SD(I)=DBLE(THETA(I))
  150 CONTINUE
      CALL GYTST2(X,Y,CI,THETA,WA,COV,NI,OI,N,NP,MDX,NCOV,GAM,TOLT,
     +     TAU,0.01,10,IOPT,ICASE,ICNVT,MAXTT,NITMNT,NITT,Q0,DELTA,
     +     F0,F1,F2,VTHETA,GRAD,HESSNV,SE,SF,SG,SH,SC,SX,SP,INI,QMIN)
c      call intpr('Nit',3,nit,1)
c      call realpr('T-step',6,theta,np)
C
C  STEP 2 : CHECK CONVERGENCE
C  ------
      IF (NIT.EQ.MAXIT.OR.NITT.LT.0) GOTO 500
      DO 200 I=1,NP
      DELTA(I)=THETA(I)-SNGL(SD(I))
  200 CONTINUE
      IF (ICTHET(NP,NCOV,DELTA,1.0,COV,TOL,ICNVT).EQ.1) GOTO 500
C
C  STEP 3 : COMPUTE THE A MATRIX AND THE ai's
C  ------
      CALL GYASTPZ(X,Y,NI,VTHETA,CI,A,OI,B,IUGL,ICASE,N,NP,NCOV,MDX,
     1            TAU,MAXTA,NITMNA,ICNVA,TOLA,NITA,WA,SU,SA,ST,SD)
C
c      call dblepr('A-step',6,a,ncov)
c      nta(1)=NITT
c      nta(2)=NITA
c      call intpr('T&A-nit',7,nta,2)
      IF (NITA.LT.0) GOTO 500
      DO 340 I=1,N   
      ZNR=WA(I)
      IF (ZNR.GT.ZMIN) GOTO 320
      CALL MESSGE(201,'GYMAIN',0)
      ZNR=ZMIN
  320 WA(I)=B/ZNR
  340 CONTINUE

C
C  STEP 4 : COMPUTE THE ci's 
C  ------
      CALL GICSTPZ(ICASE,IALG,NI,VTHETA,WA,OI,N,TOLC,MAXTC,CI)
c      call realpr('C-step',6,ci,20)
C
C  STEP 5 : SET NIT:=NIT+1 AND GOTO STEP 1
C  ------
      NIT=NIT+1
      GOTO 100
  500 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
c     SUBROUTINE GINTACZ(X,Y,NI,OI,MDX,MDT,N,NP,NCOV,ICASE,MAXTT,MAXTA,
c    +                  TOLT,TOLA,B,C,NITT,NITA,SIGMA,A,THETA,CI,DIST,
c    +                  RW1,RW2,IW1,DW1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  INITIAL VALUES FOR THE LOGISTIC REGRESSION
C   
c     DIMENSION X(MDX,NP),Y(N),THETA(MDT),OI(N),CI(N),DIST(N),
c    +          RW1(5*NCOV+3*N),RW2(MDX,NP)
c     DOUBLE PRECISION A(NCOV),DW1(2*NCOV+NP+N)
c     INTEGER NI(N),IW1(NP)
c     LOGICAL NPRCHK
c     NN=NP*(NP+1)/2
c     NPRCHK=NP.GT.0.AND.NP.LE.N.AND.MDX.GE.N.AND.MDT.GE.N.AND.
c    1       NN.EQ.NCOV.AND.(ICASE.EQ.1.OR.ICASE.EQ.2.OR.ICASE.EQ.3)
c    2       .AND.MAXTT.GT.0.AND.MAXTA.GT.0..AND.TOLT.GT.0..AND.
c    3       TOLA.GT.0..AND.B.GT.SQRT(FLOAT(NP)).AND.C.GT.0.
c     IF (.NOT.NPRCHK) CALL MESSGE(500,'GINTAC',1)
c     IWG=NCOV+1
c     ISC=IWG+N
c     ISF=ISC+NCOV
c     ISG=ISF+NCOV
c     ISH=ISG+NCOV
c     ISY=ISH+NCOV
c     ISW=ISY+N
c     IST=NCOV+1
c     ISD=IST+NCOV
c     ISU=ISD+NP
c     CALL GITAC2(X,Y,NI,OI,MDX,MDT,N,NP,NCOV,ICASE,MAXTT,MAXTA,
c    +TOLT,TOLA,B,C,NITT,NITA,SIGMA,A,THETA,CI,DIST,RW1,RW1(IWG),
c    +RW1(ISC),RW1(ISF),RW1(ISG),RW1(ISH),RW1(ISY),RW1(ISW),
c    +RW2,IW1,DW1,DW1(IST),DW1(ISD),DW1(ISU))
c     RETURN
c     END
C 
C-----------------------------------------------------------------------
C
      SUBROUTINE GINTACZ(X,Y,NI,OI,MDX,MDT,N,NP,NCOV,ICASE,MAXTT,
     +           MAXTA,TOLT,TOLA,B,C,NITT,NITA,SIGMA,A,THETA,CI,
     +           DIST) !,COV,WGT,SC,SF,SG,SH,SY,SW,SX,SP,SA,ST,SD,SU)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  INITIAL VALUES FOR THE LOGISTIC REGRESSION
C
      DIMENSION X(MDX,NP),Y(N),CI(N),THETA(MDT),DIST(N),COV(NCOV),WGT(N)
     + ,OI(N),SC(NCOV),SF(NCOV),SG(NCOV),SH(NCOV),SX(MDX,NP),SY(N),SW(N)
      DOUBLE PRECISION UCV,A(NCOV),SA(NCOV),SD(NP),ST(NCOV),SU(N),
     +       SZ(NP),Z
      INTEGER NI(N),SP(NP)
      LOGICAL NPRCHK
      EXTERNAL UCV,PSY,CHI,RHO
      COMMON/BETA/BETA,BET0
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BT,DV,CW
      COMMON/PSIPR/IPSI,CPSI,H1,H2,H3,XK,D
      NN=NP*(NP+1)/2
      NPRCHK=NP.GT.0.AND.NP.LE.N.AND.MDX.GE.N.AND.MDT.GE.N.AND.
     1       NN.EQ.NCOV.AND.(ICASE.EQ.1.OR.ICASE.EQ.2.OR.ICASE.EQ.3)
     2       .AND.MAXTT.GT.0.AND.MAXTA.GT.0..AND.TOLT.GT.0..AND.
     3       TOLA.GT.0..AND.B.GT.SQRT(FLOAT(NP)).AND.C.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'GINTAC',1)
c      IUCO=IUCV  !
c      A2O=A2
c      B2O=B2
      IUCV=1
      A2=0.
      B2=B*B
      NFIRST=N
      TAU=1.E-6
      IERR=0
      NITMON=0
      TOLARS=TAU  
      INIT=1
      ICNV=2
      CALL WIMEDVZ(X,N,NP,NCOV,MDX,1,INIT,NFIRST,A,SY)
      CALL WYFALG(X,A,SW,UCV,N,NP,0,NCOV,MDX,TAU,MAXTA,NITMON,ICNV,
     1            1,0,TOLA,NITA,WGT,SU,SA,ST,SD,SZ)
c     CPSO=CPSI
c     IPSO=IPSI
      CPSI=B
      IPSI=1
      DO 5 I=1,N
       S=WGT(I)
       IF (S.LE.1.E-3) S=1.E-3
       WGT(I)=PSY(S)/S
   5  CONTINUE
      DO 30 L=1,N
      IF (ICASE.EQ.3) GOTO 10
      IF (ICASE.EQ.1) THEN
        ENP1=2.
      ELSE
        ENP1=FLOAT(NI(L))+1.
      ENDIF
      S=(Y(L)+0.5)/ENP1
      SY(L)=ALOG(S/(1.-S))-OI(L)
      GOTO 15
   10 S=Y(L)
      IF (S.LE.0.) S=0.5
      SY(L)=ALOG(S)-OI(L)
   15 SU(L)=DBLE(SY(L)) 
      DO 20 K=1,NP
      SX(L,K)=X(L,K)
   20 CONTINUE
   30 CONTINUE
      CALL RIBET0Z(WGT,N,2,1,TOLT,BET0)
      CALL RILARSZ(SX,SY,N,NP,MDX,MDT,TOLARS,NIT,K,KODE,
     +  SIG0,THETA,SW,CI,SF,SG,SH)
      IF (SIG0.LE.TOLARS) SIG0=1.
      CPSI=C
      D=C
      ITYP=2
      ISIGMA=2
      PSP0=1.0
      GAMT=1.0
      ICNV=1
      MAXIS=1
      MONIT=0
      IA=1
      IAINV=0
      F1=1./FLOAT(N)
      F0=0.
      DO 40 L=1,N
      SY(L)=SNGL(SU(L))
   40 CONTINUE
      DO 50 L=1,N
      SU(L)=DBLE(Y(L))
   50 CONTINUE
      CALL KIEDCHZ(WGT,N,CPSI,ITYP,CI,SW)
      JAINV=IAINV
      CALL KTASKWZ(X,CI,SW,N,NP,MDX,MDX,NCOV,TAU,IA,F1,F0,JAINV,
     +  SC,SF,SG,SH,COV,SX)
      IF (JAINV.GT.400) CALL MESSGE(400,'GINTAC',0)
      CALL RYWALG(X,SY,THETA,WGT,COV,PSP0,PSY,CHI,RHO,SIG0,N,NP,MDX,MDT,
     +  NCOV,TOLT,GAMT,TAU,ITYP,ISIGMA,ICNV,MAXTT,MAXIS,MONIT,NITT,
     +  SIGMA,Y,SC,CI,SF,SG,SH,SP,SW,SX)
      IF (NITT.EQ.MAXTT) IERR=1
c     IUCV=IUCO
c     A2=A2O
c     B2=B2O
c     IPSI=IPSO
c     CPSI=CPSO
      DO 60 L=1,N
      CI(L)=0.
   60 CONTINUE
      DO 70 L=1,N
      Y(L)=SNGL(SU(L))
   70 CONTINUE
      DO 90 I=1,N
      DO 80 J=1,NP
      SD(J)=DBLE(X(I,J))
   80 CONTINUE
      CALL MLYZD(A,SD,NP,NCOV,NP,1)
      CALL NRM2ZD(SD,NP,1,NP,Z)
      DIST(I)=SNGL(Z)
   90 CONTINUE
      RETURN
      END
C 
C-----------------------------------------------------------------------
C
      SUBROUTINE GFEDCAZ(VTHETA,CI,WA,NI,OI,N,ICASE,D,E)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  D AND E MATRICES FOR THE COV. MATRIX OF THE COEFF. ESTIMATES
C
      DIMENSION VTHETA(N),CI(N),OI(N),WA(N),NI(N),D(N),E(N)
      DOUBLE PRECISION GFUN,AA,CC,PREC,TMP,TMPSI,ETERM,DTERM,SUME,SUMD,
     +       SML,ALSML,ZMIN,ZMAX,PROBI,GI,EXGI,LPIJ
      LOGICAL NPRCHK
      DATA PREC,SML,ALSML,ZMIN,ZMAX,NCALL/0.D0,0.D0,0.D0,0.D0,0.D0,0/
      NPRCHK=N.GT.0.AND.(ICASE.EQ.1.OR.ICASE.EQ.2.OR.ICASE.EQ.3)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'GFEDCA',1)
C
      IF (NCALL.EQ.0) THEN
        CALL MACHZ(2,PRCS)
        CALL MACHZD(4,SML)
        CALL MACHZD(5,ALSML)
        PREC = DBLE(PRCS)
        ZMIN = -30.D0
        ZMAX = 70.D0
        NCALL=1
      ENDIF
      DO 500 I=1,N
        GI=DBLE(VTHETA(I)+OI(I))
        CII=CI(I)
        CC=DBLE(CII)
        A=WA(I)
        AA=DBLE(A)
        NN=1
        IF (ICASE.EQ.2) NN=NI(I)
C  MODIFIED 12.03.2018
c       PP=GFUN(ICASE,NN,GI)
c       PI=SNGL(PP)
        ILG=1
        ETERM=100.D0
        DTERM=100.D0
        J=0
        SUME=0.D0
        SUMD=0.D0
        IF (ICASE.LE.2) THEN
C==>    LOGISTIC BERNOUILLI OR BINOMIAL
          IF (GI.LE.ZMIN) THEN
            PROBI=0.D0
          ELSEIF (GI.GE.ZMAX) THEN
            PROBI=1.D0
          ELSE
            EXGI=DEXP(GI)
            PROBI=EXGI/(1.D0+EXGI)
          ENDIF
          GFUN=PROBI*DBLE(NN)
        ELSE
C==>    LOGISTIC POISSON (ICASE=3)
          IF (GI.LE.ZMIN) THEN
            GFUN=DEXP(ZMIN)
          ELSEIF (GI.GE.ZMAX) THEN
            GFUN=DEXP(ZMAX)
          ELSE
            GFUN=DEXP(GI)
          ENDIF
        ENDIF
  250   IF (DMAX1(ETERM,DTERM).LE.PREC) GOTO 350 
        IF (ICASE.LE.2) THEN
C==>    LOGISTIC BERNOUILLI OR BINOMIAL
          CALL PROBINZ(J,NN,PROBI,ILG,LPIJ)
        ELSE
C==>    LOGISTIC POISSON (ICASE=3)
          CALL PRPOISZ(GFUN,J,ILG,LPIJ)
        ENDIF
        TMP=DBLE(J)-CC-GFUN
        TMPSI=DMIN1(AA,TMP)
        TMPSI=DMAX1(-AA,TMPSI)
        TMP=TMPSI**2 
        ETERM = 2.D0*ALSML + LPIJ
        IF (TMP.GT.SML) ETERM = DLOG(TMP) + LPIJ
        ETERM = DEXP(ETERM)
        SUME = SUME + ETERM
        TMPSI=TMPSI*(DBLE(J)-GFUN)
        IF (TMPSI.GT.0.D0) THEN
          DTERM = ALSML + LPIJ
          IF (TMPSI.GT.SML) DTERM = DLOG(TMPSI) + LPIJ
          DTERM = DEXP(DTERM)
          SUMD = SUMD + DTERM
        ELSE
          DTERM = TMPSI*DEXP(LPIJ)
          SUMD = SUMD + DTERM
          DTERM = DABS(DTERM)
        ENDIF
        J = J + 1
        IF (J.GT.NN.AND.ICASE.LE.2) GOTO 350
        GOTO 250 
  350   D(I)=SNGL(SUMD)
        E(I)=SNGL(SUME)
  500 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GYCSTPZ(ICASE,IALG,NI,A,E,TOL,MAXIT,T)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI / G. VAN MELLE
C.......................................................................
C
C  NEWTON-TYPE ALGORITHM FOR THE C-STEP
C   
      DOUBLE PRECISION E1,T1,DPI,PJ,TMPJ,DPREC
      INTEGER H,K
      LOGICAL NPRCHK
      DATA INICA,INIAL,PREC,XP30/0,0,0.0,0.0/
      IF (INICA.EQ.ICASE.AND.INIAL.EQ.IALG) GOTO 10
      INICA=ICASE
      INIAL=IALG
      NPRCHK=(ICASE.EQ.1.OR.ICASE.EQ.2.OR.ICASE.EQ.3).AND.
     1       (IALG.EQ.1.OR.IALG.EQ.2.OR.IALG.EQ.-1.OR.
     2       IALG.EQ.-2).AND.(NI.GT.0.OR.ICASE.NE.2).AND.
     3       A.GT.0..AND.E.GT.0..AND.TOL.GT.0..AND.MAXIT.GT.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'GYCSTP',1)
      IF (PREC.EQ.0.0) THEN
       CALL MACHZ(2,PRCS)
       CALL MACHZD(2,DPREC)
       PREC=100.*PRCS
       XP30=EXP(-30.0)
      ENDIF
C
C  STEP 0.   SET NIT=1
C  ------
C
   10 NIT=1
      PI=0.
C
C  STEP 1.   COMPUTE CURRENT OBJECTIVE FUNCTION VALUE 
C  ------
      IF (ICASE.EQ.1) THEN
C==>    LOGISTIC BERNOULLI : SOLVE EXPLICITLY
        P1=E
        P0=1-P1
        T=0.
        IF (P1.LT.0.5.AND.A.LT.P0) THEN
          T=A*P1/P0 - P1
        ELSEIF (P1.GT.0.5.AND.A.LT.P1) THEN
          T=P0-A*P0/P1
        ENDIF
        T=T+E
        RETURN
      ENDIF
      IF (ICASE.EQ.2) PI=E/FLOAT(NI)
      M=NI
      IF (ICASE.EQ.3) M=MAX0(INT(100.*E),500)
  100 H=-1
      K=-1
      J1=0
      J2=M
      IF (IALG.EQ.1) GOTO 200
      IF (IALG.EQ.2) GOTO 200
C
C  STEP 2.  COMPUTE AUXILIARY QUANTITIES FOR STEP 2, 4 AND 5.
C  _______
      AUX=T-A
      H=INT(AUX)
      IF (AUX.LT.0.AND.AUX.NE.FLOAT(H)) H=H-1
      H=MAX0(H,-1)
      AUX=T+A
      K=INT(AUX)
      IF (AUX.LT.0.AND.AUX.NE.FLOAT(K)) K=K-1
      K=MIN0(K,NI)
      J1=0
      J2=H
  200 SH=0.
      EH=0.
      SK=0.
      EK=0.
      S1=0.
      E1=0.D0
      T1=0.D0
      DJBAR=0.
  210 IF (ICASE.EQ.2) THEN
C==>    LOGISTIC BINOMIAL
        DPI=DBLE(PI)
        DO 220 J=J1,J2
          CALL PROBINZ(J,NI,DPI,0,PJ)
          DEN=(FLOAT(J)-T)
          TEMP=AMIN1(A,ABS(DEN))
          IF (DEN.LT.0.) TEMP=-TEMP
          T1=T1+DBLE(TEMP)*PJ
          E1=E1+DBLE(J)*PJ
          IF (IABS(IALG).NE.2) GOTO 220
          DJ=SNGL(PJ)
          IF (ABS(DEN).GT.1.E-6) DJ=TEMP*SNGL(PJ)/DEN
          DJBAR=DJBAR+DJ
  220   CONTINUE
      ELSEIF (ICASE.EQ.3) THEN
C==>    LOGISTIC POISSON
         EE=E
         IF (E.LT.XP30) EE=XP30
         IF (E.GT.1.E6) EE=1.E6
         DPI=DBLE(EE)
         DO 230 J=J1,J2
          CALL PRPOISZ(DPI,J,0,PJ)
          DEN=(FLOAT(J)-T)
          TEMP=AMIN1(A,ABS(DEN))
          IF (DEN.LT.0.) TEMP=-TEMP
          TMP=TEMP*SNGL(PJ)
          IF (ABS(TMP).LT.PREC) TMP=0. 
          T1=T1+DBLE(TMP)
          TMPJ=DBLE(J)*PJ
          IF (DABS(TMPJ).LT.DPREC) TMPJ=0.D0
          E1=E1+TMPJ
          IF (IABS(IALG).NE.2) GOTO 225
          DJ=SNGL(PJ)
          IF (ABS(DEN).GT.1.E-6) DJ=TMP/DEN
          DJBAR=DJBAR+DJ
  225     IF (TMP.EQ.0..AND.TMPJ.EQ.0.D0) GOTO 240
  230   CONTINUE
      ENDIF
  240 IF (IALG.EQ.1) GOTO 400
      IF (IALG.EQ.2) GOTO 500
      IF (J1.EQ.0.AND.J2.EQ.H) THEN
        IF (H.NE.-1) SH=S1
        IF (H.NE.-1) EH=SNGL(E1)
        J1=H+1
        J2=K
        GOTO 210
      ELSEIF (J1.EQ.H+1.AND.J2.EQ.K) THEN
        SK=S1
        EK=SNGL(E1)
        J1=K+1
        J2=NI
        GOTO 210
      ENDIF
      IF (K.EQ.NI) THEN
        SK=1.
        EK=E
      ENDIF
      TSTAR=A*(1.-SH-SK)+(EK-EH)
      DEN=SK-SH
      IF (ABS(DEN).LE.PREC) DEN=PREC
      TSTAR=TSTAR/DEN
C
C  STEP 3.  VERIFY IF H AND K ARE GOOD
C  _______
      TH=FLOAT(H)-TSTAR
      IF (H+1.EQ.0) TH=-A-1.
      THP1=TH+1.
      TK=FLOAT(K)-TSTAR
      TKP1=TK+1
      IF (K.EQ.NI) TKP1=A+1.
      IF (TH.LE.-A.AND.-A.LT.THP1.AND.TK.LE.A.AND.A.LT.TKP1) THEN
        T=TSTAR
        RETURN
      ENDIF
C
C  STEP 4.  H-ALGORITHM : SET T:=T0 + DELTA
C  _______
  400 DELTA=SNGL(T1)
      IF (IABS(IALG).EQ.1) T=T+DELTA
C
C   STEP 5.  W-ALGORITHM : SET T:=T0 + DELTA/DBAR
C   _______
  500 IF (IABS(IALG).EQ.2) THEN
        IF (ABS(DJBAR).LE.1.E-5) DJBAR=SIGN(1.0,DJBAR)
        DELTA=SNGL(T1)/DJBAR
        T=T+DELTA
      ENDIF
C
C   STEP 6.   CHECK CONVERGENCE
C   ------
C
      IF (ABS(DELTA).LT.TOL.OR.NIT.EQ.MAXIT) GOTO 600
      NIT=NIT+1
      GOTO 100
  600 RETURN 
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GYTSTPZ(X,Y,CI,THETA,WA,COV,NI,OI,N,NP,MDX,NCOV,GAM,
     1           TOL,TAU,IOPT,ICASE,ICNV,MAXIT,NITMON,NIT,Q0,DELTA,
     2           F0,F1,F2,VTHETA,GRAD,HESSNV,RW1,RW2,IW1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  NEWTON ALGORITHM FOR ROBUST LOGISTIC REGRESSION : THETA-STEP
C
      DIMENSION X(MDX,NP),Y(N),THETA(NP),OI(N),WA(N),COV(NCOV),
     1          F0(N),F1(N),F2(N),CI(N),VTHETA(N),GRAD(NP),HESSNV(NCOV),
     2          DELTA(NP),RW1(5*NP),RW2(MDX,NP)
      INTEGER NI(N),IW1(NP)
      LOGICAL NPRCHK
      DATA ZETA,IQ/0.01,10/
C
C  PARAMETER CHECK
C
      NN=NP*(NP+1)/2
      NPRCHK=NP.GT.0.AND.NP.LE.N.AND.MDX.GE.N.AND.NN.EQ.NCOV
     1       .AND.GAM.GT.0..AND.GAM.LT.2..AND.TAU.GE.0..AND.
     2       TOL.GT.0..AND.(ICASE.EQ.1.OR.ICASE.EQ.2.OR.ICASE.EQ.3)
     3       .AND.MAXIT.GT.0.AND.(IOPT.EQ.1.OR.IOPT.EQ.2).AND.
     4       (ICNV.EQ.1.OR.ICNV.EQ.2.OR.ICNV.EQ.3)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'GYTSTP',1)
      ISF=NP+1
      ISG=ISF+NP
      ISH=ISG+NP
      IST=ISH+NP
      NIT=-1
      QMIN=1.E10
      CALL GYTST2(X,Y,CI,THETA,WA,COV,NI,OI,N,NP,MDX,NCOV,GAM,TOL,
     +TAU,ZETA,IQ,IOPT,ICASE,ICNV,MAXIT,NITMON,NIT,Q0,DELTA,F0,
     +F1,F2,VTHETA,GRAD,HESSNV,RW1,RW1(ISF),RW1(ISG),RW1(ISH),
     +RW1(IST),RW2,IW1,NIT,QMIN)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GYTST2(X,Y,CI,THETA,WA,COV,NI,OI,N,NP,MDX,NCOV,GAM,TOL,
     1       TAU,ZETA,IQ,IOPT,ICASE,ICNV,MAXIT,NITMON,NIT,Q0,DELTA,
     2       F0,F1,F2,VTHETA,GRAD,HESSNV,SE,SF,SG,SH,ST,SX,IP,INI,QMIN)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  NEWTON ALGORITHM FOR ROBUST LOGISTIC REGRESSION : THETA-STEP
C
      CHARACTER CC*28
      REAL  X(MDX,NP),Y(N),THETA(NP),WA(N),COV(NCOV),DELTA(NP),
     1      F0(N),F1(N),F2(N),CI(N),VTHETA(N),GRAD(NP),HESSNV(NCOV),
     2      SE(NP),SF(NP),SG(NP),SH(NP),ST(NP),SX(MDX,NP),OI(N)
      INTEGER NI(N),IP(NP),III(1)
      LOGICAL NPRCHK,FIRST,NULF2
C
C  PARAMETER CHECK
C
      NN=NP*(NP+1)/2
      INTCH=1
      NPRCHK=NP.GT.0.AND.NP.LE.N.AND.MDX.GE.N.AND.NN.EQ.NCOV
     1       .AND.GAM.GT.0..AND.TAU.GE.0..AND.TOL.GT.0..AND.
     2       (ICASE.EQ.1.OR.ICASE.EQ.2.OR.ICASE.EQ.3).AND.
     3       MAXIT.GT.0.AND.(IOPT.EQ.1.OR.IOPT.EQ.2).AND.
     4       (ICNV.EQ.1.OR.ICNV.EQ.2.OR.ICNV.EQ.3)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'GYTST2',1)
C
C  STEP 1.   SET NIT=1
C  ------
C
      NIT=1

C
C  STEP 2.   COMPUTE CURRENT OBJECTIVE FUNCTION VALUE AND (-)DERIVATIVES
C  ------
  200 CALL MFYZ(X,THETA,VTHETA,N,NP,MDX,NP,1,N,1)
      CALL LRFNCTZ(ICASE,Y,CI,VTHETA,OI,WA,NI,N,1,1,1,F0,F1,F2,Q0)
      IF (INI.EQ.1) THEN
       INI=-1
       QMIN=Q0
       DO 210 J=1,NP
       SE(J)=THETA(J)
  210  CONTINUE
      ENDIF
C
C  ITERATION MONITORING
C
      IF (NITMON.GT.0.AND.MOD(NIT,NITMON).EQ.0)
     1CALL MONITG(NIT,NP,GAM0,Q0,THETA,DELTA)
C
C  STEP 3.   COMPUTE THE NEGATIVE GRADIENT
C  ------
C
      CALL GRADNT(X,F1,N,NP,MDX,GRAD)
C
C  STEP 4.  COMPUTE THE GENERALIZED INVERSE OF THE NEGATIVE HESSIAN MAT.
C  ------
C
      FIRST=.TRUE.
  400 NULF2=.TRUE.
      DO 420 I=1,N
      SQF2=SQRT(F2(I))
      IF (SQF2.GE.1.E-4) NULF2=.FALSE.
      DO 410 J=1,NP
      SX(I,J)=X(I,J)*SQF2
  410 CONTINUE
  420 CONTINUE
      IF (NULF2) THEN
        DO 430 J=1,NP
        THETA(J)=SE(J)
        DELTA(J)=-GRAD(J)
  430   CONTINUE
        NIT=-NIT
        GOTO 730
      ELSE
        CALL RIMTRFZ(SX,N,NP,MDX,INTCH,TAU,K,SF,SG,SH,IP)
        IF (K.NE.NP) THEN
c          CALL MESSGE(111,'GYTSTP',0)
           III(1)=K
           CC="GYTSTP: Inverse hessian rank"
           L=LEN(CC)
           call intpr(CC,L,III,1)
        ENDIF
        CALL KIASCVZ(SX,K,NP,MDX,NCOV,1.,1.,HESSNV)
        CALL KFASCVZ(SX,HESSNV,K,NP,MDX,NCOV,1.,DELTA,SG,IP) 
      ENDIF
C
C  STEP 5.   COMPUTE THE INCREMENT VECTOR
C  ------
C
      CALL MSFZ(HESSNV,GRAD,DELTA,NP,NCOV,1,NP,NP)
      GAM0=GAM
C     IF (K.LT.NP.AND.IOPT.NE.2) THEN     ! 25.10.12 AR
C       CALL DOTPZ(DELTA,DELTA,NP,1,1,NP,NP,GAM0)
C       IF (GAM0.GT.1.) GAM0=1./SQRT(GAM0)
C       call realpr('gam0',4,gam0,1)
C     ENDIF
      DO 510 J=1,NP
      DELTA(J)=-DELTA(J)
      IF (FIRST) THEN
        ST(J)=THETA(J)
        THETA(J)=THETA(J)+DELTA(J)*GAM0
      ELSE
        THETA(J)=ST(J)+DELTA(J)*GAM0
      ENDIF
  510 CONTINUE
C
C  STEP 6.   DETERMINE THE STEP-LENGTH
C  ------
C
      CALL MFYZ(X,THETA,F2,N,NP,MDX,NP,1,N,1)
      IF (.NOT.FIRST) Q01=Q0L
      CALL LRFNCTZ(ICASE,Y,CI,F2,OI,WA,NI,N,1,0,0,F1,F1,F1,Q0L)
      IF (Q0L.LE.QMIN) THEN
        QMIN=Q0L
        DO 520 J=1,NP
        SE(J)=THETA(J)
  520   CONTINUE
      ENDIF 
      IF (Q0L.LE.Q0) GOTO 700
      IF (.NOT.FIRST) GOTO 650
      FIRST=.FALSE.
      IF (IOPT.EQ.1) THEN
        IF (ICASE.LE.2) CALL DBINOM(Y,CI,VTHETA,WA,NI,F0,OI,N,1.E-6,F2)
        IF (ICASE.EQ.3) CALL DPOISSZ(Y,CI,VTHETA,WA,F0,OI,N,1.E-6,F2)
        GOTO 400
      ELSE
        CALL STPLRGZ(ICASE,X,Y,CI,OI,ZETA,IQ,ST,DELTA,WA,NI,GRAD,
     1              N,NP,MDX,Q0,Q01,GAM0,ST,F0,VTHETA)
        DO 600 J=1,NP
        THETA(J)=ST(J)+DELTA(J)*GAM0
  600   CONTINUE
        GOTO 700
      ENDIF   
  650 IF (Q01.LT.Q0L) THEN
        DO 670 J=1,NP
        DELTA(J)=ST(J)-THETA(J)
        THETA(J)=ST(J)
  670   CONTINUE
      ENDIF
C     STEP LENGTH BY CUBIC INTERPOLATION (USE THE ARRAY SE)
C     CALL STEPLR(ICASE,X,Y,CI,OI,THETA,DELTA,WA,NI,GRAD,SE,
C    1            N,NP,MDX,Q00,Q01,GAM,ST,F0,F1,F2,VTHETA)
C
C  STEP 7. STOP ITERATIONS IF DESIRED PRECISION HAS BEEN REACHED
C  -------
  700 CONTINUE
      IF (NIT.EQ.MAXIT) GOTO 730
      IF (ICTHET(NP,NCOV,DELTA,1.0,COV,TOL,ICNV).EQ.1) GOTO 730
      NIT=NIT+1
      GOTO 200
  730 CALL MFYZ(X,THETA,VTHETA,N,NP,MDX,NP,1,N,1)
      CALL LRFNCTZ(ICASE,Y,CI,VTHETA,OI,WA,NI,N,1,1,1,F0,F1,F2,Q0)
      RETURN
      END
C     
C-----------------------------------------------------------------------
C
      SUBROUTINE DPOISSZ(Y,CI,VTHETA,WA,F0,OI,N,KAP,D)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  APPROXIMATION FOR D(I) IN THE THETA STEP. POISSON CASE.
C
      DOUBLE PRECISION GG,ZMIN,XMIN,YMIN,ZMAX,T1,T2,S1,S2,
     +       DXLOG,XBIG,YY,OO
      REAL Y(N),CI(N),VTHETA(N),OI(N),WA(N),F0(N),KAP,D(N)
      EXTERNAL DXLOG
      DATA ZMIN,XMIN,YMIN,ZMAX,NCALL/0.D0,0.D0,0.D0,0.D0,0/
C
      IF (NCALL.EQ.1) GOTO 10
      IF (KAP.LT.0.) CALL MESSGE(500,'DPOISS',1)
      CALL MACHZD(3,ZMIN)
      CALL MACHZD(4,XMIN)
      CALL MACHZD(5,YMIN)
      CALL MACHZD(6,XBIG)
      XBIG=XBIG/10.D0
      ZMAX=DLOG(XBIG)
      NCALL=1
   10 CONTINUE
      DO 500 I=1,N
        GI=VTHETA(I)
        GG=DBLE(GI)
        OF=OI(I)
        OO=DBLE(OF)
        YI=Y(I)-CI(I)
        YY=DBLE(YI)
        AI=WA(I)
        IF (YI.GT.AI) THEN
          CALL TS12PO(YI,AI,OO,XMIN,YMIN,T1,S1,T2,S2)
        ELSEIF (YI.GT.-AI) THEN
          T2=DXLOG(DBLE(YI+AI),XMIN,YMIN)-OO
          IF (GG.LE.T2.OR.YI.LE.0.) GOTO 300
          ENO=SNGL(DXLOG(YY,XMIN,YMIN))
          IF (F0(I).LT.AI*(ENO-GI)/2.+YI*(1.-ENO)) GOTO 300
          D(I)=AI/ABS(ENO-GI)
          GOTO 500
        ELSE
          D(I)=KAP
          GOTO 500
        ENDIF 
        ELG0=F0(I)
        ELG1=SNGL(-AI*T1+S1)
        ELG2=SNGL( AI*T2+S2)
        IF (ELG0.GE.AMAX1(ELG1,ELG2)) THEN
          DI=SNGL(S1-S2)/(2*AI)-GI
          D(I)=ABS(AI/DI)
          GOTO 500
        ENDIF
  300   IF (T2+OO.LE.ZMIN) THEN
          D(I)=KAP
        ELSEIF (T2+OF.GT.ZMAX) THEN
          D(I)=SNGL(XBIG)
        ELSE
          D(I)=SNGL(DEXP(T2+OO))
        ENDIF
  500 CONTINUE
      RETURN
      END
C     
C-----------------------------------------------------------------------
C
      SUBROUTINE DBINOM(Y,CI,VTHETA,WA,NI,F0,OI,N,KAP,D)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  APPROXIMATION FOR D(I) IN THE THETA STEP. BINOMIAL CASE.
C
      DOUBLE PRECISION GG,ZMIN,ZMAX,XMIN,YMIN,T1,T2,S1,S2,DXLOG,XBIG,YY
      DOUBLE PRECISION GFUN,OO,T01,S01,DNI
      INTEGER NI(N)
      REAL Y(N),CI(N),VTHETA(N),OI(N),WA(N),F0(N),KAP,D(N)
      EXTERNAL GFUN,DXLOG
      DATA ZMIN,XMIN,YMIN,ZMAX,NCALL/0.D0,0.D0,0.D0,0.D0,0/
C
      IF (KAP.LT.0.) CALL MESSGE(500,'DBINOM',1)
      IF (NCALL.EQ.1) GOTO 10
      CALL MACHZD(3,ZMIN)
      CALL MACHZD(4,XMIN)
      CALL MACHZD(5,YMIN)
      CALL MACHZD(6,XBIG)
      XBIG=XBIG/10.D0
      ZMAX=DLOG(XBIG)
      NCALL=1
   10 CONTINUE
      DO 500 I=1,N
        GI=VTHETA(I)
        GG=DBLE(GI)
        OF=OI(I)
        OO=DBLE(OF)
        YI=Y(I)-CI(I)
        YY=DBLE(YI)
        AI=WA(I)
        NII=NI(I)
        ENI=FLOAT(NII)
        DNI=DBLE(ENI)
        ENO=ENI
        IF (YI.GT.AI) THEN
          IF (-YI+ENI.LE.-AI) THEN
            D(I)=KAP
            GOTO 500
          ELSEIF (-YI+ENI.LE.AI) THEN
            IF (ENI.GT.YI) THEN
              CALL TS12BI(YI,AI,ENO,OO,XMIN,YMIN,T01,S01,T2,S2)
              IF (GG.GE.T01) GOTO 300
  100         ENI=ENI+1
              IF (-YI+ENI.LT.AI) GOTO 100
              CALL TS12BI(YI,AI,ENI,OO,XMIN,YMIN,T1,S1,T2,S2)
              T01=GG+(S1-S01)/DBLE(AI)
              DNI=DBLE(ENI)
              DI=SNGL(S1-S2)/(2.*AI) - SNGL(T01)
              CALL BIGGBI(T01,DNI,ZMIN,ZMAX,S2)
              ELG0=SNGL(-YY*T01+ENI*S2)
              CALL BIGGBI(T1,DNI,ZMIN,ZMAX,S2)
              ELG1=SNGL(-YY*T1+ENI*S2)
              CALL BIGGBI(T2,DNI,ZMIN,ZMAX,S2)
              ELG2=SNGL(-YY*T2+ENI*S2)
              IF (ELG0.LE.AMAX1(ELG1,ELG2)) GOTO 300
              D(I)=ABS(AI/DI)
              GOTO 500
            ELSE
              GOTO 300
            ENDIF
          ENDIF
        ELSEIF (YI.LE.AI.AND.-YI.LT.AI) THEN
          S2=DNI/4.D0
          IF (-YI+ENI.GT.AI) THEN
            T2=DBLE(YI+AI)/DBLE(ENI-YI-AI)
            T2=DXLOG(T2,XMIN,YMIN)-OO
            IF (T2.LT.0.D0) THEN
              S2=GFUN(2,1,SNGL(T2)+OF)
              S2=S2*(1.D0-S2)*DNI
            ENDIF
          ENDIF
          D(I)=SNGL(S2)
          GOTO 500
        ELSEIF (-YI.GE.AI) THEN
          D(I)=KAP
          GOTO 500
        ENDIF  
        CALL TS12BI(YI,AI,ENI,OO,XMIN,YMIN,T1,S1,T2,S2)
        ELG0=F0(I)
        ELG1=SNGL(-AI*T1+S1)
        ELG2=SNGL( AI*T2+S2)
        IF (ELG0.GE.AMAX1(ELG1,ELG2)) THEN
          DI=SNGL(S1-S2)/(2*AI)-GI
          D(I)=ABS(AI/DI)
        ELSE
          IF (T2.LE.0.D0) THEN
            S2=GFUN(2,1,SNGL(T2)+OF)
            D(I)=SNGL(S2*(1.D0-S2)*DNI)
          ELSEIF (T1.LE.0.D0.AND.T2.GT.0.D0) THEN
            D(I)=ENI/4.
          ELSE
            S1=GFUN(2,1,SNGL(T1)+OF)
            D(I)=SNGL(S1*(1.D0-S1)*DNI)
          ENDIF
        ENDIF
        GOTO 500
  300   T1=DBLE(YI-AI)/DBLE(ENI-YI+AI)
        T1=DXLOG(T1,XMIN,YMIN)-OO
        S2=DNI/4.D0
        IF (T1.GT.0.D0) THEN
          S2=GFUN(2,1,SNGL(T1)+OF)
          S2=S2*(1.D0-S2)*DNI
        ENDIF
        D(I)=SNGL(S2)
  500 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GYASTPZ(X,Y,NI,VTHETA,CI,A,OI,B,IUGL,ICASE,NOBS,NVAR,
     1                  NCOV,MDX,TAU,MAXIT,NITMON,ICNV,TOL,NIT,DIST,
     2                  SU,SA,ST,SD)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  FIXED POINT ALGORITHM FOR THE COMPUTATION OF THE MATRIX A
C  (STANDARDIZED CASE, A LOWER TRIANGULAR)
C
      REAL X(MDX,NVAR),Y(NOBS),OI(NOBS),VTHETA(NOBS),CI(NOBS),DIST(NOBS)
      DOUBLE PRECISION A(NCOV),SA(NCOV),ST(NCOV),SD(NVAR),SU(NOBS),UGL
      INTEGER NI(NOBS)
      LOGICAL NPRCHK
      EXTERNAL UGL,ICNVA
      COMMON/UGLPR/IIUGL,IICASE,BB
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=B.GT.SQRT(FLOAT(NVAR)).AND.(IUGL.EQ.1.OR.IUGL.EQ.2).AND.
     1       (ICASE.EQ.1.OR.ICASE.EQ.2.OR.ICASE.EQ.3).AND.NVAR.GT.0
     2       .AND.NOBS.GE.NVAR.AND.NCOV.EQ.NN.AND.MDX.GE.NOBS.AND.
     3       TAU.GE.0..AND.MAXIT.GT.0.AND.(ICNV.EQ.1.OR.ICNV.EQ.2)
     4       .AND.TOL.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'GYASTP',1)
C
C  STEP 0 : INITIALIZATION
C  ------
      IIUGL=IUGL
      BB=B
      IICASE=ICASE
      NIT=0
      IF (ICNV.EQ.1) THEN
        L=0
        DO 20 I=1,NVAR
        DO 10 J=1,I
        L=L+1
        SA(L)=0.D0
        IF (I.EQ.J) SA(L)=-1.D0
   10   CONTINUE
   20   CONTINUE
      ENDIF
      DO 30 L=1,NOBS
      DIST(L)=-1.0
   30 CONTINUE
C
C  STEP 1: COMPUTE WEIGHTED COVARIANCE (ST) AND AUXILIARY VALUES
C  ------
  100 CALL UCOWJ(X,Y,NI,VTHETA,OI,CI,UGL,A,ST,NOBS,NVAR,NCOV,MDX,
     1           ICNV,NIT,DELTA,DIST,SU,SD)
C
C  STEP 2: CHECK CONVERGENCE
C  ------
      IF (NIT.EQ.MAXIT.OR.ICNVA(NCOV,DELTA,A,SA,TOL,ICNV).EQ.1)
     +GOTO 500
C
C  STEP 3: FIND IMPROVEMENT MATRIX SS=I-ST FOR A
C  ------ 
      INFO=0
      CALL PRSCF0(ST,NVAR,NCOV,TAU,INFO)
      IF (INFO.NE.0) CALL MESSGE(400+INFO,'GYASTP',0)
      IF (INFO.EQ.1) THEN 
         NIT= -NIT
         RETURN
      ENDIF
C
C  STEP 4: SET SA:=A AND A:=(I-SS)*SA
C  -------
      DO 410 IJ=1,NCOV
      SA(IJ)=A(IJ)
  410 CONTINUE
      CALL MTT3ZD(SA,ST,A,NVAR,NCOV)
      NIT=NIT+1
C
C  STEP 4A: ITERATION MONITORING
C  -------
      IF (NITMON.LE.0) GOTO 100
      IF (MOD(NIT,NITMON).EQ.0) CALL MONITW(NIT,NVAR,NCOV,A,DELTA)
      GOTO 100
C
C  STEP 5: EXIT
C  ------
  500 RETURN
      END
C***********************************************************************
C**************************** H B A U X I ******************************
C
      FUNCTION ICNREP(N,NP,IOPT,IMODE)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : J. JOSS
C.......................................................................
C
C  COMPUTE NUMBER OF REPETITIONS IN RYLMSR
C  M  NUMBER OF OBSERVATIONS
C  NP NUMBER OF PARAMETERS
C  IOPT  0  QUICK VERSION
C        1  EXTENDED VERSION
C        2  NOT USED
C        3  ALL COMBINATIONS
C
      DIMENSION NREPQ(8),NREPE(5)
      DATA NREPQ/150,300,400,500,600,700,850,1250/
      DATA NREPE/500,1000,1500,2000,2500/
C     GOTO (1,2,3,4) IOPT+1
      ICNREP=0
      IF (IOPT.EQ.1) GOTO 2
      IF (IOPT.EQ.2) GOTO 3
      IF (IOPT.EQ.3) GOTO 4
      IF(NP .GE. 9) THEN
         ICNREP=1500
      ELSE
         ICNREP=NREPQ(NP)
      ENDIF
      RETURN
    2 IF(NP .GE. 6) THEN
         ICNREP=3000
      ELSE
         ICNREP=NREPE(NP)
      ENDIF
    3 RETURN
    4 NN=N
      NR=1
      DO 10 I=1,NP
         NR=(NR*NN)/I
         NN=NN-1
   10 CONTINUE
      IF (IMODE.GE.3) NR=NR*2**(NP-1)
      ICNREP=NR
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE NCOMB(N,NP,IT)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : J .JOSS
C.......................................................................
C
C  COMPUTE ALL COMBINATIONS FOR RESAMPLING ALGORITHM
C
      DIMENSION IT(NP)
      IN=NP
   10 IT(IN)=IT(IN)+1
      IF(IT(IN).GT.N-NP+IN) THEN
         IN=IN-1
         GOTO 10
      ENDIF
      IF(IN.NE.NP) THEN
         DO 20 I=IN+1,NP
         IT(I)=IT(I-1)+1
   20    CONTINUE
      ENDIF
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE RICLL1(XT,Y,N,NP,MDXT,THETA,SH,SP)
      DIMENSION XT(MDXT,NP),Y(N),THETA(MDXT),SH(NP)
      INTEGER SP(NP)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : J. JOSS
C.......................................................................
C
C  HOUSHOLDER TRANSFORMATION OF THE RIGHT SIDE
C
      DO 20 JJ=1,NP
      J=JJ
      CALL H12Z(2,J,J+1,N,XT(1,J),1,SH(J),Y,1,N,1,N)
   20 CONTINUE
C
C  SOLVE THE SYSTEM
C
      DO 30 I=1,N
      THETA(I)=Y(I)
   30 CONTINUE
      CALL SOLV(XT,THETA,NP,NP,MDXT,N)
C
C  TRANSFORM THE SOLUTION VECTOR FOR OUTPUT
C
      CALL PERM(THETA,SP,NP,NP)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE LMSADJ(N,N2,N2P,IR,CSTETA,RS,EM,SZ)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION RS(N),SZ(N)
C     N2=N/2
C     N2P=N-N2
      DO 100 I=1,N
      SZ(I)=RS(I)
  100 CONTINUE
      CALL SRT1Z(SZ,N,1,N)
      EM=SZ(1+N2)-SZ(1)
      JP=1
      DO 200 J=1,N2P
        IF (SZ(J+N2)-SZ(J).LT.EM) THEN
          EM=SZ(J+N2)-SZ(J)
          JP=J
        ENDIF
  200 CONTINUE
      EM=EM/2.
      T=CSTETA
      CSTETA=CSTETA+0.5*(SZ(JP)+SZ(JP+N2))
      IF (IR.EQ.0) RETURN
      DO 300 I=1,N
      RS(I)=RS(I)+T-CSTETA
  300 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LTSADJ(N,K1,K2,IR,CSTETA,RS,S,SZ)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION RS(N),SZ(N)
C     K1=N/2+1
C     K2=N-K1+1
      FK=FLOAT(K1)
      DO 100 I=1,N
      SZ(I)=RS(I)
  100 CONTINUE
      CALL SRT1Z(SZ,N,1,N)
      ZM=0.
      DO 200 I=1,K1
      ZM=ZM+SZ(I)
  200 CONTINUE
      ZM=ZM/FK
      Q=0.
      DO 300 I=1,K1
      Q=Q+(SZ(I)-ZM)**2
  300 CONTINUE
      T=CSTETA
      S=Q
      CSTETA=T+ZM
      DO 400 J=2,K2
        ZM1=ZM
        ZM=(FK*ZM1-SZ(J-1)+SZ(J+K1-1))/FK
        Q=Q-SZ(J-1)**2+SZ(J+K1-1)**2-FK*ZM**2+FK*ZM1**2
        IF (Q.LT.S) THEN
          S=Q
          CSTETA=T+ZM
        ENDIF
  400 CONTINUE
      IF (IR.EQ.0) RETURN
      DO 500 I=1,N
      RS(I)=RS(I)+T-CSTETA
  500 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE QRSSH(RS,EXRHO,N,NP,SIGMA,QR)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION RS(N)
      EXTERNAL EXRHO
      TMP=0.
      DO 10 I=1,N
        S=RS(I)/SIGMA
        TMP=TMP+EXRHO(S)
   10 CONTINUE
      QR=TMP/FLOAT(N-NP)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE QRSSHW(RS,WGT,EXRHO,N,NP,SIGMA,QR)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION RS(N),WGT(N)
      EXTERNAL EXRHO
      TMP=0.
      DO 10 I=1,N
        S=RS(I)/SIGMA
        TMP=TMP+WGT(I)*EXRHO(S)
   10 CONTINUE
      QR=TMP/FLOAT(N-NP)
      RETURN
      END
C***********************************************************************
C***************************  H B M A I N  *****************************
C
      SUBROUTINE HYLMSEZ(X,Y,N,NP,NQ,MDX,MDW,MDI,IK,IOPT,INTCH,NREP,
     *           TOL,TAU,ISEED,IERR,XMIN,THETA,RS,IT1,WORK,IWORK)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),THETA(NP),RS(N),IT1(NQ)
      DIMENSION WORK(MDW),IWORK(MDI)
C
C                            N         NP                   2
C     HYLMSE SOLVES    Min  Med ( Y(I)-Sum X(I,K)*THETA(K) )
C                     THETA I=1        K=1
C
      MQ=NQ
      IF (IK.EQ.3) MQ=NP+1
      IF (MQ.NE.NQ) CALL MESSGE(150,'HYLMSE',0)
      IMIN=NP+MQ
      NP1=NP+1
      MINW=(NP+2)*MQ+3*NP+N
      IF (N.LE.0 .OR. MDX.LT.N .OR. NP.LE.0 .OR. 2*NP.GE.N
     * .OR. NQ.LT.NP .OR. MDW.LT.MINW .OR. MDI.LT.IMIN .OR.
     * IK.LT.0 .OR. IK.GT.3 .OR.IOPT.LT.0 .OR. IOPT.GT.3 .OR.
     * (IOPT.EQ.2 .AND. NREP.LE.0) .OR. (INTCH.NE.0.AND.INTCH.NE.1)
     * .OR. TOL.LE.0. .OR. TAU.LT.0.) CALL MESSGE(500,'HYLMSE',1)
      N0=NP*MQ+1
      N1=N0+MQ
      N2=N1+MQ
      N3=N2+NP
      N4=N3+NP
      N5=N4+NP
      CALL HLMSE2(X,Y,N,NP,MQ,MDX,IK,IOPT,INTCH,NREP,TOL,TAU,ISEED,
     *            IERR,XMIN,THETA,RS,IT1,WORK(1),WORK(N0),WORK(N1),
     *            WORK(N2),WORK(N3),WORK(N4),WORK(N5),
     *            IWORK(1),IWORK(NP1))
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE HLMSE2(X,Y,N,NP,NQ,MDX,IK,IOPT,INTCH,NREP,TOL,TAU,
     *                  ISEED,IERR,XMIN,THETA,RS,IT1,
     *                  XX,YY,XTHETA,SF,SG,SH,SZ,SP,IT)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),THETA(NP),RS(N)
      DIMENSION XX(NQ,NP),YY(NQ),XTHETA(NQ)
      DIMENSION SF(NP),SG(NP),SH(NP),SZ(N)
      INTEGER IT1(NQ),SP(NP),IT(NQ)
      EXTERNAL ICNREP
C
C                            N         NP                   2
C     HLMSE2 SOLVES    Min  Med ( Y(I)-Sum X(I,K)*THETA(K) )
C                     THETA I=1        K=1
C
      IF (N.LE.0 .OR. MDX.LT.N .OR. NP.LE.0 .OR. 2*NP.GE.N
     * .OR. NQ.LT.NP .OR. IK.LT.0 .OR. IK.GT.3 .OR.
     * IOPT.LT.0 .OR. IOPT.GT.3 .OR. (IOPT.EQ.2 .AND. NREP.LE.0)
     * .OR. (INTCH.NE.0.AND.INTCH.NE.1) .OR. TOL.LE.0. .OR. TAU.LT.0.)
     * CALL MESSGE(500,'HLMSE2',1)
C
C STEP 0: INITIALIZATIONS
C ------
      N2=N/2
      N2P=N-N2
      K1=N2+1
      NK1=N-K1+1
      IF (IOPT.NE.2) NREP=ICNREP(N,NQ,IOPT,0)
      NIT=1
      IERR=2
      XMIN=0.
      DO 10 I=1,NP
      SP(I)=I
   10 CONTINUE
C
C  Check for a constant term and set NC to the constant index
C
      NC=0
      DO 25 J=1,NP
         X1J=X(1,J)
         DO 20 I=2,N
            IF (X(I,J).NE.X1J) GOTO 25
   20    CONTINUE
         NC=J
         GOTO 30
   25 CONTINUE
   30 IF (IK.GE.1.AND.IK.LE.2.AND.NC.EQ.0) CALL MESSGE(499,'HLMSE2',1)
C
C STEP 1: DRAW A SUBSAMPLE
C ------
  100 IF (IOPT.NE.3) THEN
        DO 130 K=1,NQ
  110     CALL RANDOW(ISEED,RND)
          ITK=INT(RND*FLOAT(N))+1
          IF (ITK.GT.N) ITK=N
          DO 120 KK=1,K-1
          IF (ITK.EQ.IT(KK)) GOTO 110
  120     CONTINUE
          IT(K)=ITK
  130   CONTINUE
      ELSE
        IF (NIT.EQ.1) THEN
          DO 140 K=1,NQ
          IT(K)=K
  140     CONTINUE
        ELSE
          CALL  NCOMB(N,NQ,IT)
        ENDIF
      ENDIF
      DO 160 K=1,NQ
      ITK=IT(K)
      DO 150 J=1,NP
      XX(K,J)=X(ITK,J)
  150 CONTINUE
      YY(K)=Y(ITK)
  160 CONTINUE
C
C STEP 2: DECOMPOSE SAMPLE MATRIX
C -------
      CALL RIMTRFZ(XX,NQ,NP,NQ,INTCH,TAU,KK,SF,SG,SH,SP)
      IF(KK.NE.NP) GOTO 800
C
C STEP 3: SOLVE SYSTEM OF LINEAR EQUATIONS
C -------
      CALL RICLL1(XX,YY,NQ,NP,NQ,XTHETA,SH,SP)
      IF (IK.NE.3) GOTO 400
      SUMAR=0.
      SUMR2=0.
      DO 320 I=1,NQ
        ITK=IT(I)
        RI=Y(ITK)
        DO 310 J=1,NP
        RI=RI-XTHETA(J)*X(ITK,J)
  310   CONTINUE
        YY(I)=SIGN(1.,RI)
        SUMAR=SUMAR+ABS(RI)
        SUMR2=SUMR2+RI**2
  320 CONTINUE
      IF (SUMAR.LE.TAU) GOTO 400
      DO 330 I=1,NQ
        YY(I)=YY(I)*SUMR2/SUMAR
  330 CONTINUE
      CALL RICLL1(XX,YY,NQ,NP,NQ,SZ,SH,SP)
      DO 340 I=1,NP
        XTHETA(I)=XTHETA(I)-SZ(I)
  340 CONTINUE
C
C STEP 4: COMPUTE RESIDUALS
C -------
  400 L=0
      DO 420 I=1,N
      RI=Y(I)
      DO 410 J=1,NP
      RI=RI-XTHETA(J)*X(I,J)
  410 CONTINUE
      IF (IK.EQ.2) GOTO 415
      ARI=ABS(RI)
      IF (ARI.GT.XMIN) L=L+1
      IF (XMIN.NE.0..AND.L.GE.NK1) GOTO 800
      SZ(I)=ARI
  415 RS(I)=RI
  420 CONTINUE
C
C STEP 5: IF K.LT.1 COMPUTE THE K-TH ORDER STATISTIC OF THE |RS(I)|
C -------
      IF (IK.EQ.2) GOTO 600
      CALL FSTORDZ(SZ,N,K1,XRES)
      GOTO 700
C
C STEP 6: COMPUTE XRES, ADJUST CONSTANT COEFFICIENT AND RS
C -------
  600 CALL LMSADJ(N,N2,N2P,0,XTHETA(NC),RS,XRES,SZ)
C
C STEP 7: UPDATE BEST FIT
C ------
  700 IF (XMIN.NE.0. .AND. XRES.GE.XMIN) GOTO 800
      IERR=0
      XMIN=XRES
      DO 710 K=1,NP
      THETA(K)=XTHETA(K)
  710 CONTINUE
      DO 720 K=1,NQ
      IT1(K)=IT(K)
  720 CONTINUE
      IF (XRES .LE. TOL) THEN
        IERR=1
        GOTO 900
      ENDIF
C
C STEP 8: END OF MAIN LOOP
C -------
  800 IF (NIT.EQ.NREP) GOTO 900
      NIT=NIT+1
      GOTO 100
C
C STEP 9: IF IK=1 ADJUST CONSTANT COEFFICIENT AND RESIDUALS
C --------
  900 IF (IERR.EQ.2) RETURN
      DO 920 I=1,N
      S=Y(I)
      DO 910 J=1,NP
      S=S-THETA(J)*X(I,J)
  910 CONTINUE
      RS(I)=S
  920 CONTINUE
      IF (IK.EQ.1) CALL LMSADJ(N,N2,N2P,1,THETA(NC),RS,XMIN,SZ)
C
C STEP 10: EXIT
C --------
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE HYLTSEZ(X,Y,N,NP,NQ,MDX,MDW,MDI,IK,IOPT,INTCH,NREP,
     *           TOL,TAU,ISEED,IERR,SMIN,THETA,RS,IT1,WORK,IWORK)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),THETA(NP),RS(N),IT1(NQ)
      DIMENSION WORK(MDW),IWORK(MDI)
C
C                          K             2
C   HYLTSE SOLVES    Min  Sum ( RS([I]) ), with K=N/2+1
C                   THETA I=1
C
      IMIN=NP+NQ
      MINW=(NP+2)*NQ+3*NP+N
      IF (N.LE.0 .OR. MDX.LT.N .OR. NP.LE.0 .OR. 2*NP.GE.N
     * .OR. NQ.LT.NP .OR. MDW.LT.MINW .OR. MDI.LT.IMIN .OR.
     * IK.LT.0 .OR. IK.GT.2 .OR.IOPT.LT.0 .OR. IOPT.GT.3 .OR.
     * (IOPT.EQ.2 .AND. NREP.LE.0) .OR. (INTCH.NE.0.AND.INTCH.NE.1)
     * .OR. TOL.LE.0. .OR. TAU.LT.0.) CALL MESSGE(500,'HYLTSE',1)
      NP1=NP+1
      N0=NP*NQ+1
      N1=N0+NQ
      N2=N1+NQ
      N3=N2+NP
      N4=N3+NP
      N5=N4+NP
      CALL HLTSE2(X,Y,N,NP,NQ,MDX,IK,IOPT,INTCH,NREP,TOL,TAU,ISEED,
     *            IERR,SMIN,THETA,RS,IT1,WORK(1),WORK(N0),WORK(N1),
     *            WORK(N2),WORK(N3),WORK(N4),WORK(N5),
     *            IWORK(1),IWORK(NP1))
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE HLTSE2(X,Y,N,NP,NQ,MDX,IK,IOPT,INTCH,NREP,TOL,
     *                  TAU,ISEED,IERR,SMIN,THETA,RS,IT1,
     *                  XX,YY,XTHETA,SF,SG,SH,SZ,SP,IT)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),THETA(NP),RS(N)
      DIMENSION XX(NQ,NP),YY(NQ),XTHETA(NQ)
      DIMENSION SF(NP),SG(NP),SH(NP),SZ(N)
      INTEGER IT1(NQ),SP(NP),IT(NQ)
      EXTERNAL ICNREP
C
C                          K             2
C   HLTSE2 SOLVES    Min  Sum ( RS([I]) ), with K=N/2+1
C                   THETA I=1
C
      IF (N.LE.0 .OR. MDX.LT.N .OR. NP.LE.0 .OR. 2*NP.GE.N
     * .OR. NQ.LT.NP .OR. IK.LT.0 .OR. IK.GT.2 .OR.
     * IOPT.LT.0 .OR. IOPT.GT.3 .OR. (IOPT.EQ.2 .AND. NREP.LE.0)
     * .OR. (INTCH.NE.0.AND.INTCH.NE.1) .OR. TOL.LE.0. .OR. TAU.LT.0.)
     * CALL MESSGE(500,'HLTSE2',1)
C
C STEP 0: INITIALIZATIONS
C ------
      N2=N/2
      N2P=N-N2
      K1=N2+1
      NK1=N-K1+1
      IF (IOPT.NE.2) NREP=ICNREP(N,NQ,IOPT,0)
      NIT=1
      IERR=2
      SMIN=0.
      DO 10 I=1,NP
      SP(I)=I
   10 CONTINUE
C
C  Check for a constant term and set NC to the constant index
C
      NC=0
      DO 25 J=1,NP
         X1J=X(1,J)
         DO 20 I=2,N
            IF (X(I,J).NE.X1J) GOTO 25
   20    CONTINUE
         NC=J
         GOTO 30
   25 CONTINUE
   30 IF (IK.EQ.0.AND.NC.NE.0) CALL MESSGE(150,'HLTSE2',0)
      IF (IK.NE.0.AND.NC.EQ.0) CALL MESSGE(501,'HLTSE2',1)
C
C STEP 1: DRAW A SUBSAMPLE
C ------
  100 IF (IOPT.NE.3) THEN
        DO 130 K=1,NQ
  110     CALL RANDOW(ISEED,RND)
          ITK=INT(RND*FLOAT(N))+1
          IF (ITK.GT.N) ITK=N
          DO 120 KK=1,K-1
          IF (ITK.EQ.IT(KK)) GOTO 110
  120     CONTINUE
          IT(K)=ITK
  130   CONTINUE
      ELSE
        IF (NIT.EQ.1) THEN
          DO 140 K=1,NQ
          IT(K)=K
  140     CONTINUE
        ELSE
          CALL  NCOMB(N,NQ,IT)
        ENDIF
      ENDIF
      DO 160 K=1,NQ
      ITK=IT(K)
      DO 150 J=1,NP
      XX(K,J)=X(ITK,J)
  150 CONTINUE
      YY(K)=Y(ITK)
  160 CONTINUE
C
C STEP 2: DECOMPOSE SAMPLE MATRIX
C -------
      CALL RIMTRFZ(XX,NQ,NP,NQ,INTCH,TAU,KK,SF,SG,SH,SP)
      IF(KK.NE.NP) GOTO 800
C
C STEP 3: SOLVE SYSTEM OF LINEAR EQUATIONS
C -------
      CALL RICLL1(XX,YY,NQ,NP,NQ,XTHETA,SH,SP)
C
C STEP 4: COMPUTE RESIDUALS
C -------
      DO 420 I=1,N
      S=Y(I)
      DO 410 J=1,NP
      S=S-XTHETA(J)*X(I,J)
  410 CONTINUE
      IF (IK.NE.2) SZ(I)=S*S
      RS(I)=S
  420 CONTINUE
C
C STEP 5: COMPUTE THE K-TH ORDER STATISTIC OF THE |RS(I)|-S
C -------
      IF (IK.EQ.2) GOTO 600
      CALL SRT1Z(SZ,N,1,N)
      SRES=0.
      DO 510 I=1,K1
      SRES=SRES+SZ(I)
  510 CONTINUE
      GOTO 700
C
C STEP 6: COMPUTE SRES, ADJUST CONSTANT COEFFICIENT AND RS
C -------
  600 CALL LTSADJ(N,K1,NK1,0,XTHETA(NC),RS,SRES,SZ)
C
C STEP 7: UPDATE BEST FIT
C ------
  700 IF (SMIN.NE.0. .AND. SRES.GE.SMIN) GOTO 800
      IERR=0
      SMIN=SRES
      DO 710 K=1,NP
      THETA(K)=XTHETA(K)
  710 CONTINUE
      DO 720 K=1,NQ
      IT1(K)=IT(K)
  720 CONTINUE
      IF (SRES .LE. TOL) THEN
        IERR=1
        GOTO 900
      ENDIF
C
C STEP 8: END OF MAIN LOOP
C -------
  800 IF (NIT.EQ.NREP) GOTO 900
      NIT=NIT+1
      GOTO 100
C
C STEP 9: IF IK.NE.2 ADJUST CONSTANT COEFFICIENT AND RESIDUALS
C --------
  900 IF (IERR.EQ.2) RETURN
      DO 920 I=1,N
      S=Y(I)
      DO 910 J=1,NP
      S=S-THETA(J)*X(I,J)
  910 CONTINUE
      RS(I)=S
  920 CONTINUE
      IF (IK.NE.2) CALL LTSADJ(N,K1,NK1,1,THETA(NC),RS,SMIN,SZ)
C
C STEP 10: EXIT
C --------
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE HYSEST(X,Y,N,NP,NQ,NCOV,MDX,MDW,MDI,IOPT,INTCH,NREP,
     *           TOLS,TOLR,TAU,GAM,MAXIT,MAXS1,MAXS2,EXPSI,EXPSP,EXCHI,
     *           ISEED,IERR,SMIN,THETA,RS,IT1,COV,WORK,IWORK)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),THETA(N),RS(N),IT1(NQ),COV(NCOV)
      DIMENSION WORK(MDW),IWORK(MDI)
      EXTERNAL EXPSI,EXPSP,EXCHI
      COMMON/BETA/BETA,BET0
C
C   Resampling algorithm for the computation of S-estimates
C
      IMIN=NP+NQ
      NP1=NP+1
      MINW=NQ*(NP+2)+(MDX+3)*NP+N
      NN=NP*(NP+1)/2
      IF (N.LE.0 .OR. MDX.LT.N .OR. NP.LE.0 
     * .OR. NQ.LT.NP .OR. NCOV.NE.NN .OR. MDW.LT.MINW .OR. MDI.LT.IMIN
     * .OR. IOPT.LT.0 .OR. IOPT.GT.3 .OR. (IOPT.EQ.2 .AND. NREP.LE.0)
     * .OR. (INTCH.NE.0.AND.INTCH.NE.1) .OR. TOLS.LE.0. OR.
     *  TOLR.LE.0. .OR. TAU.LT.0. .OR. GAM.LE.0. .OR. GAM.GT.2. .OR.
     * MAXIT.LE.0 .OR. MAXS1.LE.0 .OR. MAXS2.LE.0)
     * CALL MESSGE(500,'HYSEST',1)
      N0=NP*NQ+1
      N1=N0+NQ
      N2=N1+NQ
      N3=N2+NP
      N4=N3+NP
      N5=N4+NP
      N6=N5+MDX*NP
      CALL HSEST2(X,Y,N,NP,NQ,NCOV,MDX,IOPT,INTCH,NREP,TOLS,TOLR,
     *            TAU,GAM,MAXIT,MAXS1,MAXS2,EXPSI,EXPSP,EXCHI,
     *            ISEED,IERR,SMIN,THETA,RS,IT1,COV,
     *            WORK(1),WORK(N0),WORK(N1),WORK(N2),WORK(N3),WORK(N4),
     *            WORK(N5),WORK(N6),IWORK(1),IWORK(NP1))
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE HSEST2(X,Y,N,NP,NQ,NCOV,MDX,IOPT,INTCH,NREP,TOLS,TOLR,
     *           TAU,GAM,MAXIT,MAXS1,MAXS2,EXPSI,EXPSP,EXCHI,ISEED,
     *           IERR,SMIN,THETA,RS,IT1,COV,XX,YY,XTHETA,
     *           SF,SG,SH,SX,SZ,SP,IT)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),THETA(N),RS(N),COV(NCOV),XX(NQ,NP),YY(NQ)
      DIMENSION XTHETA(NQ),SF(NP),SG(NP),SH(NP),SX(MDX,NP),SZ(N)
      INTEGER IT1(NQ),SP(NP),IT(NQ)
      EXTERNAL EXPSI,EXPSP,EXCHI,ICNREP
      COMMON/BETA/BETA,BET0,/CONST/CONST
C
C   Resampling algorithm for the computation of S-estimates
C
      IF (N.LE.0 .OR. MDX.LT.N .OR. NP.LE.0
     * .OR. NQ.LT.NP .OR. NCOV.NE.NP*(NP+1)/2 .OR.
     *  IOPT.LT.0 .OR. IOPT.GT.3 .OR. (IOPT.EQ.2 .AND. NREP.LE.0)
     * .OR. (INTCH.NE.0.AND.INTCH.NE.1) .OR. TOLS.LE.0. OR.
     *  TOLR.LE.0. .OR. TAU.LT.0. .OR. GAM.LE.0. .OR. GAM.GT.2. .OR.
     * MAXIT.LE.0 .OR. MAXS1.LE.0 .OR. MAXS2.LE.0)
     * CALL MESSGE(500,'HSEST2',1)
C
C STEP 0: INITIALIZATIONS
C ------
      N2=N/2
      N2P=N-N2
      K1=N2+1
      NK1=N-K1+1
      CONST=BETA*FLOAT(N-NP)
      IF (IOPT.NE.2) NREP=ICNREP(N,NQ,IOPT,0)
      NIT=1
      IERR=2
      SMIN=0.
      ITYPE=1
      NITMON=0
      PSP0=EXPSP(0.)
C
C STEP 1: DRAW A SUBSAMPLE
C ------ 
c     irep=0 
  100 IF (IOPT.NE.3) THEN
        DO 130 K=1,NQ
  110     CALL RANDOW(ISEED,RND)
c         irep=irep+1
c         ver(irep)=rnd 
c         IF (irep.eq.5) then
c           call realpr('rnd',3,ver,5)
c           irep=0
c         endif
          ITK=INT(RND*FLOAT(N))+1
          DO 120 KK=1,K-1
          IF (ITK.EQ.IT(KK)) GOTO 110
  120     CONTINUE
          IT(K)=ITK
  130   CONTINUE
      ELSE
        IF (NIT.EQ.1) THEN
          DO 140 K=1,NQ
          IT(K)=K
  140     CONTINUE
        ELSE
          CALL  NCOMB(N,NQ,IT)
        ENDIF
      ENDIF
      DO 160 K=1,NQ
      ITK=IT(K)
      DO 150 J=1,NP
      XX(K,J)=X(ITK,J)
  150 CONTINUE
      YY(K)=Y(ITK)
  160 CONTINUE
C
C STEP 2: DECOMPOSE SAMPLE MATRIX
C -------
      CALL RIMTRFZ(XX,NQ,NP,NQ,INTCH,TAU,KK,SF,SG,SH,SP)
      IF(KK.NE.NP) GOTO 700
C
C STEP 3: SOLVE SYSTEM OF LINEAR EQUATIONS
C -------
      CALL RICLL1(XX,YY,NQ,NP,NQ,XTHETA,SH,SP)
C
C STEP 4: COMPUTE RESIDUALS
C -------
      DO 420 I=1,N
      S=Y(I)
      DO 410 J=1,NP
      S=S-XTHETA(J)*X(I,J)
  410 CONTINUE
      RS(I)=S
  420 CONTINUE
      IF (SMIN.EQ.0.) THEN
        S=1.0E7
        DO 430 I=1,N
        ARI=ABS(RS(I))
        SZ(I)=ARI
        IF (ARI.NE.0.) S=AMIN1(S,ARI)
  430   CONTINUE
        IF (S.EQ.1.0E7) GOTO 915
        CALL FSTORDZ(SZ,N,K1,S0)
        S0=2.*S0
        IF (S0.EQ.0.) S0=S
        SRES=S0
      ENDIF
  435 D=0.
      DO 440 I=1,N
      D=D+EXCHI(RS(I)/SRES)
  440 CONTINUE
      IF (SMIN.NE.0..AND.D.GT.CONST) GOTO 700
      IF (D.LE.CONST) GOTO 500
      S0=1.5*S0
      SRES=S0
      GOTO 435
C
C STEP 5: SOLVE FOR SRES
C -------
  500 CALL RYSIGM(RS,SZ,EXCHI,S0,N,NP,TOLR,ITYPE,1,MAXS1,NIS,SRES,SZ,SZ)
      IF (NIS.EQ.MAXS1) CALL MESSGE(110,'HSEST2',0)
C
C STEP 6: UPDATE BEST FIT
C ------
      IERR=0
      SMIN=SRES
      S0=SMIN
      DO 610 K=1,NP
      THETA(K)=XTHETA(K)
  610 CONTINUE
      DO 620 K=1,NQ
      IT1(K)=IT(K)
  620 CONTINUE
      IF (SRES .LE. TOLS) THEN
        IERR=1
        GOTO 800
      ENDIF
C
C STEP 7: END OF MAIN LOOP
C -------
  700 IF (NIT.EQ.NREP) GOTO 800
      NIT=NIT+1
      GOTO 100
C
C STEP 8: SOLVE SYSTEM OF EQUATIONS FOR THETA AND SRES
C --------
  800 IF (IERR.EQ.2) RETURN
      DO 820 I=1,N
      S=Y(I)
      DO 810 J=1,NP
      S=S-THETA(J)*X(I,J)
  810 CONTINUE
      RS(I)=S
  820 CONTINUE
      K=1
      MAXIW=1
      ISIGMA=-1
  830 SWI=0.
      DO 860 I=1,N
      WI=0.
      IF (RS(I).EQ.0.) GOTO 840
      T=RS(I)/SMIN
      WI=EXPSI(T)/T
      SWI=SWI+WI
      WI=SQRT(WI)
  840 DO 850 J=1,NP
      SX(I,J)=WI*X(I,J)
  850 CONTINUE
  860 CONTINUE
      CALL KFFACV(RS,EXPSI,EXPSP,N,NP,SMIN,FH)
      FACT=FH*SWI/FLOAT(N)
      IF (K.EQ.0) FACT=FACT*SMIN*SMIN
      CALL KTASKVZ(SX,N,NP,MDX,NCOV,TAU,FACT,XX,COV)
      IF (K.EQ.0) RETURN
      SRES=SMIN
      ICNV=1
      DO 870 J=1,NP
      XTHETA(J)=THETA(J)
  870 CONTINUE
      IF (MAXIW.EQ.1) CALL QRSSH(RS,EXCHI,N,NP,SRES,QR0)
  880 CONTINUE
       CALL RYWALG(X,Y,THETA,SZ,COV,PSP0,EXPSI,EXCHI,EXCHI,SRES,
     * N,NP,MDX,MDX,NCOV,TOLR,GAM,TAU,ITYPE,ISIGMA,ICNV,MAXIW,
     * MAXS2,NITMON,NIT8,SMIN,RS,YY,SZ,SF,SG,SH,SP,SZ,SX)
C
C STEP 9: EXIT
C -------
      IF (MAXIT.EQ.1) GOTO 900
      IF (MAXIW.EQ.1) THEN
        CALL QRSSH(RS,EXCHI,N,NP,SRES,QR1)
        IF (QR0.LE.QR1) GOTO 910
        ISIGMA=1
        MAXIW=MAXIW+MAXIT
        GOTO 880
      ENDIF
      IF (NIT8.EQ.MAXIW) CALL MESSGE(111,'HSEST2',0)
      IF (SMIN.GE.SRES) GOTO 910
  900 K=0
      GOTO 830
  910 CALL MESSGE(112,'HSEST2',0)
      SMIN=SRES
      FACT=SMIN*SMIN
      CALL SCALZ(COV,FACT,NCOV,1,NCOV)
  915 DO 920 J=1,NP
      THETA(J)=XTHETA(J)
  920 CONTINUE
      DO 940 I=1,N
      S=Y(I)
      DO 930 J=1,NP
      S=S-THETA(J)*X(I,J)
  930 CONTINUE
      RS(I)=S
  940 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE HYSESTW(X,Y,WGT,N,NP,NQ,NCOV,MDX,MDW,MDI,IOPT,INTCH,
     *           NREP,TOLS,TOLR,TAU,GAM,MAXIT,MAXS1,MAXS2,EXPSI,EXPSP,
     *           EXCHI,ISEED,IERR,SMIN,THETA,RS,IT1,COV,WORK,IWORK)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),WGT(N),THETA(N),RS(N),IT1(NQ),COV(NCOV)
      DIMENSION WORK(MDW),IWORK(MDI)
      EXTERNAL EXPSI,EXPSP,EXCHI
      COMMON/BETA/BETA,BET0
C
C   Resampling algorithm for the computation of S-estimates
C
      IMIN=NP+NQ
      NP1=NP+1
      MINW=NQ*(NP+2)+(MDX+3)*NP+2*N
      NN=NP*(NP+1)/2
      IF (N.LE.0 .OR. MDX.LT.N .OR. NP.LE.0 
     * .OR. NQ.LT.NP .OR. NCOV.NE.NN .OR. MDW.LT.MINW .OR. MDI.LT.IMIN
     * .OR. IOPT.LT.0 .OR. IOPT.GT.3 .OR. (IOPT.EQ.2 .AND. NREP.LE.0)
     * .OR. (INTCH.NE.0.AND.INTCH.NE.1) .OR. TOLS.LE.0. OR.
     *  TOLR.LE.0. .OR. TAU.LT.0. .OR. GAM.LE.0. .OR. GAM.GT.2. .OR.
     * MAXIT.LE.0 .OR. MAXS1.LE.0 .OR. MAXS2.LE.0)
     * CALL MESSGE(500,'HYSESTW',1)
      N0=NP*NQ+1
      N1=N0+NQ
      N2=N1+NQ
      N3=N2+NP
      N4=N3+NP
      N5=N4+NP
      N6=N5+MDX*NP
      N7=N6+N
      CALL HSESTW2(X,Y,WGT,N,NP,NQ,NCOV,MDX,IOPT,INTCH,NREP,TOLS,TOLR,
     *            TAU,GAM,MAXIT,MAXS1,MAXS2,EXPSI,EXPSP,EXCHI,
     *            ISEED,IERR,SMIN,THETA,RS,IT1,COV,
     *            WORK(1),WORK(N0),WORK(N1),WORK(N2),WORK(N3),WORK(N4),
     *            WORK(N5),WORK(N6),WORK(N7),IWORK(1),IWORK(NP1))
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE HSESTW2(X,Y,WGT,N,NP,NQ,NCOV,MDX,IOPT,INTCH,NREP,TOLS,
     *           TOLR,TAU,GAM,MAXIT,MAXS1,MAXS2,EXPSI,EXPSP,EXCHI,ISEED,
     *           IERR,SMIN,THETA,RS,IT1,COV,XX,YY,XTHETA,
     *           SF,SG,SH,SX,SZ,SW,SP,IT)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),WGT(N),THETA(N),RS(N),COV(NCOV),XX(NQ,NP)
      DIMENSION XTHETA(NQ),SF(NP),SG(NP),SH(NP),SX(MDX,NP),SZ(N),YY(NQ)
      DIMENSION SW(N)
      INTEGER IT1(NQ),SP(NP),IT(NQ)
      EXTERNAL EXPSI,EXPSP,EXCHI,ICNREP
      COMMON/BETA/BETA,BET0,/CONST/CONST
C
C   Resampling algorithm for the computation of S-estimates
C
      IF (N.LE.0 .OR. MDX.LT.N .OR. NP.LE.0
     * .OR. NQ.LT.NP .OR. NCOV.NE.NP*(NP+1)/2 .OR.
     *  IOPT.LT.0 .OR. IOPT.GT.3 .OR. (IOPT.EQ.2 .AND. NREP.LE.0)
     * .OR. (INTCH.NE.0.AND.INTCH.NE.1) .OR. TOLS.LE.0. OR.
     *  TOLR.LE.0. .OR. TAU.LT.0. .OR. GAM.LE.0. .OR. GAM.GT.2. .OR.
     * MAXIT.LE.0 .OR. MAXS1.LE.0 .OR. MAXS2.LE.0)
     * CALL MESSGE(500,'HSESTW2',1)
C
C STEP 0: INITIALIZATIONS
C ------
      N2=N/2
      N2P=N-N2
      K1=N2+1
      NK1=N-K1+1
      CONST=BETA*FLOAT(N-NP)
      IF (IOPT.NE.2) NREP=ICNREP(N,NQ,IOPT,0)
      NIT=1
      IERR=2
      SMIN=0.
      ITYPE=2
      NITMON=0
      PSP0=EXPSP(0.)
C
C STEP 1: DRAW A SUBSAMPLE
C ------ 
c     irep=0 
  100 IF (IOPT.NE.3) THEN
        DO 130 K=1,NQ
  110     CALL RANDOW(ISEED,RND)
c         irep=irep+1
c         ver(irep)=rnd 
c         IF (irep.eq.5) then
c           call realpr('rnd',3,ver,5)
c           irep=0
c         endif
          ITK=INT(RND*FLOAT(N))+1
          DO 120 KK=1,K-1
          IF (ITK.EQ.IT(KK)) GOTO 110
  120     CONTINUE
          IT(K)=ITK
  130   CONTINUE
      ELSE
        IF (NIT.EQ.1) THEN
          DO 140 K=1,NQ
          IT(K)=K
  140     CONTINUE
        ELSE
          CALL  NCOMB(N,NQ,IT)
        ENDIF
      ENDIF
      DO 160 K=1,NQ
      ITK=IT(K)
      DO 150 J=1,NP
      XX(K,J)=X(ITK,J)
  150 CONTINUE
      YY(K)=Y(ITK)
  160 CONTINUE
C
C STEP 2: DECOMPOSE SAMPLE MATRIX
C -------
      CALL RIMTRFZ(XX,NQ,NP,NQ,INTCH,TAU,KK,SF,SG,SH,SP)
      IF(KK.NE.NP) GOTO 700
C
C STEP 3: SOLVE SYSTEM OF LINEAR EQUATIONS
C -------
      CALL RICLL1(XX,YY,NQ,NP,NQ,XTHETA,SH,SP)
C
C STEP 4: COMPUTE RESIDUALS
C -------
      DO 420 I=1,N
      S=Y(I)
      DO 410 J=1,NP
      S=S-XTHETA(J)*X(I,J)
  410 CONTINUE
      RS(I)=S
  420 CONTINUE
      IF (SMIN.EQ.0.) THEN
        S=1.0E7
        DO 430 I=1,N
        ARI=ABS(RS(I))
        SZ(I)=ARI
        IF (ARI.NE.0.) S=AMIN1(S,ARI)
  430   CONTINUE
        IF (S.EQ.1.0E7) GOTO 915
        CALL FSTORDZ(SZ,N,K1,S0)
        S0=2.*S0
        IF (S0.EQ.0.) S0=S
        SRES=S0
      ENDIF
  435 D=0.
      DO 440 I=1,N
      D=D+WGT(I)*EXCHI(RS(I)/SRES)
  440 CONTINUE
      IF (SMIN.NE.0..AND.D.GT.CONST) GOTO 700
      IF (D.LE.CONST) GOTO 500
      S0=1.5*S0
      SRES=S0
      GOTO 435
C
C STEP 5: SOLVE FOR SRES
C -------
  500 CALL RYSIGM(RS,WGT,EXCHI,S0,N,NP,TOLR,ITYPE,1,MAXS1,NIS,SRES,
     +     SW,SZ)
      IF (NIS.EQ.MAXS1) CALL MESSGE(110,'HSESTW2',0)
C
C STEP 6: UPDATE BEST FIT
C ------
      IERR=0
      SMIN=SRES
      S0=SMIN
      DO 610 K=1,NP
      THETA(K)=XTHETA(K)
  610 CONTINUE
      DO 620 K=1,NQ
      IT1(K)=IT(K)
  620 CONTINUE
      IF (SRES .LE. TOLS) THEN
        IERR=1
        GOTO 800
      ENDIF
C
C STEP 7: END OF MAIN LOOP
C -------
  700 IF (NIT.EQ.NREP) GOTO 800
      NIT=NIT+1
      GOTO 100
C
C STEP 8: SOLVE SYSTEM OF EQUATIONS FOR THETA AND SRES
C --------
  800 IF (IERR.EQ.2) RETURN
      DO 820 I=1,N
      S=Y(I)
      DO 810 J=1,NP
      S=S-THETA(J)*X(I,J)
  810 CONTINUE
      RS(I)=S
  820 CONTINUE
      K=1
      MAXIW=1
      ISIGMA=-1
  830 SWI=0.
      DO 860 I=1,N
      WI=0.
      IF (RS(I).EQ.0.) GOTO 840
      T=RS(I)/SMIN
      WI=EXPSI(T)/T
      SWI=SWI+WI
      WI=SQRT(WI)
  840 DO 850 J=1,NP
      SX(I,J)=WI*X(I,J)
  850 CONTINUE
  860 CONTINUE
      CALL KFFACV(RS,EXPSI,EXPSP,N,NP,SMIN,FH)
      FACT=FH*SWI/FLOAT(N)
      IF (K.EQ.0) FACT=FACT*SMIN*SMIN
      CALL KTASKVZ(SX,N,NP,MDX,NCOV,TAU,FACT,XX,COV)
      IF (K.EQ.0) RETURN
      SRES=SMIN
      ICNV=1
      DO 870 J=1,NP
      XTHETA(J)=THETA(J)
  870 CONTINUE
      IF (MAXIW.EQ.1) CALL QRSSHW(RS,WGT,EXCHI,N,NP,SRES,QR0)
  880  CALL RYWALG(X,Y,THETA,WGT,COV,PSP0,EXPSI,EXCHI,EXCHI,SRES,
     * N,NP,MDX,MDX,NCOV,TOLR,GAM,TAU,ITYPE,ISIGMA,ICNV,MAXIW,
     * MAXS2,NITMON,NIT8,SMIN,RS,YY,SZ,SF,SG,SH,SP,SW,SX)
C
C STEP 9: EXIT
C -------
      IF (MAXIW.EQ.1) THEN
        CALL QRSSHW(RS,WGT,EXCHI,N,NP,SRES,QR1)
        IF (QR0.LE.QR1) GOTO 910
        ISIGMA=1
        MAXIW=MAXIW+MAXIT
        GOTO 880
      ENDIF
      IF (NIT8.EQ.MAXIW) CALL MESSGE(111,'HSESTW2',0)
      IF (SMIN.LT.SRES) THEN
        K=0
        GOTO 830
      ENDIF
  910 CALL MESSGE(112,'HSESTW2',0)
      SMIN=SRES
      FACT=SMIN*SMIN
      CALL SCALZ(COV,FACT,NCOV,1,NCOV)
  915 DO 920 J=1,NP
      THETA(J)=XTHETA(J)
  920 CONTINUE
      DO 940 I=1,N
      S=Y(I)
      DO 930 J=1,NP
      S=S-THETA(J)*X(I,J)
  930 CONTINUE
      RS(I)=S
  940 CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
C
C                 R O B E T H  Interface to S _ P L U S
C
C  File INTREX.F  Interface for ROBETH routines with external functions
C                 in the parameter list (FORTRAN code)
C
C-----------------------------------------------------------------------
      subroutine int0(expsi,exu,exw,itype,mu,ialfa,sigmx,upper,til,
     x                maxit,tol,nit,alfa,beta,reff)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsi,exu,exw,itype,mu,ialfa,maxit,nit
       real sigmx,upper,til,tol,alfa,beta,reff
       external psy,userfs
       if (expsi.eq.1) then
         call int1(psy,exu,exw,itype,mu,ialfa,sigmx,upper,til,
     x             maxit,tol,nit,alfa,beta,reff)
       else
         call int1(userfs,exu,exw,itype,mu,ialfa,sigmx,upper,til,
     x             maxit,tol,nit,alfa,beta,reff)
       endif
       return
      end
      subroutine int1(expsi,exu,exw,itype,mu,ialfa,sigmx,upper,til,
     x                 maxit,tol,nit,alfa,beta,reff)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exu,exw,itype,mu,ialfa,maxit,nit
       real sigmx,upper,til,tol,alfa,beta,reff
       double precision ucv,userfd
       external expsi,ucv,userfd
       if (exu.eq.5)then
         call intrf2(expsi,ucv,exw,itype,mu,ialfa,sigmx,upper,til,
     x             maxit,tol,nit,alfa,beta,reff)
       else
         call intrf2(expsi,userfd,exw,itype,mu,ialfa,sigmx,upper,til,
     x             maxit,tol,nit,alfa,beta,reff)
       endif
       return
      end
      subroutine intrf2(expsi,exu,exw,itype,mu,ialfa,sigmx,upper,til,
     x                maxit,tol,nit,alfa,beta,reff)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exw,itype,mu,ialfa,maxit,nit
       real til,sigmx,upper,tol,alfa,beta,reff
       double precision exu,www,userfd
       external expsi,exu,www,userfd
       if (exw.eq.11) then
         call airef0(expsi,exu,www,itype,mu,ialfa,sigmx,upper,til,
     x               maxit,tol,nit,alfa,beta,reff)
       else
         call airef0(expsi,exu,userfd,itype,mu,ialfa,sigmx,upper,til,
     x               maxit,tol,nit,alfa,beta,reff)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int3(t,expsi,exu,exw,itype,nu,mu,sigmx,upper,til,
     x                tau,nobs,ncov,mdx,mdz,maxit,tol,init,nitmon,nit,
     x                beta,reff,a,sa,su1,sa0,sd,ss,wgt,dl,el,sz)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsi,exu,exw,itype,nu,mu,nobs,ncov,mdx,mdz,maxit
       integer init,nitmon,nit
       real t(mdx,nu),sigmx,upper,til,tau,tol,beta,reff,ss(5*ncov),
     x     wgt(nobs),dl(nobs),el(nobs),sz(mdz,nu)
       double precision a(ncov),sa(ncov),su1(ncov),sa0(ncov),sd(nu)
       external psy,userfs
       if (expsi.eq.1) then
         call int4(t,psy,exu,exw,itype,nu,mu,sigmx,upper,til,
     x        tau,nobs,ncov,mdx,mdz,maxit,tol,init,nitmon,nit,
     x        beta,reff,a,sa,su1,sa0,sd,ss,wgt,dl,el,sz)
       else
         call int4(t,userfs,exu,exw,itype,nu,mu,sigmx,upper,til,
     x        tau,nobs,ncov,mdx,mdz,maxit,tol,init,nitmon,nit,
     x        beta,reff,a,sa,su1,sa0,sd,ss,wgt,dl,el,sz)
       endif
       return
      end
      subroutine int4(t,expsi,exu,exw,itype,nu,mu,sigmx,upper,til,
     x                tau,nobs,ncov,mdx,mdz,maxit,tol,init,nitmon,nit,
     x                beta,reff,a,sa,su1,sa0,sd,ss,wgt,dl,el,sz)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exu,exw,itype,nu,mu,nobs,ncov,mdx,mdz,maxit,init
       integer nitmon,nit
       real t(mdx,nu),sigmx,upper,til,tau,tol,beta,reff,ss(5*ncov),
     x     wgt(nobs),dl(nobs),el(nobs),sz(mdz,nu)
       double precision a(ncov),sa(ncov),su1(ncov),sa0(ncov),sd(nu),
     x     ucv,userfd
       external expsi,ucv,userfd
       if (exu.eq.5) then
          call int5(t,expsi,ucv,exw,itype,nu,mu,sigmx,upper,til,
     x         tau,nobs,ncov,mdx,mdz,maxit,tol,init,nitmon,nit,
     x         beta,reff,a,sa,su1,sa0,sd,ss,wgt,dl,el,sz)
       else
          call int5(t,expsi,userfd,exw,itype,nu,mu,sigmx,upper,til,
     x         tau,nobs,ncov,mdx,mdz,maxit,tol,init,nitmon,nit,
     x         beta,reff,a,sa,su1,sa0,sd,ss,wgt,dl,el,sz)
       endif
       return
      end
      subroutine int5(t,expsi,exu,exw,itype,nu,mu,sigmx,upper,til,
     x                tau,nobs,ncov,mdx,mdz,maxit,tol,init,nitmon,nit,
     x                beta,reff,a,sa,su1,sa0,sd,ss,wgt,dl,el,sz)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exw,itype,nu,mu,nobs,ncov,mdx,mdz,maxit,init,nitmon,nit
       real t(mdx,nu),sigmx,upper,til,tau,tol,beta,reff,ss(5*ncov),
     x     wgt(nobs),dl(nobs),el(nobs),sz(mdz,nu)
       double precision a(ncov),sa(ncov),su1(ncov),sa0(ncov),sd(nu),
     x     exu,www,userfd
       external expsi,exu,www,userfd
       if (exw.eq.11) then
          call airefq(t,expsi,exu,www,itype,nu,mu,sigmx,upper,til,
     x         tau,nobs,ncov,mdx,mdz,maxit,tol,init,nitmon,nit,
     x         beta,reff,a,sa,su1,sa0,sd,ss,wgt,dl,el,sz)
       else
          call airefq(t,expsi,exu,userfd,itype,nu,mu,sigmx,upper,til,
     x         tau,nobs,ncov,mdx,mdz,maxit,tol,init,nitmon,nit,
     x         beta,reff,a,sa,su1,sa0,sd,ss,wgt,dl,el,sz)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int6(f,y,a,b,tol,maxit,x,iterm)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer f,maxit,iterm
       real y,a,b,tol,x
       external userfs
       if (f.eq.13) then
         call rgfl(userfs,y,a,b,tol,maxit,x,iterm)
       else
         maxit=0
         x=-9999.
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int7(x,a,t,exu,exv,exw,nobs,nvar,ncov,mdx,tau,maxit,
     x                nitmon,iloc,icnv,tol,nit,dist,sa,st,sr,sd)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exu,exv,exw,nobs,nvar,ncov,mdx,maxit,nitmon,iloc,icnv,nit
       real tol,dist(nobs),x(mdx,nvar),t(nvar),tau
       double precision a(ncov),sa(ncov),st(ncov),sr(nvar),sd(nvar),
     x        ucv,userfd
       external ucv,userfd
       if (exu.eq.5) then
         call intrf8(x,a,t,ucv,exv,exw,nobs,nvar,ncov,mdx,tau,maxit,
     x             nitmon,iloc,icnv,tol,nit,dist,sa,st,sr,sd)
       else
         call intrf8(x,a,t,userfd,exv,exw,nobs,nvar,ncov,mdx,tau,maxit,
     x             nitmon,iloc,icnv,tol,nit,dist,sa,st,sr,sd)
       endif
       return
      end
      subroutine intrf8(x,a,t,exu,exv,exw,nobs,nvar,ncov,mdx,tau,maxit,
     x                nitmon,iloc,icnv,tol,nit,dist,sa,st,sr,sd)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exv,exw,nobs,nvar,ncov,mdx,maxit,nitmon,iloc,icnv,nit
       real tol,dist(nobs),x(mdx,nvar),t(nvar),tau
       double precision a(ncov),sa(ncov),st(ncov),sr(nvar),sd(nvar),
     x        exu,vcv,userfd
       external exu,vcv,userfd
       if (exv.eq.7) then
         call int9(x,a,t,exu,vcv,exw,nobs,nvar,ncov,mdx,tau,maxit,
     x             nitmon,iloc,icnv,tol,nit,dist,sa,st,sr,sd)
       else
         call int9(x,a,t,exu,userfd,exw,nobs,nvar,ncov,mdx,tau,maxit,
     x             nitmon,iloc,icnv,tol,nit,dist,sa,st,sr,sd)
       endif
       return
      end
      subroutine int9(x,a,t,exu,exv,exw,nobs,nvar,ncov,mdx,tau,maxit,
     x                nitmon,iloc,icnv,tol,nit,dist,sa,st,sr,sd)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exw,nobs,nvar,ncov,mdx,maxit,nitmon,iloc,icnv,nit
       real tol,dist(nobs),x(mdx,nvar),t(nvar),tau
       double precision a(ncov),sa(ncov),st(ncov),sr(nvar),sd(nvar),
     x        exu,exv,wcv,userfd
       external exu,exv,wcv,userfd
       if (exw.eq.9) then
         call cyfalg(x,a,t,exu,exv,wcv,nobs,nvar,ncov,mdx,tau,maxit,
     x        nitmon,iloc,icnv,tol,nit,dist,sa,st,sr,sd)
       else
         call cyfalg(x,a,t,exu,exv,userfd,nobs,nvar,ncov,mdx,tau,
     x        maxit,nitmon,iloc,icnv,tol,nit,dist,sa,st,sr,sd)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int10(x,a,t,exu,exup,exv,exvp,exw,exwp,nobs,
     x           nvar,ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,
     x           nit,dist,sa,ss,su,sup,st,sd)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exu,exup,exv,exvp,exw,exwp,nobs,nvar,ncov,mdx
       integer maxit,nitmon,iloc,icnv,nit
       real x(mdx,nvar),t(nvar),tol,xfud,dist(nobs)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        st(ncov),sd(nvar),ucv,userfd
       external ucv,userfd
       if (exu.eq.5) then
         call int11(x,a,t,ucv,exup,exv,exvp,exw,exwp,nobs,nvar,ncov,
     x              mdx,maxit,nitmon,iloc,icnv,tol,xfud,nit,dist,
     x              sa,ss,su,sup,st,sd)
       else
         call int11(x,a,t,userfd,exup,exv,exvp,exw,exwp,nobs,nvar,ncov,
     x              mdx,maxit,nitmon,iloc,icnv,tol,xfud,nit,dist,
     x              sa,ss,su,sup,st,sd)
       endif
       return
      end
      subroutine int11(x,a,t,exu,exup,exv,exvp,exw,exwp,nobs,
     x           nvar,ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,
     x           nit,dist,sa,ss,su,sup,st,sd)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exup,exv,exvp,exw,exwp,nobs,nvar,ncov,mdx,maxit
       integer nitmon,iloc,icnv,nit
       real x(mdx,nvar),t(nvar),tol,xfud,dist(nobs)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        st(ncov),sd(nvar),exu,upcv,userfd
       external exu,upcv,userfd
       if (exup.eq.6) then
         call int12(x,a,t,exu,upcv,exv,exvp,exw,exwp,nobs,nvar,
     x              ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,su,sup,st,sd)
       else
         call int12(x,a,t,exu,userfd,exv,exvp,exw,exwp,nobs,nvar,
     x              ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,su,sup,st,sd)
       endif
       return
      end
      subroutine int12(x,a,t,exu,exup,exv,exvp,exw,exwp,nobs,
     x           nvar,ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,
     x           nit,dist,sa,ss,su,sup,st,sd)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exv,exvp,exw,exwp,nobs,nvar,ncov,mdx,maxit,nitmon
       integer iloc,icnv,nit
       real x(mdx,nvar),t(nvar),tol,xfud,dist(nobs)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        st(ncov),sd(nvar),exu,exup,vcv,userfd
       external exu,exup,vcv,userfd
       if (exv.eq.7) then
         call int13(x,a,t,exu,exup,vcv,exvp,exw,exwp,nobs,nvar,
     x              ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,su,sup,st,sd)
       else
         call int13(x,a,t,exu,exup,userfd,exvp,exw,exwp,nobs,nvar,
     x              ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,su,sup,st,sd)
       endif
       return
      end
      subroutine int13(x,a,t,exu,exup,exv,exvp,exw,exwp,nobs,
     x           nvar,ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,
     x           nit,dist,sa,ss,su,sup,st,sd)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exvp,exw,exwp,nobs,nvar,ncov,mdx,maxit,nitmon
       integer iloc,icnv,nit
       real x(mdx,nvar),t(nvar),tol,xfud,dist(nobs)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        st(ncov),sd(nvar),exu,exup,exv,vpcv,userfd
       external exu,exup,exv,vpcv,userfd
       if (exvp.eq.8) then
         call int14(x,a,t,exu,exup,exv,vpcv,exw,exwp,nobs,nvar,
     x              ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,su,sup,st,sd)
       else
         call int14(x,a,t,exu,exup,exv,userfd,exw,exwp,nobs,nvar,
     x              ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,su,sup,st,sd)
       endif
       return
      end
      subroutine int14(x,a,t,exu,exup,exv,exvp,exw,exwp,nobs,
     x           nvar,ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,
     x           nit,dist,sa,ss,su,sup,st,sd)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exw,exwp,nobs,nvar,ncov,mdx,maxit,nitmon,iloc,icnv,nit
       real x(mdx,nvar),t(nvar),tol,xfud,dist(nobs)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        st(ncov),sd(nvar),exu,exup,exv,exvp,wcv,userfd
       external exu,exup,exv,exvp,wcv,userfd
       if (exw.eq.9) then
         call int15(x,a,t,exu,exup,exv,exvp,wcv,exwp,nobs,nvar,
     x              ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,su,sup,st,sd)
       else
         call int15(x,a,t,exu,exup,exv,exvp,userfd,exwp,nobs,nvar,
     x              ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,su,sup,st,sd)
       endif
       return
      end
      subroutine int15(x,a,t,exu,exup,exv,exvp,exw,exwp,nobs,
     x           nvar,ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,
     x           nit,dist,sa,ss,su,sup,st,sd)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exwp,nobs,nvar,ncov,mdx,maxit,nitmon,iloc,icnv,nit
       real x(mdx,nvar),t(nvar),tol,xfud,dist(nobs)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        st(ncov),sd(nvar),exu,exup,exv,exvp,exw,wpcv,userfd
       external exu,exup,exv,exvp,exw,wpcv,userfd
       if (exwp.eq.10) then
         call cynalg(x,a,t,exu,exup,exv,exvp,exw,wpcv,nobs,nvar,
     x               ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x               dist,sa,ss,su,sup,st,sd)
       else
         call cynalg(x,a,t,exu,exup,exv,exvp,exw,userfd,nobs,nvar,
     x               ncov,mdx,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x               dist,sa,ss,su,sup,st,sd)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int16(x,a,t,exu,exup,exv,exw,exwp,nobs,nvar,
     x           ncov,mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,
     x           nit,dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exu,exup,exv,exw,exwp,nobs,nvar,ncov,mdx,mdz,maxit
       integer nitmon,iloc,icnv,nit
       real x(mdx,nvar),t(nvar),tol,xfud,dist(nobs),sz(mdz,nvar)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        sy1(nvar),sy2(nvar),sd(nvar),st(ncov),st1(ncov),
     x        ucv,userfd
       external ucv,userfd
       if (exu.eq.5) then
         call int17(x,a,t,ucv,exup,exv,exw,exwp,nobs,nvar,ncov,
     x              mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
       else
         call int17(x,a,t,userfd,exup,exv,exw,exwp,nobs,nvar,ncov,
     x              mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
       endif
       return
      end
      subroutine int17(x,a,t,exu,exup,exv,exw,exwp,nobs,nvar,
     x           ncov,mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,
     x           nit,dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exup,exv,exw,exwp,nobs,nvar,ncov,mdx,mdz,maxit 
       integer nitmon,iloc,icnv,nit
       real x(mdx,nvar),t(nvar),tol,xfud,dist(nobs),sz(mdz,nvar)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        sy1(nvar),sy2(nvar),sd(nvar),st(ncov),st1(ncov),
     x        exu,upcv,userfd
       external exu,upcv,userfd
       if (exup.eq.6) then
         call int18(x,a,t,exu,upcv,exv,exw,exwp,nobs,nvar,ncov,
     x              mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
       else
         call int18(x,a,t,exu,userfd,exv,exw,exwp,nobs,nvar,ncov,
     x              mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
       endif
       return
      end
      subroutine int18(x,a,t,exu,exup,exv,exw,exwp,nobs,nvar,
     x           ncov,mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,
     x           nit,dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exv,exw,exwp,nobs,nvar,ncov,mdx,mdz,maxit,nitmon
       integer iloc,icnv,nit
       real x(mdx,nvar),t(nvar),tol,xfud,dist(nobs),sz(mdz,nvar)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        sy1(nvar),sy2(nvar),sd(nvar),st(ncov),st1(ncov),
     x        exu,exup,vcv,userfd
       external exu,exup,vcv,userfd
       if (exv.eq.7) then
         call int19(x,a,t,exu,exup,vcv,exw,exwp,nobs,nvar,ncov,
     x              mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
       else
         call int19(x,a,t,exu,exup,userfd,exw,exwp,nobs,nvar,ncov,
     x              mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
       endif
       return
      end
      subroutine int19(x,a,t,exu,exup,exv,exw,exwp,nobs,nvar,
     x           ncov,mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,
     x           nit,dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exw,exwp,nobs,nvar,ncov,mdx,mdz,maxit,nitmon,iloc
       integer icnv,nit
       real x(mdx,nvar),t(nvar),tol,xfud,dist(nobs),sz(mdz,nvar)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        sy1(nvar),sy2(nvar),sd(nvar),st(ncov),st1(ncov),
     x        exu,exup,exv,wcv,userfd
       external exu,exup,exv,wcv,userfd
       if (exw.eq.9) then
         call int20(x,a,t,exu,exup,exv,wcv,exwp,nobs,nvar,ncov,
     x              mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
       else
         call int20(x,a,t,exu,exup,exv,userfd,exwp,nobs,nvar,ncov,
     x              mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x              dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
       endif
       return
      end
      subroutine int20(x,a,t,exu,exup,exv,exw,exwp,nobs,nvar,
     x           ncov,mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,
     x           nit,dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exwp,nobs,nvar,ncov,mdx,mdz,maxit,nitmon,iloc
       integer icnv,nit
       real x(mdx,nvar),t(nvar),tol,xfud,dist(nobs),sz(mdz,nvar)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        sy1(nvar),sy2(nvar),sd(nvar),st(ncov),st1(ncov),
     x        exu,exup,exv,exw,wpcv,userfd
       external exu,exup,exv,exw,wpcv,userfd
       if (exwp.eq.10) then
         call cygalg(x,a,t,exu,exup,exv,exw,wpcv,nobs,nvar,ncov,
     x               mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x               dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
       else
         call cygalg(x,a,t,exu,exup,exv,exw,userfd,nobs,nvar,ncov,
     x               mdx,mdz,maxit,nitmon,iloc,icnv,tol,xfud,nit,
     x               dist,sa,ss,sz,su,sup,sy1,sy2,sd,st,st1)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int21(x,y,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x           tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,expsp,
     x           exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,maxit,maxs1
       integer maxs2,expsi,expsp,exchi,iseed,ierr,it1(nq),iwork(mdi)
       real x(mdx,np),y(n),tols,tolr,tau,gam,smin,theta(n),rs(n),
     x      cov(ncov),work(mdw)
       external psy,userfs
       if (expsi.eq.1) then
         call int22(x,y,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x              tols,tolr,tau,gam,maxit,maxs1,maxs2,psy,expsp,
     x              exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
       else
         call int22(x,y,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x              tols,tolr,tau,gam,maxit,maxs1,maxs2,userfs,expsp,
     x              exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
       endif
       return
      end
      subroutine int22(x,y,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x           tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,expsp,
     x           exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,maxit,maxs1
       integer maxs2,expsp,exchi,iseed,ierr,it1(nq),iwork(mdi)
       real x(mdx,np),y(n),tols,tolr,tau,gam,smin,theta(n),rs(n),
     x      cov(ncov),work(mdw)
       external expsi,psp,userfs
       if (expsp.eq.3) then
         call int23(x,y,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x              tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,psp,
     x              exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
       else
         call int23(x,y,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x              tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,userfs,
     x              exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
       endif
       return
      end
      subroutine int23(x,y,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x           tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,expsp,
     x           exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,maxit,maxs1
       integer maxs2,exchi,iseed,ierr,it1(nq),iwork(mdi)
       real x(mdx,np),y(n),tols,tolr,tau,gam,smin,theta(n),rs(n),
     x      cov(ncov),work(mdw)
       external expsi,expsp,chi,userfs
       if (exchi.eq.4) then
         call hysest(x,y,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x               tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,expsp,
     x               chi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
       else
         call hysest(x,y,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x               tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,expsp,
     x               userfs,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int21w(x,y,wgt,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,
     x           nrep,tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,expsp,
     x           exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS. Modified by A.R.
C.......................................................................
C
       integer n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,maxit,maxs1
       integer maxs2,expsi,expsp,exchi,iseed,ierr,it1(nq),iwork(mdi)
       real x(mdx,np),y(n),wgt(n),tols,tolr,tau,gam,smin,theta(n),
     x      rs(n),cov(ncov),work(mdw)
       external psy,userfs
       if (expsi.eq.1) then
         call int22w(x,y,wgt,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x              tols,tolr,tau,gam,maxit,maxs1,maxs2,psy,expsp,
     x              exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
       else
         call int22w(x,y,wgt,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x              tols,tolr,tau,gam,maxit,maxs1,maxs2,userfs,expsp,
     x              exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
       endif
       return
      end
      subroutine int22w(x,y,wgt,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,
     x           nrep,tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,expsp,
     x           exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS. Modified by A.R.
C.......................................................................
C
       integer n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,maxit,maxs1
       integer maxs2,expsp,exchi,iseed,ierr,it1(nq),iwork(mdi)
       real x(mdx,np),y(n),wgt(n),tols,tolr,tau,gam,smin,theta(n),
     x      rs(n),cov(ncov),work(mdw)
       external expsi,psp,userfs
       if (expsp.eq.3) then
         call int23w(x,y,wgt,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x              tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,psp,
     x              exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
       else
         call int23w(x,y,wgt,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x              tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,userfs,
     x              exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
       endif
       return
      end
      subroutine int23w(x,y,wgt,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,
     x           nrep,tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,expsp,
     x           exchi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS: Modified by A.R.
C.......................................................................
C
       integer n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,maxit,maxs1
       integer maxs2,exchi,iseed,ierr,it1(nq),iwork(mdi)
       real x(mdx,np),y(n),wgt(n),tols,tolr,tau,gam,smin,theta(n),
     x      rs(n),cov(ncov),work(mdw)
       external expsi,expsp,chi,userfs
       if (exchi.eq.4) then
         call hysestw(x,y,wgt,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x               tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,expsp,
     x               chi,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
       else
         call hysestw(x,y,wgt,n,np,nq,ncov,mdx,mdw,mdi,iopt,intch,nrep,
     x               tols,tolr,tau,gam,maxit,maxs1,maxs2,expsi,expsp,
     x               userfs,iseed,ierr,smin,theta,rs,it1,cov,work,iwork)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int24(wgt,expsi,n,itype,upper,til,errest,d,e)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsi,n,itype
       real wgt(n),upper,til,errest,d(n),e(n)
       external psy,userfs
       if (expsi.eq.1) then
         call kiedcu(wgt,psy,n,itype,upper,til,errest,d,e)
       else
         call kiedcu(wgt,userfs,n,itype,upper,til,errest,d,e)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int25(rs,expsi,expsp,n,np,sigma,fh)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsi,expsp,n,np
       real rs(n),sigma,fh
       external psy,userfs
       if (expsi.eq.1) then
         call int26(rs,psy,expsp,n,np,sigma,fh)
       else
         call int26(rs,userfs,expsp,n,np,sigma,fh)
       endif
       return
      end
      subroutine int26(rs,expsi,expsp,n,np,sigma,fh)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsp,n,np
       real rs(n),sigma,fh
       external expsi,psp,userfs
       if (expsp.eq.3) then
         call kffacv(rs,expsi,psp,n,np,sigma,fh)
       else
         call kffacv(rs,expsi,userfs,n,np,sigma,fh)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int27(wgt,rs,expsi,expsp,n,sigma,itype,d,e)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsi,expsp,n,itype
       real wgt(n),rs(n),sigma,d(n),e(n)
       external psy,userfs
       if (expsi.eq.1) then
         call int28(wgt,rs,psy,expsp,n,sigma,itype,d,e)
       else
         call int28(wgt,rs,userfs,expsp,n,sigma,itype,d,e)
       endif
       return
      end
      subroutine int28(wgt,rs,expsi,expsp,n,sigma,itype,d,e)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsp,n,itype
       real wgt(n),rs(n),sigma,d(n),e(n)
       external expsi,psp,userfs
       if (expsp.eq.3) then
         call kfedcc(wgt,rs,expsi,psp,n,sigma,itype,d,e)
       else
         call kfedcc(wgt,rs,expsi,userfs,n,sigma,itype,d,e)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int29(wgt,rs,expsi,expsp,n,sigma,itype,d,e)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsi,expsp,n,itype
       real wgt(n),rs(n),sigma,d(n),e(n)
       external psy,userfs
       if (expsi.eq.1) then
         call int30(wgt,rs,psy,expsp,n,sigma,itype,d,e)
       else
         call int30(wgt,rs,userfs,expsp,n,sigma,itype,d,e)
       endif
       return
      end
      subroutine int30(wgt,rs,expsi,expsp,n,sigma,itype,d,e)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsp,n,itype
       real wgt(n),rs(n),sigma,d(n),e(n)
       external expsi,psp,userfs
       if (expsp.eq.3) then
         call kfedcb(wgt,rs,expsi,psp,n,sigma,itype,d,e)
       else
         call kfedcb(wgt,rs,expsi,userfs,n,sigma,itype,d,e)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int31(exchi,upper,til,errest,bta)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exchi
       real upper,til,errest,bta
       external chi,userfs
       if (exchi.eq.4) then
         call libetu(chi,upper,til,errest,bta)
       else
         call libetu(userfs,upper,til,errest,bta)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int32(expsi,upper,til,errest,epsi2,epsip)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsi
       real upper,til,errest,epsi2,epsip
       external psy,userfs
       if (expsi.eq.1) then
         call liepsu(psy,upper,til,errest,epsi2,epsip)
       else
         call liepsu(userfs,upper,til,errest,epsi2,epsip)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int33(y,expsi,expsp,exchi,theta,sigmai,n,tol,gam,
     x                 isigma,maxit,maxis,nit,sigmaf,var,rs,sc)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsi,expsp,exchi,n,isigma,maxit,maxis,nit
       real y(n),theta,sigmai,tol,gam,sigmaf,var,rs(n),sc(n)
       external psy,userfs
       if (expsi.eq.1) then
         call int34(y,psy,expsp,exchi,theta,sigmai,n,tol,gam,isigma,
     x              maxit,maxis,nit,sigmaf,var,rs,sc)
       else
         call int34(y,userfs,expsp,exchi,theta,sigmai,n,tol,gam,isigma,
     x              maxit,maxis,nit,sigmaf,var,rs,sc)
       endif
       return
      end
      subroutine int34(y,expsi,expsp,exchi,theta,sigmai,n,tol,gam,
     x                 isigma,maxit,maxis,nit,sigmaf,var,rs,sc)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsp,exchi,n,isigma,maxit,maxis,nit
       real y(n),theta,sigmai,tol,gam,sigmaf,var,rs(n),sc(n)
       external expsi,psp,userfs
       if (expsp.eq.3) then
         call int35(y,expsi,psp,exchi,theta,sigmai,n,tol,gam,isigma,
     x              maxit,maxis,nit,sigmaf,var,rs,sc)
       else
         call int35(y,expsi,userfs,exchi,theta,sigmai,n,tol,gam,isigma,
     x              maxit,maxis,nit,sigmaf,var,rs,sc)
       endif
       return
      end
      subroutine int35(y,expsi,expsp,exchi,theta,sigmai,n,tol,gam,
     x                 isigma,maxit,maxis,nit,sigmaf,var,rs,sc)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exchi,n,isigma,maxit,maxis,nit
       real y(n),theta,sigmai,tol,gam,sigmaf,var,rs(n),sc(n)
       external expsi,expsp,chi,userfs
       if (exchi.eq.4) then
         call lyhalg(y,expsi,expsp,chi,theta,sigmai,n,tol,gam,isigma,
     x               maxit,maxis,nit,sigmaf,var,rs,sc)
       else
         call lyhalg(y,expsi,expsp,userfs,theta,sigmai,n,tol,gam,isigma,
     x               maxit,maxis,nit,sigmaf,var,rs,sc)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int36(z,expsi,expsp,exchi,exrho,m,n,mpn,tol,gam,
     x                 isigma,maxit,nitmon,thetal,deltal,thetas,
     x                 sigmaf,ftau,p,rs1,rs2,cov,work1,work2,iwork)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsi,expsp,exchi,exrho,m,n,mpn,isigma,maxit,iwork(2)
       real z(mpn),tol,gam,thetal,deltal,thetas,sigmaf,ftau,p,rs1(mpn),
     x      rs2(mpn),cov(3),work1(mpn,6),work2(8)
       external psy,userfs
       if (expsi.eq.1) then
         call int37(z,psy,expsp,exchi,exrho,m,n,mpn,tol,gam,isigma,
     x              maxit,nitmon,thetal,deltal,thetas,sigmaf,ftau,p,
     x              rs1,rs2,cov,work1,work2,iwork)
       else
         call int37(z,userfs,expsp,exchi,exrho,m,n,mpn,tol,gam,isigma,
     x              maxit,nitmon,thetal,deltal,thetas,sigmaf,ftau,p,
     x              rs1,rs2,cov,work1,work2,iwork)
       endif
       return
      end
      subroutine int37(z,expsi,expsp,exchi,exrho,m,n,mpn,tol,gam,
     x                 isigma,maxit,nitmon,thetal,deltal,thetas,
     x                 sigmaf,ftau,p,rs1,rs2,cov,work1,work2,iwork)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsp,exchi,exrho,m,n,mpn,isigma,maxit,iwork(2)
       real z(mpn),tol,gam,thetal,deltal,thetas,sigmaf,ftau,p,rs1(mpn),
     x      rs2(mpn),cov(3),work1(mpn,6),work2(8)
       external expsi,psp,userfs
       if (expsp.eq.3) then
         call int38(z,expsi,psp,exchi,exrho,m,n,mpn,tol,gam,
     x              isigma,maxit,nitmon,thetal,deltal,thetas,sigmaf,
     x              ftau,p,rs1,rs2,cov,work1,work2,iwork)
       else
         call int38(z,expsi,userfs,exchi,exrho,m,n,mpn,tol,gam,
     x              isigma,maxit,nitmon,thetal,deltal,thetas,sigmaf,
     x              ftau,p,rs1,rs2,cov,work1,work2,iwork)
       endif
       return
      end
      subroutine int38(z,expsi,expsp,exchi,exrho,m,n,mpn,tol,gam,
     x                 isigma,maxit,nitmon,thetal,deltal,thetas,
     x                 sigmaf,ftau,p,rs1,rs2,cov,work1,work2,iwork)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exchi,exrho,m,n,mpn,isigma,maxit,iwork(2)
       real z(mpn),tol,gam,thetal,deltal,thetas,sigmaf,ftau,p,rs1(mpn),
     x      rs2(mpn),cov(3),work1(mpn,6),work2(8)
       external expsi,expsp,chi,userfs
       if (exchi.eq.4) then
         call int39(z,expsi,expsp,chi,exrho,m,n,mpn,tol,gam,
     x              isigma,maxit,nitmon,thetal,deltal,thetas,sigmaf,
     x              ftau,p,rs1,rs2,cov,work1,work2,iwork)
       else
         call int39(z,expsi,expsp,userfs,exrho,m,n,mpn,tol,gam,
     x              isigma,maxit,nitmon,thetal,deltal,thetas,sigmaf,
     x              ftau,p,rs1,rs2,cov,work1,work2,iwork)
       endif
       return
      end
      subroutine int39(z,expsi,expsp,exchi,exrho,m,n,mpn,tol,gam,
     x                 isigma,maxit,nitmon,thetal,deltal,thetas,
     x                 sigmaf,ftau,p,rs1,rs2,cov,work1,work2,iwork)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exrho,m,n,mpn,isigma,maxit,iwork(2)
       real z(mpn),tol,gam,thetal,deltal,thetas,sigmaf,ftau,p,rs1(mpn),
     x      rs2(mpn),cov(3),work1(mpn,6),work2(8)
       external expsi,expsp,exchi,rho,userfs
       if (exrho.eq.2) then
         call lytau2(z,expsi,expsp,exchi,rho,m,n,mpn,tol,gam,
     x               isigma,maxit,nitmon,thetal,deltal,thetas,sigmaf,
     x               ftau,p,rs1,rs2,cov,work1,work2,iwork)
       else
         call lytau2(z,expsi,expsp,exchi,userfs,m,n,mpn,tol,gam,
     x               isigma,maxit,nitmon,thetal,deltal,thetas,sigmaf,
     x               ftau,p,rs1,rs2,cov,work1,work2,iwork)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int40(wgt,exchi,n,itype,upper,til,errest,bta)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exchi,n,itype
       real wgt(n),upper,til,errest,bta
       external chi,userfs
       if (exchi.eq.4) then
         call ribetu(wgt,chi,n,itype,upper,til,errest,bta)
       else
         call ribetu(wgt,userfs,n,itype,upper,til,errest,bta)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int41(x,y,theta,wgt,cov,expsi,exchi,exrho,sigmai,n,
     x                 np,mdx,mdt,ncov,k,tol,gam,tau,itype,ix,iy,ic,
     x                 isigma,icnv,maxit,maxis,nitmon,nit,sigmaf,rs1,
     x                 rs2,delta,sc,se,sf,sg,sh,ip)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsi,exchi,exrho,n,np,mdx,mdt,ncov,k,itype,ix,iy,ic
       integer isigma,icnv,maxit,maxis,nitmon,nit,ip(np)
       real x(mdx,np),y(n),theta(mdt),wgt(n),cov(ncov),rs1(n),rs2(n),
     x      delta(np),sc(n),se(np),sf(np),sg(np),sh(np)
       external psy,userfs
       if (expsi.eq.1) then
         call int42(x,y,theta,wgt,cov,psy,exchi,exrho,sigmai,n,np,
     x              mdx,mdt,ncov,k,tol,gam,tau,itype,ix,iy,ic,isigma,
     x              icnv,maxit,maxis,nitmon,nit,sigmaf,rs1,rs2,delta,
     x              sc,se,sf,sg,sh,ip)
       else
         call int42(x,y,theta,wgt,cov,userfs,exchi,exrho,sigmai,n,np,
     x              mdx,mdt,ncov,k,tol,gam,tau,itype,ix,iy,ic,isigma,
     x              icnv,maxit,maxis,nitmon,nit,sigmaf,rs1,rs2,delta,
     x              sc,se,sf,sg,sh,ip)
       endif
       return
      end
      subroutine int42(x,y,theta,wgt,cov,expsi,exchi,exrho,sigmai,n,
     x                 np,mdx,mdt,ncov,k,tol,gam,tau,itype,ix,iy,ic,
     x                 isigma,icnv,maxit,maxis,nitmon,nit,sigmaf,rs1,
     x                 rs2,delta,sc,se,sf,sg,sh,ip)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exchi,exrho,n,np,mdx,mdt,ncov,k,itype,ix,iy,ic,isigma
       integer icnv,maxit,maxis,nitmon,nit,ip(np)
       real x(mdx,np),y(n),theta(mdt),wgt(n),cov(ncov),rs1(n),rs2(n),
     x      delta(np),sc(n),se(np),sf(np),sg(np),sh(np)
       external expsi,chi,userfs
       if (exchi.eq.4) then
         call int43(x,y,theta,wgt,cov,expsi,chi,exrho,sigmai,n,np,
     x              mdx,mdt,ncov,k,tol,gam,tau,itype,ix,iy,ic,isigma,
     x              icnv,maxit,maxis,nitmon,nit,sigmaf,rs1,rs2,delta,
     x              sc,se,sf,sg,sh,ip)
       else
         call int43(x,y,theta,wgt,cov,expsi,userfs,exrho,sigmai,n,np,
     x              mdx,mdt,ncov,k,tol,gam,tau,itype,ix,iy,ic,isigma,
     x              icnv,maxit,maxis,nitmon,nit,sigmaf,rs1,rs2,delta,
     x              sc,se,sf,sg,sh,ip)
       endif
       return
      end
      subroutine int43(x,y,theta,wgt,cov,expsi,exchi,exrho,sigmai,n,
     x                 np,mdx,mdt,ncov,k,tol,gam,tau,itype,ix,iy,ic,
     x                 isigma,icnv,maxit,maxis,nitmon,nit,sigmaf,rs1,
     x                 rs2,delta,sc,se,sf,sg,sh,ip)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exrho,n,np,mdx,mdt,ncov,itype,ix,iy,ic,isigma,icnv
       integer maxit,maxis,nitmon,nit,k,ip(np)
       real x(mdx,np),y(n),theta(mdt),wgt(n),cov(ncov),rs1(n),rs2(n),
     x      delta(np),sc(n),se(np),sf(np),sg(np),sh(np)
       external expsi,exchi,rho,userfs
       if (exrho.eq.2) then
         call ryhalg(x,y,theta,wgt,cov,expsi,exchi,rho,sigmai,n,np,
     x               mdx,mdt,ncov,k,tol,gam,tau,itype,ix,iy,ic,isigma,
     x               icnv,maxit,maxis,nitmon,nit,sigmaf,rs1,rs2,delta,
     x               sc,se,sf,sg,sh,ip)
       else
         call ryhalg(x,y,theta,wgt,cov,expsi,exchi,userfs,sigmai,n,np,
     x               mdx,mdt,ncov,k,tol,gam,tau,itype,ix,iy,ic,isigma,
     x               icnv,maxit,maxis,nitmon,nit,sigmaf,rs1,rs2,delta,
     x               sc,se,sf,sg,sh,ip)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int44(x,y,theta,wgt,cov,psp0,expsi,exchi,exrho,sigmai,
     x                 n,np,mdx,mdt,ncov,tol,gam,tau,itype,isigma,icnv,
     x                 maxit,maxis,nitmon,nit,sigmaf,rs,delta,sc,sf,
     x                 sg,sh,ip,sw,sx)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsi,exchi,exrho,n,np,mdx,mdt,ncov,itype,isigma,icnv
       integer maxit,maxis,nitmon,nit,ip(np)
       real x(mdx,np),y(n),theta(mdt),wgt(n),cov(ncov),rs(n),delta(np)
       real sc(n),sf(np),sg(np),sh(np),sw(n),sx(mdx,np)
       external psy,userfs
       if (expsi.eq.1) then
         call int45(x,y,theta,wgt,cov,psp0,psy,exchi,exrho,sigmai,
     x              n,np,mdx,mdt,ncov,tol,gam,tau,itype,isigma,
     x              icnv,maxit,maxis,nitmon,nit,sigmaf,rs,delta,
     x              sc,sf,sg,sh,ip,sw,sx)
       else
         call int45(x,y,theta,wgt,cov,psp0,userfs,exchi,exrho,sigmai,
     x              n,np,mdx,mdt,ncov,tol,gam,tau,itype,isigma,
     x              icnv,maxit,maxis,nitmon,nit,sigmaf,rs,delta,
     x              sc,sf,sg,sh,ip,sw,sx)
       endif
       return
      end
      subroutine int45(x,y,theta,wgt,cov,psp0,expsi,exchi,exrho,sigmai,
     x                 n,np,mdx,mdt,ncov,tol,gam,tau,itype,isigma,icnv,
     x                 maxit,maxis,nitmon,nit,sigmaf,rs,delta,sc,sf,
     x                 sg,sh,ip,sw,sx)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exchi,exrho,n,np,mdx,mdt,ncov,itype,isigma,icnv,maxit
       integer maxis,nitmon,nit,ip(np)
       real x(mdx,np),y(n),theta(mdt),wgt(n),cov(ncov),rs(n),delta(np)
       real sc(n),sf(np),sg(np),sh(np),sw(n),sx(mdx,np)
       external expsi,chi,userfs
       if (exchi.eq.4) then
         call int46(x,y,theta,wgt,cov,psp0,expsi,chi,exrho,sigmai,
     x              n,np,mdx,mdt,ncov,tol,gam,tau,itype,isigma,icnv,
     x              maxit,maxis,nitmon,nit,sigmaf,rs,delta,sc,sf,
     x              sg,sh,ip,sw,sx)
       else
         call int46(x,y,theta,wgt,cov,psp0,expsi,userfs,exrho,sigmai,
     x              n,np,mdx,mdt,ncov,tol,gam,tau,itype,isigma,icnv,
     x              maxit,maxis,nitmon,nit,sigmaf,rs,delta,sc,sf,
     x              sg,sh,ip,sw,sx)
       endif
       return
      end
      subroutine int46(x,y,theta,wgt,cov,psp0,expsi,exchi,exrho,sigmai,
     x                 n,np,mdx,mdt,ncov,tol,gam,tau,itype,isigma,icnv,
     x                 maxit,maxis,nitmon,nit,sigmaf,rs,delta,sc,sf,
     x                 sg,sh,ip,sw,sx)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer n,np,mdx,mdt,ncov,itype,isigma,icnv,maxit,maxis,nitmon
       integer nit,exrho,ip(np)
       real x(mdx,np),y(n),theta(mdt),wgt(n),cov(ncov),rs(n),delta(np)
       real sc(n),sf(np),sg(np),sh(np),sw(n),sx(mdx,np)
       external expsi,exchi,rho,userfs
       if (exrho.eq.2) then
         call rywalg(x,y,theta,wgt,cov,psp0,expsi,exchi,rho,sigmai,
     x               n,np,mdx,mdt,ncov,tol,gam,tau,itype,isigma,icnv,
     x               maxit,maxis,nitmon,nit,sigmaf,rs,delta,sc,sf,
     x               sg,sh,ip,sw,sx)
       else
         call rywalg(x,y,theta,wgt,cov,psp0,expsi,exchi,userfs,sigmai,
     x               n,np,mdx,mdt,ncov,tol,gam,tau,itype,isigma,icnv,
     x               maxit,maxis,nitmon,nit,sigmaf,rs,delta,sc,sf,
     x               sg,sh,ip,sw,sx)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int47(x,y,theta,wgt,cov,expsi,expsp,exchi,exrho,sigmai,
     x                 n,np,mdx,mdt,ncov,gam,tol,tau,itype,iopt,isigma,
     x                 icnv,maxit,maxis,nitmon,nit,sigmaf,qs1,rs,delta,
     x                 grad,hessnv,sd,sw,sf,sg,sh,ip,sx)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsi,expsp,exchi,exrho,n,np,mdx,mdt,ncov,itype,iopt
       integer isigma,icnv,maxit,maxis,nitmon,nit,ip(np)
       real x(mdx,np),y(n),theta(mdt),wgt(n),rs(n),delta(np),grad(np)
       real cov(ncov),hessnv(ncov),sd(n),sf(np),sg(np),sh(np),sw(n),
     x      sx(mdx,np)
       external psy,userfs
       if (expsi.eq.1) then
         call int48(x,y,theta,wgt,cov,psy,expsp,exchi,exrho,sigmai,
     x              n,np,mdx,mdt,ncov,gam,tol,tau,itype,iopt,isigma,
     x              icnv,maxit,maxis,nitmon,nit,sigmaf,qs1,rs,delta,
     x              grad,hessnv,sd,sw,sf,sg,sh,ip,sx)
       else
         call int48(x,y,theta,wgt,cov,userfs,expsp,exchi,exrho,sigmai,
     x              n,np,mdx,mdt,ncov,gam,tol,tau,itype,iopt,isigma,
     x              icnv,maxit,maxis,nitmon,nit,sigmaf,qs1,rs,delta,
     x              grad,hessnv,sd,sw,sf,sg,sh,ip,sx)
       endif
       return
      end
      subroutine int48(x,y,theta,wgt,cov,expsi,expsp,exchi,exrho,sigmai,
     x                 n,np,mdx,mdt,ncov,gam,tol,tau,itype,iopt,isigma,
     x                 icnv,maxit,maxis,nitmon,nit,sigmaf,qs1,rs,delta,
     x                 grad,hessnv,sd,sw,sf,sg,sh,ip,sx)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsp,exchi,exrho,n,np,mdx,mdt,ncov,itype,iopt,isigma
       integer icnv,maxit,maxis,nitmon,nit,ip(np)
       real x(mdx,np),y(n),theta(mdt),wgt(n),rs(n),delta(np),grad(np)
       real cov(ncov),hessnv(ncov),sd(n),sf(np),sg(np),sh(np),sw(n),
     x      sx(mdx,np)
       external expsi,psp,userfs
       if (expsp.eq.3) then
         call int49(x,y,theta,wgt,cov,expsi,psp,exchi,exrho,sigmai,
     x              n,np,mdx,mdt,ncov,gam,tol,tau,itype,iopt,isigma,
     x              icnv,maxit,maxis,nitmon,nit,sigmaf,qs1,rs,delta,
     x              grad,hessnv,sd,sw,sf,sg,sh,ip,sx)
       else
         call int49(x,y,theta,wgt,cov,expsi,userfs,exchi,exrho,sigmai,
     x              n,np,mdx,mdt,ncov,gam,tol,tau,itype,iopt,isigma,
     x              icnv,maxit,maxis,nitmon,nit,sigmaf,qs1,rs,delta,
     x              grad,hessnv,sd,sw,sf,sg,sh,ip,sx)
       endif
       return
      end
      subroutine int49(x,y,theta,wgt,cov,expsi,expsp,exchi,exrho,sigmai,
     x                 n,np,mdx,mdt,ncov,gam,tol,tau,itype,iopt,isigma,
     x                 icnv,maxit,maxis,nitmon,nit,sigmaf,qs1,rs,delta,
     x                 grad,hessnv,sd,sw,sf,sg,sh,ip,sx)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exchi,exrho,n,np,mdx,mdt,ncov,itype,iopt,isigma,icnv
       integer maxit,maxis,nitmon,nit,ip(np)
       real x(mdx,np),y(n),theta(mdt),wgt(n),rs(n),delta(np),grad(np)
       real cov(ncov),hessnv(ncov),sd(n),sf(np),sg(np),sh(np),sw(n),
     x      sx(mdx,np)
       external expsi,expsp,chi,userfs
       if (exchi.eq.4) then
         call int50(x,y,theta,wgt,cov,expsi,expsp,chi,exrho,sigmai,
     x              n,np,mdx,mdt,ncov,gam,tol,tau,itype,iopt,isigma,
     x              icnv,maxit,maxis,nitmon,nit,sigmaf,qs1,rs,delta,
     x              grad,hessnv,sd,sw,sf,sg,sh,ip,sx)
       else
         call int50(x,y,theta,wgt,cov,expsi,expsp,userfs,exrho,sigmai,
     x              n,np,mdx,mdt,ncov,gam,tol,tau,itype,iopt,isigma,
     x              icnv,maxit,maxis,nitmon,nit,sigmaf,qs1,rs,delta,
     x              grad,hessnv,sd,sw,sf,sg,sh,ip,sx)
       endif
       return
      end
      subroutine int50(x,y,theta,wgt,cov,expsi,expsp,exchi,exrho,sigmai,
     x                 n,np,mdx,mdt,ncov,gam,tol,tau,itype,iopt,isigma,
     x                 icnv,maxit,maxis,nitmon,nit,sigmaf,qs1,rs,delta,
     x                 grad,hessnv,sd,sw,sf,sg,sh,ip,sx)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exrho,n,np,mdx,mdt,ncov,itype,iopt,isigma,icnv,maxit
       integer maxis,nitmon,nit,ip(np)
       real x(mdx,np),y(n),theta(mdt),wgt(n),rs(n),delta(np),grad(np)
       real cov(ncov),hessnv(ncov),sd(n),sf(np),sg(np),sh(np),sw(n),
     x      sx(mdx,np)
       external expsi,expsp,exchi,rho,userfs
       if (exrho.eq.2) then
         call rynalg(x,y,theta,wgt,cov,expsi,expsp,exchi,rho,sigmai,
     x               n,np,mdx,mdt,ncov,gam,tol,tau,itype,iopt,isigma,
     x               icnv,maxit,maxis,nitmon,nit,sigmaf,qs1,rs,delta,
     x               grad,hessnv,sd,sw,sf,sg,sh,ip,sx)
       else
         call rynalg(x,y,theta,wgt,cov,expsi,expsp,exchi,userfs,sigmai,
     x               n,np,mdx,mdt,ncov,gam,tol,tau,itype,iopt,isigma,
     x               icnv,maxit,maxis,nitmon,nit,sigmaf,qs1,rs,delta,
     x               grad,hessnv,sd,sw,sf,sg,sh,ip,sx)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int51(rs,wgt,exchi,sigmai,n,np,tol,itype,isigma,maxis,
     x                 nit,sigmaf,sw,sc)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer n,np,exchi,itype,isigma,maxis,nit
       real rs(n),wgt(n),sigmai,tol,sigmaf,sw(n),sc(n)
       external chi,userfs
       if (exchi.eq.4) then
         call rysigm(rs,wgt,chi,sigmai,n,np,tol,itype,isigma,maxis,
     x               nit,sigmaf,sw,sc)
       else
         call rysigm(rs,wgt,userfs,sigmai,n,np,tol,itype,isigma,maxis,
     x               nit,sigmaf,sw,sc)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int52(rs1,rs2,wgt,exrho,n,np,nq,sigma,itype,
     x                 sum1,sum2,ftau)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exrho,n,np,nq,itype
       real rs1(n),rs2(n),wgt(n),sigma,sum1,sum2,ftau
       external rho,userfs
       if (exrho.eq.2) then
         call tftaut(rs1,rs2,wgt,rho,n,np,nq,sigma,itype,
     x               sum1,sum2,ftau)
       else
         call tftaut(rs1,rs2,wgt,userfs,n,np,nq,sigma,itype,
     x               sum1,sum2,ftau)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int53(x,a,exu,exup,nobs,nvar,ncov,mdx,maxit,nitmon,
     x                 icnv,tol,xfud,nit,dist,sa,ss,su,sup,st,sd)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exu,exup,nobs,nvar,ncov,mdx,maxit,nitmon,icnv,nit
       real x(mdx,nvar),tol,xfud,dist(nobs)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        st(ncov),sd(nvar),ucv,userfd
       external ucv,userfd
       if (exu.eq.5) then
         call int54(x,a,ucv,exup,nobs,nvar,ncov,mdx,maxit,nitmon,
     x              icnv,tol,xfud,nit,dist,sa,ss,su,sup,st,sd)
       else
         call int54(x,a,userfd,exup,nobs,nvar,ncov,mdx,maxit,nitmon,
     x              icnv,tol,xfud,nit,dist,sa,ss,su,sup,st,sd)
       endif
       return
      end
      subroutine int54(x,a,exu,exup,nobs,nvar,ncov,mdx,maxit,nitmon,
     x                 icnv,tol,xfud,nit,dist,sa,ss,su,sup,st,sd)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exup,nobs,nvar,ncov,mdx,maxit,nitmon,icnv,nit
       real x(mdx,nvar),tol,xfud,dist(nobs)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        st(ncov),sd(nvar),exu,upcv,userfd
       external exu,upcv,userfd
       if (exup.eq.6) then
         call wynalg(x,a,exu,upcv,nobs,nvar,ncov,mdx,maxit,nitmon,
     x               icnv,tol,xfud,nit,dist,sa,ss,su,sup,st,sd)
       else
         call wynalg(x,a,exu,userfd,nobs,nvar,ncov,mdx,maxit,nitmon,
     x               icnv,tol,xfud,nit,dist,sa,ss,su,sup,st,sd)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int55(x,a,exu,exup,nobs,nvar,ncov,mdx,mdz,maxit,
     x                 nitmon,icnv,tol,xfud,nit,dist,sa,ss,sz,su,
     x                 sup,sy1,sy2,st,st1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exu,exup,nobs,nvar,ncov,mdx,mdz,maxit,nitmon,icnv,nit
       real x(mdx,nvar),tol,xfud,dist(nobs),sz(mdz,nvar)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        sy1(nvar),sy2(nvar),st(ncov),st1(ncov),ucv,userfd
       external ucv,userfd
       if (exu.eq.5) then
         call int56(x,a,ucv,exup,nobs,nvar,ncov,mdx,mdz,maxit,
     x              nitmon,icnv,tol,xfud,nit,dist,sa,ss,sz,su,sup,
     x              sy1,sy2,st,st1)
       else
         call int56(x,a,userfd,exup,nobs,nvar,ncov,mdx,mdz,maxit,
     x              nitmon,icnv,tol,xfud,nit,dist,sa,ss,sz,su,sup,
     x              sy1,sy2,st,st1)
       endif
       return
      end
      subroutine int56(x,a,exu,exup,nobs,nvar,ncov,mdx,mdz,maxit,
     x                 nitmon,icnv,tol,xfud,nit,dist,sa,ss,sz,su,
     x                 sup,sy1,sy2,st,st1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exup,nobs,nvar,ncov,mdx,mdz,maxit,nitmon,icnv,nit
       real x(mdx,nvar),tol,xfud,dist(nobs),sz(mdz,nvar)
       double precision a(ncov),sa(ncov),ss(ncov),su(nobs),sup(nobs),
     x        sy1(nvar),sy2(nvar),st(ncov),st1(ncov),exu,upcv,userfd
       external exu,upcv,userfd
       if (exup.eq.6) then
         call wygalg(x,a,exu,upcv,nobs,nvar,ncov,mdx,mdz,maxit,
     x               nitmon,icnv,tol,xfud,nit,dist,sa,ss,sz,su,sup,
     x               sy1,sy2,st,st1)
       else
         call wygalg(x,a,exu,userfd,nobs,nvar,ncov,mdx,mdz,maxit,
     x               nitmon,icnv,tol,xfud,nit,dist,sa,ss,sz,su,sup,
     x               sy1,sy2,st,st1)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int57(x,a,gwt,exu,nobs,nvar,nvarq,ncov,mdx,tau,
     x                 maxit,nitmon,icnv,itypw,igwt,tol,nit,dist,
     x                 su,sa,st,sd,sz)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exu,nobs,nvar,nvarq,ncov,mdx,maxit,nitmon,icnv,itypw
       integer igwt,nit
       real x(mdx,nvar),tau,tol,dist(nobs),gwt(nobs)
       double precision a(ncov),su(nobs),sa(ncov),st(ncov),sd(nvar),
     x        sz(nvar),ucv,userfd
       external ucv,userfd
       if (exu.eq.5) then
         call wyfalg(x,a,gwt,ucv,nobs,nvar,nvarq,ncov,mdx,tau,
     x               maxit,nitmon,icnv,itypw,igwt,tol,nit,dist,
     x               su,sa,st,sd,sz)
       else
         call wyfalg(x,a,gwt,userfd,nobs,nvar,nvarq,ncov,mdx,tau,
     x               maxit,nitmon,icnv,itypw,igwt,tol,nit,dist,
     x               su,sa,st,sd,sz)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int58(x,exu,nobs,nvar,ncov,mdx,mda,mdw,iwgt,apar,
     x                 tau,tol,maxit,nitmon,icnv,k,nit,dist,a,su,
     x                 sb,sb0,sf,sg,sh,ip,sw,sz)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exu,nobs,nvar,ncov,mdx,mda,mdw,iwgt,ip(nvar)
       real dist(nobs),apar,tau,tol
       double precision x(mdx,nvar),a(mda,nvar),su(nobs),sb(ncov),
     x        sb0(ncov),sf(nvar),sg(nvar),sh(nvar),sw(mdw,nvar),
     x        sz(nvar),ucv,userfd
       external ucv,userfd
       if (exu.eq.5) then
         call wyfcol(x,ucv,nobs,nvar,ncov,mdx,mda,mdw,iwgt,apar,
     x               tau,tol,maxit,nitmon,icnv,k,nit,dist,a,su,
     x               sb,sb0,sf,sg,sh,ip,sw,sz)
       else
         call wyfcol(x,userfd,nobs,nvar,ncov,mdx,mda,mdw,iwgt,apar,
     x               tau,tol,maxit,nitmon,icnv,k,nit,dist,a,su,
     x               sb,sb0,sf,sg,sh,ip,sw,sz)
       endif
       return
      end
c
c***********************************************************************
c
      subroutine int59(s,result)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       real s,result,psy
       external psy
       result=psy(s)
      end
c
c***********************************************************************
c
      subroutine int60(s,result)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       real s,result,rho
       external rho
       result=rho(s)
      end
c
c***********************************************************************
c
      subroutine int61(s,result)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       real s,result,psp
       external psp
       result=psp(s)
      end
c
c***********************************************************************
c
      subroutine int62(s,result)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       real s,result,chi
       external chi
       result=chi(s)
      end
c
c***********************************************************************
c
      subroutine int63(s,result)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       real s
       double precision result,ucv
       external ucv
       result=ucv(s)
      end
c
c***********************************************************************
c
      subroutine int64(s,result)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       real s
       double precision result,upcv
       external upcv
       result=upcv(s)
      end
c
c***********************************************************************
c
      subroutine int65(s,result)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       real s
       double precision result,vcv
       external vcv
       result=vcv(s)
      end
c
c***********************************************************************
c
      subroutine int66(s,result)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       real s
       double precision result,vpcv
       external vpcv
       result=vpcv(s)
      end
c
c***********************************************************************
c
      subroutine int67(s,result)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       real s
       double precision result,wcv
       external wcv
       result=wcv(s)
      end
c
c***********************************************************************
c
      subroutine int68(s,result)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       real s
       double precision result,wpcv
       external wpcv
       result=wpcv(s)
      end
c
c***********************************************************************
c
      subroutine int69(s,result)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       real s
       double precision result,www
       external www
       result=www(s)
      end
c
c***********************************************************************
c
      subroutine int70(upar,npar,s,result)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       real upar(npar),s
       integer npar
       double precision result,ugl
       external ugl
       result=ugl(upar,npar,s)
      end
C-----------------------------------------------------------------------
C
C                     R O B E T H  FORTRAN Source
C
C  File KVMAIN.F  Main subroutines of Chapter 4
C
C-----------------------------------------------------------------------
C
      SUBROUTINE KIEDCHZ(WGT,N,C,ITYPE,D,E)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION WGT(N),E(N),D(N)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NPRCHK=C.GE.0..AND.N.GT.0.AND.(ITYPE.EQ.2.OR.ITYPE.EQ.3)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'KIEDCH',1)
      IF (ITYPE.EQ.3) GOTO 30
C
C  MALLOWS CASE
C
      C2=C*C
      CALL GAUSSZ(1,C,PC)
      CALL XERFZ(2,C,PD)
      G1=C2+(1.-C2)*(2.*PC-1.)-2.*C*PD
      F1=2.*PC-1.
      DO 20 I=1,N
      D(I)=F1*WGT(I)
      E(I)=G1*WGT(I)*WGT(I)
   20 CONTINUE
      RETURN
C
C  SCHWEPPE CASE
C
   30 DO 40 I=1,N
      Z=C*WGT(I)
      Z2=Z*Z
      CALL GAUSSZ(1,Z,PC)
      CALL XERFZ(2,Z,PD)
      E(I)=Z2+(1.-Z2)*(2.*PC-1.)-2.*Z*PD
      D(I)=2.*PC-1.
   40 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE KIEDCU(WGT,EXPSI,N,ITYPE,UPPER,TIL,ERREST,D,E)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION WGT(N),E(N),D(N)
      EXTERNAL EXPSI,PS2PHI,PSPPHI
      LOGICAL NPRCHK
      COMMON/INTPAR/ITYP,I,NEVAL,LIMIT,KEY
      COMMON/INTEG/UUPER,TTIL,IWORK(40),WORK(160),IER1,ERRST1
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.(ITYPE.EQ.2.OR.ITYPE.EQ.3)
     1       .AND.TIL.GT.0.AND.UPPER.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'KIEDCU',1)
      ITYP=ITYPE
C
C  MALLOWS CASE
C
      LIMIT=40
      KEY=1
      CALL INTGRS(PSPPHI,WGT,N,EXPSI,EXPSI,0.,UPPER,TIL,TIL,KEY,
     1          LIMIT,D2,ERRSTD,NEVAL1,IER1,WORK,IWORK)
      CALL INTGRS(PS2PHI,WGT,N,EXPSI,EXPSI,0.,UPPER,TIL,TIL,KEY,
     1          LIMIT,E2,ERRSTE,NEVAL2,IER2,WORK,IWORK)
      D2=2.*D2
      E2=2.*E2
      NEVAL=NEVAL1+NEVAL2
      ERREST=AMAX1(ERRSTD,ERRSTE)
      IER=MAX0(IER1,IER2)
      IF (ITYP.EQ.3) GOTO 30
      DO 20 I=1,N
      D(I)=0.
      E(I)=0.
      IF (WGT(I).LE.0.) GOTO 20
      D(I)=D2*WGT(I)
      E(I)=E2*WGT(I)*WGT(I)
   20 CONTINUE
      GOTO 50
C
C  SCHWEPPE CASE
C
   30 DO 40 I=1,N
      D(I)=0.
      E(I)=0.
      IF (WGT(I).LE.0.) GOTO 40
      IF (WGT(I).NE.1.) GOTO 35
      D(I)=D2
      E(I)=E2
      GOTO 40
   35 CALL INTGRS(PSPPHI,WGT,N,EXPSI,EXPSI,0.,UPPER,TIL,TIL,KEY,
     1          LIMIT,D3,ERRSTD,NEVAL1,IER1,WORK,IWORK)
      IER=MAX0(IER,IER1)
      CALL INTGRS(PS2PHI,WGT,N,EXPSI,EXPSI,0.,UPPER,TIL,TIL,KEY,
     1          LIMIT,E3,ERRSTE,NEVAL2,IER2,WORK,IWORK)
      IER=MAX0(IER,IER2)
      NEVAL=NEVAL1+NEVAL2
      ERREST=AMAX1(ERREST,ERRSTD,ERRSTE)
      D(I)=2.*D3*WGT(I)
      E(I)=2.*E3*WGT(I)*WGT(I)
   40 CONTINUE
   50 IF (IER.NE.0) CALL MESSGE(400+IER,'KIEDCU',0)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE KIASCVZ(XT,K,NP,MDX,NCOV,FU,FB,COV)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DIMENSION XT(MDX,NP),COV(NCOV)
      DOUBLE PRECISION SM,DZERO
      LOGICAL NPRCHK
C
C  PARAMETER CHECK AND INITIALIZATION
C
      DZERO=0.D0
      LDIAG=MIN0(MDX,NP)
      NN=NP*(NP+1)/2
      NPRCHK=LDIAG.GT.0.AND.K.GT.0.AND.K.LE.LDIAG.AND.NCOV.EQ.NN
      IF (.NOT.NPRCHK) CALL MESSGE(500,'KIASCV',1)
      KP1=K+1
C
C  STORE U IN COV.
C
      L=0
      DO 20 J=1,K
      DO 10 I=1,J
      L=L+1
      COV(L)=XT(I,J)
   10 CONTINUE
   20 CONTINUE
C
C  INVERT U UPON ITSELF
C
      DO 30 J=1,K
      XT(J,J)=1./XT(J,J)
   30 CONTINUE
      IF (K.EQ.1) GOTO 60
      KM1=K-1
      DO 50 I=1,KM1
      IP1=I+1
      DO 45 J=IP1,K
      JM1=J-1
      SM=DZERO
      DO 40 L=I,JM1
      SM=SM+XT(I,L)*DBLE(XT(L,J))
   40 CONTINUE 
      XT(I,J)=-SNGL(SM)*XT(J,J)
   45 CONTINUE
   50 CONTINUE
C
C  REPLACE U**(-1) BY UPPER TRIANG.PART OF (U*U**T)**(-1)
C
   60 CONTINUE
      DO 90 I=1,K
      DO 80 J=I,K
      SM=DZERO
      DO 70 L=J,K
      SM=SM+XT(I,L)*DBLE(XT(J,L))
   70 CONTINUE
      XT(I,J)=SNGL(SM)
   80 CONTINUE
   90 CONTINUE
C
C  INTERCH. (U*U**T)**(-1) WITH COV(1)...COV(K*(K+1)/2)
C
      L=0
      DO 130 J=1,K
      DO 120 I=1,J
      L=L+1
      AIJ=XT(I,J)
      XT(I,J)=COV(L)
      COV(L)=AIJ
  120 CONTINUE
  130 CONTINUE
C
C  MULTIPLY COV BY THE SCALE FACTOR FU
C
      IF (FU.GT.0.) CALL SCALZ(COV,FU,NCOV,1,NCOV)
C
C  COMPLETE COV
C
      IF (K.EQ.NP) RETURN
      II=K*(K+1)/2+1
      J=KP1
      L=II+K
      DO 160 I=II,NCOV
      COV(I)=0.0
      IF (I.NE.L) GOTO 160
      COV(I)=FB
      J=J+1
      L=L+J
  160 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE KFASCVZ(XT,COV,K,NP,MDX,NCOV,F,SE,SG,IP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DIMENSION COV(NCOV),XT(MDX,NP),SE(NP),SG(NP)
      INTEGER IP(NP)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK AND INITIALIZATION
C
      KP1=K+1
      NN=NP*(NP+1)/2
      LDIAG=MIN0(MDX,NP)
      NPRCHK=LDIAG.GT.0.AND.K.GT.0.AND.K.LE.LDIAG.AND.NCOV.EQ.NN
      IF (.NOT.NPRCHK) CALL MESSGE(500,'KFASCV',1)
C
C  TRANSFORM UNSCALED COVARIANCE MATRIX TO COMPENSATE
C  HOUSEHOLDER TRANSFORMATIONS AND PERMUTATIONS
C
      IF (K.EQ.NP) GOTO 130
      DO 120 II=1,K
      I=II
      CALL VSV(I,KP1,NP,XT(I,1),MDX,SG(I),COV,NCOV,SE)
  120 CONTINUE
  130 CONTINUE
      DO 150 JJ=1,LDIAG
      J=LDIAG-JJ+1
      IF (IP(J).EQ.J) GOTO 150
      L=IP(J)
      CALL EXCHZ(COV,NP,NCOV,J,L)
  150 CONTINUE
C
C  MULTIPLY COV BY THE SCALE FACTOR F
C
      IF (F.GT.0.) CALL SCALZ(COV,F,NCOV,1,NCOV)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE KTASKVZ(X,N,NP,MDX,NCOV,TAU,F,A,COV)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DIMENSION X(MDX,NP),A(NCOV),COV(NCOV)
      LOGICAL NPRCHK
      DOUBLE PRECISION SM1,DZERO
C
C  PARAMETER CHECK
C
      NN=NP*(NP+1)/2
      NPRCHK=NP.GT.0.AND.N.GE.NP.AND.N.LE.MDX.AND.NCOV.EQ.NN
     1       .AND.TAU.GE.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'KTASKV',1)
 
      DZERO=0.D0
C
C  COMPUTE X**T*X AND STORE IT TEMPORARILY IN COV
C
      L=0
      DO 60 I=1,NP
      DO 50 J=1,I
      L=L+1
      SM1=DZERO
      DO 20 K=1,N
      SM1=SM1+DBLE(X(K,I))*X(K,J)
   20 CONTINUE
      COV(L)=SNGL(SM1)
   50 CONTINUE
   60 CONTINUE
C
C  COMPUTE A LOWER TRIANGULAR MATRIX A SUCH THAT
C  (X**T*X)**(-1)=A**T*A; SET COV=A**T*A.
C
      CALL MCHLZ(COV,NP,NN,INFO)
      IF (INFO.EQ.0) GOTO 65
      CALL MESSGE(400+INFO,'KTASKV',0)
      RETURN
   65 DO 70 L=1,NN
      A(L)=COV(L)
   70 CONTINUE
      CALL MINVZ(A,NP,NN,TAU,ISING)
      IF (ISING.EQ.0) GOTO 75
      CALL MESSGE(450,'KTASKV',0)
      RETURN
   75 CALL MTT1Z(A,COV,NP,NN)
      IF (F.GT.0.) CALL SCALZ(COV,F,NCOV,1,NCOV)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE KTASKWZ(X,D,E,N,NP,MDX,MDSC,NCOV,TAU,IA,F,F1,
     1                  IAINV,A,S1INV,S2,AINV,COV,SC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DIMENSION X(MDX,NP),D(N),E(N),S1INV(NCOV),S2(NCOV),
     1          A(NCOV),AINV(NCOV),COV(NCOV),SC(MDSC,NP)
      LOGICAL NPRCHK
      DOUBLE PRECISION SM1,SM2,DZERO,DXX
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NP*(NP+1)/2
      NPRCHK=NP.GT.0.AND.N.GE.NP.AND.N.LE.MDX.AND.NCOV.EQ.NN
     1       .AND.(IA.EQ.0.OR.IA.EQ.1.OR.IA.EQ.-1)
     2       .AND.TAU.GE.0..AND.MDSC.GE.NP
     3       .AND.(IAINV.EQ.0.OR.IAINV.EQ.1)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'KTASKW',1)
      DZERO=0.D0
      XN1=FLOAT(N)
C
C  IF IA.EQ.-1 SET S1INV=F1*A
C
      IF (IA.NE.-1) GOTO 40
      DO 35 L=1,NCOV
      S1INV(L)=A(L)
   35 CONTINUE
      IF (F1.GT.0.) CALL SCALZ(S1INV,F1,NN,1,NN)
   40 CONTINUE
C
C  IF IA.EQ.0 SET S1INV=F1*(A**T)*A
C
      IF (IA.NE.0) GOTO 45
      CALL MTT1Z(A,S1INV,NP,NN)
      IF (F1.GT.0.) CALL SCALZ(S1INV,F1,NN,1,NN)
   45 CONTINUE
C
C  COMPUTE S2=X**T*E*X/N (AND STORE IT IN S2).
C  IF IA.EQ.1 COMPUTE S1=X**T*D*X/N (AND STORE IT TEMPORARILY IN COV)
C
      L=0
      DO 60 I=1,NP
      DO 50 J=1,I
      L=L+1
      SM2=DZERO
      SM1=DZERO
      DO 20 K=1,N
      DXX=DBLE(X(K,I))*X(K,J)
      SM2=SM2+DXX*E(K)
      IF (IA.EQ.1) SM1=SM1+DXX*D(K)
   20 CONTINUE
      S2(L)=SNGL(SM2)/XN1
      IF (IA.EQ.1) COV(L)=SNGL(SM1)/XN1
   50 CONTINUE
   60 CONTINUE
C
C  IF IA .EQ.1 COMPUTE A LOWER TRIANGULAR MATRIX A (AND ITS INVERSE)
C  SUCH THAT S1INV=S1**(-1)=A**T*A (IF IAINV.EQ.1 STORE THE INVERSE
C  OF A IN AINV)
C
C
      IF (IA.EQ.-1.OR.IA.EQ.0) GOTO 80
      CALL MCHLZ(COV,NP,NN,INFO)
      IF (INFO.EQ.0) GOTO 65
      CALL MESSGE(400+INFO,'KTASKW',0)
      IAINV=400+INFO
      RETURN
   65 CONTINUE
      DO 70 L=1,NN
      IF (IAINV.EQ.1) AINV(L)=COV(L)
      A(L)=COV(L)
   70 CONTINUE
      CALL MINVZ(A,NP,NN,TAU,ISING)
      IF (ISING.EQ.0) GOTO 75
      CALL MESSGE(450,'KTASKW',0)
      IAINV=450
      RETURN
   75 CONTINUE
      CALL MTT1Z(A,S1INV,NP,NN)
C
C  COMPUTE S2*S1**(-1) (AND STORE IT IN SC)
C
   80 CALL MSSZ(S2,S1INV,SC,NP,NN,MDSC)
C
C  COMPUTE COV=F*S1**(-1)*S2*S1**(-1)
C
      CALL MSF1Z(S1INV,SC,COV,NP,NN,MDSC)
      IF (F.GT.0.) CALL SCALZ(COV,F,NN,1,NN)
C
C  IF IAINV.EQ.1 (AND IA.NE.1) COMPUTE THE INVERSE
C  OF A AND STORE IT IN AINV
C
      IF (IA.EQ.1.OR.IAINV.EQ.0) RETURN
      DO 90 L=1,NN
      AINV(L)=A(L)
   90 CONTINUE
      CALL MINVZ(AINV,NP,NN,TAU,ISING)
      IF (ISING.NE.0) CALL MESSGE(460,'KTASKW',0)
      IAINV=460
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE KFFACV(RS,EXPSI,EXPSP,N,NP,SIGMA,FH)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C  HUBER'S CORRECTION FACTOR FOR AS. COVARIANCE
C  MATRIX OF PARAMETER ESTIMATES
C
      DIMENSION RS(N)
      EXTERNAL EXPSI,EXPSP
      DATA TL/1.E-8/
C
C  PARAMETER CHECK
C
      IF (N.LT.NP.OR.NP.LE.0) CALL MESSGE(500,'KFFACV',1)
C
      FH=1.
      IF (NP.EQ.N) RETURN
      CALL FACS(RS,N,NP,SIGMA,TL,XKAPPA,SUM2,EXPSI,EXPSP)
      IF (XKAPPA.EQ.0.) CALL MESSGE(301,'KFFACV',0)
      IF (XKAPPA.EQ.0.) RETURN
      FH=(XKAPPA*XKAPPA)*SUM2
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE KFEDCC(WGT,RS,EXPSI,EXPSP,N,SIGMA,ITYPE,D,E)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DIMENSION WGT(N),RS(N),D(N),E(N)
      EXTERNAL EXPSI,EXPSP
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=SIGMA.GT.0..AND.N.GT.0.AND.(ITYPE.EQ.2.OR.ITYPE.EQ.3)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'KFEDCC',1)
      IF (ITYPE.EQ.3) GOTO 20
C
C  MALLOWS CASE
C
      DO 10 I=1,N
      IF (WGT(I).GT.0.) GOTO 5
      D(I)=-1.
      E(I)=0.
      GOTO 10
    5 X=RS(I)/SIGMA
      D(I)=EXPSP(X)*WGT(I)
      E(I)=(EXPSI(X)*WGT(I))**2
   10 CONTINUE
      RETURN
C
C  SCHWEPPE CASE
C
   20 DO 40 I=1,N
      IF (WGT(I).GT.0.) GOTO 30
      D(I)=-1.
      E(I)=0.
      GOTO 40
   30 X=RS(I)/SIGMA/WGT(I)
      D(I)=EXPSP(X)
      E(I)=(EXPSI(X)*WGT(I))**2
   40 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE KFEDCB(WGT,RS,EXPSI,EXPSP,N,SIGMA,ITYPE,D,E)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DIMENSION WGT(N),RS(N),D(N),E(N)
      EXTERNAL EXPSI,EXPSP
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=SIGMA.GT.0..AND.N.GT.0.AND.(ITYPE.EQ.2.OR.ITYPE.EQ.3)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'KFEDCB',1)
      IF (ITYPE.EQ.3) GOTO 30
C
C  MALLOWS CASE
C
      S1=0.
      S2=0.
      DO 10 J=1,N
      IF (WGT(J).LE.0.) GOTO 10
      X=RS(J)/SIGMA
      S1=S1+EXPSP(X)
      S2=S2+(EXPSI(X))**2
   10 CONTINUE
      S1=S1/FLOAT(N)
      S2=S2/FLOAT(N)
      DO 20 I=1,N
      D(I)=S1*WGT(I)
      E(I)=S2*WGT(I)*WGT(I)
   20 CONTINUE
      RETURN
C
C  SCHWEPPE CASE
C
   30 DO 50 I=1,N
      S1=0.
      S2=0.
      IF (WGT(I).LE.0.) GOTO 45
      DO 40 J=1,N
      X=RS(J)/SIGMA/WGT(I)
      S1=S1+EXPSP(X)
      S2=S2+(EXPSI(X))**2
   40 CONTINUE
   45 D(I)=S1/FLOAT(N)
      E(I)=S2/FLOAT(N)*WGT(I)*WGT(I)
   50 CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
C
C                     R O B E T H  FORTRAN Source
C
C  File LCAUXI.F  Auxiliary subroutines of Chapter 1
C
C-----------------------------------------------------------------------
C
      FUNCTION VS(X,N,T)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C     PURPOSE:
C     --------
C     VS(X,N,T) GIVES THE VALUE OF THE ONE SAMPLE WILCOXON TEST
C     STATISTIC (SUM OF THE POSITIVE RANKS) ON THE OBSERVATIONS X(I)-T,
C     I=1...N.  IT IS ASSUMED THAT X(1).LE.....LE.X(N) AND THAT
C     X(1).LE.T.LE.X(N).  LET AD1=ABS(X(NL)-T) AND AD2=ABS(X(NU)-T)
C     WHERE NL.LT.NU.  TIES ARE BROKEN ASSUMING
C        AD1.GT.AD2   IF X(NL)-T.LE.0
C        AD1.LT.AD2   IF X(NL)-T.GT.0.
C     THE FUNCTION VS IS CALLED BY THE FUNCTION HDLE.
C     THIS FUNCTION IS A MODIFICATION OF THE FUNCTION WILCXN IN
C     ANDREWS ET AL., ROBUST ESTIMATES OF LOCATION, PRINCETON UN.PR.1972
C
C     METHOD:
C     -------
C     SEE PROGRAM.
C
C     PARAMETERS:
C     -----------
C     INPUT   X              ORIGINAL OBSERVATIONS
C     INPUT   N              NUMBER OF OBSERVATIONS
C     INPUT   T
C
      DIMENSION X(N)
      IS=0
      NU=N
      NL=1
      D1=T-X(NL)
      D2=X(NU)-T
      DO 1 I=1,N
      IF (D1.LT.D2) GOTO 10
      NL=NL+1
      IF (NL.GT.N) GOTO 1
      D1=T-X(NL)
      GOTO 1
   10 IS=IS+N-I+1
      NU=NU-1
      IF (NU.LT.1) GOTO 1
      D2=X(NU)-T
    1 CONTINUE
      VS=FLOAT(IS)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      FUNCTION WXYz(X,Y,M,N,T)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C     PURPOSE
C     -------
C     WXYz(X,Y,M,N,T) GIVES THE NUMBER OF PAIRS (I,J) SUCH THAT
C     X(I) < Y(J) - T. I=1...M, J=1...N AND I < J. IT IS ASSUMED
C     THAT X(1).LE.....LE.X(M) AND Y(1).LE.....LE.Y(N)
C
      DIMENSION X(M),Y(N)
      NIJ=M*N
      IF (X(M).LT.Y(1)-T) GOTO 300
      NIJ=0
      IF (Y(N)-T.LE.X(1)) GOTO 300
      I=1
      J=0
  100 J=J+1
      IF (J.GT.N) GOTO 300
  200 IF (X(I).GE.Y(J)-T) GOTO 100
      NIJ=NIJ+N-J+1
      I=I+1
      IF (I.GT.M) GOTO 300
      GOTO 200
  300 WXYz=FLOAT(NIJ)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      REAL FUNCTION RANKZ(N,II)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      I=II
      IF(I.GT.N/2) GOTO 10
      RANKZ=(I/2)*4+MOD(I,2)
      RETURN
   10 CONTINUE
      I=N-I+1
      RANKZ=(I/2)*4+3*MOD(I,2)-1
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION ENDTIE(TIEST,OBS,SNPM)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      INTEGER TIEST,SNPM
      REAL OBS(SNPM)
      ENDTIE=TIEST
   10 IF(ENDTIE.GE.SNPM) GOTO 20 
      I=ENDTIE
      IF(OBS(I+1).NE.OBS(I))RETURN
      ENDTIE=ENDTIE+1
      GOTO 10
   20 RETURN 
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION CHIFI(S,AUX,N,CHI,FEXT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  AUXILIARY ROUTINE FOR LIBETU
C
      DIMENSION AUX(N)
      EXTERNAL CHI,FEXT
      DATA NCALL,FX1/0,0.0/
      IF (NCALL.EQ.1) FX1=FEXT(1.0)
      CHIFI=FX1*AUX(1)
      CALL XERFZ(2,S,PHI)
      CHIFI=CHI(S)*PHI
      RETURN
      END
C-----------------------------------------------------------------------
C
C                     R O B E T H  FORTRAN Source
C
C  File LCMAIN.F  Main subroutines of Chapter 1
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LIBETHZ(D,BTA)
      COMMON/BETA/BETA,BET0
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : B. MARTIN / A. MARAZZI
C.......................................................................
C
C  COMPUTE BETA AS A FUNCTION OF D
C
      IF (D.LE.0.) CALL MESSGE(500,'LIBETH',1)
      D2=D*D
      CALL GAUSSZ(1,D,PD)
      CALL XERFZ(2,D,DD)
      BETA=-D*DD+PD-.5+D2*(1.-PD)
      BTA=BETA
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LIBETU(EXCHI,UPPER,TIL,ERREST,BTA)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION AUX(1)
      LOGICAL NPRCHK
      EXTERNAL EXCHI,CHIFI
      COMMON/BETA/BETA,BET0
      COMMON/INTPAR/ITYP,INTPAR,NEVAL,LIMIT,KEY
      COMMON/INTEG/UUPER,TTIL,IWORK(40),WORK(160),IER1,ERRST1
C
C  PARAMETERS CHECK
C
      NPRCHK=UPPER.GT.0..AND.TIL.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'LIBETU',1)
C
      LIMIT=40
      KEY=1
      CALL INTGRS(CHIFI,AUX,1,EXCHI,EXCHI,0.,UPPER,TIL,TIL,KEY,
     1          LIMIT,BETA,ERREST,NEVAL,IER,WORK,IWORK)
      IF (IER.GT.0) CALL MESSGE(400+IER,'LIBETU',0)
      BETA=2.*BETA
      BTA=BETA
      RETURN
      END
 
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LIEPSHZ(C,EPSI2,EPSIP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
C  EXPECTED VALUE OF PSI(X,C)**2 AND PSP(X,C)
C  (WHERE X IS A STANDARD NORMAL DEVIATE)
C
      IF (C.LE.0.) CALL MESSGE(500,'LIEPSH',1)
      CALL GAUSSZ(1,C,PC)
      CALL XERFZ(2,C,PD)
      C2=C*C
      EPSI2=C2+(1.-C2)*(2.*PC-1.)-2.*C*PD
      EPSIP=(2.*PC-1.)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LIEPSU(EXPSI,UPPER,TIL,ERREST,EPSI2,EPSIP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
C  EXPECTED VALUES OF PSI(X)**2 AND PSP(X)
C  (WHERE X IS A STANDARD NORMAL DEVIATE)
C
      DIMENSION WGT(1)
      LOGICAL NPRCHK
      EXTERNAL EXPSI,PS2PHI,PSPPHI
      COMMON/INTPAR/ITYP,INTPAR,NEVAL,LIMIT,KEY
      COMMON/INTEG/UUPER,TTIL,IWORK(40),WORK(160),IER1,ERRST1
C
C  PARAMETERS CHECK
C
      NPRCHK=UPPER.GT.0.AND.TIL.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'LIEPSU',1)
      LIMIT=40
      KEY=1
      CALL INTGRS(PS2PHI,WGT,1,EXPSI,EXPSI,0.,UPPER,TIL,TIL,KEY,
     1         LIMIT,EPSI2,ERRST1,NEVAL1,IER1,WORK,IWORK)
      EPSI2=2.*EPSI2
      CALL INTGRS(PSPPHI,WGT,1,EXPSI,EXPSI,0.,UPPER,TIL,TIL,KEY,
     1         LIMIT,EPSIP,ERRST2,NEVAL2,IER2,WORK,IWORK)
      EPSIP=2.*EPSIP
      NEVAL=NEVAL1+NEVAL2
      ERREST=AMAX1(ERRST1,ERRST2)
      IER=MAX0(IER1,IER2)
      IF (IER.GT.0) CALL MESSGE(400+IER,'LIEPSU',0)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LIBET0Z(BT0)
      COMMON/BETA/BETA,BET0
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      P=.75
      CALL NQUANT(P,BET0)
      BT0=BET0
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LICLLSZ(Y,N,THETA,SIGMA,VAR,RS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
      DIMENSION Y(N),RS(N)
      DOUBLE PRECISION SUM
C
C PARAMETER CHECK AND INITIALIZATION
C
      IF (N.LE.1) CALL MESSGE(500,'LICLLS',1)
C
C  COMPUTE THETA
C
      SUM=0.D0
      DO 100 I=1,N
      SUM=SUM+DBLE(Y(I))
  100 CONTINUE
      THETA=SNGL(SUM)/FLOAT(N)
C
C  COMPUTE SIGMA, UNSCALED VAR. AND RESIDUALS
C
      SUM=0.D0
      DO 200 I=1,N
      RS(I)=Y(I)-THETA
      SUM=SUM+DBLE(RS(I)*RS(I))
  200 CONTINUE
      DF=FLOAT(N-1)
      VAR=SNGL(SUM)/DF
      SIGMA=SQRT(VAR)
      VAR=VAR/FLOAT(N)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LILARSZ(Y,N,ISORT,THETA,SIGMA,XMAD,VAR,RS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
C  MEDIAN, ESTIMATE OF STD. DEVIATION AND MEDIAN (ABSOLUTE) DEVIATION
C
      DIMENSION Y(N),RS(N)
      DATA PI/3.141592653/
C
      IF (N.LE.0) CALL MESSGE(500,'LILARS',1)
      DO 100 I=1,N
      RS(I)=Y(I)
  100 CONTINUE
      CALL LMDDZ(RS,Y,N,ISORT,THETA,XMAD,SIGMA)
      VAR=PI*SIGMA*SIGMA/FLOAT(2*N)
      DO 200 I=1,N
      RS(I)=RS(I)-THETA
  200 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LYHALG(Y,EXPSI,EXPSP,EXCHI,THETA,SIGMAI,N,TOL,GAM,
     1                  ISIGMA,MAXIT,MAXIS,NIT,SIGMAF,VAR,RS,SC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : B.MARTIN / A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION Y(N),RS(N),SC(N)
      LOGICAL NPRCHK
      EXTERNAL EXPSI,EXPSP,EXCHI
      COMMON/BETA/BETA,BET0
      COMMON/CONST/CONST
      DATA TL/1.E-8/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NPRCHK= N.GT.0.AND.SIGMAI.GT.0..AND.TOL.GT.0..AND.(GAM.GT.0.
     1        .AND.GAM.LT.2.).AND.MAXIT.GT.0.AND.MAXIS.GT.0.AND.
     2        (ISIGMA.GE.-2.AND.ISIGMA.LE.2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'LYHALG',1)
      IASG=IABS(ISIGMA)
      IF (IASG.EQ.2) BET0=.6745
      IF (IASG.EQ.1) CONST=BETA*FLOAT(N-1)
      IF (IASG.EQ.2) CONST=BET0*FLOAT(N-1)
      SIGMA=SIGMAI
      SIGMB=SIGMAI
C
C  STEP1 - SET NIT = 1
C
      NIT=1
C
C  STEP2  -  COMPUTE RESIDUALS Y-THETA
C
   10 DO 20 I=1,N
      RS(I)=Y(I)-THETA
   20 CONTINUE
      IF (ISIGMA.LT.0.AND.NIT.EQ.1) GOTO 50
      IF (ISIGMA.EQ.0) GOTO 50
C
C  STEP3  -  COMPUTE A NEW VALUE SIGMB FOR SIGMA
C
      CALL RYSIGM(RS,SC,EXCHI,SIGMA,N,1,TOL,1,ISIGMA,MAXIS,
     +NIS,SIGMB,SC,SC)
      IF (SIGMB.GT.TL) GOTO 50
      CALL MESSGE(401,'LYHALG',0)
      RETURN
C
C  STEP4  -  WINSORIZE THE RESIDUALS
C
   50 DO 55 I=1,N
      RSS=RS(I)/SIGMB
      RS(I)=EXPSI(RSS)*SIGMB
   55 CONTINUE
C
C  STEP5  -  COMPUTE THE INCREMENT
C
      D=0.
      DO 60 I=1,N
      D=D+RS(I)
   60 CONTINUE
      D=D/FLOAT(N)
C
C  STEP6  -  UPDATE THETA
C
      THETA=THETA+GAM*D
C
C  STEP7  -  STOP IF DESIRED PRECISION IS REACHED
C
      TOL1=TOL*AMAX1(1.,SIGMB)
      IF (ABS(D).GE.TOL1) GOTO 70
      DSIG=SIGMB-SIGMA
      IF (ABS(DSIG).GE.TOL1) GOTO 70
      GOTO 100
   70 CONTINUE
C
C  ITERATIONS CONTINUE IF NIT. LT. MAXIT
C
      SIGMA=SIGMB
      IF (NIT.GE.MAXIT) GOTO 100
      NIT=NIT+1
      GOTO 10
C
C  ITERATIONS ARE TERMINATED
C  COMPUTE SCALED VAR. OF THETA
C
  100 CONTINUE
      SIGMAF=SIGMB
      TMP1=0.
      TMP2=0.
      DO 110 I=1,N
       RS(I)=Y(I)-THETA
       S=RS(I)/SIGMB
       TMP1=TMP1+EXPSP(S)
       PS=EXPSI(S)
       TMP2=TMP2+PS*PS
  110 CONTINUE
      VAR=TMP2*SIGMA*SIGMA*FLOAT(N)/(TMP1*TMP1*FLOAT(N-1))
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LYHDLEZ(Y,N,ISORT,K,TOL,MAXIT,NIT,HDLE)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR: A. MARAZZI
C.......................................................................
C
      DIMENSION Y(N)
      LOGICAL NPRCHK
      EXTERNAL VS
      DATA ZETA/0.0001/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NPRCHK=N.GT.0.AND.MAXIT.GT.0.AND.TOL.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'LYHDLE',1)
      M=N*(N+1)/2
      NPRCHK=K.LT.M.AND.K.GT.1
      IF (ISORT.NE.0) CALL SRT1Z(Y,N,1,N)
      KK=0
      IF (NPRCHK) GO TO 30
      IF (K.EQ.1) GOTO 10
      IF (K.EQ.M) GOTO 20
      CALL MESSGE(500,'LYHDLE',1)
   10 T=Y(1)
      KK=0
      GOTO 999
   20 T=Y(N)
      KK=0
      GOTO 999
   30 CONTINUE
      YINC=0.5*SQRT(FLOAT(N))
      INC=INT(YINC)
      IF (INC.EQ.0) INC=1
      SCONST=FLOAT(K-M)-ZETA
C
C   INITIALIZE: FIND T0 AND T1 SUCH THAT VS(Y,N,T0)+K-M-ZETA.GT.0
C   AND VS(Y,N,T1)+K-M-ZETA.LT.0.
C
      IY0=K*N/M
      IF (IY0.LT.1) IY0=1
      IF(IY0.GT.N) IY0=N
      T0=Y(IY0)
      S0=VS(Y,N,T0)+SCONST
      IF (IY0.NE.1.OR.S0.GE.0.) GOTO 33
      T=Y(1)
      KK=0
      GOTO 999
   33 CONTINUE
      IF (S0.LT.0.) GOTO 40
      IY1=IY0
   35 IY1=IY1+INC
      IF (IY1.GT.N) IY1=N
      T1=Y(IY1)
      S1=VS(Y,N,T1)+SCONST
      IF (IY1.NE.N.OR.S1.LT.0.) GOTO 38
      T=Y(N)
      KK=0
      GOTO 999
   38 CONTINUE
      IF (S1.LT.0.) GOTO 45
      IY0=IY1
      T0=T1
      S0=S1
      GOTO 35
   40 T1=T0
      S1=S0
      IY1=IY0
      IY0=IY0-INC
      IF (IY0.LT.1) IY0=1
      T0=Y(IY0)
      S0=VS(Y,N,T0)+SCONST
      IF (IY0.NE.1.OR.S0.GE.0.) GOTO 43
      T=Y(1)
      KK=0
      GOTO 999
   43 CONTINUE
      IF (S0.GT.0.) GOTO 45
      GOTO 40
C
C  INTERPOLATE AND ITERATE
C
   45 CONTINUE
      DO 90 J=1,MAXIT
      KK=J
      IF (J.LE.2) T=T0+(T1-T0)*S0/(S0-S1)
      IF (J.GT.2) T=0.5*(T0+T1)
      IF (J.GE.MAXIT) GOTO 999
      IF (ABS(T1-T0).LT.TOL) GOTO 999
      S=VS(Y,N,T)+SCONST
      IF (S.LT.0.) GOTO 50
      T0=T
      S0=S
      GOTO 90
   50 T1=T
      S1=S
   90 CONTINUE
  999 HDLE=T
      NIT=KK
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LIINDSZ(ALPHA,N,K,ALPHA1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.ALPHA.GT.0..AND.ALPHA.LT.1
      IF (.NOT.NPRCHK) CALL MESSGE(500,'LIINDS',1)
C
      GAM=ALPHA
      CALL NQUANT(GAM,X)
      TNUM=FLOAT(N+1)
      TDEN=SQRT(FLOAT(N))
      XI=(TDEN*X+TNUM)/2.
      I=INT(XI)
      IF (ABS(XI-FLOAT(I)).LE.1.E-6) GOTO 50
    5 XI=FLOAT(I)
      X=(2.*XI-TNUM)/TDEN
      CALL GAUSSZ(1,X,PH)
      IF (PH.LT.GAM) GOTO 10
      I=I-1
      GOTO 5
   10 I0=I
      I1=I+1
      XI0=XI
      X0=X
   20 XI1=FLOAT(I1)
      X1=(2.*XI1-TNUM)/TDEN
      CALL GAUSSZ(1,X0,PH0)
      CALL GAUSSZ(1,X1,PH1)
      IF (PH0.LT.GAM.AND.PH1.GE.GAM) GOTO 30
      I0=I0+1
      I1=I0+1
      XI0=FLOAT(I0)
      X0=(2.*XI0-TNUM)/TDEN
      GOTO 20
   30 D0=ABS(PH0-GAM)
      D1=ABS(PH1-GAM)
      IF (D0.LT.D1) GOTO 40
      I=I1
      GAM=PH1
      GOTO 50
   40 I=I0
      GAM=PH0
   50 K=I
      ALPHA1=GAM
      IF (I.GE.1.AND.I.LE.N) RETURN
      K=1
      IF (I.GT.N) K=N
      CALL MESSGE(301,'LIINDS',0)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LIINDHZ(ALPHA,N,K,ALPHA1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR: A. MARAZZI
C.......................................................................
C
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      ALFA=ALPHA
      NPRCHK=ALPHA.GT.0..AND.ALPHA.LT.1..AND.N.GT.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'LIINDH',1)
      M=N*(N+1)/2
C
      EW=FLOAT(M)
      SQ=SQRT(FLOAT(N*(N+1)))*SQRT(FLOAT(2*N+1))
      SQ=SQ/2.449489742
      CALL NQUANT(ALFA,X)
      XI=(X*SQ+1.+EW)/2.
      I=INT(XI)
    5 CONTINUE
      XI=FLOAT(I)
      X=(2.*XI-1.-EW)/SQ
      CALL GAUSSZ(1,X,PH)
      IF (PH.LT.ALFA) GOTO 10
      I=I-1
      GOTO 5
   10 I0=I
      I1=I+1
      XI0=XI
      X0=X
   20 XI1=FLOAT(I1)
      X1=(2.*XI1-1.-EW)/SQ
      CALL GAUSSZ(1,X0,PH0)
      CALL GAUSSZ(1,X1,PH1)
      IF(PH0.LT.ALFA.AND.PH1.GE.ALFA) GOTO 30
      I0=I0+1
      I1=I0+1
      XI0=FLOAT(I0)
      X0=(2.*XI0-1.-EW)/SQ
      GOTO 20
   30 D0=ABS(PH0-ALFA)
      D1=ABS(PH1-ALFA)
      IF(D0.LT.D1) GOTO 40
      I=I1
      ALFA=PH1
      GOTO 50
   40 I=I0
      ALFA=PH0
   50 K=I
      ALPHA1=ALFA
      IF (I.GE.1.AND.I.LE.M) GOTO 999
      K=1
      IF (I.GT.M) K=M
      CALL MESSGE(301,'LIINDH',0)
  999 RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LITTSTZ(X,Y,M,N,ALPHA,DELTA,S1,S2,SIGMA,TL,TU,P)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(M),Y(N)
      DOUBLE PRECISION SUM
C
C PARAMETER CHECK AND INITIALIZATION
C
      IF (M.LE.1.OR.N.LE.1.OR.ALPHA.LE.0.OR.ALPHA.GE.1.)
     + CALL MESSGE(500,'LITTST',1)
C
C  COMPUTE AN ESTIMATE OF DELTA
C
      SUM=0.D0
      DO 100 I=1,M
      SUM=SUM+DBLE(X(I))
  100 CONTINUE
      XBAR=SNGL(SUM)/FLOAT(M)
      SUM=0.D0
      DO 200 I=1,N
      SUM=SUM+DBLE(Y(I))
  200 CONTINUE
      YBAR=SNGL(SUM)/FLOAT(N)
      DELTA=YBAR-XBAR
C
C  COMPUTE S1, S2 AND SIGMA
C
      SUM=0.D0
      DO 300 I=1,M
      RS=X(I)-XBAR
      SUM=SUM+RS*DBLE(RS)
  300 CONTINUE
      DF1=FLOAT(M-1)
      S1=SNGL(SUM)
      SUM=0.D0
      DO 400 I=1,N
      RS=Y(I)-YBAR
      SUM=SUM+RS*DBLE(RS)
  400 CONTINUE
      DF2=FLOAT(N-1)
      S2=SNGL(SUM)
      SIGMA=SQRT((S1+S2)/(DF1+DF2))
      S1=SQRT(S1/DF1)
      S2=SQRT(S2/DF2)
C
C  COMPUTE CONFIDENCE INTERVAL FOR DELTA WITH COEFF. 1-2*ALPHA AND P
C
      IFN=INT(DF1+DF2)
      CALL TQUANTZ(ALPHA,IFN,T)
      EM=FLOAT(M)
      EN=FLOAT(N)
      DEN=SIGMA*SQRT(1./EM+1./EN)
      DEL=ABS(T)*DEN
      TL=DELTA-DEL
      TU=DELTA+DEL
      T=DELTA/DEN
      CALL PROBSTZ(T,IFN,P)
      P=1-P
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LYTAU2(Z,EXPSI,EXPSP,EXCHI,EXRHO,
     +           M,N,MPN,TOL,GAM,ISIGMA,MAXIT,NITMON,
     +           THETAL,DELTAL,THETAS,SIGMAF,FTAU,P,RS1,RS2,COV,
     +           WORK1,WORK2,IWORK)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
      DIMENSION Z(MPN),RS1(MPN),RS2(MPN),COV(3)
      DIMENSION IWORK(2),WORK1(MPN,6),WORK2(8)
      COMMON/BETA/BETA,BET0
      LOGICAL NPRCHK
      EXTERNAL EXPSI,EXPSP,EXCHI,EXRHO
 
      IAS=IABS(ISIGMA)
      NPRCHK=M.GT.1.AND.N.GT.1.AND.MPN.EQ.M+N.AND.TOL.GT.0..AND.
     +       GAM.GT.0..AND.GAM.LT.2..AND.
     +       (IAS.GT.0.AND.IAS.LE.2).AND.MAXIT.GT.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'LYTAU2',1)
 
      CALL LTAUT2(Z,EXPSI,EXPSP,EXCHI,EXRHO,
     +           M,N,MPN,TOL,GAM,ISIGMA,MAXIT,NITMON,
     +           THETAL,DELTAL,THETAS,SIGMAF,FTAU,P,RS1,RS2,COV,
     +           WORK1(1,1),WORK1(1,3),WORK1(1,5),WORK1(1,6),
     +           WORK2(1),WORK2(3),WORK2(5),WORK2(7),IWORK)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LTAUT2(Z,EXPSI,EXPSP,EXCHI,EXRHO,
     +           M,N,MPN,TOL,GAM,ISIGMA,MAXIT,NITMON,
     +           THETAL,DELTAL,THETAS,SIGMAF,FTAU,P,RS1,RS2,COV,
     +           X,SX,SY,THETA,DELTA,SF,SG,SH,SP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
      INTEGER SP(2)
      REAL Z(MPN),COV(3),RS1(MPN),RS2(MPN),X(MPN,2),SX(MPN,2),SY(MPN),
     +     THETA(MPN),DELTA(2),SF(2),SG(2),SH(2)
      LOGICAL NPRCHK
      EXTERNAL EXPSI,EXPSP,EXCHI,EXRHO
      COMMON/BETA/BETA,BET0
 
      IAS=IABS(ISIGMA)
      NPRCHK=M.GT.1.AND.N.GT.1.AND.MPN.EQ.M+N.AND.TOL.GT.0..AND.
     +       GAM.GT.0..AND.GAM.LT.2..AND.
     +       (IAS.GT.0.AND.IAS.LE.2).AND.MAXIT.GT.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'LTAUT2',1)
C
C INITIALIZATION
C
      NP=2
      NCOV=NP*(NP+1)/2
      BET0=.6745
      DO 10 I=1,M
        X(I,1)=1.
        X(I,2)=0.
   10 CONTINUE
      DO 20 I=1,N
        X(I+M,1)=1.
        X(I+M,2)=1.
   20 CONTINUE
      DO 40 I=1,MPN
        SY(I)=Z(I)
        DO 30 J=1,2
        SX(I,J)=X(I,J)
   30 CONTINUE
   40 CONTINUE
C
C  INITIAL VALUES OF THETA AND SIGMA
C
      CALL RILARSZ(SX,SY,MPN,NP,MPN,MPN,TOL,NIT,K,KODE,
     +     SIG0,THETA,RS1,RS2,SF,SG,SH)
      IF (SIG0.LE.1.E-3) SIG0=1.
C
C  COMPUTE COV FOR THE CONVERGENCE CRITERION
C
      UPPER=10.
      CALL LIEPSU(EXPSI,UPPER,TOL,ERR,EPSI2,EPSIP)
      FH=EPSI2/(EPSIP*2)
      EM=FLOAT(M)
      EN=FLOAT(N)
      COV(1)=(EM+EN)*FH/EM
      COV(2)=-COV(1)
      COV(3)=(2.+EM/EN+EN/EM)*FH
C
C  COMPUTE AN HUBER-ESTIMATE OF THETA=(THETAL,DELTAL) AND SIGMA
C
      ITYPE=1
      MAXIS=1
      TAU=1.E-6
      ICNV=1
      PSP0=EXPSP(0)
      CALL RYWALG(X,Z,THETA,RS2,COV,PSP0,EXPSI,EXCHI,EXRHO,SIG0,MPN,
     +     NP,MPN,MPN,NCOV,TOL,GAM,TAU,ITYPE,ISIGMA,ICNV,MAXIT,MAXIS,
     +     NITMON,NIT,SIGMAF,RS1,DELTA,SY,SF,SG,SH,SP,RS2,SX)
      THETAL=THETA(1)
      DELTAL=THETA(2)
C
C  COMPUTE COV. MATRIX OF THETA(1..2) (EMPIRICAL TYPE)
C
      CALL KFFACV(RS1,EXPSI,EXPSP,MPN,NP,SIGMAF,FH)
      FACT=(SIGMAF**2)*FH
      COV(1)=FACT/EM
      COV(2)=-COV(1)
      COV(3)=FACT*(1./EM+1./EN)
C
C  REDUCED MODEL
C
      IISIGM=0
      CALL LYHALG(Z,EXPSI,EXPSP,EXCHI,THETA(1),SIGMAF,MPN,TOL,GAM,
     +     IISIGM,MAXIT,MAXIS,NIT,SIGMAF,VAR,RS2,SY)
      THETAS=THETA(1)
C
C  COMPUTE TAU-TEST SIGNIFICANCE
C
      CALL TFTAUT(RS1,RS2,SY,EXRHO,MPN,2,1,SIGMAF,ITYPE,S0,S1,FTAU)
      CALL CHISQZ(1,1,FTAU*EPSIP/EPSI2,P)
      P=1.-P
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LYMNWTZ(X,Y,M,N,ISORT,K,TOL,MAXIT,NIT,TMNWT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
      DIMENSION X(M),Y(N)
      LOGICAL NPRCHK
      EXTERNAL WXYz
      DATA ZETA/0.0001/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      MN=M*N
      NPRCHK=M.GT.0.AND.N.GT.0.AND.MAXIT.GT.0.AND.TOL.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'LYMNWT',1)
      NPRCHK=K.LT.MN.AND.K.GT.0
      KK=0
      SCONST=FLOAT(K-MN)-ZETA
      IF (ISORT.EQ.0) GOTO 5
      CALL SRT1Z(X,M,1,M)
      CALL SRT1Z(Y,N,1,N)
    5 IF (NPRCHK) GO TO 30
      IF (K.EQ.0) GOTO 10
      IF (K.EQ.MN) GOTO 20
      CALL MESSGE(500,'LYMNWT',1)
   10 T=Y(1)-X(M)-ZETA
      KK=0
      GOTO 999
   20 T=Y(N)-X(1)
      KK=0
      GOTO 999
   30 CONTINUE
C
C  INITIALIZE: FIND T0 AND T1 SUCH THAT WXYz(X,Y,M,N,T0)+K-MN-ZETA.GT.0
C  AND WXYz(X,Y,M,N,T1)+K-MN-ZETA.LT.0.
C
      T0=Y(1)-X(M)-ZETA
      S0=WXYz(X,Y,M,N,T0)+SCONST
      T1=Y(N)-X(1)
      S1=WXYz(X,Y,M,N,T1)+SCONST
C
C  INTERPOLATE AND ITERATE
C
      DO 90 L=1,MAXIT
      KK=L
      IF (L.LE.2) T=T0+(T1-T0)*S0/(S0-S1)
      IF (L.GT.2) T=0.5*(T0+T1)
      IF (L.GE.MAXIT) GOTO 999
      IF (ABS(T1-T0).LT.TOL) GOTO 999
      S=WXYz(X,Y,M,N,T)+SCONST
      IF (S.LT.0.) GOTO 50
      T0=T
      S0=S
      GOTO 90
   50 T1=T
      S1=S
   90 CONTINUE
  999 TMNWT=T
      NIT=KK
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE LIINDWZ(ALPHA,M,N,K,ALPHA1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=M.GT.0.AND.N.GT.0.AND.ALPHA.GT.0..AND.ALPHA.LT.1
      IF (.NOT.NPRCHK) CALL MESSGE(500,'LIINDW',1)
C
      MN=M*N
      GAM=ALPHA
      CALL NQUANT(GAM,X)
      TNUM=FLOAT(M*N+1)
      TDEN=SQRT(FLOAT(M*N)*FLOAT(M+N+1)/3.)
      XI=(TDEN*X+TNUM)/2.
      I=INT(XI)
      IF (ABS(XI-FLOAT(I)).LE.1.E-6) GOTO 50
    5 XI=FLOAT(I)
      X=(2.*XI-TNUM)/TDEN
      CALL GAUSSZ(1,X,PH)
      IF (PH.LT.GAM) GOTO 10
      I=I-1
      GOTO 5
   10 I0=I
      I1=I+1
      XI0=XI
      X0=X
   20 XI1=FLOAT(I1)
      X1=(2.*XI1-TNUM)/TDEN
      CALL GAUSSZ(1,X0,PH0)
      CALL GAUSSZ(1,X1,PH1)
      IF (PH0.LT.GAM.AND.PH1.GE.GAM) GOTO 30
      I0=I0+1
      I1=I0+1
      XI0=FLOAT(I0)
      X0=(2.*XI0-TNUM)/TDEN
      GOTO 20
   30 D0=ABS(PH0-GAM)
      D1=ABS(PH1-GAM)
      IF (D0.LT.D1) GOTO 40
      I=I1
      GAM=PH1
      GOTO 50
   40 I=I0
      GAM=PH0
   50 K=I
      ALPHA1=GAM
      IF (I.GE.1.AND.I.LE.MN) RETURN
      K=1
      IF (I.GT.MN) K=MN
      CALL MESSGE(301,'LIINDW',0)
      RETURN
      END
C***********************************************************************
C***************************  M X A U X I  *****************************
C
      SUBROUTINE NXST(IB,IU,NP,J)
      INTEGER IB(NP),IU(NP)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : A. MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  THIS SUBROUTINE DETERMINES THE NEXT STEP OF A HAMILTONIAN
C  WALK AROUND A UNIT HYPERCUBE OF DIMENSION K. THE VALUE OF THE
C  OUTPUT PARAMETER J CORRESPONDS TO THE DIMENSION ALONG WHICH THE
C  WALK SHOULD PROGRESS AND IS EQUIVALENT TO CHANGING THE STATUS
C  OF THE CORRESPONDING VARIABLE IN THE REGRESSION ANALYSIS.
C  BEFORE FIRST ENTRY THE ARRAYS IB AND IA MUST BE INITIALIZED AS
C  IB(I)=2**(I-1) AND IA(I)=2*IB(I). THE WALK SHOULD BE TERMINATED
C  AFTER 2**(NP-1) STEPS.  SEE M.J.GARSIDE, SOME COMPUTATIONAL PRO-
C  CEDURES FOR THE BEST SUBSET PROBLEM, APPL.STAT.,20,8-15 (1971).
C
      IMIN=IB(NP)
      J=NP
      DO 20 I=1,NP
      IF (IB(I).GE.IMIN) GOTO 20
      IMIN=IB(I)
      J=I
   20 CONTINUE
      IB(J)=IB(J)+IU(J)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE STCP(CP,IP,NC,NC1,CP0,IP0,CMAX,JMAX)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : A. MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  TESTS IF MAXIMUM NUMBER (NC) OF STORED CP VALUES HAS BEEN REACHED.
C  IF SO CURRENT VALUE CP0 OF CP AND POSITION NUMBER IP0 ARE STORED
C  ONLY IF CP0 IS LESS THAN THE PREVIOUS LARGEST VALUE CMAX
C  (STORED IN CP(JMAX)).
C
      DIMENSION CP(NC)
      INTEGER IP(NC)
      NC1=NC1+1
      IF (NC1.GT.NC) GOTO 20
      CP(NC1)=CP0
      IP(NC1)=IP0
      IF (CP0.LE.CMAX) RETURN
      CMAX=CP0
      JMAX=NC1
      RETURN
C
   20 IF (CP0.GE.CMAX) RETURN
      CP(JMAX)=CP0
      IP(JMAX)=IP0
      CMAX=0.
      DO 30 J=1,NC
      IF (CP(J).LE.CMAX) GOTO 30
      CMAX=CP(J)
      JMAX=J
   30 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CLL0(XT,Y,N,NP,MDX,MDT,SIGMA,THETA,RS,SC,SH)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : A. MARAZZI
C.......................................................................
C
C  LS SOLUTION (RESIDUALS AND SIGMA).
C  XT CONTAINS THE TRANSFORMED DESIGN MATRIX.
C  Y CONTAINS ON INPUT THE ORIGINAL RIGHT-HAND SIDE.
C  THIS ROUTINE ASSUMES THAT THE PSEUDORANK OF XT IS NP.
C
      DIMENSION XT(MDX,NP),Y(N),THETA(MDT),RS(N),SC(NP),SH(NP)
C
C  HOUSEHOLDER TRANSFORMATIONS OF THE OBSERVATIONS VECTOR
C
      DO 20 JJ=1,NP
      J=JJ
      CALL H12Z(2,J,J+1,N,XT(1,J),1,SH(J),Y,1,N,1,N)
   20 CONTINUE
C
C  SOLVE THE TRANSFORMED LS-PROBLEM
C
      DO 30 I=1,N
      THETA(I)=Y(I)
   30 CONTINUE
      CALL SOLV(XT,THETA,NP,NP,MDX,MDT)
C
C  COMPUTE THE TRANSFORMED RESIDUAL VECTOR
C
      CALL RES(1,XT,Y,THETA,RS,SC,SC,N,NP,NP,NP,MDX,MDT)
C
C  COMPUTE SIGMA
C
      SIGMA=0.
      IF (NP.EQ.N) GOTO 80
      CALL NRM2Z(RS,N,1,N,SIGMA)
      SIGMA=SIGMA/SQRT(FLOAT(N-NP))
   80 CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
C
C                 R O B E T H  FORTRAN Source
C
C  File MXMAIN.F  Main subroutines of Chapter 9
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MIRTSRZ(X,Y,N,NP,MDX,MDT,NCOV,ITYPE,C,D,TOL,GAM,
     1                  MAXIT,MAXIS,TAU,K,NIT,SIGMA,THETA,COV,T,
     2                  RS,DELTA,SC,SE,SF,SG,SH,IP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   PROGRAMMER : A. MARAZZI
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),THETA(MDT),COV(NCOV),T(NP),RS(N),
     3          DELTA(NP),SC(N),SE(NP),SF(NP),SG(NP),SH(NP)
      INTEGER IP(NP)
      COMMON/PSIPR/IPSI,CC,H1,H2,H3,XK,DD
      LOGICAL NPRCHK
      EXTERNAL PSY,CHI,PSP
C
C  PARAMETER CHECK
C
      NN=NP*(NP+1)/2
      NPRCHK=MIN0(N,NP).GT.0.AND.MDX.GE.N.AND.MDT.GE.MAX0(N,NP)
     1       .AND.NCOV.EQ.NN.AND.(ITYPE.EQ.0.OR.(ITYPE.EQ.1
     2       .AND.TOL.GT.0..AND.C.GT.0..AND.D.GT.0..AND.
     3       (GAM.GT.0..AND.GAM.LT.2.).AND.MAXIT.GT.0.AND.
     4       MAXIS.GT.0)).AND.TAU.GE.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MIRTSR',1)
      IF (ITYPE.EQ.0) GOTO 10
      IPSI=1
      CC=C
      DD=D
   10 CONTINUE
C
C  HOUSEHOLDER TRANSFORMATION OF THE DESIGN MATRIX
C  AND DETERMINATION OF THE PSEUDORANK K
C
      CALL RIMTRFZ(X,N,NP,MDX,1,TAU,K,SF,SG,SH,IP)
C
C  LEAST SQUARES SOLUTION FOR THE COMPLETE MODEL
C
      CALL RICLLSZ(X,Y,N,NP,MDX,MDT,K,0,1,SIGMA,THETA,RS,SC,
     1            SE,SF,SG,SH,IP)
C
C  COMPUTE UNSCALED COVARIANCE MATRIX OF PARAMETER ESTIMATES
C  IN TRANSFORMED COORD. SYSTEM
C
      CALL KIASCVZ(X,K,NP,MDX,NCOV,1.,0.,COV)
      IF (ITYPE.EQ.0) GOTO 50
C
C  COMPUTE AN HUBER ESTIMATE
C
      CALL RIBETHZ(SC,N,D,ITYPE,BETA1)
      CALL LIEPSHZ(C,EPSI2,EPSIP)
      FCTI=EPSI2/(EPSIP**2)
      SIG0=SIGMA
      CALL SCALZ(COV,FCTI,NCOV,1,NCOV)
      CALL RYHALG(X,Y,THETA,SC,COV,PSY,CHI,CHI,SIG0,
     1            N,NP,MDX,MDT,NCOV,K,TOL,GAM,TAU,ITYPE,
     2            0,0,0,1,1,MAXIT,MAXIS,-1,NIT,SIGMA,
     3            RS,SC,DELTA,SC,SE,SF,SG,SH,IP)
      CALL KFFACV(RS,PSY,PSP,N,K,SIGMA,FCTF)
C
C  COMPUTE SCALED COVARIANCE MATRIX OF PARAMETER
C  ESTIMATES IN ORIGINAL COORD. SYSTEM
C
   50 IF (ITYPE.EQ.0) SCLF=SIGMA**2
      IF (ITYPE.EQ.1) SCLF=FCTF/FCTI
      CALL KFASCVZ(X,COV,K,NP,MDX,NCOV,SCLF,SE,SG,IP)
C
C  COMPUTE T-VALUES
C
      L=0
      DO 200 J=1,NP
      L=L+J
      T(J)=-10.
      IF (COV(L).GT.0) T(J)=ABS(THETA(J)/SQRT(COV(L)))
  200 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MFRAGRZ(X,Y,VP,N,NP,MDX,NCOV,NC,ITYPE,C,TOL,GAM,
     1                  MAXIT,SIGMAC,SIGMAR,CPC,CPR,IPC,IPR,
     2                  SC1,SC2,SC3,SC4,SC5,SC6,SC7,IA,IB,IC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   PROGRAMMER : A. MARAZZI
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),VP(NP),CPC(NC),CPR(NC),
     1          SC1(N),SC2(N),SC3(N),SC4(N),SC5(NP),SC6(NP),SC7(NCOV)
      INTEGER IA(NP),IB(NP),IC(NP),IPC(NC),IPR(NC)
      COMMON/PSIPR/IPSI,CC,H1,H2,H3,XK,DD
      LOGICAL NPRCHK
      EXTERNAL PSY,RHO
C
C  PARAMETER CHECK
C
      NN=NP*(NP+1)/2
      NPRCHK=N.GE.NP.AND.NP.GT.0.AND.MDX.GE.N.AND.NN.EQ.NCOV
     1       .AND.NC.GT.0.AND.NC.LT.2**NP.AND.(ITYPE.EQ.0.OR.
     2       (ITYPE.EQ.1.AND.C.GT.0..AND.TOL.GT.0.
     3       .AND.(GAM.GT.0..AND.GAM.LT.2.).AND.MAXIT.GT.0
     4       .AND.SIGMAR.GT.0.)).AND.SIGMAC.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MFRAGR',1)
C
C  INITIALIZATION
C
      ALPHA=0.0
      IF (ITYPE.EQ.0) GOTO 10
      IPSI=1
      CC=C
      CALL LIEPSHZ(C,EPSI2,EPSIP)
      FCTI=EPSI2/(EPSIP**2)
      ALPHA=2.*EPSI2/EPSIP
   10 CONTINUE
      N0=0
      L=0
      IP0=0
      NC1R=0
      NC1C=0
      CMAXR=0
      CMAXC=0
      DO 40 J=1,NP
      IC(J)=J
      IB(J)=0
      IF (VP(J).GT.-1..AND.VP(J).LE.0.) N0=N0+1
   40 CONTINUE
      IBMAX=2**N0
      DO 50 I=1,N
      SC1(I)=Y(I)
   50 CONTINUE
      SIGMC2=SIGMAC*SIGMAC
C
C  INTRODUCE VARIABLES WITH POSITIVE PRIORITY
C  ------------------------------------------
C
   60 VPMAX=0.
      JINC=0
      DO 70 J=1,NP
      IF (VP(J).LE.VPMAX.OR.IB(J).EQ.IBMAX) GOTO 70
      VPMAX=VP(J)
      JINC=J
   70 CONTINUE
      IF (JINC.EQ.0) GOTO 100
      IB(JINC)=IBMAX
C
C  LOCATE POSITION OF VARIABLE JINC
C
      DO 80 J=1,NP
      IF (IC(J).EQ.JINC) JPOS=J
   80 CONTINUE
C
C  INTRODUCE VARIABLE JINC (POSITION JPOS)
C
      CALL ADDCoL(X,N,NP,MDX,L,JPOS,SC5,IC,SC4)
      IP0=IP0+2**(JINC-1)
C
C  COMPUTE CLASSICAL CP
C
      CALL CLL0(X,Y,N,L,MDX,N,SIG,SC2,SC3,SC4,SC5)
      CP0=SIG**2.*FLOAT(N-L)/SIGMC2+FLOAT(2*L-N)
      CALL STCP(CPC,IPC,NC,NC1C,CP0,IP0,CMAXC,JMAXC)
      IF (ITYPE.EQ.0) GOTO 87
C
C  COMPUTE ROBUST CP
C
      LCOV=L*(L+1)/2
      CALL KIASCVZ(X,L,L,MDX,LCOV,FCTI,0.,SC7)
      CALL HALG(1,X,Y,SC2,SC4,SC4,SC7,SC3,SC6,SIGMAR,0,N,L,
     1          MDX,N,LCOV,L,TOL,GAM,MAXIT,MAXIS,-1,1,NIT,
     2          PSY,RHO,RHO,SC4,SC4,SC4,SC4,SC5)
      CALL RES(2,X,Y,SC2,SC3,SC7,SC4,N,L,L,NCOV,MDX,N)
      DO 85 J1=1,L
      J=L-J1+1
      CALL H12Z(2,J,J+1,N,X(1,J),1,SC5(J),SC3,1,N,1,N)
   85 CONTINUE
      CALL QRSS(SC3,SC4,SC4,RHO,N,ITYPE,SIGMAR,0.,QR)
      IP1=IP0
      CP1=2.*QR/SIGMAR+ALPHA*FLOAT(L)-FLOAT(N)
      CALL STCP(CPR,IPR,NC,NC1R,CP1,IP1,CMAXR,JMAXR)
   87 CONTINUE
C
C  REGENERATE Y
C
      DO 90 I=1,N
      Y(I)=SC1(I)
   90 CONTINUE
      GOTO 60
C
C  INITIALIZE NEXT STEP PROCEDURE
C  ------------------------------
C
  100 CONTINUE
      JB=1
      DO 120 J=1,NP
      IF (VP(J).GT.-1..AND.VP(J).LE.0.) GOTO 110
      IB(J)=IBMAX
      GOTO 120
  110 IB(J)=JB
      JB=2*JB
      IA(J)=2*IB(J)
  120 CONTINUE
C
C  ADD AND REMOVE OPTIONAL VARIABLES
C  ---------------------------------
C
      NSTP=JB-1
      IF (NSTP.EQ.0) RETURN
      DO 200 ISTP=1,NSTP
      CALL NXST(IB,IA,NP,JINC)
C
C  LOCATE POSITION OF VARIABLE JINC
C
      DO 130 J=1,NP
      IF (IC(J).EQ.JINC) JPOS=J
  130 CONTINUE
C
C  ADD OR REMOVE VARIABLE JINC (POSITION JPOS)
C
      IF (JPOS.LE.L) GOTO 140
      CALL ADDCoL(X,N,NP,MDX,L,JPOS,SC5,IC,SC4)
      IP0=IP0+2**(JINC-1)
      GOTO 150
  140 CALL RMVCZ(X,N,NP,MDX,L,JPOS,SC5,IC,SC4)
      IP0=IP0-2**(JINC-1)
C
C  COMPUTE CLASSICAL CP
C
  150 CALL CLL0(X,Y,N,L,MDX,N,SIG,SC2,SC3,SC4,SC5)
      CP0=SIG**2.*FLOAT(N-L)/SIGMC2+FLOAT(2*L-N)
      CALL STCP(CPC,IPC,NC,NC1C,CP0,IP0,CMAXC,JMAXC)
      IF (ITYPE.EQ.0) GOTO 157
C
C  COMPUTE ROBUST CP
C
      LCOV=L*(L+1)/2
      CALL KIASCVZ(X,L,L,MDX,LCOV,FCTI,0.,SC7)
      CALL HALG(1,X,Y,SC2,SC4,SC4,SC7,SC3,SC6,SIGMAR,0,N,L,
     1          MDX,N,LCOV,L,TOL,GAM,MAXIT,MAXIS,-1,1,NIT,
     2          PSY,RHO,RHO,SC4,SC4,SC4,SC4,SC5)
      CALL RES(2,X,Y,SC2,SC3,SC7,SC4,N,L,L,NCOV,MDX,N)
      DO 155 J1=1,L
      J=L-J1+1
      CALL H12Z(2,J,J+1,N,X(1,J),1,SC5(J),SC3,1,N,1,N)
  155 CONTINUE
      CALL QRSS(SC3,SC4,SC4,RHO,N,ITYPE,SIGMAR,0.,QR)
      IP1=IP0
      CP1=2.*QR/SIGMAR+ALPHA*FLOAT(L)-FLOAT(N)
      CALL STCP(CPR,IPR,NC,NC1R,CP1,IP1,CMAXR,JMAXR)
  157 CONTINUE
C
C  REGENERATE Y
C
      DO 160 I=1,N
      Y(I)=SC1(I)
  160 CONTINUE
  200 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MYMVLMZ(X,Y,N,NP,NQ,NCOV,MDX,MDW,MDI,ILMS,IOPT,INTCH,
     1           NREP,TOLV,TOLM,TAU,ISEED,IERR,XVOL,XMIN,COV,T,THETA,
     2           RS,D,ITV,ITM,WORK,IWORK)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   PROGRAMMERS : A. MARAZZI / J. JOSS / A. RANDRIAMIHARISOA
C.......................................................................
C
      LOGICAL NPRCHK
      DIMENSION X(MDX,NP),Y(N),COV(NCOV),T(NP),THETA(NQ),RS(N),D(N),
     +          ITV(NQ),ITM(NQ),WORK(MDW),IWORK(MDI)
      NP1=NP+1
      NP2=NP1+NP
      NN=NP*(NP+1)/2
      NYY=NP1*NP+1
      NSF=NYY+NP1
      NSG=NSF+NP1
      NSH=NSG+NP
      NXM=NSH+NP
      NSD=NXM+NP
      NSZ=NSD+N
      MINW=(NP+6)*NP+2*N+2
      MINI=3*NP+1
      NPRCHK=NP.GT.0.AND.2*NP.LT.N.AND.NQ.EQ.NP1.AND.NCOV.EQ.NN
     +       .AND.MDX.GE.N.AND.MDW.GE.MINW.AND.MDI.GE.MINI.AND.
     +       (ILMS.EQ.0.OR.ILMS.EQ.1).AND.IOPT.GE.0.AND.(IOPT.NE.2
     +       .OR.NREP.GT.0.).AND.IOPT.LE.3.AND.TAU.GE.0..AND.
     +       (INTCH.EQ.0.OR.INTCH.EQ.1).AND.TOLM.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MYMVLM',1)
      CALL MMVLM2(X,Y,N,NP,NQ,NCOV,MDX,ILMS,IOPT,INTCH,NREP,TOLV,TOLM,
     +     TAU,ISEED,IERR,XVOL,XMIN,COV,T,THETA,RS,D,ITV,ITM,
     +     WORK(1),WORK(NYY),WORK(NSF),WORK(NSG),WORK(NSH),WORK(NXM),
     +     WORK(NSD),WORK(NSZ),IWORK(1),IWORK(NP1),IWORK(NP2))
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MMVLM2(X,Y,N,NP,NQ,NCOV,MDX,ILMS,IOPT,INTCH,NREP,TOLV,
     1           TOLM,TAU,ISEED,IERR,XVOL,XMIN,COV,T,THETA,RS,D,ITV,ITM,
     2           XX,YY,SF,SG,SH,XM,SD,SZ,SP,STP,IT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   PROGRAMMERS : A. MARAZZI / A. JOSS / A.RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),COV(NCOV),T(NP),THETA(NQ),RS(N),D(N),
     +          XX(NQ,NP),YY(NQ),SF(NQ),SG(NP),SH(NP),XM(NP),SD(N),SZ(N)
      INTEGER ITV(NQ),ITM(NQ),SP(NP),STP(NP),IT(NQ),H,EL
      LOGICAL NPRCHK,NOTCHG
      EXTERNAL ICNREP,XEXP
      NN=NP*(NP+1)/2
      NPRCHK=N.GT.0.AND.NP.GT.0.AND.NQ.EQ.NP+1.AND.NCOV.EQ.NN.AND.
     +       MDX.GE.N.AND.(ILMS.EQ.0.OR.ILMS.EQ.1).AND.IOPT.GE.0.AND.
     +       IOPT.LE.3.AND.(IOPT.NE.2.OR.NREP.GT.0).AND.(INTCH.EQ.0
     +       .OR.INTCH.EQ.1).AND.TOLM.GT.0..AND.TAU.GE.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'MMVLM2',1)
C
C STEP 0
C ------
      N2=N/2
      N2P=N-N2
      K1=N2+1
      NK1=N-K1+1
      P=FLOAT(NP)
      Q=FLOAT(NQ)
      H=(N+NP+1)/2
      HN=FLOAT(H)/FLOAT(N)
      N1H=N+1-H
      CNP2=(1.+15./FLOAT(N-NP))**2
      IERR=2
      EM2=0.
      NIT=1
      IM=ILMS
      IV=1
      XVOL=0.
      XMIN=0.
      DO 10 I=1,NP
      SP(I)=I
   10 CONTINUE
      IF (IOPT.NE.2) NREP=ICNREP(N,NQ,IOPT,0)
C
C Check for a constant term
C
      NC=0
      DO 25 J=1,NP
        X1J=X(1,J)
        DO 20 I=2,N
          IF (X(I,J).NE.X1J) GOTO 25
   20   CONTINUE
        NC=J
        GOTO 30
   25 CONTINUE
   30 IF (NC.NE.0) CALL MESSGE(150,'MMVLM2',0)
C
C STEP 1 : DRAW RANDOM SAMPLE
C ------
  100 IF (IOPT.NE.3) THEN
        DO 130 K=1,NQ
  110     CALL RANDOW(ISEED,RND)
          ITK=INT(RND*N)+1
          IF (ITK.GT.N) ITK=N
          DO 120 KK=1,K-1
          IF (ITK.EQ.IT(KK)) GOTO 110
  120     CONTINUE
          IT(K)=ITK
  130   CONTINUE
      ELSE
        IF (NIT.EQ.1) THEN
          DO 140 K=1,NQ
          IT(K)=K
  140     CONTINUE
        ELSE
          CALL NCOMB(N,NQ,IT)
        ENDIF
      ENDIF
C
C MAKE SAMPLE X-MATRIX
C
      DO 160 K=1,NQ
      ITK=IT(K)
      DO 150 L=1,NP
      XX(K,L)=X(ITK,L)
  150 CONTINUE
      IF (IM.EQ.1) YY(K)=Y(ITK)
  160 CONTINUE
C
C STEP 2 : COMPUTE MEANS AND SUBTRACT
C ------
      DO 230 K=1,NP
        S=0.
        DO 210 L=1,NQ
        S=S+XX(L,K)
  210   CONTINUE
        S=S/Q
        XM(K)=S
        DO 220 L=1,NQ
        XX(L,K)=XX(L,K)-S
  220   CONTINUE
  230 CONTINUE
      IF (IM.EQ.0) GOTO 300
      YM=0.
      DO 240 L=1,NQ
      YM=YM+YY(L)
  240 CONTINUE
      YM=YM/Q
      DO 250 L=1,NQ
      YY(L)=YY(L)-YM
  250 CONTINUE
C
C STEP 3 : DECOMPOSE SAMPLE MATRIX
C ------
  300 CALL RIMTRFZ(XX,NQ,NP,NQ,INTCH,TAU,KK,SF,SG,SH,SP)
      IF (KK.NE.NP) GOTO 1200
C
C STEP 4 : SOLVE SYSTEM OF LINEAR EQUATIONS
C ------
      IF (IM.EQ.0) GOTO 800
      CALL RICLL1(XX,YY,NQ,NP,NQ,SF,SH,SP)
      B=YM
      DO 410 L=1,NP
      B=B-XM(L)*SF(L)
  410 CONTINUE
C
C STEP 5 : COMPUTE RESIDUALS
C ------
      EL=0
      DO 520 I=1,N
      RI=Y(I)-B
      DO 510 J=1,NP
      RI=RI-SF(J)*X(I,J)
  510 CONTINUE
      ARI=ABS(RI)
      IF (ARI.GT.XMIN) EL=EL+1
      IF (XMIN.NE.0.AND.EL.GE.NK1) GOTO 800
      SZ(I)=ARI
  520 CONTINUE
C
C STEP 6 : COMPUTE THE K-TH ORDER STATISTIC OF THE |RS(I)|
C ------
      CALL FSTORDZ(SZ,N,K1,XRES)
C
C STEP 7 : UPDATE BEST FIT FOR XMIN
C ------
      IF (XMIN.NE.0. .AND. XRES.GE.XMIN) GOTO 800
      IERR=0
      XMIN=XRES
      DO 710 K=1,NP
      THETA(K)=SF(K)
  710 CONTINUE
      THETA(NQ)=B
      DO 720 K=1,NQ
      ITM(K)=IT(K)
  720 CONTINUE
      IF (XRES .LE. TOLM) THEN
        IERR=1
        IM=0
      ENDIF
C
C STEP 8 : COMPUTE THE LOGARITHM OF THE DETERMINANTE AND G
C ------
  800 IF (IV.EQ.0) GOTO 1200
      DET=0.
      DO 810 K=1,NP
      DET=DET+ALOG(ABS(XX(K,K)))
  810 CONTINUE
      DET=2.*DET
      G=XEXP((XVOL-DET)/P)
C
C STEP 9 : COMPUTE THE DISTANCES Di^2
C ------
      EL=0
      DO 930 K=1,N
        DO 910 L=1,NP
        YY(L)=X(K,L)-XM(L)
  910   CONTINUE
        CALL MYP(YY,SP,NP)
        CALL SOLVT(XX,YY,NP,NP,NQ,NP)
        AI2=0.
        DO 920 L=1,NP
        AI2=AI2+YY(L)*YY(L)
  920   CONTINUE
        SZ(K)=AI2
        IF (AI2.GT.G) EL=EL+1
        IF (XVOL.NE.0..AND.EL.GE.N1H) GOTO 1200
        SD(K)=AI2
  930 CONTINUE
C
C STEP 10 : COMPUTE H-TH ORDER OF THE AI2 and VOLUME
C -------
      CALL FSTORDZ(SZ,N,H,DIST)
      VOL=DET+P*ALOG(DIST)
C
C STEP 11 : UPDATE BEST FIT FOR XVOL
C -------
      IF (XVOL.NE.0..AND.XVOL.LT.VOL) GOTO 1200
      XVOL=VOL
      EM2=DIST
      IERR=0
      J=0
      DO 1130 L=1,NP
        DO 1110 K=L,NP
          J=J+1
          COV(J)=XX(L,K)
 1110   CONTINUE
        T(L)=XM(L)
        STP(L)=SP(L)
 1130 CONTINUE
      DO 1140 I=1,N
      D(I)=SD(I)
 1140 CONTINUE 
      DO 1150 K=1,NQ
      ITV(K)=IT(K)
 1150 CONTINUE 
      IF (XVOL.LT.TOLV) THEN
        IERR=1
        IV=0
      ENDIF
C
C Step 12 : END OF MAIN LOOP
C ------
 1200 IF (IM.EQ.0.AND.IV.EQ.0) GOTO 1300
      IF (NIT.EQ.NREP) GOTO 1300
      NIT=NIT+1
      GOTO 100
C
C STEP 13 : REFINEMENT
C ------
 1300 IF (ILMS.EQ.0) GOTO 1400
      DO 1320 I=1,N
      S=Y(I)-THETA(NQ)
      DO 1310 J=1,NP
      S=S-THETA(J)*X(I,J)
 1310 CONTINUE
      RS(I)=S
 1320 CONTINUE 
      CALL LMSADJ(N,N2,N2P,1,THETA(NQ),RS,XMIN,SZ)
C
C STEP 14 : EXIT. COMPUTE COV AND D.
C ------
 1400 IF (NIT.LT.NREP) IERR=1
      IF (IERR.EQ.2) RETURN
      NOTCHG=.TRUE.
      IF (INTCH.EQ.0) GOTO 1415
C
C Compute R*P**T
C
      DO 1405 K=1,NP
      SP(K)=K
 1405 CONTINUE
      DO 1410 K=1,NP
      L=STP(K)
      IF (K.EQ.L) GOTO 1410
      NOTCHG=.FALSE.
      J=SP(K)
      SP(K)=SP(L)
      SP(L)=J
 1410 CONTINUE
 1415 J=0
      DO 1430 L=1,NP
        DO 1420 K=L,NP
          J=J+1
          XX(L,K)=COV(J)
          IF (K.NE.L) XX(K,L)=0.
 1420   CONTINUE
 1430 CONTINUE
      IF (.NOT.NOTCHG) CALL PERMCZ(XX,SP,NP,NP,NQ,2)
C
C Compute COV=(P*R**T)*(R*P**T)
C
      J=0
      DO 1460 K=1,NP
      DO 1450 L=1,K
        S=0.
        DO 1440 I=1,NP
        S=S+XX(I,K)*XX(I,L)
 1440   CONTINUE
        J=J+1
        COV(J)=S
 1450 CONTINUE
 1460 CONTINUE
      CALL CQUANTZ(HN,NP,0.5E-5,20,XCHI)
      FACT=CNP2*EM2/XCHI
      IF (ILMS.EQ.0) THETA(1)=FACT
      CALL SCALZ(COV,FACT,NCOV,1,NCOV)
      DO 1470 I=1,N
      D(I)=SQRT(D(I)/FACT)
 1470 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MYHBHEZ(X,Y,N,NP,NCOV,MDX,MDW,MDI,ISEED,IERR,
     *           SIGM0,SIGM1,THETA0,THETA1,TBIAS,RS0,RS1,IT1,COV,
     *           WORK,IWORK)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   PROGRAMMERS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),THETA0(NP),THETA1(NP),RS0(N),RS1(N),
     *          COV(NCOV),WORK(MDW)
      INTEGER IT1(NP),IWORK(MDI)
      DATA TL/1.E-8/
      NQ=NP
      MINI=NP+NQ
      NP1=NP+1
      MINW=(MDX+NQ+3)*NP+2*N+NQ
      NN=NP*(NP+1)/2
      IF (N.LE.0 .OR. NP.LE.0 .OR. MDX.LT.N .OR.
     *  NCOV.NE.NN .OR. MDW.LT.MINW .OR. MDI.LT.MINI)
     * CALL MESSGE(500,'MYHBHE',1)
      NYY=NP*NQ+1
      NXT=NYY+NQ
      NSF=NXT+N
      NSG=NSF+NP
      NSH=NSG+NP
      NSX=NSH+NP
      NSZ=NSX+MDX*NP
      CALL MHBHE2(X,Y,N,NP,NQ,NCOV,MDX,TL,ISEED,IERR,SIGM0,SIGM1,
     *            THETA0,THETA1,TBIAS,RS0,RS1,IT1,COV,
     *            WORK(1),WORK(NYY),WORK(NXT),WORK(NSF),WORK(NSG),
     *            WORK(NSH),WORK(NSX),WORK(NSZ),IWORK(1),IWORK(NP1))
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MHBHE2(X,Y,N,NP,NQ,NCOV,MDX,TL,ISEED,IERR,SIGM0,SIGM1,
     *           THETA0,THETA1,TBIAS,RS0,RS1,IT1,COV,XX,YY,XTHETA,
     *           SF,SG,SH,SX,SZ,SP,IT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   PROGRAMMERS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),THETA0(NP),THETA1(NP),RS0(N),RS1(N)
      DIMENSION COV(NCOV),XX(NQ,NP),YY(NQ),XTHETA(N),SF(NP),SG(NP)
      DIMENSION SH(NP),SX(MDX,NP),SZ(N)
      INTEGER IT1(NQ),SP(NP),IT(NQ)
      EXTERNAL PSY,PSP,CHI,RHO
      COMMON/BETA/BETA,BET0,/PSIPR/IPSI,C,H1,H2,H3,PK,D
C
      NN=NP*(NP+1)/2
      IF (N.LE.0 .OR. NP.LE.0 .OR. NQ.LT.NP .OR. NCOV.NE.NN
     *  .OR. MDX.LT.N) CALL MESSGE(500,'MHBHE2',1)
C
C  Initial coefficient and scale estimates with high breakdown point
C
      IOPT=2
      INTCH=1
      CALL NLGMZ(2*(N+1),GN)
      CALL NLGMZ(2*(N-NQ+1),GNMQ)
      CALL NLGMZ(2*(NQ+1),GQ)
      NREP=MAX0(INT(4.6*(2.**NP)),1000)
      AREP=ALOG(4.6*(2.**NP)+1000.)
      IF (AREP.GE.GN-GNMQ-GQ) IOPT=3
      TOLS=0.001
      TOLR=0.001
      TAU=0.
      GAM=1.0
      MAXIT=100
      MAXS1=100
      MAXS2=1
      EN=FLOAT(N)
      BETA=0.5
      IPSI=4
      PK=1.5477
      CALL HSEST2(X,Y,N,NP,NQ,NCOV,MDX,IOPT,INTCH,NREP,TOLS,TOLR,
     +            TAU,GAM,MAXIT,MAXS1,MAXS2,PSY,PSP,CHI,ISEED,IERR,
     +            SIGM0,XTHETA,RS0,IT1,COV,XX,YY,RS1,
     +            SF,SG,SH,SX,SZ,SP,IT)
      DO 100 K=1,NP
      THETA0(K)=XTHETA(K)
 100  CONTINUE
      IF (SIGM0.LT.TL) RETURN
C
C  Covariance matrix of the estimated coefficients
C
      PK=4.6873
      SWI=0.
      DO 230 I=1,N
      WI=0.
      IF (RS0(I).EQ.0.) GOTO 210
      T=RS0(I)/SIGM0
      WI=PSY(T)/T
      SWI=SWI+WI
      WI=SQRT(WI)
  210 DO 220 J=1,NP
      SX(I,J)=WI*X(I,J)
  220 CONTINUE
  230 CONTINUE
      CALL KFFACV(RS0,PSY,PSP,N,NP,SIGM0,FH)
      FACT=FH*SWI
      CALL KTASKVZ(SX,N,NP,MDX,NCOV,TAU,FACT,XX,COV)
C
C  Final coefficient estimate
C
      PSP0=PSP(0.)
      ITYPE=1
      ISIGMA=0
      ICNV=1
      MAXIS=1
      NITMON=0
      CALL RYWALG(X,Y,XTHETA,SZ,COV,PSP0,PSY,CHI,RHO,SIGM0,
     + N,NP,MDX,MDX,NCOV,TOLR,GAM,TAU,ITYPE,ISIGMA,ICNV,MAXIT,
     + MAXIS,NITMON,NIT2,SIGM0,RS1,YY,SZ,SF,SG,SH,SP,SZ,SX)
      DO 240 K=1,NP
      THETA1(K)=XTHETA(K)
  240 CONTINUE
      CALL QRSSH(RS0,RHO,N,NP,SIGM0,QMM0)
      CALL QRSSH(RS1,RHO,N,NP,SIGM0,QMM1)
      IF (QMM1.GT.QMM0) CALL MESSGE(101,'MHBHE2',0)
      IF (NIT2.EQ.MAXIT) CALL MESSGE(102,'MHBHE2',0)
      S1P=0.
      DO 410 I=1,N
        RI=RS0(I)/SIGM0
        XTHETA(I)=RI
        S1P=S1P+PSP(RI)
        SZ(I)=PSY(RI)
  410 CONTINUE
      S1P=S1P/EN
C
C  Final scale estimate and COV matrix
C
      PK=1.5477
      ISIGMA=1
      TOL=0.001
      MAXIS=100
      CALL RYSIGM(RS1,SX(1,1),CHI,SIGM0,N,NP,TOL,ITYPE,ISIGMA,
     +            MAXIS,NIT3,SIGM1,SX(1,1),SX(1,1))
      IF (NIT3.EQ.MAXIS) CALL MESSGE(103,'MHBHE2',0)
      FACT=SIGM0**2/FLOAT(N)
      CALL SCALZ(COV,FACT,NCOV,1,NCOV)
C
C  Test for bias
C
      S0P=0.
      S0R=0.
      DO 420 I=1,N
        RI=XTHETA(I)
        S0P=S0P+PSP(RI)
        S0R=S0R+PSY(RI)*RI
        XTHETA(I)=PSY(RI)
  420 CONTINUE
      S0P=S0P/EN
      S0R=S0R*SIGM0/EN
      IF (S0R.LT.TL.OR.S0P.LT.TL.OR.S1P.LT.TL) GOTO 500
      V0=S0P/S0R
      IF (V0.LT.TL) GOTO 500
      D2=0
      DO 430 I=1,N
      D2=D2+(SZ(I)/S1P - XTHETA(I)/S0P)**2
  430 CONTINUE
      D2=D2/EN
      IF (D2.LT.TL) GOTO 500
      TBIAS=2.*EN*(SIGM1-SIGM0)/(V0*D2*SIGM0*SIGM0)
      CALL CQUANTZ(0.95,NP,0.5E-5,20,XCHI)
      IF (TBIAS.GT.XCHI) CALL MESSGE(101,'MHBHE2',0)
      RETURN
  500 CALL MESSGE(401,'MHBHE2',0)
      RETURN
      END
C***********************************************************************
C********************* INTEGRATION SUBROUTINES *************************
C
      SUBROUTINE INTGRD(F,FARR,N,FEXT,GEXT,A,B,EPSABS,EPSREL,KEY,LIMIT,
     1           RESULT,ABSERR,NEVAL,IER,WORK,IWORK)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : QUADPACK
C                ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION A,ABSERR,B,EPSABS,EPSREL,F,RESULT,FEXT,WORK
      INTEGER IER,KEY,LAST,LIMIT,NEVAL,ALIST,BLIST,ELIST,RLIST
C
      DIMENSION FARR(N),WORK(4*LIMIT),IWORK(LIMIT)
C
      EXTERNAL F,FEXT,GEXT
C
C         LIMIT IS THE MAXIMUM NUMBER OF SUBINTERVALS ALLOWED IN THE
C         SUBDIVISION PROCESS OF QAGE1D. TAKE CARE THAT LIMIT.GE.1.
C
C**** DATA LIMIT/500/
C
      IF ((EPSABS.LT.0..AND.EPSREL.LT.0.).OR.LIMIT.LE.1
     1   .OR.LIMIT.GT.500)  CALL MESSGE(500,'INTGRD',1)
      ALIST=1
      BLIST=ALIST+LIMIT
      RLIST=BLIST+LIMIT
      ELIST=RLIST+LIMIT
      CALL QAGE1D(F,FARR,N,FEXT,GEXT,A,B,EPSABS,EPSREL,KEY,LIMIT,
     1     RESULT,ABSERR,NEVAL,IER,
     2     WORK,WORK(BLIST),WORK(RLIST),WORK(ELIST),IWORK,LAST)
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE QAGE1D(F,FARR,N,FEXT,GEXT,A,B,EPSABS,EPSREL,KEY,LIMIT,
     *  RESULT,ABSERR,NEVAL,IER,ALIST,BLIST,RLIST,ELIST,IORD,LAST)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : QUADPACK
C                ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION A,ABSERR,ALIST,AREA,AREA1,AREA12,AREA2,A1,A2,B,
     *  BLIST,B1,B2,C,DABS,DEFABS,DEFAB1,DEFAB2,DMAX1,ELIST,EPMACH,
     *  EPSABS,EPSREL,ERRBND,ERRMAX,ERROR1,ERROR2,ERRO12,ERRSUM,F,OFLOW,
     *  RESABS,RESULT,RLIST,UFLOW,FEXT
      INTEGER IER,IORD,IROFF1,IROFF2,K,KEY,KEYF,LAST,LIMIT,MAXERR,NEVAL,
     *  NRMAX
C
      DIMENSION ALIST(LIMIT),BLIST(LIMIT),ELIST(LIMIT),IORD(LIMIT),
     *  RLIST(LIMIT),FARR(N)
C
      EXTERNAL F,FEXT,GEXT
C
C            LIST OF MAJOR VARIABLES
C            -----------------------
C
C           ALIST     - LIST OF LEFT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           BLIST     - LIST OF RIGHT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           RLIST(I)  - APPROXIMATION TO THE INTEGRAL OVER
C                      (ALIST(I),BLIST(I))
C           ELIST(I)  - ERROR ESTIMATE APPLYING TO RLIST(I)
C           MAXERR    - POINTER TO THE INTERVAL WITH LARGEST
C                       ERROR ESTIMATE
C           ERRMAX    - ELIST(MAXERR)
C           AREA      - SUM OF THE INTEGRALS OVER THE SUBINTERVALS
C           ERRSUM    - SUM OF THE ERRORS OVER THE SUBINTERVALS
C           ERRBND    - REQUESTED ACCURACY MAX(EPSABS,EPSREL*
C                       ABS(RESULT))
C           *****1    - VARIABLE FOR THE LEFT SUBINTERVAL
C           *****2    - VARIABLE FOR THE RIGHT SUBINTERVAL
C           LAST      - INDEX FOR SUBDIVISION
C
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH  IS THE LARGEST RELATIVE SPACING.
C           UFLOW  IS THE SMALLEST POSITIVE MAGNITUDE.
C           OFLOW  IS THE LARGEST MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENTS
      CALL MACHZD(7,EPMACH)
      CALL MACHZD(4,UFLOW)
      CALL MACHZD(6,OFLOW)
C
C           TEST ON VALIDITY OF PARAMETERS
C           ------------------------------
C
      NEVAL = 0
      LAST = 0
      RESULT = 0.0D+00
      ABSERR = 0.0D+00
      ALIST(1) = A
      BLIST(1) = B
      RLIST(1) = 0.0D+00
      ELIST(1) = 0.0D+00
      IORD(1) = 0
      IER=6
      IF ((EPSABS.LT.0..AND.EPSREL.LT.0.).OR.LIMIT.LE.1
     1   .OR.LIMIT.GT.500)  CALL MESSGE(500,'QAGE1D',1)
      IER = 0
C
C           FIRST APPROXIMATION TO THE INTEGRAL
C           -----------------------------------
C
      KEYF = KEY
      IF(KEY.LE.0) KEYF = 1
      IF(KEY.GE.7) KEYF = 6
      C = KEYF
      NEVAL = 0
      IF (KEYF.EQ.1)
     *  CALL Q1K15D(F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,DEFABS,RESABS)
CC    IF (KEYF.EQ.2)
CC   *  CALL Q1K21D(F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,DEFABS,RESABS)
CC    IF (KEYF.EQ.3)
CC   *  CALL Q1K31D(F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,DEFABS,RESABS)
CC    IF (KEYF.EQ.4)
CC   *  CALL Q1K41D(F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,DEFABS,RESABS)
CC    IF (KEYF.EQ.5)
CC   *  CALL Q1K51D(F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,DEFABS,RESABS)
CC    IF (KEYF.EQ.6)
CC   *  CALL Q1K61D(F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,DEFABS,RESABS)
      LAST = 1
      RLIST(1) = RESULT
      ELIST(1) = ABSERR
      IORD(1) = 1
C
C           TEST ON ACCURACY.
C
      ERRBND = DMAX1(EPSABS,EPSREL*DABS(RESULT))
      IF(ABSERR.LE.5.0D+01*EPMACH*DEFABS.AND.ABSERR.GT.ERRBND) IER = 2
      IF(LIMIT.EQ.1) IER = 1
      IF(IER.NE.0.OR.(ABSERR.LE.ERRBND.AND.ABSERR.NE.RESABS)
     *  .OR.ABSERR.EQ.0.0D+00) GO TO 60
C
C           INITIALIZATION
C           --------------
C
C
      ERRMAX = ABSERR
      MAXERR = 1
      AREA = RESULT
      ERRSUM = ABSERR
      NRMAX = 1
      IROFF1 = 0
      IROFF2 = 0
C
C           MAIN DO-LOOP
C           ------------
C
      DO 30 LAST = 2,LIMIT
C
C           BISECT THE SUBINTERVAL WITH THE LARGEST ERROR ESTIMATE.
C
        A1 = ALIST(MAXERR)
        B1 = 5.0D-01*(ALIST(MAXERR)+BLIST(MAXERR))
        A2 = B1
        B2 = BLIST(MAXERR)
        IF (KEYF.EQ.1)
     *  CALL Q1K15D(F,FARR,N,FEXT,GEXT,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
CC      IF (KEYF.EQ.2)
CC   *  CALL Q1K21D(F,FARR,N,FEXT,GEXT,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
CC      IF (KEYF.EQ.3)
CC   *  CALL Q1K31D(F,FARR,N,FEXT,GEXT,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
CC      IF (KEYF.EQ.4)
CC   *  CALL Q1K41D(F,FARR,N,FEXT,GEXT,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
CC      IF (KEYF.EQ.5)
CC   *  CALL Q1K51D(F,FARR,N,FEXT,GEXT,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
CC      IF (KEYF.EQ.6)
CC   *  CALL Q1K61D(F,FARR,N,FEXT,GEXT,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
        IF (KEYF.EQ.1)
     *  CALL Q1K15D(F,FARR,N,FEXT,GEXT,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
CC      IF (KEYF.EQ.2)
CC   *  CALL Q1K21D(F,FARR,N,FEXT,GEXT,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
CC      IF (KEYF.EQ.3)
CC   *  CALL Q1K31D(F,FARR,N,FEXT,GEXT,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
CC      IF (KEYF.EQ.4)
CC   *  CALL Q1K41D(F,FARR,N,FEXT,GEXT,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
CC      IF (KEYF.EQ.5)
CC   *  CALL Q1K51D(F,FARR,N,FEXT,GEXT,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
CC      IF (KEYF.EQ.6)
CC   *  CALL Q1K61D(F,FARR,N,FEXT,GEXT,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
C
C           IMPROVE PREVIOUS APPROXIMATIONS TO INTEGRAL
C           AND ERROR AND TEST FOR ACCURACY.
C
        NEVAL = NEVAL+1
        AREA12 = AREA1+AREA2
        ERRO12 = ERROR1+ERROR2
        ERRSUM = ERRSUM+ERRO12-ERRMAX
        AREA = AREA+AREA12-RLIST(MAXERR)
        IF(DEFAB1.EQ.ERROR1.OR.DEFAB2.EQ.ERROR2) GO TO 5
        IF(DABS(RLIST(MAXERR)-AREA12).LE.1.0D-05*DABS(AREA12)
     *  .AND.ERRO12.GE.9.9D-01*ERRMAX) IROFF1 = IROFF1+1
        IF(LAST.GT.10.AND.ERRO12.GT.ERRMAX) IROFF2 = IROFF2+1
    5   RLIST(MAXERR) = AREA1
        RLIST(LAST) = AREA2
        ERRBND = DMAX1(EPSABS,EPSREL*DABS(AREA))
        IF(ERRSUM.LE.ERRBND) GO TO 8
C
C           TEST FOR ROUNDOFF ERROR AND EVENTUALLY SET ERROR FLAG.
C
        IF(IROFF1.GE.6.OR.IROFF2.GE.20) IER = 2
C
C           SET ERROR FLAG IN THE CASE THAT THE NUMBER OF
C           SUBINTERVALS EQUALS LIMIT.
C
        IF(LAST.EQ.LIMIT) IER = 1
C
C           SET ERROR FLAG IN THE CASE OF BAD INTEGRAND BEHAVIOUR
C           AT A POINT OF THE INTEGRATION RANGE.
C
        IF(DMAX1(DABS(A1),DABS(B2)).LE.(1.0D+00+C*1.0D+03*
     *  EPMACH)*(DABS(A2)+1.0D+04*UFLOW)) IER = 3
C
C           APPEND THE NEWLY-CREATED INTERVALS TO THE LIST.
C
    8   IF(ERROR2.GT.ERROR1) GO TO 10
        ALIST(LAST) = A2
        BLIST(MAXERR) = B1
        BLIST(LAST) = B2
        ELIST(MAXERR) = ERROR1
        ELIST(LAST) = ERROR2
        GO TO 20
   10   ALIST(MAXERR) = A2
        ALIST(LAST) = A1
        BLIST(LAST) = B1
        RLIST(MAXERR) = AREA2
        RLIST(LAST) = AREA1
        ELIST(MAXERR) = ERROR2
        ELIST(LAST) = ERROR1
C
C           CALL SUBROUTINE QSORTD TO MAINTAIN THE DESCENDING ORDERING
C           IN THE LIST OF ERROR ESTIMATES AND SELECT THE SUBINTERVAL
C           WITH THE LARGEST ERROR ESTIMATE (TO BE BISECTED NEXT).
C
   20   CALL QSORTD(LIMIT,LAST,MAXERR,ERRMAX,ELIST,IORD,NRMAX)
C***JUMP OUT OF DO-LOOP
        IF(IER.NE.0.OR.ERRSUM.LE.ERRBND) GO TO 40
   30 CONTINUE
C
C           COMPUTE FINAL RESULT.
C           ---------------------
C
   40 RESULT = 0.0D+00
      DO 50 K=1,LAST
        RESULT = RESULT+RLIST(K)
   50 CONTINUE
      ABSERR = ERRSUM
   60 IF(KEYF.NE.1) NEVAL = (10*KEYF+1)*(2*NEVAL+1)
      IF(KEYF.EQ.1) NEVAL = 30*NEVAL+15
c  999 IF (IER.NE.0) CALL MESSGE(400+IER,'QAGE1 ',0)
c temporairement remplacé
      if (ier.gt.1) call messge(400+ier,'qage1 ',0) 
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE Q1K15D
     *  (F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,RESABS,RESASC)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : QUADPACK
C                ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION A,ABSC,ABSERR,B,CENTR,DABS,DHLGTH,DMAX1,DMIN1,
     *  EPMACH,F,FC,FSUM,FVAL1,FVAL2,FV1,FV2,HLGTH,OFLOW,RESABS,RESASC,
     *  RESG,RESK,RESKH,RESULT,UFLOW,WG,WGK,XGK,FEXT
      INTEGER J,JTW,JTWM1
      EXTERNAL F,FEXT,GEXT
C
      DIMENSION FV1(7),FV2(7),WG(4),WGK(8),XGK(8),FARR(N)
C
C           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
C           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
C           CORRESPONDING WEIGHTS ARE GIVEN.
C
C           XGK    - ABSCISSAE OF THE 15-POINT KRONROD RULE
C                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 7-POINT
C                    GAUSS RULE
C                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
C                    ADDED TO THE 7-POINT GAUSS RULE
C
C           WGK    - WEIGHTS OF THE 15-POINT KRONROD RULE
C
C           WG     - WEIGHTS OF THE 7-POINT GAUSS RULE
C
      DATA XGK(1),XGK(2),XGK(3),XGK(4),XGK(5),XGK(6),XGK(7),XGK(8)/
     *     9.914553711208126D-01,   9.491079123427585D-01,
     *     8.648644233597691D-01,   7.415311855993944D-01,
     *     5.860872354676911D-01,   4.058451513773972D-01,
     *     2.077849550078985D-01,   0.0D+00              /
      DATA WGK(1),WGK(2),WGK(3),WGK(4),WGK(5),WGK(6),WGK(7),WGK(8)/
     *     2.293532201052922D-02,   6.309209262997855D-02,
     *     1.047900103222502D-01,   1.406532597155259D-01,
     *     1.690047266392679D-01,   1.903505780647854D-01,
     *     2.044329400752989D-01,   2.094821410847278D-01/
      DATA WG(1),WG(2),WG(3),WG(4)/
     *     1.294849661688697D-01,   2.797053914892767D-01,
     *     3.818300505051189D-01,   4.179591836734694D-01/
C
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C
C           CENTR  - MID POINT OF THE INTERVAL
C           HLGTH  - HALF-LENGTH OF THE INTERVAL
C           ABSC   - ABSCISSA
C           FVAL*  - FUNCTION VALUE
C           RESG   - RESULT OF THE 7-POINT GAUSS FORMULA
C           RESK   - RESULT OF THE 15-POINT KRONROD FORMULA
C           RESKH  - APPROXIMATION TO THE MEAN VALUE OF F OVER (A,B),
C                    I.E. TO I/(B-A)
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C           OFLOW IS THE LARGEST MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENTS
      CALL MACHZD(7,EPMACH)
      CALL MACHZD(4,UFLOW)
      CALL MACHZD(6,OFLOW)
C
      CENTR = 5.0D-01*(A+B)
      HLGTH = 5.0D-01*(B-A)
      DHLGTH = DABS(HLGTH)
C
C           COMPUTE THE 15-POINT KRONROD APPROXIMATION TO
C           THE INTEGRAL, AND ESTIMATE THE ABSOLUTE ERROR.
C
      FC = F(CENTR,FARR,N,FEXT,GEXT)
      RESG = FC*WG(4)
      RESK = FC*WGK(8)
      RESABS = DABS(RESK)
      DO 10 J=1,3
        JTW = J*2
        ABSC = HLGTH*XGK(JTW)
        FVAL1 = F(CENTR-ABSC,FARR,N,FEXT,GEXT)
        FVAL2 = F(CENTR+ABSC,FARR,N,FEXT,GEXT)
        FV1(JTW) = FVAL1
        FV2(JTW) = FVAL2
        FSUM = FVAL1+FVAL2
        RESG = RESG+WG(J)*FSUM
        RESK = RESK+WGK(JTW)*FSUM
        RESABS = RESABS+WGK(JTW)*(DABS(FVAL1)+DABS(FVAL2))
   10 CONTINUE
      DO 15 J = 1,4
        JTWM1 = J*2-1
        ABSC = HLGTH*XGK(JTWM1)
        FVAL1 = F(CENTR-ABSC,FARR,N,FEXT,GEXT)
        FVAL2 = F(CENTR+ABSC,FARR,N,FEXT,GEXT)
        FV1(JTWM1) = FVAL1
        FV2(JTWM1) = FVAL2
        FSUM = FVAL1+FVAL2
        RESK = RESK+WGK(JTWM1)*FSUM
        RESABS = RESABS+WGK(JTWM1)*(DABS(FVAL1)+DABS(FVAL2))
   15 CONTINUE
      RESKH = RESK*5.0D-01
      RESASC = WGK(8)*DABS(FC-RESKH)
      DO 20 J=1,7
        RESASC = RESASC+WGK(J)*(DABS(FV1(J)-RESKH)+DABS(FV2(J)-RESKH))
   20 CONTINUE
      RESULT = RESK*HLGTH
      RESABS = RESABS*DHLGTH
      RESASC = RESASC*DHLGTH
      ABSERR = DABS((RESK-RESG)*HLGTH)
      IF(RESASC.NE.0.0D+00.AND.ABSERR.NE.0.0D+00)
     *  ABSERR = RESASC*DMIN1(1.0D+00,(2.0D+02*ABSERR/RESASC)**1.5D+00)
      IF(RESABS.GT.UFLOW/(5.0D+01*EPMACH)) ABSERR = DMAX1
     *  ((EPMACH*5.0D+01)*RESABS,ABSERR)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE QSORTD(LIMIT,LAST,MAXERR,ERMAX,ELIST,IORD,NRMAX)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : QUADPACK
C                ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION ELIST,ERMAX,ERRMAX,ERRMIN
      INTEGER I,IBEG,IDO,IORD,ISUCC,J,JBND,JUPBN,K,LAST,LIMIT,MAXERR,
     *  NRMAX
      DIMENSION ELIST(LAST),IORD(LAST)
C
C           CHECK WHETHER THE LIST CONTAINS MORE THAN
C           TWO ERROR ESTIMATES.
C
C***FIRST EXECUTABLE STATEMENT
      IF(LAST.GT.2) GO TO 10
      IORD(1) = 1
      IORD(2) = 2
      GO TO 90
C
C           THIS PART OF THE ROUTINE IS ONLY EXECUTED IF, DUE TO A
C           DIFFICULT INTEGRAND, SUBDIVISION INCREASED THE ERROR
C           ESTIMATE. IN THE NORMAL CASE THE INSERT PROCEDURE SHOULD
C           START AFTER THE NRMAX-TH LARGEST ERROR ESTIMATE.
C
   10 ERRMAX = ELIST(MAXERR)
      IF(NRMAX.EQ.1) GO TO 30
      IDO = NRMAX-1
      DO 20 I = 1,IDO
        ISUCC = IORD(NRMAX-1)
C***JUMP OUT OF DO-LOOP
        IF(ERRMAX.LE.ELIST(ISUCC)) GO TO 30
        IORD(NRMAX) = ISUCC
        NRMAX = NRMAX-1
   20 CONTINUE
C
C           COMPUTE THE NUMBER OF ELEMENTS IN THE LIST TO BE
C           MAINTAINED IN DESCENDING ORDER. THIS NUMBER
C           DEPENDS ON THE NUMBER OF SUBDIVISIONS STILL ALLOWED.
C
   30 JUPBN = LAST
      IF(LAST.GT.(LIMIT/2+2)) JUPBN = LIMIT+3-LAST
      ERRMIN = ELIST(LAST)
C
C           INSERT ERRMAX BY TRAVERSING THE LIST TOP-DOWN,
C           STARTING COMPARISON FROM THE ELEMENT ELIST(IORD(NRMAX+1)).
C
      JBND = JUPBN-1
      IBEG = NRMAX+1
      IF(IBEG.GT.JBND) GO TO 50
      DO 40 I=IBEG,JBND
        ISUCC = IORD(I)
C***JUMP OUT OF DO-LOOP
        IF(ERRMAX.GE.ELIST(ISUCC)) GO TO 60
        IORD(I-1) = ISUCC
   40 CONTINUE
   50 IORD(JBND) = MAXERR
      IORD(JUPBN) = LAST
      GO TO 90
C
C           INSERT ERRMIN BY TRAVERSING THE LIST BOTTOM-UP.
C
   60 IORD(I-1) = MAXERR
      K = JBND
      DO 70 J=I,JBND
        ISUCC = IORD(K)
C***JUMP OUT OF DO-LOOP
        IF(ERRMIN.LT.ELIST(ISUCC)) GO TO 80
        IORD(K+1) = ISUCC
        K = K-1
   70 CONTINUE
      IORD(I) = LAST
      GO TO 90
   80 IORD(K+1) = LAST
C
C           SET MAXERR AND ERMAX.
C
   90 MAXERR = IORD(NRMAX)
      ERMAX = ELIST(MAXERR)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE INTGRS(F,FARR,N,FEXT,GEXT,A,B,EPSABS,EPSREL,KEY,LIMIT,
     1           RESULT,ABSERR,NEVAL,IER,WORK,IWORK)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : QUADPACK
C                ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      REAL A,ABSERR,B,EPSABS,EPSREL,F,RESULT,FEXT,GEXT,WORK
      INTEGER ALIST,BLIST,ELIST,IER,IWORK,KEY,LAST,LIMIT,NEVAL,RLIST
C
      DIMENSION FARR(N),WORK(4*LIMIT),IWORK(LIMIT)
 
C
      EXTERNAL F,FEXT,GEXT
C
C         LIMIT IS THE MAXIMUM NUMBER OF SUBINTERVALS ALLOWED IN
C         THE SUBDIVISION PROCESS OF QAGE1. TAKE CARE THAT LIMIT.GE.1.
C
C**** DATA LIMIT/500/
C
C         KEY DETERMINES THE QUADRATURE FORMULAE TO BE APPLIED
C         BY QAGE1.
C
C
C***FIRST EXECUTABLE STATEMENT
      IF ((EPSABS.LT.0..AND.EPSREL.LT.0.).OR.LIMIT.LE.1
     1   .OR.LIMIT.GT.500)  CALL MESSGE(500,'INTGRS',1)
      ALIST=1
      BLIST=ALIST+LIMIT
      RLIST=BLIST+LIMIT
      ELIST=RLIST+LIMIT
      CALL QAGE1(F,FARR,N,FEXT,GEXT,A,B,EPSABS,EPSREL,KEY,LIMIT,
     1     RESULT,ABSERR,NEVAL,IER,
     2     WORK,WORK(BLIST),WORK(RLIST),WORK(ELIST),IWORK,LAST)
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE QAGE1(F,FARR,N,FEXT,GEXT,A,B,EPSABS,EPSREL,KEY,LIMIT,
     *  RESULT,ABSERR,NEVAL,IER,ALIST,BLIST,RLIST,ELIST,IORD,LAST)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : QUADPACK
C                ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      REAL A,ABSERR,ALIST,AREA,AREA1,AREA12,AREA2,A1,A2,B,BLIST,B1,B2,C,
     *  DEFABS,DEFAB1,DEFAB2,ELIST,EPMACH,EPSABS,EPSREL,ERRBND,ERRMAX,
     *  ERROR1,ERROR2,ERRO12,ERRSUM,F,OFLOW,RESABS,RESULT,RLIST,UFLOW
      INTEGER IER,IORD,IROFF1,IROFF2,K,KEY,KEYF,LAST,LIMIT,MAXERR,NEVAL,
     *  NRMAX
C
      DIMENSION ALIST(LIMIT),BLIST(LIMIT),ELIST(LIMIT),IORD(LIMIT),
     *  RLIST(LIMIT),FARR(N)
C
      EXTERNAL F,FEXT,GEXT
C
C            LIST OF MAJOR VARIABLES
C            -----------------------
C
C           ALIST     - LIST OF LEFT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           BLIST     - LIST OF RIGHT END POINTS OF ALL SUBINTERVALS
C                       CONSIDERED UP TO NOW
C           RLIST(I)  - APPROXIMATION TO THE INTEGRAL OVER
C                       (ALIST(I),BLIST(I))
C           ELIST(I)  - ERROR ESTIMATE APPLYING TO RLIST(I)
C           MAXERR    - POINTER TO THE INTERVAL WITH LARGEST ERROR
C                       ESTIMATE
C           ERRMAX    - ELIST(MAXERR)
C           AREA      - SUM OF THE INTEGRALS OVER THE SUBINTERVALS
C           ERRSUM    - SUM OF THE ERRORS OVER THE SUBINTERVALS
C           ERRBND    - REQUESTED ACCURACY MAX(EPSABS,EPSREL*
C                       ABS(RESULT))
C           *****1    - VARIABLE FOR THE LEFT SUBINTERVAL
C           *****2    - VARIABLE FOR THE RIGHT SUBINTERVAL
C           LAST      - INDEX FOR SUBDIVISION
C
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C           OFLOW IS THE LARGEST MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENTS
      CALL MACHZ(7,EPMACH)
      CALL MACHZ(4,UFLOW)
      CALL MACHZ(6,OFLOW)
C
C           TEST ON VALIDITY OF PARAMETERS
C           ------------------------------
C
      NEVAL = 0
      LAST = 0
      RESULT = 0.0E+00
      ABSERR = 0.0E+00
      ALIST(1) = A
      BLIST(1) = B
      RLIST(1) = 0.0E+00
      ELIST(1) = 0.0E+00
      IORD(1) = 0
      IER = 6
      IF ((EPSABS.LT.0..AND.EPSREL.LT.0.).OR.LIMIT.LE.1
     1   .OR.LIMIT.GT.500)  CALL MESSGE(500,'QAGE1 ',1)
      IER = 0
C
C           FIRST APPROXIMATION TO THE INTEGRAL
C           -----------------------------------
C
      KEYF = KEY
      IF(KEY.LE.0) KEYF = 1
      IF(KEY.GE.7) KEYF = 6
      C = KEYF
      NEVAL = 0
      IF (KEYF.EQ.1)
     *  CALL Q1K15(F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,DEFABS,RESABS)
CC    IF (KEYF.EQ.2)
CC   *  CALL Q1K21(F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,DEFABS,RESABS)
CC    IF (KEYF.EQ.3)
CC   *  CALL Q1K31(F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,DEFABS,RESABS)
CC    IF (KEYF.EQ.4)
CC   *  CALL Q1K41(F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,DEFABS,RESABS)
CC    IF (KEYF.EQ.5)
CC   *  CALL Q1K51(F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,DEFABS,RESABS)
CC    IF (KEYF.EQ.6)
CC   *  CALL Q1K61(F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,DEFABS,RESABS)
      LAST = 1
      RLIST(1) = RESULT
      ELIST(1) = ABSERR
      IORD(1) = 1
C
C           TEST ON ACCURACY.
C
      ERRBND = AMAX1(EPSABS,EPSREL*ABS(RESULT))
      IF(ABSERR.LE.5.0E+01*EPMACH*DEFABS.AND.ABSERR.GT.ERRBND) IER = 2
      IF(LIMIT.EQ.1) IER = 1
      IF(IER.NE.0.OR.(ABSERR.LE.ERRBND.AND.ABSERR.NE.RESABS)
     *  .OR.ABSERR.EQ.0.0E+00) GO TO 60
C
C           INITIALIZATION
C           --------------
C
C
      ERRMAX = ABSERR
      MAXERR = 1
      AREA = RESULT
      ERRSUM = ABSERR
      NRMAX = 1
      IROFF1 = 0
      IROFF2 = 0
C
C           MAIN DO-LOOP
C           ------------
C
      DO 30 LAST = 2,LIMIT
C
C           BISECT THE SUBINTERVAL WITH THE LARGEST ERROR ESTIMATE.
C
        A1 = ALIST(MAXERR)
        B1 = 5.0E-01*(ALIST(MAXERR)+BLIST(MAXERR))
        A2 = B1
        B2 = BLIST(MAXERR)
        IF (KEYF.EQ.1)
     *  CALL Q1K15(F,FARR,N,FEXT,GEXT,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
CC      IF (KEYF.EQ.2)
CC   *  CALL Q1K21(F,FARR,N,FEXT,GEXT,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
CC      IF (KEYF.EQ.3)
CC   *  CALL Q1K31(F,FARR,N,FEXT,GEXT,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
CC      IF (KEYF.EQ.4)
CC   *  CALL Q1K41(F,FARR,N,FEXT,GEXT,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
CC      IF (KEYF.EQ.5)
CC   *  CALL Q1K51(F,FARR,N,FEXT,GEXT,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
CC      IF (KEYF.EQ.6)
CC   *  CALL Q1K61(F,FARR,N,FEXT,GEXT,A1,B1,AREA1,ERROR1,RESABS,DEFAB1)
        IF (KEYF.EQ.1)
     *  CALL Q1K15(F,FARR,N,FEXT,GEXT,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
CC      IF (KEYF.EQ.2)
CC   *  CALL Q1K21(F,FARR,N,FEXT,GEXT,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
CC      IF (KEYF.EQ.3)
CC   *  CALL Q1K31(F,FARR,N,FEXT,GEXT,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
CC      IF (KEYF.EQ.4)
CC   *  CALL Q1K41(F,FARR,N,FEXT,GEXT,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
CC      IF (KEYF.EQ.5)
CC   *  CALL Q1K51(F,FARR,N,FEXT,GEXT,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
CC      IF (KEYF.EQ.6)
CC   *  CALL Q1K61(F,FARR,N,FEXT,GEXT,A2,B2,AREA2,ERROR2,RESABS,DEFAB2)
C
C           IMPROVE PREVIOUS APPROXIMATIONS TO INTEGRAL AND ERROR AND
C           TEST FOR ACCURACY.
C
        NEVAL = NEVAL+1
        AREA12 = AREA1+AREA2
        ERRO12 = ERROR1+ERROR2
        ERRSUM = ERRSUM+ERRO12-ERRMAX
        AREA = AREA+AREA12-RLIST(MAXERR)
        IF(DEFAB1.EQ.ERROR1.OR.DEFAB2.EQ.ERROR2) GO TO 5
        IF(ABS(RLIST(MAXERR)-AREA12).LE.1.0E-05*ABS(AREA12)
     *  .AND.ERRO12.GE.9.9E-01*ERRMAX) IROFF1 = IROFF1+1
        IF(LAST.GT.10.AND.ERRO12.GT.ERRMAX) IROFF2 = IROFF2+1
    5   RLIST(MAXERR) = AREA1
        RLIST(LAST) = AREA2
        ERRBND = AMAX1(EPSABS,EPSREL*ABS(AREA))
        IF(ERRSUM.LE.ERRBND) GO TO 8
C
C           TEST FOR ROUNDOFF ERROR AND EVENTUALLY SET ERROR FLAG
C
        IF(IROFF1.GE.6.OR.IROFF2.GE.20) IER = 2
C
C           SET ERROR FLAG IN THE CASE THAT THE NUMBER OF SUBINTERVALS
C           EQUALS LIMIT.
C
        IF(LAST.EQ.LIMIT) IER = 1
C
C           SET ERROR FLAG IN THE CASE OF BAD INTEGRAND BEHAVIOUR
C           AT A POINT OF THE INTEGRATION RANGE.
C
        IF(AMAX1(ABS(A1),ABS(B2)).LE.(1.0E+00+C*1.0E+03*
     *  EPMACH)*(ABS(A2)+1.0E+04*UFLOW)) IER = 3
C
C           APPEND THE NEWLY-CREATED INTERVALS TO THE LIST.
C
    8   IF(ERROR2.GT.ERROR1) GO TO 10
        ALIST(LAST) = A2
        BLIST(MAXERR) = B1
        BLIST(LAST) = B2
        ELIST(MAXERR) = ERROR1
        ELIST(LAST) = ERROR2
        GO TO 20
   10   ALIST(MAXERR) = A2
        ALIST(LAST) = A1
        BLIST(LAST) = B1
        RLIST(MAXERR) = AREA2
        RLIST(LAST) = AREA1
        ELIST(MAXERR) = ERROR2
        ELIST(LAST) = ERROR1
C
C           CALL SUBROUTINE QDSRT TO MAINTAIN THE DESCENDING ORDERING
C           IN THE LIST OF ERROR ESTIMATES AND SELECT THE SUBINTERVAL
C           WITH THE LARGEST ERROR ESTIMATE (TO BE BISECTED NEXT).
C
   20   CALL QDSRT(LIMIT,LAST,MAXERR,ERRMAX,ELIST,IORD,NRMAX)
C***JUMP OUT OF DO-LOOP
        IF(IER.NE.0.OR.ERRSUM.LE.ERRBND) GO TO 40
   30 CONTINUE
C
C           COMPUTE FINAL RESULT.
C           ---------------------
C
   40 RESULT = 0.0E+00
      DO 50 K=1,LAST
        RESULT = RESULT+RLIST(K)
   50 CONTINUE
      ABSERR = ERRSUM
   60 IF(KEYF.NE.1) NEVAL = (10*KEYF+1)*(2*NEVAL+1)
      IF(KEYF.EQ.1) NEVAL = 30*NEVAL+15
      IF (IER.NE.0) CALL MESSGE(400+IER,'QAGE1 ',0)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE Q1K15
     *  (F,FARR,N,FEXT,GEXT,A,B,RESULT,ABSERR,RESABS,RESASC)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : QUADPACK
C                ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      REAL A,ABSC,ABSERR,B,CENTR,DHLGTH,EPMACH,F,FC,FSUM,FVAL1,FVAL2,
     *  FV1,FV2,HLGTH,OFLOW,RESABS,RESASC,RESG,RESK,RESKH,RESULT,UFLOW,
     *  WG,WGK,XGK,FEXT,GEXT
      INTEGER J,JTW,JTWM1
      EXTERNAL F,FEXT,GEXT
C
      DIMENSION FV1(7),FV2(7),WG(4),WGK(8),XGK(8),FARR(N)
C
C           THE ABSCISSAE AND WEIGHTS ARE GIVEN FOR THE INTERVAL (-1,1).
C           BECAUSE OF SYMMETRY ONLY THE POSITIVE ABSCISSAE AND THEIR
C           CORRESPONDING WEIGHTS ARE GIVEN.
C
C           XGK    - ABSCISSAE OF THE 15-POINT KRONROD RULE
C                    XGK(2), XGK(4), ...  ABSCISSAE OF THE 7-POINT
C                    GAUSS RULE
C                    XGK(1), XGK(3), ...  ABSCISSAE WHICH ARE OPTIMALLY
C                    ADDED TO THE 7-POINT GAUSS RULE
C
C           WGK    - WEIGHTS OF THE 15-POINT KRONROD RULE
C
C           WG     - WEIGHTS OF THE 7-POINT GAUSS RULE
C
      DATA XGK(1),XGK(2),XGK(3),XGK(4),XGK(5),XGK(6),XGK(7),XGK(8)/
     *     9.914553711208126E-01,   9.491079123427585E-01,
     *     8.648644233597691E-01,   7.415311855993944E-01,
     *     5.860872354676911E-01,   4.058451513773972E-01,
     *     2.077849550078985E-01,   0.0E+00              /
      DATA WGK(1),WGK(2),WGK(3),WGK(4),WGK(5),WGK(6),WGK(7),WGK(8)/
     *     2.293532201052922E-02,   6.309209262997855E-02,
     *     1.047900103222502E-01,   1.406532597155259E-01,
     *     1.690047266392679E-01,   1.903505780647854E-01,
     *     2.044329400752989E-01,   2.094821410847278E-01/
      DATA WG(1),WG(2),WG(3),WG(4)/
     *     1.294849661688697E-01,   2.797053914892767E-01,
     *     3.818300505051189E-01,   4.179591836734694E-01/
C
C
C           LIST OF MAJOR VARIABLES
C           -----------------------
C
C           CENTR  - MID POINT OF THE INTERVAL
C           HLGTH  - HALF-LENGTH OF THE INTERVAL
C           ABSC   - ABSCISSA
C           FVAL*  - FUNCTION VALUE
C           RESG   - RESULT OF THE 7-POINT GAUSS FORMULA
C           RESK   - RESULT OF THE 15-POINT KRONROD FORMULA
C           RESKH  - APPROXIMATION TO THE MEAN VALUE OF F OVER (A,B),
C                    I.E. TO I/(B-A)
C
C           MACHINE DEPENDENT CONSTANTS
C           ---------------------------
C
C           EPMACH IS THE LARGEST RELATIVE SPACING.
C           UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
C           OFLOW IS THE LARGEST MAGNITUDE.
C
C***FIRST EXECUTABLE STATEMENTS
      CALL MACHZ(7,EPMACH)
      CALL MACHZ(4,UFLOW)
      CALL MACHZ(6,OFLOW)
C
      CENTR = 5.0E-01*(A+B)
      HLGTH = 5.0E-01*(B-A)
      DHLGTH = ABS(HLGTH)
C
C           COMPUTE THE 15-POINT KRONROD APPROXIMATION TO THE INTEGRAL,
C           AND ESTIMATE THE ABSOLUTE ERROR.
C
      FC = F(CENTR,FARR,N,FEXT,GEXT)
      RESG = FC*WG(4)
      RESK = FC*WGK(8)
      RESABS = ABS(RESK)
      DO 10 J=1,3
        JTW = J*2
        ABSC = HLGTH*XGK(JTW)
        FVAL1 = F(CENTR-ABSC,FARR,N,FEXT,GEXT)
        FVAL2 = F(CENTR+ABSC,FARR,N,FEXT,GEXT)
        FV1(JTW) = FVAL1
        FV2(JTW) = FVAL2
        FSUM = FVAL1+FVAL2
        RESG = RESG+WG(J)*FSUM
        RESK = RESK+WGK(JTW)*FSUM
        RESABS = RESABS+WGK(JTW)*(ABS(FVAL1)+ABS(FVAL2))
   10 CONTINUE
      DO 15 J = 1,4
        JTWM1 = J*2-1
        ABSC = HLGTH*XGK(JTWM1)
        FVAL1 = F(CENTR-ABSC,FARR,N,FEXT,GEXT)
        FVAL2 = F(CENTR+ABSC,FARR,N,FEXT,GEXT)
        FV1(JTWM1) = FVAL1
        FV2(JTWM1) = FVAL2
        FSUM = FVAL1+FVAL2
        RESK = RESK+WGK(JTWM1)*FSUM
        RESABS = RESABS+WGK(JTWM1)*(ABS(FVAL1)+ABS(FVAL2))
   15 CONTINUE
      RESKH = RESK*5.0E-01
      RESASC = WGK(8)*ABS(FC-RESKH)
      DO 20 J=1,7
        RESASC = RESASC+WGK(J)*(ABS(FV1(J)-RESKH)+ABS(FV2(J)-RESKH))
   20 CONTINUE
      RESULT = RESK*HLGTH
      RESABS = RESABS*DHLGTH
      RESASC = RESASC*DHLGTH
      ABSERR = ABS((RESK-RESG)*HLGTH)
      IF(RESASC.NE.0.0E+00.AND.ABSERR.NE.0.0E+00)
     *  ABSERR = RESASC*AMIN1(1.0E+00,(2.0E+02*ABSERR/RESASC)**1.5E+00)
      IF(RESABS.GT.UFLOW/(5.0E+01*EPMACH)) ABSERR = AMAX1
     *  ((EPMACH*5.0E+01)*RESABS,ABSERR)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE QDSRT(LIMIT,LAST,MAXERR,ERMAX,ELIST,IORD,NRMAX)
C.......................................................................
C
C   R O B E T H  -  R O B S Y S   RELEASE 3.0 (COPYRIGHT) 1985, 1990
C
C   PROGRAMMER : QUADPACK
C                ADAPTED FOR ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      REAL ELIST,ERMAX,ERRMAX,ERRMIN
      INTEGER I,IBEG,IDO,IORD,ISUCC,J,JBND,JUPBN,K,LAST,LIMIT,MAXERR,
     *  NRMAX
      DIMENSION ELIST(LAST),IORD(LAST)
C
C           CHECK WHETHER THE LIST CONTAINS MORE THAN TWO ERROR
C           ESTIMATES.
C
C***FIRST EXECUTABLE STATEMENT
      IF(LAST.GT.2) GO TO 10
      IORD(1) = 1
      IORD(2) = 2
      GO TO 90
C
C           THIS PART OF THE ROUTINE IS ONLY EXECUTED IF, DUE TO A
C           DIFFICULT INTEGRAND, SUBDIVISION INCREASED THE ERROR
C           ESTIMATE. IN THE NORMAL CASE THE INSERT PROCEDURE SHOULD
C           START AFTER THE NRMAX-TH LARGEST ERROR ESTIMATE.
C
   10 ERRMAX = ELIST(MAXERR)
      IF(NRMAX.EQ.1) GO TO 30
      IDO = NRMAX-1
      DO 20 I = 1,IDO
        ISUCC = IORD(NRMAX-1)
C***JUMP OUT OF DO-LOOP
        IF(ERRMAX.LE.ELIST(ISUCC)) GO TO 30
        IORD(NRMAX) = ISUCC
        NRMAX = NRMAX-1
   20    CONTINUE
C
C           COMPUTE THE NUMBER OF ELEMENTS IN THE LIST TO BE MAINTAINED
C           IN DESCENDING ORDER. THIS NUMBER DEPENDS ON THE NUMBER OF
C           SUBDIVISIONS STILL ALLOWED.
C
   30 JUPBN = LAST
      IF(LAST.GT.(LIMIT/2+2)) JUPBN = LIMIT+3-LAST
      ERRMIN = ELIST(LAST)
C
C           INSERT ERRMAX BY TRAVERSING THE LIST TOP-DOWN, STARTING
C           COMPARISON FROM THE ELEMENT ELIST(IORD(NRMAX+1)).
C
      JBND = JUPBN-1
      IBEG = NRMAX+1
      IF(IBEG.GT.JBND) GO TO 50
      DO 40 I=IBEG,JBND
        ISUCC = IORD(I)
C***JUMP OUT OF DO-LOOP
        IF(ERRMAX.GE.ELIST(ISUCC)) GO TO 60
        IORD(I-1) = ISUCC
   40 CONTINUE
   50 IORD(JBND) = MAXERR
      IORD(JUPBN) = LAST
      GO TO 90
C
C           INSERT ERRMIN BY TRAVERSING THE LIST BOTTOM-UP.
C
   60 IORD(I-1) = MAXERR
      K = JBND
      DO 70 J=I,JBND
        ISUCC = IORD(K)
C***JUMP OUT OF DO-LOOP
        IF(ERRMIN.LT.ELIST(ISUCC)) GO TO 80
        IORD(K+1) = ISUCC
        K = K-1
   70 CONTINUE
      IORD(I) = LAST
      GO TO 90
   80 IORD(K+1) = LAST
C
C           SET MAXERR AND ERMAX.
C
   90 MAXERR = IORD(NRMAX)
      ERMAX = ELIST(MAXERR)
      RETURN
      END
C-----------------------------------------------------------------------
C
C                     R O B E T H  FORTRAN Source
C
C   File RGAUXI.F  Auxiliary subroutines of Chapter 2
C
C-----------------------------------------------------------------------
C
      SUBROUTINE HALG(ITYPE,X,Y,THETA,WGT,WGT2,COV,RS,DELTA,SIGMA,
     1                ISIGMA,N,NP,MDX,MDT,NCOV,K,TOL,GAM,MAXIT,MAXIS,
     1                NITMON,ICNV,NIT,EXPSI,EXCHI,EXRHO,SC,SE,SF,SG,SH)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  MODIFIED H-ALGORITHM FOR THE TRANSFORMED PROBLEM
C
      DIMENSION X(MDX,NP),Y(N),THETA(MDT),WGT(N),WGT2(N),COV(NCOV),
     1          RS(N),DELTA(NP),SC(N),SE(NP),SF(NP),SG(NP),SH(NP)
      EXTERNAL EXPSI,EXCHI,EXRHO,ICSIGM,ICTHET
      COMMON/CONST/CONST
C
C  INITIALIZATION
C
      MDXP1=MDX+1
      KP1=K+1
      KK=MDX*(K-1)+K
      KS=K*(K+1)/2
      LDIAG=MIN0(N,NP)
      SIGMB=SIGMA
      IASG=IABS(ISIGMA)
C
C  STEP 1. SET NIT=1
C  -------
      NIT=1
C
C  STEP 2. COMPUTE RESIDUALS Y-X1*THETA
C  -------
  100 CALL RES(2,X,Y,THETA,RS,SE,SG,N,NP,K,NP,MDX,MDT)
      IF (K.NE.NP) CALL SWAPZ(X,SF,K,MDXP1,1,KK,K)
      DO 110 J1=1,LDIAG
      J=LDIAG-J1+1
      CALL H12Z(2,J,J+1,N,X(1,J),1,SH(J),RS,1,N,1,N)
  110 CONTINUE
      IF (ITYPE.EQ.2) THEN
        DO 120 I=1,N
        IF (WGT(I).LE.0.) GOTO 120
        RS(I)=RS(I)/WGT2(I)
  120   CONTINUE
      ENDIF
C
C  STEP 3. COMPUTE A NEW VALUE SIGMB FOR SIGMA
C  -------
      IF (ISIGMA.LT.0.AND.NIT.EQ.1) GOTO 125
      IF (ISIGMA.EQ.0) GOTO 125
      SIGMA=SIGMB
      CALL RYSIGM(RS,WGT,EXCHI,SIGMA,N,NP,TOL,ITYPE,ISIGMA,MAXIS,
     1            NIS,SIGMB,WGT2,SC)
      IF (SIGMB.LE.0.) CALL MESSGE(460,'LYHALG',0)
      IF (SIGMB.LE.0.) RETURN
C
C  ITERATION MONITORING
C
  125 IF (NITMON.LE.0) GOTO 130
      IF (MOD(NIT,NITMON).NE.0) GOTO 130
      CALL QRSS(RS,WGT,WGT2,EXRHO,N,ITYPE,SIGMA,CONST,QS)
      CALL MONITR(NIT,NP,GAM,QS/FLOAT(N),SIGMB,THETA,DELTA)
C
C  STEP 4. HUBERIZE THE RESIDUALS
C  -------
  130 CALL HUB(RS,WGT,WGT2,SIGMB,N,ITYPE,EXPSI)
C
C  STEP 5. TRANSFORM RESIDUALS
C  ------
      DO 135 JJ=1,LDIAG
      J=JJ
      CALL H12Z(2,J,J+1,N,X(1,J),1,SH(J),RS,1,N,1,N)
  135 CONTINUE
      IF (K.NE.NP) CALL SWAPZ(X,SF,K,MDXP1,1,KK,K)
C
C  SOLVE THE LS-PROBLEM FOR THE INCREMENT VECTOR
C
      CALL SOLV(X,RS,NP,K,MDX,N)
C
C  STEP 6. COMPUTE NEW TRANSFORMED SOLUTION
C  -------
      DO 140 J=1,K
      DELTA(J)=RS(J)*GAM
      THETA(J)=THETA(J)+DELTA(J)
  140 CONTINUE
C
C  STEP 7. STOP ITERATIONS IF DESIRED PRECISION IS REACHED
C  -------
      IF (ISIGMA.LT.0.AND.NIT.EQ.1) GOTO 180
      IF (NIT.EQ.MAXIT) GOTO 200
      IF (ICTHET(NP,NCOV,DELTA,SIGMB,COV,TOL,ICNV).EQ.1.AND.
     +    ICSIGM(SIGMA,SIGMB,TOL).EQ.1) GOTO 200
C
C  EXIT. ITERATIONS CONTINUE IF NIT.LT.MAXIT
C  ----
  180 NIT=NIT+1
      GOTO 100
C
C  ITERATIONS ARE TERMINATED.
C
  200 SIGMA=SIGMB
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RES(MODE,X,Y,S,R,COV,SG,N,NP,K,NCOV,MDX,MDS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  THE ARRAY X CONTAINS THE TRANSFORMED DESIGN MATRIX.
C  S(1)...S(K) CONTAIN A SOLUTION OF THE TRANSFORMED
C  LS-PROBLEM AND Y, THE TRANSFORMED RIGHT SIDE.
C  LET U BE THE K BY K UPPER TRIANGULAR MATRIX WITH ELEMENTS
C  X(1,1)...X(1,K),X(2,2)...X(2,K),...,X(K,K).
C  IF MODE=1 OR 2 RES SETS (R(1)...R(K)=(Y(1)...Y(K))
C  -U*(S(1)...S(K)),(R(K+1)...R(N))=(Y(K+1)...Y(N))
C  (SET MODE=1 IF (S(1)...S(K))=U**(-1)*(Y(1)...Y(K)))
C  IF MODE=3 RES COMPUTES THE RESIDUALS OF THE ORIGINAL
C  LS-PROBLEM AFTER THE APPLICATION OF Q.
C  SG CONTAINS PIVOT SCALARS FOR HOUSEHOLDER TRANSFORMATIONS.
C  COV IS USED AS SCRATCH (ONLY THE LAST NP LOCATIONS AND
C  ONLY IF K.LT.NP)
C
      DIMENSION X(MDX,NP),Y(N),R(N),S(MDS),COV(NCOV),SG(NP)
      DOUBLE PRECISION SM,DZERO
      LDIAG=MIN0(N,NP)
      DZERO=0.D0
C
C  COMPUTE THE RESIDUALS R(1)...R(K).
C
      IF (MODE.EQ.2.OR.MODE.EQ.3) GOTO 20
      DO 10 I=1,K
      R(I)=0.
   10 CONTINUE
      GOTO 50
   20 CONTINUE
      IF (K.LT.N) GOTO 25
      DO 26 I=1,N
      R(I)=0.
   26 CONTINUE
      GOTO 130
   25 CONTINUE
      DO 40 I=1,K
      SM=DZERO
      DO 30 J=I,K
      SM=SM+X(I,J)*DBLE(S(J))
   30 CONTINUE
      SM1=SNGL(SM)
      R(I)=Y(I)-SM1
   40 CONTINUE  
   50 CONTINUE
C
C  COMPUTE THE RESIDUALS R(K+1)...R(NP)
C
      IF (K.EQ.NP) GOTO 110
      IF (K.EQ.N) GOTO 130
      KP1=K+1
      IF (MODE.EQ.3) GOTO 55
      DO 52 I=KP1,LDIAG
      R(I)=Y(I)
   52 CONTINUE
      GOTO 110
   55 INZ=NCOV-NP
      DO 100 I=KP1,LDIAG
      IM1=I-1
      DO 60 J=1,IM1
      J1=J+INZ
      COV(J1)=0.
   60 CONTINUE
      DO 70 J=I,NP
      J1=J+INZ
      COV(J1)=X(I,J)
   70 CONTINUE
      DO 80 II=1,K
      I1=KP1-II
      CALL R3V(I1,KP1,NP,X(I1,1),MDX,SG(I1),COV,NCOV,INZ)
   80 CONTINUE
      SM=DZERO
      DO 90 J=1,K
      J1=J+INZ
      SM=SM+COV(J1)*DBLE(S(J))
   90 CONTINUE
      SM1=SNGL(SM)
      R(I)=Y(I)-SM1
  100 CONTINUE
C
C  COMPUTE THE RESIDUALS R(NP+1)...R(N)
C
  110 NPP1=NP+1
      IF (NP.GE.N) GOTO 130
      DO 120 I=NPP1,N
      R(I)=Y(I)
  120 CONTINUE
  130 RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE R3V(LPIVOT,L1,M,U,IUE,UP,C,NCOV,INZ)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  R3V APPLIES THE HOUSEHOLDER TRANSFORMATION DEFINED
C  BY THE VECTOR U TO A VECTOR E=(0,...,0,E(I+1),...,E(N))
C  WHERE I.GE.LPIVOT.
C  E IS STORED IN THE LOCATIONS INZ+1,...,INZ+N OF C.
C  PARAMETERS LPIVOT,L1,M,U,IUE,UP AS IN H12.
C
C
      DIMENSION U(IUE,M),C(NCOV)
      DOUBLE PRECISION SM,B,DZERO
      ONE=1.
      DZERO=0.D0
      IF (0.GE.LPIVOT.OR.LPIVOT.GE.L1.OR.L1.GT.M) RETURN
      CL=ABS(U(1,LPIVOT))
      IF (CL.LE.0.0) RETURN
      B=DBLE(UP)*U(1,LPIVOT)
C
C  B MUST BE NONPOSITIVE HERE. IF B=0., RETURN.
C
      IF (B.GE.0.D0) RETURN
      B=ONE/B
      I2=LPIVOT-1+INZ
      INCR=L1-LPIVOT
      I2=I2+1
      I3=I2+INCR
      I4=I3
      SM=DZERO
      DO 90 I=L1,M
      SM=SM+C(I3)*DBLE(U(1,I))
      I3=I3+1
   90 CONTINUE
      IF (SM.EQ.0.D0) GOTO 120
      SM=SM*B
      C(I2)=C(I2)+SNGL(SM*DBLE(UP))
      DO 110 I=L1,M
      C(I4)=C(I4)+SNGL(SM*DBLE(U(1,I)))
      I4=I4+1
  110 CONTINUE
  120 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SOLV(X,THETA,NP,K,MDX,MDT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  LET U BE THE K BY K UPPER TRIANGULAR MATRIX WITH ELEMENTS
C  X(1,1)...X(1,K),X(2,2)...X(2,K),...X(K,K).
C  SOLV SOLVES THE TRIANGULAR SYSTEM U*THETA=Y (BY BACK SUBSTI-
C  TUTION). ON INPUT Y IS CONTAINED IN THETA.  ON OUTPUT
C  THETA(1)...THETA(K) CONTAIN THE DESIRED SOLUTION.
C
C  ERRORS
C  ------
C  1   AN ELEMENT OF THE PRINCIPAL DIAGONAL OF X IS =0.
C
      DIMENSION X(MDX,NP),THETA(MDT)
      DOUBLE PRECISION SM,DZERO
      DZERO=0.D0
      KP1=K+1
      DO 90 L=1,K
      SM=DZERO
      I=KP1-L
      IF (I.EQ.K) GOTO 60
      IP1=I+1
      DO 50 J=IP1,K
      SM=SM+X(I,J)*DBLE(THETA(J))
   50 CONTINUE
   60 SM1=SNGL(SM)
      IF (X(I,I).NE.0.0) GOTO 80
      CALL MESSGE(501,'SOLV  ',1)
   80 THETA(I)=(THETA(I)-SM1)/X(I,I)
   90 CONTINUE 
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SOLVT(X,THETA,NP,K,MDX,MDT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS / A.MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  LET U BE THE K BY K LOWER TRIANGULAR MATRIX WITH ELEMENTS
C  X(1,1)...X(1,K),X(2,2)...X(2,K),...X(K,K).
C  SOLVT SOLVES THE TRIANGULAR SYSTEM U*THETA=Y (BY FORWARD
C  SUBSTITUTION). ON INPUT Y IS CONTAINED IN THETA. ON OUTPUT
C  THETA(1)...THETA(K) CONTAIN THE DESIRED SOLUTION.
C
C  ERRORS
C  ------
C  1 AN ELEMENT OF THE PRINCIPAL DIAGONAL OF X IS = 0.
C
      DIMENSION X(MDX,NP),THETA(MDT)
      DOUBLE PRECISION SM,DZERO
      DZERO=0.D0
      DO 90 I=1,K
      SM=DZERO
      IF (I.EQ.1) GOTO 60
      IM1=I-1
      DO 50 J=1,IM1
      SM=SM+X(J,I)*DBLE(THETA(J))
   50 CONTINUE
   60 SM1=SNGL(SM)
      IF (X(I,I).NE.0.0) GOTO 80
      CALL MESSGE(501,'SOLVT ',1)
   80 THETA(I)=(THETA(I)-SM1)/X(I,I)
   90 CONTINUE 
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PVM1(X,S,SP,SG,N,NP,K,MDX,MDS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  TRANSFORM A GIVEN SOLUTION VECTOR S IN (P*V)**(-1)*S.
C  ON OUTPUT THE RESULT IS STORED IN S.
C
      DIMENSION X(MDX,NP),S(MDS),SG(NP)
      INTEGER SP(NP)
      LDIAG=MIN0(N,NP)
      KP1=K+1
      DO 20 J=1,LDIAG
      IF (SP(J).EQ.J) GOTO 20
      L=SP(J)
      TMP=S(L)
      S(L)=S(J)
      S(J)=TMP
   20 CONTINUE
      IF (K.EQ.NP) GOTO 40
      DO 30 J=1,K
      I=KP1-J
      CALL H12Z(2,I,KP1,NP,X(I,1),MDX,SG(I),S,1,N,1,NP)
   30 CONTINUE 
   40 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PERM(X,SP,N,NDIM)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  PERMUTE COMPONENTS OF X TO COMPENSATE COLUMN INTERCH.
C
      DIMENSION X(NDIM)
      INTEGER SP(NDIM)
      DO 10 JJ=1,N
      J=N-JJ+1
      IF (SP(J).EQ.J) GOTO 10
      L=SP(J)
      TMP=X(L)
      X(L)=X(J)
      X(J)=TMP
   10 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VSV(LPIVOT,L1,M,U,IUE,UP,S,NCOV,SB)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  LET V BE THE ELEMENTARY HOUSEHOLDER TRANSFORMATION DEFINED
C  BY THE VECTOR U=(U(1)...U(N)) (WITH U(1)=...=U(LPIVOT-1)=0,
C  U(LPIVOT)=UP,U(LPIVOT+1)=...=U(K)=0 AND U(K+1)...U(N) POSSIBLY
C  DIFFERENT FROM 0) AND S A SYMMETRIC MATRIX STORED COLUMNWISE
C  IN THE ARRAY S OF LENGTH NCOV=N*(N+1)/2.  VSV COMPUTES THE
C  SYMMETRIC MATRIX V*S*V AND STORES IT IN S.  IUE IS THE STORAGE
C  INCREMENT BETWEEN ELEMENTS OF THE VECTOR U.  THE LPIVOT-TH COMPO-
C  NENT OF U IS STORED IN UP WHEREAS THE STORAGE LOCATION U(LPIVOT)
C  CONTAINS THE NORM S (S.L6,P.55,FORMULA 10.5).
C
C  ERRORS
C  ------
C  1.  NCOV.NE.N*(N+1)/2.  NO COMPUTATION DONE BY VSV.
C
      DIMENSION S(NCOV),U(IUE,M),SB(M)
      INTEGER H,HP1
      DOUBLE PRECISION SM,B
      LOGICAL NPRCHK
C
C  PARAMETER CHECK AND INITIALIZATION
C
      MM=M*(M+1)/2
      NPRCHK=NCOV.EQ.MM.AND.LPIVOT.LT.L1.AND.LPIVOT.GE.1.AND.LPIVOT.LE.M
      IF (.NOT.NPRCHK) CALL MESSGE(500,'VSV   ',1)
      IF (L1.GT.M) RETURN
      ONE=1.
      K=L1-1
      B=U(1,LPIVOT)*DBLE(UP)
      IF (B.GE.0.D0) GOTO 999
      B=ONE/B
C
C  COMPUTE THE SCALAR PRODUCTS OF U WITH THE H-TH COLUMN OF S FOR H=1..M
C
      L=0
      DO 90 H=1,M
      L=L+H
      L0=L-H
      IF (H.GE.LPIVOT) GOTO 20
      I=(LPIVOT-1)*LPIVOT/2+H
      GOTO 30
   20 I=L0+LPIVOT
   30 SM=DBLE(UP)*S(I)
      IF (H.LE.K) GOTO 60
      L0=L0+K
      DO 40 I=L1,H
      L0=L0+1
      SM=SM+S(L0)*DBLE(U(1,I))
   40 CONTINUE 
      HP1=H+1
      IF (H.EQ.M) GOTO 80
      DO 50 J=HP1,M
      L0=L0+J-1
      SM=SM+DBLE(U(1,J))*S(L0)
   50 CONTINUE 
      GOTO 80
   60 L0=(K-1)*K/2+H
      DO 70 J=L1,M
      L0=L0+J-1
      SM=SM+DBLE(U(1,J))*S(L0)
   70 CONTINUE 
   80 SB(H)=SNGL(SM*B)
   90 CONTINUE 
C
C  COMPUTE THE QUADRATIC FORM U**T*S*U
C
      SM=DBLE(UP)*SB(LPIVOT)
      DO 95 J=L1,M
      SM=SM+SB(J)*DBLE(U(1,J))
   95 CONTINUE 
      S1=SNGL(SM*B)
C
C  SET U(LPIVOT)=UP
C
      CSC=U(1,LPIVOT)
      U(1,LPIVOT)=UP
C
C  COMPUTE S(1,LPIVOT)...S(LPIVOT-1,LPIVOT)
C
      LPM1=LPIVOT-1
      L0=LPIVOT*LPM1/2
      IF (LPM1.LT.1) GOTO 105
      DO 100 I=1,LPM1
      L0=L0+1
      S(L0)=S(L0)+SB(I)*U(1,LPIVOT)
  100 CONTINUE 
  105 CONTINUE
C
C  COMPUTE S(LPIVOT,LPIVOT)
C
      L0=L0+1
      S(L0)=S(L0)+U(1,LPIVOT)*(S1*U(1,LPIVOT)+2.*SB(LPIVOT))
C
C  COMPUTE S(LPIVOT,LPIVOT+1)...S(LPIVOT,L1-1)
C
      LPP1=LPIVOT+1
      IF (LPP1.GT.K) GOTO 115
      DO 110 J=LPP1,K
      L0=L0+J-1
      S(L0)=S(L0)+SB(J)*U(1,LPIVOT)
  110 CONTINUE 
  115 CONTINUE
C
C  COMPUTE S(I,J),J=L1...N,I=1...K (J.GE.L1,I.LT.L1)
C
      L0=(K+1)*K/2-K
      DO 135 J=L1,M
      L0=L0+J-1
      DO 130 I=1,K
      L=L0+I
      S(L)=S(L)+SB(I)*U(1,J)
  130 CONTINUE 
      I=LPIVOT
      L=L0+I
      S(L)=S(L)+SB(J)*U(1,I)+U(1,I)*S1*U(1,J)
  135 CONTINUE
C
C  COMPUTE S(I,J),I=L1...M,J=I...M
C
      L0=K*(K+1)/2-K
      DO 150 J=L1,M
      L0=L0+J-1
      DO 140 I=L1,J
      L=L0+I
      S(L)=S(L)+(S1*U(1,I)*U(1,J)+(U(1,I)*SB(J)+U(1,J)*SB(I)))
  140 CONTINUE
  150 CONTINUE
      U(1,LPIVOT)=CSC
  999 RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MYP(Y,SP,NP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE  Y**T:=(Y**T)*P
C  -------
C  PERMUTE A GIVEN VECTOR Y ACCORDING TO THE PERMUTATION MATRIX SP
C
      DIMENSION Y(NP)
      INTEGER SP(NP)
      DO 100 J=1,NP
        L=SP(J)
        IF (L.EQ.J) GOTO 100
        TMP=Y(J)
        Y(J)=Y(L)
        Y(L)=TMP
  100 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MZP(Z,SP,MDZ,N,NP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE  Z:=Z*P
C  -------
C  PERMUTE A GIVEN MATRIX Z ACCORDING TO THE PERMUTATION MATRIX SP
C
      DIMENSION Z(MDZ,NP)
      INTEGER SP(NP)
      DO 200 J=1,NP
        L=SP(J)
        IF (L.EQ.J) GOTO 200
        DO 100 I=1,N
        TMP=Z(I,J)
        Z(I,J)=Z(I,L)
        Z(I,L)=TMP
  100   CONTINUE
  200 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MZPD(Z,SP,MDZ,N,NP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE  Z:=Z*P
C  -------
C  PERMUTE A GIVEN MATRIX Z ACCORDING TO THE PERMUTATION MATRIX SP
C
      DOUBLE PRECISION Z(MDZ,NP),TMP
      INTEGER SP(NP)
      DO 200 J=1,NP
        L=SP(J)
        IF (L.EQ.J) GOTO 200
        DO 100 I=1,N
        TMP=Z(I,J)
        Z(I,J)=Z(I,L)
        Z(I,L)=TMP
  100   CONTINUE
  200 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MZPVD(X,SP,SG,Z,N,NZ,NP,K,MDX,MDZ,SC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  TRANSFORM A GIVEN MATRIX Z IN Z*P*V.
C
      DOUBLE PRECISION Z(MDZ,NP),X(MDX,NP),SC(NP),SG(NP)
      INTEGER SP(NP)
      CALL MZPD(Z,SP,MDZ,NZ,NP)
      IF (K.EQ.NP) RETURN
      KP1=K+1
      DO 400 L=1,NZ
        DO 100 J=1,NP
        SC(J)=Z(L,J)
  100   CONTINUE 
        DO 200 J=1,K
        I=KP1-J
        CALL H12ZD(2,I,KP1,NP,X(I,1),MDX,SG(I),SC,1,N,1,NP)
  200   CONTINUE 
        DO 300 J=1,NP
        Z(L,J)=SC(J)
  300   CONTINUE 
  400 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE NEWSIG(RS,WGT,WGT2,SIGMA,SIGMB,N,ITYPE,EXCHI)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  COMPUTES A NEW VALUE SIGMB FOR THE ROBUST ESTIMATE OF THE
C  ERROR STANDARD DEVIATION IN THE HUBER'S ALGORITHM FOR REGRESSION.
C  NEWSIG CALLS THE FUNCTION EXCHI.
C
      DIMENSION RS(N),WGT(N),WGT2(N)
      EXTERNAL EXCHI
      COMMON/CONST/CONST
      TMP=0.0
      IF (ITYPE.NE.1) GOTO 20
C
C  HUBER-TYPE
C
      DO 10 I=1,N
      S=RS(I)/SIGMA
      TMP=TMP+EXCHI(S)
   10 CONTINUE 
      GOTO 90
C
C  MALLOWS-TYPE
   20 IF (ITYPE.NE.2) GOTO 40
      DO 30 I=1,N
      S=RS(I)/SIGMA
      IF (WGT(I).LE.0.) GOTO 30
      TMP=TMP+EXCHI(S)*WGT(I)
   30 CONTINUE
      GOTO 90
C
C  SCHWEPPE-TYPE
C
   40 DO 50 I=1,N
      SW=SIGMA*WGT(I)
      IF (SW.EQ.0..OR.WGT(I).LE.0.) GOTO 50
      S=RS(I)/SW
      TMP=TMP+EXCHI(S)*WGT2(I)
   50 CONTINUE
   90 SIGMB=SQRT(TMP/CONST)*SIGMA
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE HUB(RS,WGT,WGT2,SIGMB,N,ITYPE,EXPSI)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  ------
C  COMPUTES HUBERIZED RESIDUALS IN THE HUBER'S ALGORITHM FOR REGRESSION.
C  HUB CALLS THE FUNCTION EXPSI.
C
      DIMENSION RS(N),WGT(N),WGT2(N)
      EXTERNAL EXPSI
      IF (ITYPE.NE.1) GOTO 20
C
C  HUBER-TYPE
C
      DO 10 I=1,N
      S=RS(I)/SIGMB
      RS(I)=EXPSI(S)*SIGMB
   10 CONTINUE 
      RETURN
C
C  MALLOWS-TYPE (WGT2 is the square root of the WEIGHTS)
C
   20 IF (ITYPE.NE.2) GOTO 40
      DO 30 I=1,N
      SW=WGT2(I)*SIGMB
      IF (SW.GT.0.) GOTO 25
      RS(I)=0.
      GOTO 30
   25 S=RS(I)/SIGMB
      RS(I)=EXPSI(S)*SW
   30 CONTINUE
      RETURN
C
C  SCHWEPPE-TYPE
C
   40 DO 50 I=1,N
      SW=WGT(I)*SIGMB
      IF (SW.GT.0..AND.WGT(I).GT.0.) GOTO 45
      RS(I)=0.
      GOTO 50
   45 S=RS(I)/SW
      RS(I)=EXPSI(S)*SW
   50 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE FACS(RS,N,K,SIGMA,TL,XKAPPA,SUM2,PSY,PSP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  COMPUTES CORRECTION FACTORS XKAPPA AND SUM2 FOR
C  THE COVARIANCE MATRIX.
C  FACS CALLS THE FUNCTIONS PSI AND PSP
C
      DIMENSION RS(N)
      EXTERNAL PSY,PSP
      TMP1=0.
      TMP2=0.
      DO 10 J=1,N
      S=RS(J)/SIGMA
      TMP1=TMP1+PSP(S)
      PS=PSY(S)
      TMP2=TMP2+PS*PS
   10 CONTINUE 
      XMU=TMP1/FLOAT(N)
      SUM2=TMP2
      VAR=0.
      DO 20 J=1,N
      S=RS(J)/SIGMA
      VAR=VAR+(PSP(S)-XMU)**2
   20 CONTINUE 
      VAR=VAR/FLOAT(N)
      XKAPPA=0.
      IF (XMU.LE.TL) RETURN
      XMU2=XMU*XMU
      XKAPPA=1.+FLOAT(K)*VAR/FLOAT(N)/XMU2
      SUM2=SUM2/XMU2/FLOAT(N-K)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION DIFF(X,Y)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  SEE (L6) IN H12, P.278.
C
      DIFF=X-Y
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION DIFFD(X,Y)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION X,Y
      DIFFD=SNGL(X-Y)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION CHIPHI(S,WGT,N,FCHI,FEXT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  AUXILIARY ROUTINE FOR RIBETU.
C
      DIMENSION WGT(N)
      EXTERNAL FCHI,FEXT
      COMMON/INTPAR/ITYPE,INTPAR,NEVAL,LIMIT,KEY
      DATA NCALL,FX1/0,0.0/
      IF (NCALL.EQ.1) FX1=FEXT(1.0)
      CHIPHI=FX1
      CALL XERFZ(2,S,PHI)
      IF (ITYPE.EQ.3) GOTO 30
C
C  HUBER & MALLOWS CASE
C
      CHIPHI=FCHI(S)*PHI
      RETURN
C
C  SCHWEPPE CASE
C
   30 SM=0.
      DO 40 J=1,N
      IF (WGT(J).GT.0.) SM=SM+WGT(J)*WGT(J)*FCHI(S/WGT(J))
   40 CONTINUE
      CHIPHI=SM*PHI
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION PSPPHI(S,WGT,N,FPSI,FEXT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  AUXILIARY ROUTINE FOR KIEDCU.
C
      DIMENSION WGT(N)
      EXTERNAL FPSI,FEXT
      COMMON/INTPAR/ITYPE,I,NEVAL,LIMIT,KEY
      DATA NCALL,FX1/0,0.0/
      IF (NCALL.EQ.1) FX1=FEXT(1.0)
      PSPPHI=FX1
      R=S
      CALL XERFZ(2,R,PHI)
      PHI=R*PHI
      IF (ITYPE.EQ.3) R=R/WGT(I)
      PSPPHI=FPSI(R)*PHI
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION PS2PHI(S,WGT,N,FPSI,FEXT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  AUXILIARY ROUTINE FOR KIEDCU
C
      DIMENSION WGT(N)
      EXTERNAL FPSI,FEXT
      COMMON/INTPAR/ITYPE,I,NEVAL,LIMIT,KEY
      DATA NCALL,FX1/0,0.0/
      IF (NCALL.EQ.1) FX1=FEXT(1.0)
      PS2PHI=FX1
      R=S
      CALL XERFZ(2,R,PHI)
      IF (ITYPE.EQ.3) R=R/WGT(I)
      PS2PHI=FPSI(R)*FPSI(R)*PHI
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE INTCHG(A,B)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  AUXILIARY ROUTINE FOR RILARSz
C
      C=A
      A=B
      B=C
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE COL(V1,V2,MLT,M,IOUT)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C     PURPOSE
C     -------
C     AUXILIARY ROUTINE FOR RILARSz
C
      REAL V1(M),V2(M),MLT
      DO 220 I=1,M
        IF (I.EQ.IOUT) GOTO 220
        V1(I)=V1(I)-V2(I)*MLT
  220 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE QRSS(RS,WGT,WGT2,EXRHO,N,ITYPE,SIGMA,CONST,QR)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DIMENSION RS(N),WGT(N),WGT2(N)
      DOUBLE PRECISION TMP
      EXTERNAL EXRHO
      TMP=0.D0
      IF (ITYPE.NE.1) GOTO 15
C
C  HUBER-TYPE
C
      DO 10 I=1,N
        S=RS(I)/SIGMA
        TMP=TMP+DBLE(EXRHO(S))
   10 CONTINUE
      GOTO 50
C
C MALLOWS-TYPE
C
   15 IF (ITYPE.NE.2) GOTO 30
      DO 20 I=1,N
      IF (WGT(I).EQ.0..OR.WGT(I).EQ.-1.) GOTO 20
      S=RS(I)/SIGMA
      TMP=TMP+EXRHO(S)*DBLE(WGT(I))
   20 CONTINUE
      GOTO 50
C
C SCHWEPPE-TYPE
C
   30 DO 40 I=1,N
      IF (WGT(I).EQ.0..OR.WGT(I).EQ.-1.) GOTO 40
      S=RS(I)/(SIGMA*WGT(I))
      TMP=TMP+EXRHO(S)*DBLE(WGT2(I))
   40 CONTINUE
   50 QR=(SNGL(TMP)+CONST)*SIGMA
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RESIDU(X,Y,THETA,N,NP,MDX,RS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  COMPUTES RESIDUALS
C  RS(I)=Y(I)-SUM X(I,J)*THETA(J)
C
      DIMENSION X(MDX,NP),Y(N),THETA(NP),RS(N)
      DOUBLE PRECISION SUM
      DO 200 I=1,N
        SUM=0.D0
        DO 100 J=1,NP
          SUM=SUM+DBLE(X(I,J)*THETA(J))
  100   CONTINUE
        RS(I)=Y(I)-SNGL(SUM)
  200 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE COMPAR(WGT,RS,P,C,SIGMA,N,ITYPE,ISAME)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  COMPARE PARTITIONS IN RYSALG
C
      DIMENSION WGT(N),RS(N),P(N)
      ISAME=1
      IF (ITYPE.EQ.3) GOTO 150
C
C  HUBER & MALLOWS-TYPE
C
      DO 100 I=1,N
        PJ=0.
        T=RS(I)/SIGMA
        IF (T.LT.-C) PJ=-1.
        IF (T.GT.C) PJ=1.
        IF (PJ.NE.P(I)) RETURN
  100 CONTINUE
      GOTO 300
C
C  SCHWEPPE-TYPE
C
  150 DO 200 I=1,N
        PJ=0.
        IF (WGT(I).EQ.0.) GOTO 200
        T=RS(I)/(SIGMA*WGT(I))
        IF (T.LT.-C) PJ=-1.
        IF (T.GT.C) PJ=1.
        IF (PJ.NE.P(I)) RETURN
  200 CONTINUE
  300 ISAME=0
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE S4ALG(XO,WGT,SW,RS,SIGMB,C,ITYPE,N,NP,MDX,SX,SJ,K0)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  FIND THE PARTITIONS J-, J0, J+ IN RYSALG (STEP 4)
C
      DIMENSION XO(MDX,NP),WGT(N),RS(N),SX(MDX,NP),SJ(N),SW(N)
      K0=0
      SQW=1.0
      DO 340 I=1,N
        PJ=0.
        T=RS(I)/SIGMB
        IF (ITYPE.EQ.1) GOTO 300
        IF (WGT(I).EQ.0.) GOTO 320
        IF (ITYPE.EQ.2) THEN
         SQW=SW(I)
        ELSE
         T=T/WGT(I)
        ENDIF
  300   IF (T.LT.-C) PJ=-1.
        IF (T.GT.C) PJ=1.
        SJ(I)=PJ
        IF (PJ.NE.0.) GOTO 320
        K0=K0+1
        DO 310 J=1,NP
          SX(I,J)=XO(I,J)
          IF (ITYPE.EQ.2) SX(I,J)=SX(I,J)*SQW
  310   CONTINUE
        GOTO 340
  320   DO 330 J=1,NP
          SX(I,J)=0.
  330   CONTINUE
  340 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE S5ALG(XO,Y,WGT,RS,THETA,SJ,N,NP,MDX,NCOV,INTCH,TAU,
     1                 SIGMB,C,ITYPE,K0,DELTA,SX,SC,SE,SF,SG,SH,SP,KR)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  COMPUTE THE INCREMENT P-VECTOR DELTA IN RYSALG (STEP 5)
C
      DIMENSION XO(MDX,NP),Y(N),WGT(N),RS(N),THETA(NP),SJ(N),DELTA(NP),
     1          SX(MDX,NP),SC(NCOV),SE(NP),SF(NP),SG(NP),SH(NP)
      INTEGER SP(NP)
C
C   COMPUTE THE PSEUDO-INVERSE MATRIX SC=(XO**T*D*XO)**-1
C   D=diag(W) if ITYPE=2 (Mallows case) and D=I otherwise.
C
      L=0
      DO 320 I=1,NP
        DELTA(I)=0.
        SF(I)=0.
        DO 300 J=1,I
        L=L+1
        SC(L)=0.
        IF (J.EQ.I) SC(L)=1.
  300   CONTINUE
  320 CONTINUE
      KR=0
      IF (K0.EQ.0) GOTO 410
      CALL RIMTRFZ(SX,N,NP,MDX,INTCH,TAU,KR,SF,SG,SH,SP)
      CALL KIASCVZ(SX,KR,NP,MDX,NCOV,1.,1.,SC)
      CALL KFASCVZ(SX,SC,KR,NP,MDX,NCOV,1.,SE,SG,SP)
C
C   COMPUTE SC*X0**T*D*RS AND SC*C*V
C
      DO 350 I=1,N
        WI=0.
        IF (SJ(I).EQ.0.) WI=1.
        DO 330 J=1,NP
        SX(I,J)=WI*XO(I,J)
  330 CONTINUE
  350 CONTINUE
      IF (ITYPE.EQ.2) THEN
        DO 407 I=1,N
        RS(I)=WGT(I)*RS(I)
  407   CONTINUE 
      ENDIF
      CALL GRADNT(SX,RS,N,NP,MDX,SE)
      CALL MSFZ(SC,SE,DELTA,NP,NCOV,1,NP,NP)
  410 DO 415 I=1,NP
      SE(I)=0.
  415 CONTINUE 
      DO 440 I=1,N
        IF (SJ(I).EQ.0.) GOTO 440
        WI=SJ(I)*C
        IF (ITYPE.EQ.1) GOTO 420
        IF (WGT(I).EQ.0.) GOTO 440
        WI=WI*WGT(I)
  420   DO 430 J=1,NP
          SE(J)=SE(J)+WI*XO(I,J)
  430   CONTINUE
  440 CONTINUE
      CALL MSFZ(SC,SE,RS,NP,NCOV,1,NP,N)
      DO 460 J=1,NP
        DELTA(J)=DELTA(J)+SIGMB*RS(J)
        SE(J)=THETA(J)
        THETA(J)=THETA(J)+DELTA(J)
  460 CONTINUE
      IF (KR.NE.NP) GOTO 480
      DO 470 I=1,NP
      SF(I)=RS(I)
  470 CONTINUE  
  480 CALL RESIDU(XO,Y,THETA,N,NP,MDX,RS)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE S7ALG(XO,Y,WGT,SW,DELTA,SJ,N,NP,MDX,SIGMA,SIGMB,C,
     1                 ITYPE,KR,QR0,THETA,RS,SC,GAM,ISAME)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  TEST FOR THETA IN RYSALG (STEP 7)
C
      DIMENSION XO(MDX,NP),Y(N),WGT(N),SW(N),DELTA(NP),SJ(N),
     1          THETA(NP),RS(N),SC(NP)
      DOUBLE PRECISION SUM
      EXTERNAL RHO,PSY
      COMMON/CONST/CONST
C
C   STEP 7A.
C   --------
      GAM=1.
      CALL COMPAR(WGT,RS,SJ,C,SIGMB,N,ITYPE,ISAME)
      IF (ISAME.EQ.0.AND.KR.EQ.NP) RETURN
      ISAME=1
C
C  STEP 7B.
C  --------
      CALL QRSS(RS,WGT,SW,RHO,N,ITYPE,SIGMB,CONST,QR)
      QS=QR/FLOAT(N)
      QS0=QR0/FLOAT(N)
      IT=1
      WDG=0.
      CALL RESIDU(XO,Y,SC,N,NP,MDX,SJ)
      DO 620 J=1,NP
        SUM=0.D0
        IF (ITYPE.NE.1) GOTO 605
        DO 600 I=1,N
          T=SJ(I)/SIGMB
          SUM=SUM-XO(I,J)*DBLE(PSY(T))
  600   CONTINUE
        GOTO 615
  605   DO 610 I=1,N
          IF (WGT(I).LE.0.) GOTO 610
          T=SJ(I)/SIGMB
          IF (ITYPE.EQ.3) T=T/WGT(I)
          SUM=SUM-XO(I,J)*WGT(I)*DBLE(PSY(T))
  610   CONTINUE
  615   WDG=WDG+DELTA(J)*SNGL(SUM)
  620 CONTINUE
      WDG=WDG/FLOAT(N)
      IF (WDG.NE.0.) GOTO 630
      SIGMB=SIGMB*SIGMB/SIGMA
      GOTO 650
  630 QGW=(QS-QS0)/(GAM*WDG)
      IF (QGW.GE.1.E-2) GOTO 650
      IF (QS.LE.QS0.AND.IT.NE.1) GOTO 650
      IF (IT.GT.3) GOTO 650
      GAM=GAM/2.
      DO 640 J=1,NP
        THETA(J)=SC(J)+GAM*DELTA(J)
  640 CONTINUE
  645 IT=IT+1
      CALL RESIDU(XO,Y,THETA,N,NP,MDX,RS)
      CALL QRSS(RS,WGT,SW,RHO,N,ITYPE,SIGMB,CONST,QR)
      QS=QR/FLOAT(N)
      IF (GAM.NE.0.25) GOTO 630
C
C  STEP 7C.
C  --------
  650 IF (KR.EQ.NP) GOTO 670
C
C  STEP 7D.
C  --------
C     IF (IT.GT.10) CALL MESSGE(200,'S7ALG',0)
      IF (QS.LT.QS0.OR.IT.GT.10) GOTO 670
      GAM=GAM/2.
      DO 660 J=1,NP
        THETA(J)=0.5*(THETA(J)+SC(J))
  660 CONTINUE
      GOTO 645
  670 QR0=QR
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE S9ALG(XO,Y,WGT,SW,SJ,SX,SC,SE,SF,N,NP,MDX,
     1                 NCOV,C,ITYPE,RS,THETA,SIGMB,ISAME)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  SOLVE FOR (THETA,SIGMB) IN RYSALG (STEP 9)
C
      DIMENSION XO(MDX,NP),Y(N),WGT(N),SW(N),SJ(N),THETA(NP),
     1          SX(MDX,NP),SC(NCOV),RS(N),SE(NP),SF(NP)
      DOUBLE PRECISION SUM,SUMJ,SUM1,SUM2,SUM3,WW,YY
      COMMON/CONST/CONST
      IF (ITYPE.NE.2) THEN
        CALL GRADNT(SX,Y,N,NP,MDX,SE)
      ELSE
        DO 810 I=1,N
        RS(I)=WGT(I)*Y(I)
  810   CONTINUE 
        CALL GRADNT(SX,RS,N,NP,MDX,SE)
      ENDIF
      CALL MSFZ(SC,SE,RS,NP,NCOV,1,NP,N)
      SUM1=0.D0
      SUM2=0.D0
      SUM3=0.D0
      WI=1.0
      DO 840 I=1,N
        IF (ITYPE.EQ.2) WI=WGT(I)
        IF (SJ(I).NE.0.) GOTO 830
        SUM=0.D0
        SUMJ=0.D0
        DO 820 J=1,NP
          SUM=SUM+DBLE(XO(I,J)*RS(J))
          SUMJ=SUMJ+DBLE(XO(I,J)*SF(J))
  820   CONTINUE
        WW=DBLE(WI)
        YY=DBLE(Y(I))
        SUM1=SUM1+WW*(YY-SUM)**2
        SUM2=SUM2+WW*SUMJ**2
        GOTO 840
  830   S3=WI
        IF (ITYPE.EQ.3) S3=SW(I)
        SUM3=SUM3+DBLE(S3)
  840 CONTINUE
      SUM2=SUM2+SUM3*C*C-2.D0*CONST
      S=-SNGL(SUM1/SUM2)
      IF (S.LE.0.) GOTO 870
      SIGMB=SQRT(S)
      DO 850 J=1,NP
        THETA(J)=RS(J)+SIGMB*SF(J)
  850 CONTINUE
      CALL RESIDU(XO,Y,THETA,N,NP,MDX,RS)
      ISAME=1
      CALL COMPAR(WGT,RS,SJ,C,SIGMB,N,ITYPE,ISAME)
      RETURN
  870 ISAME=-1
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE GRADNT(X,HBRS,N,NP,MDX,GRAD)
      DIMENSION X(MDX,NP),HBRS(N),GRAD(NP)
      DOUBLE PRECISION SUM
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DO 20 J=1,NP
      SUM=0.D0
      DO 10 I=1,N
      SUM=SUM+DBLE(X(I,J)*HBRS(I))
   10 CONTINUE 
      GRAD(J)=SNGL(SUM)
   20 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE STPLNG(X,Y,THETA,DELTA,WGT,WGT2,GRAD,
     1                  EXPSI,EXRHO,
     2                  N,NP,MDX,ITYPE,SIGMA,CONST,QS0,QS1,GAM,IER,
     3                  ST,SR)
      DIMENSION X(MDX,NP),Y(N),THETA(NP),DELTA(NP),WGT(N),WGT2(N),
     1          GRAD(NP),ST(NP),SR(N)
      EXTERNAL EXRHO,EXPSI
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      IER=0
      CALL DOTPZ(DELTA,GRAD,NP,1,1,NP,NP,S0)
      S0=S0/(-SIGMA)
      ETA=1.
      IF (S0.NE.0.) ETA=AMIN1(1.,-2.*QS0/S0)
      IF (ETA.NE.1.) THEN
        DO 10 J=1,NP
        ST(J)=THETA(J)+ETA*DELTA(J)
   10   CONTINUE 
        CALL RESIDU(X,Y,ST,N,NP,MDX,SR)
        CALL QRSS(SR,WGT,WGT2,EXRHO,N,ITYPE,SIGMA,CONST,QS1)
      ENDIF
      CALL HUB(SR,WGT,WGT,SIGMA,N,ITYPE,EXPSI)
      CALL GRADNT(X,SR,N,NP,MDX,GRAD)
      CALL DOTPZ(DELTA,GRAD,NP,1,1,NP,NP,S1)
      S1=S1/(-SIGMA)
      Z=(3./ETA)*(QS0-QS1)+S0+S1
      A=Z*Z-S0*S1
      W=0.
      IF (A.GT.0.) W=SQRT(A)
      IF (S1-S0+2.*W .EQ. 0.) GOTO 20
      GAM=ETA*(1.-(S1+W-Z)/(S1-S0+2.*W))
      RETURN
   20 IER=1
      GAM=1.
      RETURN
      END
C
C * * SET THE LIMIT OF INTEGRATION * * *
      SUBROUTINE LIMINT(UPPER)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,DCHI
      UPP=6.
      IPS=IABS(IPSI)
      IF (IPS.EQ.2) UPP=AMIN1(6.,H3)
      IF (IPS.EQ.3) UPP=3.1416
      IF (IPS.EQ.4) UPP=1.
      IF (UPPER.LE.0.) UPPER=UPP
      RETURN
      END
C
C----------------------------------------------------------------------
C
      FUNCTION ICSIGM(SIGMA,SIGMB,TOL)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      DS=ABS(SIGMA-SIGMB)/AMAX1(1.,SIGMA)
      ICSIGM=0
      IF (TOL.GE.DS) ICSIGM=1
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION ICTHET(NP,NCOV,DELTA,SIGMA,S,TOL,ICNV)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION DELTA(NP),S(NCOV)
      ICTHET=0
      TOL1=TOL*SIGMA
      IF (ICNV.EQ.2) GOTO 200
      IF (ICNV.EQ.3) GOTO 300
      L=0
      DO 100 J=1,NP
      L=L+J
      TOL2=TOL1*SQRT(S(L))
      IF (TOL2.LT.ABS(DELTA(J))) RETURN
  100 CONTINUE
      GOTO 500
  200 CALL XSYZ(DELTA,DELTA,S,NP,NCOV,TOL2)
      TOL2=SQRT(TOL2)
      IF (TOL1.GE.TOL2) ICTHET=1
      RETURN
  300 L=0
      DO 350 J=1,NP
      L=L+J
      TOL2=ABS(DELTA(J))*SQRT(S(L))
      IF (TOL1.LT.TOL2) RETURN
  350 CONTINUE
  500 ICTHET=1
      RETURN
      END
C-----------------------------------------------------------------------
C
C                     R O B E T H  FORTRAN Source
C
C  File RGMAIN.F  Main subroutines of Chapter 2
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RIBETHZ(WGT,N,D,ITYPE,BTA)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION WGT(N)
      LOGICAL NPRCHK
      COMMON/BETA/BETA,BET0
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.D.GT.0..AND.
     1       (ITYPE.EQ.1.OR.ITYPE.EQ.2.OR.ITYPE.EQ.3)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RIBETH',1)
      SM=0.
      XN=FLOAT(N)
      C2=D*D
      IF (ITYPE.EQ.3) GOTO 30
C
C  HUBER CASE
C
      CALL GAUSSZ(1,D,PC)
      CALL XERFZ(2,D,DC)
      BETA=-D*DC+PC-.5+C2*(1.-PC)
      BTA=BETA
      IF (ITYPE.EQ.1) RETURN
C
C  MALLOWS CASE
C
      DO 20 I=1,N
      SM=SM+WGT(I)
   20 CONTINUE
      BETA=SM*BETA/XN
      BTA=BETA
      RETURN
C
C  SCHWEPPE CASE
C
   30 DO 40 I=1,N
      W2=WGT(I)*WGT(I)
      CW=D*WGT(I)
      CALL GAUSSZ(1,CW,PC)
      CALL XERFZ(2,CW,DC)
      B=C2*(1.-PC)+(-CW*DC+PC-.5)/W2
      SM=SM+B*W2/XN
   40 CONTINUE
      BETA=SM
      BTA=BETA
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RIBETU(WGT,EXCHI,N,ITYPE,UPPER,TIL,ERREST,BTA)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION WGT(N)
      LOGICAL NPRCHK
      EXTERNAL EXCHI,CHIPHI
      COMMON/BETA/BETA,BET0
      COMMON/INTPAR/ITYP,I,NEVAL,LIMIT,KEY
      COMMON/INTEG/UUPER,TTIL,IWORK(40),WORK(160),IER1,ERRST1
C
C  PARAMETERS CHECK
C
      NPRCHK=N.GT.0.AND.(ITYPE.EQ.1.OR.ITYPE.EQ.2.OR.ITYPE.EQ.3)
     1      .AND.UPPER.GT.0..AND.TIL.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RIBETU',1)
C
      XN=FLOAT(N)
      SM=0.
      ITYP=ITYPE
      LIMIT=40
      KEY=1
      CALL INTGRS(CHIPHI,WGT,N,EXCHI,EXCHI,0.,UPPER,TIL,TIL,KEY,
     1          LIMIT,BETA,ERREST,NEVAL,IER,WORK,IWORK)
      IF (IER.GT.0) CALL MESSGE(400+IER,'RIBETU',0)
      IF (ITYPE.EQ.3) GOTO 30
C
C  HUBER CASE
C
      BETA=2.*BETA
      BTA=BETA
      IF (ITYPE.EQ.1) RETURN
C
C  MALLOWS CASE
C
      DO 20 I=1,N
      SM=SM+WGT(I)
   20 CONTINUE   
      BETA=(SM/XN)*BETA
      BTA=BETA
      RETURN
C
C  SCHWEPPE CASE
C
   30 BETA=(2./XN)*BETA
      BTA=BETA
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RIBET0Z(WGT,N,ITYPE,ISQW,TOL,BT0)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI
C.......................................................................
C
      DIMENSION WGT(N)
      LOGICAL NPRCHK
      COMMON/BETA/BETA,BET0
C
C  PARAMETER CHECK
C
      NPRCHK=N.GT.0.AND.TOL.GT.0..AND.
     1       (ITYPE.EQ.1.OR.ITYPE.EQ.3.OR.(ITYPE.EQ.2
     2       .AND.(ISQW.EQ.-1.OR.ISQW.EQ.0.OR.ISQW.EQ.1)))
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RIBET0',1)
      P=.75
      E=1.0
      CALL NQUANT(P,BET0)
      BT0=BET0
      IF (ITYPE.NE.2) RETURN
      XN=FLOAT(N)
      IF (ISQW.EQ.0) GOTO 10
      E=2.
      IF (ISQW.EQ.1) E=0.5
      DO 5 I=1,N
      IF (WGT(I).LE.0.) GOTO 5
      WGT(I)=WGT(I)**E
    5 CONTINUE
   10 BET0=0.
   20 SMF=0.
      SMFP=0.
      DO 30 I=1,N
      IF (WGT(I).LE.0.) GOTO 30
      X=BET0/WGT(I)
      CALL GAUSSZ(1,X,A)
      CALL XERFZ(2,X,B)
      SMF=SMF+A
      SMFP=SMFP+B/WGT(I)
   30 CONTINUE
      F=SMF/XN-P
      FP=SMFP/XN
      BET0=BET0-F/FP
      IF (ABS(F).LT.TOL) GOTO 40
      GOTO 20
   40 BT0=BET0
      IF (ISQW.EQ.0) RETURN
      E=1./E
      DO 45 I=1,N
      IF (WGT(I).LE.0.) GOTO 45
      WGT(I)=WGT(I)**E
   45 CONTINUE
      BT0=BET0
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RILARSZ(X,Y,N,NP,MDX,MDT,TOL,NIT,K,KODE,
     +                  SIGMA,THETA,RS,SC1,SC2,SC3,SC4)
C.......................................................................
C
C   AUTHORS :     BARRODALE I. AND ROBERTS F.D.K. (1974)
C                 ALGORITHM 478: SOLUTION OF AN OVERDETERMINED
C                 SYSTEM OF EQUATION IN THE L1-NORM.
C                 COMMUNICATIONS OF THE ACM, VOL.17,6,PP.319-320
C                 ADAPTATION TO ROBETH BY A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION SUM
      REAL X(MDX,NP),Y(N),THETA(MDT),RS(N)
      REAL SC1(N),SC2(NP),SC3(NP),SC4(NP)
      INTEGER OUT
      LOGICAL STAGE,TEST,NPRCHK
      COMMON/BETA/BETA,BET0
C
C  PARAMETER CHECK
C
      NPRCHK=N.GE.NP.AND.NP.GT.0.AND.MDX.GE.N.AND.MDT.GE.N
     1       .AND.TOL.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RILARS',1)
C
C  INITIALIZATION (BIG MUST BE SET TO ANY VERY LARGE CONSTANT).
C
      CALL MACHZ(6,BIG)
      SUM=0.D0
      DO 10 J=1,NP
        SC4(J)=FLOAT(J)
        SC2(J)=0.
   10 CONTINUE
      DO 40 I=1,N
        SC1(I)=FLOAT(NP+I)
        THETA(I)=Y(I)
        IF (Y(I).GE.0.) GOTO 30
        DO 20 J=1,NP
          X(I,J)=-X(I,J)
   20   CONTINUE
        THETA(I)=-THETA(I)
        SC1(I)=-SC1(I)
   30   SUM=SUM+THETA(I)
   40 CONTINUE
C
C  COMPUTE THE MARGINAL COSTS.
C
      SUMIN=SNGL(SUM)
      DO 60 J=1,NP
        SUM=0.D0
        DO 50 I=1,N
          SUM=SUM+DBLE(X(I,J))
   50   CONTINUE
        SC3(J)=SNGL(SUM)
   60 CONTINUE
C
C  STAGE I.
C  DETERMINE THE VECTOR TO ENTER THE BASIS.
C
      STAGE=.TRUE.
      KOUNT=0
      KR=1
      KL=1
      IN=1
   70 VMAX=-1.
      DO 80 J=KR,NP
        IF (ABS(SC4(J)).GT.FLOAT(NP)) GOTO 80
        D=ABS(SC3(J))
        IF (D.LE.VMAX) GOTO 80
        VMAX=D
        IN=J
   80 CONTINUE
      IF (SC3(IN).GE.0.) GOTO 100
      DO 90 I=1,N
        X(I,IN)=-X(I,IN)
   90 CONTINUE
      SC3(IN)=-SC3(IN)
      SC4(IN)=-SC4(IN)
C
C  DETERMINE THE VECTOR TO LEAVE THE BASIS.
C
  100 K=0
      DO 110 I=KL,N
        D=X(I,IN)
        IF (D.LE.TOL) GOTO 110
        K=K+1
        Y(K)=THETA(I)/D
        RS(K)=FLOAT(I)
        TEST=.TRUE.
  110 CONTINUE
  120 IF (K.GT.0) GOTO 130
      TEST=.FALSE.
      GOTO 150
  130 VMIN=BIG
      DO 140 I=1,K
        IF (Y(I).GE.VMIN) GOTO 140
        J=I
        VMIN=Y(I)
        OUT=INT(RS(I))
  140 CONTINUE
      Y(J)=Y(K)
      RS(J)=RS(K)
      K=K-1
C
C  CHECK FOR LINEAR DEPENDENCE IN STAGE I.
C
  150 IF (TEST.OR..NOT.STAGE) GOTO 170
      DO 160 I=1,N
        CALL INTCHG(X(I,KR),X(I,IN))
  160 CONTINUE
      CALL INTCHG(SC3(KR),SC3(IN))
      CALL INTCHG(SC4(KR),SC4(IN))
      KR=KR+1
      GOTO 260
  170 IF (TEST) GOTO 180
      KODE=2
      GOTO 350
  180 PIVOT=X(OUT,IN)
      IF (SC3(IN)-PIVOT-PIVOT.LE.TOL) GOTO 200
      DO 190 J=KR,NP
        D=X(OUT,J)
        SC3(J)=SC3(J)-D-D
        X(OUT,J)=-D
  190 CONTINUE
      D=THETA(OUT)
      SUMIN=SUMIN-D-D
      THETA(OUT)=-D
      SC1(OUT)=-SC1(OUT)
      GOTO 120
C
C  PIVOT ON X(OUT,IN).
C
  200 DO 210 J=KR,NP
        IF (J.EQ.IN) GOTO 210
        X(OUT,J)=X(OUT,J)/PIVOT
  210 CONTINUE
      THETA(OUT)=THETA(OUT)/PIVOT
      DO 230 J=KR,NP
        IF (J.EQ.IN) GOTO 230
        D=X(OUT,J)
        SC3(J)=SC3(J)-D*SC3(IN)
        CALL COL(X(1,J),X(1,IN),D,N,OUT)
  230 CONTINUE
      SUMIN=SUMIN-SC3(IN)*THETA(OUT)
      DO 240 I=1,N
        IF (I.EQ.OUT) GOTO 240
        D=X(I,IN)
        THETA(I)=THETA(I)-D*THETA(OUT)
        X(I,IN)=-D/PIVOT
  240 CONTINUE
      SC3(IN)=-SC3(IN)/PIVOT
      X(OUT,IN)=1./PIVOT
      CALL INTCHG(SC1(OUT),SC4(IN))
      KOUNT=KOUNT+1
      IF (.NOT.STAGE) GOTO 270
C
C  INTERCHANGE ROWS IN STAGE I.
C
      KL=KL+1
      DO 250 J=KR,NP
        CALL INTCHG(X(OUT,J),X(KOUNT,J))
  250 CONTINUE
      CALL INTCHG(THETA(OUT),THETA(KOUNT))
      CALL INTCHG(SC1(OUT),SC1(KOUNT))
  260 IF (KOUNT+KR.NE.NP+1) GOTO 70
C
C  STAGE II.
C
      STAGE=.FALSE.
C
C  DETERMINE THE VECTOR TO ENTER THE BASIS.
C
  270 VMAX=-BIG
      DO 290 J=KR,NP
        D=SC3(J)
        IF (D.GE.0.) GOTO 280
        IF (D.GT.-2.) GOTO 290
        D=-D-2.
  280   IF (D.LE.VMAX) GOTO 290
        VMAX=D
        IN=J
  290 CONTINUE
      IF (VMAX.LE.TOL) GOTO 310
      IF (SC3(IN).GT.0.) GOTO 100
      DO 300 I=1,N
      X(I,IN)=-X(I,IN)
  300 CONTINUE
      SC3(IN)=-SC3(IN)-2.
      SC4(IN)=-SC4(IN)
      GOTO 100
C
C  PREPARE OUTPUT
C
  310 L=KL-1
      DO 330 I=1,N
        RS(I)=0.
        IF (I.GT.L.OR.THETA(I).GE.0.) GOTO 330
        DO 320 J=KR,NP
          X(I,J)=-X(I,J)
  320   CONTINUE
        THETA(I)=-THETA(I)
        SC1(I)=-SC1(I)
  330 CONTINUE
      KODE=0
      IF (KR.NE.1) GOTO 350
      DO 340 J=1,NP
        D=ABS(SC3(J))
        IF (D.LE.TOL.OR.2.-D.LE.TOL) GOTO 350
  340 CONTINUE
      KODE=1
  350 DO 380 I=1,N
        K=INT(SC1(I))
        D=THETA(I)
        IF (K.GT.0) GOTO 360
        K=-K
        D=-D
  360   IF (I.GE.KL) GOTO 370
        SC2(K)=D
        GOTO 380
  370   K=K-NP
        RS(K)=D
  380 CONTINUE
      K=NP+1-KR
      SUM=0.D0
      DO 390 I=KL,N
        SUM=SUM+DBLE(THETA(I))
  390 CONTINUE
      SUMIN=SNGL(SUM)
      NIT=KOUNT
      DO 400 J=1,NP
        THETA(J)=SC2(J)
  400 CONTINUE
      DO 500 I=1,N
      Y(I)=ABS(RS(I))
  500 CONTINUE
      N2=N/2+1
      CALL FSTORDZ(Y,N,N2,SIGMA)
      SIGMA=SIGMA/BET0
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RIMTRFZ(X,N,NP,MDX,INTCH,TAU,K,SF,SG,SH,IP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR: A. MARAZZI
C.......................................................................
C
      DIMENSION X(MDX,NP),SF(NP),SG(NP),SH(NP)
      INTEGER IP(NP)
      LOGICAL NPRCHK
      EXTERNAL DIFF
C
C  PARAMETER CHECK AND INITIALIZATION
C
      FACTOR=0.001
      LDIAG=MIN0(N,NP)
      NPRCHK=LDIAG.GT.0.AND.N.LE.MDX.AND.TAU.GE.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RIMTRF',1)
C
      DO 80 JJ=1,LDIAG
      J=JJ
      IF (INTCH.EQ.0) IP(J)=J
      IF (INTCH.EQ.0) GOTO 70
      IF (J.EQ.1) GOTO 20
C
C  UPDATE SQUARED COLUMN LENGTHS AND FIND LMAX
C
      LMAX=J
      DO 10 L=J,NP
      SH(L)=SH(L)-X(J-1,L)**2
      IF(SH(L).GT.SH(LMAX)) LMAX=L
   10 CONTINUE
      IF (DIFF(HMAX+FACTOR*SH(LMAX),HMAX).GT.0.0) GOTO 50      
C
C  COMPUTE SQUARED COLUMN LENGTHS AND FIND LMAX
C
   20 LMAX=J
      DO 40 L=J,NP
      SH(L)=0.
      DO 30 I=J,N
      SH(L)=SH(L)+X(I,L)**2
   30 CONTINUE
      IF (SH(L).GT.SH(LMAX)) LMAX=L
   40 CONTINUE
      HMAX=SH(LMAX)
C
C  LMAX HAS BEEN DETERMINED: INTERCHANGE COLUMNS IF NEEDED
C
   50 CONTINUE
      IP(J)=LMAX
      IF (IP(J).EQ.J) GOTO 70
      DO 60 I=1,N
      TMP=X(I,J)
      X(I,J)=X(I,LMAX)
      X(I,LMAX)=TMP
   60 CONTINUE
      SH(LMAX)=SH(J)
C
C  COMPUTE THE HOUSEHOLDER TRANSF. Q AND APPLY IT TO X
C
   70 MDC=NP-J
      IF (MDC.GT.0)
     1CALL H12Z(1,J,J+1,N,X(1,J),1,SH(J),X(1,J+1),1,MDX,MDC,MDX*MDC)
      IF (MDC.EQ.0)
     1CALL H12Z(1,J,J+1,N,X(1,J),1,SH(J),SF,1,1,0,1)
   80 CONTINUE
C
C  X CONTAINS NOW THE TRANSFORMED DESIGN MATRIX Q*X.
C  DETERMINE THE PSEUDORANK K USING THE TOLERANCE TAU
C
      DO 100 J=1,LDIAG
      IF (ABS(X(J,J)).LE.TAU) GOTO 110
  100 CONTINUE
      K=LDIAG
      GOTO 120
  110 K=J-1
  120 KP1=K+1
C
C  IF THE PSEUDORANK IS LESS THAN NP STORE THE FIRST K
C  DIAG.ELEMENTS OF X FOR FURTHER APPLICATIONS OF Q
C
      IF (K.EQ.NP) GOTO 130
      DO 125 I=1,K
      SF(I)=X(I,I)
  125 CONTINUE
  130 CONTINUE
C
C  SPECIAL FOR PSEUDORANK=0
C
      IF (K.GT.0) GOTO 140
      CALL MESSGE(401,'RIMTRF',0)
      RETURN
C
C  IF THE PSEUDORANK IS LESS THAN NP COMPUTE Q*X*V
C
  140 IF (K.EQ.NP) GOTO 160
      MDC=MDX*(NP-1)
      DO 150 II=1,K
      I=KP1-II
      CALL H12Z(1,I,KP1,NP,X(I,1),MDX,SG(I),X,MDX,1,I-1,MDC+I-1)
  150 CONTINUE
  160 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RIMTRDZ(X,N,NP,MDX,INTCH,TAU,K,SF,SG,SH,IP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION X(MDX,NP),SF(NP),SG(NP),SH(NP)
      DOUBLE PRECISION TAUD,TMP,HMAX,FACTOR
      INTEGER IP(NP)
      LOGICAL NPRCHK
      EXTERNAL DIFFD
C
C  PARAMETER CHECK AND INITIALIZATION
C
      FACTOR=1.D-3
      LDIAG=MIN0(N,NP)
      NPRCHK=LDIAG.GT.0.AND.N.LE.MDX.AND.TAU.GE.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RIMTRD',1)
C
      TAUD=DBLE(TAU)
      DO 80 JJ=1,LDIAG
      J=JJ
      IF (INTCH.EQ.0) IP(J)=J
      IF (INTCH.EQ.0) GOTO 70
      IF (J.EQ.1) GOTO 20
C
C  UPDATE SQUARED COLUMN LENGTHS AND FIND LMAX
C
      LMAX=J
      DO 10 L=J,NP
      SH(L)=SH(L)-X(J-1,L)**2
      IF(SH(L).GT.SH(LMAX)) LMAX=L
   10 CONTINUE
      IF (DIFFD(HMAX+FACTOR*SH(LMAX),HMAX).GT.0.D0) GOTO 50
C
C  COMPUTE SQUARED COLUMN LENGTHS AND FIND LMAX
C
   20 LMAX=J
      DO 40 L=J,NP
      SH(L)=0.D0
      DO 30 I=J,N
      SH(L)=SH(L)+X(I,L)**2
   30 CONTINUE
      IF (SH(L).GT.SH(LMAX)) LMAX=L
   40 CONTINUE
      HMAX=SH(LMAX)
C
C  LMAX HAS BEEN DETERMINED: INTERCHANGE COLUMNS IF NEEDED
C
   50 CONTINUE
      IP(J)=LMAX
      IF (IP(J).EQ.J) GOTO 70
      DO 60 I=1,N
      TMP=X(I,J)
      X(I,J)=X(I,LMAX)
      X(I,LMAX)=TMP
   60 CONTINUE
      SH(LMAX)=SH(J)
C
C  COMPUTE THE HOUSEHOLDER TRANSF. Q AND APPLY IT TO X
C
   70 MDC=NP-J
      IF (MDC.GT.0)
     1CALL H12ZD(1,J,J+1,N,X(1,J),1,SH(J),X(1,J+1),1,MDX,MDC,MDX*MDC)
      IF (MDC.EQ.0)
     1CALL H12ZD(1,J,J+1,N,X(1,J),1,SH(J),SF,1,1,0,1)
   80 CONTINUE
C
C  X CONTAINS NOW THE TRANSFORMED DESIGN MATRIX Q*X.
C  DETERMINE THE PSEUDORANK K USING THE TOLERANCE TAU
C
      DO 100 J=1,LDIAG
      IF (DABS(X(J,J)).LE.TAUD) GOTO 110
  100 CONTINUE
      K=LDIAG
      GOTO 120
  110 K=J-1
  120 KP1=K+1
C
C  IF THE PSEUDORANK IS LESS THAN NP STORE THE FIRST K
C  DIAG.ELEMENTS OF X FOR FURTHER APPLICATIONS OF Q
C
      IF (K.EQ.NP) GOTO 130
      DO 125 I=1,K
      SF(I)=X(I,I)
  125 CONTINUE
  130 CONTINUE
C
C  SPECIAL FOR PSEUDORANK=0
C
      IF (K.GT.0) GOTO 140
      CALL MESSGE(401,'RIMTRD',0)
      RETURN
C
C  IF THE PSEUDORANK IS LESS THAN NP COMPUTE Q*X*V
C
  140 IF (K.EQ.NP) GOTO 160
      MDC=MDX*(NP-1)
      DO 150 II=1,K
      I=KP1-II
      CALL H12ZD(1,I,KP1,NP,X(I,1),MDX,SG(I),X,MDX,1,I-1,MDC+I-1)
  150 CONTINUE
  160 CONTINUE
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE RICLLSZ(XT,Y,N,NP,MDX,MDT,K,IX,IY,SIGMA,THETA,RS1,RS2,
     1                   SE,SF,SG,SH,IP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR: A. MARAZZI
C.......................................................................
C
      DIMENSION XT(MDX,NP),Y(N),THETA(MDT),RS1(N),RS2(N),
     1          SE(NP),SF(NP),SG(NP),SH(NP)
      INTEGER IP(NP)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK AND INITIALIZATION
C
      KP1=K+1
      MDXP1=MDX+1
      KK=MDX*(K-1)+K
      LDIAG=MIN0(N,NP)
      MX=MAX0(N,NP)
      NPRCHK=LDIAG.GT.0.AND.N.LE.MDX.AND.MDT.GE.MX
     1       .AND.((IX.EQ.0.AND.K.GT.0.AND.K.LE.LDIAG).OR.
     2       IX.EQ.1).AND.(IY.EQ.0.OR.IY.EQ.1)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RICLLS',1)
      IF (IX.EQ.1) CALL RIMTRFZ(XT,N,NP,MDX,0,1.E-6,K,SF,SG,SH,IP)
C
C  HOUSEHOLDER TRANSFORMATIONS OF THE OBSERVATION VECTOR
C
      IF (IY.EQ.0) GOTO 25
      IF (K.NE.NP) CALL SWAPZ(XT,SF,K,MDXP1,1,KK,K)
      DO 20 JJ=1,LDIAG
      J=JJ
      CALL H12Z(2,J,J+1,N,XT(1,J),1,SH(J),Y,1,N,1,N)
   20 CONTINUE
      IF (K.NE.NP) CALL SWAPZ(XT,SF,K,MDXP1,1,KK,K)
C
C  SOLVE THE TRANSFORMED LS-PROBLEM
C
   25 DO 30 I=1,N
      THETA(I)=Y(I)
   30 CONTINUE 
      CALL SOLV(XT,THETA,NP,K,MDX,MDT)
      IF (K.EQ.NP) GOTO 50
      DO 40 J=KP1,NP
      THETA(J)=0.0
   40 CONTINUE
   50 CONTINUE
C
C  COMPUTE THE TRANSFORMED RESIDUAL VECTORS Q*(Y-X1*THETA) (RS1)
C  AND (IF K.LT.NP) Q*(Y-X*THETA) (RS2)
C
      IF (K.EQ.NP) GOTO 60
      CALL RES(3,XT,Y,THETA,RS2,SE,SG,N,NP,K,NP,MDX,MDT)
   60 CALL RES(1,XT,Y,THETA,RS1,SE,SG,N,NP,K,NP,MDX,MDT)
C
C  COMPUTE SIGMA
C
      SIGMA=0.
      IF (K.EQ.N) GOTO 80
      CALL NRM2Z(RS1,N,1,N,SIGMA)
      SIGMA=SIGMA/SQRT(FLOAT(N-K))
   80 IF (SIGMA.GT.0.) GOTO 90
      CALL MESSGE(101,'RICLLS',0)
   90 CONTINUE
C
C  TRANSFORM RESIDUAL VECTORS FOR OUTPUT
C
      IF (K.NE.NP) CALL SWAPZ(XT,SF,K,MDXP1,1,KK,K)
      DO 105 J1=1,LDIAG
      J=LDIAG-J1+1
      IF (K.EQ.NP) GOTO 100
      CALL H12Z(2,J,J+1,N,XT(1,J),1,SH(J),RS2,1,N,1,N)
  100 CALL H12Z(2,J,J+1,N,XT(1,J),1,SH(J),RS1,1,N,1,N)
  105 CONTINUE
      IF (K.NE.NP) CALL SWAPZ(XT,SF,K,MDXP1,1,KK,K)
C
C  TRANSFORM SOLUTION VECTOR FOR OUTPUT
C
      IF (K.EQ.NP) GOTO 120
      DO 110 J=1,K
      I=J
      CALL H12Z(2,I,KP1,NP,XT(I,1),MDX,SG(I),THETA,1,N,1,NP)
  110 CONTINUE
  120 CALL PERM(THETA,IP,LDIAG,NP)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RYHALG(X,Y,THETA,WGT,COV,EXPSI,EXCHI,EXRHO,SIGMAI,
     1                  N,NP,MDX,MDT,NCOV,K,TOL,GAM,TAU,ITYPE,IX,IY,IC,
     1                  ISIGMA,ICNV,MAXIT,MAXIS,NITMON,NIT,SIGMAF,RS1,
     1                  RS2,DELTA,SC,SE,SF,SG,SH,IP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(MDX,NP),Y(N),WGT(N),THETA(MDT),RS1(N),RS2(N),
     1          DELTA(NP),COV(NCOV),SC(N),SE(NP),SF(NP),SG(NP),
     2          SH(NP)
      INTEGER IP(NP)
      LOGICAL NPRCHK
      EXTERNAL EXPSI,EXCHI,EXRHO
      COMMON/CONST/CONST
      COMMON/BETA/BETA,BET0
C     SAVE /CONST/
C
C   PARAMETER CHECK AND INITIALIZATION
C
      KP1=K+1
      MDXP1=MDX+1
      KK=MDX*(K-1)+K
      KS=K*(K+1)/2
      LDIAG=MIN0(N,NP)
      MX=MAX0(N,NP)
      NN=NP*(NP+1)/2
      SIGMA=SIGMAI
      IASG=IABS(ISIGMA)
      NPRCHK=LDIAG.GT.0.AND.N.LE.MDX.AND.MDT.GE.MX.AND.NCOV.EQ.NN
     1      .AND.K.GT.0.AND.K.LE.LDIAG.AND.MAXIT.GT.0.AND.(MAXIS.GT.0
     1      .OR.IASG.NE.1).AND.SIGMA.GE.0..AND.TOL.GT.0..AND.(GAM.GT.0.
     1      .AND.GAM.LT.2.).AND.(IX.EQ.0.OR.IX.EQ.1).AND.(IY.EQ.0.OR.
     1       IY.EQ.1).AND.(IC.EQ.0.OR.IC.EQ.1).AND.(ITYPE.EQ.1.OR.
     1      ITYPE.EQ.2.OR.ITYPE.EQ.3).AND.(ISIGMA.GE.-2.AND.ISIGMA.LE.2)
     1      .AND.(ICNV.EQ.1.OR.ICNV.EQ.2.OR.ICNV.EQ.3).AND.TAU.GE.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RYHALG',1)
      ITYP=ITYPE
      DO 5 J=1,NP
      DELTA(J)=0.0
    5 CONTINUE
      IF (ITYP.EQ.1) GOTO 20
      E=2.0
      IF (ITYP.EQ.2) E=0.5
      N0=N
      DO 10 I=1,N
        IF (WGT(I).LE.0.) THEN
          RS2(I)=-1.
          N0=N0-1
        ELSE
          RS2(I)=WGT(I)**E
        ENDIF
   10 CONTINUE
      IF (N0.EQ.0) ITYP=1
   20 CONTINUE
      IF (ISIGMA.EQ.0) C0NST=0.
      IF (IASG.EQ.1) CONST=BETA*FLOAT(N-K)
      IF (IASG.EQ.2) CONST=BET0*FLOAT(N-K)
      IF (IX.EQ.1) CALL RIMTRFZ(X,N,NP,MDX,1,TAU,K,SF,SG,SH,IP)
C
C  HOUSEHOLDER TRANSFORMATIONS OF THE Y-VECTOR (WHEN IY.EQ.1)
C
      IF (IY.EQ.0) GOTO 35
      IF (K.NE.NP) CALL SWAPZ(X,SF,K,MDXP1,1,KK,K)
      DO 30 JJ=1,LDIAG
      J=JJ
      CALL H12Z(2,J,J+1,N,X(1,J),1,SH(J),Y,1,N,1,N)
   30 CONTINUE
      IF (K.NE.NP) CALL SWAPZ(X,SF,K,MDXP1,1,KK,K)
   35 CONTINUE
C
C  COMPUTE (V**T)*(P**T)*COV*P*V (WHEN IC.EQ.1)
C
      IF (IC.EQ.0) GOTO 50
      DO 40 JJ=1,NP
      J=JJ
      IF (IP(J).EQ.J) GOTO 40
      L=IP(J)
      CALL EXCHZ(COV,NP,NCOV,J,L)
   40 CONTINUE
      IF (K.EQ.NP) GOTO 50
      DO 45 II=1,K
      I=K-II+1
      CALL VSV(I,KP1,NP,X(I,1),MDX,SG(I),COV,NCOV,DELTA)
   45 CONTINUE
C
C  TRANSFORM THE GIVEN SOLUTION OF THE PROBLEM X1*THETA=Y
C
   50 CALL PVM1(X,THETA,IP,SG,N,NP,K,MDX,MDT)
C
C  SPECIAL FOR SIGMA.EQ.0 OR K.GE.N
C
      IF (SIGMA.GT.0..AND.N.GT.K) GOTO 70
      CALL MESSGE(102,'RYHALG',0)
      NIT=0
      GOTO 235
   70 CONTINUE
C
C  ITERATIONS
C
      CALL HALG(ITYP,X,Y,THETA,WGT,RS2,COV,RS1,DELTA,SIGMA,ISIGMA,
     1          N,NP,MDX,MDT,NCOV,K,TOL,GAM,MAXIT,MAXIS,NITMON,ICNV,
     1          NIT,EXPSI,EXCHI,EXRHO,SC,SE,SF,SG,SH)
C
C  COMPUTE RESIDUALS FOR OUTPUT
C
  235 IF (K.EQ.NP) GOTO 240
      CALL RES(3,X,Y,THETA,RS2,SE,SG,N,NP,K,NP,MDX,MDT)
  240 CALL RES(2,X,Y,THETA,RS1,SE,SG,N,NP,K,NP,MDX,MDT)
      IF (K.NE.NP) CALL SWAPZ(X,SF,K,MDXP1,1,KK,K)
      DO 255 J1=1,LDIAG
      J=LDIAG-J1+1
      IF (K.EQ.NP) GOTO 250
      CALL H12Z(2,J,J+1,N,X(1,J),1,SH(J),RS2,1,N,1,N)
  250 CALL H12Z(2,J,J+1,N,X(1,J),1,SH(J),RS1,1,N,1,N)
  255 CONTINUE
      IF (K.NE.NP) CALL SWAPZ(X,SF,K,MDXP1,1,KK,K)
      IF (ITYP.EQ.2) THEN
        DO 260 I=1,N
        IF (WGT(I).LE.0.) GOTO 260
        SQW=SQRT(WGT(I))
        RS1(I)=RS1(I)/SQW
  260   CONTINUE
      ENDIF
C
C  TRANSFORM AND REORDER THE SOLUTION VECTOR AND THE LAST INCREMENT
C  VECTOR TO COMPENSATE HOUSEHOLDER TRANSF. AND COLUMN INTERCHANGES
C
      IF (K.EQ.NP) GOTO 280
      DO 270 J=1,K
      I=J
      CALL H12Z(2,I,KP1,NP,X(I,1),MDX,SG(I),THETA,1,N,1,NP)
      CALL H12Z(2,I,KP1,NP,X(I,1),MDX,SG(I),DELTA,1,N,1,NP)
  270 CONTINUE
  280 CALL PERM(THETA,IP,LDIAG,NP)
      CALL PERM(DELTA,IP,LDIAG,NP)
      SIGMAF=SIGMA
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RYWALG(X,Y,THETA,WGT,COV,PSP0,EXPSI,EXCHI,EXRHO,
     1                  SIGMAI,N,NP,MDX,MDT,NCOV,TOL,GAM,TAU,ITYPE,
     1                  ISIGMA,ICNV,MAXIT,MAXIS,NITMON,NIT,SIGMAF,RS,
     1                  DELTA,SC,SF,SG,SH,IP,SW,SX)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR: A. RANDRIAMIHARISOA
C.......................................................................
C
C   PURPOSE
C   -------
C   W-ALGORITHM FOR ROBUST AND BOUNDED INFLUENCE LINEAR REGRESSION
C
      DIMENSION X(MDX,NP),Y(N),THETA(MDT),WGT(N),COV(NCOV),RS(N),
     1 DELTA(NP),SC(N),SF(NP),SG(NP),SH(NP),SW(N),SX(MDX,NP)
      INTEGER IP(NP)
      LOGICAL NPRCHK
      EXTERNAL EXPSI,EXCHI,EXRHO,ICTHET,ICSIGM
      COMMON/CONST/CONST
      COMMON/BETA/BETA,BET0
      DATA TL/1.E-8/
C     SAVE /CONST/
C
C   PARAMETER CHECK AND INITIALIZATION
C
      MDXP1=MDX+1
      LDIAG=MIN0(N,NP)
      MX=MAX0(N,NP)
      NN=NP*(NP+1)/2
      SIGMA=SIGMAI
      SIGMB=SIGMA
      IASG=IABS(ISIGMA)
      INTCH=1
      NPRCHK=N.LE.MDX.AND.MDT.GE.MX.AND.NCOV.EQ.NN.AND.GAM.GT.0.
     1       .AND.GAM.LT.2..AND.MAXIT.GT.0.AND.(MAXIS.GT.0.OR.
     1        IASG.NE.1).AND.LDIAG.GT.0.AND.SIGMA.GT.0..AND.TOL.GT.0.
     1       .AND.TAU.GE.0..AND.(ITYPE.EQ.1.OR.ITYPE.EQ.2.OR.
     1        ITYPE.EQ.3).AND.(ISIGMA.GE.-2.AND.ISIGMA.LE.2)
     1       .AND.(ICNV.EQ.1.OR.ICNV.EQ.2.OR.ICNV.EQ.3)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RYWALG',1)
      ITYP=ITYPE
      IF (ITYP.EQ.1) GOTO 15
      N0=N
      E=2.0
      IF (ITYP.EQ.2) E=0.5
      DO 10 I=1,N
        IF (WGT(I).LE.0.) THEN
          SW(I)=-1.
          N0=N0-1
        ELSE
          SW(I)=WGT(I)**E
        ENDIF
   10 CONTINUE
      IF (N0.EQ.0) ITYP=1
   15 IF (IASG.EQ.0) CONST=0.
      IF (IASG.EQ.1) CONST=BETA*FLOAT(N-NP)
      IF (IASG.EQ.2) CONST=BET0*FLOAT(N-NP)
C
C   STEP 1. SET NIT := 1
C   -------
      NIT=1
C
C   STEP 2. COMPUTE RESIDUALS R=Y-X*THETA
C   -------
  200 CALL RESIDU(X,Y,THETA,N,NP,MDX,RS)
C
C   STEP 3. COMPUTE A NEW VALUE SIGMB FOR SIGMA.
C   -------
      IF (ISIGMA.LT.0.AND.NIT.EQ.1) GOTO 300
      IF (ISIGMA.EQ.0) GOTO 300
      SIGMA=SIGMB
      CALL RYSIGM(RS,WGT,EXCHI,SIGMA,N,NP,TOL,ITYP,ISIGMA,MAXIS,
     1            NIS,SIGMB,SW,SC)
      IF (SIGMB.LE.TL) CALL MESSGE(460,'RYWALG',0)
      IF (SIGMB.LE.TL) RETURN
C
C  ITERATION MONITORING
C
      IF (NITMON.LE.0) GOTO 300
      IF (MOD(NIT,NITMON).NE.0) GOTO 300
      CALL QRSS(RS,WGT,SW,EXRHO,N,ITYP,SIGMB,CONST,QS)
      CALL MONITR(NIT,NP,GAM,QS/FLOAT(N),SIGMB,THETA,DELTA)
C
C   STEP 4. COMPUTE WEIGHTS AND APPLY THEM TO X; STORE RESULT IN SX
C   -------
  300 DO 430 I=1,N
        SC(I)=PSP0
        IF (RS(I).EQ.0.) GOTO 410
        T=RS(I)/SIGMB
        IF (ITYP.EQ.1) GOTO 400
        SC(I)=0.
        IF (WGT(I).LE.0.) GOTO 410
        IF (ITYP.EQ.2) GOTO 400
        T=T/WGT(I)
  400   SC(I)=EXPSI(T)/T
  410   PI=SQRT(SC(I))
        IF (ITYP.EQ.2) PI=PI*SW(I)
        RS(I)=PI*RS(I)
        DO 420 J=1,NP
          SX(I,J)=PI*X(I,J)
  420   CONTINUE
  430 CONTINUE
C   STEP 5. SOLVE FOR DELTA
C   -------
C
C   TRIANGULARIZATION OF SX
C
      CALL RIMTRFZ(SX,N,NP,MDX,INTCH,TAU,K,SF,SG,SH,IP)
      IF (K.EQ.0) CALL MESSGE(461,'RYWALG',0)
      IF (K.EQ.0) RETURN 
C
C   HOUSEHOLDER TRANSFORMATIONS OF THE RIGHT HAND SIDE
C
      KK=MDX*(K-1)+K
      IF (K.NE.NP) CALL SWAPZ(SX,SF,K,MDXP1,1,KK,K)
      DO 500 JJ=1,LDIAG
        J=JJ
        CALL H12Z(2,J,J+1,N,SX(1,J),1,SH(J),RS,1,N,1,N)
  500 CONTINUE
      IF (K.NE.NP) CALL SWAPZ(SX,SF,K,MDXP1,1,KK,K)
C
C   SOLVE FOR DELTA
C
      CALL SOLV(SX,RS,NP,K,MDX,N)
      IF (K.EQ.NP) GOTO 530
      KP1=K+1
      DO 510 J=KP1,NP
        RS(J)=0.0
  510 CONTINUE
      DO 520 J=1,K
        I=J
        CALL H12Z(2,I,KP1,NP,SX(I,1),MDX,SG(I),RS,1,N,1,NP)
  520 CONTINUE
  530 DO 540 J=1,NP
        DELTA(J)=GAM*RS(J)
  540 CONTINUE
      CALL PERM(DELTA,IP,LDIAG,NP)
C
C   STEP 6. COMPUTE NEW SOLUTION
C   -------
      DO 600 J=1,NP
        THETA(J)=THETA(J)+DELTA(J)
  600 CONTINUE
C
C   STEP 7. STOP ITERATIONS IF DESIRED PRECISION IS REACHED
C   -------
      IF (NIT.EQ.MAXIT) GOTO 800
      IF (ISIGMA.LT.0.AND.NIT.EQ.1) GOTO 700
      IF(ICTHET(NP,NCOV,DELTA,SIGMA,COV,TOL,ICNV).EQ.1
     +   .AND.ICSIGM(SIGMA,SIGMB,TOL).EQ.1) GOTO 800
  700 NIT=NIT+1
      GOTO 200
  800 SIGMAF=SIGMB
      CALL RESIDU(X,Y,THETA,N,NP,MDX,RS)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RYSALGZ(X,Y,THETA,WGT,COV,SIGMAI,N,NP,MDX,MDT,
     1                  NCOV,TOL,TAU,ITYPE,ISIGMA,ICNV,MAXIT,
     1                  MAXIS,NITMON,NIT,KODE,SIGMAF,QR0,RS,
     1                  DELTA,SC,SJ,SE,SF,SG,SH,IP,SW,SX)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR: A. RANDRIAMIHARISOA
C.......................................................................
C
C   PURPOSE
C   -------
C   S-ALGORITHM FOR ROBUST AND BOUNDED INFLUENCE LINEAR REGRESSION
C
      DIMENSION X(MDX,NP),Y(N),THETA(MDT),WGT(N),COV(NCOV),RS(N),
     1          DELTA(NP),SJ(N),SE(NP),SF(NP),SG(NP),SH(NP),SW(N),
     1          SC(NCOV),SX(MDX,NP)
      INTEGER IP(NP)
      LOGICAL NPRCHK
      EXTERNAL CHI,RHO,ICTHET,ICSIGM
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      COMMON/CONST/CONST
      COMMON/BETA/BETA,BET0
      DATA TL/1.E-8/
C     SAVE /CONST/
C
C   PARAMETER CHECK AND INITIALIZATION
C
      LDIAG=MIN0(N,NP)
      MX=MAX0(N,NP)
      NN=NP*(NP+1)/2
      SIGMA=SIGMAI
      SIGMB=SIGMA
      IASG=IABS(ISIGMA)
      IF (IPSI.NE.1.AND.IPSI.NE.-1) CALL MESSGE(300,'RYSALG',0)
      IPSI=ISIGN(1,IPSI)
      IF (IASG.EQ.1.AND.C.NE.D) CALL MESSGE(350,'RYSALG',0)
      IF (IASG.EQ.1) D=C
      INTCH=1
      KODE=0
      NPRCHK=N.LE.MDX.AND.MDT.GE.MX.AND.NCOV.EQ.NN.AND.
     1       MAXIT.GT.0.AND.(MAXIS.GT.0.OR.IASG.NE.1).AND.
     1       LDIAG.GT.0..AND.SIGMA.GT.0..AND.TOL.GT.0..AND.
     1       TAU.GE.0..AND.(ISIGMA.GE.-2.AND.ISIGMA.LE.2)
     1       .AND.(ITYPE.EQ.1.OR.ITYPE.EQ.2.OR.ITYPE.EQ.3)
     1       .AND.(ICNV.EQ.1.OR.ICNV.EQ.2.OR.ICNV.EQ.3)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RYSALG',1)
      ITYP=ITYPE
      IF (ITYP.EQ.1) GOTO 15
      N0=N
      E=2.0
      IF (ITYP.EQ.2) E=0.5
      DO 10 I=1,N
        IF (WGT(I).LE.0.) THEN
          SW(I)=-1.
          N0=N0-1
        ELSE
          SW(I)=WGT(I)**E
        ENDIF
   10 CONTINUE
      IF (N0.EQ.0) ITYP=1
   15 IF (IASG.EQ.0) CONST=0.
      IF (IASG.EQ.1) CONST=BETA*FLOAT(N-NP)
      IF (IASG.EQ.2) CONST=BET0*FLOAT(N-NP)
      GAM=1.0
C
C   STEP 1. SET NIT := 1
C   -------
      NIT=1
C
C   STEP 2. COMPUTE R := Y -X*THETA
C   -------
      CALL RESIDU(X,Y,THETA,N,NP,MDX,RS)
C
C   STEP 3. COMPUTE A NEW VALUE SIGMB FOR SIGMA.
C   -------
  200 IF (ISIGMA.LT.0.AND.NIT.EQ.1) GOTO 250
      IF (ISIGMA.EQ.0) GOTO 250
      SIGMA=SIGMB
      CALL RYSIGM(RS,WGT,CHI,SIGMA,N,NP,TOL,ITYP,ISIGMA,MAXIS,
     1            NIS,SIGMB,SW,SJ)
      IF (SIGMB.GT.TL) GOTO 250
      CALL MESSGE(460,'RYSALG',0)
      RETURN
  250 CALL QRSS(RS,WGT,SW,RHO,N,ITYP,SIGMB,CONST,QR0)
C
C  ITERATION MONITORING
C
      IF (NITMON.LE.0) GOTO 300
      IF (MOD(NIT,NITMON).NE.0) GOTO 300
      CALL MONITR(NIT,NP,GAM,QR0/FLOAT(N),SIGMB,THETA,DELTA)
C
C   STEP 4. FIND THE PARTITIONS J-, J0, J+
C   -------
  300 CALL S4ALG(X,WGT,SW,RS,SIGMB,C,ITYP,N,NP,MDX,SX,SJ,K0)
C
C   STEP 5. COMPUTE THE INCREMENT P-VECTOR DELTA.
C   -------
      CALL S5ALG(X,Y,WGT,RS,THETA,SJ,N,NP,MDX,NCOV,INTCH,TAU,
     +           SIGMB,C,ITYP,K0,DELTA,SX,SC,SE,SF,SG,SH,IP,KR)
C
C   STEP 6. STOP ITERATIONS IF DESIRED PRECISION IS REACHED
C   -------
      IF (ISIGMA.LT.0.AND.NIT.EQ.1) GOTO 600
      KODE=1
      IF(ICTHET(NP,NCOV,DELTA,SIGMA,COV,TOL,ICNV).EQ.1
     +   .AND.ICSIGM(SIGMA,SIGMB,TOL).EQ.1) GOTO 950
C
C  STEP 7. TEST FOR THETA
C  -------
  600 CALL S7ALG(X,Y,WGT,SW,DELTA,SJ,N,NP,MDX,SIGMA,SIGMB,C,
     1          ITYP,KR,QR0,THETA,RS,SE,GAM,I)
      IF (I.NE.0) GOTO 900
C
C   STEP 8. TEST FOR SIGMB
C   -------
      IF (IASG.EQ.2) GOTO 900
      KODE=2
      IF (ICSIGM(SIGMA,SIGMB,TOL).EQ.1.
     1    .AND.(ISIGMA.GE.0.OR.NIT.GT.1)) GOTO 950
C
C   STEP 9. SOLVE FOR (THETA,SIGMB)
C   -------
      CALL S9ALG(X,Y,WGT,SW,SJ,SX,SC,SE,SF,N,NP,MDX,NCOV,C,
     1           ITYP,RS,THETA,SIGMB,I)
      KODE=3
      IF (I.LT.0) THEN
        CALL MESSGE(470,'RYSALG',0)
        RETURN
      ENDIF
      IF (I.EQ.0) GOTO 950
C
C  STEP 10.
C  --------
  900 KODE=4
      IF (NIT.EQ.MAXIT) GOTO 950
      NIT=NIT+1
      GOTO 200
  950 SIGMAF=SIGMB
      QR0=QR0/FLOAT(N)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RYNALG(X,Y,THETA,WGT,COV,EXPSI,EXPSP,EXCHI,EXRHO,
     1                  SIGMAI,N,NP,MDX,MDT,NCOV,GAM,TOL,TAU,
     2                  ITYPE,IOPT,ISIGMA,ICNV,MAXIT,MAXIS,
     3                  NITMON,NIT,SIGMAF,QS1,
     4                  RS,DELTA,GRAD,HESSNV,SD,SW,
     5                  SF,SG,SH,IP,SX)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS: A. MARAZZI / A.RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  MODIFIED NEWTON ALGORITHMS FOR ROBUST AND BOUNDED INFLUENCE
C  LINEAR REGRESSION
C
      DIMENSION X(MDX,NP),Y(N),THETA(MDT),
     1          WGT(N),COV(NCOV),RS(N),
     2          DELTA(NP),GRAD(NP),SD(N),HESSNV(NCOV),
     3          SF(NP),SG(NP),SH(NP),SW(N),SX(MDX,NP)
      INTEGER IP(NP)
      LOGICAL NPRCHK,FIRST
      EXTERNAL EXPSP,EXCHI,EXPSI,EXRHO,ICTHET,ICSIGM
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      COMMON/CONST/CONST
      COMMON/BETA/BETA,BET0
      DATA TL/1.E-8/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      LDIAG=MIN0(N,NP)
      MX=MAX0(N,NP)
      NN=NP*(NP+1)/2
      XN=FLOAT(N)
      SIGMA=SIGMAI
      SIGMB=SIGMA
      IASG=IABS(ISIGMA)
      PSP0=EXPSP(0.)
      INTCH=1
      NPRCHK=N.LE.MDX.AND.MDT.GE.MX.AND.NCOV.EQ.NN.AND.
     1       MAXIT.GT.0.AND.(MAXIS.GT.0.OR.IASG.NE.1).AND.LDIAG.GT.0
     2       .AND.SIGMAI.GT.0..AND.GAM.GT.0..AND.GAM.LT.2..AND.TOL.GT.0.
     3       .AND.(ITYPE.EQ.1.OR.ITYPE.EQ.2.OR.ITYPE.EQ.3).AND.TAU.GE.0.
     4       .AND.(ISIGMA.GE.-2.AND.ISIGMA.LE.2).AND.(ICNV.EQ.1.OR.
     5       ICNV.EQ.2.OR.ICNV.EQ.3).AND.(IOPT.EQ.1.OR.IOPT.EQ.2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RYNALG',1)
C
      UPPER=6.
      TIL=1.E-6
      ITYP=ITYPE
      IF (ITYP.EQ.1) GOTO 15
      N0=N
      E=2.0
      IF (ITYP.EQ.2) E=0.5
      DO 10 I=1,N
        IF (WGT(I).LE.0.) THEN
          SW(I)=-1.
          N0=N0-1
        ELSE
          SW(I)=WGT(I)**E
        ENDIF
   10 CONTINUE
      IF (N0.EQ.0) ITYP=1
   15 IF (IASG.EQ.0) CONST=0.
      IF (IASG.EQ.1) CONST=BETA*FLOAT(N-NP)
      IF (IASG.EQ.2) CONST=BET0*FLOAT(N-NP)
C
C  STEP 1. SET NIT:=1
C  -------
      NIT=1
C
C  STEP 2. COMPUTE RESIDUALS
C  -------
  200 CALL RESIDU(X,Y,THETA,N,NP,MDX,RS)
C
C  STEP 3. COMPUTE A NEW VALUE SIGMB FOR SIGMA
C  -------
      IF (ISIGMA.LT.0.AND.NIT.EQ.1) GOTO 300
      IF (ISIGMA.EQ.0) GOTO 300
      SIGMA=SIGMB
      CALL RYSIGM(RS,WGT,EXCHI,SIGMA,N,NP,TOL,ITYP,ISIGMA,MAXIS,
     1            NIS,SIGMB,SW,SD)
      IF (SIGMB.GT.TL) GOTO 300
      CALL MESSGE(460,'RYNALG',0)
      RETURN
  300 CALL QRSS(RS,WGT,SW,EXRHO,N,ITYP,SIGMB,CONST,QS0)
C
C  ITERATION MONITORING
C
      IF (NITMON.LE.0.OR.NIT.EQ.1) GOTO 400
      IF (MOD(NIT,NITMON).NE.0) GOTO 400
      CALL MONITR(NIT,NP,GAM,QS0/XN,SIGMB,THETA,DELTA)
C
C  STEP 4. COMPUTE THE (UNSCALED) NEGATIVE GRADIENT
C  -------
  400 DO 410 I=1,N
      SD(I)=RS(I)
  410 CONTINUE
      CALL HUB(SD,WGT,WGT,SIGMB,N,ITYP,EXPSI)
      CALL GRADNT(X,SD,N,NP,MDX,GRAD)
C
C  STEP 5. COMPUTE WEIGHTS AND APPLY THEM TO X; STORE RESULT IN SX
C  -------
      DO 550 I=1,N
      T=RS(I)/SIGMB
      IF (ITYP.EQ.1) GOTO 510
      SDI=0.
      IF (WGT(I).LE.0.) GOTO 520
      IF (ITYP.EQ.3) T=T/WGT(I)
  510 SDI=EXPSP(T)
  520 SQD=SQRT(SDI)
      IF (ITYP.EQ.2) SQD=SQD*SW(I)
      DO 530 J=1,NP
      SX(I,J)=X(I,J)*SQD
  530 CONTINUE
  550 CONTINUE
      FIRST=.TRUE.
C
C  STEP 6. COMPUTE GENERALIZED INVERSE OF UNSCALED HESSIAN MATRIX
C  ------
  600 CALL RIMTRFZ(SX,N,NP,MDX,INTCH,TAU,K,SF,SG,SH,IP)
         IF (K.NE.NP) CALL MESSGE(111,'RYNALG',0)
         IF (K.EQ.0) CALL MESSGE(112,'RYNALG',0)
         IF (K.EQ.0) RETURN 
      CALL KIASCVZ(SX,K,NP,MDX,NN,1.,1.,HESSNV)
      CALL KFASCVZ(SX,HESSNV,K,NP,MDX,NN,1.,DELTA,SG,IP)
C
C  STEP 7. COMPUTE THE INCREMENT VECTOR
C  -------
      CALL MSFZ(HESSNV,GRAD,DELTA,NP,NN,1,NP,NP)
      DO 710 J=1,NP
      DELTA(J)=GAM*DELTA(J)
      IF (FIRST) THEN
        SD(J)=THETA(J)
        THETA(J)=THETA(J)+DELTA(J)
      ELSE
        SF(J)=THETA(J)
        THETA(J)=SD(J)+DELTA(J)
      ENDIF
  710 CONTINUE
C
C  STEP 8. IF NECESSARY DETERMINE ANOTHER STEP LENGTH.
C  ------
      CALL RESIDU(X,Y,THETA,N,NP,MDX,RS)
      IF (.NOT.FIRST) QSF=QS1
      CALL QRSS(RS,WGT,SW,EXRHO,N,ITYP,SIGMB,CONST,QS1)
      IF (QS1.LE.QS0) GOTO 900
      IF (.NOT.FIRST) GOTO 880
      FIRST=.FALSE.
      IF (IOPT.EQ.1) GOTO 800
      IF (IOPT.EQ.2) GOTO 830
C
C  H-ALGORITHM OPTION
C
  800 IF (ITYP.EQ.2) THEN
        DO 815 J=1,NP
        DO 810 I=1,N
        SX(I,J)=X(I,J)*SW(I)
  810   CONTINUE
  815   CONTINUE
      ELSE
        DO 825 J=1,NP
        DO 820 I=1,N
        SX(I,J)=X(I,J)
  820   CONTINUE
  825   CONTINUE
      ENDIF
      GOTO 600
C
C  W-ALGORITHM OPTION
C
  830 DO 850 I=1,N
      SDI=PSP0
      IF (RS(I).EQ.0.) GOTO 840
      T=RS(I)/SIGMB
      IF (ITYP.EQ.1) GOTO 835
      SDI=0.
      IF (WGT(I).LE.0.) GOTO 840
      IF (ITYP.EQ.2) GOTO 835
      T=T/WGT(I)
  835 SDI=EXPSI(T)/T
  840 PI=SQRT(SDI)
      IF (ITYP.EQ.2) PI=PI*SW(I)
      DO 845 J=1,NP
        SX(I,J)=PI*X(I,J)
  845 CONTINUE
  850 CONTINUE
      GOTO 600
  880 IF (QSF.LT.QS1) THEN
        DO 890 J=1,NP
        THETA(J)=SF(J)
  890   CONTINUE
      ENDIF
C
C  STEP 9. STOP ITERATIONS IF DESIRED PRECISION HAS BEEN REACHED
C  -------
  900 IF (ISIGMA.LT.0.AND.NIT.EQ.1) GOTO 950
      IF (NIT.EQ.MAXIT) GOTO 990
      IF (ICTHET(NP,NCOV,DELTA,SIGMA,COV,TOL,ICNV).EQ.1
     +   .AND.ICSIGM(SIGMA,SIGMB,TOL).EQ.1) GOTO 990
  950 NIT=NIT+1
      GOTO 200
C
C  FORM RESULTS
C
  990 SIGMAF=SIGMB
      QS1=QS1/XN
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RYSIGM(RS,WGT,EXCHI,SIGMAI,N,NP,TOL,ITYPE,ISIGMA,
     1                  MAXIS,NIT,SIGMAF,SW,SC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS: A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION RS(N),WGT(N),SW(N),SC(N)
      LOGICAL NPRCHK
      EXTERNAL EXCHI,ICSIGM
      COMMON/BETA/BETA,BET0
      COMMON/CONST/CONST
      DATA TL/1.E-8/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      N0=N
      SIGMB=SIGMAI
      IASG=IABS(ISIGMA)
      NPRCHK=NP.GT.0.AND.N.GT.0.AND.(ITYPE.GE.1.AND.ITYPE.LE.3)
     1       .AND.((IASG.EQ.1.AND.MAXIS.GT.0.AND.TOL.GT.0..AND.
     2       SIGMAI.GT.0.).OR.IASG.EQ.2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'RYSIGM',1)
      ITYP=ITYPE
      IF (ITYP.EQ.1) GOTO 20
      IF (SIGMAI.EQ.SIGMAF) GOTO 20
      E=2.0
      IF (ITYP.EQ.2) E=0.5
      DO 10 I=1,N
      IF (WGT(I).LE.0.) THEN
        SW(I)=-1.
        N0=N0-1
      ELSE
        SW(I)=WGT(I)**E
      ENDIF
   10 CONTINUE
      IF (N0.EQ.0) ITYP=1
   20 CONTINUE
      IF (IASG.EQ.2) GOTO 500
      CONST=BETA*FLOAT(N-NP)
C
C  STEP 1. SET NIT := 1
C  -------
      NIT=1
C
C  STEP 2. COMPUTE A NEW VALUE SIGMB FOR SIGMA
C  -------
  100 SIGMA=SIGMB
      CALL NEWSIG(RS,WGT,SW,SIGMA,SIGMB,N,ITYP,EXCHI)
      IF (SIGMB.GT.TL) GOTO 300
      CALL MESSGE(460,'RYSIGM',0)
      RETURN
C
C  STEP 3. STOP ITERATIONS IF DESIRED PRECISION IS REACHED
C  -------
  300 IF (ICSIGM(SIGMA,SIGMB,TOL).EQ.1.OR.NIT.EQ.MAXIS) GOTO 400
      NIT=NIT+1
      GOTO 100
  400 SIGMAF=SIGMB
      RETURN
C
C COMPUTE SIGMA USING MEDIAN
C --------------------------
  500 IF (ITYPE.NE.1) GOTO 650
C
C  HUBER-TYPE
C
      DO 600 I=1,N
        SC(I)=ABS(RS(I))
  600 CONTINUE
      N0=N
      GOTO 900
C
C  MALLOWS
C
  650 IF (ITYPE.NE.2) GOTO 750
      N0=0
      DO 700 I=1,N
        IF (SW(I).LE.0.) GOTO 700
        N0=N0+1
        SC(N0)=ABS(RS(I))*SW(I)
  700 CONTINUE
      GOTO 900
C
C  SCHWEPPE-TYPE
C
  750 N0=0
      DO 800 I=1,N
        IF (WGT(I).EQ.0.) GOTO 800
        N0=N0+1
        SC(N0)=ABS(RS(I))
  800 CONTINUE
  900 MED=(N0/2)+1
      CALL FSTORDZ(SC,N0,MED,SIGMAF)
      SIGMAF=SIGMAF/BET0
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RYBIFRZ(X,Y,N,NP,NTHET,NCOV,ITYPE,ICOLL,ISIGMA,
     *           CH,CK,BM,TOL,TAU,MAXITT,MAXITW,SIGMAF,THETA,
     *           RS,WGT,COV)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio MARAZZI
C
C   AUTHOR :  A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION X(N,NTHET),Y(N),THETA(NTHET),RS(N),WGT(N),COV(NCOV)
C
C   Main subroutine for the computation of M-estimates
C
      COMMON/BETA/BETA,BET0,/CONST/CONST,/WWWPR/IWWW
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,BT,CW
      COMMON/PSIPR/IPSI,CPSI,H1,H2,H3,XK,DCHI
C
      MP=NTHET
      SQRP=SQRT(FLOAT(NTHET))
      NN=MP*(MP+1)/2  
      IF (N.LE.1 .OR. NP.LE.0 .OR. NCOV.NE.NN .OR. (MP.NE.NP+1 .AND.
     * MP.NE.NP) .OR.(ICOLL.NE.0.AND.ICOLL.NE.1.AND.ITYPE.NE.1) .OR. 
     * ITYPE.LE.0 .OR. ITYPE.GT.3 .OR. (CH.LE.0. .AND. ITYPE.NE.3) .OR. 
     * (CK.LE.SQRP .AND. ITYPE.EQ.3 ).OR.(BM.LE.SQRP .AND. ITYPE.EQ.2)
     * .OR. (ISIGMA.LT.-2 .AND. ISIGMA.GT.2) .OR. TOL.LE.0. .OR.
     * TAU.LT.0. .OR. MAXITT.LE.0 .OR. MAXITW.LE.0) 
     * CALL MESSGE(500,'RYBIFR',1)
C
C STEP 0: INITIALIZATIONS
C ------
C 
C   ADD COLUMN IF (ITCP=1)
      ITCP=NTHET-NP
      NP=MP
      DO 10 J=1,NP
      THETA(J)=0. 
   10 CONTINUE
      IF (ITCP.EQ.0) GOTO 50
      DO 30 J=1,NP-1
      CALL LMDDZ(X(1,J),RS,N,1,XME,XMD,XSD)
      THETA(J)=XME
      DO 20 I=1,N
      X(I,J)=X(I,J)-XME
   20 CONTINUE
   30 CONTINUE
      DO 40 I=1,N
      X(I,NP)=1.
   40 CONTINUE
   50 IFLAG=0
      ISG=IABS(ISIGMA)
      IPSI=1
      IF (ITYPE.EQ.1) THEN
        CPSI=CH
      ELSEIF (ITYPE.EQ.2) THEN
        IWWW=3
        IUCV=1
        A2=0.
        B2=BM*BM
        CPSI=CH
      ELSE
        IWWW=1
        IUCV=3
        CKW=CK
        CPSI=CKW
      ENDIF
      IF (ISG.EQ.1) DCHI=CPSI
      PSP0=PSP(0.)
      NITMON=0
      XFUD=2.0
      ICNV=2
C
C COMPUTE WEIGHTS
C 
      IF (ITYPE.NE.1) CALL RYBIF2(X,N,NP,NCOV,ITYPE,ITYPW,
     *   ICOLL,CK,BM,TOL,TAU,MAXITW,RS,WGT,IFLAG)
C
C ITERATIONS
C
      CALL RYBIF3(X,Y,N,NP,ITCP,NCOV,ITYPE,ISIGMA,TOL,
     *     TAU,MAXITT,SIGMAF,THETA,RS,WGT,COV,IFLAG)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RYBIF2(X,N,NP,NCOV,ITYPE,ITYPW,ICOLL,CK,BM,
     *           TOL,TAU,MAXITW,RS,WGT,IFLAG)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio MARAZZI
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION AA(NCOV),XD(N,NP),AD(NP,NP),SU(N),SB(NCOV),
     +       SA(NCOV),SB0(NCOV),SFD(NP),SGD(NP),SHD(NP),SWD(N,NP),
     +       SZD(NP),UCV,UPCV,WWW
C
      REAL X(N,NP),RS(N),WGT(N)
C
      INTEGER SP(NP)
      EXTERNAL UCV,UPCV,WWW
      COMMON/WWWPR/IWWW
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,BT,CW
C
C STEP 1: COMPUTE WEIGHTS
C ------  
      IF (ICOLL.EQ.1) GOTO 120
      ITYPW=1
      NITMON=0
      XFUD=2.0
      ICNV=2
      CALL WIMEDVZ(X,N,NP,NCOV,N,ITYPW,2,N,AA,RS)
      CALL WYNALG(X,AA,UCV,UPCV,N,NP,NCOV,N,MAXITW,NITMON,ICNV,
     +            TOL,XFUD,NIT,WGT,SA,SB,SU,XD(1,1),SB0,SFD)
      IF (NIT.LT.MAXITW) GOTO 150
      IFLAG=1
      DO 110 IJ=1,NCOV
      AA(IJ)=SA(IJ)
  110 CONTINUE
      CALL WYFALG(X,AA,WGT,UCV,N,NP,0,NCOV,N,TAU,MAXITW,NITMON,
     +            ICNV,ITYPW,0,TOL,NIT,WGT,SU,SA,SB,SFD,SGD)
      IF (NIT.LT.MAXITW) GOTO 150
      IFLAG=2
      GOTO 150
  120 DO 130 J=1,NP
      DO 125 I=1,N
       XD(I,J)=DBLE(X(I,J))
  125 CONTINUE
  130 CONTINUE
      IWGT=1
      APAR=CK*CK/FLOAT(NP)
      IF (ITYPE.EQ.2) THEN
        APAR=BM*BM/FLOAT(NP)
        IWGT=2
      ENDIF
      MAXTWY=2*MAXITW
      CALL WYFCOL(XD,UCV,N,NP,NCOV,N,NP,N,IWGT,APAR,TAU,TOL,
     +            MAXTWY,NITMON,ICNV,K,NIT,WGT,AD,SU,SB,SB0,
     +            SFD,SGD,SHD,SP,SWD,SZD)
      IF (NIT.LT.MAXTWY) GOTO 150
      IFLAG=3
  150 DO 170 I=1,N
       S=WGT(I)
       WGT(I)=SNGL(WWW(S))
  170 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RYBIF3(X,Y,N,NP,ITCP,NCOV,ITYPE,ISIGMA,TOL,
     *           TAU,MAXITT,SIGMAF,THETA,RS,WGT,COV,IFLAG)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio MARAZZI
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      REAL X(N,NP),Y(N),THETA(NP),RS(N),COV(NCOV),XTHETA(N),WGT(N),
     +     SC(N),SE(NP),SF(NP),SG(NP),SH(NP),SX(N,NP),SZ(N),
     +     A(NCOV),S1INV(NCOV),S2(NCOV),AINV(NCOV)
      INTEGER SP(NP)
      EXTERNAL PSY,PSP,CHI,RHO,UCV,UPCV,WWW
      COMMON/BETA/BETA,BET0,/CONST/CONST,/WWWPR/IWWW
      COMMON/PSIPR/IPSI,CPSI,H1,H2,H3,XK,DCHI
C
      ISG=IABS(ISIGMA)
      GAM=1.5
      MAXIS=1
      NITMON=0
      IA=1
      IAINV=0
      INTCH=1
C
C STEP 2: COMPUTE INITIAL VALUES (THETA,SIGMA,COV)
C -------
      IF (ITYPE.EQ.1) THEN
        DO 220 I=1,N
         DO 210 J=1,NP
          SX(I,J)=X(I,J)
  210    CONTINUE
         SC(I)=Y(I) 
  220   CONTINUE
        CALL RIMTRFZ(X,N,NP,N,INTCH,TAU,K,SF,SG,SH,SP)
        CALL LIEPSHZ(CPSI,EPSI2,EPSIP)
        FH=EPSI2/EPSIP**2
        FB=0.
        CALL KIASCVZ(X,K,NP,N,NCOV,FH,FB,COV)
      ELSE
        F1=1./FLOAT(N)
        CALL KIEDCHZ(WGT,N,CPSI,ITYPE,SC,SZ)
        CALL KTASKWZ(X,SC,SZ,N,NP,N,N,NCOV,TAU,IA,F1,0.,IAINV,
     +  A,S1INV,S2,AINV,COV,SX)
        DO 240 I=1,N
          WI=WGT(I)
          DO 230 J=1,NP
           SX(I,J)=WI*X(I,J)
  230     CONTINUE
          SC(I)=WI*Y(I) 
  240   CONTINUE
      ENDIF   
      CALL RIBET0Z(WGT,N,ITYPE,0,TOL,BT0)
      CALL RILARSZ(SX,SC,N,NP,N,N,TOL,NIT,KR,KODE,
     1            SIGMA,XTHETA,RS,SZ,S1INV,S2,AINV)
      IF (ISG.EQ.1) CALL RIBETHZ(WGT,N,CPSI,ITYPE,BTA)
C
C STEP 3: ITERATIONS
C -------
      ICNV=1
      IF (ITYPE.EQ.1) THEN
        IX=0
        IY=1
        IC=0
        CALL RYHALG(X,Y,XTHETA,WGT,COV,PSY,CHI,RHO,SIGMA,N,NP,N,N,
     1     NCOV,K,TOL,GAM,TAU,ITYPE,IX,IY,IC,ISIGMA,ICNV,MAXITT,MAXIS,
     2     NITMON,NIT,SIGMAF,RS,SC,AINV,SZ,SE,SF,SG,SH,SP)
        IF (NIT.EQ.MAXITT) IFLAG=4
      ELSE
        CALL RYWALG(X,Y,XTHETA,WGT,COV,PSP0,PSY,CHI,RHO,SIGMA,N,NP,N,
     1  N,NCOV,TOL,GAM,TAU,ITYPE,ISIGMA,ICNV,MAXITT,MAXIS,NITMON,NIT,
     2  SIGMAF,RS,SE,SC,SF,SG,SH,SP,SZ,SX)
        IF (NIT.EQ.MAXITT) IFLAG=10*IFLAG+5
      ENDIF
C
C ADJUST THETA(NP) IF ITCP=1
C
      ADJ=0.
      IF (ITCP.EQ.1) THEN
        DO 310 J=1,NP
          ADJ=ADJ+THETA(J)*XTHETA(J)
  310   CONTINUE
      ENDIF
      DO 320 J=1,NP
        THETA(J)=XTHETA(J)
  320 CONTINUE
      THETA(NP)=THETA(NP)-ADJ
C
C STEP 4: COMPUTE COV. MATRIX OF PAR. ESTIMATES (AVERAGE)
C -------
      IF (ITYPE.EQ.1) THEN
        CALL KFFACV(RS,PSY,PSP,N,NP,SIGMAF,F1)
        CALL KFASCVZ(X,COV,K,NP,N,NCOV,F1,SE,SG,SP)
      ELSE
        F1=(SIGMAF**2)/FLOAT(N)
        CALL KFEDCB(WGT,RS,PSY,PSP,N,SIGMAF,ITYPE,SC,SZ)
        CALL KTASKWZ(X,SC,SZ,N,NP,N,N,NCOV,TAU,IA,F1,0.,IAINV,
     1              A,S1INV,S2,AINV,COV,SX)
      ENDIF
      IF (IFLAG.NE.0) CALL MESSGE(100+IFLAG,'RYBIFR',0)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
C                 R O B E T H  FORTRAN Source
C
C  File TSAUXI.F  Auxiliary subroutines of Chapter 6
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CMPT(XO,IV,N,NP,MDX,IV2,IP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C  CMPT PUTS THE COLUMNS OF XO CORRESPONDING TO IV(I)=IV2
C  IN THE RIGHT PART OF XO. FOR EACH COLUMN EXCHANGE
C  PERFORMED BY CMPT AN EXCHANGE OF THE CORRESPONDING
C  VALUES OF IP IS ALSO PERFORMED
C
      DIMENSION XO(MDX,NP)
      INTEGER IP(NP),IV(NP)
      I=0
      J=NP+1
   20 I=I+1
      IF (IV(I).NE.IV2) GOTO 20
   30 J=J-1
      IF (IV(J).EQ.IV2) GOTO 30
      IF (I.GE.J) GOTO 40
      CALL SWAPZ(XO(1,I),XO(1,J),N,1,1,MDX,MDX)
      L=IP(I)
      IP(I)=IP(J)
      IP(J)=L
      L=IV(I)
      IV(I)=IV(J)
      IV(J)=L
      GOTO 20
   40 RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION PSISG(DS,WGT,N,EXU,PSY)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PSISG(S)=PSI_B(S/BETA/T)*S*dG(S) FOR THE MALLOWS TAU-TEST
C
      DIMENSION WGT(N)
      DOUBLE PRECISION EXU,DS
      EXTERNAL EXU,PSY
      COMMON/BCPAR/BPAR,CPAR,BETA,XLCNST,IPMQ,T,SIGM
      DATA NCALL,FX1/0,0.0/
      IF (NCALL.EQ.1) FX1=SNGL(EXU(1.0))*PSY(1.0)
      PSISG=DBLE(FX1*WGT(1))
      S=SNGL(DS)
      SSCA=S/SIGM
      CALL XERPZ(IPMQ,XLCNST,SSCA,ANS)
      ANS=ANS/SIGM
      Z=S/(BETA*T)
      TMP=AMIN1(BPAR,ABS(Z))
      IF (Z.LT.0.) TMP=-TMP
      PSISG=DBLE(TMP)*DS*DBLE(ANS)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION PSI2G(DS,WGT,N,EXU,PSY)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PSI2G(S)=[PSI_B(S*BETA/T)]^2*dG(S) FOR THE MALLOWS TAU-TEST
C
      DIMENSION WGT(N)
      DOUBLE PRECISION EXU,DS
      EXTERNAL EXU,PSY
      COMMON/BCPAR/BPAR,CPAR,BETA,XLCNST,IPMQ,T,SIGM
      DATA NCALL,FX1/0,0.0/
      IF (NCALL.EQ.1) FX1=SNGL(EXU(1.0))*PSY(1.0)
      PSI2G=DBLE(FX1*WGT(1))
      S=SNGL(DS)
      CALL XERPZ(IPMQ,XLCNST,S,ANS)
      Z=(BETA/T)*S
      TMP=AMIN1(BPAR,ABS(Z))
      IF (Z.LT.0.) TMP=-TMP
      PSI2G=DBLE(TMP*TMP*ANS)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION PHIS2G(DS,WGT,N,EXU,PSY)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PHIS2G(S)=[2*PHI(C*T*BETA/S)-1]*S^2*dG(S) FOR THE SCHWEPPE TAU-TEST
C
      DIMENSION WGT(N)
      DOUBLE PRECISION EXU,DS,Z,PZ
      EXTERNAL EXU,PSY
      COMMON/BCPAR/BPAR,CPAR,BETA,XLCNST,IPMQ,T,SIGM
      DATA NCALL,FX1/0,0.0/
      IF (NCALL.EQ.1) FX1=SNGL(EXU(1.0))*PSY(1.0)
      PHIS2G=DBLE(FX1*WGT(1))
      S=SNGL(DS)
      SSCA=S/SIGM
      CALL XERPZ(IPMQ,XLCNST,SSCA,ANS)
      ANS=ANS/SIGM
      Z=DBLE(CPAR*T*BETA)/DS
      CALL GAUSSZD(1,Z,PZ)
      PHIS2G=(2.D0*PZ-1.D0)*DS*DS*DBLE(ANS)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION EPSI2G(DS,WGT,N,EXU,PSY)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  EPSI2G(S)=E_Phi[PSI_a(Z)^2]*S^2*dG(S) FOR THE SCHWEPPE TAU-TEST
C  WHERE a=(CPAR*T)/(BETA*S)
C
      DIMENSION WGT(N)
      DOUBLE PRECISION EXU,DS,A,PA,TMP
      EXTERNAL EXU,PSY
      COMMON/BCPAR/BPAR,CPAR,BETA,XLCNST,IPMQ,T,SIGM
      DATA NCALL,FX1/0,0.0/
      IF (NCALL.EQ.1) FX1=SNGL(EXU(1.0))*PSY(1.0)
      EPSI2G=DBLE(FX1*WGT(1))
      S=SNGL(DS)
      CALL XERPZ(IPMQ,XLCNST,S,ANS)
      A=DBLE(CPAR*T/BETA)/DS
      CALL GAUSSZD(1,A,PA)
      CALL XERFZ(2,SNGL(A),PX)
      TMP=A*A+(1.D0-A*A)*(2.D0*PA-1.D0)-2.D0*A*DBLE(PX)
      EPSI2G=TMP*DS*DS*DBLE(ANS)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE EIGVAL(MDC,NP,COVTAU,XLMBDA,IERR,IWRK,WORK)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      INTEGER IWRK(NP)
      REAL COVTAU(MDC,NP),XLMBDA(NP),WORK(NP)
      IF (MDC.LT.NP.OR.NP.LE.0) CALL MESSGE(500,'EIGVAL',1)
C
      CALL BALANX(MDC,NP,COVTAU,IS1,IS2,WORK)
      CALL ELMHEX(MDC,NP,IS1,IS2,COVTAU,IWRK)
      CALL HQRX(MDC,NP,IS1,IS2,COVTAU,XLMBDA,WORK,IERR)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE BALANX(M,N,A,LW,IG,SCALE)
C.......................................................................
C
C   AUTHOR : EISPACK, ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      INTEGER I,J,K,L,MM,N,JJ,M,IG,LW,IEXC
      REAL A(M,N),SCALE(N)
      REAL C,F,G,R,S,B2,RADIX
      REAL ABS
      LOGICAL NOCONV
      IF (M.LT.N.OR.N.LE.0) CALL MESSGE(500,'BALANX',1)
C
C  RADIX IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C  THE BASE OF THE MACHINE FLOATING POINT REPRESENTATION
C
      CALL MACHZ(1,RADIX)
      B2=RADIX*RADIX
      K=1
      L=N
      GOTO 100
C
C  IN-LINE PROCEDURE FOR ROW AND COLUMN EXCHANGE
C
   20 SCALE(MM)=J
      IF (J.EQ.MM) GOTO 50
      DO 30 I=1,L
      F=A(I,J)
      A(I,J)=A(I,MM)
      A(I,MM)=F
   30 CONTINUE
      DO 40 I=K,N
      F=A(J,I)
      A(J,I)=A(MM,I)
      A(MM,I)=F
   40 CONTINUE
   50 IF (IEXC.EQ.2) GOTO 130
C
C  SEARCH FOR ROWS ISOLATING AN EIGENVALUE
C  AND PUSH THEM DOWN
C
      IF (L.EQ.1) GOTO 280
      L=L-1
C
C  FOR J=L STEP-1 UNTIL 1 DO ...
C
  100 DO 120 JJ=1,L
      J=L+1-JJ
      DO 110 I=1,L
      IF (I.EQ.J) GOTO 110
      IF (A(J,I).NE.0.0) GOTO 120
  110 CONTINUE
      MM=L
      IEXC=1
      GOTO 20
  120 CONTINUE
      GOTO 140
C
C  SEARCH FOR COLUMNS ISOLATING AN EIGENVALUE
C  AND PUSH THEM LEFT
C
  130 K=K+1
  140 DO 170 J=K,L
      DO 150 I=K,L
      IF (I.EQ.J) GOTO 150
      IF (A(I,J).NE.0.0) GOTO 170
  150 CONTINUE
      MM=K
      IEXC=2
      GOTO 20
  170 CONTINUE
C
C  NOW BALANCE THE SUBMATRIX IN ROWS K TO L
C
      DO 180 I=K,L
      SCALE(I)=1.0
  180 CONTINUE
C
C  ITERATIVE LOOP FOR NORM REDUCTION
C
  190 NOCONV=.FALSE.
      DO 270 I=K,L
      C=0.0
      R=0.0
      DO 200 J=K,L
      IF (J.EQ.I) GOTO 200
      C=C+ABS(A(J,I))
      R=R+ABS(A(I,J))
  200 CONTINUE
      G=R/RADIX
      F=1.0
      S=C+R
  210 IF (C.GE.G) GOTO 220
      F=F*RADIX
      C=C*B2
      GOTO 210
  220 G=R*RADIX
  230 IF (C.LT.G) GOTO 240
      F=F/RADIX
      C=C/B2
      GOTO 230
C
C  NOW BALANCE
C
  240 IF ((C+R)/F.GE.0.95*S) GOTO 270
      G=1.0/F
      SCALE(I)=SCALE(I)*F
      NOCONV=.TRUE.
      DO 250 J=K,N
      A(I,J)=A(I,J)*G
  250 CONTINUE
      DO 260 J=1,L
      A(J,I)=A(J,I)*F
  260 CONTINUE
  270 CONTINUE
      IF (NOCONV) GOTO 190
  280 LW=K
      IG=L
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE ELMHEX(M,N,LW,IG,A,INT)
C.......................................................................
C
C   AUTHOR : EISPACK, ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      INTEGER I,J,M,N,LA,MM,IG,KP1,LW,MM1,MP1
      REAL A(M,N)
      REAL X,Y
      REAL ABS
      INTEGER INT(IG)
      IF (M.LT.N.OR.N.LE.0.OR.N.LT.IG.OR.IG.LT.LW.OR.LW.LT.1)
     1   CALL MESSGE(500,'ELMHEX',1)
C
      LA=IG-1
      KP1=LW+1
      IF (LA.LT.KP1) GOTO 200
      DO 180 MM=KP1,LA
      MM1=MM-1
      X=0.0
      I=MM
      DO 100 J=MM,IG
      IF (ABS(A(J,MM1)).LE.ABS(X)) GOTO 100
      X=A(J,MM1)
      I=J
  100 CONTINUE
      INT(MM)=I
      IF (I.EQ.MM) GOTO 130
C
C  INTERCHANGE ROWS AND COLUMNS OF A
C
      DO 110 J=MM1,N
      Y=A(I,J)
      A(I,J)=A(MM,J)
      A(MM,J)=Y
  110 CONTINUE
      DO 120 J=1,IG
      Y=A(J,I)
      A(J,I)=A(J,MM)
      A(J,MM)=Y
  120 CONTINUE
C
C  END INTERCHANGE
C
  130 IF (X.EQ.0.0) GOTO 180
      MP1=MM+1
      DO 160 I=MP1,IG
      Y=A(I,MM1)
      IF (Y.EQ.0.0) GOTO 160
      Y=Y/X
      A(I,MM1)=Y
      DO 140 J=MM,N
      A(I,J)=A(I,J)-Y*A(MM,J)
  140 CONTINUE
      DO 150 J=1,IG
      A(J,MM)=A(J,MM)+Y*A(J,I)
  150 CONTINUE
  160 CONTINUE
  180 CONTINUE
  200 RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE HQRX(M,N,LW,IG,H,WR,WI,IERR)
C.......................................................................
C
C   AUTHOR : EISPACK, ADAPTED FOR ROBETH BY A. MARAZZI
C.......................................................................
C
      INTEGER I,J,K,L,M,N,EN,LL,MM,NA,IM,IG,ITS,LW,MP2,ENM2,IERR
      REAL H(M,N),WR(N),WI(N)
      REAL P,Q,R,S,T,W,X,Y,ZZ,PREC
      REAL SQRT,ABS,SIGN
      INTEGER MIN0
      LOGICAL NOTLAS
      IF (M.LT.N.OR.N.LE.0.OR.N.LT.IG.OR.IG.LT.LW.OR.LW.LT.1)
     1   CALL MESSGE(500,'HQRX  ',1)
C
C  PREC IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C  THE SMALLEST POSITIVE REAL NUMBER SUCH THAT (1.0+PREC).GT.1.0
C
      CALL MACHZ(2,PREC)
      IERR=0
      P=0.  ! added by A.R. 28.02.2020
      Q=0.  ! added by A.R. 28.02.2020
      R=0.  ! added by A.R. 28.02.2020
      IM=0  ! added by A.R. 28.02.2020
C
C  STORE ROOTS ISOLATED BY BALANX
C
      DO 50 I=1,N
      IF (I.GE.LW.AND.I.LE.IG) GOTO 50
      WR(I)=H(I,I)
      WI(I)=0.0
   50 CONTINUE
      EN=IG
      T=0.0
C
C  SEARCH FOR NEXT EIGENVALUES
C
   60 IF (EN.LT.LW) GOTO 1001
      ITS=0
      NA=EN-1
      ENM2=NA-1
C
C  LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
C  FOR L=EN STEP -1 UNTIL LOW DO ...
C
   70 DO 80 LL=LW,EN
      L=EN+LW-LL
      IF (L.EQ.LW) GOTO 100
      IF (ABS(H(L,L-1)).LE.PREC*(ABS(H(L-1,L-1))
     1    +ABS(H(L,L)))) GOTO 100
   80 CONTINUE
C
C  FORM SHIFT
C
  100 X=H(EN,EN)
      IF (L.EQ.EN) GOTO 270
      Y=H(NA,NA)
      W=H(EN,NA)*H(NA,EN)
      IF (L.EQ.NA) GOTO 280
      IF (ITS.EQ.30) GOTO 1000
      IF (ITS.NE.10.AND.ITS.NE.20) GOTO 130
C
C  FORM EXCEPTIONAL SHIFT
C
      T=T+X
      DO 120 I=LW,EN
      H(I,I)=H(I,I)-X
  120 CONTINUE
      S=ABS(H(EN,NA))+ABS(H(NA,ENM2))
      X=0.75*S
      Y=X
      W=-0.4375*S*S
  130 ITS=ITS+1
C
C  LOOK FOR TWO CONSECUTIVE SMALL SUB-DIAGONAL ELEMENTS.
C  FOR IM=EN-2 STEP -1 UNTIL L DO ...
C
      DO 140 MM=L,ENM2
      IM=ENM2+L-MM
      ZZ=H(IM,IM)
      R=X-ZZ
      S=Y-ZZ
      P=(R*S-W)/H(IM+1,IM)+H(IM,IM+1)
      Q=H(IM+1,IM+1)-ZZ-R-S
      R=H(IM+2,IM+1)
      S=ABS(P)+ABS(Q)+ABS(R)
      P=P/S
      Q=Q/S
      R=R/S
      IF (IM.EQ.L) GOTO 150
      IF (ABS(H(IM,IM-1))*(ABS(Q)+ABS(R)).LE.PREC*ABS(P)
     1   *(ABS(H(IM-1,IM-1))+ABS(ZZ)+ABS(H(IM+1,IM+1)))) GOTO 150
  140 CONTINUE
  150 MP2=IM+2
      DO 160 I=MP2,EN
      H(I,I-2)=0.0
      IF (I.EQ.MP2) GOTO 160
      H(I,I-3)=0.0
  160 CONTINUE
C
C  DOUBLE QR STEP INVOLVING ROWS L TO EN AND
C  COLUMNS IM TO EN
C
      DO 260 K=IM,NA
      NOTLAS=K.NE.NA
      IF (K.EQ.IM) GOTO 170
      P=H(K,K-1)
      Q=H(K+1,K-1)
      R=0.0
      IF (NOTLAS) R=H(K+2,K-1)
      X=ABS(P)+ABS(Q)+ABS(R)
      IF (X.EQ.0.0) GOTO 260
      P=P/X
      Q=Q/X
      R=R/X
  170 S=SIGN(SQRT(P*P+Q*Q+R*R),P)
      IF (K.EQ.IM) GOTO 180
      H(K,K-1)=-S*X
      GOTO 190
  180 IF (L.NE.IM) H(K,K-1)=-H(K,K-1)
  190 P=P+S
      X=P/S
      Y=Q/S
      ZZ=R/S
      Q=Q/P
      R=R/P
C
C  ROW MODIFICATION
C
      DO 210 J=K,EN
      P=H(K,J)+Q*H(K+1,J)
      IF (.NOT.NOTLAS) GOTO 200
      P=P+R*H(K+2,J)
      H(K+2,J)=H(K+2,J)-P*ZZ
  200 H(K+1,J)=H(K+1,J)-P*Y
      H(K,J)=H(K,J)-P*X
  210 CONTINUE
      J=MIN0(EN,K+3)
C
C  COLUMN MODIFICATION
C
      DO 230 I=L,J
      P=X*H(I,K)+Y*H(I,K+1)
      IF (.NOT.NOTLAS) GOTO 220
      P=P+ZZ*H(I,K+2)
      H(I,K+2)=H(I,K+2)-P*R
  220 H(I,K+1)=H(I,K+1)-P*Q
      H(I,K)=H(I,K)-P
  230 CONTINUE
  260 CONTINUE
      GOTO 70
C
C  ONE ROOT FOUND
C
  270 WR(EN)=X+T
      WI(EN)=0.0
      EN=NA
      GOTO 60
C
C  TWO ROOTS FOUND
C
  280 P=(Y-X)/2.0
      Q=P*P+W
      ZZ=SQRT(ABS(Q))
      X=X+T
      IF (Q.LT.0.0) GOTO 320
C
C  REAL PAIR
C
      ZZ=P+SIGN(ZZ,P)
      WR(NA)=X+ZZ
      WR(EN)=WR(NA)
      IF (ZZ.NE.0.0) WR(EN)=X-W/ZZ
      WI(NA)=0.0
      WI(EN)=0.0
      GOTO 330
C
C  COMPLEX PAIR
C
  320 WR(NA)=X+P
      WR(EN)=X+P
      WI(NA)=ZZ
      WI(EN)=-ZZ
  330 EN=ENM2
      GOTO 60
C
C  SET ERROR -- NO CONVERGENCE TO AN
C  EIGENVALUE AFTER 30 ITERATIONS
C
 1000 IERR=EN
 1001 RETURN
      END
C-----------------------------------------------------------------------
C
C                     R O B E T H  FORTRAN Source
C
C  File TSMAIN.F  Main subroutines of Chapter 6
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TISRTCZ(X,IV,N,NVAR,MDX,NP,NQ,IP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DIMENSION X(MDX,NVAR)
      INTEGER IV(NVAR),IP(NVAR)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NPRCHK=NVAR.GT.0.AND.MDX.GE.N.AND.N.GT.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'TISRTC',1)
      NP=0
      NQ=0
      DO 10 I=1,NVAR
      NPRCHK=NPRCHK.AND.(IV(I).EQ.0.OR.IV(I).EQ.1.OR.IV(I).EQ.2)
      IF (IV(I).EQ.2) NQ=NQ+1
      IF (IV(I).EQ.1) NP=NP+1
      IP(I)=I
   10 CONTINUE
      IF (.NOT.NPRCHK) CALL MESSGE(500,'TISRTC',1)
      NP=NQ+NP
C
C  PUT COLUMNS CORRESPONDING TO IV(J)=0 IN POSITIONS NP+1...NVAR
C
      IF (NP.LT.NVAR) CALL CMPT(X,IV,N,NVAR,MDX,0,IP)
C
C  PUT COLUMNS CORRESPONDING TO IV(I)=2 IN POSITIONS 1...NQ
C
      CALL CMPT(X,IV,N,NP,MDX,1,IP)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TTASKTZ(COV,AINV,NP,NQ,MDC,NCOV,FACT,COVTAU,SC1,SC2)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DIMENSION COV(NCOV),AINV(NCOV),COVTAU(MDC,NP),
     1          SC1(NCOV),SC2(NCOV)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NN=NP*(NP+1)/2
      NPRCHK=NQ.GT.0.AND.NP.GT.NQ.AND.NN.EQ.NCOV
     1       .AND.MDC.GE.(NP-NQ)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'TTASKT',1)
C
      NPMNQ=NP-NQ
      IJ=0
      NQP1=NQ+1
      DO 30 J=NQP1,NP
      DO 20 I=NQP1,J
      IJ=IJ+1
      K=J*(J-1)/2+I
      SC1(IJ)=COV(K)
      SC2(IJ)=AINV(K)
   20 CONTINUE
   30 CONTINUE
      CALL MTT2Z(SC2,SC2,NPMNQ,IJ)
      CALL MSSZ(SC1,SC2,COVTAU,NPMNQ,IJ,MDC)
      IF (FACT.LE.0) RETURN
      DO 50 I=1,NPMNQ
      DO 40 J=1,NPMNQ
      COVTAU(I,J)=COVTAU(I,J)*FACT
   40 CONTINUE
   50 CONTINUE 
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TFTAUT(RS1,RS2,WGT,EXRHO,N,NP,NQ,SIGMA,ITYPE,
     1                  SUM1,SUM2,FTAU)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C  COMPUTATION OF THE TAU-TEST STATISTIC. RS1 IS THE RESIDUAL VECTOR
C  OF THE LARGE MODEL. RS2 IS THE RESIDUAL VECTOR OF THE SMALL MODEL.
C  (FOR ITYPE=0, THE CLASSICAL F-STATISTIC IS COMPUTED)
C
      DIMENSION RS1(N),RS2(N),WGT(N)
      LOGICAL NPRCHK
      EXTERNAL EXRHO
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NPRCHK=NP.GT.NQ.AND.NQ.GT.0.AND.N.GT.NP
     1       .AND.(SIGMA.GT.0..OR.ITYPE.EQ.0)
     2       .AND.(ITYPE.GE.0.AND.ITYPE.LE.3)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'TFTAUT',1)
      SUM1=0.
      SUM2=0.
      IF (ITYPE.EQ.1) GOTO 10
      IF (ITYPE.EQ.2) GOTO 30
      IF (ITYPE.EQ.3) GOTO 50
C
C  CLASSICAL CASE
C
      CALL NRM2Z(RS1,N,1,N,SUM1)
      CALL NRM2Z(RS2,N,1,N,SUM2)
      SUM1=SUM1*SUM1
      SUM2=SUM2*SUM2
      FTAU=FLOAT(N-NP)/FLOAT(NP-NQ)*(SUM2-SUM1)/SUM1
      RETURN
C
C  HUBER CASE
C
   10 DO 20 I=1,N
      S1=RS1(I)/SIGMA
      S2=RS2(I)/SIGMA
      SUM1=SUM1+EXRHO(S1)
      SUM2=SUM2+EXRHO(S2)
   20 CONTINUE
      GOTO 90
C
C  MALLOWS CASE
C
   30 DO 40 I=1,N
      IF (WGT(I).LE.0.) GOTO 40
      S1=RS1(I)/SIGMA
      S2=RS2(I)/SIGMA
      SUM1=SUM1+WGT(I)*EXRHO(S1)
      SUM2=SUM2+WGT(I)*EXRHO(S2)
   40 CONTINUE
      GOTO 90
C
C  SCHWEPPE CASE
C
   50 DO 60 I=1,N
      SW=SIGMA*WGT(I)
      IF (SW.EQ.0..OR.WGT(I).LE.0.) GOTO 60
      S1=RS1(I)/SW
      S2=RS2(I)/SW
      W2=WGT(I)*WGT(I)
      SUM1=SUM1+EXRHO(S1)*W2
      SUM2=SUM2+EXRHO(S2)*W2
   60 CONTINUE
   90 SUM1=2.*SUM1
      SUM2=2.*SUM2
      FTAU=(SUM2-SUM1)/FLOAT(NP-NQ)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TTEIGNZ(COVTAU,NP,NQ,MDC,XLMBDA,IV,SV)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
      DIMENSION COVTAU(MDC,NP),XLMBDA(NP),SV(NP)
      INTEGER IV(NP)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK
C
      NPRCHK=NP.GT.NQ.AND.NQ.GE.0.AND.MDC.GE.(NP-NQ)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'TTEIGN',1)
C
C  COMPUTE EIGENVALUES
C
      CALL EIGVAL(MDC,NP-NQ,COVTAU,XLMBDA,IERR,IV,SV)      
      IF (IERR.NE.0) CALL MESSGE(101,'TTEIGN',0)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TFRN2TZ(COV,THETA,N,NP,NQ,NCOV,TAU,RN2T,SA)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  LET A THE MATRIX WITH ELEMENTS COV(I,J), I=NQ+1,NP AND J=NQ+1,NP
C  AND LET Y THE VECTOR WITH ELEMENTS THETA(NQ+1), ..., THETA(NP)
C  TFRN2TZ COMPUTES THE QUADRATIC FORM RN2T=Y**T*(A**-1)*Y*N.
C
      LOGICAL NPRCHK
      DIMENSION COV(NCOV),THETA(NP),SA(NCOV)
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NP*(NP+1)/2
      NPRCHK=NP.GT.0.AND.NQ.GT.0.AND.NP.GT.NQ.AND.NCOV.GE.NN
      IF (.NOT.NPRCHK) CALL MESSGE(500,'TFRN2T',1)
      RN2T=0.
      II=NP-NQ
      NN=II*(II+1)/2
      K=0
      L=NQ*(NQ+1)/2
C
C  COMPUTE A, A**-1 AND Y**T*(A**-1)*Y
C
      DO 200 I=1,II
        L=L+NQ+I-1
        DO 100 J=1,I
          K=K+1
          SA(K)=COV(L+J)
  100   CONTINUE
  200 CONTINUE
      CALL MCHLZ(SA,II,NN,INFO)
      IF (INFO.EQ.0) GOTO 300
      CALL MESSGE(400+INFO,'TFRN2T',0)
      RETURN
  300 CALL MINVZ(SA,II,NN,TAU,ISING)
      IF (ISING.EQ.0) GOTO 400
      CALL MESSGE(450,'TFRN2T',0)
      RETURN
  400 CALL MTT1Z(SA,SA,II,NN)
      CALL XSYZ(THETA(NQ+1),THETA(NQ+1),SA,II,NN,RN2T)
      RN2T=FLOAT(N)*RN2T
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE TAUAREZ(ITYPE,MU,MAXIT,CPSI,BB,SIGMAX,UPPER,TIL,TOL,
     1                  NIT,BETA,ARE)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DIMENSION WGT(1)
      DOUBLE PRECISION UCV,UPERD,PSISG,PSI2G,PHIS2G,EPSI2G,
     * ANSB,ANSM,ANSS,WORK(80),TILD,TILR,ERRSTD
      LOGICAL NPRCHK
      EXTERNAL PSISG,PSI2G,PHIS2G,EPSI2G,PSY,UCV
      COMMON/BCPAR/BPAR,CPAR,BET0,XLCNST,JPMQ,T,SIGMX
      COMMON/INTEG/UUPER,TTIL,IWORK(40),WORK,IER1,ERRST1
      COMMON/INTPAR/ITYP,I,NEVAL,LIMIT,KEY
      DATA TL/1.E-6/
C
C  PARAMETER CHECK AND INITIALISATION
C
       NPRCHK=(ITYPE.EQ.1.OR.ITYPE.EQ.2.OR.ITYPE.EQ.3).AND.MU.GT.0
     * .AND.MAXIT.GT.0.AND.CPSI.GT.0..AND.(BB.GT.0..OR.ITYPE.NE.2)
     * .AND.SIGMAX.GT.0..AND.UPPER.GT.0..AND.TIL.GT.0..AND.TOL.GT.0.
       IF (.NOT.NPRCHK) CALL MESSGE(500,'TAUARE',1)
       JPMQ=MU
       T=SQRT(FLOAT(MU))
       SIGMX=SIGMAX
       ITYP=ITYPE
       LIMIT=20
       KEY=1
       WGT(1)=1.
       BET0=SIGMAX
       IF (ITYPE.EQ.1) BETA=1.
       BPAR=BB
       CPAR=CPSI
       I=1
       UPERD=DBLE(UPPER)
       UUPER=UPPER
       TLI=TIL
       IF (TIL.GT.TOL) TLI=TOL
       TLO=AMIN1(TIL,TOL)
       TILD=DBLE(TLI)
       TILR=0.D0
       TTIL=TLI
       XLCNST=-1.
       NIT=1
       IF (ITYPE.EQ.3) GOTO 310
       CALL LIEPSHZ(CPSI,EPSI2,EPSIP)
       DEN=EPSI2
       IF (ITYPE.EQ.1) GOTO 230
C
C  ITYPE.EQ.2 (MALLOWS TAU-TEST)
C
C  STEP 1M: SOLVE FOR BETA
C  -------
  210 CALL INTGRD(PSISG,WGT,1,UCV,PSY,0.D0,UPERD,TILD,TILR,KEY,
     *     LIMIT,ANSB,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'TAUARE',0)
      ANS=SNGL(ANSB)
      BETA=ANS/T
C
C CHECK CONVERGENCE
C
      IF (ABS(BETA-BET0).LT.TLO.OR.NIT.EQ.MAXIT) GOTO 220
      IF (MOD(NIT,2).EQ.1) THEN
        BET0=BETA
      ELSE
        BET0=0.5*(BET0+BETA)
      ENDIF
      NIT=NIT+1
      GOTO 210
  220 IF (NIT.EQ.MAXIT) CALL MESSGE (200,'TAUARE',0)
C
C  STEP2M: COMPUTE AUXILIARY QUANTITIES
C  ------
      BETA=1./BETA
      BET0=BETA
      CALL INTGRD(PSI2G,WGT,1,UCV,PSY,0.D0,UPERD,TILD,TILR,KEY,
     *     LIMIT,ANSM,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'TAUARE',0)
C
C  STEP 3M: COMPUTE ARE_M
C  -------
      DEN=(BETA**2)*EPSI2*SNGL(ANSM)
  230 IF (DEN.LE.1E-6) THEN
        CALL MESSGE(350,'TAUARE',0)
        DEN=1.E-6
      ENDIF
      ARE=(EPSIP**2)/DEN
      RETURN
C
C  ITYPE.EQ.3 (SCHWEPPE TAU-TEST)
C
C  STEP 1S: SOLVE FOR BETA
C  -------
  310 CALL INTGRD(PHIS2G,WGT,1,UCV,PSY,0.D0,UPERD,TILD,TILR,KEY,
     *     LIMIT,ANSB,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'TAUARE',0)
      BETA=SQRT(SNGL(ANSB))/T
C
C CHECK CONVERGENCE
C 
      IF (ABS(BETA-BET0).LT.TLO.OR.NIT.EQ.MAXIT) GOTO 320
      IF (MOD(NIT,2).EQ.1) THEN
        BET0=BETA
      ELSE
        BET0=0.5*(BET0+BETA)
      ENDIF
      NIT=NIT+1
      GOTO 310
  320 IF (NIT.EQ.MAXIT) CALL MESSGE (200,'TAUARE',0)
C
C  STEP2S: COMPUTE AUXILIARY QUANTITIES
C  ------
      BETA=1./BETA
      BET0=BETA
      CALL INTGRD(EPSI2G,WGT,1,UCV,PSY,0.D0,UPERD,TILD,TILR,KEY,
     *     LIMIT,ANSS,ERRSTD,NEVAL,IER,WORK,IWORK)
        IF (IER.GT.0) CALL MESSGE (300+IER,'TAUARE',0)
C
C  STEP 3S: COMPUTE ARE_S
C  -------
      DEN=(BETA**4)*SNGL(ANSS)
      IF (DEN.LE.TL) THEN
        CALL MESSGE(350,'TAUARE',0)
        DEN=TL
      ENDIF
      ARE=FLOAT(MU)/DEN
      RETURN
      END
C-----------------------------------------------------------------------
C
C                 R O B E T H  Interface to  S - P L U S
C
C  File USREXT.F  Interface for vector and user-defined weight
C                 functions (FORTRAN CODE)
C                 Note: This file also contains a version of SRDPRT.F
C                 used by the Interface. 
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PSIA(N,SVALS,FVALS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi  
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE OF THE FUNCTION PSI FOR SVALS(I), I=1,...,N
C  This subroutine is mainly used by the S-interface
C
      DIMENSION SVALS(N),FVALS(N)
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      IPS=IABS(IPSI)
      IF (IPS.EQ.0) GOTO 100
      IF (IPS.EQ.1) GOTO 200
      IF (IPS.EQ.2) GOTO 300
      IF (IPS.EQ.3) GOTO 400
      IF (IPS.EQ.4) GOTO 500
      IF (IPS.EQ.10) GOTO 700
C
C  PSI(S)=USER PSI FUNCTION
C
C      DO 50 I=1,N
C      S=SVALS(I)
C   50 FVALS(I)=UPSI(S)
C      RETURN
C
C  PSI(S)=S
C
  100 DO 150 I=1,N
      FVALS(I)=SVALS(I)
  150 CONTINUE
      RETURN
C
C  PSI(S,C)=MAX(-C,MIN(C,S))
C
  200 DO 250 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      TMP=AMIN1(C,ABST)
      IF (S.LT.0.) TMP=-TMP
      FVALS(I)=TMP
  250 CONTINUE
      RETURN
C
C  PSI(S,H1,H2,H3)=-PSI(-S,H1,H2,H3)
C                 =S FOR 0 .LE. S .LE. H1
C                 =H1 FOR H1 .LE. S .LE. H2
C                 =H1*(H3-S)/(H3-H2) FOR H2 .LE. S .LE. H3
C                 =0 FOR S .GT. H3
C
  300 DO 355 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      TMP=0
      IF (ABST.GE.H3) GOTO 350
      IF (ABST.LE.H2) TMP=AMIN1(H1,ABST)
      IF (ABST.GT.H2) TMP=H1*(H3-ABST)/(H3-H2)
      IF (S.LT.0.) TMP=-TMP
  350 FVALS(I)=TMP
  355 CONTINUE
      RETURN
C
C  PSI(S)=S*[MAX(1-S**2,0)]**2
C
  400 DO 455 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      TMP=0.
      IF (ABST.GE.1.) GOTO 450
      TMP=S*(1.-S*S)*(1.-S*S)
  450 FVALS(I)=TMP
  455 CONTINUE
      RETURN
C
C  PSI(S)=(6/K)*(S/K)*[MAX(1-(S/K)**2,0)]**2
C
  500 DO 555 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      TMP=0.
      IF (ABST.GE.XK) GOTO 550
      SK=S/XK
      TMP=(6.*SK/XK)*(1.-SK*SK)*(1.-SK*SK)
  550 FVALS(I)=TMP
  555 CONTINUE
      RETURN
C
C  PSI(S,C)=MAX(-C,MIN(C,S))
C

  700 DO 750 I=1,N
      S=SVALS(I)
      TMP=AMIN1(H2,S)
      IF (TMP.LE.H1) TMP=H1
      FVALS(I)=TMP
  750 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE RHOA(N,SVALS,FVALS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUES OF THE INTEGRAL FROM 0 TO SVALS(I) OF PSI, I=1,..,N
C  This subroutine is mainly used by the S-interface
C
      DIMENSION SVALS(N),FVALS(N)
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      IPS=IABS(IPSI)
      IF (IPS.EQ.0) GOTO 100
      IF (IPS.EQ.1) GOTO 200
      IF (IPS.EQ.2) GOTO 300
      IF (IPS.EQ.3) GOTO 400
      IF (IPS.EQ.4) GOTO 500
      IF (IPS.EQ.10) GOTO 700
C      DO 50 I=1,N
C      S=SVALS(I)
C      RHO=URHO(S)
C   50 FVALS(I)=TMP
C      RETURN
  100 DO 150 I=1,N
      S=SVALS(I)
      FVALS(I)=S*S/2.
  150 CONTINUE
      RETURN
  200 DO 250 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      TMP=S*S/2.
      IF (ABST.GT.C) TMP=C*(ABST-C/2.)
      FVALS(I)=TMP
  250 CONTINUE
      RETURN
  300 DO 350 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      IF (ABST.GT.H2) GOTO 325
      TMP=S*S/2.
      IF (ABST.GT.H1) TMP=H1*(ABST-H1/2.)
  325 TMP=0.5*H1*(H3+H2-H1)
      IF (ABST.LT.H3) TMP=TMP-.5*H1*(H3-ABST)**2/(H3-H2)
      FVALS(I)=TMP
  350 CONTINUE
      RETURN
  400 DO 455 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      TMP=1./6.
      IF (ABST.GE.1.) GOTO 450
      S2=S*S
      TMP=(S2*(S2-3)+3)*S2/6.
  450 FVALS(I)=TMP
  455 CONTINUE
      RETURN
  500 DO 555 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      TMP=1.
      IF (ABST.GE.XK) GOTO 550
      S2=(S/XK)**2
      TMP=(S2*(S2-3)+3)*S2
  550 FVALS(I)=TMP
  555 CONTINUE
      RETURN
  700 DO 750 I=1,N
      S=SVALS(I)
      TMP=S*S/2.
      IF (S.LT.H1) TMP=H1*(S-H1/2.)
      IF (S.GT.H2) TMP=H2*(S-H2/2.)
      FVALS(I)=TMP
  750 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PSPA(N,SVALS,FVALS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINTS S=SVALS(I), I=1,...,N
C  OF THE FIRST DERIVATIVE OF THE FUNCTION PSI.
C  This subroutine is mainly used by the S-interface
C
      DIMENSION SVALS(N),FVALS(N)
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      IPS=IABS(IPSI)
      IF (IPS.EQ.0) GOTO 100
      IF (IPS.EQ.1) GOTO 200
      IF (IPS.EQ.2) GOTO 300
      IF (IPS.EQ.3) GOTO 400
      IF (IPS.EQ.4) GOTO 500
      IF (IPS.EQ.10) GOTO 700
C      DO 50 I=1,N
C      S=SVALS(I)
C   50 FVALS(I)=UPSP(S)
C      RETURN
  100 DO 150 I=1,N
      FVALS(I)=1.
  150 CONTINUE
      RETURN
  200 DO 250 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      TMP=0.
      IF (ABST.LE.C) TMP=1.
      FVALS(I)=TMP
  250 CONTINUE
      RETURN
  300 DO 355 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      TMP=1.
      IF (ABST.LT.H1) GOTO 350
      TMP=0.
      IF ((ABST.LE.H2).OR.(ABST.GE.H3)) GOTO 350
      TMP=H1/(H2-H3)
  350 FVALS(I)=TMP
  355 CONTINUE
      RETURN
  400 DO 455 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      TMP=0.
      IF (ABST.GE.1.) GOTO 450
      S2=S*S
      TMP=(1.-S2)*(1.-5.*S2)
  450 FVALS(I)=TMP
  455 CONTINUE
      RETURN
  500 DO 555 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      TMP=0.
      IF (ABST.GE.XK) GOTO 550
      S2=(S/XK)**2
      TMP=(6./XK)*(1.-S2)*(1.-5.*S2)/XK
  550 FVALS(I)=TMP
  555 CONTINUE
      RETURN
  700 DO 750 I=1,N
      S=SVALS(I)
      TMP=0.
      IF (S.GE.H1.AND.S.LE.H2) TMP=1.
      FVALS(I)=TMP
  750 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE CHIA(N,SVALS,FVALS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  FOR S=SVALS(I), I=1,...,N, CHIA GIVES THE VALUE OF :
C  THE FUNCTION CHI(S)=S*S/2 IF IPSI=0,
C  THE HUBER'S CHI FUNCTION IF ABS(IPSI)<4, AND
C  THE FUNCTION CHI(S)=CHIK(S) FOR S-ESTIMATES IF IPS=4.
C  This subroutine is mainly used by the S-interface
C
      DIMENSION SVALS(N),FVALS(N)
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      IF (IPSI.EQ.0) GOTO 100
      IPS=IABS(IPSI)
      IF (IPS.LE.3) GOTO 200
      IF (IPS.EQ.4) GOTO 300
      IF (IPS.EQ.10) GOTO 400
C      DO 50 I=1,N
C      S=SVALS(I)
C   50 FVALS(I)=UCHI(S)
C      RETURN
  100 DO 150 I=1,N
      S=SVALS(I)
      FVALS(I)=S*S/2.
  150 CONTINUE
      RETURN
  200 DO 250 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      PS=AMIN1(D,ABST)
      FVALS(I)=PS*PS/2.
  250 CONTINUE
      RETURN
  300 DO 355 I=1,N
      S=SVALS(I)
      ABST=ABS(S)
      TMP=1.
      IF (ABST.GE.XK) GOTO 350
      S2=(S/XK)**2
      TMP=(S2*(S2-3)+3)*S2
  350 FVALS(I)=TMP
  355 CONTINUE
      RETURN
  400 DO 450 I=1,N
      S=SVALS(I)
      PS=AMIN1(H2,S)
      IF (PS.LT.H1) PS=H1
      FVALS(I)=PS*PS/2.
  450 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE UCVA(N,SVALS,FVALS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINTS S=SVALS(I), I=1,...,N
C  OF THE U-FUNCTION
C  This subroutine is mainly used by the S-interface
C
      REAL SVALS(N)
      DOUBLE PRECISION FVALS(N),Z2,Q,Q2,PD,PC,XEXPD,DSPI
      EXTERNAL XEXPD
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,DV,CW
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
      DATA GAM,DSPI/1.E-6,2.506628274631001/
      DO 10 I=1,N
      FVALS(I)=1.D0
   10 CONTINUE
      IF (IUCV.EQ.0) RETURN
      IF (IUCV.EQ.1) GOTO 100
      IF (IUCV.EQ.2) GOTO 200
      IF (IUCV.EQ.3) GOTO 300
      IF (IUCV.EQ.4) GOTO 400
      IF (IUCV.EQ.5) GOTO 500
      IF (IUCV.EQ.6) GOTO 600
      IF (IUCV.EQ.7) GOTO 700
C      DO 50 I=1,N
C      S=SVALS(I)
C   50 FVALS(I)=UUCV(S)
C      RETURN
  100 DO 150 I=1,N
      S=SVALS(I)
      IF (S*S.GE.A2.AND.S.GE.0.) GOTO 110
      IF (S.GT.GAM) GOTO 110
      CALL MESSGE(200,'UCVA  ',0)
      S=GAM
  110 Z2=DBLE(S)*S
      IF (Z2.GT.B2) FVALS(I)=B2/Z2
      IF (Z2.LT.A2) FVALS(I)=A2/Z2
  150 CONTINUE
      RETURN
  200 DO 250 I=1,N
      S=SVALS(I)
      IF (S.LE.0.) GOTO 250
      IF (S.LE.GAM) S=GAM
      Q=CHK/DBLE(S)
      CALL GAUSSZD(1,Q,PC)
      FVALS(I)=2.D0*PC-1.D0
  250 CONTINUE
      RETURN
  300 DO 350 I=1,N
      S=SVALS(I)
      IF (S.LE.0.) GOTO 350
      IF (S.LE.GAM) S=GAM
      Q=CKW/DBLE(S)
      Q2=Q*Q
      CALL GAUSSZD(1,Q,PC)
      PD=XEXPD(-Q2/2.D0)/DSPI
      FVALS(I)=Q2+(1.D0-Q2)*(2.D0*PC-1.D0)-2.D0*Q*PD
  350 CONTINUE
      RETURN
  400 DO 450 I=1,N
      S=SVALS(I)
      IF (S.LE.BB) GOTO 450
      IF (S.GT.GAM) GOTO 410
      CALL MESSGE(200,'UCVA  ',0)
      S=GAM
  410 FVALS(I)=BB/DBLE(S)
  450 CONTINUE
  500 CONTINUE
  600 DO 650 I=1,N
      S=SVALS(I)
      IF (S.LE.EM) GOTO 650
      FVALS(I)=0.D0
      IF (S.GE.EM+CR) GOTO 650
      ZED=1.0-((S-EM)/CR)**2
      FVALS(I)=DBLE(ZED)**2
  650 CONTINUE      
      RETURN
  700 DO 750 I=1,N
      S=SVALS(I)
      ZED=1.0/(S+ENU)
      FVALS(I)=DBLE(ZED)
  750 CONTINUE
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE UPCVA(N,SVALS,FVALS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINTS S=SVALS(I), I=1,...,N
C  OF THE FIRST DERIVATIVE OF THE U-FUNCTION
C  This subroutine is mainly used by the S-interface
C
      REAL SVALS(N)
      DOUBLE PRECISION FVALS(N),Z2,Q,Q2,PD,PC,XEXPD,DSPI
      EXTERNAL XEXPD
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,DV,CW
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
      DATA GAM,DSPI/1.E-6,2.506628274631001/
      DO 10 I=1,N
      FVALS(I)=0.D0
   10 CONTINUE
      IF (IUCV.EQ.0) RETURN
      IF (IUCV.EQ.1) GOTO 100
      IF (IUCV.EQ.2) GOTO 200
      IF (IUCV.EQ.3) GOTO 300
      IF (IUCV.EQ.4) GOTO 400
      IF (IUCV.EQ.5) GOTO 500
      IF (IUCV.EQ.6) GOTO 600
      IF (IUCV.EQ.7) GOTO 700
C      DO 50 I=1,N
C      S=SVALS(I)
C   50 FVALS(I)=UUPCV(S)
C      RETURN
  100 DO 150 I=1,N
      S=SVALS(I)
      IF (S*S.GE.A2.AND.S.GE.0.) GOTO 110
      IF (S.GT.GAM) GOTO 110
      CALL MESSGE(200,'UPCVA ',0)
      S=GAM
  110 Z2=DBLE(S*S)
      IF (Z2.GT.B2) FVALS(I)=-2.D0*B2/Z2/S
      IF (Z2.LT.A2) FVALS(I)=-2.D0*A2/Z2/S
  150 CONTINUE
      RETURN
  200 DO 250 I=1,N
      S=SVALS(I)
      IF (S.LE.0.) GOTO 250
      IF (S.LE.GAM) S=GAM
      Z2=DBLE(S)*S
      Q=CHK/DBLE(S)
      Q2=Q*Q
      PD=XEXPD(-Q2/2.D0)/DSPI
      FVALS(I)=2.D0*PD*(-CHK/Z2)
  250 CONTINUE
      RETURN
  300 DO 350 I=1,N
      S=SVALS(I)
      IF (S.LE.0.) GOTO 350
      IF (S.LE.GAM) S=GAM
      Q=CKW/DBLE(S)
      CALL GAUSSZD(1,Q,PC)
      FVALS(I)=-4.D0*(Q*Q)*(1.D0-PC)/DBLE(S)
  350 CONTINUE
      RETURN
  400 DO 450 I=1,N
      S=SVALS(I)
      IF (S.LT.BB) GOTO 450
      IF (S.GT.GAM) GOTO 410
      CALL MESSGE(200,'UPCVA ',0)
      Z=GAM
  410 FVALS(I)=-BB/(DBLE(S)*S)
  450 CONTINUE
  500 CONTINUE
  600 Z2=DBLE(CR**2)
      DO 650 I=1,N
      S=SVALS(I)
      IF (S.LE.EM.OR.S.GE.EM+CR) GOTO 650
      Q=DBLE(EM-S)
      FVALS(I)=-4.D0*(Q**2-Z2)*Q/(Z2**2)
  650 CONTINUE      
      RETURN
  700 DO 750 I=1,N
      S=SVALS(I)
      ZED=-1.0/(S+ENU)**2
      FVALS(I)=DBLE(ZED)
  750 CONTINUE
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE VCVA(N,SVALS,FVALS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINTS S=SVALS(I), I=1,...,N
C  OF THE V-FUNCTION
C  This subroutine is mainly used by the S-interface
C
      REAL SVALS(N)
      DOUBLE PRECISION FVALS(N),VD
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,DV,CW
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
C      IF (IUCV.GT.4) GOTO 100
      VD=1.D0
      IF (IUCV.EQ.1.OR.IUCV.EQ.4) VD=DBLE(DV)
      IF (IUCV.EQ.5) VD=DBLE(VK)
      IF (IUCV.EQ.7) VD=DBLE(V7)
      DO 50 I=1,N
       IF (IUCV.EQ.6) THEN
         VD=0.D0
         S=SVALS(I)
         IF (S.GE.EM+CR) GOTO 10
         IF (S.GE.0.AND.S.LE.EM) THEN
           VD=DBLE(S*S)/DBLE(FLOAT(NP))
         ELSEIF (S.GT.EM) THEN
           ZED=S*(1.0-((S-EM)/CR)**2)
           VD=DBLE(ZED)**2/DBLE(FLOAT(NP))
         ENDIF
       ENDIF
   10  FVALS(I)=VD
   50 CONTINUE
      RETURN
C  100 DO 150 I=1,N
C      S=SVALS(I)
C  150 FVALS(I)=UVCV(S)
C      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE VPCVA(N,SVALS,FVALS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINTS S=SVALS(I), I=1,...,N
C  OF THE FIRST DERIVATIVE OF THE V-FUNCTION
C  This subroutine is mainly used by the S-interface
C
      REAL SVALS(N)
      DOUBLE PRECISION FVALS(N),VD
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,DV,CW
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
C      IF (IUCV.GT.4) GOTO 100
      VD=0.D0
      DO 50 I=1,N
      IF (IUCV.EQ.6) THEN
       S=SVALS(I)
       IF (S.GE.EM+CR) GOTO 20
       IF (S.GE.0.AND.S.LE.EM) THEN
         VD=2.D0*DBLE(S)/DBLE(FLOAT(NP))
       ELSEIF (S.GT.EM) THEN
         CR2=CR*CR
         ZED=2.*S*(1.0+(EM-3.0*S)*((EM-S)**3)/(CR2**2)-
     +       2.0*(EM-S)*(EM-2.0*S)/CR2)
         VD=DBLE(ZED)/DBLE(FLOAT(NP))
       ENDIF
      ENDIF
   20 FVALS(I)=VD
   50 CONTINUE
C  100 DO 150 I=1,N
C      S=SVALS(I)
C  150 FVALS(I)=UVPCV(S)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE WCVA(N,SVALS,FVALS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINTS S=SVALS(I), I=1,...,N
C  OF THE W-FUNCTION
C  This subroutine is mainly used by the S-interface
C
      REAL SVALS(N)
      DOUBLE PRECISION FVALS(N)
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,DV,CW
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
      DATA GAM/1.E-6/
      DO 50 I=1,N
      FVALS(I)=1.D0
   50 CONTINUE
      IF (IUCV.EQ.7) GOTO 700
      IF (IUCV.GE.5) GOTO 500
      IF (IUCV.NE.1) RETURN
      DO 150 I=1,N
      S=SVALS(I)
      IF (S.LE.CW) GOTO 150
      IF (S.GT.GAM) GOTO 110
      CALL MESSGE(200,'WCVA  ',0)
      S=GAM
  110 FVALS(I)=CW/DBLE(S)
  150 CONTINUE
      RETURN
  500 DO 600 I=1,N
      S=SVALS(I)
      IF (S.LE.EM) GOTO 600
      FVALS(I)=0.D0
      IF (S.GE.EM+CR) GOTO 600
      ZED=1.0-((S-EM)/CR)**2
      FVALS(I)=DBLE(ZED)**2
  600 CONTINUE
C  200 DO 250 I=1,N
C      S=SVALS(I)
C  250 FVALS(I)=UWCV(S)
      RETURN
  700 DO 750 I=1,N
      S=SVALS(I)
      ZED=1.0/(S+ENU)
      FVALS(I)=DBLE(ZED)
  750 CONTINUE
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE WPCVA(N,SVALS,FVALS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINTS S=SVALS(I), I=1,...,N
C  OF THE FIRST DERIVATIVE OF THE W-FUNCTION
C  This subroutine is mainly used by the S-interface
C
      REAL SVALS(N)
      DOUBLE PRECISION FVALS(N),Q,Z2
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,DV,CW
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
      DATA GAM/1.E-6/
      DO 50 I=1,N
      FVALS(I)=0.D0
   50 CONTINUE
      IF (IUCV.EQ.7) GOTO 700
      IF (IUCV.GE.5) GOTO 500
      IF (IUCV.NE.1) RETURN
      DO 150 I=1,N
      S=SVALS(I)
      IF (S.LE.CW) GOTO 150
      IF (S.GT.GAM) GOTO 110
      CALL MESSGE(200,'WPCVA ',0)
      Z=GAM
  110 FVALS(I)=-DBLE(CW/(S*S))
  150 CONTINUE
      RETURN
  500 Z2=DBLE(CR**2)
      DO 600 I=1,N
      S=SVALS(I)
      IF (S.LE.EM.OR.S.GE.EM+CR) GOTO 600
      Q=DBLE(EM-S)
      FVALS(I)=-4.D0*(Q**2-Z2)*Q/(Z2**2)
  600 CONTINUE      
C  200 DO 250 I=1,N
C      S=SVALS(I)
C  250 FVALS(I)=UWPCV(S)
      RETURN
  700 DO 750 I=1,N
      S=SVALS(I)
      ZED=-1.0/(S+ENU)**2
      FVALS(I)=DBLE(ZED)
  750 CONTINUE
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE WWWA(N,SVALS,FVALS)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINTS S=SVALS(I), I=1,...,N
C  OF THE W_BAR-FUNCTION
C  This subroutine is mainly used by the S-interface
C
      REAL SVALS(N)
      DOUBLE PRECISION UCV,Z,GAM,FVALS(N)
      EXTERNAL UCV
      COMMON/WWWPR/IWWW
      DATA GAM/1.D-6/
      DO 50 I=1,N
      FVALS(I)=1.D0
   50 CONTINUE
      IF (IWWW.EQ.0) RETURN
      IF (IWWW.EQ.1) GOTO 100
      IF (IWWW.EQ.2) GOTO 200
      IF (IWWW.EQ.3) GOTO 300
  100 DO 150 I=1,N
      Z=DBLE(SVALS(I))
      IF (Z.GT.GAM) GOTO 110
      CALL MESSGE(200,'WWWA  ',0)
      Z=GAM
  110 FVALS(I)=1.D0/Z
  150 CONTINUE
      RETURN
  200 DO 250 I=1,N
      S=SVALS(I)
      FVALS(I)=UCV(S)
  250 CONTINUE
      RETURN
  300 DO 350 I=1,N
      S=SVALS(I)
      Z=UCV(S)
      FVALS(I)=DSQRT(Z)
  350 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PRT0(ITEXT,VAR)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER ITEXT*64 
      REAL TMP(1)
      L=LEN(ITEXT)
      TMP(1)=VAR
      CALL REALPR(ITEXT,L,TMP,1)
      RETURN
      END
C
      SUBROUTINE PRT1(ITEXT,DIM,N)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER ITEXT*64 
      REAL DIM(N)
      L=LEN(ITEXT)
      CALL REALPR(ITEXT,L,DIM,N)
      RETURN
      END
C
      SUBROUTINE PRT2(ITEXT,DIM,MDX,M,N)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER ITEXT*64 ,C*1
      REAL DIM(MDX,N)
      INTEGER III(1)
      L=LEN(ITEXT)
      III(1)=M
      CALL INTPR(ITEXT,L,III,1)
      J=ICHAR('0')
      DO 10 I=1,N
       IF (I.EQ.10) J=J-10
       IF (I.EQ.11) J=ICHAR('A')-11
       C=CHAR(J+I)
       CALL REALPR(C,1,DIM(1,I),M)
   10 CONTINUE
      RETURN
      END
C
      SUBROUTINE PRT3(ITEXT,COV,NCOV,N)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER ITEXT*64 
      REAL COV(NCOV)
      L=LEN(ITEXT) + 0*N
      CALL REALPR(ITEXT,L,COV,NCOV)
      RETURN
      END
C
      SUBROUTINE PRT1D(ITEXT,DIM,N)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER ITEXT*64 
      DOUBLE PRECISION DIM(N)
      L=LEN(ITEXT)
      CALL DBLEPR(ITEXT,L,DIM,N)
      RETURN
      END
C
      SUBROUTINE PRT2D(ITEXT,DIM,MDX,M,N)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER ITEXT*64 ,C*1
      DOUBLE PRECISION DIM(MDX,N)
      INTEGER III(19)
      L=LEN(ITEXT)
      III(1)=M
      CALL INTPR(ITEXT,L,III,1)
      J=ICHAR('0')
      DO 10 I=1,N
       IF (I.EQ.10) J=J-10
       IF (I.EQ.11) J=ICHAR('A')-11
       C=CHAR(J+I)
       CALL DBLEPR(C,1,DIM(1,I),M)
   10 CONTINUE
      RETURN
      END
C
      SUBROUTINE PRT3D(ITEXT,COV,NCOV,N)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER ITEXT*64 
      DOUBLE PRECISION COV(NCOV)
      L=LEN(ITEXT) + 0*N
      CALL DBLEPR(ITEXT,L,COV,NCOV)
      RETURN
      END
C
      SUBROUTINE PRTI(ITEXT,I)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER ITEXT*64 
      INTEGER III(1)
      L=LEN(ITEXT)
      III(1)=I
      CALL INTPR(ITEXT,L,III,1)
      RETURN
      END
C
      SUBROUTINE PRTJ(ITEXT,IDIM,N)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER ITEXT*64 
      INTEGER IDIM(N)
      L=LEN(ITEXT)
      CALL INTPR(ITEXT,L,IDIM,N)
      RETURN
      END
C
      SUBROUTINE MESSGE(NUMBER,ITEXT,ISTOP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER ITEXT*6, CC*36
      INTEGER III(1)
      IF (ISTOP.EQ.1) THEN
C
C Error exit from R
C
       CC='Input parameter error(s) in '//ITEXT
       CALL REXIT(CC)
      ELSE
       CC='Warning message in '//ITEXT
       III(1)=NUMBER
       L=LEN(CC)
       CALL INTPR(CC,L,III,1)
      ENDIF
      RETURN
      END
C
      SUBROUTINE MONITR(NIT,NP,GAM,Q,SIGMA,THETA,DELTA)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER CC*51
      REAL THETA(NP),DELTA(NP),TMP(2),SSS(1)
      INTEGER III(1)
      DATA NEXT,INIT/0,0/
      III(1)=NIT
      IF (NEXT.NE.NIT) NEXT=0
      IF (NEXT.EQ.0) INIT=NIT
      CC="* * * I T E R A T I O N   M O N I T O R I N G * * *"
      L=LEN(CC)
      IF (NEXT.EQ.0) CALL INTPR(CC,L,III,0)
      NEXT=NIT+INIT
      TMP(1)=Q
      TMP(2)=GAM
      SSS(1)=SIGMA
      CC="Nb of iterations"
      L=LEN(CC)
      CALL INTPR(CC,L,III,1)
      CALL REALPR('Qs, Gamma',9,TMP,2)
      CALL REALPR('Theta',5,THETA,NP)
      CALL REALPR('Sigma',5,SSS,1)
      CALL REALPR('Delta',5,DELTA,NP)
      RETURN
      END
C
      SUBROUTINE MONITW(NIT,NP,NCOV,A,TOLA)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER CC*51
      DOUBLE PRECISION A(NCOV)
      INTEGER III(1)
      REAL TMP(1)
      DATA NEXT,INIT/0,0/
      III(1)=NP
      IF (NEXT.NE.NIT) NEXT=0
      IF (NEXT.EQ.0) INIT=NIT
      CC="* * * I T E R A T I O N   M O N I T O R I N G * * *"
      L=LEN(CC)
      IF (NEXT.EQ.0) CALL INTPR(CC,L,III,0)
      NEXT=NIT+INIT
      III(1)=NIT
      CC="Nb of iterations"
      L=LEN(CC)
      CALL INTPR(CC,L,III,1)
      TMP(1)=TOLA
      CALL REALPR('TOLA',4,TMP,1)
      CALL DBLEPR('A matrix',8,A,NCOV)
      RETURN
      END
C
      SUBROUTINE MONITC(NIT,NVAR,NCOV,B,A,TOLB,TOLA)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER CC*51
      DOUBLE PRECISION A(NCOV)
      REAL B(NVAR),TOL(2)
      INTEGER III(1)
      DATA NEXT,INIT/0,0/
      TOL(1)=TOLA
      TOL(2)=TOLB
      III(1)=NIT
      IF (NEXT.NE.NIT) NEXT=0
      IF (NEXT.EQ.0) INIT=NIT
      CC="* * * I T E R A T I O N   M O N I T O R I N G * * *"
      L=LEN(CC)
      IF (NEXT.EQ.0) CALL INTPR(CC,L,III,0)
      NEXT=NIT+INIT
      CC="Nb of iterations"
      L=LEN(CC)
      CALL INTPR(CC,L,III,1)
      CALL DBLEPR('A matrix',8,A,NCOV)
      CALL REALPR('B vector',8,B,NVAR)
      CALL REALPR(' ',1,TOL,0)
      RETURN
      END
C
      SUBROUTINE MONITA(NIT,NVAR,NCOV,B,A,TOLB,TOLA)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER CC*51
      DOUBLE PRECISION A(NCOV)
      REAL B,TMP(4)
      INTEGER III(1)
      DATA NEXT,INIT/0,0/
      TMP(1)=B
      TMP(2)=TOLA
      TMP(3)=TOLB
      TMP(4)=NVAR
      III(1)=NIT
      IF (NEXT.NE.NIT) NEXT=0
      IF (NEXT.EQ.0) INIT=NIT
      CC="* * * I T E R A T I O N   M O N I T O R I N G * * *"
      L=LEN(CC)
      IF (NEXT.EQ.0) CALL INTPR(CC,L,III,0)
      NEXT=NIT+INIT
      CC="Nb of iterations"
      L=LEN(CC)
      CALL INTPR(CC,L,III,1)
      CALL REALPR('B',1,TMP,1)
      CALL DBLEPR('A matrix',8,A,NCOV)
      RETURN
      END
C
      SUBROUTINE MONITG(NIT,NP,GAM,Q,THETA,DELTA)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER CC*51
      REAL THETA(NP),DELTA(NP),TMP(2)
      INTEGER III(1)
      COMMON/OUT/IOUT
      DATA NEXT,INIT/0,0/
      IF (NEXT.NE.NIT) NEXT=0
      IF (NEXT.EQ.0) INIT=NIT
      III(1)=NIT
      CC="* * * I T E R A T I O N   M O N I T O R I N G * * *"
      L=LEN(CC)
      IF (NEXT.EQ.0) CALL INTPR(CC,L,III,0)
      NEXT=NIT+INIT
      TMP(1)=Q
      TMP(2)=GAM
      CC="Nb of iterations"
      L=LEN(CC)
      CALL INTPR(CC,L,III,1)
      CALL REALPR('Q0, Gamma',9,TMP,2)
      CALL REALPR('Theta',5,THETA,NP)
      CALL REALPR('Delta',5,DELTA,NP)
      RETURN
      END
C-----------------------------------------------------------------------
C
C                     R O B E T H  FORTRAN Source
C
C   File WGAUXI.F  Auxiliary subroutines of Chapter 3
C
C-----------------------------------------------------------------------
C
      SUBROUTINE WIDEG0(X,UCV,NOBS,NVAR,NCOV,MDX,A,SC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION  X(MDX,NVAR),A(NCOV),UCV,XNR,SUM
      REAL SC(MDX)
      EXTERNAL UCV
      DO 200 I=1,NOBS
      DO 100 J=1,NVAR
      A(J)=X(I,J)
  100 CONTINUE
      CALL NRM2ZD(A,NVAR,1,NVAR,XNR)
      SC(I)=SNGL(XNR)
  200 CONTINUE
      SIG0=1.0
  300 SUM=0.D0
      DO 400 I=1,NOBS
      S0Z=SIG0*SC(I)
      SUM=SUM+(DBLE(S0Z))**2*UCV(S0Z)
  400 CONTINUE
      IF (SUM.GT.DBLE(NOBS)) THEN
        IF (SIG0.GT.0.01) THEN
          SIG0=SIG0-0.01
        ELSE
          SIG0=SIG0/2.
        ENDIF
        IF (SIG0.LE.1.E-7) THEN
C         Value for 'SIG0' not found, 0.5 assumed.
          SIG0=0.5
          RETURN
        ENDIF
        GOTO 300
      ENDIF
      DO 500 I=1,NCOV
      A(I)=0.D0
  500 CONTINUE
      DO 600 I=1,NVAR
      II=I*(I+1)/2
      A(II)=DBLE(SIG0)
  600 CONTINUE
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE UCOW(X,SA,ST,EXU,EXUP,N,NP,NQ,NCOV,MDX,MDZ,NU,
     1           IALG,ICNV,IGWT,NIT,GWT,ZMAX,DIST,SU,SUP,SZ,SD)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  COMPUTE WEIGHTED COVARIANCE MATRIX;
C  STORE EXU VALUES IN SU AND IF (IALG.NE.1) EXUP VALUES IN SUP;
C  EXUP IS NOT USED IF IALG=1; SZ IS NOT USED IF IALG=1 OR IALG=2
C
      DIMENSION X(MDX,NP),DIST(N),SZ(MDZ,NP),GWT(N)
      DOUBLE PRECISION SA(NCOV),ST(NCOV),SU(N),SUP(NU),U,UP
      DOUBLE PRECISION SD(NP),XN,ZNR,EXU,EXUP
      EXTERNAL         EXU,EXUP
      DATA XN,SQPMQ,NQP1/2*0.,0/
C
      IF (NIT.GT.1) GOTO 10
      XN=DBLE(N)
      SQPMQ=SQRT(FLOAT(NP-NQ))
      NQP1=NQ+1
   10 ZMAX=0.0
      DO 20 IJ=1,NCOV
      ST(IJ)=0.D0
   20 CONTINUE
      DO 100 L=1,N
      DO  50 J=1,NP
      SD(J)=DBLE(X(L,J))
   50 CONTINUE
      CALL MLYZD(SA,SD,NP,NCOV,NP,1)
      CALL NRM2ZD(SD(NQP1),NP-NQ,1,NP-NQ,ZNR)
      DISTL=SNGL(ZNR)
      IF (NQ.NE.0) DISTL=DISTL/SQPMQ
      IF (ICNV.EQ.2) ZMAX=AMAX1(ZMAX,ABS(DISTL-DIST(L)))
      DIST(L)=DISTL
      U=EXU(DISTL)
      SU(L)=U
      IF (IGWT.EQ.1) U=U*DBLE(GWT(L))
      IF (IALG.EQ.1) GOTO 80
      UP=EXUP(DISTL)
      IF (NQ.NE.0) UP=UP/DBLE(SQPMQ)
      SUP(L)=UP
      IF (IALG.EQ.2) GOTO 80
      DO 70 I=1,NP
      SZ(L,I)=SNGL(SD(I))
   70 CONTINUE
   80 IJ=0
      DO 95 I=1,NP
      DO 90 J=1,I
      IJ=IJ+1
      ST(IJ)=ST(IJ)+(SD(I)*U)*SD(J)
   90 CONTINUE
   95 CONTINUE 
  100 CONTINUE
      DO 110 IJ=1,NCOV
      ST(IJ)=ST(IJ)/XN
  110 CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
C
C                     R O B E T H  FORTRAN Source
C
C  File WGMAIN.F  Main subroutines of Chapter 3
C
C-----------------------------------------------------------------------
C
      SUBROUTINE WFSHATZ(XT,N,NP,MDX,WGT,SH,SC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. MARAZZI
C.......................................................................
C
C  WEIGHTS FOR SCHWEPPE ESTIMATOR : SCHWEPPE ORIGINAL PROPOSAL
C
      DIMENSION XT(MDX,NP),SH(NP),SC(N),WGT(N)
      LOGICAL NPRCHK
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NPRCHK=N.GT.0.AND.NP.GT.0.AND.MDX.GE.N
      IF (.NOT.NPRCHK) CALL MESSGE(500,'WFSHAT',1)
C
      CALL MHATZ(XT,N,NP,NP,MDX,WGT,SH,SC)
      DO 20 I=1,N
      WGT(I)=SQRT(1.-WGT(I))
   20 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE WIMEDVZ(X,NOBS,NVAR,NCOV,MDX,ITYPW,INIT,NFIRST,A,SC)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  INITIAL VALUE OF A : STANDARDIZED CASE, A LOWER TRIANGULAR.
C                     : UNSTANDARDIZED CASE, A SYMMETRIC.
C
      DIMENSION X(MDX,NVAR),SC(NFIRST)
      DOUBLE PRECISION A(NCOV)
      LOGICAL NPRCHK
      DATA TL/1.E-8/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=NVAR.GT.0.AND.NOBS.GE.NVAR.AND.NN.EQ.NCOV.AND.
     1       NFIRST.GT.0.AND.NFIRST.LE.NOBS.AND.MDX.GE.NOBS.AND.
     2       (INIT.EQ.1.OR.INIT.EQ.2).AND.(ITYPW.EQ.1.OR.ITYPW.EQ.2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'WIMEDV',1)
C
C  COMPUTE INITIAL VALUE FOR A
C
      DO 10 J=1,NCOV
      A(J)=0.D0
   10 CONTINUE
      DO 15 J=1,NVAR
        JJ=J*(J+1)/2
        A(JJ)=1.D0
   15 CONTINUE
      IF (INIT.EQ.1) RETURN
      IF (ITYPW.EQ.2) GOTO 100
      DO 50 J=1,NVAR
      CALL LMDDZ(X(1,J),SC,NFIRST,1,XME,XMD,XSD)
      SQDEV2=SQRT(XSD**2+XME**2)
      JJ=(J*J+J)/2
      IF (SQDEV2.GT.TL) GOTO 40
      CALL MESSGE(301,'WIMEDV',0)
      A(JJ)=9999.D0
      GOTO 50
   40 A(JJ)=1.D0/DBLE(SQDEV2)
   50 CONTINUE
      RETURN
  100 DO 150 J=1,NVAR
      JJ=J*(J+1)/2
      CALL LMDDZ(X(1,J),SC,NFIRST,1,XME,XMD,XSD)
      DEV2=XSD**2+XME**2
      IF (DEV2.GT.TL) GOTO 145
      CALL MESSGE(302,'WIMEDV',0)
      A(JJ)=9999.D0
      GOTO 150
  145 A(JJ)=1.D0/DBLE(DEV2)
  150 CONTINUE
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE WYNALG(X,A,EXU,EXUP,NOBS,NVAR,NCOV,MDX,
     1                  MAXIT,NITMON,ICNV,TOL,XFUD,NIT,
     2                  DIST,SA,SS,SU,SUP,ST,SD)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  NEWTON-HUBER ALGORITHM FOR THE COMPUTATION OF THE MATRIX A
C  (STANDARDIZED CASE, A LOWER TRIANGULAR)
C
      DIMENSION X(MDX,NVAR),DIST(NOBS)
      DOUBLE PRECISION A(NCOV),SA(NCOV),SS(NCOV),ST(NCOV)
      DOUBLE PRECISION SU(NOBS),SUP(NOBS),EXU,EXUP,SD(NVAR),XN
      LOGICAL NPRCHK
      EXTERNAL EXU,EXUP,ICNVA
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=NVAR.GT.0.AND.NOBS.GE.NVAR.AND.NCOV.EQ.NN
     1       .AND.MDX.GE.NOBS.AND.(ICNV.EQ.1.OR.ICNV.EQ.2)
     2       .AND.TOL.GT.0.0.AND.MAXIT.GT.0.AND.XFUD.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'WYNALG',1)
C
C  STEP 0 : INITIALIZATION
C  ------
      IALG=2
      NU=NOBS
      XN=DBLE(NOBS)
      NIT=0
      NVARQ=0
      IF (ICNV.EQ.1) THEN
        L=0
        DO 20 I=1,NVAR
        DO 10 J=1,I
        L=L+1
        SA(L)=0.D0
        IF (I.EQ.J) SA(L)=-1.D0
   10   CONTINUE
   20   CONTINUE
      ENDIF
      DO 30 L=1,NOBS
      DIST(L)=-1.0
   30 CONTINUE
C
C  STEP 1: COMPUTE WEIGHTED COVARIANCE (ST) AND AUXILIARY VALUES
C  ------
  100 CALL UCOW(X,A,ST,EXU,EXUP,NOBS,NVAR,NVARQ,NCOV,MDX,MDX,
     1          NU,IALG,ICNV,0,NIT,DIST,DELTA,DIST,SU,SUP,X,SD)
C
C  STEP 2: CHECK CONVERGENCE
C  ------
      IF (NIT.EQ.MAXIT.OR.ICNVA(NCOV,DELTA,A,SA,TOL,ICNV).EQ.1)
     +GOTO 500
C
C  STEP 3: FIND IMPROVEMENT MATRIX SS FOR A
C  ------
      CALL PRSCNH(ST,SS,DIST,SU,SUP,XN,0.D0,NOBS,NVAR,NCOV)
C
C  STEP 4: COMPUTE GAM0 AND SET A:=(I-GAM0*SS)*SA
C  -------
      DO 410 IJ=1,NCOV
      SA(IJ)=A(IJ)
  410 CONTINUE
      CALL FUDGE(SS,NVAR,NCOV,XFUD,GAM0)
      CALL UPDATA(SS,SA,A,GAM0,NVAR,NCOV)
      NIT=NIT+1
C
C  STEP 4A: ITERATION MONITORING
C  -------
      IF (NITMON.LE.0) GOTO 100
      IF (MOD(NIT,NITMON).EQ.0) CALL MONITW(NIT,NVAR,NCOV,A,DELTA)
      GOTO 100
C
C  STEP 5: EXIT
C  ------
  500 RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE WYGALG(X,A,EXU,EXUP,NOBS,NVAR,NCOV,MDX,MDZ,
     1                  MAXIT,NITMON,ICNV,TOL,XFUD,NIT,DIST,
     2                  SA,SS,SZ,SU,SUP,SY1,SY2,ST,ST1)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  CONJUGATE GRADIENT ALGORITHM FOR THE COMPUTATION OF THE MATRIX A
C  (STANDARDIZED CASE, A LOWER TRIANGULAR)
C
      DIMENSION X(MDX,NVAR),SZ(MDZ,NVAR),DIST(NOBS)
      DOUBLE PRECISION A(NCOV),SA(NCOV),SS(NCOV),ST(NCOV),ST1(NCOV),
     +       SU(NOBS),SUP(NOBS),SY1(NVAR),SY2(NVAR),TL,EXU,EXUP,XN
      LOGICAL NPRCHK
      EXTERNAL EXU,EXUP,ICNVA
      DATA TL/1.D-8/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=NVAR.GT.0.AND.NOBS.GE.NVAR.AND.NCOV.EQ.NN
     1       .AND.MDX.GE.NOBS.AND.MDZ.GE.NOBS
     2       .AND.(ICNV.EQ.1.OR.ICNV.EQ.2).AND.
     3       TOL.GT.0.0.AND.MAXIT.GT.0.AND.XFUD.GT.0.
      IF (.NOT.NPRCHK) CALL MESSGE(500,'WYGALG',1)
C
C  STEP 0 : INITIALIZATION
C  ------
      IALG=3
      NU=NOBS
      XN=DBLE(NOBS)
      NIT=0
      NVARQ=0
      IF (ICNV.EQ.1) THEN
        L=0
        DO 20 I=1,NVAR
        DO 10 J=1,I
        L=L+1
        SA(L)=0.D0
        IF (I.EQ.J) SA(L)=-1.D0
   10   CONTINUE
   20   CONTINUE
      ENDIF
      DO 30 L=1,NOBS
      DIST(L)=-1.0
   30 CONTINUE
C
C  STEP 1: COMPUTE WEIGHTED COVARIANCE (ST) AND AUXILIARY VALUES
C  ------
  100 CALL UCOW(X,A,ST,EXU,EXUP,NOBS,NVAR,NVARQ,NCOV,MDX,MDZ,
     1          NU,IALG,ICNV,0,NIT,DIST,DELTA,DIST,SU,SUP,SZ,SY1)
C
C  STEP 2: CHECK CONVERGENCE
C  ------
      IF (NIT.EQ.MAXIT.OR.ICNVA(NCOV,DELTA,A,SA,TOL,ICNV).EQ.1)
     +GOTO 500
C
C  STEP 3: FIND IMPROVEMENT MATRIX SS FOR A
C  ------
      CALL PRSCCG(ST,ST1,SS,NOBS,NVAR,NCOV,MDZ,NIT,TL,XN,
     1            DIST,SU,SUP,SY1,SY2,SZ)
C
C  STEP 4: COMPUTE GAM0 AND SET A:=(I-GAM0*SS)*SA
C  -------
      DO 410 IJ=1,NCOV
      SA(IJ)=A(IJ)
  410 CONTINUE
      CALL FUDGE(SS,NVAR,NCOV,XFUD,GAM0)
      CALL UPDATA(SS,SA,A,GAM0,NVAR,NCOV)
      NIT=NIT+1
C
C  STEP 4A: ITERATION MONITORING
C  -------
      IF (NITMON.LE.0) GOTO 100
      IF (MOD(NIT,NITMON).EQ.0) CALL MONITW(NIT,NVAR,NCOV,A,DELTA)
      GOTO 100
C
C  STEP 5: EXIT
C  ------
  500 RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE WYFALG(X,A,GWT,EXU,NOBS,NVAR,NVARQ,NCOV,MDX,
     1                  TAU,MAXIT,NITMON,ICNV,ITYPW,IGWT,TOL,
     2                  NIT,DIST,SU,SA,ST,SD,SZ)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  FIXED POINT ALGORITHM FOR THE COMPUTATION OF THE MATRIX A
C  STANDARDIZED CASE   : A LOWER TRIANGULAR (ITYPW.EQ.1)
C  UNSTANDARDIZED CASE : A SYMMETRIC (ITYPW.EQ.2)
C
      DIMENSION X(MDX,NVAR),DIST(NOBS),GWT(NOBS)
      DOUBLE PRECISION A(NCOV),SA(NCOV),ST(NCOV),SD(NVAR),SZ(NVAR)
      DOUBLE PRECISION SU(NOBS),WUP(1),U,EXU,XN,ZNR
      LOGICAL NPRCHK
      EXTERNAL EXU,ICNVA
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=NVAR.GT.0.AND.NVARQ.GE.0.AND.NVARQ.LT.NVAR.AND.
     1       NOBS.GE.NVAR.AND.NCOV.EQ.NN.AND.MDX.GE.NOBS.AND.
     2       TAU.GE.0.0.AND.TOL.GT.0.0.AND.(ICNV.EQ.1.OR.
     3       ICNV.EQ.2).AND.(ITYPW.EQ.1.OR.ITYPW.EQ.2).AND.
     4       MAXIT.GT.0.AND.(IGWT.EQ.0.OR.IGWT.EQ.1)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'WYFALG',1)
C
C  STEP 0 : INITIALIZATION
C  ------
      IALG=1
      NU=1
      NIT=0
      XN=DBLE(NOBS)
      IF (ICNV.EQ.1) THEN
        L=0
        DO 20 I=1,NVAR
        DO 10 J=1,I
        L=L+1
        SA(L)=0.D0
        IF (I.EQ.J) SA(L)=-1.D0
   10   CONTINUE
   20   CONTINUE
      ENDIF
      DO 30 L=1,NOBS
      DIST(L)=-1.0
   30 CONTINUE
C
C  STEP 1: COMPUTE WEIGHTED COVARIANCE (ST) AND AUXILIARY VALUES
C  ------
  100 IF (ITYPW.EQ.1) THEN
        CALL UCOW(X,A,ST,EXU,EXU,NOBS,NVAR,NVARQ,NCOV,MDX,MDX,
     1       NU,IALG,ICNV,IGWT,NIT,GWT,DELTA,DIST,SU,WUP,X,SD)
      ELSE
        DO 130 I=1,NCOV
        ST(I)=0.D0
  130   CONTINUE
        DELTA=0.
        DO 180 L=1,NOBS
        DO 150 J=1,NVAR
        SD(J)=DBLE(X(L,J))
  150   CONTINUE
        CALL MSFZD(A,SD,SZ,NVAR,NCOV,1,NVAR,NVAR)
        CALL NRM2ZD(SZ,NVAR,1,NVAR,ZNR)
        DISTL=SNGL(ZNR)
        IF (ICNV.NE.1) DELTA=AMAX1(DELTA,ABS(DISTL-DIST(L)))
        DIST(L)=DISTL
        U=EXU(DISTL)
        SU(L)=U
        IF (IGWT.EQ.1) U=U*DBLE(GWT(L))
        IJ=0
        DO 175 I=1,NVAR
        DO 170 J=1,I
        IJ=IJ+1
        ST(IJ)=ST(IJ)+(SD(I)*U)*SD(J)
  170   CONTINUE 
  175   CONTINUE 
  180   CONTINUE
        DO 190 IJ=1,NCOV
        ST(IJ)=ST(IJ)/XN
  190   CONTINUE
      ENDIF
C
C  STEP 2: CHECK CONVERGENCE
C  ------
      IF (NIT.EQ.MAXIT.OR.ICNVA(NCOV,DELTA,A,SA,TOL,ICNV).EQ.1)
     +GOTO 500
C
C  STEP 3: FIND IMPROVEMENT MATRIX ST FOR A
C  ------
      INFO=0
      CALL PRSCF0(ST,NVAR,NCOV,TAU,INFO)
      IF (INFO.NE.0) CALL MESSGE(400+INFO,'WYFALG',0)
C
C  STEP 4: SET SA:=A AND A:=(ST)*SA IF ITYPW.EQ.1 ELSE A:=(ST**T)*ST
C  -------
      DO 410 IJ=1,NCOV
      SA(IJ)=A(IJ)
  410 CONTINUE
      IF (ITYPW.EQ.1) THEN
        CALL MTT3ZD(SA,ST,A,NVAR,NCOV)
      ELSE
        CALL MTT1ZD(ST,A,NVAR,NCOV)
      ENDIF
      NIT=NIT+1
C
C  STEP 4A: ITERATION MONITORING
C  -------
      IF (NITMON.LE.0) GOTO 100
      IF (MOD(NIT,NITMON).EQ.0) CALL MONITW(NIT,NVAR,NCOV,A,DELTA)
      GOTO 100
C
C  STEP 5: EXIT
C  ------
  500 RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE WYFCOL(X,EXU,NOBS,NVAR,NCOV,MDX,MDA,MDW,IWGT,APAR,
     1                  TAU,TOL,MAXIT,NITMON,ICNV,K,NIT,DIST,
     2                  A,SU,SB,SB0,SF,SG,SH,IP,SW,SZ)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION X(MDX,NVAR),A(MDA,NVAR),SF(NVAR),SG(NVAR)
      DOUBLE PRECISION SH(NVAR),SU(NOBS),SW(MDW,NVAR),SB(NCOV)
      DOUBLE PRECISION SB0(NCOV),SZ(NVAR),ZNR,SQN,SUM,U,EXU
      REAL DIST(NOBS)
      INTEGER IP(NVAR)
      LOGICAL NPRCHK
      EXTERNAL EXU,ICNVA
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,BT,CW
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NN=NVAR*(NVAR+1)/2
      NPRCHK=NVAR.GT.0.AND.NOBS.LE.MDX.AND.NOBS.LE.MDW.AND.NVAR.LE.MDA
     1        .AND.NCOV.EQ.NN.AND.TAU.GE.0..AND.NOBS.GE.NVAR.AND.
     2        (ICNV.EQ.1.OR.ICNV.EQ.2).AND.(IWGT.EQ.0.OR.IWGT.EQ.1.OR.
     3        IWGT.EQ.2).AND.APAR.GT.0..AND.TOL.GT.0.0.AND.MAXIT.GT.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'WYFCOL',1)
C
C STEP 0.1:  Initialization and setting of the u-parameter
C --------   A <=> T until step 5
      INTCH=1
      MDXP1=MDX+1
      KCOV=NCOV
      SQN=DBLE(NOBS)
      SQN=DSQRT(SQN)
      NIT=0
      K0=NVAR
      DO 20 J=1,NVAR
      DO 10 I=1,NVAR
      A(I,J)=0.D0
   10 CONTINUE
      A(J,J)=1.D0
   20 CONTINUE
      IF (IWGT.EQ.1) CKW=SQRT(APAR*K0)
      IF (IWGT.NE.2) GOTO 30
      A2=0.
      B2=APAR*K0
C
C STEP 0.1: Compute an initial value of B
C --------
   30 CALL WIDEG0(X,EXU,NOBS,K0,NCOV,MDX,SB,DIST)
      IF (ICNV.EQ.1) THEN
        L=0
        DO 50 I=1,NVAR
        DO 40 J=1,I
        L=L+1
        SB0(L)=0.D0
        IF (I.EQ.J) SB0(L)=-1.D0
   40   CONTINUE
   50   CONTINUE
      ENDIF
      DO 60 I=1,NOBS
      DIST(I)=-1.0
   60 CONTINUE
C
C STEP 1: Set D:= diag(u|Bz|) and W:=D^0.5*X
C -----
  100 DELTA=0.
      DO 130 I=1,NOBS
      DO 110 J=1,NVAR
      SZ(J)=X(I,J)
  110 CONTINUE
      CALL MLYZD(SB,SZ,K0,KCOV,K0,1)
      CALL NRM2ZD(SZ,K0,1,K0,ZNR)
      DISTI=SNGL(ZNR)
      IF (ICNV.EQ.2) DELTA=AMAX1(DELTA,ABS(DISTI-DIST(I)))
      DIST(I)=DISTI
      U=EXU(DISTI)
      SU(I)=U
      U=DSQRT(U)
      DO 120 J=1,NVAR
      SW(I,J)=U*X(I,J)
  120 CONTINUE
  130 CONTINUE
C
C STEP 2: Check convergence
C ------
      IF (NIT.EQ.MAXIT.OR.ICNVA(NCOV,DELTA,SB,SB0,TOL,ICNV).EQ.1)
     +GOTO 500
C
C STEP 3.1: Compute the pseudorank K and the Q, R, P and V matrices
C --------
      CALL RIMTRDZ(SW,NOBS,NVAR,MDW,INTCH,TAU,K,SF,SG,SH,IP)
      IF (K.EQ.K0) GOTO 400
C
C STEP 3.2:  Update APAR and set K0:=K, T:=T*P*V, X:=X*P*V*Jk0
C --------
      K0=K
      IF (IWGT.EQ.1) CKW=SQRT(APAR*K0)
      IF (IWGT.EQ.2) B2=APAR*K0
      KP1=K+1
      KCOV=K*(KP1)/2
      CALL MZPVD(SW,IP,SG,A,NOBS,NVAR,NVAR,K,MDW,MDA,SZ)
      CALL MZPVD(SW,IP,SG,X,NOBS,NOBS,NVAR,K,MDW,MDX,SZ)
      DO 320 J=KP1,NVAR
      DO 310 I=1,NOBS
      X(I,J)=0.D0
  310 CONTINUE
  320 CONTINUE
      GOTO 30
C
C STEP 4: Set X:=X*P, T:=T*P, B0:=B & B:=sqrt(N)*[R1**T 0]-
C ------
  400 CALL MZPD(X,IP,MDX,NOBS,NVAR)
      CALL MZPD(A,IP,MDA,NVAR,NVAR)
C
C Store R1 in SB
C
      DO 410 IJ=1,NCOV
      SB0(IJ)=SB(IJ)
  410 CONTINUE
      DO 420 L=1,NCOV
      SB(L)=0.D0
  420 CONTINUE
      L=0
      DO 430 J=1,K
      DO 425 I=1,J
      L=L+1
      SB(L)=SW(I,J)
  425 CONTINUE
  430 CONTINUE
C
C Invert SB upon itself and multiply it by sqrt(N)
C
      CALL MINVZD(SB,K,L,TAU,INFO)
      IF (INFO.NE.0) CALL MESSGE(400+INFO,'WFDEGD',0)
      DO 470 IJ=1,NCOV
      SB(IJ)=SQN*SB(IJ)
  470 CONTINUE
      NIT=NIT+1
C
C STEP 4A: Iteration monitoring
C ------
      IF (NITMON.LE.0) GOTO 100
      IF (MOD(NIT,NITMON).EQ.0) CALL MONITW(NIT,NVAR,NCOV,SB,DELTA)
      GOTO 100
C
C STEP 5: Put A in SW and set A:=B*T**T and exit
C ------
  500 DO 515 I=1,NVAR
      DO 510 J=1,NVAR
        SW(I,J)=A(I,J)
        A(I,J)=0.D0
  510 CONTINUE
  515 CONTINUE 
      L=0
      DO 540 I=1,NVAR
        DO 530 J=1,NVAR
        SUM=0.D0
        DO 520 JJ=1,I
        SUM=SUM+SB(L+JJ)*SW(J,JJ)
  520   CONTINUE
        A(I,J)=SUM
  530   CONTINUE
        L=L+I
  540 CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
C
C                 R O B E T H  FORTRAN Source
C
C  File WGTFUN.F  Weight functions of Chapter 11
C
C-----------------------------------------------------------------------
C
      FUNCTION PSY(S)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE IN THE POINT T OF THE FUNCTION PSI
C
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      IPS=IABS(IPSI)
      ABST=ABS(S)
      IF (IPS.EQ.0) GOTO 100
      IF (IPS.EQ.1) GOTO 200
      IF (IPS.EQ.2) GOTO 300
      IF (IPS.EQ.3) GOTO 400
      IF (IPS.EQ.4) GOTO 500
      IF (IPS.EQ.10) GOTO 700
C
C  PSI(S)=USER PSI FUNCTION
C
C      PSY=UPSI(S)
C      RETURN
C
C  PSI(S)=S
C
  100 PSY=S
      RETURN
C
C  PSI(S,C)=MAX(-C,MIN(C,S))
C
  200 TMP=AMIN1(C,ABST)
      IF (S.LT.0.) TMP=-TMP
      GOTO 600
C
C  PSI(S,H1,H2,H3)=-PSI(-S,H1,H2,H3)
C                 =S FOR 0 .LE. S .LE. H1
C                 =H1 FOR H1 .LE. S .LE. H2
C                 =H1*(H3-S)/(H3-H2) FOR H2 .LE. S .LE. H3
C                 =0 FOR S .GT. H3
C
  300 TMP=0
      IF (ABST.GE.H3) GOTO 600
      IF (ABST.LE.H2) TMP=AMIN1(H1,ABST)
      IF (ABST.GT.H2) TMP=H1*(H3-ABST)/(H3-H2)
      IF (S.LT.0.) TMP=-TMP
      GOTO 600
C
C  PSI(S)=S*[MAX(1-S**2,0)]**2
C
  400 TMP=0.
      IF (ABST.GE.1.) GOTO 600
      TMP=S*(1.-S*S)*(1.-S*S)
      GOTO 600
C
C  PSI(S)=(6/K)*(S/K)*[MAX(1-(S/K)**2,0)]**2
C
  500 TMP=0.
      IF (ABST.GE.XK) GOTO 600
      SK=S/XK
      TMP=(6.*SK/XK)*(1.-SK*SK)*(1.-SK*SK)
      GOTO 600
C
C  PSI(S,H1,H2)=MAX(H1,MIN(H2,S))
C
  700 TMP=AMIN1(H2,S)
      IF (TMP.LE.H1) TMP=H1

  600 PSY=TMP
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION RHO(S)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE OF THE INTEGRAL FROM 0 TO S OF PSI(T)
C
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      IPS=IABS(IPSI)
      ABST=ABS(S)
      S2=S*S
      IF (IPS.EQ.0) GOTO 100
      IF (IPS.EQ.1) GOTO 200
      IF (IPS.EQ.2) GOTO 300
      IF (IPS.EQ.3) GOTO 400
      IF (IPS.EQ.4) GOTO 500
      IF (IPS.EQ.10) GOTO 700
C      RHO=URHO(S)
C      RETURN
  100 RHO=S2/2.
      RETURN
  200 TMP=S2/2.
      IF (ABST.GT.C) TMP=C*(ABST-C/2.)
      GOTO 600
  300 IF (ABST.GT.H2) GOTO 350
      TMP=S2/2.
      IF (ABST.GT.H1) TMP=H1*(ABST-H1/2.)
      GOTO 600
  350 TMP=0.5*H1*(H3+H2-H1)
      IF (ABST.LT.H3) TMP=TMP-.5*H1*(H3-ABST)**2/(H3-H2)
      GOTO 600
  400 TMP=1./6.
      IF (ABST.GE.1.) GOTO 600
      TMP=(S2*(S2-3)+3)*S2/6.
      GOTO 600
  500 TMP=1.
      IF (ABST.GE.XK) GOTO 600
      S2=S2/(XK**2)
      TMP=(S2*(S2-3)+3)*S2
      GOTO 600
  700 TMP=S2/2.
      IF (S.LT.H1) TMP=H1*(S-H1/2.)
      IF (S.GT.H2) TMP=H2*(S-H2/2.)
  600 RHO=TMP
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION PSP(S)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE FIRST DERI-
C  VATIVE OF THE FUNCTION PSI .
C
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      IPS=IABS(IPSI)
      ABST=ABS(S)
      IF (IPS.EQ.0) GOTO 100
      IF (IPS.EQ.1) GOTO 200
      IF (IPS.EQ.2) GOTO 300
      IF (IPS.EQ.3) GOTO 400
      IF (IPS.EQ.4) GOTO 500
      IF (IPS.EQ.10) GOTO 700

C      PSP=UPSP(S)
C      RETURN
  100 PSP=1.
      RETURN
  200 TMP=0.
      IF (ABST.LE.C) TMP=1.
      GOTO 600
  300 TMP=1.
      IF (ABST.LE.H1) GOTO 600
      TMP=0.
      IF ((ABST.LT.H2).OR.(ABST.GT.H3)) GOTO 600
      TMP=H1/(H2-H3)
      GOTO 600
  400 TMP=0.
      IF (ABST.GE.1.) GOTO 600
      S2=S*S
      TMP=(1.-S2)*(1.-5.*S2)
      GOTO 600
  500 TMP=0.
      IF (ABST.GE.XK) GOTO 600
      S2=(S/XK)**2
      TMP=(6./XK)*(1.-S2)*(1.-5.*S2)/XK
      GOTO 600
  700 TMP=0.
      IF (S.GE.H1.AND.S.LE.H2) TMP=1.
  600 PSP=TMP
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION CHI(S)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE OF THE FUNCTION CHI(S)=S*S/2 IF IPSI=0,
C  CHI(S)=HUBER'S CHI FUNCTION IF ABS(IPSI) IS LESS THAN 4,
C  AND CHI(S)=CHIK(S) FOR S-ESTIMATES IF IPS=4.
C
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
      IF (IPSI.EQ.0) GOTO 100
      IPS=IABS(IPSI)
      IF (IPS.LE.3) GOTO 200
      IF (IPS.EQ.4) GOTO 300
      IF (IPS.EQ.10) GOTO 500

C      CHI=UCHI(S)
C      RETURN
  100 CHI=S*S/2.
      RETURN
  200 ABST=ABS(S)
      PS=AMIN1(D,ABST)
      CHI=PS*PS/2.
      RETURN
  300 TMP=1.
      ABST=ABS(S)
      IF (ABST.GE.XK) GOTO 600
      S2=(S/XK)**2
      TMP=(S2*(S2-3)+3)*S2
      GOTO 600
  500 PS=AMAX1(H1,AMIN1(S,H2))
      TMP=PS*PS/2.
  600 CHI=TMP
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION UCV(S)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE U-FUNCTION
C  FOR AFFINE INVARIANT COVARIANCES
C
      DOUBLE PRECISION Z2,Q,Q2,PD,PC,XEXPD,DSPI
      EXTERNAL XEXPD
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,DV,CW
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
      DATA GAM,DSPI/1.E-6,2.506628274631001/
      UCV=1.D0
      IF (IUCV.EQ.0) RETURN
      ZED=S
      IF (IUCV.EQ.1) GOTO 100
      IF (IUCV.EQ.2) GOTO 200
      IF (IUCV.EQ.3) GOTO 300
      IF (IUCV.EQ.4) GOTO 400
      IF (IUCV.EQ.5) GOTO 500
      IF (IUCV.EQ.6) GOTO 600
      IF (IUCV.EQ.7) GOTO 700
C      UCV=UUCV(S)
C      RETURN
  100 IF (S*S.GE.A2.AND.S.GE.0.) GOTO 110
      IF (S.GT.GAM) GOTO 110
      CALL MESSGE(200,'UCV   ',0)
      ZED=GAM
  110 ZZ=ZED*ZED
      Z2=DBLE(ZZ)
      IF (ZZ.GT.B2) UCV=DBLE(B2)/Z2
      IF (ZZ.LT.A2) UCV=DBLE(A2)/Z2
      RETURN
  200 IF (S.LE.0.) RETURN
      IF (S.LE.GAM) ZED=GAM
      Q=DBLE(CHK/ZED)
      CALL GAUSSZD(1,Q,PC)
      UCV=2.D0*PC-1.D0
      RETURN
  300 IF (S.LE.0.) RETURN
      IF (S.LE.GAM) ZED=GAM
      Q=CKW/DBLE(ZED)
      Q2=Q*Q
      CALL GAUSSZD(1,Q,PC)
      PD=XEXPD(-Q2/2.D0)/DSPI
      UCV=Q2+(1.D0-Q2)*(2.D0*PC-1.D0)-2.D0*Q*PD
      RETURN
  400 IF (S.LE.BB) RETURN
      IF (S.GT.GAM) GOTO 410
      CALL MESSGE(200,'UCV   ',0)
      ZED=GAM
  410 UCV=DBLE(BB/ZED)
      RETURN
  500 CONTINUE
  600 IF (S.LE.EM) RETURN
      UCV=0.D0
      IF (S.GE.EM+CR) RETURN
      ZED=1.0-((S-EM)/CR)**2
      UCV=DBLE(ZED)**2
      RETURN
  700 ZED=1.0/(S+ENU)
      UCV=DBLE(ZED)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION UPCV(S)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE FIRST DERIVATIVE
C  OF THE U-FUNCTION FOR AFFINE INVARIANT COVARIANCES
C
      DOUBLE PRECISION Z2,Q,Q2,PD,PC,XEXPD,DSPI
      EXTERNAL XEXPD
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,DV,CW
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
      DATA GAM,DSPI/1.E-6,2.506628274631001/
      UPCV=0.D0
      IF (IUCV.EQ.0) RETURN
      ZED=S
      IF (IUCV.EQ.1) GOTO 100
      IF (IUCV.EQ.2) GOTO 200
      IF (IUCV.EQ.3) GOTO 300
      IF (IUCV.EQ.4) GOTO 400
      IF (IUCV.EQ.5) GOTO 500
      IF (IUCV.EQ.6) GOTO 600
      IF (IUCV.EQ.7) GOTO 700
C      UPCV=UUPCV(S)
C      RETURN
  100 IF (S*S.GE.A2.AND.S.GE.0.) GOTO 110
      IF (S.GT.GAM) GOTO 110
      CALL MESSGE(200,'UPCV  ',0)
      ZED=GAM
  110 ZZ=ZED*ZED
      Z2=DBLE(ZZ)
      IF (ZZ.GT.B2) UPCV=-2.D0*DBLE(B2/ZED)/Z2
      IF (ZZ.LT.A2) UPCV=-2.D0*DBLE(A2/ZED)/Z2
      RETURN
  200 IF (S.LE.0.) RETURN
      IF (S.LE.GAM) ZED=GAM
      Z2=DBLE(ZED*ZED)
      Q=DBLE(CHK/ZED)
      Q2=Q*Q
      PD=XEXPD(-Q2/2.D0)/DSPI
      UPCV=2.D0*PD*(-DBLE(CHK)/Z2)
      RETURN
  300 IF (S.LE.0.) RETURN
      IF (S.LE.GAM) ZED=GAM
      Q=DBLE(CKW/ZED)
      CALL GAUSSZD(1,Q,PC)
      UPCV=-4.D0*(Q*Q)*(1.D0-PC)/DBLE(ZED)
      RETURN
  400 IF (S.LT.BB) RETURN
      IF (S.GT.GAM) GOTO 410
      CALL MESSGE(200,'UPCV  ',0)
      ZED=GAM
  410 IF (S.GT.BB) UPCV=-DBLE(BB/(ZED*ZED))
      RETURN
  500 CONTINUE
  600 IF (S.LE.EM.OR.S.GE.EM+CR) RETURN
      Z2=DBLE(CR**2)
      Q=DBLE(EM-S)
      UPCV=-4.D0*(Q**2-Z2)*Q/(Z2**2)
      RETURN
  700 ZED=-1.0/(S+ENU)**2
      UPCV=DBLE(ZED)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION VCV(S)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE V-FUNCTION
C  FOR AFFINE INVARIANT COVARIANCES
C
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,DV,CW
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
      VCV=1.D0
      IF (IUCV.EQ.1.OR.IUCV.EQ.4) VCV=DBLE(DV)
      IF (IUCV.EQ.5) VCV=DBLE(VK)
      IF (IUCV.EQ.6) THEN
       VCV=0.D0
       IF (S.GE.EM+CR) RETURN
       IF (S.GE.0.AND.S.LE.EM) THEN
         VCV=DBLE(S*S)/DBLE(FLOAT(NP))
       ELSEIF (S.GT.EM) THEN
         ZED=S*(1.0-((S-EM)/CR)**2)
         VCV=DBLE(ZED)**2/DBLE(FLOAT(NP))
       ENDIF
      ENDIF
      IF (IUCV.EQ.7) VCV=DBLE(V7)
C     VCV=UVCV(S)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION VPCV(S)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE FIRST DERIVATIVE
C  OF THE V-FUNCTION FOR AFFINE INVARIANT COVARIANCES
C
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,DV,CW
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
      VPCV=0.D0
      IF (IUCV.EQ.6) THEN
       IF (S.GE.EM+CR) RETURN
       IF (S.GE.0.AND.S.LE.EM) THEN
         VPCV=2.D0*DBLE(S)/DBLE(FLOAT(NP))
       ELSEIF (S.GT.EM) THEN
         CR2=CR*CR
         ZED=2.*S*(1.0+(EM-3.0*S)*((EM-S)**3)/(CR2**2)-
     +       2.0*(EM-S)*(EM-2.0*S)/CR2)
         VPCV=DBLE(ZED)/DBLE(FLOAT(NP))
       ENDIF
      ENDIF
C      VPCV=UVPCV(S)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION WCV(S)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE W-FUNCTION
C  FOR AFFINE INVARIANT COVARIANCES
C
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,DV,CW
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
      DATA GAM/1.E-6/
      WCV=1.D0
      IF (IUCV.EQ.1) GOTO 100
      IF (IUCV.EQ.7) GOTO 700
      IF (IUCV.GE.5) GOTO 500
C      IF (IUCV.LE.4) RETURN
C      WCV=UWCV(S)
      RETURN
  100 IF (S.LE.CW) RETURN
      ZED=S
      IF (S.GT.GAM) GOTO 110
      CALL MESSGE(200,'WCV   ',0)
      ZED=GAM
  110 WCV=DBLE(CW/ZED)
      RETURN
  500 IF (S.LE.EM) RETURN
      WCV=0.D0
      IF (S.GE.EM+CR) RETURN
      ZED=1.0-((S-EM)/CR)**2
      WCV=DBLE(ZED)**2
      RETURN
  700 ZED=1.0/(S+ENU)
      WCV=DBLE(ZED)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION WPCV(S)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE FIRST DERIVATIVE
C  OF THE W-FUNCTION FOR AFFINE INVARIANT COVARIANCES
C
      DOUBLE PRECISION Z2,Q
      COMMON/UCVPR/IUCV,A2,B2,CHK,CKW,BB,DV,CW
      COMMON/UCV56/EM,CR,VK,NP,ENU,V7
      DATA GAM/1.E-6/
      WPCV=0.D0
      IF (IUCV.EQ.1) GOTO 100
      IF (IUCV.EQ.7) GOTO 700
      IF (IUCV.GE.5) GOTO 500
C      IF (IUCV.LE.4) RETURN
C      WPCV=UWPCV(S)
      RETURN
  100 IF (S.LE.CW) RETURN
      ZED=S
      IF (S.GT.GAM) GOTO 110
      CALL MESSGE(200,'WPCV  ',0)
      ZED=GAM
  110 WPCV=-DBLE(CW/(ZED*ZED))
      RETURN
  500 IF (S.LE.EM.OR.S.GE.EM+CR) RETURN
      Z2=DBLE(CR**2)
      Q=DBLE(EM-S)
      WPCV=-4.D0*(Q**2-Z2)*Q/(Z2**2)
      RETURN
  700 ZED=-1.0/(S+ENU)**2
      WPCV=DBLE(ZED)
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION WWW(S)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE W BAR-FUNCTION
C
      DOUBLE PRECISION Z,UCV
      EXTERNAL UCV
      COMMON/WWWPR/IWWW
      DATA GAM/1.E-3/
      WWW=1.D0
      IF (IWWW.EQ.0) RETURN
      Z=DBLE(S)
      IF (IWWW.EQ.1) GOTO 100
      IF (IWWW.EQ.2) GOTO 200
      IF (IWWW.EQ.3) GOTO 300
  100 IF (S.GT.GAM) GOTO 110
      CALL MESSGE(200,'WWW   ',0)
      Z=DBLE(GAM)
  110 WWW=1.D0/Z
      RETURN
  200 WWW=UCV(S)
      RETURN
  300 WWW=DSQRT(UCV(S))
      RETURN
      END
C 
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION UGL(UPAR,NPAR,S)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHORS : A. RANDRIAMIHARISOA / A. MARAZZI
C.......................................................................
C
C  PURPOSE
C  -------
C  WEIGHT FUNCTION FOR THE A-STEP 
C
      DIMENSION UPAR(NPAR)
      DOUBLE PRECISION AA,CC,T1,T2,PREC,PP,GFUN,DNI,PI,PJ,TEMP,XP30  
      EXTERNAL GFUN
      COMMON/UGLPR/IUGL,ICASE,B
      DATA STOL,PREC,XP30/1.E-3,0.D0,0.D0/
C
      IF (PREC.EQ.0.D0) THEN
c       CALL MACHZ(2,PRCS)
c       PREC=100.D0*DBLE(PRCS)
        PREC=6.02007D-7
        XP30=DEXP(-30.D0)
      ENDIF
      YI=UPAR(1)
      ENI=UPAR(2)
      GI=UPAR(3)
      CI=UPAR(4)
      CC=DBLE(CI)
      NI=INT(ENI+0.001)
      DNI=DBLE(ENI)
      SI=S
      IF (S.LE.STOL) SI=STOL
      A=B/SI
      AA=DBLE(B)/DBLE(SI)
      UGL=1.D0
      IF (IUGL.EQ.1) THEN
C====>  OPTION 1 
        PP=GFUN(ICASE,1,GI)
        PI=PP
        IF (ICASE.EQ.1) THEN
C==>      LOGISTIC BERNOULLI
          TEMP=DABS(1.D0-PI-CC)
          T1=AA**2
          IF (TEMP.LT.AA) T1=TEMP**2
          TEMP=DABS(-PI-CC)
          T2=AA**2
          IF (TEMP.LT.AA) T2=TEMP**2 
          UGL=T1*PI + T2*(1.D0-PI)
        ELSEIF (ICASE.EQ.2) THEN
C==>      LOGISTIC BINOMIAL
          T2=0.D0
          DO 200 J=0,NI
            CALL PROBINZ(J,NI,PI,0,PJ)
            TEMP=DABS(DBLE(J)-(DNI*PP)-CC)
            T1=AA**2
            IF (TEMP.LT.AA) T1=TEMP**2
            T2=T2+T1*PJ
  200     CONTINUE
          UGL=T2
        ELSEIF (ICASE.EQ.3) THEN
C==>      LOGISTIC POISSON
          MI=INT(100.*PI)
          IF (MI.LT.1) MI=150
          IF (MI.GT.150) MI=150
          IF (PI.LT.XP30) PI=XP30
          IF (PI.GT.1.D6) PI=1.D6
          T2=0.D0
          DO 300 J=0,MI
            CALL PRPOISZ(PI,J,0,PJ)
c           FJME=DBLE(J)-PI
            TEMP=DABS(DBLE(J)-PI-CC)
            T1=AA**2
            IF (TEMP.LT.A) T1=TEMP**2
            T2=T2+T1*PJ
            IF (DBLE(J).GT.PI.AND.T1*PJ.LT.PREC) GOTO 350
  300     CONTINUE
  350     UGL=T2
        ENDIF
      ELSE
C====>  OPTION 2 (IUGL=2)
        PP=GFUN(ICASE,NI,GI)
        TEMP=DABS(DBLE(YI)-PP-CC)
        UGL=AA**2
        IF (TEMP.LT.AA) UGL=TEMP**2
      ENDIF
      RETURN
      END

C
C-----------------------------------------------------------------------
C
      FUNCTION USERFS(S)
C
C  PURPOSE
C  -------
C  Dummy function (Single precision)
C
      USERFS=S
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      double precision function userfd(s)
      real s
C
C This function is implicitly used by the S-PLUS examples included 
C in WGSEXM.S and TSSEXM.S.
C The P parameter can be assigned via the common PSIPR by means of 
C the dfcomn(xk=...) command. If xk is needed, use h1 with :
C     common/psipr/ipsi,c,p,h2,h3,xk,d 
c and assign p a value by typing dfcomn(h1=...) from S-PLUS.
C
C     parameter (p=4.0)
      common/psipr/ipsi,c,h1,h2,h3,p,d
      userfd=dble(p)*1.d12
      if (s.gt.1.e-6) userfd=dble(p)/dble(s*s)
      return
      end
cc
cc======================================================================
cc
      subroutine int92(y,theta,psp0,expsi,exchi,exrho,sigmai,
     x                 n,tol,gam,isigma,maxit,maxis,nitmon,
     x                 nit,sigmaf,rs,sc)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer expsi,exchi,exrho,n,isigma,maxit,maxis,nitmon,nit
       real y(n),theta,psp0,sigmai,tol,gam,sigmaf,rs(n),sc(n)
       external psy,userfs
       if (expsi.eq.1) then
         call int93(y,theta,psp0,psy,exchi,exrho,sigmai,
     x              n,tol,gam,isigma,maxit,maxis,nitmon,
     x              nit,sigmaf,rs,sc)
       else
         call int93(y,theta,psp0,userfs,exchi,exrho,sigmai,
     x              n,tol,gam,isigma,maxit,maxis,nitmon,
     x              nit,sigmaf,rs,sc)
       endif
       return
      end
      subroutine int93(y,theta,psp0,expsi,exchi,exrho,sigmai,
     x                 n,tol,gam,isigma,maxit,maxis,nitmon,
     x                 nit,sigmaf,rs,sc)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer exchi,exrho,n,isigma,maxit,maxis,nitmon,nit
       real y(n),theta,psp0,sigmai,tol,gam,sigmaf,rs(n),sc(n)
       external expsi,chi,userfs
       if (exchi.eq.4) then
         call int94(y,theta,psp0,expsi,chi,exrho,sigmai,
     x              n,tol,gam,isigma,maxit,maxis,nitmon,
     x              nit,sigmaf,rs,sc)
       else
         call int94(y,theta,psp0,expsi,userfs,exrho,sigmai,
     x              n,tol,gam,isigma,maxit,maxis,nitmon,
     x              nit,sigmaf,rs,sc)
       endif
       return
      end
      subroutine int94(y,theta,psp0,expsi,exchi,exrho,sigmai,
     x                 n,tol,gam,isigma,maxit,maxis,nitmon,
     x                 nit,sigmaf,rs,sc)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : J. JOSS
C.......................................................................
C
       integer n,isigma,maxit,maxis,nitmon,nit,exrho 
       real y(n),theta,psp0,sigmai,tol,gam,sigmaf,rs(n),sc(n)
       external expsi,exchi,rho,userfs
       if (exrho.eq.2) then
         call lywalg(y,theta,psp0,expsi,exchi,rho,sigmai,
     x               n,tol,gam,isigma,maxit,maxis,nitmon,
     x               nit,sigmaf,rs,sc)
       else
         call lywalg(y,theta,psp0,expsi,exchi,userfs,sigmai,
     x               n,tol,gam,isigma,maxit,maxis,nitmon,
     x               nit,sigmaf,rs,sc)
       endif
       return
      end
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LYWALG(Y,THETA,PSP0,EXPSI,EXCHI,EXRHO,SIGMAI,
     1                  N,TOL,GAM,ISIGMA,MAXIT,MAXIS,NITMON,
     1                  NIT,SIGMAF,RS,SC)
C.......................................................................
C
C   COPYRIGHT 1996 Alfio Marazzi
C
C   AUTHOR: A. RANDRIAMIHARISOA
C.......................................................................
C
C   PURPOSE
C   -------
C   W-ALGORITHM FOR ROBUST LOCATION (Huber case)
C
      DIMENSION Y(N),RS(N),SC(N),TETA(1),DLTA(1)
      LOGICAL NPRCHK
      EXTERNAL EXPSI,EXCHI,EXRHO
      COMMON/CONST/CONST
      COMMON/BETA/BETA,BET0
      DATA TL/1.E-8/
C     SAVE /CONST/
C
C   PARAMETER CHECK AND INITIALIZATION
C
      SIGMA=SIGMAI
      SIGMB=SIGMA
      IASG=IABS(ISIGMA)
      NPRCHK=GAM.GT.0..AND.GAM.LT.2..AND.MAXIT.GT.0.AND.(MAXIS.GT.0
     1       .OR.IASG.NE.1).AND.SIGMA.GT.0..AND.TOL.GT.0..AND.
     1       (ISIGMA.GE.-2.AND.ISIGMA.LE.2)
      IF (.NOT.NPRCHK) CALL MESSGE(500,'LYWALG',1)
      ITYP=1
      NP=1
      DELTA=1.0
      IF (IASG.EQ.0) CONST=0.
      IF (IASG.EQ.1) CONST=BETA*FLOAT(N-1)
      IF (IASG.EQ.2) CONST=BET0*FLOAT(N-1)
C
C   STEP 1. SET NIT := 1
C   -------
      NIT=1
C
C   STEP 2. COMPUTE RESIDUALS R=Y-X*THETA
C   -------
  200 DO 210 I=1,N
      RS(I)=Y(I)-THETA
  210 CONTINUE 
C
C   STEP 3. COMPUTE A NEW VALUE SIGMB FOR SIGMA.
C   -------
      IF (ISIGMA.LT.0.AND.NIT.EQ.1) GOTO 400
      IF (ISIGMA.EQ.0) GOTO 400
      SIGMA=SIGMB
      CALL RYSIGM(RS,SC,EXCHI,SIGMA,N,1,TOL,ITYP,ISIGMA,MAXIS,
     1            NIS,SIGMB,SC,SC)
      IF (SIGMB.LE.TL) CALL MESSGE(460,'LYWALG',0)
      IF (SIGMB.LE.TL) RETURN
C
C  ITERATION MONITORING
C
      IF (NITMON.LE.0) GOTO 400
      IF (MOD(NIT,NITMON).NE.0) GOTO 400
      CALL QRSS(RS,SC,SC,EXRHO,N,ITYP,SIGMB,CONST,QS)
      TETA(1)=THETA
      DLTA(1)=DELTA
      CALL MONITR(NIT,NP,GAM,QS/FLOAT(N),SIGMB,TETA,DLTA)
C
C   STEP 4. COMPUTE WEIGHTS AND APPLY THEM TO X; STORE RESULT IN SX
C   -------
  400 SUM1=0.
      SUM2=0.
      DO 430 I=1,N
        SC(I)=PSP0
        IF (RS(I).EQ.0.) GOTO 410
        T=RS(I)/SIGMB
        SC(I)=EXPSI(T)/T
  410   S1=SC(I)
        S2=RS(I)*SC(I)                               
        SUM1=SUM1+S1 
        SUM2=SUM2+S2 
  430 CONTINUE
C
C   STEP 5. SOLVE FOR DELTA
C   -------
      IF (ABS(SUM1).LT.1E-6) THEN
        CALL MESSGE(450,'LYWALG',0)
        SUM1=SIGN(1E-6,SUM1)
      ENDIF
      DELTA=GAM*SUM2/SUM1
C
C   STEP 6. COMPUTE NEW SOLUTION
C   -------
      THETA=THETA+DELTA
C
C   STEP 7. STOP ITERATIONS IF DESIRED PRECISION IS REACHED
C   -------
      IF (NIT.EQ.MAXIT) GOTO 800
      IF (ISIGMA.LT.0.AND.NIT.EQ.1) GOTO 700
      IF(ABS(DELTA).LT.TOL*AMAX1(1.,ABS(THETA)).AND.
     +   ABS(SIGMA-SIGMB).LT.TOL*AMAX1(1.,ABS(SIGMB))) GOTO 800
  700 NIT=NIT+1
      GOTO 200
  800 SIGMAF=SIGMB
      DO 900 I=1,N
        RS(I)=Y(I)-THETA
  900 CONTINUE
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE UGLTST(IUGL,ICASE,B,N,NI,Y,VTHETA,OI,CI,DIST,SU)
C.......................................................................
C TEST
C.......................................................................
C
      DIMENSION Y(N),CI(N),VTHETA(N),OI(N),DIST(N),UARR(4)
      DOUBLE PRECISION SU(N),U,UGL
      INTEGER NI(N)
      EXTERNAL UGL
      COMMON/UGLPR/IUG,ICS,BB

C
      IUG=IUGL
      ICS=ICASE
      BB=B
      YL=1.
      NL=1
      DO 100 L=1,N
      DISTL=DIST(L)
      GL=VTHETA(L)+OI(L)
      CL=CI(L)
      IF (IUGL.EQ.2) YL=Y(L)
      IF (ICASE.EQ.2) NL=NI(L)
      UARR(1)=YL
      UARR(2)=FLOAT(NL)
      UARR(3)=GL
      UARR(4)=CL
      U=UGL(UARR,4,DISTL)
      SU(L)=U
  100 CONTINUE
      RETURN
      END

