      SUBROUTINE REGTAU(X,Y,N,NN,B1,C1,B2,C2,TOL,ISEED,AO,BO,TO,
     +           RS,SA,SB,TA,TMP1,TMP2)
C.......................................................................
C
C   COPYRIGHT 2009 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION X(N),Y(N),RS(N),SA(NN),SB(NN),TA(NN),TAI
      DOUBLE PRECISION AO,BO,TO,SRJ,SX(2),SY(2),SUMXY,SUMXX,SUMX,SUMY
      REAL TMP1(N),TMP2(N)
      INTEGER IND(2)
      LOGICAL NPRCHK
      EXTERNAL RHO
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NPRCHK=N.GT.0.AND.TOL.GT.0.AND.ISEED.NE.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'REGTAU',1)
      N1=N/2
      IPS0=IPSI
      XK0=XK
      IPSI=4
      IO=N
      TAI=1.D6
      KSEED=ISEED
      IND(1)=1  ! added by A.R. 28.02.2020 
      IND(2)=2  ! added by A.R. 28.02.2020
      IND1=1    ! added by A.R. 28.02.2020
      DO 500 I=1,NN
c       call intpr('i',1,i,1)
        DO 30 K=1,2
   10     CALL RANDOW(KSEED,RND)
          IK=INT(RND*FLOAT(N))+1
          IF (IK.GT.N) IK=N
          IF (K.EQ.1) THEN
            IND(1)=IK
            IND1=IK
            GOTO 30
          ENDIF
          IF (IK.EQ.IND1) GOTO 10
          IF (DABS(X(IND1)-X(IK)).LE.1.D-5) GOTO 10
          IND(2)=IK
   30   CONTINUE
      DO 100 K=1,2
      IK=IND(K)
      SX(K)=X(IK)
      SY(K)=Y(IK)
  100 CONTINUE 
      SB(I)=(SY(2)-SY(1))/(SX(2)-SX(1))
      SA(I)=SY(1)-SB(I)*SX(1)
      DO 120 J=1,N
      RS(J)=Y(J)-SB(I)*X(J)-SA(I)
      SRJ=DABS(RS(J))
      TMP1(J)=SNGL(SRJ)
      TMP2(J)=FLOAT(J)
  120 CONTINUE
      CALL SRT2Z(TMP1,TMP2,N,1,N)
      SUMXX=0.D0
      SUMXY=0.D0
      SUMX=0.D0
      SUMY=0.D0
      DO 140 J=1,N1
      K=INT(TMP2(J))
      SUMXX=SUMXX+X(K)*X(K)
      SUMXY=SUMXY+X(K)*Y(K)
      SUMX=SUMX+X(K)
      SUMY=SUMY+Y(K)
  140 CONTINUE 
      SB(I)=(SUMXY-SUMX*SUMY/DBLE(N1))/(SUMXX-SUMX*SUMX/DBLE(N1))
      SA(I)=(SUMY-SB(I)*SUMX)/DBLE(N1)
      DO 160 J=1,N
      RS(J)=Y(J)-SB(I)*X(J)-SA(I)
      SRJ=DABS(RS(J))
      TMP1(J)=SNGL(SRJ)
  160 CONTINUE
      CALL SRT1Z(TMP1,N,1,N)
      S0=TMP1(N1+1)
      IF (2*N1.EQ.N) S0=(TMP1(N1)+S0)/2.0
      S0=S0/0.6745
      TAU=TOL
      IF (S0.LE.TOL) GOTO 300
      IT=0
      H=1.0
      XK=C1
c      call realpr('s0',2,s0,1)
  180 IT=IT+1
      SUM=0.0
C     RHO(S)=RHO(-S) !!
      DO 200 J=1,N
      U=TMP1(J)
      SUM=SUM+RHO(U/S0)
  200 CONTINUE 
      S1=SUM/(FLOAT(N)*B1)
      S1=S0*SQRT(S1)
      H=ABS(S1-S0)/S0
      S0=S1
      IF (H.GT.TOL.AND.IT.LT.50) GOTO 180
c      call realpr('h',1,h,1)
c      call intpr('it',2,it,1)
  300 CONTINUE
      IF (S0.LE.TOL) GOTO 400
      SUM=0.0
      XK=C2
      DO 320 J=1,N
      U=TMP1(J)
      SUM=SUM+RHO(U/S0)
  320 CONTINUE 
      S1=SUM/(FLOAT(N)*B2)
      TAU=S0*SQRT(S1)      
  400 TA(I)=DBLE(TAU)
      IF (TA(I).GE.TAI) GOTO 500
      IO=I
      TAI=TA(I)
  500 CONTINUE
      IPSI=IPS0
      XK=XK0
      AO=SA(IO)
      BO=SB(IO)
      TO=TA(IO)
      RETURN
      END
C
      SUBROUTINE REGTAUW(X,Y,W,N,NN,B1,C1,B2,C2,TOL,ISEED,AO,BO,TO,
     +           RS,SA,SB,TA,TMP1,TMP2)
C.......................................................................
C
C   COPYRIGHT 2009 Alfio Marazzi
C
C   AUTHORS : A. MARAZZI / A. RANDRIAMIHARISOA
C.......................................................................
C
      DOUBLE PRECISION X(N),Y(N),W(N),RS(N),SA(NN),SB(NN),TA(NN),TAI
      DOUBLE PRECISION AO,BO,TO,SRJ,SX(2),SY(2),SUMXY,SUMXX,SUMX,SUMY
      REAL TMP1(N),TMP2(N)
      INTEGER IND(2)
      LOGICAL NPRCHK
      EXTERNAL RHO
      COMMON/PSIPR/IPSI,C,H1,H2,H3,XK,D
C
C  PARAMETER CHECK AND INITIALIZATION
C
      NPRCHK=N.GT.0.AND.TOL.GT.0.AND.ISEED.NE.0
      IF (.NOT.NPRCHK) CALL MESSGE(500,'REGTAU',1)
      N1=N/2
      IPS0=IPSI
      XK0=XK
      IPSI=4
      IO=N
      TAI=1.D6
      KSEED=ISEED
      IND(1)=1  ! added by A.R. 28.02.2020 
      IND(2)=2  ! added by A.R. 28.02.2020
      IND1=1    ! added by A.R. 28.02.2020
      DO 500 I=1,NN
c       call intpr('i',1,i,1)
        DO 30 K=1,2
   10     CALL RANDOW(KSEED,RND)
          IK=INT(RND*FLOAT(N))+1
          IF (IK.GT.N) IK=N
          IF (K.EQ.1) THEN
            IND(1)=IK
            IND1=IK
            GOTO 30
          ENDIF
          IF (IK.EQ.IND1) GOTO 10
          IF (DABS(X(IND1)-X(IK)).LE.1.D-5) GOTO 10
          IND(2)=IK
   30   CONTINUE
      DO 100 K=1,2
      IK=IND(K)
      SX(K)=X(IK)
      SY(K)=Y(IK)
  100 CONTINUE 
      SB(I)=(SY(2)-SY(1))/(SX(2)-SX(1))
      SA(I)=SY(1)-SB(I)*SX(1)
      DO 120 J=1,N
      RS(J)=Y(J)-SB(I)*X(J)-SA(I)
      SRJ=DABS(RS(J))
      TMP1(J)=SNGL(SRJ)
      TMP2(J)=FLOAT(J)
  120 CONTINUE
      CALL SRT2Z(TMP1,TMP2,N,1,N)
      SUMXX=0.D0
      SUMXY=0.D0
      SUMX=0.D0
      SUMY=0.D0
      DO 140 J=1,N1
      K=INT(TMP2(J))
      SUMXX=SUMXX+X(K)*X(K)
      SUMXY=SUMXY+X(K)*Y(K)
      SUMX=SUMX+X(K)
      SUMY=SUMY+Y(K)
  140 CONTINUE 
      SB(I)=(SUMXY-SUMX*SUMY/DBLE(N1))/(SUMXX-SUMX*SUMX/DBLE(N1))
      SA(I)=(SUMY-SB(I)*SUMX)/DBLE(N1)
      DO 160 J=1,N
      RS(J)=Y(J)-SB(I)*X(J)-SA(I)
      SRJ=DABS(RS(J)*W(J))
      TMP1(J)=SNGL(SRJ)
  160 CONTINUE
      CALL SRT1Z(TMP1,N,1,N)
      S0=TMP1(N1+1)
      IF (2*N1.EQ.N) S0=(TMP1(N1)+S0)/2.0
      S0=S0/0.6745
      TAU=TOL
      IF (S0.LE.TOL) GOTO 300
      IT=0
      H=1.0
      XK=C1
c      call realpr('s0',2,s0,1)
  180 IT=IT+1
      SUM=0.0
C     RHO(S)=RHO(-S) !!
      DO 200 J=1,N
      U=TMP1(J)
      SUM=SUM+RHO(U/S0)
  200 CONTINUE 
      S1=SUM/(FLOAT(N)*B1)
      S1=S0*SQRT(S1)
      H=ABS(S1-S0)/S0
      S0=S1
      IF (H.GT.TOL.AND.IT.LT.50) GOTO 180
c      call realpr('h',1,h,1)
c      call intpr('it',2,it,1)
  300 CONTINUE
      IF (S0.LE.TOL) GOTO 400
      SUM=0.0
      XK=C2
      DO 320 J=1,N
      U=TMP1(J)
      SUM=SUM+RHO(U/S0)
  320 CONTINUE 
      S1=SUM/(FLOAT(N)*B2)
      TAU=S0*SQRT(S1)      
  400 TA(I)=DBLE(TAU)
      IF (TA(I).GE.TAI) GOTO 500
      IO=I
      TAI=TA(I)
  500 CONTINUE
      IPSI=IPS0
      XK=XK0
      AO=SA(IO)
      BO=SB(IO)
      TO=TA(IO)
      RETURN
      END

