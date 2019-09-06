c ************************************************
C **  NUMERICAL SIMULATION of AIR and PARTICLE  **
C **  MOTIONS in BUBBLING FLUIDIZED BED. by TU. **
C ************************************************
C
      PROGRAM Bubbling Fluidized Bed
C
C *** SET DIMENSIONS ***
C
      DIMENSION  U(1290,74),V(1290,74),VOID(1290,74)
C     &          ,VORZ(1290,74),
     &          ,P(1290,74)
     &          ,UB(1290,74),VB(1290,74),VOIDB(1290,74)
     &          ,R(1290,74),RRM(1290,74)
     &          ,FU(1290,74),FV(1290,74)
     &          ,VISX(1290,74),VISY(1290,74)
     &          ,SPX(1290,74),SPY(1290,74),VOLP(1290,74)
     &          ,SPFLX(1290,74),SPFLY(1290,74)
      DIMENSION  UP(300000),VP(300000),XP(300000),YP(300000)
      INTEGER    CYCLE,TNO,TNOK
C     &          ,BCYCLE
      INTEGER    PARTTL(1281,65),PART(1281,65,32)
C
C *** DIMENSION of PARTICLE (DEM) ***
C
      include 'demA.h'
C
C *** MESH NUMBER & SIZE ***
C
      IBAR = 1280
      JBAR =  64
      DP   = 1.0/258.0
      DELX = 1.0/64.0
      DELY = 1.0/64.0
      DELZ = DP
      NTIME = 1
C
C *** MINIMUM VOID FRACTION ***
C
      VOID00 = 0.4764
      VOIDMIN = 0.2595
C
C *** SUPERFICIAL AIR VELOCITY (=SU0), BED ***
C *** WIDTH (=SL0) and TIME STEP (=DELT)   ***
C
      SU0  = 21.0
      SL0  =  8.0625
      DELT = 0.25e-5*SU0/SL0
C
C *** INITIAL CYCLE of CALCULATION ***
C
      CYCLE = 217001
C
C *** TERMINATION TIME of CALCULATION (=DELT*CYCLE) ***
C
      TWFIN = DELT*4003.0
C 
C *** SW01 = 0.0 ----- DO NOT READ AIR DATAFILE ***
C *** OTHERS     ----- READ READ AIR DATAFILE   ***
C
      SW01 = 1.0
C
C *** CYCLE of DATA OUTPUT ***
C
      NCYCLE00 = cycle+ 1000
      NCYCLE01 = cycle+ 2000
      NCYCLE02 = cycle+ 3000
      NCYCLE03 = cycle+ 4000
      NCYCLE04 = cycle+ 5000
      NCYCLE05 = cycle+ 6000
      NCYCLE06 = cycle+ 7000
      NCYCLE07 = cycle+ 8000
      NCYCLE08 = cycle+ 9000
      NCYCLE09 = cycle+10000
C
      NENDLESS = 10000
C
C *** EPSIRON ***
C
      EPSI = 250.0
C
C *** DENSITY(=arho), VISCOSITY(=amyu)  and   ***
C *** KINETIC VISCOSITY(=anyu) (C.G.S.SYSTEM) ***
C
C      arho = 1.21/1000.0
       amyu = 1.81/10000.0
C      anyu = amyu/arho
C
C *** OTHER PARAMETERS ***
C
      RE = SL0*SU0/0.15
      UI = 0.0
      VI = 0.0
      OMG= 1.0
      NP = 300000
      DPARTX = 1
      DPARTY = 1
      ITER = 0
      T = DELT
      PI = 4.0*ATAN(1.0)
      VOLPP = (PI*DP**3)/6.0
      VOLC = DELX*DELY*DELZ
      VOID01 = 0.385
C
C *** DEFINITION of MESH NUMBER ***
C
      IMAX = IBAR+10
      JMAX = JBAR+10
      IM1 = IMAX-1
      JM1 = JMAX-1
      IM2 = IMAX-2
      JM2 = JMAX-2
      IM3 = IMAX-3
      JM3 = JMAX-3
      IM4 = IMAX-4
      JM4 = JMAX-4
      IM5 = IMAX-5
      JM5 = JMAX-5
C     IM6 = IMAX-6
      JM6 = JMAX-6
C     IM7 = IMAX-7
      JM7 = JMAX-7
C     IM8 = IMAX-8
      JM8 = JMAX-8
C     IM9 = IMAX-9
      JM9 = JMAX-9
      RDX = 1.0/DELX
      RDY = 1.0/DELY
C     RDZ = 1.0/DELZ
      NPARTX = IBAR/DPARTX
      NPARTY = JBAR/DPARTY
C
C *** CLEAR ALL DATA ***
C
      DO 560 J=1,JMAX
      DO 560 I=1,IMAX
        U(I,J) = UI
        V(I,J) = VI
        UB(I,J) = UI
        VB(I,J) = VI
        SPX(I,J) = 0.0
        SPY(I,J) = 0.0
        VOLP(I,J) = 0.0
        VOID(I,J) = 1.0
        VOIDB(I,J) = 1.0
        SPFLX(I,J) = 0.0
        SPFLY(I,J) = 0.0
        P(I,J) = 0.0
C       VORZ(I,J) = 0.0
  560 CONTINUE
C
C *** READ PARTICLE DATAFILE ("infile.dat") ***
C
C        CALL PRE
C
      dt = DELT*SL0/SU0
C
C *** READ "airinfile.dat" or NOT ***
C
      IF(SW01.NE.0) GO TO 750
C
C *** CALCULATION of VOID FRACTION ***
C
      DO 7141 N=1,ipq
        XP(N)=y(N)/SL0
        YP(N)=x(N)/SL0
        UP(N)=vy(N)/SU0
        VP(N)=vx(N)/SU0
 7141 CONTINUE
C
      DO 7140 IP=1,NPARTX+1
      DO 7140 JP=1,NPARTY+1
        PARTTL(IP,JP)=0
        DO 7140 NPP=1,32
        PART(IP,JP,NPP)=0
 7140 CONTINUE
C
      DO 7103 N=1,NP
        XPP=XP(N)
        YPP=YP(N)
        II=INT(XPP/(DELX*DPARTX))+1
        JJ=INT(YPP/(DELY*DPARTY))+1
        TNO=PARTTL(II,JJ)+1
        IF(TNO.GT.32) GO TO 7103
        PARTTL(II,JJ)=TNO
        PART(II,JJ,TNO)=N 
 7103 CONTINUE
C
      DO 7125 N=1,NP
        X1=XP(N)-DELX*INT(XP(N)/DELX)
        X2=DELX*(INT(XP(N)/DELX)+1)-XP(N)
        Y1=YP(N)-DELY*INT(YP(N)/DELY)
        Y2=DELY*(INT(YP(N)/DELY)+1)-YP(N)
        IX=INT(XP(N)/DELX)+6
        JY=INT(YP(N)/DELY)+6
        XX1=DP/2-X1
        XX2=DP/2-X2
        YY1=DP/2-Y1
        YY2=DP/2-Y2
        XX1=MAX(XX1,0.0)
        XX2=MAX(XX2,0.0)
        YY1=MAX(YY1,0.0)
        YY2=MAX(YY2,0.0)
        VOLPX1=PI*XX1**2*(DP+X1)/3.0
        VOLPX2=PI*XX2**2*(DP+X2)/3.0
        VOLPY1=PI*YY1**2*(DP+Y1)/3.0
        VOLPY2=PI*YY2**2*(DP+Y2)/3.0
        VOLP(IX-1,JY)=VOLP(IX-1,JY)+VOLPX1
        VOLP(IX,JY-1)=VOLP(IX,JY-1)+VOLPY1
        VOLP(IX,JY)=VOLP(IX,JY)+VOLPP-(VOLPX1+VOLPY1+VOLPX2+VOLPY2)
        VOLP(IX+1,JY)=VOLP(IX+1,JY)+VOLPX2
        VOLP(IX,JY+1)=VOLP(IX,JY+1)+VOLPY2
 7125 CONTINUE
C
      DO 7130 J=6,JM5
      DO 7130 I=6,IM5
        VOIDD=(VOLC-VOLP(I,J))/VOLC 
        IF(VOIDD.GE.VOID00) GOTO 7149
          VOIDD=1.0-(1.0-VOIDD)*((1.0-VOIDD)/(1.0-VOID00))**0.77
          IF(VOIDD.LT.VOIDMIN) VOIDD=VOIDMIN
 7149   VOID(I,J)=VOIDD
 7130 CONTINUE
C
C  ** BOTTOM **
      DO 7131 J=1,JMAX
        VOID(5,J)=VOID(6,J)
        VOID(4,J)=VOID(7,J)
        VOID(3,J)=VOID(8,J)
        VOID(2,J)=VOID(9,J)
        VOID(1,J)=VOID(10,J)
 7131 CONTINUE
C
C  ** LEFT **
      DO 7132 I=1,IMAX
        VOID(I,1)=VOID(I,10)
        VOID(I,2)=VOID(I,9)
        VOID(I,3)=VOID(I,8)
        VOID(I,4)=VOID(I,7)
        VOID(I,5)=VOID(I,6)
 7132 CONTINUE
C
C  ** RIGHT **
      DO 7133 I=1,IMAX
        VOID(I,JMAX)=VOID(I,JM9)
        VOID(I,JM1)=VOID(I,JM8)
        VOID(I,JM2)=VOID(I,JM7)
        VOID(I,JM3)=VOID(I,JM6)
        VOID(I,JM4)=VOID(I,JM5)
 7133 CONTINUE
C
C  ** UPPER **
      DO 7134 J=1,JMAX
        VOID(IMAX,J)=VOID(IM5,J)
        VOID(IM1,J)=VOID(IM5,J)
        VOID(IM2,J)=VOID(IM5,J)
        VOID(IM3,J)=VOID(IM5,J)
        VOID(IM4,J)=VOID(IM5,J)
 7134 CONTINUE
C
      DO 7136 J=1,JMAX
      DO 7136 I=1,IMAX
        VOIDB(I,J)=VOID(I,J)
 7136 CONTINUE
C
C *** SET BED BOTTTOM AIR VELOCITY ***
C
      DO 573 J=22,52,15
        U(5,J)=(32.0/3.0)/((VOID(5,J)+VOID(6,J))/2.0)
        UB(5,J)=(32.0/3.0)/((VOIDB(5,J)+VOIDB(6,J))/2.0)
        U(4,J)=(32.0/3.0)/((VOID(4,J)+VOID(5,J))/2.0)
        UB(4,J)=(32.0/3.0)/((VOIDB(4,J)+VOIDB(5,J))/2.0)
        U(3,J)=(32.0/3.0)/((VOID(3,J)+VOID(4,J))/2.0)
        UB(3,J)=(32.0/3.0)/((VOIDB(3,J)+VOIDB(4,J))/2.0)
        U(2,J)=(32.0/3.0)/((VOID(2,J)+VOID(3,J))/2.0)
        UB(2,J)=(32.0/3.0)/((VOIDB(2,J)+VOIDB(3,J))/2.0)
        U(1,J)=(32.0/3.0)/((VOID(1,J)+VOID(2,J))/2.0)
        UB(1,J)=(32.0/3.0)/((VOIDB(1,J)+VOIDB(2,J))/2.0)
  573 CONTINUE
      DO 574 J=23,53,15
        U(5,J)=(32.0/3.0)/((VOID(5,J)+VOID(6,J))/2.0)
        UB(5,J)=(32.0/3.0)/((VOIDB(5,J)+VOIDB(6,J))/2.0)
        U(4,J)=(32.0/3.0)/((VOID(4,J)+VOID(5,J))/2.0)
        UB(4,J)=(32.0/3.0)/((VOIDB(4,J)+VOIDB(5,J))/2.0)
        U(3,J)=(32.0/3.0)/((VOID(3,J)+VOID(4,J))/2.0)
        UB(3,J)=(32.0/3.0)/((VOIDB(3,J)+VOIDB(4,J))/2.0)
        U(2,J)=(32.0/3.0)/((VOID(2,J)+VOID(3,J))/2.0)
        UB(2,J)=(32.0/3.0)/((VOIDB(2,J)+VOIDB(3,J))/2.0)
        U(1,J)=(32.0/3.0)/((VOID(1,J)+VOID(2,J))/2.0)
        UB(1,J)=(32.0/3.0)/((VOIDB(1,J)+VOIDB(2,J))/2.0)
 574  CONTINUE
C
C *********************************************
C
      GO TO 920
C
  750 CONTINUE
C
C *** READ AIR DATAFILE ("airinfile.dat") ***
C 
      open( unit = 1, err = 9906, status='old',
     &file = 'airinfile.dat' )
      READ(1,*) I
      DO 900 I=1,IMAX
      DO 900 J=1,JMAX
        READ(1,*) U(I,J),V(I,J),VOID(I,J),P(I,J)
        READ(1,*) UB(I,J),VB(I,J),VOIDB(I,J)
  900 CONTINUE
      DO 905 I=1,IMAX
      DO 905 J=1,JMAX
        READ(1,*) SPX(I,J),SPY(I,J)
        READ(1,*) SPFLX(I,J),SPFLY(I,J)
  905 CONTINUE
      close( 1 )
      goto 9907
 9906 write(9,*) 'open error airinfile.dat (unit=1)'
 9907 continue
C
C *********************
C
  920 CONTINUE
C
      PE  = 1000000.0
      PNU = 0.25
C
C *********************
C **** START CYCLE ****
C *********************
C
 1000 CONTINUE
C
  930 CONTINUE
  755 CONTINUE
C
      ITER=0
      FLG=1.0
      ASSIGN 3200 TO KRET
C
C *** CALCULATION of CONVECTION TERM & VISCOUS TERM ***
C
      DO 3300 J=5,JM4
      DO 3300 I=5,IM4
C
C *** CONVECTION TERMS ***
C
      VU1=(V(I,J+2)+V(I+1,J+2)+V(I,J+1)+V(I+1,J+1))/4.0
      VU2=(V(I,J+1)+V(I+1,J+1)+V(I,J)+V(I+1,J))/4.0
      VU3=(V(I,J)+V(I+1,J)+V(I,J-1)+V(I+1,J-1))/4.0
      VU4=(V(I,J-1)+V(I+1,J-1)+V(I,J-2)+V(I+1,J-2))/4.0
      VU5=(V(I,J-2)+V(I+1,J-2)+V(I,J-3)+V(I+1,J-3))/4.0
      UV1=(U(I+2,J)+U(I+2,J+1)+U(I+1,J)+U(I+1,J+1))/4.0
      UV2=(U(I+1,J)+U(I+1,J+1)+U(I,J)+U(I,J+1))/4.0
      UV3=(U(I,J)+U(I,J+1)+U(I-1,J)+U(I-1,J+1))/4.0
      UV4=(U(I-1,J)+U(I-1,J+1)+U(I-2,J)+U(I-2,J+1))/4.0
      UV5=(U(I-2,J)+U(I-2,J+1)+U(I-3,J)+U(I-3,J+1))/4.0
C
C  ** 4TH ORDER CENTRAL DIFFERENTIATION **
      FUX1=U(I,J)*(-U(I+2,J)+8.0*U(I+1,J)-8.0*U(I-1,J)+U(I-2,J))
     &/(12.0*DELX)
      FUY1=VU3*(-U(I,J+2)+8.0*U(I,J+1)-8.0*U(I,J-1)+U(I,J-2))
     &/(12.0*DELY)
      FVX1=UV3*(-V(I+2,J)+8.0*V(I+1,J)-8.0*V(I-1,J)+V(I-2,J))
     &/(12.0*DELX)
      FVY1=V(I,J)*(-V(I,J+2)+8.0*V(I,J+1)-8.0*V(I,J-1)+V(I,J-2))
     &/(12.0*DELY)
C
      UU1=U(I+2,J)*(VOID(I+3,J)+VOID(I+2,J))/2.0
      UU2=U(I+1,J)*(VOID(I+2,J)+VOID(I+1,J))/2.0
      UU4=U(I-1,J)*(VOID(I,J)+VOID(I-1,J))/2.0
      UU5=U(I-2,J)*(VOID(I-1,J)+VOID(I-2,J))/2.0
      UU6=U(I,J+2)*(VOID(I,J+2)+VOID(I+1,J+2))/2.0
      UU7=U(I,J+1)*(VOID(I,J+1)+VOID(I+1,J+1))/2.0
      UU8=U(I,J-1)*(VOID(I,J-1)+VOID(I+1,J-1))/2.0
      UU9=U(I,J-2)*(VOID(I,J-2)+VOID(I+1,J-2))/2.0
      VV1=V(I,J+2)*(VOID(I,J+3)+VOID(I,J+2))/2.0
      VV2=V(I,J+1)*(VOID(I,J+2)+VOID(I,J+1))/2.0
      VV4=V(I,J-1)*(VOID(I,J)+VOID(I,J-1))/2.0
      VV5=V(I,J-2)*(VOID(I,J-1)+VOID(I,J-2))/2.0
      VV6=V(I+2,J)*(VOID(I+2,J)+VOID(I+2,J+1))/2.0
      VV7=V(I+1,J)*(VOID(I+1,J)+VOID(I+1,J+1))/2.0
      VV8=V(I-1,J)*(VOID(I-1,J)+VOID(I-1,J+1))/2.0
      VV9=V(I-2,J)*(VOID(I-2,J)+VOID(I-2,J+1))/2.0
      VU1=VU1*(VOID(I,J+2)+VOID(I+1,J+2))/2.0
      VU2=VU2*(VOID(I,J+1)+VOID(I+1,J+1))/2.0
      VU4=VU4*(VOID(I,J-1)+VOID(I+1,J-1))/2.0
      VU5=VU5*(VOID(I,J-2)+VOID(I+1,J-2))/2.0
      UV1=UV1*(VOID(I+2,J)+VOID(I+2,J+1))/2.0
      UV2=UV2*(VOID(I+1,J)+VOID(I+1,J+1))/2.0
      UV4=UV4*(VOID(I-1,J)+VOID(I-1,J+1))/2.0
      UV5=UV5*(VOID(I-2,J)+VOID(I-2,J+1))/2.0
C
C  ** 4TH ORDER CENTRAL DIFFERENTIATION **
      FUX2=(-UU1**2+8.0*UU2**2-8.0*UU4**2+UU5**2)
     &/(12.0*DELX)
      FUY2=(-VU1*UU6+8.0*VU2*UU7-8.0*VU4*UU8+VU5*UU9)
     &/(12.0*DELY)
      FVX2=(-UV1*VV6+8.0*UV2*VV7-8.0*UV4*VV8+UV5*VV9)
     &/(12.0*DELX)
      FVY2=(-VV1**2+8.0*VV2**2-8.0*VV4**2+VV5**2)
     &/(12.0*DELY)
C
      VOIDX=(VOID(I+1,J)+VOID(I,J))/2.0
      VOIDY=(VOID(I,J+1)+VOID(I,J))/2.0
      FU(I,J)=(VOIDX*(FUX1+FUY1)+FUX2+FUY2)/2.0
      FV(I,J)=(VOIDY*(FVX1+FVY1)+FVX2+FVY2)/2.0
C
C *** VISCOUS TERMS ***
C
      VU2=(V(I,J+1)+V(I+1,J+1)+V(I,J)+V(I+1,J))/4.0
      VU4=(V(I,J-1)+V(I+1,J-1)+V(I,J-2)+V(I+1,J-2))/4.0
      UV2=(U(I+1,J)+U(I+1,J+1)+U(I,J)+U(I,J+1))/4.0
      UV4=(U(I-1,J)+U(I-1,J+1)+U(I-2,J)+U(I-2,J+1))/4.0
C
      VISX1=((U(I+1,J)-2.0*U(I,J)+U(I-1,J))/DELX**2
     &+(U(I,J+1)-2.0*U(I,J)+U(I,J-1))/DELY**2)
     &*(VOID(I+1,J)+VOID(I,J))/2.0
      VISY1=((V(I+1,J)-2.0*V(I,J)+V(I-1,J))/DELX**2
     &+(V(I,J+1)-2.0*V(I,J)+V(I,J-1))/DELY**2)
     &*(VOID(I,J+1)+VOID(I,J))/2.0
      VISX2=((U(I+1,J)-2.0*U(I,J)+U(I-1,J))/(DELX**2)
     &+(V(I+1,J)-V(I+1,J-1)-V(I,J)+V(I,J-1))/(DELY*DELX))
     &*(VOID(I+1,J)+VOID(I,J))/2.0
      VISY2=((U(I,J+1)-U(I-1,J+1)-U(I,J)+U(I-1,J))/(DELX*DELY)
     &+(V(I,J+1)-2.0*V(I,J)+V(I,J-1))/(DELY**2))
     &*(VOID(I,J+1)+VOID(I,J))/2.0
      VISX3X=((VOID(I+1,J)-VOID(I,J))/(3.0*DELX))*(2.0*(U(I+1,J)
     &-U(I-1,J))/DELX-((VU2-VU4)/DELY))
      VISX3Y=(((VOID(I,J+1)+VOID(I+1,J+1))-(VOID(I,J-1)
     &+VOID(I+1,J-1)))/(4.0*DELY))*((U(I,J+1)-U(I,J-1))/(2.0*DELY)
     &+((V(I+1,J-1)+V(I+1,J))-(V(I,J-1)+V(I,J)))/(2.0*DELX))
      VISY3X=(((VOID(I+1,J)+VOID(I+1,J+1))-(VOID(I-1,J)
     &+VOID(I-1,J+1)))/(4.0*DELX))*((V(I+1,J)-V(I-1,J))/(2.0*DELX)
     &+((U(I-1,J+1)+U(I,J+1))-(U(I-1,J)+U(I,J)))/(2.0*DELY))
      VISY3Y=((VOID(I,J+1)-VOID(I,J))/(3.0*DELY))*(2.0*(V(I,J+1)
     &-V(I,J-1))/DELY-((UV2-UV4)/DELX))
C
      VISX(I,J)=(VISX1+VISX2/3+VISX3X+VISX3Y)/RE
      VISY(I,J)=(VISY1+VISY2/3+VISY3X+VISY3Y)/RE
C
 3300 CONTINUE
C
C ******************************************************
C
 2000 CONTINUE
C
C  ** LOWER **
      DO 2100 J=5,JM4
      I=5
      P(I,J)=(P(I+1,J)*VOID(I+1,J)
     &+DELX*(FU(I,J)
     &-VISX(I,J)
     &+SPX(I,J)
     &))/VOID(I,J)
C        P(I,J)=P(I+1,J)
C     &         +DELX*(FU(I,J)
C     &         -VISX(I,J)
C     &         +SPX(I,J)
C     &         +SPFLX(I,J)
C     &         )/(VOID(I,J)+VOID(I+1,J))*2.0
 2100 CONTINUE
C
C  ** LEFT **
      DO 2300 I=5,IM4
      J=5
      P(I,J)=(P(I,J+1)*VOID(I,J+1)
     &+DELY*(FV(I,J)
     &-VISY(I,J)
     &+SPY(I,J)
     &))/VOID(I,J)
C        P(I,J)=P(I,J+1)
C     &         +DELY*(FV(I,J)
C     &         -VISY(I,J)
C     &         +SPY(I,J)
C     &         +SPFLY(I,J)
C     &         )/(VOID(I,J)+VOID(I,J+1))*2.0
 2300 CONTINUE
C
C  ** RIGHT **
      DO 2400 I=5,IM4
      J=JM4
      P(I,J)=(P(I,J-1)*VOID(I,J-1)
     &-DELY*(FV(I,J-1)
     &-VISY(I,J-1)
     &+SPY(I,J-1)
     &))/VOID(I,J)
C        P(I,J)=P(I,J-1)
C     &         -DELY*(FV(I,J-1)
C     &         -VISY(I,J-1)
C     &         +SPY(I,J-1)
C     &         +SPFLY(I,J-1)
C     &         )/(VOID(I,J)+VOID(I,J-1))*2.0
 2400 CONTINUE
C
C  ** UPPER **
      DO 2700 J=5,JM4
      P(IM4,J)=P(IM5,J)*VOID(IM5,J)/VOID(IM4,J)
C        P(IM4,J)=P(IM5,J)*VOID(IM5,J)/VOID(IM4,J)
 2700 CONTINUE
C
      IF (CYCLE.GT.2) THEN
      PMAX = 0.0
      DO 2755 J=5,JM4
      DO 2755 I=5,IM4
        IF(ABS(P(I,J)).LE.PMAX) GOTO 2755
        PMAX = ABS(P(I,J))
 2755 CONTINUE
      EPSI=PMAX/1000.0
      ENDIF
C
C *****************************
C
 2800 CONTINUE
C
C  ** LOWER **
      DO 2105 J=5,JM4
      I=5
      P(I,J)=(P(I+1,J)*VOID(I+1,J)
     &       +DELX*(FU(I,J)
     &       -VISX(I,J)
     &       +SPX(I,J)
     &       ))/VOID(I,J)
C      P(I,J)=P(I+1,J)
C    &         +DELX*(FU(I,J)
C    &         -VISX(I,J)
C    &         +SPX(I,J)
C    &         +SPFLX(I,J)
C    &         )/(VOID(I,J)+VOID(I+1,J))*2.0
 2105 CONTINUE
C
C  ** LEFT **
      DO 2305 I=5,IM4
      J=5
      P(I,J)=(P(I,J+1)*VOID(I,J+1)
     &       +DELY*(FV(I,J)
     &       -VISY(I,J)
     &       +SPY(I,J)
     &       ))/VOID(I,J)
C      P(I,J)=P(I,J+1)
C    &        +DELY*(FV(I,J)
C    &        -VISY(I,J)
C    &        +SPY(I,J)
C    &        +SPFLY(I,J)
C    &        )/(VOID(I,J)+VOID(I,J+1))*2.0
 2305 CONTINUE
C
C  ** RIGHT **
      DO 2405 I=5,IM4
      J=JM4
      P(I,J)=(P(I,J-1)*VOID(I,J-1)
     &       -DELY*(FV(I,J-1)
     &       -VISY(I,J-1)
     &       +SPY(I,J-1)
     &       ))/VOID(I,J)
C       P(I,J)=P(I,J-1)
C    &         -DELY*(FV(I,J-1)
C    &         -VISY(I,J-1)
C    &         +SPY(I,J-1)
C    &         +SPFLY(I,J-1)
C    &         )/(VOID(I,J)+VOID(I,J-1))*2.0
 2405 CONTINUE
C
C  ** UPPER **
      DO 2705 J=5,JM4
      P(IM4,J)=P(IM5,J)*VOID(IM5,J)/VOID(IM4,J)
C       P(IM4,J)=P(IM5,J)*VOID(IM5,J)/VOID(IM4,J)
 2705 CONTINUE
C
      IF(FLG.EQ.0.0) GO TO 4000
      ITER=ITER+1
      IF(ITER.LT.10000) GO TO 3050
      T= 1.0E+10
      GO TO 5060
 3050 FLG=0.0
      GO TO KRET
 3200 CONTINUE
C
C *** CALCULATION of R(I,J) ***
C
      DO 3500 J=6,JM5
      DO 3500 I=6,IM5
C
      F1=FU(I,J)-FU(I-1,J)
      F2=FV(I,J)-FV(I,J-1)
      VIS1=VISX(I,J)-VISX(I-1,J)
      VIS2=VISY(I,J)-VISY(I,J-1)
      SP1=SPX(I,J)-SPX(I-1,J)
      SP2=SPY(I,J)-SPY(I,J-1)
C      SPFL1=SPFLX(I,J)-SPFLX(I-1,J)
C      SPFL2=SPFLY(I,J)-SPFLY(I,J-1)
C
      Q=F1/DELX+F2/DELY
     &-VIS1/DELX-VIS2/DELY
     &+SP1/DELX+SP2/DELY
C     &+SPFL1/DELX+SPFL2/DELY
C
      VOIDBX1=(VOIDB(I+1,J)+VOIDB(I,J))/2.0
      VOIDBX2=(VOIDB(I,J)+VOIDB(I-1,J))/2.0
      VOIDBY1=(VOIDB(I,J+1)+VOIDB(I,J))/2.0
      VOIDBY2=(VOIDB(I,J)+VOIDB(I,J-1))/2.0
      DN1=((VOIDBX1*UB(I,J)-VOIDBX2*UB(I-1,J))*RDX
     &+(VOIDBY1*VB(I,J)-VOIDBY2*VB(I,J-1))*RDY)
     &+(VOID(I,J)-VOIDB(I,J))/(2*DELT)
      R(I,J)=Q-DN1/(2.0*DELT)
C      VOIDBX1=(VOIDB(I+1,J)+VOIDB(I,J))/2.0
C      VOIDBX2=(VOIDB(I,J)+VOIDB(I-1,J))/2.0
C      VOIDBY1=(VOIDB(I,J+1)+VOIDB(I,J))/2.0
C      VOIDBY2=(VOIDB(I,J)+VOIDB(I,J-1))/2.0
C      DN1=((VOIDBX1*UB(I,J)-VOIDBX2*UB(I-1,J))*RDX
C     &+(VOIDBY1*VB(I,J)-VOIDBY2*VB(I,J-1))*RDY)
C     &+(VOID(I,J)-VOIDB(I,J))/(2.0*DELT)
C      R(I,J)=Q-DN1/(2.0*DELT)
C
 3500 CONTINUE
C
C *****************************
C
      DO 4900 I=1,IMAX
      DO 4900 J=1,JMAX
        UB(I,J)=U(I,J)
        VB(I,J)=V(I,J)
 4900 CONTINUE
C
C *** RELAXATION METHOD of POISSON EQUATION ***
C
 3600 CONTINUE
C
      DO 3700 J=6,JM5
      DO 3700 I=6,IM5
      RRM(I,J)=((P(I+1,J)*VOID(I+1,J)+P(I-1,J)*VOID(I-1,J))*RDX**2
     &+(P(I,J+1)*VOID(I,J+1)+P(I,J-1)*VOID(I,J-1))*RDY**2
     &+R(I,J))/(2.0*(RDX**2+RDY**2
     &)*VOID(I,J))-P(I,J)
C        RRM(I,J)=(VOID(I,J)*(P(I+1,J)+P(I-1,J))*RDX**2
C     &           +VOID(I,J)*(P(I,J+1)+P(I,J-1))*RDY**2
C     &           +R(I,J))/(2.0*(RDX**2+RDY**2
C     &           )*VOID(I,J))-P(I,J)
 3700 CONTINUE
C
      RRMAX=0.0
C
      DO 3800 J=6,JM5
      DO 3800 I=6,IM5
      IF(ABS(RRM(I,J)).GE.EPSI) FLG=1.0
      P(I,J)=P(I,J)+OMG*RRM(I,J)
      IF(ABS(RRM(I,J)).LE.RRMAX) GO TO 3800
      RRMAX=ABS(RRM(I,J))
C        IF(ABS(RRM(I,J)).GE.EPSI) FLG=1.0
C          P(I,J)=P(I,J)+OMG*RRM(I,J)
C        IF(ABS(RRM(I,J)).LE.RRMAX) GO TO 3800
C          RRMAX=ABS(RRM(I,J))
 3800 CONTINUE
C
      ASSIGN 3600 TO KRET
C
      GO TO 2800
C
C *** CALCULATION of AIR VELOCITIES, U and V ***
C
 4000 CONTINUE
C
      DO 3900 J=6,JM5
      DO 3900 I=6,IM5
      PXY=P(I,J)*VOID(I,J)
      PX=P(I+1,J)*VOID(I+1,J)
      PY=P(I,J+1)*VOID(I,J+1)
      VOIDX=(VOID(I,J)+VOID(I+1,J))/2
      VOIDY=(VOID(I,J)+VOID(I,J+1))/2
      U(I,J)=U(I,J)+DELT*((PXY-PX)/DELX
     &-FU(I,J)
     &+VISX(I,J)
     &-SPX(I,J)
     &)/VOIDX
      V(I,J)=V(I,J)+DELT*((PXY-PY)/DELY
     &-FV(I,J)
     &+VISY(I,J)
     &-SPY(I,J)
     &)/VOIDY
C        PXY=P(I,J)
C        PX=P(I+1,J)
C        PY=P(I,J+1)
C        VOIDX=(VOID(I,J)+VOID(I+1,J))/2.0
C        VOIDY=(VOID(I,J)+VOID(I,J+1))/2.0
C        U(I,J)=U(I,J)+DELT*(VOIDX*(PXY-PX)/DELX
C     &         -FU(I,J)
C     &         +VISX(I,J)
C     &         -SPX(I,J)
C     &         -SPFLX(I,J)
C     &         )/VOIDX
C        V(I,J)=V(I,J)+DELT*(VOIDY*(PXY-PY)/DELY
C     &         -FV(I,J)
C     &         +VISY(I,J)
C     &         -SPY(I,J)
C     &         -SPFLY(I,J)
C     &         )/VOIDY
 3900 CONTINUE
C
 9600 CONTINUE
C
C *** SET BED BOTTOM AIR VELOCITY ***
      DO 4350 J=6,JM5
        U(5,J)=0.0
        U(4,J)=U(6,J)
        U(3,J)=U(7,J)
        U(2,J)=U(8,J)
        U(1,J)=U(9,J)
        V(5,J)=-V(6,J)
        V(4,J)=-V(7,J)
        V(3,J)=-V(8,J)
        V(2,J)=-V(9,J)
        V(1,J)=-V(10,J)
 4350 CONTINUE
      DO 4300 J=22,52,15
        U(5,J)=(32.0/3.0)/((VOID(5,J)+VOID(6,J))/2.0)
        U(4,J)=(32.0/3.0)/((VOID(4,J)+VOID(5,J))/2.0)
        U(3,J)=(32.0/3.0)/((VOID(3,J)+VOID(4,J))/2.0)
        U(2,J)=(32.0/3.0)/((VOID(2,J)+VOID(3,J))/2.0)
        U(1,J)=(32.0/3.0)/((VOID(1,J)+VOID(2,J))/2.0)
        V(5,J)=0.0
        V(4,J)=0.0
        V(3,J)=0.0
        V(2,J)=0.0
        V(1,J)=0.0
 4300 CONTINUE
      DO 4310 J=23,53,15
        U(5,J)=(32.0/3.0)/((VOID(5,J)+VOID(6,J))/2.0)
        U(4,J)=(32.0/3.0)/((VOID(4,J)+VOID(5,J))/2.0)
        U(3,J)=(32.0/3.0)/((VOID(3,J)+VOID(4,J))/2.0)
        U(2,J)=(32.0/3.0)/((VOID(2,J)+VOID(3,J))/2.0)
        U(1,J)=(32.0/3.0)/((VOID(1,J)+VOID(2,J))/2.0)
 4310 CONTINUE
C
C  ** LEFT **
      DO 4200 I=1,IMAX
        U(I,1)=-U(I,10)
        U(I,2)=-U(I,9)
        U(I,3)=-U(I,8)
        U(I,4)=-U(I,7)
        U(I,5)=-U(I,6)
        V(I,1)=V(I,9)
        V(I,2)=V(I,8)
        V(I,3)=V(I,7)
        V(I,4)=V(I,6)
        V(I,5)=0.0
 4200 CONTINUE
C
C  ** RIGHT **
      DO 4210 I=1,IMAX
        U(I,JMAX)=-U(I,JM9)
        U(I,JM1)=-U(I,JM8)
        U(I,JM2)=-U(I,JM7)
        U(I,JM3)=-U(I,JM6)
        U(I,JM4)=-U(I,JM5)
        V(I,JMAX)=V(I,JBAR)
        V(I,JM1)=V(I,JM9)
        V(I,JM2)=V(I,JM8)
        V(I,JM3)=V(I,JM7)
        V(I,JM4)=V(I,JM6)
        V(I,JM5)=0.0
 4210 CONTINUE
C
C  ** UPPER **
      DO 4800 J=1,JMAX
        U(IM4,J)=U(IM5,J)
        U(IM3,J)=U(IM4,J)
        U(IM2,J)=U(IM3,J)
        U(IM1,J)=U(IM2,J)
        U(IMAX,J)=U(IM1,J)
        V(IM4,J)=V(IM5,J)
        V(IM3,J)=V(IM4,J)
        V(IM2,J)=V(IM3,J)
        V(IM1,J)=V(IM2,J)
        V(IMAX,J)=V(IM1,J)
 4800 CONTINUE
C
C ****************************
C
 8810 DO 8805 J=1,JMAX
      DO 8805 I=1,IMAX
        SPX(I,J)=0.0
        SPY(I,J)=0.0
        SPFLX(I,J)=0.0
        SPFLY(I,J)=0.0
C        VORZ(I,J)=0.0
        VOLP(I,J)=0.0
 8805 CONTINUE
C
C *** CALCULATION of VORTICITY ***
C
C      DO 6750 I=6,IM5
C      DO 6750 J=6,JM5
C        VORZ(I,J)=(((V(I+1,J)+V(I+1,J-1))-(V(I-1,J)
C     &            +V(I-1,J-1)))*RDX-((U(I,J+1)+U(I-1,J+1))
C     &            -(U(I,J-1)+U(I-1,J-1)))*RDY)/8.0
C 6750 CONTINUE
C
C  ** BOTTOM **
C      DO 6751 J=1,JMAX
C        VORZ(5,J)=-VORZ(6,J)
C 6751 CONTINUE
C
C  ** LEFT **
C      DO 6752 I=1,IMAX
C        VORZ(I,5)=-VORZ(I,6)
C 6752 CONTINUE
C
C  ** RIGHT **
C      DO 6753 I=1,IMAX
C        VORZ(I,JM4)=-VORZ(I,JM5)
C 6753 CONTINUE
C
C  ** UPPER **
C      DO 6754 J=1,JMAX
C        VORZ(IM4,J)=VORZ(IM5,J)
C 6754 CONTINUE
C
C *** CALCULATION of PARTICLE MOTION (DEM) ***
C
      outcnt = 0
C
C      de=delt*SL0/SU0
C
C      CALL PARTITION
C
      dpp=DP*SL0
C
C *** CALCULATION of FORCES & TORQUE ACTING on PARTICLE ***
C
      DO 777 N=1,ipq
        XP(N)=y(N)/SL0
        YP(N)=x(N)/SL0
  777 CONTINUE
C
      DO 20 IQ = 1,IPQ
C
      ISU=INT(XP(iq)/DELX)+6
      JSU=INT((YP(iq)+DELY/2)/DELY)+6
      ISV=INT((XP(iq)+DELX/2)/DELX)+6
      JSV=INT(YP(iq)/DELY)+6
C
C      VORpZ=(VORZ(ISV-1,JSU-1)+((VORZ(ISV,JSU-1)
C     &      -VORZ(ISV-1,JSU-1))/DELX)
C     &      *((XP(iq)+DELX/2.0)-(ISV-6)*DELX)
C     &      +((VORZ(ISV-1,JSU)+((VORZ(ISV,JSU)
C     &      -VORZ(ISV-1,JSU))/DELX)
C     &      *((XP(iq)+DELX/2.0)-(ISV-6)*DELX))
C     &      -(VORZ(ISV-1,JSU-1)+((VORZ(ISV,JSU-1)
C     &      -VORZ(ISV-1,JSU-1))/DELX)
C     &      *((XP(iq)+DELX/2.0)-(ISV-6)*DELX)))/DELY
C     &      *((YP(iq)+DELY/2.0)-(JSU-6)*DELY))
C     &      *(SU0/SL0)*(-1.0)
C
      VOIDD=(VOID(ISV-1,JSU-1)+((VOID(ISV,JSU-1)
     &      -VOID(ISV-1,JSU-1))/DELX)
     &      *((XP(iq)+DELX/2.0)-(ISV-6)*DELX)
     &      +((VOID(ISV-1,JSU)+((VOID(ISV,JSU)
     &      -VOID(ISV-1,JSU))/DELX)
     &      *((XP(iq)+DELX/2.0)-(ISV-6)*DELX))
     &      -(VOID(ISV-1,JSU-1)+((VOID(ISV,JSU-1)
     &      -VOID(ISV-1,JSU-1))/DELX)
     &      *((XP(iq)+DELX/2.0)-(ISV-6)*DELX)))/DELY
     &      *((YP(iq)+DELY/2.0)-(JSU-6)*DELY))
      IF(VOIDD.GT.VOID01) VOIDD=VOID01
C
      faip=3.757-5.376/VOIDD+2.619/(VOIDD**2)
C
      usp=(V(ISV-1,JSV-1)+((V(ISV-1,JSV)
     &    -V(ISV-1,JSV-1))/DELY)
     &    *(x(iq)/SL0-(JSV-6)*DELY)
     &    +((V(ISV,JSV-1)+((V(ISV,JSV)
     &    -V(ISV,JSV-1))/DELY)
     &    *(x(iq)/SL0-(JSV-6)*DELY))
     &    -(V(ISV-1,JSV-1)+((V(ISV-1,JSV)
     &    -V(ISV-1,JSV-1))/DELY)
     &    *(x(iq)/SL0-(JSV-6)*DELY)))/DELX
     &    *((y(iq)/SL0+DELX/2.0)-(ISV-6)*DELX))
     &    *SU0
      vsp=(U(ISU-1,JSU-1)+((U(ISU,JSU-1)
     &    -U(ISU-1,JSU-1))/DELX)
     &    *(y(iq)/SL0-(ISU-6)*DELX)
     &    +((U(ISU-1,JSU)+((U(ISU,JSU)
     &    -U(ISU-1,JSU))/DELX)
     &    *(y(iq)/SL0-(ISU-6)*DELX))
     &    -(U(ISU-1,JSU-1)+((U(ISU,JSU-1)
     &    -U(ISU-1,JSU-1))/DELX)
     &    *(y(iq)/SL0-(ISU-6)*DELX)))/DELY
     &    *((x(iq)/SL0+DELY/2.0)-(JSU-6)*DELY))
     &    *SU0
C
      upxp=usp-vx(iq)
      vpyp=vsp-vy(iq)
C      omgpp=VORpZ-omgq(iq)
      REpp=RE*DP*ABS(SQRT(upxp**2+vpyp**2))/SU0
C      REpomg=abs(omgpp)*dpp**2/(4.0*anyu)
C
C *** FLUID FORCES ***
C
C  ** DRAG FORCE **
      fx(iq) = 3.0*PI*amyu*dpp*upxp*(1+0.15*repp**0.687)*faip
C  ** LIFT FORCE **
C     &         +PI/8.0*arho*dpp**3*vpyp*omgpp
C
C  ** DRAG FORCE **
      fy(iq) = 3.0*PI*amyu*dpp*vpyp*(1+0.15*repp**0.687)*faip
C  ** LIFT FORCE **
C     &         +PI/8.0*arho*dpp**3*upxp*omgpp
C  ** GRAVITATIONAL FORCE **
     &         -980.7*mass
C
C  ** FLUID FRICTION TORQUE **
C      IF (REpomg.lt.1.0) goto 6710
C      CMp=16.0*PI/REpomg+0.0418
C      GOTO 6705
C 6710 CMp=16.0*PI/REpomg
C 6705 m(iq)  = 0.5*CMp*(dpp/2.0)**5*arho*abs(omgpp)*omgpp
C
   20 continue
C
C      CALL CALCULATION
C
C      CALL MOVEP
C
C *** CALCULATION of VOID FRACTION ***
C
      DO 7777 N=1,ipq
        XP(N)=y(N)/SL0
        YP(N)=x(N)/SL0
        UP(N)=vy(N)/SU0
        VP(N)=vx(N)/SU0
 7777 CONTINUE
C
      DO 6100 IP=1,NPARTX+1
      DO 6100 JP=1,NPARTY+1
        PARTTL(IP,JP)=0
        DO 6100 NPP=1,32
          PART(IP,JP,NPP)=0
 6100 CONTINUE
C
      DO 6151 N=1,NP
        XPP=XP(N)
        YPP=YP(N)
        II=INT(XPP/(DELX*DPARTX))+1
        JJ=INT(YPP/(DELY*DPARTY))+1
        TNO=PARTTL(II,JJ)+1
        IF(TNO.GT.32) GO TO 6151
        PARTTL(II,JJ)=TNO
        PART(II,JJ,TNO)=N 
 6151 CONTINUE
C
      DO 6700 N=1,NP
        X1=XP(N)-DELX*INT(XP(N)/DELX)
        X2=DELX*(INT(XP(N)/DELX)+1)-XP(N)
        Y1=YP(N)-DELY*INT(YP(N)/DELY)
        Y2=DELY*(INT(YP(N)/DELY)+1)-YP(N)
        IX=INT(XP(N)/DELX)+6
        JY=INT(YP(N)/DELY)+6
        XX1=DP/2-X1
        XX2=DP/2-X2
        YY1=DP/2-Y1
        YY2=DP/2-Y2
        XX1=MAX(XX1,0.0)
        XX2=MAX(XX2,0.0)
        YY1=MAX(YY1,0.0)
        YY2=MAX(YY2,0.0)
        VOLPX1=PI*XX1**2*(DP+X1)/3.0
        VOLPX2=PI*XX2**2*(DP+X2)/3.0
        VOLPY1=PI*YY1**2*(DP+Y1)/3.0
        VOLPY2=PI*YY2**2*(DP+Y2)/3.0
        VOLP(IX-1,JY)=VOLP(IX-1,JY)+VOLPX1
        VOLP(IX,JY-1)=VOLP(IX,JY-1)+VOLPY1
        VOLP(IX,JY)=VOLP(IX,JY)+VOLPP-(VOLPX1+VOLPY1+VOLPX2+VOLPY2)
        VOLP(IX+1,JY)=VOLP(IX+1,JY)+VOLPX2
        VOLP(IX,JY+1)=VOLP(IX,JY+1)+VOLPY2
 6700 CONTINUE
C
      DO 6810 J=1,JMAX
      DO 6810 I=1,IMAX
        VOIDB(I,J)=VOID(I,J)
 6810 CONTINUE
C
      DO 6800 J=6,JM5
      DO 6800 I=6,IM5
        VOIDD=(VOLC-VOLP(I,J))/VOLC
        IF(VOIDD.GE.VOID00) GOTO 6801
          VOIDD=1.0-(1.0-VOIDD)*((1.0-VOIDD)/(1.0-VOID00))**0.77
          IF(VOIDD.LT.VOIDMIN) VOIDD=VOIDMIN
 6801   VOID(I,J)=VOIDD
 6800 CONTINUE
C
C  ** BOTTOM **
      DO 7135 J=1,JMAX
        VOID(5,J)=VOID(6,J)
        VOID(4,J)=VOID(7,J)
        VOID(3,J)=VOID(8,J)
        VOID(2,J)=VOID(9,J)
        VOID(1,J)=VOID(10,J)
 7135 CONTINUE
C
C  ** LEFT **
      DO 7139 I=1,IMAX
        VOID(I,1)=VOID(I,10)
        VOID(I,2)=VOID(I,9)
        VOID(I,3)=VOID(I,8)
        VOID(I,4)=VOID(I,7)
        VOID(I,5)=VOID(I,6)
 7139 CONTINUE
C
C  ** RIGHT **
      DO 7137 I=1,IMAX
        VOID(I,JMAX)=VOID(I,JM9)
        VOID(I,JM1)=VOID(I,JM8)
        VOID(I,JM2)=VOID(I,JM7)
        VOID(I,JM3)=VOID(I,JM6)
        VOID(I,JM4)=VOID(I,JM5)
 7137 CONTINUE
C
C  ** UPPER **
      DO 7138 J=1,JMAX
        VOID(IMAX,J)=VOID(IM5,J)
        VOID(IM1,J)=VOID(IM5,J)
        VOID(IM2,J)=VOID(IM5,J)
        VOID(IM3,J)=VOID(IM5,J)
        VOID(IM4,J)=VOID(IM5,J)
 7138 CONTINUE
C
C *** CALCULATION of PARTICLE SOURCE TERM ***
C
 8100 DO 7201 II=1,NPARTX+1
      DO 7201 JJ=1,NPARTY+1
        TNOK=PARTTL(II,JJ)
        DO 7202 NO1=1,TNOK
          N=PART(II,JJ,NO1)
          ISU=INT(XP(N)/DELX)+6
          JSU=INT((YP(N)+DELY/2.0)/DELY)+6
          ISV=INT((XP(N)+DELX/2.0)/DELX)+6
          JSV=INT(YP(N)/DELY)+6
C          VORZZ=VORZ(ISV-1,JSU-1)+((VORZ(ISV,JSU-1)
C     &          -VORZ(ISV-1,JSU-1))/DELX)
C     &          *((XP(N)+DELX/2.0)-(ISV-6)*DELX)
C     &          +((VORZ(ISV-1,JSU)+((VORZ(ISV,JSU)
C     &          -VORZ(ISV-1,JSU))/DELX)
C     &          *((XP(N)+DELX/2.0)-(ISV-6)*DELX))
C     &          -(VORZ(ISV-1,JSU-1)+((VORZ(ISV,JSU-1)
C     &          -VORZ(ISV-1,JSU-1))/DELX)
C     &          *((XP(N)+DELX/2.0)-(ISV-6)*DELX)))/DELY
C     &          *((YP(N)+DELY/2.0)-(JSU-6)*DELY)
C
          VOIDD=(VOID(ISV-1,JSU-1)+((VOID(ISV,JSU-1)
     &          -VOID(ISV-1,JSU-1))/DELX)
     &          *((XP(N)+DELX/2.0)-(ISV-6)*DELX)
     &          +((VOID(ISV-1,JSU)+((VOID(ISV,JSU)
     &          -VOID(ISV-1,JSU))/DELX)
     &          *((XP(N)+DELX/2.0)-(ISV-6)*DELX))
     &          -(VOID(ISV-1,JSU-1)+((VOID(ISV,JSU-1)
     &          -VOID(ISV-1,JSU-1))/DELX)
     &          *((XP(N)+DELX/2.0)-(ISV-6)*DELX)))/DELY
     &          *((YP(N)+DELY/2.0)-(JSU-6)*DELY))
C
      FAI=3.757-5.376/VOIDD+2.619/(VOIDD**2)
C
      US=U(ISU-1,JSU-1)+((U(ISU,JSU-1)
     &   -U(ISU-1,JSU-1))/DELX)
     &   *(XP(N)-(ISU-6)*DELX)
     &   +((U(ISU-1,JSU)+((U(ISU,JSU)
     &   -U(ISU-1,JSU))/DELX)
     &   *(XP(N)-(ISU-6)*DELX))
     &   -(U(ISU-1,JSU-1)+((U(ISU,JSU-1)
     &   -U(ISU-1,JSU-1))/DELX)
     &   *(XP(N)-(ISU-6)*DELX)))/DELY
     &   *((YP(N)+DELY/2.0)-(JSU-6)*DELY)
C
      VS=V(ISV-1,JSV-1)+((V(ISV-1,JSV)
     &   -V(ISV-1,JSV-1))/DELY)
     &   *(YP(N)-(JSV-6)*DELY)
     &   +((V(ISV,JSV-1)+((V(ISV,JSV)
     &   -V(ISV,JSV-1))/DELY)
     &   *(YP(N)-(JSV-6)*DELY))
     &   -(V(ISV-1,JSV-1)+((V(ISV-1,JSV)
     &   -V(ISV-1,JSV-1))/DELY)
     &   *(YP(N)-(JSV-6)*DELY)))/DELX
     &   *((XP(N)+DELX/2.0)-(ISV-6)*DELX)
C
      UPX=UP(N)-US
      VPY=VP(N)-VS
      REP=RE*DP*ABS(SQRT(UPX**2+VPY**2))
C
C  ** due to DRAG FORCE **
      SPX(ISU,JSV)=SPX(ISU,JSV)+3.0*PI*DP
     &             *(1+0.15*REP**0.687)*(US-UP(N))*FAI
     &             /(RE*DELX*DELY*DELZ)
      SPY(ISU,JSV)=SPY(ISU,JSV)+3.0*PI*DP
     &             *(1+0.15*REP**0.687)*(VS-VP(N))*FAI
     &             /(RE*DELX*DELY*DELZ)
C
C  ** due to LIFT FORCE **
C      SPFLX(ISU,JSV)=SPFLX(ISU,JSV)+PI/8.0*DP**3
C     &               *(VS-VP(N))*(VORZZ-OMGQ(N)*SL0/SU0*(-1.0))
C     &               /(DELX*DELY*DELZ)
C      SPFLY(ISU,JSV)=SPFLY(ISU,JSV)+PI/8.0*DP**3
C     &               *(US-UP(N))*(VORZZ-OMGQ(N)*SL0/SU0*(-1.0))
C     &               /(DELX*DELY*DELZ)
C
 7202 CONTINUE
 7201 CONTINUE
C
C  ** BOTTOM & UPPER **
      DO 7210 J=1,JMAX
        SPX(5,J)=SPX(6,J)
        SPX(IM4,J)=SPX(IM5,J)
C        SPFLX(5,J)=SPFLX(6,J)
C        SPFLX(IM4,J)=SPFLX(IM5,J)
 7210 CONTINUE
C
C  ** LEFT & RIGHT **
      DO 7220 I=1,IMAX
        SPY(I,5)=SPY(I,6)
        SPY(I,JM4)=SPFLY(I,JM5)
C        SPFLY(I,5)=SPFLY(I,6)
C        SPFLY(I,JM4)=SPY(I,JM5)
 7220 CONTINUE
C
      DO 7222 I=1,IMAX
      DO 7222 J=1,JMAX
      SPFLX(I,J)=(SPX(I+1,J)+SPX(I-1,J)
     &+SPX(I,J+1)+SPX(I,J-1))/(4.0*20.0)
      SPFLY(I,J)=(SPY(I+1,J)+SPY(I-1,J)
     &+SPY(I,J+1)+SPY(I,J-1))/(4.0*20.0)
 7222 CONTINUE
C
C **** OUTPUT DATAFILE (CALCULATION CONDITIONS) ****
C
      IF(CYCLE.GT.1) GO TO 5050
      WRITE(60,50) IBAR,JBAR,DELX,DELY,DELT,RE,SU0,SL0,OMG,TWFIN
 5050 CONTINUE
      WRITE(60,77) CYCLE,ITER,RRMAX,EPSI
      IF(CYCLE.EQ.1) GO TO 6000
      TW=TWFIN-DELT
      IF(T.GT.TW) GO TO 5060
      GO TO 6000
C
 5060 WRITE(60,77) CYCLE,ITER,RRMAX,EPSI
C
 6000 CONTINUE
C
C ****************************************************************
C
      IF(MOD(CYCLE,20).LT.1) THEN
        WRITE(90,79) U(11,10),  V(11,10),  U(11,38),  V(11,38)
        WRITE(91,79) U(21,10),  V(21,10),  U(21,38),  V(21,38)
        WRITE(92,79) U(37,10),  V(37,10),  U(37,38),  V(37,38)
        WRITE(93,79) U(53,10),  V(53,10),  U(53,38),  V(53,38)
        WRITE(94,79) U(69,10),  V(69,10),  U(69,38),  V(69,38)
        WRITE(95,79) U(85,10),  V(85,10),  U(85,38),  V(85,38)
        WRITE(96,79) U(101,10), V(101,10), U(101,38), V(101,38)
        WRITE(97,79) U(133,10), V(133,10), U(133,38), V(133,38)
        WRITE(98,79) U(197,10), V(197,10), U(197,38), V(197,38)
      ENDIF
C
C *** OUTPUT DATAFILE ("airfile0*.dat" and "file0*.dat") ***
C
      IF(MOD(CYCLE,NCYCLE00).LT.1) THEN
C        CALL POST00
      open( unit = 1, err = 9012, status='old',
     &file = 'airfile00.dat' )
      close( unit = 1, status = 'delete')
 9012 open( unit = 1, err = 9904, status='new',
     &file = 'airfile00.dat' )
      WRITE(1,93) CYCLE
      DO 5203 I=1,IMAX
      DO 5203 J=1,JMAX
        WRITE(1,78) U(I,J),V(I,J),VOID(I,J),P(I,J)
        WRITE(1,48) UB(I,J),VB(I,J),VOIDB(I,J)
 5203 CONTINUE
      DO 5303 I=1,IMAX
      DO 5303 J=1,JMAX
        WRITE(1,12) SPX(I,J),SPY(I,J)
        WRITE(1,12) SPFLX(I,J),SPFLY(I,J)
 5303 CONTINUE
      close( 1 )
C
      goto 9905
 9904 write(9,*) 'open error airfile00.dat (unit=1)'
 9905 continue
      NCYCLE00=NCYCLE00+NENDLESS
      ENDIF
C
C *******************************
C
      IF(MOD(CYCLE,NCYCLE01).LT.1) THEN
C        CALL POST01
      open( unit = 1, err = 8992, status='old',
     &file = 'airfile01.dat' )
      close( unit = 1, status = 'delete')
 8992 open( unit = 1, err = 9884, status='new',
     &file = 'airfile01.dat' )
      WRITE(1,93) CYCLE
      DO 5183 I=1,IMAX
      DO 5183 J=1,JMAX
        WRITE(1,78) U(I,J),V(I,J),VOID(I,J),P(I,J)
        WRITE(1,48) UB(I,J),VB(I,J),VOIDB(I,J)
 5183 CONTINUE
      DO 5283 I=1,IMAX
      DO 5283 J=1,JMAX
        WRITE(1,12) SPX(I,J),SPY(I,J)
        WRITE(1,12) SPFLX(I,J),SPFLY(I,J)
 5283 CONTINUE
      close( 1 )
C
      goto 9885
 9884 write(9,*) 'open error airfile01.dat (unit=1)'
 9885 continue
      NCYCLE01=NCYCLE01+NENDLESS
      ENDIF
C
C *******************************
C
      IF(MOD(CYCLE,NCYCLE02).LT.1) THEN
C        CALL POST02
      open( unit = 1, err = 9015, status='old',
     &file = 'airfile02.dat' )
      close( unit = 1, status = 'delete')
 9015 open( unit = 1, err = 9007, status='new',
     &file = 'airfile02.dat' )
      WRITE(1,93) CYCLE
      DO 5206 I=1,IMAX
      DO 5206 J=1,JMAX
        WRITE(1,78) U(I,J),V(I,J),VOID(I,J),P(I,J)
        WRITE(1,48) UB(I,J),VB(I,J),VOIDB(I,J)
 5206 CONTINUE
      DO 5306 I=1,IMAX
      DO 5306 J=1,JMAX
        WRITE(1,12) SPX(I,J),SPY(I,J)
        WRITE(1,12) SPFLX(I,J),SPFLY(I,J)
 5306 CONTINUE
      close( 1 )
C
      goto 9908
 9007 write(9,*) 'open error airfile02.dat (unit=1)'
 9908 continue
      NCYCLE02=NCYCLE02+NENDLESS
      ENDIF
C
C *******************************
C
      IF(MOD(CYCLE,NCYCLE03).LT.1) THEN
C        CALL POST03
      open( unit = 1, err = 9016, status='old',
     &file = 'airfile03.dat' )
      close( unit = 1, status = 'delete')
 9016 open( unit = 1, err = 9008, status='new',
     &file = 'airfile03.dat' )
      WRITE(1,93) CYCLE
      DO 5207 I=1,IMAX
      DO 5207 J=1,JMAX
        WRITE(1,78) U(I,J),V(I,J),VOID(I,J),P(I,J)
        WRITE(1,48) UB(I,J),VB(I,J),VOIDB(I,J)
 5207 CONTINUE
      DO 5307 I=1,IMAX
      DO 5307 J=1,JMAX
        WRITE(1,12) SPX(I,J),SPY(I,J)
        WRITE(1,12) SPFLX(I,J),SPFLY(I,J)
 5307 CONTINUE
      close( 1 )
C
      goto 9909
 9008 write(9,*) 'open error airfile03.dat (unit=1)'
 9909 continue
      NCYCLE03=NCYCLE03+NENDLESS
      ENDIF
C
C *******************************
C
      IF(MOD(CYCLE,NCYCLE04).LT.1) THEN
C        CALL POST04
      open( unit = 1, err = 9017, status='old',
     &file = 'airfile04.dat' )
      close( unit = 1, status = 'delete')
 9017 open( unit = 1, err = 9009, status='new',
     &file = 'airfile04.dat' )
      WRITE(1,93) CYCLE
      DO 5208 I=1,IMAX
      DO 5208 J=1,JMAX
        WRITE(1,78) U(I,J),V(I,J),VOID(I,J),P(I,J)
        WRITE(1,48) UB(I,J),VB(I,J),VOIDB(I,J)
 5208 CONTINUE
      DO 5308 I=1,IMAX
      DO 5308 J=1,JMAX
        WRITE(1,12) SPX(I,J),SPY(I,J)
        WRITE(1,12) SPFLX(I,J),SPFLY(I,J)
 5308 CONTINUE
      close( 1 )
C
      goto 9917
 9009 write(9,*) 'open error airfile04.dat (unit=1)'
 9917 continue
      NCYCLE04=NCYCLE04+NENDLESS
      ENDIF
C
C *******************************
C
      IF(MOD(CYCLE,NCYCLE05).LT.1) THEN
C        CALL POST05
      open( unit = 1, err = 9018, status='old',
     &file = 'airfile05.dat' )
      close( unit = 1, status = 'delete')
 9018 open( unit = 1, err = 9079, status='new',
     &file = 'airfile05.dat' )
      WRITE(1,93) CYCLE
      DO 5209 I=1,IMAX
      DO 5209 J=1,JMAX
        WRITE(1,78) U(I,J),V(I,J),VOID(I,J),P(I,J)
        WRITE(1,48) UB(I,J),VB(I,J),VOIDB(I,J)
 5209 CONTINUE
      DO 5309 I=1,IMAX
      DO 5309 J=1,JMAX
        WRITE(1,12) SPX(I,J),SPY(I,J)
        WRITE(1,12) SPFLX(I,J),SPFLY(I,J)
 5309 CONTINUE
      close( 1 )
C
      goto 9918
 9079 write(9,*) 'open error airfile05.dat (unit=1)'
 9918 continue
      NCYCLE05=NCYCLE05+NENDLESS
      ENDIF
C
C *******************************
C
      IF(MOD(CYCLE,NCYCLE06).LT.1) THEN
C        CALL POST06
      open( unit = 1, err = 9128, status='old',
     &file = 'airfile06.dat' )
      close( unit = 1, status = 'delete')
 9128 open( unit = 1, err = 9189, status='new',
     &file = 'airfile06.dat' )
      WRITE(1,93) CYCLE
      DO 5319 I=1,IMAX
      DO 5319 J=1,JMAX
        WRITE(1,78) U(I,J),V(I,J),VOID(I,J),P(I,J)
        WRITE(1,48) UB(I,J),VB(I,J),VOIDB(I,J)
 5319 CONTINUE
      DO 5419 I=1,IMAX
      DO 5419 J=1,JMAX
        WRITE(1,12) SPX(I,J),SPY(I,J)
        WRITE(1,12) SPFLX(I,J),SPFLY(I,J)
 5419    CONTINUE
      close( 1 )
C
      goto 9808
 9189 write(9,*) 'open error airfile06.dat (unit=1)'
 9808 continue
      NCYCLE06=NCYCLE06+NENDLESS
      ENDIF
C
C *******************************
C
      IF(MOD(CYCLE,NCYCLE07).LT.1) THEN
C        CALL POST07
      open( unit = 1, err = 9069, status='old',
     &file = 'airfile07.dat' )
      close( unit = 1, status = 'delete')
 9069 open( unit = 1, err = 9130, status='new',
     &file = 'airfile07.dat' )
      WRITE(1,93) CYCLE
      DO 5260 I=1,IMAX
      DO 5260 J=1,JMAX
       WRITE(1,78) U(I,J),V(I,J),VOID(I,J),P(I,J)
       WRITE(1,48) UB(I,J),VB(I,J),VOIDB(I,J)
 5260 CONTINUE
      DO 5360 I=1,IMAX
      DO 5360 J=1,JMAX
        WRITE(1,12) SPX(I,J),SPY(I,J)
        WRITE(1,12) SPFLX(I,J),SPFLY(I,J)
 5360 CONTINUE
      close( 1 )
C
      goto 9969
 9130 write(9,*) 'open error airfile07.dat (unit=1)'
 9969 continue
      NCYCLE07=NCYCLE07+NENDLESS
      ENDIF
C
C *******************************
C
      IF(MOD(CYCLE,NCYCLE08).LT.1) THEN
C        CALL POST08
      open( unit = 1, err = 9059, status='old',
     &file = 'airfile08.dat' )
      close( unit = 1, status = 'delete')
 9059 open( unit = 1, err = 9121, status='new',
     &file = 'airfile08.dat' )
      WRITE(1,93) CYCLE
      DO 5250 I=1,IMAX
      DO 5250 J=1,JMAX
       WRITE(1,78) U(I,J),V(I,J),VOID(I,J),P(I,J)
       WRITE(1,48) UB(I,J),VB(I,J),VOIDB(I,J)
 5250 CONTINUE
      DO 5957 I=1,IMAX
      DO 5957 J=1,JMAX
        WRITE(1,12) SPX(I,J),SPY(I,J)
        WRITE(1,12) SPFLX(I,J),SPFLY(I,J)
 5957 CONTINUE
      close( 1 )
C
      goto 9959
 9121 write(9,*) 'open error airfile08.dat (unit=1)'
 9959 continue
      NCYCLE08=NCYCLE08+NENDLESS
      ENDIF
C
C *******************************
C
      IF(MOD(CYCLE,NCYCLE09).LT.1) THEN
C        CALL POST09
      open( unit = 1, err = 9049, status='old',
     &file = 'airfile09.dat' )
      close( unit = 1, status = 'delete')
 9049 open( unit = 1, err = 9110, status='new',
     &file = 'airfile09.dat' )
      WRITE(1,93) CYCLE
      DO 5240 I=1,IMAX
      DO 5240 J=1,JMAX
       WRITE(1,78) U(I,J),V(I,J),VOID(I,J),P(I,J)
        WRITE(1,48) UB(I,J),VB(I,J),VOIDB(I,J)
 5240 CONTINUE
      DO 5340 I=1,IMAX
      DO 5340 J=1,JMAX
        WRITE(1,12) SPX(I,J),SPY(I,J)
        WRITE(1,12) SPFLX(I,J),SPFLY(I,J)
 5340 CONTINUE
      close( 1 )
C
      goto 9949
 9110 write(9,*) 'open error airfile09.dat (unit=1)'
 9949 continue
      NCYCLE09=NCYCLE09+NENDLESS
      ENDIF
C
C *******************************************************
C
      IF(MOD(CYCLE,50).LT.1) THEN
      DO 6640 I=26,32
      DO 6640 J= 6,69
        II=I-25
        JJ=J-5
        WRITE(75,82) NTIME,II,JJ,U(I,J),V(I,J),VOID(I,J),P(I,J)
     &              ,SPX(I,J),SPY(I,J),SPFLX(I,J),SPFLY(I,J)
 6640 CONTINUE
      DO 6645 I=82,88
      DO 6645 J= 6,69
        II=I-81
        JJ=J-5
        WRITE(76,82) NTIME,II,JJ,U(I,J),V(I,J),VOID(I,J),P(I,J)
     &              ,SPX(I,J),SPY(I,J),SPFLX(I,J),SPFLY(I,J)
 6645 CONTINUE
        NTIME=NTIME+1
      ENDIF
C
C *******************************************************
C
      WRITE(*,*) 'CYCLE = ', CYCLE, ' -------- OK.'
C
C **** TIME T=T+DELT ************
C
 5150 T=T+DELT
      IF(T.GT.TWFIN) GO TO 5100
      CYCLE=CYCLE+1
C
      GO TO 1000
C
C *******************************
C
 5100 CONTINUE
C
      T=T-DELT
      WRITE(60,110) T,CYCLE
      WRITE(60,*) 'END KEKKA'
C
  999 continue
C
 6500 STOP
C
C *** FORMAT of OUTPUT DATA ***
C
   50 FORMAT(1H ,5X'IBAR= 'I3/6X'JBAR= 'I3/ 
     &6X'DELX= '1PE12.5/6X'DELY= 'E12.5/ 
     &6X'DELT= 'E12.5/8X'RE= 'E12.5/8X'SU0= 'E12.5/8X'SL0= 'E12.5/ 
     &7X'OMG= 'E12.5/5X'TWFIN= 'E12.5)
  110 FORMAT(2X'TIME=  '1PE12.5,4X'CYCLE= 'I6)
   48 FORMAT(2X,3(1X,1PE12.5))
   49 FORMAT(2X,2(1X,1PE12.5))
   79 FORMAT(4(1X,1PE10.3))
   78 FORMAT(2X,4(1X,1PE12.5))
   77 FORMAT(2X'CYCLE= 'I6,5X,'ITER= 'I5,5X,'RRMAX= '1PE12.5
     &,5X,'EPSI= 'E12.5,5X
     &)
   91 FORMAT(2X,8(F8.4,1X))
   92 FORMAT(2X,'CYCLE= 'I6)
   93 FORMAT(2X,I6)
   94 FORMAT(1PE12.5)
   12 FORMAT(2X,2(1X,1PE12.5))
   81 FORMAT(1X,4(1X,1PE12.5))
   82 FORMAT(1X,3I6,8(1X,1PE12.5))
  901 FORMAT(2X,(1X,1PE12.5))
  902 FORMAT(2X,4(1X,1PE12.5))
  903 FORMAT(1X,8(1X,1PE12.5))
C
C *****************************
C
      END








































