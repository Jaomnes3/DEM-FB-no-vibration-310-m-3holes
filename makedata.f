      PROGRAM MAKEDATA
C
      REAL mu, muw,
     &     XW(3), YW(3), EWNX(3), EWNY(3), EWTX(3), EWTY(3),
     &     X(300000), Y(300000),  R(300000),
     &     XP(300000), YP(300000), RANSU(2700000),
     &     LENGX, LENGY
C
      INTEGER PXM, PYM, OS, COUNT
C     
      OPEN ( 1, ERR = 9900,STATUS = 'OLD',FILE ='infile.dat')
      CLOSE( 1, STATUS = 'DELETE' )
 9900 OPEN ( 1, STATUS = 'NEW',    FILE = 'infile.dat' )
C
            TS = 0.0
            TP = 0.0
            TE = 2.0
            DT = 0.00001
C
            R01  = 0.0090625
            R02  = 0.0091875
            R03  = 0.0093125
            R04  = 0.0094375
            R05  = 0.0095625
            R06  = 0.0096875
            R07  = 0.0098125
            R08  = 0.0099375
            R09  = 0.0100625
            R10  = 0.0101875
            R11  = 0.0103125
            R12  = 0.0104375
            R13  = 0.0105625
            R14  = 0.0106875
            R15  = 0.0108125
            R16  = 0.0109275
            R17  = 0.0110625
            R18  = 0.0111875
            R19  = 0.0113125
            R20  = 0.0114375
C
            NN01 = 0
            NN02 = 0
            NN03 = 0
            NN04 = 0
            NN05 = 0
            NN06 = 0
            NN07 = 0
            NN08 = 0
            NN09 = 0
            NN10 = 0
            NN11 = 0
            NN12 = 0
            NN13 = 0
            NN14 = 0
            NN15 = 0
            NN16 = 0
            NN17 = 0
            NN18 = 0
            NN19 = 0
            NN20 = 0
C
            RHO = 2.5
            mu  = 0.25
            muw = 0.30
C
C            pi = 4.0*atan(1.0)
C
            VPY0 = -1.0*SQRT(2.0*980.7*(2.0*R20)*1.0)
            WRITE(*,*) 'VPY0 = ', VPY0
C
C
            LENGX = 2.0*R20
            LENGY = 2.0*R20
C
            PXM =  353
            PYM =  1761
C
            IWALL = 3
            OS = 1
C
            DO 15 I=1,IWALL
              XW(I) = 0.0
              YW(I) = 0.0
              EWNX(I) = 0.0
              EWNY(I) = 0.0
              EWTX(I) = 0.0
              EWTY(I) = 0.0
 15         CONTINUE
C
            XW(3) = 8.0520
C
            EWNX(1) = -1.0
            EWTY(1) = -1.0
            EWNY(2) = -1.0
            EWTX(2) =  1.0
            EWNX(3) =  1.0
            EWTY(3) =  1.0
C
            ANG = 0.0
            PE = 1000000.0
            PNU = 0.25
            VX = 0.0
            VY = VPY0
            OMG = 0.0
C
C --------------------------------
C
            NIP = 300000
C           NIP = 100000
C
C *******************************************************************
C
      lflag = 1
      pint0 = R20
      pint1 = R20*1.7321
C
      do 1001 j = 1, 427
         do 2001 i = 1, 352
               xp(lflag) = pint0+2.0*pint0*float(i-1)
               yp(lflag) = pint0+2.0*pint1*float(j-1)
               r(lflag) = R20
               lflag = lflag + 1
C         if(lflag .EQ. 300001) then
C           goto 3001
C           endif
C
 2001    continue
 1001 continue
C
      do 1501 j = 1, 427
         do 2501 i = 1, 351
               xp(lflag) = 2.0*pint0+2.0*pint0*float(i-1)
               yp(lflag) = (pint0+pint1)+2.0*pint1*float(j-1)
               r(lflag) = R20
               lflag = lflag + 1
         if(lflag .EQ. 300001) then
           goto 3001
           endif
C
 2501    continue
 1501 continue
C
 3001 continue
C
      lflag=lflag-1
      write(*,*)'Particle Number = ', lflag
      IP=lflag
C
            WRITE(*,*) lflag, ' Particles --- OK (by PAR.F).'
C
            WRITE(*,*) 'SET ALL PARTICLES ----- ENDED.'
            WRITE(*,*) 'IP = ', IP
C
C ------------ R1 ---------------------------------------------
C
            count = 1
C
            do 5010 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
              ransu(i)=rand(0)
 5010       continue
C
            DO 1005 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
               if ( r(change) .ne. R20 ) then
                  goto 1005
               endif
               r(CHANGE) = R01
               count = count + 1
C               write(*,*) 'Number of R01 = ', count-1
               IF ( count .EQ. 1953 ) THEN
                  GOTO 900
               ENDIF
 1005       CONTINUE
C
 900        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R1 (1952). '
C
C --------------------------------------------------------
C
C ------------ R2 ----------------------------------------------
C
            count = 1
C
C            do 5011 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5011       continue
C
            DO 1006 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1006
               endif
               r(CHANGE) = R02
               count = count + 1
C               write(*,*) 'Number of R02 = ', count-1
               IF ( count .EQ. 3973 ) THEN
                  GOTO 901
               ENDIF
 1006       CONTINUE
C
 901        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R2 (3972).'
C
C --------------------------------------------------------
C ------------ R3 ----------------------------------------------
C
            count = 1
C
C            do 5012 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5012      continue
C
            DO 1007 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1007
               endif
               r(CHANGE) = R03
               count = count + 1
C               write(*,*) 'Number of R03 = ', count-1
               IF ( count .EQ. 7003 ) THEN
                  GOTO 902
               ENDIF
 1007       CONTINUE
C
 902        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R3 (7002).'
C
C --------------------------------------------------------
C ------------ R4 ----------------------------------------------
C
            count = 1
C
C            do 5013 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5013      continue
C
            DO 1008 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1008
               endif
               r(CHANGE) = R04
               count = count + 1
C               write(*,*) 'Number of R04 = ', count-1
               IF ( count .EQ. 10202 ) THEN
                  GOTO 903
               ENDIF
 1008       CONTINUE
C
 903        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R4 (10201).'
C
C --------------------------------------------------------
C ------------ R5 ----------------------------------------------
C
            count = 1
C
C            do 5014 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5014      continue
C
            DO 1009 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1009
               endif
               r(CHANGE) = R05
               count = count + 1
C               write(*,*) 'Number of R05 = ', count-1
               IF ( count .EQ. 14242 ) THEN
                  GOTO 904
               ENDIF
 1009       CONTINUE
C
 904        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R5 (14241).'
C
C --------------------------------------------------------
C ------------ R6 ----------------------------------------------
C
            count = 1
C
C            do 5015 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5015      continue
C
            DO 1010 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1010
               endif
               r(CHANGE) = R06
               count = count + 1
C               write(*,*) 'Number of R06 = ', count-1
               IF ( count .EQ. 19125 ) THEN
                  GOTO 905
               ENDIF
 1010       CONTINUE
C
 905        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R6 (19124).'
C
C --------------------------------------------------------
C ------------ R7 ----------------------------------------------
C
            count = 1
C
C            do 5016 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5016      continue
C
            DO 1011 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5)
C     &                                  .and.(r(change) .ne. R6) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1011
               endif
               r(CHANGE) = R07
               count = count + 1
C               write(*,*) 'Number of R07 = ', count-1
               IF ( count .EQ. 23502 ) THEN
                  GOTO 906
               ENDIF
 1011       CONTINUE
C
 906        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R7 (23501).'
C
C --------------------------------------------------------
C ------------ R8 ----------------------------------------------
C
            count = 1
C
C            do 5017 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5017      continue
C
            DO 1012 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5)
C     &                                  .and.(r(change) .ne. R6)
C     &                                  .and.(r(change) .ne. R7) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1012
               endif
               r(CHANGE) = R08
               count = count + 1
C               write(*,*) 'Number of R08 = ', count-1
               IF ( count .EQ. 27037 ) THEN
                  GOTO 907
               ENDIF
 1012       CONTINUE
C
 907        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R8 (27036).'
C
C --------------------------------------------------------
C ------------ R9 ----------------------------------------------
C
            count = 1
C
C            do 5018 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5018      continue
C
            DO 1013 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5)
C     &                                  .and.(r(change) .ne. R6)
C     &                                  .and.(r(change) .ne. R7)
C     &                                  .and.(r(change) .ne. R8) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1013
               endif
               r(CHANGE) = R09
               count = count + 1
C               write(*,*) 'Number of R09 = ', count-1
               IF ( count .EQ. 28889 ) THEN
                  GOTO 908
               ENDIF
 1013       CONTINUE
C
 908        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R9 (28888).'
C
C --------------------------------------------------------
C ------------ R10 ----------------------------------------------
C
            count = 1
C
C            do 5019 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5019      continue
C
            DO 1014 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5)
C     &                                  .and.(r(change) .ne. R6)
C     &                                  .and.(r(change) .ne. R7)
C     &                                  .and.(r(change) .ne. R8)
C     &                                  .and.(r(change) .ne. R9) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1014
               endif
               r(CHANGE) = R10
               count = count + 1
C               write(*,*) 'Number of R10 = ', count-1
               IF ( count .EQ. 29899 ) THEN
                  GOTO 909
               ENDIF
 1014       CONTINUE
C
 909        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R10 (29898)).'
C
C --------------------------------------------------------
C ------------ R11 ----------------------------------------------
C
            count = 1
C
C            do 5020 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5020                continue
C
            DO 1015 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5)
C     &                                  .and.(r(change) .ne. R6)
C     &                                  .and.(r(change) .ne. R7)
C     &                                  .and.(r(change) .ne. R8)
C     &                                  .and.(r(change) .ne. R9)
C     &                                  .and.(r(change) .ne. R10) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1015
               endif
               r(CHANGE) = R11
               count = count + 1
C               write(*,*) 'Number of R11 = ', count-1
               IF ( count .EQ. 29562 ) THEN
                  GOTO 910
               ENDIF
 1015       CONTINUE
C
 910        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R11 (29561).'
C
C --------------------------------------------------------
C ------------ R12 ----------------------------------------------
C
            count = 1
C
C            do 5021 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5021      continue
C
            DO 1016 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5)
C     &                                  .and.(r(change) .ne. R6)
C     &                                  .and.(r(change) .ne. R7)
C     &                                  .and.(r(change) .ne. R8)
C     &                                  .and.(r(change) .ne. R9)
C     &                                  .and.(r(change) .ne. R10)
C     &                                  .and.(r(change) .ne. R11) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1016
               endif
               r(CHANGE) = R12
               count = count + 1
C               write(*,*) 'Number of R12 = ', count-1
               IF ( count .EQ. 28889 ) THEN
                  GOTO 911
               ENDIF
 1016       CONTINUE
C
 911        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R12 (28888).'
C
C --------------------------------------------------------
C ------------ R13 ----------------------------------------------
C
            count = 1
C
C            do 5022 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5022      continue
C
            DO 1017 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5)
C     &                                  .and.(r(change) .ne. R6)
C     &                                  .and.(r(change) .ne. R7)
C     &                                  .and.(r(change) .ne. R8)
C     &                                  .and.(r(change) .ne. R9)
C     &                                  .and.(r(change) .ne. R10)
C     &                                  .and.(r(change) .ne. R11)
C     &                                  .and.(r(change) .ne. R12) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1017
               endif
               r(CHANGE) = R13
               count = count + 1
C               write(*,*) 'Number of R13 = ', count-1
               IF ( count .EQ. 26700 ) THEN
                  GOTO 912
               ENDIF
 1017       CONTINUE
C
 912        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R13 (26699).'
C
C --------------------------------------------------------
C ------------ R14 ----------------------------------------------
C
            count = 1
C
C            do 5023 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5023      continue
C
            DO 1018 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5)
C     &                                  .and.(r(change) .ne. R6)
C     &                                  .and.(r(change) .ne. R7)
C     &                                  .and.(r(change) .ne. R8)
C     &                                  .and.(r(change) .ne. R9)
C     &                                  .and.(r(change) .ne. R10)
C     &                                  .and.(r(change) .ne. R11)
C     &                                  .and.(r(change) .ne. R12)
C     &                                  .and.(r(change) .ne. R13) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1018
               endif
               r(CHANGE) = R14
               count = count + 1
C               write(*,*) 'Number of R14 = ', count-1
               IF ( count .EQ. 20808 ) THEN
                  GOTO 913
               ENDIF
 1018       CONTINUE
C
 913        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R14 (20807).'
C
C --------------------------------------------------------
C ------------ R15 ----------------------------------------------
C
            count = 1
C
C            do 5024 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5024      continue
C
            DO 1019 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5)
C     &                                  .and.(r(change) .ne. R6)
C     &                                  .and.(r(change) .ne. R7)
C     &                                  .and.(r(change) .ne. R8)
C     &                                  .and.(r(change) .ne. R9)
C     &                                  .and.(r(change) .ne. R10)
C     &                                  .and.(r(change) .ne. R11)
C     &                                  .and.(r(change) .ne. R12)
C     &                                  .and.(r(change) .ne. R13)
C     &                                  .and.(r(change) .ne. R14) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1019
               endif
               r(CHANGE) = R15
               count = count + 1
C               write(*,*) 'Number of R15 = ', count-1
               IF ( count .EQ. 12054 ) THEN
                  GOTO 914
               ENDIF
 1019       CONTINUE
C
 914        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R15 (12053).'
C
C --------------------------------------------------------
C ------------ R16 ----------------------------------------------
C
            count = 1
C
C            do 5025 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5025      continue
C
            DO 1020 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5)
C     &                                  .and.(r(change) .ne. R6)
C     &                                  .and.(r(change) .ne. R7)
C     &                                  .and.(r(change) .ne. R8)
C     &                                  .and.(r(change) .ne. R9)
C     &                                  .and.(r(change) .ne. R10)
C     &                                  .and.(r(change) .ne. R11)
C     &                                  .and.(r(change) .ne. R12)
C     &                                  .and.(r(change) .ne. R13)
C     &                                  .and.(r(change) .ne. R14)
C     &                                  .and.(r(change) .ne. R15) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1020
               endif
               r(CHANGE) = R16
               count = count + 1
C               write(*,*) 'Number of R16 = ', count-1
               IF ( count .EQ. 5993 ) THEN
                  GOTO 915
               ENDIF
 1020       CONTINUE
C
 915        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R16 (5992).'
C
C --------------------------------------------------------
C ------------ R17 ----------------------------------------------
C
            count = 1
C
C            do 5026 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5026      continue
C
            DO 1021 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5)
C     &                                  .and.(r(change) .ne. R6)
C     &                                  .and.(r(change) .ne. R7)
C     &                                  .and.(r(change) .ne. R8)
C     &                                  .and.(r(change) .ne. R9)
C     &                                  .and.(r(change) .ne. R10)
C     &                                  .and.(r(change) .ne. R11)
C     &                                  .and.(r(change) .ne. R12)
C     &                                  .and.(r(change) .ne. R13)
C     &                                  .and.(r(change) .ne. R14)
C     &                                  .and.(r(change) .ne. R15)
C     &                                  .and.(r(change) .ne. R16) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1021
               endif
               r(CHANGE) = R17
               count = count + 1
C               write(*,*) 'Number of R17 = ', count-1
               IF ( count .EQ. 4815 ) THEN
                  GOTO 916
               ENDIF
 1021       CONTINUE
C
 916        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R17 (4814).'
C
C --------------------------------------------------------
C ------------ R18 ----------------------------------------------
C
            count = 1
C
C            do 5027 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5027      continue
C
            DO 1022 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5)
C     &                                  .and.(r(change) .ne. R6)
C     &                                  .and.(r(change) .ne. R7)
C     &                                  .and.(r(change) .ne. R8)
C     &                                  .and.(r(change) .ne. R9)
C     &                                  .and.(r(change) .ne. R10)
C     &                                  .and.(r(change) .ne. R11)
C     &                                  .and.(r(change) .ne. R12)
C     &                                  .and.(r(change) .ne. R13)
C     &                                  .and.(r(change) .ne. R14)
C     &                                  .and.(r(change) .ne. R15)
C     &                                  .and.(r(change) .ne. R16)
C     &                                  .and.(r(change) .ne. R17) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1022
               endif
               r(CHANGE) = R18
               count = count + 1
C               write(*,*) 'Number of R18 = ', count-1
               IF ( count .EQ. 2795 ) THEN
                  GOTO 917
               ENDIF
 1022       CONTINUE
C
 917        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R18 (2794).'
C
C --------------------------------------------------------
C ------------ R19 ----------------------------------------------
C
            count = 1
C
C            do 5028 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
C              ransu(i)=rand(0)
C 5028      continue
C
            DO 1023 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
               change =int(ransu(i) * float(ip))
C               if ( (r(change) .ne. R20).and.(r(change) .ne. R1)
C     &                                  .and.(r(change) .ne. R2)
C     &                                  .and.(r(change) .ne. R3)
C     &                                  .and.(r(change) .ne. R4)
C     &                                  .and.(r(change) .ne. R5)
C     &                                  .and.(r(change) .ne. R6)
C     &                                  .and.(r(change) .ne. R7)
C     &                                  .and.(r(change) .ne. R8)
C     &                                  .and.(r(change) .ne. R9)
C     &                                  .and.(r(change) .ne. R10)
C     &                                  .and.(r(change) .ne. R11)
C     &                                  .and.(r(change) .ne. R12)
C     &                                  .and.(r(change) .ne. R13)
C     &                                  .and.(r(change) .ne. R14)
C     &                                  .and.(r(change) .ne. R15)
C     &                                  .and.(r(change) .ne. R16)
C     &                                  .and.(r(change) .ne. R17)
C     &                                  .and.(r(change) .ne. R18) ) then
               if ( r(change) .ne. R20 ) then
                  goto 1023
               endif
               r(CHANGE) = R19
C               write(*,*) 'Number of R19 = ', count-1
               count = count + 1
               IF ( count .EQ. 1616 ) THEN
                  GOTO 918
               ENDIF
 1023          CONTINUE
C
 918           CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R19 (1615).'
C
C --------------------------------------------------------
C ------------ R20 ----------------------------------------------
C
C            count = 1
C
CC            do 5029 i = 1,ip+ip+ip+ip+ip+ip+ip+ip+ip
CC              ransu(i)=rand(0)
CC 5029      continue
C
C            DO 1024 i = 1,IP+ip+ip+ip+ip+ip+ip+ip+ip
C               change =int(ransu(i) * float(ip))
C               if ( r(change) .eq. R20 ) then
C                  goto1024
C               endif
C               r(CHANGE) = R20
C               count = count + 1
C               IF ( count .EQ. 943 ) THEN
C                  GOTO 919
C               ENDIF
C 1024       CONTINUE
C
C 919        CONTINUE
C
            WRITE(*,*) count, ' PARTICLES CHANGED R20 (942).'
C
C --------------------------------------------------------
C
C            COUNT = 1
C            DO 2000 I = 1, IP+ip+ip+ip+ip
C              IF ((R(I).EQ.R1).and.(R(I).EQ.R2).and.(R(I).EQ.R3)
C     &                        .and.(R(I).EQ.R4).and.(R(I).EQ.R5)
C     &                        .and.(R(I).EQ.R6).and.(R(I).EQ.R7)
C     &                        .and.(R(I).EQ.R8).and.(R(I).EQ.R9)
C     &                        .and.(R(I).EQ.R10).and.(R(I).EQ.R11)
C     &                        .and.(R(I).EQ.R12).and.(R(I).EQ.R13)
C     &                        .and.(R(I).EQ.R14).and.(R(I).EQ.R15)
C     &                        .and.(R(I).EQ.R16).and.(R(I).EQ.R17)
C     &                        .and.(R(I).EQ.R18).and.(R(I).EQ.R19)
C     &                        .and.(R(I).EQ.R20)) THEN
C                COUNT = COUNT+1
C                XP(COUNT) = X(I)
C                YP(COUNT) = Y(I)
C              ENDIF
C 2000       CONTINUE
C
C            WRITE(*,*) count, ' ALL PARTICLES HAVE BEEN SELECTED.'
C
C --------------------------------------------------------
C
            WRITE(1,1050) TS, TP, TE, DT
C           WRITE(1,1050) R1, R2, R3, R4, R5, R6, R7, R8, R9, 
C     &                   R11, R12, R13, R14, R15, R16, R17, 
C     &                   R18, R19, R20, 
            WRITE(1,1050) RHO, MU, MUW
            WRITE(1,1000) ANG, PE, PNU
C
            WRITE(1,1200) IWALL, OS
C
            DO 40 I = 1, IWALL
               WRITE(1,1200)      I
               WRITE(1,1000)   XW(I),  YW(I)
               WRITE(1,1000) EWNX(I),EWNY(I)
               WRITE(1,1000) EWTX(I),EWTY(I)
 40         CONTINUE
C     
            WRITE(1,1200)   PXM,   PYM
            WRITE(1,1000) LENGX, LENGY
C            
C           WRITE(1,1200) count
            WRITE(1,1200) NIP
C
            DO 50 I=1,NIP
               WRITE(1,1100)    I
               WRITE(1,1000) Xp(I), Yp(I), r(i)
               WRITE(1,1000)    VX,    VY,   OMG
               IF(R(I).EQ.R01) THEN
                 NN01 = NN01+1
               ENDIF
               IF(R(I).EQ.R02) THEN
                 NN02 = NN02+1
               ENDIF
               IF(R(I).EQ.R03) THEN
                 NN03 = NN03+1
               ENDIF
               IF(R(I).EQ.R04) THEN
                 NN04 = NN04+1
               ENDIF
               IF(R(I).EQ.R05) THEN
                 NN05 = NN05+1
               ENDIF
               IF(R(I).EQ.R06) THEN
                 NN06 = NN06+1
               ENDIF
               IF(R(I).EQ.R07) THEN
                 NN07 = NN07+1
               ENDIF
               IF(R(I).EQ.R08) THEN
                 NN08 = NN08+1
               ENDIF
               IF(R(I).EQ.R09) THEN
                 NN09 = NN09+1
               ENDIF
               IF(R(I).EQ.R10) THEN
                 NN10 = NN10+1
               ENDIF
               IF(R(I).EQ.R11) THEN
                 NN11 = NN11+1
               ENDIF
               IF(R(I).EQ.R12) THEN
                 NN12 = NN12+1
               ENDIF
               IF(R(I).EQ.R13) THEN
                 NN13 = NN13+1
               ENDIF
               IF(R(I).EQ.R14) THEN
                 NN14 = NN14+1
               ENDIF
               IF(R(I).EQ.R15) THEN
                 NN15 = NN15+1
               ENDIF
               IF(R(I).EQ.R16) THEN
                 NN16 = NN16+1
               ENDIF
               IF(R(I).EQ.R17) THEN
                 NN17 = NN17+1
               ENDIF
               IF(R(I).EQ.R18) THEN
                 NN18 = NN18+1
               ENDIF
               IF(R(I).EQ.R19) THEN
                 NN19 = NN19+1
               ENDIF
               IF(R(I).EQ.R20) THEN
                 NN20 = NN20+1
               ENDIF

 50         CONTINUE
C
          WRITE(*,*) 'R01 = ', NN01
          WRITE(*,*) 'R02 = ', NN02
          WRITE(*,*) 'R03 = ', NN03
          WRITE(*,*) 'R04 = ', NN04
          WRITE(*,*) 'R05 = ', NN05
          WRITE(*,*) 'R06 = ', NN06
          WRITE(*,*) 'R07 = ', NN07
          WRITE(*,*) 'R08 = ', NN08
          WRITE(*,*) 'R09 = ', NN09
          WRITE(*,*) 'R10 = ', NN10
          WRITE(*,*) 'R11 = ', NN11
          WRITE(*,*) 'R12 = ', NN12
          WRITE(*,*) 'R13 = ', NN13
          WRITE(*,*) 'R14 = ', NN14
          WRITE(*,*) 'R15 = ', NN15
          WRITE(*,*) 'R16 = ', NN16
          WRITE(*,*) 'R17 = ', NN17
          WRITE(*,*) 'R18 = ', NN18
          WRITE(*,*) 'R19 = ', NN19
          WRITE(*,*) 'R20 = ', NN20
C
          CLOSE(1, STATUS = 'KEEP')
C          
 1000     FORMAT(3(1PE14.6))
 1050     FORMAT(4(1PE14.6))
 1100     FORMAT(2I7,2(1PE14.6))
 1200     FORMAT(3I7)
C
          STOP
          END
      















