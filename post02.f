      subroutine post02
C
      include 'demA.h'
C
         open( unit = 1, err = 9, status='old', file = 'file02.dat' )
         close( unit = 1, status = 'delete')  
 9       open( unit = 1, err = 9000, status='new', file = 'file02.dat' )
C
      write(1,1000) tp, tq, te, dt
      write(1,1000) rq, rho, mu, muw
      write(1,1000) ang, pe, pnu
C
      write(1,*) iwall, os
      do 10 iq = 1, iwall
         write(1,*)      iq
         write(1,1000)   xw(iq),   yw(iq) 
         write(1,1000) ewnx(iq), ewny(iq)  
         write(1,1000) ewtx(iq), ewty(iq)  
 10   continue
C
      write(1,*)    upxq,    upy
      write(1,*) dpartxq, dpartyq
C
      write(1,*) ipq
      do 20 iq = 1, ipq
         write(1,*)  iq
         write(1,1000)    x(iq),    y(iq)
         write(1,1000)   vx(iq),   vy(iq), omgq(iq)
 20   continue
C
      do 30 iq = 1,ipq
         do 40 jq = 1,tnomax
            if( fn(iq,jq) .ne. 0. ) then
               write(1,1020) iq,jq,ftbl(iq,jq),fn(iq,jq),ft(iq,jq)
            endif
 40      continue
         do 50 jq = 1,iwall
            if( fwn(iq,jq) .ne. 0. ) then
               write(1,1030) iq,jq,fwn(iq,jq),fwt(iq,jq)
            endif
 50      continue
 30   continue
C
      close( 1 )
C
      return
C
 3    format( f8.5,4e16.7 )
 1000 format( 4e16.7 )
 1010 format( 3i16 ) 
 1020 format( ' 1   ',3i7,6e13.5 )
 1030 format( ' 0   ',2i7,'      0',6e13.5 )
 9000 write(9,*) 'open error file02.dat (unit=3)'
C
      stop
      end
