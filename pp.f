      subroutine pp( iq, jq )
C
      include 'demA.h'
C
C Defining tno as an integer in response to errors.
      integer tnojq, tno
C
      real   dx, xc, enx, etx, dvx, fpx,
     &         dy, yc, eny, ety, dvy, fpy,
     &         d,      dvn, fpn, dpn, 
     &         bdt,    dvt, fpt, dpt,
     &         bt, cn, ct2, bn
C Defining fricf as a real*8.
      real*8   fricf
C
      do 10 tno = 1,tnomax
         if ( ftbl(iq,tno). eq .jq ) goto 11

 10   continue
C
      do 20 tno = 1,tnomax
         if ( ftbl(iq,tno). eq .0 ) goto 12
 20   continue
C
      write(2,*) 'contact error'
      write(2,*) 't = ',tq
      write(2,*) ( ftbl(iq,tno),tno=1,32 )
      ct = 99999
      goto 998
C
 12   continue
      do 30 tnojq = 1,tnomax
         if ( ftbl(jq,tnojq). eq .iq ) goto 13
 30   continue
C
      ftbl(iq,tno) = jq
      fn(iq,tno) = 0.
      ft(iq,tno) = 0.
      vn(iq,tno) = 0.
      vt(iq,tno) = 0.
      goto 11
C
 13   continue
C
      ftbl(iq,tno) = jq
      fn(iq,tno) = fn(jq,tnojq)
      ft(iq,tno) = ft(jq,tnojq)
      vn(iq,tno) = vn(jq,tnojq)
      vt(iq,tno) = vt(jq,tnojq)
      ftbl(jq,tnojq) = 0
      fn(jq,tnojq) = 0.
      ft(jq,tnojq) = 0.
      vn(jq,tnojq) = 0.
      vt(jq,tnojq) = 0.
 11   continue
C
      dx = ( x(jq) - x(iq) )
      dy = ( y(jq) - y(iq) )
      d  = sqrt( dx**2 + dy**2 )
C
       if ( d .gt. rq*2.) then
         ftbl(iq,tno) = 0
         fn(iq,tno) = 0.
         ft(iq,tno) = 0.
         vn(iq,tno) = 0.
         vt(iq,tno) = 0.
         goto 999
      endif
C
      xc = ( x(iq) + x(jq) )/2.
      yc = ( y(iq) + y(jq) )/2.
C
      enx =  dx/d
      eny =  dy/d
      etx = -dy/d
      ety =  dx/d
C
      dvx = ( vx(jq) - vx(iq) + omgq(iq) * ( yc - y(iq))
     &                      - omgq(jq) * ( yc - y(jq))) * dt
      dvy = ( vy(jq) - vy(iq) - omgq(iq) * ( xc - x(iq))
     &                      + omgq(jq) * ( xc - x(jq))) * dt
C
      dvn =     dvx * enx +   dvy * eny 
      dvt =     dvx * etx +   dvy * ety 
C
      vn(iq,tno) = vn(iq,tno) + dvn
      vt(iq,tno) = vt(iq,tno) + dvt
C
      kn = (pe/(1-pnu**2))*(rq/2.)**0.5
     &                  *(abs(vn(iq,tno)))**0.5
      ks = 0.2*kn
C
      fpn = -dvn * kn
      fn(iq,tno) = fn(iq,tno) + fpn
      if ( fn(iq,tno) .lt. 0. ) then
         ftbl(iq,tno) = 0
         fn(iq,tno) = 0.
         ft(iq,tno) = 0.
         vn(iq,tno) = 0.
         vt(iq,tno) = 0.
         goto 999
      else
         fpt = -dvt * ks
         ft(iq,tno) = ft(iq,tno) + fpt
         fricf = mu * fn(iq,tno)
         if( abs(ft(iq,tno)) .lt. fricf ) then
            bt = ft(iq,tno)
            cn = 2. * sqrt(abs(kn * mass))
            ct2 = 2. * sqrt(abs(ks * mass))
         else
            ft(iq,tno) = sign(fricf,ft(iq,tno))
            bt = ft(iq,tno)
            cn = 2. * sqrt(abs(kn * mass))
            ct2 = 0.
         endif
         dpn = -cn * dvn / dt
         dpt = -ct2 * dvt / dt
      endif
C
      bn = fn(iq,tno)
C
      fpx = (bn+dpn)*enx+(bt+dpt)*etx
      fpy = (bn+dpn)*eny+(bt+dpt)*ety
C
      fx(iq) = fx(iq) - fpx
      fy(iq) = fy(iq) - fpy  
      m(iq) = m(iq) + fpx*(yc-y(iq))-fpy*(xc-x(iq))
C
      fx(jq) = fx(jq) + fpx
      fy(jq) = fy(jq) + fpy
      m(jq) = m(jq) - fpx*(yc-y(jq))+fpy*(xc-x(jq))
C
 999  return
 998  stop
      end
