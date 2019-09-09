      subroutine pw( iq, jq )
C
      include 'demA.h'
C
      real   ax, bx, xc, yc, d,
     &         dwx, dwy, dvn, dvt,   fpn, fpt,
     &         dpn, dpt, frict, bdt,  
     &         fpx, fpy,
     &         cn, ct2, bt, bn
C
      dx = xw(jq) - x(iq)
      dy = yw(jq) - y(iq)
C
      d = abs( dx * ewnx(jq) + dy * ewny(jq) )
C
      if ( rq .le. d ) then
         fwn(iq,jq) = 0.
         fwt(iq,jq) = 0.
         vwn(iq,jq) = 0.
         vwt(iq,jq) = 0.
         goto 999
      endif
C
      xc = x(iq) + d * ewnx(jq)
      yc = y(iq) + d * ewny(jq)
C
 11   continue
C
      dwx = (-vx(iq)+omgq(iq)*(yc-y(iq)))*dt
      dwy = (-vy(iq)-omgq(iq)*(xc-x(iq)))*dt
C
      dvn =     dwx * ewnx(jq) +   dwy * ewny(jq) 
      dvt =     dwx * ewtx(jq) +   dwy * ewty(jq) 
C
      vwn(iq,jq) = vwn(iq,jq) + dvn
      vwt(iq,jq) = vwt(iq,jq) + dvt
C
      kn = (pe/(1-pnu**2))*rq**0.5
     &                *(abs(vwn(iq,jq)))**0.5
      ks = 0.2*kn
C
      fpn = -dvn * kn
      fwn(iq,jq) = fwn(iq,jq) + fpn
      if( fwn(iq,jq) .le. 0. ) then
         fwn(iq,jq) = 0.
         fwt(iq,jq) = 0.
         vwn(iq,jq) = 0.
         vwt(iq,jq) = 0.
         goto 999
      else
         fpt = -dvt * ks
         fwt(iq,jq) = fwt(iq,jq) + fpt
         fricf = muw * fwn(iq,jq)
         if( abs(fwt(iq,jq)) .lt. fricf ) then
            bt = fwt(iq,jq)
            cn = 2. * sqrt( abs(kn * mass))
            ct2 = 2. * sqrt( abs(ks * mass))
         else
C	    fwt(iq,jq) = sign(fricf,fwt(iq,jq))
C Argument a, b のタイプを揃えるため、intを導入した。
            fwt(iq,jq) = sign(int(fricf),int(fwt(iq,jq)))
            bt = fwt(iq,jq)
            cn = 2. * sqrt( abs(kn * mass))
            ct2 = 0.
         endif
         dpn = -cn * dvn / dt
         dpt = -ct2 * dvt / dt
      endif
C
      bn = fwn(iq,jq)
C
      fpx = (bn+dpn)*ewnx(jq)+(bt+dpt)*ewtx(jq)
      fpy = (bn+dpn)*ewny(jq)+(bt+dpt)*ewty(jq)
C
      fx(iq) = fx(iq) - fpx
      fy(iq) = fy(iq) - fpy 
C
      m(iq) = m(iq) + fpx*(yc-y(iq)) - fpy*(xc-x(iq))
C
 999  return
      end


