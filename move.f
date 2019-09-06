      subroutine movep
C
      include 'demA.h'
C
      do 100 iq = 1, ipq
C
         cr = 0.
         ca = 0.
C
         vx(iq)  = ( vx(iq)*(1-cr)  + fx(iq)/mass*dt )/(1+cr)
         vy(iq)  = ( vy(iq)*(1-cr)  + fy(iq)/mass*dt )/(1+cr)
C     
         omgq(iq) = ( omgq(iq)*(1-ca) + m(iq)/inat*dt )/(1+ca)
C
         x(iq) = x(iq) + vx(iq)*dt
         y(iq) = y(iq) + vy(iq)*dt
C
 100  continue
C     
      return
      end
