C ***************
C ***  demA.h ***
C ***************
C
      parameter (l1=300000,l2=3,l3=13)
C
      common /d1/
     $   tq, ts, tp, dt, te, ftbl, rho, mu, muw, os, ang,  
     $   mass, inat, pe, pnu,  rq, outcnt, kn, ks,
     $   upxq,  fn, fwn, vn, vwn, dpartxq,     
     $   upy,  ft, fwt, vt, vwt, dpartyq, 
     $   partq, parttlq, tnomax,  
C
     $   iwall, xw, ewnx, ewtx,    
     $          yw, ewny, ewty, 
C
     $   ipq,    x, vx, omgq, fx, m,    
     $          y, vy,      fy
C
      integer 
     $   ipq,
     $   pxq,
     $   upxq,
     $   tnop,
     $   parttlq(259,2581),
     $   iwall,
     $   pyq,
     $   upy, tnomax,  partq(259,2581,l3), 
     $           outcnt,  ftbl(l1,32),
     $   ct,
     $   os
C
      real*8
     $   tq, ts, tp, dt, te, rho, mu, muw, ang,
     $   mass, inat, pe, pnu, rq, kn, ks,
     $   fn(l1,32), fwn(l1,32), vn(l1,32), vwn(l1,32),
     $   ft(l1,32), fwt(l1,32), vt(l1,32), vwt(l1,32),
C
     $   xw(l2), ewnx(l2), ewtx(l2), 
     $   yw(l2), ewny(l2), ewty(l2),  
C
     $   x(l1), vx(l1), omgq(l1), fx(l1), m(l1),
     $   y(l1), vy(l1),           fy(l1),
C
     $   dpartxq, dpartyq
C




