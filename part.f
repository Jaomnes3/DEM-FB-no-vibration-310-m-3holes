      subroutine partition
C
      include 'demA.h'
C
      do 10 pxq = 1, upxq
      do 10 pyq = 1, upy
         parttlq( pxq, pyq ) = 0
         do 11 tnop = 1, 10
            partq(pxq, pyq, tnop)= 0
 11      continue
 10   continue
C
      tnop = 0
C
      do 30 iq = 1, ipq
         pxq = int( x(iq) / dpartxq ) + 1
         pyq = int( y(iq) / dpartyq ) + 1
         tnop = parttlq( pxq, pyq) + 1
         parttlq( pxq, pyq ) = tnop
         partq( pxq, pyq,  tnop ) =  iq
 30   continue
C
      return
      end
