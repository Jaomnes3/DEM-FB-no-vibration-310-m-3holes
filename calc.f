      subroutine calculation
C
      include 'demA.h'
      integer no1q,no2,tnoi, tnojq, yo
C
C *** SAME PARTITION ***
      do 1 pyq =  1, upy
      do 1 pxq =  1, upxq
         tnop = parttlq( pxq, pyq )
         do 2 no1q = 1, tnop-1
         do 2 no2 = no1q+1, tnop
            iq = partq( pxq, pyq, no1q)
            jq = partq( pxq, pyq, no2)
            call pp ( iq, jq )
 2       continue
 1    continue
C
C *** RIGHT ***
      do 11 pyq = 1, upy
      do 11 pxq = 1, upxq-1
         tnoi = parttlq( pxq, pyq )
         do 12 no1q = 1, tnoi
            tnojq = parttlq( pxq+1, pyq )
            do 13 no2 = 1, tnojq
               iq = partq( pxq,    pyq, no1q)
               jq = partq( pxq+1,  pyq, no2)
               call pp ( iq, jq )
 13         continue
 12      continue
 11   continue
C
C *** LEFT ***
      do 21 pyq = 1,upy
      do 21 pxq = 1+1 ,upxq
         tnoi = parttlq( pxq, pyq )
         do 22 no1q = 1, tnoi
            tnojq = parttlq( pxq-1, pyq )
            do 23 no2 = 1, tnojq
               iq = partq( pxq,    pyq, no1q)
               jq = partq( pxq-1,  pyq, no2)
               call pp ( iq, jq )
 23         continue
 22      continue
 21   continue
C
C *** UPPER ***
      do 31 pyq = 1, upy-1
      do 31 pxq = 1, upxq
         tnoi = parttlq( pxq, pyq )
         do 32 no1q = 1, tnoi
            tnojq = parttlq( pxq, pyq+1 )
            do 33 no2 = 1, tnojq
               iq = partq( pxq,  pyq, no1q)
               jq = partq( pxq,  pyq+1, no2)
               call pp ( iq, jq )
 33         continue
 32      continue
 31   continue
C
C *** LOWER ***
      do 41 pyq = 1+1, upy
      do 41 pxq = 1, upxq
         tnoi = parttlq( pxq, pyq )
         do 42 no1q = 1, tnojq
            tnojq = parttlq( pxq, pyq-1 )
            do 43 no2 = 1, tnojq
               iq = partq( pxq, pyq, no1q )
               jq = partq( pxq, pyq-1, no2 )
               call  pp (iq,jq)
 43         continue
 42      continue
 41   continue
C
C *** UPPER-RIGHT ***
      do 51 pyq = 1, upy-1
      do 51 pxq = 1, upxq-1
         tnoi = parttlq( pxq, pyq )
         do 52 no1q = 1, tnoi
            tnojq = parttlq( pxq+1, pyq+1 )
            do 53 no2 = 1, tnojq
               iq = partq( pxq,    pyq, no1q)
               jq = partq( pxq+1,  pyq+1, no2)
               call pp ( iq, jq )
 53         continue
 52      continue
 51   continue
C
C *** LOWER-RIGHT ***
      do 61 pyq = 1+1,upy
      do 61 pxq = 1,  upxq-1
         tnoi = parttlq( pxq, pyq )
         do 62 no1q = 1, tnoi
            tnojq = parttlq( pxq+1, pyq-1 )
            do 63 no2 = 1, tnojq
               iq = partq( pxq,    pyq, no1q)
               jq = partq( pxq+1,  pyq-1, no2)
               call pp ( iq, jq )
 63         continue
 62      continue
 61   continue
C
C *** UPPER-LEFT ***
      do 71 pyq = 1,  upy-1
      do 71 pxq = 1+1,upxq
         tnoi = parttlq( pxq, pyq )
         do 72 no1q = 1, tnoi
            tnojq = parttlq( pxq-1, pyq+1 )
            do 73 no2 = 1, tnojq
               iq = partq( pxq,   pyq, no1q)
               jq = partq( pxq-1, pyq+1, no2)
               call pp ( iq, jq )
 73         continue
 72      continue
 71   continue
C
C *** LOWER-LEFT ***
      do 81 pyq = 1,  upy-1
      do 81 pxq = 1,  upxq-1
         tnoi = parttlq( pxq, pyq)
         do 82 no1q = 1, tnoi
            tnojq = parttlq( pxq-1, pyq-1)
            do 83 no2 = 1, tnojq
               iq = partq( pxq,   pyq, no1q)
               jq = partq( pxq-1, pyq-1, no2)
               call pp ( iq, jq )
 83         continue
 82      continue
 81   continue
C
C *** 3 WALLS ***
C
      jq = 1
      pxq = 1
      do 91 pyq = 1, upy
         tnoi = parttlq( pxq, pyq )
         do 92 no1q = 1, tnoi
            iq = partq( pxq, pyq, no1q)
            call pw( iq, jq )
 92     continue
 91   continue
C
      jq = 2
      pyq = 1
      do 101 pxq = 1, upxq
         tnoi = parttlq( pxq, pyq )
         do 102 no1q = 1, tnoi
            iq = partq( pxq, pyq, no1q)
            call pw( iq, jq )
 102     continue
 101  continue
C
      jq = 3
      pxq = upxq      
      do 111 pyq = 1, upy
         tnoi = parttlq( pxq, pyq)
         do 112 no1q = 1, tnoi
            iq = partq( pxq, pyq, no1q)
            call pw( iq, jq )
 112     continue
 111  continue
C
      return
      end

