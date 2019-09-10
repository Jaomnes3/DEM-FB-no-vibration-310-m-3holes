# DEM-FB-no-vibration-310-m-3holes
Also named "DEM流動層A　振動なし(310μm3孔)".
9/6/19
・Files were somehow corrupted, so all files were copied and pasted into newly created files under the same names.
・Intrinsic function "ifix" was replaced with "int" in response to warnings.
・Program can now run in LF95 without error or warning, and a calc.exe will be created.
       ... However, due to the absence of a "makefile.f" and the "infile" it creates, no output will be obtained.

9/9/19
・For "demA.h", all "$" were replaced with "&". 

・Below are the errors encountered after running DEM-FB in g95

$ g95 ./*.f
In file ./pp.f:13

      do 10 tno = 1,tnomax
            1
Error: REAL loop variable at (1) - to enable REAL loop parameters use -freal-loops
In file ./pp.f:14

         if ( ftbl(iq,tno). eq .jq ) goto 11
                      1
Error: Array index at (1) must be of INTEGER type
In file ./pw.f:59

            fwt(iq,jq) = sign(fricf,fwt(iq,jq))
                                    1
Error: 'b' argument of 'sign' intrinsic at (1) must be the same type and kind as 'a'
   ⇒Solved by converting arguments a,b into integers using the "int" function.
 
9/10/19
◎Modifications to pp.f
 ・Declared "tno" as an integer at pp.f:6.
 ・Declared "fricf" as a real*8 at pp.f:14.

◎General
 ・All programs were compiled in g95(-freal-loops) without error, and a.exe was created.
 ・By running a.exe, fort.60 and fort.9 were created; the latter of which noted the necessity of an "airinfile.dat".
     →Simulation initially references a missing "airinfile.dat" for necessary data, and will likely output data in the form of "airfile.dat". 
