9/6/19
�EFiles were somehow corrupted, so all files were copied and pasted into newly created files under the same names.
�EIntrinsic function "ifix" was replaced with "int" in response to warnings.
�EProgram can now run in LF95 without error or warning, and a calc.exe will be created.
       ... However, due to the absence of a "makefile.f" and the "infile" it creates, no output will be obtained.

9/9/19
�EFor "demA.h", all "$" were replaced with "&". 

�EBelow are the errors encountered after running DEM-FB in g95

$ g95 ./*.f
In file ./pp.f:13

      do 10 tno = 1,tnomax
            1
Error: REAL loop variable at (1) - to enable REAL loop parameters use -freal-loops
   ��Solved by running program with -freal-loops enabled in command prompt.
   �� g95 ,/*.f -freal-loops

In file ./pp.f:14

         if ( ftbl(iq,tno). eq .jq ) goto 11
                      1
Error: Array index at (1) must be of INTEGER type
  �� Variables iq, tno must be declared as integers, and presumably common to all other .f files.
  ��iq is a variable assigned to the integer partq(a,b).
  ��Solved by declaring tno as an integer in pp.f:6.

In file ./pw.f:59

            fwt(iq,jq) = sign(fricf,fwt(iq,jq))
                                    1
Error: 'b' argument of 'sign' intrinsic at (1) must be the same type and kind as 'a'
   ��Solved by converting arguments a,b into integers using the "int" function.
 
9/10/19
��Modifications to pp.f
 �EDeclared "tno" as an integer at pp.f:6.
 �EDeclared "fricf" as a real*8 at pp.f:14.

��General
 �EAll programs were compiled in g95(-freal-loops) without error, and a.exe was created.
 �EBy running a.exe, fort.60 and fort.9 were created; the latter of which noted the necessity of an "airinfile.dat".
     ��Simulation initially references a missing "airinfile.dat" for necessary data, and will likely output data in the form of "airfile.dat". 