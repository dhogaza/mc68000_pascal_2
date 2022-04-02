{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1987, 1988, 1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Single Precision Real Logarithm Function

 Last modified by KRIS on 26-Nov-1990 13:56:39
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Single Precision Real Logarithm Function'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}
%include ffphdr;

{ 
 Algorithms, constants, ideas, ... are all directly from :
   "Software Manual for the Elementary Functions"
    William J. Cody, Jr. and William Waite
    Prentice-Hall 1980
}


procedure p_fextr(x: real;
                  var e: integer;
                  var f: real);

  var
    trix: RealTrix;


  begin
    with trix do
      begin
      r := x;
      with hack do
        begin
        e := exp - 126;
        exp := 126
        end;
      f := r;
      end;
  end;


function p_fln {( realval: real) : real} ;

  const
    c0 = sqrtHalf;
    c1 = point543;
    c2 = - 2.121944400546905827679E-4;
    a0 = - 0.64124943423745581147E+2;
    a1 = 0.16383943563021534222E+2;
    a2 = - 0.78956112887491257267E+0;
    b0 = - 0.76949932108494879777E+3;
    b1 = 0.31203222091924532844E+3;
    b2 = - 0.35667977739034646171E+2;

  var
    n: integer;
    xn: real;
    f: real;
    r: real;
    w: real;
    z: real;
    znum: real;
    zden: real;


  begin
    p_entlib;
    if realval <= 0.0 then p_logzne;
    p_fextr(realval, n, f);
    if f <= sqrtHalf then
      begin
      n := n - 1;
      znum := f - 0.5;
      zden := znum * 0.5 + 0.5;
      end
    else
      begin
      znum := (f - 0.5) - 0.5;
      zden := f * 0.5 + 0.5;
      end;
    z := znum / zden;
    w := z * z;
    r := w * ((a2 * w + a1) * w + a0) / (((w + b2) * w + b1) * w + b0);
    r := z + z * r;
    xn := n;
    p_fln := (xn * c2 + r) + xn * c1;
    p_exitlb;
  end;
