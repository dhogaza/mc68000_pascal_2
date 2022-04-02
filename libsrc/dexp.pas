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

  Double Precision Real Exponential Function

 Last modified by KRIS on 26-Nov-1990 14:00:28
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Double Precision Real Exponential Function'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}
{$double}
%include dfphdr;

{ 
 Algorithms, constants, ideas, ... are all directly from :
   "Software Manual for the Elementary Functions"
    William J. Cody, Jr. and William Waite
    Prentice-Hall 1980
}


function p_dadexp(x: real;
                  i: integer): real;

  var
    trix: RealTrix;


  begin
    with trix do
      begin
      r := x;
      with hack do exp := exp + i;
      p_dadexp := r;
      end;
  end;


function p_dexp {( realval: real) : real} ;

  const
    bigX = 7.090895657128E+002; { LN ( MaxReal ) }
    smallX = - 7.083964185323E+002; { LN ( MinReal ) }
    ln2Inv = 1.4426950408889634074; { 1 / LN (2) }
    c1 = point543;
    c2 = - 2.1219444005469058277E-4;
    p0 = 0.249999999999999993E+0;
    p1 = 0.694360001511792852E-2;
    p2 = 0.165203300268279130E-4;
    q0 = 0.500000000000000000E+0;
    q1 = 0.555538666969001188E-1;
    q2 = 0.495862884905441294E-3;

  var
    n: integer;
    xn: real;
    g: real;
    z: real;
    gpz: real;
    qz: real;
    rg: real;


  function intrnd(realval: real): integer;


    begin
      if realval >= 0 then intrnd := trunc(realval + 0.5)
      else intrnd := trunc(realval - 0.5);
    end;


  begin
    if realval < smallX then p_dexp := 0.0
    else if abs(realval) < eps then p_dexp := 1.0
    else
      begin
      if realval > bigX then realval := bigX;
      n := intrnd(realval * ln2Inv);
      xn := n;
      g := (realval - xn * c1) - xn * c2;
      z := g * g;
      gpz := ((p2 * z + p1) * z + p0) * g;
      qz := (q2 * z + q1) * z + q0;
      rg := 0.5 + gpz / (qz - gpz);
      n := n + 1;
      p_dexp := p_dadexp(rg, n);
      end;
  end;
