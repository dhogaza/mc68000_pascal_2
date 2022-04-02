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

  Single Precision Real Arctangent Function

 Last modified by KRIS on 26-Nov-1990 13:56:54
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Single Precision Real Arctangent Function'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}
%include ffphdr;

{ 
 Algorithms, constants, ideas, ... are all directly from :
   "Software Manual for the Elementary Functions"
    William J. Cody, Jr. and William Waite
    Prentice-Hall 1980
}


function p_fatn {(realval: real) : real} ;

  const
    c0 = 0.26794919243112270647;
    sqrt3 = 1.7320508075688772901;
    p0 = - 0.13688768894191926929E+2;
    p1 = - 0.20505855195861651981E+2;
    p2 = - 0.84946240351320683534E+1;
    p3 = - 0.83758299368150059274E+0;
    q0 = 0.41066306682575781263E+2;
    q1 = 0.86157349597130242515E+2;
    q2 = 0.59578436142597344465E+2;
    q3 = 0.15024001160028576121E+2;

  var
    f: real;
    g: real;
    r: real;
    n: integer;


  begin
    f := abs(realval);
    if f > 1.0 then
      begin
      f := 1.0 / f;
      n := 2;
      end
    else n := 0;
    if f > c0 then
      begin
      f := ((((sqrt3 - 1.0) * f - 0.5) - 0.5) + f) / (sqrt3 + f);
      n := n + 1;
      end;
    if abs(f) < eps then r := f
    else
      begin
      g := f * f;
      r := (g * (((p3 * g + p2) * g + p1) * g + p0)) / ((((g + q3) * g + q2) *
           g + q1) * g + q0);
      r := f + f * r;
      end;
    if n > 1 then r := - r;
    case n of
      0: ;
      1: r := r + 0.52359877559829887308;
      2: r := r + 1.57079632679489661923;
      3: r := r + 1.04719755119659774615;
      end;
    if realval < 0.0 then p_fatn := - r
    else p_fatn := r;
  end;
