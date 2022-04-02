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

  Double Precision Real Sine and Cosine Functions

 Last modified by KRIS on 26-Nov-1990 14:00:35
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Double Precision Real Sine and Cosine Functions'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}
{$double}
%include dfphdr;

{ 
 Algorithms, constants, ideas, ... are all directly from :
   "Software Manual for the Elementary Functions"
    William J. Cody, Jr. and William Waite
    Prentice-Hall 1980
}

const
  { for Sin and Cos }
  r1 = - 0.16666666666666665052E+00;
  r2 = 0.83333333333331650314E-02;
  r3 = - 0.19841269841201840457E-03;
  r4 = 0.27557319210152756119E-05;
  r5 = - 0.25052106798274584544E-07;
  r6 = 0.16058936490371589114E-09;
  r7 = - 0.76429178068910467734E-12;
  r8 = 0.27204790957888846175E-14;


function intrnd(realval: real): integer;


  begin
    if realval >= 0 then intrnd := trunc(realval + 0.5)
    else intrnd := trunc(realval - 0.5);
  end;


function p_dsin {( realval: real) : real} ;

  const
    xMax = 2.9815682E+8;
    c1 = 3.1416015625;
    c2 = - 8.908910206761537356617E-6;

  var
    f: real;
    g: real;
    r: real;
    n: cardinal;
    xn: real;
    negative: boolean;


  begin
    negative := realval < 0.0;
    realval := abs(realval);
    if realval > xMax then p_dsin := 0.0
    else
      begin
      n := intrnd(realval * piInv);
      xn := n;
      if odd(n) then negative := not negative;
      f := (realval - xn * c1) - xn * c2;
      if abs(f) < eps then r := f
      else
        begin
        g := f * f;
        r := (((((((r8 * g + r7) * g + r6) * g + r5) * g + r4) * g + r3) * g +
             r2) * g + r1) * g;
        r := f + f * r
        end;
      if negative then p_dsin := - r
      else p_dsin := r;
      end;
  end;


function p_dcos {( realval: real) : real} ;

  const
    yMax = 2.9815682E+8;
    halfpi = 1.57079632679489661923;
    c1 = 3.1416015625;
    c2 = - 8.908910206761537356617E-6;

  var
    y: real;
    f: real;
    g: real;
    r: real;
    n: cardinal;
    xn: real;
    negative: boolean;


  begin
    realval := abs(realval);
    y := realval + halfpi;
    if y > yMax then p_dcos := 0.0
    else
      begin
      n := intrnd(y * piInv);
      xn := n - 0.5;
      negative := odd(n);
      f := (realval - xn * c1) - xn * c2;
      if abs(f) < eps then r := f
      else
        begin
        g := f * f;
        r := (((((((r8 * g + r7) * g + r6) * g + r5) * g + r4) * g + r3) * g +
             r2) * g + r1) * g;
        r := f + f * r;
        end;
      if negative then p_dcos := - r
      else p_dcos := r;
      end;
  end;
