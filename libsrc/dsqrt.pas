{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987, 1988, 1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Double Square Root routine.

 Last modified by KRIS on 26-Nov-1990 14:00:46
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Double Square Root routine.'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

{$double}


procedure p_sqrter; { "negative square root" error }
  external;


procedure p_fpperr; { "invalid operand" error }
  external;


function p_dadx(x: real;
                n: integer): real; { to add integer to exponent value of real }
  external;


function p_dintx(x: real): integer; { to return exponent value of real argument
                                     }
  external;


function p_dsqrt(x: real): real; { export name of double square root routine }
  external;


procedure p_entlib;
  external;


procedure p_exitlb;
  external;


function p_dsqrt; { define body of double square root routine }

  const
    c0 = 0.41731;
    c1 = 0.59016;
    half_root_2 = 0.70710678118654752440; { 1 / sqrt(2.0) }

  var
    f, y0, z, y2, y3: real;
    n: integer;

  begin { p_dsqrt }
    p_entlib;
    if not (x > 0.0) then

      if (x = 0.0) then p_dsqrt := x { preserves -0.0 }

      else { the "invalid" mask could be tested here }
      if (x < 0.0) then p_sqrter
      else p_fpperr

    else { +0.0 < x <= +oo }
      begin
      n := p_dintx(x);
      f := p_dadx(x, - n);

      y0 := c0 + c1 * f; { 7 bits }
      z := y0 + f / y0; { 15 bits }
      y2 := p_dadx(z, - 2) + f / z; { 31 bits }
      y3 := p_dadx(y2 + f / y2, - 1); { 63 bits }

      if odd(n) then
        begin
        n := n + 1;
        y3 := y3 * half_root_2;
        end;

      p_dsqrt := p_dadx(y3, n div 2);
      end;

    p_exitlb;
  end { p_dsqrt } ;
