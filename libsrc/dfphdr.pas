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

  Header File for Double Precision Reals Sofware Emulation Routines

 Last modified by KRIS on 26-Nov-1990 14:10:02
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

{$double}

{ 
 Algorithms, constants, ideas, ... are all directly from :
   "Software Manual for the Elementary Functions"
    William J. Cody, Jr. and William Waite
    Prentice-Hall 1980
}

const
  eps = 5.551115123125783E-017;
  sqrtHalf = 0.7071067811865475244; {.55202 36314 77473 63110 oct}
  point543 = 0.693359375; {.543 octal }
  piInv = 0.31830988618379067154;

type
  RealTrix =
    record
      case integer of
        0: (r: real);
        1:
          (hack:
             packed record
               sign: 0..1;
               exp: 0..1024;
               frac:
                 packed record
                   r1: 0..15;
                   r2: array [1..6] of char;
                 end;
             end);
    end;

  cardinal = 0..maxint;


function p_datn(realval: real): real;
  external;


function p_dcos(realval: real): real;
  external;


function p_dexp(realval: real): real;
  external;


function p_dln(realval: real): real;
  external;


function p_dsin(realval: real): real;
  external;

{library utility routines}


procedure p_entlib;
  external;


procedure p_exitlb;
  external;


procedure p_logzne;
  external;
