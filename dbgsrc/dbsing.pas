{[l-,b+]}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1984 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045  Level: 1
  Processor: all
  System: all

  Single real operation funtions for Pascal-2 debugger.

 Last modified by KRIS on 26-Nov-1990 13:45:29
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}



{$nomain}

%include arccon;
%include hstcon;

function d$sglconv(i: integer; fpp: boolean): real;
  external;

function d$sglnegate(x: real; fpp: boolean): real;
  external;

function d$sgladd(x, y: real; fpp: boolean): real;
  external;  

function d$sglsub(x, y: real; fpp: boolean): real;
  external;

function d$sglmult(x, y: real; fpp: boolean): real;
  external;

function d$sglinto(x, y: real; fpp: boolean): real;
  external;




function d$sglconv;

  begin
    if fpc_available and fpp then d$sglconv := d$fpcnvs(i)
    else d$sglconv := i;
  end;

function d$sglnegate;

  begin
    if fpc_available and fpp then d$sglnegate := d$fpnegs(x)
    else d$sglnegate := - x;
  end;

function d$sgladd;
  
  begin
    if fpc_available and fpp then d$sgladd := d$fpadds(x, y)
    else d$sgladd := x + y;
  end;

function d$sglsub;

  begin
    if fpc_available and fpp then d$sglsub := d$fpsubs(x, y)
    else d$sglsub := x - y;
  end;

function d$sglmult;

  begin
    if fpc_available and fpp then d$sglmult := d$fpmuls(x, y)
    else d$sglmult := x * y;
  end;

function d$sglinto;

  begin
    if fpc_available and fpp then d$sglinto := d$fpins(x, y)
    else d$sglinto := x / y;
  end;

