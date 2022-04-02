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

  Procedure declarations for fpc routines

 Last modified by KRIS on 26-Nov-1990 13:51:22
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}


function d$fpcnvs(i: integer): real;
  external;

function d$fpnegs(x: real): real;
  external;

function d$fpadds(x, y: real): real;
  external;  

function d$fpsubs(x, y: real): real;
  external;  

function d$fpmuls(x, y: real): real;
  external;  

function d$fpins(x, y: real): real;
  external;  


function d$fpcnvd(i: integer): real;
  external;

function d$fpnegd(x: real): real;
  external;

function d$fpaddd(x, y: real): real;
  external;  

function d$fpsubd(x, y: real): real;
  external;  

function d$fpmuld(x, y: real): real;
  external;  

function d$fpind(x, y: real): real;
  external;  


function d$fplesd(x, y: real): integer;
  external;

function d$fpleqd(x, y: real): integer;
  external;

function d$fpgred(x, y: real): integer;
  external;

function d$fpgeqd(x, y: real): integer;
  external;

