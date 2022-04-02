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

  In-line fp-coprocessor double operations.

 Last modified by KRIS on 26-Nov-1990 13:45:46
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

{$nomain}

function d$fpcnvd {(i: integer): real} ;

  begin
    d$fpcnvd := i;
  end;


function d$fpnegd {(x: real): real};

  begin
    d$fpnegd := - x;
  end;


function d$fpaddd {(x, y: real): real} ;

  begin
    d$fpaddd := x + y;
  end;


function d$fpsubd {(x, y: real): real} ;

  begin
    d$fpsubd := x - y;
  end;


function d$fpmuld {(x, y: real): real} ;

  begin
    d$fpmuld := x * y;
  end;


function d$fpind {(x, y: real): real} ;

  begin
    d$fpind := x / y;
  end;



function d$fplesd {(x, y: real): integer} ;

  begin 
    d$fplesd := ord(x < y);
  end;


function d$fpleqd {(x, y: real): integer} ;

  begin 
    d$fpleqd := ord(x <= y);
  end;


function d$fpgred {(x, y: real): integer} ;

  begin 
    d$fpgred := ord(x > y);
  end;


function d$fpgeqd {(x, y: real): integer} ;

  begin 
    d$fpgeqd := ord(x >= y);
  end;


