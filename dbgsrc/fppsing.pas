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

  In fp co-processor single operations

 Last modified by KRIS on 26-Nov-1990 13:45:43
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

{$nomain}

function d$fpcnvs {(i: integer): real} ;

  begin
    d$fpcnvs := i;
  end;


function d$fpnegs {(x: real): real};

  begin
    d$fpnegs := - x;
  end;


function d$fpadds {(x, y: real): real};

  begin
    d$fpadds := x + y;
  end;


function d$fpsubs {(x, y: real): real} ;

  begin
    d$fpsubs := x - y;
  end;


function d$fpmuls {(x, y: real): real} ;

  begin
    d$fpmuls := x * y;
  end;


function d$fpins {(x, y: real): real} ;

  begin
    d$fpins := x / y;
  end;

