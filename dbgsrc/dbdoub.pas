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

  Double real operations for the Pascal-2 debugger.

 Last modified by KRIS on 26-Nov-1990 13:45:36
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{$nomain}

%include arccon;
%include hstcon;

function d$dblconv(i: integer; fpp: boolean): real;
  external;

function d$dblnegate(x: real; fpp: boolean): real;
  external;

function d$dbladd(x, y: real; fpp: boolean): real;
  external;

function d$dblsub(x, y: real; fpp: boolean): real;
  external;

function d$dblmult(x, y: real; fpp: boolean): real;
  external;

function d$dblinto(x, y: real; fpp: boolean): real;
  external;

function d$dblless(x, y: real; fpp: boolean): integer;
  external;

function d$dbllesseq(x, y: real; fpp: boolean): integer;
  external;

function d$dblgret(x, y: real; fpp: boolean): integer;
  external;

function d$dblgreteq(x, y: real; fpp: boolean): integer;
  external;

  function d$dblconv {(i: integer; fpp: boolean): real} ;
   {Convert an integer to a double precision real.}

    begin {d$dblconv}
        if fpc_available and fpp then 
          d$dblconv := d$fpcnvd(i)
        else
          d$dblconv := i;
    end; {d$dblconv}


  function d$dblnegate {(x: real; fpp: boolean): real} ;

    begin {d$dblnegate}
        if fpc_available and fpp then 
          d$dblnegate := d$fpnegd(x)
        else
          d$dblnegate := -x;
    end; {d$dblnegate}


  function d$dbladd {(x, y: real; fpp: boolean): real} ;

    begin {d$dbladd}
        if fpc_available and fpp then 
          d$dbladd := d$fpaddd(x, y)
        else
          d$dbladd := x + y;
    end; {d$dbladd}


  function d$dblsub {(x, y: real; fpp: boolean): real} ;

    begin {d$dblsub}
        if fpc_available and fpp then 
          d$dblsub := d$fpsubd(x, y)
        else
          d$dblsub := x - y;
    end; {d$dblsub}


  function d$dblmult{(x, y: real; fpp: boolean): real} ;

    begin {d$dblmult}
        if fpc_available and fpp then 
          d$dblmult := d$fpmuld(x, y)
        else
          d$dblmult := x * y;
    end; {d$dblmult}


  function d$dblinto {(x, y: real; fpp: boolean): real} ;

    begin {d$dblinto}
        if fpc_available and fpp then 
          d$dblinto := d$fpind(x, y)
        else
          d$dblinto := x / y;
    end; {d$dblinto}


  function d$dblless {(x, y: real; fpp: boolean): integer} ;

    begin {d$dblinto}
        if fpc_available and fpp then 
          d$dblless := d$fplesd(x, y)
        else
          d$dblless := ord(x < y);
    end; {d$dblinto}


  function d$dbllesseq {(x, y: real; fpp: boolean): integer} ;

    begin {d$dbllseeeq}
        if fpc_available and fpp then 
          d$dbllesseq := d$fpleqd(x, y)
        else
          d$dbllesseq := ord(x <= y);
    end; {d$dbllesseq}


  function d$dblgret {(x, y: real; fpp: boolean): integer} ;

    begin {d$dblgret}
        if fpc_available and fpp then 
          d$dblgret := d$fpgred(x, y)
        else
          d$dblgret := ord(x > y);
    end; {d$dblgret}


  function d$dblgreteq {(x, y: real; fpp: boolean): integer} ;

    begin {d$dblgreteq}
        if fpc_available and fpp then 
          d$dblgreteq := d$fpgeqd(x, y)
        else
          d$dblgreteq := ord(x >= y);
    end; {d$dblgreteq}

