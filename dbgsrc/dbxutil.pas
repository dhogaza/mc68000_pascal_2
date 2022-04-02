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
  System: versados

  Operating system dependent routines for debugger executive.

 Last modified by KRIS on 26-Nov-1990 13:46:18
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}


procedure p_stdio(var inp, out: text);
  external;


{*************************************************************************

     Interrupt handling


*************************************************************************}


procedure d$recint;
    { This is the routine invoked when an interrupt is encoutered.  It
      sets global flag "interrupted", informs the target supervisor with
      the call xt_interrupt and re-initailizes the interrupt vector with
      "enable_cc". }


  begin {d$recint}
    if interrupted then d$finalend
    else
      begin
      interrupted := true;
      xt_intrrpt;
      end;
  end; {d$recint}


procedure catchinterrupts;
 {  Call the routine "enable_cc" to initialize the interrupt vector. }


  begin {catchinterrupts}
  end; {catchinterrupts}



procedure initstdinout;
    { redirect out and inp to standard output and standard input }

  begin
    p_stdio(inp, out);
  end;



function d$beingdebugged {: boolean};

  begin
    d$beingdebugged := false;
  end;


function d$maxfilesopen {: boolean};

  begin
    d$maxfilesopen := maxfilesopen;
  end;


procedure d$spawn {(var commandline: stringdescr)};
  {This is not implemented on versados}
  
  begin
    d$imesg(notacommand);
    writeln(out);
  end;
