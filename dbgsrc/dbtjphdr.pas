{[l-,b+]}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: all
  System: all

  Target supervisor initialization for embedded jump debuggers.

 Last modified by KRIS on 26-Nov-1990 13:51:16
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}


%include dbtprg;
%include arccon;
%include hstcon;
  
type

  { Action to be taken when a the target program enters the debugger. }

  actiontype = (initialize, procedureentry, procedureexit, statement, error,
                nonlocalgoto, terminated, procedurecall);

  breakpointrecptr = ^breakpointrec;

  breakpointrec =
    record
      next: breakpointrecptr;
      addr, {address of breakpoint}
       entryaddr, {entry address of procedure with breakpoint}
       exitaddr: addressrec; {exit address of procedure with breakpoint}
      knownlevel: integer; {visible dynamic level of breakpoint}
    end;

var

  entrypoint: addressrec;  {used to specify debugger or profiler entry}

    {These flags are used at entry into the debugger to determine whether or
     not to return immediately to the target program.}
  notskipping: boolean; {If false then debugger will only be entered upon
                         error, termination or interrupt}
  looking: boolean; {If false then debugger will not be entered at statement}
  terminalcondition: boolean; {has target program terminated}

  register: savedregisters; {registers saved at entry to debugger}

    {Do not touch the declarations up to this point without also modifying
     opdbg.}

  interrupted: boolean; {has an interrupt signal been intercepted}
  errorflag: boolean; {run-time error has been encoutered}

    {These flags are used to determine how the target program is to be 
     allowed to execute.  They are set by commands for the interpreter.}

  stepping: boolean; {is debugger in a single stepping mode}
  justproceeding: boolean; {proceeding without step, watch, trace or history}

    {The following are used to keep track of execution of the target
     program, and are used to determine whether to return to the
     interpreter or continue execution of the target program.}

  knownlevel: integer; {the current relative dynamic level known}
  proceedlevel: integer; {the actual dynamic level used to track a p command}
  steplevel: integer;
  currentlevel: integer; 
  leveldropped: boolean; {the dynamic level has dropped}
  levelchanged: boolean; {the dynamic level has changed since the last 
                          statement}
  newinterplevel: boolean; {let interpreter know that level changed since the
                            last entry to the interpreter as it will require 
                            a new stack frame}

  initialized: boolean; {has initialization been completed}

    {breakpoints list}
  breakpoints: breakpointrecptr;

    {frames list}
  globalfp: addressrange;  {value of frame poiinter a global level}

procedure d$rego;
  external;

procedure d$clearexit;
  external;


function d$glbadr: addressrec;
  external;


function d$stkseg: unsignedword;
  external;


function d$dataseg: unsignedword;
  external;


function d$globalfp: addressrange;
  external;


function p_stdia: addressrec;
  external;


function p_enddia: addressrec;
  external;


function p_getdia: unitinfoptr;
  external;


procedure d2$dbg2(action: actiontype);
  external;

