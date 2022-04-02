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

  Interface routines between the Pascal-2 debugger and the running program.

 Last modified by KRIS on 26-Nov-1990 13:45:05
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}


function min(a, b: integer): integer;


  begin {min}
    if a < b then min := a
    else min := b;
  end; {min}

procedure d_addtoset(var s: watchidtype; i: integer);
  begin
    s := s + [i];
  end;

{***************************************************************************

  Target supervisor initialization

  Initialize the target supervisor, including the construction of a list
  of compilation unit descriptor records based on the module information
  passed from the compiler via the daignostics section.  This list is
  passed to the interpreter for its use in initialization, then the
  interpreter is called to prompt the user.

***************************************************************************}


procedure xt_inittarget {(var execfilename: pstringdescr; var unitlist: unitptr;
                         testing: boolean;
                         var globaladdr: addressrec;
        		 var stackseg: unsignedword);
                         var errmsg: xtmessage;
                         var errnum: integer)} ;

  var
    tmppc: addressrec;
    i: 0..maxstack;


  begin {xt_inittarget}
    errmsg := noerror;

    { Initialize global variables }
    active := true;  {this remains constant since this is embedded}
    processerror := false;  {this remains constant since embedded}
    initialized := false;
    breakpoints := nil;
    watchinglist := nil;
    datapacketfreelist := nil;
    for i := 0 to maxstack do packetstack[i] := nil;
    stepping := false;
    justproceeding := false;
    interrupted := false;
    looking := false;
    notskipping := true;
    terminalcondition := false;
    errorflag := false;
    levelchanged := false;
    newinterplevel := false;
    knownlevel := 1;
    currentlevel := 1;
    previouspc := register.pc;
    fppregsaved := false;
    fppinuse := false;
    if targetopsys = apollo then globalfp := d$globalfp
    else globalfp := 0;

    {Create the base frame for the "quick" frame list.}
    new(framelisthead);
    with framelisthead^ do
      begin
      fp := 0;
      pc.addr := 0;
      next := nil;
      prev := nil;
      end;

    getunitlist(unitlist);
    globaladdr := d$glbadr;
    if segmented then
      stacksegment := d$stkseg;
    stackseg := stacksegment;

    initialized := true;

  end; {xt_inittarget}





function getdynamiclevel: integer;
 {Return the number of dynamic level currently active.}

  type
    kptr = ^kludge;
    kludge =
      record
        case 0..1 of
          0: (fp: addressrec);
          1: (next: kptr);
      end;

  var
    k: kludge;
    level: integer;

    {$nopointercheck}

  begin {getdynamiclevel}

    k.fp.addr := register.fp;
    if segmented then k.fp.segment := stacksegment;
    level := 1;

    if initialized then
      begin
      while (k.fp.addr <> globalfp) do
        begin
        level := level + 1;
        k := k.next^;
        if segmented then k.fp.segment := stacksegment;
        end;
      if targetopsys = msdos then level := level - 1;
      end;

    getdynamiclevel := level;

  end; {getdynamiclevel}
{$pointercheck}




procedure d2$dbg2 {(action: actiontypes)} ;

  var
    b: breakpointrecptr;
    w: watchingrecptr;
    watchpointhit: boolean;
    breakpointhit: boolean;
    updatewatch: boolean;
    still_looking: boolean;
    newvalue: data_array;
    i: integer;
    tmppc: addressrec;


  begin {d$dbg}
    case action of

      initialize:
        begin
        tx_interpreter(register.pc, false, false, false,
                     false, interrupted, false, true);
        end;



{**************************************************************************

  Procedure entry

  Enter this at every procedure entry known to the debugger.  The level is
  incremented and flags are set to indicate that a new level has been
  entered.  Check the brakpoints list to see if any breakpoints are in this
  new procedure, and if so set the flag "looking" so that we start single
  stepping in the procedure.  If we are "justproceeding" then we stop "looking"
  since we are now in a higher level than before.

***************************************************************************}

      procedureentry:
        begin
        knownlevel := knownlevel + 1;
        levelchanged := true;
        newinterplevel := true;
        if justproceeding then looking := false;
        if breakpoints <> nil then
          begin
          b := breakpoints;
          while b <> nil do
            begin
            if (register.pc.addr > b^.entryaddr.addr) and
               (register.pc.addr < b^.exitaddr.addr) then
              begin
              b^.knownlevel := knownlevel;
              looking := true;
              end;
            b := b^.next;
            end;
          end;
        end;



{************************************************************************s

  Statement

  If the "looking" flag is on then this routine will be entered before
  every statement.  If there are breakpoints we want to determine if 
  a breakpoint has been encoutered and also whether the "looking" flag 
  should be kept on.  The watching list is checked for any modifications.  
  If warranted, the interpreter is called.

  Special precautions must be taken if "justproceeding" of if there are
  breakpoints. For breakpoints we need to check at each new level to 
  determine if a breakpoint is actually resident in that procedure.  
  For "justproceeding"  we need to determine if the new level is at or 
  below the original level at which the proceed began.  When a level is 
  being dropped, either by procedure exit of nonlocal goto, the debugger 
  is only informed just prior to the action so it does not then know the 
  new level into which it is entering.  For proceeding or breakpoints what
  is done is to turn on "looking" then a check must be made at the very
  next statement encoutered.  Hence the first action here is to turn off
  "looking" unless there are either watched variables ar the "stepping"
  flag is on.  It is then determined if we really want to turn it back on
  for the case of "justproceeding" or the existence of breakpoints.

***************************************************************************}

      statement:
        begin

          {Zero the "looking" flag unless we are really stepping or
           if we are watching.}
        if not stepping and (watchinglist = nil) then looking := false;

          {If the current dynamic level is still greater than that at the
           outset of the proceed command then we really don't want to be 
           "looking" until the next level change, otherwise set the "stepping"
           flag so the interpreter is called.}
        if justproceeding then
          if (proceedlevel < getdynamiclevel) then
            begin
            looking := false;
            stepping := false;
            end
          else stepping := true;

          {Call the interpreter to check the watched variables for those that
           may no longer be active, or for register variables that may have 
           ended up on the stack.}

        breakpointhit := false;
        if breakpoints <> nil then
          begin
          b := breakpoints;
          while b <> nil do
            begin
            if register.pc.addr = b^.addr.addr then breakpointhit := true
            else if (register.pc.addr > b^.entryaddr.addr) and
                    (register.pc.addr < b^.exitaddr.addr) then
              begin
              b^.knownlevel := knownlevel;
              looking := true;
              end;
            b := b^.next;
            end;
          end;

        watchpointhit := false;
        updatewatch := false;
        if watchinglist <> nil then
          begin
          watchfound := [];
          looking := true;
          w := watchinglist;
          if levelchanged then currentlevel := getdynamiclevel;
          while w <> nil do
            begin
            if levelchanged and (currentlevel < w^.level) then 
              updatewatch := true;
            if not (w^.store in [gen_reg, ptr_reg, real_reg, fpp_reg]) or 
                (w^.level = currentlevel) then
              with w^ do
                begin
                if fpc_available then 
                  if (w^.store = real_reg) then d$fppsav;
                fetch(store, watchlength, addr, newvalue);
                newvalue[1] := newvalue[1] and firstmask;
                newvalue[watchlength] := newvalue[watchlength] and lastmask;
                still_looking := true;
                i := 1;
                while still_looking and (i <= watchlength) do
                  begin
                  still_looking := (value[i] = newvalue[i]);
                  i := i + 1;
                  end;
                if not still_looking then
                  begin
{}
d_addtoset(watchfound, ident);
{
                  watchfound := watchfound + [ident];
}
                  watchpointhit := true;
                  end;
                end;
            w := w^.next;
            end;
          end;

        if stepping or breakpointhit or watchpointhit or interrupted or
            updatewatch then
              {need to determine if we have left scope of watched var}
          begin
          tx_interpreter(register.pc, breakpointhit, watchpointhit,
                         newinterplevel, terminalcondition, interrupted,
                         false, false);
          interrupted := false;
          newinterplevel := false;
          if watchinglist <> nil then currentlevel := getdynamiclevel;
          end;

        previouspc := register.pc;
        levelchanged := false;
        if fpc_available then 
          if fppregsaved then
            begin
            d$fppres;
            fppregsaved := false;
            end;

        end;



{**************************************************************************

  Procedure exit

  Enter here just prior to exit from a procedure.  Decrement the known level
  and set the flags to indicate that the level has changed.  If there are
  any breakpoints then set "looking" so that at the very next statement it
  can be determined if any breakpoints are in the procedure we are returning
  to.  If we are justproceeding and the new level raltes to the level at
  which the proceed started, then also set "looking".

***************************************************************************}

      procedureexit:
        begin
        knownlevel := knownlevel - 1;
        levelchanged := true;
        newinterplevel := true;
        if breakpoints <> nil then looking := true;
        leveldropped := true;
        if justproceeding and (steplevel = knownlevel) then looking := true;
        end;



{**************************************************************************

  Run time error

  Upon detection of an error, enter here.  If the error occurs before the
  debugger completed initialization, or if there are multiple errors, then
  abort.  Set the flags for a newlevel, error and termination, then
  call the interpreter.

***************************************************************************}

      error:
        begin
        if not initialized or errorflag then 
	  begin
          if targetopsys = msdos then d$clearexit;
	  exitst(1);
	  end;
        errorflag := true;
        levelchanged := true;
        newinterplevel := true;
        terminalcondition := true;
        tx_interpreter(register.pc, false, false, newinterplevel,
                       terminalcondition, interrupted, true, false);
        end;

{**************************************************************************

  Non local goto

  Just prior to a nonlocal goto the target program call the debugger here.
  Set the flags for a change in level, and set "looking" if there are any
  breakpoints or if we are justproceeding, so that it can be determined at
  the next statement whether a breakpoint is in the procedure, or if a
  proceed has been accomplished.

***************************************************************************}

      nonlocalgoto:
        begin
        levelchanged := true;
        newinterplevel := true;
        if (breakpoints <> nil) or justproceeding then looking := true;
        end;

{**************************************************************************

  Termination

  Upon termination abort if the debugger has not been able to complete
  initialization, otherwise set the flags fro termination and a new level
  then call the interpreter.

***************************************************************************}

      terminated:
        begin
        if not initialized then 
	  begin
          if targetopsys = msdos then d$clearexit;
	  exitst(1);
	  end;
        terminalcondition := true;
        levelchanged := true;
        newinterplevel := true;
        tx_interpreter(register.pc, false, false, true, true, interrupted,
                       false, false);
        end;

      end;

  end; {d$dbg}




procedure xt_setrunconditions {(xproceed, xhistory, xtrace,xstep: boolean) } ;
  {Set the flags and couters that will determine how the target program
   is to be executed.  If quit is set the exit immediately.}

  var
    b: breakpointrecptr;


  begin {xt_setrunconditions}
      steplevel := knownlevel;
      stepping := xproceed or xhistory or xtrace or xstep;
      justproceeding := xproceed and not (xhistory or xtrace or xstep);
      if justproceeding then proceedlevel := getdynamiclevel;
      looking := stepping or (watchinglist <> nil);
      if not looking and (breakpoints <> nil) then
        begin
        b := breakpoints;
        while b <> nil do
          begin
          if (register.pc.addr >= b^.entryaddr.addr) and
             (register.pc.addr < b^.exitaddr.addr) then
            looking := true;
          b := b^.next;
          end;
        end;
      if not looking and (breakpoints = nil) then notskipping := false;
      errorflag := false;
  end; {xt_setrunconditions}


procedure xt_start {(execargs: pstringlist;
                          var errmsg: xtmessage;
                          var errnum: integer)} ;
 {Restart the target program.}

  begin {xt_start}
    interrupted := false;
    terminalcondition := false;
    levelchanged := true;
    newinterplevel := true;
    if breakpoints <> nil then looking := true;
    if looking then notskipping := true
    else notskipping := false;
    d$rego;
  end;  {xt_start}


procedure xt_stop {(var errmsg: xtmessage;
                         var errnum: integer)} ;
  {Kill the program}

  begin  {xt_stop}
    terminalcondition := true;
    if targetopsys = vdos then p_term 
    else 
      begin
      if targetopsys = msdos then d$clearexit;
      exitst(1);
      end;
  end;  {xt_stop}


procedure xt_intrrpt;
 {Set flags when an interrupt has been encoutered.}


  begin
    interrupted := true;
    looking := true;
    notskipping := true;
  end;



{***************************************************************************

  Breakpoints maintenance

****************************************************************************}


procedure xt_getwatchinfo {(var xwatchfound: watchidtype; var xpreviouspc:
                           addressrec) } ;
  {Retrieve the set indicating the watched variables that were modified at
   the last statement.}


  begin {xt_getwatchinfo}
    xwatchfound := watchfound;
    xpreviouspc := previouspc;
  end; {xt_getwatchinfo}

procedure xt_setbreak {(addr, entryaddr, exitaddr: addressrec; var success:
                       boolean; errnum: integer)} ;
 {Create a new entry in the breakpoints list.}

  var
    b: breakpointrecptr;
    found: boolean;


  begin {xt_setbreak}
    b := breakpoints;
    found := false;
    while (b <> nil) and not found do
      begin
      if addr.addr = b^.addr.addr then found := true
      else b := b^.next;
      end;
    if not found then
      begin
      new(b);
      b^.addr := addr;
      b^.entryaddr := entryaddr;
      b^.exitaddr := exitaddr;
      b^.knownlevel := knownlevel - 1;
      b^.next := breakpoints;
      breakpoints := b;
      end;
  end; {xt_setbreak}


procedure xt_releasebreak {(addr: addressrec)} ;
 {Release a breakpoint identified by its address.}

  var
    b, bback: breakpointrecptr;


  begin {xt_releasebreak}
    b := breakpoints;
    while (b <> nil) and (b^.addr.addr <> addr.addr) do
      begin
      bback := b;
      b := b^.next
      end;
    if b <> nil then
      begin
      if b = breakpoints then breakpoints := breakpoints^.next
      else bback^.next := b^.next;
      dispose(b);
      end;
  end; {xt_releasebreak}



{**************************************************************************

  Stack frames construction

  Construct the "quick" frame list by starting from the current pc and 
  frame pointer and continuing down the frame pointers until a zero fp
  is encoutered.  The list is a doubly linked list, and the resulting
  pointer ends up pointing at the base (level 1) frame.

***************************************************************************}


procedure xt_getframes { (var frameslist: framelistptr; level: integer; var
                        errmsg: xtmessage; var errnum: integer) } ;

  type
    kludge =
      record
        fp: addressrange;
        pc: addressrec;
      end;
    kptr = ^kludge;

  var
    k: kptr;
    oldfp: addressrec;
    f: framelistptr;


  begin {xt_getframes}

    errmsg := noerror;
    level := 1;
    f := framelisthead;
    f^.fp := register.fp;
    f^.pc := register.pc;
    oldfp.addr := f^.fp;
    if segmented then oldfp.segment := stacksegment;

    while (oldfp.addr <> globalfp) do
      begin
      level := level + 1;
      if f^.next = nil then
        begin
        new(f^.next);
        f^.next^.next := nil;
        f^.next^.prev := f;
        end;
      f := f^.next;
      k := loophole(kptr, oldfp.ptr);
      f^.fp := k^.fp;
      f^.pc := k^.pc;
      oldfp.addr := f^.fp;
      end;

    if targetopsys = msdos then
      begin
      f := f^.prev;
      f^.next := nil;
      level := level - 1;
      end;
    framelist := f;

  end; {xt_getframes}
