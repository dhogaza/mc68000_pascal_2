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

  Debugger executive to target supervisor filter routines

 Last modified by KRIS on 26-Nov-1990 13:46:11
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}


procedure tx_interpreter {(xcurrentpc: addressrec;
                         xbreakpointhit: boolean;
                         xwatchpointhit: boolean;
                         xnewlevel: boolean;
                         xterminalcondition: boolean;
                         xcontrol_c: boolean;
                         xerrorencountered: boolean;
                         xinitialize: boolean)} ;
  { This routine provides a jacket between the target supervisor and the
    interpreter main driver.  Upon entry a series of flags is set that
    will describe the current state of execution of the target program,
    after which the interpreter is called. }


  begin {xt_interpreter}
    if embeddedjump then
      begin
      if xinitialize then
        begin
        initexecutive;
        d$initinterp;
        end;
      interrupted := xcontrol_c;
      watchpointhit := xwatchpointhit;
      breakpointhit := xbreakpointhit;
      programdead := xterminalcondition or xerrorencountered;
      currentpc := xcurrentpc;
      if watchpointhit then xt_getwatchinfo(watchfound, previouspc);
      if xnewlevel then framesupdated := false
      else if framesupdated then currentstackframe^.pc := currentpc;
      if (watchlist <> nil) and xnewlevel then 
        d$updatewatchlist;
      if breakpointhit or stepping or proceeding or tracing or history or 
          interrupted or watchpointhit or programdead or xinitialize then
        d$interpreter;
      end;
  end;

{*****************************************************************************

    Target initialization

*****************************************************************************}

procedure d$inittarget {(var units: unitptr: unitptr)}; 

  var
    errnum: integer;
    errmsg: xtmessage;
    err: imessage;

  begin  {d$inittarget}
    errmsg := noerror;
    xt_inittarget(execname, units, testflg,
                 globaldatabase, stacksegment, errmsg, errnum);
    if errmsg <> noerror then 
      begin
      processerror(errmsg, errnum, err);
      writeln(out); writeln(out);
      d$iwarn(err);
      writeln(out); writeln(out);
      end;
  end;  {d$inittarget}
                        



{*****************************************************************************

    Execution control

*****************************************************************************}

procedure d$kill;

 {Stop the execution of the program being debugged.}

  var
    errmsg: xtmessage;
    errnum: integer;
    err: imessage;


  begin {d$kill}
    errmsg := noerror;
    xt_stop(errmsg, errnum);
      {In embedded debuggers we do not return}
    if errmsg <> noerror then processerror(errmsg, errnum, err);
    active := false;
  end;  {d$kill}


procedure d$restart;

 {Restart (or start for the first time) the program being debugged.}

  const
    ourname = 'restart';
  var
    errmsg: xtmessage;
    errnum: integer;
    err: imessage;

  begin {d$restart}
    if separateprocess then
      begin
      runtimeerror := false;
      if active then d$kill;
      end;
    xt_start(execargs, errmsg, errnum);
    active := true;
    if errmsg <> noerror then processerror(errmsg, errnum, err);
  end; {d$restart}


procedure d$finalend;
 { Instruct the target supervisor to stop the program.  In embedded debuggers
   this will kill the entire debugging process.  If the host runs as a 
   separate process, then d$signoff is called to end that process. }

  begin
    d$kill;
    if separateprocess or remote then d$signoff;
  end;


procedure d$continue;

 {Continue program from the current location.}

  var
    tmppc: addressrec;
    flg: integer;
    errmsg: xtmessage;
    errnum: integer;
    err: imessage;


  begin {d$continue}
    if separateprocess then
      begin
      runtimeerror := false;
      xt_continue(currentpc, breakpointhit, exited, errmsg, errnum);
      if errmsg <> noerror then processerror(errmsg, errnum, err);
      if exited then programdead := true;
      framesupdated := false;
      end;
  end; {continue}


procedure d$tracesteps {(startpc, endpc: addressrec;
                      var returnpc: addressrec)} ;

 {Machine single-step until outside of the pc boudaries.}

  var
    tmppc: addressrec;
    currentsegment: integer;
    errmsg: xtmessage;
    errnum: integer;
    err: imessage;


  begin {d$tracesteps}
    if not embeddedjump then
      begin
      if segmented then currentsegment := currentpc.segment;
      runtimeerror := false;
      xt_tracesteps(startpc, endpc, currentpc, returnpc, breakpointhit, exited,
                    errmsg, errnum);
      if errmsg <> noerror then processerror(errmsg, errnum, err);
      if exited then programdead := true;
      end;
  end; {tracesteps}


procedure d$trapcontinue {(inward:boolean)} ;

  {Establish a net of potential locations for a single step to end up,
   then continue the process until such a breakpoint is encountered.}

  var
    depth, level: integer;
    framelist: framelistptr;
    flg: integer;
    errmsg: xtmessage;
    errnum: integer;
    err: imessage;


procedure setreturntraps;

  {Set traps at all the locations where a single step may go when it steps
   out into the void.}

  var
    s, sdyn: stackpointer;
    success: boolean;
    errmsg: xtmessage;
    errnum: integer;
    err: imessage;


  begin {setreturntraps}
    if not embeddedjump then
      begin
      xt_settrapmod(errmsg, errnum);
      if errmsg <> noerror then processerror(errmsg, errnum, err);
      if not firsttime and (currentstackframe^.proc <> nil) then
      {set a trap at the return pc}
        begin
        xt_settmpbreak(currentstackframe^.pc, errmsg, errnum);
        if errmsg <> noerror then processerror(errmsg, errnum, err);
        end
      else
        begin
        s := currentstackframe^.dynamiclink;
        while (s <> nil) and (s^.proc = nil) do s := s^.dynamiclink;
        if s <> nil then
          begin
          xt_settmpbreak(s^.pc, errmsg, errnum);
          if errmsg <> noerror then processerror(errmsg, errnum, err);
          end;
        end;
      s := currentstackframe^.staticlink;
      while s <> nil do
        begin
        if s^.proc = nil then
          begin
          sdyn := s^.dynamiclink;
          while (sdyn <> nil) and (s^.proc = nil) do sdyn := sdyn^.dynamiclink;
          if s <> nil then
            begin
            xt_settmpbreak(sdyn^.pc, errmsg, errnum);
            if errmsg <> noerror then processerror(errmsg, errnum, err);
            end;
          end;
        s := s^.staticlink;
        end;
      end;
  end; {setreturntraps}


  begin {d$trapcontinue}

    if not embeddedjump then
      begin
      runtimeerror := false;
      if inward then
        begin
        depth := currentstackframe^.dynamiclevel;
        errmsg := noerror;
        repeat
          setreturntraps;
          xt_continue(currentpc, breakpointhit, exited, errmsg, errnum);
          if errmsg = noerror then 
            xt_getframes(framelist, level, errmsg, errnum);
        until breakpointhit or (level <= depth) or (framelist = nil) or
              (errmsg <> noerror) or exited;
        if errmsg <> noerror then processerror(errmsg, errnum, err);
        if exited then programdead := true;
        end

      else
        begin
        setreturntraps;
        xt_continue(currentpc, breakpointhit, exited, errmsg, errnum);
        if errmsg <> noerror then processerror(errmsg, errnum, err);
        if exited then programdead := true;
        end;
  
      framesupdated := false;
      end;

  end; {d$trapcontinue}


procedure d$singlestep;

  {Perform a pascal single step.  In this case a single step has been 
   completed, so this is just an instruction to check the watched 
   variables list for modifications.  This is required since performing a
   Pascal single statement step may require many transactions between the
   host and target.  The check on watched variables is only made after the
   step has been completed, and only the host can determine that.}

  var
    errmsg: xtmessage;
    errnum: integer;
    err: imessage;


  begin {d$singlestep}
    if not embeddedjump then
      begin
      xt_singlestep(watchpointhit, errmsg, errnum);
      if errmsg <> noerror then processerror(errmsg, errnum, err)
      else if watchpointhit then xt_getwatchinfo(watchfound, previouspc); 
      end;
  end; {d$singlestep}


procedure d$setrunconditions;


  begin
    if embeddedjump then
      xt_setrunconditions(stepping, history, tracing, proceeding);
  end;




{*****************************************************************************

    Stack frames 

*****************************************************************************}


procedure d$getframes {(framelist: framelistptr; level: integer)} ;
  { Reaquest the "quick" frames list. }

  var
    errmsg: xtmessage;
    errnum: integer;
    err: imessage;

  begin  {d$getframes}
    errmsg := noerror;
    xt_getframes(framelist, level, errmsg, errnum);
    if (errmsg <> noerror) then processerror(errmsg, errnum, err);
  end;  {d$getframes}



{*****************************************************************************

    Data access

*****************************************************************************}

procedure d$dataaccess {(datatokenlist: datatokenptr; var packet: datapacketptr;
                      var err: imessage; var errnum: integer)} ;

 {Invoke the target expression evaluation machine with a list of data tokens.}

  var
    d: datatokenptr;
    errnum: integer;
    errmsg: xtmessage;


  begin {d$dataaccess}
    errmsg := noerror;
    errnum := 0;
    xt_dataaccess(datatokenlist, packet, errmsg, errnum);
    if (errmsg <> noerror) then
      begin
      processerror(errmsg, errnum, err);
      if errmsg = processerr then success := false
      else if errnum < 0 then
        begin
        success := false;
        location := - errnum;
        end
      else 
        begin
        location := errnum;
        success := true;
        end;
      end
    else 
      begin
      success := true;
      err := notanerror;
      end;
  end; {d$dataaccess}



{*****************************************************************************

    Breakpoint and watch point control

*****************************************************************************}


procedure d$setbreak {(addr, entryaddr, exitaddr: addressrec)} ;

 {Set a breakpoint.}

  var
    errmsg: xtmessage;
    errnum: integer;
    err: imessage;


  begin
    errmsg := noerror;
    xt_setbreak(addr, entryaddr, exitaddr, errmsg, errnum);
    if errmsg <> noerror then processerror(errmsg, errnum, err);
  end;


procedure d$releasebreak {(addr: addressrec; var errmsg: xtmessage; var errnum:
                        integer)} ;

 {Release the breakpoint at addr.}

  var
    errmsg: xtmessage;
    errnum: integer;
    err: imessage;


  begin
    errmsg := noerror;
    xt_releasebreak(addr, errmsg, errnum);
    if errmsg <> noerror then processerror(errmsg, errnum, err);
  end;


procedure d$watchrelease{(watchid: watchidrange; errmsg: xtmessage; errnum:
                        integer) } ;

 {Release the watch identified by watchid.}

  var
    errmsg: xtmessage;
    errnum: integer;
    err: imessage;


  begin
    errmsg := noerror;
    xt_wtchrelease(watchid, errmsg, errnum);
    if errmsg <> noerror then processerror(errmsg, errnum, err);
  end;


procedure d$watchlistrelease{(errmsg: xtmessage; errnum: integer)} ;

 {Release the complete watch list.}

  var
    errmsg: xtmessage;
    errnum: integer;
    err: imessage;


  begin
    errmsg := noerror;
    xt_wtchlstrelease(errmsg, errnum);
    if errmsg <> noerror then processerror(errmsg, errnum, err);
  end;


procedure d$wrtnonpascaladdr {(addr: addressrec)} ;

  var
    symdescr: stringdescr;
    errmsg: xtmessage;
    errnum: integer;
    err: imessage;
  begin
    if separateprocess then
      begin
      errmsg := noerror;
      errnum := 0;
      xt_linksymbol(addr, symdescr, errmsg, errnum);
      if errmsg <> noerror then processerror(errmsg, errnum, err)
      else writeln(out, symdescr.txt: symdescr.len);
      end;
  end;


procedure d$setsigblock {(sigblock: sigmask)};

  begin  {d$setsigblock}
    if targetopsys = unix then xt_setsigblock(sigblock);
  end;  {d$setsigblock}


procedure d$getsigmask {(sigmask: sigmask)};

  begin  {d$getsigmask}
    if targetopsys = unix then xt_getsigmask(sigmask);
  end;  {d$getsigmask}

