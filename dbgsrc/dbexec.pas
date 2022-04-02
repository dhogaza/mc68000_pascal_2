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

  Pascal-2 debugger interpreter program execution routines.

 Last modified by KRIS on 26-Nov-1990 13:46:41
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

{************************************************************************

     ------ Procedure Stack Frame List Construction

  The stackframe list is a linked list which abstracts the actual 
  stack frames structure of the running program.  The frame list is
  linked by dynamic links down to the main (base level) frame.  Static
  links are also maintained in this list.  Each frame entry maintains
  a certain amount of data about the particular frame, including the
  pc (either the current pc for the active frame, or the return pc for
  the stacked frames), the frame pointer, the dynamic level and the
  module.  A link is also made to the procedure tree entry for that 
  procedure, which makes more information available.  The target
  supervisor will inform if the frame list needs to be rebuilt, and
  the global flag FRAMESUPDATED is set accordingly.  The frames are
  rebuilt by first getting from the target supervisor a "quick" frame
  list, which contains just the return pc and frame pointer for each
  frame.  As an optimization, the existing frame list and the new 
  "quick" frame list are compared (pc and fp), starting from the base 
  level.  The point of divergence determines where to start rebuilding
  the frame list.  The frame list is only updated when required, 
  e.g. to do variable searches, since these have to proceed down the
  static links.

************************************************************************}


procedure d$frameupdate;

  const
    ourname = 'frameupdate';
  var
    proc, stmt: mapindex;
    mdl: mdl_pointer;
    maprec: stmtrecord;
    symrec: debugrecord;
    stackmark: stackpointer;
    s: stackpointer;
    i, j: integer;
    done: boolean;


  procedure mergeframes;

  {Compare the pc and fp from the quick frame list with those of the 
   existing stack frame list to determine the point of divergnece, 
   which is returned as "stackmark".}

    var
      f: framelistptr;
      s, mark: stackpointer;
      limit: integer;
      i, k: integer;


    begin {mergeframes}
      d$getframes(f, framelistlevel);
      s := globalstackframe;
      limit := d$min(framelistlevel, currentstackframe^.dynamiclevel);
      i := 1;
      if f = nil then
        begin
        writeln('Unable to recover stack frame');
        currentstackframe := globalstackframe;
        stackmark := nil;
        end
      else
        begin
        while (f^.pc.addr = s^.pc.addr) and (f^.fp = s^.fp) and (i < limit) do
          begin
          i := i + 1;
          if s^.uplink = nil then
            begin
            new(s^.uplink);
            s^.uplink^.uplink := nil;
            s^.uplink^.dynamiclink := s;
            if needcaching then s^.uplink^.proc := d$getproctreerec;
            end;
          s := s^.uplink;
          f := f^.prev;
          end;
        stackmark := s;
        {work around 68k compiler bug}
        s := stackmark;
        s^.pc := f^.pc;
        s^.fp := f^.fp;
        for k := (i + 1) to framelistlevel do
          begin
          if s^.uplink = nil then
            begin
            new(s^.uplink);
            s^.uplink^.uplink := nil;
            s^.uplink^.dynamiclink := s;
            if needcaching then s^.uplink^.proc := d$getproctreerec;
            end;
          s := s^.uplink;
          f := f^.prev;
          s^.pc := f^.pc;
          s^.fp := f^.fp;
          s^.dynamiclevel := k;
          end;
        currentstackframe := s;
        end;

    end; {mergeframes}


  begin {d$frameupdate}

    mergeframes;

    if currentstackframe = nil then
      begin
      writeln(out, 'cannot identify stack frames');
      currentstackframe := globalstackframe;
      end

    else
      while (stackmark <> nil) and
            (currentstackframe^.dynamiclevel >= stackmark^.dynamiclevel) do
        with stackmark^ do
          begin
          if not d$locateaddr(pc, mdl, proc, stmt, true) then
            begin
            database := 0;
            if needcaching then
              if proc <> nil then
                begin
                d$freeproctreerec(proc);
                proc := nil;
                end;
            end
          else
            begin
            if needcaching then 
              begin
              if proc = nil then proc := d$getproctreerec;
              proc^ := proctreefile^;
              end;
            if not embeddedjump then
            {determine the pc of the next pascal statement}
              begin
              if not d$readmap(mdl, stmt, maprec) then choke(ourname);
              if (d$reladdr(mdl, pc) = maprec.pc) or maprec.exit then
                nextstmtpc := pc
              else
                begin
                if not d$readmap(mdl, stmt + 1, maprec) then
                  choke(ourname);
                nextstmtpc := d$absaddr(mdl, maprec.pc);
                end;
              end;
            if not d$readsym(mdl, proc^.firstsym + 1, symrec) then
              choke(ourname);
            lexiclevel := symrec.level;
            if lexiclevel > 1 then database := fp
            else database := mdl^.data_base;
            end;

          if dynamiclink <> nil then
            dynamiclevel := dynamiclink^.dynamiclevel + 1;

          { determine the static links}
          if (proc = nil) or (dynamiclink = nil) then staticlink := nil
          else
            begin
            s := dynamiclink;
            done := false;
            staticlink := nil;
            while not done and (s <> nil) do
              if (s^.proc <> nil) and (mdl = s^.mdl) and
                  (proc^.staticidx = s^.proc^.firstsym) then
                begin
                staticlink := s;
                done := true;
                end
              else s := s^.dynamiclink;
           if staticlink = nil then staticlink := mdl^.base_frame;
           end;

{
              if mdl <> s^.mdl then
                begin
                staticlink := mdl^.base_frame;
                done := true;
                end
              else if s^.proc = nil then s := s^.dynamiclink
              else
                begin
                j := s^.lexiclevel;
                for i := lexiclevel to j do if s <> nil then s := s^.staticlink;
                staticlink := s;
                done := true;
                end;
            if (s = nil) then staticlink := nil;
            end;
}

          stackmark := stackmark^.uplink;
          end;

    framesupdated := true;

  end; {d$frameupdate}



{************************************************************************

     ------ Stepping Record

  This is used in patching types of debuggers to help determine how a
  pascal single step is to be executed.  Essentially a record is kept
  of certain key addresses for quick reference during the process of
  executing a single step.

************************************************************************}


procedure updatestepparams;

 {Update the step record for the current statement}

  const
    ourname = 'updatestepparams';
  var
    i: integer;
    rec, tmprec: stmtrecord;
    found: boolean;
    proc_exit: boolean;
    tmpaddr: addressrec;
    pc: addressrec;
    currentcontext: stackpointer;


  begin { updatestepparams }
    if not embeddedjump then
      begin
      currentcontext := d$firstvisibleframe;
      with currentcontext^, steprec do
        begin
        stmtentrypc := pc.addr;
        segment := pc.segment;
        if (pc.addr <> stmtexitpc) or (segmented and
           (pc.segment <> segment)) then
          if d$locateaddr(pc, mdl, proc, stmt, true) then
            begin
            if needcaching then
              begin
              if proc = nil then proc := d$getproctreerec;
              proc^ := proctreefile^;
              end
            end
          else
            if needcaching then
              if proc <> nil then
                begin
                d$freeproctreerec(proc);
                proc := nil;
                end;

      {determine the procedure boundaries}
        if not d$addrinrange(pc, procentrypc, procexitpc, segment) then
{
      if (pc.addr > procexitpc) or (pc.addr < procentrypc) or (segmented and
         (pc.segment <> segment)) then
}
          begin
          procentrypc := proc^.startpc + 2; 
			{avoid problems with recursive calls}
          procexitpc := proc^.endpc;
          end;

        if not d$readmap(mdl, stmt, rec) then choke(ourname);
        if not rec.exit then
          begin
          if not d$readmap(mdl, stmt + 1, tmprec) then
            choke(ourname);
          i := 2;
          while rec.pc = tmprec.pc do
            begin
            rec := tmprec;
            if not d$readmap(mdl, stmt + i, tmprec) then
              choke(ourname);
            i := i + 1;
            end;
          stmt := stmt + i - 1;
          tmpaddr := d$absaddr(mdl, tmprec.pc);
          stmtexitpc := tmpaddr.addr;
          exitstep := false;
          end;
        if rec.exit then
          begin
          stmtexitpc := procexitpc + 10;
          exitstep := true;
          end;

        prevaddr := pc.addr;
        nextstmtpc.addr := stmtexitpc;
        end;
      end;

  end; { updatestepparams }


function isstmtpc: boolean;

  const
    ourname = 'isstmtpc';
  var
    mdl: mdl_pointer;
    proc: proctreeptr;
    stmt: mapindex;
    rec: stmtrecord;


  begin {isstmtpc}
    if not embeddedjump then
      begin
      isstmtpc := false;
      if (currentpc.addr = steprec.stmtentrypc) or
         (currentpc.addr = steprec.stmtexitpc) then
        isstmtpc := true
      else if d$locateaddr(currentpc, mdl, proc, stmt, true) then
        begin
        if not d$readmap(mdl, stmt, rec) then choke(ourname);
        isstmtpc := (d$reladdr(mdl, currentpc) = rec.pc);
        end;     
      end;
  end; {isstmtpc}


procedure d$updatewatchlist;

    {Check the list of watch variables for those variables that may no longer
     be active.  A watched variable becomes inactive if it is a local variable
     and the program exits its particular block.  This is determined by 
     comparing the current dynamic level to that of the variable.  }

  var
    w, wback, wnext: watchpointer;

  begin {d$updatewatchlist}
    if not framesupdated then d$frameupdate;
    w := watchlist;
    while w <> nil do
      begin
      wback := w;
      wnext := w^.next;
      if w^.level > currentstackframe^.dynamiclevel then
        begin
        writeln(out, 'exited scope of watched variable '' ', w^.name.txt:
                w^.name.len, ' '' value did not change');
        d$watchrelease(w^.ident);
        watchidset := watchidset - [w^.ident];
        if w = watchlist then watchlist := wnext
        else wback^.next := wnext;
        if w^.cmds <> nil then 
          begin
          d$deactivate(w^.cmds);
          dispose(w^.cmds);
          end;
        dispose(w);
        end;
      w := wnext;
      end;
    if watchlist = nil then watching := false;
  end; {d$updatewatchlist}



{************************************************************************

     ------ Interpreter Driver

  This is the main driver for the interpreter.  For embeddedjump debuggers,
  this is set up to be called by the target supervisor every time the
  target program is determined to be at an "interrupt" state, e.g. at
  a breakpoint, or run-time error.  For separate process debuggers, the
  calls are initiated from here to the target supervisor in order to control
  the execution of the target program.  A repeat loop toggled by the 
  contant EMBEDDEDJUMP is used, hence control returns to the target supervisor
  after one loop for embeddedjump debuggers, otherwise the loop is 
  infinite, so control is maintained from the loop.

************************************************************************}


procedure d$interpreter;

  var
    done, interruptstate: boolean;
    returnpc: addressrec;
    proc: proctreeptr;
    stmt: mapindex;
    mdl: mdl_pointer;
    success: boolean;
    err: imessage;
    packet: datapacketptr;
    stepstate: (tracesteps, procreturn, callgoto, exiting, jumping);


  procedure scanpurpose;

    { Determine the actions to be taken whenever the target supervisor has
      called the interpreter.  If a watched variable has been touched, then
      emit the needed information.  If a breakpoint has been reached, then
      publish it.  Unless just history is on, the current location is emitted.
     }

    var
      printtrace: boolean;
      getcompleteframes: boolean;
      currentbreakpoint: breakpointer;
      w: watchpointer;
      b: breakpointer;
      c: commandpointer;
      mdl: mdl_pointer;
      proc: proctreeptr;
      stmt: mapindex;
      visibleframe: stackpointer;


    begin {scanpurpose}

      mdl := nil;
      proc := nil;
      stmt := 0;
      currentbreakpoint := nil;

      paused := not (tracing or history);

      if breakpointhit then
        begin
        breakpointhit := false;
        b := breaklist;
        while b <> nil do
          with b^ do
            begin
            if (addr.addr = currentpc.addr) and (not segmented or
               (addr.segment = currentpc.segment)) then
              begin
              count := count - 1;
              if count <= 0 then
                begin
                count := 0;
                breakpointhit := true;
                currentbreakpoint := b;
                c := b^.cmds;
                while c <> nil do
                  begin
                  d$makeactive(c);
                  c := c^.embedlink;
                  end;
                end
              end;
            b := b^.next;
            end; {while}
        end;

      if watchpointhit then
        begin
        w := watchlist;
        if needcaching then proc := d$getproctreerec;
        while w <> nil do
          begin
          if w^.ident in watchfound then
            begin
            write(out, 'The value of "', w^.name.txt: w^.name.len);
            writeln(out, '" was changed by statement:');
            if d$locateaddr(previouspc, mdl, proc, stmt, true) then
              if needcaching then proc^ := proctreefile^;
            d$wrtlocation(mdl, proc, stmt, previouspc, false);
            d$wrtwtch(w);
            if w^.cmds <> nil then d$makeactive(w^.cmds);
            end;
          w := w^.next;
          end;
        paused := true;
        if needcaching then if proc <> nil then d$freeproctreerec(proc);
        end;

      paused := ((targetopsys = vms) and firsttime) or ((stepping or
                proceeding) and (stepcount <= 0)) or breakpointhit or
                watchpointhit or interrupted or programdead;

      printtrace := paused or tracing;

      paused := firsttime or paused;

      if currentbreakpoint <> nil then
        begin
        writeln(out, 'Breakpoint at ');
        end
      else if interrupted then
        begin
        writeln(out, 'process interrupted ');
        interrupted := false;
        currentstackframe := d$firstvisibleframe;
        end;

      if printtrace then
        begin
        getcompleteframes := false;
        if needcaching then proc := d$getproctreerec;
        if not framesupdated then
          begin
          if d$locateaddr(currentpc, mdl, proc, stmt, true) then
            begin
            if needcaching then proc^ := proctreefile^;
            end
          else 
            begin
            getcompleteframes := true;
            if needcaching then
              begin
              d$freeproctreerec(proc);
              proc := nil;
              end;
            end;
          end;
        if framesupdated or getcompleteframes then
          begin
          visibleframe := d$firstvisibleframe;
          if visibleframe <> nil then
            with visibleframe^ do
              begin
              d$stmtnumlocate(pc, mdl, proc, stmt);
              d$wrtlocation(mdl, proc, stmt, pc, false);
              end
          else d$wrtlocation(mdl, proc, stmt, currentpc, false);
          end
        else d$wrtlocation(mdl, proc, stmt, currentpc, false);
        if needcaching and (proc <> nil) then d$freeproctreerec(proc);
        end;

      if interrupted then
        begin
        writeln(out, 'process interrupted ');
        interrupted := false;
        paused := true;
        end;

      watchfound := [];
      watchpointhit := false;
      breakpointhit := false;
      if stepcount <= 0 then
        begin
        stepping := false;
        proceeding := false;
        end;

    end; {scanpurpose}


  begin { d$interpreter }

    if not embeddedjump then stepstate := tracesteps;

    repeat

      if embeddedjump then
        if not firsttime then
          begin
          d$addhistory(currentpc);
          stepcount := stepcount - 1;
          restarting := false;
          end;

      scanpurpose;

      if paused then
        begin
        d$interactive;
        paused := false;
        end;

      if embeddedjump then
        begin
        if restarting then
          begin
          currentstackframe := globalstackframe;
          d$updatewatchlist;
          end;
        firsttime := false;
        d$setrunconditions;
        interrupted := false;
        if quitting then d$finalend;
        if not (history or tracing or stepping or watching or proceeding) or
           restarting then d$inithistory;
        if restarting then d$restart;
        end

      else
        begin

        if quitting then
          begin
          d$finalend;
          end

        else if (history or tracing or proceeding or stepping or
                watching) then
          while not paused do

            begin
            done := false;
            {avoid tracing steps if an exception has been encountered}
            if runtimeerror then
              begin
              stepstate := jumping;
              runtimeerror := false;
              end;
            previouspc := currentpc;
            if firsttime then
              begin
              d$restart;
              d$trapcontinue(false);
              d$resetwatchlist;
              d$frameupdate;
              d$updatewatchlist;
              {watching may have been turned off by d$updatewatchlist}
              if not (history or tracing or proceeding or stepping or
                 watching) then
                begin
                d$continue;
                paused := true;
                end;
              d$inithistory;
              firsttime := false;
              restarting := false;
              stepstate := tracesteps;
              done := true;
              end
            else updatestepparams;

            while (not done) and (not interrupted) do
              begin
              case stepstate of

                tracesteps:
                  begin
                  d$tracesteps(steprec.stmtentrypc, steprec.stmtexitpc,
                               returnpc);
{
                  if (currentpc.addr <= steprec.procexitpc) and
                     (currentpc.addr > steprec.procentrypc) and
                     (not segmented or
                     (currentpc.segment <> steprec.segment)) then
}
		  if d$addrinrange(currentpc, steprec.procentrypc,
				   steprec.procexitpc, steprec.segment) then
                    begin
                    currentstackframe^.pc := currentpc;
                    if exited or term or isstmtpc then done := true
                    else updatestepparams;
                    end
                  else if steprec.exitstep then stepstate := procreturn
                  else stepstate := callgoto;
                  end;

                procreturn:
                  begin
                  if d$locateaddr(currentpc, mdl, proc, stmt, false) then
                      {visible} 
                    begin
                    {drop one frame}
                    currentstackframe := currentstackframe^.dynamiclink;
                    if currentpc.addr =
                       currentstackframe^.nextstmtpc.addr then
                      begin
                      currentstackframe^.pc := currentpc;
                      currentstackframe^.nextstmtpc.addr :=
                       steprec.stmtexitpc;
                      done := true;
                      stepstate := tracesteps;
                      end
                    else
                      begin
                      updatestepparams;
                      stepstate := tracesteps;
                      end;
                    end
                  else {not visible} stepstate := exiting;
                  end;

                callgoto:
                  begin
                  {procedure call or goto}
                  if (returnpc.addr >= steprec.procentrypc) and
                     (returnpc.addr <= steprec.procexitpc) then
                    begin {procedure call}
                    currentstackframe^.pc := returnpc;
                    stepstate := jumping;
                    end
                  else {nonlocal goto}
                    begin
                    if d$locateaddr(currentpc, mdl, proc, stmt, false) then
                      begin
                      while (currentpc.addr >=
                            currentstackframe^.proc^.startpc) and
                            (currentpc.addr <=
                            currentstackframe^.proc^.endpc) do
                        currentstackframe := currentstackframe^.staticlink;
                      done := true;
                      stepstate := tracesteps;
                      end
                    else stepstate := exiting;
                    end;
                  end;

                exiting:
                  begin
                  d$trapcontinue(false);
                  if exited or term or isstmtpc then done := true
                  else
                    updatestepparams;
                  stepstate := tracesteps;
                  end;

                jumping:
                  begin
                  repeat
                    d$trapcontinue(proceeding);
                  until d$locateaddr(currentpc, mdl, proc, stmt, false) or
                    interrupted or exited or programdead;
                  done := programdead;
                  if d$locateaddr(currentpc, mdl, proc, stmt, false) then
                    begin
                    if needcaching then
                      done :=
                              (proctreefile^.startpc <> 
                                steprec.proc^.startpc) or
                              ((proctreefile^.startpc = 
                                 steprec.proc^.startpc) and
                              ((currentpc.addr <= steprec.stmtentrypc) or
                              (currentpc.addr >= steprec.stmtexitpc)))
                    else 
                      done := (proc <> steprec.proc) or
                              ((proc = steprec.proc) and
                              ((currentpc.addr <= steprec.stmtentrypc) or
                              (currentpc.addr >= steprec.stmtexitpc)));
                    if not interrupted then stepstate := tracesteps;
                    end;
                  end;

                end {case} ;
              end {while} ;

            if not (interrupted or programdead) then
              if (watchlist <> nil) then
                begin
                d$updatewatchlist;
                d$singlestep;
                end;

            d$addhistory(currentpc);
            stepcount := stepcount - 1;
            paused := ((stepping or proceeding) and (stepcount <= 0)) or
                      tracing or history or breakpointhit or watchpointhit or
                      programdead or interrupted;
            end {stepping}

        else
          begin
          if restarting then
            begin
            d$restart;
            d$resetwatchlist;
            currentstackframe := globalstackframe;
            restarting := false;
            firsttime := false;
            stepstate := tracesteps;
            end;
          d$inithistory;
          d$continue;
          end;

        end; {not embeddedjump}

    until embeddedjump;

  end;
