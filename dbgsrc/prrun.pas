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

  Pascal-2 profiler

 Last modified by KRIS on 26-Nov-1990 13:52:17
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}


procedure killprogram;

  begin  {killprogram}
    if targetopsys = vdos then p_term 
    else 
      begin
      if targetopsys = msdos then d$clearexit;
      exitst(1);
      end;
  end;  {killprogram}


function addrinrange(addr: addressrec; 
			 startrange, endrange: addressrange;
			 rangesegment: unsignedword): boolean;

  begin
    addrinrange := (startrange <= addr.addr) and (endrange > addr.addr) and
		     (not segmented or (rangesegment = addr.segment));
  end;


function absaddr(mdl: mdl_pointer;
                 addr: addressrange): addressrec;
 { Convert a relative address to an absolute address. }

  var
    tmpaddr: addressrec;


  begin
    tmpaddr.addr := addr + mdl^.code_start;
    if segmented then tmpaddr.segment := mdl^.codesegment;
    absaddr := tmpaddr;
  end;

{****************************************************************************

        Files management routines

  These routines manage the opening and closing of the profiler auxilliary
  files (the statement map, symbol table and listing file).  A queue is
  maintained to determine which files to close when the max is approached.

****************************************************************************}


procedure add_lmo_mdl(mdl: mdl_pointer);

  var
    new_lmo_mdl: plastmdlopened;


  begin {add_lmo_mdl}
    new(new_lmo_mdl);
    new_lmo_mdl^.next := nil;
    new_lmo_mdl^.mdl := mdl;
    if num_mdls_open > 0 then first_lmo^.next := new_lmo_mdl
    else last_lmo := new_lmo_mdl;
    first_lmo := new_lmo_mdl;
    new_lmo_mdl := nil;
    num_mdls_open := num_mdls_open + 1;
  end; {add_lmo_mdl}


procedure closemdl(var last_lmo: plastmdlopened);

  var
    disp_lmo: plastmdlopened;


  begin {closemdl}
    with last_lmo^.mdl^ do
      begin
      if info = allinfo then close(listfile);
      close(symfile);
      close(mapfile);
      info := unloaded;
      end;
    disp_lmo := last_lmo; { set LastLUO to next in list }
    last_lmo := disp_lmo^.next;
    dispose(disp_lmo); { and dispose of pointer }
    disp_lmo := nil;
    num_mdls_open := num_mdls_open - 1;
  end; {closemdl}


procedure loadmdl(mdl: mdl_pointer);
 { Locate symbol table and statement map files for module. }

  var
    i: isymbolname;
    flg: integer;
    addr: addressrec;


  begin { loadmdl }
    if mdl^.info = unloaded then
      begin
      mdl^.info := noinfo;
      if (num_mdls_open + 1) > max_mdls_open then closemdl(last_lmo);
      reset(mdl^.symfile, mdl^.nam, symext, flg);
      if flg = - 1 then 
        begin
        writeln(out, 'symbol table file missing for module ', mdl^.nam);
        killprogram;
        end;
      reset(mdl^.mapfile, mdl^.nam, mapext, flg);
      if flg = - 1 then 
        begin
        writeln(out, 'statement map file missing for module ', mdl^.nam);
        killprogram;
        end;
      mdl^.info := syminfo;
      reset(mdl^.listfile, mdl^.nam, listext, flg);
      if flg = - 1 then 
        begin
        writeln(out, 'listing file missing for module ', mdl^.nam);
        killprogram;
        end;
      mdl^.info := allinfo;
      add_lmo_mdl(mdl);
      end; {if unloaded}
  end; { loadmdl }



{*************************************************************************

     ------ Debugger Procedure Tree Initialization

  Each module has a binary tree of procedure descriptor records.  Each node
  of the tree is a record that maintains certain information about each
  procedure that is "visible" to the profiler.  This information includes
  the starting and ending pc's, index pointers into the symbol table
  and statement map, and some holding buckets for the counting.  The tree is
  built on the basis of the start and end pc's so that fast binary searches
  can be made on the basis of a location in code.  The tree is constructed
  by first establishing an ordered linear list of records as derived from
  the symbol table and statement map (which thankfully is already 
  ordered).  Once the linear list is completed, it is converted into a
  balanced binary tree.

*************************************************************************}


function maketree(root: proctreeptr;
                  start, finish: integer): proctreeptr;

  { Convert a linear (ordered) list of procedure tree records into
    a balnced binary tree.  This routine does this by recursively
    "pulling" up the midpoint of the given list.}

  var
    mark, i: integer;
    p, markproc: proctreeptr;


  begin
    if start >= finish then maketree := root
    else
      begin
      mark := (start + finish) div 2;
      p := root;
      markproc := root;
      for i := start to mark do
        begin
        markproc := p;
        p := p^.right;
        end;
      p^.left := root;
      markproc^.right := nil;

      p^.left := maketree(p^.left, start, mark);
      p^.right := maketree(p^.right, mark + 2, finish);

      maketree := p;
      end;
  end;


procedure initproctree {(mdl: mdl_pointer)} ;

  { Establish a balanced binary tree of procedure records for each module.
    An ordered linear list is first procduced of all the procedures, then
    then function maketree in invoked to convert that list to a binary tree. }

  var
    prochead, p: proctreeptr;
    done: boolean;
    cnt: integer;
    index: mapindex;
    tmpaddr: addressrec;
    i: integer;
    kludge: intptr;


  begin {initproctree}

    if mdl^.info = unloaded then loadmdl(mdl);
    if mdl^.info = noinfo then mdl^.proctree := nil

    else
      begin
      p := nil;
      cnt := 0;
      done := false;
      seek(mdl^.mapfile, 1);
      if eof(mdl^.mapfile) then killprogram;
      index := mdl^.mapfile^.recordnr;

      while not done do
        begin
        seek(mdl^.symfile, index);
        if eof(mdl^.symfile) then 
          begin
          writeln(out, 'symbol table mismatch');
          killprogram;
          end;
        if mdl^.symfile^.kind = symboldesc then
          begin
          if (mdl^.symfile^.namekind = procname) or
             (mdl^.symfile^.namekind = funcname) then
            begin
            if (mdl^.symfile^.firststmt <> 0) then
              begin
              if (p = nil) then
                begin
                new(p);
                prochead := p;
                end
              else
                begin
                new(p^.right);
                p := p^.right;
                end;
              with p^, mdl^.symfile^ do
                begin
                right := nil;
                left := nil;
                procno := firststmt;
                seek(mdl^.mapfile, procno);
                if eof(mdl^.mapfile) then 
                  begin
                  writeln(out, 'symbol table mismatch');
                  killprogram;
                  end;
                tmpaddr := absaddr(mdl, mdl^.mapfile^.pc);
                startpc := tmpaddr.addr;
                symid := index;
                calls := 0;
                total := 0;
                lines := 1;
                get(mdl^.mapfile);
                while not eof(mdl^.mapfile) and not (mdl^.mapfile^.exit) do
                  begin
                  lines := lines + 1;
                  get(mdl^.mapfile);
                  end;
                end;
              kludge := p_inew(p^.lines * size(addressrange));
              p^.pcmap := loophole(pcpointer, kludge);
              kludge := p_inew(p^.lines * 2 * size(integer));
              p^.counts := loophole(countpointer, kludge);
              if (p^.pcmap = nil) or (p^.counts = nil) then
                begin
                writeln(out, 'Not enough memory to hold count information');
                killprogram;
                end;

              cnt := cnt + 1;
              if mdl^.symfile^.level = 1 then mainproc := p;
              end;
            if mdl^.symfile^.level = 0 then done := true;
            index := mdl^.symfile^.lastname + 2;
            end
          else index := index + 1
          end
        else index := index + 1;
        end;

      p := prochead;
      while p <> nil do
        begin
        with p^ do
          begin
          seek(mdl^.mapfile, procno + 1);
          for i := 1 to lines do
            begin
            counts^[i] := 0;
            tmpaddr := absaddr(mdl, mdl^.mapfile^.pc);
            pcmap^[i] := tmpaddr.addr;
            get(mdl^.mapfile);
            end;
          if eof(mdl^.mapfile) then
            endpc := mdl^.code_end
          else
            begin
            tmpaddr := absaddr(mdl, mdl^.mapfile^.pc);
            endpc := tmpaddr.addr;
            end;
          end;
        p := p^.right;
        end;

      mdl^.proctree := maketree(prochead, 1, cnt);
      end;
  end; {initproctree}


procedure locateaddr(addr: addressrec;
                     var mdl: mdl_pointer;
                     var proc: proctreeptr);

  { Locate the module, and procedure containing an address;
    return NIL module pointer when the address is not within any known module,
    return zero procedure and statement values when the module containing
    the address has no associated map file. }

  label
    1;

  var
    idx: mapindex;
    found: boolean;
    p: proctreeptr;
    rec: stmtrecord;
    base: addressrange;
    offset: addressrange;


  begin { locateaddr }

    { Locate program module containing address. }

    mdl := mdl_list;
    proc := nil;
    found := false;
    while not found do
      begin
      if mdl = nil then goto 1;
      if addrinrange(addr, mdl^.code_start, mdl^.code_end, mdl^.codesegment)
      then found := true
      else mdl := mdl^.next;
      end;
    if mdl^.info = unloaded then loadmdl(mdl);
    if mdl^.info = noinfo then goto 1;

    { Binary search map file for records with desired address (if there is
      no match, locate last record with address less than desired one). }

    found := false;
    p := mdl^.proctree;
    while (p <> nil) and not found do
      begin
      if (addr.addr < p^.startpc) then p := p^.left
      else if (addr.addr > p^.endpc) then p := p^.right
      else found := true;
      end;

    if found then proc := p;

  1: ;

  end; { locateaddr }


procedure copyline(mdl: mdl_pointer);
 { Transfer a line of code from the module listing to the profile output. }

  var p:lineptr;
      len: integer;

  begin
    while not eoln(mdl^.listfile) do
      begin
      if fastread then
        begin p_rdsfst(mdl^.listfile, p, len);
          write(profilefile, p^:len);
        end
      else
        begin
        write(profilefile, mdl^.listfile^);
        get(mdl^.listfile);
        end;
      end;
    readln(mdl^.listfile);
    writeln(profilefile);
  end;



{************************************************************************}
{                                                                        }
{    -- Runtime interface to the user's program                          }
{                                                                        }
{    When the user's program is executing, the code calls routines in    }
{    the library module OPDBG to identify procedures and statements.     }
{    The routines in OPDBG call this procedure with a parameter (ACTION) }
{    describing what happened.  The only other information passed to     }
{    the debugger is the register context in the variable REGISTER       }
{    which is also set up by OPDBG.                                      }
{                                                                        }
{    These routines maintain the execution history and the profiler      }
{    context stack.  For each statement a count is kept execution,       }
{    the count being maintained in the particular procedure node of      }
{    the procedure tree.                                                 }
{                                                                        }
{************************************************************************}


procedure d$pro2;

  var
    c1, c2: 0..255;
    line, i, j: integer;
    poshi, poslo: integer;
    lastline, totalstatements: unsignedword;
    count, totalcount, proceduretotal: unsignedint;
    outputfilename: packed array [1..80] of char;
    found: boolean;
    mdl: mdl_pointer;
    d: debugrecord;




  procedure writestmts(mdl: mdl_pointer;
                       p: proctreeptr);
      {  Write out the count information for each line in a procedure
         then write out the line itself.  This routine is recursive, 
         and will traverse the procedure tree.  }

    var
      line: integer;


    begin

      if p^.left <> nil then writestmts(mdl, p^.left);

      with p^ do
        begin
        lastline := 0;
        totalstatements := totalstatements + lines - 1;
        seek(mdl^.mapfile, procno + 1);

        for line := 1 to lines - 1 do
          if counts^[line] <> 0 then
            begin
            count := counts^[line];
            if mdl^.mapfile^.lineno <> lastline then
              begin
              lastline := mdl^.mapfile^.lineno;
              getpos(mdl^.listfile, poshi, poslo);
              if hostopsys = msdos then
                poshi := poshi + poslo * bitmask[16];   
              while (poshi < mdl^.mapfile^.filepos1) or
                    ((poshi = mdl^.mapfile^.filepos1) and
                     (poslo < mdl^.mapfile^.filepos2) and
                     (hostopsys <> unix)) do
                begin
                if not eoln(mdl^.listfile) and found then
                  write(profilefile, chr(9));
                write(profilefile, ' ');
                copyline(mdl);
                getpos(mdl^.listfile, poshi, poslo);
                if hostopsys = msdos then
                  poshi := poshi + poslo * bitmask[16];   
                found := true;
                end;
              write(profilefile, count: 8);
              write(profilefile, ' ');
              copyline(mdl);
              end;
            get(mdl^.mapfile);
            end
          else get(mdl^.mapfile);
        end;

      if p^.right <> nil then writestmts(mdl, p^.right);
    end;


  procedure writeprocs(mdl: mdl_pointer;
                       p: proctreeptr);
      {  Emit the totals for a procedure.  This routine is recursive
         and will traverse the procedure tree.  }


    begin
      if p^.left <> nil then writeprocs(mdl, p^.left);

      with p^ do
        begin
        totalprocs := totalprocs + 1;
        if (totalprocs mod 25) = 0 then
          begin
          page(profilefile);
          writeln(profilefile);
          end;
        seek(mdl^.symfile, symid - 1);
        if mdl^.symfile^.kind <> identdesc then
          writeln('*** WRONG SYMBOL TABLE FILE! ***')
        else
          writeln(profilefile, mdl^.symfile^.identchars, lines - 1: 9,
                  calls:13, total: 15, 100.0 * total / totalcount: 7: 2, '%');
        writeln(profilefile);
        end;

      if p^.right <> nil then writeprocs(mdl, p^.right);

    end; {writeprocs}


  procedure calcsums;
      {  Calculate the procedure and program totals.  The module list is
         traversed, and the recursive routine calcnode is called to 
         traverse each procedure tree. }

    var
      mdl: mdl_pointer;


    procedure calcnode(mdl: mdl_pointer;
                       p: proctreeptr);

      var
        line: integer;

      begin {calcnode}
        if p^.left <> nil then calcnode(mdl, p^.left);
        with p^ do
          begin
          total := 0;
          for line := 1 to lines - 1 do
            if counts^[line] <> 0 then
              begin
              totalcount := totalcount + counts^[line];
              total := total + counts^[line];
              mdl^.count := mdl^.count + counts^[line];
              end;
          end;
        if p^.right <> nil then calcnode(mdl, p^.right);
      end; {calcnode}


    begin {calcsums}
      mdl := mdl_list;
      while mdl <> nil do
        begin
        if mdl^.info = unloaded then loadmdl(mdl);
        if mdl^.info = allinfo then
          begin
          mdl^.count := 0;
          calcnode(mdl, mdl^.proctree);
          end;
        mdl := mdl^.next;
        end;
    end; {calcsums}




  begin
    case action of
      initialize:
        begin
        notskipping := true;
        looking := true;
        running := true;
        mdl_list := nil;
        initialized := false;
        levelchanged := false;
        totalprocs := 0;
        max_mdls_open := (maxfilesopen - ord(needcaching)) div 3;
        p_dbioinit;
        writeln(out);
        buildmdllist;
        mdl := main_mdl;
        terminalcondition := false;
        new(currentstackframe);
        with currentstackframe^ do
          begin
          downlink := nil;
          uplink := nil;
          fp := 0;
          locateaddr(register.pc, mdl, proc);
          proc := mainproc;
          proc^.calls := 1;
          stmt := 1;
          end;
        write(out, 'Profiling program ', main_mdl^.nam);
        writeln(out);
        write(out, 'Profile output file name? ');
        break(out);
        if eof(inp) then killprogram;
        readln(inp, outputfilename);
        close(inp);
        rewrite(profilefile, outputfilename, profilext);
        terminalcondition := false;
        initialized := true;

        d$rego;
        running := false;
        terminalcondition := true;
        end;


    {************************************************************************}
    {                                                                        }
    {    PROCEDUREENTRY                                                      }
    {                                                                        }
    {    Enter a new procedure.  This routine adds a new entry on the        }
    {    debugger context stack, and finds the procedure tree node for       }
    {    the new context.                                                    }
    {                                                                        }
    {************************************************************************}

      procedureentry:
        begin
        with currentstackframe^ do
          begin
          if uplink = nil then
            begin
            new(uplink);
            uplink^.uplink := nil;
            uplink^.downlink := currentstackframe;
            end;
          end;
        currentstackframe := currentstackframe^.uplink;
        with currentstackframe^ do
          begin
          fp := register.fp;
          locateaddr(register.pc, mdl, proc);
          with proc^ do calls := calls + 1;
          stmt := 1;
          end;
        end;


    {************************************************************************}
    {                                                                        }
    {    STATEMENT                                                           }
    {                                                                        }
    {    This is the heart of the profiler.  Before each statement in the    }
    {    user's program executes, this routine is called.  The counting      }
    {    array for the current context is incremented for the current        }
    {    statement.                                                          }
    {                                                                        }
    {************************************************************************}

      statement:
        begin

        if levelchanged then
          begin
          while (currentstackframe <> nil) and
                (currentstackframe^.fp <> register.fp) do
            currentstackframe := currentstackframe^.downlink;
          levelchanged := false;
          if currentstackframe = nil then killprogram;
          end;

        with currentstackframe^, currentstackframe^.proc^ do
          begin
          if register.pc.addr >= pcmap^[stmt] then
            if stmt + 4 <= lines then
              if register.pc.addr <= pcmap^[stmt + 4] then
                if register.pc.addr = pcmap^[stmt + 1] then
                  begin
                  i := stmt + 1;
                  j := stmt + 1;
                  end
                else
                  begin
                  i := stmt;
                  j := stmt + 4;
                  end
              else
                begin
                i := stmt + 4;
                j := lines;
                end
            else
              begin
              i := stmt;
              j := lines;
              end
          else
            begin
            i := 1;
            j := stmt;
            end;
          repeat
            line := (i + j) div 2;
            if register.pc.addr > pcmap^[line] then i := line + 1
            else j := line - 1;
          until (pcmap^[line] = register.pc.addr) or (i > j);
          if pcmap^[line] = register.pc.addr then
            begin
            stmt := line;
            counts^[line] := counts^[line] + 1;
            end
          else
            writeln(out, 'not found in ', currentstackframe^.proc^.procno: 1,
                    ' ', register.pc.addr: - 1);
          end;
        end;


    {************************************************************************}
    {                                                                        }
    {    PROCEDUREEXIT                                                       }
    {                                                                        }
    {    This routine is called when a procedure exits in the user program.  }
    {    The debugger context stack is dropped one level.                    }
    {                                                                        }
    {************************************************************************}

      procedureexit:
        begin
        if currentstackframe^.downlink = nil then
          begin
          writeln(out, 'Fatal error -- procedure underflow');
          running := false;
          end
        else currentstackframe := currentstackframe^.downlink;
        end;


    {************************************************************************}
    {                                                                        }
    {    TRAP                                                                }
    {                                                                        }
    {    All unexpected traps in the user program are vectored through       }
    {    this routine which reports the error and terminates the program.    }
    {                                                                        }
    {************************************************************************}

      error:
        begin
        if not initialized then killprogram;
        writeln(out, 'PROFILE -- unexpected trap.  program terminated');
        terminalcondition := true;
        running := false;
        end;


    {************************************************************************}
    {                                                                        }
    {    NON-LOCAL GOTO                                                      }
    {                                                                        }
    {    When the user's program executes a non-local GOTO to branch to      }
    {    a statement in a different enclosing procedure, the profiler must   }
    {    be notified so that it can unwind its parallel lexical stack.       }
    {                                                                        }
    {************************************************************************}

      nonlocalgoto:
        begin
        levelchanged := true;
        end;


    {************************************************************************}
    {                                                                        }
    {    TERMINATION                                                         }
    {                                                                        }
    {    When a program terminates for any reason (end of the code or an     }
    {    error) this routine is called to notify the profiler.               }
    {                                                                        }
    {************************************************************************}

      terminated:
        begin
        if not initialized then running := true
        else
          begin
          terminalcondition := true;
          running := false;
          writeln(out);
          writeln(out, 'Program terminated');
          writeln(out);
          end;
        end;
      end;


    {************************************************************************}
    {                                                                        }
    {    Determine if the user program should continue executing or if       }
    {    the profilershould take control and enter produce then final        }
    {    output.                                                             }
    {                                                                        }
    {************************************************************************}

    if not running then
      begin
      writeln(out);
      writeln(out, 'Profile being generated');
      totalcount := 0;
      found := false;
      totalstatements := 0;

      calcsums;
      mdl := mdl_list;
      while mdl <> nil do
        with mdl^ do
          begin
          if mdl^.info = unloaded then loadmdl(mdl);

          writeln(profilefile);
          writeln(profilefile, 'Profile for module  ', mdl^.nam);
          writeln(profilefile);

          if mdl^.proctree <> nil then writestmts(mdl, mdl^.proctree);

          while not eof(mdl^.listfile) do
            begin
            if not eoln(mdl^.listfile) then write(profilefile, chr(9));
            write(profilefile, ' ');
            copyline(mdl);
            end;
          close(mdl^.mapfile);
          reset(mdl^.symfile, mdl^.nam, symext);
          page(profilefile);
          writeln(profilefile);
          writeln(profilefile, 'PROCEDURE EXECUTION SUMMARY': 50);
          writeln(profilefile);
          writeln(profilefile);
          writeln(profilefile, 'Procedure name                  ',
                  'statements   ', 'times called   ', 'statements executed');
          writeln(profilefile);
          writeln(profilefile);

          if mdl^.proctree <> nil then writeprocs(mdl, mdl^.proctree);

          mdl := mdl^.next;
          if mdl <> nil then page(profilefile);
          end;

      writeln(profilefile);
      writeln(profilefile);
      writeln(profilefile, 'There are ', totalstatements: 1,
              ' statements in ', totalprocs: 1,
              ' procedures in this program.');
      writeln(profilefile, totalcount: 10,
              ' statements were executed during the profile.');
      close(profilefile);
      killprogram;
      end;
  end;
