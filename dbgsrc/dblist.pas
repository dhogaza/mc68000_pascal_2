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

  Pascal-2 debugger listing routines.

 Last modified by KRIS on 26-Nov-1990 13:46:30
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}



{************************************************************************

     ------ Listing Output

************************************************************************}


function readlist(mdl: mdl_pointer;
                  var rec: dbstring;
                  var len: integer): boolean;

 { Read a line of the source program from the current position in the
   listing file for module MDL.  Return a false if the listing file 
    cannot be found. }

  const
    numfieldwidth = 5;

  var
    found: boolean;


  procedure readnextline;

    label
      1;

    var
      ch: char;
      skip, blank, done: boolean;


    begin { ReadNextLine }
    1:
      len := 0;
      blank := true;
      skip := false;
      repeat
        if eof(mdl^.listfile) then skip := true
        else
          begin
          read(mdl^.listfile, ch);
          if eoln(mdl^.listfile) then skip := true
          else if not (ch in [' ', '0'..'9']) then skip := true
          else
            begin
            if ch <> ' ' then blank := false;
            len := len + 1;
            end;
          end;
      until (len > numfieldwidth) or skip;
      if blank then skip := true;
      len := 0;
      done := false;
      repeat
        if eof(mdl^.listfile) then done := true
        else if eoln(mdl^.listfile) then
          begin
          readln(mdl^.listfile);
          done := true;
          end
        else
          begin
          if len < stringsize then
            begin
            read(mdl^.listfile, ch);
            len := len + 1;
            rec[len] := ch
            end
	  else read(mdl^.listfile, ch);
          end;
      until done;
      if not eof(mdl^.listfile) and skip then
        begin
        readln(mdl^.listfile);
        goto 1;
        end;
    end; { ReadNextLine }


  begin { ReadList }
    found := false;
    if not mdl^.loaded then d$loadmdl(mdl);
    if mdl^.info >= allinfo then
      begin
      readnextline;
      readlist := not eof(mdl^.listfile);
      end;
  end; { ReadList }


function fold(ch: char): char;
 { Convert upper case to lower case. }


  begin
    fold := ch;
    if ch in ['A'..'Z'] then fold := chr(ord(ch) + (ord('a') - ord('A')));
  end;


procedure d$wrtaddress {(val: addressrec)} ;
 { Write address value in format suited to particular host. }

  var
    len: 0..12;

  procedure wrtaddr(offset: addressrange);
    type
      digitarray = array [0..15] of char;

    const
      chardigit = digitarray('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                             'A', 'B', 'C', 'D', 'E', 'F');

    var
      digits: array [1..12] of char;
      width: shortint;


    begin
      if segmented then width := 4 else width := 8;
      len := 0;
      while offset <> 0 do
        begin
        len := len + 1;
        digits[len] := chardigit[offset mod defaultaddressradix];
        offset := offset div defaultaddressradix;
        end;
      while width > len do
	begin
        write(out, '0');
        width := width - 1;
	end;
      while len > 0 do
        begin
        write(out, digits[len]);
        len := len - 1
        end;
    end;

  begin {d$wrtaddress}
    if segmented then 
      begin
      wrtaddr(loophole(addressrange, val.segment));
      write(out, ':');
      end;
    wrtaddr(val.addr);
  end;  {d$wrtaddress}


procedure d$wrtsymbolname {(var sym: symbolname; width: integer)} ;
 { Emit a symbol name, usually either a module name or a procedure name. }

  label
    1;

  var
    tmp: symbolname;
    i: isymbolname;


  begin {d$wrtsymbolname}
    i := 0;
    while sym[i + 1] <> ' ' do
      begin
      i := i + 1;
      tmp[i] := fold(sym[i]);
      if i = msymbolname then goto 1;
      end;
  1:
    write(out, tmp: i);
    if i < width then write(out, ' ': (width - i));
  end; {d$wrtsymbolname}


procedure d$wrtlocation { (mdl: mdl_pointer; proc: proctreepointer; stmt:
                           mapindex; addr: addressrec; complete: boolean) } ;

  { Print a program location and, if the listing is available, the 
    associated statement line.  If the location is not "visible" to the
    debugger then either just the pc is put out, or, if possible 
    (as in the unix environment), d$wrtnonpascaladdress is invoked to
    emit a symbolic location as derived from reading the executable file.
  }

  var
    firstlineno, lastlineno, firstlinepos1, firstlinepos2: integer;
    line: dbstring;
    linelen: integer;
    done: boolean;
    tmpaddr: addressrec;


  procedure writepascaladdr;

    const
      ourname = 'writepascaladdr';
    var
      procrec, stmtrec: stmtrecord;
      rec: debugrecord;
      base, offset: addressrange;
      first, done: boolean;


    begin {writepascaladdress}
      offset := d$reladdr(mdl, addr);

      { Write procedure name. }

      d$getobject(mdl, proc^.firstsym, rec);
      if rec.kind <> identdesc then choke(ourname);
      d$wrtsymbolname(rec.identchars, 1);

      { Write relative statement number. }

      write(out, ',');
      if not d$readmap(mdl, stmt, stmtrec) then choke(ourname);
      base := stmtrec.pc;
      first := true;
      repeat
        if stmtrec.lineno <> 0 then
          begin
          if firstlineno = 0 then
            begin
            firstlineno := stmtrec.lineno;
            firstlinepos1 := stmtrec.filepos1;
            firstlinepos2 := stmtrec.filepos2;
            end;
          if lastlineno < stmtrec.lineno then lastlineno := stmtrec.lineno;
          end;
        case stmtrec.typ of
          plabrec:
            begin
            if first then first := false;
            write(out, 'entry');
            end;
          stmntrec:
            begin
            if first then first := false;
            if stmtrec.exit then write(out, 'exit')
            else if mdl^.info < allinfo then
              write(out, stmtrec.proclinenr: 1);
            end;
          otherwise;
          end;
        stmt := stmt + 1;
        done := true;
        if d$readmap(mdl, stmt, stmtrec) then
          if stmtrec.pc = base then done := false;
      until done;

    end; {writepascaladdress}


  begin { d$wrtlocation }
    firstlineno := 0;
    lastlineno := 0;
    if mdl <> nil then
      begin
      d$wrtsymbolname(mdl^.nam, 1);
      write(out, ':')
      end;
    if proc <> nil then writepascaladdr
    else if complete then
      d$wrtnonpascaladdr(addr)
    else
      begin
      write(out, 'Unknown module: pc = ');
      d$wrtaddress(addr);
      end;
    if (firstlineno <> 0) and (mdl^.info > syminfo) then
      begin
      done := false;
      if hostopsys = msdos then seek(mdl^.listfile, firstlinepos1)
      else setpos(mdl^.listfile, firstlinepos1, firstlinepos2);
      while (firstlineno <= lastlineno) and not done do
        begin
        if not readlist(mdl, line, linelen) then done := true
        else
          begin
          writeln(out, line: linelen);
          firstlineno := firstlineno + 1;
          end;
        end {while} ;
      end
    else writeln(out);
  end; { d$wrtlocation }


procedure d$wrtlist {(mdl: mdl_pointer; first, last, firstpos1, firstpos2:
                       integer) } ;
 { Write out a block of the listing as delimitted by FIRST and LAST. }

  var
    line: dbstring;
    len: integer;
    done: boolean;


  begin { d$wrtlist }
    done := false;
    if not mdl^.loaded then d$loadmdl(mdl);
    if mdl^.info > syminfo then
      begin
      if hostopsys = msdos then seek(mdl^.listfile, firstpos1)
      else setpos(mdl^.listfile, firstpos1, firstpos2);
      while (first <= last) and not interrupted and not done do
        begin
        if not readlist(mdl, line, len) then done := true
        else
          begin
          writeln(out, line: len);
          first := first + 1;
          end;
        end;
      end;
  end; { d$wrtlist }



{************************************************************************

     ------ History Output

************************************************************************}


procedure d$wrthistory { (cnt: integer) } ;
 { Write out the specified number of statements from the history list. }

  var
    addr: addressrec;
    mdl: mdl_pointer;
    proc: proctreeptr;
    stmt: mapindex;


  begin { d$wrthistory }
    cnt := d$historycount(cnt);
    if needcaching then proc := d$getproctreerec;
    while (cnt > 0) do
      begin
      addr := d$getprevhistory(cnt);
      if needcaching then
        begin
        if d$locateaddr(addr, mdl, proc, stmt, true) then 
          begin
          proc^ := proctreefile^;
          d$wrtlocation(mdl, proc, stmt, addr, false);
          end
        else d$wrtlocation(mdl, nil, stmt, addr, false);
        end
      else
        begin
        if d$locateaddr(addr, mdl, proc, stmt, true) then;
        d$wrtlocation(mdl, proc, stmt, addr, false);
        end;
      cnt := cnt - 1;
      end;
    if needcaching then d$freeproctreerec(proc);
  end; { d$wrthistory }



{************************************************************************

     ------ Frame List Output

     The stack dump is printed with the innermost frame first.  Each
     frame is numbered with its dynamic nesting level, counting
     upwards from the outermost frame.  An angle (<) marks the current
     context level, which is the top procedure unless the 'E' command
     has been used.  An asterisk marks each frame that is accessible
     from the current context level according to Pascal scope rules.

************************************************************************}


procedure d$wrtframes { (complete, verbose: boolean; limitframe:
                         stackpointer) } ;

  var
    f: stackpointer;
    static: stackpointer;
    addr: addressrec;
    first, visible: boolean;
    mdl: mdl_pointer;
    stmt: mapindex;
    proc: proctreeptr;


  begin { d$wrtframes }
    if not framesupdated then d$frameupdate;
    f := currentstackframe;
    first := true;
    static := currentcontext;
    if needcaching then proc := d$getproctreerec;
    while (f <> limitframe) and not interrupted do
      begin
      write(out, f^.dynamiclevel: 2);
      if static = f then
        begin
        static := f^.staticlink;
        if f = currentcontext then write(out, '<')
        else write(out, '*');
        end
      else write(out, ' ');
      write(out, ' ');
      addr := f^.pc;
      if not first then addr.addr := addr.addr - 2;
      if needcaching then
        begin
        if d$locateaddr(addr, mdl, proc, stmt, true) then 
          begin
          proc^ := proctreefile^;
          d$wrtlocation(mdl, proc, stmt, addr, complete);
          end
        else d$wrtlocation(mdl, nil, stmt, addr, false);
        end
      else
        begin
        if d$locateaddr(addr, mdl, proc, stmt, true) then;
        d$wrtlocation(mdl, proc, stmt, addr, complete);
        end;
      f := f^.dynamiclink;
      first := false;
      end;
    if needcaching then d$freeproctreerec(proc);
  end; { d$wrtframes }
