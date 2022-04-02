{$nomain}
{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045 Level: 1
  Processor: ~processor~
  System: ~system~

  PDB specific code to supplement files.pas.

 Last modified by KRIS on 21-Nov-1990 15:35:31

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1
}

{ PDB version }


procedure dumpform(f: entryptr;
                   id: index;
                   level: levelindex);

  var
    p: entryptr;


  begin {dumpform}
    with f^ do
      if dbgsymbol > 0 then
        begin
        seek(debugfile, dbgsymbol);
        debugfile^.kind := formdesc;
        debugfile^.packedflag := packedflag;
        debugfile^.bitaddress := bitaddress;
        debugfile^.size := size;
        debugfile^.typ := typ;
        case typ of
          scalars:
            begin
            debugfile^.lastord := lastord;
            debugfile^.firstscalar := 0;
            end;
          subranges:
            begin
            debugfile^.lowerord := lowerord;
            debugfile^.upperord := upperord;
            debugfile^.extended := extendedrange;
            if bigcompilerversion then p := ref(bigtable[parenttype])
            else areadaccess(parenttype, p);
            debugfile^.parenttype := p^.dbgsymbol;
            end;
          fields:
            begin
            debugfile^.fieldid := fieldid;
            debugfile^.fieldlevel := level;
            { new....... speed up pdb }
            debugfile^.firstfield := 0;
            debugfile^.lastfield := 0;
            if bigcompilerversion then p := ref(bigtable[f^.firstfield])
            else awriteaccess(f^.firstfield, p);
            { modify the first field name to help pdb }
            if not p^.form and (p^.namekind = fieldname) then
              p^.sparelink := id;
            end;
          strings, arrays, conformantarrays:
            begin
            debugfile^.elementsize := elementsize;
            if bigcompilerversion then p := ref(bigtable[indextype])
            else areadaccess(indextype, p);
            debugfile^.indextype := p^.dbgsymbol;
            if bigcompilerversion then p := ref(bigtable[elementtype])
            else areadaccess(elementtype, p);
            debugfile^.elementtype := p^.dbgsymbol;
            end;
          sets:
            begin
            if bigcompilerversion then p := ref(bigtable[basetype])
            else areadaccess(basetype, p);
            debugfile^.basetype := p^.dbgsymbol;
            end;
          files:
            begin
            if bigcompilerversion then p := ref(bigtable[filebasetype])
            else areadaccess(filebasetype, p);
            debugfile^.filebasetype := p^.dbgsymbol;
            end;
          ptrs:
            if ptrtypename <> 0 then
              begin
              if bigcompilerversion then p := ref(bigtable[ptrtypename])
              else areadaccess(ptrtypename, p);
              if bigcompilerversion then p := ref(bigtable[p^.typeindex])
              else areadaccess(p^.typeindex, p);
              debugfile^.ptrtype := p^.dbgsymbol
              end;
          otherwise;
          end;
        put(debugfile);
        end;
  end {dumpform} ;


procedure dumpname(p: entryptr;
                   level: levelindex;
                   idno: integer);

  var
    j: integer;
    f: entryptr; {used to access form data}


  function debughash(ch: char): debughashindex;

  { return hash value for debugger hash table.
  }


    begin {debughash}
      if ch = '$' then debughash := 26
      else if ch = '_' then debughash := 27
      else if ch in ['a'..'z'] then debughash := ord(ch) - ord('a')
      else debughash := ord(ch) - ord('A');
    end {debughash} ;


  begin {dumpname}
    with p^ do
      if charlen > 0 then
        begin
        lastdebugrecord := lastdebugrecord + 2;
        with debugfile^ do
          begin
          kind := identdesc;
          for j := 1 to debugsymbolmax do identchars[j] := ' ';
          if not scanalys then seekstringfile(stringfilecount + charindex - 1);
          for j := 1 to min(debugsymbolmax, charlen) do
            begin
            if scanalys then identchars[j] := stringtable^[charindex - 1 + j]
            else
              begin
              if needcaching then
                identchars[j] := chr(stringfile^[nextstringfile])
              else identchars[j] := chr(stringblkptr^[nextstringfile]);
              getstringfile;
              end;
            end;
          chainoffset := debughashtable[debughash(identchars[1])];
          debughashtable[debughash(identchars[1])] := lastdebugrecord - 1;
          end {with} ;
        put(debugfile);
        debugfile^.kind := symboldesc;
        debugfile^.name := name;
        debugfile^.namekind := namekind;
        case namekind of
          typename:
            begin
            if bigcompilerversion then f := ref(bigtable[typeindex])
            else areadaccess(typeindex, f);
            debugfile^.typeindex := f^.dbgsymbol;
            end;
          constname:
            begin
            if bigcompilerversion then f := ref(bigtable[consttype])
            else areadaccess(consttype, f);
            debugfile^.constform := f^.typ;
            debugfile^.consttype := f^.dbgsymbol;
            if (debugfile^.constform = reals) or
               (debugfile^.constform = doubles) then
              debugfile^.r := constvalue.realvalue.realbuffer
            else if debugfile^.constform = ptrs then
              debugfile^.nilpointer := constvalue.ptrvalue
            else
              begin
              debugfile^.i := constvalue.intvalue;
              debugfile^.extendedint := (constvalue.intvalue < 0) and
                                        not constvalue.negated;
              end;
            if (debugfile^.constform = scalars) and (debugfile^.i = 0) then
              begin
              put(debugfile);
              seek(debugfile, f^.dbgsymbol);
              if debugfile^.firstscalar = 0 then {not an alias}
                begin
                debugfile^.firstscalar := lastdebugrecord - 1;
                put(debugfile);
                end;
              seek(debugfile, lastdebugrecord);
              end;
            end;
          varname, param, varparam, fieldname, procparam, funcparam, confparam,
          varconfparam, boundid:
            begin
            if (level > 1) and (namekind <> fieldname) and
               (varalloc in [pointeralloc, normalalloc]) then
              debugfile^.offset := offset - display[level].blocksize
            else if varalloc in [sharedalloc, usealloc, definealloc] then
              debugfile^.offset := sparelink { index into vartable }
            else debugfile^.offset := offset;
            debugfile^.length := length;
            debugfile^.varalloc := varalloc;
            if bigcompilerversion then f := ref(bigtable[vartype])
            else areadaccess(vartype, f);
            debugfile^.vartype := f^.dbgsymbol;

            { This is a kludge. sparelink is used to point to parent type..}
            { and only for the first field names... }
            if (namekind = fieldname) and (sparelink <> 0) then
              begin
              { idno is set for the first field name }
              if bigcompilerversion then f := ref(bigtable[sparelink])
              else areadaccess(sparelink, f);
              put(debugfile);
              seek(debugfile, f^.dbgsymbol);
              debugfile^.firstfield := lastdebugrecord - 1;
              put(debugfile);
              seek(debugfile, lastdebugrecord);
              end;
            if (namekind = boundid) and (sparelink <> 0) then
              begin
              if bigcompilerversion then f := ref(bigtable[sparelink])
              else areadaccess(sparelink, f);
              put(debugfile);
              seek(debugfile, f^.dbgsymbol);
              debugfile^.lowbound := lastdebugrecord - 2;
              put(debugfile);
              seek(debugfile, lastdebugrecord);
              end;
            end;
          procname, funcname:
            begin
            debugfile^.funclen := funclen;
            if proctable[display[level].blockref].registerfunction <> 0 then
              debugfile^.funcoffset := - display[level].blocksize
            else debugfile^.funcoffset := display[level].paramsize;
            if proctable[display[level].blockref].externallinkage then
              debugfile^.funcoffset := debugfile^.funcoffset + extreturnlinksize
            else
              debugfile^.funcoffset := debugfile^.funcoffset + returnlinksize;
            debugfile^.id := display[level].blockid;
            debugfile^.level := level;
            debugfile^.paramsize := display[level].paramsize;
            debugfile^.blocksize := display[level].blocksize;
            debugfile^.entryaddress := 0;
            if bigcompilerversion then f := ref(bigtable[functype])
            else areadaccess(functype, f);
            debugfile^.functype := f^.dbgsymbol;
            if newdebugger then
              begin
              debugfile^.firststmt := display[level].firststmt;
              debugfile^.laststmt := display[level].laststmt;
              end
            else debugfile^.firststmt := 0;
            end;
          otherwise;
          end;
        put(debugfile);
        end;
  end {dumpname} ;


procedure dumpsymboltable {level: levelindex; (level of block to dump) regok:
                           boolean (ok to allocate global reg variables)} ;

{ PDB only
Dump the symbol table entries for this block to the debug file.
First all forms are dumped, followed by the name table for the
block. The first name entry is the block name, then
parameters, then other names
}

  var
    i: index; {for stepping through symbol table}
    p: entryptr; {for pointing to what we step on}
    t: integer; {for tracking this block's name definition}
    firstfieldname: targetint; {debugrecord containing first field name}


  begin {dumpsymboltable}
    with display[level] do
      begin
      {Dump the forms defined locally}
      for i := oldtabletop + 1 to tabletop do
        begin
        if bigcompilerversion then p := ref(bigtable[i])
        else areadaccess(i, p);
        if p^.form then if not p^.disposable then dumpform(p, i, level);
        end;

      seek(debugfile, lastdebugrecord + 1);

      if bigcompilerversion then p := ref(bigtable[blockname])
      else areadaccess(blockname, p);

      t := lastdebugrecord + 1;

      genint(t + 1);

      for i := blockname to p^.paramlist do
        begin {dump block and parameters}
        if bigcompilerversion then p := ref(bigtable[i])
        else areadaccess(i, p);
        if not p^.form then dumpname(p, level, i)
        end;

      { don't dump field names on this pass }
      for i := oldtabletop + 1 to tabletop do
        begin {dump all other names}
        if bigcompilerversion then p := ref(bigtable[i])
        else areadaccess(i, p);
        if not p^.form then
          if not (p^.namekind in
             [fieldname, procname, funcname, forwardproc, forwardfunc,
             externalproc, externalfunc, param, varparam, funcparam, procparam,
             boundid, confparam, varconfparam]) then
            begin
            dumpname(p, level, i);
            { tell travrs about any var that may be assigned to a register }
            if regok and (p^.namekind = varname) and p^.registercandidate then
              possibletemp(p^.offset, p^.vartype, lastdebugrecord);
            end;
        end;

      {set symbol table limits into block entry in debugfile}
      seek(debugfile, t + 1);
      debugfile^.firstname := t;
      debugfile^.lastname := lastdebugrecord - 1;
      debugfile^.nextprocedure := 0;
      put(debugfile);
      seek(debugfile, lastdebugrecord + 1);

      { now dump field names where they won't clog up pdb }

      firstfieldname := lastdebugrecord + 1;
      for i := oldtabletop + 1 to tabletop do
        begin {dump all other names}
        if bigcompilerversion then p := ref(bigtable[i])
        else areadaccess(i, p);
        if not p^.form then
          if p^.namekind = fieldname then dumpname(p, level, i);
        end;
      { fix all the lastfield id's }
      for i := oldtabletop + 1 to tabletop do
        begin
        if bigcompilerversion then p := ref(bigtable[i])
        else areadaccess(i, p);
        if p^.form then
          if not p^.disposable and (p^.typ = fields) then
            begin
            seek(debugfile, p^.dbgsymbol);
            debugfile^.lastfield := lastdebugrecord - 1;
            if debugfile^.firstfield = 0 then
              debugfile^.firstfield := firstfieldname;
            put(debugfile);
            end;
        end;
      { build linked list of procedure entries, speed up pdb!}
      if lastprocrecord <> 0 then
        begin
        { set procedure link }
        seek(debugfile, lastprocrecord);
        { point to the name, not the description }
        debugfile^.nextprocedure := t;
        put(debugfile);
        end;
      lastprocrecord := t + 1;
      if not scanalys then seekstringfile(consttablelimit);
      end
  end {dumpsymboltable} ;


procedure positionstmtfile {linecount: integer; var stmtno: integer} ;

 { Move stmtfile to current line }


  begin {positionstmtfile}
    case targetopsys of
      rsx, rsts, rt, cpp:
        begin
        while not eof(stmtfile) and (stmtfile^.lineno < linecount) do
          get(stmtfile);
        if stmtfile^.lineno = linecount then stmtno := stmtfile^.stmtno;
        end;
      vms, unix, vdos, msdos, apollo:
        begin
        while not eof(stmtfile) and (stmtfile^.lineno < linecount) and
              ((stmtfile^.typ = plabrec) or stmtfile^.exit) do
          get(stmtfile);
        if stmtfile^.lineno = linecount then stmtno := stmtfile^.proclinenr;
        end;
      end {case} ;
  end {positionstmtfile} ;


procedure updatestmtfile {linecount, temppos1, temppos2: integer} ;


  begin {updatestmtfile}
    while not eof(stmtfile) and (stmtfile^.lineno = linecount) do
      begin
      stmtfile^.filepos1 := temppos1;
      stmtfile^.filepos2 := temppos2;
      put(stmtfile);
      end;
  end {updatestmtfile} ;


procedure dbg_alloc {index: integer; (index into debugfile) new_alloc:
                     allockind; (new allocation of var) new_offset:
                     addressrange (new register number)} ;

{ Called by improve to dump register allocation info into the debugfile.
  Hidden in a language-depended routine to keep improve sharable between
  C, Modula-2 and Pascal.
}


  begin {dbg_alloc}
    seek(debugfile, index);
    debugfile^.varalloc := new_alloc;
    debugfile^.offset := new_offset;
    put(debugfile);
  end {dbg_alloc} ;


procedure dbg_regs {(index: m_symbolindex; {procedure name to be updated}
                   {var reg: array[lo..hi: integer] of boolean;
                   {var pc_index: integer) {map file index for pc} ;

{ Modify the symbol file to reflect the actual register usage for a procedure.
  Used by code after register usage is known.  pc_index, if non-zero, is
  the location in the map file to place the function starting pc.
}

  var
    i: integer;


  begin { dbg_regs }
    seek(debugfile, index + 2); {get procdescm}

    { NOTE: Left unspecified.  Choice must be made to either map to existing
      debugfile format or to change.  If change is the answer it might be
      wise to preserve the old debug format for those systems where PDB is
      still in use, rather than burning bridges to the past arbitrarily.
    }

    pc_index := 0; { We don't want the caller to touch the map file, yet}
    writeln('dbg_regs not yet implemented for P2 ODB interface');

    put(debugfile);
  end {dbg_regs} ;


procedure closed;

{ close debugger file
}


  begin {closed}
    close(debugfile);
  end {closed} ;
