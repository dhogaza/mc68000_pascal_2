{$nomain}
{[b+,l+,a+,o=80]}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1984 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045  Level: 1
  Processor: ~processor~
  System: ~system~

  Pascal-2 compiler object file generator.

 Last modified by KRIS on 21-Nov-1990 15:31:24 
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}


const
  objreslen = objtypesx(0, {objnorm}
                        0, {objext}
                        3, {objforw}
                        0, {objsup}
                        3, {objcom}
                        0, {objoff}
                        4, {objpic}
                        0, {objign}
                        2 {objlong}
                        );
  {[f+]}


procedure put_diags;

{ Generate one procedure's worth of incore diagnostic tables.

  The tables are highly packed, with characters Huffman encoded and
  numbers stored in a compressed format.  They are bit packed with
  no regard for word boundaries.

  The syntax of the tables is:

  tables = [* lines
              [ ("diag_proc" procname) |
                ("diag_err" pcdiff, errorno ) ] *]  .

  procname = character [* character *] ' '  .
}

{ The following declarations define the translation from the upper case
  alphanumerics to a Huffman code.  The code values were determined with
  statistics from a large number of Pascal programs, and take an average
  of 4.5 bits per character.
}

  const
    hm_chars = 39;
    hm_max = 15;

  type
    hm_code = array [' '..'_'] of
        record
          length: 0..hm_max;
          value: unsigned;
        end;

  const

    hm_value = hm_code((3, 5), (0, 0), (0, 0), (0, 0), (14, 4670), (0, 0),
                       (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),
                       (0, 0), (0, 0), (0, 0), (12, 1166), (10, 290), (10, 295),
                       (12, 1165), (13, 2329), (13, 2334), (14, 4656),
                       (15, 9315), (15, 9314), (14, 4671), (0, 0), (0, 0),
                       (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (4, 7), (6, 10),
                       (5, 8), (5, 4), (3, 7), (6, 4), (6, 11), (6, 19), (4, 12)
                       , (10, 294), (7, 5), (4, 13), (5, 19), (4, 5), (5, 0),
                       (5, 18), (9, 144), (4, 6), (4, 8), (4, 3), (6, 3),
                       (7, 37), (7, 11), (7, 4), (7, 10), (9, 146), (0, 0),
                       (0, 0), (0, 0), (0, 0), (5, 3));

  var
    i: 1..maxprocnamelen; {induction var for writing procedure names}
    codewords: addressrange; {pc in current procedure (in words)}
    thisnode: nodeindex; {induction var for scanning nodes}
    n: nodeptr; {used to access thisnode}
    firststmt: boolean; {first statement in the procedure}
    stringfilebyte: hostfilebyte;
    name_part: shortint;
    ch: 0..255;
    filenameindex: integer;


  procedure diag_number(n: integer);

{ Generate a compressed format number.  The format was determined
  empirically from data gathered over a large selection of Pascal
  programs.

  0..2          2 bits of n
  3..17         2 bits = 3, 4 bits of (n-3)
  19..48        2 bits = 3, 4 bits = 15, 5 bits of (n-18)
  otherwise     2 bits = 3, 4 bits = 15, 5 bits = 31, 16 bits of n

  cute, huh?
}


    begin
      if (n >= 0) and (n < 3) then diag_bits(n, 2)
      else
        begin
        diag_bits(3, 2);
        if (n >= 0) and (n < 18) then diag_bits(n - 3, 4)
        else
          begin
          diag_bits(15, 4);
          if (n >= 0) and (n < 49) then diag_bits(n - 18, 5)
          else
            begin
            diag_bits(31, 5);
            diag_bits(n, 16);
            end;
          end;
        end;
    end; {diag_number}



  procedure diag_line(line: integer; {new line reference}
                      pc: addressrange {pc at that line} );

  { Generate code to indicate that there is a new line number "line"
    beginning at "pc" in the code.  Again, this is very tight coding,
    based on actual statistics.  The default assumption in the diagnostic
    tables is that we are keeping track of lines.  Anything else is treated
    as an exception.  The syntax is:

    lines = [* [* pcinc *]
               "0"
               ( lineinc |
                 ("0"
                  <other stuff>)) *]  .

    pcinc = number  . (add one to line, number to pc)

    lineinc = number  . (add number to line)

    Thus the otherwise illegal value "0" is used to flag a change in the
    data.  Normally we have a series of pcincs with an occasional lineinc
    thrown in.
  }


    begin
      if (pc <> lastdiagpc) and (line <> lastdiagline) then
        begin { we have a change in both, note it }
        if line <> lastdiagline + 1 then
          begin {set in lineinc before the pcinc}
          diag_number(0); {flag change to lineinc}
          diag_number(line - lastdiagline - 1);
          end;
        diag_number(pc - lastdiagpc); {line := line + 1, pc = pc + pcinc}
        lastdiagpc := pc;
        lastdiagline := line;
        end;
    end; {diag_line}


  begin {put_diags}
    if not everdiagnosing then initdiags;

    { first generate the procedure name }
    diag_number(0); {end of lineincs}
    diag_number(0); {end of lines, on to something else}
    case language of
      modula2:
        if proctable[blockref].calllinkage = modulebody then
          diag_number(diag_mod_m2)
          else diag_number(diag_proc_m2);
      pascal:
        if level > 1 then diag_number(diag_proc_p2) {say this is a procedure}
        else diag_number(diag_prog_p2);
      c: diag_number(diag_proc_c);
      end;
    if proctable[blockref].charindex <> 0 then
      begin
      if needcaching then
        seekstringfile(stringfilecount + proctable[blockref].charindex - 1)
      else
        begin
        curstringblock := (stringfilecount + proctable[blockref].charindex -
                          1) div (diskbufsize + 1) + 1;
        stringblkptr := stringblkptrtbl[curstringblock];
        nextstringfile := (stringfilecount + proctable[blockref].charindex -
                          1) mod (diskbufsize + 1);
        end;
      for i := 1 to proctable[blockref].charlen do
        begin
        stringfilebyte := getstringfile;
        case language of
          modula2:
            diag_bits(stringfilebyte, 8);
          pascal:
            diag_bits(hm_value[uppercase(chr(stringfilebyte))].value,
                      hm_value[uppercase(chr(stringfilebyte))].length
		     );
          end;
        end;
      end
    else
      for i := 1 to 10 do
        if outputname[i] <> ' ' then
          case language of
            modula2:
              diag_bits(ord(outputname[i]), 8);
            pascal:
              diag_bits(hm_value[uppercase(outputname[i])].value,
                        hm_value[uppercase(outputname[i])].length);
            end;

    { mark the end with a blank }
    case language of
      modula2:
        diag_bits(ord(' '), 8);
      pascal:
        diag_bits(hm_value[' '].value, hm_value[' '].length);
      end;

    {Now scan the code, generating table entries for errors and lines}
    codewords := highcode div 2;
    thisnode := 1;
    firststmt := true;
    while thisnode < lastnode do
      begin
      if bigcompilerversion then n := ref(bignodetable[thisnode])
      else creadaccess(thisnode, n);
      with n^ do
        if kind = instnode then
          begin
          codewords := codewords + n^.computed_length div 2;
          thisnode := thisnode + oprndcount;
          end
        else if kind = labeldeltanode then codewords := codewords + 1
        else if kind = errornode then
          begin {write an error label}
          diag_number(0); {first end lines}
          diag_number(0);
          diag_number(diag_error); {say it is an error}
          diag_number(codewords - lastdiagpc); {pc increment}
          diag_number(errorno); {actual error number}
          end
        else if kind = stmtref then
          begin
          if filename <> 0 then
            begin
            diag_number(0);
            diag_number(0);
            diag_number(0);
            diag_number(0);

            filenameindex := filename;
            filenameindex := stringfilecount + filenameindex - 1; {skip strings}

            if needcaching then
              seekstringfile(filenameindex)
            else
              begin
              curstringblock := filenameindex div (diskbufsize + 1) + 1;
              stringblkptr := stringblkptrtbl[curstringblock];
              nextstringfile := filenameindex mod (diskbufsize + 1);
              end;

            { Pull the null terminated filename from the stringfile and
              write it as 8 bit ascii to the diag table.
            }
	    ch := getstringfile;
            name_part := 0; {dev(0):usrno(1).cat(2).fname(3).ext(4)}
	    while (ch <> 0) and (chr(ch) <> ' ') do
	      begin
              if hostopsys <> unix then ch := ord(uppercase(chr(ch)));
              if hostopsys = vdos then
                begin 
                if (ch = ord(':')) or (ch = ord('.')) then
                  name_part := name_part + 1
                else if name_part = 3 then diag_bits(ch, 8);
                end
	      else diag_bits(ch, 8);
	      ch := getstringfile;
	      end;
              
            diag_bits(ord(' '), 8);
            diag_number(sourceline - lineoffset);
            lastdiagline := sourceline - lineoffset;
            end
          else if firststmt then firststmt := false
          else diag_line(sourceline - lineoffset, codewords);
          end;
      thisnode := thisnode + 1;
      end;
    diag_line(lastdiagline + 1, codewords);
    newsection(codesect);
  end; {put_diags}



procedure findlabelpc(labelno: integer; {label to match}
                      var forwardlab: integer {forward reference} );
 { Search the label table for a match on the label number parameter.
  Return the label's address in "labelpc".  If not found the label is
  a forward non-local goto, which must be appended to the fixup list.
}

  var
    lscan: labelindex; { induction var for scanning label table }
    found: boolean; { boolean to control loop escape }


  begin
    lscan := 1; { start with first entry in labeltable }
    found := false;
    forwardlab := 0;

    while (lscan <= nextlabel) and not found do
      if labeltable[lscan].labno = labelno then found := true
      else lscan := lscan + 1;

    if found then labelpc := labeltable[lscan].address
    else
      begin { an entry for future fixup }
      labelpc := 0; { don't use undefinedaddr here }
      forwardlab := 1;
      end;
  end; { findlabelpc }




procedure starttempgroup(length: addressrange);

{ Make sure that a group of "length" bytes will all fit in a single
  tempbuffer
}


  begin
    if nexttempbuffer > maxtempbuffer - (length + 1) div 2 then
      flushtempbuffer;
  end; {starttempgroup}



procedure insertobj(data: uns_word);


  begin { insertobj }
    objctr := objctr + 1;
    relocn[objctr] := false; { assume its not relocatable }
    fixups[objctr] := nil;
    object[objctr] := data;
    objtype[objctr] := objnorm;
    currentpc := currentpc + 2;
  end; { insertobj }


procedure puterror(err: puterrortype);


  begin
    if lineerrors < maxerrs then lineerrors := lineerrors + 1;
    pcerrortable[lineerrors] := err;
    totalputerr := totalputerr + 1;
  end; {puterror}


procedure dumperrors;

  var
    i: 0..maxerrs; { to cycle thru errortable }


  begin
    for i := 1 to lineerrors do
      begin
      write(MacFile, stars);

      case pcerrortable[i] of
        endofnodes: writeln(MacFile, 'end of nodes');
        badrelative: writeln(MacFile, 'bad relative');
        bitindexmode: writeln(MacFile, 'bit indexed mode');
        baddisplacement: writeln(MacFile, 'bad displacement');
        nolongaddrs: writeln(MacFile, 'no long addrs');
        badsupportcall: writeln(MacFile, 'bad support call');
        missingoperand: writeln(MacFile, 'missing operand');
        missinginst: writeln(MacFile, 'missing instruction');
        badoffset: writeln(MacFile, 'bad offset');
        badoperand: writeln(MacFile, 'bad operand');
        unknowninst: writeln(MacFile, 'unknown instruction');
        badoprndlength: writeln(MacFile, 'bad operand length');
        missingDreg: writeln(MacFile, 'missing D-register');
        missingAreg: writeln(MacFile, 'missing A-register');
        badopcount: writeln(MacFile, 'bad operand count');
        badsource: writeln(MacFile, 'bad source operand');
        baddestination: writeln(MacFile, 'bad destination operand');
        nomodeoprnd: writeln(MacFile, '"no mode" operand');
        negativesp: writeln(MacFile, 'negative offset to SP');
        nolabelnode: writeln(MacFile, 'missing label node');
        badpcindexed: writeln(MacFile, 'pc indexed mode disallowed');
        badsize: writeln(MacFile, 'bad data size');
        end; {case err}
      end; {for loop}
  end; {dumperrors}



procedure getnextnode;

{ localizes calls to "creadaccess", which accepts a nodeindex
  request and returns a node pointer result.
}


  begin
    if currnode >= lastnode then puterror(endofnodes)
    else
      begin
      currnode := currnode + 1;
      if bigcompilerversion then n := ref(bignodetable[currnode])
      else creadaccess(currnode, n);
      end
  end; {getnextnode}


procedure lookahead(n: integer);

{ Return with "p" pointing "n" nodes away.  Similar to "getnextnode".
}


  begin {lookahead}
    if currnode + n > lastnode then puterror(endofnodes)
    else if bigcompilerversion then p := ref(bignodetable[currnode + n])
    else creadaccess(currnode + n, p)
  end; {lookahead}


procedure getoperand;

 { get another node and give error if not an operand }


  begin
    getnextnode;
    if n^.kind <> oprndnode then
      begin
      puterror(missingoperand);
      currinst := nop; { to facilitate recovery }
      end { if }
  end; { getoperand }


procedure getprevoperand(num: integer);

   { Fetch the node NUM nodes back and give error if not an operand.  This is
     required for some 68020 two word instructions in which the effective
     address descriptors must follow the second instruction word, but
     the information is passed from genblk in the first operand.

     There is no need to move forward again after this call because
     "getoperand" will increment and use "currinst" which this procedure
     leaves unchanged.

     Returns with the global "n" pointing to the node.
   }


  begin { getprevoperand }
    if bigcompilerversion then n := ref(bignodetable[currnode - num])
    else creadaccess(currnode - num, n);

    if n^.kind <> oprndnode then
      begin
      puterror(missingoperand);
      currinst := nop; { to facilitate recovery }
      end { if }
  end; { getprevoperand }



procedure writemaprecord(typ: maprecordtype;
                         flags, lineno, info: integer);
 { write record to map file }


  begin
    stmtfile^.pc := currentpc;
    stmtfile^.typ := typ;
    stmtfile^.exit := (flags and 8) <> 0;
    stmtfile^.profile := (switchcounters[profiling] > 0) and ((typ = plabrec) or
                         ((typ = stmntrec) and (lineno <> 0)));
    stmtfile^.lineno := lineno;
    case typ of
      plabrec: stmtfile^.recordnr := info;
      stmntrec: stmtfile^.proclinenr := info;
      end;
    stmtfile^.filepos1 := lineno;
    put(stmtfile);
    lastmaprecord := lastmaprecord + 1;
  end;





procedure writeobjline;

  var
    i: 1..maxwords;

{ When called, one complete instruction has been converted to binary in the
  word array "object."  A parallel array of booleans, "relocn", contains the
  relocation status of each word (true = relocate).  Writeobjline transfers
  this data (word and boolean) to putbuffer, which fills corresponding 
  32 element arrays.  When those buffers are filled, they in turn are trans-
  ferred to the tempobj file buffer for an eventual put to relfile.
}


  begin
    if switcheverplus[outputobj] then
      for i := 1 to objctr do
        begin
        if (objtype[i] = objoff) or (objtype[i] = objign) then
          begin
          if fixups[i] <> nil then
            fixups[i]^.fixupfileloc := tempfilesize + nexttempbuffer + 4;
          putdata(object[i]);
          end
        else
          begin
          if objtype[i] = objlong then starttempgroup(4)
          else if objtype[i] = objcom then starttempgroup(6)
          else if objtype[i] = objforw then starttempgroup(6)
          else if objtype[i] = objpic then starttempgroup(8);

          if fixups[i] <> nil then
            fixups[i]^.fixupfileloc := tempfilesize + nexttempbuffer + 4;

          putbuffer(object[i], relocn[i]);
          end;
        end;

    if testing and not skip_macro_details then
      begin
      if column >= nodecolumn then
        begin
        writeline;
        writech('*');
        end;

      reposition(nodecolumn);

   { "column" has served its purpose and is ignored until the next "writeline" }

      { pc when opcode was scanned }
      write(MacFile, '(', instindex: 3, ')   ', lastobjpc: - 4, '  ');
      WriteHex(object[1]); { write opcode in hexadecimal }

      for i := 2 to objctr do
        begin
        if not (objtype[i] in [objcom, objforw, objpic, objign]) then
          writech(' ');
        case objtype[i] of
          objnorm, objoff, objlong: WriteHex(object[i]);
          objext: write(MacFile, 'xxxx xxxx');
          objforw: ;
          objsup:
            begin
            write(MacFile, 'ssss');
            if switcheverplus[longlib] then write(MacFile, ' ssss');
            end;
          objcom: ;
          end;
        end;
      end; { testing dump }

    lastobjpc := currentpc;

    if switcheverplus[outputmacro] then
      begin
      writeline;
      dumperrors; { if any }
      end;
  end; { writeobjline }

function dump_externals;

    { Put out the debugger entries for externals.
    }

  var
    vtptr: vartablerecptr;
    j: integer;
    temp: unsigned;
    ctr: integer;


  begin {dump_externals}
    skip_macro_details := true;
    objctr := 0;
    ctr := 0;

    if lastvartableentry > 0 then
      for j := 1 to lastvartableentry do
        begin
        vtptr := getvartableptr(j);
        with vtptr^ do
          ctr := ctr + long;
        end;

    temp := ctr;
    insertobj(temp div 16#10000); { high order }
    insertobj(temp mod 16#10000); { low order }

    if switcheverplus[outputmacro] then
      begin
      reposition(opcolumn);
      writestr('DC.L');
      reposition(opndcolumn);
      writeint(ctr);
      end;

    writeobjline;

    if lastvartableentry > 0 then
      begin

      for j := 1 to lastvartableentry do
        begin
        objctr := 0;
        vtptr := getvartableptr(j);
        with vtptr^ do
          case extvaralloc of
            definealloc:
              begin
              newESD.ESDkind := ESDdefine;
              newESD.vartabindex := j;
              findESDid;

              insertobj(54B * 256 + datasection + 1);
              relocn[objctr] := true; { tag the word relocatable }
              objtype[objctr] := objforw;
              currentpc := currentpc - 2; { apply the PC correction }
              temp := vtptr^.offset;
              insertobj(temp div 16#10000); { high order offset in psect }
              objtype[objctr] := objoff;
              insertobj(temp mod 16#10000); { low order offset in psect }
              objtype[objctr] := objoff;
              end;

            sharedalloc:
              if referenced then
                begin
                { Named common esd's must go out after all xref's and xdef's, so
                  we must patch in the ESDID in writedatarecords.
                }
                insertobj(50B * 256);
                relocn[objctr] := true; { tag the word relocatable }
                currentpc := currentpc + 2; { apply the PC correction }

                allocfixup;
                fixups[objctr] := fixuptail;
                with fixuptail^ do
                  begin
                  fixupkind := fixupesdid;
                  fixupobjpc := fixupobjpc - 4;
                  vartabindex := j;
                  end;
                end
              else
                begin
                insertobj(0);
                insertobj(0);
                end;

            usealloc:
              if referenced then
                begin
                newESD.ESDkind := ESDuse;
                newESD.vartabindex := j;
                findESDid;

                insertobj(50B * 256 + ESDid);
                relocn[objctr] := true; { tag the word relocatable }
                currentpc := currentpc + 2; { apply the PC correction }
                end
              else
                begin
                insertobj(0);
                insertobj(0);
                end;
            end; { case extvaralloc }

        if switcheverplus[outputmacro] then
          begin
          reposition(opcolumn);
          writestr('DC.L');
          reposition(opndcolumn);

          with vtptr^ do
            if referenced or (extvaralloc = definealloc) then
              WriteSymbolName(get_from_sfile(charindex, charlen, not aliased))
            else writech('0');
          end;

        writeobjline;
        end; { for }
      end; { lastvartableentry > 0 }

    dump_externals := ctr;  { used by unix }
  end; {dump_externals}

procedure dump_import_table;

{ Dump the import table for Modula2 to the object file and assembly listing.
}
  var
    procno : proctableindex;
    s: longname;
    temp: unsigned;

  begin {dump_import_table}
    if language = modula2 then { deadcode this stuff otherwise }
      begin
      skip_macro_details := true;
      if switcheverplus[outputmacro] then writeline;

      if switcheverplus[outputmacro] then
        begin
	reposition(opcolumn);
	writestr('SECTION');
	writestr(sectiontail);
	reposition(opndcolumn);
	writeint(sectionno[codesect]);
	writeline;
        end
      else
writeln('NYI');
      { If this is not a program module, make module-table a global symbol.
      }
      if not switcheverplus[outputmacro] then
        procmap[import_table_ref].addr := currentpc
      else if proctable[1].calllinkage = implementationbody then
        begin
        reposition(opcolumn);
        writestr('XREF');
        reposition(opndcolumn);
        import_name(1, s);
        WriteSymbolName(s);
        writeobjline;

        { Label the import list.  (Non-main body)
        }
        import_name(1, s);
        WriteSymbolName(s);
        writech(':');
        writeobjline;
        end
      else
        begin
        { Label the import list.  (main body)
        }
        writestr('p_main');
        writech(':');
        writeobjline;
        end;

      { Generate a pointer to the procedure constituting the module body.
        The name is always p1.
      }
      if switcheverplus[outputmacro] then
        begin
        reposition(opcolumn);

        writestr('DC.L');
        reposition(opndcolumn);
        WriteSymbolName('P1');
        if level = 1 then
          begin
          writech('+');
          writeint(main_import_offset);
          end;
        end
      else
        begin
writeln('NYI');
        end;

      writeobjline;

      for procno := 1 to proctabletop do
        begin
        if (proctable[procno].calllinkage = definitionbody) and
           (proctable[procno].level = 1) {directly imported} then
          if switcheverplus[outputmacro] then
            begin
            reposition(opcolumn);
            writestr('XREF');
            reposition(opndcolumn);
            import_name(procno, s);
            WriteSymbolName(s);
            writeobjline;
            reposition(opcolumn);

            writestr('DC.L');
            reposition(opndcolumn);
            import_name(procno, s);
            WriteSymbolName(s);
            writeobjline;
            end {if macro output}
          else
            begin
            newESD.ESDkind := ESDexternal;
            newESD.exproc := procno;
            findESDid;

            insertobj(50B * 256 + ESDid);
            relocn[objctr] := true; { tag the word relocatable }
            writeobjline;
            end;
          end; {for}

        { Now generate a -1 to terminate the table.
        }
        if switcheverplus[outputmacro] then
          begin
          reposition(opcolumn);

          writestr('DC.L');
          reposition(opndcolumn);
          writech('$');
          writehexlong(16#ffffffff);
          end
        else
          begin
          objctr := 0;
          insertobj(16#ffff);
          insertobj(16#ffff);
          end;

        writeobjline;
      end; {if modula2}
  end {dump_import_table} ;



procedure setmodeonly;

{ scan the current operand node pointed to by "n" and set "mode" to
  the appropriate mode and register pair (six bits)
}

 { 68020 scale factor encoding }

  type
    scale_factor_type = array [1..8] of integer;

  const {  scale factor in bytes --> 1  2  .  4  .  .  .  8 }
    scale_factor = scale_factor_type(0, 1, 0, 2, 0, 0, 0, 3);

    word_bd_size = 2; { 68020 base displacement size for a word }
    long_bd_size = 3; { 68020 base displacement size for a long }

  var
    isforward: integer; {true if forward reference}
    vtptr: vartablerecptr;
    temp: unsigned;
    extension_size: integer;
    kluge:
      record
        case integer of
          1: (l: integer {long word} );
          2: (w: packed array [boolean] of - 32767..32767);
      end {kluge} ;


  procedure pic_gen;

      { For $pic only.

        This procedure will generate a pcrelative mode for an external
        symbol.  First the relocated external symbol is added to the
        negative of the offset in the module, then the base of the current
        section is subtracted to kill the relocation of the external symbol.
        The result is the distance to the desired object.

        The ESDID of the current section is the section number + 1.  A
        do-nothing zero ESDID is added in to align the file to a word.

        NOTE:  Variable ESDID must be setup prior to call.
      }

    var
      temp_esdid: integer;


    begin {pic_gen}
      if mc68020 then
        begin
        { Generate a "PC indirect with index (base displacement)"
          mode with the index register suppressed.
        }
        extension_size := long_bd_size;

        insertobj( {no index reg}
        {no index size}
        {no scale}
                  + 400B {bit 8}
        {BS = 0}
                  + 100B {IS = 1}
                  + (extension_size * 20B) {BD size}
                  + 000 {index/indirect selection}
        {no indirection}
                  );
        end;

      temp_esdid := ESDid;

      insertobj(16#6000 { Emit 3 ESDID's }
                + (ord(mc68020) * 16#800) {1 word reloc'ed, 2 for 020}
                + (ord(mc68020) * 16#200 + 16#200) {2 disp bytes, 4 for 020}
                + temp_esdid); { first ESDID }
      relocn[objctr] := true; { tag the word relocatable }
      objtype[objctr] := objpic; { be sure this won't span buffers }
      insertobj((sectionno[codesect] + 1) * 256);
      { ^^^ a zero ESDID is emitted to keep file word aligned }
      objtype[objctr] := objign;
      currentpc := currentpc - 4; { last two words don't go into memory }

      if mc68020 then
        begin

        { Generate a long offset.  The pc is the address of the extension word.
        }
        insertobj(( - currentpc + 2) div 16#10000);
        objtype[objctr] := objoff;
        insertobj(( - currentpc + 4) mod 16#10000);
        objtype[objctr] := objoff;
        mode := 73B; { pcrelative, long disp }
        end
      else
        begin
        insertobj( - currentpc);
        objtype[objctr] := objoff;
        mode := 72B; { pcrelative }
        end;
    end; {pic_gen}


  begin {setmodeonly}
    with n^.oprnd do
      case m of

        nomode: puterror(nomodeoprnd);

        fpreg: puterror(badoperand);

        dreg:
          begin
          mode := reg;
          end; {dreg}

        areg:
          begin
          mode := reg + 10B;
          end; {areg}

        indr:
          begin
          mode := reg + 20B;
          end; {indr}

        autoi:
          begin
          mode := reg + 30B;
          end; {autoi}

        autod:
          begin
          mode := reg + 40B;
          end; {autod}

        relative:
          begin
            { This generates the 68000 mode "address register indirect with
              displacement" which has a 16 bit displacement.  If a long word
              displacement is needed on the mc68020, we generate an "address
              register indirect with index (base displacement)" mode with the
              index register suppressed.
            }
          if (reg = 7) and (offset < 0) then puterror(negativesp);

          if (offset > 32767) or (offset < - 32768) then
            begin
            if mc68020 then
              begin
              mode := reg + 60B;

              extension_size := long_bd_size;

              insertobj( {no index reg}
              {no index size}
              {no scale}
                        + 400B {bit 8}
              {BS = 0}
                        + 100B {IS = 1}
                        + (extension_size * 20B) {BD size}
                        + 000 {index/indirect selection}
              {no indirection}
                        );

                { Generate long offset
                }
              kluge.l := offset;
                { the next two lines assume that "reversebytes" implies that
                  words are also reversed. }
              insertobj(kluge.w[reversebytes]);
              insertobj(kluge.w[not reversebytes]);
              end
            else puterror(baddisplacement);
            end
          else
            begin {mc68000 16-bit register indirect mode}
            mode := reg + 50B;
            insertobj(offset);
            end;
          end; {relative}

        indexed:
            { This generates the 68000 mode "address register indirect with
              index (8-bit displacement)".  If a word or long word displacement 
              is needed on the mc68020, we generate an "address register 
              indirect with index (base displacement)" mode.  Suppression of
              the base and index registers is currently not supported.
            }
          begin
            { NOTE:  The scale is always 1 for the 68000, but may be 1, 2, 4
              or 8 for the 68020.
            }
          mode := reg + 60B;

          if (offset > 127) or (offset < - 128) then
            begin
            if mc68020 then
              begin
              if (offset <= 32767) and (offset >= - 32768) then
                extension_size := word_bd_size
              else extension_size := long_bd_size;

              insertobj(indxr * 10000B {index reg}
                        + ord(indxlong) * 4000B {word/long word index size}
                        + (scale_factor[scale] * 1000B) + 400B {bit 8}
              {BS = 0}
              {IS = 0}
                        + (extension_size * 20B) {BD size}
                        + 000 {index/indirect selection}
              {no indirection}
                        );

              if extension_size = long_bd_size then {generate long offset}
                begin
                kluge.l := offset;
                  { the next two lines assume that "reversebytes" implies that
                    words are also reversed. }
                insertobj(kluge.w[reversebytes]);
                insertobj(kluge.w[not reversebytes]);
                end
              else {generate word offset} insertobj(offset and 16#FFFF);
              end
            else {not mc68020} puterror(baddisplacement);
            end
          else {mc68000 or byte displacement}
            begin
            if (offset > 127) or (offset < - 128) then
              puterror(baddisplacement);

            insertobj(indxr * 10000B + ord(indxlong) * 4000B +
                      (scale_factor[scale] * 1000B) + (offset and 16#FF));
              { The scale is always 1 for the 68000, but may be 1, 2, 4 or 8
                for the 68020.
              }
            end;
          end; {indexed}

        bitindexed:
          begin
          puterror(bitindexmode);
          end; {bitindexed}

        absshort:
          begin
          mode := 70B;
          insertobj(offset);
          end; {absshort}

        abslong:
          begin
          mode := 71B;
          kluge.l := offset;
            { the next two lines assume that "reversebytes" implies that
              words are also reversed. }
          insertobj(kluge.w[reversebytes]);
          insertobj(kluge.w[not reversebytes]);
          end; {abslong}

        immediate:
          begin
          mode := 74B;
          if datasize <= word then insertobj(offset)
          else
            begin
            if hostintsize < long then
              begin
              if offset < 0 then insertobj(maxusword) {really -1}
              else insertobj(0);
              insertobj(offset);
              end
            else
              begin
              kluge.l := offset;
                { the next two lines assume that "reversebytes" implies that
                  words are also reversed. }
              insertobj(kluge.w[reversebytes]);
              insertobj(kluge.w[not reversebytes]);
              end;
            end;
          end; {immediate}

        immediatelong:
          begin
          mode := 74B;
          insertobj(offset1); { high order }
          insertobj(offset); { low order }
          end; {immediatelong}

        immediatequad:
          begin
          mode := 74B;

          { The lines below assume that "reversebytes" implies that
            words are also reversed.
          }
          kluge.l := offset1; { high order }
          insertobj(kluge.w[reversebytes]);
          insertobj(kluge.w[not reversebytes]);

          kluge.l := offset; { low order }
          insertobj(kluge.w[reversebytes]);
          insertobj(kluge.w[not reversebytes]);
          end; {immediatelong}

        immediate_extended:
          begin
          mode := 74B;

          { The lines below assume that "reversebytes" implies that
            words are also reversed.
          }
          kluge.l := offset2; { 1st longword }
          insertobj(kluge.w[reversebytes]);
          insertobj(kluge.w[not reversebytes]);

          kluge.l := offset1; { 2nd longword }
          insertobj(kluge.w[reversebytes]);
          insertobj(kluge.w[not reversebytes]);

          kluge.l := offset; { 3rd longword }
          insertobj(kluge.w[reversebytes]);
          insertobj(kluge.w[not reversebytes]);
          end;

        commonlong:
          begin
          mode := 71B; {absolute long}

          if commonlong_reloc < 0 then
            begin
            {first get the common section esdid}
            insertobj(54B * 256); {relocate by common section base (zero)}
            relocn[objctr] := true;
            objtype[objctr] := objcom;
            currentpc := currentpc - 2;
  
            { Output offset within the common block.
            }
            temp := offset; { convert to unsigned }
            insertobj(temp div 16#10000);
            objtype[objctr] := objoff;
            insertobj(temp mod 16#10000);
            objtype[objctr] := objoff;
            end;

          if commonlong_reloc > 0 then
            begin
            vtptr := getvartableptr(commonlong_reloc);

            case vtptr^.extvaralloc of
              definealloc:
                begin
                newESD.ESDkind := ESDdefine;
                newESD.vartabindex := commonlong_reloc;
                findESDid;
                insertobj(54B * 256 + datasection + 1);
                relocn[objctr] := true; { tag the word relocatable }
                objtype[objctr] := objforw;
                currentpc := currentpc - 2; { apply the PC correction }
                temp := offset + vtptr^.offset;
                insertobj(temp div 16#10000); { high order offset in psect }
                objtype[objctr] := objoff;
                insertobj(temp mod 16#10000); { low order offset in psect }
                objtype[objctr] := objoff;
                end;

              sharedalloc:
                begin
                temp := 0;
                if $pic then
                  begin

                  { Generate "#<global>-P_OWN" for a PIC reference to a
                    shared variable, i.e. named common.

                    Named common esd's must go out after all xref's and
                    xdef's, so we must patch in the ESDID in writedatarecords.
                  }
                  insertobj(154B * 256);
                  relocn[objctr] := true; { tag the word relocatable }
                  objtype[objctr] := objpic;
                  mode := 74B; { immediate }

                  allocfixup;
                  fixups[objctr] := fixuptail;
                  with fixuptail^ do
                    begin
                    fixupkind := fixupesdid;
                    vartabindex := commonlong_reloc;
                    end;

                  newESD.ESDkind := ESDsupport;
                  newESD.suppno := libown;
                  findESDid;

                  insertobj(ESDid * 256); { The third ESDID is a "do nothing"
                                            zero to word align the file. }
                  objtype[objctr] := objign;
                  currentpc := currentpc - 4; { apply the PC correction }
                  end
                else
                  begin
                  { Named common esd's must go out after all xref's and xdef's,
                    so we must patch in the ESDID in writedatarecords.
                  }
                  insertobj(54B * 256);
                  relocn[objctr] := true; { tag the word relocatable }
                  objtype[objctr] := objforw;
                  currentpc := currentpc - 2; { apply the PC correction }
  
                  allocfixup;
                  fixups[objctr] := fixuptail;
                  with fixuptail^ do
                    begin
                    fixupkind := fixupesdid;
                    vartabindex := commonlong_reloc;
                    end;
                  temp := offset + vtptr^.offset;
                  end;

                insertobj(temp div 16#10000); { high order offset in psect }
                objtype[objctr] := objoff;
                insertobj(temp mod 16#10000); { low order offset in psect }
                objtype[objctr] := objoff;
                end;

              usealloc:
                begin
                newESD.ESDkind := ESDuse;
                newESD.vartabindex := commonlong_reloc;
                findESDid;

                insertobj(54B * 256 + ESDid);
                relocn[objctr] := true; { tag the word relocatable }
                currentpc := currentpc - 2; { apply the PC correction }
                objtype[objctr] := objforw;
                temp := offset + vtptr^.offset;
                insertobj(temp div 16#10000); { high order offset in psect }
                objtype[objctr] := objoff;
                insertobj(temp mod 16#10000); { low order offset in psect }
                objtype[objctr] := objoff;
                end;
              end;
            end;
          end; {commonlong}

        pcrelative:
          begin
            { Pcrelative is only used to access the current section, so
              we always know exactly what the distance is.

              This generates the 68000 mode "address register indirect with
              displacement" which has a 16 bit displacement.  If a long word
              displacement is needed on the mc68020 (PIC only as it is slow),
              we generate an "PC indirect with index (base displacement)"
              mode with the index register suppressed.  If a long displacement
              is needed in nopic mode on the 68020 we use absolute.  A long
              displacement in pic mode on the 68000 causes an error.
            }

          if $pic and (n^.operandcost >= long) then
            begin
            if mc68020 then
              begin
              mode := 73B;

              extension_size := long_bd_size;

              insertobj( {no index reg}
              {no index size}
              {no scale}
                        + 400B {bit 8}
              {BS = 0}
                        + 100B {IS = 1}
                        + (extension_size * 20B) {BD size}
                        + 000 {index/indirect selection}
              {no indirection}
                        );

              {generate a long offset}
              kluge.l := offset - currentpc + 2; { pc = addr of extension word }
                { the next two lines assume that "reversebytes" implies that
                  words are also reversed. }
              insertobj(kluge.w[reversebytes]);
              insertobj(kluge.w[not reversebytes]);
              end
            else {not mc68020} puterror(baddisplacement);
            end
          else if n^.operandcost >= long then
            begin {far, far away}
            mode := 71B; {absolute long}
            insertobj(54B * 256 + sectionno[codesect] + 1);
            objtype[objctr] := objforw;
            relocn[objctr] := true;
            temp := offset; { convert to unsigned }
            insertobj(temp div 16#10000); {high order}
            objtype[objctr] := objoff;
            currentpc := currentpc - 2; {this stuff's longer than code}
            insertobj(temp mod 16#10000);
            objtype[objctr] := objoff;
            end {long displacement}
          else
            begin {in pcrelative range}
            mode := 72B;
            if offset - currentpc < - 32768 then puterror(baddisplacement);
            insertobj(offset - currentpc); {pc = addr of displacement}
            end;
          end; {pcrelative}

        pcindexed:
            { This generates the 68000 mode "PC indirect with index (8-bit
              displacement)".  If a word or long word displacement is needed
              on the mc68020, we generate an "PC indirect with index (base
              displacement)" mode.  Suppression of the PC and index registers
              is currently not supported.
            }
          begin
          mode := 73B;
            { NOTE:  The scale is always 1 for the 68000, but may be 1, 2, 4
              or 8 for the 68020.
            }

          if (offset > 127) or (offset < - 128) then
            begin
            if mc68020 then
              begin
              if (offset <= 32767) and (offset >= - 32768) then
                extension_size := word_bd_size
              else extension_size := long_bd_size;

              insertobj(indxr * 10000B {index reg}
              {word/long word index size (always word)}
                        + (scale_factor[scale] * 1000B) + 400B {bit 8}
              {BS = 0}
              {IS = 0}
                        + (extension_size * 20B) {BD size}
                        + 000 {index/indirect selection}
              {no indirection}
                        );

              if extension_size = long_bd_size then {generate long offset}
                begin
                kluge.l := offset;
                  { the next two lines assume that "reversebytes" implies that
                    words are also reversed. }
                insertobj(kluge.w[reversebytes]);
                insertobj(kluge.w[not reversebytes]);
                end
              else {generate word offset} insertobj(offset and 16#FFFF);
              end
            else {not mc68020} puterror(baddisplacement);
            end
          else {byte displacement}
            begin
            { note: this mode is only issued as "Dreg.W" }

            insertobj(indxr * 10000B + (scale_factor[scale] * 1000B) +
                      (offset and 16#FF));
            end;
          end; {pcindexed}

        supportcall:
          begin {treat it like an external usercall}
          if (offset < ord(first_call)) or (offset > ord(last_call)) then
            puterror(badsupportcall);

{ note: support call operands generate 2 bytes of relocation data.
  The first is always hexadecimal(20), which signifies one ESD index
  byte follows (with no offset data).  The second is the ESD index
  value, which is the index into the ESD table which has the unique
  occurrence of this support number.  Therefore, we search the table
  to see if this support routine is already present; if not, we enter
  it, and use the newly assigned location for the second byte.
}

          newESD.ESDkind := ESDsupport;
          newESD.suppno := loophole(libroutines, offset);
          findESDid;

          if $pic then pic_gen
          else
            begin
            if switcheverplus[longlib] then
              begin {treat it like an external usercall}
              insertobj(50B * 256 + ESDid);
              mode := 71B; {absolute long}
              currentpc := currentpc + 2; {correct for long address}
              end
            else
              begin
              insertobj(40B * 256 + ESDid);
              mode := 70B; {absolute short}
              end;
            relocn[objctr] := true; { tag the word relocatable }
            objtype[objctr] := objsup;
            end; {not $pic}
          end; {supportcall}

        usercall:
          begin
          if $pic then
            begin
            if proctable[offset].externallinkage and
               not proctable[offset].bodydefined then
              begin {external reference}

                { We must search the ESD table to match the procedure
                  number, thereby calculating the ESDID. 
                }
              newESD.ESDkind := ESDexternal;
              newESD.exproc := offset;
              findESDid;
              pic_gen;
              end
            else
              begin { not external call }
              if procmap[offset].addr = undefinedaddr then
                begin
                if mc68020 then
                  begin
                    { Generate a "PC indirect with index (base displacement)"
                      mode with the index register suppressed.
                    }
                  extension_size := long_bd_size;

                  insertobj( {no index reg}
                  {no index size}
                  {no scale}
                            + 400B {bit 8}
                  {BS = 0}
                            + 100B {IS = 1}
                            + (extension_size * 20B) {BD size}
                            + 000 {index/indirect selection}
                  {no indirection}
                            );

                    { Generate a long offset.  The fixup will plug in the
                      pic displacement.  The pc is the address of the
                      extension word.
                    }
                  temp := offset1 - currentpc + 2; { convert to unsigned }
                  insertobj(temp div 16#10000);
                  objtype[objctr] := objlong;
                  insertobj(temp mod 16#10000);
                  mode := 73B; { pcrelative, long disp }
                  allocfixup;
                  fixups[objctr - 1] := fixuptail;
                  with fixuptail^ do
                    begin
                    fixupkind := fixupproc;
                    fixuplen := long;
                    fixupprocno := offset;
                    fixupobjpc := fixupobjpc - 4;
                    end;
                  end
                else { not mc68020 -- generate simple 16-bit PIC }
                  begin
                  insertobj(offset1 - currentpc);
                  mode := 72B; { pcrelative }
                  allocfixup;
                  fixups[objctr] := fixuptail;
                  with fixuptail^ do
                    begin
                    fixupkind := fixupproc;
                    fixuplen := word;
                    fixupprocno := offset;
                    fixupobjpc := fixupobjpc - 2;
                    end;
                  end; { not mc68020 }
                end { undefined addr }
              else if n^.operandcost >= long then
                begin {far, far away}

                if mc68020 then
                  begin
                    { Generate a "PC indirect with index (base displacement)"
                      mode with the index register suppressed.
                    }
                  extension_size := long_bd_size;

                  insertobj( {no index reg}
                  {no index size}
                  {no scale}
                            + 400B {bit 8}
                  {BS = 0}
                            + 100B {IS = 1}
                            + (extension_size * 20B) {BD size}
                            + 000 {index/indirect selection}
                  {no indirection}
                            );

                    { Generate a long offset.  The pc is the address of the
                      extension word.
                    }
                  insertobj((procmap[offset].addr + offset1 - currentpc + 2) div
                            16#10000);
                  insertobj((procmap[offset].addr + offset1 - currentpc + 4) mod
                            16#10000);
                  mode := 73B; { pcrelative, long disp }
                  end
                else
                  begin
                  write('Reference too long for 16-bit PIC');
                  abort(inconsistent);
                  end;
                end
              else
                begin { simple case -- 16-bit offset to known location }
                mode := 72B; {68000 PC relative}
                insertobj(procmap[offset].addr + offset1 - currentpc);
                end;
              end;
            end {if $pic}
          else
          if proctable[offset].externallinkage and
             not proctable[offset].bodydefined then
            begin {external reference}

{ note: although this operand will be 4 bytes long when eventually loaded,
  it is emitted to the relocatable object file as just 2 bytes.  The first
  is an encoded byte which says one ESD is used to resolve a 4 byte operand;
  the second is the number of that ESD.  We must search the ESD table to
  match the procedure number, thereby calculating the ESD number.  The dump
  file will show only this 2 byte operand, but the PC will increment by 4,
  and the line will be flagged "EXTERNAL".
}

            newESD.ESDkind := ESDexternal;
            newESD.exproc := offset;
            findESDid;

            insertobj(50B * 256 + ESDid);
            relocn[objctr] := true; { tag the word relocatable }
            mode := 71B; { absolute long }
            currentpc := currentpc + 2; { apply the PC correction }
            objtype[objctr] := objext;
            end

          else
            begin { not external call }
            if procmap[offset].addr = undefinedaddr then
              begin {forward procedure call}
              mode := 71B; {absolute long}
              insertobj(54B * 256 + sectionno[codesect] + 1);
              objtype[objctr] := objforw;
              relocn[objctr] := true;
              currentpc := currentpc - 2; {this stuff's longer than code}
              temp := offset1; { convert to unsigned }
              insertobj(temp div 16#10000); {high order}
              objtype[objctr] := objoff;
              insertobj(temp mod 16#10000); { fixup adds proctable addr }
              objtype[objctr] := objoff;
              allocfixup;
              fixups[objctr - 1] := fixuptail;
              with fixuptail^ do
                begin
                fixupkind := fixupproc;
                fixuplen := long;
                fixupprocno := offset;
                fixupobjpc := fixupobjpc - 4;
                end;
              end { undefined addr }

            else if n^.operandcost >= long then
              begin {long, but not forward}
              mode := 71B; {absolute long}
              insertobj(54B * 256 + sectionno[codesect] + 1);
              objtype[objctr] := objforw;
              relocn[objctr] := true;
              insertobj((procmap[offset].addr + offset1) div 16#10000);
              { ^^^ high order }
              objtype[objctr] := objoff;
              currentpc := currentpc - 2; {this stuff's longer than code}
              insertobj((procmap[offset].addr + offset1) mod 16#10000);
              objtype[objctr] := objoff;
              end
            else
              begin {normal call within 32k bytes}
              mode := 72B; {PC relative}
              insertobj(procmap[offset].addr + offset1 - currentpc);
              end;
            end;
          end; {usercall}

        pic_own_immed:
            { In PIC mode this can only occur for the code to load A3
              at the beginning of each procedure.
            }
          begin
          newESD.ESDkind := ESDsupport;
          newESD.suppno := libown;
          findESDid;

          mode := 74B; { immediate }
            { A zero in the first position is a hack for own section
              relocation. }
          insertobj(150B * 256);
          relocn[objctr] := true;
          objtype[objctr] := objlong;
          insertobj(ESDid * 256); { The third ESDID is a "do nothing" zero to
                                   word align the file. }
          objtype[objctr] := objign;
          end; {pic_own_immed}

        pic_splat_pcrel:

          { For 68000 24-bit PIC only.  Generates "<offset>+*(PC)".
          }
          begin
          mode := 72B; { pc-relative }
          insertobj(offset - 2);
          end;

        pic_usercall:

          { For 68000 24-bit PIC only.  Generates "#<name>-<offset1>-*".
          }
          begin
          if proctable[offset].externallinkage and
             not proctable[offset].bodydefined then
            begin {external reference}

              { We must search the ESD table to match the procedure
                number, thereby calculating the ESDID. 
              }
            newESD.ESDkind := ESDexternal;
            newESD.exproc := offset;
            findESDid;

            mode := 74B; { immediate }
            insertobj(154B * 256 + ESDid);
            relocn[objctr] := true;
            objtype[objctr] := objpic;
            insertobj((sectionno[codesect] + 1) * 256);
                  { ^^^ The third ESDid is a "do nothing" zero to word align the
                    file. }
            objtype[objctr] := objign;
            currentpc := currentpc - 4;

            insertobj(( - (offset1 - 2) - currentpc) div 16#10000);
            objtype[objctr] := objoff;
            insertobj(( - (offset1 - 2) - currentpc + 2) mod 16#10000);
            objtype[objctr] := objoff;
            end
          else if procmap[offset].addr = undefinedaddr then
            begin { long forward reference }
            mode := 74B; { immediate }
            insertobj(( - (offset1 - 2) - currentpc) div 16#10000);
            objtype[objctr] := objlong;
            insertobj(( - (offset1 - 2) - currentpc + 2) mod 16#10000);
            allocfixup;
            fixups[objctr - 1] := fixuptail;
            with fixuptail^ do
              begin
              fixupkind := fixupproc;
              fixuplen := long;
              fixupprocno := offset;
              fixupobjpc := fixupobjpc - 4;
              end;
            end { undefined addr }
          else
            begin { simple case -- 24-bit offset to known location }
            mode := 74B; { immediate }
            insertobj((procmap[offset].addr - (offset1 - 2) - currentpc) div
                      16#10000);
            insertobj((procmap[offset].addr - (offset1 - 2) - currentpc + 2) mod
                      16#10000);
            end;
          end;

        pic_supportcall:

          { For 68000 24-bit PIC only.  Generates "#<suppt_call>-<offset1>-*".
          }
          begin
          newESD.ESDkind := ESDsupport;
          newESD.suppno := loophole(libroutines, offset);
          findESDid;

          mode := 74B; { immediate }
          insertobj(154B * 256 + ESDid);
          relocn[objctr] := true;
          objtype[objctr] := objpic;
          insertobj((sectionno[codesect] + 1) * 256);
            { ^^^ The third ESDid is a "do nothing" zero to word align the
              file. }
          objtype[objctr] := objign;
          currentpc := currentpc - 4;

          insertobj(( - (offset1 - 2) - currentpc) div 16#10000);
          objtype[objctr] := objoff;
          insertobj(( - (offset1 - 2) - currentpc + 2) mod 16#10000);
          objtype[objctr] := objoff;
          end;

        pic_branch:

          { For 68000 24-bit PIC only.  Generates "#L<labelno>-<offset1>-*"
            only for Pascal goto's.
          }
          begin
          mode := 74B; {immediate}
          op := op + mode;
          findlabelpc(offset, isforward);

          insertobj((labelpc - (offset1 - 2) - currentpc) div 16#10000);
          insertobj((labelpc - (offset1 - 2) - currentpc + 2) mod 16#10000);

          if isforward <> 0 then
            begin
            objtype[objctr - 1] := objlong;
            allocfixup; { generate a new fixupnode }
            fixups[objctr - 1] := fixuptail;
            with fixuptail^ do
              begin
              fixupkind := fixuplabel;
              fixuplen := long;
              fixuplabno := offset;
              fixupobjpc := fixupobjpc - 4;
              end;
            end;
          end;

        pic_pcrelative:

          { For 68000 24-bit PIC only.  Generates "#L+<offset>-<offset1>-*"
            only for constant section references greater than 32k bytes.
          }
          begin
          mode := 74B; {immediate}
          op := op + mode;
          insertobj((offset - (offset1 - 2) - currentpc) div 16#10000);
          insertobj((offset - (offset1 - 2) - currentpc + 2) mod 16#10000);
          end;
        end; { case m }
  end; {setmodeonly}



procedure seteffective;

{ call setmodeonly to get the current mode, then logically insert mode
  into the low order 6 bits of the current instruction word
}


  begin { seteffective }
    setmodeonly;
    op := op or mode;
  end; { seteffective }


procedure insertsize;

{ Insert size field into bits 6..7 of the main instruction word.
}


  begin
    op := (op and 177477B); {make certain the field is clear}
    if datasize = word then op := op + 100B
    else if datasize = long then op := op + 200B;
  end; {insertsize}


procedure insertreghi;

{ extract register specification from current operand node, shift left
  9 bits, and insert into the current instruction binary in bits 9..11.
}


  begin
    op := (n^.oprnd.reg and 7B) * 1000B + op;
  end; { insertreghi }


procedure insertreglo;

{ extract register specification from current operand node,
  and insert into the current instruction binary in bits 0..2.
}


  begin
    op := (n^.oprnd.reg and 7B) + op;
  end; { insertreglo }



procedure writeinst(inst: insttype);

{ Write the 68000 mnemonic for the current instruction.
  Note that the Motorola assembler accepts only upper case!
}

  var
    mnemonic: string [10];
    i: integer; {induction var for printing mnemonic}
    short_ext, long_ext: char; { Extension character to use for non-Bcc
                                instructions }
    branch_byte_ext, branch_word_ext, branch_long_ext: char; { Extension
      character to use for 68000 or 68020 Bcc insts}


  procedure postcorrect;


    begin
      if mc68020 then
        begin
        branch_byte_ext := 'B';
        branch_word_ext := 'W';
        branch_long_ext := 'L';
        end
      else
        begin
        branch_byte_ext := 'S';
        branch_word_ext := 'L';
        branch_long_ext := '?'; {error}
        end;

      short_ext := 'S';
      long_ext := 'L';

      if inst = jsr then
        begin
        lookahead(1);
        if p^.oprnd.m = supportcall then
          begin
          writech('.');
          if switcheverplus[longlib] then writech(long_ext)
          else writech(short_ext);
          end
        else if (p^.oprnd.m = usercall) and (p^.operandcost > word) then
          begin
          writech('.');
          writech(long_ext); {external, forward, or far away procedure}
          end;
        end {inst = jsr}
      else if inst = jmp then
        begin
        lookahead(1);
        if p^.kind = oprndnode then
          begin
          if p^.oprnd.m = supportcall then
            begin
            writech('.');
            if switcheverplus[longlib] then writech(long_ext)
            else writech(short_ext);
            end;
          end
        else {must be a labelnode}
          begin
          writech('.');
          writech(long_ext);
          end;
        end
    { several names were too long to fill "mnemonic",
      so a post-correction will be applied }

      else if inst = movea then writech('A')
      else if inst = movem then writech('M')
      else if inst = moveq then writech('Q');
      if inst in qualifiedinsts then
        begin
        writech('.');

        case datasize of
          byte: writech('B');
          word: writech('W');
          long: writech('L');
          otherwise puterror(badoprndlength)
          end {case oprndlength}
        end {qualifiedinsts}

      else if (inst in [fp_first..fp_last]) and not (inst in fpbranches) then
        begin
        writech('.');
        case n^.fp_format of
          single_real: writech('S');
          double_real: writech('D');
          extended_real: writech('X');
          byte_integer: writech('B');
          word_integer: writech('W');
          long_integer: writech('L');
          end;
        end
      else if (inst in branches) or (inst in fpbranches) then
        begin
        writech('.');
        lookahead(1);
        if p^.kind = relnode then
          writech(branch_byte_ext)
        else if p^.kind = labelnode then
          begin
          if p^.labelcost = 0 then writech(branch_byte_ext)
          else if p^.labelcost = word then writech(branch_word_ext)
          else if p^.labelcost = long then
            writech(branch_long_ext)
            { ^^^ 68020 and pic only }
          end
        else puterror(missingoperand);
        end;

    end {postcorrect} ;


  begin {writeinst}
    case inst of

        { 68881 instructions
        }
      fabs:
        begin
        mnemonic := 'fabs';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000030B;
        end;

      facos:
        begin
        mnemonic := 'facos';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000034B;
        end;

      fadd:
        begin
        mnemonic := 'fadd';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000042B;
        end;

      fasin:
        begin
        mnemonic := 'fasin';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000014B;
        end;

      fatan:
        begin
        mnemonic := 'fatan';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000012B;
        end;

      fatanh:
        begin
        mnemonic := 'fatanh';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000015B;
        end;

      fbeq:
        begin
        mnemonic := 'fbeq';
        op := 170201B + coprocessor_id * 1000B;
        end;

      fbne:
        begin
        mnemonic := 'fbne';
        op := 170216B + coprocessor_id * 1000B;
        end;

      fbgt:
        begin
        if aware then
          begin
          op := 170202B + coprocessor_id * 1000B;
          mnemonic := 'fbogt';
          end
        else
          begin
          op := 170222B + coprocessor_id * 1000B;
          mnemonic := 'fbgt';
          end;
        end;

      fbngt:
        begin
        if aware then
          begin
          op := 170215B + coprocessor_id * 1000B;
          mnemonic := 'fbule';
          end
        else
          begin
          op := 170235B + coprocessor_id * 1000B;
          mnemonic := 'fbngt';
          end;
        end;

      fbge:
        begin
        if aware then
          begin
          op := 170203B + coprocessor_id * 1000B;
          mnemonic := 'fboge';
          end
        else
          begin
          op := 170223B + coprocessor_id * 1000B;
          mnemonic := 'fbge';
          end;
        end;

      fbnge:
        begin
        if aware then
          begin
          op := 170214B + coprocessor_id * 1000B;
          mnemonic := 'fbult';
          end
        else
          begin
          op := 170234B + coprocessor_id * 1000B;
          mnemonic := 'fbnge';
          end;
        end;

      fblt:
        begin
        if aware then
          begin
          op := 170204B + coprocessor_id * 1000B;
          mnemonic := 'fbolt';
          end
        else
          begin
          op := 170224B + coprocessor_id * 1000B;
          mnemonic := 'fblt';
          end;
        end;

      fbnlt:
        begin
        if aware then
          begin
          op := 170213B + coprocessor_id * 1000B;
          mnemonic := 'fbuge';
          end
        else
          begin
          op := 170233B + coprocessor_id * 1000B;
          mnemonic := 'fbnlt';
          end;
        end;

      fble:
        begin
        if aware then
          begin
          op := 170205B + coprocessor_id * 1000B;
          mnemonic := 'fbole';
          end
        else
          begin
          op := 170225B + coprocessor_id * 1000B;
          mnemonic := 'fble';
          end;
        end;

      fbnle:
        begin
        if aware then
          begin
          op := 170212B + coprocessor_id * 1000B;
          mnemonic := 'fbugt';
          end
        else
          begin
          op := 170232B + coprocessor_id * 1000B;
          mnemonic := 'fbnle';
          end;
        end;

      fbgl:
        begin
        if aware then
          begin
          op := 170206B + coprocessor_id * 1000B;
          mnemonic := 'fbogl';
          end
        else
          begin
          op := 170226B + coprocessor_id * 1000B;
          mnemonic := 'fbgl';
          end;
        end;

      fbngl:
        begin
        if aware then
          begin
          op := 170211B + coprocessor_id * 1000B;
          mnemonic := 'fbueq';
          end
        else
          begin
          op := 170231B + coprocessor_id * 1000B;
          mnemonic := 'fbngl';
          end;
        end;

      fbgle:
        begin
        if aware then
          begin
          op := 170207B + coprocessor_id * 1000B;
          mnemonic := 'fbor';
          end
        else
          begin
          op := 170227B + coprocessor_id * 1000B;
          mnemonic := 'fbgle';
          end;
        end;

      fbngle:
        begin
        if aware then
          begin
          op := 170210B + coprocessor_id * 1000B;
          mnemonic := 'fbun';
          end
        else
          begin
          op := 170230B + coprocessor_id * 1000B;
          mnemonic := 'fbngle';
          end;
        end;

      fcmp:
        begin
        mnemonic := 'fcmp';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000070B;
        end;

      fcos:
        begin
        mnemonic := 'fcos';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000035B;
        end;

      fcosh:
        begin
        mnemonic := 'fcosh';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000031B;
        end;

      fdiv:
        begin
        mnemonic := 'fdiv';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000040B;
        end;

      fetox:
        begin
        mnemonic := 'fetox';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000020B;
        end;

      fetoxm1:
        begin
        mnemonic := 'fetoxm1';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000010B;
        end;

      fgetexp:
        begin
        mnemonic := 'fgetexp';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000036B;
        end;

      fgetman:
        begin
        mnemonic := 'fgetman';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000037B;
        end;

      fint:
        begin
        mnemonic := 'fint';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000001B;
        end;

      fintrz:
        begin
        mnemonic := 'fintrz';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000003B;
        end;

      flog10:
        begin
        mnemonic := 'flog10';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000025B;
        end;

      flog2:
        begin
        mnemonic := 'flog2';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000026B;
        end;

      flogn:
        begin
        mnemonic := 'flogn';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000024B;
        end;

      flognp1:
        begin
        mnemonic := 'flognp1';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000006B;
        end;

      fmod:
        begin
        mnemonic := 'fmod';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000041B;
        end;

      fmove:
        begin
        mnemonic := 'fmove';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 0; { must know which direction }
        end;

      fmovecr:
        begin
        mnemonic := 'fmovecr';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 056000B;
        end;

      fmove_to_fpcr, fmove_from_fpcr:
        begin
        mnemonic := 'fmove';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 100000B;
        end;

      fmovem:
        begin
        mnemonic := 'fmovem';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 140000B;
        end;

      fmul:
        begin
        mnemonic := 'fmul';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000043B;
        end;

      fneg:
        begin
        mnemonic := 'fneg';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000032B;
        end;

      fnop:
        begin
        mnemonic := 'fnop';
        op := 170200B + coprocessor_id * 1000B;
        op2 := 0;
        end;

      frem:
        begin
        mnemonic := 'frem';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000045B;
        end;

      fscale:
        begin
        mnemonic := 'fscale';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000046B;
        end;

      fsgldiv:
        begin
        mnemonic := 'fsgldiv';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000044B;
        end;

      fsglmul:
        begin
        mnemonic := 'fsglmul';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000047B;
        end;

      fsin:
        begin
        mnemonic := 'fsin';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000016B;
        end;

      fsincos:
        begin
        mnemonic := 'fsincos';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000060B;
        end;

      fsinh:
        begin
        mnemonic := 'fsinh';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000002B;
        end;

      fsqrt:
        begin
        mnemonic := 'fsqrt';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000004B;
        end;

      fsub:
        begin
        mnemonic := 'fsub';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000050B;
        end;

      ftan:
        begin
        mnemonic := 'ftan';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000017B;
        end;

      ftanh:
        begin
        mnemonic := 'ftanh';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000011B;
        end;

      ftentox:
        begin
        mnemonic := 'ftentox';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000022B;
        end;

      ftrap: { NYI }
        begin
        mnemonic := 'ftrap';
        op := 170170B + coprocessor_id * 1000B;
        op2 := 0;
        end;

      ftst:
        begin
        mnemonic := 'ftest';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000072B;
        end;

      ftwotox:
        begin
        mnemonic := 'ftwotox';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000021B;
        end;

        { 68000 and 68020 instructions
        }
      add:
        begin
        mnemonic := 'add';
        op := 150000B; { + reg*512 + mode*64 + EA }
        end;

      adda:
        begin
        mnemonic := 'ADDA';
        op := 150000B; { + reg*512 + mode*64 + EA }
        end;

      addi:
        begin
        mnemonic := 'ADDI';
        op := 003000B; { + size*64 + EA }
        end;

      addq:
        begin
        mnemonic := 'ADDQ';
        op := 050000B; { data*512 + size*64 + EA }
        end;

      addx:
        begin
        mnemonic := 'addx';
        op := 150400B; { + reg*512 + size*64 + RM*8 + regy }
        end;

      andi:
        begin
        mnemonic := 'ANDI';
        op := 001000B; { + size*64 + EA }
        end;

      andinst:
        begin
        mnemonic := 'and';
        op := 140000B; { + reg*512 + mode*64 + EA }
        end;

      asl:
        begin
        mnemonic := 'asl';
        op := 160400B;
        end;

      asr:
        begin
        mnemonic := 'asr';
        op := 160000B;
        end;

      beq:
        begin
        mnemonic := 'beq';
        op := 063400B;
        end;

      bge:
        begin
        mnemonic := 'bge';
        op := 066000B;
        end;

      bgt:
        begin
        mnemonic := 'bgt';
        op := 067000B;
        end;

      bhi:
        begin
        mnemonic := 'bhi';
        op := 061000B;
        end;

      ble:
        begin
        mnemonic := 'ble';
        op := 067400B;
        end;

      bls:
        begin
        mnemonic := 'bls';
        op := 061400B;
        end;

      blt:
        begin
        mnemonic := 'blt';
        op := 066400B;
        end;

      bmi:
        begin
        mnemonic := 'bmi';
        op := 065400B;
        end;

      bpl:
        begin
        mnemonic := 'bpl';
        op := 065000B;
        end;

      bne:
        begin
        mnemonic := 'bne';
        op := 063000B;
        end;

      blo:
        begin
        mnemonic := 'bcs';
        op := 062400B; { =bcs }
        end;

      bhs:
        begin
        mnemonic := 'bcc';
        op := 062000B; { =bcc }
        end;

      bvc:
        begin
        mnemonic := 'bvc';
        op := 064000B;
        end;

      bvs:
        begin
        mnemonic := 'bvs';
        op := 064400B;
        end;

      bchg:
        begin
        mnemonic := 'bchg';
        op := 000100B;
        end;

      bclr:
        begin
        mnemonic := 'bclr';
        op := 000200B;
        end;

      bfclr:
        begin
        mnemonic := 'bfclr';
        op := 166300B;
        end;

      bfexts:
        begin
        mnemonic := 'bfexts';
        op := 165700B;
        end;

      bfextu:
        begin
        mnemonic := 'bfextu';
        op := 164700B;
        end;

      bfins:
        begin
        mnemonic := 'bfins';
        op := 167700B;
        end;

      bfset:
        begin
        mnemonic := 'bfset';
        op := 167300B;
        end;

      bftst:
        begin
        mnemonic := 'bftst';
        op := 164300B;
        end;

      bra:
        begin
        mnemonic := 'bra';
        op := 060000B;
        end;

      bset:
        begin
        mnemonic := 'bset';
        op := 000300B;
        end;

      bsr:
        begin
        mnemonic := 'bsr';
        op := 060400B;
        end;

      btst:
        begin
        mnemonic := 'btst';
        op := 000000B;
        end;

      chk:
        begin
        mnemonic := 'chk';

        if datasize = word then op := 040600B { + reg*512 + EA }
        else {long} op := 040400B; { + reg*512 + EA }
        end;

      clr:
        begin
        mnemonic := 'clr';
        op := 041000B; { + size*64 + EA }
        end;

      cmp:
        begin
        mnemonic := 'cmp';
        op := 130000B; { + reg*512 + mode*64 + EA }
        end;

      cmpa:
        begin
        mnemonic := 'CMPA';
        op := 130000B; { + reg*512 + mode*64 + EA }
        end;

      cmpi:
        begin
        mnemonic := 'CMPI';
        op := 006000B; { + size*64 + EA }
        end;

      cmpm:
        begin
        mnemonic := 'cmpm';
        op := 130410B;
        end;

      dbra:
        begin
        mnemonic := 'dbra';
        op := 050710B; { =dbf }
        end;

      dbeq:
        begin
        mnemonic := 'dbeq';
        op := 053710B;
        end;

      dbge:
        begin
        mnemonic := 'dbge';
        op := 056310B;
        end;

      dbgt:
        begin
        mnemonic := 'dbgt';
        op := 057310B;
        end;

      dbhi:
        begin
        mnemonic := 'dbhi';
        op := 051310B;
        end;

      dbhs:
        begin
        mnemonic := 'dbcc';
        op := 052310B; { =dbcc }
        end;

      dble:
        begin
        mnemonic := 'dble';
        op := 057710B;
        end;

      dbls:
        begin
        mnemonic := 'dbls';
        op := 051710B;
        end;

      dblt:
        begin
        mnemonic := 'dblt';
        op := 056710B;
        end;

      dblo:
        begin
        mnemonic := 'dbcs';
        op := 052710B; { =dbcs }
        end;

      dbmi:
        begin
        mnemonic := 'dbmi';
        op := 055710B;
        end;

      dbpl:
        begin
        mnemonic := 'dbpl';
        op := 055310B;
        end;

      dbne:
        begin
        mnemonic := 'dbne';
        op := 053310B;
        end;

      dbvc:
        begin
        mnemonic := 'dbvc';
        op := 054310B;
        end;

      dbvs:
        begin
        mnemonic := 'dbvs';
        op := 054710B;
        end;

      divs:
        begin
        mnemonic := 'divs';
        op := 100700B; { + reg*512 + EA }
        end;

      divu:
        begin
        mnemonic := 'divu';
        op := 100300B; { + reg*512 + EA }
        end;

      divsl:
        begin
        mnemonic := 'tdivsl';
        op := 046100B; { + EA }
        end;

      divul:
        begin
        mnemonic := 'tdivul';
        op := 046100B; { + EA }
        end;

      eor:
        begin
        mnemonic := 'eor';
        op := 130000B; { + reg*512 + mode*64 + EA }
        end;

      eori:
        begin
        mnemonic := 'EORI';
        op := 005000B; { + size*64 + EA }
        end;

      exg:
        begin
        mnemonic := 'exg';
        op := 140400B; { + reg*512 + regy }
        end;

      ext:
        begin
        mnemonic := 'ext';
        op := 044200B; { + longflag*64 + reg }
        end;

      extb:
        begin
        mnemonic := 'extb';
        op := 044200B; { + byte-to-long-bits*64 + reg }
        end;

      jmp:
        begin
        mnemonic := 'jmp';
        op := 047300B; { + EA }
        end;

      jsr:
        begin
        mnemonic := 'jsr';
        op := 047200B; { + EA }
        end;

      lea:
        begin
        mnemonic := 'lea';
        op := 040700B; { + reg*512 + EA }
        end;

      link:
        begin
        mnemonic := 'link';

        if datasize = word then op := 047120B { + reg }
        else {long} op := 044010B; { + reg }
        end;

      lsl:
        begin
        mnemonic := 'lsl';
        op := 160410B;
        end;

      lsr:
        begin
        mnemonic := 'lsr';
        op := 160010B;
        end;

      movea, move:
        begin
        mnemonic := 'MOVE'; { postcorrect adds 'A' if movea }
        if datasize = byte then op := 010000B
        else if datasize = word then op := 030000B
        else op := 020000B;
        end;

      move_to_ccr:
        begin
        mnemonic := 'move';
        op := 042300B;
        end;

      movem:
        begin
        mnemonic := 'MOVE'; { postcorrect adds 'M' }
        op := 044200B; { + size*64 + EA }
        end;

      moveq:
        begin
        mnemonic := 'MOVE'; { postcorrect adds 'Q' }
        op := 070000B; { + reg*512 + data }
        end;

      muls:
        begin
        if mc68020 and (datasize = long) then
          begin
          mnemonic := 'tmulsl';
          op := 046000B; { + EA }
          end
        else
          begin
          mnemonic := 'muls';
          op := 140700B { + reg*512 + EA }
          end;
        end;

      mulu:
        begin
        if mc68020 and (datasize = long) then
          begin
          mnemonic := 'tmulul';
          op := 046000B; { + EA }
          end
        else
          begin
          mnemonic := 'mulu';
          op := 140300B { + reg*512 + EA }
          end;
        end;

      neg:
        begin
        mnemonic := 'neg';
        op := 042000B; { + size*64 + EA }
        end;

      negx:
        begin
        mnemonic := 'negx';
        op := 040000B; { + size*64 + EA }
        end;

      notinst:
        begin
        mnemonic := 'not';
        op := 043000B; { + size*64 + EA }
        end;

      ori:
        begin
        mnemonic := 'ORI';
        op := 000000B; { + size*64 + EA }
        end;

      orinst:
        begin
        mnemonic := 'or';
        op := 100000B; { + reg*512 + mode*64 + EA }
        end;

      pea:
        begin
        mnemonic := 'pea';
        op := 044100B; { + EA, control modes only }
        end;

      rol:
        begin
        mnemonic := 'rol';
        op := 160430B;
        end;

      ror:
        begin
        mnemonic := 'ror';
        op := 160030B;
        end;

      roxl:
        begin
        mnemonic := 'roxl';
        op := 160420B;
        end;

      roxr:
        begin
        mnemonic := 'roxr';
        op := 160020B;
        end;

      rte:
        begin
        mnemonic := 'rte';
        op := 047163B;
        end;

      rts:
        begin
        mnemonic := 'rts';
        op := 047165B;
        end;

      sub:
        begin
        mnemonic := 'sub';
        op := 110000B; { + reg*512 + mode*64 + EA }
        end;

      suba:
        begin
        mnemonic := 'SUBA';
        op := 110000B; { + reg*512 + mode*64 + EA }
        end;

      subi:
        begin
        mnemonic := 'SUBI';
        op := 002000B; { + size*64 + EA }
        end;

      subq:
        begin
        mnemonic := 'SUBQ';
        op := 050400B; { data*512 + size*64 + EA }
        end;

      subx:
        begin
        mnemonic := 'subx';
        op := 110400B; { + reg*512 + size*64 + RM*8 + regy }
        end;

      swap:
        begin
        mnemonic := 'swap';
        op := 044100B; { + Dreg }
        end;

      trap:
        begin
        mnemonic := 'trap';
        op := 047100B; { + vector }
        end;

      trapcc:
        begin
          { Thus far we only need the TRAPLS form of this instruction (for
            Versados).  The Versados assembler wants the mnemonic to be TLS
            instead of TRAPLS.
          }
        mnemonic := 'tls';
        op := 051774B;
        end;

      trapv:
        begin
        mnemonic := 'trapv';
        op := 047166B;
        end;

      tst:
        begin
        mnemonic := 'tst';
        op := 045000B; { + size*64 + EA }
        end;

      unlk:
        begin
        mnemonic := 'unlk';
        op := 047130B; { + reg }
        end;

      end; {case inst}

    insertobj(op);

    if inst in [fp_first..fp_last] then
      fp_src_spec := loophole(integer, n^.fp_format);

    if switcheverplus[outputmacro] then
      begin
      { print mnemonic to macro file, suppressing trailing blanks }
      reposition(opcolumn);

      for i := 1 to ord(mnemonic[0]) do writech(uppercase(mnemonic[i]));

      postcorrect;

      if n^.oprndcount <> 0 then reposition(opndcolumn);
      end; {macro output}

  end; {writeinst}



procedure writebitfield(reg, offset, width: integer);

{ Output the bit field descriptor for the 68020 bit field instructions.
  If the reg field is not -1 then it indicates that the offset is in that
  D-reg.
}


  begin
    if switcheverplus[outputmacro] then
      begin
      writech('{');
      if reg <> - 1 then
        begin
        writech('D');
        writeint(reg);
        end
      else
        begin
        writeint(offset);
        end;

      writech(':');
      writeint(width);
      writech('}');
      end;
  end;


procedure writelastopnd;

{ Outputs the assembler code for the node currently pointed to by "n".
  If this procedure has been called by "BuildInstruction" then it is
  the last operand for the current instruction.  If, however, it has
  been called by "writeopnd", it is actually the first of two operands,
  and will have a comma appended by "writeopnd" before returning to
  process the second operand.
}

  var
    vtptr: vartablerecptr;
    kluge:
      record
        case integer of
          1: (l: integer {long word} );
          2:
            (w: packed array [boolean] of - 32767..32767);
      end {kluge} ;
    s: longname;

    { Write out the scale factor for the 68020 indexed instructions.
      A scale factor of 1 is the default and so is ignored.
    }


  procedure write_scale;


    begin
      if mc68020 then
        with n^.oprnd do
          if scale <> 1 then
            begin
            writech('*');
            writeint(scale);
            end;
    end;


  begin {writelastopnd}
    if switcheverplus[outputmacro] then
      with n^.oprnd do
        case m of

          nomode: puterror(nomodeoprnd);

          dreg:
            begin
            writech('D');
            writeint(reg);
            end; {dreg}

          twodregs:
              { This is currently only for the divsl and divul instructions.
                Reg is the quotient register and indxr is the remainder
                register.  The format is Dr:Dq (remainder:quotient).

                Note: If the quotient and remainder registers are the same
                then only a 32 bit quotient will be generated.
              }

            begin
            writech('D');
            writeint(indxr);

            if reg <> indxr then
              begin
              writech(':');
              writech('D');
              writeint(reg);
              end;
            end; {twodregs}

          areg:
            begin
            if reg = 7 then writestr('SP')
            else
              begin
              writech('A');
              writeint(reg);
              end;
            end; {areg}

          fpreg:
            begin
            writestr('FP');
            writeint(reg);
            end;

          twofpregs:
              { This is currently only for 68881 fsincos instruction.
                Reg is the cosine register and indxr is the sine.  The
                format is FPc:FPs (cosine:sine).
              }

            begin
            writestr('FP');
            writeint(reg);
            writech(':');
            writestr('FP');
            writeint(indxr);
            end; {twofpregs}

          indr:
            begin
            if reg = 7 then writestr('(SP)')
            else
              begin
              writestr('(A');
              writeint(reg);
              writech(')');
              end;
            end; {indr}

          autoi:
            begin
            if reg = 7 then writestr('(SP)+')
            else
              begin
              writestr('(A');
              writeint(reg);
              writestr(')+');
              end;
            end; {autoi}

          autod:
            begin
            if reg = 7 then writestr('-(SP)')
            else
              begin
              writestr('-(A');
              writeint(reg);
              writech(')');
              end;
            end; {autod}

          relative:
            begin
            if mc68020 and ((offset > 32767) or (offset < -32768)) then
              begin
              writech('(');
              writeint(offset);
              writech(',');
              end
            else
              begin
              writeint(offset);
              writech('(');
              end;

            if reg = 7 then
              begin
              writestr('SP)');
              if offset < 0 then puterror(negativesp)
              end
            else
              begin
              writech('A');
              writeint(reg);
              writech(')');
              end;
            end; {relative}

          indexed:
            begin
            if not mc68020 and ((offset > 127) or (offset < - 128)) then
              puterror(baddisplacement);
            if mc68020 then
              begin
              writech('(');
              writeint(offset);
              writech(',');
              end
            else
              begin
              writeint(offset);
              writech('(');
              end;

            writech('A');
            writeint(reg);
            writestr(',D');
            writeint(indxr);
            if indxlong then writestr('.L')
            else writestr('.W');
            write_scale;
            writech(')');
            end; {indexed}

          bitindexed:
            begin
            puterror(bitindexmode);
            end; {bitindexed}

          absshort:
            begin
            writeint(offset);
            end; {absshort}

          abslong:
            begin
            writech('$');
            kluge.l := offset;
              { the next two lines assume that "reversebytes" implies that
                words are also reversed. }
            WriteHex(kluge.w[reversebytes]);
            WriteHex(kluge.w[not reversebytes]);
            end; {abslong}

          immediate, special_immediate:
            begin
            writech('#');
            if datasize <= word then writeint(offset)
            else
              begin
              writech('$');
              if hostintsize <= word then
                begin
                if offset < 0 then WriteHex( - 1)
                else WriteHex(0);
                WriteHex(offset);
                end
              else
                begin
                kluge.l := offset;
                  { the next two lines assume that "reversebytes" implies that
                    words are also reversed. }
                WriteHex(kluge.w[reversebytes]);
                WriteHex(kluge.w[not reversebytes]);
                end;
              end;
            end; {immediate}

          immediatelong:
            begin
            writech('#');

              { Floating point constants in hex in 68881 instructions must
                be prefixed by a ":" instead of a "$" -- Strange!
              }
            if (currinst in [fp_first..fp_last]) then writech(':')
            else writech('$');

            WriteHex(offset1);
            WriteHex(offset);
            end; { immediatelong }

          immediatequad:
            begin
            writech('#');

              { Floating point constants in hex in 68881 instructions must
                be prefixed by a ":" instead of a "$" -- Strange!
              }
            if (currinst in [fp_first..fp_last]) then writech(':')
            else writech('$');

            WriteHexLong(offset1);
            WriteHexLong(offset);
            end; { immediatequad }

          immediate_extended:
            begin
            writech('#');

              { Floating point constants in hex in 68881 instructions must
                be prefixed by a ":" instead of a "$" -- Strange!
              }
            if (currinst in [fp_first..fp_last]) then writech(':')
            else writech('$');

            WriteHexLong(offset2);
            WriteHexLong(offset1);
            WriteHexLong(offset);
            end; { immediate_extended }

          commonlong:
            begin
            if commonlong_reloc < 0 then writech('G');
            if commonlong_reloc > 0 then
              begin
              vtptr := getvartableptr(commonlong_reloc);
              with vtptr^ do
                begin
{                n^.oprnd.offset := n^.oprnd.offset + offset;}
                  { Add in psect offset for Versados define'd var }
                if $pic and (extvaralloc = sharedalloc) then
                  begin
                  writech('#');
                  WriteSymbolName(get_from_sfile(charindex, charlen, true));
                  writech('-');
                  supname(loophole(libroutines, libown), s);
                  WriteSymbolName(s);
                  end
                else WriteSymbolName(get_from_sfile(charindex, charlen,
                                     not aliased));
                end;
              end;

            if offset <> 0 then
              begin
              if offset >= 0 then writech('+');
              writeint(offset);
              end;
            end; {commonlong}

          pcrelative:
            begin
            writech('L');
            if offset >= 0 then writech('+');
            writeint(offset);
            if (n^.operandcost < long) or $pic then writestr('(PC)');
            end; {pcrelative}

          pcindexed:
            begin
            if not mc68020 and ((offset > 127) or (offset < - 128)) then
              puterror(baddisplacement);
            if mc68020 then
              begin
              writestr('(*');
              {??? how much bias is needed for 68020 ???}
              if offset + word >= 0 then writech('+');
              writeint(offset + word);
              writech(',');
              end
            else
              begin
              writech('*');
              if offset + word >= 0 then writech('+');
              writeint(offset + word);
              writech('(');
              end;
            writestr('PC,D');
            writeint(indxr);
            writestr('.W');
            write_scale;
            writech(')');
            end; {pcindexed}

          supportcall:
            begin
            if (offset < ord(first_call)) or (offset > ord(last_call)) then
              puterror(badsupportcall);

            supname(loophole(libroutines, offset), s);

            if $pic then
              if mc68020 then
                begin { must use 68020 32 bit form }
                writech('(');
                WriteSymbolName(s);
                writestr(',PC)');
                end
              else
                begin { use 68000 16 bit form }
                WriteSymbolName(s);
                writestr('(PC)');
                end
            else WriteSymbolName(s); { non pic -- use absolute form }
            end; {supportcall}

          usercall:
            begin
            if $pic then
              if mc68020 and ((proctable[offset].externallinkage and
                 not proctable[offset].bodydefined) or
                 (procmap[offset].addr = undefinedaddr) or
                 (n^.operandcost >= long)) then
                begin { must use 68020 32 bit form }
                writech('(');
                if proctable[n^.oprnd.offset].externallinkage then
                  writeprocname(offset, linknameused) {maintains col counter}
                else
                  begin
                  writech('P');
                  writeint(offset);
                  end;

		if offset1 <> 0 then
		  begin
		  writeint(offset1);
		  if odd(offset1) or (offset1 > 0) then puterror(badoffset);
		  end;

                writestr(',PC)');
                end
              else
                begin { use 68000 16 bit form }
                if proctable[n^.oprnd.offset].externallinkage then
                  writeprocname(offset, linknameused) {maintains col counter}
                else
                  begin
                  writech('P');
                  writeint(offset);
                  end;

		if offset1 <> 0 then
		  begin
		  writeint(offset1);
		  if odd(offset1) or (offset1 > 0) then puterror(badoffset);
		  end;

                writestr('(PC)');
                end
            else if proctable[n^.oprnd.offset].externallinkage then
              begin
              writeprocname(offset, linknameused); {maintains column counter}

              if proctable[offset].bodydefined and
                 (procmap[offset].addr <> undefinedaddr) and
                 (n^.operandcost < long) then writestr('(PC)');
              end
            else
              begin { not external call }
              writech('P');
              writeint(offset);

              if offset1 <> 0 then
                begin
                writeint(offset1);
                if odd(offset1) or (offset1 > 0) then puterror(badoffset);
                end;

              if n^.operandcost < long then writestr('(PC)');
              end;
            end; {usercall}

          pic_own_immed:
              { In PIC mode this can only occur for the code to load A3
                at the beginning of each procedure.
              }
            begin
            writestr('#G-');
            supname(loophole(libroutines, libown), s);
            WriteSymbolName(s);
            end;

          pic_splat_pcrel:

            { For 68000 24-bit PIC only.  Generates "#<offset>+*(PC)".
            }
            begin
            writeint(offset);
            writestr('+*');
            writestr('(PC)');
            end;

          pic_usercall:

            { For 68000 24-bit PIC only.  Generates "#<name>-<offset>-*".
            }
            begin
            writech('#');

            if proctable[n^.oprnd.offset].externallinkage then
              writeprocname(offset, linknameused) {maintains column counter}
            else
              begin { not external call }
              writech('P');
              writeint(offset);
              end;

            writech('-');
            writeint(offset1);
            writestr('-*');
            end;

          pic_supportcall:

            { For 68000 24-bit PIC only.  Generates "#<suppt_call>-<offset>-*".
            }
            begin
            writech('#');
            supname(loophole(libroutines, offset), s);
            WriteSymbolName(s);
            writech('-');
            writeint(offset1);
            writestr('-*');
            end;

          pic_branch:
            begin
            writestr('#L');
            writeint(offset);
            writech('-');
            writeint(offset1);
            writestr('-*');
            end;

          pic_pcrelative:
            begin
            writestr('#L+');
            writeint(offset);
            writech('-');
            writeint(offset1);
            writestr('-*');
            end;
          end; {case mode}

  end; {writelastopnd}


procedure writeopnd;


  begin
    if switcheverplus[outputmacro] then
      begin
      writelastopnd;
      writech(',');
      end;
  end;



function computedistance: addressrange;

{ The current node contains a (signed) number of instructions to branch
  over.  The current instruction is either a branch or a decrement-and-
  branch.  If the latter, the value returned is relative to the 2nd word
  of the instruction.
}

  var
    tempnode: nodeindex; { so we don't screw up currnode }
    instcount: integer; { number of instructions to skip over }
    bytecount: integer; { accumulates the byte offset }
    i: integer; { induction var for counting instructions }


  begin
    tempnode := currnode;
    instcount := n^.distance;

    repeat { find current instruction node }
      tempnode := tempnode - 1;
      if bigcompilerversion then p := ref(bignodetable[tempnode])
      else creadaccess(tempnode, p);
    until p^.kind = instnode;

    bytecount := - 2; { the opcode is at (PC-2) regardless of length! }

    if instcount < 0 then { backward scan }
      for i := - 2 downto instcount do
        begin
        repeat { find previous instruction node }
          tempnode := tempnode - 1;
          if bigcompilerversion then p := ref(bignodetable[tempnode])
          else creadaccess(tempnode, p);
        until p^.kind = instnode;

        bytecount := bytecount - p^.computed_length {instlength(tempnode)}
        end

    else { instcount > 0 } { forward scan }
      for i := 0 to instcount do
        begin
        bytecount := bytecount + p^.computed_length {instlength(tempnode)} ;

        repeat { find next instruction node }
          tempnode := tempnode + 1;
          if bigcompilerversion then p := ref(bignodetable[tempnode])
          else creadaccess(tempnode, p);
        until p^.kind = instnode;
        end;

    computedistance := bytecount
  end; { computedistance }


procedure writelabels;


  begin { write all labels which refer to this node }
    if column <> 1 then writeline; { a previous label may be "open" if a nop
                                     node has intervened }
    writech('L');
    writeint(labeltable[currlabel].labno);
    writech(':');
    currlabel := currlabel + 1;

    while currnode = labeltable[currlabel].nodelink do
      begin { write additional labels on separate lines }
      writeline;
      writech('L');
      writeint(labeltable[currlabel].labno);
      writech(':');
      currlabel := currlabel + 1;
      end; { write additional labels }

  end; { write all labels }


procedure doblocklabel;

{ Print a label in the assembler file to mark the start of a procedure.
}



  begin
    writech('*');
    writeline;
    writestr('*  [');
    write(MacFile, currentpc: - 4);
    write(MacFile, ']  ');

    if blockref = 0 then write(MacFile, 'Main Body:')
    else
      begin
      writeprocname(blockref, 100);
      end;
    writeline;
    writech('*');
    writeline;
  end; {doblocklabel}




procedure DoBlockEntryCode;

  var
    lscan: labelindex;


  begin
    if (switchcounters[debugging] > 0) or (switchcounters[profiling] > 0) then
      writemaprecord(plabrec, 0, 0, procsym);
    procmap[blockref].addr := currentpc; { update procedure address table }

    if  (level = 1) 
    and (  (proctable[blockref].calllinkage = pascal2call)
        or (proctable[blockref].calllinkage = modulebody))
    then
      if switchcounters[mainbody] > 0 then
        begin { process main body of program }
        if switcheverplus[outputmacro] then writestr('BEGIN$:');

        startaddress := currentpc;
        end { switchcounters[mainbody] > 0 }
      else
        begin { level=1 and nomainbody }
        if switcheverplus[outputmacro] then
          begin
          writech('*');
          writeline;
          end;
        end

    else
      begin { block other than main }

      { test for external procedure (body is obviously defined) }
      if proctable[blockref].externallinkage 
      or (   (proctable[blockref].calllinkage = implementationbody)
	 and (level = 1))
      then
        with newESD do
          begin { prepare a new table entry }
          ESDkind := ESDentry;
          exproc := blockref;
          insertnewESD;
          end; { table entry }

      if switcheverplus[outputmacro] then
        begin
        if proctable[blockref].externallinkage
        or (   (proctable[blockref].calllinkage = implementationbody)
	   and (level = 1))
	then
          begin
          writeprocname(blockref, linknameused);
          writech(':');
          writeline;
          end
        else
          begin { not external }
          writech('P');
          writeint(blockref);
          writech(':');
          end;
        end {macro output for block entry} ;
      end; { block other than main }


{ Service the fixup list with new label definitions, and possibly the
  current procedure address, if it was declared forward.
}

    fixp := fixuphead;
    while fixp <> nil do
      begin
      with fixp^ do { examine the node }
        if fixupkind = fixupproc then { compare proc numbers }

          if fixupprocno = blockref then
            fixupaddr := currentpc { this is the forward target }
          else { maybe next time }

        else { fixupkind = fixuplabel }
          for lscan := 1 to nextlabel do
            if labeltable[lscan].labno = fixuplabno then
              fixupaddr := labeltable[lscan].address;

      fixp := fixp^.fixuplink;
      end;
  end; { DoBlockEntryCode }


procedure buildbranches;

{ Code to build a branch instruction.  This is pulled out of buildinstruction
  so the compiler can handle it.
}

  var
    labeldelta: integer; {for signed comparisons}
    isforward: integer; {true if forward reference}


  begin
    if n^.kind = labelnode then
      begin
      if switcheverplus[outputmacro] then
        begin
        writech('L');
        writeint(n^.labelno);
        end;

      findlabelpc(n^.labelno, isforward); { find or compute target's addr }

      if n^.labelcost = long then { 68020 and pic only }
        begin
        op := 16#FF + op;
        insertobj((labelpc - currentpc) div 16#10000);
        insertobj((labelpc - currentpc + 2) mod 16#10000);

        if isforward <> 0 then
          begin
          objtype[objctr - 1] := objlong;
          allocfixup; { generate a new fixupnode }
          fixups[objctr - 1] := fixuptail;
          with fixuptail^ do
            begin
            fixupkind := fixuplabel;
            fixuplen := long;
            fixuplabno := n^.labelno;
            fixupobjpc := fixupobjpc - 4;
            end;
          end;
        end
      else
      if n^.labelcost = word then
        begin { word branch }
        insertobj(labelpc - currentpc); { this bumps pc by 2 }
        if isforward <> 0 then
          begin
          write('Forward BRA illegal');
          abort(inconsistent);
          end;
        end { long branch }

      else
        begin { short branch }
        labeldelta := labelpc - currentpc;

          { This is a signed test on unsigned operands
          }
        if (labeldelta > 127) or (labeldelta < - 128) then
          puterror(badoperand);

        op := (labeldelta and 377B) + op;
        end { short branch }
      end {labelnode}

    else if n^.kind = relnode then
      begin { relnodes are short, unlabeled offsets }
      distancetemp := computedistance;
      op := (distancetemp and 255) + op;

      if switcheverplus[outputmacro] then
        begin
        writech('*');
        if distancetemp < 0 then writeint(distancetemp)
        else
          begin
          writech('+');
          writeint(distancetemp + word);
          end;
        end;
      end
    else puterror(nolabelnode);
  end; {buildbranches}


procedure buildfpbranches;

{ Code to build a 68881 branch instruction.
}

  var
    isforward: integer; {true if forward reference}


  begin
    if n^.kind = labelnode then
      begin
      if switcheverplus[outputmacro] then
        begin
        writech('L');
        writeint(n^.labelno);
        end;

      findlabelpc(n^.labelno, isforward); { find or compute target's addr }
      distancetemp := labelpc - currentpc; { bump pc by 2 }

      if n^.labelcost = long then { 32 bit branch }
        begin
        op := op + 100B; { set size bit }
        insertobj(distancetemp div 16#10000);
        insertobj(distancetemp mod 16#10000);

        if isforward <> 0 then
          begin
          objtype[objctr - 1] := objlong;
          allocfixup; { generate a new fixupnode }
          fixups[objctr - 1] := fixuptail;
          with fixuptail^ do
            begin
            fixupkind := fixuplabel;
            fixuplen := long;
            fixuplabno := n^.labelno;
            fixupobjpc := fixupobjpc - 4;
            end;
          end;
        end
      else { 16 bit branch } insertobj(distancetemp);
      end {labelnode}
    else if n^.kind = relnode then
      begin { relnodes are short, unlabeled branches. }
      distancetemp := computedistance;

      insertobj(distancetemp);

      if switcheverplus[outputmacro] then
        begin
        writech('*');
        if distancetemp < 0 then writeint(distancetemp)
        else
          begin
          writech('+');
          writeint(distancetemp + word);
          end;
        end;
      end
    else puterror(nolabelnode);
  end; {buildfpbranches}


procedure builddbxx;

  var
    isforward: integer; {true if forward reference}


  begin
    if n^.oprnd.m <> dreg then puterror(badsource);
    op := op + n^.oprnd.reg;
    writeopnd;
    getnextnode;

    if n^.kind = labelnode then
      begin { process the label }
      if switcheverplus[outputmacro] then
        begin
        writech('L');
        writeint(n^.labelno);
        end;

      findlabelpc(n^.labelno, isforward); { find or compute target's addr }
      insertobj(labelpc - currentpc); { this bumps pc by 2 }
      if isforward <> 0 then abort(inconsistent);
      end
    else if n^.kind = relnode then
      begin
      distancetemp := computedistance;
      insertobj(labelpc - currentpc);

      if switcheverplus[outputmacro] then
        begin
        writech('*');
        if distancetemp < 0 then writeint(distancetemp)
        else
          begin
          writech('+');
          writeint(distancetemp + word);
          end;
        end;
      end
    else puterror(nolabelnode);
  end; {builddbxx}


procedure buildmovem(gen_fmovem: boolean);

  var
    i: 0..7;


  begin
    if n^.oprnd.m = immediate then
      begin { save registers }
      datasize := word; {mask is only 16 bits long}

      if not gen_fmovem then setmodeonly; { process the register mask }

      if switcheverplus[outputmacro] then
      begin
      mask := 1;
      first := true;

      if gen_fmovem then
        for i := 0 to 7 do
          begin
          if n^.oprnd.offset and mask <> 0 then
            begin
            if not first then writech('/');
            writestr('FP');
            writech(chr(i + ord('0')));
            first := false;
            end;
          mask := mask * 2;
          end
      else
        begin
        for i := 7 downto 0 do
          begin
          if n^.oprnd.offset and mask <> 0 then
            begin
            if not first then writech('/');
            writech('A');
            writech(chr(i + ord('0')));
            first := false;
            end;
          mask := mask * 2;
          end;

        for i := 7 downto 0 do
          begin
          if n^.oprnd.offset and mask <> 0 then
            begin
            if not first then writech('/');
            writech('D');
            writech(chr(i + ord('0')));
            first := false;
            end;
          mask := mask * 2;
          end;
        end;
      writech(',');
      end;
      if gen_fmovem then
        begin { mode 00 is static list, -(An) }
        op2 := op2 + 20000B { indicate direction mem-to-reg }
               + n^.oprnd.offset; { glue in list }
        insertobj(op2);
        end;

      getoperand;
      if not (n^.oprnd.m in [relative, autod]) then puterror(baddestination);
      seteffective; { autodec mode and register }
      writelastopnd;
      end
    else if n^.oprnd.m = autoi then
      begin { restore registers }
      writeopnd;
      seteffective;
      getoperand;

      if gen_fmovem then
        begin
        op2 := op2 + 2 * 4000B { mode 10 is static list, (An)+ }
               + n^.oprnd.offset; { glue in list }
        insertobj(op2);
        end
      else op := op + 2000B; { indicate direction mem-to-reg }

      datasize := word; { mask is only 16 bits long }

      if not gen_fmovem then setmodeonly; { append register mask }

      if n^.oprnd.m <> immediate then puterror(baddestination);
      if switcheverplus[outputmacro] then
      begin
      mask := 1;
      first := true;

      if gen_fmovem then
        for i := 7 downto 0 do
          begin
          if n^.oprnd.offset and mask <> 0 then
            begin
            if not first then writech('/');
            writestr('FP');
            writech(chr(i + ord('0')));
            first := false;
            end;
          mask := mask * 2;
          end
      else
        begin
        for i := 0 to 7 do
          begin
          if n^.oprnd.offset and mask <> 0 then
            begin
            if not first then writech('/');
            writech('D');
            writech(chr(i + ord('0')));
            first := false;
            end;
          mask := mask * 2;
          end;

        for i := 0 to 7 do
          begin
          if n^.oprnd.offset and mask <> 0 then
            begin
            if not first then writech('/');
            writech('A');
            writech(chr(i + ord('0')));
            first := false;
            end;
          mask := mask * 2;
          end;
        end;
      end;
      end
    else puterror(badsource);

    if not gen_fmovem then op := op + 100B; { preinitialized for word; change
                                              to long }

  end; {buildmovem}




procedure BuildInstruction;

  var
    i: 0..7;
    offset1: integer;
    register: regindex;
    memory: boolean; { used for 68881 instructions }
    n1: nodeptr;
    isforward: integer;


  procedure output_fp_creg;

    { Output the 68881 control register name for the FMOVE system control
      register instruction.
    }


    begin
      if switcheverplus[outputmacro] then
        begin
        case n^.oprnd.offset of
          1: writestr('FPIAR');
          2: writestr('FPSR');
          4: writestr('FPCR');
          end;
        end
    end; {output_fp_creg}


  begin {BuildInstruction}
    {branches are pulled out to let this fit through the 11 compiler}

    if currinst in branches then buildbranches
    else if currinst in fpbranches then buildfpbranches
    else if currinst in
            [dbra, dbeq, dbge, dbgt, dbhi, dbhs, dble, dblo, dbls, dblt, dbmi,
            dbpl, dbne, dbvc, dbvs] then
      builddbxx
    else
      case currinst of

          { 68881 instructions
          }

        fabs, facos, fadd, fasin, fatan, fatanh, fcos, fcosh, fetox, fetoxm1,
        fgetexp, fgetman, fint, fintrz, flog10, flog2, flogn, flognp1, fmod,
        fmul, fneg, frem, fscale, fsglmul, fsin, fsinh, fsqrt, ftan, ftanh,
        ftentox, ftrap, ftwotox:
          begin
          if n^.oprnd.m <> fpreg then
            begin
            memory := true;
            register := fp_src_spec;
            writeopnd;
            getoperand;
            end
          else
            begin
            memory := false;
            register := n^.oprnd.reg;
            getoperand;

            { Suppress duplicate registers
            }
            if (register = n^.oprnd.reg) and
               not (currinst in [fadd, fmul, fsglmul, frem, fmod, fscale]) then
              {suppress}
            else
              begin
              getprevoperand(1);
              writeopnd;
              currnode := currnode - 1; { Get back in sync }
              getoperand;
              end;
            end;

          writelastopnd;
          insertobj(op2 + ord(memory) * 40000B + register * 2000B +
                    n^.oprnd.reg * 200B);

          if memory then
            begin
            getprevoperand(1);
            seteffective;
            end;
          end;

        fsincos:
          begin
          if n^.oprnd.m <> fpreg then
            begin
            memory := true;
            register := fp_src_spec;
            end
          else
            begin
            memory := false;
            register := n^.oprnd.reg;
            end;

          writeopnd;
          getoperand;
          writelastopnd;
          insertobj(op2 + ord(memory) * 40000B + register * 2000B +
                    n^.oprnd.indxr * 200B + n^.oprnd.reg);
          if memory then
            begin
            getprevoperand(1);
            seteffective;
            end;
          end;

        ftst:
          begin
          if n^.oprnd.m <> fpreg then
            begin
            memory := true;
            register := fp_src_spec;
            end
          else
            begin
            memory := false;
            register := n^.oprnd.reg;
            end;

          writelastopnd;
          insertobj(op2 + ord(memory) * 40000B + register * 2000B);
          if memory then seteffective;
          end;

        fcmp, fsub, fdiv, fsgldiv:

          { These sometimes have reversed operands in the assembler source.
          }
          begin
          if n^.oprnd.m = fpreg then
            begin
            memory := false;
            register := n^.oprnd.reg;
            end
          else
            begin
            memory := true;
            register := fp_src_spec;
            end;

            begin { put out funny Motorola assembler form }
            writeopnd;
            getoperand;
            writelastopnd;
            insertobj(op2 + ord(memory) * 40000B + register * 2000B +
                      n^.oprnd.reg * 200B);
            if memory then
              begin
              getprevoperand(1);
              seteffective;
              end;
            end;
          end;

        fmove:
          begin
          memory := n^.oprnd.m <> fpreg;
          writeopnd;
          register := n^.oprnd.reg;
          getoperand;
          writelastopnd;

          if n^.oprnd.m = fpreg then { memory-to-register form (includes
                                      register-to-register) }
            if memory then
              begin
              insertobj(ord(memory) * 40000B + fp_src_spec * 2000B +
                        n^.oprnd.reg * 200B);
              getprevoperand(1);
              seteffective;
              end
            else { fpreg-to-fpreg }
              insertobj(register * 2000B + n^.oprnd.reg * 200B)
          else { register-to-memory form }
            begin
            insertobj(60000B + fp_src_spec * 2000B + register * 200B);
            seteffective;
            end;
          end;

        fnop:
          begin
          insertobj(op2);
          end;

        fmove_from_fpcr:

          { Move from system control register.
          }
          begin
          offset1 := n^.oprnd.offset;
          output_fp_creg;
          if switcheverplus[outputmacro] then writech(',');
          getoperand;
          writelastopnd;
          insertobj(op2 + 20000B + offset1 * 2000B);
          seteffective;
          end;

        fmove_to_fpcr:

          { Move to system control register.
          }
          begin
          writeopnd;
          getoperand;
          output_fp_creg;
          insertobj(op2 + n^.oprnd.offset * 2000B);
          getprevoperand(1);
          seteffective;
          end;

        fmovecr:

          { Move from 68881 constant rom.
          }
          begin
          offset1 := n^.oprnd.offset;
          writeopnd;
          getoperand;
          writelastopnd;
          insertobj(op2 + n^.oprnd.reg * 200B + offset1);
          end;

        fmovem: buildmovem(true);

          { 68000 and 68020 instructions
          }

        movea, move:
          begin
          writeopnd;
          seteffective;
          getoperand;
          writelastopnd;
          setmodeonly;
          if currinst = movea then
            if datasize = byte then puterror(badsize)
            else if n^.oprnd.m <> areg then puterror(missingAreg);
          op := ((((mode and 7B) * 100B + mode) * 10B) and 7700B) + op;
          end;

        move_to_ccr:
          begin
          writeopnd;
          seteffective;
          if switcheverplus[outputmacro] then writestr('CCR');
          end;

        moveq:
          begin
          if not (n^.oprnd.m in [immediatelong, immediate]) or
             (n^.oprnd.offset > 127) or (n^.oprnd.offset < - 128) then
            puterror(badoperand);
          datasize := byte; { just in case }
          writeopnd;
          op := (n^.oprnd.offset and 377B) + op;
          getoperand;
          writelastopnd;
          if n^.oprnd.m <> dreg then puterror(badoperand);
          op := n^.oprnd.reg * 1000B + op;
          end;

        add, cmp, sub, andinst, orinst:
          begin
          if datasize = word then op := op + 100B
          else if datasize = long then op := op + 200B;

            begin
            writeopnd;

            lookahead(1); { Check destination for d-reg first! If it is, then
                           emit "<Dn> op <EA> --> <Dn>" form only }
            if (n^.oprnd.m = dreg) and (p^.oprnd.m <> dreg) then
              begin
              op := op + 400B; { indicate direction }
              if currinst = cmp then puterror(badsource); {cmp is one-way}
              insertreghi;
              getoperand;
              seteffective;
              end
            else
              begin { must be "<EA> to Dn" form }
              seteffective;
              getoperand;
              if n^.oprnd.m <> dreg then puterror(missingDreg);
              insertreghi;
              end;
            writelastopnd;
            end;
          end;

        addq, subq:
          begin
          if (n^.oprnd.m <> immediate) or (n^.oprnd.offset < 1) or
             (n^.oprnd.offset > 8) then
            puterror(badoperand);
          if n^.oprnd.offset < 8 then { value 8 == bit pattern 0 }
            op := n^.oprnd.offset * 1000B + op;
          insertsize;
          datasize := byte;
          writeopnd;
          getoperand;
          writelastopnd;
          seteffective;
          end;

        adda, cmpa, suba: { address register destination }
          begin
            begin
            seteffective;
            writeopnd;
            getoperand;
            writelastopnd;
            if n^.oprnd.m <> areg then puterror(missingAreg);
            insertreghi;
            if datasize = word then op := op + 300B
            else if datasize = long then op := op + 700B
            else puterror(badoperand); { no byte mode }
            end;
          end;

        addi, cmpi, subi, andi, eori, ori: { immediate source }
          begin
          if (n^.oprnd.m <> immediate) and (n^.oprnd.m <> immediatelong) then
            puterror(badoperand);
            begin
            insertsize;
            setmodeonly; { processes the immediate data }
            writeopnd;
            getoperand;
            writelastopnd;
            seteffective;
            end;
          end;

        eor: { exclusive or -- differs from other logicals }
          begin
          if n^.oprnd.m <> dreg then puterror(missingDreg);

          if datasize = word then op := op + 500B
          else if datasize = long then op := op + 600B
          else op := op + 400B;

          writeopnd;
          insertreghi;
          getoperand;
          writelastopnd;
          if n^.oprnd.m = areg then puterror(badoperand);
          seteffective;
          end;

        asl, asr, lsl, lsr, rol, ror, roxl, roxr: { shift group }
          begin
          if n^.oprnd.m = immediate then
            begin
            if (n^.oprnd.offset < 1) or (n^.oprnd.offset > 8) then
              puterror(badoperand);
            lookahead(1); { check for single bit memory shifts }
            if p^.kind <> oprndnode then puterror(missingoperand);

            if p^.oprnd.m = dreg then
              begin { immediate/register }
              if n^.oprnd.offset < 8 then { shift 8 == bit pattern 0 }
                op := n^.oprnd.offset * 1000B + op;
              insertsize;
              datasize := word;
              writeopnd;
              getoperand;
              op := n^.oprnd.reg + op;
              end { immediate/register }

            else
              begin { immediate/memory -- enforce shift count = 1 }
              if n^.oprnd.offset <> 1 then puterror(badsource)
              else
                op := (op and 30B) * 100B { relocate subtype }
                      + (op and 400B) { save direction bit }
                      + 160300B; { size of 3 decodes to memory shift! }
              if datasize <> word then puterror(badsize);
              getoperand; { do not write out the shift count! }
              seteffective;
              end; { immediate/memory }
            end { immediate (or implied) shift count form }

          else { register/register form -- instruction needs correction }
            begin
            op := op + 40B;
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            insertreghi; { this reg has the shift count }
            insertsize; { all sizes are permissible }
            writeopnd;
            getoperand;
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            op := n^.oprnd.reg + op;
            end;

          writelastopnd;
          end;

        bchg, bclr, bset, btst: { bit manipulation group }
          begin
          if (n^.oprnd.m <> dreg) and (n^.oprnd.m <> immediate) then
            puterror(badsource);
          if n^.oprnd.m = dreg then
            begin { bit number dynamic mode }
            op := op + 400B;
            insertreghi; { register containing bit number }
            end
          else
            begin { bit number static mode }
            op := op + 4000B;
            setmodeonly; { process the immediate data }
            end;
          writeopnd;
          getoperand;
          writelastopnd;
          seteffective;
          end;

        bfclr, bfset, bftst:
          begin
          writelastopnd; { The effective address }
          getoperand;

          if n^.oprnd.m = bit_field_const then
            begin
              { "Len" is the length in bits, "offset1" is the offset in bits.
              }
            insertobj(((n^.oprnd.offset1 and 37B) * 100B) + (datasize and 37B));
            writebitfield( - 1, n^.oprnd.offset1, datasize);
            end
          else
            begin
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            insertobj(4000B + ((n^.oprnd.reg and 37B) * 100B) + (datasize and
                      37B));
            writebitfield(n^.oprnd.reg, 0, datasize);
            end;

          getprevoperand(1);
          seteffective;
          end;

        bfexts, bfextu:
          begin
          writelastopnd; { The effective address (leave off comma) }
          getoperand;

          if n^.oprnd.m = bit_field_const then
            begin
              { "Len" is the length in bits, "offset1" is the offset in bits,
                "reg" is the source register.
              }
            offset1 := n^.oprnd.offset1;
            getoperand;
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            insertobj(((n^.oprnd.reg and 7B) * 10000B) + ((offset1 and
                      37B) * 100B) + (datasize and 37B));
            writebitfield( - 1, offset1, datasize);
            end
          else
            begin
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            register := n^.oprnd.reg;
            getoperand;
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            insertobj(((n^.oprnd.reg and 7B) * 10000B) + 4000B + ((register and
                      37B) * 100B) + (datasize and 37B));
            writebitfield(register, 0, datasize);
            end;

          if switcheverplus[outputmacro] then writech(',');
          writelastopnd; { The register }

            { Back up two operands and output the effective address
              field to the object file. This is neccessary because the effective
              address is the source field in the assembler output, but any
              effect address descriptor words must follow the second word
              of the instruction.
            }
          getprevoperand(2);
          seteffective;
          end;

        bfins:
          begin
          if n^.oprnd.m <> dreg then puterror(missingDreg);
          writeopnd; { The register }
          register := n^.oprnd.reg;
          getoperand;
          writelastopnd; { The effective address }
          getoperand;

          if n^.oprnd.m = bit_field_const then
            begin
              { "Len" is the length in bits, "offset1" is the offset in bits,
                "reg" is the source register.
              }
            insertobj(((register and 7B) * 10000B) + ((n^.oprnd.offset1 and
                      37B) * 100B) + (datasize and 37B));
            writebitfield( - 1, n^.oprnd.offset1, datasize);
            end
          else
            begin
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            insertobj(((register and 7B) * 10000B) + 4000B + ((n^.oprnd.reg and
                      37B) * 100B) + (datasize and 37B));
            writebitfield(n^.oprnd.reg, 0, datasize);
            end;
          getprevoperand(1);
          seteffective;
          end;

        chk:
          begin
          seteffective;
          writeopnd;
          getoperand;
          if n^.oprnd.m <> dreg then puterror(missingDreg);
          writelastopnd;
          insertreghi;
          end;

        clr, neg, negx, notinst, tst:
          begin
          insertsize;
          seteffective;
          writelastopnd;
          end;

        cmpm:
          begin
          if n^.oprnd.m <> autoi then puterror(badsource);
            begin
            insertsize;
            op := (n^.oprnd.reg and 7B) + op;
            writeopnd;
            getoperand;
            writelastopnd;
            if n^.oprnd.m <> autoi then puterror(badoperand);
            insertreghi;
            end;
          end;

        divs, divu:
          begin
          if datasize <> word then puterror(badsize);
          seteffective;
          writeopnd;
          getoperand;
          writelastopnd;
          if n^.oprnd.m <> dreg then puterror(missingDreg);
          insertreghi;
          end;

        divsl, divul:
          begin
          if datasize <> long then puterror(badsize);
          writeopnd;
          getoperand;
          writelastopnd;

            { reg is the quotient register and indxr is the remainder
              register.  Note: If the quotient and remainder registers
              are the same then only a 32 bit quotient will be generated.
            }
          if n^.oprnd.m = twodregs then
            insertobj((((n^.oprnd.reg and
                      7B) * 10000B) + ord(currinst =
                      divsl) * 4000B) + n^.oprnd.indxr)
          else
            insertobj((((n^.oprnd.reg and
                      7B) * 10000B) + ord(currinst =
                      divsl) * 4000B) + n^.oprnd.reg);

            { Back up one operand and output the effective address field
              to the opject file. This is neccessary because the effective
              address is the source field in the assembler output, but any
              effect address descriptor words must follow the second word
              of the instruction.
            }
          getprevoperand(1);
          seteffective;
          end;

        exg:
          begin
{**note: genblk fix
            if datasize <> long then puterror(badsize);
}
          writeopnd;
          insertreghi; { assume that this is ok }
          if n^.oprnd.m = dreg then
            begin
            getoperand;
            if (n^.oprnd.m <> dreg) and (n^.oprnd.m <> areg) then
              puterror(baddestination);
            if n^.oprnd.m = dreg then op := op + 100B
            else op := op + 210B;
            insertreglo;
            end
          else
            begin
            if n^.oprnd.m <> areg then puterror(badsource);
            getoperand;
            if n^.oprnd.m = areg then
              begin
              op := op + 110B;
              insertreglo;
              end
            else if n^.oprnd.m = dreg then
              begin
              op := ((op and 7000B) div 1000B {remove high reg}
                    + op + 210B) and 170777B; {put it in lowend}
              insertreghi;
              end
            else puterror(baddestination);
            end;
          writelastopnd;
          end;

        ext, extb:
          begin
          if n^.oprnd.m <> dreg then puterror(missingDreg);
          if datasize = byte then puterror(badsize);

            { The mask is setup for a word to long, if the instruction is
              an EXTB (68020 only) or if this is an extend word to long set
              the correct bits.
            }
          if currinst = extb then
            op := op + 500B { change to byte-to-long form }
          else if datasize = long then op := op + 100B; { change to
                                                          word-to-long form }
          insertreglo;
          writelastopnd;
          end;

        jmp, jsr: { special operands }
          begin
          if n^.kind = oprndnode then
            begin
            writelastopnd;
            if (n^.oprnd.m = usercall) and switcheverplus[outputmacro] then
              begin
              reposition(procnamecolumn);
              writeprocname(n^.oprnd.offset, 100); {write procedure name}
              end;
            seteffective;
            end
          else {must be a labelnode}
            begin
            if switcheverplus[outputmacro] then
              begin
              writech('L');
              writeint(n^.labelno);
              end;

            mode := 71B; {absolute long}
            op := op + mode;
            insertobj(54B * 256 + sectionno[codesect] + 1);
            objtype[objctr] := objforw;
            currentpc := currentpc - 2; {this stuff's longer than code}
            findlabelpc(n^.labelno, isforward);
            relocn[objctr] := true;

            insertobj(labelpc div 16#10000); {high order}
            objtype[objctr] := objoff;
            insertobj(labelpc mod 16#10000); {low order}

            if isforward <> 0 then
              begin
              allocfixup; { generate a new fixupnode }
              fixups[objctr - 1] := fixuptail;
              with fixuptail^ do
                begin
                fixupkind := fixuplabel;
                fixuplen := long;
                fixuplabno := n^.labelno;
                fixupobjpc := fixupobjpc - 4;
                end;
              end;
            objtype[objctr] := objoff;
            end;
          end;

        lea:
          begin
          n1 := nil;
          if n^.kind = oprndnode then
            begin
            seteffective;
            if n^.oprnd.m = usercall then {caused by stuffregisters} n1 := n;
            if n^.oprnd.m in
               [areg, dreg, autoi, autod, immediate, immediatelong] then
              puterror(badoperand);
            writeopnd;
            end
          else
            begin {must be relnode, used only for initial call}
            distancetemp := computedistance;
            op := op + 72B;
            insertobj(distancetemp);

            if switcheverplus[outputmacro] then
              begin
              writech('*');
              writech('+');
              writeint(distancetemp + word);
              writestr('(PC)');
              writech(',');
              end;
            end;
          getoperand;
          writelastopnd;
          if (n1 <> nil) and switcheverplus[outputmacro] then
            begin
            reposition(procnamecolumn);
            writeprocname(n1^.oprnd.offset, 100); {write procedure name}
            end;
          if n^.oprnd.m <> areg then puterror(missingAreg);
{**note: genblk fix
            if datasize <> long then puterror(badsize);
}
          insertreghi;
          end;

        link:
          begin
          if n^.oprnd.m <> areg then puterror(missingAreg);
          insertreglo; { dynamic link register }
          writeopnd;
          getoperand;

          if not mc68020 then datasize := word; {size operand is only 16 bits
                                                  long}

          writelastopnd; { 68020 long is written here }
          if n^.oprnd.m <> immediate then puterror(baddestination)
          else if n^.oprnd.offset > 0 then puterror(badoffset);
          setmodeonly;
          end;

        movem: buildmovem(false);

        muls, mulu:
          begin
          if mc68020 and (datasize = long) then
            begin
            writeopnd;
            getoperand;
            writelastopnd;
            if n^.oprnd.m <> dreg then puterror(missingDreg);

            insertobj(((n^.oprnd.reg and
                      7B) * 10000B) + ord(currinst = muls) * 4000B);

              { Back up one operand and output the effective address field
                to the opject file. This is neccessary because the effective
                address is the source field in the assembler output, but any
                effect address descriptor words must follow the second word
                of the instruction.
              }
            getprevoperand(1);
            seteffective;
            end
          else if datasize = word then
            begin
            seteffective;
            writeopnd;
            getoperand;
            writelastopnd;
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            insertreghi;
            end
          else puterror(badsize);
          end;

        pea:
          begin {* * * add control mode only checks * * *}
{**note: genblk fix
            if datasize <> long then puterror(badsize);
}
          seteffective;
          writelastopnd;
          end;

        swap:
          begin
          if n^.oprnd.m <> dreg then puterror(badoperand);
          insertreglo;
          writelastopnd;
          end;

        rte, rts, trapcc, trapv:
        { remember, we did no "getoperand" for these guys } ;

        trap:
          begin
          if (n^.oprnd.m <> immediate) or (n^.oprnd.offset < 0) or
             (n^.oprnd.offset > 15) then
            puterror(badoperand);
          op := op + n^.oprnd.offset;
          writelastopnd;
          end;

        unlk:
          begin
          if n^.oprnd.m <> areg then puterror(missingAreg);
          insertreglo;
          writelastopnd;
          end

        otherwise puterror(unknowninst)
        end; {case inst}
  end {BuildInstruction} ;



procedure PutCode;

{ Output a block of either (or both) macro code or object code.  This simply
  scans the nodes and writes the instructions out.  It is basically a large
  case statement which does the formatting necessary for any unusual 
  instructions (of which there are an unfortunate number).
}

  var
    i, j: integer;
    s: longname;
    swbegkludgecount: unsignedint; {Do we need to put out obnoxious 'swbeg'
                                    assembly pseudo-op? Non-zero value = 'yes'}

  begin {PutCode}
    newsection(codesect);
    currentpc := highcode;
    lastobjpc := highcode;

    currnode := 0; { initialize node counter for code generation scan }
    relocn[1] := false; { opcodes do not get relocated }
    fixups[1] := nil;
    currlabel := 1;
    if switcheverplus[outputmacro] then doblocklabel;

    while currnode < lastnode do
      begin
      getnextnode;
      lineerrors := 0;
      instindex := currnode;
      instpc := currentpc;

      if currnode = blocklabelnode then DoBlockEntryCode;

      if switcheverplus[outputmacro] then
        if currnode = labeltable[currlabel].nodelink then writelabels;

      objctr := 0;
      with n^ do
        if kind <> instnode then

          if kind = labeldeltanode then
            begin
            findlabelpc(targetlabel, isforward); {can't be forward}
            labelpctemp := labelpc;
            findlabelpc(tablebase, isforward); {can't be forward}
            insertobj(labelpctemp - labelpc);

            if switcheverplus[outputmacro] then
              begin
              reposition(opcolumn);
              writestr('DC.W');
              reposition(opndcolumn);
              writech('L');
              writeint(targetlabel);
              writestr('-L');
              writeint(tablebase);
              end;

            writeobjline;
            currinst := nop; { to flush nodes to next inst }
            end { labeldeltanode}

          else
            begin
            if kind = stmtref then
              begin
	      i := stmtno;
              if i <> 0 then i := i - firststmt + 1;
              if switcheverplus[outputmacro] then
                begin
                if column > 1 then writeline;

                writeln(MacFile, '* Line: ', sourceline - lineoffset: 1,
                        ', Stmt: ', i: 1);
                end;

              if ((switchcounters[debugging] > 0) or
		 (switchcounters[profiling] > 0)) then
		begin
		writemaprecord(stmntrec, flags, sourceline, i);
		end;
              end
            else if kind = datanode then { insert constant data for 68881 }
              begin
              putdata(data div 16#10000);
              putdata(data mod 16#10000);

              if switcheverplus[outputmacro] then
                begin
                reposition(opcolumn);
                writestr('DC.W');
                reposition(opndcolumn);
                writech('$');
                WriteHex(data div 16#10000);
                writech(',');
                writech('$');
                WriteHex(data mod 16#10000);
                writeline;
                end
              end

            else if kind <> errornode then
              begin { HINT: prologuelength may be too small }
              puterror(missinginst);
              if switcheverplus[outputmacro] then dumperrors; { if any }
              end;
            currinst := nop { to flush nodes to next inst }
            end

        else
          begin { save instruction }
          currinst := inst;
          opcount := oprndcount;
          datasize := oprndlength;
          computed_len := computed_length;
          end; { save instruction }
 
      swbegkludgecount := 0; 
      if currinst <> nop then
        begin
        writeinst(currinst);
        if opcount = 0 then { check mnemonic }
          if not (currinst in [rte, rts, trapcc, trapv]) then
            puterror(badopcount)
          else { no operands required }
        else
          begin
          getnextnode;
          if (n^.kind = oprndnode) and (n^.oprnd.m = pcindexed)
          then swbegkludgecount := n^.oprnd.offset2;
          end;

        mode := 0;
        BuildInstruction;

        if computed_len <> (currentpc - instpc) then
          begin
          writeln('Instruction length mismatch, PC=', instpc: - 4, ', pass1=',
                  computed_len: 1, ', pass2=', currentpc - instpc: 1);
          abort(inconsistent);
          end;

        {update op value: may have changed due to operand modes}

        object[1] := op;
        writeobjline;

        if (swbegkludgecount <> 0) and
           (unixtarget in [Nti, VMEV2, NCR, Lmi, UniPlusV2, CTIX, NEC]) and
           switcheverplus[outputmacro]
        then {this is ugly, but we never wanted to do it in the first place}
          begin
          reposition(opcolumn);
          writestr('swbeg');
          reposition(opndcolumn);
          writech('&');
          writeint(swbegkludgecount);
          writeline;
          end;

        end; {inst <> nop}
      end; {while currnode}

    sectionpc[codesect] := currentpc;
    if nowdiagnosing then put_diags;
    highcode := sectionpc[codesect];
  end {PutCode} ;
