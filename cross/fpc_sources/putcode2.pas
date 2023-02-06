{$nomain}
{[b+,l-]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF CONFIDENTIAL SOFTWARE:
  Copyright 1977, 1978, 1979, 1980, 1981, 1982, 1985, 1988
            by Oregon Software, Inc.
  ALL RIGHTS RESERVED.

  This computer program is the proprietary property of Oregon
  Software, Inc. of Portland, Oregon, U.S.A., and may be used
  and copied only as specifically permitted under written
  license agreement signed by Oregon Software, Inc.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Versados specific code generation.
  Release version: 0045  Level: 1  Date: 21-Nov-1990 15:33:58
  Processor: ~processor~
}
{[a+]}


function get_from_sfile(loc, len: integer;
                        ident: boolean {true if in identifier section}
                       ): linknametype;

{ Read a string starting from "loc", "len" characters long from the stringtable.
}
  var
    i: integer;
    linkname: linknametype;

  begin {get_from_sfile}
  if ident then loc := stringfilecount + loc - 1; {skip strings}

  if needcaching then
    seekstringfile(loc)
  else
    begin
    curstringblock := loc div (diskbufsize + 1) + 1;
    stringblkptr := stringblkptrtbl[curstringblock];
    nextstringfile := loc mod (diskbufsize + 1);
    end;

  for i := 1 to maxprocnamelen do
    linkname[i] := ' '; { initialize link name }

  for i := 1 to min(linknameused, len) do
    begin { copy procedure name from string file }
    if language = pascal then linkname[i] := uppercase(chr(getstringfile))
    else linkname[i] := chr(getstringfile);
    end;

  get_from_sfile := linkname;
  end; {get_from_sfile}


procedure CopySFile;

{ Copy the string table and constant table from the string file to
  the macro file.  This is written as straight binary data, with no
  attempt to interpret it as strings or real numbers.

  The string file is actually divided into three parts.  The first is
  the string table, which contains string constants parsed by the
  scanner.  The second part is a table of identifiers, and the third
  part is constants built by analys.  Only the first and third
  parts are written here.
}

  var
    i: integer; { outer loop counter }


  procedure write_constants(i: integer);

    const
      charsperline = 12;  { number of chars in translated string }
      wordsperline = 6;  { number of constant structure values (hex) per line }

    var
      buffer: packed array [1..charsperline] of char;
      ch: char;
      old_i: integer;
      k: integer;
      v: uns_word;

    begin {write_constants}
    while i > 0 do
      begin
      reposition(opcolumn);
      writestr('DC.W');
      reposition(opndcolumn);
      old_i := i;

      for k := 1 to min(wordsperline, (i + 1) div 2) do
        begin
        if k > 1 then { separate words with commas }
          writech(',');
        v := getstringfile * 256;
        i := i - 1;

        if i > 0 then
          begin { don't grab a non-existent odd byte }
          v := getstringfile + v;
          i := i - 1;
          end;

        Writech('$');
        WriteHex(v);

        ch := chr(v div 256);

        if (ch < ' ') or (ch > '~') then buffer[k * 2 - 1] := '.'
        else buffer[k * 2 - 1] := ch;

        ch := chr(v mod 256);

        if (ch < ' ') or (ch > '~') then buffer[k * 2] := '.'
        else buffer[k * 2] := ch;
        end; { for k }

      reposition(nodecolumn);

      for k := 1 to min(charsperline, old_i) do
        writech(buffer[k]);

      writeline;
      end;
    end; {write_constants}


  begin {CopySFile}
    if needcaching then
      seekstringfile(0)
    else
      begin
      curstringblock := 1;
      stringblkptr := stringblkptrtbl[curstringblock];
      nextstringfile := 0;
    end;

    { first write the string table as fixed length character strings }

    i := stringfilecount;
    if i > 0 then begin
      writech('*');
      writeline;
      writeln(MacFile, '*  String Constants:');
      writech('*');
      writeline;
      writech('L'); { the label associated with string constants }
      writech(':');
      end;

    write_constants(i);

    currentpc := stringfilecount;

    {now move on to the constant table and write it}

    if needcaching then
      seekstringfile(stringtablelimit)
    else
      begin
      curstringblock := stringtablelimit div (diskbufsize + 1) + 1;
      stringblkptr := stringblkptrtbl[curstringblock];
      nextstringfile := stringtablelimit mod (diskbufsize + 1);
      end;
 
    i := consttablelimit - stringtablelimit;

    if i > 0 then
      begin
      writech('*');
      writeline;
      writeln(MacFile, '*  Structured Constants:');
      writech('*');
      writeline;

      if currentpc = 0 then
        begin
        writech('L'); { the label associated with structured constants }
        writech(':');
        end;

      currentpc := currentpc + i;
      end;

    if odd(currentpc) then currentpc := currentpc + 1; { should not be necessary! }

    write_constants(i);

    if odd(currentpc) then currentpc := currentpc + 1; { should not be necessary! }

    if currentpc = 0 then
      begin
      writech('L');
      reposition(opcolumn);
      writestr('EQU');
      reposition(opndcolumn);
      writech('*');
      writeline;
      end;
  end {CopySFile} ;



procedure InitMac;

{ Generate preliminary pieces of the macro file. In the process, some
  global variables used for code generation are initialized to point to
  some generated tables.
}

  var
    loc: integer; {location in the string file}
    i: integer; {induction variable}


  begin {initmac}

    if shortsection then sectiontail := '.S' else sectiontail := '  ';

    write(MacFile, '*  Oregon Software Pascal-2 V', version, ' ');

    if (hostopsys = targetopsys) and (hostmachine = targetmachine) then
      begin
      writeln(MacFile, 'Compiler');
      write(MacFile, '*         ');
      end
    else
      begin
      writeln(MacFile, 'Cross Compiler');

      case hostmachine of
        vax:
          write(MacFile, '*    VAX ');
        iapx86:
          write(MacFile, '*    IAPX-86 ');
        ns32k:
          write(MacFile, '*    NS32000 ');
        mc68000:
          write(MacFile, '*    MC68000 ');
        otherwise
          write(MacFile, '*    ??? ');
        end;

      case hostopsys of
        vms:
          write(MacFile, '(VMS) to ');
        unix:
          write(MacFile, '(Unix) to ');
        msdos:
          write(MacFile, '(MSDOS) to ');
        end;
      end;

    if mc68020 then
      begin
      write(MacFile, 'MC68020');
      if mc68881 then write(MacFile, '/MC68881');
      writeln(MacFile, ' (VERSAdos)');
      end
    else
      writeln(MacFile, 'MC68000 (VERSAdos)');

    write(MacFile, '*  Assembly Listing of:  ');
    writesymbolname(outputname);
    writeline;
    writech('*');
    writeline;

    if newobjectfmt then
      begin
      reposition(opcolumn);
      writestr('OPT');
      reposition(opndcolumn);
      writestr('NEWOBJ');
      writeline;

      reposition(opcolumn);
      writestr('OPT');
      reposition(opndcolumn);
      writestr('CASE');
      writeline;

      reposition(opcolumn);
      writestr('OPT');
      reposition(opndcolumn);
      writestr('MODULA2');
      writeline;
      end;

    writesymbolname(outputname);
    reposition(opcolumn);

    if newobjectfmt then
      writestr('NIDNT')
    else writestr('IDNT');

    reposition(opndcolumn);

    if newobjectfmt then writech('''');

    writeint(objversion);
    writech(',');
    writeint(objrevision);

    if newobjectfmt then writech('''');

    if ident_strlength > 0 then
      begin
      writech(' ');
      loc := stringfilecount + ident_string - 1;

      if needcaching then
	seekstringfile(loc)
      else
	begin
	curstringblock := loc div (diskbufsize + 1) + 1;
	stringblkptr := stringblkptrtbl[curstringblock];
	nextstringfile := loc mod (diskbufsize + 1);
	end;

      if newobjectfmt then writech('''');

      for i := 1 to ident_strlength do
	writech(chr(getstringfile));

      if newobjectfmt then writech('''');
      end;

    writeline;

    { Tell the assembler this is 68020 code.
    }
    if mc68020 then
      begin
      reposition(opcolumn);
      writestr('OPT');
      reposition(opndcolumn);
      writestr('P=68020');
      if mc68881 then
        begin
        writestr('/68881');
        writeline;
        reposition(opcolumn);
        writestr('FOPT');
        reposition(opndcolumn);
        writestr('ID=');
        writeint(coprocessor_id);
        end;
      writeline;
      end;

    reposition(opcolumn);
    writestr('SECTION');
    writestr(sectiontail);
    reposition(opndcolumn);
    writeint(sectionno[codesect]);
    writeline;

    CopySFile; { emit string and structured constants }
    highcode := currentpc; { initialize code address }
    lastobjpc := currentpc; { required if dumping object code }
  end {InitMac} ;


procedure InitObj;

  var
    i: integer;
    j: 0..proctablesize;
    data: unsigned;    { assembles string constant bytes into words }
    count: integer;    { number of stringfile bytes }


  begin {initObj}

{ Write the string constants and structured constants to the temporary
  object file.  Every 32 words of such data are preceeded by 2 words of
  zeroes, which signals the final object file that the data does not
  require relocation.
}
    nexttempbuffer := 0;
    nexttemprelocn := 0; 
    nextrelfile := - 1;
    nextobjfile := - 1;
    nextobjblk := 0;
    tempfilesize := 0; { number of bytes written to temp file }

    if scanalys then count := stringfilecount + ord(odd(stringfilecount))
    else count := stringfilecount;

    if needcaching then
      seekstringfile(0)
    else
      begin
      curstringblock := 1;
      stringblkptr := stringblkptrtbl[curstringblock];
      nextstringfile := 0;
      end;

    for i := 1 to count div 2 do
      begin
      data := getstringfile * 256;
      data := data + getstringfile;
      putbuffer(data, false);
      end;

    currentpc := count;
    if odd(currentpc) then
      currentpc := currentpc + 1; {should not be necessary!}

    {now move on to the constant table and write it}

    if needcaching then
      seekstringfile(stringtablelimit)
    else
      begin 
      curstringblock := stringtablelimit div (diskbufsize + 1) + 1;
      stringblkptr := stringblkptrtbl[curstringblock];
      nextstringfile := stringtablelimit mod (diskbufsize + 1);
      end;

    i := consttablelimit - stringtablelimit;

    if i > 0 then currentpc := currentpc + i;

    if odd(currentpc) then currentpc := currentpc + 1;

    while i > 0 do
      begin
      data := getstringfile * 256;
      i := i - 1;

      if i > 0 then
        begin { don't grab a non-existent odd byte }
        data := data + getstringfile;
        i := i - 1;
        end;

      putbuffer(data, false);
      end;

    if odd(currentpc) then currentpc := currentpc + 1;

    lastobjpc := currentpc;

    highcode := currentpc; { highest address so far }
    sectionpc[codesect] := currentpc;
  end {InitObj} ;



procedure FixMac;

{ Clean up the macro file.  There isn't much to do for this file.
}

  var
    i: ESDrange;
    j: integer;
    k: 1..linknamesize;
    suppcall: libroutines;
    support: packed array [libroutines] of boolean;
    s: namestring;
    linkname: linknametype;
    running_offset: integer;
    vtptr: vartablerecptr;


  procedure do_setuse;

  { Write SET/USE lines to the macro file for Modula2.
  }

    var
      procno : proctableindex;
      linkname: linknametype;
      fixupblock, fixupbyte: integer;

    procedure writetimestamp(start: integer);

    { Convert a four byte binary time stamp to 8 hex digits.
    }

      begin {writetimestamp}
        if language = modula2 then
          begin
          linkname := get_from_sfile(start, 4, true);

          write(Macfile, ord(linkname[1]):-2, ord(linkname[2]):-2,
                ord(linkname[3]):-2, ord(linkname[4]):-2)
          end;
      end; {writetimestamp}


    begin {do_setuse}
      if language = modula2 then
        begin
	if proctable[1].calllinkage = implementationbody then
	  writeprocname(1, linknameused)
	else { main body }
	  writestr('p_main');

	writech(':');
	reposition(opcolumn);
	writestr('SETKEY');
	reposition(opndcolumn);
	writech('''');
	writetimestamp(proctable[1].charindex + proctable[1].charlen);
	writech('''');
	writeline;

	for procno := 1 to proctabletop do
	  begin
	  if (proctable[procno].calllinkage = definitionbody) and
	     (proctable[procno].level = 1) {directly imported} then
	    begin
	    writeprocname(procno, linknameused);
	    writech(':');
	    reposition(opcolumn);
	    writestr('USEKEY');
	    reposition(opndcolumn);
	    writech('''');
	    writetimestamp(proctable[procno].charindex +
			   proctable[procno].charlen);
	    writech('''');
	    writeline;
	    end;
	  end;
        end;
    end; {do_setuse}

  begin {fixmac}
    writech('*');
    writeline;

    for suppcall := first_call to last_call do support[suppcall] := false;

    for i := firstESD to nextESD - 1 do
      begin
      with ESDtable[i] do
        case esdkind of
          esdsupport:
            support[suppno] := true;

          esdentry, esdexternal:
            begin
            reposition(opcolumn);
            if esdkind = esdentry then
              writestr('XDEF')
            else writestr('XREF');
            reposition(opndcolumn);
            writeprocname(exproc, linknameused);
            writeline;
            end;

          esdglobal:
            begin
            reposition(opcolumn);
            writestr('XDEF');
            reposition(opndcolumn);
            writestr('GLOBAL$$');
            writeline;
            writestr('GLOBAL$$');
            reposition(opcolumn);
            writestr('EQU');
            reposition(opndcolumn);
            writeint(glbsize);
            writeline;
            end;

          esddiag, esdload, esdcommon: { ignore this one };
          end; {case}
        end;

    for suppcall := first_call to last_call do
      if support[suppcall] then
        begin
        reposition(opcolumn);
        writestr('XREF');
        if not switcheverplus[longlib] then writestr('.S');
        reposition(opndcolumn);
        supname(suppcall, s);
        WriteSymbolName(s);
        writeline;
        end;

    if ownsize > 0 then
      begin
      if ownsect_string > 0 then
        begin
        linkname := get_from_sfile(ownsect_string, ownsect_strlength, true);
        WriteSymbolName(linkname);
        end
      else if proctable[0].charindex = 0 then writesymbolname(outputname)
      else writeprocname(0, linknameused); {use program name}
      reposition(opcolumn);
      writestr('SECTION');
      reposition(opndcolumn);
      writeint(datasection);
      writeline;
      writech('G');
      reposition(opcolumn);
      writestr('EQU');
      reposition(opndcolumn);
      writech('*');
      writeline;
      reposition(opcolumn);
      writestr('DS.B');
      reposition(opndcolumn);
      writech('$');
      write(MacFile, ownsize: -4);
      writeline;
      end;


    { Dump out all the "use" and "shared" variables that have the referenced
      flag set and dump all of the "define" variables.
    }
    if lastvartableentry > 0 then
      begin
      writech('*');
      writeline;

      if definesize > 0 then
        begin
        sectionpc[datasect] := definesize;
        sectionno[datasect] := datasection;
        newsection(datasect);
        end;

      running_offset := 0;

      for j := 1 to lastvartableentry do
        begin
        vtptr := getvartableptr(j);
        with vtptr^ do
          begin
          if referenced and (extvaralloc = usealloc) then
            begin
            reposition(opcolumn);
            writestr('XREF');
            reposition(opndcolumn);
            linkname := get_from_sfile(charindex, charlen, not aliased);
            WriteSymbolName(linkname);
            writeline;
            end
          else if extvaralloc = definealloc then
            begin
            { Adjust for alignment.
            }
            if running_offset < offset then
              begin
              reposition(opcolumn);
              writestr('DS.B');
              reposition(opndcolumn);
              writech('$');
              write(MacFile, offset - running_offset: -4);
              writeline;
              running_offset := offset + size;
              end
            else running_offset := running_offset + size;

            reposition(opcolumn);
            writestr('XDEF');
            reposition(opndcolumn);
            linkname := get_from_sfile(charindex, charlen, not aliased);
            WriteSymbolName(linkname);
            writeline;
            WriteSymbolName(linkname);
            writech(':');
            reposition(opcolumn);
            writestr('DS.B');
            reposition(opndcolumn);
            writech('$');
            write(MacFile, size: -4);
            writeline;
            end
          else if referenced then { shared_var }
            begin
            linkname := get_from_sfile(charindex, charlen, true);
            WriteSymbolName(linkname);
            reposition(opcolumn);
            writestr('SECTION');
            reposition(opndcolumn);
            writeint(datasection);
            writeline;

            WriteSymbolName(linkname);
            writech(':');
            reposition(opcolumn);
            writestr('DS.B');
            reposition(opndcolumn);
            writech('$');
            write(MacFile, size + ord(odd(size)): -4);
            writeline;
            end;
          end; {with}
        end; {for}

      { Align the defined variables.
      }
      if odd(definesize) then
        begin
        reposition(opcolumn);
        writestr('DS.B');
        reposition(opndcolumn);
        writech('$');
        write(MacFile, 1: 1);
        writeline;
        end;
      end; {lastvartableentry > 0}

    if language = modula2 then do_setuse;

    if testing and (fixuphead <> nil) then
      begin
      fixp := fixuphead;
      writech('*');
      writeline;
      while fixp <> nil do
        with fixp^ do
          begin
          if fixupkind <> fixupESDid then
            writeln(MacFile, '* Fixup location ', fixupobjpc: -4,
                    ' with value ', fixupaddr: -4);
          fixp := fixuplink; { get next pointer }
          end; { with }
      end {if testing};

    writech('*');
    writeline;

    if totalputerr > 0 then
      begin
      writeln(MacFile, stars, totalputerr:1, checkmsg);
      write(totalputerr: 1, checkmsg);
      abort(inconsistent);
      end

    else
      begin
      writeln(MacFile, '*  [', sectionpc[codesect]: -4,
              ']  ', sectionpc[codesect]:1, ' code bytes generated');

      if everdiagnosing then
        begin
        writeln(MacFile, '*  [', sectionpc[diagsect]: -4, ']  ',
                sectionpc[diagsect]:1, ' diagnostic bytes generated');
        end;
      writech('*');
      writeline;

      { Oasys assembler likes to know where begin$ is }
      if startaddress <> undefinedaddr then
        begin
        reposition(opcolumn);
        writestr('XDEF');
        reposition(opndcolumn);
        writestr('BEGIN$');
        writeline;
        end;

      reposition(opcolumn);
      writestr('END');
      if startaddress <> undefinedaddr then
        begin
        reposition(opndcolumn);
        writestr('BEGIN$');
        end;
      writeline;
      end;
  end; {FixMac}


procedure FixDefines;

  { Put out an XDEF to the object file for each non-referenced "define"
    variable.
  }

  var
    vtptr: vartablerecptr;
    j: integer;

  begin {fixdefines}
    if lastvartableentry > 0 then
      begin
      for j := 1 to lastvartableentry do
        begin
        vtptr := getvartableptr(j);
        with vtptr^ do
          if not referenced and (extvaralloc = definealloc) then
            begin
            newESD.ESDkind := ESDdefine;
            newESD.vartabindex := j;
            findESDid;
            end; { not referenced and (extvaralloc = definealloc) }
        end; { for }
      end; { lastvartableentry > 0 }
  end; {fixdefines}


procedure FixObj;

  var
    procid: proctableindex;
    loc: integer;
    j: integer;
    vtptr: vartablerecptr;


  procedure putbyte(data: bytesize);


    begin {putbyte}
      if nextobjfile = 255 then
        begin
        put(objfile); { good riddence }
        nextobjfile := 0; { reset fill index }
        nextobjblk := nextobjblk + 1;
        end
      else nextobjfile := nextobjfile + 1;

      objfile^[nextobjfile] := data;
      objbytecount := objbytecount + 1;
    end; {putbyte}


  procedure putlong(data: unsigned);

    begin {putlong}
      putbyte((data and $FF000000) div $1000000); { high order first }
      putbyte((data and $FF0000) div $10000);
      putbyte((data and $FF00) div $100);
      putbyte(data and $FF); { down to low order }
    end; {putlong}


  procedure putword(data: unsigned);

    begin {putword}
      putbyte(data div 256); { high order first }
      putbyte(data mod 256); { then low order }
    end; {putword}


  procedure putname(linkname: packed array [l..h: shortint] of char);

  { Write out the given string to the object file.  If we are writing a
    new format object, a null byte is added to the end.
  }

    var i: shortint;


    begin {putname}
      if newobjectfmt then
        begin
        i := 1;
        while (i <= h) and (linkname[i] <> ' ') do
          begin
          if language = pascal then putbyte(ord(uppercase(linkname[i])))
          else putbyte(ord(linkname[i]));
          i := i + 1;
          end;
	putbyte(0);
        end
      else
        begin
	for i := 1 to h do
          if i <= linknameused then putbyte(ord(uppercase(linkname[i])));
	for i := linknameused + 1 to linknamesize do
	  putbyte(ord(' '));
        end;
    end; {putname}

  procedure putdateandtime;

  { Write the current date and time to the object file.
  }
    type
      datimtype = (hour, min, sec, month, day, year);

    var
      bcdbuf: array [datimtype] of integer;
      i: datimtype;


    begin {putdateandtime}
      timestamp(bcdbuf[day], bcdbuf[month], bcdbuf[year], bcdbuf[hour],
                bcdbuf[min], bcdbuf[sec]);
      bcdbuf[year] := bcdbuf[year] mod 100;
      for i := hour to year do
        begin
        bcdbuf[i] := (bcdbuf[i] div 10) * 16 + bcdbuf[i] mod 10;
        putbyte(bcdbuf[i]);
        end;
    end {putdateandtime} ;


  procedure fixuplength(block, byte, count: integer);

    var
      old_nextobjfile: integer;
      old_nextobjblk: integer;

    begin {fixuplength}
      if block <> nextobjblk then
        begin
        put(objfile);
        seek(objfile, block + 1);
        end;

      old_nextobjfile := nextobjfile;
      old_nextobjblk := nextobjblk;
      nextobjfile := byte;
      putlong(count);

      if block <> nextobjblk then
        begin
        put(objfile);
        nextobjblk := old_nextobjblk;
        seek(objfile, nextobjblk + 1);
        end;

      nextobjfile := old_nextobjfile;
    end; {fixuplength}


  procedure writeidentrecord;

  { Write a old type 1 ident record or a new type 5 ident record to the object
    file.
  }
    const
      magiclength = 44; { number of bytes generated below }

    var
      i: 1..magiclength; { induction var for filling record }
      fixupblock, fixupbyte: integer;


    begin {writeidentrecord}
      if newobjectfmt then
        begin
        putbyte(1);
	objbytecount := 0;
        putbyte(ord('5')); { type 5 record }
	fixupblock := nextobjblk;
	fixupbyte := nextobjfile;
	putlong(0);
        putname(outputname);
        putbyte(0); { version --  NYI }
        case language of
          pascal: putbyte(ord('P'));
          modula2: putbyte(ord('M'));
          c: putbyte(ord('?')); { ??? I don't know what this is going to be }
          end;
        putbyte(0); { filename -- NYI }
        end
      else
        begin
	putbyte(magiclength + ident_strlength);
	putbyte(ord('1'));
	putname(outputname);
	putbyte(objversion);
	putbyte(objrevision);
	putbyte(ord('P')); { Language = Pascal }
	for i := 1 to 4 do { Volume Name }
	  putbyte(ord(' '));
	putword(0); { User Number }
	for i := 1 to 18 do { Cat, Fname, Ex }
	  putbyte(ord(' '));
        end;

      putdateandtime;

      if ident_strlength > 0 then
	begin
	loc := stringfilecount + ident_string - 1;

	if needcaching then
	  seekstringfile(loc)
	else
	  begin
	  curstringblock := loc div (diskbufsize + 1) + 1;
	  stringblkptr := stringblkptrtbl[curstringblock];
	  nextstringfile := loc mod (diskbufsize + 1);
	  end;

        for i := 1 to ident_strlength do
	  putbyte(getstringfile);
	end;
      if newobjectfmt then
        begin
        putbyte(0);
        fixuplength(fixupblock, fixupbyte, objbytecount);
        end;
    end; {writeidentrecord}


  procedure write_setuse_records;

  { Write a SET/USE record to the object file for Modula2.
  }
    var
      procno : proctableindex;
      linkname: linknametype;
      fixupblock, fixupbyte: integer;

    procedure writetimestamp(start: integer);

    { Convert a four byte binary time stamp to 8 hex digits.
    }

      type
        cvtarray = packed array [0..15] of char;

      const
        cvt = cvtarray('0', '1', '2', '3', '4', '5', '6', '7',
                       '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

      var
        i :integer;

      begin {writetimestamp}
        if language = modula2 then
          begin
	  linkname := get_from_sfile(start, 4, true);

	  for i := 1 to 4 do
	    begin
	    putbyte(ord(cvt[ord(linkname[i]) div 16]));
	    putbyte(ord(cvt[ord(linkname[i]) mod 16]));
	    end;

	  putbyte(0);
          end;
      end; {writetimestamp}


    begin {write_setuse_records}
      if language = modula2 then
        begin
	putbyte(1); { length of ESD record always 1 }
	objbytecount := 0;
	putbyte(ord('7')); { type 7 record }
	fixupblock := nextobjblk;
	fixupbyte := nextobjfile;
	putlong(0);
	putbyte(1); { set }

	if proctable[1].calllinkage = implementationbody then
	  begin
	  linkname := get_from_sfile(proctable[1].charindex,
				     proctable[1].charlen, true);

	  putname(linkname); { send name to object file }
	  end
	else { main body }
	  putname('p_main');

	writetimestamp(proctable[1].charindex + proctable[1].charlen);

	for procno := 1 to proctabletop do
	  begin
	  if (proctable[procno].calllinkage = definitionbody) and
	     (proctable[procno].level = 1) {directly imported} then
	    begin
	    putbyte(0); { use }
	    linkname := get_from_sfile(proctable[procno].charindex,
				       proctable[procno].charlen, true);
	    putname(linkname); { send name to object file }
	    writetimestamp(proctable[procno].charindex +
			   proctable[procno].charlen);
	    end;
	  end;

	fixuplength(fixupblock, fixupbyte, objbytecount);
        end;
    end; {write_setuse_records}


  procedure writeESDrecord;

  { Write an only type 2 ESD record or a new type 6 ESD record to the object
    file.
  }
    var
      s: namestring;
      linkname: linknametype;
      i: 1..linknamesize;
      loc: integer; { location in string file }
      temp: integer; { used to convert support number to decimal }
      ESDlo,
      ESDhi,
      ESD: ESDrange; { induction vars for scanning ESDtable }
      ESDcounter: bytesize; { tracks length of ESDrecord }
      count: 0..15; { records length of one table entry }
      vtptr: vartablerecptr;
      fixupblock, fixupbyte: integer;
      writeheader: boolean;

    begin {writeESDrecord}
      writeheader := true;
      ESDlo := firstESD;
      objbytecount := 0;

      while ESDlo < nextESD do
        begin
        ESDhi := ESDlo;
        ESDcounter := 1; { for the identification byte }

        while (ESDcounter <= 255) and (ESDhi < nextESD) do
	  begin

	  with ESDtable[ESDhi] do { compute length of next entry }
	    case ESDkind of
	      ESDuse,
	      ESDsupport,
	      ESDexternal: count := 11;
	      ESDglobal,
	      ESDcommon,
	      ESDdiag,
	      ESDdefine,
	      ESDshrvar,
	      ESDentry: count := 15;
	      ESDload: count :=  5;
	      end; { case newESD.kind }
	  ESDhi := ESDhi + 1;
	  ESDcounter := ESDcounter + count;
	  end; { while }

	if ESDcounter > 255 then
	  begin
	  ESDhi := ESDhi - 1; { because we're 2 beyond }
	  ESDcounter := ESDcounter - count;
	  end;

        if not newobjectfmt then
          begin
          { now that the length of the record is known in advance ... }
          putbyte(ESDcounter); { length of ESD record }
          putbyte(ord('2')); { type 2 record }
          end
        else if writeheader then { newobjectfmt }
          begin
          writeheader := false;
          putbyte(1); { length of ESD record always 1 }
          objbytecount := 0;
          putbyte(ord('6')); { type 6 record }
          fixupblock := nextobjblk;
          fixupbyte := nextobjfile;
          putlong(0);
          end;

        for ESD := ESDlo to ESDhi - 1 do
          with ESDtable[ESD] do
            case ESDkind of

              ESDsupport: { construct support routine name }
                begin
                putbyte(160B);
                supname(suppno, s);
                putname(s);
                end; { ESD support name }

              ESDexternal, { pull name from StringFile }
              ESDentry,
              ESDcommon: { pull name from StringFile, append offset }
                begin
                procid := exproc;
                if ESDkind = ESDentry then
                  putbyte(100B + sectionno[codesect])
                else if ESDkind = ESDcommon then
                  begin
                  procid := 0;
                  putbyte(20B + datasection);
                  end
                else { ESDexternal }
                  putbyte(160B); { to any section }

                if (ESDkind = ESDcommon) and (ownsect_string > 0) then
                  linkname := get_from_sfile(ownsect_string, ownsect_strlength,
                                             true)
                else if proctable[procid].charindex = 0 then
                  linkname := outputname
                else linkname := get_from_sfile(proctable[procid].charindex,
                                 proctable[procid].charlen, true);

                putname(linkname); { send name to object file }

                if ESDkind = ESDentry then
                  begin
                  putlong(procmap[procid].addr);
                  end
                else if ESDkind = ESDcommon then
                  begin
                  putlong(glbsize);
                  end;
                end; { ESD entry and external }

              ESDglobal: { global name and address }
                begin
                putbyte(120B); { XDEF in absolute section }
                putname('GLOBAL$$       ');
                putlong(glbsize); { offset may be > 64K }
                end;

              ESDbegin: { BEGIN$ and startaddress }
                begin
                  { XDEF in relocatable code section }
                putbyte(100B + sectionno[codesect]);
                putname('BEGIN$         ');
                putlong(startaddress); { but offset will be < 64K }
                end;

              ESDdiag: {diagnostic code common reference}
                begin
                putbyte(20B+sectionno[diagsect]); {common section}
                putname('P_DIAG         '); {named common}
                putlong(0); {just a nominal reference}
                end;

              ESDload: { standard load section and length of load module }
                begin
                if (sect = codesect) and shortsection then
                  putbyte(60B + sectionno[sect])
                else putbyte(40B + sectionno[sect]);
                putlong(sectionpc[sect]); { this is the next free word }
                end;

              ESDuse:
                begin
                vtptr := getvartableptr(vartabindex);

                with vtptr^ do
                  begin
                  linkname := get_from_sfile(charindex, charlen, not aliased);
                  putbyte(160B); { to any section }
                  putname(linkname); { send name to object file }
                  end; { with }
                end;

              ESDdefine:
                begin
                vtptr := getvartableptr(vartabindex);

                with vtptr^ do
                  begin
                  linkname := get_from_sfile(charindex, charlen, not aliased);
                  putbyte(100B + datasection);
                  putname(linkname); { send name to object file }
                  putlong(offset);
                  end; { with }
                end;

              ESDshrvar:
                begin
                vtptr := getvartableptr(vartabindex);

                with vtptr^ do
                  begin
                  linkname := get_from_sfile(charindex, charlen, true);
                  putbyte(20B + datasection);
                  putname(linkname); { send name to object file }
                  putlong(size + ord(odd(size)));
                  end; { with }
                end;
              end; { case ESDtable[i] }

        ESDlo := ESDhi; { prepare for the next record, if any }
        end; { while ESDlo < nextESD }

      if newobjectfmt then fixuplength(fixupblock, fixupbyte, objbytecount);
    end; {writeESDrecord}


  procedure writeDataRecords;

{ Note that this contains a gross kluge to put the esd record for
  the common section at the end.  Any esdid of 0 is assumed to be
  a reference to the common esdid, and is so treated.

  This is kluged in because Wayne wants to do it right and sort
  esdids anyway, and I didn't feel like doing it twice.
}

    var
      data, offset_len: unsigned; { single word buffer between files }
      finalcount: addressrange; { number of words re-read from temp file }
      cursection: 0..15; {current section}
      tempbuffer: unsigned; {first word before being broken up}      
      datacount: 0..32; { number of words remaining before header is req'd }
      fixuplocation,
      fixupaddress: addressrange; { pulled from fixup list }
      fixuplength: integer; { word or long fixup for procedures }
      fixupobjectpc: integer; { address of fixup }
      fixupvartabindex: integer;
      { The following is a kluge and should be replaced later }
      relcode: array [1..32] of boolean;
      pieceno: 0..32;
      i: 0..7; { loop counter for ESDID's }
      esdid_count: 0..7; { Number of ESDID's in relocation expression }
      vtptr: vartablerecptr;

    const
      debug = false;


    function gettemp: unsigned;


      begin {gettemp}
        if nextrelfile = maxrelfile then
          begin
          get(relfile);
          nextrelfile := 0;
          end
        else nextrelfile := nextrelfile + 1;

        finalcount := finalcount + 1;
        gettemp := relfile^[nextrelfile];
      end; {gettemp}


    procedure nextfixup;


      begin {nextfixup}
        if fixuphead = nil then { no fixups to be had }
          begin
          fixupaddress := 0;
          fixuplocation := maxint;
          end
        else
          begin { consume fixupnode }
          fixp := fixuphead;
          with fixp^ do
            begin
            fixupaddress := fixupaddr;
            fixuplocation := fixupfileloc;
            fixuphead := fixuplink; { unhook this node }
            fixuplength := fixuplen;
            fixupobjectpc := fixupobjpc;
            if fixupkind = fixupesdid then fixupvartabindex := vartabindex;
            end; { with fixp^ }
          dispose(fixp); { and return it to the heap }
          end; { consume fixupnode }
      end; {nextfixup}


    procedure decr_datacount;
      { Decrement the word count and check for consistency.
      }
      begin {decr_datacount}
      datacount := datacount - 1;

      if datacount < 0 then
        begin
        write('WRITEDATARECORDS internal error -- bad datacount');
        abort(inconsistent);
        end;
      end; {decr_datacount}


    procedure do_fixup(relocated: boolean {Don't bump pieceno if true});

      { If the current location is the next fixup then handle it.  If a long
        fixup is needed, get the next word and build the longword before adding.
      }
      begin {do_fixup}
      if finalcount = fixuplocation then
        begin { apply the fixup }
        if fixuplength = long then
          begin
          data := data * $10000 + gettemp;
          if not relocated then pieceno := pieceno + 1;
          end;

        data := data + fixupaddress;

        if fixuplength = long then
          begin
          putword(data div $10000);
          data := data mod $10000;
          decr_datacount; { account for extra word }
          end;

        nextfixup;
        end { apply the fixup }
      else if finalcount > fixuplocation then
        begin
        write('DO_FIXUP internal error -- missed fixup at filepos=',
              fixuplocation:1, ', fixup=',fixupaddress:-1,', pc=',
              fixupobjectpc:-1);
        abort(inconsistent);
        nextfixup;  { Just in case of /test }
        end;

      putword(data);
      end; {do_fixup}


    begin {writeDataRecords}
      nextfixup; { get fixup data, if any }
      datacount := 0;
      finalcount := 0;
      nextrelfile := - 1;

      while finalcount <> tempfilesize do
        begin
        if datacount = 0 then
          begin { new record header }
          tempbuffer := gettemp;
          datacount := tempbuffer mod 256;
          cursection := tempbuffer div 256;
          putbyte(datacount * 2 + 6); { size in bytes}
          putbyte(ord('3')); { type of Data Record }

          {the following is part of the main kluge}
          data := gettemp;
          putword(data); { 1st 16 relocation bits }
          for pieceno := 16 downto 1 do
            begin
            relcode[pieceno] := odd(data);
            data := data div 2;
            end;
          data := gettemp;
          putword(data); { 2nd 16 relocation bits }
          for pieceno := 32 downto 17 do
            begin
            relcode[pieceno] := odd(data);
            data := data div 2;
            end;
          pieceno := 0;
          putbyte(cursection); { in every record }
          end; { new record header }

        decr_datacount;
        data := gettemp; { fetch word from temp file }

        { The main part of the kluge}
        pieceno := pieceno + 1;
        if debug then writeln('pieceno = ', pieceno:1);

        if pieceno > 32 then
          begin
          write('WRITEDATARECORDS internal error -- bad pieceno');
          abort(inconsistent);
          end;

        if relcode[pieceno] then
          begin
          offset_len := (data and $700) div 256; { offset field length }

          { There are two hacks here:

            1 - There may be a fixup of the ESD record to fill in the ESDid
                of a "shared" variable which is really a named common.  This
                is necessary because all named commons must appear after all
                xref's and xdef's.
            2 - If there is no fixup, then a zero ESDid in the first ESDid
                position, which would normally be meaningless, means common
                section relocation.
          }
          if (finalcount >= fixuplocation) then
            if finalcount > fixuplocation then
              begin
              write('WRITEDATARECORDS internal error -- missed fixup at filepos=',
                fixuplocation:1, ', fixup=',fixupaddress:-1,', pc=',
                fixupobjectpc:-1);
              abort(inconsistent);
              nextfixup;  { Just in case of /test }
              end
            else
              begin { insert the ESDid in data word }
              vtptr := getvartableptr(fixupvartabindex);
              putword(data + vtptr^.offset);
              nextfixup;
              end
          else if data and 255 = 0 then putword(data + commonesdid)
          else putword(data);

          esdid_count := data div (32 * 256); { shift the ESDID count down }
          if debug then writeln('esdid_count = ', esdid_count:1);

          { Account for multiple ESDID's.  They will always appear in odd
            numbers.  A zero ESDID in the last position is used to pad to
            word boundary for our self-imposed file of words.
          }
          for i := 2 to esdid_count do
            if not odd(i) then { by two's }
              begin
              decr_datacount;
              data := gettemp;
              putword(data);
              end;

          if offset_len > 0 then
            begin
            decr_datacount;
            data := gettemp;
            end;

          { If there is no fixup then process the high order word here, 
            otherwise the application of the fixup below will skip over
            both words.
          }
          if (offset_len = 4) and (finalcount <> fixuplocation) then
            begin
            putword(data);
            decr_datacount;
            data := gettemp;
            end;

          if offset_len > 0 then do_fixup(true);
          end { if relcode[pieceno] }
        else
          do_fixup(false);

        end; { while }

    end; {writeDataRecords}


  procedure writeEndRecord;


    begin {writeEndRecord}
      if startaddress = undefinedaddr then
        begin
        putword(2 * 256 + ord('4')); { length = 2 bytes }
        putbyte(17); { indicates no start address }
        end
      else
        begin
        putword(6 * 256 + ord('4')); {length = 6 bytes}
        putbyte(sectionno[codesect]); {starts in the code}
        putlong(startaddress);
        end;
    end; {writeEndRecord}


  begin {FixObj}
    newESD.ESDkind := ESDload;
    newesd.sect := codesect;
    insertnewESD;

    if ownsize > 0 then
      begin
      newesd.esdkind := esdcommon;
      newesd.glbsize := ownsize;
      findesdid;
      commonesdid := esdid;
      end;

    if definesize > 0 then
      begin
      sectionpc[datasect] := definesize + ord(odd(definesize));
      sectionno[datasect] := datasection;
      newESD.ESDkind := ESDload;
      newesd.sect := datasect;
      insertnewESD;
      end;

    { Allocate an ESDID for each "shared" variable that has been referenced.
    }
    if lastvartableentry > 0 then
      begin
      for j := 1 to lastvartableentry do
        begin
        vtptr := getvartableptr(j);
        with vtptr^ do
          if referenced and (extvaralloc = sharedalloc) then
            begin
            newESD.ESDkind := ESDshrvar;
            newESD.vartabindex := j;
            findESDid;
            offset := ESDid;
            end; { referenced and (extvaralloc = sharedalloc) }
        end; { for }
      end; { lastvartableentry > 0 }

    flushtempbuffer;

    repeat
      putrelfile(0); { flush the file buffer }
    until nextrelfile = 0;

    seek(relfile, 1); { prepare to read it back in }

    writeidentrecord;
    if language = modula2 then write_setuse_records;
    writeESDrecord;
    if newobjectfmt and odd(nextobjfile) then putbyte(0);
    writeDataRecords;
    writeEndRecord;

    repeat
      putbyte(0); { flush objfilebuffer }
    until nextobjfile = 0;

    if totalputerr <> 0 then
      begin
      writeln(stars, totalputerr:1, checkmsg);
      abort(inconsistent);
      end;

  end; {FixObj}


procedure fixdiags;

{ Final code when diagnostics are generated
}

  var
    junk: integer; { throw-away result of dump_externals }
    switches: packed record case boolean of
      true: (sw: packed array [1..16] of boolean);
      false: (u: uns_word);
      end;

  begin {fixdiags}
    switches.u := 0;

    if reversebytes then
      begin
      switches.sw[16] := switchcounters[mainbody] > 0;
      switches.sw[15] := switcheverplus[own];
      switches.sw[14] := switcheverplus[doublereals];
      switches.sw[13] := switcheverplus[caseswitch];
      switches.sw[12] := switcheverplus[shortintegers];
      switches.sw[11] := mc68881;
      end
    else
      begin
      switches.sw[1] := switchcounters[mainbody] > 0;
      switches.sw[2] := switcheverplus[own];
      switches.sw[3] := switcheverplus[doublereals];
      switches.sw[4] := switcheverplus[caseswitch];
      switches.sw[5] := switcheverplus[shortintegers];
      switches.sw[6] := mc68881;
      end;

    if switcheverplus[debugging] or switcheverplus[profiling] then
      currentpc := currentpc + 2;

    if switcheverplus[outputmacro] then
      begin
      newsection(codesect);
      writeln(macfile, 'LAST', 'EQU': opcolumn - 4 + 2,
              '*': opndcolumn - opcolumn - 3 + 1);
      end;

    newsection(diagsect);
    if nextdiagbit > 0 then diag_bits(0, 16 - nextdiagbit);

    if switcheverplus[outputmacro] then
      if switcheverplus[debugging] or switcheverplus[profiling] then
        writeln(macfile, 'DC.W': opcolumn + 3, ' ': opndcolumn - opcolumn - 4,
                switches.u: 1);

    if switcheverplus[outputobj] then
      if switcheverplus[debugging] or switcheverplus[profiling] then
        putbuffer(switches.u, false);

    if switcheverplus[debugging] then junk := dump_externals;

    if switcheverplus[outputmacro] then
      writeln(macfile, 'ENDIAG', 'DC.W': opcolumn - 6 + 3,
              'STDIAG-ENDIAG': opndcolumn - opcolumn - 4 + 13);

    if switcheverplus[outputobj] then
      begin
      diaglenfix^.fixupaddr := currentpc;
      codelenfix^.fixupaddr := sectionpc[codesect];
      putbuffer(-currentpc, false);
      newesd.esdkind := ESDdiag;
      insertnewesd;
      end;

    currentpc := currentpc + 2;
    sectionpc[diagsect] := currentpc;
    newesd.esdkind := esdload;
    newesd.sect := diagsect;
    insertnewesd;
  end; {fixdiags}
