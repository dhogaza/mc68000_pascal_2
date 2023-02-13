{[b+,l+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1977, 1978, 1979, 1980, 1981, 1982, 1985, 1988
  by Oregon Software, Inc.  All Rights Reserved.

  This computer program is the property of Oregon Software, Inc.
  of Portland, Oregon, U.S.A., and may be used
  and copied only as specifically permitted under written
  license agreement signed by Oregon Software, Inc.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Common routines for CODE and PUTCODE only
  Release version: 0045  Level: 1  Date: 21-Nov-1990 15:34:32
  Processor: M68000
}


{ The following procedures perform formatted output to the assembler file.
  The routines which accept strings of 4 and 8 characters will suppress
  trailing blanks.  The "writeint" routine outputs decimal integers with
  leading zero suppression.  "WriteHex" always outputs 4 hexadecimal digits
  without zero suppression.  "Reposition" essentially performs the tabbing
  function to a given column position.  Finally, "writeline" emits an end-
  of-line, and resets the software column counter.  There are, however,
  several places in the code which perform output to MacFile without going
  through these routines; usually these are writeln's of long strings, where
  the value of "column" will not be affected.  See "initmac" for examples.
}

procedure writech(ch: char);

  begin
    write(MacFile, ch);
    column := column + 1;
  end {writech};


procedure writestr(s: packed array[low..high: integer] of char);

  var i, j: integer;

  begin
    j := high;
    while (j > 0) and (s[j] = ' ') do j := j - 1;
    column := column + j;
    for i := low to j do
      write(MacFile, s[i]);
  end {writestr};


procedure writeint(v: integer);

  var
    bufptr: 0..9;
    buffer: array [1..20] of char;
    u: unsigned;

  begin
    bufptr := 0;
    if v < 0 then
      writech('-');
    u := abs(v);

    repeat
      bufptr := bufptr + 1;
      buffer[bufptr] := chr (u mod 10 + ord('0'));
      u := u div 10;
    until u = 0;

    repeat
      writech(buffer[bufptr]);
      bufptr := bufptr - 1;
    until bufptr = 0;
  end; {writeint}


procedure WriteHex(v: unsigned {value to write} );

{ Write an unsigned value to the macro file as a hexadecimal number.
  16 bit values only.
}
  const
    maxhex = 4;

  var
    hexbuf: packed array [1..maxhex] of char;
    i: 1..maxhex;    { induction var for filling hexbuf }
    j: 0..15;        { numeric value of one hex digit }

  begin
    v := v and 65535; {** a kludge **}
    for i := maxhex downto 1 do begin
      j  := v mod 16;
      v := v div 16;
      if j <= 9 then
        hexbuf[i] := chr(ord('0') + j)
      else hexbuf[i] := chr(ord('A') + j - 10);
      end; { for i }
    write(MacFile, hexbuf);
    column := column + 4;
  end {WriteHex} ;


procedure WriteHexLong(v: unsigned {value to write} );

{ Write an unsigned 32 bit value to the macro file as a hexadecimal number.
}
  const
    maxhex = 8;

  var
    hexbuf: packed array [1..maxhex] of char;
    i: 1..maxhex;    { induction var for filling hexbuf }
    j: 0..15;        { numeric value of one hex digit }

  begin {WriteHexLong}
    for i := maxhex downto 1 do begin
      j  := v mod 16;
      v := v div 16;
      if j <= 9 then
        hexbuf[i] := chr(ord('0') + j)
      else hexbuf[i] := chr(ord('A') + j - 10);
      end; { for i }
    write(MacFile, hexbuf);
    column := column + 4;
  end; {WriteHexLong}


procedure reposition(col: columnrange);

  begin
    if (column >= col) {we're at or past the desired column}
       then writech(' ');  { emit one space at least }

    while column < col do
      writech(' ');
  end {reposition};


procedure writeline;

  begin
    writeln(MacFile);
    column := 1;
  end {writeline};


function uppercase(ch: char): char;

  begin
    if (ch >= 'a') and (ch <= 'z') then
      uppercase := chr(ord(ch) - ord('a') + ord('A'))
    else uppercase := ch;
  end; {uppercase}


procedure allocfixup;

  begin
    if fixuphead = nil then begin { first time }
      new(fixuphead);
      fixuptail := fixuphead;
      end

    else begin { tack new node on end of list }
      new(fixuptail^.fixuplink);
      fixuptail := fixuptail^.fixuplink;
      end;  { tack new node }

    with fixuptail^ do begin
      fixuplink := nil;  { new end of list }
      fixupaddr := undefinedaddr;
      fixupobjpc := currentpc;  { this data is only required by the test
                                  dumper }
      end;
  end;  { allocfixup }


  procedure absfixup(var ref: fixupptr; len: integer);

{ Generate a fixup record for an absolute fixup.  This is expected to
  be called just before the word is generated, and "ref" will be used
  to provide the absolute value at some later time
}

  begin
    allocfixup;
    ref := fixuptail;
    with ref^ do
      begin
      fixupkind := fixupabs;
      fixupfileloc := tempfilesize + nexttempbuffer + 4;
      fixupobjpc := currentpc;
      fixuplen := len;
      end;
  end; {absfixup}


  procedure insertnewESD;

{ The global variable "newESD" is to be inserted into the ESDtable.
  Additionally, we must check for table overflow (abort if so).
}
    begin
      if nextESD = lastESD then compilerabort(manyexterns);

      ESDtable[nextESD] := newESD;  { that's easy enough }
      nextESD := nextESD + 1;   { may actually be beyond linker's range }
    end;  { insertnewESD }


procedure findESDid;

{ Compute the ESDid for the given entry.  If not found, insert it.

  Note that the ESDid is not just the index into the ESDtable because
  XDEF's and "standard load sections" (for instance) are not assigned ESDid's.
}
  var
    ESD: ESDrange;

  begin
    ESD := firstESD;
    ESDid := firstESD;
    found := false;

    while (ESD < nextESD) and not found do begin

      with ESDtable[ESD] do
        if ESDkind = ESDsupport then

          if (newESD.ESDkind = ESDsupport) and
             (newESD.suppno = suppno) then
            found := true
          else ESDid := ESDid + 1

        else if ESDkind = ESDexternal then

          if (newESD.ESDkind = ESDexternal) and
             (newESD.exproc = exproc) then
            found := true
          else ESDid := ESDid + 1

        else if ESDkind = ESDdefine then
          begin
          { ESDdefine is odd because there is no ESDid assigned, but the check
            is here so xdefs are not added to the table more than once.
          }
          if (newESD.ESDkind = ESDdefine) and
             (newESD.vartabindex = vartabindex) then
            found := true
          end

        else if ESDkind in [ESDuse, ESDshrvar] then

          if (newESD.ESDkind = ESDkind) and
             (newESD.vartabindex = vartabindex) then
            found := true
          else ESDid := ESDid + 1

        else if ESDkind in [ESDcommon, ESDdiag] then
          if newESD.ESDkind = ESDkind then found := true
          else ESDid := ESDid + 1;

      ESD := ESD + 1;
      end; {while}

    if not found then
      insertnewESD;   { nextESD bumps, but not ESDid }
  end; { findESDid }

procedure putrelfile(data: unsigned);

  begin
    if nextrelfile = maxrelfile then begin
      put(relfile);
      nextrelfile := 0;
      end
    else nextrelfile := nextrelfile + 1;
    relfile^[nextrelfile] := data;
  end; {puttemp}


procedure flushtempbuffer;

{ Write the current contents of the tempbuffer to the temporary object
  file.
}
  var
    i : 0..31;  { induction var for buffer copying }
    packbits: unsigned;  { pseudo packed array of boolean }


  begin
    if nexttempbuffer > 0 then
      begin
      putrelfile((sectionno[currentsect] + 1) * 256 +
                     nexttempbuffer);
      for i := nexttemprelocn to 31 do temprelocn[i] := false;
      packbits := 0;
      for i := 0 to 15 do begin
        packbits := packbits * 2;
        if temprelocn[i] then
          packbits := packbits + 1;
        end;
      putrelfile(packbits);

      packbits := 0;
      for i := 16 to 31 do begin
        packbits := packbits * 2;
        if temprelocn[i] then
          packbits := packbits + 1;
        end;
      putrelfile(packbits);

      for i := 0 to nexttempbuffer - 1 do
        putrelfile(tempbuffer[i]);

      tempfilesize := tempfilesize + nexttempbuffer + 3;
      nexttempbuffer := 0;
      nexttemprelocn:= 0;
      end;
  end; {flushtempbuffer}


procedure putdata(data: unsigned);

  begin
    tempbuffer[nexttempbuffer] := data and 65535;
    nexttempbuffer := nexttempbuffer + 1;
    if nexttempbuffer >= maxtempbuffer then flushtempbuffer;
  end;


procedure putbuffer(data: unsigned; reloc: boolean);

  begin
    temprelocn[nexttemprelocn] := reloc;
    nexttemprelocn := nexttemprelocn + 1;
    putdata(data);
  end;  {putbuffer}


procedure newsection(newsect: section {section to switch to} );

{ Change sections to "newsect".  If there is any data accumulated for the
  object file, that buffer is written.
}

  begin
    if newsect <> currentsect then
      begin
      if switcheverplus[outputmacro] then
        begin
        reposition(opcolumn);
        writestr('SECTION');
        if newsect = codesect then writestr(sectiontail);
        reposition(opndcolumn);
        writeint(sectionno[newsect]);
        writeline;
        end;
      if switcheverplus[outputobj] then flushtempbuffer;
      sectionpc[currentsect] := currentpc;
      currentsect := newsect;
      currentpc := sectionpc[currentsect];
      end;
  end; {newsection}


procedure seekstringfile(n: integer {byte to access});

{ Do the equivalent of a "seek" on the string file.  This sets the
  file and "nextstringfile" to access byte "n" of the stringfile.
}

  var
    newblock: 1..maxstringblks; { block to which seeking }


  begin {seekstringfile}
    newblock := n div (diskbufsize + 1) + 1;
    if newblock <> curstringblock then
      begin
      if needcaching then
        begin
        seek(stringfile, newblock);
        curstringblock := newblock;
        end
      else
        begin
        curstringblock := newblock;
        stringblkptr := stringblkptrtbl[newblock];
        if stringblkptr = nil then
          begin
          write('unexpected end of stringtable ');
          compilerabort(inconsistent);
          end;
        end;
      end;
    nextstringfile := n mod (diskbufsize + 1);
  end {seekstringfile} ;


function getstringfile: hostfilebyte;

{ move stringfile buffer pointer to next entry.  'get' is called
  to fill buffer if pointer is at the end.
}

  begin
    if nextstringfile > diskbufsize then
      begin
      nextstringfile := 0;
      if needcaching then
        begin
        get(stringfile);
        curstringblock := curstringblock + 1;
        end
      else
        begin
        curstringblock := curstringblock + 1;
        stringblkptr := stringblkptrtbl[curstringblock];
        if stringblkptr = nil then
          begin
          write('unexpected end of stringtable ');
          compilerabort(inconsistent);
          end;
        end;
      end;

    if needcaching then getstringfile := stringfile^[nextstringfile]
    else getstringfile := stringblkptr^[nextstringfile];

    nextstringfile := nextstringfile + 1;
  end {getstringfile} ;


procedure writeprocname(procn: proctableindex; {number of procedure to copy}
                        len: integer {characters to write} );

{ Copy the procedure name for procedure "procn" from the string file
  to the macro file.
}

  var
    i: integer; {induction var for copy}

  begin
    if needcaching then
      seekstringfile(stringfilecount + proctable[procn].charindex - 1)
    else
      begin
      curstringblock := (stringfilecount + proctable[procn].charindex - 1) div
         (diskbufsize + 1) + 1;
      stringblkptr := stringblkptrtbl[curstringblock];
      nextstringfile := (stringfilecount + proctable[procn].charindex - 1) mod
                        (diskbufsize + 1);
      end;
    for i := 1 to min(len, proctable[procn].charlen) do
      if language = pascal then writech(uppercase(chr(getstringfile)))
      else writech(chr(getstringfile));
  end {writeprocname} ;



{ Diagnostic table generation.

  The diagnostic tables are generated to allow a walkback by line and
  procedure in case of error.  They are generated in section 14, so the
  diagnostics for each compilation unit are concatenated.  A reference to
  a common section in section 14 is generated to indicate the end of
  the tables.

  The syntax of the tables, as currently generated, is

  diagtables = endpointer       (distance to end of tables)
               startpc          (start of code for this unit)
               codelength       (length of code for this unit)
               [* procedure *]
               startpointer     (distance to start of tables)

  The tables are bitwise packed and highly encoded.  The format for various
  pieces is given in the individual routines generating them.
}


procedure diag_word(value: unsigned {value to put} );

{ Put a single word of diagnostic data to the macro and object files.
  At the moment it is formatted with one value per line.  This is
  subject to change to make the file look prettier.
}


  begin
    newsection(diagsect);
    if switcheverplus[outputmacro] then
      begin
      reposition(opcolumn);
      writestr('DC.W');
      reposition(opndcolumn);
      writech('$');
      writehex(value);
      writeline;
      end;

    if switcheverplus[outputobj] then
      putbuffer(value, false);
    currentpc := currentpc + 2;
  end; {diag_word}


procedure diag_bits(value: unsigned; {value to generate}
                    length: bits {number of bits in value} );

{ Put "length" bits of "value" to the diagnostic bit stream.
}

  var
    i: bits; {induction var}
    bit: array [bits] of 0..1; {each bit}


  begin
    for i := 0 to length - 1 do
      begin
      bit[i] := value mod 2;
      value := value div 2;
      end;

    for i := length - 1 downto 0 do
      begin
      diagbitbuffer := diagbitbuffer * 2 + bit[i];
      if nextdiagbit = 15 then
        begin
        diag_word(diagbitbuffer);
        diagbitbuffer := 0;
        nextdiagbit := 0;
        end
      else
        nextdiagbit := nextdiagbit + 1;
      end;
  end; {diag_bits}



procedure initdiags;

{ Initialize the diagnostic code, called only if diagnostics are
  going to be generated.
}

var kludge: integer; {to get around a purposeful range error}

  begin
    everdiagnosing := true;
    nextdiagbit := 0;
    diagbitbuffer := 0;

    if switcheverplus[outputmacro] then
      begin
      writeln(macfile, 'P_DIAG', 'SECTION': opcolumn + 6 - 6,
              ' ': opndcolumn - opcolumn - 7, sectionno[diagsect]:1);
      newsection(diagsect);
      writeln(macfile, 'STDIAG', 'DC.W': opcolumn + 3 - 6,
              'ENDIAG-STDIAG': opndcolumn - opcolumn - 4 + 13);
      writeln(macfile, 'DC.L': opcolumn + 3,
              'L': opndcolumn - opcolumn - 4 + 1);
      writeln(macfile, 'DC.L': opcolumn + 3, 'LAST-L':
              opndcolumn - opcolumn - 4 + 6);
      if switcheverplus[debugging] or switcheverplus[profiling] then
        begin
        writeln(macfile, 'DC.B': opcolumn+3, '''':opndcolumn - opcolumn - 4,
                outputname: 8, '''');
        write(macfile, 'DC.L':opcolumn + 3, ' ':opndcolumn - opcolumn - 4);
        if switcheverplus[own] and (ownsize > 0) then writeln(macfile, 'G')
        else writeln(macfile, '0');
        end;
      end;

    if switcheverplus[outputobj] then
      begin
      newsection(diagsect);
      absfixup(diaglenfix, word);
      putbuffer(0, false);
      putbuffer(50B * 256 + sectionno[codesect] + 1, true);
      currentpc := 6; {needed for absfixup diagnostic}
      absfixup(codelenfix, long);
      putbuffer(0, false);
      putbuffer(0, false);

      if switcheverplus[debugging] or switcheverplus[profiling] then
        begin
        putbuffer(ord(outputname[1]) * 256 + ord(outputname[2]), false);
        putbuffer(ord(outputname[3]) * 256 + ord(outputname[4]), false);
        putbuffer(ord(outputname[5]) * 256 + ord(outputname[6]), false);
        putbuffer(ord(outputname[7]) * 256 + ord(outputname[8]), false);

        if switcheverplus[own] and (ownsize > 0) then
          putbuffer(50B * 256, true)
        else
          begin
          putbuffer(0, false);
          putbuffer(0, false);
          end;
        end;
      end;

    if switcheverplus[debugging] or switcheverplus[profiling] then
      currentpc := 22
    else currentpc := 10; {length of header data}
  end; {initdiags}


  procedure WriteSymbolName(name: packed array[m..n: integer] of char);
  { Write a symbol name string to the macro file. }
  var i: 0..maxprocnamelen;
  begin
    i := 0;
    while (i < n) and (name[i + 1] <> ' ') do begin
      i := i + 1;
      if language = pascal then writech(uppercase(name[i]))
      else writech(name[i]);
      end;
  end;


procedure import_name(n: integer;
                      var name: packed array [lo..hi: integer] of char);

  { Return a Modula2 import name;
    name is prefixed with 'i_' and is blank-terminated (blank may be
    followed by garbage).
  }

  var
    i, t: 0..maxprocnamelen;


  begin
    if language = modula2 then
      begin
      seekstringfile(stringfilecount + proctable[n].charindex - 1);
      t := proctable[n].charlen + 2;
      if t > maxprocnamelen then t := maxprocnamelen;
      name[1] := 'i';
      name[2] := '_';

      for i := 3 to t do name[i] := chr(getstringfile);

      name[t + 1] := ' ';
      end;
  end; {import_name}
