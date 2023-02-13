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

unit putcode;

interface

uses config, product, hdr, hdrc, t_c, utils, commonc, sysutils;

procedure putcode;

procedure openc;

procedure closec;

procedure initmac;

procedure initdiags;

procedure fixdefines;

procedure fixmac;

implementation

const
  objreslen:objtypesx= (0, {objnorm}
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


procedure supname(libroutine: libroutines;
                  var s: string);

  begin {supname}
    case libroutine of
      lib68881init:     s := 'p_68881   ';
      libarctan:        s := 'p_fatn    ';
      libbreak:         s := 'p_break   ';
      libcap:           s := 'p_cap     ';  { for Modula-2}
      libcasetrap:      s := 'p_caserr  ';
      libcexit:         s := 'p_cexit   ';  { for C }
      libmexit:         s := 'p_mexit   ';  { for Modula-2 }
      libcidiv:         s := 'p_cidiv   ';  { for C }
      libcinit:         s := 'p_centry  ';  { for C }
      libminit:         s := 'p_minit   ';  { for Modula-2 }
      libclose:         s := 'p_close   ';
      libcloseinrange:  s := 'p_clsrng  ';
      libconnect:       s := 'p_connect ';
      libcopy:          s := 'p_copy    ';
      libcos:           s := 'p_fcos    ';
      libcvtdr:         s := 'p_cvtdf   ';
      libcvtrd:         s := 'p_cvtfd   ';
      libdadd:          s := 'p_dadd    ';
      libdarctan:       s := 'p_datn    ';
      libdbgtrap:       s := 'p_dbgtrp  ';
      libdcos:          s := 'p_dcos    ';
      libddiv:          s := 'p_ddiv    ';
      libdefinebuf:     s := 'p_define  ';
      libdelete:        s := 'p_delete  ';
      libdeql:          s := 'p_deql    ';
      libdexp:          s := 'p_dexp    ';
      libdfloat:        s := 'p_dfloat  ';
      libdufloat:       s := 'p_dufloat ';  { for C }
      libdfloat_uns:    s := 'p_dfltu   ';
      libdispose:       s := 'p_dispos  ';
      libdgtr:          s := 'p_dgtr    ';
      libdln:           s := 'p_dln     ';
      libdlss:          s := 'p_dlss    ';
      libdmult:         s := 'p_dmul    ';
      libdround:        s := 'p_dround  ';
      libdsin:          s := 'p_dsin    ';
      libdsqr:          s := 'p_dsqr    ';
      libdsqrt:         s := 'p_dsqrt   ';
      libdsub:          s := 'p_dsub    ';
      libdswap:         s := 'p_dswap   ';
      libdtime:         s := 'p_dtime   ';
      libdtrunc:        s := 'p_dtrunc  ';
      libdf:            s := 'p_libdf   ';  { for C }
      libfd:            s := 'p_libfd   ';  { for C }
      libexit:          s := 'p_exit    ';
      libexp:           s := 'p_fexp    ';
      libfadd:          s := 'p_fadd    ';
      libfcmp:          s := 'p_fcmp    ';
      libfiletrap:      s := 'p_filerr  ';
      libfdiv:          s := 'p_fdiv    ';
      libffloat:        s := 'p_ffloat  ';
      libfufloat:       s := 'p_fufloat ';  { for C }
      libffloat_uns:    s := 'p_ffltu   ';
      libfmult:         s := 'p_fmul    ';
      libfree:          s := 'p_free    ';
      libfround:        s := 'p_fround  ';
      libfsqr:          s := 'p_fsqr    ';
      libfsub:          s := 'p_fsub    ';
      libftrunc:        s := 'p_ftrunc  ';
      libget:           s := 'p_get     ';
      libhalt:          s := 'p_halt    ';  { for Modula-2}
      libidiv:          s := 'p_idiv    ';
      libimult:         s := 'p_imul    ';
      libinitialize:    s := 'p_initio  ';
      libioerror:       s := 'p_ioerro  ';
      libiostatus:      s := 'p_iostat  ';
      libiotransfer:    s := 'p_iotrans ';  { for Modula-2}
      libln:            s := 'p_fln     ';
      libmcopy1:        s := 'm_copy1   ';  { for Modula-2}
      libmcopy2:        s := 'm_copy2   ';  { for Modula-2}
      libmcopy4:        s := 'm_copy4   ';  { for Modula-2}
      libmcopymd:       s := 'm_copymd  ';  { for Modula-2}
      libnew:           s := 'p_new     ';
      libnewprocess:    s := 'p_newprc  ';  { for Modula-2}
      libnoioerror:     s := 'p_noioer  ';
      libpack:          s := 'p_pack    ';
      libpage:          s := 'p_page    ';
      libpageo:         s := 'p_page_o  ';
      libpointertrap:   s := 'p_badptr  ';
      libpos:           s := 'p_pos     ';
      libprofilerdump:  s := 'p_prdump  ';
      libprofilerinit:  s := 'p_prinit  ';
      libprofilerstmt:  s := 'p_prstmt  ';
      libput:           s := 'p_put     ';
      librangetrap:     s := 'p_subrng  ';
      libreadchar:      s := 'p_rdc     ';
      libreadchari:     s := 'p_rdc_i   ';
      libreaddouble:    s := 'p_rdd     ';
      libreaddoublei:   s := 'p_rdd_i   ';
      libreadint:       s := 'p_rdi     ';
      libreadinti:      s := 'p_rdi_i   ';
      libreadln:        s := 'p_rdln    ';
      libreadlni:       s := 'p_rdln_i  ';
      libreadreal:      s := 'p_rdf     ';
      libreadreali:     s := 'p_rdf_i   ';
      libreadstring:    s := 'p_rds     ';
      libreadstringi:   s := 'p_rds_i   ';
      libreadxstring:   s := 'p_rdxs    ';
      libreadxstringi:  s := 'p_rdxs_i  ';
      librealloc:       s := 'p_realloc ';
      librename:        s := 'p_rename  ';
      libreset:         s := 'p_reset   ';
      librewrite:       s := 'p_rewrit  ';
      libscan:          s := 'p_scan    ';  { for Modula-2}
      libseek:          s := 'p_seek    ';
      libsin:           s := 'p_fsin    ';
      libsqrt:          s := 'p_fsqrt   ';
      libstrovr:        s := 'p_strovr  ';
      libsubscripttrap: s := 'p_subscr  ';
      libtell:          s := 'p_tell    ';
      libtime:          s := 'p_ftime   ';
      libtransfer:      s := 'p_trans   ';  { for Modula-2}
      libunpack:        s := 'p_unpack  ';
      libunsdiv:        s := 'p_udiv    ';
      libunsmod:        s := 'p_umod    ';
      libunsmult:       s := 'p_umul    ';
      libwritebool:     s := 'p_wtb     ';
      libwriteboolo:    s := 'p_wtb_o   ';
      libwritechar:     s := 'p_wtc     ';
      libwritecharo:    s := 'p_wtc_o   ';
      libwritedouble1:  s := 'p_wtd1    ';
      libwritedouble1o: s := 'p_wtd1_o  ';
      libwritedouble2:  s := 'p_wtd2    ';
      libwritedouble2o: s := 'p_wtd2_o  ';
      libwriteint:      s := 'p_wti     ';
      libwriteinto:     s := 'p_wti_o   ';
      libwriteln:       s := 'p_wtln    ';
      libwritelno:      s := 'p_wtln_o  ';
      libwritereal1:    s := 'p_wtf1    ';
      libwritereal1o:   s := 'p_wtf1_o  ';
      libwritereal2:    s := 'p_wtf2    ';
      libwritereal2o:   s := 'p_wtf2_o  ';
      libwritestring:   s := 'p_wts     ';
      libwritestringo:  s := 'p_wts_o   ';
      libdebugger_goto: s := 'p_dbggto  ';
      libdebugger_init: s := 'p_dbgint  ';
      libdebugger_entry:s := 'p_dbgent  ';
      libdebugger_exit: s := 'p_dbgext  ';
      libdebugger_step: s := 'p_dbstmt  ';
      libown:           s := 'p_own     ';
      libstrint0:       s := 'p_stri0   ';
      libstrint1:       s := 'p_stri1   ';
      libvalint:        s := 'p_vali    ';
      libstrreal0:      s := 'p_strf0   ';
      libstrreal1:      s := 'p_strf1   ';
      libstrreal2:      s := 'p_strf2   ';
      libvalreal:       s := 'p_valf    ';
      libstrdouble0:    s := 'p_strd0   ';
      libstrdouble1:    s := 'p_strd1   ';
      libstrdouble2:    s := 'p_strd2   ';
      libvaldouble:     s := 'p_vald    ';
      libinsert:        s := 'p_ins     ';
      libdeletestr:     s := 'p_delstr  ';
      otherwise
        begin
        write('Unexpected library name (', ord(libroutine):1, ')');
        compilerabort(inconsistent);
        end;
      end;

  end {supname} ;

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


procedure writestr(s: string);

  var i, j: integer;

  begin
    write(MacFile, s);
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
    write(relfile, data);
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
      curstringblock := curstringblock + 1;
      stringblkptr := stringblkptrtbl[curstringblock];
      if stringblkptr = nil then
        begin
        write('unexpected end of stringtable ');
        compilerabort(inconsistent);
        end;
      end;

    getstringfile := stringblkptr^[nextstringfile];

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

    curstringblock := (stringfilecount + proctable[procn].charindex - 1) div
       (diskbufsize + 1) + 1;
    stringblkptr := stringblkptrtbl[curstringblock];
    nextstringfile := (stringfilecount + proctable[procn].charindex - 1) mod
                        (diskbufsize + 1);

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
      putbuffer(&50 * 256 + sectionno[codesect] + 1, true);
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
          putbuffer(&50 * 256, true)
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


  procedure WriteSymbolName(name: string);
  { Write a symbol name string to the macro file. }
  var i: 0..maxprocnamelen;
  begin
    i := 0;
    while (i < length(name)) do begin
      i := i + 1;
      if language = pascal then writech(uppercase(name[i]))
      else writech(name[i]);
      end;
  end;


procedure import_name(n: integer;
                      var name: string);

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


procedure FixMac;

{ Clean up the macro file.  There isn't much to do for this file.
}

  var
    i: ESDrange;
    j: integer;
    k: 1..linknamesize;
    suppcall: libroutines;
    support: packed array [libroutines] of boolean;
    s: string;
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

    {
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
	}

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
      compilerabort(inconsistent);
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
      if bigcompilerversion then n := @(bignodetable[currnode]);
      end
  end; {getnextnode}


procedure lookahead(n: integer);

{ Return with "p" pointing "n" nodes away.  Similar to "getnextnode".
}


  begin {lookahead}
    if currnode + n > lastnode then puterror(endofnodes)
    else if bigcompilerversion then p := @(bignodetable[currnode + n]);
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
    if bigcompilerversion then n := @(bignodetable[currnode - num]);

    if n^.kind <> oprndnode then
      begin
      puterror(missingoperand);
      currinst := nop; { to facilitate recovery }
      end { if }
  end; { getprevoperand }

procedure writeobjline;

{ DRB object code hacked out }

begin
  if switcheverplus[outputmacro] then writeline;
end;

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
        end;

      facos:
        begin
        mnemonic := 'facos';
        end;

      fadd:
        begin
        mnemonic := 'fadd';
        end;

      fasin:
        begin
        mnemonic := 'fasin';
        end;

      fatan:
        begin
        mnemonic := 'fatan';
        end;

      fatanh:
        begin
        mnemonic := 'fatanh';
        end;

      fbeq:
        begin
        mnemonic := 'fbeq';
        end;

      fbne:
        begin
        mnemonic := 'fbne';
        end;

      fbgt:
        begin
        if aware then
          begin
          mnemonic := 'fbogt';
          end
        else
          begin
          mnemonic := 'fbgt';
          end;
        end;

      fbngt:
        begin
        if aware then
          begin
          mnemonic := 'fbule';
          end
        else
          begin
          mnemonic := 'fbngt';
          end;
        end;

      fbge:
        begin
        if aware then
          begin
          mnemonic := 'fboge';
          end
        else
          begin
          mnemonic := 'fbge';
          end;
        end;

      fbnge:
        begin
        if aware then
          begin
          mnemonic := 'fbult';
          end
        else
          begin
          mnemonic := 'fbnge';
          end;
        end;

      fblt:
        begin
        if aware then
          begin
          mnemonic := 'fbolt';
          end
        else
          begin
          mnemonic := 'fblt';
          end;
        end;

      fbnlt:
        begin
        if aware then
          begin
          mnemonic := 'fbuge';
          end
        else
          begin
          mnemonic := 'fbnlt';
          end;
        end;

      fble:
        begin
        if aware then
          begin
          mnemonic := 'fbole';
          end
        else
          begin
          mnemonic := 'fble';
          end;
        end;

      fbnle:
        begin
        if aware then
          begin
          mnemonic := 'fbugt';
          end
        else
          begin
          mnemonic := 'fbnle';
          end;
        end;

      fbgl:
        begin
        if aware then
          begin
          mnemonic := 'fbogl';
          end
        else
          begin
          mnemonic := 'fbgl';
          end;
        end;

      fbngl:
        begin
        if aware then
          begin
          mnemonic := 'fbueq';
          end
        else
          begin
          mnemonic := 'fbngl';
          end;
        end;

      fbgle:
        begin
        if aware then
          begin
          mnemonic := 'fbor';
          end
        else
          begin
          mnemonic := 'fbgle';
          end;
        end;

      fbngle:
        begin
        if aware then
          begin
          mnemonic := 'fbun';
          end
        else
          begin
          mnemonic := 'fbngle';
          end;
        end;

      fcmp:
        begin
        mnemonic := 'fcmp';
        end;

      fcos:
        begin
        mnemonic := 'fcos';
        end;

      fcosh:
        begin
        mnemonic := 'fcosh';
        end;

      fdiv:
        begin
        mnemonic := 'fdiv';
        end;

      fetox:
        begin
        mnemonic := 'fetox';
        end;

      fetoxm1:
        begin
        mnemonic := 'fetoxm1';
        end;

      fgetexp:
        begin
        mnemonic := 'fgetexp';
        end;

      fgetman:
        begin
        mnemonic := 'fgetman';
        end;

      fint:
        begin
        mnemonic := 'fint';
        end;

      fintrz:
        begin
        mnemonic := 'fintrz';
        end;

      flog10:
        begin
        mnemonic := 'flog10';
        end;

      flog2:
        begin
        mnemonic := 'flog2';
        end;

      flogn:
        begin
        mnemonic := 'flogn';
        end;

      flognp1:
        begin
        mnemonic := 'flognp1';
        end;

      fmod:
        begin
        mnemonic := 'fmod';
        end;

      fmove:
        begin
        mnemonic := 'fmove';
        end;

      fmovecr:
        begin
        mnemonic := 'fmovecr';
        end;

      fmove_to_fpcr, fmove_from_fpcr:
        begin
        mnemonic := 'fmove';
        end;

      fmovem:
        begin
        mnemonic := 'fmovem';
        end;

      fmul:
        begin
        mnemonic := 'fmul';
        end;

      fneg:
        begin
        mnemonic := 'fneg';
        end;

      fnop:
        begin
        mnemonic := 'fnop';
        end;

      frem:
        begin
        mnemonic := 'frem';
        end;

      fscale:
        begin
        mnemonic := 'fscale';
        end;

      fsgldiv:
        begin
        mnemonic := 'fsgldiv';
        end;

      fsglmul:
        begin
        mnemonic := 'fsglmul';
        end;

      fsin:
        begin
        mnemonic := 'fsin';
        end;

      fsincos:
        begin
        mnemonic := 'fsincos';
        end;

      fsinh:
        begin
        mnemonic := 'fsinh';
        end;

      fsqrt:
        begin
        mnemonic := 'fsqrt';
        end;

      fsub:
        begin
        mnemonic := 'fsub';
        end;

      ftan:
        begin
        mnemonic := 'ftan';
        end;

      ftanh:
        begin
        mnemonic := 'ftanh';
        end;

      ftentox:
        begin
        mnemonic := 'ftentox';
        end;

      ftrap: { NYI }
        begin
        mnemonic := 'ftrap';
        end;

      ftst:
        begin
        mnemonic := 'ftest';
        end;

      ftwotox:
        begin
        mnemonic := 'ftwotox';
        end;

        { 68000 and 68020 instructions
        }
      add:
        begin
        mnemonic := 'add';
        end;

      adda:
        begin
        mnemonic := 'ADDA';
        end;

      addi:
        begin
        mnemonic := 'ADDI';
        end;

      addq:
        begin
        mnemonic := 'ADDQ';
        end;

      addx:
        begin
        mnemonic := 'addx';
        end;

      andi:
        begin
        mnemonic := 'ANDI';
        end;

      andinst:
        begin
        mnemonic := 'and';
        end;

      asl:
        begin
        mnemonic := 'asl';
        end;

      asr:
        begin
        mnemonic := 'asr';
        end;

      beq:
        begin
        mnemonic := 'beq';
        end;

      bge:
        begin
        mnemonic := 'bge';
        end;

      bgt:
        begin
        mnemonic := 'bgt';
        end;

      bhi:
        begin
        mnemonic := 'bhi';
        end;

      ble:
        begin
        mnemonic := 'ble';
        end;

      bls:
        begin
        mnemonic := 'bls';
        end;

      blt:
        begin
        mnemonic := 'blt';
        end;

      bmi:
        begin
        mnemonic := 'bmi';
        end;

      bpl:
        begin
        mnemonic := 'bpl';
        end;

      bne:
        begin
        mnemonic := 'bne';
        end;

      blo:
        begin
        mnemonic := 'bcs';
        end;

      bhs:
        begin
        mnemonic := 'bcc';
        end;

      bvc:
        begin
        mnemonic := 'bvc';
        end;

      bvs:
        begin
        mnemonic := 'bvs';
        end;

      bchg:
        begin
        mnemonic := 'bchg';
        end;

      bclr:
        begin
        mnemonic := 'bclr';
        end;

      bfclr:
        begin
        mnemonic := 'bfclr';
        end;

      bfexts:
        begin
        mnemonic := 'bfexts';
        end;

      bfextu:
        begin
        mnemonic := 'bfextu';
        end;

      bfins:
        begin
        mnemonic := 'bfins';
        end;

      bfset:
        begin
        mnemonic := 'bfset';
        end;

      bftst:
        begin
        mnemonic := 'bftst';
        end;

      bra:
        begin
        mnemonic := 'bra';
        end;

      bset:
        begin
        mnemonic := 'bset';
        end;

      bsr:
        begin
        mnemonic := 'bsr';
        end;

      btst:
        begin
        mnemonic := 'btst';
        end;

      chk:
        begin
        mnemonic := 'chk';
        end;

      clr:
        begin
        mnemonic := 'clr';
        end;

      cmp:
        begin
        mnemonic := 'cmp';
        end;

      cmpa:
        begin
        mnemonic := 'CMPA';
        end;

      cmpi:
        begin
        mnemonic := 'CMPI';
        end;

      cmpm:
        begin
        mnemonic := 'cmpm';
        end;

      dbra:
        begin
        mnemonic := 'dbra';
        end;

      dbeq:
        begin
        mnemonic := 'dbeq';
        end;

      dbge:
        begin
        mnemonic := 'dbge';
        end;

      dbgt:
        begin
        mnemonic := 'dbgt';
        end;

      dbhi:
        begin
        mnemonic := 'dbhi';
        end;

      dbhs:
        begin
        mnemonic := 'dbcc';
        end;

      dble:
        begin
        mnemonic := 'dble';
        end;

      dbls:
        begin
        mnemonic := 'dbls';
        end;

      dblt:
        begin
        mnemonic := 'dblt';
        end;

      dblo:
        begin
        mnemonic := 'dbcs';
        end;

      dbmi:
        begin
        mnemonic := 'dbmi';
        end;

      dbpl:
        begin
        mnemonic := 'dbpl';
        end;

      dbne:
        begin
        mnemonic := 'dbne';
        end;

      dbvc:
        begin
        mnemonic := 'dbvc';
        end;

      dbvs:
        begin
        mnemonic := 'dbvs';
        end;

      divs:
        begin
        mnemonic := 'divs';
        end;

      divu:
        begin
        mnemonic := 'divu';
        end;

      divsl:
        begin
        mnemonic := 'tdivsl';
        end;

      divul:
        begin
        mnemonic := 'tdivul';
        end;

      eor:
        begin
        mnemonic := 'eor';
        end;

      eori:
        begin
        mnemonic := 'EORI';
        end;

      exg:
        begin
        mnemonic := 'exg';
        end;

      ext:
        begin
        mnemonic := 'ext';
        end;

      extb:
        begin
        mnemonic := 'extb';
        end;

      jmp:
        begin
        mnemonic := 'jmp';
        end;

      jsr:
        begin
        mnemonic := 'jsr';
        end;

      lea:
        begin
        mnemonic := 'lea';
        end;

      link:
        begin
        mnemonic := 'link';
        end;

      lsl:
        begin
        mnemonic := 'lsl';
        end;

      lsr:
        begin
        mnemonic := 'lsr';
        end;

      movea, move:
        begin
        mnemonic := 'MOVE'; { postcorrect adds 'A' if movea }
        end;

      move_to_ccr:
        begin
        mnemonic := 'move';
        end;

      movem:
        begin
        mnemonic := 'MOVE'; { postcorrect adds 'M' }
        end;

      moveq:
        begin
        mnemonic := 'MOVE'; { postcorrect adds 'Q' }
        end;

      muls:
        begin
        if mc68020 and (datasize = long) then
          begin
          mnemonic := 'tmulsl';
          end
        else
          begin
          mnemonic := 'muls';
          end;
        end;

      mulu:
        begin
        if mc68020 and (datasize = long) then
          begin
          mnemonic := 'tmulul';
          end
        else
          begin
          mnemonic := 'mulu';
          end;
        end;

      neg:
        begin
        mnemonic := 'neg';
        end;

      negx:
        begin
        mnemonic := 'negx';
        end;

      notinst:
        begin
        mnemonic := 'not';
        end;

      ori:
        begin
        mnemonic := 'ORI';
        end;

      orinst:
        begin
        mnemonic := 'or';
        end;

      pea:
        begin
        mnemonic := 'pea';
        end;

      rol:
        begin
        mnemonic := 'rol';
        end;

      ror:
        begin
        mnemonic := 'ror';
        end;

      roxl:
        begin
        mnemonic := 'roxl';
        end;

      roxr:
        begin
        mnemonic := 'roxr';
        end;

      rte:
        begin
        mnemonic := 'rte';
        end;

      rts:
        begin
        mnemonic := 'rts';
        end;

      sub:
        begin
        mnemonic := 'sub';
        end;

      suba:
        begin
        mnemonic := 'SUBA';
        end;

      subi:
        begin
        mnemonic := 'SUBI';
        end;

      subq:
        begin
        mnemonic := 'SUBQ';
        end;

      subx:
        begin
        mnemonic := 'subx';
        end;

      swap:
        begin
        mnemonic := 'swap';
        end;

      trap:
        begin
        mnemonic := 'trap';
        end;

      trapcc:
        begin
          { Thus far we only need the TRAPLS form of this instruction (for
            Versados).  The Versados assembler wants the mnemonic to be TLS
            instead of TRAPLS.
          }
        mnemonic := 'tls';
        end;

      trapv:
        begin
        mnemonic := 'trapv';
        end;

      tst:
        begin
        mnemonic := 'tst';
        end;

      unlk:
        begin
        mnemonic := 'unlk';
        end;

      end; {case inst}

    if inst in [fp_first..fp_last] then
      fp_src_spec := integer(n^.fp_format);

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
    s: string;

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
                if pic_enabled and (extvaralloc = sharedalloc) then
                  begin
                  writech('#');
                  WriteSymbolName(get_from_sfile(charindex, charlen, true));
                  writech('-');
                  supname(libroutines(libown), s);
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
            if (n^.operandcost < long) or pic_enabled then writestr('(PC)');
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

            supname(libroutines(offset), s);

            if pic_enabled then
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
            if pic_enabled then
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
            supname(libroutines(libown), s);
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
            supname(libroutines(offset), s);
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
      if bigcompilerversion then p := @(bignodetable[tempnode]);
    until p^.kind = instnode;

    bytecount := - 2; { the opcode is at (PC-2) regardless of length! }

    if instcount < 0 then { backward scan }
      for i := - 2 downto instcount do
        begin
        repeat { find previous instruction node }
          tempnode := tempnode - 1;
          if bigcompilerversion then p := @(bignodetable[tempnode]);
        until p^.kind = instnode;

        bytecount := bytecount - p^.computed_length {instlength(tempnode)}
        end

    else { instcount > 0 } { forward scan }
      for i := 0 to instcount do
        begin
        bytecount := bytecount + p^.computed_length {instlength(tempnode)} ;

        repeat { find next instruction node }
          tempnode := tempnode + 1;
          if bigcompilerversion then p := @(bignodetable[tempnode]);
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
      end {labelnode}

    else if n^.kind = relnode then
      begin { relnodes are short, unlabeled offsets }
      distancetemp := computedistance;

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
      end {labelnode}
    else if n^.kind = relnode then
      begin { relnodes are short, unlabeled branches. }
      distancetemp := computedistance;
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
    writeopnd;
    getnextnode;

    if n^.kind = labelnode then
      begin { process the label }
      if switcheverplus[outputmacro] then
        begin
        writech('L');
        writeint(n^.labelno);
        end;
      end
    else if n^.kind = relnode then
      begin
      distancetemp := computedistance;

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

      getoperand;
      writelastopnd;
      end
    else if n^.oprnd.m = autoi then
      begin { restore registers }
      writeopnd;
      getoperand;

      datasize := word; { mask is only 16 bits long }

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

          if memory then
            begin
            getprevoperand(1);
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
          if memory then
            begin
            getprevoperand(1);
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
            end;
          end;

        fmove:
          begin
          memory := n^.oprnd.m <> fpreg;
          writeopnd;
          register := n^.oprnd.reg;
          getoperand;
          writelastopnd;
          end;

        fnop:
          begin
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
          end;

        fmove_to_fpcr:

          { Move to system control register.
          }
          begin
          writeopnd;
          getoperand;
          output_fp_creg;
          end;

        fmovecr:

          { Move from 68881 constant rom.
          }
          begin
          offset1 := n^.oprnd.offset;
          writeopnd;
          getoperand;
          writelastopnd;
          end;

        fmovem: buildmovem(true);

          { 68000 and 68020 instructions
          }

        movea, move:
          begin
          writeopnd;
          getoperand;
          writelastopnd;
          if currinst = movea then
            if datasize = byte then puterror(badsize)
            else if n^.oprnd.m <> areg then puterror(missingAreg);
          end;

        move_to_ccr:
          begin
          writeopnd;
          if switcheverplus[outputmacro] then writestr('CCR');
          end;

        moveq:
          begin
          if not (n^.oprnd.m in [immediatelong, immediate]) or
             (n^.oprnd.offset > 127) or (n^.oprnd.offset < - 128) then
            puterror(badoperand);
          datasize := byte; { just in case }
          writeopnd;
          getoperand;
          writelastopnd;
          if n^.oprnd.m <> dreg then puterror(badoperand);
          end;

        add, cmp, sub, andinst, orinst:
          begin

            begin
            writeopnd;

            lookahead(1); { Check destination for d-reg first! If it is, then
                           emit "<Dn> op <EA> --> <Dn>" form only }
            if (n^.oprnd.m = dreg) and (p^.oprnd.m <> dreg) then
              begin
              if currinst = cmp then puterror(badsource); {cmp is one-way}
              getoperand;
              end
            else
              begin { must be "<EA> to Dn" form }
              getoperand;
              if n^.oprnd.m <> dreg then puterror(missingDreg);
              end;
            writelastopnd;
            end;
          end;

        addq, subq:
          begin
          if (n^.oprnd.m <> immediate) or (n^.oprnd.offset < 1) or
             (n^.oprnd.offset > 8) then
            puterror(badoperand);
          datasize := byte;
          writeopnd;
          getoperand;
          writelastopnd;
          end;

        adda, cmpa, suba: { address register destination }
          begin
            begin
            writeopnd;
            getoperand;
            writelastopnd;
            if n^.oprnd.m <> areg then puterror(missingAreg);
            end;
          end;

        addi, cmpi, subi, andi, eori, ori: { immediate source }
          begin
          if (n^.oprnd.m <> immediate) and (n^.oprnd.m <> immediatelong) then
            puterror(badoperand);
            begin
            writeopnd;
            getoperand;
            writelastopnd;
            end;
          end;

        eor: { exclusive or -- differs from other logicals }
          begin
          if n^.oprnd.m <> dreg then puterror(missingDreg);
          writeopnd;
          getoperand;
          writelastopnd;
          if n^.oprnd.m = areg then puterror(badoperand);
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
              datasize := word;
              writeopnd;
              getoperand;
              end { immediate/register }

            else
              begin { immediate/memory -- enforce shift count = 1 }
              if n^.oprnd.offset <> 1 then puterror(badsource)
              else
              if datasize <> word then puterror(badsize);
              getoperand; { do not write out the shift count! }
              end; { immediate/memory }
            end { immediate (or implied) shift count form }

          else { register/register form -- instruction needs correction }
            begin
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            writeopnd;
            getoperand;
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            end;

          writelastopnd;
          end;

        bchg, bclr, bset, btst: { bit manipulation group }
          begin
          if (n^.oprnd.m <> dreg) and (n^.oprnd.m <> immediate) then
            puterror(badsource);
          writeopnd;
          getoperand;
          writelastopnd;
          end;

        bfclr, bfset, bftst:
          begin
          writelastopnd; { The effective address }
          getoperand;

          if n^.oprnd.m = bit_field_const then
            begin
              { "Len" is the length in bits, "offset1" is the offset in bits.
              }
            writebitfield( - 1, n^.oprnd.offset1, datasize);
            end
          else
            begin
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            writebitfield(n^.oprnd.reg, 0, datasize);
            end;

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
            writebitfield( - 1, offset1, datasize);
            end
          else
            begin
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            register := n^.oprnd.reg;
            getoperand;
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            writebitfield(register, 0, datasize);
            end;

          if switcheverplus[outputmacro] then writech(',');
          writelastopnd; { The register }
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
            writebitfield( - 1, n^.oprnd.offset1, datasize);
            end
          else
            begin
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            writebitfield(n^.oprnd.reg, 0, datasize);
            end;
          end;

        chk:
          begin
          writeopnd;
          getoperand;
          if n^.oprnd.m <> dreg then puterror(missingDreg);
          writelastopnd;
          end;

        clr, neg, negx, notinst, tst:
          begin
          writelastopnd;
          end;

        cmpm:
          begin
          if n^.oprnd.m <> autoi then puterror(badsource);
            begin
            writeopnd;
            getoperand;
            writelastopnd;
            if n^.oprnd.m <> autoi then puterror(badoperand);
            end;
          end;

        divs, divu:
          begin
          if datasize <> word then puterror(badsize);
          writeopnd;
          getoperand;
          writelastopnd;
          if n^.oprnd.m <> dreg then puterror(missingDreg);
          end;

        divsl, divul:
          begin
          if datasize <> long then puterror(badsize);
          writeopnd;
          getoperand;
          writelastopnd;
          end;
        exg:
          begin
{**note: genblk fix
            if datasize <> long then puterror(badsize);
}
          writeopnd;
          if n^.oprnd.m = dreg then
            begin
            getoperand;
            end
          else
            begin
            if n^.oprnd.m <> areg then puterror(badsource);
            getoperand;
            end;
          writelastopnd;
          end;

        ext, extb:
          begin
          if n^.oprnd.m <> dreg then puterror(missingDreg);
          if datasize = byte then puterror(badsize);

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
            end
          else {must be a labelnode}
            begin
            if switcheverplus[outputmacro] then
              begin
              writech('L');
              writeint(n^.labelno);
              end;
	    end;
          end;

        lea:
          begin
          n1 := nil;
          if n^.kind = oprndnode then
            begin
            if n^.oprnd.m = usercall then {caused by stuffregisters} n1 := n;
            if n^.oprnd.m in
               [areg, dreg, autoi, autod, immediate, immediatelong] then
              puterror(badoperand);
            writeopnd;
            end
          else
            begin {must be relnode, used only for initial call}
            distancetemp := computedistance;

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
          end;

        link:
          begin
          if n^.oprnd.m <> areg then puterror(missingAreg);
          writeopnd;
          getoperand;

          if not mc68020 then datasize := word; {size operand is only 16 bits
                                                  long}

          writelastopnd; { 68020 long is written here }
          if n^.oprnd.m <> immediate then puterror(baddestination)
          else if n^.oprnd.offset > 0 then puterror(badoffset);
          end;

        movem: buildmovem(false);

        muls, mulu:
          begin
          if mc68020 and (datasize = long) then
            begin
            writeopnd;
            getoperand;
            writelastopnd;
            end
          else if datasize = word then
            begin
            writeopnd;
            getoperand;
            writelastopnd;
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            end
          else puterror(badsize);
          end;

        pea:
          begin {* * * add control mode only checks * * *}
{**note: genblk fix
            if datasize <> long then puterror(badsize);
}
          writelastopnd;
          end;

        swap:
          begin
          if n^.oprnd.m <> dreg then puterror(badoperand);
          writelastopnd;
          end;

        rte, rts, trapcc, trapv:
        { remember, we did no "getoperand" for these guys } ;

        trap:
          begin
          if (n^.oprnd.m <> immediate) or (n^.oprnd.offset < 0) or
             (n^.oprnd.offset > 15) then
            puterror(badoperand);
          writelastopnd;
          end;

        unlk:
          begin
          if n^.oprnd.m <> areg then puterror(missingAreg);
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

              end
            else if kind = datanode then { insert constant data for 68881 }
              begin
              putdata(data div $10000);
              putdata(data mod $10000);

              if switcheverplus[outputmacro] then
                begin
                reposition(opcolumn);
                writestr('DC.W');
                reposition(opndcolumn);
                writech('$');
                WriteHex(data div $10000);
                writech(',');
                writech('$');
                WriteHex(data mod $10000);
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
	{DRB
        if computed_len <> (currentpc - instpc) then
          begin
          writeln('Instruction length mismatch, PC=', instpc: - 4, ', pass1=',
                  computed_len: 1, ', pass2=', currentpc - instpc: 1);
          compilerabort(inconsistent);
          end;
	  }

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
    highcode := sectionpc[codesect];
  end {PutCode} ;

procedure openc;

{ Open files for code generator.
}

  procedure getoutputname;

  { Fill the globals "filename" and "outputname".
  }

    var
      i: FilenameIndex; {induction on outputname}
      limit: 1..maxprocnamelen; {length of outputname used}


    begin {getoutputname}
      getfilename(nil, true, true, filename, filename_length);
      limit := min(filename_length, maxprocnamelen);
      case targetopsys of
        unix,apollo:
          for i := 1 to limit do
            outputname[i] := filename[i];	
        msdos:
          for i := 1 to limit do
            if (filename[i] >= 'A') and (filename[i] <= 'Z') then
              outputname[i] := chr(ord(filename[i]) + (ord('a') - ord('A')))
            else outputname[i] := filename[i];
        otherwise
          for i := 1 to limit do
            if (filename[i] >= 'a') and (filename[i] <= 'z') then
              outputname[i] := chr(ord(filename[i]) - (ord('a') - ord('A')))
            else outputname[i] := filename[i];
        end;
      for i := limit + 1 to maxprocnamelen do outputname[i] := ' ';
    end {getoutputname} ;



  begin {openc}
    getoutputname;
    if switcheverplus[outputmacro] then
      begin
      getfilename(macname, false, false, filename, filename_length);
      case hostopsys of
        vdos:
          case targetopsys of
            vms, rsx, rsts, rt: assign(macfile, trim(string(filename)) + '.ma');
            unix: assign(macfile, trim(string(filename)) + '.s');
            vdos: assign(macfile, trim(string(filename)) + '.sa');
            msdos, apollo: assign(macfile, trim(string(filename)) + '.as');
            end {case} ;
        otherwise
          case targetopsys of
            vms: assign(macfile, trim(string(filename)) + '.mar');
            rsx, rsts, rt: assign(macfile, trim(string(filename)) + '.mac');
            unix: assign(macfile, trim(string(filename)) + '.s');
            vdos: assign(macfile, trim(string(filename)) + '.sa');
            msdos, apollo: assign(macfile, trim(string(filename)) + '.asm');
            end {case} ;
        end {case} ;
	rewrite(macfile);
      end {/macro} ;

    if switcheverplus[outputobj] then
      begin
      getfilename(objname, false, false, filename, filename_length);
      case hostopsys of
        vdos:
          case targetopsys of
            vms, rsx, rsts, rt, msdos: assign(objfile, trim(string(filename)) + '.ob');
            vdos: assign(objfile, trim(string(filename)) + '.ro');
            unix: assign(objfile, trim(string(filename)) + '.o');
            apollo: assign(objfile, trim(string(filename)) + '.bi');
            end {case} ;
        vms:
          case targetopsys of
            vms: assign(objfile, trim(string(filename)) + '.obj/nocr');
            rsx, rsts, rt: assign(objfile, trim(string(filename)) + '.obj');
            unix: assign(objfile, trim(string(filename)) + '.o/seek');
            vdos: assign(objfile, trim(string(filename)) + '.ro');
            msdos: assign(objfile, trim(string(filename)) + '.obj/seek');
            apollo: assign(objfile, trim(string(filename)) + '.bin/seek');
            end;
        msdos:
          case targetopsys of
            vms: assign(binobjfile, trim(string(filename)) + '.obj');
            rsx, rsts, rt: assign(objfile, trim(string(filename)) + '.obj');
            unix: assign(objfile, trim(string(filename)) + '.o/seek');
            vdos: assign(objfile, trim(string(filename)) + '.ro');
            msdos: assign(objfile, trim(string(filename)) + '.obj');
            apollo: assign(objfile, trim(string(filename)) + '.bin/seek');
            end;
        unix, apollo:
          case targetopsys of
            vms: assign(binobjfile, trim(string(filename)) + '.obj');
            rsx, rsts, rt: assign(objfile, trim(string(filename)) + '.obj');
            unix: assign(objfile, trim(string(filename)) + '.o');
            vdos: assign(objfile, trim(string(filename)) + '.ro');
            msdos: assign(objfile, trim(string(filename)) + '.obj');
            apollo: assign(objfile, trim(string(filename)) + '.bin');
            end;
        otherwise
          case targetopsys of
            vms: assign(objfile, trim(string(filename)) + '.obj');
            rsx, rsts, rt: assign(objfile, trim(string(filename)) + '.obj');
            unix: assign(objfile, trim(string(filename)) + '.o/seek');
            vdos: assign(objfile, trim(string(filename)) + '.ro');
            msdos: assign(objfile, trim(string(filename)) + '.obj/seek');
            apollo: assign(objfile, trim(string(filename)) + '.bin/seek');
            end;
        end {case} ;
	rewrite(objfile);
      end {/object} ;

  end {openc} ;


procedure closec;

{ Close object and macro files.
}


  begin {closec}
    if switcheverplus[outputmacro] then close(macfile);
    if switcheverplus[outputobj] then close(objfile);
  end {closec} ;

  end.
