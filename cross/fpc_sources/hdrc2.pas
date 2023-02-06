{[b+,l-]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF CONFIDENTIAL SOFTWARE:
  Copyright 1977, 1978, 1979, 1980, 1981, 1982, 1988 by Oregon Software, Inc.
  ALL RIGHTS RESERVED.

  This computer program is the proprietary property of Oregon
  Software, Inc. of Portland, Oregon, U.S.A., and may be used
  and copied only as specifically permitted under written
  license agreement signed by Oregon Software, Inc.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.


  Release version: 0045  Level: 1
  Processor: M68000
  System: ~system~

  Header file for CODE and PUTCODE only

 Last modified by KRIS on 21-Nov-1990 15:28:54
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}


const

  maxstuffnodes = 39; {maximum number of nodes needed to stuff registers}
  prologuelength = 25; {maximum number of nodes in procedure prologue}

{ tab stops for macro file:
}
  opcolumn = 10;
  opndcolumn = 19;
  procnamecolumn = 27;
  nodecolumn = 45;

type
  bytesize = 0..255;
  section = (codesect, diagsect, datasect); { code sections }
  supportrange = integer;
  linknametype = packed array [1..maxprocnamelen] of char;

  fixupptr = ^fixupnode;
  fixuptype = (fixuplabel, fixupproc, fixupabs, fixupesdid);
  fixupnode =
    record
      fixuplink: fixupptr;
      fixupfileloc: addressrange; { location in relfile }
      fixupobjpc: addressrange;
      fixupaddr: addressrange; { relative address when found }
      fixuplen: 2..4; { word or long fixup -- for 68020 }
      case fixupkind: fixuptype of
        fixuplabel: (fixuplabno: integer);
        fixupproc: (fixupprocno: integer);
        fixupabs: ();
        fixupesdid: (vartabindex: integer); { index into vartable }
    end;

  esdrange = firstesd..lastesd;

  esdtype = (esdsupport, esdexternal, esdentry, esdload, esdglobal,
	     esddiag, esdcodestart, esdcodesize,
             esdcommon, esdbegin, esduse, esddefine, esdshrvar);

  esdtabentry =
    packed record
      case esdkind: esdtype of
        esdsupport: (suppno: libroutines); { support number }
        esdexternal, esdentry:
          (exproc: proctableindex); { index to proctable }
        esduse, esddefine: (vartabindex: integer); { index into vartable }
        esdload: (sect: section);
        esdglobal, esdcommon, esdbegin: (glbsize: addressrange);
        esddiag, esdcodestart, esdcodesize: ();
    end;

{ types to support formatted macro output:
}
  longname = packed array [1..max_string_len] of char;
  columnrange = 0..120;

  bits = 0..16; {counts bits in a word}

  objfiletype = file of packed array [0..255] of 0..255;

  tempreltype = (externrel, forwardrel, backwardrel, textrel,
                 nonlocalrel, supportrel, commonrel, stackrel);

  tempreloffset = 0..5000; {max of proctablesize, maxsupport, maxnonlocal}

  tempnegoffset = 0..15; {offset in words for cross-level procedure calls}

  tempreloc = packed record
                treltype: tempreltype;
                treloffset: tempreloffset;
                tnegoffset: tempnegoffset;
                trellocn: addressrange;
              end;

  relfiletype = file of packed array [0..255] of 0..maxusword;

  worddiskblock = packed array [0..worddiskbufsize] of 0..maxusword;
  wordstream = file of worddiskblock;

  { Putcode object types.
  }
  objtypes =
    (objnorm, { normal code word }
     objext, { external symbol reference }
     objforw, { forward ref--to be fixed up }
     objsup, { support call }
     objcom, { common reference }
     objoff, { offset -- can not be start of relocation group }
     objpic, { start of long pic relocation }
     objign, { word containing linker commands which usually follows
               objpic -- ignore for /test listing }
     objlong { reserve a longword -- must not be split across records }
    );
  objtypesx = array [objtypes] of 0..4;

var
  savedebinst: nodeindex; {debug instruction for fixup}
  profstmtcount: integer; {number of statements being profiled}

  highcode, currentpc: addressrange;
  sectionpc: array [section] of addressrange; { currentpc for this section}
  sectionno: array [section] of integer; {current section number}
  currentsect: section; { section for current code}

  totalputerr: integer; { total putmac errors in program }
  testing: boolean; { true if /test AND /macro }
  column: columnrange;
  sectiontail: string2; {contains .S or blanks}

  labelpc: addressrange; { object address of last-found label }
  found: boolean; { general boolean used to control search loops }

  lastobjpc: addressrange; { buffered objpc for intermediate file dump }

  fixuphead: fixupptr; { points to head of fixup list }
  fixuptail: fixupptr; { points to the last entry in the list }
  fixp: fixupptr; { temporary pointer into fixup list }

  esdtable: array [esdrange] of esdtabentry; { filled first-come, first-served
                                              }
  nextesd: esdrange; { next available entry in ESDtable }
  newesd: esdtabentry; { next argument for ESDtable insertions }
  esd: esdrange; { induction var for scanning ESD table }
  esdid: esdrange; { returns ESD index from ESD table searches }

  { buffers and counters to manage object file production }

  tempfilesize: addressrange; { total length in words of relfile }

  nextobjfile: - 1..255; { index into objfile buffer }
  nextrelfile: - 1..255; { index into relfile buffer }
  nextobjblk: integer; { Block number for seek }
  objbytecount: integer;

  nexttempbuffer: 0..32; { index into tempbuffer }
  nexttemprelocn: 0..32; { index into temprelocn }
  tempbuffer: array [0..31] of unsigned; {buffers constants and instructions}
  temprelocn: array [0..31] of boolean; {gets packed going into relfile}

  { Diagnostic code generation variables }

  nowdiagnosing: boolean; {generate diagnostic code for this block}
  everdiagnosing: boolean; {ever generated diagnostic code}
  lastdiagline: integer; {last line number reference generated}
  lastdiagpc: addressrange; {last pc reference generated}
  diaglenfix: fixupptr; {fixup for length of diag tables}
  codelenfix: fixupptr; {fixup for length of code}
  diagbitbuffer: unsigned; {holds diagnostic bits}
  nextdiagbit: bits; {next diagnostic bit to be filled in}
  commonesdid: esdrange; {kluge for common section (own)}

  macfile: text; {receives macro assembler image}
  objfile: objfiletype; {receives object image} {??}
  relfile: relfiletype; { array [0..255] of unsigned }
  diagfile: wordstream; {temporary diagnostics file}

  newobjectfmt: boolean; {if true generate new object types for long names}

  objtype: array [1..maxwords] of objtypes;
  object: array [1..maxwords] of uns_word; { table for one instruction }
  relocn: array [1..maxwords] of boolean; { table of relocation status }
  fixups: array [1..maxwords] of fixupptr; { table of fixups }

  { Procmap is a table to map from internal indices for pascal procedures
    to memory addresses or symbol table indices.  If a procedure is defined,
    the map contains its memory address, otherwise the map contains its
    symbol table index.  "Index" is used only by Unix.
  }
  procmap: array [proctableindex] of
             record
               case boolean of
                 false: (index: shortint {symbol table index} );
                 true: (addr: addressrange {memory addresses} );
               end {procmap} ;
