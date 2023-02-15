{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1985, 1986, 1987 Oregon Software, Inc.
  ALL RIGHTS RESERVED.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied, modified, transferred, and used only as
  provided under a signed license agreement with Oregon Software.  
  Any support purchased from Oregon Software does not apply to 
  user-modified programs.  All copies of this program must display 
  this notice and all copyright notices. 

}

{**************************************************************}
{                                                              }
{                Lexical Scanner for Pascal-2                  }
{                                                              }
{**************************************************************}



{ Pascal 2 Lexical scanner --

  This routine reads the input file, processing "include" directives,
  and breaks it up into tokens.  As each token is scanned, it is written
  to an intermediate file for use by analys.

  Identifiers are looked up using a hash method, with the actual text
  of the identifier kept in a large table in the heap.  This table is
  written to the "string file" when the pass is done.

  Strings are written to the "string file" as they are scanned, and
  are handled by reference to their addresses within this file.
}



unit scan;

interface

uses config, hdr, error, product, utils, sysutils;

{ Pascal tokens -- identified by scanner }

type

  tokentype = (programsym, labelsym, constsym, typesym, varsym, sharedsym,
               proceduresym, functionsym, uparrow, arraysym, filesym,
               setsym, recordsym, stringsym, univsym, packedsym,
               originsym, usesym, definesym, beginsym, ifsym, casesym,
               whilesym, repeatsym, forsym, withsym,
               gotosym, eql, lss, gtr, neq, leq, geq, insym, plus, minus,
               orsym, star, slash, divsym, modsym, andsym, ofsym, endsym,
               elsesym, thensym, otherwisesym, dosym, untilsym, tosym,
               downtosym, notsym, at, nilsym, colon, dot, dotdot, comma,
               semicolon, becomes, lpar, rpar, lbrack, rbrack, intconst,
               realconst, dblrealconst, charconst, stringconst, ident, eofsym,
               lineinc, lineadd, newfile);

  { tokenrecord is the interface between scan and analys }

  { the Modula-2 and Pascal-2 scanner differ here, in that M-2 keeps
    the file index of the current source file in a global variable.

    This is impossible in the P-2 scanner, as we support the optional
    configuration of the compiler as four passes, with scanner output
    written to an intermediate file.  M-2 (due to the way definition
    modules are handled) requires the SCAN and ANALYS to run as one
    pass, thus gets a way with this little hack.
  }

  tokenrecord =
    record
      baseline: integer; {line the current file started on}
      line: integer; { line this token appeared on }
      fileindex: integer; { pointer into file name structure }
      filepos: integer; { getpos position of this token }
      left, right: columnindex; { where on the line it appeared }
      case token: tokentype of { the actual token }
        ident:
          (key: hashindex; { key uniquely identifies us }
           keypos: integer { location of name in string file } );
        intconst, charconst: (intvalue: integer {value or ord(value)} );
        realconst, dblrealconst: (realvalue: realarray {value} );
        stringconst:
          (pos: integer;
           len: columnindex) { position and length within stringfile }
    end;

  tokenlengthtable = array [tokentype] of 0..10; {defines token lengths}

var
  toklengths: tokenlengthtable;

  { Nexttoken is shared by scan and analys when bigcompilerversion is true,
    thereby avoiding intermediate file I/O.
  }

  nexttoken: tokenrecord; {token being built by scanner}

  procedure opens;
  procedure closes;
  procedure closeall;

  procedure scan1;
  procedure scan2;

  procedure warnat(error: warning; {Error message number}
                   line: integer; {Text line number for message}
                   column: columnindex {text column for marker} );

  { Generate an error message at the specified line and column.
    Any error turns off generation of the intermediate file.
  }

  procedure fatal(err: warning);

  procedure scantoken;

  procedure dumpstr(len: columnindex; {number of chars to dump}
                  buf: boolean; {which buffer}
                  dumplen: boolean {true says to dump the length byte} );

  procedure putstringfile;

implementation

const

  { Machine Dependent parameters for scanner }

  maxscanswitch = 40; { max number of recognized embedded switches }
  maxscanswitchlen = 14; { max length is NoPointerCheck }

  { Language specific parameters for scanner }

  tabspace = 8; { The number of spaces a tab input becomes }

  reservedcount = 42; {Number of reserved words}
  minreslen = 2; {Length of shortest reserved word}
  maxreslen = 9; {Length of longest reserved word}
  maxreslen1 = 10; {maxreslen + 1}

 { special characters - ignore nul,rubout and convert tabch, formfeed to ' ' }

  nul = 0;
  rubout = 127;
  tabch = 9;
  formfeed = 12;
  lowercase = 32; {difference between ord of upper case and lower case char}

  { real number conversion parameters -- passed into p2readreal }

  DECformat = 0; { format used by PDP-11 and VAX }
  IEEEformat = 256; { format used by M68000 and N16000 }
  INTELformat = 512; { same as IEEE, except result gets stored backwards }
  IBMformat = 768; { hexadecimal format used by 360/370/31xx/43xx, etc. }

  SinglePrecision = 0;
  DoublePrecision = 16;
  QuadPrecision = 32; { not currently implemented }

  inputbufsize = 1; {length of input line if using fixed arrays}
 
{DRB  inputbufsize = 20;} {length of input line if using fixed arrays}

type
  lineindex = 0..linelen; {index into input line}
  linetype = packed array [1..linelen] of char;
  lineptr = ^linetype;

  halfrealsize = 0..65535; {subrange which is half size of single real}

  alfa = packed array [1..10] of char; {to hold directives}

  reswordtype = packed array [1..maxreslen] of char; {reserved word spelling}
  reservedindex = 1..reservedcount; {index for reserved words}
  identifierrange = 0..1000; {some arbitrary range}

  {internal scanner switches}
  internalswitch = (codesectsw, modulesw, identsw, xshortsectsw, xversionsw,
                    xsectionsw);
  scanswitchindex = 0..maxscanswitch; {switchtable switch}
  switchvalue = - maxswitchvalue..maxswitchvalue; {bumpvalue}
  switchname = packed array [1..maxscanswitchlen] of char; {name of switch}

  hashtablerecord = record
                      pos: stringindex; {string index of id text}
                      len: columnindex; {length of id text}
                      key: hashindex {key assigned to this id}
                    end;

 { declarations for kluged type to allow writing to environment file }
  envirtype = (en_scan_var_block, en_string_block, en_hash_block,
               en_disk_block, en_switch_block);

  { file begins with scanner globals (en_scan_var_block), followed by
    compressed hash table (ehashblock, where ehashblock.pos < 0 means
    that - ehashblock.pos zero entries were compressed from the file),
    and finally the stringtable.
  }

  switchblock = packed record
                  s: switch;
                  v: switchvalue;
                end;

  scanswitchentry = record
                      n: switchname; {name of switch}
                      case internal: boolean of
                        true: (is: internalswitch; {internal switch label} );
                        false:
                          (s: switch; {switch for which this is the entry}
                           v: switchvalue;
                           {amount to increment counter} );
                    end;

  hashtableblock = packed record
                     pos: -hashtablesize .. stringtablesize;
                     len: columnindex; {length of id text}
                     key: hashindex {key assigned to this id}
                   end;

  envirrecord =
    record
      case envirtype of
        en_scan_var_block:
         (elastswitch: switchindex;
          estringfilecount: integer;
          enextstringfile: 0..diskbufsize;
          ecurstringblock: shortint;
          estringtabletop: stringindex;
          einsertions: integer;
          ecodesect_string: stringindex;
          ecodesect_strlength: columnindex;
          emodule_string: stringindex;
          emodule_strlength: columnindex;
          eident_string: stringindex;
          eident_strlength: columnindex;
          ecodesection: shortint;
          eshortsection: boolean;
          eobjversion: shortint;
          edatasection: shortint;
          eownsect_string: stringindex;
          eownsect_strlength: columnindex;
          eswitcheverplus: switcheverplusarray;
          eversion: packed array[1..40] of char);
        en_switch_block:
         (eswitches: array [0..switchesperblock] of switchblock);
        en_string_block: (estringblock: diskblock);
        en_hash_block:
         (ehashblock: array [0..hashtableentriesperblock] of hashtableblock);
        en_disk_block: (ediskblock: diskblock);
    end;

var
  nextch: char; {next character to be scanned}
  ch: char; {current character being scanned}
  unconvertedch: char; {current character in raw (not case converted) state}
  currentline: lineptr; {current input line}
  currentbuf: packed array [1..inputbufsize] of char; {input buffer}
  linepos: lineindex; {current position in input line}
  linesize: integer; {length of current input line}
  charcount: columnindex; {effective position of nextch in this line}
  chpos: columnindex; {effective position of "ch" in this line}
  oldchpos: columnindex; {effective position of last "ch"}
  new_filepos: integer; {for tracking fileposition}
  current_filepos: integer; {new_filepos, delayed by one character}
  lasttokenline: integer; {line of last token read}
  lastbaseline: integer; {baseline for last file read}
  endofline: boolean; {effective eoln with nextch}
  endofinput: boolean; {effective overall eof}
  convertingcase: boolean; {true if uppercase wanted (except in strings)}
  skippingblanks: boolean; {currently skipping blanks}
  saveinput: array [1..sourcedepth] of
      record
        savech: char; {nextch for pushed source levels}
        saveendofline: boolean;
        saveline: lineptr;
        savelen: lineindex;
        savebuf: packed array [1..inputbufsize] of char;
        savepos: lineindex;
        savefileindex: integer;
        savefilename_length: filenameindex;
      end;
  baseline: array [1..sourcedepth] of integer; {starting line for current
                                                file}

  { stringbuf[curstringbuf] is used to buffer the current quoted string
    (nexttoken), while stringbuf[not curstringbuf] is the buffer for the
    previous (token) string, if any. }

  stringbuf: array [boolean] of packed array [0..linelen] of char;

  tokenbufindex: 0..diskbufsize; {next available space in token file block}

  { hashtable, includes pointer to loc in string table }
  hashtable: array [hashindex] of hashtablerecord;

  scanswitchtable: array [scanswitchindex] of {table of embedded switch names}
                          scanswitchentry;

  {Table of reserved words, by length, and pointers into that table}

  {pointers to reswords}
  reslentable: array [minreslen..maxreslen1] of reservedindex;
  reswords: array [reservedindex] of reswordtype; {text of reswords}
  reswordtokens: array [reservedindex] of tokentype; {tokens for reswords}

  tokentable: array [')'..'^'] of tokentype; {tokens for single char tokens}

  mapchars: packed array ['A'..'Z'] of char; {lower to upper conversion table}
  incomment: boolean; {used to detect an unfinished comment}
  inliteralstring: boolean; {used to detect an unfinished literal string}
  first_real_seen: boolean; {used to detect error when $double occurs after
                             first real constant.}
  first_token_seen: boolean; {used to detect error when $case occurs after
                              the first token.}
  current_fileindex: integer; { pointer to current filename }

  {switch buffers used to delay effect of switches processed while reading
   nexttoken until it actually becomes thistoken (scanalys only).
  }

  nextswitchread: boolean; {set true when first switch within comment is found}
  nextswitchcounters: switchcounterarray; {buffer for switchcounters}
  nextswitcheverplus: switcheverplusarray; {buffer for switcheverplus}

procedure p_rdsfst(var f: text;
                   var p: lineptr;
                   var l2: integer);
  external;


procedure p_expfnm(var f: text; {open text file whose name we want}
                   var fn  : packed array of char; {file name to be updated}
                   var len : shortint {length to be updated} );
  external;

function p_prctyp: integer;
  external;

{ Return processor type for 68000.
}


procedure dumpidentifiers;

  forward;

procedure seekstringfile (n: integer {byte to access});

  forward;

{ Utility procedures - Issue an error message, and handle the output files.
}

procedure closes;

{ Close current output file.
}


  begin {closes}
    close(source[sourcelevel]);
  end {closes} ;

procedure opens;

{ Open an included source file at current nesting level. The file
  is expected to be in the global variable "filename". The current
  directory is searched first, followed by any stored include list.
  If the file is not present in any of these, a fatal error occurs.
}

  var
    prefix: FilenameBuf; {temp to create full path name}
    prefixlen: FilenameIndex; {length of prefix}
    i: integer; {induction on include list}
    found, exists: boolean;


  procedure GetIncludeName(which: integer; {pathname desired}
                           var result: FilenameBuf; {resulting pathname}
                           var resultlen: FilenameIndex; {result length}
                           var exists: boolean {true if pathname exists} );

{ Procedure to get a pathname, as was specified on the command line.
}

    var
      NextElement: FilenameListPtr;


    begin {GetIncludeName}
      exists := false;
      if which >= 0 then
        begin
        NextElement := IncludeListHead;
        while (which > 0) and (NextElement <> nil) do
          begin
          NextElement := NextElement^.next;
          which := which - 1;
          end;
        if NextElement <> nil then {within available pathnames}
          begin
          getfilename(NextElement, false, true, result, resultlen);
          exists := true;
          end;
        end;
    end {GetIncludeName} ;


  function length(var s: FilenameBuf): integer;

{ Return the length of string s, i.e. the last index in s that contains
  a non-space character.  If there is no string at all in s, length
  returns 0;
}

    var
      count: integer; {induction on file name characters}


    begin {length}
      count := 0;
      repeat
        count := count + 1;
      until (s[count] = ' ') or (count = filenamelen);
      length := min(count, filenamelen);
    end {length} ;


  function FileExists(addprefix: boolean): boolean;

{ Check if a target directory contains the file. This routine
  assumes that "prefix" contains a directory string, and creates
  a full pathname in prefix if addprefix is true.
  The function returns "true" if the file exists in the directory.

  DRB: free pascal version
}

    const
      DefExt = '.pas';
      DefLen = 4;

    var
      fpc_filename: string;

    begin {FileExists}
      if addprefix then fpc_filename := trim(prefix) + trim(filename)
      else fpc_filename := trim(filename);
      {$I-}
      assign(source[sourcelevel], fpc_filename);
      reset(source[sourcelevel]);
      if (ioresult <> 0) then
        begin
        assign(source[sourcelevel], fpc_filename + DefExt);
        reset(source[sourcelevel]);
        end;
      {$I}
      FileExists := (ioresult = 0);
    end {FileExists} ;


  begin {opens}
    if not FileExists(false) then
      begin {check other directories}
      i := 0;
      found := false;
      repeat
        GetIncludeName(i, prefix, prefixlen, exists);
        if exists then found := FileExists(true);
        i := i + 1;
      until found or not exists;
      if not found then
        begin
        writeln('Can''t open include file ''',trim(filename), '''');
        halt;
        end
      end
  end {opens} ;

procedure opennext;

{ Open the next source file at the current level.  If this is not the
  first input file, the old one is closed.

  DRB: free pascal version
}

  var
    i: integer; {counts file names}
    p: FilenameListPtr; {used to find the next filename}
    fpc_filename: string;


  begin {opennext}
    i := 1;
    p := SourceListHead;
    while (i < curfile) and (p <> nil) do
      begin
      i := i + 1;
      p := p^.next;
      end;
    morefiles := (p <> nil);
    if morefiles then
      begin
      if curfile > 1 then close(source[sourcelevel]);
      getfilename(p, false, false, filename, filename_length);
      fpc_filename := trim(filename);
      assign(source[sourcelevel], fpc_filename);
      {$I-}
      reset(source[sourcelevel]);
      if ioresult <> 0 then
        begin
	fpc_filename := fpc_filename + '.pas';
        assign(source[sourcelevel], fpc_filename);
        reset(source[sourcelevel]);
        end;
      {$I}
      if ioresult <> 0 then
      begin
        write('Can''t open source file ''');
        writeln(trim(filename), '''');
        halt();
      end;
    end;
  end {opennext} ;


procedure warnat(error: warning; {Error message number}
                 line: integer; {Text line number for message}
                 column: columnindex {text column for marker} );

{ put an error message into errortable. Halt compilation if
  this is a fatal error, and force fatal error if errortable
  is filled by this error.
}

  begin
    if lasterror < errortablesize - 1 { Too many errors? } then
      lasterror := lasterror + 1 { No - next table entry }
    else { Yes - quit }
      begin
      fatalflag := true;
      column := 0;
      error := toomanyerrors;
      lasterror := errortablesize
      end;
    with errortable[lasterror] do
      begin
      err := error;
      errline := line;
      errcolumn := column
      end;
    if fatalflag then
      begin
{DRB: need to solve goto 99;
      scan2;
      }

      end;
  end {warnat} ;


procedure fatal(err: warning);

{ Issue an error message and set the "fatalflag" to stop compilation.
}


  begin {fatal}
    fatalflag := true;
    if scanalys then seekstringfile(stringfilecount);
    dumpidentifiers;
    closes;
    sourcelevel := 0;
    warnat(err, lastline, chpos);
  end {fatal} ;



procedure seekstringfile(n: integer {byte to access});

{ Do the equivalent of a "seek" on the string file.  This sets the
  file and "nextstringfile" to access byte "n" of the stringfile.

  Note: Guard calls with "if scanalys" as this routine is
  only meaningful when scan and analys are operating as one pass.

}

  var
    newblock: 1..maxstringblks; { block to which seeking }


  begin {seekstringfile}
    newblock := n div (diskbufsize + 1) + 1;
    if newblock <> curstringblock then
      begin
        stringblkptr := stringblkptrtbl[newblock];
        if stringblkptr = nil then
          begin
          new(stringblkptr);
          stringblkptrtbl[newblock] := stringblkptr;
          end;
        curstringblock := newblock;
      end;
    nextstringfile := n mod (diskbufsize + 1);
  end {seekstringfile} ;

procedure putstringfile;

{ Do the equivalent of a "put" on the stringfile.  The file is organized
  as blocks of bytes, and the next byte to be written is always refered to
  as:

        stringfile^[nextstringfile]    if caching is enabled

        stringblkptr^[nextstringfile]  if caching is disabled

  This procedure does the bookkeeping to make this scheme work.
}


  begin {putstringfile}
    if nextstringfile = diskbufsize then {string buffer full, write it out}
      begin
      nextstringfile := 0;
      curstringblock := curstringblock + 1;
      new(stringblkptr);
      stringblkptrtbl[curstringblock] := stringblkptr;
      end
    else nextstringfile := nextstringfile + 1; { not full, update pointer }
  end {putstringfile} ;


procedure loginputfilename;

{ Put the name of the file just opened (found in global "filename", with
  its length in global "filename_length") in the string table, among the
  identifiers.  The name in the string table is terminated by a chr(0).

  On systems that use logical names or file versions, and where we have
  provided a routine to translate filenames, we use that routine, and put
  the fully qualified name in the string table.  This assures that LIST
  will be accessing the same file that SCAN accessed, even if the user
  has edited the file during compilation, and created a new version.
  This also handles the case of a file being created during compilation
  with the same name as one that was included via the /include= switch,
  when the new file is in a "closer" directory.
}

  var
    p: filerememberptr; {induction on filerememberlist}
    q: filerememberptr; {points to item to be added}
    i: FilenameIndex; {induction on file name}
    temp: shortint; { temporary file name length }

  begin {loginputfilename}

    new(q);
    q^.next := nil;
    q^.offset := stringtabletop + 1;

    if sourcelevel = 1 then nexttoken.baseline := lastline - 1;

    current_fileindex := q^.offset;

    if filerememberlist <> nil then
      begin
      p := filerememberlist;
      while p^.next <> nil do p := p^.next;
      p^.next := q;
      end
    else filerememberlist := q;

    if stringtabletop + filename_length >= stringtablesize then
      fatal(stringtableoverflow);

    for i := 1 to filename_length do
      begin
      stringtabletop := stringtabletop + 1;
      stringtable^[stringtabletop] := filename[i];
      end;

    stringtabletop := stringtabletop + 1;
    stringtable^[stringtabletop] := chr(0);
  end {loginputfilename} ;



{ Source file Input ---
}


procedure getch;

{ Transfers the global "nextch" to "ch" and gets a new value for
  "nextch".  This always provides a one character lookahead to
  simplify some of the lexical scanning.  In the process, the
  globals "endofinput", "endofline", and "chpos" are updated. 
  Formfeed and tab characters are converted to blanks.

  The source files are organized as a stack of files, and input is
  always from the top of the stack.  If the end of an included file
  is found, the stack is popped to the including source file.
}


  procedure special;


    begin {special}
      if (nextch = chr(rubout)) then
        nextch := ' ' { ignore weird characters }
      else if (nextch < ' ') and (nextch <> chr(tabch)) then
        begin
        if nextch = chr(formfeed) then
          begin { Ascii form feed char }
          endofline := true; { but ends line as well }
          nextch := ' '; { also becomes space }
          end
        else if nextch <> chr(nul) then
          begin {other control character, warn and ignore}
          warnat(badchar, lastline, charcount - 1);
          nextch := ' ';
          end
        end
      else charcount := charcount + 1;
    end {special} ;


  procedure dolinetoolong;


    begin {dolinetoolong}
      {Line too long, issue warning}
      warnat(linetoolong, lastline, linelen);
      lastline := lastline + 1;
      charcount := 1;
    end; {dolinetoolong}


  procedure getnextch;

  var
    i1, i2 : integer;


    begin {getnextch}
      if linepos >= inputbufsize then
        begin
{DRB
        getpos(source[sourcelevel], i1, i2);
        case hostopsys of
          unix:new_filepos := i1;
          vms: new_filepos:= i1*4096 + i2 mod 4095;
        end;
}
        read(source[sourcelevel], currentbuf);
        linepos := 0;
        end;
        linepos := linepos + 1;
        nextch := currentbuf[linepos];
      if (nextch = chr(rubout)) or (nextch < ' ') then special
      else charcount := charcount + 1;
      if charcount > linelen + inputbufsize then dolinetoolong;
    end {getnextch} ;


  procedure dotab;


    begin {dotab}
      ch := ' '; {becomes space, update column}
      if not inliteralstring or incomment then
        begin
        charcount := min(((charcount - 1) div tabspace) * tabspace + tabspace,
                         linelen);
        chpos := charcount;
        end
      else warnat(badchar, lastline, chpos);
    end {dotab} ;


  procedure doeof;


    begin {doeof}
      if sourcelevel > 1 then
        begin {inside include, pop to parent level}
        closes;
        with saveinput[sourcelevel] do
          begin
          nextch := savech; {restore next char to read}
          currentbuf := savebuf; {restore input line}
          linepos := savepos; {restore index in line}
          current_fileindex := savefileindex;
          endofline := saveendofline;
          filename_length := savefilename_length;
          end;
        nexttoken.baseline := lastline - baseline[sourcelevel];
        if endofline then lastline := lastline + 1;
        sourcelevel := sourcelevel - 1; {pops the source stack}
        end
      else
        begin {end of main source file, so quit}
        curfile := curfile + 1;
        opennext;
        if morefiles then
          begin
          loginputfilename;
          linepos := inputbufsize;
          getnextch;
          end
        else
          begin
          endofinput := true;
          nextch := ' ';
          skippingblanks := false;
          end;
        end
    end {doeof} ;


  procedure doeoln;

  var
    i1, i2 : integer;

    begin {doeoln}
      { Will nextch come from new line ? }
      if nextch = ' ' then
        begin
        readln(source[sourcelevel]); {skip past eoln}
{DRB
        getpos(source[sourcelevel], i1, i2);
        case targetopsys of
          unix:new_filepos := i1;
          vms: new_filepos:= i1*4096 + i2 mod 4095;
        end;
}
        linepos := inputbufsize;
        if (sourcelevel = 1) or not eof(source[sourcelevel]) then
          begin
          endofline := true;
          lastline := lastline + 1;
          end;
        end
      else
        begin {hack for eof ?????????????}
        nextch := ' ';
        charcount := charcount + 1;
        skippingblanks := false;
        end
    end {doeoln} ;


  procedure doendofline;


    begin {doendofline}
      charcount := 0; {this char is beginning of new line}
      endofline := false;
    end {doendofline} ;


  begin {getch}
    repeat
      { Move to next character }
      ch := nextch;
      oldchpos := chpos;
      chpos := charcount;
      current_filepos := new_filepos;
      if ch = chr(tabch) then dotab;
      unconvertedch := ch;  { saved for case switch }
      if convertingcase and (ch in ['A'..'Z']) then ch := mapchars[ch];
      if endofline then doendofline;
      if linepos < inputbufsize then {normal case -- fill nextch}
        begin
        linepos := linepos + 1;
        nextch := currentbuf[linepos];
        if (nextch = chr(rubout)) or (nextch < ' ') then special
        else charcount := charcount + 1;
        if charcount > linelen + inputbufsize then dolinetoolong;
        end
      else if eof(source[sourcelevel]) then doeof
      else if eoln(source[sourcelevel]) then doeoln
      else getnextch; {read a new line or fill the buffer again}
    until (not skippingblanks) or (ch <> ' ');
  end {getch} ;


procedure dumpstr (len: columnindex; buf, dumplen: boolean);

{ Copy stringbuf[buf] to the string file.
}

  var
    i: 1..linelen;


  begin {dumpstr}

    seekstringfile(stringfilecount);
    stringfilecount := stringfilecount + len;

    if dumplen then
      begin
      stringblkptr^[nextstringfile] := ord(stringbuf[buf, 0]);
      putstringfile;
      len := len - 1;
      end;

    for i := 1 to len do
      begin
      stringblkptr^[nextstringfile] := ord(stringbuf[buf, i]);
      putstringfile;
      end;
  end {dumpstr} ;


procedure dumpidentifiers;

{ Dumpidentifiers -- dumps stringtable into stringfile.
  Analys and Code occasionally need the character representation of
  an identifier.
}

  var
    i: stringindex; {induction var for writing}


  begin {dumpidentifiers}
    for i := 1 to stringtabletop do
      begin
      stringblkptr^[nextstringfile] := ord(stringtable^[i]);
      putstringfile;
      end;
  end {dumpidentifiers} ;




procedure scantoken;

{ Get the next token from the source file.

  This is the main procedure in scan, and converts all tokens to
  internal format.
}

  var
    somethingelse: boolean; { set true when we have skipped all separators }
    commentch: char; { Comment terminator (needed for obsolete comments)}


  procedure skipcomment;

{ Skip comments --

  Skip characters until the appropriate ending comment bracket is found.
  If the comment contains compiler switches (first character "$), these
  are parsed and the new value set into the switch table.
}

    var
      termchars: set of char; {terminating characters}


    procedure scanswitches;

{ Scans a list of compiler switches, separated by commas, and checks them
  against the known switches.

  Switches must match exactly on all characters provided, with a minimum
  of three characters needed to distinguish a switch.  Unknown switches
  are ignored, as distasteful as this might seem.

  If a switch is found, the existence of the switch is logged in the
  "switchtable" along with the token count so that other passes can use
  it.  In addition, the switch counters are updated for this pass, and
  "switcheverplus" is set if the switch is turned on.

  In the "scanalys" case we are actually reading the next token while
  processing the current token so we delay the effect of counting switches
  (this normally happens while analys is reading in the intermediate file).

  If this switch affects the state of the listing, an entry is made in
  the global "listtable" for the listing pass.
}

      const
        checkswitchcount = 5; {nr of checking switches}

      var
        i, j, l: identifierrange; {induction var for search}
        oldcount: identifierrange; {old value of switchcounter}
        slen: identifierrange; {length of switch as read}
        sname: switchname; {holds switch name}
        nofound: boolean; {negated switch}

      function match: boolean;

{ Match the switch string in sname against the switch name in the
  global constant "scanswitchtable[i]"
}

        var
          j: identifierrange; {induction variable for match}


        begin {match}
          with scanswitchtable[i] do
            begin
            j := 1;
            while (j < slen) and (sname[j] = n[j]) do j := j + 1;
            match := (j = slen);
            end;
        end {match} ;


      procedure stuff;

{ Insert the current character into the stringtable if there is room.
}


        begin {stuff}
          if stringtabletop < stringtablesize then
            begin
            stringtabletop := stringtabletop + 1;
            stringtable^[stringtabletop] := nextch
            end
          else fatal(stringtableoverflow);
        end {stuff} ;


      function snumber: integer;

{ Read a number for a scanner switch.
}

        var
          val: integer;


        begin {snumber}
          val := 0;
          while (nextch >= '0') and (nextch <= '9') do
            begin
            val := val * 10 + (ord(nextch) - ord('0'));
            getch;
            end;
          snumber := val;
        end {snumber} ;


      procedure addswitch(i: scanswitchindex {switchentry to add});

        {add entry i to the switch table, the table of consecutive
         switches encountered.
        }

        begin {addswitch}
          if lastswitch < switchtablesize {table full ?} then {no}
            begin {all other switches}
            lastswitch := lastswitch + 1;
            with switches[lastswitch] do
              begin
              s := scanswitchtable[i].s;
              v := scanswitchtable[i].v * ((1 - ord(nofound)) * 2  - 1);
              mlow := putlow;
              mhi := 0;
              end;
            end; {note that we ignore switch if table full}
          end {addswitch} ;


      procedure bumpswitch(s: switch; {switch to bump}
                           v: switchvalue {value to use});

        {bump switch count for switch s.
        }

        begin {bumpswitch}
          if scanalys and (s <> listcount) then
            begin {track look-ahead}
            if not nextswitchread then
              begin
              nextswitchread := true;
              nextswitchcounters := switchcounters;
              nextswitcheverplus := switcheverplus;
              end;
            nextswitchcounters[s] := nextswitchcounters[s] + v;
            if nextswitchcounters[s] > 0 then nextswitcheverplus[s] := true;
            end
          else
            begin
            oldcount := switchcounters[s]; {old value}
            switchcounters[s] := switchcounters[s] + v;
            if switchcounters[s] > 0 then switcheverplus[s] := true;
            {set nextswitch buffer in case we've just read a switch}
            nextswitchcounters[s] := switchcounters[s];
            nextswitcheverplus[s] := switcheverplus[s];
            end;
        end {bumpswitch} ;

      begin {scanswitches}
        repeat
          getch; {nextch in ['$',',']}

          if ch = ',' then {skip blanks following ','}
            while nextch = ' ' do getch;

          { Build switch name from source file }

          slen := 0;
          while nextch in ['a'..'z', 'A'..'Z', '0'..'9'] do
            begin
            slen := slen + 1;
            getch;
            if slen <= maxscanswitchlen then sname[slen] := ch;
            end;

          { Search for legal switches -- unknown are ignored }

          scanswitchtable[0].n := sname;
          scanswitchtable[0].internal := false;
          scanswitchtable[0].s := noswitch;
          scanswitchtable[0].v := 0;
          i := maxscanswitch;
          nofound := false;
          if (slen < 3) or (slen > maxscanswitchlen) then i := 0
          else
            begin
            if (sname[1] = 'n') and (sname[2] = 'o') then
              begin
              nofound := true;
              for l := 3 to slen do sname[l - 2] := sname[l];
              scanswitchtable[0].n := sname;
              slen := slen - 2;
              end;
            while not match do i := i - 1; {sentinel search}
            end;
          with scanswitchtable[i] do
            begin
            if s = fpc68881 then
              begin
              switcheverplus[cpu68020] := true;
              nextswitcheverplus[cpu68020] := true;
              end;

            if nofound and (internal or
                    (s in [cpu8086, cpu68000, cpu68020, own, multidef])) then
              i := 0 {ignore illegal no's}
            else if not internal then
              begin
              { Detect the case where $double occurs after the
                first token.
              }
              if (s = doublereals) and first_token_seen then
                if switchcounters[standard] <= 0 then
                  warnat(baddouble, lastline, chpos)
                else i := 0; {ignore if standard active}

              { Detect the case where $case occurs after the
                first token.
              }
              if (s = caseswitch) and first_token_seen then
                if switchcounters[standard] <= 0 then
                  warnat(badcase, lastline, chpos)
                else i := 0; {ignore if standard active}
              end;
            end;

          { Process switch }

          { i = 0 if switch not found, but zero was set up above }
          with scanswitchtable[i] do
            if internal then
              begin
              if nextch = '=' then
                begin
                getch;
                case is of
                  codesectsw:
                    begin
                    codesect_string := stringtabletop + 1;
                    if nextch = '''' then
                      begin
                      convertingcase := false;
                      inliteralstring := true;
                      getch;
                      repeat
                        while (nextch <> '''') and not endofline do
                          begin
                          stuff;
                          getch;
                          end;
                        if not endofline then
                          begin
                          getch;
                          stuff;
                          end;
                      until (nextch <> '''') or endofline;
                      codesect_strlength := stringtabletop - codesect_string;
                      inliteralstring := false;
                      convertingcase := true;
                      end;
                    end;
                  modulesw:
                    begin
                    module_string := stringtabletop + 1;
                    if nextch = '''' then
                      begin
                      convertingcase := false;
                      inliteralstring := true;
                      getch;
                      repeat
                        while (nextch <> '''') and not endofline do
                          begin
                          stuff;
                          getch;
                          end;
                        if not endofline then
                          begin
                          getch;
                          stuff;
                          end;
                      until (nextch <> '''') or endofline;
                      module_strlength := stringtabletop - module_string;
                      inliteralstring := false;
                      convertingcase := true;
                      end;
                    end;
                  identsw:
                    begin
                    ident_string := stringtabletop + 1;
                    if nextch = '''' then
                      begin
                      convertingcase := false;
                      inliteralstring := true;
                      getch;
                      repeat
                        while (nextch <> '''') and not endofline do
                          begin
                          stuff;
                          getch;
                          end;
                        if not endofline then
                          begin
                          getch;
                          stuff;
                          end;
                      until (nextch <> '''') or endofline;
                      ident_strlength := stringtabletop - ident_string;
                      inliteralstring := false;
                      convertingcase := true;
                      end;
                    end;
                  xsectionsw: codesection := snumber mod 16;
                  xshortsectsw:
                    begin
                    shortsection := true;
                    codesection := snumber mod 16;
                    end;
                  xversionsw:
                    begin
                    objversion := snumber mod 256;
                    if nextch = '.' then
                      begin
                      getch;
                      objrevision := snumber mod 256;
                      end;
                    end;
                  end {case} ;
                end;
              end
            else {no internal switch}
              begin
              if i = checkswitchcount + 1 then {check switch}
                for j := 1 to checkswitchcount do
                  bumpswitch(scanswitchtable[j].s, ((1-ord(nofound)) * 2  - 1))
              else bumpswitch(s, v * ((1 - ord(nofound)) * 2  - 1));
              if s = listcount then
                begin
                { List and nolist are entered into a special table,
                  'listtable' }
                if (oldcount + switchcounters[listcount] = 1) and
                   switcheverplus[listcount] then
                { Transition occurred }
                  if (oldcount = 0) and (lastlist < listtablesize) then
                    begin
                    lastlist := lastlist + 1;
                    listtable[lastlist].start := lastline;
                    listtable[lastlist].count := 0;
                    end
                  else if oldcount > 0 then
                    listtable[lastlist].count :=
                      lastline - listtable[lastlist].start + 1;
                end {listing switches}
              else
                begin
                if (s = own) and (nextch = '=') then
                  begin
                  getch;

                  if (targetopsys = vdos) and (nextch >= '0') and
                     (nextch <= '9') then
                    datasection := snumber mod 16
                  else
                    begin
                    ownsect_string := stringtabletop + 1;
                    if nextch = '''' then
                      begin
                      convertingcase := false;
                      inliteralstring := true;
                      getch;
                      repeat
                        while (nextch <> '''') and not endofline do
                          begin
                          stuff;
                          getch;
                          end;
                        if not endofline then
                          begin
                          getch;
                          stuff;
                          end;
                      until (nextch <> '''') or endofline;
                      ownsect_strlength := stringtabletop - ownsect_string;
                      inliteralstring := false;
                      convertingcase := true;
                      end;
                    end;
                  end;
                if i = checkswitchcount + 1 then
                  begin {check switch}
                  for j := 1 to checkswitchcount do
                    addswitch(j);
                  end
                else addswitch(i);
                end;
              end;
        until nextch <> ','; {process entire list}
      end {scanswitches} ;


    begin {skipcomment}
      incomment := true;
      { Check for switch stream and handle if needed }
      termchars := ['}', ')', commentch];
      if nextch = '$' then scanswitches;
      convertingcase := false;
      repeat
        getch;
        skippingblanks := true;
        while not ((ch in ['}', '*']) or endofinput) do getch; {bump to '*'}
        skippingblanks := false;
        while (ch = '*') and not endofinput do getch; {ignore multiples}
        if ch in termchars then incomment := false;
      until not incomment or endofinput;
      convertingcase := true;
      if incomment then warnat(eofincomment, lastline - 1, chpos)
      else if not endofinput then getch; {skip past delimiter if present}
      incomment := false;
    end {skipcomment} ;




  procedure stringliteral(quotech: char);

{ Scan a quoted string, building a string or character literal.

  As the string is read, a copy of it is built up in the string table.
  If the string length is exactly one, the token is returned as a character
  constant, otherwise the string is written to the string file and the
  token is a string constant.
}

    var
      stringpos: lineindex; {start of the string in the stringtable}
      i: lineindex; {induction var}

    procedure stuff;

{ Insert the current character into the stringtable if there is room.
}


      begin {stuff}
        if stringpos < linelen then
          begin
          stringpos := stringpos + 1;
          stringbuf[curstringbuf, stringpos] := ch
          end
        else warnat(longstring, lastline, chpos);
      end {stuff} ;


    begin {stringliteral}
      { First read in the string and copy to stringtable }
      curstringbuf := not curstringbuf;
      stringpos := 0;
      repeat
        convertingcase := false; {we want a literal copy}
        inliteralstring := true;
        getch;
        while (ch <> quotech) and not endofline do
          begin
          stuff;
          getch;
          end;
        inliteralstring := false;
        convertingcase := true;
        if endofline then warnat(longstring, lastline - 1, chpos)
        else
          begin
          getch;
          stuff;
          end;
      until (ch <> quotech) or endofline;

      stringpos := stringpos - 1;
      stringbuf[curstringbuf, 0] := chr(stringpos);

      with nexttoken do {check length and set returned token}
        if (stringpos = 0) and switcheverplus[standard] then
          warnat(zerostring, line, left)
        else if stringpos = 1 then
          begin
          token := charconst;
          if stringpos = 0 then intvalue := 0
          else intvalue := ord(stringbuf[curstringbuf, 1])
          end
        else
          begin
          token := stringconst;
          len := stringpos;
          pos := - 1 {don't dump yet}
          end;
    end {stringliteral} ;


  procedure charliteral;

{ Process a character constant of the form #123.
}

    var
      val: integer;
      numbers: set of char;
      ok: boolean;


    procedure snumber;

{ Read a number.
}


      begin {snumber}
        val := 0;
        while nextch in numbers do
          begin
          val := val * 10 + (ord(nextch) - ord('0'));
          getch;
          end;
      end {snumber} ;


    begin {charliteral}
      numbers := ['0'..'9'];
      ok := false;
      if nextch in numbers then
        begin
        snumber;
        if val <= 255 then
          with nexttoken do {check length and set returned token}
            begin
            token := charconst;
            intvalue := val;
            ok := true;
            end;
        end;
      if not ok then warnat(badconsterr, lastline, chpos);
      getch;
    end {charliteral} ;


{ Directives addressed to the scanner and listing pass may be included
  in the source text.  They are flagged by a percent sign (%), and have
  no direct effect on the pascal code.

  The only important scanner directive is "%INCLUDE", which causes the
  inclusion of other source files in the source.
}


  procedure scannerdirective(l: integer;
                             c: columnindex);

{ Read and process a scanner directive.

  The only directives accepted are "include" and "page", and only "include"
  has any effect on the scanner.

  An include directive is expected to be followed by a file name, delimited
  by a blank or semicolon.  When such a file has been found, the source
  stack is pushed and the file specified opened as the next source file.
}

    var
      s: alfa; {directive as scanned}
      i: identifierrange; {general use induction var}
      filenamebuilt: boolean; {true if filename correctly built}
      newform: boolean; {true if new type %include (quoted filename)}
      oldfilename_length: filenameindex; {for saving filename_length in stack}
      newfilename_length: filenameindex; {to temporarily save new filename_length}


    begin {scannerdirective}
      if (switchcounters[standard] > 0) then warnat(badchar, l, c);
      i := 0;
      s := '          ';
      getch;
      while ch in ['A'..'Z', 'a'..'z'] do
        begin {read the directive}
        if i < 10 then
          begin
          i := i + 1;
          if ch in ['A'..'Z'] then s[i] := chr(ord(nextch) + lowercase)
          else s[i] := ch;
          end;
        getch;
        end;

      { Now see if the directive is known, and handle if necessary }

      if s = 'include   ' then
        begin {open a new level of source}
        if sourcelevel = sourcedepth then fatal(deepinclude);
        while nextch in [' ', chr(tabch)] do getch;
        filenamebuilt := false;
        i := 0;
        newform := nextch = '''';
        if newform then
          begin
          getch;
          while (nextch <> '''') and not endofline do
            begin
            if i < filenamelen then
              begin
              i := i + 1;
              filename[i] := nextch;
              end;
            getch;
            end;
          if nextch = '''' then
            begin
            getch;
            filenamebuilt := true;
            end
          else warnat(longstring, lastline - 1, chpos);
          end
        else
          begin
          while not (nextch in [' ', chr(tabch), ';']) do
            begin
            if i < filenamelen then
              begin
              i := i + 1;
              filename[i] := nextch;
              end;
            getch;
            end;
          filenamebuilt := true;
          getch;
          end;
        oldfilename_length := filename_length;
        newfilename_length := i;
        while i < filenamelen do
          begin
          i := i + 1;
          filename[i] := ' '
          end;

        while (nextch in [' ', chr(tabch)]) and not endofline do getch;

        if nextch = ';' then getch;

        if filenamebuilt then
          begin
          sourcelevel := sourcelevel + 1;
          opens;
          with saveinput[sourcelevel] do
            begin
            savech := nextch;
            savepos := linepos;
            saveendofline := endofline;
            savefileindex := current_fileindex; { stringtable index of filename}
            savefilename_length := oldfilename_length;

            savebuf := currentbuf;
            linepos := inputbufsize; {force reading of a new line}
            end;
          filename_length := newfilename_length;
          loginputfilename;
          baseline[sourcelevel] := lastline - nexttoken.baseline -
                                   ord(endofline);
          if not endofline then lastline := lastline + 1;
          nexttoken.baseline := lastline - 1;
          endofline := true;
          end;
        end
      else if s <> 'page      ' then
        begin
        warnat(baddirective, l, c);
        getch
        end;
      ch := ' ';
      nextch := ' ';
    end {scannerdirective} ;




  procedure identifier;

{ Scan an identifier or reserved word.

  The identifier text is stored in the string table, and a hash
  function computed as it is scanned.  If the identifier is not a
  reserved word, it is checked in the hash table and inserted if not
  found.

  The hash function used was determined empirically from actual Pascal
  programs.
}

    var
      t1, t2: 0..maxusword; {used in computing hash function}
      lastch: integer; {position of end of id in stringtable}
      count: identifierrange; {identifier length}
      hash: hashindex; {location within hash table}
      w: reswordtype; {first part of id for reserved word check}


    procedure maptoken(var token: tokentype);

{ Check the identifier just scanned against the reserved word table.
  Token is set to "ident" if not found, or the the corresponding token
  if this is a reserved word.

  The reserved word table is sorted by identifier length and alphabetical
  order, and a binary search is done on the portion of the table with the
  length of the identifier just read.
}

      var
        left, right, middle: reservedindex; {binary search pointers}


      begin {maptoken}
        token := ident;
        if (count >= minreslen) and (count <= maxreslen) then
          begin
          left := reslentable[count];
          right := reslentable[count + 1] - 1;
          while left < right do
            begin
            middle := (left + right) div 2;
            if reswords[middle] < w then left := middle + 1
            else right := middle;
            end;
          if reswords[right] = w then token := reswordtokens[right]
          end
      end {maptoken} ;


    function idfound: boolean;

{ Check the identifier just read against the hash table and return
  "true" if it is already there.
}

      var
        i, j: integer; {induction vars for match}


      begin {idfound}
        idfound := false;
        with hashtable[hash] do
          if len = count then
            begin
            j := pos;
            i := stringtabletop + 1;
            while (stringtable^[i] = stringtable^[j]) and (i <= lastch) do
              begin
              i := i + 1;
              j := j + 1
              end;
            if i > lastch then idfound := true;
            end;
      end {idfound} ;


    begin {identifier}
      if switcheverplus[caseswitch] then 
        begin
        convertingcase := false;
        ch := unconvertedch;
        end;
      w := '         ';
      count := 0;
      lastch := stringtabletop;
      t2 := 0;
      repeat
        count := count + 1;
        if count <= maxreslen then
          begin
          w[count] := ch;
          t1 := ord(ch) mod 32;
          case (count - 1) mod 3 of
            0: t2 := t2 + t1;
            1: t2 := t2 + t1 * 32;
            2: t2 := t2 + t1 * 1024;
            end;
          end;
        if lastch < stringtablesize then
          begin
          lastch := lastch + 1;
          stringtable^[lastch] := ch;
          end
        else fatal(stringtableoverflow);
        getch;
        if (switchcounters[standard] > 0) and (ch in ['$', '_']) then
          warnat(badchar, lastline, chpos);
      until not (ch in ['$', '_', 'A'..'Z', 'a'..'z', '0'..'9']);
      with nexttoken do
        begin
        maptoken(token);
        if token = ident then
          begin
          hash := t2 mod hashtablesize;
          while (hashtable[hash].pos <> 0) and not (idfound or fatalflag) do
            hash := (hash + count) mod hashtablesize;
          with hashtable[hash] do
            if pos = 0 then
              begin
              insertions := insertions + 1;
              if insertions = hashtablesize then fatal(tablefull);
              pos := stringtabletop + 1;
              len := count;
              key := insertions;
              stringtabletop := lastch
              end;
          key := hashtable[hash].key;
          keypos := hashtable[hash].pos;
          end
        end;
      if switcheverplus[caseswitch] then convertingcase := true;
    end {identifier} ;





  procedure number;

{ Converts all number tokens.

  Determines the type of number being read and calls the correct
  routine to read it.

  Real numbers have token "realconstant" or "dblrealconst", and the
  actual value is returned in "realvalue".  Integers, of whatever
  base, have the token "intconstant", and the value is left in "intvalue".
}

    const
      binbase = 256; {binary base for conversions}
      maxproduct = 4095; {maximum base * binbase - 1}

      bytesize = 256; {number of elements in target byte}
      maxbytevalue = 255; {greatest single value in byte}
      maxinbuf = linelen; {buffer up to entire line length of digits}

    type
      digitindex = -1..maxinbuf;
      byte = 0..maxbytevalue;

      realstatus = (noerror, syntaxerror, underflowerr, overflowerr);
      realmodetype = 0..maxusword; {to specify the kind of real we want}

    var
      digits: array [digitindex] of byte; {actual digits read}
      length: digitindex; {length of number read}
      leadingzeros: digitindex; {number of leading zeros (for real fraction)}
      fill: digitindex; {index used to fill the digit array}
      draw: digitindex; {index used to extract ch values from the digit array}
      i: digitindex; {induction}
      radix: integer; {controls conversion radix for integers}

      realresult: realarray; {receives result of call to real conversion
                              routine}
      realerror: realstatus; {returns real conversion error code}
      realmode: realmodetype; {real number conversion control code}
      j: 1..maxrealwords; {induction}
      isdouble: boolean; {true if p2rdreal scans 'D' in real}


    procedure numbererror(err: warning);

{ Issue a warning message with a pointer in the middle of the number.
}


      begin {numbererror}
        with nexttoken do
          if lastline = line then warnat(err, line, (left + chpos - 1) div 2)
          else warnat(err, lastline, (2 + right) div 2)
      end {numbererror} ;




    procedure readdigits(skipzeros: boolean {skip and count leading zeros } );

{ Read digits into the digit array.  If "skipzeros" is true, then leading
  zeros are not significant.  They are counted in "leadingzeros" rather
  than inserted into the digit array.  Otherwise, all digit characters are
  converted to numerics and inserted into "digits" beginning at "fill" + 1.
}

      var
        digit: 0..15; {buffers the numeric value of "ch"}
        scanning: boolean;


      begin {readdigits}
        scanning := true;
        leadingzeros := 0;
        length := 0;

        while skipzeros and (ch = '0') do
          begin {scan leading zeros}
          leadingzeros := leadingzeros + 1;
          getch;
          end;

        while scanning do
          begin
          if (ch >= '0') and (ch <= '9') then digit := ord(ch) - ord('0')
          else if (ch >= 'a') and (ch <= 'f') then
            digit := ord(ch) - ord('a') + 10
          else scanning := false;

          if scanning and (digit < radix) then
            begin
            length := length + 1;
            fill := fill + 1;
            digits[fill] := digit;
            getch;
            end
          else scanning := false;
          end {while} ;

        if length + leadingzeros = 0 then numbererror(missingdigits);
      end {readdigits} ;




    procedure convertinteger(var value1: integer);

{ Interpret the digits according to the current radix, and return the value.
}

      type
        intarray = array [1..maxintarray] of integer;

      var
        valrec:
          record
            case boolean of
              false: (val: intarray); {used to loophole long integers}
              true: (valint: integer);
          end;

        i, j, head: digitindex;
        carry: byte;
        product: 0..maxproduct;
        digit: integer;


      begin {convertinteger}
        head := 1;

	{DRB loops 1 through maxinbuf if digitindex declared 0..maxinbuf and
	 length = 0 as FPC does an unsigned comparison which seems rude.}
        for i := 1 to length - 1 do
          begin
          carry := digits[i + 1];

          for j := i downto head do
            begin
            product := digits[j] * radix + carry;
            digits[j + 1] := product mod binbase;
            carry := product div binbase;
            end {for j} ;

          if carry = 0 then head := head + 1
          else digits[head] := carry;
          end {for i} ;

        if (length - head + 1 > targetintsize) or
           (length - head + 1 = targetintsize) and
           (switchcounters[standard] > 0) and
           (digits[head] >= bytesize div 2) then

          begin {fabricate a multi-precision "maxint"}
          valrec.val[1] := maxint;
          for i := 2 to maxintarray do valrec.val[i] := maxusint;
          numbererror(badinteger)
          end {integer error processing}

        else
          begin {transfer}
          j := 0; {now an index into "valrec.val"}
          digit := 0; {accumulates one host integer}

          for i := targetintsize - 1 downto 0 do
            begin

            if length - i >= head then {we've come to a byte to xfer}
              digit := (digit * 256) or digits[length - i];

            if i mod hostintsize = 0 then
              begin {move one host integer into target intarray}
              j := j + 1;
              valrec.val[j] := digit;
              digit := 0;
              end;

            end {for i} ;
          end {transfer} ;

        value1 := valrec.valint;
      end {convertinteger} ;



const

  maxrealwords  =     4 ;       { maximum number of words per real }
  maxrealbytes  =     8 ;       { maximum number of bytes per real }
  maxrealbits   =    64 ;       { maximum number of bits per real }




    function getrealch(first: boolean): char;

   {returns a character from the current number. This number
    may have been buffered partly in the array 'digits'.
   }


      begin {getrealch}
        if first then draw := 0;

        if leadingzeros <> 0 then
          begin
          getrealch := '0';
          leadingzeros := 0;
          end
        else if draw < fill then
          begin
          draw := draw + 1;
          getrealch := chr(digits[draw] + ord('0'));
          end
        else
          begin
          if draw > fill then getch
          else draw := draw + 1;
          getrealch := ch;
          end;
      end {getrealch} ;

procedure p2readreal(var result: realarray;
                     var errorcode: realstatus;
                     mode: realmodetype;
                     var isdouble: boolean);

  label
    1;

  const
    MaxInBuf = 804; { to support IEEE double with denormals }

  {
    == (leading binary zeroes) + (precision+1) - (leading decimal zeroes)

    DECsingle   ==      127     +  25   -   38  ==        114
    DECdouble   ==      127     +  57   -   38  ==        146

    IEEEsingle  == ( 126 + 23)  +  25   -   45  ==        129
    IEEEdouble  == (1022 + 52)  +  54   -  323  ==        805

      buffer size for formats with 15-bit exponent fields :

    DECquad(H)  ==      16383    + 114  - 4932  ==      11565

    IEEEextend  == (16382 +  63) +  65  - 4950  ==      11560
    IEEEquad    == (16382 + 111) + 113  - 4964  ==      11642
  }

    decbase = 10; { decimal radix }
    binbase = 256; { binary radix }
    maxproduct = 2559; { decbase * binbase - 1 }

    MaxExpon = 9999; { maximum exponent accepted during input scan }
    ExpLimit = 999; { (MaxExpon - 9) div 10 }


const
  {[f-]}
  bitsperbyte   =     8 ;       { number of bits per binary byte }

  nibblesize    =    16 ;       { number of elements in nibble }
{ wordsize      = 65536 }       { number of elements in word }

{ maxnibble     =    15 }       { nibblesize - 1 }
  maxbyte       =   255 ;       { bytesize - 1  }
  maxword       = 65535 ;       { wordsize - 1 }

  halfwordsize  = 32768 ;       { wordsize div 2 }
  halfmaxword   = 32767 ;       { maxword div 2 }
  {[f+]}

type

  {[f-]}
  word          = 0..maxword ;

  RealFormat    = ( DECformat
                  , IEEEformat
                  , INTELformat
                  , IBMformat
                  ) ;

  RealPrecision = ( SinglePrecision
                  , DoublePrecision
                  , QuadPrecision
                  ) ;

  RealRounding  = ( ToNearest
                  , ToZero
                  , ToPosInf
                  , ToNegInf
                  ) ;

  RealClass     = ( ZeroClass           { signed zero }
                  , NormalClass         { normalized operand }
                  , DenormalClass       { denormalized operand }
                  , InfClass            { signed Infinity }
                  , QNaNClass           { quiet NaN }
                  , SNaNClass           { signalling NaN }
                  ) ;
{[f+]}
  type

    BufferIndex = 0..MaxInBuf;
    DecExpType = - MaxExpon..MaxExpon;
    BinExpType = - halfmaxword..halfmaxword;

  var

    digits: packed array [BufferIndex] of byte;
    value: realarray; { local copy of final binary result }

    format: RealFormat; { }
    precision: RealPrecision; { }
    rounding: RealRounding; { }

    realbits: 1..maxrealbits; { total number of binary bits of significance }
    realbytes: 1..maxrealbytes; { number of bytes of significance }
    realwords: 1..maxrealwords; { number of words in target real }

    impliedbit: 0..1; { 1 if binary format uses "hidden bit" }
    pointoffset: 0..bitsperbyte; { 1 for IEEEformat, 0 for others }

    exponoffset: 0..bitsperbyte; { exponent offset within high byte }

    ch: char; { last char returned from "nextch" function }

    ExpValue: DecExpType; { value which follows "E" during scan }
    DecExp: DecExpType; { value of decimal exponent after scan }

    MaxDecExp: DecExpType; { maximum decimal exponent value }
    MinDecExp: DecExpType; { minimum decimal exponent value }

    MaxBinExp: BinExpType; { maximum unbiased exponent value }
    MinBinExp: BinExpType; { minimum unbiased exponent value }

    ExpBias: BinExpType; { exponent bias for this format }
    BinExp: BinExpType; { final unbiased binary exponent }

    NeedBits: BinExpType; { binary bits needed to finish conversion }

    ZeroBits: 0..bitsperbyte; { leading zero bits in high order byte }
    DenormBits: BinExpType; { number of bits to denormalize result by }

    negative: boolean; { true if minus sign appeared in input string }
    signed: boolean; { true if minus sign or plus sign encountered }
    expneg: boolean; { true if exponent sign is negative }
    StickyBit: boolean; { true if nonzero digits beyond buffer capacity }
    RoundBit: boolean; { true if the next bit past the LSB is non-zero }
    RoundUp: boolean; { true if rounding is applied to the fraction }
    Inexact: boolean; { true if the conversion is not exact }
    reversewds: boolean; { true if result is stored low word to hi word }
    denormalizing: boolean; { true if result may be gradually denormalized }

    product: 0..maxproduct;
    shifter: word;

    power: byte;
    carry: byte;
    temp: byte;

    dpt: BufferIndex; { index into digits[] when scanning input }
    i: BufferIndex; { temporary induction var }
    j: BufferIndex; { temporary induction var }

   { indices which delimit the integer and fraction parts of digits[] buffer }

    inthead: BufferIndex;
    inttail: BufferIndex;
    frachead: BufferIndex;
    fractail: BufferIndex;


  procedure getch;


    begin
      ch := getrealch(false);
    end { getch } ;


  function alfanumeric(ch: char): boolean;


    begin
      alfanumeric := (ch >= 'A') and (ch <= 'Z') or (ch >= 'a') and
                     (ch <= 'z') or (ch >= '0') and (ch <= '9');
    end { alfanumeric } ;


  function uppercase(ch: char): char;


    begin
      if (ch >= 'a') and (ch <= 'z') then
        uppercase := chr(ord(ch) - ord('a') + ord('A'))
      else uppercase := ch
    end { uppercase } ;


  function chEquals(c: char): boolean;


    begin
      if (uppercase(ch) = c) then
        begin
        getch;
        chEquals := true
        end
      else chEquals := false
    end { chEquals } ;


  procedure GetFractionByte(NeedBits: BinExpType);

{ Maintains the 2 indices, "frachead" and "fractail", which delimit the
  fraction string, and "StickyBit", which records the state of digits to
  the right of "fractail", which can no longer directly affect the result.
  "NeedBits" is the computed maximum number of decimal digits which are
  required to complete the binary conversion.  The output is one byte of
  binary stored in "carry".  This routine relocates the fraction string to
  the high end of the digit buffer.
}

    var
      src, dst: BufferIndex; { used to relocate the fraction string }


    begin

      while (digits[fractail] = 0) and (frachead <= fractail) do { zero
              suppress from right }
        fractail := fractail - 1;

      carry := 0;

      if (frachead + NeedBits <= fractail) then
        begin { adjust tail }
        fractail := frachead + NeedBits - 1;
        StickyBit := true { truncating non-zero digits }
        end; { adjust tail }

      if (fractail >= frachead) then
        begin
        dst := MaxInBuf;

        for src := fractail downto frachead do
          begin
          product := digits[src] * binbase + carry;
          digits[dst] := product mod decbase;
          carry := product div decbase;
          dst := dst - 1;
          end;

        frachead := dst + 1;
        fractail := MaxInBuf;
        end;

    end { GetFractionByte } ;


  procedure initialize;


    var
      i: integer;

    begin
    case (format) of

      IEEEformat, INTELformat:
        begin
        pointoffset := 1;
        denormalizing := true;
        if (format = INTELformat) then reversewds := true;

        case (precision) of
          SinglePrecision:
            begin
            realwords := 2;
            realbytes := 3;
            realbits := 24;

            ExpBias := 127;
            MaxBinExp := + 127;
            MinBinExp := - 126;
            MaxDecExp := + 39;
            MinDecExp := - 45;
            end { SinglePrecision } ;
          DoublePrecision:
            begin
            realwords := 4;
            realbytes := 7;
            realbits := 53;
            exponoffset := 4;

            ExpBias := 1023;
            MaxBinExp := + 1023;
            MinBinExp := - 1022;
            MaxDecExp := + 309;
            MinDecExp := - 323;
            end { DoublePrecision } ;
          end { case precision } ;
        end { IEEEformat } ;

      DECformat:
        begin
        pointoffset := 0;
        ExpBias := 128;
        MaxDecExp := + 39;
        MinDecExp := - 39;
        MaxBinExp := + 127;
        MinBinExp := - 127;
        case (precision) of
          SinglePrecision:
            begin
            realwords := 2;
            realbytes := 3;
            realbits := 24;
            end { SinglePrecision } ;
          DoublePrecision:
            begin
            realwords := 4;
            realbytes := 7;
            realbits := 56;
            end { SinglePrecision } ;
          end { case precision } ;
        end { DECformat } ;

      IBMformat:
        begin
        end { IBMformat } ;

      end { case format } ;

    for i := 1 to realwords do value[i] := 0;
    end;



  begin { p2readreal }

    isdouble := false; {default}

    { unpack the mode parameter word into constituent parts }

    rounding := RealRounding((mode mod nibblesize));

    precision := RealPrecision((mode div nibblesize) mod nibblesize);

    format := RealFormat((mode div bytesize) mod nibblesize);

    impliedbit := 1; { correct value for most formats }
    exponoffset := 7; { correct value for all single formats }
    reversewds := false; { correct value for all except iAPX86 }
    denormalizing := false; { correct value for non-IEEE formats }

    dpt := 0;
    DecExp := 0;
    BinExp := 0;
    StickyBit := false;
    errorcode := noerror;

    ch := getrealch(true);

    while (ch = ' ') do getch;

    negative := (ch = '-');

    signed := negative or (ch = '+');

    if signed then getch;

    if (ch < '0') or (ch > '9') then
      begin
      errorcode := syntaxerror;

      if (format = IEEEformat) or (format = INTELformat) then

        begin { permit certain keywords }

        if chEquals('I') then
          if chEquals('N') then
            if chEquals('F') then

              begin { scan Infinity syntax }
              { permit alternate spellings }
              if chEquals('I') then
                if chEquals('N') then
                  if chEquals('I') then
                    if chEquals('T') then if chEquals('Y') then getch;

              { test for proper termination }
              end { aliases }

            else { ch <> 'F' }
          else { ch <> 'N' }
        else { ch <> 'I' }

        if chEquals('N') then
          if chEquals('A') then
            if chEquals('N') then

              begin { check NaN syntax }
              { permit parenthesized argument }
              if chEquals('(') then while not chEquals(')') do getch;

              { test for proper termination }
              end { check NaN syntax }

            else { ch <> 'N' }
          else { ch <> 'A' }
        else { ch <> 'N' }
        end { if format = IEEEformat } ;

      goto 1
      end { special operands } ;

    { - - -   scan integer part   - - - }

    while (ch = '0') do getch;

    while (ch >= '0') and (ch <= '9') do
      begin
      if (dpt < MaxInBuf) then
        begin { insert integer digit }
        dpt := dpt + 1;
        digits[dpt] := ord(ch) - ord('0')
        end { insert integer digit }
      else { dpt => MaxInBuf }
        begin { buffer is full }
        DecExp := DecExp + 1;
        if (ch <> '0') then StickyBit := true
        end; { buffer is full }
      getch
      end { while ch is digit } ;

    DecExp := DecExp + dpt;

    { - - -   scan fraction part   - - - }

    if chEquals('.') then
      begin
      if (ch < '0') or (ch > '9') then errorcode := syntaxerror;

      if (dpt = 0) then { no integer part }
        while chEquals('0') do DecExp := DecExp - 1;

      while (ch >= '0') and (ch <= '9') do
        begin
        if (dpt < MaxInBuf) then
          begin { insert fraction digit }
          dpt := dpt + 1;
          digits[dpt] := ord(ch) - ord('0')
          end { insert fraction digit }
        else { dpt => MaxInBuf }
        if (ch <> '0') then StickyBit := true;
        getch
        end { while ch is digit }

      end { scan fraction part } ;

    { - - -   scan exponent part   - - - }

    ch := uppercase(ch);

    if (ch = 'E') or (ch = 'D') then
      begin { exponent scan }

      { It's a double constant, set the flag and precision.
      }
      if ch = 'D' then
        begin
        isdouble := true;
        precision := DoublePrecision;
        end;

      getch;
      expneg := (ch = '-');
      if expneg or (ch = '+') then getch;

      if (ch < '0') or (ch > '9') then errorcode := syntaxerror;

      ExpValue := 0;
      while (ch >= '0') and (ch <= '9') do
        begin
        if (ExpValue <= ExpLimit) then
          ExpValue := ExpValue * 10 + ord(ch) - ord('0');
        getch
        end;

      if expneg then DecExp := DecExp - ExpValue
      else DecExp := DecExp + ExpValue
      end { exponent scan } ;

    { Now that 'E' or 'D' is passed, we can set the exponent limits.
    }
    initialize;


    {----- scanning is complete; commence conversion -----}

    if (dpt > 0) then
      begin { conversion of non-zero string }
      inthead := 1;

      while (DecExp < dpt) { if a fraction part exists }
            and (digits[dpt] = 0) do
        dpt := dpt - 1; { then delete trailing zeroes }

      if (DecExp > 0) then
        begin { convert integer portion }

        if (DecExp > MaxDecExp) then
          begin { obvious exponent overflow }
          errorcode := overflowerr;
          DecExp := MaxDecExp;
          digits[1] := ord('9');
          end;

        while (DecExp > dpt) do
          begin { append trailing zeroes }
          dpt := dpt + 1;
          digits[dpt] := 0
          end; { append trailing zeroes }

        inttail := DecExp;
        frachead := DecExp + 1;
        fractail := dpt;

        { perform the integer conversion }

        for i := inthead to inttail - 1 do
          begin
          carry := digits[i + 1];

          for j := i downto inthead do
            begin
            product := digits[j] * decbase + carry;
            digits[j + 1] := product mod binbase;
            carry := product div binbase;
            end { for j } ;

          if carry = 0 then inthead := inthead + 1
          else digits[inthead] := carry;
          end { for i } ;

        BinExp := (inttail - inthead + 1) * bitsperbyte;

        end { convert integer portion }

      else { DecExp <= 0 }
        begin { the value has no integer part }

        if (DecExp < MinDecExp) then
          begin { obvious exponent underflow }
          errorcode := underflowerr;
          goto 1
          end;

        frachead := 1;
        fractail := dpt;
        BinExp := bitsperbyte;

        while (DecExp <= 0) do
          begin { fraction scaling }

          { It may not be necessary for all of the decimal fraction digits
          to take part in the conversion -- the exact number of decimal
          digits needed is equivalent to the number of bits in the binary
          result INCLUDING leading zero bits (plus a few for rounding).
          Rather than multiply the decimal exponent by log(2)10 (3.3219...)
          to compute leading binary zeroes, a simpler approximation of 3 3/8
          (3.375) is used, taking care to avoid overflow on 16 bit hosts, even
          when developing extended format reals with 15 bit exponent values.
          }

          NeedBits := - DecExp - DecExp - DecExp; { 3 * abs(DecExp) }

          GetFractionByte(NeedBits div 8 + NeedBits + realbits + 3);

          BinExp := BinExp - bitsperbyte; { adjust binary exponent }

          while (DecExp <> 0) and (carry <> 0) do
            begin
            frachead := frachead - 1;
            digits[frachead] := carry mod decbase;
            carry := carry div decbase;
            DecExp := DecExp + 1;
            end;

          if (carry <> 0) then { force termination of scaling }
            DecExp := DecExp + 1;
          end; { fraction scaling }

        { store first converted binary byte in low order end of buffer }

        digits[1] := carry;
        inttail := 1;
        end { DecExp <= 0 } ;


{ The number now has at least one byte of converted binary integer.
  Truncate the binary integer if there are excess bytes, or call
  GetFractionBytes for additional bytes if there are too few.  Also,
  count the number of significant bits in the high order byte.
}
      ZeroBits := bitsperbyte; { number of leading zero bits }
      temp := digits[inthead]; { the high order byte }

      repeat
        ZeroBits := ZeroBits - 1;
        temp := temp div 2
      until temp = 0;

      BinExp := BinExp - ZeroBits;
      { BinExp is now the correct unbiased binary exponent }

      if (inttail - inthead >= realbytes) then
        begin { truncate excess bytes }
        inttail := inthead + realbytes;
        frachead := inttail + 1;
        end { truncate excess bytes }

      else { inttail - inthead < realbytes }
        begin { generate additional bytes }
        NeedBits := (realbytes - (inttail - inthead) - 1) * bitsperbyte +
                    ZeroBits + 1;
        repeat
          GetFractionByte(NeedBits);
          inttail := inttail + 1;
          digits[inttail] := carry;
          NeedBits := NeedBits - bitsperbyte;
        until (NeedBits <= 0);
        end; { generate additional bytes }

      while (not StickyBit) and (frachead <= fractail) do
        if (digits[fractail] = 0) then fractail := fractail - 1
        else StickyBit := true;

{ the binary significand is now a (realbytes + 1) byte string
  starting at digits[inthead] and ending at digits[inttail].
  "StickyBit" is true if the conversion is inexact thus far.
}

      {-------- normalize or denormalize --------}

      DenormBits := 0;
      if (BinExp <= MinBinExp) then
        begin
        errorcode := underflowerr;
        DenormBits := MinBinExp - BinExp;
        BinExp := MinBinExp;
        impliedbit := 0;

        if (not denormalizing) then
          begin
          for i := inthead to inthead + realbytes do digits[i] := 0;
          goto 1
          end
        end;

      { compute bit offset multiplier }

      shifter := (realbits + ZeroBits - DenormBits) mod bitsperbyte;
      power := 1;
      for i := 1 to shifter do power := power + power;

      { compute byte offset index }

      temp := (bitsperbyte - 1) - (realbits - 1) mod bitsperbyte;
      i := inttail - (DenormBits + temp) div bitsperbyte;

      { test the need for an extra right shift }

      if (ZeroBits < shifter) then i := i - 1;

      j := i;

      { scan any bytes being discarded from tail end }

      while (j < inttail) do
        begin
        j := j + 1;
        if (digits[j] <> 0) then StickyBit := true;
        end { while } ;

      { simultaneously shift bits left and bytes right }

      carry := 0;
      while (i >= inthead) do
        begin
        shifter := digits[i] * power + carry;
        digits[j] := shifter mod binbase;
        carry := shifter div binbase;
        i := i - 1;
        j := j - 1;
        end { while } ;

      { flush out remainder of high order, if any }

      while (j >= inthead) do
        begin
        digits[j] := carry;
        carry := 0;
        j := j - 1;
        end { while } ;

      {-------- test for rounding --------}

      RoundBit := (digits[inttail] >= (binbase div 2));

      if (not StickyBit) then
        StickyBit := (digits[inttail] mod (binbase div 2) <> 0);

      Inexact := (RoundBit) or (StickyBit);

      case (rounding) of

        ToNearest:
          RoundUp := (RoundBit) and (StickyBit or odd(digits[inttail - 1]));
        ToZero: RoundUp := false;

        ToPosInf: RoundUp := (not negative) and (Inexact);

        ToNegInf: RoundUp := (negative) and (Inexact);

        end { case (rounding) } ;

      {-------- apply rounding --------}

      if (RoundUp) then
        begin
        carry := 1;
        i := inttail - 1;

        while (carry <> 0) and (i > inthead) do
          begin
          if (digits[i] = binbase - 1) then digits[i] := 0
          else
            begin
            digits[i] := digits[i] + 1;
            carry := 0;
            end;
          i := i - 1;
          end;

        if (carry <> 0) then
          begin { round up high order byte }
          temp := 0;
          for i := 1 to ((realbits - 1) mod bitsperbyte + 1) do
            temp := temp + temp + 1;
          if (digits[inthead] = temp) then
            begin
            digits[inthead] := (temp div 2) + 1;
            BinExp := BinExp + 1;
            end
          else
            begin
            digits[inthead] := digits[inthead] + 1;
            carry := 0;
            end;
          end; { round up high order byte }

        end { rounding } ;

      {-------- test for exponent range error --------}

      if (BinExp - pointoffset > MaxBinExp) then
        begin
        errorcode := overflowerr;
        BinExp := MaxBinExp;
        if (format = IEEEformat) or (format = INTELformat) then
          begin
          BinExp := BinExp + 1;
          temp := 0
          end
        else temp := maxbyte;
        for i := inthead to inthead + realbytes do digits[i] := temp;
        goto 1;
        end;

      {-------- packing --------}

      shifter := (BinExp + ExpBias - pointoffset - impliedbit);

      for i := 1 to exponoffset do shifter := shifter + shifter; { shift
        exponent left }

      value[1] := shifter + digits[inthead];

      j := inthead;

      for i := 2 to realwords do
        begin
        j := j + 2;
        value[i] := digits[j - 1] * binbase + digits[j];
        end;

      end {of non-zero value } ;

    {---- append sign and deliver result ----}

  1:
    if negative then value[1] := value[1] + halfwordsize;

    if (reversewds) then
      begin
      j := realwords;
      for i := 1 to realwords do
        begin
        result[i] := value[j];
        j := j - 1;
        end;
      end

    else { not reverse words }

      for i := 1 to realwords do result[i] := value[i];

  end { p2readreal } ;

    begin {number}
      fill := 0;
      radix := 10;

      readdigits(true); {fill digit array, skip leading zeros}

      with nexttoken do
        if ((ch = '.') and (nextch <> '.') and (nextch <> ')')) or
           (ch = 'e') or (ch = 'd') then
          begin
          token := realconst;
          case targetmachine of
            pdp11, vax: realmode := DECformat;
            iapx86, i80386, ns32k: realmode := INTELformat;
            mc68000: realmode := IEEEformat;
            otherwise
              begin
              write('unknown targetmachine ');
              compilerabort(inconsistent);
              realmode := DECformat;
              end;
            end;

          if switcheverplus[doublereals] then
            realmode := realmode + doubleprecision
          else realmode := realmode + singleprecision;

          for j := 1 to maxrealwords do realresult[j] := 0; {clear the result
            first}
          p2readreal(realresult, realerror, realmode, isdouble);
          realvalue := realresult;
          first_real_seen := true; {to detect $double error}
          case realerror of {test error code for abnormal conditions}
            noerror: ;
            syntaxerror: numbererror(missingdigits);
            underflowerr, overflowerr: numbererror(badexpon);
            end;

          if isdouble then
            if switchcounters[standard] <= 0 then token := dblrealconst
            else numbererror(badconsterr);
          end

        else
          begin {some flavor of integer}
          token := intconst;

          if ch = 'b' then
            begin
            if (switchcounters[standard] > 0) then numbererror(octalconst);
            getch;
            radix := 8;
            for i := 1 to length do {check for 8's or 9's}
              if digits[i] >= 8 then
                warnat(badoctal, lastline, chpos + i - length - 2);
            convertinteger(intvalue);
            end {octal 'b' form}

          else if ch = '#' then
            begin
            if (switchcounters[standard] > 0) then
              numbererror(nondecimalconst); {"...not standard Pascal"}

            convertinteger(radix);
            if (radix < 2) or (radix > 16) then
              begin
              radix := 16;
              numbererror(badradix); {"...must lie in range 2..16"}
              end;

            getch;
            fill := 0;
            readdigits(true);
            convertinteger(intvalue);
            end {non-decimal integer}

          else convertinteger(intvalue);

          if ch in ['a'..'z'] {as in "10div"}
             then
            numbererror(badnumber);

          end;
    end {number} ;




  begin {scantoken}
    somethingelse := false;
    if nextswitchread then
      begin
      switchcounters := nextswitchcounters;
      switcheverplus := nextswitcheverplus;
      nextswitchread := false;
      end;
    while not (somethingelse or endofinput) do
      case ch of

        ' ':
          begin
          skippingblanks := true;
          getch;
          skippingblanks := false;
          end;
        '{':
          begin
          commentch := '}';
          skipcomment
          end;

        '(':
          if nextch = '*' then
            begin
            commentch := ')';
            getch;
            skipcomment
            end
          else somethingelse := true;

        '/':
          if nextch = '*' then
            begin
            warnat(obsoletecomments, lastline, chpos);
            commentch := '/';
            getch;
            skipcomment;
            end
          else somethingelse := true;

        '%': scannerdirective(lastline, chpos);

        '$', '''', ')', '*', '+', ',', '-', '.', ':', ';', '<', '=', '>', '@',
        '#', '[', ']', '^', '_', '0', '1', '2', '3', '4', '5', '6', '7', '8',
        '9', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
        'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a',
        'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
        'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z':
          somethingelse := true;

        otherwise
          begin
          warnat(badchar, lastline, chpos);
          getch;
          end;
        end {case, while} ;

    if not first_token_seen and nextswitchread then
      { This is to set embedded switches prior to the first token.  It is
        especially important for the 68000, 68020 and 68881 embedded switches
        because of the processor type check that is below.
      }
      begin
      switchcounters := nextswitchcounters;
      switcheverplus := nextswitcheverplus;
      nextswitchread := false;
      end;

    first_token_seen := true;
    with nexttoken do
      begin
      line := lastline;
      left := chpos;
      filepos := current_filepos;

      if (endofinput or fatalflag) then token := eofsym
      else
        case ch of

          '.':
            begin
            if nextch = '.' then
              begin
              token := dotdot;
              getch;
              end
            else if nextch = ')' then
              begin
              token := rbrack;
              getch;
              end
            else token := dot;
            getch;
            end;

          '<':
            begin
            if nextch = '=' then
              begin
              token := leq;
              getch;
              end
            else if nextch = '>' then
              begin
              token := neq;
              getch;
              end
            else token := lss;
            getch;
            end;

          '>':
            begin
            if nextch = '=' then
              begin
              token := geq;
              getch;
              end
            else token := gtr;
            getch;
            end;

          ':':
            begin
            if nextch = '=' then
              begin
              token := becomes;
              getch;
              end
            else token := colon;
            getch;
            end;

          '(':
            begin
            if nextch = '.' then
              begin
              token := lbrack;
              getch;
              end
            else token := lpar;
            getch;
            end;

          ')', '*', '+', ',', '-', '/', ';', '=', '[', ']', '@', '^':
            begin
            token := tokentable[ch];
            getch;
            end;

          '0', '1', '2', '3', '4', '5', '6', '7', '8', '9': number;

          'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
          'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z':
            identifier;

          '$', '_':
            begin
            if (switchcounters[standard] > 0) then
              warnat(badchar, lastline, chpos);
            identifier;
            end;

          '''', '"': stringliteral(ch);

          '#':
            begin
            if (targetopsys <> msdos) or (switchcounters[standard] > 0) then
              warnat(badchar, lastline, chpos);
            charliteral;
            end;

          otherwise
            begin
            warnat(badchar, lastline, chpos);
            getch
            end
          end {case} ;
      if not endofline and (lastline > line) then
        begin
        left := 1;
        line := lastline
        end;
      right := oldchpos;
      fileindex := current_fileindex; { stringtable index of filename}
      end {with nexttoken} ;

  end {scantoken} ;



{**************************************************************}
{                                                              }
{    Initscanner -- initialize the scanner.                    }
{                                                              }
{    Enterstandardid -- builds entries in hashtable and string-}
{      table for each standard identifier (i.e. true,false).   }
{                                                              }
{    Enterresword -- builds entries in reswords and            }
{      and reswordtokens for tokens like BEGIN, END.           }
{                                                              }
{    Initscanswitches -- set up switch name table              }
{                                                              }
{    Inittokentable -- set up tokentable for all one letter    }
{      tokens.                                                 }
{                                                              }
{    Initreswords -- enter all reserved words, also sets       }
{      up reslentable                                          }
{                                                              }
{    Initstandardids -- uses enterstandardid to enter all      }
{      predefined identifiers.                                 }
{                                                              }
{**************************************************************}


procedure initscanner;

  var
    resindex: 0..reservedcount; {index in reslentable}
    i: hashindex; {general induction}
    t1, t2: 0..maxusword; {hash value temps}


  procedure enterstandardid(n: alfa;
                            newlen: identifierrange;
                            id: standardids);

    var
      hash: hashindex;
      i: identifierrange;


    begin {enterstandardid}
      insertions := insertions + 1;
      t2 := 0;
      for i := 1 to newlen do
        begin
        stringtable^[stringtabletop + i] := n[i];
        t1 := ord(n[i]) mod 32;
        case (i - 1) mod 3 of
          0: t2 := t2 + t1;
          1: t2 := t2 + t1 * 32;
          2: t2 := t2 + t1 * 1024;
          end;
        end;
      hash := t2 mod hashtablesize;
      while hashtable[hash].pos <> 0 do
        hash := (hash + newlen) mod hashtablesize;

      with hashtable[hash] do
        begin
        pos := stringtabletop + 1;
        len := newlen;
        key := insertions;
        end;

      stringtabletop := stringtabletop + newlen;
      standardidtable[id] := insertions;
    end {enterstandardid} ;


  procedure enterresword(resword: reswordtype;
                         restoken: tokentype);


    begin {enterresword}
      resindex := resindex + 1;
      reswords[resindex] := resword;
      reswordtokens[resindex] := restoken;
    end {enterresword} ;


  procedure initscanswitches;

    var
      i: scanswitchindex; {index}

   procedure initoneswitch(thiss: switch; {switch type}
                           thisn: switchname; {name}
                           thisv: switchvalue {bumpvalue});

      begin {initoneswitch}
      i := i + 1;
      with scanswitchtable[i] do
        begin
        internal := false;
        s := thiss;
        v := thisv;
        n := thisn;
        end;
      end {initoneswitch} ;


    begin {initscanswitches}
      i := 0;
      {must put the checking switches at the beginning}
      initoneswitch(rangecheck ,   'rangecheck    ', 1);
      initoneswitch(indexcheck,    'indexcheck    ', 1);
      initoneswitch(nilcheck,      'pointercheck  ', 1);
      initoneswitch(stackcheck,    'stackcheck    ', 1);
      initoneswitch(mathcheck,     'mathcheck     ', 1);
      initoneswitch(noswitch,      'check         ', 1);
      initoneswitch(doublereals,   'double        ', 1);
      initoneswitch(mainbody,      'main          ', 1);
      initoneswitch(own,           'own           ', 1);

{ The walkback, debug, and profile embedded switches have been disabled.
  They are still allowed, for source code compatibility, but they do nothing. }

      initoneswitch(walkback,      'walkback      ', 0);
             { 1 - ord(switcheverplus[profiling] or switcheverplus[debugging] or
                     switcheverplus[sharecode])); }
      initoneswitch(debugging,     'debug         ', 0);
                    { ord(switcheverplus[debugging])); }
      initoneswitch(debugging,     'profile       ', 0);
                    { ord (switcheverplus[profiling])); }

      if newdebugger then
        initoneswitch(listcount,     'list          ',
                      ord(switcheverplus[listcount]))
      else
        begin { PDB requires a complete listing file }
        if switcheverplus[debugging] or switcheverplus[profiling] then
          initoneswitch(listcount, 'list          ', 0) {ignore list/nolist}
        else initoneswitch(listcount, 'list          ',
                           ord(switcheverplus[listcount]));
        end;
      initoneswitch(shortintegers, 'shortints     ', 1);
      initoneswitch(fpp,           'fpp           ', 1);
      initoneswitch(oldpacking,    'oldpacking    ', 1);
      initoneswitch(standard,      'standard      ', 1);
      initoneswitch(stmtnumbers,   'stmtnum       ', 1);
      initoneswitch(details,       'details       ', 1);
      initoneswitch(pic,           'pic           ', 1);
      initoneswitch(caseswitch,    'case          ', 1);
      initoneswitch(multidef,      'multidef      ', 1);
      initoneswitch(framepointer,  'framepointer  ', 1);
      case targetmachine of
        vax:
          initoneswitch(pdp11data,     'pdp11         ', 1);
        iapx86:
          begin
          initoneswitch(bytealloc,     'bytealloc     ', 1);
          initoneswitch(bstep,         'bstep         ', 1);
          initoneswitch(codeconst,     'codeconst     ', 1);
          initoneswitch(groupown,      'groupown      ', 1);
          initoneswitch(largemodel,    'largemodel    ', 1);
          initoneswitch(truncatesw,    'truncate      ', 1);
          initoneswitch(windows,       'windows       ', 1);
          initoneswitch(commonvars,    'communal      ', 1);
          initoneswitch(cpu8086,       '8086          ', 1);
          initoneswitch(cpu8086,       '80286         ', 1);
          end;
        i80386:
          begin
          initoneswitch(codeconst,     'codeconst     ', 1);
          end;
        mc68000:
          begin
          initoneswitch(awaremode,     'aware         ', 1);
          if targetopsys = apollo then
            begin
            initoneswitch(sharecode,     'sharecode     ', 1);
            initoneswitch(usebsd42lib,   'bsd42lib      ', 1);
            initoneswitch(usesysVlib,    'sysvlib       ', 1);
            end
          else
            begin
            initoneswitch(cpu68000,      '68000         ', 1);
            initoneswitch(cpu68020,      '68020         ', 1);
            initoneswitch(fpc68881,      '68881         ', 1);
            end;
          if (targetopsys = vdos) or (targetopsys = unix) then
            initoneswitch(longlib,       'longlib       ', 1);
          end;
        end;

{DRB
      i := i + 1;
      scanswitchtable[i] :=
          scanswitchentry('ident         ', true, identsw);
      i := i + 1;
      scanswitchtable[i] :=
          scanswitchentry('module        ', true, modulesw);
      i := i + 1;
      scanswitchtable[i] :=
          scanswitchentry('codesect      ', true, codesectsw);
      i := i + 1;
      scanswitchtable[i] :=
          scanswitchentry('section       ', true, xsectionsw);
      i := i + 1;
      scanswitchtable[i] :=
          scanswitchentry('xxshortsection', true, xshortsectsw);
      if targetopsys = vdos then scanswitchtable[i].n := 'shortsection  ';
      i := i + 1;
      scanswitchtable[i] :=
          scanswitchentry('version       ', true, xversionsw);
}
      if i > maxscanswitch then
        begin
        write('scan switches init error');
        compilerabort(inconsistent);
        end;
    end {initscanswitches} ;


  procedure inittokentable;


    begin {inittokentable}
      tokentable[')'] := rpar;
      tokentable['*'] := star;
      tokentable['+'] := plus;
      tokentable[','] := comma;
      tokentable['-'] := minus;
      tokentable['.'] := dot;
      tokentable['/'] := slash;
      tokentable[':'] := colon;
      tokentable[';'] := semicolon;
      tokentable['<'] := lss;
      tokentable['='] := eql;
      tokentable['>'] := gtr;
      tokentable['['] := lbrack;
      tokentable[']'] := rbrack;
      tokentable['@'] := uparrow;
      tokentable['^'] := uparrow;
    end {inittokentable} ;

  procedure inittoklengths;

  begin
    toklengths[programsym] := 6;
    toklengths[labelsym] := 4;
    toklengths[constsym] := 4;
    toklengths[typesym] := 3;
    toklengths[varsym] := 2;
    toklengths[proceduresym] := 8;
    toklengths[functionsym] := 7;
    toklengths[uparrow] := 0;
    toklengths[arraysym] := 4;
    toklengths[filesym] := 3;
    toklengths[setsym] := 2;
    toklengths[recordsym] := 5;
    toklengths[stringsym] := 5;
    toklengths[univsym] := 3;
    toklengths[packedsym] := 5;
    toklengths[originsym] := 5;
    toklengths[beginsym] := 4;
    toklengths[ifsym] := 1;
    toklengths[casesym] := 3;
    toklengths[whilesym] := 4;
    toklengths[repeatsym] := 5;
    toklengths[forsym] := 2;
    toklengths[withsym] := 3;
    toklengths[gotosym] := 3;
    toklengths[usesym] := 2;
    toklengths[definesym] := 5;
    toklengths[sharedsym] := 5;
    toklengths[eql] := 0;
    toklengths[lss] := 0;
    toklengths[gtr] := 0;
    toklengths[neq] := 1;
    toklengths[leq] := 1;
    toklengths[geq] := 1;
    toklengths[insym] := 1;
    toklengths[plus] := 0;
    toklengths[minus] := 0;
    toklengths[orsym] := 1;
    toklengths[star] := 0;
    toklengths[slash] := 0;
    toklengths[divsym] := 2;
    toklengths[modsym] := 2;
    toklengths[andsym] := 2;
    toklengths[ofsym] := 1;
    toklengths[endsym] := 2;
    toklengths[elsesym] := 3;
    toklengths[thensym] := 3;
    toklengths[otherwisesym] := 8;
    toklengths[dosym] := 1;
    toklengths[untilsym] := 4;
    toklengths[tosym] := 1;
    toklengths[downtosym] := 5;
    toklengths[notsym] := 2;
    toklengths[at] := 0;
    toklengths[nilsym] := 2;
    toklengths[colon] := 0;
    toklengths[dot] := 0;
    toklengths[dotdot] := 1;
    toklengths[comma] := 0;
    toklengths[semicolon] := 0;
    toklengths[becomes] := 1;
    toklengths[lpar] := 0;
    toklengths[rpar] := 0;
    toklengths[lbrack] := 0;
    toklengths[rbrack] := 0;
    toklengths[intconst] := 0;
    toklengths[realconst] := 0;
    toklengths[dblrealconst] := 0;
    toklengths[charconst] := 0;
    toklengths[stringconst] := 0;
    toklengths[ident] := 0;
    toklengths[eofsym] := 0;
    toklengths[lineinc] := 0;
    toklengths[lineadd] := 0;
    toklengths[newfile] := 0;
  end;

  procedure initreswords;


    begin {initreswords}
      reslentable[2] := resindex + 1;
      enterresword('do       ', dosym);
      enterresword('if       ', ifsym);
      enterresword('in       ', insym);
      enterresword('of       ', ofsym);
      enterresword('or       ', orsym);
      enterresword('to       ', tosym);
      reslentable[3] := resindex + 1;
      enterresword('and      ', andsym);
      enterresword('div      ', divsym);
      enterresword('end      ', endsym);
      enterresword('for      ', forsym);
      enterresword('mod      ', modsym);
      enterresword('nil      ', nilsym);
      enterresword('not      ', notsym);
      enterresword('set      ', setsym);
      if not (switcheverplus[standard] or switcheverplus[oldreswords]) then
        enterresword('use      ', usesym);
      enterresword('var      ', varsym);
      reslentable[4] := resindex + 1;
      enterresword('case     ', casesym);
      enterresword('else     ', elsesym);
      enterresword('file     ', filesym);
      enterresword('goto     ', gotosym);
      enterresword('then     ', thensym);
      enterresword('type     ', typesym);
      if not switcheverplus[standard] then
        enterresword('univ     ', univsym);
      enterresword('with     ', withsym);
      reslentable[5] := resindex + 1;
      enterresword('array    ', arraysym);
      enterresword('begin    ', beginsym);
      enterresword('const    ', constsym);
      enterresword('label    ', labelsym);
      enterresword('until    ', untilsym);
      enterresword('while    ', whilesym);
      reslentable[6] := resindex + 1;
      if not (switcheverplus[standard] or switcheverplus[oldreswords]) then
        enterresword('define   ', definesym);
      enterresword('downto   ', downtosym);
      if not switcheverplus[standard] then
        enterresword('origin   ', originsym);
      enterresword('packed   ', packedsym);
      enterresword('record   ', recordsym);
      enterresword('repeat   ', repeatsym);
      if not (switcheverplus[standard] or switcheverplus[oldreswords]) then
        enterresword('shared   ', sharedsym);
      if not switcheverplus[standard] then
        enterresword('string   ', stringsym);
      reslentable[7] := resindex + 1;
      enterresword('program  ', programsym);
      reslentable[8] := resindex + 1;
      enterresword('function ', functionsym);
      reslentable[9] := resindex + 1;
      if not switcheverplus[standard] then
        enterresword('otherwise', otherwisesym);
      enterresword('procedure', proceduresym);
      reslentable[10] := resindex + 1;
    end {initreswords} ;


  procedure initstandardids;


    begin {initstandardids}
      enterstandardid('integer   ', 7, integerid);
      enterstandardid('real      ', 4, realid);
      enterstandardid('double    ', 6, doubleid);
      enterstandardid('char      ', 4, charid);
      enterstandardid('boolean   ', 7, booleanid);
      enterstandardid('true      ', 4, trueid);
      enterstandardid('false     ', 5, falseid);
      enterstandardid('text      ', 4, textid);
      enterstandardid('input     ', 5, inputid);
      enterstandardid('output    ', 6, outputid);
      enterstandardid('write     ', 5, writeid);
      enterstandardid('writeln   ', 7, writelnid);
      enterstandardid('read      ', 4, readid);
      enterstandardid('readln    ', 6, readlnid);
      enterstandardid('get       ', 3, getid);
      enterstandardid('put       ', 3, putid);
      enterstandardid('reset     ', 5, resetid);
      enterstandardid('rewrite   ', 7, rewriteid);
      enterstandardid('close     ', 5, closeid);
      enterstandardid('break     ', 5, breakid);
      enterstandardid('new       ', 3, newid);
      enterstandardid('dispose   ', 7, disposeid);
      enterstandardid('pack      ', 4, packid);
      enterstandardid('unpack    ', 6, unpackid);
      enterstandardid('abs       ', 3, absid);
      enterstandardid('sqr       ', 3, sqrid);
      enterstandardid('sin       ', 3, sinid);
      enterstandardid('cos       ', 3, cosid);
      enterstandardid('exp       ', 3, expid);
      enterstandardid('ln        ', 2, lnid);
      enterstandardid('sqrt      ', 4, sqrtid);
      enterstandardid('arctan    ', 6, arctanid);
      enterstandardid('odd       ', 3, oddid);
      enterstandardid('eof       ', 3, eofid);
      enterstandardid('eoln      ', 4, eolnid);
      enterstandardid('trunc     ', 5, truncid);
      enterstandardid('round     ', 5, roundid);
      enterstandardid('sngl      ', 4, snglid);
      enterstandardid('dbl       ', 3, dblid);
      enterstandardid('ord       ', 3, ordid);
      enterstandardid('chr       ', 3, chrid);
      enterstandardid('succ      ', 4, succid);
      enterstandardid('pred      ', 4, predid);
      enterstandardid('maxint    ', 6, maxintid);
      enterstandardid('seek      ', 4, seekid);
      enterstandardid('page      ', 4, pageid);
      enterstandardid('time      ', 4, timeid);
      enterstandardid('size      ', 4, sizeid);
      enterstandardid('bitsize   ', 7, bitsizeid);
      enterstandardid('upper     ', 5, upperid);
      enterstandardid('lower     ', 5, lowerid);
      enterstandardid('loophole  ', 8, loopholeid);
      if targetmachine = pdp11 then enterstandardid('emt       ', 3, emtid);
      enterstandardid('ref       ', 3, refid);
      enterstandardid('noioerror ', 9, noioerrorid);
      enterstandardid('ioerror   ', 7, ioerrorid);
      enterstandardid('iostatus  ', 8, iostatusid);
      enterstandardid('delete    ', 6, deleteid);
      enterstandardid('rename    ', 6, renameid);
      enterstandardid('forward   ', 7, forwardid);
      enterstandardid('external  ', 8, externalid);
      enterstandardid('nonpascal ', 9, nonpascalid);

      if targetmachine <> iapx86 then
        enterstandardid('interrupt ', 9, interruptid);

      if targetopsys = msdos then
        enterstandardid('fortran   ', 7, fortranid);

      enterstandardid('minint    ', 6, minintid);
      enterstandardid('shortint  ', 8, shortintid);
      enterstandardid('insert    ', 6, insertid);
      enterstandardid('str       ', 3, strid);
      enterstandardid('val       ', 3, valprocid);
      enterstandardid('copy      ', 4, copyid);
      enterstandardid('concat    ', 6, concatid);
      enterstandardid('length    ', 6, lengthid);
      enterstandardid('pos       ', 3, posid);
      enterstandardid('deletestr ', 9, deletestrid);

      if targetmachine = mc68000 then
        begin
        enterstandardid('facos     ', 5, facosid);
        enterstandardid('fasin     ', 5, fasinid);
        enterstandardid('fatan     ', 5, fatanid);
        enterstandardid('fatanh    ', 6, fatanhid);
        enterstandardid('fcosh     ', 5, fcoshid);
        enterstandardid('fetoxm1   ', 7, fetoxm1id);
        enterstandardid('fgetexp   ', 7, fgetexpid);
        enterstandardid('fgetman   ', 7, fgetmanid);
        enterstandardid('fint      ', 4, fintid);
        enterstandardid('flog10    ', 6, flog10id);
        enterstandardid('flog2     ', 5, flog2id);
        enterstandardid('flognp1   ', 7, flognp1id);
        enterstandardid('fmod      ', 4, fmodid);
        enterstandardid('frem      ', 4, fremid);
        enterstandardid('fscale    ', 6, fscaleid);
        enterstandardid('fsgldiv   ', 7, fsgldivid);
        enterstandardid('fsglmul   ', 7, fsglmulid);
        enterstandardid('fsinh     ', 5, fsinhid);
        enterstandardid('ftan      ', 4, ftanid);
        enterstandardid('ftanh     ', 5, ftanhid);
        enterstandardid('ftentox   ', 7, ftentoxid);
        enterstandardid('ftwotox   ', 7, ftwotoxid);
        enterstandardid('fsincos   ', 7, fsincosid);
        enterstandardid('fmovecr   ', 7, fmovecrid);
        enterstandardid('setfpcr   ', 7, setfpcrid);
        enterstandardid('readfpcr  ', 8, readfpcrid);
        end;
    end {initstandardids} ;


  function rdup(i: integer): integer;

  { Round size.
  }

    begin {rdup}
      case hostmachine of
        mc68000, i80386: if odd(i) then rdup := i + 1 else rdup := i;
        iapx86: rdup := i; {alignment only if non-packed structures}
        otherwise rdup := i;
        end;
    end {rdup} ;


  begin {initscanner}

    { This code checks certain configuration parameters and reports any
      potential problems. }

    { Check the sizes environment file components. }

    if switchesperblock + 1 <>
       (diskbufsize + 1) div rdup(sizeof(switchblock)) then
      writeln('Environment files:  SWITCHESPERBLOCK should be ',
             (diskbufsize + 1) div rdup(sizeof(switchblock)) - 1: 1);

    if hashtableentriesperblock + 1 <>
       (diskbufsize + 1) div rdup(sizeof(hashtableblock)) then
      writeln('Environment files:  HASHTABLEENTRIESPERBLOCK should be ',
             (diskbufsize + 1) div rdup(sizeof(hashtableblock)) - 1: 1);

    if proctableentriesperblock + 1 <>
       (diskbufsize + 1) div rdup(sizeof(proctableentry)) then
      writeln('Environment files:  PROCTABLEENTRIESPERBLOCK should be ',
             (diskbufsize + 1) div rdup(sizeof(proctableentry)) - 1: 1);

    { End of special configuration checks}

    curstringbuf := true;
    nextswitchread := false;

    stringfilecount := 0;
    stringtablelimit := 0;
    stringtabletop := 0;
    curstringblock := 1;
    new(stringblkptr);
    stringblkptrtbl[1] := stringblkptr;
    for i := 2 to maxstringblks do stringblkptrtbl[i] := nil;
    new(stringtable);

    initscanswitches;
    inittokentable;
    inittoklengths;

    for i := 0 to hashtablesize do
      begin
      hashtable[i].pos := 0;
      hashtable[i].len := 0;
      end;

    resindex := 0;
    initreswords;

    insertions := 0;

    initstandardids;

    incomment := false;
    inliteralstring := false;
    charcount := 0;
    lastline := 1;
    lasttokenline := 1;
    baseline[1] := 0;
    nexttoken.baseline := 0;
    lastbaseline := - 1;
    tokenbufindex := 0;
    convertingcase := true;
    endofinput := false;
    linesize := 0;
    linepos := inputbufsize;
    skippingblanks := false;

    lastswitch := 0;

    nextstringfile := 0;
    filerememberlist := nil;

    { Initialize some switches }
    first_real_seen := false; {used to detect error when $double occurs after
                               first real constant.}
    first_token_seen := false; {used to detect error when $case occurs after
                               first token is scanned.}
    shortsection := false;
    codesection := oursection; {default code section}
    identstring := 0;
    identstrlength := 0;
    objversion := 0;
    objrevision := 0;
    codesect_string := 0; { codesect name}
    codesect_strlength := 0; {length of codesect name}
    module_string := 0; { module name}
    module_strlength := 0; {length of module name}
    ownsect_string := 0; { ownsect name}
    ownsect_strlength := 0; {length of ownsect name}
    ident_string := 0; { ident name}
    ident_strlength := 0; {length of ident name}

    {init the upper to lower case conversion table}

    mapchars['A'] := 'a';
    mapchars['B'] := 'b';
    mapchars['C'] := 'c';
    mapchars['D'] := 'd';
    mapchars['E'] := 'e';
    mapchars['F'] := 'f';
    mapchars['G'] := 'g';
    mapchars['H'] := 'h';
    mapchars['I'] := 'i';
    mapchars['J'] := 'j';
    mapchars['K'] := 'k';
    mapchars['L'] := 'l';
    mapchars['M'] := 'm';
    mapchars['N'] := 'n';
    mapchars['O'] := 'o';
    mapchars['P'] := 'p';
    mapchars['Q'] := 'q';
    mapchars['R'] := 'r';
    mapchars['S'] := 's';
    mapchars['T'] := 't';
    mapchars['U'] := 'u';
    mapchars['V'] := 'v';
    mapchars['W'] := 'w';
    mapchars['X'] := 'x';
    mapchars['Y'] := 'y';
    mapchars['Z'] := 'z';

  end {initscanner} ;




procedure scan1;

{ Init scanner, read first char, etc.
}


  begin {scan1}
    initscanner;
    sourcelevel := 1;
    curfile := 1;
    opennext;
    loginputfilename;

    nextch := ' ';
    getch;
  end {scan1} ;


procedure scan2;


  begin {scan2}
    if scanalys then seekstringfile(stringfilecount);

    dumpidentifiers;
    closes;
    sourcelevel := 0;

    with nexttoken do
      begin
      token := eofsym;
      line := lastline;
      left := chpos;
      right := chpos
      end;

    stringtablelimit := stringfilecount + stringtabletop;

    dispose(stringtable);
  end {scan2} ;

procedure closeall;

{ Close all source files.
}


  begin {closeall}
    while sourcelevel > 0 do
      begin
      close(source[sourcelevel]);
      sourcelevel := sourcelevel - 1;
      end;
  end {closeall} ;


procedure scan;

{ Main scan procedure, see forward declaration for more data
}


  begin {scan}

    scan1;

    repeat
      scantoken;
    until (fatalflag or endofinput);

    { align strings }

    if not switcheverplus[defineswitch] then
      while stringfilecount mod stringroundoff <> 0 do
        begin
        stringblkptr^[nextstringfile] := 0;
        putstringfile;
        stringfilecount := stringfilecount + 1;
        end;

    scan2;

  end {scan} ;

end.
