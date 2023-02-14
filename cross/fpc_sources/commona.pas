{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Syntax/Semantic Analysis Common Routines

 Last modified by KRIS on 21-Nov-1990 15:18:37
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

unit commona;

interface

uses config, hdr, utils, a_t, scan, hdra,mda;

procedure gettoken;

procedure warnbefore(err: warning {Error message number} );

{ Generate an error message at the center of the last token.
}

procedure warnbetween(err: warning {Error message number} );

{ Generate an error message half way between the last token and the
  current token.
}

procedure warn(err: warning {Error message number} );

{ Generate an error message in the middle of the current token.
}

procedure warnnonstandard(err: warning {Error message number} );

{ Generate a warning only if the standard switch is set.
  Used to warn of non-standard features.
}

procedure fatal(err: warning {Error message number} );

{ Generate a fatal warning which will terminate the compilation.
}

procedure putbyte(a: integer {value of byte to access} );

{ Write the byte "a" to the next location in the string file.  This
  is assumed to be added to the constant table, and the global
  "consttablelimit" is incremented as a result.
}

procedure genform(f: types {form to emit} );

{ If no errors found so far, emit a form to the intermediate file.
}


procedure genint(i: integer {value to emit} );

{ If no errors found so far, emit an integer value to the intermediate file.
  Since each intermediate file element is only in the range 0..255 (one byte),
  multiple elements are used.

  Note that only unsigned integers are emitted.
}


procedure genop(o: operatortype {operator to emit} );

{ If no errors are found so far, emit an operator to the intermediate file.
}


procedure genstmt(s: stmttype {statement to emit} );

{ If no errors are found so far, emit a statement to the intermediate file.
}

procedure verify(set1: tokenset; {acceptable tokens}
                 set2: tokenset; {tokens to finish skip}
                 err: warning {Message if token not ok} );

{ Check that the current token is in set1, emit an error message
  "err" if not, and skip until the token is in set1 + set2 +
  neverskipset.  The error message will be placed as close
  to the estimated location of the token as possible.
}

procedure verify1(set1: tokenset; {acceptable tokens}
                  err: warning {message if token not ok} );

{ Same as verify, except no separate skip set is provided.
}

procedure verifytoken(tok: tokentype; {acceptable token}
                      err: warning {message if token not ok} );

{ Check for a given token (tok) and skip it if found.  If not
  found, emit an error message set by "err".  This is used for
  redundant tokens in the syntax, where parsing can continue if it
  is missing.
}

procedure enterform(newtyp: types; {type for this form}
                    var where: index; {new entry}
                    var whereptr: entryptr {for access to new entry} );

{ Enter a new formentry at the current level.  This also
  gets a formnumber for use with the debugger, and sets the type
  to be newtyp.
}

procedure searchsection(id: scoperange; {scope id for search}
                        var wherefound: index {resulting name index} );

{ Search the symbol table for the current token identifier in scope
  "id".  If found, the name index will be placed in "wherefound".  If not
  found, zero will be returned. If the identifier found proves to be
  a constant or type identifier we will update "lastoccurrence" within
  the symbol table entry to allow enforcement of scope restrictions.
}


procedure searchlsection(value1: integer; {label value}
                         labellist: labelptr; {root of label list}
                         var wherefound: labelptr {result of search} );

{ Search a list of labels starting at "labellist" for a label with the
  value "value".  The result is returned in "wherefound".  If the label
  is not in the list, the returned entry will be "labelflag".

  "Labelflag" is set to the desired value to simplify the search algorithm.
}


procedure searchlabels(value1: integer; {label value}
                       var wherefound: labelptr {result of search} );

{ Search all available scopes for a label with "value", returning the
  result in "wherefound."  The result will be set to "labelflag"
  if the label cannot be found.
}

procedure search(var wherefound: index {result of search} );

{ Search all available scopes for the current token.  The result is
  returned in "wherefound", with zero indicating no find.  The global
  variable "lev" is set to the level where the token was found.
}

procedure searchvariants(var currentrecord: index; {record to search}
                         labvalue: operand {varnt label value} );

{ Search a record variant part starting at "currentrecord" for a
  variant with a label of "labvalue" and set "currentrecord" to that
  variant.  If there is no variant with the desired label,
  "currentrecord" is unmodified, and an error message is emitted.
}

{ Utilities for use with types}


procedure stripsubrange(var objectindex: index {form to be stripped} );

{ Convert a subrange type to the base type for use in an expression.
}

function forcealign(size: addressrange; {value to align}
                    alignment: addressrange; {requirement}
                    packedresult: boolean {size is in bits} ): addressrange;

{ Forces "size" to the next higher multiple of "alignment".
  Used to overcome limitations built into much contemporary hardware.
}

function unsigned(f: entryptr; {type to check}
                  len: addressrange; {space actually allocated for var}
                  packedelement: boolean {set if packed var} ): boolean;

{ Returns true if the values of type "f" are unsigned.
  If "len" is not equal to the space required for the value, it is being
  allocated a space larger than required, and should be treated as signed
  or unsigned for unpacking, depending on the global "unsignedprefered".
}

function simplesize(i: integer {value to find size of} ): integer;

{ Returns the size in multiples of addressing units needed to contain
  the value of i.
}


function negaterealconst(realbuffer: realarray {real constant value} ):
 realarray;

 { function to negate a real constant independent of the host }


procedure constant(follow: tokenset; {legal following symbols}
                   dumpvalue: boolean; {true says dump string}
                   var value1: operand {resulting constant} );

{ Syntactic routine to parse a constant.

  productions:

  constant = [ sign ] (unsigned-number | constant-identifier) |
         character-string | structured-constant  .

  If the constant is a simple constant, the actual value is returned in
  "value", otherwise a pointer to the string file is returned.  Constant
  structures are added to the "consttable" portion of the string file
  if they are longer than an integer value, otherwise they are returned
  as an integer value.
}

function getform(objecttype: entryptr {desired form} ): types;

{ Get the basic form associated with a type.
}


function identical(left, right: index {types to compare} ): boolean;

{ True if two types are identical, or if either is undefined (to avoid
  redundant messages).
}

function compatible(left, right: index {types to compare} ): boolean;

{ True if the types represented by the two input forms are compatible
  as defined by the Pascal standard.  If either input is undefined,
  they are assumed to be compatible to eliminate redundant error
  messages.
}

procedure seekstringfile(n: integer {byte to access} );

{ Do the equivalent of a "seek" on the string file.  This sets the
  file and "nextstringfile" to access byte "n" of the stringfile.
}

procedure getstringfile;

{ Do the equivalent of a get on the stringfile.

  The string file contains constant data collected by SCAN and
  ANALYS.  It is organized as blocks containing arrays 0..
  diskbufsize of bytes.  The string file is always accessed
  as stringblkptr^[nextstringfile].
}

function do_hash(charindex:integer; charlen:integer) : integer;
{perform a hashing of the entry of length "charlen" at the location "charindex"
 in the stringtable, using the function concealed inside the debugger package.}

implementation

procedure warnbefore (err: warning {Error message number} ) ;

{ Generate an error message at the center of the last token.
}


  begin {warnbefore}
    emitflag := false;
    with lasttoken do warnat(err, line, (left + right) div 2)
  end {warnbefore} ;


procedure warnbetween(err: warning {Error message number} );

{ Generate an error message half way between the last token and the
  current token.
}


  begin {warnbetween}
    emitflag := false;
    with lasttoken do
      if line = thistoken.line then
        warnat(err, line, (right + thistoken.left + 1) div 2)
      else warnat(err, line, min(right + 1, linelen))
  end {warnbetween} ;


procedure warn(err: warning {Error message number} );

{ Generate an error message in the middle of the current token.
}


  begin {warn}
    emitflag := false;
    with thistoken do warnat(err, line, (left + right) div 2)
  end {warn} ;


procedure warnnonstandard(err: warning {Error message number} );

{ Generate a warning only if the standard switch is set.
  Used to warn of non-standard features.
}


  begin {warnnonstandard}
    if (switchcounters[standard] > 0) then warn(err)
  end {warnnonstandard} ;


procedure fatal(err: warning {Error message number} );

{ Generate a fatal warning which will terminate the compilation.
}


  begin {fatal}
    fatalflag := true;
    warn(err);
  end {fatal} ;


procedure gettoken;

{ Read next lexical token

  This is one of the most popular procedures in ANALYS.  What it
  does is read the "next" token.  Several global level variables
  are altered by this routine:
      thistoken -- Receives the "next" token from the token file
      lasttoken -- Receives previous value of thistoken
      token     -- Receives tokentype of thistoken
      nexttoken -- Receives tokentype of what will be the next
                   token read from the token file.  This provides
                   a one token "look ahead" for ANALYS.

  If scanalys is true, SCAN's scantoken routine is
  called directly, otherwise tempfileone is read.
}


  begin {gettoken}
    lasttoken := thistoken;
    thistoken := nexttoken;
    token := thistoken.token;
    if token <> eofsym then scantoken;
  end {gettoken} ;



{ String File Processing

  The string file contains constant data collected by scan and analys.
  It is organized in blocks, each consisting of:

    array [0..diskbufsize] of byte

  The string file is always accessed as stringblkptr^[nextstringfile];

  The following routines manipulate this file.
}

procedure putbyte(a: integer {value of byte to access});

{ Write the byte "a" to the next location in the string file.  This
  is assumed to be added to the constant table, and the global
  "consttablelimit" or "stringfilecount" is incremented as a result.
}


  begin {putbyte}
    stringfilecount := stringfilecount + 1;
    stringblkptr^[nextstringfile] := a;
    putstringfile;
  end {putbyte} ;



{ Intermediate File Output

  DRB not true for FPC
  The intermediate file is the interface to the next pass (travrs),
  and consists of blocks of undiscriminated variant records.  The
  type of the next record is determined strictly by context.

  The following routines write the different kinds of values to this
  file.

  As with all files in the compiler, this one is blocked, and the next
  element is always accessed as:

    tempfiletwo^[nextintcode]

  The global flag "emitflag", which is set false if errors are detected,
  controls the actual emission of output to this file.
}

procedure genform (f: types {form to emit});

{ If no errors found so far, emit a form to the intermediate file.
}


  begin {genform}
    if emitflag then
      begin
      tempfilebuf.intcode := form;
      tempfilebuf.f := f;
      write(tempfiletwo, tempfilebuf);
      end;
  end {genform} ;


procedure genint(i: integer {value to emit} );

{ If no errors found so far, emit an integer value to the intermediate file.
  Since each intermediate file element is only in the range 0..255 (one byte),
  multiple elements are used.

  Note that only unsigned integers are emitted.
}

  var
    { This fudges an integer into bytes.  The constant "32" is }
    { simply a large enough number to include all probable systems. }
    fudge:
      record
        case boolean of
          true: (int: integer);
          false: (byte: packed array [1..32] of hostfilebyte);
      end;
    j: 1..32; {induction var}


  begin {genint}
    if emitflag then
      tempfilebuf.intcode := literal;
      if (i >= 0) and (i < hostfilelim) then
        begin
        tempfilebuf.b := i;
        write(tempfiletwo, tempfilebuf);
        end
      else
        begin
        tempfilebuf.b := hostfilelim;
        write(tempfiletwo, tempfilebuf);;
        fudge.int := i;
        for j := 1 to hostintsize * hostfileunits do
          begin
          tempfilebuf.b := fudge.byte[j];
          write(tempfiletwo, tempfilebuf);;
          end;
        end;
  end {genint} ;


procedure genop (o: operatortype {operator to emit} );

{ If no errors are found so far, emit an operator to the intermediate file.
}


  begin {genop}
    if emitflag then
      begin
      tempfilebuf.intcode := op;
      tempfilebuf.o := o;
      write(tempfiletwo, tempfilebuf);
      end;
  end {genop} ;


procedure genstmt (s: stmttype {statement to emit} );

{ If no errors are found so far, emit a statement to the intermediate file.
}


  begin {genstmt}
    if emitflag then
      begin
      if intstate = opstate then
        begin
        genop(endexpr);
        intstate := stmtstate
        end;
      tempfilebuf.intcode := stmt;
      tempfilebuf.s := s;
      write(tempfiletwo, tempfilebuf);
      end
  end {genstmt} ;




procedure verify(set1: tokenset; {acceptable tokens}
                 set2: tokenset; {tokens to finish skip}
                 err: warning {Message if token not ok} );

{ Check that the current token is in set1, emit an error message
  "err" if not, and skip until the token is in set1 + set2 +
  neverskipset.  The error message will be placed as close
  to the estimated location of the token as possible.
}

  var
    skipset: tokenset; {Tokens which end skipping}


  begin {verify}
    if not (token in set1) then
      begin
      skipset := set1 + set2 + neverskipset;
      if token in skipset then warnbetween(err)
      else
        begin
        warn(err);
        while not (token in skipset) do gettoken
        end
      end
  end {verify} ;


procedure verify1(set1: tokenset; {acceptable tokens}
                  err: warning {message if token not ok} );

{ Same as verify, except no separate skip set is provided.
}


  begin {verify1}
    verify(set1, [], err);
  end {verify1} ;


procedure verifytoken(tok: tokentype; {acceptable token}
                      err: warning {message if token not ok} );

{ Check for a given token (tok) and skip it if found.  If not
  found, emit an error message set by "err".  This is used for
  redundant tokens in the syntax, where parsing can continue if it
  is missing.
}


  begin {verifytoken}
    if token = tok then gettoken
    else warnbetween(err)
  end {verifytoken} ;



procedure enterform(newtyp: types; {type for this form}
                    var where: index; {new entry}
                    var whereptr: entryptr {for access to new entry} );

{ Enter a new formentry at the current level.  This also
  gets a formnumber for use with the debugger, and sets the type
  to be newtyp.
}

  begin {enterform}
    if tabletop = tablesize then fatal(tablefull)
    else tabletop := tabletop + 1;
    where := tabletop;
    if bigcompilerversion then whereptr := @(bigtable[tabletop]);
    with whereptr^ do
      begin
      if not newdebugger and (newtyp in
         [subranges, scalars, fields, arrays, sets, files, ptrs, ints, bools,
         chars, reals, doubles, conformantarrays, strings]) then
        begin
        lastdebugrecord := lastdebugrecord + 1;
        dbgsymbol := lastdebugrecord;
        end
      else dbgsymbol := 0;
      form := true;
      typ := newtyp;
      containsfile := false;
      packedflag := false;
      bitaddress := false;
      extendedrange := false;
      disposable := false;
      end;
  end {enterform} ;



procedure searchsection(id: scoperange; {scope id for search}
                        var wherefound: index {resulting name index} );

{ Search the symbol table for the current token identifier in scope
  "id".  If found, the name index will be placed in "wherefound".  If not
  found, zero will be returned. If the identifier found proves to be
  a constant or type identifier we will update "lastoccurrence" within
  the symbol table entry to allow enforcement of scope restrictions.
}

  var
    p: entryptr; {used for name table access}
    twherefound: index; {temp for wherefound during procedure}


  begin {searchsection}
    twherefound := keymap[thistoken.key];
    if bigcompilerversion then p := @(bigtable[twherefound]);
    while (twherefound <> 0) and (p^.name <> id) do
      begin
      twherefound := p^.nextname;
      if bigcompilerversion then p := @(bigtable[twherefound]);
      end;
    if not probing and (twherefound <> 0) and
       (p^.lastoccurrence < display[displaytop].scopeid) and
       (p^.namekind in
       [procname, funcname, constname, typename, standardproc, standardfunc, undeftypename, fieldname, undefname]) then
      begin
      if not bigcompilerversion then blocksin[1].written := true;
      p^.lastoccurrence := display[displaytop].scopeid;
      end;
    wherefound := twherefound;
  end {searchsection} ;



procedure searchlsection(value1: integer; {label value}
                         labellist: labelptr; {root of label list}
                         var wherefound: labelptr {result of search} );

{ Search a list of labels starting at "labellist" for a label with the
  value "value".  The result is returned in "wherefound".  If the label
  is not in the list, the returned entry will be "labelflag".

  "Labelflag" is set to the desired value to simplify the search algorithm.
}

  begin {searchlsection}
    labelflag^.labelvalue := value1;
    wherefound := labellist;
    while wherefound^.labelvalue <> value1 do
      wherefound := wherefound^.nextlabel;
  end {searchlsection} ;


procedure searchlabels(value1: integer; {label value}
                       var wherefound: labelptr {result of search} );

{ Search all available scopes for a label with "value", returning the
  result in "wherefound."  The result will be set to "labelflag"
  if the label cannot be found.
}

  var
    i: levelindex; {induction var for level search}


  begin {searchlabels}
    i := level;
    repeat
      searchlsection(value1, display[i].labellist, wherefound);
      i := i - 1;
    until (i = 0) or (wherefound <> labelflag);
    lev := i + 1;
  end {searchlabels} ;

procedure search(var wherefound: index {result of search} );

{ Search all available scopes for the current token.  The result is
  returned in "wherefound", with zero indicating no find.  The global
  variable "lev" is set to the level where the token was found.
}

  var
    i: levelindex; {induction var for level search}
    t: index; {temp result of search for each level}


  begin {search}
    i := displaytop + 1;
    repeat
      i := i - 1;
      searchsection(display[i].blockid, t);
    until (i = 0) or (t <> 0);
    lev := i;
    wherefound := t;
  end {search} ;

procedure searchvariants(var currentrecord: index; {record to search}
                         labvalue: operand {varnt label value} );

{ Search a record variant part starting at "currentrecord" for a
  variant with a label of "labvalue" and set "currentrecord" to that
  variant.  If there is no variant with the desired label,
  "currentrecord" is unmodified, and an error message is emitted.
}

  var
    t: index; {used to trace variant chain}
    t1: index; {used to trace label chain}
    t2: index; {holds last value of t for later use}
    ptr, ptr1: entryptr; {used to access variant and label chains}
    found: boolean; {set if label found, controls search}


  begin {searchvariants}
    found := false;
    if bigcompilerversion then ptr := @(bigtable[currentrecord]);
    t := ptr^.firstvariant;
    while (t <> 0) and not found do
      begin
      if bigcompilerversion then ptr := @(bigtable[t]);
      with ptr^ do
        begin
        t2 := t;
        t := nextvariant;
        t1 := firstlabel;
        while (t1 <> 0) and not found do
          begin
          if bigcompilerversion then ptr1 := @(bigtable[t1]);
          with ptr1^ do
            begin
            found := (labvalue.cvalue.intvalue = varlabvalue);
            t1 := nextvarlab;
            end;
          end;
        end;
      end;
    if found then currentrecord := t2
    else warnbefore(badtagerr);
  end {searchvariants} ;



{ Utilities for use with types}

procedure stripsubrange(var objectindex: index {form to be stripped} );

{ Convert a subrange type to the base type for use in an expression.
}

  var
    ptr: entryptr;


  begin {stripsubrange}
    if bigcompilerversion then ptr := @(bigtable[objectindex]);
    with ptr^ do if (typ = subranges) then objectindex := parenttype;
  end {stripsubrange} ;

function forcealign(size: addressrange; {value to align}
                    alignment: addressrange; {requirement}
                    packedresult: boolean {size is in bits} ): addressrange;

{ Forces "size" to the next higher multiple of "alignment".
  Used to overcome limitations built into much contemporary hardware.
}

  begin {forcealign}
    if packedresult then alignment := alignment * bitsperunit;
    if alignment > 1 then
      size := ((size + alignment - 1) div alignment) * alignment;
    forcealign := size;
  end {forcealign} ;

function unsigned(f: entryptr; {type to check}
                  len: addressrange; {space actually allocated for var}
                  packedelement: boolean {set if packed var} ): boolean;

{ Returns true if the values of type "f" are unsigned.
  If "len" is not equal to the space required for the value, it is being
  allocated a space larger than required, and should be treated as signed
  or unsigned for unpacking, depending on the global "unsignedprefered".
}

  begin {unsigned}
    if not packedelement then len := len * bitsperunit;
    unsigned := not (f^.typ in [subranges, ints, bools, chars, scalars]) or
                (lower(f) >= 0) and (unsignedprefered or (len = sizeof(f,
                true))) or f^.extendedrange;
  end {unsigned} ;

function simplesize(i: integer {value to find size of} ): integer;

{ Returns the size in multiples of addressing units needed to contain
  the value of i.
}

  var
    b: integer; {bits to contain i}
    t: integer; {used to accumulate size in units}


  begin {simplesize}
    b := bits(i);
    t := 1;
    while b > t * bitsperunit do t := t + 1;
    simplesize := t;
  end {simplesize} ;



function negaterealconst(realbuffer: realarray {real constant value} ):
 realarray;

 { function to negate a real constant independent of the host }

  const
    halfword = 32768; {for constant negating}

  var
    signidx: 1..maxrealwords; {index to the signpart}


  begin {negaterealconst}
    case targetmachine of
      vax, pdp11:
        begin
        if realbuffer[1] <> 0 then
          if realbuffer[1] >= halfword then
            realbuffer[1] := realbuffer[1] - halfword
          else realbuffer[1] := realbuffer[1] + halfword;
        end;
      mc68000:
        begin
        if realbuffer[1] >= halfword then
          realbuffer[1] := realbuffer[1] - halfword
        else realbuffer[1] := realbuffer[1] + halfword;
        end;
      iapx86, i80386, ns32k:
        begin
        if switcheverplus[doublereals] then signidx := maxrealwords
        else signidx := maxrealwords div 2;
        if realbuffer[signidx] >= halfword then
          realbuffer[signidx] := realbuffer[signidx] - halfword
        else realbuffer[signidx] := realbuffer[signidx] + halfword;
        end;
      end;
    negaterealconst := realbuffer;
  end {negaterealconst} ;

procedure constant(follow: tokenset; {legal following symbols}
                   dumpvalue: boolean; {true says dump string}
                   var value1: operand {resulting constant} );

{ Syntactic routine to parse a constant.

  productions:

  constant = [ sign ] (unsigned-number | constant-identifier) |
         character-string | structured-constant  .

  If the constant is a simple constant, the actual value is returned in
  "value", otherwise a pointer to the string file is returned.  Constant
  structures are added to the "consttable" portion of the string file
  if they are longer than an integer value, otherwise they are returned
  as an integer value.
}

procedure conststructure(form: index; {form found for this constant}
                         var value1: operand {resulting value } );

{ Syntactic routine to parse a constant structure.

  Productions:

  structured-constant = type-identifier structured-value  .

  structured-value = "(" constant-element [* "," constant-element *]
                     ")"  .

  This routine checks for a legal type, and parses arrays with
  one routine and records with another.

  The constant value is buffered as it is generated, and is written
  to the string file if the size cannot be represented as an integer.

  NOTE:

    In order to understand what is going on in the handling of constants
    that are being treated as integers (representation = ints), it is
    necessary to be aware of two inviolate rules:

    (1) All integer constant elements are kept in HOST form within
        the compiler.  They are changed to target form, if there is
        a difference, only when they are actually written to the
        constant buffer.

    (2) If a value is smaller than an integer, the value is kept in
        the low order bytes of the integer.  For high-to-low packing,
        this implies that the internal representation of the last or
        only portion of a packed value must be shifted down to the
        low order as soon as the constant is complete.

  The values of "reversebytes" and "hostintlowbytefirst" (defined in
  config) determine the transformations that must take place, if any.

  In some places, there are tests such as:
    if hostintlowbytefirst <> reversebytes then ...
  This test, if true, says that TARGET integers have the low byte first.

  The possibilities are:

    hostintlowbytefirst  reversebytes    Implications

  =               False         False    Target has high byte first
  <>              False          True    Target has low byte first
  <>               True         False    Target has low byte first
  =                True          True    Target has high byte first
}

  const
    cbufsize = 32; {constant buffering, must be big enough to cover a real and
                    an integer}

  type
    constbuffer = {holds and converts constant values}
      record
        case integer of
          1: (b: packed array [1..cbufsize] of hostfilebyte);
          2: (i: integer);
          3: (r: realarray {double} );
          4: (p: integer {targep} );
      end;

  var
    cbuf: constbuffer; {holds bytes to write}
    cbytes: 0..cbufsize; {current bytes in cbuf}
    curloc: addressrange; {current location relative to start of constant}
    baseloc: addressrange; {start of packed buffers}
    pbuf1, pbuf2: integer; {packed data buffers}
    f: entryptr; {for access to form data}
    packedflag: boolean; {this is a packed form}

  procedure putcbyte(b: hostfilebyte {constant byte to put} );

{ Put a byte to the constant buffer, writing the buffer to the string
  file if it is full.
}

    var
      i: 1..cbufsize; {induction value}


    begin {putcbyte}
      if emitflag then
        begin
        if cbytes = cbufsize then
          begin
          if scanalys then seekstringfile(stringfilecount)
          else seekstringfile(consttablelimit);
          for i := 1 to cbufsize do putbyte(cbuf.b[i]);
          cbytes := 0;
          end;
        cbytes := cbytes + 1;
        cbuf.b[cbytes] := b;
        end;
    end {putcbyte} ;


  procedure alignpartialint(var int: integer; {integer to be aligned}
                            bytes: addressrange {actual size in use} );

{ If the value is in the high order end of the integer, and it hasn't
  used all bytes of the integer, we need to slide the value down to the
  low order end, consistent with the rule that all values are kept in
  the low order end of the integer.
}

    var
      kludge: constbuffer; {used to shift left or right}
      dist: 0..cbufsize; {distance to shift}
      i: 0..cbufsize; {induction on bytes in integer}


    begin {alignpartialint}
      if (bytes < hostintsize * hostfileunits) and (bytes > 0) then
        begin
        kludge.i := int;
        dist := hostintsize * hostfileunits - bytes;
        if hostintlowbytefirst then
          begin {shift left}
          for i := 1 to bytes do kludge.b[i] := kludge.b[i + dist];
          for i := bytes + 1 to hostintsize * hostfileunits do
            kludge.b[i] := 0;
          end
        else
          begin {shift right}
          for i := bytes downto 1 do kludge.b[i + dist] := kludge.b[i];
          for i := dist downto 1 do kludge.b[i] := 0;
          end;
        int := kludge.i;
        end;
    end {alignpartialint} ;


  procedure reversestructure(var s: constbuffer; {structure to munge}
                             bytes: integer {number of bytes to flip} );

{ Reverse the bytes within each integer of a structure.
}

    var
      i, j: 1..cbufsize; {induction on bytes of an integer}
      k: 0..cbufsize; {offset of integer in constant buffer}
      t: hostfilebyte; {temp for flipping structure}


    begin {reversestructure}

      if reversebytes then
        begin
        k := 0;

        while bytes - k > 0 do
          begin {reverse the active bytes in each integer}
          i := k + 1;
          j := k + hostintsize * hostfileunits;
          if bytes - k < hostintsize * hostfileunits then
            if hostintlowbytefirst then j := bytes {do left part}
            else i := j + 1 - (bytes - k); {do right part}
          while i < j do
            begin
            t := s.b[i];
            s.b[i] := s.b[j];
            s.b[j] := t;
            i := i + 1;
            j := j - 1;
            end;
          k := k + hostintsize * hostfileunits;
          end;
        end;
    end {reversestructure} ;


  procedure putcint(int: integer; {integer to write}
                    bytes: addressrange {bytes to write} );

{ Put "bytes" bytes of an integer to the constant buffer.  If
  there are more bytes specified than in an integer, empty bytes
  are appended to the end.
}

    var
      kludge: constbuffer; {used to separate bytes of the integer}
      datasize: 0..cbufsize; {number of bytes of actual integer}
      i: addressrange; {induction on "bytes"}


    begin {putcint}
      if emitflag then
        begin
        kludge.i := int;
        datasize := min(bytes, hostintsize);
        if reversebytes then
          reversestructure(kludge, hostintsize * hostfileunits);
        if hostintlowbytefirst <> reversebytes {low order first} then
          for i := 1 to datasize do putcbyte(kludge.b[i])
        else {high order first}
          for i := hostintsize * hostfileunits + 1 - datasize to hostintsize *
           hostfileunits do
            putcbyte(kludge.b[i]);
        for i := hostintsize * hostfileunits + 1 to bytes do putcbyte(0);
        end;
    end {putcint} ;


  procedure putcreal(r: realarray; {double}
                     size: integer);

{ Put a real value to the constant buffer.
}

    var
      kludge: constbuffer; {used to separate the bytes of the real}
      i: 1..cbufsize; {induction on words of the real number}
      t: hostfilebyte; {temp for switching bytes}


    begin {putcreal}
      kludge.r := r;
      if reversebytes then
        for i := 1 to (size * hostfileunits) div 2 do
          begin
          t := kludge.b[i * 2 - 1];
          kludge.b[i * 2 - 1] := kludge.b[i * 2];
          kludge.b[i * 2] := t;
          end;
      for i := 1 to size * hostfileunits do putcbyte(kludge.b[i]);
    end {putcreal} ;


  procedure putcptr(p: integer {targep} {pointer to generate} );

{ Put a pointer value to the constant buffer.  Pointer constants are
  kept in target form, so there is no need to call reversestructure.

  NOTE: This routine assumes that a pointer will never be smaller
        than an integer.
}

    var
      kludge: constbuffer; {used to separate the bytes of the pointer}
      i: 1..cbufsize; {induction on bytes of the pointer}


    begin {putcptr}
      for i := 1 to ptrsize * hostfileunits do kludge.b[i] := 0;
      kludge.p := p;
      if hostintsize < ptrsize then
        begin
        if hostintlowbytefirst = reversebytes {high order first} then
          for i := 1 to (ptrsize - hostintsize) * hostfileunits do
            putcbyte(0);
        for i := 1 to hostintsize * hostfileunits do putcbyte(kludge.b[i]);
        if hostintlowbytefirst <> reversebytes {low order first} then
          for i := 1 to (ptrsize - hostintsize) * hostfileunits do
            putcbyte(0);
        end
      else {pointer size = integer size}
        for i := 1 to ptrsize * hostfileunits do putcbyte(kludge.b[i]);
    end {putcptr} ;


  procedure flushpackbuffer;

{ Flush anything in the packed constant buffer and update baseloc and
  curloc to reflect the new location.
}

    var
      bytes: addressrange; {how many bytes it takes to hold the value}


    begin {flushpackbuffer}
      if curloc > baseloc then
        begin
        bytes := (curloc - baseloc + bitsperunit - 1) div bitsperunit;
        if packinghightolow then alignpartialint(pbuf1, bytes);
        putcint(pbuf1, bytes);
        baseloc := ((baseloc + bitsperunit - 1) div bitsperunit + bytes) *
                   bitsperunit;
        curloc := baseloc;
        pbuf1 := 0;
        end;
    end {flushpackbuffer} ;

  procedure flushbuffer;

{ Flush any partial value kept as an intvalue thus far.
}
    var
      i: 1..cbufsize; {induction var}

    begin {flushbuffer}
      if not scanalys then seekstringfile(consttablelimit);
      with value1.cvalue do
        begin
        if representation = ints then
          begin
          if reversebytes then
            reversestructure(cbuf, hostintsize * hostfileunits);
          if hostintlowbytefirst = reversebytes {high order first} then
            alignpartialint(cbuf.i, cbytes);
          intvalue := cbuf.i;
          negated := false; { PJS force init }
          end
        else if emitflag then
          begin
          for i := 1 to cbytes do putbyte(cbuf.b[i]);
          cbytes := 0;
          end;
        end;
    end   {flushbuffer};

  procedure putvalue(vloc: addressrange; {loc to put value}
                     eltstring: boolean; {target elt is a string}
                     whichbuf: boolean; {which string buffer}
                     packing: boolean; {this is being packed in}
                     eltsize: addressrange; {size of constant element}
                     var value1: operand {value to place} );

{ Put a constant value in "value" into the current structured constant.
  There is an assumption that only items capable of representation as
  integers will actually be packed.  The packing is done by the routine
  "packedstore".

  Strings require some extra work as a length byte must prefix the
  actual data, as the result might need padding, and it might be
  a character needing conversion to a string!
}

    var
      full: boolean; {a packed word is full}
      start, finish: addressrange; {start and end of constant in file}
      bytes: addressrange; {bytes already used in the packing buffer}


    procedure putpackedvalue(data: integer; {bits to put}
                             dataloc: addressrange; {where to put bits}
                             datasize: addressrange {number of bits to put} );

{ Put one packed data item.  This code was inline until strings were
  added, as conversion of a character to a string requires both the
  length and data bytes to be emitted.
}


      begin {putpackedvalue}
        { If the value won't fit in this integer, then we need to
          flush the packing buffer.

          The VAX and iAPX, however, are a special case, for which packing
          is done to the maximum extent possible. }

        if (targetmachine <> iapx86) and (targetmachine <> vax) and
           (targetmachine <> i80386) or
           (switchcounters[oldpacking] > 0) then
          begin
          if dataloc + datasize > baseloc + bitsperunit * hostintsize then
            begin
            bytes := (dataloc - baseloc + bitsperunit - 1) div bitsperunit;
            if packinghightolow then alignpartialint(pbuf1, bytes);
            putcint(pbuf1, bytes);
            baseloc := ((dataloc + bitsperunit - 1) div bitsperunit) *
                       bitsperunit;
            curloc := baseloc;
            pbuf1 := 0;
            end;
          end {not vax} ;

        packedstore(dataloc, datasize, baseloc, data, pbuf1, pbuf2, full);

        if full then
          begin {we write out the lower buffer}
          putcint(pbuf1, hostintsize);
          pbuf1 := pbuf2;
          pbuf2 := 0;
          baseloc := baseloc + hostintsize * bitsperunit;
          end;
        curloc := dataloc + datasize;
      end {putpackedvalue} ;


    begin {putvalue}
      if packing and (value1.cvalue.representation = ints) then
        begin
        if eltstring then
          begin
          putpackedvalue(1, vloc, bitsperunit); {length of string = 1 char}
          putpackedvalue(value1.cvalue.intvalue, vloc + bitsperunit,
                         eltsize - bitsperunit);
          end
        else putpackedvalue(value1.cvalue.intvalue, vloc, eltsize);
        end
      else { not packing or (representation <> ints) }
        begin
        if packing then
          begin
          bytes := (vloc - baseloc + bitsperunit - 1) div bitsperunit;
          if packinghightolow then alignpartialint(pbuf1, bytes);
          putcint(pbuf1, bytes);
          pbuf1 := 0;
          end
        else putcint(0, vloc - curloc);
        with value1.cvalue do
          case representation of
            ints:
              if eltstring then
                begin {convert char to string w/length byte then pad}
                putcbyte(1);
                putcbyte(value1.cvalue.intvalue);
                if packing then putcint(0, eltsize div bitsperunit - 2)
                else putcint(0, eltsize - 2);
                end
              else putcint(intvalue, eltsize);
            reals: putcreal(realvalue.realbuffer, targetrealsize);
            doubles: putcreal(realvalue.realbuffer, doublesize);
            ptrs: putcptr(ptrvalue);
            otherwise
              begin
              if packing then bytes := eltsize div bitsperunit
              else bytes := eltsize;
              if scanalys and (pos < 0) then {has not been dumped to
                                                        file at all, yet}
                begin
                flushbuffer;
                if scanalys then {make sure this is deadcoded}
                  dumpstr(bytes, whichbuf, eltstring);
                end
              else
                begin
                start := pos;
                if start >= stringfilecount then
                  start := start + (stringtablelimit - stringfilecount);
                finish := start + len;
                if eltstring then putcbyte(len);
                while start < finish do
                  begin
                  seekstringfile(start);
                  putcbyte(stringblkptr^[nextstringfile]);
                  start := start + 1;
                  end;
                seekstringfile(stringfilecount);

                if eltstring then len := len + 1; {a kludge, the length byte}

                { This fixes structured constant bug tr2158.  Odd-byte length
                  structures actually allocated the next greater even numbered
                  byte worth of space and were not padded correctly.  Putvalue
                  only emitted the actual data bytes but moved the current
                  location counter to the allocated boundary.  This change
                  causes Putvalue to correctly emit the actual data bytes, and
                  eltsize-len (i.e. allocated length - data length) bytes of
                  pad. }

                if bytes < len then
                  if eltstring then warn(stringoverflowerr)
                  else warn(compilerwritererr);
                putcint(0, bytes - len);
                end;
              end;
            end {case} ;
        curloc := vloc + eltsize;
        baseloc := curloc;
        end;
    end {putvalue} ;


  procedure initcdump(var value1: operand; {value to initialize}
                      consttypeindx: index; {index for this element type}
                      consttype: entryptr {type of this element} );

{ Initialize the value of the constant and set up the constant buffer
  for generating the value.
}

    var
      t: addressrange; {aligned value of consttablelimit}


    begin {initcdump}
      cbytes := 0;
      cbuf.i := 0;
      pbuf1 := 0;
      pbuf2 := 0;
      curloc := 0;
      baseloc := 0;
      with value1 do
        begin
        typeindex := consttypeindx;
        oprndlen := sizeof(consttype, false);
        operandkind := constoperand;
        end;
      if (value1.oprndlen > targetintsize) or (value1.oprndlen = 3) or
         ((targetmachine = mc68000) and not switcheverplus[cpu68020]) or
        scanalys then
        begin {must align the string file}
        if scanalys then
          begin
          t := forcealign(stringfilecount, alignmentof(consttype,
                          false) * hostfileunits, false);
          putcint(0, t - stringfilecount);
          end
        else
          begin
          t := forcealign(consttablelimit, alignmentof(consttype,
                          false) * hostfileunits, false);
          putcint(0, t - consttablelimit);
          end;
        curloc := 0;
        baseloc := 0;
        value1.extended := false;
        value1.cvalue.representation := consttype^.typ;
        value1.cvalue.stringconstflag := false;
        if scanalys then value1.cvalue.pos := t
        else value1.cvalue.pos := t - stringtablelimit + stringfilecount;
        value1.cvalue.len := value1.oprndlen * hostfileunits;
        if value1.cvalue.representation = ints then
          value1.cvalue.negated := false; {PJS force init }
        end
      else
        begin
        value1.cvalue.representation := ints;
        value1.cvalue.negated := false; { PJS force init }
        end;
    end {initcdump} ;


  procedure finishdump(var value1: operand; {value being finished off}
                       packing: boolean {this is a packed value} );

{ After the entire constant has been parsed, this forces final output,
  if necessary, and finishes off the resulting operand
}

    begin {finishdump}
      if value1.oprndlen > curloc then
        if packing then
          begin
          flushpackbuffer;
          if value1.oprndlen > curloc then
            putcint(0, (value1.oprndlen - curloc) div bitsperunit);
          end
        else putcint(0, value1.oprndlen - curloc);
      flushbuffer;
    end {finishdump} ;


  procedure innercstruct(eltloc: addressrange; {rel loc for this element}
                         form: index; {form of this constant}
                         outerpacked: boolean {outer structure was packed} );

{ Given a structured value, determine the type and parse it.
}

    var
      f: entryptr; {for access to form}
      packedflag: boolean; {form is packed}


    procedure constvalue(eltloc: addressrange; {rel loc for this element}
                         elttype: index; {desired value type}
                         eltstring: boolean; {current element is a string}
                         packing: boolean; {set if packing this element}
                         var value1: operand; {result, if not written}
                         var written: boolean {value already written} );

{ Parse a constant value.  If the value is in turn a structured constant,
  it may be written to the string file as it is scanned.  Otherwise,
  its value is placed in "value1", and must be written.
}

      var
        tindex: index; {type name index}
        p: entryptr; {access to type name block}
        elp: entryptr; {for access to elttype}
        kludge: constbuffer; {converts to integer}
        i: 1..cbufsize; {induction var for conversion}
        start, finish: addressrange; {start and end of constant in file}


      procedure getconstant(follow: tokenset; {legal following symbols}
                            elttype: index; {desired value type}
                            var value1: operand {result} );

{ Parse a simple constant and make sure it's in range for the field
  it's being assigned to.
}

        var
          elp: entryptr; {for access to elttype}
          unsvalue: unsignedint; {for unsigned comparisons}


        begin {getconstant}
          constant(follow, false, value1);
          with value1, cvalue do
            if (representation = ints) and (elttype <> noneindex) and
               compatible(typeindex, elttype) then
              begin
              if bigcompilerversion then elp := @(bigtable[elttype]);
              if extended and not elp^.extendedrange or (intvalue < 0) and
                 negated and elp^.extendedrange then
                warnbefore(badconsterr)
              else if elp^.extendedrange then
                begin
                unsvalue := intvalue;
                if (unsvalue < longword(lower(elp))) or
                   (unsvalue > longword(upper(elp))) then
                  warnbefore(badconsterr);
                end
              else if (intvalue < lower(elp)) or
                      (intvalue > upper(elp)) then
                warnbefore(badconsterr);
              end;
        end {getconstant} ;


      begin {constvalue}
        written := false;
        if token = ident then
          begin
          search(tindex);
          if bigcompilerversion then p := @(bigtable[tindex]);
          with p^ do
            if namekind = typename then
              begin
              if not compatible(typeindex, elttype) then warn(typesincomp);
              gettoken;
              innercstruct(eltloc, typeindex, packing);
              written := true;
              end
            else getconstant(follow, elttype, value1);
          end
        else if token = lpar then
          begin
          if bigcompilerversion then elp := @(bigtable[elttype]);
          if not (elp^.typ in [arrays, fields, none]) then
            begin
            warn(badconsterr);
            value1.typeindex := noneindex;
            value1.cvalue.representation := ints;
            end;
          innercstruct(eltloc, elttype, packing);
          written := true;
          end
        else getconstant(follow, elttype, value1);
        if not written then
          begin
          if bigcompilerversion then
            elp := @(bigtable[value1.typeindex]);
          if not (eltstring and ((elp^.typ = chars) or
             (elp^.typ = arrays) and (elp^.stringtype)) or
             compatible(elttype, value1.typeindex)) then
            warnbefore(typesincomp);
          if (value1.cvalue.representation in [arrays, fields]) and
             (value1.cvalue.len <= hostintsize * hostfileunits) and
             not eltstring and not scanalys then
            begin
            kludge.i := 0;
            with value1.cvalue do
              begin
              i := 1;
              start := pos;
              if start >= stringfilecount then
                start := start + (stringtablelimit - stringfilecount);
              finish := start + len;
              while start < finish do
                begin
                seekstringfile(start);
                kludge.b[i] := stringblkptr^[nextstringfile];
                start := start + 1;
                i := i + 1;
                end;

              if reversebytes then
                reversestructure(kludge, hostintsize * hostfileunits);
              if hostintlowbytefirst = reversebytes {high order first} then
                alignpartialint(kludge.i, len);
              representation := ints;
              intvalue := kludge.i;
              negated := false; {PJS force init}
              end;
            end;
          end;
      end {constvalue} ;


    procedure constelement(eltloc: addressrange; {loc to for this element}
                           elttype: index; {type desired}
                           eltsize: addressrange; {size of the element}
                           eltstring: boolean; {current element is a string}
                           packing: boolean {this is a packed field} );

{ Read and store a constant element.  The result is written to the string
  file buffer.
}

      var
        temp: operand; {holds a value}
        written: boolean; {value already written}
        whichbuf: boolean; {which string buf contains thistoken's string if
                            any}


      begin {constelement}
        whichbuf := curstringbuf;
        constvalue(eltloc, elttype, eltstring, packing, temp, written);
        if not written and (temp.typeindex <> noneindex) then
          putvalue(eltloc, eltstring, whichbuf, packing, eltsize, temp);
      end {constelement} ;


    procedure constarray(eltloc: addressrange; {rel loc for this element}
                         form: index {form for this array} );

{ Syntactic routine to parse a constant array.
  Each element is identical, and the number must match.
}

      var
        eltcount: addressrange; {elements defined so far}
        eltsinarray: addressrange; {total elements in array}
        elttype: index; {element type}
        eltstring: boolean; {element is a string}
        packing: boolean; {set if packed array}
        eltsize: addressrange; {size of each element}
        f: entryptr; {for access to type data}


      begin {constarray}
        eltcount := 1;
        if bigcompilerversion then f := @(bigtable[form]);
        elttype := f^.elementtype;
        packing := f^.packedflag;
        eltsize := f^.elementsize;
        eltsinarray := f^.arraymembers;
        if bigcompilerversion then f := @(bigtable[elttype]);
        eltstring := f^.typ = strings;
        constelement(eltloc, elttype, eltsize, eltstring, packing);
	while token in [lpar, comma] + begconstset do
          begin
          verifytoken(comma, nocommaerr);
          eltloc := eltloc + eltsize;
          if eltcount = eltsinarray then warnbefore(badconsterr);
          eltcount := eltcount + 1;
          constelement(eltloc, elttype, eltsize, eltstring, packing);
          end;

        if eltcount < eltsinarray then warnbefore(badconsterr);
      end {constarray} ;


    procedure constrecord(eltloc: addressrange; {start of this element}
                          form: index {form for this record} );

{ Syntactic routine to parse a constant record.
}

      var
        currentfield: index; {name entry for this field}
        finished: boolean; {we used all of the fields we have}


      procedure constfield;

{ Find the next field in this record and get a value for it.
}

        var
          found: boolean; {field was found}
          p: entryptr; {access to field names}
          elttype: index; {form for a variant}
          temp: operand; {temp value for variant}
          written: boolean; {dummy argument to constvalue}
          tagoffset: addressrange; {offset for tag field, if any}
          localform: tableentry; {local copy of form entry}
          f, f1: entryptr; {access to form data}


        begin {constfield}
          if finished then
            constelement(eltloc, noneindex, unitsize, false, false)
          else
            begin
            if bigcompilerversion then f := @(bigtable[form]);
            localform := f^;
            found := false;
            while (currentfield < localform.lastfield) and not found do
              begin
              currentfield := currentfield + 1;
              if bigcompilerversion then p := @(bigtable[currentfield]);
              if not p^.form then found := p^.name = localform.fieldid;
              end;
            if found then
              begin
              if bigcompilerversion then f1 := @(bigtable[p^.vartype]);
              constelement(p^.offset + eltloc, p^.vartype, sizeof(f1,
                           localform.packedflag), f1^.typ = strings,
                           localform.packedflag)
              end
            else if localform.firstvariant = 0 then
              begin
              finished := true;
              warnbefore(badconsterr);
              constelement(curloc, noneindex, unitsize, false, false);
              end
            else
              begin
              tagoffset := 0;
              if localform.tagfield <> 0 then
                begin
                if bigcompilerversion then
                  p := @(bigtable[localform.tagfield]);
                elttype := p^.vartype;
                tagoffset := p^.offset;
                end
              else if localform.firstvariant <> 0 then
                begin
                if bigcompilerversion then
                  f := @(bigtable[localform.firstvariant]);
                if f^.firstlabel <> 0 then
                  begin
                  if bigcompilerversion then
                    f := @(bigtable[f^.firstlabel]);
                  elttype := f^.varlabtype;
                  end
                else elttype := noneindex;
                end
              else elttype := noneindex;
              constvalue(tagoffset + eltloc, elttype, false,
                         localform.packedflag, temp, written);
              if localform.tagfield <> 0 then
                begin
                if bigcompilerversion then f := @(bigtable[elttype]);
                putvalue(tagoffset + eltloc, false, false,
                         localform.packedflag, sizeof(f, localform.packedflag),
                         temp);
                end;
              searchvariants(form, temp);
              if bigcompilerversion then f := @(bigtable[form]);
              currentfield := f^.firstfield - 1;
              end;
            end;
        end {constfield} ;


      begin {constrecord}
        finished := false;
        if bigcompilerversion then f := @(bigtable[form]);
        currentfield := f^.firstfield - 1;

        constfield;
        while token in [lpar, comma] + begconstset do
          begin
          verifytoken(comma, nocommaerr);
          constfield;
          end;

        if bigcompilerversion then f := @(bigtable[form]);
        if (currentfield < f^.lastfield) or (f^.firstvariant <> 0) then
          warnbefore(badconsterr);
      end {constrecord} ;


    procedure badconst;

{ Parse off a bad constant.  Just throws away the values.
}


      begin {badconst}
        warnbefore(badconsterr);
        constelement(curloc, noneindex, unitsize, false, false);
        while token in [lpar, comma] + begconstset do
          begin
          verifytoken(comma, nocommaerr);
          constelement(curloc, noneindex, unitsize, false, false);
          end;
        value1.typeindex := noneindex;
        value1.cvalue.representation := ints;
      end {badconst} ;


    begin {innercstruct}
      if token = lpar then
        begin
        gettoken;
        if bigcompilerversion then f := @(bigtable[form]);
        packedflag := f^.packedflag;
        if outerpacked and not packedflag then
          begin
          flushpackbuffer;
          eltloc := eltloc div bitsperunit;
          curloc := curloc div bitsperunit;
          end
        else if not outerpacked and packedflag then
          begin
          eltloc := eltloc * bitsperunit;
          curloc := curloc * bitsperunit;
          baseloc := curloc;
          end;
        if f^.typ in [strings, arrays] then constarray(eltloc, form)
        else if f^.typ = fields then constrecord(eltloc, form)
        else badconst;
        if not outerpacked and packedflag then
          begin
          flushpackbuffer;
          curloc := curloc div bitsperunit;
          end
        else if outerpacked and not packedflag then
          begin
          curloc := curloc * bitsperunit;
          baseloc := curloc;
          end;
        verifytoken(rpar, norparerr);
        end
      else
        begin
        warnbefore(typenotallowed);
        value1.typeindex := noneindex;
        value1.cvalue.representation := ints;
        end;
    end {innercstruct} ;


  begin {conststructure}
    if not scanalys and switcheverplus[defineswitch] then
      warnbefore(baddefine);
    gettoken;
    if bigcompilerversion then f := @(bigtable[form]);
    packedflag := f^.packedflag;
    initcdump(value1, form, f);
    innercstruct(0, form, false);
    finishdump(value1, packedflag);
  end {conststructure} ;

  var
    negate: boolean; {set if neg sign found}
    sign: boolean; {set if any sign found}
    t: index; {temp index for constant identifier}
    p: entryptr; {temp access to constant identifier entry}
    t1: entryptr; {Temp for index type for string constant}
    unsvalue: unsignedint; {temp for unsigned operation}


  begin {constant}
    {init the descriptor}
    with value1, cvalue do
      begin
      typeindex := noneindex;
      operandkind := constoperand;
      representation := ints; {the most common case}
      negated := false;
      intvalue := 0; {this field will be used, even if it is a bad constant}
      extended := false;
      end;
    negate := (token = minus);
    sign := negate or (token = plus);
    if sign then gettoken;
    verify(begunsignedset, follow, badconsterr);
    if token in begunsignedset then
      begin
      if token = nilsym then
        begin
        value1 := nilvalue;
        gettoken
        end
      else if token = ident then
        begin {either constant or constant structure}
        search(t);
        if bigcompilerversion then p := @(bigtable[t]);
        if t = 0 then
          begin
          warn(undefidenterr);
          gettoken;
          end
        else
          with p^ do
            if namekind = constname then
              begin
              value1.typeindex := consttype;
              if bigcompilerversion then t1 := @(bigtable[consttype]);
              value1.oprndlen := sizeof(t1, false);
              value1.cvalue := constvalue;
              gettoken;
              end
            else if (namekind = typename) then
              begin
              conststructure(p^.typeindex, value1);
              end
            else
              begin
              warn(badconsterr);
              gettoken;
              end;
        end
      else {not nil or an identifier}
        with value1, cvalue do
          begin
          case token of
            intconst:
              begin
              typeindex := intindex;
              intvalue := thistoken.intvalue;
              negated := false; {is default}
              oprndlen := targetintsize;
              end;
            realconst:
              begin
              typeindex := realindex;
              representation := reals;
              realvalue.realbuffer := thistoken.realvalue;
              oprndlen := targetrealsize;
              end;
            dblrealconst:
              begin
              typeindex := doubleindex;
              representation := doubles;
              realvalue.realbuffer := thistoken.realvalue;
              oprndlen := doublesize;
              end;
            charconst:
              begin
              typeindex := chartypeindex;
              intvalue := thistoken.intvalue;
              negated := false; {is default!}
              oprndlen := charsize;
              end;
            stringconst:
              begin {Must make type entry for the string}
              representation := arrays;
              stringconstflag := true;
              len := thistoken.len;
              if scanalys and (thistoken.pos < 0) and dumpvalue
              then
                begin
                pos := stringfilecount + 1;
                if scanalys then  { make sure it's deadcoded }
                  dumpstr(len + 1, curstringbuf, true);
                end
              else pos := thistoken.pos;
              enterform(subranges, t, t1);
              with t1^ do
                begin
                size := targetintsize;
                align := intalign;
                parenttype := intindex;
                parentform := ints;
                lowerord := 1;
                upperord := len
                end;
              enterform(arrays, typeindex, t1);
              with t1^ do
                begin
                packedflag := true;
                bitaddress := true;
                containsfile := false;
                elementtype := chartypeindex;
                stringtype := true;
                arraymembers := len;
                indextype := t;
                size := len div (bitsperunit div stringeltsize);
                if len mod (bitsperunit div stringeltsize) <> 0 then
                  size := size + 1;
                oprndlen := size;
                size := size * bitsperunit;
                elementsize := stringeltsize;
                align := stringalign;
                end;
              end
            end {case} ;
          gettoken;
          end;
      with value1, cvalue do
        if sign and (typeindex <> realindex) and (typeindex <> intindex) then
          warn(badconsterr)
        else if negate then
          if typeindex = realindex then
            realvalue.realbuffer := negaterealconst(realvalue.realbuffer)
          else {non real}
            begin
            if intvalue <> - targetmaxint - 1 then intvalue := - intvalue;
            negated := not negated;
            end;
      end;
    with value1, cvalue do
      if representation = ints then
        begin
        unsvalue := intvalue;
        extended := (unsvalue > targetmaxint) and not negated;
        end;
  end {constant} ;



function getform(objecttype: entryptr {desired form} ): types;

{ Get the basic form associated with a type.
}

  begin {getform}
    with objecttype^ do
      if typ = subranges then getform := parentform
      else getform := typ;
  end {getform} ;

function identical(left, right: index {types to compare} ): boolean;

{ True if two types are identical, or if either is undefined (to avoid
  redundant messages).
}

  begin {identical}
    identical := (left = right) or (left = noneindex) or (right = noneindex);
  end {identical} ;


function compatible(left, right: index {types to compare} ): boolean;

{ True if the types represented by the two input forms are compatible
  as defined by the Pascal standard.  If either input is undefined,
  they are assumed to be compatible to eliminate redundant error
  messages.
}

  var
    lptr, rptr: entryptr; { used for access to symbol table }
    c: boolean; {temporary value of compatible}


  begin {compatible}
    stripsubrange(left);
    stripsubrange(right);
    if identical(left, right) then compatible := true
    else
      begin
      compatible := false;
      if bigcompilerversion then lptr := @(bigtable[left]);
      if bigcompilerversion then rptr := @(bigtable[right]);
      if lptr^.typ = rptr^.typ then
        case lptr^.typ of
          strings: compatible := true;
          arrays:
            compatible := lptr^.stringtype and rptr^.stringtype and
                          (lptr^.arraymembers = rptr^.arraymembers);
          sets:
            compatible := compatible(lptr^.basetype, rptr^.basetype) and
                          ((lptr^.packedflag = rptr^.packedflag) or
                          lptr^.constructedset or rptr^.constructedset);
          ptrs:
            if (left = nilindex) or (right = nilindex) then
              compatible := true
            else
              begin {Allow compatibility between pointer types and pointers
                     created with the address operator if the base types are
                     the same. Also forstall error messages if either pointer
                     base type is undef. }
              if bigcompilerversion then lptr := @(bigtable[lptr^.ptrtypename]);
              if bigcompilerversion then rptr := @(bigtable[rptr^.ptrtypename]);

              c := (lptr^.typeindex = noneindex) or
                   (rptr^.typeindex = noneindex);
              if rptr^.refdefined or lptr^.refdefined then
                c := c or compatible(rptr^.typeindex, lptr^.typeindex);
              compatible := c;
              end;
          end;
      end;
  end {compatible} ;



procedure seekstringfile(n: integer {byte to access} );

{ Do the equivalent of a "seek" on the string file.  This sets the
  file and "nextstringfile" to access byte "n" of the stringfile.
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

procedure getstringfile;

{ Do the equivalent of a get on the stringfile.

  The string file contains constant data collected by SCAN and
  ANALYS.  It is organized as blocks containing arrays 0..
  diskbufsize of bytes.  The string file is always accessed
  as stringblkptr^[nextstringfile].
}

  begin {getstringfile}
    if nextstringfile = diskbufsize then
      begin
      nextstringfile := 0;
      curstringblock := curstringblock + 1;
      stringblkptr := stringblkptrtbl[curstringblock];
      end
    else nextstringfile := nextstringfile + 1;
  end {getstringfile} ;


function do_hash(charindex:integer; charlen:integer) : integer;
{perform a hashing of the entry of length "charlen" at the location "charindex"
 in the stringtable, using the function concealed inside the debugger package.}

  function nextch:char;
  begin {nextch}
    nextch := stringtable^[charindex];
    charindex := charindex + 1
  end {nextch} ;

begin {do_hash}
{DRBif newdebugger then do_hash := p_debughash(charlen, nextch);}
  do_hash := 0;
end {do_hash} ;

end.
