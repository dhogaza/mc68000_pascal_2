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


  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Syntax/Semantic Analysis Common Routines

 Last modified by KRIS on 21-Nov-1990 15:18:37
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}


procedure warnbefore {err: warning (Error message number) } ;

{ Generate an error message at the center of the last token.
}


  begin {warnbefore}
    emitflag := false;
    with lasttoken do warnat(err, line, (left + right) div 2)
  end {warnbefore} ;


procedure warnbetween {err: warning (Error message number) } ;

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


procedure warn {err: warning (Error message number) } ;

{ Generate an error message in the middle of the current token.
}


  begin {warn}
    emitflag := false;
    with thistoken do warnat(err, line, (left + right) div 2)
  end {warn} ;


procedure warnnonstandard {err: warning (Error message number) } ;

{ Generate a warning only if the standard switch is set.
  Used to warn of non-standard features.
}


  begin {warnnonstandard}
    if (switchcounters[standard] > 0) then warn(err)
  end {warnnonstandard} ;


procedure fatal {err: warning (Error message number) } ;

{ Generate a fatal warning which will terminate the compilation.
}


  begin {fatal}
    fatalflag := true;
    warn(err);
  end {fatal} ;

{ Virtual Memory System }

{ Virtual Memory System for Instruction Nodes.
  These routines implement the virtual memory tree used for instructions.
  Nodes are stored on the file "nodefile" in blocks.  A maximum of
  "amaxblocksin" of these blocks are kept in buffers in main memory.
  The array "blocksin" is used to keep track of the blocks in memory,
  and to implement a least recently used update strategy.

  When a node is requested, the list of blocks in memory is searched,
  and if the node is in one of them that block is moved to the head
  of the list (blocksin[1]).  All blocks between the former position
  and the head of the list are moved one position toward the foot of
  the list.  If the block is not already in memory, the last block on the
  list is made available by writing its contents to the file (if they have
  been modified), and it is moved to the start of the list and loaded
  from the file.  Finally a pointer to the desired node is generated
  using the "ref" function.  Of course, if the block is not in memory and
  there are less than "amaxblocksin" blocks already in memory, a new
  buffer is created without writing an old one.

  Since measurements indicated that this is a major time consumer
  in this pass, the mechanism is effectively bypassed if the blocks
  will all fit in core.  In this case,  there is a direct mapping
  from index to buffers in "blocksin".  Once any buffer has been
  written (indicated by the global "thrashing"), the system reverts
  to the search of blocksin to locate a buffer.

  At the point where thrashing becomes true,  all blocks are
  assumed to be written.
}


procedure accessblk(blockwanted: integer {Index of block to access} );

{ Make the block with index "blockwanted" available at the top
  of "blocksin".  If not already in core, the contents of the file
  block with that index are read in.  If the least recently used block
  must be written ("written" set and no more space in core), it is
  written to the appropriate place in the file.

  This procedure is used locally from the virtual memory package.

  Although the code within "accessblk" will work with amaxblocksin
  equal to 1, the compiler code assumes that amaxblocksin >= 2, and
  will not work without this condition.
}

  var

    i: 0..amaxblocksin; {Induction var for search and copy}
    temp: blockmap; {temp storage for moving block to head of list.}


  begin {accessblk}
    if needcaching then
      begin
      if not thrashing and switcheverplus[test] then
        writeln('analys thrashing state reached');
      thrashing := true;
       {find this block or last block in memory}
     i := 1;
     repeat
        i := i + 1
      until (blocksin[i].blkno = blockwanted) or (i = lastblocksin);

      {now make room for blockwanted at head of the list}
      temp := blocksin[i];
      for i := i downto 2 do blocksin[i] := blocksin[i - 1];

      {write out old block if necessary, read blockwanted or create new buffer}
      with temp do
        begin
        if blockwanted <> blkno then
          begin
          if written then
            begin
            seek(cache, blkno + 1);
            cache^ := buffer^.physical;
            put(cache)
            end;
          written := false;
          seek(cache, blockwanted + 1);
          buffer^.physical := cache^;
          end;
        blkno := blockwanted;
        end;

      {update list}
      blocksin[1] := temp
      end;
  end {accessblk} ;


procedure areadaccess{i: index; (node wanted)
                      var p: entryptr (provides access to desired node ) } ;

{ Make virtual element "i" available for read-only access
  and return a pointer to it.

}

  var
    blockno: integer; {1..tablesize + 1}


  begin {areadaccess}
    if bigcompilerversion then writeln('areadaccess called!')
    else
      begin
      if needcaching then
        begin
        blockno := i div (entriesperblock + 1) + 1;
        if thrashing or (blockno >= lastblocksin) then
          begin
          if blocksin[1].blkno <> i div (entriesperblock + 1) then
            accessblk(i div (entriesperblock + 1));
          p := ref(blocksin[1].buffer^.logical[i mod (entriesperblock +
                   1)]);
          end
        else
          with blocksin[blockno] do
            begin
            if buffer = nil then new(buffer);
            p := ref(buffer^.logical[i mod (entriesperblock + 1)]);
            end;
        end
      else {nocaching}
        begin
        blockno := i div (entriesperblock + 1) + 1;
        if blockno > amaxblocksin then abort(manynodes);
        with blocksin[blockno] do
          begin
          if buffer = nil then new(buffer);
          p := ref(buffer^.logical[i mod (entriesperblock + 1)]);
          end;
        end;
      end;
  end {areadaccess} ;


procedure awriteaccess {i: index; (node to access)
                       var p: entryptr (provides access to the node) } ;

{ Make virtual element "i" available for access and modification, and
  return a pointer to it.

}

  var
    blockno: integer; {1..tablesize + 1}


  begin {awriteaccess}
    if bigcompilerversion then write('awriteaccess called!')
    else
      begin
      if needcaching then
        begin
        areadaccess(i, p);
        blocksin[1].written := true;
        end
      else
        begin
        blockno := i div (entriesperblock + 1) + 1;
        if blockno > amaxblocksin then abort(manynodes);
        with blocksin[blockno] do
          begin
          if buffer = nil then new(buffer);
          p := ref(buffer^.logical[i mod (entriesperblock + 1)]);
          written := true;
          end;
        end;
      end;
  end {awriteaccess} ;


procedure adecreasebuffers;

{ If available memory is running low relinquish a cache buffer.
}

  var
    i: 0..amaxblocksin; {induction variable}


  begin {adecreasebuffers}
    if needcaching then
      begin
      if not newok(size(vartableblock)) and (space < arequiredspace) or
         (space < apanicspace) then
        begin
        if lastblocksin <> maxblockslow then
          begin
          i := lastblocksin;
          while blocksin[i].lowblock do i := i - 1;
          with blocksin[i] do
            begin
            if written then
              begin
              seek(cache, blkno + 1);
              cache^ := buffer^.physical;
              put(cache);
              end;
            dispose(buffer);
            for i := i to lastblocksin - 1 do blocksin[i] := blocksin[i + 1];
            lastblocksin := lastblocksin - 1;
            if lastblocksin < fewestblocks then fewestblocks := lastblocksin;
            end;
          end;
        end;
      end;
    if space < apanicspace then abort(outofmem);
  end {adecreasebuffers} ;


procedure aincreasebuffers;

{ Add a new virtual buffer if space has gotten large due to the
  dispose routine.
}


  begin {aincreasebuffers}
    if needcaching then
      begin
      while (lastblocksin < amaxblocksin) and ((space > aexcessivespace) or
            newok(size(doublediskblock))) do
        begin
        if not thrashing and switcheverplus[test] then
          writeln('analys thrashing state reached');
        thrashing := true;
        lastblocksin := lastblocksin + 1;
        with blocksin[lastblocksin] do
          begin
          new(buffer);
          lowblock := false;
          written := false;
          blkno := - 1;
          end;
        end;
      end;
  end {aincreasebuffers} ;


procedure getnexttoken;

{ Get the next token from the intermediate file into the global "nexttoken".
  In the process, linenumbers and switches are incremented as necessary.

  The file is compressed as described in the "put" routine for the scanner,
  and this routine undoes those compressions.
}

  var
    t: tokentype; {temp storage for the token}


  procedure gettempfile;

{ Does the equivalent of a get on the temp file.  This also keeps track
  of the number of bytes read from the temp file to make the switches
  work.  The temp file is actually a block of hostfilebytes, and each
  byte is referenced by:
        tempfileone^[tokenbufindex].<value>
}


    begin {gettempfile}
      if tokenbufindex = diskbufsize then
        begin
        tokenbufindex := 0;
        get(tempfileone);
        end
      else tokenbufindex := tokenbufindex + 1;
    end {gettempfile} ;


  function getint: integer;

{ Get an integer value from the file.  This is made into a function to
  allow returning the value into subranges of integer.
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


    begin {getint}
      fudge.int := tempfileone^[tokenbufindex].byte;
      gettempfile;
      if fudge.int = hostfilelim then
        for j := 1 to hostintsize * hostfileunits do
          begin
          fudge.byte[j] := tempfileone^[tokenbufindex].byte;
          gettempfile;
          end;
      getint := fudge.int;
    end {getint} ;


  procedure getreal(var r: realarray);

{ Get a real value from the file.
}

    var
      { this fudges a real into bytes.  The constant "32" is }
      { simply a large enough number to include all probable systems. }
      fudge:
        record
          case boolean of
            true: (rl: realarray);
            false: (byte: packed array [1..32] of hostfilebyte);
        end;
      j: 1..32; {induction var}


    begin {getreal}
      for j := 1 to size(realarray) do
        begin
        fudge.byte[j] := tempfileone^[tokenbufindex].byte;
        gettempfile;
        end;
      r := fudge.rl;
    end {getreal} ;


  begin {getnexttoken}
    with nexttoken do
      begin

      repeat
        t := tempfileone^[tokenbufindex].toke;
        gettempfile;
        if t = newfile then
          begin
          baseline := getint;
          fileindex := getint; { stringtable index of file name }
          end
        else if t = lineinc then line := line + 1
        else if t = lineadd then
          begin
          line := line + tempfileone^[tokenbufindex].byte;
          gettempfile;
          end
      until (t <> lineinc) and (t <> lineadd) and (t <> newfile);

      token := t;
      left := tempfileone^[tokenbufindex].byte;
      gettempfile;

      if t in [ident, intconst, charconst, realconst, dblrealconst,
              stringconst] then
        begin
        right := tempfileone^[tokenbufindex].byte;
        gettempfile;
        case t of
          ident:
            begin
            key := getint;
            keypos := getint;
            end;
          intconst: intvalue := getint;
          charconst:
            begin
            intvalue := tempfileone^[tokenbufindex].byte;
            gettempfile;
            end;
          realconst, dblrealconst: getreal(realvalue);
          stringconst:
            begin
            pos := getint;
            len := getint;
            end;
          end
        end
      else right := left + toklengths[t];
      end {with}
  end {getnexttoken} ;


procedure gettokenfile;

{ Read next token from token file created by SCAN.

  The token file created by SCAN is organized as an array
  0..diskbufsize of tokens rather than simply a file of tokens.
  This greatly reduces i/o overhead, especially on record oriented
  systems.  What this routine does is modify a global level
  variable called tokenbufindex, which is an index into the array
  of tokens in the token file buffer.  All references to tokens
  are of the form: tokenfile^[nexttokenfile].
}


  begin {gettokenfile}
    while (currentswitch <= lastswitch) and
          ((switches[currentswitch].mlow <= getlow) and
          (switches[currentswitch].mhi <= gethi) or
          (switches[currentswitch].mhi < gethi)) do
      with switches[currentswitch] do
        begin
        switchcounters[s] := switchcounters[s] + v;
        mlow := putlow;
        if false then mhi := puthi;
        currentswitch := currentswitch + 1;
        end;
    if getlow = maxint then
      begin
      getlow := 0;
      gethi := gethi + 1
      end
    else getlow := getlow + 1;
    getnexttoken;
  end {gettokenfile} ;


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

if newdebugger then
      if (switchcounters[symboltable] > 0) and
    (lasttoken.fileindex <> thistoken.fileindex) then {switched files}
      begin
      dbgsourceindex :=
        create_filename(
          thistoken.fileindex,
          do_hash(thistoken.fileindex, filename_length),
          stringtable^[thistoken.fileindex],
          filename_length);
      end;


    if scanalys then
      begin
      if token <> eofsym then scantoken;
      end
    else if token <> eofsym then gettokenfile
    else
      while currentswitch <= lastswitch do
        with switches[currentswitch] do
          begin
          switchcounters[s] := switchcounters[s] + v;
          mlow := putlow;
          if false then mhi := puthi;
          currentswitch := currentswitch + 1;
          end;
  end {gettoken} ;


procedure putintfile;

{ Write intermediate code file.

  The output of ANALYS is an intermediate code file which is used   as
  input to TRAVRS.  The intermediate code file is organized as blocks
  containing arrays 0..diskbufsize of intermediate code records.  What
  this routine does is to calculate an index (nextintcode) into the
  current buffer where the next intermediate code record should be
  written.

  The intermediate code file should be accessed as: 

     tempfiletwo^[nextintcode]:=<data>; putintfile; 
}


  begin {putintfile}
    if false and (putlow = maxint) then
      begin
      putlow := 0;
      puthi := puthi + 1;
      end
    else putlow := putlow + 1;
    if nextintcode = diskbufsize then
      begin
      nextintcode := 0;
      put(tempfiletwo);
      end
    else nextintcode := nextintcode + 1;
  end {putintfile} ;



{ String File Processing

  The string file contains constant data collected by scan and analys.
  It is organized in blocks, each consisting of:

    array [0..diskbufsize] of byte

  The string file is always accessed as:

    stringfile^[nextstringfile]    if caching enabled

    stringblkptr^[nextstringfile]  if caching disabled

  The following routines manipulate this file.
}


procedure putstringfile;

{ Do the equivalent of a "put" on the stringfile.  The global
  "nextstringfile" is incremented, and if the buffer is full an
  actual "put" on the file is done.  The last element is assumed
  to be placed in:

    stringfile^[nextstringfile]    if caching enabled

    stringblkptr^[nextstringfile]  if caching disabled
}


  begin {putstringfile}
    if nextstringfile = diskbufsize then
      begin
      curstringblock := curstringblock + 1;
      nextstringfile := 0;
      if needcaching then
        begin
        put(stringfile);
        end
      else
        begin
        new(stringblkptr);
        stringblkptrtbl[curstringblock] := stringblkptr;
        end;
      end
    else
      begin
      nextstringfile := nextstringfile + 1;
      end;
    if needcaching then stringfiledirty := true;
  end {putstringfile} ;


procedure putbyte {a: integer (value of byte to access) } ;

{ Write the byte "a" to the next location in the string file.  This
  is assumed to be added to the constant table, and the global
  "consttablelimit" or "stringfilecount" is incremented as a result.
}


  begin {putbyte}
    if scanalys then stringfilecount := stringfilecount + 1
    else consttablelimit := consttablelimit + 1;
    if needcaching then stringfile^[nextstringfile] := a
    else stringblkptr^[nextstringfile] := a;
    putstringfile;
  end {putbyte} ;



{ Intermediate File Output

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


procedure genform {f: types (form to emit) } ;

{ If no errors found so far, emit a form to the intermediate file.
}


  begin {genform}
    if emitflag then
      begin
      tempfiletwo^[nextintcode].f := f;
      putintfile;
      end;
  end {genform} ;


procedure genint {i: integer (value to emit) } ;

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
      if (i >= 0) and (i < hostfilelim) then
        begin
        tempfiletwo^[nextintcode].b := i;
        putintfile;
        end
      else
        begin
        tempfiletwo^[nextintcode].b := hostfilelim;
        putintfile;
        fudge.int := i;
        for j := 1 to hostintsize * hostfileunits do
          begin
          tempfiletwo^[nextintcode].b := fudge.byte[j];
          putintfile;
          end;
        end;
  end {genint} ;


procedure genop {o: operator (operator to emit) } ;

{ If no errors are found so far, emit an operator to the intermediate file.
}


  begin {genop}
    if emitflag then
      begin
      tempfiletwo^[nextintcode].o := o;
      putintfile;
      end;
  end {genop} ;


procedure genstmt {s: stmttype (statement to emit) } ;

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
      tempfiletwo^[nextintcode].s := s;
      putintfile;
      end
  end {genstmt} ;




procedure verify
                {set1: tokenset; (acceptable tokens)
                 set2: tokenset; (tokens to finish skip)
                 err: warning (Message if token not ok) } ;

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


procedure verify1
                 {set1: tokenset; (acceptable tokens)
                  err: warning (message if token not ok) } ;

{ Same as verify, except no separate skip set is provided.
}


  begin {verify1}
    verify(set1, [], err);
  end {verify1} ;


procedure verifytoken
                     {tok: tokentype; (acceptable token)
                      err: warning (message if token not ok) } ;

{ Check for a given token (tok) and skip it if found.  If not
  found, emit an error message set by "err".  This is used for
  redundant tokens in the syntax, where parsing can continue if it
  is missing.
}


  begin {verifytoken}
    if token = tok then gettoken
    else warnbetween(err)
  end {verifytoken} ;




procedure enterform
                   {newtyp: types; (type for this form)
                    var where: index; (new entry)
                    var whereptr: entryptr (for access to new entry) } ;

{ Enter a new formentry at the current level.  This also
  gets a dbgsymbol for use with the debugger, and sets the type
  to be newtyp.
}


  begin {enterform}
    if tabletop = tablesize then fatal(tablefull)
    else tabletop := tabletop + 1;
    where := tabletop;
    if bigcompilerversion then whereptr := ref(bigtable[tabletop])
    else awriteaccess(tabletop, whereptr);
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




procedure searchsection
                       {id: integer; (scope id for search)
                        var wherefound: index (resulting name index) } ;

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
    if bigcompilerversion then p := ref(bigtable[twherefound])
    else areadaccess(twherefound, p);
    while (twherefound <> 0) and (p^.name <> id) do
      begin
      twherefound := p^.nextname;
      if bigcompilerversion then p := ref(bigtable[twherefound])
      else areadaccess(twherefound, p);
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




procedure searchlsection
                        {value1: integer; (label value)
                         labellist: labelptr; (root of label list)
                         var wherefound: labelptr (result of search) } ;

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


procedure searchlabels
                      {value1: integer; (label value)
                       var wherefound: labelptr (result of search ) } ;

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


procedure search {var wherefound: index (result of search) } ;

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


procedure searchvariants
                        {var currentrecord: index; (record to search)
                         labvalue: operand (varnt label value) } ;

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
    if bigcompilerversion then ptr := ref(bigtable[currentrecord])
    else areadaccess(currentrecord, ptr);
    t := ptr^.firstvariant;
    while (t <> 0) and not found do
      begin
      if bigcompilerversion then ptr := ref(bigtable[t])
      else areadaccess(t, ptr);
      with ptr^ do
        begin
        t2 := t;
        t := nextvariant;
        t1 := firstlabel;
        while (t1 <> 0) and not found do
          begin
          if bigcompilerversion then ptr1 := ref(bigtable[t1])
          else areadaccess(t1, ptr1);
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


procedure stripsubrange {var objectindex: index (form to be stripped) } ;

{ Convert a subrange type to the base type for use in an expression.
}

  var
    ptr: entryptr;


  begin {stripsubrange}
    if bigcompilerversion then ptr := ref(bigtable[objectindex])
    else areadaccess(objectindex, ptr);
    with ptr^ do if (typ = subranges) then objectindex := parenttype;
  end {stripsubrange} ;


function lower {f: entryptr (form to check) } {: integer} ;

{ Returns the lower bound of "f".  This is meaningful only for
  scalar types.
}


  begin {lower}
    with f^ do
      if typ = ints then lower := targetminint
      else if typ = subranges then lower := lowerord
      else lower := 0;
  end {lower} ;


function upper {f: entryptr (form to check) } {: integer} ;

{ Returns the upper bound of "f".  This is meaningful only for
  scalar types.
}


  begin {upper}
    with f^ do
      case typ of
        ints: upper := targetmaxint;
        bools: upper := 1;
        chars: upper := charsetsize - 1;
        none: upper := 255;
        scalars: upper := lastord;
        subranges: upper := upperord;
        otherwise upper := targetmaxint
        end
  end {upper} ;




function bits {i: integer (value to find size of) } {: integer} ;

{ Returns the number of bits needed to contain the value of i.
}

  var
    b: integer; {Accumulates number of bits}
    value: unsignedint; {Temp so can use a register and shift inst}


  begin {bits}
    if i < 0 then bits := targetintsize * bitsperunit
    else
      begin
      value := i;
      b := 1;
      while value > 1 do
        begin
        b := b + 1;
        value := value div 2;
        end;
      bits := b;
      end;
  end {bits} ;


function sizeof
               {f: entryptr; (Form to get size of)
                packedresult: boolean (set if packed value) }
 {: addressrange} ;

{ Returns the amount of storage needed to contain a value of the type
  specified by "f".  If "packedresult" is set, this is in bits, otherwise
  it is in addressing units.
}

  var
    lowerf: integer; { temp holding lower(f) }
    magnitude: addressrange; {absolute value of max number of bits}


  begin {sizeof}
    if packedresult = f^.bitaddress then sizeof := f^.size
    else if packedresult then
      case f^.typ of
        chars, bools, scalars, subranges, none:
          begin
          if (targetmachine = iapx86) and (f^.size > wordsize) then
            sizeof := defaulttargetintsize * bitsperunit
          else
            begin
            lowerf := lower(f);
            if (lowerf < 0) then
              begin
              magnitude := max(abs(upper(f)), abs(lowerf + 1));
              if magnitude = 0 then sizeof := 1 {handles the case of -1..0}
              else sizeof := bits(magnitude) + 1; {the normal case}
              end
            else sizeof := bits(upper(f));
            end;
          end
        otherwise
          if maxaddr div bitsperunit < f^.size then sizeof := maxaddr
          else sizeof := f^.size * bitsperunit;
        end
    else sizeof := (f^.size + bitsperunit - 1) div bitsperunit;
  end {sizeof} ;




function forcealign
                   {size: addressrange; (value to align)
                    alignment: addressrange; (requirement)
                    packedresult: boolean (size is in bits) }
 {: addressrange} ;

{ Forces "size" to the next higher multiple of "alignment".
  Used to overcome limitations built into much contemporary hardware.
}


  begin {forcealign}
    if packedresult then alignment := alignment * bitsperunit;
    if alignment > 1 then
      size := ((size + alignment - 1) div alignment) * alignment;
    forcealign := size;
  end {forcealign} ;


function unsigned
                 {f: entryptr; (type to check)
                  len: addressrange; (space actually allocated for var)
                  packedelement: boolean (set if packed var) } {: boolean} ;

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


function simplesize {i: integer (value to find size of) } {: integer} ;

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




function negaterealconst {realbuffer: realarray (real constant value) }
 {: realarray} ;

{ Function to negate a real constant, independent of the host
}

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


procedure constant
                  {follow: tokenset; (legal following symbols)
                   dumpvalue: boolean; (true says dump string)
                   var value1: operand (resulting constant) } ;

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
        structtype := noneindex;
        search(t);
        if bigcompilerversion then p := ref(bigtable[t])
        else areadaccess(t, p);
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
              if bigcompilerversion then t1 := ref(bigtable[consttype])
              else areadaccess(consttype, t1);
              value1.oprndlen := sizeof(t1, false);
              value1.cvalue := constvalue;
              gettoken;
              end
            else if (namekind = typename) then
              begin
              { Note:  this is a kluge to get structconst into an overlay}
              structtype := p^.typeindex;
              structfollow := follow;
              if hostopsys <> msdos then ovrlay(xcstruct)
              else cstruct;
              value1 := structvalue;
              { Note: end of kluge, normally a simple call would suffice}
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




function getform {objecttype: entryptr (desired form) } {: types} ;

{ Get the basic form associated with a type.
}


  begin {getform}
    with objecttype^ do
      if typ = subranges then getform := parentform
      else getform := typ;
  end {getform} ;


function identical {left, right: index (types to compare) } {: boolean} ;

{ True if two types are identical, or if either is undefined (to avoid
  redundant messages).
}


  begin {identical}
    identical := (left = right) or (left = noneindex) or (right = noneindex);
  end {identical} ;


function compatible {left, right: index (types to compare) } {: boolean} ;

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
      if bigcompilerversion then lptr := ref(bigtable[left])
      else areadaccess(left, lptr);
      if bigcompilerversion then rptr := ref(bigtable[right])
      else areadaccess(right, rptr);
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
              if bigcompilerversion then
                lptr := ref(bigtable[lptr^.ptrtypename])
              else areadaccess(lptr^.ptrtypename, lptr);
              if bigcompilerversion then
                rptr := ref(bigtable[rptr^.ptrtypename])
              else areadaccess(rptr^.ptrtypename, rptr);

              c := (lptr^.typeindex = noneindex) or
                   (rptr^.typeindex = noneindex);
              if rptr^.refdefined or lptr^.refdefined then
                c := c or compatible(rptr^.typeindex, lptr^.typeindex);
              compatible := c;
              end;
          end;
      end;
  end {compatible} ;




function alignmentof
                    {f: entryptr; (form to check)
                     packedresult: boolean (result is packed) }
 {: alignmentrange} ;

{ Compute the alignment requirement of a type.  This function is needed
  strictly because the alignment of a subrange is kluged to the parent
  type to give better code generation on certain types of machines.  This
  kluge causes trouble with packed types, so is deleted if the result
  is to be used in a packed structure.
}


  begin {alignmentof}
    if packedresult = f^.bitaddress then alignmentof := f^.align
    else if packedresult then alignmentof := f^.align * bitsperunit
    else alignmentof := (f^.align + bitsperunit - 1) div bitsperunit;
  end {alignmentof} ;


procedure seekstringfile {n: integer (byte to access) } ;

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
        if stringfiledirty then put(stringfile);
        stringfiledirty := false;
        seek(stringfile, newblock);
        end
      else
        begin
        stringblkptr := stringblkptrtbl[newblock];
        if stringblkptr = nil then
          begin
          new(stringblkptr);
          stringblkptrtbl[newblock] := stringblkptr;
          end;
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
  as 

    stringfile^[nextstringfile]    if caching is enabled

    stringblkptr^[nextstringfile]  if caching is disabled
}


  begin {getstringfile}
    if nextstringfile = diskbufsize then
      begin
      nextstringfile := 0;
      curstringblock := curstringblock + 1;
      if needcaching then get(stringfile)
      else stringblkptr := stringblkptrtbl[curstringblock];
      end
    else nextstringfile := nextstringfile + 1;
  end {getstringfile} ;

function do_hash{charindex:integer; charlen:integer}{ : integer};

{perform a hashing of the entry of length "charlen" at the location "charindex"
 in the stringtable, using the function concealed inside the debugger package.}

  function nextch:char;
  begin {nextch}
    nextch := stringtable^[charindex];
    charindex := charindex + 1
  end {nextch} ;

begin {do_hash}
if newdebugger then do_hash := p_debughash(charlen, nextch);
end {do_hash} ;


