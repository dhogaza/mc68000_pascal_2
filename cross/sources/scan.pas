{$nomain}
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




function p_prctyp: integer;
  external;

{ Return processor type for 68000.
}


procedure dumpidentifiers;

  forward;

procedure seekstringfile (n: integer {byte to access} ) ;

  forward;

{ Utility procedures - Issue an error message, and handle the output files.
}


procedure fatal(err: warning);

{ Issue an error message and set the "fatalflag" to stop compilation.
}


  begin {fatal}
    fatalflag := true;
    if scanalys then seekstringfile(stringfilecount);
    dumpidentifiers;
    ovrlay(xcloses);
    sourcelevel := 0;
    warnat(err, lastline, chpos);
  end {fatal} ;




procedure puttoken;

{ Put the current token to the token file.

  This is encoded to the hostfilebyte level, with only the data
  required being sent.  This reduces the amount of intermediate file
  I/O required, resulting in a significant speedup on machines with
  slow disks.

  Since this is a high-bandwidth spot, code put sequences are written
  in line rather than being isolated in procedures.

  Assumptions built in are:

        1.  "left" and "right" will fit in a hostfilebyte

        2.  a target character will fit in a hostfilebyte

  These seem likely enough to be true that the speed-up available
  by making these assumptions is worth taking.
}

  var
    dif: hostfilebyte; {difference in line numbers}


  procedure puttempfile;

{ Does the equivalent of a put on the token file.  Actually the file is
  a file of blocks, and an actual put is done only if the block is full.
  The reference is always:
    tempfileone^[tokenbufindex].<field>
}


    begin {puttempfile}
      if tokenbufindex = diskbufsize then
        begin
        tokenbufindex := 0;
        put(tempfileone);
        end
      else tokenbufindex := tokenbufindex + 1;
    end {puttempfile} ;


  procedure putint(i: integer {value to put} );

{ Puts an integer value to the token file as successive bytes.
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


    begin {putint}
      if (i >= 0) and (i < hostfilelim) then
        begin
        tempfileone^[tokenbufindex].byte := i;
        if tokenbufindex = diskbufsize then
          begin
          tokenbufindex := 0;
          put(tempfileone);
          end
        else tokenbufindex := tokenbufindex + 1;
        end
      else
        begin
        tempfileone^[tokenbufindex].byte := hostfilelim;
        puttempfile;
        fudge.int := i;
        for j := 1 to hostintsize * hostfileunits do
          begin
          tempfileone^[tokenbufindex].byte := fudge.byte[j];
          puttempfile;
          end;
        end;
    end {putint} ;


  procedure putreal;

{ Put a real value to the token file as successive bytes
}

    var
      { this fudges a real into a bytes.  The constant "32" is }
      { simply a large enough number to include all probable systems. }
      fudge:
        record
          case boolean of
            true: (rl: realarray);
            false: (byte: packed array [1..32] of hostfilebyte);
        end;
      j: 1..32; {induction var}


    begin {putreal}
      for j := 1 to 32 do fudge.byte[j] := 0;
      fudge.rl := nexttoken.realvalue;
      for j := 1 to size(realarray) do
        begin
        tempfileone^[tokenbufindex].byte := fudge.byte[j];
        puttempfile;
        end;
    end {putreal} ;


  begin {puttoken}
    with nexttoken do
      begin

      { Put line increments as necessary }
      while lasttokenline < line do
        begin
        dif := min(line - lasttokenline, hostfilelim);
        if dif = 1 then
          begin
          tempfileone^[tokenbufindex].toke := lineinc;
          if tokenbufindex = diskbufsize then
            begin
            tokenbufindex := 0;
            put(tempfileone);
            end
          else tokenbufindex := tokenbufindex + 1;
          end
        else
          begin
          tempfileone^[tokenbufindex].toke := lineadd;
          puttempfile;
          tempfileone^[tokenbufindex].byte := dif;
          puttempfile;
          end;
        lasttokenline := lasttokenline + dif;
        end;

      if lastbaseline <> baseline then
        begin
        tempfileone^[tokenbufindex].toke := newfile;
        puttempfile;
        putint(baseline);
        putint(fileindex); { filename pointer }
        lastbaseline := baseline;
        end;

      tempfileone^[tokenbufindex].toke := token;
      if tokenbufindex = diskbufsize then
        begin
        tokenbufindex := 0;
        put(tempfileone);
        end
      else tokenbufindex := tokenbufindex + 1;

      if newdebugger then putint(filepos);

      tempfileone^[tokenbufindex].byte := left;
      if tokenbufindex = diskbufsize then
        begin
        tokenbufindex := 0;
        put(tempfileone);
        end
      else tokenbufindex := tokenbufindex + 1;

      if token in [ident, intconst, realconst, dblrealconst, charconst,
         stringconst] then
        begin
        tempfileone^[tokenbufindex].byte := right;
        if tokenbufindex = diskbufsize then
          begin
          tokenbufindex := 0;
          put(tempfileone);
          end
        else tokenbufindex := tokenbufindex + 1;

        case token of
          ident:
            begin
            putint(key);
            putint(keypos);
            end;
          intconst: putint(intvalue);
          charconst:
            begin
            tempfileone^[tokenbufindex].byte := intvalue;
            puttempfile;
            end;
          realconst, dblrealconst: putreal;
          stringconst:
            begin
            putint(pos);
            putint(len);
            end;
          end {case}
        end;
      end {with}
  end {puttoken} ;


procedure puttokenfile;

{ Do the equivalent of a put to the token file.  The value of the global
  variable "token" is encoded by "puttoken" and written in a packed format
  to the tokenfile.  A count of tokens is kept and used to indicate the
  place in the file where embedded switches are found.
}


  begin {puttokenfile}
    { update token count -- two words for small computers }
    if putlow = maxint then
      begin
      putlow := 0;
      puthi := puthi + 1
      end
    else putlow := putlow + 1;
    puttoken;
  end {puttokenfile} ;


procedure seekstringfile {n: integer (byte to access) } ;

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
    else nextstringfile := nextstringfile + 1; { not full, update pointer }
    if needcaching then stringfiledirty := true;
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

    { Expand filename in place }

    temp := filename_length;
    p_expfnm(source[sourcelevel], filename, temp);
    filename_length := temp;

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
      if fastread then
        begin
        if linepos >= linesize then
          begin

          { Handle case where the first line in a concatenated file is blank.
          }
          if (linesize = 0) and eoln(source[sourcelevel]) then
            readln(source[sourcelevel]); {skip past eoln}

          getpos(source[sourcelevel], i1, i2);
          case hostopsys of
            unix:new_filepos := i1; {only one longword needed for Unix}
            vms: new_filepos:= i1*4096 + i2 mod 4095;
                 {this dirty trick to pack 48 bits into 32}
          end;
          p_rdsfst(source[sourcelevel], currentline, linesize);
          linepos := 0;
          end;
        linepos := linepos + 1;
        nextch := currentline^[linepos];
        end
      else
        begin
        if linepos >= inputbufsize then
          begin
          getpos(source[sourcelevel], i1, i2);
          case hostopsys of
            unix:new_filepos := i1; {only one longword needed for Unix}
            vms: new_filepos:= i1*4096 + i2 mod 4095;
                 {this dirty trick to pack 48 bits into 32}
          end;
          read(source[sourcelevel], currentbuf);
          linepos := 0;
          end;
        linepos := linepos + 1;
        nextch := currentbuf[linepos];
        end;
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
        ovrlay(xcloses);
        with saveinput[sourcelevel] do
          begin
          nextch := savech; {restore next char to read}
          if fastread then
            begin
            currentline := saveline; {restore input line}
            linesize := savelen; {restore current line length}
            end
          else currentbuf := savebuf; {restore input line}
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
        ovrlay(xopennext);
        if morefiles then
          begin
          loginputfilename;
          if fastread then
            begin
            linesize := 0;
            linepos := 0;
            end
          else linepos := inputbufsize;
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
        getpos(source[sourcelevel], i1, i2);
        case targetopsys of
          unix:new_filepos := i1; {only one longword needed for Unix}
          vms: new_filepos:= i1*4096 + i2 mod 4095;
               {this dirty trick to pack 48 bits into 32}
        end;
        if fastread then
          begin
          linesize := 0; {force new line read}
          linepos := 0;
          end
        else linepos := inputbufsize;
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
      if fastread then
        begin
        if linepos < linesize then {normal case -- fill nextch}
          begin
          linepos := linepos + 1;
          nextch := currentline^[linepos];
          if (nextch = chr(rubout)) or (nextch < ' ') then special
          else charcount := charcount + 1;
          if charcount > linelen then dolinetoolong;
          end
        else if eof(source[sourcelevel]) then doeof
        else if eoln(source[sourcelevel]) then doeoln
        else getnextch; {read a new line or fill the buffer again}
        end
      else
        begin
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
        end
    until (not skippingblanks) or (ch <> ' ');
  end {getch} ;



{ Environment file handling.  Data in the string table and hash table must
  be written out along with various global variables.
}

procedure swriteenv;

{ Save scanner state in the environment file
}

  var envirblock: envirrecord; {temp to hold one environment record}
      hashentry, empty: hashindex;
      fileentry: 0..hashtableentriesperblock;
      i: integer; {for stepping through various tables}

  procedure puthashblock;

    begin {puthashblock}
      if fileentry = hashtableentriesperblock then
        begin
        write(enviroutfile, envirblock.ediskblock);
        fileentry := 0;
        end
      else fileentry := fileentry + 1;
    end   {puthashblock};

  procedure copyversion(s: packed array[l..h: integer] of char);

    var
      i: integer; {induction}

    begin {copyversion}
      for i := 1 to min(40, h - l + 1) do
        envirblock.eversion[i] := s[i];
      for i := h - l + 2 to 40 do
        envirblock.eversion[i] := ' ';
    end {copyversion};      

  begin {swriteenv}

    with envirblock do
      begin {save scanner variables}
      elastswitch := lastswitch;
      estringfilecount := stringfilecount;
      enextstringfile := nextstringfile;
      ecurstringblock := curstringblock;
      estringtabletop := stringtabletop;
      einsertions := insertions;
      ecodesect_string := codesect_string;
      ecodesect_strlength := codesect_strlength;
      emodule_string := module_string;
      emodule_strlength := module_strlength;
      eident_string := ident_string;
      eident_strlength := ident_strlength;
      ecodesection := codesection;
      eshortsection := shortsection;
      eobjversion := objversion;
      edatasection := datasection;
      eownsect_string := ownsect_string;
      eownsect_strlength := ownsect_strlength;
      eswitcheverplus := switcheverplus;
      copyversion(ournamestring);
      end;
    write(enviroutfile, envirblock.ediskblock);

    fileentry := 0;
    for i := 1 to lastswitch do
      begin
      envirblock.eswitches[fileentry].s := switches[i].s;
      envirblock.eswitches[fileentry].v := switches[i].v;
      if fileentry = switchesperblock then
        begin
        write(enviroutfile, envirblock.ediskblock);
        fileentry := 0;
        end
      else fileentry := fileentry + 1;
      end;
    if fileentry > 0 then write(enviroutfile, envirblock.ediskblock);

    hashentry := 0;
    fileentry := 0;

    { write out hash table, with zero entries compressed out}

    repeat

      empty := 0;
      while (hashtable[hashentry].pos = 0) and
            (hashentry < hashtablesize) do
        begin
        hashentry := hashentry + 1;
        empty := empty + 1;
        end;

      if empty > 0 then
        begin
        envirblock.ehashblock[fileentry].pos := - empty;
        puthashblock;
        end;

      if hashentry < hashtablesize then
        begin
        with envirblock.ehashblock[fileentry] do
          begin
          pos := hashtable[hashentry].pos;
          len := hashtable[hashentry].len;
          key := hashtable[hashentry].key;
          end;
        puthashblock;
        hashentry := hashentry + 1;
        end;

    until hashentry = hashtablesize; {last entry in hashtable is never used}

    if fileentry > 0 then
      write(enviroutfile, envirblock.ediskblock);

    if needcaching then
      begin {code depends on stringfile and envirfile both being of diskblock}
      seekstringfile(0);
      for i := 1 to stringfilecount do
        begin
        envirblock.ediskblock[nextstringfile] := stringfile^[nextstringfile];
        if nextstringfile = diskbufsize then
          begin
          nextstringfile := 0;
          if i < stringfilecount then get(stringfile);
          write(enviroutfile, envirblock.ediskblock);
          end
        else nextstringfile := nextstringfile + 1;
        end;
        if nextstringfile > 0 then
          write(enviroutfile, envirblock.ediskblock);
      end
    else
      for i := 1 to (stringfilecount + diskbufsize) div (diskbufsize + 1) do
        write(enviroutfile, stringblkptrtbl[i]^);

    fileentry := 0;
{
    Stringtabletop is the index of the LAST valid entry in
    the stringtable.  Entry 0 is unused.
}
    for i := 1 to stringtabletop do
      begin
      envirblock.ediskblock[fileentry] := ord(stringtable^[i]);
      if fileentry = diskbufsize then
        begin
        fileentry := 0;
        write(enviroutfile, envirblock.ediskblock);
        end
      else fileentry := fileentry + 1;
      end;
    if fileentry > 0 then
      write(enviroutfile, envirblock.ediskblock);
  end   {swriteenv};

procedure sreadenv;

{ Read environment file written by previous "define" compilation.
  There is some tricky stuff due to the way that input files are remembered.
  We essentially must reinitialize "filerememberlist" and relog the input
  file name.
}

  var envirblock: envirrecord; {temp to hold one environment record}
      hashentry, empty: hashindex;
      fileentry: 0..hashtableentriesperblock;
      i: integer; {for stepping through various tables}

  function equalversion(s: packed array[l..h: integer] of char): boolean;

    var
      i: integer; {induction}
      equal: boolean;

    begin {equalversion}
      equal := true;
      i := 1;
      while equal and (i <= min(40, h - l + 1)) do
        begin
        equal := envirblock.eversion[i] = s[i];
        i := i + 1;
        end;
      equalversion := equal;
    end {equalversion};      

     
  begin {sreadenv}

    read(envirinfile, envirblock.ediskblock);

    with envirblock do
      begin {save scanner variables}
      if not equalversion(ournamestring) then abort(wrongversionenv);
      lastswitch := elastswitch;
      stringfilecount := estringfilecount;
      nextstringfile := enextstringfile;
      curstringblock := ecurstringblock;
      stringtabletop := estringtabletop;
      insertions := einsertions;
      codesect_string := ecodesect_string;
      codesect_strlength := ecodesect_strlength;
      module_string := emodule_string;
      module_strlength := emodule_strlength;
      ident_string := eident_string;
      ident_strlength := eident_strlength;
      codesection := ecodesection;
      shortsection := eshortsection;
      objversion := eobjversion;
      datasection := edatasection;
      ownsect_string := eownsect_string;
      ownsect_strlength := eownsect_strlength;

      {Switch handling is special.  If this compilation is compiled
       with /double (for example), the /define compilation must have
       been compiled /double as well.  However, if /double is not specified
       for this compilation but was for the /define compilation, it is
       forced for this compilation as though it had occurred on the
       command line.
      }

      if (switcheverplus[doublereals] > eswitcheverplus[doublereals]) or
         (switcheverplus[largemodel] > eswitcheverplus[largemodel]) or
         (switcheverplus[shortintegers] > eswitcheverplus[shortintegers])
      then
        begin
        write('Environment file options inconsistent with current options');
        abort(inconsistent);
        end;
      end;

    fileentry := 0;
    for i := 1 to lastswitch do
      begin
      if fileentry = 0 then read(envirinfile, envirblock.ediskblock);
      with envirblock.eswitches[fileentry] do
        begin
        switchcounters[s] := switchcounters[s] + v;
        if switchcounters[s] > 0 then switcheverplus[s] := true;
        switches[i].s := s;
        switches[i].v := v;
        switches[i].mhi := 0;
        switches[i].mlow := 0;
        end;
      fileentry := (fileentry + 1) mod (switchesperblock + 1);
      end;

    hashentry := 0;
    fileentry := 0;

    repeat
      if fileentry = 0 then read(envirinfile, envirblock.ediskblock);
      with envirblock do
      if ehashblock[fileentry].pos > 0 then
        begin
        hashtable[hashentry].pos := ehashblock[fileentry].pos;
        hashtable[hashentry].len := ehashblock[fileentry].len;
        hashtable[hashentry].key := ehashblock[fileentry].key;
        hashentry := hashentry + 1;
        end
      else hashentry := hashentry - ehashblock[fileentry].pos;
      fileentry := (fileentry + 1) mod (hashtableentriesperblock + 1);
    until hashentry = hashtablesize;

    if needcaching then
      begin {code depends on stringfile and envirfile both being of diskblock}
      seekstringfile(0);
      for i := 1 to stringfilecount do
        begin
        if nextstringfile = 0 then read(envirinfile, envirblock.ediskblock);
        stringfile^[nextstringfile] := envirblock.ediskblock[nextstringfile];
        if nextstringfile = diskbufsize then
          begin
          nextstringfile := 0;
          put(stringfile);
          end
        else nextstringfile := nextstringfile + 1;
        end
      end
    else
      for i:=1 to (stringfilecount + diskbufsize) div (diskbufsize + 1) do
        begin
        if stringblkptrtbl[i] = nil then new(stringblkptrtbl[i]);
        read(envirinfile, stringblkptrtbl[i]^);
        end;

    fileentry := 0;
{
    Stringtabletop is the index of the LAST valid entry in
    the stringtable.  Entry 0 is unused.
}
    for i := 1 to stringtabletop do
      begin
      if fileentry = 0 then read(envirinfile, envirblock.ediskblock);
      stringtable^[i] := chr(envirblock.ediskblock[fileentry]);
      fileentry := (fileentry + 1) mod (diskbufsize + 1);
      end;

    filerememberlist := nil;
    loginputfilename;

  end   {sreadenv};

procedure dumpstr {(len: lineindex; buf, dumplen: boolean)} ;

{ Copy stringbuf[buf] to the string file.
}

  var
    i: 1..linelen;


  begin {dumpstr}

    seekstringfile(stringfilecount);
    stringfilecount := stringfilecount + len;

    if dumplen then
      begin
      if needcaching then
        stringfile^[nextstringfile] := ord(stringbuf[buf, 0])
      else stringblkptr^[nextstringfile] := ord(stringbuf[buf, 0]);
      putstringfile;
      len := len - 1;
      end;

    for i := 1 to len do
      begin
      if needcaching then
        stringfile^[nextstringfile] := ord(stringbuf[buf, i])
      else stringblkptr^[nextstringfile] := ord(stringbuf[buf, i]);
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
      if needcaching then stringfile^[nextstringfile] := ord(stringtable^[i])
      else stringblkptr^[nextstringfile] := ord(stringtable^[i]);
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
              if false then mhi := puthi; {mark for future passes}
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
      stringlen: lineindex; {string length}
      i: lineindex; {induction var}
      tempch: char; {temporary holder for ch}


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
          if scanalys then pos := - 1 {don't dump yet}
          else
            begin
            pos := stringfilecount + 1;
            dumpstr(len + 1, curstringbuf, true);
            end;
          end
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
          ovrlay(xopens);
          with saveinput[sourcelevel] do
            begin
            savech := nextch;
            savepos := linepos;
            saveendofline := endofline;
            savefileindex := current_fileindex; { stringtable index of filename}
            savefilename_length := oldfilename_length;

            if fastread then
              begin
              saveline := currentline;
              savelen := linesize;
              linesize := 0; {force reading of a new line}
              linepos := 0;
              end
            else
              begin
              savebuf := currentbuf;
              linepos := inputbufsize; {force reading of a new line}
              end;
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
      digitindex = 0..maxinbuf;
      byte = 0..maxbytevalue;

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
              abort(inconsistent);
              realmode := DECformat;
              end;
            end;

          if switcheverplus[doublereals] then
            realmode := realmode + doubleprecision
          else realmode := realmode + singleprecision;

          for j := 1 to maxrealwords do realresult[j] := 0; {clear the result
            first}
          p2readreal(getrealch, realresult, realerror, realmode, isdouble);
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

    if hostmachine = mc68000 then
      if not first_token_seen then
        { Set default processor switch for 68000 native compilers by testing
          the hardware.  This not done if the user has specified one of
          the 68000, 68020 or 68881 switches.
        }
        if (targetmachine = mc68000) and
           (targetmachine = hostmachine) and
          (hostopsys = targetopsys) and
           not (switcheverplus[fpc68881] or switcheverplus[cpu68000] or
                switcheverplus[cpu68020]) then
          case p_prctyp of
            1:
              begin
              switcheverplus[cpu68000] := true;
              nextswitcheverplus[cpu68000] := true;
              end;
            2:
              begin
              switcheverplus[cpu68020] := true;
              nextswitcheverplus[cpu68020] := true;
              end;
            3:
              begin
              switcheverplus[fpc68881] := true;
              nextswitcheverplus[fpc68881] := true;
              switcheverplus[cpu68020] := true;
              nextswitcheverplus[cpu68020] := true;
              end;
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

    if not scanalys or switcheverplus[test] then puttokenfile;

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
      if i > maxscanswitch then
        begin
        write('scan switches init error');
        abort(inconsistent);
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
       (diskbufsize + 1) div rdup(size(switchblock)) then
      writeln('Environment files:  SWITCHESPERBLOCK should be ',
             (diskbufsize + 1) div rdup(size(switchblock)) - 1: 1);

    if hashtableentriesperblock + 1 <>
       (diskbufsize + 1) div rdup(size(hashtableblock)) then
      writeln('Environment files:  HASHTABLEENTRIESPERBLOCK should be ',
             (diskbufsize + 1) div rdup(size(hashtableblock)) - 1: 1);

    if proctableentriesperblock + 1 <>
       (diskbufsize + 1) div rdup(size(proctableentry)) then
      writeln('Environment files:  PROCTABLEENTRIESPERBLOCK should be ',
             (diskbufsize + 1) div rdup(size(proctableentry)) - 1: 1);

    { End of special configuration checks}

    curstringbuf := true;
    nextswitchread := false;

    stringfilecount := 0;
    stringtablelimit := 0;
    stringtabletop := 0;
    curstringblock := 1;
    if needcaching then
      begin
      {nothing to do; stringfile is opened in module files}
      end
    else
      begin
      new(stringblkptr);
      stringblkptrtbl[1] := stringblkptr;
      for i := 2 to maxstringblks do stringblkptrtbl[i] := nil;
      end;
    new(stringtable);

    initscanswitches;
    inittokentable;

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
    if fastread then linepos := 0
    else linepos := inputbufsize;
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
    ovrlay(xopennext);
    loginputfilename;

    nextch := ' ';
    getch;
  end {scan1} ;


procedure scan2;


  begin {scan2}
    if scanalys then seekstringfile(stringfilecount);

    dumpidentifiers;
    ovrlay(xcloses);
    sourcelevel := 0;

    with nexttoken do
      begin
      token := eofsym;
      line := lastline;
      left := chpos;
      right := chpos
      end;

    if not scanalys or switcheverplus[test] then
      begin
      puttoken;
      if tokenbufindex > 0 then put(tempfileone);
      end;

    stringtablelimit := stringfilecount + stringtabletop;

    if needcaching then put(stringfile);

    if not scanalys then
      if switcheverplus[defineswitch] then swriteenv;

    dispose(stringtable);
  end {scan2} ;


procedure scan;

{ Main scan procedure, see forward declaration for more data
}


  begin {scan}

    scan1;

    if switcheverplus[environswitch] then sreadenv;

    repeat
      scantoken;
    until (fatalflag or endofinput);

    { align strings }

    if not switcheverplus[defineswitch] then
      while stringfilecount mod stringroundoff <> 0 do
        begin
        if needcaching then stringfile^[nextstringfile] := 0
        else stringblkptr^[nextstringfile] := 0;
        putstringfile;
        stringfilecount := stringfilecount + 1;
        end;

    scan2;

  end {scan} ;
