{[b+]}


{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: All
  System: All


 Last modified by KRIS on 21-Nov-1990 17:18:48
 Purpose:
Update release version for PU-VV0-GS0 at 2.3.0.1

}
{-----------------------------------------------------------------------|
|                                                                       |
|                 PASCAL PROCEDURAL CROSS-REFERENCER                    |
|                                                                       |
|          (c) Copyright 1979 A.H.J.Sale, Southampton, England.         |
|                                                                       |
|   DEVELOPMENT                                                         |
|   This program is a software tool developed  from  a  prototype  by   |
|   A.J.Currie at the University of Southampton, England.  The proto-   |
|   type of 231 lines of source text was used firstly as a basis  for   |
|   extensions,   and   then   rewritten  to  assure  correctness  by   |
|   A.H.J.Sale, on leave from the University  of  Tasmania  and  then   |
|   stabilized at 1979 December 4; the  development  time  being  es-   |
|   timated at 4 man-days from prototype to production.                 |
|                                                                       |
|   PURPOSE                                                             |
|   The program reads Pascal source programs and produces two  tables   |
|   as  output.  One documents all procedure or function headings  in   |
|   a  format  that  illustrates  lexical  nesting.  The other tables   |
|   gives the locations of heading, block, and  body  for  each  pro-   |
|   cedure and function, and what procedures and functions it immedi-   |
|   ately calls.                                                        |
|                                                                       |
|   There is a User Manual for this program; if it has not been  pro-   |
|   vided with your installation write to:                              |
|             Department of Information Science                         |
|             University of Tasmania                                    |
|             P.O.Box 252C, G.P.O Hobart                                |
|             Tasmania  7001                                            |
|   and ask for the Technical Report on referencer, if  it  is  still   |
|   available.  The program is written to be portable and is believed   |
|   to be in Standard Pascal.                                           |
|                                                                       |
|   Permission is granted to copy this program, store it in a comput-   |
|   er  system,  and distribute it, provided that this header comment   |
|   is retained in all copies.                                          |
|                                                                       |
|-----------------------------------------------------------------------}

{ The above comment is included as requested.  The source for this program
  was obtained from Pascal News, and modified at Oregon Software to add the
  following capabilities:

        1.  Command line scanning.
        2.  Multiple source and %include files.
        3.  Remove requirement for "program" header
        4.  Allow external procedure (no main body)
        5.  Reverse (called by) references
        6.  Tabs in input lines.

  In addition, the program as distributed contained a major bug in the
  handling of field names, and any field name in a record with the
  same name as a procedure would mask any use of the procedure within
  that scope.  Removing the problem completely requires a large increase
  in the complexity of the analysis, and was judged infeasable.  The
  current limitation is that an unqualified reference to a field with
  the same name as a procedure will be treated as a reference to that
  procedure.  This can only occur within a "with" statement.  This
  restriction may be removed in a later release.

}


  label
    99; {exit for error or eof}

  const

    sigcharlimit = 16; {This constant is the number of significant characters
                        kept in the identifier entries. It can readily be
                        changed. It is not advised that it be reduced below
                        10 (reserved words get to 9). }

    uclcdisplacement = 32; {This constant is used to convert upper-case
                            letters to lower-case and vice-versa. It should
                            be equal to ord('a') - ord('A').}

    linelimit = 161; {This constant determines the size of the input line
                      buffer. The maximum acceptable input line is one
                      smaller because a sentinel space is appended to every
                      line.}

    linewidth = 80; {This constant determines the default maximum width of the
                     printing of the second cross-reference table. The
                     program deduces how many names will fit on a line.}

    indentation = 4; {This determines the indentation of the lex-levels.}

    prefix = 9; {width of crossref prefix data}

    { These constants are used for the sketchy syntax analysis. }
    { They are collected here so that their lengths may be altered}
    { if sigcharlimit is altered. }
    sprogram = 'program         ';
    sprocedure = 'procedure       ';
    sfunction = 'function        ';
    slabel = 'label           ';
    sconst = 'const           ';
    stype = 'type            ';
    svar = 'var             ';
    sbegin = 'begin           ';
    scase = 'case            ';
    send = 'end             ';
    sforward = 'forward         ';
    srecord = 'record          ';
    sinclude = 'include         ';
    spaces = '                ';

    labelsize = 13; {max size used for labeling files}

    blanklabel = '             '; {blank file label}

    fakeprog = '.MAIN.          '; {fake program name if none}

    {ASCII special character handline}

    ff = 14B; {ascii form feed}
    ht = 11B; {ascii tab character}

    tabinterval = 8; {DEC standard tab interval}

    sourcedepth = 8; {max source nesting depth for includes}


  type
    natural = 0..maxint;
    positive = 1..maxint;

    prefixname = packed array [1..prefix] of char;

    sigcharrange = 1..sigcharlimit;

    pseudostring = packed array [sigcharrange] of char;
    stringcases = packed array [sigcharrange] of boolean;

    linesize = 1..linelimit;
    lineindex = 0..linelimit;

    setofchar = set of char;

    prockind = (fwdhalf, allfwd, shortform, formal, outside, outsidedef,
                notproc);

    ptrtoentry = ^entry;

    listofusages = ^usagecell;

    ptrtostackcell = ^stackcell;

    tokentype = (othersy, namesy, lparensy, rparensy, colonsy, semicolsy,
                 periodsy, assignsy, subrangesy);

    filelabel = packed array [1..labelsize] of char;

    lineref =
      record
        line: natural; {line number of reference}
        fileref: filelabel; {file of reference}
      end;

    entry =
      record
        procname: pseudostring;
        procuppers: stringcases;
        linenumber: lineref; {line where header found}
        startofbody: lineref; {line where body is found}
        left, right: ptrtoentry;
        before, after: ptrtoentry;
        calls: listofusages;
        called: listofusages;
        localtree: ptrtoentry;
        case status: prockind of
          fwdhalf, shortform, formal, outside, notproc: ();
          allfwd, outsidedef:
            (forwardblock: lineref {line where forward label found} );
      end;

    usagecell =
      record
        what: ptrtoentry;
        next: listofusages;
      end;

    stackcell =
      record
        current: ptrtoentry;
        scopetree: ptrtoentry;
        substack: ptrtostackcell;
      end;

    {source include control}

    sourceindex = 1..sourcedepth; {current source nexting level}

    sourcedescriptor =
      record
        inputfile: text; {current input file}
        line: array [linesize] of char; {current input line}
        chno, total: lineindex; {char pointers in line}
        lineno: natural; {line number of this line}
        currentlabel: filelabel; {current file label}
      end;

    sourcestack = array [sourceindex] of sourcedescriptor;


  var

    lowers: stringcases;
    i: sigcharrange; {induction var}

    depth: natural;
    level: - 1..maxint;
    pretty: natural;

    adjustment: (first, other);
    movement: integer;
    printflag: boolean;
    errorflag: boolean;

    ch: char;

    token: tokentype;
    symbol: pseudostring;
    symbolcase: stringcases;

    savesymbol: pseudostring;

    superroot: ptrtoentry;
    stack: ptrtostackcell;

    alphabet: setofchar;

    alphanums: setofchar;
    uppercase: setofchar;
    lowercase: setofchar;
    digits: setofchar;
    usefulchars: setofchar;

    namesperline: positive;
    outwidth: integer;

    lastlabel: filelabel; {last label printed}
    currentlabel: filelabel; {label for current lines}


    outputfile: text; {result file for reference table}
    source: sourcestack; {source files}
    sourcelevel: sourceindex; {current include depth}


{*---------------------------------*
 | Read and Process Command String |
 *---------------------------------*}

  const

    InputExt = 'pas';
    OutputExt = 'prf';
    CSIprompt = 'PRF>'; {to use if prompting}
    %include 'csicon'; {constants for VMS opsys configuration}   
   type
    ArgType = (UnknownArg, 
               InputFileArg, OutputFileArg,
               IncludeArg, WidthArg,
               MalformedArg, MissingArg);
    SubArgType = 0..0;

    %include csityp; {types used in csi processing}

    ListPtr = ^List;
    List = record
      Next: Listptr;
      Arg: ArgValue;
      end;

  var
    InputListHead, NextInput: ListPtr; {input filename list}
    IncludeListHead, NextInclude: ListPtr; {include filename list}
    currentfile: ArgValue; {currently active filename}
    iExtVar, oExtVar: FileExt;
    
  %include csipro;  {standard CSI procedures}
  %include getcs;   {Get command string procedures}
  %include fixarg;  {FixFileArg procedure}
  %include fixinc;  {FixFileInclude procedure}
  %include cnvnum;  {CnvNumericArg procedure}

  procedure exitst(i: Integer);

  { Procedure to exit cleanly with specified status.
  } 
    external;


  procedure csi;

  { Retrieve and process command string.
  }
    const
      {[f-]}
      ArgDefs = ArgDefTable (
        (('                ',  1,  0), 0, OptionalArg, NullArg),
        (('Input_File      ', 11, 10), 1, RequiredArg, FileArg),
        (('Output_File     ',  1, 11), 2, OptionalArg, FileArg),
        (('Include_Dir     ',  1, 11), 0, OptionalArg, FileArg),
        (('Width           ',  1,  5), 0, OptionalArg, StringArg),
        (('                ',  1,  0), 0, OptionalArg, NullArg),
        (('                ',  1,  0), 0, OptionalArg, NullArg));
      {[f+]}

    type
      ErrorMsg = (UnknownArgMsg, MalformedArgMsg, MissingArgMsg,
                  ExtraOutputMsg, BadWidthMsg);

    var
      OutputFlg: (No, Yes, Unknown); {current condition of output file}
      InputArg, OutputArg: ArgValue; {file arguments}
      error: Boolean; {error flag}
      j,k: iArgValue;

    procedure SetupError(msg: ErrorMsg;
                         arg: ArgValue);

    { Print error message and flag error occurred.
    }

      begin
        case msg of
          UnknownArgMsg: write('Unknown argument');
          MalformedArgMsg: write('Bad argument syntax');
          MissingArgMsg: write('Required argument missing');
          ExtraOutputMsg: write('Extra output file');
          BadWidthMsg: write('Value out of range');
          end;
        if arg.Len > 0 then write(' (', arg.txt: arg.Len, ')');
        writeln;
        error := true;
      end;


    procedure ProcessArg(arg: ArgValue; typ: ArgType);

    { Process arguments retrieved by GetCS.
    }

      var
        NewElement: ListPtr; {new input file list element}
        numerror: boolean; {error flag}

      begin
        case typ of
          UnknownArg: SetupError(UnknownArgMsg, arg);
          InputFileArg:
            begin
	    FixFileArg(arg, ActualFile, iExtVar, arg);
            new(NewElement);
            NewElement^.next := nil;
            NewElement^.arg := arg;
            if InputListHead = nil then InputListHead := NewElement else
              NextInput^.next := NewElement;
            NextInput := NewElement;
            end;
          OutputFileArg:
            begin
            if OutputFlg <> Unknown then SetupError(ExtraOutputMsg, arg);
            OutputArg := arg;
            OutputFlg := Yes;
            end;
         IncludeArg:
            begin
            new(NewElement);
            NewElement^.next := nil;
            NewElement^.arg := arg;
            if IncludeListHead = nil then IncludeListHead := NewElement else
              NextInclude^.next := NewElement;
            NextInclude := NewElement;
            end;
          WidthArg:
            begin
            CnvNumericArg(arg, outwidth, numerror);
            if numerror or (outwidth < 2 * sigcharlimit + 5) then
              SetupError(BadWidthMsg, arg);
            end;
          MalformedArg: SetupError(MalformedArgMsg, arg);
          MissingArg: SetupError(MissingArgMsg, arg);
          end;
      end;


    begin {csi}
      InputListHead := nil;
      IncludeListHead := nil;
      OutputFlg := Unknown;
      for k := 1 to ExtLen do iExtvar[k] := InputExt[k];
      for j := 1 to ExtLen do oExtVar[j] := OutputExt[j];
      error := false;
      GetCS(ArgDefs, ProcessArg);
      if error then exitst(4);
      if OutputFlg = Yes then begin
        FixFileArg(OutputArg, ActualFile, oExtVar, OutputArg) end
      else FixFileArg(NextInput^.arg, DefaultFile, oExtVar, OutputArg);
      rewrite(Outputfile, OutputArg.txt);
      NextInput := InputListHead;
    end; {csi}


  function GetNextInput(var nextfile: Argvalue ): boolean;

  { Retrieve next input file from saved argument list and return "true"
    if found.
  }

    var
      OldInput: ListPtr; {save list element for dispose}      

    begin
      if NextInput <> nil then
        begin
        GetNextInput := true;
        nextfile := NextInput^.arg;
        OldInput := NextInput;
        NextInput := NextInput^.next;
        dispose(OldInput);    
        end
      else GetNextInput := false;
    end; {GetNextInput}


  procedure openerror(includefile: boolean {true if include file});

  { Crash due to a file open error.
  }

    begin
      write('Can''t open ');
      if includefile then write('include file ');
      with currentfile do writeln('''', txt: len, '''');
      exitst(4);
    end;



  procedure extractlabel(from: ArgValue; {full file name}
                         var result: filelabel {resulting file label} );

  { Extract filename and extension for label.
  }

    var
      i: integer; {induction variable}
      Arg: ArgValue; {argument returned by FixFileArg}

    begin
        FixFileArg(from, DisplayFile, iExtVar, Arg);
        result := blanklabel;
        with Arg do
          begin
          i := 0;
          while (i < len) and (i < labelsize) do
            begin
            i := i + 1;
            result[i] := txt[i];
            end;
          end;
    end; {extractlabel}



  function openinput(newfile: ArgValue; {file name to attach}
                      var s: sourcedescriptor {source level} ): boolean;

  { Open an input file at the current source level and return "true"
    if found.
  }

    var
      check: integer; {check file existence}

    begin
      with s do
        begin
        reset(inputfile, newfile.txt,, check);
        if check < 0 then openinput := false
        else
          begin
          openinput := true;
          lineno := 0;
          chno := 0;
          total := 0;
          extractlabel(newfile, currentlabel);
          end;
        end;
    end; {openinput}



  procedure printlabel;

    var
      i: 1..labelsize; {induction var}


    begin
      with source[sourcelevel] do
        if currentlabel <> lastlabel then
          begin
          lastlabel := currentlabel;
          writeln(outputfile);
          for i := 1 to labelsize do
            if lastlabel[i] <> ' ' then write(outputfile, lastlabel[i]);
          writeln(outputfile, ':');
          writeln(outputfile);
          end;
    end; {printlabel}


  procedure printline;

    var
      i: linesize;


    begin
      with source[sourcelevel] do
        begin
        printlabel;
        write(outputfile, lineno: 5, '  ');
        i := 1;
        {is this the first time in a run or not?}
        if adjustment = first then
          begin
          {ignore any leading spaces there happen to be. }
          while (i < total) and (line[i] = ' ') do i := succ(i);
          {compute the adjustment needed for other lines. }
          movement := (level * indentation) - (i - 1);
          adjustment := other;
          {insert any necessary indentation.}
          if level > 0 then write(outputfile, ' ': (level * indentation));
          end
        else
          begin
          {it wasn't the first, so try to adjust to align with its mother}
          if movement > 0 then write(outputfile, ' ': movement)
          else if movement < 0 then
            while (i < total) and (line[i] = ' ') and (i <= - movement) do
              i := succ(i);
          end;
        {write out the line. }
        while i < total do
          begin
          write(outputfile, line[i]);
          i := succ(i);
          end;
        writeln(outputfile);
        end; {with}
    end {printline} ;


  procedure error(e: positive);
   {this procedure is the error message repository.}


    begin
      errorflag := true;
      write(outputfile, 'FATAL ERROR - ');
      case e of
        1: write(outputfile, '%INCLUDE''s nested to deeply');
        2: write(outputfile, 'No identifier after prog/proc/func');
        3: write(outputfile, 'Token after heading unexpected');
        4: write(outputfile, 'Lost "." check begin/case/end.');
        5: write(outputfile, 'Same name, but not forward-declared');
        end;
      {we shall print the offending line too.}
      writeln(outputfile, '  - at following line ');
      adjustment := first;
      printline;
      goto 99; {escape}
    end {error} ;



  procedure nextch;


    begin
      with source[sourcelevel] do
        if (chno = total) and eof(inputfile) then
          begin
          close(inputfile);
          if sourcelevel > 1 then sourcelevel := sourcelevel - 1
          else
            if not getnextinput(currentfile) then goto 99 {all done}
            else
              if not openinput(currentfile, source[1]) then openerror(false);
          end;
      with source[sourcelevel] do
        if chno = total then
          begin
          if printflag then printline;
          total := 0;
          while not eoln(inputfile) do
            begin
            total := succ(total);
            read(inputfile, line[total]);
            if line[total] = chr(ff) then total := pred(total)
            else if line[total] = chr(ht) then
              begin
              line[total] := ' ';
              while total mod tabinterval <> 0 do
                begin
                total := succ(total);
                line[total] := ' ';
                end;
              end;
            end;
          total := succ(total);
          line[total] := ' ';
          readln(inputfile);
          lineno := lineno + 1;
          chno := 1;
          ch := line[1];
          end
        else
          begin
          chno := succ(chno);
          ch := line[chno];
          end;
    end {nextch} ;



  procedure push(newscope: ptrtoentry);

    var
      newlevel: ptrtostackcell;


    begin
      new(newlevel);
      newlevel^.current := newscope;
      newlevel^.scopetree := nil;
      newlevel^.substack := stack;
      stack := newlevel;
      level := level + 1;
    end {push} ;


  procedure pop;

    var
      oldcell: ptrtostackcell;


    begin
      stack^.current^.localtree := stack^.scopetree;
      oldcell := stack;
      stack := oldcell^.substack;
      dispose(oldcell);
      level := level - 1;
    end {pop} ;


  procedure findnode(var match: boolean;
                     var follow: ptrtoentry;
                     thisnode: ptrtoentry);


    begin
      match := false;
      while (thisnode <> nil) and not match do
        begin
        follow := thisnode;
        if savesymbol < thisnode^.procname then thisnode := thisnode^.left
        else if savesymbol > thisnode^.procname then
          thisnode := thisnode^.right
        else match := true;
        end
    end {findnode} ;


  function makeentry(mainprog: boolean;
                     proc: boolean): ptrtoentry;
    { The first parameter is true if the name in symbol is the
     program indetifier, which has no scope. The second parameter
     is true if the name in symbol is that of a procedure of function.
     The result returned is the identification of the relevant record.}

    var
      newentry, node: ptrtoentry;
      located: boolean;


    procedure puttosupertree(newnode: ptrtoentry);
      {this procedure takes the entry that has been created by
       MakeEntry and inserted into the local tree, and also links
       it into the supertree.}

      var
        place: ptrtoentry;


      procedure findleaf;
        {findleaf searches the supertree to find where this
         node should be placed. It will be appended to a leaf
         of course, and placed after entries with the same
         name.}

        var
          subroot: ptrtoentry;


        begin
          subroot := superroot;
          while subroot <> nil do
            begin
            place := subroot;
            if savesymbol < subroot^.procname then subroot := subroot^.before
            else subroot := subroot^.after;
            end
        end {findleaf} ;


      begin {puttosupertree}
        if superroot = nil then
          begin
          {nothing in the supertree yet.}
          superroot := newnode
          end
        else
          begin
          {seek the right place}
          findleaf;
          with place^ do
            if savesymbol < procname then before := newnode
            else after := newnode
          end
      end {PutToSuperTree} ;


    begin {MakeEntry}
      located := false;
      savesymbol := symbol;
      if mainprog then new(newentry)
      else if stack^.scopetree = nil then
        begin
        {Nothing here yet.}
        new(newentry);
        stack^.scopetree := newentry
        end
      else
        begin
        { seek the identifier in the tree.}
        findnode(located, node, stack^.scopetree);
        if not located then
          begin
          {normal case, make an entry.}
          new(newentry);
          with node^ do
            if symbol < procname then left := newentry
            else right := newentry
          end
        end;
      if not located then
        begin
        {Here we initialize all the fields}
        with newentry^, source[sourcelevel] do
          begin
          procname := symbol;
          procuppers := symbolcase;
          linenumber.line := lineno;
          linenumber.fileref := currentlabel;
          startofbody.line := 0;
          startofbody.fileref := blanklabel;
          if proc then status := shortform
          else status := notproc;
          left := nil;
          right := nil;
          before := nil;
          after := nil;
          calls := nil;
          called := nil;
          localtree := nil;
          end;
        makeentry := newentry;
        if proc then
          begin
          puttosupertree(newentry);
          push(newentry);
          end
        end
      else
        begin
        {well, it'd better be forward or else.}
        makeentry := node;
        push(node);
        if (node^.status = fwdhalf) or (node^.status = outside) then
          with source[sourcelevel] do
            begin
            stack^.scopetree := node^.localtree;
            if node^.status = fwdhalf then node^.status := allfwd
            else node^.status := outsidedef;
            node^.forwardblock.line := lineno;
            node^.forwardblock.fileref := currentlabel;
            end
        else error(5)
        end
    end {makeentry} ;



  procedure printtree(root: ptrtoentry);

    var
      thiscell: listofusages;
      count: natural;


    procedure namewrite(p: ptrtoentry);

      var
        s: sigcharrange;


      begin
        for s := 1 to sigcharlimit do
          if p^.procuppers[s] then
            write(outputfile, chr(ord(p^.procname[s]) - uclcdisplacement))
          else write(outputfile, p^.procname[s])
      end {namewrite} ;


    procedure writelineref(thisline: lineref);

      var
        c: 1..labelsize; {induction var}


      begin {Print a line reference in the form "filelabel: line".}
        with thisline do
          begin
          for c := 1 to labelsize do
            if fileref[c] <> ' ' then write(outputfile, fileref[c]);
          write(outputfile, ', ', line: 1);
          end;
      end; {writelineref}


    procedure listrefs(firstline: prefixname; {header for first line}
                       thiscell: listofusages {usages to list} );


      begin {list a set of references}
        writeln(outputfile);
        write(outputfile, firstline: sigcharlimit - 1, '  ');
        count := 0;
        while thiscell <> nil do
          begin
          if ((count mod namesperline) = 0) and (count <> 0) then
            begin
            writeln(outputfile);
            write(outputfile, ' ': sigcharlimit + 1);
            end;
          write(outputfile, ' ');
          namewrite(thiscell^.what);
          thiscell := thiscell^.next;
          count := count + 1;
          end;
        writeln(outputfile);
      end; {listrefs}


    begin {printtree}
      if root <> nil then
        with root^ do
          begin
          printtree(before);
          if (root <> superroot) or (calls <> nil) then
            begin
            writeln(outputfile);
            writeln(outputfile);
            namewrite(root);
            write(outputfile, '  Head: ');
            writelineref(linenumber);
            if startofbody.line <> 0 then
              begin
              write(outputfile, '  Body: ');
              writelineref(startofbody);
              end;
            case status of
              fwdhalf, notproc: write(outputfile, '  Incomplete');
              formal: write(outputfile, '  formal');
              outside: write(outputfile, '  external');
              shortform: ;
              outsidedef:
                begin
                writeln(outputfile);
                write(outputfile, ' ': sigcharlimit + 2,
                      'External Def, header stub: ');
                writelineref(forwardblock);
                end;
              allfwd:
                begin
                writeln(outputfile);
                write(outputfile, ' ': sigcharlimit + 2,
                      'Forward, header stub: ');
                writelineref(forwardblock);
                end;
              end;
            writeln(outputfile);
            if calls <> nil then listrefs('Calls    ', calls);
            if called <> nil then listrefs('Called by', called);
            end;
          printtree(after);
          end;
    end {printtree} ;


  procedure nexttoken;
    {this procedure produces the next "token" in a small set of
     recognized tokens. Most of these serve an incidental purpose;
     the prime purpose is to recognize names (res'd words or identifiers).
     It serves also to skip dangerous characters in comments, strings,
     and numbers.}


    procedure ignorecomment;
      {This procedure ships over comments according to the definition
       in the Draft Pascal Standard.}


      begin
        nextch;
        repeat
          while (ch <> '*') and (ch <> '}') do nextch;
          if ch = '*' then nextch;
        until (ch = ')') or (ch = '}');
        nextch;
      end {Ignorecomment} ;


    procedure ignorenumbers;
      {This procedure skips numbers because the exponent part
       just might get recognized as a name! Care must be taken
       not to consume half of a ".." occurring in a construct like
       "1..name", or worse to consume it and treat the name as a
       possible exponent as in "1..E02". Ugh.}


      begin
        while ch in digits do nextch;
       { The construction of NextCh, chno & line ensure that
        the following tests are always defined. It is to get
        rid of tokens which begin with a period like .. & .) }
        if (ch = '.') then
          with source[sourcelevel] do
            if (line[chno + 1] in digits) then
              begin
              nextch;
              while ch in digits do nextch
              end;
        if (ch = 'E') or (ch = 'e') then
          begin
          nextch;
          if (ch = '+') or (ch = '-') then nextch;
          while ch in digits do nextch
          end
        else if (ch = 'B') or (ch = 'b') then nextch;
      end {ignorenumbers} ;


    procedure readident;
     {This procedure reads in an identifier }

      var
        j: positive;


      begin
        token := namesy;
        symbol := spaces;
        symbolcase := lowers;
        j := 1;
        while (j <= sigcharlimit) and (ch in alphanums) do
          begin
          if ch in uppercase then
            begin
            symbol[j] := chr(ord(ch) + uclcdisplacement);
            symbolcase[j] := true;
            end
          else symbol[j] := ch;
          j := j + 1;
          nextch;
          end;
        {In case there is a tail, skip it.}
        while (ch in alphanums) do nextch;
      end {readident} ;


    procedure lexicaldirective;

      var
        j: natural; {induction variable}
        testset: setofchar; {filename delimiters}

      procedure openincludefile;

     { Open an included source file, incrementing the nesting level. The
       current directory is searched first, followed by any stored by CSI.
       If the file is not present in any of these, a fatal error occurs.
     }

      var
        prefix: ArgValue; {directory prefix for the file}
        NextElement: ListPtr; {search element}
        found: boolean; {set if file found}

        function FileExists: boolean;

        { Check if a target directory contains the file. This routine
          assumes that "prefix" contains a directory string, and calls
          "FixFileInclude" to create a complete name. The function
          returns "true" if the file exists in the directory.
        }

        var
          arg: ArgValue; {complete file name}

        begin
          FixFileInclude(currentfile, prefix, iExtVar, arg);
          FileExists := openinput(arg, source[sourcelevel]);
        end;			{FileExists}


      begin                         {openincludefile}
        sourcelevel := sourcelevel + 1;
        prefix.len := 0;            {no prefix for current directory}
        if not FileExists then begin {check other directories}
          found := false;
          NextElement := IncludeListHead;
          while (NextElement <> NIL) and not found do
            begin
            prefix := NextElement^.arg;
            NextElement := NextElement^.next;
            found := FileExists;
            end;
          if not found then openerror(true);
          end;
      end;                          { openincludefile }


      begin
        nextch;
        if ch in alphabet then
          begin
          readident;
          if symbol = sinclude then
            begin
            while ch = ' ' do nextch;
            testset := [' ',';'];
            with currentfile do
              begin
              len := 0;
              if ch = '''' then 
               begin
                testset := testset + [ch];               
                nextch;
               end;
              while not (ch in testset) do
                begin
                len := len + 1;
                txt[len] := ch;
                nextch;
                end;
              end;
            openincludefile;
            nextch;
            end;
          end;
        token := othersy;
      end; {lexicaldirective}


    begin {nexttoken}
      token := othersy;
      repeat
        case ch of

          ')':
            begin
            nextch;
            token := rparensy;
            end;

          '(':
            begin
            nextch;
            if ch = '*' then ignorecomment
            else token := lparensy
            end;

          '{': ignorecomment;

          '''':
            begin
            nextch;
            while ch <> '''' do nextch;
            nextch;
            end;

          '0', '1', '2', '3', '4', '5', '6', '7', '8', '9': ignorenumbers;

          ':':
            begin
            nextch;
            if ch = '=' then
              begin
              token := assignsy;
              nextch;
              end
            else token := colonsy;
            end;

          '.':
            begin
            nextch;
            if ch <> '.' then token := periodsy
            else
              begin
              token := subrangesy;
              nextch;
              end;
            end;

          ';':
            begin
            nextch;
            token := semicolsy;
            end;

          'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
          'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
          'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
          'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
          '$':
            readident;
          '%': lexicaldirective;
          otherwise {uninteresting character } nextch;
          end;
      until token <> othersy;
    end {nexttoken} ;



  procedure processunit(programid: boolean);
    {this procedure processes a program unit. it is called on
     recognition of its leading token = program/procedure/function.
     The parameter records whether we currently have the main program
     identifier in the token, or not. It doesn;t have scope.}

    var
      at: ptrtoentry;


    function nameisinscope: boolean;
      {This function is called during the declaration phase
       of a block, and has to find any procedure which gets
       renamed by the scope rules.}

      var
        llevel: ptrtostackcell;
        discovered: boolean;
        where: ptrtoentry;


      begin
        llevel := stack;
        discovered := false;
        savesymbol := symbol;
        while (llevel <> nil) and not discovered do
          begin
          findnode(discovered, where, llevel^.scopetree);
          if not discovered then llevel := llevel^.substack
          end;
        if discovered then nameisinscope := (where^.status <> notproc)
        else nameisinscope := false;
      end {nameisinscope} ;



    procedure processblock;
      {This procedure is called by ProcessUnit when it has recognized
       the start of a block. It handles the processing of the block.}

      var
        address: ptrtoentry;


      procedure crossreference;
        {Crossreference is called whenever we have a name which
         might be a call to a procedure or function. The only way
         we tell is by looking in the table to see. If it is, then
         the list of usages if the procedure we are in is scanned and
         possibly extended.}

        var
          home: ptrtoentry;
          slevel: ptrtostackcell;
          found: boolean;


        procedure makeref(var log: listofusages; {list to add ref to}
                          usage: ptrtoentry {entry being used} );

          var
            found: boolean; {usage already noted}
            ptr: listofusages; {place to put next usage note}
            nextptr: listofusages; {scans list of usages}
            newcell: listofusages; {new usage entry being created}


          begin
            found := false;
            nextptr := log;
            if nextptr <> nil then
              repeat
                ptr := nextptr;
                found := (ptr^.what = usage);
                nextptr := ptr^.next;
              until found or (nextptr = nil)
            else ptr := nil;
            if not found then
              begin
              new(newcell);
              if ptr <> nil then ptr^.next := newcell
              else log := newcell;
              newcell^.what := usage;
              newcell^.next := nil;
              end;
          end; {makeref}


        begin {crossreference}
          slevel := stack;
          found := false;
          while (slevel <> nil) and not found do
            begin
            findnode(found, home, slevel^.scopetree);
            if not found then slevel := slevel^.substack;
            end;
          if found then
            begin
            if home^.status <> notproc then
              begin
              makeref(home^.called, stack^.current);
              makeref(stack^.current^.calls, home);
              end;
            end;
        end {crossreference} ;


      procedure scanforname;
        {This procedure is required to go forward until the
         current token is a name (reserved word or identifier).}


        begin
          nexttoken;
          while token <> namesy do nexttoken;
        end {scanforname} ;


      begin {processBlock}
        depth := 0;
        while (symbol <> sbegin) do
          begin
          while (symbol <> sbegin) and (symbol <> sprocedure) and
                (symbol <> sfunction) do
            begin
            scanforname;
            if symbol = srecord then depth := depth + 1
            else if symbol = send then depth := depth - 1
            else if (depth = 0) and nameisinscope then
              begin
              address := makeentry(false, false);
              {makeentry made its status notproc}
              end;
            end;
          if symbol <> sbegin then
            begin
            processunit(false);
            scanforname;
            end;
          end;
        {We have now arrived at the body}
        depth := 1;
        with stack^.current^, source[sourcelevel] do
          begin
          startofbody.line := lineno;
          startofbody.fileref := currentlabel;
          end;
        nexttoken;
        while depth <> 0 do
          begin
          if token = periodsy then
            begin
            nexttoken;
            if token = namesy then nexttoken;
            end;
          if token <> namesy then nexttoken
          else
            begin
            if (symbol = sbegin) or (symbol = scase) then
              begin
              depth := depth + 1;
              nexttoken;
              end
            else if (symbol = send) then
              begin
              depth := depth - 1;
              nexttoken;
              end
            else
              begin
                   {This name is a candidate call. But first we
                    must eliminate assignments to function values.}
              savesymbol := symbol;
              nexttoken;
              if token <> assignsy then
                begin
                crossreference
                end
              else
                begin
                nexttoken;
                end;
              end
            end;
          end;
      end {processblock} ;



    procedure scanparameters;
      { This procedure scans the parameter list because at the outer
       level there may be a formal procedure we ought to know about.}

      var
        which: ptrtoentry;


      procedure scantillclose;
        {This procedure is called when a left paranthese is
         detected, and its task is to find the matching right
         parenthese. It does this recursively.}


        begin
          nexttoken;
          while token <> rparensy do
            begin
            if token = lparensy then scantillclose;
            nexttoken;
            end;
        end {scantillclose} ;


      begin {scanParameters}
        nexttoken;
        while token <> rparensy do
          begin
          if (token = namesy) then
            begin
            if (symbol = sprocedure) or (symbol = sfunction) then
              begin
              { A formal procedural/functional parameter.}
              nexttoken;
              if token = namesy then
                begin
                which := makeentry(false, true);
                which^.status := formal;
                pop;
                nexttoken;
                if token = lparensy then
                  begin
                  {skip interior lists.}
                  scantillclose;
                  end;
                end
              else
                begin
                error(2);
                nexttoken;
                end;
              end
            else
              begin
              if nameisinscope then which := makeentry(false, false);
              nexttoken;
              end;
            end
          else nexttoken;
          end;
        nexttoken;
      end {scanparameters} ;



    begin {processUnit}
      if programid and (symbol <> sprogram) then
        begin
        at := makeentry(true, true);
        at^.procname := fakeprog;
        at^.procuppers := lowers;
        processblock;
        pop;
        end
      else
        begin
        printflag := true;
        adjustment := first;
        nexttoken;
        if token <> namesy then error(2)
        else
          begin
          {We now have the name to store away.}
          at := makeentry(programid, true);
          while not (token in [lparensy, semicolsy, colonsy]) do nexttoken;
          if token = lparensy then scanparameters;
          while token <> semicolsy do nexttoken;
          printline;
          { We have now printed the procedure heading.}
          printflag := false;
          writeln(outputfile);
          {Our next task is to see if there is an attached block.}
          nexttoken;
          if token <> namesy then error(3)
          else
            begin
            if (symbol <> slabel) and (symbol <> sconst) and
               (symbol <> stype) and (symbol <> sprocedure) and
               (symbol <> sfunction) and (symbol <> svar) and
               (symbol <> sbegin) then
              begin
              {bloody directive, mate.}
              if symbol = sforward then at^.status := fwdhalf
              else at^.status := outside;
              pop;
              end
            else
              begin
              processblock;
              pop;
              end;
            end;
          end;
        end;
    end {processunit} ;


  procedure printheading;


    begin
      writeln(outputfile, 'Procedural Cross-Referencer - Version 2.1-4');
{!!!      writeln(outputfile, Cmd.txt: Cmd.len); }
      writeln(outputfile);
    end {printheading} ;



  begin {referencer}
    superroot := nil;

    {Here we construct an outer-scope stack entry. This is needed
     to hold any pre-defined names. The Distributed version does not
     include any of these, but they are easily provided. See the
     outlines in the code marked with *** if you want this feature.}

    new(stack);
    with stack^ do
      begin
      current := nil;
      scopetree := nil;
      substack := nil;
      end;

    printflag := false;

    uppercase := ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
                 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
                 'Y', 'Z'];

    lowercase := ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
                 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
                 'y', 'z'];

    alphabet := uppercase + lowercase;

    digits := ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

    alphanums := alphabet + digits + ['$', '_'];

    usefulchars := alphabet + digits + ['(', ')', '{', '.', ':', ';', ''''];

    for i := 1 to sigcharlimit do lowers[i] := false;

    outwidth := linewidth;

    level := - 1;

    lastlabel := blanklabel;

    csi;
    sourcelevel := 1;

    errorflag := not getnextinput(currentfile); {init to false}
    if not openinput(currentfile, source[sourcelevel]) then openerror(false);

    namesperline := (outwidth - (sigcharlimit + 2)) div (sigcharlimit + 1);

    printheading;
    writeln(outputfile, ' Line Program/procedure/function heading');
    for pretty := 1 to 43 do write(outputfile, '-');
    writeln(outputfile);

    {now we need to get the first token, which should be program.}
    nexttoken;
    if token <> namesy then error(1)
    else processunit(true);

    {Complete phase one - now for the next.}
  99:
    if not errorflag then
      begin
      page(outputfile);
      printheading;
      writeln(outputfile, 'Cross Reference Listing');
      printtree(superroot);
      writeln(outputfile);
      end;
  end.
