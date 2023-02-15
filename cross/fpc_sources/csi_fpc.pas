{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987, 1988, 1989 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Command String Interpreter

 Last modified by KRIS on 21-Nov-1990 15:16:07
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

unit csi_fpc;

interface

uses config, product, utils, hdr, sysutils;

procedure csi;

{ Command String Interpreter, parses the command string and sets
  the initial state of the switches.
}

implementation

  const
    qualifierlength = 12; {max length of switches ('structstatic')}
    maxswitchval = 127; {largest switch value}

  type
    quals = (blanks, bstepq, byteallocq, caseq, codeconstq, commonvarsq,
             cpu8086q, cpu80286q, checkq, debugq, defineq, detailsq,
             doubleq, editlistq, eisq, environq, errorsq, fisq, floatsafeq,
             fppq, framepointerq, genmaskq, groupownq, includelistq,
             largemodelq, level0q, librequestq, listq, longlibq, macroq,
             mainq, cpu68000q, cpu68020q, fpc68881q, objectq, oldpackingq,
             oldreswordsq, ownq, pascal1q, pdp11q, picq, profileq,
             sharecodeq, shortintsq, simq, standardq, statisticsq, stmtnumq,
             symbolq, tblockq, testq, timesq, truncateq, tswitch0q,
             tswitch1q, tswitch2q, tswitch3q, unixtargetq, usebsd42libq,
             usesysVlibq, versionq, walkbackq, windowsq, workspq, cplusplusq,
             ansiq, enumintsq, expandfloatq, indexcheckq, compatq,
             listincq, listexpq, modstringsq, nilcheckq, outputprepq, ppdefq,
             ppundefq, rangecheckq, romconstsq, signedcharq, structstaticq,
             vectextrefq, vectextdefq, structstandq, notfound);


    errorname = (ambig, ambig_option, unknown, nono, qualtwice, twofilenames,
                 badfile, nofile, noinput, manyouts, badparam, numrange,
                 contradiction, badsyntax, missingparen, outconflict,
                 cmdtoolong, pic_own_section);

    qualtrans = array [quals] of switch; {translate quals to switches}
    qualset = set of quals;

  var
    param: string; {DRB current param, like commandline in the old csi}
    paramlength: integer;
    next: integer;
    qualtable: array [quals] of string; {look-up table}
    numquals: qualset; {quals with numeric parameters}
    cmdquals: qualset; {quals set on command line}
    qualsset: qualset; {quals turned on at start of compile}

    filefound: boolean; {file found in this field}
    filestart, filefinish: integer; {start/end of last file name found}
    fieldsfound: 0..cmdlinelength; {number of fields found}
    firstfound: boolean; {first file field found}
    secondfound: boolean; {second file field found}
    manyfound: boolean; {more than 2 file fields found}
    outspeced: boolean; {the <output>=<input> form was used}
    emptyfileflag: boolean; {empty file name found, possibly an error}
    emptystart: integer; {start of empty file field}
    emptyend: integer; {end of empty file field}
    lastfield: integer; {end of last field scanned}
    temp_unixtarget: unixflavors; {Temporary holder for the global unixtarget.
                                   On small compilers unixtarget may be a
                                   constant.}
    fppspecified: boolean; {if ever specified}

  procedure printversion;

{ Print a version message }

  
    procedure printwarning;
  
      var
        headerline: hdrline; {Site header line}
        headerlength: integer; {length of site header line}
        site1, site2: integer; {site number site1-site2}
  
      begin {printwarning}
        writeln;
{DRB        z_b129(headerline, headerlength, site1, site2);}
        writeln(headerline: headerlength);
        writeln;
      end {printwarning} ;
  

    begin {printversion}
      if versionq in qualsset then
        begin
        write('Pascal-2 (tm) ');
        if hostopsys <> targetopsys then
          begin
          case hostopsys of
            vms: write('VAX/VMS');
            unix: write('UNIX');
            end;
          write(' to ');
          end
        else
          write('for ');
        write(systemtitle);
        write('.  Version ', version);
        writeln;
        write('Copyright (C) 1986 - 1989 by Oregon Software, Inc.  ');
        writeln('ALL RIGHTS RESERVED.');
        end;
    end {printversion} ;


  procedure error(which: errorname; {which error}
                  startind: integer; {start of command to print}
                  endind: integer {end of command to print} );

{ Print an error message and exit from the compiler with a fatal
  error status.  The offending portion of the command line is printed
  before the error message.
}

    var
      i: integer; {induction var}


    begin {error}
      printversion;
      for i := startind to endind do
        case hostopsys of
          vdos:
            if param[i] in [',', '/', '='] then write(' ')
            else write(param[i]);
          otherwise write(param[i]);
          end;
      if startind <= endind then writeln;
      case which of
        ambig: writeln('Ambiguous switch.');
        ambig_option: writeln('Ambiguous switch option.');
        unknown: writeln('Unknown switch.');
        nono: writeln('"NO" not allowed on this switch.');
        qualtwice: writeln('Same switch used twice.');
        twofilenames: writeln('Two file names in one field.');
        badfile: writeln('Bad file name syntax.');
        nofile: writeln('No file in field.');
        noinput: writeln('No input file provided.');
        manyouts: writeln('More than two output file specifications.');
        badparam: writeln('Required parameter missing.');
        numrange: writeln('Value for qualifier out of range.');
        contradiction: writeln('Conflicting switches specified.');
        badsyntax: writeln('Bad command line syntax.');
        missingparen: writeln('Missing ")".');
        outconflict: writeln('Output file(s) specified both ways.');
        cmdtoolong: writeln('Command line too long.');
        pic_own_section: writeln('Own section must be 15 with PIC');
        end;
      halt;
    end; {error}


  procedure initquals;

{ Initialize the qualifier table.
}

    var
      i: quals; {induction in qualtable table}
      j: unixflavors; {induction in host_table}


    begin {initquals}
      qualtable[checkq] := 'CHECK';
      qualtable[caseq] := 'CASE';
      qualtable[debugq] := 'DEBUG';
      qualtable[detailsq] := 'DETAILS';
      qualtable[doubleq] := 'DOUBLE';
      qualtable[errorsq] := 'ERRORS';
      qualtable[genmaskq] := 'GENMASK';
      qualtable[includelistq] := 'INCLUDE';
      qualtable[level0q] := 'LEVEL0';
      qualtable[listq] := 'LIST';
      qualtable[macroq] := 'MACRO';
      qualtable[mainq] := 'MAIN';
      qualtable[objectq] := 'OBJECT';
      qualtable[ownq] := 'OWN';
      qualtable[profileq] := 'PROFILE';
      qualtable[shortintsq] := 'SHORTINTS';
      qualtable[standardq] := 'STANDARD';
      qualtable[stmtnumq] := 'STMTNUM';
      qualtable[symbolq] := 'SYMBOLS';
      qualtable[tblockq] := 'TBLOCK';
      qualtable[testq] := 'TEST';
      qualtable[timesq] := 'TIMES';
      qualtable[tswitch0q] := 'TSWITCH0';
      qualtable[tswitch1q] := 'TSWITCH1';
      qualtable[tswitch2q] := 'TSWITCH2';
      qualtable[tswitch3q] := 'TSWITCH3';
      qualtable[versionq] := 'VERSION';
      qualtable[walkbackq] := 'WALKBACK';
      qualtable[defineq] := 'DEFINE';
      qualtable[environq] := 'ENVIRONMENT';
      qualtable[framepointerq] := 'FRAMEPOINTER';
      qualtable[oldreswordsq] := 'OLDRESWORDS';

      case targetmachine of
        mc68000:
          begin
          if targetopsys = apollo then
            begin
            qualtable[sharecodeq] := 'SHARECODE';
            qualtable[usebsd42libq] := 'BSD42LIB';
            qualtable[usesysVlibq] := 'SYSVLIB';
            end
          else
            begin
            qualtable[cpu68000q] := '68000';
            qualtable[cpu68020q] := '68020';
            qualtable[fpc68881q] := '68881';
            qualtable[picq] := 'PIC';
            end;
          if targetopsys = vdos then
            qualtable[longlibq] := 'LONGLIB'
          else if targetopsys = unix then
            begin
            qualtable[longlibq] := 'LONGLIB';
            qualtable[unixtargetq] := 'UNIXTARGET';
            end;
          end;
        end {case} ;
    end {initquals} ;

  function uc(c: char): char;

{ Return the upper case equivalent of a character.
}


    begin {uc}
      if (c >= 'a') and (c <= 'z') then
        uc := chr(ord(c) + (ord('A') - ord('a')))
      else uc := c;
    end {uc} ;


  function twoof(mask: qualset {qualifiers to check} ): boolean;

{ True if there are two or more entries in qualsset matching the mask.
}

    var
      q: quals; {induction variable}
      count: integer; {qualifiers found in the set}
      mquals: qualset; {masked qualifier set}


    begin {twoof}
      mquals := qualsset * mask;
      count := 0;
      for q := blanks to notfound do if q in mquals then count := count + 1;
      twoof := count > 1;
    end {twoof} ;


  procedure addtofilelist(var list: filenamelistptr; {list to add to}
                          start, finish: integer; {from where}
                          isinclude: boolean {if includelist} );

{ Append a filename (from the command line) to the given list.
}

    var
      p: filenamelistptr; {induction on existing list}
      q: filenamelistptr; {points to new entry}
      i: filenameindex; {induction on filename}
      directorydelim: boolean; {flags a delimiter}
      dotcount: integer; {counts down vdos fields}


    begin {addtofilelist}
      directorydelim := false;
      new(q);
      with q^ do
        begin
        next := nil;
        arglen := min(finish - start + 1, filenamelen);
        for i := 1 to filenamelen do
          begin
          if start <= finish then
            begin
            arg[i] := param[start];
            start := start + 1;
            end
          else arg[i] := ' ';
          end;
        end {with} ;
      if list <> nil then
        begin
        p := list;
        while p^.next <> nil do p := p^.next;
        p^.next := q;
        end
      else list := q;
    end {addtofilelist} ;


  procedure takefilename(parenthesized: boolean; {it's in a parenthesized
                                                  list}
                         var list: filenamelistptr; {where to store it}
                         var next: integer; {index in command line}
                         isinclude: boolean {if this file in includes});

{ Parse a filename, and add it to the list specified by "list".
  This routine makes sure that there is only one file specified per field.
}

    var
      startindex: integer; {start of this field}
      terminators: set of char; {what stops a file name}
      quotedstring: boolean; {flag for quote removal}


    procedure skipbalancedstring;

{ Skip a string balanced with respect to parentheses and quoted strings.
}

      var
        endchar: char; {bracket which terminates string}


      begin {skipbalancedstring}
        if param[next] in ['(', '[', '<'] then
          begin
          if param[next] = '(' then endchar := ')'
          else if param[next] = '<' then endchar := '>'
          else endchar := ']';
          next := next + 1;
          while (param[next] <> endchar) and (next <> paramlength) do
            skipbalancedstring;
          end
        else if param[next] = '"' then
          begin
          quotedstring := true;
          repeat
            next := next + 1
          until (param[next] = '"') or (next = paramlength);
          end;
        if next < paramlength then next := next + 1
        else error(badfile, startindex, paramlength);
      end {skipbalancedstring} ;


    begin {takefilename}
      quotedstring := false;
      startindex := next;
      terminators := ['='];
      if parenthesized then terminators := terminators + [')'];
      while (next < paramlength) and not (param[next] in terminators) do
      skipbalancedstring;
      filefound := true;
      filestart := startindex;
      filefinish := next;
      if quotedstring then
        begin
        filestart := filestart + 1;
        if (param[next] = '"') or (filefinish <= paramlength) then
          filefinish := filefinish - 1
        end;
      addtofilelist(list, filestart, filefinish, isinclude);
    end {takefilename} ;


  procedure takequal(qual: string);

{ Parse and look up a qualifier, updating "next" to point to the next
  character in the command line.
}

    var
      s: integer; {temporary}
      startingindex: integer; {start of qualifier, for error printout}
      quali: 0..qualifierlength; {current character in qual being built}
      name: string; {qualifier name}
      nofound: boolean; {true if "no" preceeds qualifier}
      thisqual: quals; {qualifier just found}
      this_target: unixflavors; {target just found}
      ambiguous: boolean; {qualifier lookup was ambiguous}

    procedure findqual(target: string; {candidate Qual name}
                       var result: quals; {result of lookup}
                       var ambiguous: boolean {more than one match} );

{ Look up "target" in the Qualindex and set "result" to the appropriate
  qualifier.  If there is a full match, this is always taken.  Otherwise,
  a single partial match will be accepted.  Multiple partial matches cause
  "ambiguous" to be set.
}

      var
        partialmatch: boolean; {partially matches the Qual so far}
        partialresult: quals; {where the match was}
        partials: 0..maxint; {counter of partial matches}
        effectivelength: 0..qualifierlength; {significant chars in target}
        i: 1..qualifierlength; {induction var}


      begin {findqual}
        next := 1;
        partials := 0;
        effectivelength := length(target);
        result := blanks;
        qualtable[notfound] := target; {to terminate search}

        while target <> qualtable[result] do
          begin
          result := succ(result);
          partialmatch := target <> qualtable[result];
          for i := 1 to effectivelength do
            partialmatch := partialmatch and
                            (target[i] = qualtable[result, i]);
          if partialmatch then
            begin
            partialresult := result;
            partials := partials + 1;
            end;
          end;
        if (result = notfound) and (partials = 1) then
          result := partialresult;
        ambiguous := partials > 1;
      end {findqual} ;


    procedure getnumqual(var result: integer; {resulting value}
                         low_lim, hi_lim: integer {limits on value} );

{ Scan off a number, for qualifiers that take numeric arguments.
}

      var
        tempres: integer;
        accumulating, negate: boolean;


      begin {getnumqual}
        if qual[next] in [':', '='] then next := next + 1
        else error(badparam, startingindex, next - 1);
        accumulating := true;
        tempres := 0;
        negate := qual[next] = '-';
        if negate then next := next + 1;
        while qual[next] in ['0'..'9'] do
          begin
          if accumulating then
            if tempres <= maxint div 10 then tempres := tempres * 10
            else accumulating := false;
          if accumulating then
            if tempres <= maxint - (ord(qual[next]) - ord('0')) then
              tempres := tempres + (ord(qual[next]) - ord('0'))
            else accumulating := false;
          next := next + 1;
          end;
        if negate then tempres := - tempres;
        if accumulating and (tempres <= hi_lim) and (tempres >= low_lim) then
          result := tempres
        else error(numrange, startingindex, next - 1);
      end {getnumqual} ;


    procedure getspecialfilename(q: quals; {which qualifier}
                                 var next: integer);

{ Handle the "/<qual>=<filename>" format for the macro, object, list,
  errors, include and environment qualifiers, and the 
  "/include=(<filenamelist>)" format.
}

      var
        start: integer; {start of a filename}
        savefilefound: boolean; {holds filefound during takefilename}


      procedure pickalist(q: quals; {which qualifier this file goes with}
                          parenthesized: boolean {in a parenthesized list} );

{ Put the filename (just found) in the appropriate list.
}


        begin {pickalist}
          if q = defineq then
            takefilename(parenthesized, defname, next, false)
          else if q = objectq then
            takefilename(parenthesized, objname, next, false)
          else if q = macroq then
            takefilename(parenthesized, macname, next, false)
          else if (q = listq) or (q = errorsq) then
            takefilename(parenthesized, listname, next, false)
          else if q = environq then
            takefilename(parenthesized, envname, next, false)
          else { if q = includelistq then }
            takefilename(parenthesized, includelisthead, next, true);
        end {pickalist} ;


      begin {getspecialfilename}
        savefilefound := filefound;
        next := next + 1;
        if param[next] <> '(' then
          begin
          start := next;
          filefound := false;
          pickalist(q, false);
          end
        else
          begin
          repeat
            next := next + 1;
            start := next;
            filefound := false;
            pickalist(q, true);
            if not (q in [environq, includelistq]) and (param[next] <> ')')
            then error(missingparen, start - 1, next);
          until (next >= paramlength) or (param[next] = ')');
          if next < paramlength then next := next + 1
          else error(missingparen, start - 1, paramlength);
          filefound := savefilefound;
          end;
      end {getspecialfilename} ;


    begin {takequal}
      quali := 0;
      nofound := false;
      param := uppercase(param);
      if (length(param) > 2) and (pos(param, 'NO') > 0) then
	begin
        nofound := true;
	param := rightstr(param, length(param) - 2);
        end;

      findqual(param, thisqual, ambiguous);

      if ambiguous then error(ambig, startingindex, next - 1)
      else if thisqual = notfound then
        error(unknown, startingindex, next - 1);
      case targetmachine of
        pdp11:
          if nofound and (thisqual in [eisq, fisq, fppq, simq, workspq]) then
            error(nono, startingindex, next - 1);
        iapx86:
          begin
          if thisqual = fppq then fppspecified := true;
          if nofound and (thisqual in [cpu8086q, cpu80286q]) then
            error(nono, startingindex, next - 1);
          end;
        mc68000:
          if targetopsys = vdos then
            begin
            if nofound and (thisqual in [cpu68000q, cpu68020q, fpc68881q,
                                         usebsd42libq, usesysVlibq]) then
              error(nono, startingindex, next - 1);
            end;
        end {case} ;
      if thisqual in numquals then
        begin
        if nofound then error(nono, startingindex, next - 1);
        case thisqual of
          workspq: getnumqual(workspace, 0, maxint);
          ownq:
            begin
            getnumqual(s, 0, 15);
            datasection := s;
            end;
          tblockq: getnumqual(tblocknum, 0, maxint);
          genmaskq: getnumqual(genoptmask, -65535, 65535);
          end;
        end;
      if thisqual in cmdquals then error(qualtwice, startingindex, next - 1);
      if not (thisqual in [environq, includelistq])
      then {more than one of these is ok}
        cmdquals := cmdquals + [thisqual];
      if nofound then qualsset := qualsset - [thisqual]
      else qualsset := qualsset + [thisqual];

      { Handle switch-specified file names }

      if (thisqual in [environq, errorsq, includelistq, listq, macroq,
                       objectq, defineq]) and
         (next <= paramlength) and (param[next] in [':', '=']) then
        if nofound then error(nono, startingindex, next)
        else getspecialfilename(thisqual, next);
    end {takequal} ;

  procedure setdefault(s: qualset;
                       typ: quals);
   { Add the default element if none has been specified. }


    begin
      if (s * qualsset) = [] then qualsset := qualsset + [typ];
    end; { SetDefault }


  procedure checkconsistency;

{ Check the command line and qualifiers for consistency.
}

    var
      thisopt: quals; {current hardware options}


    begin {checkconsistency}

       if twoof([listq, errorsq]) or
          twoof([macroq, defineq]) or twoof([objectq, defineq]) or
          twoof([errorsq, debugq, profileq]) then
            error(contradiction, 2, 1);

      if defineq in qualsset then
        qualsset := qualsset - [objectq, macroq, mainq]
      else if ((outspeced and firstfound) or not outspeced) and
         (qualsset * [objectq, macroq] = []) then
        qualsset := qualsset + ([objectq] - cmdquals);
      if (qualsset * [listq, errorsq] = []) and (listname = nil)  then
        fakelist := true;
      if sourcelisthead = nil then error(noinput, 2, 1); 

    end {checkconsistency} ;


  procedure passtocompiler;

{ Pass the command options from qualsset to the compiler.
}

    var
      i: quals; {induction var}
      j: switch; {induction var}

      trans: qualtrans;


    begin
      trans[cplusplusq] := cplusplus;
      trans[ansiq] := noswitch;
      trans[compatq] := compatibility;
      trans[enumintsq] := enumints;
      trans[expandfloatq] := expandfloat;
      trans[indexcheckq] := indexcheck;
      trans[listincq] := listincludes;
      trans[listexpq] := listexpansion;
      trans[modstringsq] := modstrings;
      trans[nilcheckq] := nilcheck;
      trans[outputprepq] := outputprep;
      trans[ppdefq] := noswitch;
      trans[ppundefq] := noswitch;
      trans[romconstsq] := romconsts;
      trans[signedcharq] := signedchars;
      trans[structstaticq] := structstatic;
      trans[vectextrefq] := vectextref;
      trans[vectextdefq] := vectextdef;

      trans[blanks] := noswitch;
      trans[bstepq] := bstep;
      trans[byteallocq] := bytealloc;
      trans[caseq] := caseswitch;
      trans[codeconstq] := codeconst;
      trans[checkq] := rangecheck;
      trans[commonvarsq] := commonvars;
      trans[cpu68000q] := cpu68000;
      trans[cpu68020q] := cpu68020;
      trans[cpu8086q] := cpu8086;
      trans[cpu80286q] := cpu80286;
      trans[debugq] := debugging;
      trans[defineq] := defineswitch;
      trans[detailsq] := details;
      trans[doubleq] := doublereals;
      trans[editlistq] := editlist;
      trans[eisq] := eis;
      trans[environq] := environswitch;
      trans[errorsq] := listerrors;
      trans[fisq] := fis;
      trans[floatsafeq] := floatsafe;
      trans[fpc68881q] := fpc68881;
      trans[fppq] := fpp;
      trans[framepointerq] := framepointer;
      trans[genmaskq] := genmask;
      trans[groupownq] := groupown;
      trans[includelistq] := noswitch;
      trans[largemodelq] := largemodel;
      trans[level0q] := level0;
      trans[librequestq] := librequest;
      trans[listq] := listcount;
      trans[longlibq] := longlib;
      trans[macroq] := outputmacro;
      trans[mainq] := mainbody;
      trans[objectq] := outputobj;
      trans[oldpackingq] := oldpacking;
      trans[oldreswordsq] := oldreswords;
      trans[ownq] := own;
      trans[pascal1q] := pascal1;
      trans[pdp11q] := pdp11data;
      trans[picq] := pic;
      trans[profileq] := profiling;
      trans[sharecodeq] := sharecode;
      trans[shortintsq] := shortintegers;
      trans[simq] := sim;
      trans[standardq] := standard;
      trans[statisticsq] := statistics;
      trans[stmtnumq] := stmtnumbers;
      trans[symbolq] := symboltable;
      trans[tblockq] := tblock;
      trans[testq] := test;
      trans[timesq] := timing;
      trans[truncateq] := truncatesw;
      trans[tswitch0q] := tswitch0;
      trans[tswitch1q] := tswitch1;
      trans[tswitch2q] := tswitch2;
      trans[tswitch3q] := tswitch3;
      trans[usebsd42libq] := usebsd42lib;
      trans[usesysVlibq] := usesysVlib;
      trans[versionq] := noswitch;
      trans[walkbackq] := walkback;
      trans[windowsq] := windows;
      trans[workspq] := noswitch;
      trans[notfound] := noswitch;

      for j := noswitch to finalswitch do
        begin
        switchcounters[j] := 0;
        switcheverplus[j] := false;
        end;

      for i := blanks to notfound do
        if i in qualsset then
          begin
          switchcounters[trans[i]] := 1;
          switcheverplus[trans[i]] := true;
          if i = checkq then
            begin
            switchcounters[indexcheck] := 1;
            switcheverplus[indexcheck] := true;
            switchcounters[mathcheck] := 1;
            switcheverplus[mathcheck] := true;
            switchcounters[nilcheck] := 1;
            switcheverplus[nilcheck] := true;
            switchcounters[rangecheck] := 1;
            switcheverplus[rangecheck] := true;
            switchcounters[stackcheck] := 1;
            switcheverplus[stackcheck] := true;
            end;

          if i = fpc68881q then
            begin
            switcheverplus[cpu68000] := false;
            switcheverplus[cpu68020] := true;
            end;
          end;

      { For 68k native compilers, the default processor type is set
        in scan by calling p_prctyp to test the hardware.  For cross
        compilers we must assume 68000.
      }
      if not ((targetmachine = mc68000) and
              (targetmachine = hostmachine) and
              (hostopsys = targetopsys)) then
        if not (cpu68020q in qualsset) then qualsset := qualsset + [cpu68000q];

      { If a 68k unix-targeted small compiler is desired, the following line 
        may be removed and "unixtarget" can be made a constant.  This will
        allow dead-code removal of some backend code.
      }

{      unixtarget := temp_unixtarget;}
    end {passtocompiler} ;


procedure csi;

  var i: integer;

  begin {csi}
    targetintsize := defaulttargetintsize;
    targetrealsize := defaulttargetrealsize;
    targetmaxint := defaulttargetmaxint;
    ptrsize := defaultptrsize;
    returnlinksize := defreturnlinksize;
    extreturnlinksize := defextreturnlinksize;

    initquals;

    fieldsfound := 0;
    lastfield := 1;
    filefound := false;
    outspeced := false;
    emptyfileflag := false;
    firstfound := false;
    secondfound := false;
    manyfound := false;
    fakelist := false;
    sourcelisthead := nil;
    includelisthead := nil;
    envname := nil;
    defname := nil;
    objname := nil;
    macname := nil;
    listname := nil;
    numquals := [workspq, tblockq, genmaskq];
    cmdquals := [];

    if language <> c then
      qualsset := [checkq, mainq, walkbackq, framepointerq, librequestq];

    temp_unixtarget := defunixtarget;
    fppspecified := false;

    for i := 1 to paramcount do
      begin
      param := paramstr(i);
      next := 1;
      if pos('--', param) <> 0 then
	begin
	param := rightstr(param, length(param) - 2);
        paramlength := length(param);
        takequal(param);
	end
      else
	begin
        paramlength := length(param);
        takefilename(false, sourcelisthead, next, false);
	outspeced := true;
        end;
      end;

    checkconsistency;
    printversion;

    case targetmachine of
      mc68000:
        if not (framepointerq in qualsset) then
          begin
          returnlinksize := ptrsize;
          extreturnlinksize := ptrsize;
          end;
      otherwise;
      end;

    passtocompiler;

    if switcheverplus[doublereals] then targetrealsize := doublesize;

    if switcheverplus[shortintegers] then
      begin
      targetintsize := shorttargetintsize;
      targetmaxint := shortmaxint;
      end;

  end {csi} ;
end.
