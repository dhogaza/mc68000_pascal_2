{$nomain}
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

{ Command String Interpreter, parses the command string and sets
  the initial state of the switches.
}

type
  tempfileonetype = file of packed array [0..diskbufsize] of hostfilebyte;
  tempfiletwotype = file of packed array [0..diskbufsize] of hostfilebyte;

var
{ The following file declarations must be included in each pass,
  and must be the first variables declared in that pass.
}

  tempfileone: tempfileonetype; {interface file scan-analys, travrs-code}
  tempfiletwo: tempfiletwotype; {interface file analys-travrs}


procedure getcmdline(var line: cmdbuffer; {resulting command line}
                     var length: cmdindex {resulting command length} );
  external;


procedure gmcr;
  external; { Get command line for RSX }


function hdwropt: integer;

{ Tests PDP-11 hardware to see what kind of options are provided.  The results
  are:
        0:      No options.
        1:      EIS integer multiply/divide
        2:      FIS floating instruction set (four function, implies EIS also)
        3:      FPP floating point processor (incompatible instructions with
                                              FIS, but also implies EIS)
  Tests iAPX-86 hardware to see if an 80(2)87 is present.  The result is:

        0: no 80(2)87 present
        1: 80(2)87 present

  The test consists of consulting the equipment bytes (INT 11H), bit 1.
}
  external;


procedure getline(var line: cmdbuffer; {resulting command line}
                  var length: cmdindex {resulting command length} );

{ RT 11 procedure to get a command line no matter what its source.
}

  external;


procedure p_getenv(idx: shortint;
                   var param: packed array [l..h: shortint] of char;
                   var len: shortint);
 {msdos procedure to get an environment string}
  external;

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

    qualifier = packed array [1..qualifierlength] of char; {qual name}
    qualtable = array [quals] of qualifier; {look-up table}

    unix_target_table_type = array [unixflavors] of qualifier;
    {options for unixtarget switch }

    errorname = (ambig, ambig_option, unknown, nono, qualtwice, twofilenames,
                 badfile, nofile, noinput, manyouts, badparam, numrange,
                 contradiction, badsyntax, missingparen, outconflict,
                 cmdtoolong, pic_own_section);

    qualtrans = array [quals] of switch; {translate quals to switches}
    qualset = set of quals;

  var

    qualindex: qualtable; {qualifier lookup table}
    unix_target_table: unix_target_table_type; {options for unixtarget switch}
    numquals: qualset; {quals with numeric parameters}
    cmdquals: qualset; {quals set on command line}
    qualsset: qualset; {quals turned on at start of compile}

    next: cmdindex; {next char in command string}
    filefound: boolean; {file found in this field}
    filestart, filefinish: cmdindex; {start/end of last file name found}
    fieldsfound: 0..cmdlinelength; {number of fields found}
    firstfound: boolean; {first file field found}
    secondfound: boolean; {second file field found}
    manyfound: boolean; {more than 2 file fields found}
    outspeced: boolean; {the <output>=<input> form was used}
    emptyfileflag: boolean; {empty file name found, possibly an error}
    emptystart: cmdindex; {start of empty file field}
    emptyend: cmdindex; {end of empty file field}
    lastfield: cmdindex; {end of last field scanned}
    temp_unixtarget: unixflavors; {Temporary holder for the global unixtarget.
                                   On small compilers unixtarget may be a
                                   constant.}
    fppspecified: boolean; {if ever specified}

  procedure skipblanks;

    begin {skipblanks}
      while (next < cmdlength) and (cmdline[next] = ' ') do
        next := next + 1;
    end {skipblanks} ;


  procedure vdosreformat;

{ Rearranges the command line for M68000 VERSAdos into our normal format.
  This is probably easier than rewriting the various parsing routines in
  the rest of CSI.
}

    var
      next, savenext, newnext, endinfiles, startquals: cmdindex; {buffer
        indexes}
      qualspresent: boolean; {set if quals found}
      newline: cmdbuffer; {PDP type command to be built}


    begin {vdosreformat}
      case hostopsys of
        vdos:
          begin
          new(savecmdline);
          savecmdline^ := cmdline; { save for listing }
          next := 1;
          newnext := 1;
          qualspresent := false;
          skipblanks;
          savenext := next; { start of input fields }

          while (next < cmdlength) and not (cmdline[next] in [',', ';']) do
            next := next + 1; { scan past input fields }
          endinfiles := next;
          if next < cmdlength then
            begin
            if cmdline[next] = ',' then { we have output or list files }
              begin
              while (next < cmdlength) and not (cmdline[next] = ';') do
                begin
                next := next + 1;
                newline[newnext] := cmdline[next];
                newnext := newnext + 1;
                end;
              newline[newnext - 1] := '=';
              end;
            if cmdline[next] = ';' then { we have qualifiers }
              begin
              qualspresent := true;
              startquals := next;
              end;
            end;

          next := savenext;
          while (next < endinfiles) do { reformat input file(s) }
            begin
            newline[newnext] := cmdline[next];
            if newline[newnext] = '/' then newline[newnext] := ',';
            newnext := newnext + 1;
            next := next + 1;
            end;
          if qualspresent then { reformat quals }
            begin
            newline[newnext] := '/';
            next := startquals;
            while (next < cmdlength) do
              begin
              newnext := newnext + 1;
              next := next + 1;
              newline[newnext] := cmdline[next];
              if newline[newnext] = ',' then newline[newnext] := '/';
              end;
            end;
          cmdline := newline;
          cmdlength := newnext;
          cmdline[cmdlength] := ' ';
          end;
        otherwise { do nothing } ;
        end;
    end; { vdosreformat }


  procedure settoday;

{ Set todays date, in a fashion suitable for license checking.
}

    var
      month, day, year, dummy: integer;


    begin {settoday}
      timestamp(day, month, year, dummy, dummy, dummy);
      if year <= 1980 then year := 1981;
      today := (year - 1981) * 512 + month * 32 + day;
      z_b130(early, late);
    end {settoday} ;


  function skypilot: shortint;

    external;
  
  procedure printversion;

{ Print a version message }

  
    procedure printwarning;
  
      var
        headerline: hdrline; {Site header line}
        headerlength: integer; {length of site header line}
        site1, site2: integer; {site number site1-site2}
  
      begin {printwarning}
        writeln;
        z_b129(headerline, headerlength, site1, site2);
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
            msdos: write('MSDOS');
            unix: write('UNIX');
            end;
          write(' to ');
          end
        else
          write('for ');
        write(systemtitle);
        write('.  Version ', version);
        case targetopsys of
          msdos: write(versionlevel);
          end;
        writeln;
        write('Copyright (C) 1986 - 1989 by Oregon Software, Inc.  ');
        writeln('ALL RIGHTS RESERVED.');
        end;
      if hostopsys = msdos then
        if skypilot = 1 then printwarning;
    end {printversion} ;


  procedure error(which: errorname; {which error}
                  startind: cmdindex; {start of command to print}
                  endind: cmdindex {end of command to print} );

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
            if cmdline[i] in [',', '/', '='] then write(' ')
            else write(cmdline[i]);
          otherwise write(cmdline[i]);
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
      exitst(exitstatus); {value is opsys dependent}
    end; {error}


  procedure initquals;

{ Initialize the qualifier table.
}

    var
      i: quals; {induction in qualindex table}
      j: unixflavors; {induction in host_table}


    begin {initquals}
      for i := blanks to notfound do qualindex[i] := ' XXXXXXXXXXX';

      if language = c then
        begin
        qualindex[objectq] := 'OBJECT      ';
        qualindex[includelistq] := 'INCLUDE     ';
        qualindex[cplusplusq] := 'CPP         ';
        qualindex[ansiq] := 'ANSI        ';
        qualindex[compatq] := 'COMPATIBILIT';
        qualindex[checkq] := 'CHECKOUT    ';
        qualindex[debugq] := 'DEBUG       ';
        qualindex[detailsq] := 'DETAILS     ';
        qualindex[enumintsq] := 'ENUMINTS    ';
        qualindex[expandfloatq] := 'EXPANDFLOAT ';
        qualindex[framepointerq] := 'FRAME       ';
        qualindex[genmaskq] := 'GENMASK     ';
        qualindex[indexcheckq] := 'INDEXCHECK  ';
        qualindex[listq] := 'LIST        ';
        qualindex[errorsq] := 'ERRLIST     ';
        qualindex[listincq] := 'LISTINC     ';
        qualindex[listexpq] := 'EXPLIST     ';
        qualindex[modstringsq] := 'MODSTRINGS  ';
        qualindex[nilcheckq] := 'NILCHECK    ';
        qualindex[macroq] := 'MACRO       ';
        qualindex[outputprepq] := 'PREPOUTPUT  ';
        qualindex[ppdefq] := 'DEFINE      ';
        qualindex[ppundefq] := 'UNDEFINE    ';
        qualindex[rangecheckq] := 'RANGECHECK  ';
        qualindex[romconstsq] := 'ROMCONSTS   ';
        qualindex[signedcharq] := 'SIGNEDCHAR  ';
        qualindex[standardq] := 'STANDARD    ';
        qualindex[structstaticq] := 'STRUCTSTATIC';    { "no" allowed }
        qualindex[testq] := 'TEST        ';
        qualindex[vectextrefq] := 'EXTVECTREF  ';
        qualindex[vectextdefq] := 'EXTVECTDEF  ';
        qualindex[timesq] := 'TIMES       ';
        qualindex[walkbackq] := 'WALKBACK    ';  { "no" allowed }
        end
      else
        begin
        qualindex[checkq] := 'CHECK       ';
        qualindex[caseq] := 'CASE        ';
        qualindex[debugq] := 'DEBUG       ';
        qualindex[detailsq] := 'DETAILS     ';
        qualindex[doubleq] := 'DOUBLE      ';
        qualindex[errorsq] := 'ERRORS      ';
        qualindex[genmaskq] := 'GENMASK     ';
        qualindex[includelistq] := 'INCLUDE     ';
        qualindex[level0q] := 'LEVEL0      ';
        qualindex[listq] := 'LIST        ';
        qualindex[macroq] := 'MACRO       ';
        qualindex[mainq] := 'MAIN        ';
        qualindex[objectq] := 'OBJECT      ';
        qualindex[ownq] := 'OWN         ';
        qualindex[profileq] := 'PROFILE     ';
        qualindex[shortintsq] := 'SHORTINTS   ';
        qualindex[standardq] := 'STANDARD    ';
        qualindex[stmtnumq] := 'STMTNUM     ';
        qualindex[symbolq] := 'SYMBOLS     ';
        qualindex[tblockq] := 'TBLOCK      ';
        qualindex[testq] := 'TEST        ';
        qualindex[timesq] := 'TIMES       ';
        qualindex[tswitch0q] := 'TSWITCH0    ';
        qualindex[tswitch1q] := 'TSWITCH1    ';
        qualindex[tswitch2q] := 'TSWITCH2    ';
        qualindex[tswitch3q] := 'TSWITCH3    ';
        qualindex[versionq] := 'VERSION     ';
        qualindex[walkbackq] := 'WALKBACK    ';
        qualindex[defineq] := 'DEFINE      ';
        qualindex[environq] := 'ENVIRONMENT ';
        qualindex[framepointerq] := 'FRAMEPOINTER';
        qualindex[oldreswordsq] := 'OLDRESWORDS ';

        case hostopsys of
          msdos:
            begin
            qualindex[editlistq] := 'EDITLIST    ';
            end;
          end;

        case targetmachine of
          vax:
            begin
            qualindex[oldpackingq] :=   'OLDPACKING  ';
            qualindex[pdp11q] := 'PDP11       ';
            qualindex[picq] := 'PIC         ';
            end;
          pdp11:
            begin
            qualindex[eisq] := 'EIS         ';
            qualindex[fisq] := 'FIS         ';
            qualindex[fppq] := 'FPP         ';
            qualindex[pascal1q] := 'PASCAL1     ';
            qualindex[simq] := 'SIMQ        ';
            qualindex[workspq] := 'WORKSPACE   ';
            end;
          iapx86:
            begin
            qualindex[bstepq] := 'BSTEP       ';
            qualindex[byteallocq] := 'BYTEALLOC   ';
            qualindex[codeconstq] := 'CODECONST   ';
            qualindex[commonvarsq] := 'COMMUNAL    ';
            qualindex[cpu8086q] := '8086        ';
            qualindex[cpu80286q] := '80286       ';
            qualindex[fppq] := 'FPP         ';
            qualindex[groupownq] := 'GROUPOWN    ';
            qualindex[largemodelq] := 'LARGEMODEL  ';
            qualindex[librequestq] := 'LIBREQUEST  ';
            qualindex[truncateq] := 'TRUNCATE    ';
            qualindex[windowsq] := 'WINDOWS     ';
            qualindex[statisticsq] := 'DATA        ';
            end;
          mc68000:
            begin
            if targetopsys = apollo then
              begin
              qualindex[sharecodeq] := 'SHARECODE   ';
              qualindex[usebsd42libq] := 'BSD42LIB    ';
              qualindex[usesysVlibq] := 'SYSVLIB     ';
              end
            else
              begin
              qualindex[cpu68000q] := '68000       ';
              qualindex[cpu68020q] := '68020       ';
              qualindex[fpc68881q] := '68881       ';
              qualindex[picq] := 'PIC         ';
              end;
            if targetopsys = vdos then
              qualindex[longlibq] := 'LONGLIB     '
            else if targetopsys = unix then
              begin
              qualindex[longlibq] := 'LONGLIB     ';
              qualindex[unixtargetq] := 'UNIXTARGET  ';
              end;
            end;
          ns32k:
            begin
            qualindex[floatsafeq] := 'FLOATSAFE   ';
            qualindex[picq] := 'PIC         ';
            end;
          end {case} ;
        end;

      if (targetopsys = unix) and (targetmachine = mc68000) then
        begin
        for j := nohost to last_host do unix_target_table[j] := ' XXXXXXXXXXX';
        unix_target_table[uniplus] := 'UNIPLUS     ';
        unix_target_table[uniplusiii] := 'UNIPLUSIII  ';
        unix_target_table[uniplusv] := 'UNIPLUSV    ';
        unix_target_table[masscomp] := 'MASSCOMP    ';
        unix_target_table[xenix] := 'XENIX       ';
        unix_target_table[tandy] := 'TANDY       ';
        unix_target_table[sun] := 'SUN         ';
        unix_target_table[munix] := 'MUNIX       ';
        unix_target_table[regulus] := 'REGULUS     ';
        unix_target_table[wicat] := 'WICAT       ';
        unix_target_table[candd] := 'CANDD       ';
        unix_target_table[perpos] := 'PERPOS      ';
        unix_target_table[lmi] := 'LMI         ';
        unix_target_table[ncr] := 'NCR         ';
        unix_target_table[venix] := 'VENIX       ';
        unix_target_table[uniflex] := 'UNIFLEX     ';
        unix_target_table[vmev2] := 'VMEV2       ';
        unix_target_table[uniplusv2] := 'UNIPLUSV2   ';
        unix_target_table[nti] := 'NTI         ';
        unix_target_table[ctix] := 'CTIX        ';
        end;
    end {initquals} ;




  procedure getcommandline;

  { Read the command line into memory.
  }

    var
      i: cmdindex; {induction variable}


    begin {getcommandline}
      cmdlength := 1;
      getcmdline(cmdline, cmdlength); { returns actual cmdline length }
      cmdlength := min(cmdlinelength, cmdlength + 1);
      if (hostopsys <> vms) and (hostopsys <> msdos) and (cmdlength = 1) then
        begin {no command line}
        case hostopsys of
          vdos: write('MP2>');
          rsx: { RSX to Versados cross compiler }
            begin
            gmcr;
            if input^ <> ' ' then {cross-compiler assumes an RSX system}
              begin {skip program names}
              repeat
                get(input)
              until (input^ = ' ');
              while not eoln and (input^ = ' ') do get(input);
              end;
            if input^ = ' ' then write('PAS>');
            if eoln then readln;
            end;
          otherwise write('PAS>');
          end;

        while not eoln do
          begin
          if cmdlength < cmdlinelength - 2 then
            begin
            cmdline[cmdlength] := input^;
            cmdlength := cmdlength + 1;
            end;
          get(input);
          end;
        end; {no command line}
      for i := cmdlength to cmdlinelength do cmdline[i] := ' ';
      if hostopsys = vdos then vdosreformat;
    end {getcommandline} ;



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
                          start, finish: cmdindex; {from where}
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
        arglen := min(finish - start, filenamelen);
        if hostopsys = vdos then dotcount := 2;
        for i := 1 to filenamelen do
          begin
          if start < finish then
            begin
            arg[i] := cmdline[start];
            case hostopsys of
              vms:
                if (cmdline[start] = ']') or (cmdline[start] = ':') then
                  directorydelim := true;
              vdos:
                if cmdline[start] = '.' then dotcount := dotcount - 1;
              otherwise;
              end {case} ;
            start := start + 1;
            end
          else arg[i] := ' ';
          end;
        if isinclude then
          case hostopsys of
            vms:
              begin
              if (not directorydelim) and (arglen < filenamelen) then
                begin
                arglen := arglen + 1;
                arg[arglen] := ':';
                end;
              end;
            msdos:
              begin
              if (arg[arglen] <> '\') and (arglen < filenamelen) then
                begin
                arglen := arglen + 1;
                arg[arglen] := '\';
                end;
              end;
            vdos:
              while (dotcount > 0) and (arglen < filenamelen) do
                begin
                arglen := arglen + 1;
                arg[arglen] := '.';
                end;
            otherwise;
            end {case} ;
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
                         var next: cmdindex; {index in command line}
                         isinclude: boolean {if this file in includes});

{ Parse a filename, and add it to the list specified by "list".
  This routine makes sure that there is only one file specified per field.
}

    var
      startindex: cmdindex; {start of this field}
      terminators: set of char; {what stops a file name}
      quotedstring: boolean; {flag for quote removal}


    procedure skipbalancedstring;

{ Skip a string balanced with respect to parentheses and quoted strings.
}

      var
        endchar: char; {bracket which terminates string}


      begin {skipbalancedstring}
        if cmdline[next] in ['(', '[', '<'] then
          begin
          if cmdline[next] = '(' then endchar := ')'
          else if cmdline[next] = '<' then endchar := '>'
          else endchar := ']';
          next := next + 1;
          while (cmdline[next] <> endchar) and (next <> cmdlength) do
            skipbalancedstring;
          end
        else if cmdline[next] = '"' then
          begin
          quotedstring := true;
          repeat
            next := next + 1
          until (cmdline[next] = '"') or (next = cmdlength);
          end;
        if next < cmdlength then next := next + 1
        else error(badfile, startindex, cmdlength);
      end {skipbalancedstring} ;


    begin {takefilename}
      quotedstring := false;
      startindex := next;
      terminators := [',', '/', '=', ' '];
      if (hostopsys = vms) then terminators := terminators + ['+'];
      if (hostopsys = msdos) then terminators := terminators + ['+', ';'];
      if parenthesized then terminators := terminators + [')'];
      while (next < cmdlength) and not (cmdline[next] in terminators) do
        skipbalancedstring;
      if filefound then error(twofilenames, filestart, next)
      else
        begin
        filefound := true;
        filestart := startindex;
        filefinish := next;
        if quotedstring then
          begin
          filestart := filestart + 1;
          if (cmdline[next] = '"') or (filefinish <= cmdlength) then
            filefinish := filefinish - 1
          end;
        addtofilelist(list, filestart, filefinish, isinclude);
        end;
    end {takefilename} ;


  procedure takequal(var next: cmdindex);

{ Parse and look up a qualifier, updating "next" to point to the next
  character in the command line.
}

    var
      s: integer; {temporary}
      startingindex: cmdindex; {start of qualifier, for error printout}
      quali: 0..qualifierlength; {current character in qual being built}
      name: qualifier; {qualifier name}
      nofound: boolean; {true if "no" preceeds qualifier}
      thisqual: quals; {qualifier just found}
      this_target: unixflavors; {target just found}
      ambiguous: boolean; {qualifier lookup was ambiguous}


    procedure findqual(target: qualifier; {candidate Qual name}
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
        partials := 0;
        effectivelength := 0;
        for i := 1 to qualifierlength do
          if target[i] <> ' ' then effectivelength := i;
        result := blanks;
        qualindex[notfound] := target; {to terminate search}

        while target <> qualindex[result] do
          begin
          result := succ(result);
          partialmatch := target <> qualindex[result];
          for i := 1 to effectivelength do
            partialmatch := partialmatch and
                            (target[i] = qualindex[result, i]);
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


    procedure match_unix_target(target: qualifier; {candidate Qual name}
                                var result: unixflavors; {result of lookup}
                                var ambiguous: boolean {more than one match} )
                                ;

{ Look up "target" in the unix_target_table and set "result" to the
  appropriate qualifier.  If there is a full match, this is always
  taken.  Otherwise, a single partial match will be accepted.
  Multiple partial matches cause "ambiguous" to be set.
}

      var
        partialmatch: boolean; {partially matches the Qual so far}
        partialresult: unixflavors; {where the match was}
        partials: 0..maxint; {counter of partial matches}
        effectivelength: 0..qualifierlength; {significant chars in target}
        i: 1..qualifierlength; {induction var}


      begin {match_unix_target}
        if (targetopsys = unix) and (targetmachine = mc68000) then
          begin
          partials := 0;
          effectivelength := 0;
          for i := 1 to qualifierlength do
            if target[i] <> ' ' then effectivelength := i;
          result := nohost;
          unix_target_table[last_host] := target; {to terminate search}

          while target <> unix_target_table[result] do
            begin
            result := succ(result);
            partialmatch := target <> unix_target_table[result];
            for i := 1 to effectivelength do
              partialmatch := partialmatch and
                              (target[i] = unix_target_table[result, i]);
            if partialmatch then
              begin
              partialresult := result;
              partials := partials + 1;
              end;
            end;
          if (result = last_host) and (partials = 1) then
            result := partialresult;
          ambiguous := partials > 1;
          end;
      end {match_unix_target} ;


    procedure getnumqual(var result: integer; {resulting value}
                         low_lim, hi_lim: integer {limits on value} );

{ Scan off a number, for qualifiers that take numeric arguments.
}

      var
        tempres: integer;
        accumulating, negate: boolean;


      begin {getnumqual}
        if cmdline[next] in [':', '='] then next := next + 1
        else error(badparam, startingindex, next - 1);
        accumulating := true;
        tempres := 0;
        negate := cmdline[next] = '-';
        if negate then next := next + 1;
        while cmdline[next] in ['0'..'9'] do
          begin
          if accumulating then
            if tempres <= maxint div 10 then tempres := tempres * 10
            else accumulating := false;
          if accumulating then
            if tempres <= maxint - (ord(cmdline[next]) - ord('0')) then
              tempres := tempres + (ord(cmdline[next]) - ord('0'))
            else accumulating := false;
          next := next + 1;
          end;
        if negate then tempres := - tempres;
        if accumulating and (tempres <= hi_lim) and (tempres >= low_lim) then
          result := tempres
        else error(numrange, startingindex, next - 1);
      end {getnumqual} ;


    procedure getspecialfilename(q: quals; {which qualifier}
                                 var next: cmdindex);

{ Handle the "/<qual>=<filename>" format for the macro, object, list,
  errors, include and environment qualifiers, and the 
  "/include=(<filenamelist>)" format.
}

      var
        start: cmdindex; {start of a filename}
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
        if cmdline[next] <> '(' then
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
            skipblanks;
            if not (q in [environq, includelistq]) and (cmdline[next] <> ')')
            then error(missingparen, start - 1, next);
          until (next >= cmdlength) or (cmdline[next] = ')');
          if next < cmdlength then next := next + 1
          else error(missingparen, start - 1, cmdlength);
          filefound := savefilefound;
          end;
      end {getspecialfilename} ;


    begin {takequal}
      repeat
        next := next + 1;
      until (next >= cmdlength) or (cmdline[next] <> ' ');
      startingindex := next;
      quali := 0;
      nofound := false;
      if (next < cmdlinelength - 2) then
        if (uc(cmdline[next]) = 'N') and (uc(cmdline[next + 1]) = 'O') then
          begin
          nofound := true;
          next := next + 2;
          end;
      while (next <= cmdlinelength) and
            (cmdline[next] in ['A'..'Z', 'a'..'z', '0'..'9']) do
        begin
        if quali < qualifierlength then
          begin
          quali := quali + 1;
          name[quali] := uc(cmdline[next]);
          end;
        next := next + 1;
        end;

      if quali = 0 then error(badsyntax, 2, 1);

      while quali < qualifierlength do
        begin
        quali := quali + 1;
        name[quali] := ' ';
        end;
      findqual(name, thisqual, ambiguous);
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
         (next <= cmdlinelength) and (cmdline[next] in [':', '=']) then
        if nofound then error(nono, startingindex, next)
        else getspecialfilename(thisqual, next);

      { Handle the unixtarget switch option }

      if (targetopsys = unix) and (targetmachine = mc68000) then
        if thisqual = unixtargetq then
          begin

          { If there is no argument then use the default. }

          if cmdline[next] in [':', '='] then
            begin
            next := next + 1;

            startingindex := next;
            quali := 0;

            while (next <= cmdlinelength) and
                  (cmdline[next] in ['A'..'Z', 'a'..'z', '0'..'9']) do
              begin
              if quali < qualifierlength then
                begin
                quali := quali + 1;
                name[quali] := uc(cmdline[next]);
                end;
              next := next + 1;
              end;

            if quali = 0 then error(badsyntax, 2, 1);

            while quali < qualifierlength do
              begin
              quali := quali + 1;
              name[quali] := ' ';
              end;

            match_unix_target(name, this_target, ambiguous);

            if ambiguous then error(ambig_option, startingindex, next - 1)
            else if this_target = last_host then
              error(unknown, startingindex, next - 1);
            temp_unixtarget := this_target;
            end;
          end;
    end {takequal} ;


  procedure endfield(var next: cmdindex {next character} );

{ Terminate a field, making several checks.
}


    begin {endfield}
      fieldsfound := fieldsfound + 1;
      if (fieldsfound = 1) and filefound then firstfound := true
      else if (fieldsfound = 2) and filefound then secondfound := true
      else if filefound then manyfound := true;
      if not filefound then
        if outspeced then error(nofile, lastfield, next)
        else if not emptyfileflag then
          begin
          emptyfileflag := true;
          emptystart := lastfield;
          emptyend := next;
          end;
      if cmdline[next] = '=' then
        if fieldsfound > 2 then error(manyouts, 1, next - 1)
        else
          begin
          fieldsfound := 2;
          outspeced := true;
          end;
      lastfield := next;
      if next < cmdlength then next := next + 1;
      filefound := false;
    end {endfield} ;


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
      if hostopsys <> msdos then
        begin
        if not ((outspeced and manyfound) or firstfound) then
          error(noinput, 2, 1);
        if not outspeced and emptyfileflag then
          error(nofile, emptystart, emptyend);
        if outspeced then
          if firstfound and ((objname <> nil) and (macname <> nil) or
             (objname <> nil) and not (macroq in qualsset) or
             (macname <> nil) and not (objectq in qualsset)) or
             secondfound and (listname <> nil) then
            error(outconflict, 2, 1);
        end;

      case targetopsys of
        vdos:
          begin
          if (picq in qualsset) and (ownq in qualsset) and
             (datasection <> defdatasection) then
            error(pic_own_section, 2, 1);

          if twoof([listq, errorsq]) or twoof([defineq, objectq, macroq]) or
             twoof([errorsq, debugq, profileq]) or
             twoof([cpu68000q, cpu68020q]) or
             twoof([cpu68000q, fpc68881q]) or
             (([debugq, profileq] * qualsset <> []) and
             not (mainq in qualsset)) then
            error(contradiction, 2, 1);
          end;
        otherwise
          begin
          case targetmachine of
            pdp11:
              begin
              if twoof([listq, errorsq]) or twoof([defineq, objectq, macroq]) or
                 twoof([eisq, fisq, fppq, simq]) or
                 twoof([errorsq, debugq, profileq]) then
                error(contradiction, 2, 1);
              if (qualsset * [eisq, fisq, fppq, simq] = []) then
                begin
                case hostmachine of
                  pdp11:
                  {no processor options specified, use host}
                    case hdwropt of
                      0: thisopt := simq;
                      1: thisopt := eisq;
                      2: thisopt := fisq;
                      3: thisopt := fppq
                      end;
                  otherwise thisopt := fppq; {default for cross-compilers}
                  end;
                qualsset := qualsset + [thisopt];
                end;
              if eisq in qualsset then qualsset := qualsset + [simq]
              else if [fisq, fppq] * qualsset <> [] then
                qualsset := qualsset + [eisq];
              end;
            iAPX86:
              begin
              if twoof([listq, errorsq]) or twoof([listq, editlistq]) or
                 twoof([macroq, defineq]) or twoof([objectq, defineq]) or
                 twoof([errorsq, debugq, profileq]) or
                 twoof([cpu8086q, cpu80286q]) then
                error(contradiction, 2, 1);
              if cpu8086q in qualsset then qualsset := qualsset - [bstepq];
              if not fppspecified then
                begin
                if hostmachine = iAPX86 then
                {no processor options specified, use host}
                  if hdwropt = 1 then qualsset := qualsset + [fppq];
                end;
              end;
            otherwise {non pdp11 targets}
              begin
              if twoof([listq, errorsq]) or
                 twoof([macroq, defineq]) or twoof([objectq, defineq]) or
                 twoof([errorsq, debugq, profileq]) then
                error(contradiction, 2, 1);
              end;
            end {case} ;
          end;
        end {case} ;

      if defineq in qualsset then
        qualsset := qualsset - [objectq, macroq, mainq]
      else if ((outspeced and firstfound) or not outspeced) and
         (qualsset * [objectq, macroq] = []) then
        qualsset := qualsset + ([objectq] - cmdquals);
      if qualsset * [listq, errorsq] = [] then
        if outspeced and secondfound then qualsset := qualsset + [listq]
        else if newdebugger or (qualsset * [debugq, profileq] = []) then
          begin
          fakelist := true;
          qualsset := qualsset + [errorsq]
          end;
      if qualsset * [debugq, profileq] <> [] then
        if newdebugger then qualsset := qualsset + [symbolq] - [walkbackq]
        else qualsset := qualsset + [listq, symbolq] - [walkbackq];
      if qualsset * [debugq, profileq, walkbackq, sharecodeq] <> [] then
        qualsset := qualsset + [framepointerq];
      if symbolq in qualsset then
        if qualsset * [objectq, macroq, defineq] = [] then 
          error(contradiction, 2, 1);
      if twoof([standardq, caseq]) then error(contradiction, 2, 1);

      if targetopsys = apollo then
        begin
        qualsset := qualsset + [cpu68020q, fpc68881q];
        if (sharecodeq in qualsset) and not (walkbackq in cmdquals) then
          qualsset := qualsset - [walkbackq];
        if twoof([debugq, profileq, walkbackq, sharecodeq]) or
           twoof([usebsd42libq, usesysVlibq]) then
          error(contradiction, 2, 1);
        end;

      if language = c then
        begin
        if twoof([compatq, ansiq, cplusplusq]) then
          error(contradiction, 2, 1);

        setdefault([compatq, ansiq, cplusplusq], cplusplusq);

        setdefault([macroq, outputprepq], objectq);

        setdefault([checkq], structstandq);  {?????}

        if (qualsset * [indexcheckq, nilcheckq, rangecheckq] <> []) then
          qualsset := qualsset + [walkbackq];

        if walkbackq in qualsset then qualsset := qualsset + [framepointerq];

        { Adjust listing switches. }

        if listincq in qualsset then qualsset := qualsset + [listq];

        { Add default include path to IncludeList }

{        if defaultincludepath2length > 0 then addtoinclude(defaultincludepath2);
        if defaultincludepath3length > 0 then addtoinclude(defaultincludepath3);
}
        end;

      {kludge pas2/lis=brol => access violation} 
      if sourcelisthead = nil then error(noinput, 2, 1); 

    end {checkconsistency} ;


  procedure installfilenames;

{ Put the various file names where they belong.  In other words, if the
  "x,y=z..." command line format was used, move x and y to the obj, mac,
  and list entries.  We allow one switch type filename for either obj or
  mac along with the "x" specification above, if both object and macro
  are requested.  In such a case, the "x" filespec is used for the output
  that was not given by the switch.  For example, if the command line is
  "pas x,y=z/mac/obj=q", the object file will be named "q", and the macro
  file will be named "x".
}

    var
      p: filenamelistptr; {points to the filename of interest at the moment}


    begin {installfilenames}
      p := sourcelisthead;
      if outspeced and firstfound then
        begin
        if (objectq in qualsset) and (objname = nil) then objname := p;
        if (macroq in qualsset) and (macname = nil) then macname := p;
        if (defineq in qualsset) and (defname = nil) then defname := p;
        sourcelisthead := sourcelisthead^.next;
        p^.next := nil;
        end;
      p := sourcelisthead;
      if outspeced and secondfound then
        begin
        if ((qualsset * [listq, errorsq]) <> []) and (listname = nil) then
          listname := p;
        sourcelisthead := sourcelisthead^.next;
        p^.next := nil;
        end;

      { *** Here's the place to add a default include list. *** }

    end {installfilenames} ;


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



  procedure msdosinterp;

{ Parse an msdos command line which is of the form:
     source(s), object, listing, assembly
  The last three fields are optional, and the object and listing fields will
  be prompted for if not specified.  A semicolon will immediately terminate
  the parsing and prompting process.  The environment is also accessed for
  any default switches in a string of the form "SWITCH=..." and default
  include directories of the form "INCLUDE=...".  The idea of a "current
  field" is maintain through the parsing process, with filenames encoutered
  thus placed in the "current" file name list.
}

    const
      includeenv = 'INCLUDE='; {environment keyword for include directories}
      cmdenv = 'SWITCH='; {environment keyword for command line switches}

    type
      fields = (sourcefield, objectfield, listfield, macrofield, extrafield);

    var
      currentfield: fields; {current field being parsed}
      currentlist: filenamelistptr; {file name pointer associated with the
                                     current field}
      defaultname: filenamelistptr; {base name used as default name}
      envincludelisthead: filenamelistptr; {include list from the environment}
      commandcutoff: boolean; {has a ';' been encoutered in command line}
      numextrafields: integer; {number of extra fields encoutered}
      found: boolean; {was environment keyword found}
      i: integer; {induction}


    procedure getenvironment(flagname: packed array
                               [low..high: shortint] of char;
                             var found: boolean);

{ Search the environment for the keyword "flagname".  If found, then append
  that string to the command line.
}

      var
        envidx, envlen: shortint;
        j: filenameindex;


      begin {getenvironment}
        if hostopsys = msdos then
          begin
          envidx := - 1;
          found := false;
          repeat
            envidx := envidx + 1;
            p_getenv(envidx, cmdline, envlen);
            if (envlen <> - 1) and (envlen > high) then
              begin
              found := true;
              for j := 1 to high do
                if flagname[j] <> cmdline[j] then found := false;
              end
          until found or (envlen = - 1);
          if found then
            begin
            next := high + 1;
            cmdlength := min(envlen + 1, cmdlinelength - 1);
            cmdline[cmdlength] := ' ';
            end;
          end;
      end {getenvironment} ;


    procedure msdosendfield(var next: cmdindex);

{ Switch context to the next field, and note if it is an extra field. 
}


      begin {msdosendfield}
        if hostopsys = msdos then
          begin
          case currentfield of
            sourcefield:
              begin
              sourcelisthead := currentlist;
              currentlist := objname;
              end;
            objectfield:
              begin
              objname := currentlist;
              currentlist := listname;
              end;
            listfield:
              begin
              listname := currentlist;
              currentlist := macname;
              end;
            macrofield:
              begin
              macname := currentlist;
              currentlist := nil;
              end;
            end;
          if currentfield = extrafield then
            numextrafields := numextrafields + 1;
          if currentfield <> extrafield then
            currentfield := succ(currentfield);
          if next < cmdlength then next := next + 1;
          filefound := false;
          end;
      end; {msdosendfield}


    procedure msdosparse;

{ Parse the command line from the current position until either the length
  (cmdlength) is consumed or a semicolon is encoutered.
}


      begin {msdosparse}
        if hostopsys = msdos then
          begin
          if cmdlength <> 1 then
            repeat
              if currentfield = sourcefield then filefound := false;
              skipblanks;
              case cmdline[next] of
                ',': msdosendfield(next);
                '+':
                  if currentfield = sourcefield then
                    begin
                    next := next + 1;
                    takefilename(false, currentlist, next, false);
                    end
                  else error(badsyntax, 2, 1);
                '=': error(badsyntax, 2, 1);
                '/': takequal(next);
                ';':
                  begin
                  commandcutoff := true;
                  cmdlength := next;
                  end;
                ' ': ;
                otherwise takefilename(false, currentlist, next, false);
                end;

            until (next >= cmdlength);
          msdosendfield(next);
          if numextrafields > 0 then
            writeln(numextrafields: 5, ' extra fields ignored');
          end;
      end {msdosparse} ;


    procedure promptuser;

{ Prompt the user for those fields which have not been given a specific file
  name.  The users response is appended to the command line, and parsing
  is continued.  Normally only the object and listing fields will generate
  prompts, but if the /macro switch was used then the macro field will also
  be prompted.  A semicolon will terminate this process.  Commas are inserted
  into the command line as field separaters.
}

      var
        lastfield: fields;
        len: integer;


      procedure promptforname(defaulted: boolean);

        var
          i: integer;


        begin {promptforname}
          if hostopsys = msdos then
            begin
            case currentfield of
              sourcefield: write('Source filename   ', '[', '.PAS', ']: ');
              objectfield:
                begin
                write('Object filename   ', '[');
                if defaulted then
                  with defaultname^ do
                    for i := 1 to arglen do write(uc(arg[i]))
                else
                  write('NUL');
                case targetopsys of
                  msdos, vms: write('.OBJ');
                  vdos: write('.RO');
                  end;
                write(']: ');
                end;
              listfield:
                begin
                write('Source listing    ', '[');
                if defaulted then
                  with defaultname^ do
                    for i := 1 to arglen do write(uc(arg[i]))
                else
                  write('NUL');
                write('.LST', ']: ')
                end;
              macrofield:
                begin
                write('Assembly filename ', '[');
                if defaulted then
                  with defaultname^ do
                    for i := 1 to arglen do write(uc(arg[i]))
                else
                  write('NUL');
                case targetopsys of
                  msdos: write('.ASM');
                  vdos: write('.SA');
                  vms: write('.MAR');
                  end;
                write(']: ');
                end;
              end;

            while not eoln do
              begin
              if cmdlength >= cmdlinelength - 1 then error(cmdtoolong, 2, 1)
              else
                begin
                cmdline[cmdlength] := input^;
                cmdlength := cmdlength + 1;
                end;
              get(input);
              end;
            readln;
            for i := cmdlength to cmdlinelength do cmdline[i] := ' ';
            end;
        end; {promptforname}


      begin {promptuser}
        if hostopsys = msdos then
          begin
          if sourcelisthead = nil then
            begin
            currentfield := sourcefield;
            currentlist := nil;
            promptforname(true);
            msdosparse;
            if sourcelisthead = nil then error(noinput, 1, 1);
            end;

          new(defaultname);
          getfilename(nil, true, true, defaultname^.arg, defaultname^.arglen);
          defaultname^.next := nil;

          if not commandcutoff then
            begin
            if (macroq in qualsset) then lastfield := macrofield
            else lastfield := listfield;

            if cmdlength < cmdlinelength then
              begin
              cmdline[cmdlength] := ',';
              cmdlength := cmdlength + 1;
              next := cmdlength;
              end;
            end;

          while (currentfield <= lastfield) and not commandcutoff do
            begin
            case currentfield of
              objectfield:
                if objname = nil then
                  if (objectq in qualsset) or not ((objectq in cmdquals) or
                     (macroq in qualsset)) then
                    promptforname(true)
                  else promptforname(false);
              listfield:
                if listname = nil then
                  if newdebugger then
                    promptforname([listq, errorsq] <> [])
                  else
                    promptforname([listq, errorsq, debugq, profileq] * qualsset
                                   <> []);
              macrofield:
                if macname = nil then promptforname(true);
              end;

            msdosparse;
            if not commandcutoff then
              begin
              if (macroq in qualsset) then lastfield := macrofield;
              if cmdlength < cmdlinelength then
                begin
                cmdline[cmdlength] := ',';
                cmdlength := cmdlength + 1;
                next := cmdlength;
                end;
              end;
            end;
          end;
      end {promptuser} ;


    procedure checkfilenames;

{ Clean up the object, macro and list file name lists.  First the lists
  are pared down to a single name, only the last one given being saved.
  A filename of "NUL" is taken as a directive to de-activate the given
  option,  e.g. an object filename of "NUL" is equivalent to "/noobj".
  Finally the environment is searched for include directories which are
  added to the include list.
}

      var
        nulname: boolean;
        i: integer;
        p: filenamelistptr;


      procedure parenamelist(var list: filenamelistptr;
                             var nulname: boolean);

        var
          prev: filenamelistptr;


        begin {parenamelist}
          if hostopsys = msdos then
            begin
            nulname := false;
            if (list <> nil) then
              begin
              while list^.next <> nil do
                begin
                prev := list;
                list := list^.next;
                dispose(prev);
                end;
              with list^ do
                if (arglen = 3) and (uc(arg[1]) = 'N') and
                   (uc(arg[2]) = 'U') and (uc(arg[3]) = 'L') then
                  begin
                  dispose(list);
                  list := nil;
                  nulname := true;
                  end;
              end;
            end;
        end {parenamelist} ;


      begin {checkfilenames}
        if hostopsys = msdos then
          begin
          parenamelist(objname, nulname);
          if nulname then
            begin
            qualsset := qualsset - [objectq];
            cmdquals := cmdquals + [objectq];
            end
          else if objname <> nil then qualsset := qualsset + [objectq];

          parenamelist(listname, nulname);
          if nulname then qualsset := qualsset - [listq, errorsq]
          else if listname <> nil then
            if newdebugger then
              begin
              if ([errorsq] * qualsset = []) then
                qualsset := qualsset + [listq];
              end
            else
              if ([errorsq, debugq, profileq] * qualsset = []) then
                qualsset := qualsset + [listq];

          parenamelist(macname, nulname);
          if nulname then qualsset := qualsset - [macroq]
          else if macname <> nil then qualsset := qualsset + [macroq];

          {append the include list from the environment to the includes list}
          if includelisthead = nil then includelisthead := envincludelisthead
          else
            begin
            p := includelisthead;
            while p^.next <> nil do p := p^.next;
            p^.next := envincludelisthead;
            end;
          end
      end {checkfilenames} ;


    procedure setdefaultnames;


      begin {setdefaultnames}
        if hostopsys = msdos then
          begin
          if (objectq in qualsset) and (objname = nil) then
            begin
            new(objname);
            objname^ := defaultname^;
            end;
          if ([listq, errorsq] * qualsset <> []) and (listname = nil) then
            begin
            new(listname);
            listname^ := defaultname^;
            end;
          if (macroq in qualsset) and (macname = nil) then
            begin
            new(macname);
            macname^ := defaultname^;
            end;
          dispose(defaultname);
          end;
      end {setdefaultnames} ;


    begin {msdosinterp}
      if hostopsys = msdos then
        begin
        commandcutoff := false;
        currentfield := sourcefield;
        currentlist := nil;
        numextrafields := 0;
        cmdlength := 1;
        next := 1;
        envincludelisthead := nil;

        {build environment includes list with a temporary head}
        getenvironment(includeenv, found);
        if found then
          while next < cmdlength do
            begin
            filefound := false;
            if (cmdline[next] = ';') or (cmdline[next] = ' ') then
              next := next + 1
            else takefilename(false, envincludelisthead, next, true);
            end;

        cmdlength := 1;
        next := 1;

        {first parse the environment switches}
        getenvironment(cmdenv, found);
        while next < cmdlength do
          begin
          skipblanks;
          if cmdline[next] = '/' then takequal(next)
          else error(badsyntax, next, cmdlength);
          end;
        commandcutoff := false;
        currentfield := sourcefield;
        currentlist := nil;
        numextrafields := 0;
        next := 1;
        cmdquals := [];
        getcommandline;
        msdosparse;

        printversion;
        promptuser;
        checkfilenames;
        checkconsistency;
        setdefaultnames;
        if commandcutoff then
          for i := next to cmdlength do cmdline[i] := ' ';
        end;
    end {msdosinterp} ;

procedure csi;

  begin {csi}
    targetintsize := defaulttargetintsize;
    targetrealsize := defaulttargetrealsize;
    targetmaxint := defaulttargetmaxint;
    ptrsize := defaultptrsize;
    returnlinksize := defreturnlinksize;
    extreturnlinksize := defextreturnlinksize;

    settoday;
    initquals;

    next := 1;
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

    case targetopsys of
      unix:
        case defunixtarget of
          inix86: qualsset := qualsset - [walkbackq] + [bstepq];
          atxenix: qualsset := qualsset - [walkbackq];
          end;
      vdos:
        begin
        datasection := defdatasection;
        qualsset := qualsset + [longlibq];
        numquals := numquals + [ownq];
        end;
      otherwise {do nothing} ;
      end;

    case hostopsys of
      msdos: qualsset := qualsset + [versionq];
      end;

    if hostopsys = msdos then msdosinterp
    else
      begin
      getcommandline;
      repeat
        skipblanks;
        case cmdline[next] of
          '=', ',': endfield(next);
          '+':
            if hostopsys = vms then endfield(next)
            else takefilename(false, sourcelisthead, next, false);
          '/': takequal(next);
          ' ': ;
          otherwise takefilename(false, sourcelisthead, next, false);
          end;
      until next >= cmdlength;
      endfield(next);
      if not outspeced then fieldsfound := fieldsfound + 2; {fake fields}
      checkconsistency;
      installfilenames;
      printversion;
      end;

    case targetmachine of
      pdp11:
        if not (walkbackq in qualsset) then
          begin {no dynamic link on PDP11 unless walkback specified}
          returnlinksize := ptrsize;
          extreturnlinksize := ptrsize;
          end;
      vax:
        if not (framepointerq in qualsset) and (targetopsys = vms) then
          begin
          returnlinksize := ptrsize;
          extreturnlinksize := ptrsize;
          end;
      mc68000:
        if not (framepointerq in qualsset) then
          begin
          returnlinksize := ptrsize;
          extreturnlinksize := ptrsize;
          end;
      iapx86:
        begin
        if switcheverplus[largemodel] then ptrsize := longptrsize;
        case targetopsys of
          msdos: if not (cpu80286q in qualsset) then
                   qualsset := qualsset + [cpu8086q];
          unix: if not (cpu8086q in qualsset) then
                   qualsset := qualsset + [cpu80286q];
          end;
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

    { Now set up any file names needed and unspecified }

    if language = c then
      begin
      if switcheverplus[outputprep] then
        default_file(prepname, prep_file_kind);
      if switcheverplus[outputmacro] then default_file(macname, mac_file_kind);
      if switcheverplus[outputobj] then default_file(objname, obj_file_kind);
      if switcheverplus[defineswitch] then
        default_file(defname, env_file_kind);
      if switcheverplus[environswitch] then
        default_file(envname, env_file_kind);
      if switcheverplus[debugging] then
        default_file(symtabname, sym_file_kind);
      if switcheverplus[debugging] then
        default_file(stmtname, stmt_file_kind);
      if switcheverplus[listcount] or switcheverplus[listerrors] then
        default_file(listname, list_file_kind);
      end;

  end {csi} ;
