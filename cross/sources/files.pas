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


  Release version: 0045 Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler File Handling Routines

 Last modified by KRIS on 21-Nov-1990 15:35:20

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

procedure getfilename;

{ Get a file name with or without the device or extension fields.
}

  var
    start, finish: FilenameIndex; {limits of file name}
    i, j: FilenameIndex; {induction on file name}
    scanning: boolean; {loop control}
    semi: FilenameIndex; {where the semicolon is, if any}


  procedure vdosprune(which: FilenameListPtr; {file name to examine}
                      stripdevice: boolean; {want no device/directory field}
                      stripext: boolean; {want no extension field}
                      var start: FilenameIndex; {result: where to start}
                      var finish: FilenameIndex {result: where to stop} );

{ Return the bounds of the desired part of the given VersaDOS filename,
  in "start" and "finish".

  This ridiculous procedure is necessitated by the ill-conceived VersaDOS
  filename format.  Since partially qualified filenames may be ambiguous,
  we have adopted the following strategy for their interpretation:

  1. If the filename begins with a "#", or there are no "."s, or there are
     three "."s, it is complete and unambiguous.
  2. If the filename contains a ":" (device name given), or the first field
     is all numeric (user number given), it is unambiguous, and we parse
     left to right.
  3. If the next-to-last field is an "&" (directory override), it is unam-
     biguous, and we parse right to left.
  4. Otherwise, the filename is ambiguous, so we parse right to left, and
     if the last field has fewer than three characters, it is taken to be
     the file extension.  If the last field has three or more characters,
     it is taken to be the file name.  (Whew!)
}

    const
      maxdots = 3; {max number of dots we expect in a filename}

    var
      dots: 0..3; {how many dots there actually are}
      colonloc: FilenameIndex; {where we saw a colon}
      amperloc: FilenameIndex; {where we saw an ampersand}
      allnumeric: boolean; {could be a user number}
      gotuser: boolean; {there really is a user number}
      dotloc: array [1..maxdots] of FilenameIndex; {where the dots are}
      i: FilenameIndex; {induction on filename}


    begin {vdosprune}
      if hostopsys = vdos then
        with which^ do
          begin
          start := 1;
          finish := arglen;
          if arg[1] <> '#' then
            begin
            for dots := 1 to maxdots do
              dotloc[dots] := 0;
            dots := 0;
            colonloc := 0;
            amperloc := 0;
            allnumeric := true;
            gotuser := false;
            for i := 1 to arglen do
              if arg[i] = '.' then
                begin
                if dots < maxdots then
                  begin
                  dots := dots + 1;
                  dotloc[dots] := i;
                  if dots = 1 then gotuser := allnumeric;
                  end;
                end
              else if arg[i] = ':' then
                begin
                colonloc := i;
                allnumeric := (dots = 0);
                end
              else if arg[i] = '&' then amperloc := i
              else if (arg[i] < '0') or (arg[i] > '9') then
                allnumeric := false;

            if dots <> 0 then
              begin
              if dots >= 3 then
                begin
                if stripdevice then start := dotloc[dots - 2] + 1;
                if stripext then finish := dotloc[dots] - 1;
                end
              else if gotuser or (colonloc > 0) then
                begin
                if stripdevice then start := dotloc[dots] + 1;
                {not enough dots for an extension}
                end
              else if amperloc > 0 then
                begin
                if stripdevice then start := dotloc[dots] + 1;
                if stripext then
                  if amperloc < dotloc[2] then finish := dotloc[2] - 1;
                end
              else if dotloc[dots] >= arglen - 2 then
                begin
                if stripdevice then if dots = 2 then start := dotloc[1] + 1;
                if stripext then finish := dotloc[dots] - 1;
                end;
              end {dots <> 0} ;
            end {not '#'} ;
          end {with} ;
    end {vdosprune} ;


  begin {getfilename}
    if which = nil then
      begin
      which := SourceListHead;
      while which^.next <> nil do which := which^.next;
      stripdevice := true;
      stripext := true;
      end;

    with which^ do
      begin
      if hostopsys = vdos then
        vdosprune(which, stripdevice, stripext, start, finish)
      else
        begin {not vdos}
        start := 1;
        finish := arglen;
        if stripdevice then
          begin
          for i := 1 to arglen do
           {don't use char sets: too much data space used}
            case hostopsys of
              msdos:
                if (arg[i] = ':') or (arg[i] = '\') then start := i + 1;
              unix, apollo:
                if arg[i] = '/' then start := i + 1;
              otherwise
                if (arg[i] = ':') or (arg[i] = ']') then start := i + 1;
              end {case} ;
          end {stripdevice} ;

        if stripext then
          begin
          i := arglen;
          if hostopsys = vms then semi := 0;
          scanning := true;
          while scanning and (i >= start) do
          {don't use char sets: too much data space used}
            case hostopsys of
              msdos:
                if not ((arg[i] = ':') or (arg[i] = '\') or
                   (arg[i] = '.')) then
                  i := i - 1
                else scanning := false;
              unix, apollo:
                if not ((arg[i] = '/') or (arg[i] = '.')) then
                  i := i - 1
                else scanning := false;
              otherwise
                if not ((arg[i] = ':') or (arg[i] = ']') or
                   (arg[i] = '.')) then
                  begin
                  if hostopsys = vms then if arg[i] = ';' then semi := i;
                  i := i - 1;
                  end
                else scanning := false;
              end {case} ;
          if (i >= start) and (arg[i] = '.') then finish := i - 1
          else if hostopsys = vms then if semi > i then finish := semi - 1;
          end {stripext} ;
        end {not vdos} ;

      resultlength := finish - start + 1;
      i := 0;
      for j := start to finish do
        begin
        i := i + 1;
        result[i] := arg[j];
        end;
      for i := i + 1 to filenamelen do result[i] := ' ';
      end {with} ;
  end {getfilename} ;

procedure openenv;

{ Open environment file.
}

  begin {openenv}
    getfilename(envname, false, false, filename, filename_length);
      case hostopsys of
        vdos: reset(envirinfile, filename, '.en');
        otherwise reset(envirinfile, filename, '.env');
      end;
  end   {openenv};



procedure opentemp;

{ Open all temp files needed by the compiler.  These files are left open
  throughout the compilation of the program.  The macro routine 'doheap'
  is responsible for maximizing the heap space available for each pass,
  without destroying these temp files.

  'Cache' is the software virtual memory work file (used by analys,
  travrs, and code). 'Tempfileone' is used as scanner output (read by
  analys) and travrs output (read by code). 'Tempfiletwo' is used as
  analyzer output (read by travrs).  This minimizes the disk space
  required for compilation, and the amount of time needed for opening
  files between passes.
}


  begin {opentemp}
    if switcheverplus[test] then
      case hostopsys of
        vdos:
          begin
          rewrite(tempfileone, 'temp1.tm');
          rewrite(tempfiletwo, 'temp2.tm');
          end;
        otherwise
          begin
          rewrite(tempfileone, 'temp1.tmp');
          rewrite(tempfiletwo, 'temp2.tmp');
          end;
        end {case}
    else {not /test}
      case hostopsys of
        vdos:
          begin
          if not (scanalys and travcode) then rewrite(tempfileone);
          rewrite(tempfiletwo);
          end;
        unix, apollo:
          begin
          if not (scanalys and travcode) then 
            rewrite(tempfileone, 'temp1.tmp -temp');
          rewrite(tempfiletwo, 'temp2.tmp -temp');
          end;
        otherwise
          begin
          if not (scanalys and travcode) then
            rewrite(tempfileone, 'temp1.tmp/temp');
          rewrite(tempfiletwo, 'temp2.tmp/temp');
          end;
        end {case} ;

    if needcaching then
      case hostopsys of
        vdos:
          begin
          rewrite(cache);
          rewrite(stringfile);
          end;
        unix, apollo:
          begin
          rewrite(cache, 'cache.tmp -temp');
          rewrite(stringfile, 'string.tmp -temp');
          end;
        otherwise
          begin
          rewrite(cache, 'cache.tmp/seek/temp');
          rewrite(stringfile, 'string.tmp/seek/temp');
          end;
        end {case} ;

    if switcheverplus[symboltable] then
      begin
      getfilename(nil, true, true, filename, filename_length);
      if not newdebugger then
      case hostopsys of
        vdos: rewrite(debugfile, filename, '.st');
        msdos: rewrite(debugfile, filename, '.syp');
        unix, apollo: rewrite(debugfile, filename, '.sym');
        otherwise rewrite(debugfile, filename, '.sym/seek');
        end {case}
 else {newdebugger}
      case hostopsys of
        vdos: if not opendebugfile(filename, '.st') then {nothing};
        msdos: if not opendebugfile(filename, '.syp') then {nothing};
        otherwise if not opendebugfile(filename, '.sym') then {nothing};
        end {case} ;
      end;

    if switcheverplus[defineswitch] then
      begin
      getfilename(defname, false, false, filename, filename_length);
      case hostopsys of
        vdos: rewrite(enviroutfile, filename, '.en');
        otherwise rewrite(enviroutfile, filename, '.env');
        end {case} ;
      end;      

    if switcheverplus[environswitch] then openenv;

  end {opentemp} ;


procedure openl;

{ Open listing file.
}


  begin {openl}
    getfilename(listname, false, false, filename, filename_length);
    case hostopsys of
      vdos: rewrite(listing, filename, '.ls');
      msdos, unix, apollo: rewrite(listing, filename, '.lst');
      otherwise rewrite(listing, filename, '.lis');
      end {case} ;
  end {openl} ;

procedure closel;

{ Close listing and last source file.
}


  begin {closel}
    if not fakelist then close(listing);
    close(source[sourcelevel]);
    if not newdebugger and (lasterror = 0) and (switcheverplus[debugging] or
       switcheverplus[profiling]) then
      close(stmtfile);
  end {closel} ;


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
  Index must be in the range 0..n.
}

    var
      i: FilenameIndex;
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
}

    const
      DefExt = '.pas';
      DefLen = 4;

    var
      status: integer; {value returned by reset}
      i, j: FilenameIndex; {induction in strings}


    begin {FileExists}
      if addprefix then i := prefixlen
      else i := 0; {just start at the beginning}
      for j := 1 to DefLen do
        begin
        if i < filenamelen then
          begin
          i := i + 1;
          prefix[i] := DefExt[j];
          end;
        end;
      if hostopsys = vdos then i := i - 1;
      for j := i + 1 to filenamelen do prefix[j] := ' ';
      reset(Source[SourceLevel], filename, prefix, status);
      FileExists := (status >= 0);
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
        write('Can''t open include file ''');
        writeln(filename: length(filename), '''');
        exitst(exitstatus);
        end;
      end;
  end {opens} ;

procedure opennext;

{ Open the next source file at the current level.  If this is not the
  first input file, the old one is closed.
}

  var
    i: integer; {counts file names}
    p: FilenameListPtr; {used to find the next filename}


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
      case hostopsys of
        vdos: reset(source[sourcelevel], filename, '.pa');
        otherwise reset(source[sourcelevel], filename, '.pas');
        end {case} ;
      end;
  end {opennext} ;


procedure opena;

{ Open files for parser.
}


  begin {opena}
    if not scanalys
      then reset(tempfileone); {allow analys to read the output from scan}
    { init the analys-travrs symbol file }
    if switcheverplus[test] then
      case hostopsys of
        vdos: rewrite(locals, 'locals.tm')
        otherwise rewrite(locals, 'locals.tmp')
      end
    else rewrite(locals);
    if newdebugger and
      (switcheverplus[debugging] or switcheverplus[profiling]) then
      with statement_file do
        begin
        save_file := true;
        last_rec := 0;
        case hostopsys of
          vdos: rewrite(mapfile, filename, '.sm');
          unix, apollo: rewrite(mapfile, filename, '.smp');
          otherwise rewrite(mapfile, filename, '.smp/seek');
          end {case}
        end
  end {opena} ;


procedure opent;

{ Open files for tree builder/improver.
}


  begin {opent}
    reset(tempfiletwo); {allow travrs to read the output from analys}
    rewrite(tempfileone);
    reset(locals); {travrs must read local variables info from analys}
  end {opent} ;

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


procedure openc;

{ Open files for code generator.
}


  begin {openc}
    getoutputname;
    if not newdebugger and
      (switcheverplus[debugging] or switcheverplus[profiling]) then
      case hostopsys of
        vdos: rewrite(stmtfile, filename, '.sm');
        unix, apollo: rewrite(stmtfile, filename, '.smp');
        otherwise rewrite(stmtfile, filename, '.smp/seek');
        end {case} ;
    reset(tempfileone);
    if switcheverplus[outputmacro] then
      begin
      getfilename(macname, false, false, filename, filename_length);
      case hostopsys of
        vdos:
          case targetopsys of
            vms, rsx, rsts, rt: rewrite(macfile, filename, '.ma');
            unix: rewrite(macfile, filename, '.s');
            vdos: rewrite(macfile, filename, '.sa');
            msdos, apollo: rewrite(macfile, filename, '.as');
            end {case} ;
        otherwise
          case targetopsys of
            vms: rewrite(macfile, filename, '.mar');
            rsx, rsts, rt: rewrite(macfile, filename, '.mac');
            unix: rewrite(macfile, filename, '.s');
            vdos: rewrite(macfile, filename, '.sa');
            msdos, apollo: rewrite(macfile, filename, '.asm');
            end {case} ;
        end {case} ;
      end {/macro} ;

    if switcheverplus[outputobj] then
      begin
      getfilename(objname, false, false, filename, filename_length);
      case hostopsys of
        vdos:
          case targetopsys of
            vms, rsx, rsts, rt, msdos: rewrite(objfile, filename, '.ob');
            vdos: rewrite(objfile, filename, '.ro');
            unix: rewrite(objfile, filename, '.o');
            apollo: rewrite(objfile, filename, '.bi');
            end {case} ;
        vms:
          case targetopsys of
            vms: rewrite(objfile, filename, '.obj/nocr');
            rsx, rsts, rt: rewrite(objfile, filename, '.obj');
            unix: rewrite(objfile, filename, '.o/seek');
            vdos: rewrite(objfile, filename, '.ro');
            msdos: rewrite(objfile, filename, '.obj/seek');
            apollo: rewrite(objfile, filename, '.bin/seek');
            end;
        msdos:
          case targetopsys of
            vms: rewrite(binobjfile, filename, '.obj');
            rsx, rsts, rt: rewrite(objfile, filename, '.obj');
            unix: rewrite(objfile, filename, '.o/seek');
            vdos: rewrite(objfile, filename, '.ro');
            msdos: rewrite(objfile, filename, '.obj');
            apollo: rewrite(objfile, filename, '.bin/seek');
            end;
        unix, apollo:
          case targetopsys of
            vms: rewrite(binobjfile, filename, '.obj');
            rsx, rsts, rt: rewrite(objfile, filename, '.obj');
            unix: rewrite(objfile, filename, '.o');
            vdos: rewrite(objfile, filename, '.ro');
            msdos: rewrite(objfile, filename, '.obj');
            apollo: rewrite(objfile, filename, '.bin');
            end;
        otherwise
          case targetopsys of
            vms: rewrite(objfile, filename, '.obj');
            rsx, rsts, rt: rewrite(objfile, filename, '.obj');
            unix: rewrite(objfile, filename, '.o/seek');
            vdos: rewrite(objfile, filename, '.ro');
            msdos: rewrite(objfile, filename, '.obj/seek');
            apollo: rewrite(objfile, filename, '.bin/seek');
            end;
        end {case} ;
      end {/object} ;

    case targetopsys of
      unix, apollo:
        if unixtarget <> atxenix then
          begin
          rewrite(relfile);
          if switcheverplus[walkback] then rewrite(diagfile);
          end;
      vdos:
        if switcheverplus[test] then
          case hostopsys of
            vdos: rewrite(relfile, 'temp3.tm');
            unix: rewrite(relfile, 'temp3.tmp');
            otherwise rewrite(relfile, 'temp3.tmp/seek');
            end {case}
        else
          case hostopsys of
            vdos: rewrite(relfile);
            unix: rewrite(relfile, 'temp3.tmp -temp');
            otherwise rewrite(relfile, 'temp3.tmp/seek/temp');
            end {case} ;
      otherwise {do nothing} ;
      end {case} ;
  end {openc} ;


procedure closec;

{ Close object and macro files.
}


  begin {closec}
    close(locals); {close local variables file used in travrs}
    if switcheverplus[outputmacro] then close(macfile);
    if switcheverplus[outputobj] then close(objfile);
    if newdebugger and
       (switcheverplus[debugging] or switcheverplus[profiling]) then
      close(statement_file.mapfile);
    case targetopsys of
      unix, apollo:
        begin
        if unixtarget <> atxenix then
          begin
          close(relfile);
          if switcheverplus[walkback] then close(diagfile);
          end;
        end;
      vdos:
        close(relfile);
      end;
  end {closec} ;


procedure closes;

{ Close current output file.
}


  begin {closes}
    close(source[sourcelevel]);
  end {closes} ;


procedure closeall;

{ Close all source files.

  Split into an external routine for overlaying purposes (of course!).
}


  begin {closeall}
    if switcheverplus[defineswitch] then close(enviroutfile);
    while sourcelevel > 0 do
      begin
      close(source[sourcelevel]);
      sourcelevel := sourcelevel - 1;
      end;
    close(tempfileone);
    close(tempfiletwo);
    if needcaching then
      begin
      close(cache);
{      close(stringfile);
{ Do not close here because LIST needs it. }
      end;
  end {closeall} ;


procedure close_str;

{ Close the stringfile if needed.
}


  begin
    if needcaching then close(stringfile);
  end;


function stf_stmt
                 {lineno: integer; (line number for this statement)
                  filepos: integer (file pos for this statement)): integer} ;

{ Append a statement record and return its index.
}

  begin
    if newdebugger then
      with statement_file do
        begin
        mapfile^.pc := 0;
        mapfile^.lineno := lineno;
        mapfile^.filepos := filepos;
        put(mapfile);
        last_rec := last_rec + 1;
        stf_stmt := last_rec;
        end;
  end; {stf_stmt}


function stf_tell{:integer} ;

{ Return the next record to be written
}
  begin
    stf_tell := statement_file.last_rec + 1;
  end; {stf_tell}


procedure stf_pc
                {where: integer; (record to patch)
                 pc: integer (new pc)} ;

{ Put the pc value into a statement record.
}

  begin
  if (where > 0) and (where <= statement_file.last_rec) then
    with statement_file do
      begin
      seek(mapfile, where);
      mapfile^.pc := pc;
      put(mapfile);
      end
  end; {stf_pc}
