{[l-,b+]}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: all
  System: all

  Debugger executive initialization

 Last modified by KRIS on 26-Nov-1990 13:45:55
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}


{******************************************************************************

      Write version information

******************************************************************************}

  procedure writeversion;
    {Write out debugger version information}
    begin
    writeln('PDB version ',version);
    end;

{******************************************************************************

      Write copyright header

******************************************************************************}


  procedure doheader(name: packed array[low..high: integer] of char);
    {Write out the copyright header}

    var
      nam: packed array [1..msymbolname] of char;
      i,j: integer;

    begin  {doheader}
      i := low;
      while (i <= high) and (name[i] = ' ') and (i <= msymbolname) do
        i := i + 1;
      j := 0;
      while (i <= high) and (name[i] <> ' ') and (i <= msymbolname) do
        begin
        j := j + 1;
        nam[j] := name[i];
        i := i + 1;
        end;
      writeln(out);
      writeln(out, 'Pascal-2 (tm) for ', systemtitle, '    Version ',
            version);
      writeln(out, 'Copyright (c) 1989 by Oregon Software, Inc  ',
            'ALL RIGHTS RESERVED');
      writeln(out);
      writeln(out);
      write(out, 'Debugging program ');
      writeln(out, nam:j);
      writeln(out);
      writeln(out);
    end;  {doheader}


{******************************************************************************

      Module list initialization

******************************************************************************}

%include csipro;
%include fixarg;
%include fixinc;


procedure buildmdllist(unitlist: unitptr);
    { Construct a linked list of module descriptors based on the units list
      passed in from the target supervisor.  For now, only the code address
      boundaries, the base of its own data and the name are known. }

  var
    unit: unitptr;
    mdl: mdl_pointer;
    idx: debughashindex;
    i: isymbolname;


  begin {buildmdllist}
    mdl_list := nil;
    main_mdl := nil;
    unit := unitlist;
    while unit <> nil do
      begin
      new(mdl);
      with mdl^ do
        begin
        code_start := unit^.code_start;
        code_end := unit^.code_end;
        if segmented then
          begin
          codesegment := unit^.codesegment;
          datasegment := unit^.datasegment;
          { this only work when stacksegment = datasegment 386}
          stacksegment := datasegment;
          end
        else picoffset := unit^.picoffset;
        for i := 1 to unitnamelength do nam[i] := unit^.nam[i];
        for i := unitnamelength + 1 to msymbolname do nam[i] := ' ';
        data_base := unit^.data_base;
	externaladdress := unit^.externaladdress;
        info := unloaded;
        loaded := false;
        proctree := nil;
        proctreeinit := false;
        base_frame := nil;
        if unit^.doublereal then realsize := 8 else realsize := 4;
	if unit^.shortinteger then intsize := 2 else intsize := 4;
        for idx := 0 to debughashtablesize do firstoccur[idx] := 0;
        next := mdl_list;
        mdl_list := mdl;
        if unit^.mainunit then main_mdl := mdl;
        casesensitive := unit^.casesensitive;
        fpcoprocessor := unit^.fpcoprocessor;
        everfpcoprocessor := everfpcoprocessor or fpcoprocessor;
        filenames := nil;
        end;
      unit := unit^.next;
      end;
  end; {buildmdllist}


procedure findfilenames;
    {Search the includes list for auxilliary files for each module
     that currently lacks such files.  If the files are found, then
     save the complete name.  All files for a particular module are
     expected to reside in the same directory.}

  var
    i: isymbolname; {induction variable}
    mdl: mdl_pointer; {current module record being examined}
    done: boolean;
    tmparg, spec: argvalue; {holding variables for file names}
    p: pargvaluelist; {current include directory}
    flg: integer; {status return from reset}
    symfile: file of debugrecord; { sybol table file }
    mapfile: file of stmtrecord; {statement map file }
    listfile: text; { listing file }


  begin {findfilenames}
    mdl := mdl_list;

    while (mdl <> nil) do
      begin

      if (mdl^.info = unloaded) then
          {Try the current directory if this is the first search}
        begin
        mdl^.info := noinfo;
        reset(symfile, mdl^.nam, symext, flg);
        if flg <> - 1 then
          begin
          close(symfile);
          reset(mapfile, mdl^.nam, mapext, flg);
          if flg <> - 1 then
            begin
            close(mapfile);
            mdl^.info := syminfo;
            reset(listfile, mdl^.nam, listext, flg);
            if flg <> - 1 then
              begin
              close(listfile);
              mdl^.info := allinfo;
              end; {listfile present}
            end; {mapfile present}
          end; {symfile present}
        end {info=unloaded} ;

      if (mdl^.info < syminfo) and (includes <> nil) then
              {if not in the current directory and an includes list exist,
               then search the included directories}

        begin
        p := includes;
        new(mdl^.filenames);
        done := false;
        mdl^.info := noinfo;
        for i := 1 to msymbolname do
          begin
          tmparg.txt[i] := mdl^.nam[i];
          if tmparg.txt[i] <> ' ' then tmparg.len := i;
          end;

        while (p <> nil) and not done do
        {Run down the includes list}
          begin
          fixfileinclude(tmparg, p^.arg, csisymext, spec);
          reset(symfile, spec.txt, , flg);
          if flg <> - 1 then
            begin
            close(symfile);
            for i := 1 to spec.len do
              mdl^.filenames^.symfilename[i] := spec.txt[i];
            for i := spec.len + 1 to stringsize do
              mdl^.filenames^.symfilename[i] := ' ';
            fixfileinclude(tmparg, p^.arg, csimapext, spec);
            reset(mapfile, spec.txt, , flg);
            if flg <> - 1 then
              begin
              close(mapfile);
              mdl^.info := syminfo;
              for i := 1 to spec.len do
                mdl^.filenames^.mapfilename[i] := spec.txt[i];
              for i := spec.len + 1 to stringsize do
                mdl^.filenames^.mapfilename[i] := ' ';
              fixfileinclude(tmparg, p^.arg, csilistext, spec);
              reset(listfile, spec.txt, , flg);
              if flg <> - 1 then
                begin
                close(listfile);
                mdl^.info := allinfo;
                for i := 1 to spec.len do
                  mdl^.filenames^.listfilename[i] := spec.txt[i];
                for i := spec.len + 1 to stringsize do
                  mdl^.filenames^.listfilename[i] := ' ';
                end;
              done := true;
              end; {mapfile present}
            end; {symfile present}

          p := p^.next;
          end; {while p<>nil and not done}

        if not done then
        { If no files found then zero filenames}
          begin
          dispose(mdl^.filenames);
          mdl^.filenames := nil;
          end;

        end; {info<syminfo and p<>nil}

      mdl := mdl^.next;
      end {while mdl<>nil} ;

  end; {findfilenames}



{*************************************************************************

     ------ Debugger Executive Initialization

*************************************************************************}


procedure initexecutive;

  var
    units: unitptr; {units lists and main unit pointer retrieved
                               from target supervisor}


  begin {initexecutive}

    initstdinout;

    { Initialize Global Variables. }

    execargs.len := 0;
    mdl_list := nil;
    execname := nil;

    terminated := false;
    testflg := false;
    paused := true;
    maxfiles := d$maxfilesopen;
    everfpcoprocessor := false;

    { Retrieve command line arguments if the debugger runs as an independent 
      program. }

    if separateprocess or remote then d$csi;

    catchinterrupts;

    { Call the target initailization, and use the returned units list to build
      the modules list. }

    d$inittarget(units);
    buildmdllist(units);
    findfilenames;

    if separateprocess or remote then doheader(execname^.txt)
    else doheader(main_mdl^.nam);

    if separateprocess or remote then
      if d$beingdebugged then prompt := '>';

  end; {initexecutive}
