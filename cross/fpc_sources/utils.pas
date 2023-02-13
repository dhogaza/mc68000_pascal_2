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

  Pascal-2 Compiler External Procedure Declarations

 Last modified by KRIS on 21-Nov-1990 15:16:36
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

unit utils;

interface

uses config, hdr, error;

function min(i, j: integer): integer;

{ Returns the lesser of its two arguments.
}

function max(i, j: integer): integer;

{ Returns the greater of its two arguments.
}

procedure compilerabort(msg: abortwarning {why we are compileraborting} );

{ 'panic' exit -- prints message, procedure name and splits. Global var
  'blockref' is assumed to contain the block reference of the
  current procedure.  Call only from analys, travrs or code.
}

procedure getfilename(which: FilenameListPtr; {file desired}
                      stripdevice: boolean; {want no device/directory field}
                      stripext: boolean; {want no extension field}
                      var result: FilenameBuf; {resulting file name}
                      var resultlength: FilenameIndex {length of file name} );
  
implementation

function min(i, j: integer): integer;
  begin
    if i < j then min := i
    else min := j
  end;

function max(i, j: integer): integer;
  begin
    if i > j then max := i
    else max := j
  end;

procedure compilerabort(msg: abortwarning);

{ compilerabort in a panic, printing a message and killing the compile
}

  begin
    abortmsg := msg;
    panic;
  end; {compilerabort}

procedure getfilename(which: FilenameListPtr; {file desired}
                      stripdevice: boolean; {want no device/directory field}
                      stripext: boolean; {want no extension field}
                      var result: FilenameBuf; {resulting file name}
                      var resultlength: FilenameIndex {length of file name} );

{ Get a file name with or without the device or extension fields.
}

  var
    start, finish: FilenameIndex; {limits of file name}
    i, j: FilenameIndex; {induction on file name}
    scanning: boolean; {loop control}
    semi: FilenameIndex; {where the semicolon is, if any}

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


end.
