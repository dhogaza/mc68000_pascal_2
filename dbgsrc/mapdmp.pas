{[r+,b+,l-]}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Format and print the contents of the Debugger map file produced by
the compiler.  The listing file must be present so that the source
lines can be printed.

 Last modified by KRIS on 26-Nov-1990 13:53:08
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

{$double}
type 
  double = real;

%INCLUDE arccon;

%INCLUDE hstcon;

%INCLUDE dbghdr;

  VAR
    i, j: integer;
    f: FILE OF stmtrecord;
    l: text;
    filename: PACKED ARRAY [1..80] OF char;
    ch, lastch: char;


  PROCEDURE setpos(VAR f: text;
                   a, b: integer);
    EXTERNAL;


  BEGIN
    writeln;
    writeln('Debugger map file dumper');
    writeln;
    write('Map file? ');
    readln(filename);
    reset(f, filename, mapext);
    reset(l, filename, listext);
    write('Output file? ');
    readln(filename);
    if filename[1]<>' ' then rewrite(output, filename, mapdmpext);
    i := 1;
    writeln;
    writeln;
    writeln('Record  Line  Stmt       PC  File pos  Text line');
    writeln;
    WHILE NOT eof(f) DO
      WITH f^ DO
        BEGIN
        if typ = plabrec then
          write(i: 6, lineno: 6, recordnr: 6, ' ', pc: - 8, ' ', filepos1: 5,
                filepos2: 4, '    PLABREC')
        else if exit then
          write(i: 6, lineno: 6, proclinenr: 6, ' ', pc: - 8, ' ', filepos1: 5,
                filepos2: 4, '    EXIT')
        else 
          begin
          write(i: 6, lineno: 6, proclinenr: 6, ' ', pc: - 8, ' ', filepos1: 5,
                filepos2: 4);
          setpos(l, filepos1, filepos2);
          write(': ');
          lastch := ' ';
          FOR j := 1 TO 10 DO read(l, ch);
          IF eof(l) THEN write('???')
          ELSE
            WHILE NOT eoln(l) DO
              BEGIN
              read(l, ch);
              IF ord(ch) = 9 THEN ch := ' ';
              IF (ch <> ' ') OR (lastch <> ' ') THEN write(ch);
              lastch := ch;
              END;
          end;
        writeln;
        i := i + 1;
        get(f);
        END;
  END.
