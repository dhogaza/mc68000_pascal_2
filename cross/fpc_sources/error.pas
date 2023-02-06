{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045 Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler error Reporting Routines

 Last modified by KRIS on 21-Nov-1990 15:35:15

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

unit error;

interface

uses config, hdr;

type
  errortype = (fatal, ioerror, nonfatal);
  message = packed array [1..1000] of char;

procedure error(class: errortype;
                errornumber, errormsglength: integer;
                var msg: message;
                var xfile: text;
                iostatus, userpc: integer;
                filenamelength: integer;
                var filename: message);

procedure panic;

implementation


procedure error(class: errortype;
                errornumber, errormsglength: integer;
                var msg: message;
                var xfile: text;
                iostatus, userpc: integer;
                filenamelength: integer;
                var filename: message);

{ Special error handler for Pascal-2.  The primary reason for
  providing our own error handler is to minimize the amount of
  useless data printed to the user if the compiler can not open
  a file.  The user does not need the internal compiler address
  at which the error occured.
}

  var
    i: integer;


  begin
    writeln;
    write(msg: errormsglength {+ 1});
    if class = ioerror then
      begin
      write(' "');
      for i := 1 to filenamelength do
        if filename[i] > ' ' then write(filename[i]);
      write('"');
      end
    else write(' ', userpc: - 1);
    writeln;
  end;


procedure panic;

{ Panic exit from analys, code or travrs.
  Assumes current procedure reference is stored in 'blockref', 
  and that string file is still open.
}

  var
    i: integer; {induction var for copy}
    nextstringfile: 0..diskbufsize; {index into buffer}
    nextstringblock: 0..maxint; {block in the string file}
    stringindex: integer; {index into stringfile}


  begin
    case abortmsg of
      wrongversionenv:
        write('Recompile environment file with this compiler. Error');
      outofmem: write('Out of memory');
      undeltemps: write('Undeleted temps');
      muchcode: write('Too much object code');
      manylabels: write('Too many labels');
      manyplabels: write('Too many Pascal labels');
      manytemps: write('Code too complex');
      manynodes: write('Too many nodes');
      builderror: ;
      manykeys: write('Too many keys');
      walkerror: ;
      interntemp: write('Internal temp error');
      badadjust: write('Bad adjustoffset value');
      inconsistent: ;
      manyexterns: write('Too many external references');
      badrelfile: write('Bad relocation file');
      manynonlocals: write('Too many non-locals');
      perposdump: write('Block too long');
      end;

    if abortmsg in [undeltemps, builderror, walkerror, interntemp, badadjust,
                    inconsistent, badrelfile]
    then
      begin
      writeln;
      write('Internal compiler error');
      end;

    if current_line <> 0 then
      write(' on line ', current_line:1,', stmt ', current_stmt:1, ',');

    if blockref = 0 then write(' in main program')
    else
      begin
      write(' in procedure ');
      for i := 1 to proctable[blockref].charlen do
        begin
        stringindex := i + stringfilecount + proctable[blockref].charindex -
                       2;
        nextstringblock := stringindex div (diskbufsize + 1) + 1;
        if stringblkptrtbl[nextstringblock] = nil then
          new(stringblkptrtbl[nextstringblock]);
        stringblkptr := stringblkptrtbl[nextstringblock];
        nextstringfile := stringindex mod (diskbufsize + 1);
        write(chr(stringblkptr^[nextstringfile]));
        end;
      end;
    writeln(' during Pascal-2 compilation');

    if not switcheverplus[test] then halt();
  end {panic} ;

end.
