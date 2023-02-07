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

  Pascal-2 SCAN Output Dumper (Debugging Tool)

 Last modified by KRIS on 21-Nov-1990 15:35:53
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

unit sd;

interface

uses config, hdr, scan;

procedure sd;

implementation

var
  tokenbufindex: 0..diskbufsize;
  listingname: packed array [1..255] of char;

procedure printreal(r: realarray);

  type
    hexdigit = 0..15;

  var
    { this fudges a real into bytes.  The constant "32" is }
    { simply a large enough number to include all probable systems. }
    fudge:
      record
        case integer of
          1: (rl: realarray);
          2: (rr: real);
          3: (byte: packed array [1..32] of hostfilebyte);
      end;
    j: 1..32; {induction var}


  procedure writehex(i: hexdigit);


    begin
      if i < 10 then write(chr(i + ord('0')))
      else write(chr(i - 10 + ord('A')));
    end;


  begin
    fudge.rl := r;
    write(fudge.rr: 21, ' (');
    for j := 1 to sizeof(realarray) do
      begin
      writehex(fudge.byte[j] div 16);
      writehex(fudge.byte[j] mod 16);
      end;
    write(')');
  end;


procedure sd;
begin {sd}

  with nexttoken do
    begin
    write(baseline: 5, line: 5, left: 4, right: 6, fileindex: 5);
    write(' ': 2);
    case token of
      programsym: write('PROGRAM');
      labelsym: write('LABEL');
      constsym: write('CONST');
      typesym: write('TYPE');
      varsym: write('VAR');
      proceduresym: write('PROCEDURE');
      functionsym: write('FUNCTION');
      otherwisesym: write('OTHERWISE');
      uparrow: write('^');
      arraysym: write('ARRAY');
      filesym: write('FILE');
      setsym: write('SET');
      packedsym: write('PACKED');
      recordsym: write('RECORD');
      stringsym: write('STRING');
      univsym: write('UNIV');
      beginsym: write('BEGIN');
      ifsym: write('IF');
      casesym: write('CASE');
      whilesym: write('WHILE');
      repeatsym: write('REPEAT');
      forsym: write('FOR');
      withsym: write('WITH');
      gotosym: write('GOTO');
      usesym: write('USE');
      definesym: write('DEFINE');
      sharedsym: write('SHARED');
      eql: write('=');
      lss: write('<');
      gtr: write('>');
      neq: write('<>');
      leq: write('<=');
      geq: write('>=');
      insym: write('IN');
      plus: write('+');
      minus: write('-');
      orsym: write('OR');
      star: write('*');
      slash: write('/');
      divsym: write('DIV');
      modsym: write('MOD');
      andsym: write('AND');
      ofsym: write('OF');
      endsym: write('END');
      elsesym: write('ELSE');
      thensym: write('THEN');
      dosym: write('DO');
      untilsym: write('UNTIL');
      tosym: write('TO');
      downtosym: write('DOWNTO');
      notsym: write('NOT');
      nilsym: write('NIL');
      colon: write(':');
      dot: write('.');
      dotdot: write('..');
      comma: write(',');
      semicolon: write(';');
      becomes: write(':=');
      lpar: write('(');
      rpar: write(')');
      lbrack: write('[');
      rbrack: write(']');
      intconst: write('INTCONST VALUE:', intvalue);
      realconst:
        begin
        write('REALCONST VALUE: ');
        printreal(realvalue);
        end;
      dblrealconst:
        begin
        write('DBLREALCONST VALUE: ');
        printreal(realvalue);
        end;
      charconst: write('CHARCONST VALUE:', intvalue: 3);
      stringconst: write('STRINGCONST POS:', pos,' LEN:', len);
      ident: write('IDENT KEY:', key);
      eofsym: write('EOF');
      otherwise write('unknown token - ', ord(token): 1);
      end;
    writeln;
    end {WITH} ;

end;{sd}
end.
