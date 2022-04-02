{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1980, 1981, 1982, 1983, 1985 by Oregon Software, Inc.
  All Rights Reserved.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Release version: 0045  Level: 1  Date: 21-Nov-1990 17:16:31
}

{  CSI standard procedures }

  function SkipToDelim
    (var arg: ArgValue; idx: iArgValue; delim: CharSet): iArgValue;
  label 1;
  var ch: char;
  begin                         { SkipToDelim }
    while idx <= arg.len do begin
      ch := arg.txt[idx];
      if ch in delim then goto 1;
      idx := idx + 1;
      if ch in ['(', '[', '<'] then begin
        if ch = '(' then ch := ')'
        else if ch = '[' then ch := ']'
        else if ch = '<' then ch := '>';
        idx := SkipToDelim(arg, idx, [ch]);
        end
      else if ch = '"' then
        repeat
          if idx > arg.len then goto 1;
          ch := arg.txt[idx];
          idx := idx + 1;
        until ch = '"';
      end;
    1:
    SkipToDelim := idx;
  end;                          { SkipToDelim }


  function FoldAlpha(ch: char): char;
  begin
    if ch in ['a'..'z'] then ch := chr(ord(ch) + (ord('A') - ord('a')));
    FoldAlpha := ch;
  end;


  procedure AppendToArg(var arg, piece: ArgValue; idx, len: iArgValue);
  begin
    while len > 0 do begin
      arg.len := arg.len + 1;
      arg.txt[arg.len] := piece.txt[idx];
      idx := idx + 1;
      len := len - 1;
      end;
  end;

  procedure CleanupArg(var arg: ArgValue);
  { Pack blanks out of argument, fold lower case to upper case. }
  var
    i, j: iArgValue;
    quoted: boolean;
  begin
    j := 0; quoted := false;
    for i := 1 to arg.len do begin
      if arg.txt[i] = '"' then quoted := not quoted;
      if quoted then begin
        j := j + 1; arg.txt[j] := arg.txt[i] end
      else if (arg.txt[i] <> ' ') then begin
        j := j + 1; arg.txt[j] := FoldAlpha(arg.txt[i]) end;
      end;
    arg.len := j;
    for i := arg.len + 1 to mArgValue do arg.txt[i] := ' ';
  end;

  procedure InitDef(var def: ArgDef; 
    name: ArgName; min: iArgName;
    position: integer;
    status: ArgStatus;
    class: ArgClass);
  var i, j: iArgName;
  begin
    def.name.len := 0; def.name.min := min; j := 0;
    for i := 1 to mArgName do begin
      j := j + 1; def.name.txt[j] := name[i];
      if name[i] <> ' ' then def.name.len := j;
      end;
    def.position := position;
    def.status := status;
    def.class := class;
  end;

