{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1980, 1981, 1982, 1985 by Oregon Software, Inc.
  All Rights Reserved.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Release version: 0045  Level: 1  Date: 21-Nov-1990 17:17:18
}

  procedure CnvNumericArg(arg: ArgValue; var Num: integer; var Err: boolean);
  { convert signed numeric command argument to integer }
  label 1;
  var
    i: iArgValue;
    MinusFound, DigitFound: boolean;
  begin
    Err := true;

    { scan sign }

    i := 1;
    if arg.txt[1] = '-' then begin
      MinusFound := true;
      i := 2;
      end
    else begin
      MinusFound := false;
      if arg.txt[1] = '+' then
        i := 2;
      end;

    { scan number }

    Num := 0;
    DigitFound := false;
    while i <= arg.len do begin
      if not (arg.txt[i] in ['0'..'9']) then goto 1;
      Num := Num * 10 - (ord(arg.txt[i]) - ord('0'));
      { check for overflow by looking for sign change }
      if Num > 0 then goto 1;
      i := i + 1;
      DigitFound := true;
      end;

    { Negate value if necessary. }

    if not MinusFound then begin
      Num := - Num;
      { check again for overflow }
      if Num < 0 then goto 1;
      end;

    if DigitFound then
      Err := false;
    1:;
  end;


