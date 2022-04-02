{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1980, 1981, 1982, 1983, 1985, 1986 by Oregon Software, Inc.
  All Rights Reserved.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Release version: 0045  Level: 1  Date: 21-Nov-1990 17:16:39
}

{  CSI type declarations}


  CharSet = packed set of char;

  ArgName = packed array[1..mArgName] of char;
  iArgName = 0..mArgName;
  ArgStatus = (RequiredArg, OptionalArg);
  ArgClass = (StringArg, FileArg, NumericArg, NullArg);

  ArgDef = record
    name: record
      txt: ArgName;
      min: iArgName;
      len: iArgName;
      end;
    position: integer;
    status: ArgStatus;
    class: ArgClass;
  end;
  ArgDefTable = array[ArgType] of ArgDef;
  SubArgDefTable = array[SubArgType] of ArgDef;

  iArgValue = 0..mArgValue;
  ArgValue = record
    txt: packed array[1..mArgValue] of char;
    len: iArgValue;
  end;

  FileSource = (ActualFile, DefaultFile, DisplayFile, TempFile);
  FileExt = packed array [1..ExtLen] of char;

