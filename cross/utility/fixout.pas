{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1983, 1985, 1986 by Oregon Software, Inc.
  All Rights Reserved.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Release version: 0045  Level: 1  Date: 21-Nov-1990 17:17:08
}

{CSI temporary output file routines}


procedure FixOutputArg(perm: ArgValue;
                       var temp: ArgValue);

  { Construct a temporary output filename, based on a permanent output
    filename. }

  { On RSX, RSTS, and RT-11 systems the temporary file name consists
    of the output file name with the extension 'tmp' substituted }

  const
    TmpExt = 'tmp';

  var
    i: iArgValue;


  begin
    CleanupArg(perm);
    temp.len := 0;
    AppendToArg(temp, perm, 1, SkipToDelim(perm, 1, ['.', ';']) - 1);
    temp.len := temp.len + 1;
    temp.txt[temp.len] := '.';
    for i := 1 to ExtLen do
      begin
      temp.len := temp.len + 1;
      temp.txt[temp.len] := TmpExt[i];
      end;
    CleanupArg(temp);
  end;

procedure FixBakOutput(var Perm: Argvalue; success: boolean);

{ Create a backup file of a Permanent file }

  const
    BakExt = 'BAK';
  var
    flg: integer;
    BackupName: ArgValue;
    f1, f2: text;
    ExtVar: FileExt;
    i: iArgValue;

  begin {FixBakOutput}
    for i := 1 to ExtLen do ExtVar[i] := BakExt[i];
    FixFileArg(Perm, DefaultFile, ExtVar, BackupName);
    reset(f1, Perm.txt, , flg);
    if flg <> -1 then
      if success then
        begin
        reset(f2, BackupName.txt, , flg);
        if flg <> -1 then delete(f2);
        rename(f1, BackupName.txt);
        end;
   end {FixBakOutput};

    
procedure FixTempOutput(temp, perm: ArgValue;
                        succ: boolean;
                        var status: boolean);

  { Turn a temporary output file into a permanent one, return error
    indication if it can't be done. }

  { On RSX, RSTS, and RT-11 systems, fixup consists of renaming the
    temporary file to the permanent file name. }

  label 999;

  var
    inp: packed file of char;
    flg: integer;


  begin
    status := false;
    reset(inp, temp.txt, , flg); {get a file variable}
    if flg <> - 1 then
      begin
      if succ then
        begin
        rename(inp, perm.txt);
        end
      else {no success}
        delete(inp);
      status := true;
      end;
  999:
  end;
