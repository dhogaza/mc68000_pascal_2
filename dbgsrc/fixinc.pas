{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1980, 1981, 1982, 1983, 1985, 1986 by Oregon Software, Inc.
  All Rights Reserved.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Release version: 0045  Level: 1  Date: 21-Nov-1990 17:17:13
}

{ CSI include file adjustment routines }


procedure FixFileInclude(arg, prefix: ArgValue;
                         ext: FileExt;
                         var spec: ArgValue);

{ Construct a file name specification, based on an included filename,
  a prefix string, and a default extension. }

  var
    first, next, last, i: iArgValue;
    done: boolean;


  begin

    { Locate start and end of name. }

    CleanupArg(arg);
    next := SkipToDelim(arg, 1, ['.', ';']);
    first := next;
    done := false;
    if MSDOSopsys then
      repeat
        if first = 1 then
          done := true
        else if not (arg.txt[first - 1] in
                ['A'..'Z', 'a'..'z', '0'..'9', '$', '_', '#', '&', '@', '!',
                '%', '(', ')', '-', '{', '}']) then
          done := true
        else
          first := first - 1;
      until done
    else {not MSDOS}
      repeat
        if first = 1 then
          done := true
        else if not (arg.txt[first - 1] in
                ['A'..'Z', 'a'..'z', '0'..'9', '$', '_']) then
          done := true
        else
          first := first - 1;
      until done;

    { If there is no device/directory specification, copy prefix. }

    spec.len := 0;
    if first = 1 then
      begin
      AppendToArg(spec, prefix, 1, prefix.len);
      if MSDOSopsys then
      { Make sure prefix and filename are separated by a backslash }
        if spec.txt[spec.len] <> '\' then
          begin
          spec.len := spec.len + 1;
          spec.txt[spec.len] := '\';
          end;
      end;

    { Copy first part of file name. }

    AppendToArg(spec, arg, 1, next - 1);

    { If extension is missing, copy default extension. }

    if arg.txt[next] <> '.' then
      begin
      spec.len := spec.len + 1;
      spec.txt[spec.len] := '.';
      for i := 1 to ExtLen do
        begin
        spec.len := spec.len + 1;
        spec.txt[spec.len] := ext[i];
        end;
      end;

    { Copy remainder of file name. }

    AppendToArg(spec, arg, next, arg.len - next + 1);
    CleanupArg(spec);
  end;
