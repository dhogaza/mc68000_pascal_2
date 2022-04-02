{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1980, 1981, 1982, 1983, 1985, 1986 by Oregon Software, Inc.
  All Rights Reserved.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Release version: 0045  Level: 1  Date: 21-Nov-1990 17:16:44
}

{ CSI argument definitions }


procedure FixFileArg(arg: ArgValue;
                     src: FileSource;
                     ext: FileExt;
                     var spec: ArgValue);

{ Construct a file name specification, based on a filename argument,
  and a default extension. }

  const
    TempExt = 'TMP'; { extension applied to temp files }

  var
    first, next, last, i: iArgValue;
    done: boolean;


  function missingExt(arg: ArgValue;
                      DotLocation: iArgValue): boolean;


    begin
      if (DotLocation < (ExtLen - 1)) or
         (arg.txt[pred(DotLocation)] = ':') then
        missingExt := false
      else
        missingExt := (arg.txt[DotLocation] <> '.');
    end; {missingExt}


  begin

    { Locate start and end of name. }
    CleanupArg(arg);
    next := SkipToDelim(arg, 1, ['.', ';']);
    if (src = ActualFile) or (src = TempFile) then
      first := 1
    else
      begin
      first := next;
      done := false;
      if MSDOSopsys then
        repeat
          if first = 1 then
            done := true
          else if not (arg.txt[first - 1] in
                  ['A'..'Z', 'a'..'z', '0'..'9', '$', '_', '#', '&', '@',
                  '!', '%', '(', ')', '-', '{', '}']) then
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
      end;

    { Copy first part of file name. }

    spec.len := 0;
    AppendToArg(spec, arg, first, next - first);

    { If source is DefaultFile or extension is missing, use default extension.
      If source is TempFile then add TempExt.
    }
    if (src = DefaultFile) or (src = TempFile) or missingExt(arg, next) then
      begin
      if src = TempFile then
        for i := 1 to ExtLen do
          ext[i] := TempExt[i];
      spec.len := spec.len + 1;
      spec.txt[spec.len] := '.';
      for i := 1 to ExtLen do
        begin
        spec.len := spec.len + 1;
        spec.txt[spec.len] := ext[i];
        end;
      end
    else {copy rest of file name}
      AppendToArg(spec, arg, next, arg.len - next + 1);
    CleanupArg(spec);
  end;


