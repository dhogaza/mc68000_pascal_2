{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1983, 1984, 1985, 1986 by Oregon Software, Inc.
  All Rights Reserved.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Release version: 0045  Level: 1  Date: 21-Nov-1990
}

{CSI procedures to get command string}


procedure GMCR;
  external; {gets RSX command line}


procedure p_GetCmd(var text:packed array[l1..h1:integer] of char;
		var length:integer);external; {get VERSAdos command}


procedure GetLine(var line: ArgValue;
                  var len: integer);
 { RT-11 procedure to get a command line. }
  external;


procedure p_get_foreign(prompt: packed array [l1..h1: integer] of char;
                        var txt: packed array [l2..h2: integer] of char;
                        var length: integer);
 { Get command line from VMS. }
  external;


procedure p_getcmdline(var txt: packed array [l..h: shortint] of char;
                       var length: shortint);
 { Get Command line from MSDOS }
  external;

var
  RT11Kludge: ArgValue; { temporary storage (out from under USR) }



procedure GetCS(ArgDefs: ArgDefTable;
                procedure ProcessArg(arg: ArgValue;
                                     typ: ArgType));

  label
    1;

  var
    Cmd: ArgValue;
    CmdIdx, NextCmdIdx: iArgValue;
    pos: integer;
    typ: ArgType;
    vmslen: integer;
    msdoslen: shortint;


  procedure GetCmdString;

    var
      i, j: iArgValue;
      n: integer;
      quoted: boolean;

      {RSTS communication area }
      status_bits origin 46B: integer; {ccl entry flag is high bit}
      run_entry: boolean; {True if status_bits >= 0 }
      entry_point origin 440B: integer; {entry line is low 15 bits}
      status_word origin 442B: integer; {job status}
      cc_len_char origin 460B: char;
      cc_len: integer;
      cc origin 461B: packed array [1..127] of char; {data buffer}
      c: integer; {induction var}


    procedure ReadRstsCmd;

      begin {ReadRstsCmd}
        write(output, csiprompt); {prompt for data}
        Cmd.len := 0;
        while not eoln(input) do
          begin
          if Cmd.len < mArgValue - 2 then
            begin
            Cmd.len := Cmd.len + 1;
            read(input, Cmd.txt[Cmd.len]);
            end
          else
            get(input);
          end;
        readln(input);
      end; {ReadRstsCmd}



    begin { GetCmdStr }

      { Prompt for command string. }

      if RSTSopsys then
        begin
        { Determine how we got started, via CCL or by a RUN }
        status_word := 4000B;
        emt(255);
        emt(46);
        run_entry := (status_bits >= 0);

        if run_entry {entered by rsts RUN command} then
          begin
          ReadRstsCmd;
          end
        else
          begin {entered by RSTS CCL}
          cc_len := ord(cc_len_char);
          c := 1;
          while (c <= cc_len) and not (cc[c] in [' ', '/']) do
            c := c + 1;
          if c > cc_len then
            begin {treat ccl's with no arguments like RUN}
            run_entry := true;
            ReadRstsCmd;
            end
          else
            begin {load the command line}
            cmd.len := 0;
            while cmd.len < cc_len - c do
              begin
              cmd.len := cmd.len + 1;
              cmd.txt[cmd.len] := cc[c + cmd.len];
              end;
            end;
          end;
        end
      else if VMSopsys then
        begin
        p_get_foreign(CSIprompt, cmd.txt, vmslen);
        if vmslen > (mArgValue - 2) then
          cmd.len := mArgValue - 2
        else
          cmd.len := vmslen;
        end
      else if MSDOSopsys then
        begin
        p_getcmdline(cmd.txt, msdoslen);
        if msdoslen > (mArgValue - 2) then
          cmd.len := mArgValue - 2
        else
          cmd.len := msdoslen;
        end
      else if PROTKopsys or RSXopsys then
        begin
        gmcr;
        if (not PROTKopsys) and (input^ <> ' ') then
          begin
          repeat
            get(input)
          until (input^ in [' ', '/']); {look for initial switches}
          while not eoln(input) and (input^ = ' ') do
            get(input);
          end;
        if input^ = ' ' then
          write(CSIprompt);
        if eoln(input) then
          readln(input);
        end;

      { Get command string. }

      if RT11opsys then
        begin
        GetLine(RT11Kludge, n);
        RT11Kludge.len := n;
        Cmd := RT11Kludge;
        end
      else if VDOSopsys then begin
        p_getcmd(cmd.txt, vmslen);
        cmd.len := vmslen;
        end
      else if RSXopsys then
        begin
        Cmd.len := 0;
        while not eoln(input) do
          begin
          if Cmd.len < mArgValue - 2 then
            begin
            Cmd.len := Cmd.len + 1;
            read(input, Cmd.txt[Cmd.len]);
            end
          else
            get(input);
          end;
        readln(input);
        end;
    end; { GetCmdStr }



  procedure ReturnSimpleArg(idx, len: iArgValue;
                            typ: ArgType);
    { Return the label (name) of the argument.  That is, if a
      switch is /DEBUG return "DEBUG". }

    var
      arg: ArgValue;

    begin
      arg.len := 0;
      AppendToArg(arg, Cmd, idx, len);
      CleanupArg(arg);
      ProcessArg(arg, typ);
      ArgDefs[typ].status := OptionalArg;
    end;


  procedure ReturnStringArg(idx, len: iArgValue;
                            typ: ArgType);
    { Return the value of a string argument.  Quotation marks are
      escaped with an extra quotation mark: "".  For example, if there
      is a switch /OUTPUT=file.ext return "FILE.EXT". }

    var
      arg: ArgValue;
      n, i: iArgValue;
      quoted: boolean;

    begin
      arg.len := 0;
      AppendToArg(arg, Cmd, idx, len);
      CleanupArg(arg);
      i := 0;
      n := arg.len;
      arg.len := 0;
      quoted := false;
      while i < n do
        begin
        i := i + 1;
        if arg.txt[i] <> '"' then
          begin
          arg.len := arg.len + 1;
          arg.txt[arg.len] := arg.txt[i]
          end
        else
          begin
          if not quoted then
            quoted := true
          else
            begin
            if arg.txt[i + 1] <> '"' then
              quoted := false
            else
              begin
              arg.len := arg.len + 1;
              arg.txt[arg.len] := '"'
              end;
            end;
          end;
        end;
      for i := arg.len + 1 to mArgValue do
        arg.txt[i] := ' ';
      ProcessArg(arg, typ);
      ArgDefs[typ].status := OptionalArg;
    end;



  procedure PositionArg(pos: integer);

    var
      class: ArgClass;
      typ: ArgType;

    begin { PositionArg }

      { Look up argument in argument definition table. }

      if NextCmdIdx > CmdIdx then
        class := FileArg
      else
        class := NullArg;
      typ := MalformedArg;
      repeat
        typ := pred(typ);
      until ((ArgDefs[typ].position = pos) and
            (ArgDefs[typ].class = class)) or (typ = UnknownArg);

      { Return argument to caller. }

      case ArgDefs[typ].class of
        FileArg, StringArg:
          ReturnStringArg(CmdIdx, NextCmdIdx - CmdIdx, typ);
        otherwise
          ReturnSimpleArg(CmdIdx, NextCmdIdx - CmdIdx, typ);
        end;
    end; { PositionArg }



  procedure LabelArg;

    label
      1;

    var
      typ: ArgType;
      j: iArgValue;

    begin { LabelArg }

      { Look up argument label in argument definition table. }

      typ := MalformedArg;
      repeat
        typ := pred(typ);
        if typ = UnknownArg then
          goto 1;
        j := 1;
        while (j <= ArgDefs[typ].name.len) and (FoldAlpha(ArgDefs[typ].name.
                                                          txt[j]) =
              FoldAlpha(Cmd.txt[CmdIdx + j])) do
          j := j + 1;
      until (j > ArgDefs[typ].name.min) and
            not (Cmd.txt[CmdIdx + j] in ['a'..'z', 'A'..'Z', '0'..'9']);

      { Check for badly formed argument. }

      j := j + CmdIdx;
      if ArgDefs[typ].class = NullArg then
        begin
        if j <> NextCmdIdx then
          typ := MalformedArg;
        end
      else
        begin
        if Cmd.txt[j] in [':', '='] then
          NextCmdIdx := SkipToDelim(Cmd, j + 1, [' ', ',', '=', '/'])
        else
          typ := MalformedArg;
        end;

      { Return argument to caller. }

    1:
      case ArgDefs[typ].class of
        NullArg:
          ReturnSimpleArg(CmdIdx, NextCmdIdx - CmdIdx, typ);
        FileArg, StringArg:
          ReturnStringArg(j + 1, NextCmdIdx - j - 1, typ);
        otherwise
          ReturnSimpleArg(j + 1, NextCmdIdx - j - 1, typ);
        end;

      { Blank out this argument so we don't get confused later }
      for j := CmdIdx to NextCmdIdx - 1 do
        Cmd.txt[j] := ' ';
    end; { LabelArg }



  begin { GetCS }

    { Get command string, note starting argument position }

    GetCmdString;

    { Process any switch arguments, blanking as we go }

    NextCmdIdx := 0;
    while NextCmdIdx < Cmd.len do
      begin
      CmdIdx := NextCmdIdx + 1;
      NextCmdIdx := SkipToDelim(Cmd, CmdIdx, [' ', '/']);

      while Cmd.txt[NextCmdIdx] = '/' do
        begin
        CmdIdx := NextCmdIdx;
        NextCmdIdx := SkipToDelim(Cmd, CmdIdx + 1, [' ', ',', ':', '=', '/']);
        LabelArg;
        end;
      end;
    CleanupArg(Cmd); {pack out blanks}

    { Process the file arguments }

    if Cmd.txt[SkipToDelim(Cmd, 1, ['='])] = '=' then
      pos := 2
    else
      pos := 1;

    NextCmdIdx := 0;
    while NextCmdIdx < Cmd.len do
      begin
      CmdIdx := NextCmdIdx + 1;
      NextCmdIdx := SkipToDelim(Cmd, CmdIdx, [' ', ',', '=']);

      PositionArg(pos);

      { Update command string position, check for extra equal-sign. }

      if pos <> 1 then
        pos := pos + 1;
      if Cmd.txt[NextCmdIdx] = '=' then
        begin
        if pos = 1 then
          begin
          ReturnSimpleArg(NextCmdIdx, Cmd.len - NextCmdIdx + 1, MalformedArg);
          goto 1;
          end;
        pos := 1;
        end;
      end;

    { Check whether any required arguments are missing. }

  1:
    for typ := UnknownArg to MissingArg do
      with ArgDefs[typ] do
        begin
        if status = RequiredArg then
          begin
          if position <> 0 then
            begin
            for CmdIdx := 1 to name.len do
              begin
              if name.txt[CmdIdx] = '_' then
                Cmd.txt[CmdIdx] := ' '
              else
                Cmd.txt[CmdIdx] := name.txt[CmdIdx];
              end;
            Cmd.len := name.len;
            end
          else
            begin
            Cmd.txt[1] := '/';
            Cmd.len := name.len + 1;
            for CmdIdx := name.len downto 1 do
              begin
              if name.txt[CmdIdx] = '_' then
                Cmd.len := CmdIdx;
              Cmd.txt[CmdIdx + 1] := FoldAlpha(name.txt[CmdIdx]);
              end;
            end;
          ProcessArg(Cmd, MissingArg);
          end;
        end;
  end; { GetCS }
