{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
}

program pascal2;

uses config, hdr, utils, csi, scan, sd, analys;

label
  99; { Target for fatal errors }



procedure resetswitches;

 { reset switch table between passes }


  begin
    switchcounters := originalswitches;
    currentswitch := 1;
    putlow := 0;
    puthi := 0;
    getlow := 0;
    gethi := 0;
  end;

procedure setoptions;

 { set overall options based on genmask and command line }

  var
    genitem: gentypes; { quasi induction for handling codegen mask }
    mask: unsignedint; { mask for handling codegen optimization options }
    maskbit: integer; { induction for checking optimization mask bits }
    disabled: boolean; { true if mask specifies disabling }


  begin {setoptions}
    { set default code generation options set }

    genset := [firstgenopt..lastgenopt];

    if genoptmask < 0 then
      begin
      mask := abs(genoptmask);
      disabled := false;
      end
    else
      begin
      disabled := true;
      mask := genoptmask;
      end;
    { now set the options based upon -debug -profile }
    if switcheverplus[debugging] or switcheverplus[profiling] then
      begin
      genset := genset - [lifetimes, propagation, hoisting, removedeadcode,
                subexpressions, tailmerging, bitops];
      end;

    { if a codegen options mask was specified, update the set }

    if switcheverplus[genmask] then
      begin
      if switcheverplus[test] then
        begin
        if disabled then write('DISABLED: ')
        else write('ENABLED: ');
        end;

      genitem := firstgenopt;

      { walk through the mask }

      overrideset := [];

      for maskbit := 0 to 15 do
        begin
        if odd(mask) then
          begin
          overrideset := overrideset + [genitem];
          if switcheverplus[test] then write(maskbit: 3);
          end;

        mask := mask div 2;
        genitem := succ(genitem);
        end;

      if disabled then genset := genset - overrideset
      else genset := genset + overrideset;

      if switcheverplus[test] then writeln;
      end;
  end {setoptions} ;

{DRB
procedure settime;


  begin {settime}
    if switcheverplus[timing] then
      timestamp(dum, dum, dum, istarthour, istartmin, istartsec);
  end {settime} ;


procedure printtime(pass: packed array [l..h: shortint] of char);

  var
    deltasec, deltamin, deltahour: integer; {to compute time per pass}
    i: shortint; {induction}


  begin {printtime}
    if switcheverplus[timing] then
      begin
      timestamp(dum, dum, dum, deltahour, deltamin, deltasec);
      if deltahour < istarthour then deltahour := deltahour + 24;
      deltasec := max(deltasec - istartsec + 60 * ((deltamin - istartmin) + 60 *
                      (deltahour - istarthour)), 1);
      for i := l to h do write(pass[i]);
      writeln(' ', deltasec: 1, ' sec., ', lastline: 1, ' lines, ',
              (lastline * 60) div deltasec: 1, ' lines/min.');
      end;
  end {printtime} ;
}



begin {main}

  current_stmt := 0; { used by error }
  current_line := 0; { used by error }
  fatalflag := false;
  lasterror := 0;
  sourcelevel := 0;
  currentswitch := 1;
  genoptmask := 0;
  csi.csi;
  setoptions;
{DRB
  if switcheverplus[timing] then
    timestamp(dum, dum, dum, starthour, startmin, startsec);

  opentemp; { open temp files }
}

  originalswitches := switchcounters;

  if switcheverplus[listcount] then
    begin
    listtable[1].start := 1;
    listtable[1].count := 0;
    lastlist := 1;
    end
  else lastlist := 0;

{DRB  opena;}
{DRB  settime;}
  if scanalys then scan.scan1;
  analys.analys;
  if scanalys then scan.scan2;
{DRB
  if switcheverplus[timing] and switcheverplus[details] then
    printtime('analys');
}
{DRB
  if (lasterror = 0) and (switcheverplus[outputmacro] or
     switcheverplus[outputobj]) then
    begin
    resetswitches;
    opent;
    settime;
    if travcode then
      begin
      openc;
      initcode;
      end;
    travrs;
    if travcode then exitcode
    else
      begin
      if switcheverplus[timing] and switcheverplus[details] then
        printtime('travrs');
      resetswitches;
      settime;
      openc;
      code;
      if switcheverplus[timing] and switcheverplus[details] then
        printtime('code  ');
      end;

    closec;

    if switcheverplus[symboltable] then closed;
    if switcheverplus[timing] then
      begin
      timestamp(dum, dum, dum, endhour, endmin, endsec);
      if endhour < starthour then endhour := endhour + 24;
      endsec := max(endsec - startsec + 60 * ((endmin - startmin) + 60 *
                    (endhour - starthour)), 1);
      writeln('Total  ', endsec: 1, ' sec., ', lastline: 1, ' lines, ',
              (lastline * 60) div endsec: 1, ' lines/min.');
      end;
    end;

    if switcheverplus[test] and switcheverplus[details] then
      writeln(insertions:1, ' identifiers, ', proctabletop:1,
              ' procedures/functions declared');
}
99:

{DRB
  closeall; { List will need stringfile open, so close all files except
             stringfile. }

  if (lastlist > 0) then
    begin
    if (listtable[lastlist].count = 0) then
      with listtable[lastlist] do count := lastline - start + 1;
    end
  else if lasterror > 0 then
    begin
    lastlist := lastlist + 1;
    with listtable[lastlist] do
      begin
      start := lastline + 1;
      count := 0;
      end;
    end;

  if not fakelist or (lasterror > 0) then
    begin
    resetswitches;
    list;
    closel;
    if lasterror > 0 then
      begin
      writeln('?Errors detected: ', lasterror: 1);
      exitst(exitstatus); {value is opsys dependent}
      end;
    end;

  close_str; { now close stringfile }
}

end {main} .
