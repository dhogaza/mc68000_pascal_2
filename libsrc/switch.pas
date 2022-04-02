{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1987, 1988, 1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Parse I/O Switches

 Last modified by KRIS on 26-Nov-1990 14:04:51
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{ Parse a string and retrive all switches
}
{$norangecheck,noindexcheck,nowalk,nomain [b+]}
{$section=8,version=0045,ident='Parse I/O Switches'}

%include libtyp;


procedure p_entlib;
{ Mark this procedure as in a library
}
  external;


procedure p_exitlb;
{ Exiting from a library procedure
}
  external;


procedure p_switch(var name: packed array [lo..hi: integer] of char;
                   var result: fdb_block; {result block}
                   var errfound: boolean {something wrong} );
  external;


procedure p_switch;

  type
    qualtype = (ambigous, lun, buff, single, noecho, echo, seeksw, asynch,
                contiguous, none);
    qualname = packed array [1..6] of char;
    switchtable = array [qualtype] of qualname;

  const
    switchnames = switchtable('      ', 'LUN   ', 'BUFF  ', 'SINGLE', 'NOECHO',
                              'ECHO  ', 'SEEK  ', 'ASYNCH', 'CONTIG',
    '      ');

  var
    slash, qualend, swend, i: integer;


  function switch: qualtype;

    var
      ioswitch: packed set of qualtype;
      i: integer;
      tempqual: qualtype;
      done: boolean;


    begin {switch}
      qualend := swend;
      for i := slash + 1 to swend do if name[i] = '=' then qualend := i - 1;
      ioswitch := [lun..contiguous];
      done := false;
      i := 1;
      repeat
        for tempqual := lun to contiguous do
          if tempqual in ioswitch then
            begin
            if (name[slash + i] <> switchnames[tempqual, i]) and
               (name[slash + i] <> chr(ord(switchnames[tempqual, i]) - ord('A'
                                        ) + ord('a'))) then
              ioswitch := ioswitch - [tempqual];
            end {if tempqual in ioswitch} ;
        if ioswitch = [] then done := true
        else if slash + i = qualend then done := true
        else i := i + 1;
      until done;
      tempqual := ambigous;
      done := false;
      repeat
        tempqual := succ(tempqual);
        done := tempqual in ioswitch;
        if done then
          begin
          ioswitch := ioswitch - [tempqual];
          if ioswitch <> [] then tempqual := ambigous;
          end {if done}
        else done := tempqual = none;
      until done;
      switch := tempqual;
    end {switch} ;


  function intval(s, e: integer): integer;

    var
      i, val: integer;


    begin {intval}
      val := 0;
      for i := s to e do val := val * 10 + ord(name[i]) - ord('0');
      intval := val;
    end {intval} ;


  begin {p_switch}
    p_entlib;
    errfound := false; {let's be a little optimistic}
    slash := lo;
    repeat
      while (slash < hi) and (name[slash] <> '/') do slash := slash + 1;
      if slash < hi then
        begin {there is a switch}
        {find the end of the switch}
        swend := slash + 1;
        while (swend < hi) and (name[swend] <> '/') and (name[swend] <> ' ') do
          swend := swend + 1;
        if (name[swend] = '/') or (name[swend] = ' ') then swend := swend - 1;
        case switch of
          ambigous: errfound := true;
          lun: result.f_fhs.p_lun := intval(qualend + 2, swend);
          buff: result.f_buffsz := intval(qualend + 2, swend);
          single:
            begin
            result.f_status := result.f_status + [s1_sngl];
            if s_inp in result.f_status then result.f_buffsz := 1;
            end {single} ;
          noecho: result.f_status := result.f_status + [s1_necho];
          echo: result.f_status := result.f_status - [s1_necho];
          seeksw: {ignore it} ;
          asynch: result.f_status := result.f_status + [s1_asnch];
          contiguous: result.f_status := result.f_status + [s1_cont];
          none: errfound := true;
          end {case switch} ;
        slash := swend + 1;
        end {if i < hi}
      else errfound := true;
    until errfound or (slash >= hi) or (name[slash] <> '/');
    p_exitlb;
  end {p_switch} ;
