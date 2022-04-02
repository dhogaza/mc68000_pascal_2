{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1982, 1983, 1984, 1985, 1986, 1987, 1988
                1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Parse a file name

 Last modified by KRIS on 26-Nov-1990 14:04:45
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{ Parse a file name or default and fill in fields in the FHS block
}
{$norangecheck,noindexcheck,nowalk,nomain [b+]}
{$section=8,version=0045,ident='Parse a file name'}

%include libtyp;


procedure p_usrtrm(var result: packed array [lo..hi: integer] of char);
 {gets user terminal name}
  external;


procedure p_entlib;
{ Mark this procedure as in a library
}
  external;


procedure p_exitlb;
{ Exiting from a library procedure
}
  external;


procedure p_parse(var fname: packed array [flo..fhi: integer] of char; {file
                    name}
                  var result: fhs_block; {result block}
                  var errfound: boolean {something wrong} );
  external;
{
Scans the filename and sets the appropriate fields within the fhs-block.
The first field of the block (p_code) is used to flag the presence of a
switch following the file name, and it is set to a nonzero value in case
a switch has been found.
}


procedure p_parse;

  type
    field_no = 1..6; {determines field in record. Should be an enumerated type
                      except that we have to do arithmetic on it}

  var
    lastfield: field_no; {last field stored}
    left_justify: boolean; {parsing from the left}
    state: (deviceid, volumeid, fields, protcode, switch, done, error);
    next_char: integer; {next character to scan}
    ch: char; {character just scanned}
    curname: packed array [1..8] of char; {currently scanned name}
    fieldname: array [1..4] of packed array [1..8] of char; {saved names}
    field_count: 0..4; {count of fields saved}
    i: 1..6; {induction var}
    alphanum: set of char;


  procedure storefield(name: packed array [lo..hi: integer] of char; {name to
                         store}
                       field: field_no {field in which to store} );

{ This procedure stores a name into a field.  There are several ways that
  a name can fit into a field, and this procedures takes all of this into
  account
}

    var
      numvalue: shortint; {Numeric equivalent of the field}
      volid: packed array [1..4] of char; {takes user terminal id if needed}
      i: 1..8; {induction var}


    begin
      p_entlib;
      lastfield := field;
      if field = 2 then
        begin {user number (sigh!)}
        numvalue := 0;
        i := 1;
        if name[6] <> ' ' then state := error; {too big}
        while (state <> error) and (name[i] <> ' ') do
          begin
          if (name[i] >= '0') and (name[i] <= '9') and ((numvalue < 6553) or
             ((name[i] <= '3') and (numvalue = 6553))) then
            begin
            numvalue := numvalue * 10 + (ord(name[i]) - ord('0'));
            i := i + 1;
            end
          else state := error;
          end;
        end
      else if field = 6 then
        begin {protection code (long sigh!)}
        numvalue := 0;
        for i := 1 to 4 do
          if name[i] <> ' ' then
            numvalue := numvalue * 16 + (ord(name[i]) - ord('A') + 1) mod 16;
        end
      else if not ((field = 3) and ((name[1] = '&') and (name[2] = ' '))) and
              ((name[1] < 'A') or (name[1] > 'Z')) and (name[1] <> ' ') then
        state := error;
      with result do
        case field of
          1:
            begin
            if name[1] = ' ' then
              begin
              p_usrtrm(volid);
              p_voln := volid;
              end
            else for i := 1 to 4 do p_voln[i] := name[i];
            if name[5] <> ' ' then state := error;
            end;
          2: p_usern := numvalue;
          3:
            begin
            if (name[1] = '&') and (name[2] = ' ') then p_catnm := '        '
            else
              for i := 1 to 8 do p_catnm[i] := name[i];
            end;
          4: for i := 1 to 8 do p_filenm[i] := name[i];
          5:
            begin
            for i := 1 to 2 do p_ext[i] := name[i];
            if name[3] <> ' ' then state := error;
            end;
          6: p_prot := numvalue;
          end;
      p_exitlb;
    end; {storefield}


  procedure getchar;


    begin
      p_entlib;
      if next_char >= fhi then ch := ' '
      else
        begin
        next_char := next_char + 1;
        ch := fname[next_char];
        if (ch >= 'a') and (ch <= 'z') then
          ch := chr(ord(ch) - ord('a') + ord('A'));
        end;
      p_exitlb;
    end; {getchar}


  procedure gettoken;

{ get the next token
}

    var
      i: 0..8; {induction var}


    begin
      p_entlib;
      if state <> error then
        begin
        curname := '        ';
        state := done;
        if ch = '/' then state := switch
        else if ch <> ' ' then
          begin
          if ch = '(' then
            begin {protection code}
            getchar;
            i := 0;
            while (i < 4) and (ch >= 'A') and (ch <= 'P') do
              begin
              i := i + 1;
              curname[i] := ch;
              getchar;
              end;
            if (ch <> ')') or ((i <> 2) and (i <> 4)) then state := error
            else
              begin
              state := protcode;
              getchar;
              end;
            end
          else
            begin
            getchar;
            if ch = '#' then
              begin
              state := deviceid;
              getchar;
              end;
            if (ch in alphanum) or (ch = '&') then
              begin
              i := 0;
              repeat
                i := i + 1;
                curname[i] := ch;
                getchar;
              until (i = 8) or not (ch in alphanum);
              end;
            if state = done then
              if ch = ':' then state := volumeid
              else if ch in ['.', '(', ' ', '/'] then state := fields
              else state := error;
            end;
          end;
        end;
      p_exitlb;
    end; {gettoken}


  begin
    p_entlib;
    result.p_code := 0; {assume there is no switch}
    alphanum := ['A'..'Z', '0'..'9'];
    next_char := 0;
    ch := ''''; {any non-blank will do}
    state := done;
    gettoken;
    if state = deviceid then
      begin {device specification}
      storefield(curname, 1);
      for i := 2 to 6 do storefield('        ', i);
      gettoken;
      end
    else
      begin
      left_justify := false;
      if state = volumeid then
        begin
        lastfield := 1;
        if curname[1] <> ' ' then storefield(curname, 1);
        left_justify := true;
        gettoken;
        end;
      if (state = fields) and (curname[1] >= '0') and (curname[1] <= '9') then
        begin
        storefield(curname, 2);
        left_justify := true;
        gettoken;
        end;
      if (state = fields) and (curname[1] = '&') and (curname[2] = ' ') then
        begin
        storefield(curname, 3);
        left_justify := true;
        gettoken;
        end;
      field_count := 0;
      while state = fields do
        begin
        if field_count = 4 then state := error
        else field_count := field_count + 1;
        fieldname[field_count] := curname;
        gettoken;
        end;
      if not left_justify then
        if field_count = 1 then lastfield := 3 {next is file name}
        else lastfield := 5 - field_count;
      if state <> error then
        for i := 1 to field_count do
          if fieldname[i, 1] = ' ' then lastfield := lastfield + 1
          else storefield(fieldname[i], lastfield + 1);
      if state = protcode then
        begin
        storefield(curname, 6);
        gettoken;
        end;
      end;
    errfound := false;
    if state = switch then result.p_code := 1 {show there is a switch}
    else errfound := state <> done;
    p_exitlb;
  end;
