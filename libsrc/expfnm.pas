{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987, 1988, 1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Expand a file name to its uniqualified form

 Last modified by KRIS on 26-Nov-1990 14:07:30
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Expand a file name to its uniqualified form'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

%include libtyp;

{ 
Expand a file name to its unqualified form
}


procedure p_itoc(procedure putf(ch: char); {receives chars of converted value}
                 e: integer; {the integer value to convert}
                 total_width: integer {total field width for conversion} );
  external;

type
  fdb_block_ptr_ptr = ^fdb_block_ptr;


procedure p_expfnm(var f: fdb_block_ptr; {open text file whose name we want}
                   var fn: packed array [l..h: shortint] of char; {file name to
                     be updated}
                   var fnlen: shortint {length to be updated} );
  external;


procedure p_expfnm;

  var
    i: integer;


  procedure storech(ch: char);

   { store a character into the filename buffer }


    begin {storech}
      fnlen := fnlen + 1;
      fn[fnlen] := ch;
    end {storech} ;


  begin {p_expfnm}
    with f^.f_fhs do
      begin
      i := 1;
      fnlen := 0;
      { volume name }
      while (p_voln[i] <> ' ') and (i <= 4) do
        begin
        fnlen := fnlen + 1;
        fn[fnlen] := p_voln[i];
        i := i + 1;
        end {while (p_voln[i] <> ' ') and (i <= 4)} ;
      fnlen := fnlen + 1;
      fn[fnlen] := ':';
      { user's name }
      p_itoc(storech, p_usern, 1); {format the integer}
      fnlen := fnlen + 1;
      fn[fnlen] := '.';
      i := 1;
      { catalog name }
      if p_catnm[i] = ' ' then
        begin
        fnlen := fnlen + 1;
        fn[fnlen] := '&';
        end
      else
        while (p_catnm[i] <> ' ') and (i <= 8) do
          begin
          fnlen := fnlen + 1;
          fn[fnlen] := p_catnm[i];
          i := i + 1;
          end {while (p_catnm[i] <> ' ') and (i <= 8)} ;
      fnlen := fnlen + 1;
      fn[fnlen] := '.';
      i := 1;
      { file name }
      while (p_filenm[i] <> ' ') and (i <= 8) do
        begin
        fnlen := fnlen + 1;
        fn[fnlen] := p_filenm[i];
        i := i + 1;
        end {while (p_filenm[i] <> ' ') and (i <= 8} ;
      fnlen := fnlen + 1;
      fn[fnlen] := '.';
      i := 1;
      { file extension }
      while (p_ext[i] <> ' ') and (i <= 2) do
        begin
        fnlen := fnlen + 1;
        fn[fnlen] := p_ext[i];
        i := i + 1;
        end {while (p_ext[i] <> ' ') and (i <= 2)} ;
      end {with f^.f_fhs} ;
  end {p_expfnm} ;
