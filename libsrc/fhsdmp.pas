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

  Dump fhs parameter block

 Last modified by KRIS on 26-Nov-1990 14:08:09
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Dump fhs parameter block'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

{ Dump the Pascal-2 file variable : FHS parameter block
}
%include libtyp;


procedure pd$dmpl(address: longint;
                  size: integer);
  external;


procedure fhsdmp(var f: fhs_block;
                 tab: integer);
  external;


procedure fhsdmp;

  var
    location: longint;


  begin { fhsdmp }
    write(' ': tab);
    write('Fhs parameter block at : ');
    location := loophole(longint, ref(f));
    pd$dmpl(location, 0);
    writeln;
    with f do
      begin
      { p_code }
      write(' ': tab);
      pd$dmpl(location, 1);
      writeln(' - fhs instruction');
      location := location + 1 {size(byte)} ;
      { p_cmd }
      write(' ': tab);
      pd$dmpl(location, 1);
      writeln(' - fhs command');
      location := location + 1 {size(byte)} ;
      { p_opt }
      write(' ': tab);
      pd$dmpl(location, 2);
      writeln(' - options which modify operation');
      location := location + size(shortint);
      { p_status }
      write(' ': tab);
      pd$dmpl(location, 1);
      writeln(' - status returned for last operation');
      location := location + 1 { size(shortint)} ;
      { p_lun }
      write(' ': tab);
      pd$dmpl(location, 1);
      writeln(' - logical unit number');
      location := location + 1 { size(shortint)} ;
      { p_voln }
      write(' ': tab);
      pd$dmpl(location, 0);
      writeln(' : <', p_voln, '> - volume name');
      location := location + 4 { size(packed array [1..4] of char) } ;
      { p_usern }
      write(' ': tab);
      pd$dmpl(location, 0);
      writeln(' : ', p_usern: 4, ' - user number');
      location := location + size(shortint);
      { p_catnm }
      write(' ': tab);
      pd$dmpl(location, 0);
      writeln(' : <', p_catnm, '> - catalog name');
      location := location + 8 { size(packed array [1..8] of char) } ;
      { p_filenm }
      write(' ': tab);
      pd$dmpl(location, 0);
      writeln(' : <', p_filenm, '> - file name');
      location := location + 8 { size(packed array [1..8] of char) } ;
      { p_ext }
      write(' ': tab);
      pd$dmpl(location, 0);
      writeln(' : <', p_ext, '> - extension');
      location := location + 2 { size(packed array [1..2] of char) } ;
      { p_resv }
      write(' ': tab);
      pd$dmpl(location, 2);
      writeln(' - reserved');
      location := location + size(shortint);
      { p_prot }
      write(' ': tab);
      pd$dmpl(location, 2);
      writeln(' - read/write protection codes');
      location := location + size(shortint);
      { p_rlen }
      write(' ': tab);
      pd$dmpl(location, 2);
      writeln(' - record length');
      location := location + size(shortint);
      { p_sizept }
      write(' ': tab);
      pd$dmpl(location, 4);
      writeln(' - size/pointer info');
      location := location + size(longint);
      end { with f } ;
  end { fhsdmp } ;
