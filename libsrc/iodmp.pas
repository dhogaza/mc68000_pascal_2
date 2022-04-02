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

  Dump io definition block

 Last modified by KRIS on 26-Nov-1990 14:08:06
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Dump io definition block'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

{ Dump the Pascal-2 file variable : io definition block
}
%include libtyp;

type
  io_block_ptr = ^io_block;


procedure pd$dmpl(address: longint;
                  size: integer);
  external;


procedure iodmp(var f: io_block;
                tab: integer);
  external;


procedure iodmp;

  var
    location: longint;


  begin { iodmp }
    write(' ': tab);
    write('Ios parameter block at : ');
    location := loophole(longint, ref(f));
    pd$dmpl(location, 0);
    writeln;
    with f do
      begin
      write(' ': tab);
      pd$dmpl(location, 2);
      writeln(' - code/command for i/o operation');
      location := location + size(shortint);
      write(' ': tab);
      pd$dmpl(location, 2);
      writeln(' - options which modify operation');
      location := location + size(shortint);
      write(' ': tab);
      pd$dmpl(location, 1);
      writeln(' - status returned for last operation');
      location := location + 1 {size(byte)} ;
      write(' ': tab);
      pd$dmpl(location, 1);
      writeln(' - logical unit number');
      location := location + 1 {size(byte)} ;
      write(' ': tab);
      pd$dmpl(location, 2);
      writeln(' - reserved, must be zero');
      location := location + size(shortint);
      write(' ': tab);
      pd$dmpl(location, 4);
      writeln(' - random record number');
      location := location + size(longint);
      write(' ': tab);
      pd$dmpl(location, 4);
      writeln(' - starting address of buffer');
      location := location + size(longint);
      write(' ': tab);
      pd$dmpl(location, 4);
      writeln(' - ending address of buffer');
      location := location + size(longint);
      write(' ': tab);
      pd$dmpl(location, 4);
      writeln(' - length of last record read ');
      location := location + size(longint);
      write(' ': tab);
      pd$dmpl(location, 4);
      writeln(' - completion address (zero for pascal)');
      location := location + size(longint);
      end { with f } ;
  end { iodmp } ;
