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

  Dump a memory area

 Last modified by KRIS on 26-Nov-1990 14:08:17
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Dump a memory area'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

{
  Debugging procedure dumps a defined amount of memory starting
  with address passed on the stack.
  This procedure must be called using external pascal definition :

    procedure pd$dmpa(address: integer;
                      size: integer);
      external;

  Calls from assembly language should follow the pattern shown below :
         ...
         pea      <starting address>
         pea      <size>
         jsr.l    pd$dmpa
         xref     pd$dmpa
         addq.w   #8,sp
         ...
}

type
  byte = 16#0..16#FF;
  word = 16#0..16#FFFF;
  long = 16#0..16#FFFFFFFF;
  fake_type =
    record
      case integer of
        0: (i: integer);
        1: (b: packed array [0..1] of byte);
        2: (w: word);
        4: (l: long);
    end { fake_type } ;
  fake_ptr = ^fake_type;
  ascii_type = packed array [1..16] of char;
  ascii_ptr = ^ascii_type;


procedure pd$dmpa(var value: fake_type;
                  size: integer);
  external;


procedure pd$dmpa;

  var
    i, j, k: integer;
    addr: fake_ptr;
    ascii: ascii_ptr;


  begin { pd$dmpa }
    addr := loophole(fake_ptr, ref(value));
    ascii := nil;
    i := 0;
    repeat
      if (i mod 16) = 0 then
        begin
        if ascii <> nil then
          begin
          write('  [');
          for k := 1 to 16 do
            if ascii^[k] in [' '..'~'] then write(ascii^[k])
            else write('.');
          write(']');
          ascii := nil;
          end { if ascii<>nil } ;
        writeln;
        write(output, loophole(integer, addr): - 8);
        end;
      write(' : ', addr^.l: - 8);
      if ascii = nil then ascii := loophole(ascii_ptr, addr);
      i := i + 4;
      j := loophole(integer, addr) + 4;
      addr := loophole(fake_ptr, j);
    until i >= size;
    if (i mod 16) <> 0 then
      begin
      k := i mod 16;
      repeat
        write(' : ', '        ');
        k := k + 4;
      until k = 16;
      write('  [');
      for k := 1 to (i mod 16) do
        if ascii^[k] in [' '..'~'] then write(ascii^[k])
        else write('.');
      write(']');
      ascii := nil;
      end { if (i mod 16) <> 0 } ;
    writeln;
  end { pd$dmpa } ;
