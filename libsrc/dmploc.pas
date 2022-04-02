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

  Dump an address and value

 Last modified by KRIS on 26-Nov-1990 14:08:13
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Dump an address and value'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

{
  Debugging procedure dumps address of variable and the value.
  This procedure must be called using external pascal definition :

    procedure pd$dmpl(address: integer;
                      size: integer);
      external;

  Calls from assembly longuage should follow the pattern shown below :
         ...
         pea      <address to dump>
         pea      <size>
         jsr.l    pd$dmpl
         xref     pd$dmpl
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


procedure pd$dmpl(var value: fake_type;
                  size: integer);
  external;


procedure pd$dmpl;


  begin { pd$dmpl }
    write(output, loophole(integer, ref(value)): - 8);
    case size of
      0: { print only address } ;
      1: write(' : ', value.b[0]: - 2);
      2: write(' : ', value.w: - 4);
      4: write(' : ', value.l: - 8);
      otherwise writeln('illegal type''s size : ', size: 1);
      end { case size } ;
  end { pd$dmpl } ;
