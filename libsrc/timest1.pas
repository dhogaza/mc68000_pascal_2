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

  compiler - library jacketting routine for timestamp

 Last modified by KRIS on 26-Nov-1990 14:06:55
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$version=0045}
{$ident='compiler - library jacketting routine for timestamp'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

{
Jacketing module for timestamp.

This is the jacket to solve incompatibility in the length of produced global
names between the Pascal-2 compiler and the VERSAdos assembler.
If the library is assembled under VERSAdos, names exceeding eight characters
in length are truncated to the maximum eight-character restriction of the 
assembler.  Currently the symbol "timestamp"  becomes "timstam".

The incompatibility arises because the Pascal-2 compiler uses the full names
in its object files, thus the linker cannot resolve the references.

This jacket accepts full-length references and calls eight-character
entry points in the library.
}


procedure timestamp(var day, month, year: integer;
                    var hour, minute, second: integer);
  external;


procedure timestam(var day, month, year: integer;
                   var hour, minute, second: integer);
  external;


procedure timestamp;


  begin {timestamp}
    timestam(day, month, year, hour, minute, second);
  end {timestamp} ;
