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

  Close files in range

 Last modified by KRIS on 26-Nov-1990 14:04:59
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Close files in range'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

%include libtyp;

type
  filevariable = fdb_block_ptr;


procedure pd$ga5(var gptr: p2_globl_ptr);
  external;


procedure p_close(f_var: longint);
  external;


procedure p_clsrng(base: integer;
                   len: integer);
  external;


procedure p_entlib;
  external;


procedure p_exitlb;
  external;


procedure p_clsrng { (var base: integer; len: integer) } ;

  var
    gb: p2_globl_ptr;
    checked, closed: filevariable;


  begin { p_clsrng }
    p_entlib;
    pd$ga5(gb);
    with gb^.g_lib^.h_lib do checked := l_files;
    while loophole(integer, checked) <> 0 do
      begin
      closed := checked;
      checked := checked^.f_next;
      if (closed^.f_fvar >= base) and (closed^.f_fvar <= base + len - 1) then
        p_close(closed^.f_fvar);
      end { while loophole(integer, checked) <> 0 } ;
    p_exitlb;
  end { p_clsrng } ;
