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

  dump the list of opened files

 Last modified by KRIS on 26-Nov-1990 14:07:57
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='dump the list of opened files'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

%include libtyp;


procedure pd$ga5(var gptr: p2_globl_ptr);
  external;


procedure pd$dmpl(address: integer;
                  size: integer);
  external;


procedure pd$sgfil(var gfil: integer);
  external;


procedure pd$rgfil(gfil: integer);
  external;


procedure pd$files;
  external;


procedure disp_alfa(s: packed array [lo..hi: integer] of char);

  var
    i: integer;


  begin {disp_alfa}
    i := lo - 1;
    repeat
      i := i + 1;
      if s[i] > ' ' then write(s[i]);
    until (s[i] <= ' ') or (i = hi);
  end {disp_alfa} ;


procedure pd$files;

  type
    fdb_block_ptr_ptr = ^fdb_block_ptr;

  var
    g_file_buff: integer;
    ptr: p2_globl_ptr;
    fpt: fdb_block_ptr;


  begin { pd$files }
    pd$sgfil(g_file_buff);
    pd$ga5(ptr);
    fpt := ptr^.g_lib^.h_lib.l_files;
    writeln('List of opened files: ');
    repeat
      with fpt^ do
        begin
        pd$dmpl(loophole(integer, f_fvar), 0);
        write(':');
        pd$dmpl(loophole(integer, fpt), 0);
        write('-');
        pd$dmpl(loophole(integer, fpt) + size(fdb_block) - 1, 0);
        write(' - (lun=');
        write(f_ios.i_lun div 10: 1, f_ios.i_lun mod 10: 1, ') ');
        if s1_perm in f_status then
          begin
          if s_inp in f_status then write('standard input')
          else write('standard output');
          end {if s1_perm in f_status}
        else
          with f_fhs do
            begin
            if p_filenm[1] = ' ' then write('#');
            disp_alfa(p_voln);
            if p_filenm[1] <> ' ' then
              begin
              write(':', p_usern: 1, '.');
              disp_alfa(p_catnm);
              write('.');
              disp_alfa(p_filenm);
              write('.');
              disp_alfa(p_ext);
              end;
            end {if not (s1_perm in f_status)} ;
        writeln;
        end { with fpt^ } ;
      fpt := fpt^.f_next;
    until loophole(integer, fpt) = 0;
    pd$rgfil(g_file_buff);
  end { pd$files } ;
