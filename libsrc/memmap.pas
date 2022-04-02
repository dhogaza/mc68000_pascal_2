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

  Dump the library work area

 Last modified by KRIS on 26-Nov-1990 14:07:53
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Dump the library work area'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

%include libtyp;

const
  consistency_check = false;
  full_debug = false;


procedure pd$ga5(var gptr: p2_globl_ptr);
  external;


procedure pd$dmpl(address: integer;
                  size: integer);
  external;


procedure pd$dmpa(address: integer;
                  size: integer);
  external;


procedure pd$dmpm;
  external;


procedure pd$sgfil(var gfil: integer);
  external;


procedure pd$rgfil(gfil: integer);
  external;


procedure pd$dmpm;

  var
    p2_paw: paw_ptr;
    ptr: p2_globl_ptr;
    l_lfile_buff: integer;


  begin { pd$dmpm }
    pd$sgfil(l_lfile_buff);
    if consistency_check then
      begin
      writeln('size of ');
      writeln(' - io_block  = ', size(io_block): - 8);
      writeln(' - fhs_block = ', size(fhs_block): - 8);
      writeln(' - fdb_block = ', size(fdb_block): - 8);
      writeln(' - io_block  = ', size(io_block): - 8);
      writeln(' - lib_area  = ', size(lib_area): - 8);
      writeln(' - p2_globl  = ', size(p2_globl): - 8);
      writeln(' - err_area  = ', size(err_area): - 8);
      writeln(' - paw       = ', size(paw): - 8);
      writeln;
      end { if consistency_check } ;
    pd$ga5(ptr);
    write('Pascal-2 global pointer (register a5) : ');
    pd$dmpl(loophole(integer, ptr), 4);
    writeln;
    writeln('Map of the Pascal-2 work segment (PAW$):');
    writeln(loophole(integer, ptr^.g_lib): - 8, ' - ', loophole(integer,
            ptr) - 3: - 8, ' - library global area (', size(lib_area): 1,
            ' bytes)');
    writeln(loophole(integer, ptr) - 2: - 8, ' - ', loophole(integer, ptr) - 1:
            - 8, ' - nesting level of library routines (2 bytes)');
    with ptr^.g_lib^.h_lib do
      begin
      writeln(loophole(integer, ptr): - 8, ' - ', loophole(integer,
              l_erra) - 1: - 8, ' - global pointers (', size(p2_globl): 1,
              ' bytes)');
      writeln(loophole(integer, l_erra): - 8, ' - ', (loophole(integer,
              l_erra) + 511): - 8, ' - error communication area (512 bytes)');
      writeln(ptr^.g_input - 4: - 8, ' - ', ptr^.g_output - 5: - 8,
              ' - file block for standard input (', size(fdb_block) + 4: 1,
              ' bytes)');
      writeln(ptr^.g_output - 4: - 8, ' - ', l_heap - 1: - 8,
              ' - file block for standard output (', size(fdb_block) + 4: 1,
              ' bytes)');
      writeln(l_heap: - 8, ' - ', ptr^.g_sp: - 8, ' - stack and heap (',
              (ptr^.g_sp - l_heap): 1, ' bytes)');
      end { with ptr^.g_lib^.h_lib } ;
    with ptr^ do
      begin
      writeln;
      writeln('pointers into global area : ');
      writeln(' - g_spovf  = ', g_spovf: - 8);
      writeln(' - g_own    = ', g_own: - 8);
      writeln(' - g_sp     = ', g_sp: - 8);
      writeln(' - g_lib    = ', loophole(integer, g_lib): - 8);
      writeln(' - g_output = ', g_output: - 8);
      writeln(' - g_input  = ', g_input: - 8);
      end { with ptr^ } ;
    with ptr^.g_lib^.h_lib do
      begin
      writeln('Pascal-2 library global area : ');
      writeln(' - l_files  = ', loophole(integer, l_files): - 8);
      writeln(' - l_free   = ', l_free: - 8);
      writeln(' - l_zero   = ', l_zero: - 8);
      writeln(' - l_heap   = ', l_heap: - 8);
      writeln(' - l_high   = ', l_high: - 8);
      writeln(' - l_hchk   = ', l_hchk: - 8);
      writeln(' - l_schk   = ', l_schk: - 8);
      writeln(' - l_rworkl = ', l_rworkl: - 8);
      writeln(' - l_aworkl = ', l_aworkl: - 8);
      writeln(' - l_globl  = ', l_globl: - 8);
      writeln(' - l_globa  = ', l_globa: - 8);
      writeln(' - l_erra   = ', loophole(integer, l_erra): - 8);
      writeln(' - l_input  = ', l_input: - 8);
      writeln(' - l_output = ', l_output: - 8);
      writeln(' - l_father = ''', l_father, '''');
      writeln(' - l_sessio = ''', l_sessio, '''');
      writeln(' - l_voln   = ''', l_voln, '''');
      writeln(' - l_user   = ', l_user: 4);
      writeln(' - l_cat    = ''', l_cat, '''');
      writeln(' - l_coml   = ', l_coml: - 4);
      writeln(' - l_luns   = ', l_luns: - 8);
      writeln(' - l_taskn  = ''', l_taskn, '''');
      writeln(' - l_uterm  = ''', l_uterm, '''');
      writeln(' - l_ruser  = ', l_ruser: - 4);
      writeln(' - l_break  = ', l_break: - 2);
      writeln(' - l_bkwant = ', l_bkwant: - 2);
      writeln(' - l_dbgbuf = ', l_dbgbuf: - 8);
      writeln(' - l_pma    = ', l_pma: - 8);
      writeln(' - l_term   = ', l_term: - 8);
      writeln(' - l_abort  = ', l_abort: - 4);
      writeln(' - l_fcheck = ', l_fcheck: - 4);
      writeln(' - l_lfile  = ', l_lfile_buff: - 8);
      end { with ptr^.g_lib^.h_lib } ;
    with ptr^.g_lib^ do
      writeln('level of nesting in library routines (g_liblev) : ', g_liblev:
              - 4);
    with ptr^.g_lib^.h_lib.l_erra^ do
      begin
      writeln(' - er_usp  = ', er_usp: - 8);
      writeln(' - er_code = ', er_code: - 4);
      writeln(' - er_aux  = ', er_aux: - 8);
      if er_aux <> improb then
        begin
        writeln('auxiliary data : ');
        pd$dmpa(er_aux, 16#40);
        if full_debug then
          begin
          writeln('stack''s content (', (loophole(integer,
                  ptr^.g_sp) - loophole(integer,
                                         ptr^.g_lib^.h_lib.l_erra^.er_usp)):
                  1, ' bytes) : ');
          pd$dmpa(loophole(integer, ptr^.g_lib^.h_lib.l_erra^.er_usp),
                  loophole(integer, ptr^.g_sp) - loophole(integer,
                                                           ptr^.g_lib^.h_lib.
                                                           l_erra^.er_usp));
          end { if full_debug } ;
        end {if er_aux <> improb} ;
      end { with ptr^.g_lib^.h_lib.l_erra^ } ;
    pd$rgfil(l_lfile_buff);
  end { pd$dmpm } ;
