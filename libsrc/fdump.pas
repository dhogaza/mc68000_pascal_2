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

  Dump Pascal-2 file variable

 Last modified by KRIS on 26-Nov-1990 14:08:01
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Dump Pascal-2 file variable'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

{ Dump the Pascal-2 file variable 
}
%include libtyp;


procedure pd$dmpl(address: longint;
                  size: integer);
  external;


procedure iodmp(var f: io_block;
                tab: integer);
  external;


procedure fhsdmp(var f: fhs_block;
                 tab: integer);
  external;


procedure fdump(var f: fdb_block_ptr;
                dump_status: integer);
  external;
{
 dump_status : bit 0 - list status bits,
               bit 1 - dump io_block,
               bit 2 - dump fhs_block

calling from macro:
         pea      <filevariable>
         pea      <dump_status>
         jsr.l    fdump
         addq.w   #8,sp
}


procedure fdump;


  begin { fdump }
    write('File variable pointer at : ');
    pd$dmpl(loophole(longint, ref(f)), 4);
    writeln;
    with f^ do
      begin
      pd$dmpl(loophole(longint, ref(f_point)), 4);
      writeln(' - pointer to data');
      pd$dmpl(loophole(longint, ref(f_status)), 2);
      writeln(' - pascal file status');
      if (dump_status and 1) <> 0 then
        begin
        if s_eof in f_status then writeln('  - end of file detected');
        if s_eoln in f_status then writeln('  - end of line detected');
        write('  - current component is ');
        if not (s_def in f_status) then write('not');
        writeln(' defined');
        if s_text in f_status then writeln('  - text file')
        else writeln('  - record file');
        if s_inp in f_status then writeln('  - input operations allowed');
        if s_out in f_status then writeln('  - output operations allowed');
        if s_newl in f_status then
          writeln('  - new input line should be read');
        if s_int in f_status then writeln('  - interactive device');
        if s1_asnch in f_status then writeln('  - asynchronous I/O');
        if s1_perm in f_status then writeln('  - standard input or output');
        if s1_ran in f_status then
          writeln('  - random access operations enabled');
        if s1_noerr in f_status then writeln('  - do not trap on error');
        if s1_necho in f_status then writeln('  - echo is off');
        if s1_sngl in f_status then writeln('  - single character mode');
        if s1_cont in f_status then writeln('  - file is contigous');
        end { if (dump_status and 1) <> 0 } ;
      pd$dmpl(loophole(longint, ref(f_next)), 4);
      writeln(' - pointer to next file in list');
      pd$dmpl(loophole(longint, ref(f_buff)), 4);
      writeln(' - address of file''s record buffer');
      pd$dmpl(loophole(longint, ref(f_buffsz)), 2);
      writeln(' - length of record buffer ');
      pd$dmpl(loophole(longint, ref(f_fvar)), 4);
      writeln(' - back pointer to file variable');
      pd$dmpl(loophole(longint, ref(f_err)), 2);
      writeln(' - pascal error code');
      pd$dmpl(loophole(longint, ref(f_ios)), 0);
      writeln(' - ios parameter block');
      if (dump_status and 2) <> 0 then iodmp(f_ios, 10);
      pd$dmpl(loophole(longint, ref(f_fhs)), 0);
      writeln(' - fhs parameter block');
      if (dump_status and 4) <> 0 then fhsdmp(f_fhs, 10);
      end { with f^ } ;
  end { fdump } ;
