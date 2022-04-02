{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1982, 1983, 1984, 1985, 1986, 1987, 1988
                1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  File handling system - Pascal-2 definitions

 Last modified by KRIS on 26-Nov-1990 14:09:58
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{ Pascal Support Library common and configuration definitions.
  The definition in this module must be fully compatible with assembly
  definitions in fhsdef.sa.     ====    =====
}
{$nolist}

const
{
FHS request types (command)
}
  fh_fcmd = 0; { Device/file commands }
  fh_ucmd = 1; { utility commands }
  fh_scmd = 2; { spooler commands }
  fh_priv = 16#80; { privileged commands }
{
Command bits for device/file commands (fh_fcmd)
}
  fh_ckp_bit = 0; { checkpoint }
  fh_del_bit = 1; { delete }
  fh_cls_bit = 2; { close }
  fh_prt_bit = 3; { protect }
  fh_ren_bit = 4; { rename }
  fh_cap_bit = 5; { change access permission }
  fh_ass_bit = 6; { assign }
  fh_all_bit = 7; { allocate }
{
command values for device/file commands
}
  fh_ckp = 16#1; { 1<<fh_ckp_bit - checkpoint }
  fh_del = 16#2; { 1<<fh_del_bit - delete }
  fh_cls = 16#4; { 1<<fh_cls_bit - close }
  fh_prt = 16#8; { 1<<fh_prt_bit - protect }
  fh_ren = 16#10; { 1<<fh_ren_bit - rename }
  fh_cap = 16#20; { 1<<fh_cap_bit - change access permission }
  fh_ass = 16#40; { 1<<fh_ass_bit - assign }
  fh_all = 16#80; { 1<<fh_all_bit - allocate }
{
option field locations for device/file commands
}
  fh_uop_bit = 12; { user options (3 bit field) }
  fh_fty_bit = 8; { file type (3 bit field) }
  fh_pos_bit = 6; { position option }
  fh_ssg_bit = 5; { shared segment option }
  fh_ovr_bit = 3; { file overwrite option }
  fh_apm_bit = 0; { access permission (3 bit field) }
{
option field values for device/file commands
}
  fh_ctg = 16#0; { 0<<fh_fty_bit - contiguous file type }
  fh_seq = 16#100; { 1<<fh_fty_bit - sequential file type }
  fh_iseq = 16#200; { 2<<fh_fty_bit - index sequential (no duplicate keys) }
  fh_isdu = 16#300; { 3<<fh_fty_bit - index sequential (duplicate keys) }
  fh_pbof = 16#0; { 0<<fh_pos_bit - position to start of file }
  fh_beof = 16#40; { 1<<fh_pos_bit - position to end of file }
  fh_ovr = 16#8; { 1<<fh_ovr_bit - overwrite existing file }
  fh_pro = 16#0; { 0<<fh_apm_bit - public read only }
  fh_ero = 16#1; { 1<<fh_apm_bit - exclusive read only }
  fh_pwo = 16#2; { 2<<fh_apm_bit - public write only }
  fh_ewo = 16#3; { 3<<fh_apm_bit - exclusive write only }
  fh_prw = 16#4; { 4<<fh_apm_bit - public read write }
  fh_prew = 16#5; { 5<<fh_apm_bit - public read exclusive write }
  fh_erpw = 16#6; { 6<<fh_apm_bit - exclusive read public write }
  fh_erw = 16#7; { 7<<fh_apm_bit - exclusive read write }
{
Command bits for utility commands
}
  fh_fdv_bit = 3; { fetch default volume }
  fh_clu_bit = 4; { change logical unit }
  fh_fdm_bit = 5; { fetch device mnemonics }
  fh_fde_bit = 6; { fetch directory entry }
  fh_rat_bit = 7; { retrieve attributes }
{
Command values for utility commands
}
  fh_fdv = 16#8; { 1<<fh_fdv_bit - fetch default volume }
  fh_clu = 16#10; { 1<<fh_clu_bit - change logical unit }
  fh_fdm = 16#20; { 1<<fh_fdm_bit - fetch device mnemonics }
  fh_fde = 16#40; { 1<<fh_fde_bit - fetch directory entry }
  fh_rat = 16#80; { 1<<fh_rat_bit - retrieve attributes }
{
Option values for change logical unit
}
  fhc_rec = 16#1; { receive the lun }
  fhc_kp = 16#8000; {keep a copy }
{
File Handling Service (FHS) error messages
}
  fhse_noerror = 16#0; { no error }
  fhse_no_trap = 16#1; { fhs trap server doesn't exists }
  fhse_bad_cmd = 16#2; { invalid command }
  fhse_bad_lun = 16#3; { invalid logical unit number }
  fhse_bad_vol = 16#4; { invalid logical unit number }
  fhse_dupl_nam = 16#5; { duplicate file name }
  fhse_bad_desc = 16#6; { file descriptor error }
  fhse_prot_err = 16#7; { protect code error }
  fhse_rec_len = 16#8; { record length error }
  fhse_shar_seg = 16#9; { shared segment error }
  fhse_dir_space = 16#A; { insufficient directory space }
  fhse_acc_perm = 16#B; { access permission error }
  fhse_sys_space = 16#C; { insufficient system space }
  fhse_bad_ass = 16#D; { invalid assignement }
  fhse_bad_devtyp = 16#E; { invalid device type }
  fhse_buf_ovfl = 16#F; { buffer overflow }
  fhse_bad_tsknam = 16#10; { invalid task name }
  fhse_bad_bufadr = 16#11; { invalid buffer address }
  fhse_bad_filtyp = 16#12; { invalid file type }
  fhse_sys_problem = 16#13; { internal fhs error caused by system problem }
  fhse_bad_bl = 16#14; { invalid parameter block address }
  fhse_block_len = 16#15; { data block length error }
  fhse_bad_size = 16#16; { size error }
  fhse_no_file = 16#17; { non-existent filename }
  fhse_eodir = 16#18; { end of directory }
  fhse_key_len = 16#19; { key length error }
  fhse_fab_len = 16#1A; { fab length error }
  fhse_def_vol = 16#1B; { default valume not defined on fetch default volume
                         request }
  fhse_not_ready = 16#1C; { file not ready to output }
  fhse_not_owner = 16#1D; { user number not owner or user 0 }

{
  Procedure to invoke the file handling service
}


procedure fhscall(var block: fhs_block; {parameter}
                  var status: shortint {resulting status from D0} );

{ Call the VERSAdos file handling system
}
  external;
{$list}
