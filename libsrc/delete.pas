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

  Delete an opened file

 Last modified by KRIS on 26-Nov-1990 14:04:28
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Delete an opened file'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

%include libtyp;
%include fhstyp;


procedure p_entlib;
  external;


procedure p_exitlb;
  external;


procedure p_close(var f_var: fdb_block_ptr);
  external;


procedure p_filerr;
  external;


procedure p_delete(var f: fdb_block_ptr);
  external;


function len(a: packed array [l..h: integer] of char): integer;

  var
    i: integer;


  begin {len}
    i := 0;
    while (a[i + 1] <> ' ') and (i < h) do i := i + 1;
    len := i;
  end {len} ;


procedure p_delete;

  var
    status: shortint; { buffer for status returned from fhscal }
    tmp_fhs: fhs_block; { file handling block used to perform delete }


  begin { p_delete }
    p_entlib;
    tmp_fhs := f^.f_fhs; { save the fhs block }
    p_close(f); { deleted file must be closed }
    with tmp_fhs do
      begin
      p_code := fh_fcmd; { device/file command }
      p_cmd := fh_del; { delete }
      p_status := 0;
      p_resv := 0;
      p_prot := 0;
      end { with tmp_fhs } ;
    fhscall(tmp_fhs, status);
    if status <> 0 then
      begin
      write('Can''t delete file ');
      with tmp_fhs do
        write(p_voln: len(p_voln), ':', p_usern: 1, '.', p_catnm: len(p_catnm),
              '.', p_filenm: len(p_filenm), '.', p_ext: len(p_ext));
      write(', status: ', status: - 2, '.');
      p_filerr;
      end;
    p_exitlb;
  end { p_delete } ;
