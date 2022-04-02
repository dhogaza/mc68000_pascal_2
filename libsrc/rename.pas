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

  Rename an opened file

 Last modified by KRIS on 26-Nov-1990 14:04:24
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=2.1A}
{$ident='Rename an opened file'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback}

%include libtyp;
%include fhstyp;

const
  string_len = 132;

type
  p_string = packed array [1..string_len] of char;
  stringindex = integer;


procedure p_entlib;
  external;


procedure p_exitlb;
  external;


procedure p_rename(var f: fdb_block_ptr;
                   var fname: p_string; { file name }
                   flength: stringindex); { length of fname }
  external;


procedure p_fdefau(var block: fhs_block);
  external;

{ The procedure p_parse is expecting fname as conformant array.  We are
  going to fake it using different definition of parameter list }


procedure p_parse(var fname: p_string; {file name}
                  flo, fhi: integer;
                  {var fname: packed array [flo..fhi: integer] of char}
                  var result: fhs_block; {result block}
                  var errfound: boolean {something wrong}
                  );
  external;


procedure p_rename;

  var
    status: shortint; { buffer for status returned from fhscal }
    bad_name: boolean; { flag set by p_parse when bad name detected }
    prot_buff: shortint; { buffer for write/read protection code }
    ready_to_exit: boolean;
    tmp_fhs: fhs_block; { file handling block used to perform rename }
    tmp_f: text; { file variable used to delete existing file }
    tmp_name: p_string; { file name used to delete existing file }
    i: integer;


  begin { p_rename }
    p_entlib;
    with f^.f_fhs do
      begin
      prot_buff := p_prot; { save write/read protection }
      p_opt := fh_erw; { exclusive read/write is necessary for rename }
      p_code := fh_fcmd; { device/file command }
      p_cmd := fh_cap; { change access permissions }
      end { with f^.f_fhs } ;
    fhscall(f^.f_fhs, status);
    tmp_fhs := f^.f_fhs;
    p_fdefau(tmp_fhs);
    p_parse(fname, 1, flength, tmp_fhs, bad_name); { parse new name and load
                                                    fhs fields }
    ready_to_exit := false;
    repeat
      with tmp_fhs do
        begin
        p_code := fh_fcmd; { device/file command }
        p_cmd := fh_ren; { rename }
        p_lun := f^.f_fhs.p_lun; { lun used to assign opend file }
        end { with tmp_fhs } ;
      fhscall(tmp_fhs, status); { rename the file }
      if status = fhse_dupl_nam then { file already exists ? }
        begin
        for i := 1 to flength do { copy file name } tmp_name[i] := fname[i];
        for i := flength + 1 to string_len do tmp_name[i] := ' '; { clean up
          not used characters in file name }
        reset(tmp_f, tmp_name); { open the file }
        delete(tmp_f); { and delete it }
        end
      else ready_to_exit := true;
    until ready_to_exit;
    p_exitlb;
  end { p_rename } ;
