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

  Pascal run time errors definition

 Last modified by KRIS on 26-Nov-1990 14:09:32
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$nolist}

type
  errortype = (noerr, { no error }
               panic, { unexpected condition }
               nomem, { out of memory }
               subscr, { subscript out of bounds }
               ovrflo, { floating point overflow }
               undrfl, { floating point underflow }
               divzer, { division by zero }
               numerr, { floating point format error }
               sqrter, { square root of a negative number }
               experr, { exp overflow }
               logerr, { log of zero or negative number }
               badsw, { bad switch }
               cntopn, { can't open file }
               defnam, { default file name syntax error }
               defswi, { default file switch error }
               filnam, { file name syntax error }
               isopn, { file already open }
               toomny, { too many files open }
               filend, { reading past end of file }
               geterr, { error reading file }
               puterr, { error writing file }
               intovr, { integer overflow }
               newof0, { new of zero length }
               ddeal, { double deallocation of dynamic memory }
               badint, { illegal value for integer }
               badset, { set element out of range }
               badptr, { reference through NIL or invalid pointer }
               fpperr, { floating point error }
               seek0, { seek to record 0 }
               notrnd, { not a random access file }
               trap10, { reserved instruction trap }
               notopn, { file not open }
               badver, { compiler/library mismatch -- recompile }
               nilptr, { attempted reference through a nil pointer }
               filerr, { file not open }
               stovfl, { stack overflow }
               subrng, { variable subrange exceeded }
               indrng, { array index out of bounds or variable subrange
                        exceeded }
               caserr, { case selector matches no case }
               disnil, { dispose of a nil pointer }
               baddis, { dispose of an invalid pointer }
               increc, { incomplete record at end of file }
               cntren, { can't rename file }
               cntdel, { can't delete file }
               strovr, { string overflow }
               lckerr, {cannot lock/unlock file}
               notinput, {no read access}
               notoutput, {no write access}
               initerr, {fatal initialization error}
               nofilememory, {not enough file buffer memory}
               putnoteof, {put() not at end of file}
               unknown, {unknown error}
               filenameinfo, {file name info error}
               fisyntax, {syntax error in floating point input}
               fioverflow, {overflow in floating point input}
               iisyntax, {syntax error in integer input}
               iioverflow, {overflow in integer input}
               dummy,
  { MC68881 floating point error messges }
               fpbsun, { branch or set on unordered cond. }
               fpinex, { inexact result }
               fpdz, { divide by zero }
               fpunfl, { underflow }
               fpoper, { operand error }
               fpovfl, { overflow }
               fpsnan { signalling not a number }
               );
