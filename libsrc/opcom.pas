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

  Pascal Support Library common definitions (part2)

 Last modified by KRIS on 26-Nov-1990 14:09:25
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$nolist}

type
  fieldwidth = integer;
  convstatus = (noerror, underflowerror, overflowerror, syntaxerror);
  str = string [255];
  stridx = 0..255;
  actualinteger = integer;


procedure p_entlib;
  external;


procedure p_exitlb;
  external;

{ input routines }


function p_rdi(var f: text): integer;
  external;


function p_rdi_i: integer;
  external;


function p_rdf(var f: text): real;
  external;


function p_rdf_i: real;
  external;


function p_rdd(var f: text): real;
  external;


function p_rdd_i: real;
  external;

{ conversion routines }


procedure p_itoc(procedure putf(ch: char); {receives chars of converted value}
                 e: integer; {the integer value to convert}
                 total_width: integer {total field width for conversion} );
  external;


procedure p_ftoc1(procedure putf(ch: char);
                  value: real;
                  total_width: fieldwidth);
  external;


procedure p_ftoc2(procedure putf(ch: char);
                  value: real;
                  total_width: fieldwidth;
                  FracDigits: integer);
  external;


procedure p_dtoc1(procedure putf(ch: char);
                  value: real;
                  total_width: fieldwidth);
  external;


procedure p_dtoc2(procedure putf(ch: char);
                  value: real;
                  total_width: fieldwidth;
                  FracDigits: integer);
  external;


procedure p_ctoi(function nextch: char;
                 var val: integer;
                 var err: convstatus);
  external;


procedure p_ctof(function nextch: char;
                 var val: real;
                 var err: convstatus);
  external;


procedure p_ctod(function nextch: char;
                 var val: real;
                 var err: convstatus);
  external;


procedure p_ins(var source, target: univ str;
                targetlen, pos: stridx);
  { Insert source string into target string at position pos.  If this
    makes target string longer than targetlen characters long, give
    string overflow error. }
  external;


procedure p_delstr(var target: univ str;
                   targetlen: stridx;
                   pos, num: integer);
 { Delete num characters from target string starting at pos. }
  external;


procedure p_stri0(value: integer;
                  var target: univ str;
                  targetlen: stridx);
{ Convert value storing result in target string, using default format.  If
  result is longer than targetlen characters give string overflow error. }
  external;


procedure p_stri1(value, fmt: integer;
                  var target: univ str;
                  targetlen: stridx);
{ Convert value storing result in target string, using explicit format.  If
  result is longer than targetlen characters give string overflow error. }
  external;


procedure p_strf0(value: real;
                  var target: univ str;
                  targetlen: stridx);
{ Convert value storing result in target string, using default format.  If
  result is longer than targetlen characters give string overflow error. }
  external;


procedure p_strf1(value: real;
                  fmt: integer;
                  var target: univ str;
                  targetlen: stridx);
{ Convert value storing result in target string, using single format argument.
  If result is longer than targetlen characters give string overflow error. }
  external;


procedure p_strf2(value: real;
                  fmt1, fmt2: integer;
                  var target: univ str;
                  targetlen: stridx);
{ Convert value storing result in target string.  If result is longer
  than targetlen characters give string overflow error. }

  external;


procedure p_strd0(value: real;
                  var target: univ str;
                  targetlen: stridx);
{ Convert value storing result in target string, using default format.  If
  result is longer than targetlen characters give string overflow error. }
  external;


procedure p_strd1(value: real;
                  fmt: integer;
                  var target: univ str;
                  targetlen: stridx);
{ Convert value storing result in target string, using single format argument.
  If result is longer than targetlen characters give string overflow error. }
  external;


procedure p_strd2(value: real;
                  fmt1, fmt2: integer;
                  var target: univ str;
                  targetlen: stridx);
{ Convert value storing result in target string.  If result is longer
  than targetlen characters give string overflow error. }
  external;


procedure p_vali(var source: univ str;
                 var value: actualinteger;
                 var errorpos: actualinteger);
{ Convert source string to binary value.  If there is an error, return
  the position of the error in errorpos, otherwise return zero. }
  external;


procedure p_valf(var source: univ str;
                 var value: real;
                 var errorpos: actualinteger);
{ Convert source string to binary value.  If there is an error, return
  the position of the error in errorpos, otherwise return zero. }
  external;


procedure p_vald(var source: univ str;
                 var value: real;
                 var errorpos: actualinteger);
{ Convert source string to binary value.  If there is an error, return
  the position of the error in errorpos, otherwise return zero. }
  external;


function p_copy(var source: univ str;
                pos, num: stridx): str;
 { Return string source[pos..pos + num]. }
  external;


function p_pos(var pattern, source: univ str): stridx;
 { Return position of source string within target string. }
  external;

{ error routines }


procedure p_rdfer;
  external;


procedure p_stxfer;
  external;


procedure p_rdier;
  external;


procedure p_stxier;
  external;


procedure p_strovr;
  external;


procedure p_seterr(var f: text;
                   errorcode: integer;
                   procedure errproc);
  external;
{$list}
