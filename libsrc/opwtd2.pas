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

  Double Precision Output Interface (two formatting parameters)

 Last modified by KRIS on 26-Nov-1990 13:56:04
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Double Precision Output Interface (two formatting parameters)'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback}
{$double}

{
  p_wtd1 -- write(file, real, width, fract)
  p_wtd1_o -- write(real, width, fract)
}

const
  real_buff_size = 13; {the default total size of converted real}

type
  buffer_type = packed array [1..real_buff_size] of char;


procedure p_dtoc2(procedure putf(ch: char);
                  val: real;
                  width: integer;
                  fract: integer);
  external;


procedure p_wts(var f: text;
                var buffer: buffer_type; {defines the address of the string}
                st_len, st_width: integer);
  external;


procedure p_entlib;
  external;


procedure p_exitlb;
  external;


procedure p_wtd2(var f: text;
                 val: real;
                 width: integer;
                 fract: integer);
  external;


procedure p_wtd2_o(val: real;
                   width: integer;
                   fract: integer);
  external;


procedure p_wtd2_o;


  begin {p_wtd2_o}
    p_entlib; {no walkback for this module}
    p_wtd2(output, val, width, fract);
    p_exitlb;
  end {p_wtd2_o} ;


procedure p_wtd2;

  var
    real_buffer: buffer_type;
    position: integer;


  procedure flush_buffer;


    begin
      p_exitlb;
      p_wts(f, real_buffer, position, position); {write the buffer}
      position := 0; {and reset counter}
      p_entlib; {no walkback for this module}
    end;


  procedure putch(ch: char);

{  This is the procedure to which p_dtoc2 will pass a character
   for output.  Characters are buffered up and the string output
   routine is used instead of single character i/o. The buffer
   is defined as a local variable on the p_wtd2's stack space.}


    begin {putch}
      p_entlib; {no walkback for this module}
      position := position + 1;
      real_buffer[position] := ch;
      if position = real_buff_size then {buffer is full} flush_buffer; {flush
        it and clear counter/pointer}
      p_exitlb;
    end {putch} ;


  begin {p_wtd2}
    p_entlib; {no walkback for this module}
    position := 0;
    p_dtoc2(putch, val, width, fract); {let format the real}
    if position <> 0 then {is there anything into the buffer?} flush_buffer;
    p_exitlb;
  end {p_wtd2} ;
