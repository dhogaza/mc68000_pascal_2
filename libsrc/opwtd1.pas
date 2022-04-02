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

  Double Precision Output Interface (one formatting parameter)

 Last modified by KRIS on 26-Nov-1990 13:55:57
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Double Precision Output Interface (one formatting parameter)'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback}
{$double}

{
  p_wtd1 -- write(file, real, width)
  p_wtd1_o -- write(real, width)
}

const
  real_buff_size = 13; {the default total size of converted real}

type
  buffer_type = packed array [1..real_buff_size] of char;


procedure p_dtoc1(procedure putf(ch: char);
                  val: real;
                  width: integer);
  external;


procedure p_wts(var f: text;
                var buffer: buffer_type; {defines the address of the string}
                st_len, st_width: integer);
  external;


procedure p_entlib;
  external;


procedure p_exitlb;
  external;


procedure p_wtd1(var f: text;
                 val: real;
                 width: integer);
  external;


procedure p_wtd1_o(val: real;
                   width: integer);
  external;


procedure p_wtd1_o;


  begin {p_wtd1_o}
    p_entlib; {no walkback for this module}
    p_wtd1(output, val, width);
    p_exitlb;
  end {p_wtd1_o} ;


procedure p_wtd1;

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

{  This is the procedure to which p_dtoc1 will pass a character
   for output.  Characters are buffered up and the string output
   routine is used instead of single character i/o. The buffer
   is defined as a local variable on the p_wtd1's stack space.}


    begin {putch}
      p_entlib; {no walkback for this module}
      position := position + 1;
      real_buffer[position] := ch;
      if position = real_buff_size then {buffer is full} flush_buffer; {flush
        it and clear counter/pointer}
      p_exitlb;
    end {putch} ;


  begin {p_wtd1}
    p_entlib; {no walkback for this module}
    position := 0;
    p_dtoc1(putch, val, width); {let format the real}
    if position <> 0 then {is there anything into the buffer?} flush_buffer;
    p_exitlb;
  end {p_wtd1} ;
