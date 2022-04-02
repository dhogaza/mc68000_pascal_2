{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1985, 1986, 1987, 1988, 1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Write an integer

 Last modified by KRIS on 26-Nov-1990 14:05:40
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Write an integer'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback}

%include opcom;

{
  p_wti -- write(file, integer, width)
  p_wti_o -- write(integer, width)
}

const
  integer_buff_size = 12; {the default total size of converted integer}

type
  buffer_type = packed array [1..integer_buff_size] of char;


procedure p_wts(var f: text;
                var buffer: buffer_type; {defines the address of the string}
                st_len, st_width: integer);
  external;


procedure p_wti(var f: text;
                val: integer;
                width: integer);
  external;


procedure p_wti_o(val: integer;
                  width: integer);
  external;


procedure p_wti_o;


  begin {p_wti_o}
    p_entlib; {no walkback for this module}
    p_wti(output, val, width);
    p_exitlb;
  end {p_wti_o} ;


procedure p_wti;

  var
    integer_buffer: buffer_type;
    position: integer;


  procedure flush_buffer;


    begin
      p_entlib; {no walkback for this module}
      p_wts(f, integer_buffer, position, position); {write the buffer}
      position := 0; {and reset counter}
      p_exitlb;
    end;


  procedure putch(ch: char);

{  This is the procedure to which p_itoc will pass a character
   for output.  Characters are buffered up and the string output
   routine is used instead of single character i/o. The buffer
   is defined as a local variable on the p_wti's stack space.}


    begin {putch}
      p_entlib; {no walkback for this module}
      position := position + 1;
      integer_buffer[position] := ch;
      if position = integer_buff_size then {buffer is full} flush_buffer;
      {flush it and clear counter/pointer}
      p_exitlb;
    end {putch} ;


  begin {p_wti}
    p_entlib; {no walkback for this module}
    position := 0;
    p_itoc(putch, val, width); {let format the integer}
    if position <> 0 then {is there anything into the buffer?} flush_buffer;
    p_exitlb;
  end {p_wti} ;
