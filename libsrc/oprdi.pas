{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1987, 1988, 1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Pascal-2 integer input routines

 Last modified by KRIS on 26-Nov-1990 14:05:36
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}
{$section=8,version=0045}
{$ident='Pascal-2 integer input routines'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback}

%include opcom;
%include pasers;
{
  p_rdi -- write(file, integer, width, fracs)
  p_rdi_i -- write(integer, width, fracs)
}


function p_rdi_i;


  begin {p_rdi_i}
    p_entlib; {no walkback for this module}
    p_rdi_i := p_rdi(input);
    p_exitlb;
  end {p_rdi_i} ;


function p_rdi;

  var
    errcode: convstatus;
    integer_value: integer;
    first_flag: boolean;


  function nextch: char;

{  This is the procedure from which p_ctoi will take a character
   from input. Note: this procedures passes the character pointed
   by the file pointer without skipping to the next character}


    begin {nextch}
      p_entlib;
      if not first_flag then get(f)
      else first_flag := false;
      nextch := f^;
      p_exitlb;
    end {nextch} ;


  begin {p_rdi}
    p_entlib; {no walkback for this module}
    first_flag := true;
    p_ctoi(nextch, integer_value, errcode); {convert the string}
    case errcode of
      noerror: p_rdi := integer_value;
      syntaxerror: p_seterr(f, ord(iisyntax), p_stxier);
      underflowerror: p_rdi := 0; {should never happend}
      overflowerror: p_seterr(f, ord(iioverflow), p_rdier);
      end { case errcode } ;
    p_exitlb;
  end {p_rdi} ;
