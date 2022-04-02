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

  Double precision real input routine

 Last modified by KRIS on 26-Nov-1990 13:55:50
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Double precision real input routine'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback}

{
  p_rdd -- write(file, real, width, fracs)
  p_rdd_i -- write(real, width, fracs)
}
{$double}
%include opcom;
%include pasers;


function p_rdd_i;


  begin {p_rdd_i}
    p_entlib; {no walkback for this module}
    p_rdd_i := p_rdd(input);
    p_exitlb;
  end {p_rdd_i} ;


function p_rdd;

  var
    errorcode: convstatus;
    real_value: real;
    first_flag: boolean;


  function nextch: char;

{  This is the procedure from which p_ctof will take a character
   from input. Note: this procedures passes the character pointed
   by the file pointer without skipping to the next character}


    begin {nextch}
      p_entlib;
      if not first_flag then get(f)
      else first_flag := false;
      nextch := f^;
      p_exitlb;
    end {nextch} ;


  begin {p_rdd}
    p_entlib; {no walkback for this module}
    first_flag := true;
    p_ctod(nextch, real_value, errorcode); {let format the real}
    case errorcode of
      noerror: p_rdd := real_value;
      syntaxerror: p_seterr(f, ord(fisyntax), p_stxfer);
      underflowerror: p_rdd := 0.0;
      overflowerror: p_seterr(f, ord(fioverflow), p_rdfer);
      end { case errorcode } ;
    p_exitlb;
  end {p_rdd} ;
