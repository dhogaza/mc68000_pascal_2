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

  Pascal-2 string extension routines for double floating point

 Last modified by KRIS on 26-Nov-1990 13:54:35
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Pascal-2 string extension routines for double floating point'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}
{$double}

%include 'opcom';


procedure p_strd0 {value: real; var target:univ str; targetlen: shortint} ;

{ Convert value storing result in target string.  If result is longer
  than targetlen characters give string overflow error.
}

  var
    i: StrIdx; {for stepping through target string}


  procedure putch(ch: char);

{ Put one char into target string.
}


    begin {putch}
      p_entlib;
      if i >= targetlen then p_strovr
      else
        begin
        i := i + 1;
        target[i] := ch;
        end;
      p_exitlb;
    end {putch} ;


  begin {p_strd0}
    p_entlib;
    i := 0;
    p_dtoc1(putch, value, 13);
    target[0] := chr(i);
    p_exitlb;
  end {p_strd0} ;


procedure p_strd1 {value: real; fmt: integer; var target:univ str; targetlen:
                   shortint} ;

{ Convert value storing result in target string.  If result is longer
  than targetlen characters give string overflow error.
}

  var
    i: StrIdx; {for stepping through target string}


  procedure putch(ch: char);

{ Put one char into target string.
}


    begin {putch}
      p_entlib;
      if i >= targetlen then p_strovr
      else
        begin
        i := i + 1;
        target[i] := ch;
        end;
      p_exitlb;
    end {putch} ;


  begin {p_strd1}
    p_entlib;
    i := 0;
    p_dtoc1(putch, value, fmt);
    target[0] := chr(i);
    p_exitlb;
  end {p_strd1} ;


procedure p_strd2 {value: real; fmt1, fmt2: integer; var target:univ str;
                   targetlen: shortint} ;

{ Convert value storing result in target string.  If result is longer
  than targetlen characters give string overflow error.
}

  var
    i: StrIdx; {for stepping through target string}


  procedure putch(ch: char);

{ Put one char into target string.
}


    begin {putch}
      p_entlib;
      if i >= targetlen then p_strovr
      else
        begin
        i := i + 1;
        target[i] := ch;
        end;
      p_exitlb;
    end {putch} ;


  begin {p_strd2}
    p_entlib;
    i := 0;
    p_dtoc2(putch, value, fmt1, fmt2);
    target[0] := chr(i);
    p_exitlb;
  end {p_strd2} ;


procedure p_vald {var source: univ str; var value: real; var errorpos: integer}
 ;

{ Convert source string to binary value.  If there is an error, return
  the position of the error in errorpos, otherwise return zero.
}

  var
    i: StrIdx;
    err: convstatus;


  function nextch: char;

{ Get next char of source string for library conversion procedure.
}


    begin {nextch}
      p_entlib;
      i := i + 1;
      if i > length(source) then nextch := ' '
      else nextch := source[i];
      p_exitlb;
    end {nextch} ;


  begin {p_vald}
    p_entlib;
    i := 0;
    value := 0.0;
    errorpos := 0;
    if length(source) > 0 then
      begin
      p_ctod(nextch, value, err);
      if (err = noerror) and (i > length(source)) then errorpos := 0
      else errorpos := i;
      end;
    p_exitlb;
  end {p_vald} ;
