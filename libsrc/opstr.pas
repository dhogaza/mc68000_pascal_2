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

  Pascal-2 string extensions : insert, delete, copy and pos

 Last modified by KRIS on 26-Nov-1990 13:54:44
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Pascal-2 string extensions : insert, delete, copy and pos'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

%include 'opcom';


procedure p_ins {(var source, target: univ str; targetlen, pos: shortint)} ;

{ Insert source string into target string at position pos.  If this
  makes target string longer than targetlen characters long, then
  only insert the numbers of characters that will fit.
  Concatenate source to destination if the position is beyond the
  current length of the target.
}

  var
    i, copylen, maxlen: StrIdx; {induction var}


  begin {p_ins}
    p_entlib;
    if (pos <= 0) or (pos > 255) then p_strovr;
    if (pos > length(target)) or (pos > targetlen) then
      pos := length(target) + 1;
    copylen := targetlen - pos + 1;
    if copylen > length(source) then copylen := length(source);
    maxlen := length(target) + copylen;
    if maxlen > targetlen then maxlen := targetlen;
    for i := maxlen downto pos + copylen do target[i] := target[i - copylen];
    for i := 1 to copylen do target[i + pos - 1] := source[i];
    target[0] := chr(maxlen);
    p_exitlb;
  end {p_ins} ;


procedure p_delstr {(var target: univ str; targetlen: shortint; pos,
                    num:integer)} ;

{ Delete num characters from target string starting at pos.
}

  var
    i: StrIdx;


  begin {p_delstr}
    p_entlib;
    if (pos < 1) or (pos > 255) then p_strovr;
    if pos <= length(target) then
      begin
      if pos + num > length(target) + 1 then num := length(target) - pos + 1;
      for i := pos + num to length(target) do target[i - num] := target[i];
      target[0] := chr(length(target) - num)
      end;
    p_exitlb;
  end {p_delstr} ;


function p_copy {(var source:univ str; pos, num:shortint): str} ;

{ Return string source[pos..pos + num].
}

  var
    temp: str; { We need a temp copy unfortunately do to inability to write
                "p_copy[anything]", p_copy being a function after all }
    i: StrIdx;


  begin {p_copy}
    p_entlib;
    if pos > length(source) then temp[0] := chr(0)
    else
      begin
      if num > length(source) - pos then num := length(source) - pos + 1;
      for i := 1 to num do temp[i] := source[i + pos - 1];
      temp[0] := chr(num);
      end;
    p_copy := temp;
    p_exitlb;
  end {p_copy} ;


function p_pos {(var pattern, source:univ str): StrIdx} ;

{ Return position of source string within target string.
}

  label
    99;

  var
    i, j, k: StrIdx;


  begin {p_pos}
    p_entlib;
    p_pos := 0;
    k := 0;
    while (length(source) - k >= length(pattern)) do
      begin
      i := 1;
      k := k + 1;
      j := k;
      while (i < length(pattern)) and (pattern[i] = source[j]) do
        begin
        i := i + 1;
        j := j + 1;
        end;
      if (i = length(pattern)) and (pattern[i] = source[j]) then
        begin
        p_pos := k;
        goto 99;
        end;
      end;
  99:
    p_exitlb;
  end {p_pos} ;
