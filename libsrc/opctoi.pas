{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1983, 1984, 1985, 1986, 1987, 1988,
                1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Char to integer conversion routine

 Last modified by KRIS on 26-Nov-1990 14:05:43
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$nomain,section=8,version=0045,ident='Char to integer conversion routine'}
{$nopointercheck,norangecheck,noindexcheck,nowalkback}

%include 'opcom';

const
  byteswapping = false; { true if byte numbering reversed (ala pdp/11) }
  maxbits = 32; { number of bits in target integer }
  maxbytes = 4; { number of bytes in target integer }
  minradix = 2; { minimum non-decimal radix }
  maxradix = 16; { maximum non-decimal radix }
  maxdigit = 15; { largest ordinal value for single digit }
  binbase = 256; { binary base for conversions }
  maxbyte = 255; { largest unsigned value of byte }
  maxproduct = 4095; { maxradix * binbase - 1 }

type
  byte = 0..maxbyte;
  digit = 0..maxdigit;
  digitrange = 0..maxbits;
  longint = packed array [1..maxbytes] of byte;
  radixtype = minradix..maxradix;


procedure p_ctoi {(function nextch: char; var result: integer; var err:
                  convstatus)} ;

  var
    radix: radixtype; { conversion radix (minimum value = 2) }
    chvalue: digit; { ordinal value of current character }
    digits: array [digitrange] of byte; { digit buffer }
    head: digitrange; { index of high order of binary result }
    fill: digitrange; { index of low order of binary result }
    i, j: digitrange; { induction variables into digits array }
    negative: boolean; { true if minus sign encountered }
    signed: boolean; { true if '+' or '-' preceeded digit string }
    scanning: boolean; { true while scanning legal digit string }
    valid: boolean; { true if numeric syntax is correct }
    overflow: boolean; { true if too much significance for destination }
    carry: byte;
    product: 0..maxproduct;
    value: longint; { used to loophole the resultant value }
    ch: char;


  begin { p_ctoi }
    p_entlib; {no walkback for this module}
    radix := 10;
    overflow := false;
    ch := nextch;
    while (ch = ' ') do ch := nextch;
    negative := (ch = '-');
    signed := negative or (ch = '+');
    if signed then ch := nextch
    else signed := true; {all decimal values are signed}
    valid := (ch = '0');
    while (ch = '0') do ch := nextch;

    {---  fill the digit buffer  ---}

    fill := 0;
    while (ch >= '0') and (ch <= '9') do
      begin
      valid := true;
      if fill < maxbits then fill := fill + 1
      else overflow := true;
      digits[fill] := ord(ch) - ord('0');
      ch := nextch
      end;


    {---  check for special radices  ---}

    if (ch = 'b') or (ch = 'B') then
      begin {special octal input}
      ch := nextch;
      signed := false;
      radix := 8;
      for i := 1 to fill do
        if digits[i] >= 8 then valid := false
      end {special octal input}
    else if ch = '#' then
      begin {special non-decimal input}
      ch := nextch;
      signed := false;
      if (fill = 1) and (digits[1] >= minradix) then radix := digits[1]
      else if (fill = 2) and ((digits[1] * 10 + digits[2]) <= maxradix) then
        radix := digits[1] * 10 + digits[2]
      else valid := false;
      fill := 0;
      scanning := true;
      while scanning do
        begin
        if (ch >= '0') and (ch <= '9') then chvalue := ord(ch) - ord('0')
        else if (ch >= 'A') and (ch <= 'F') then
          chvalue := ord(ch) - ord('A') + 10
        else if (ch >= 'a') and (ch <= 'f') then
          chvalue := ord(ch) - ord('a') + 10
        else scanning := false;
        if scanning then
          begin
          if (chvalue < radix) then
            begin
            if fill < maxbits then fill := fill + 1
            else overflow := true;
            digits[fill] := chvalue;
            end
          else valid := false; { radix error }
          ch := nextch; { continue scanning whether valid or not }
          end;
        end { while scanning } ;
      if fill = 0 then valid := false;
      end {special non-decimal input} ;


    {---  perform conversion  ---}

    if valid and (fill = 0) then negative := false; { disallow -0 }
    if valid then
      begin
      head := 1;
      for i := 1 to fill - 1 do
        begin
        carry := digits[i + 1];
        for j := i downto head do
          begin
          product := digits[j] * radix + carry;
          digits[j + 1] := product mod binbase;
          carry := product div binbase;
          end; { for j }
        if carry = 0 then head := head + 1
        else digits[head] := carry;
        end; { for i }
      if negative then
        begin
        j := fill;
        while digits[j] = 0 do j := j - 1;
        digits[j] := maxbyte + 1 - digits[j];
        for i := head to j - 1 do digits[i] := maxbyte - digits[i];
        end { negation } ;
      if (fill - head + 1 > maxbytes) or signed and
         (fill - head + 1 = maxbytes) and (not negative and
         (digits[head] >= binbase div 2) or negative and
         (digits[head] < binbase div 2)) then
        overflow := true;
      end {if valid } ;

    {---  transfer bytes to result  ---}

    for i := 1 to maxbytes do { sign extend the receiving field }
      if not valid or (overflow = negative) then value[i] := 0
      else {valid and (overflow <> negative} value[i] := maxbyte;
    if overflow then {complete conversion to (+maxint) or (-maxint-1) }
      value[1 + ord(byteswapping)] := (maxbyte div 2) + ord(negative);
    j := maxbytes; { an index into "value" array }
    if valid and not overflow then {transfer}
      for i := fill downto head do
        begin
        if byteswapping then
          if odd(j) then value[j + 1] := digits[i]
          else value[j - 1] := digits[i]
        else value[j] := digits[i];
        j := j - 1
        end;
    val := loophole(integer, value);
    if not valid then err := syntaxerror
    else if overflow then err := overflowerror
    else err := noerror;
    p_exitlb;
  end { p_ctoi } ;
