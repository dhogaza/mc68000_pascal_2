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

  Double float to char conversion routine

 Last modified by KRIS on 26-Nov-1990 13:56:10
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Double float to char conversion routine'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}
{$double}

%include opcom;

const
  decbase = 10;
  binbase = 256;
  maxproduct = 2559;

  hostintsize = 2;
  maxexpon = + 308;
  maxexpm1 = + 309; {required for certain data declarations}
  minexpon = - 309;
  minexpm1 = - 310; {required for certain data declarations}

  ExpDigits = 3; {implementation-defined number of decimal digits in exponent}
  maxbitlength = 1074; { fraction with 125 leading 0's + 24 significant bits }
  maxbinfrac = 136; {maximum number of "hosthalfword" required to contain the
                     longest real fraction. (Maxbitlength + 7) div 8. }
  bitsperbyte = 8;
  maxrealwords = 4; {number of host unsigned integers per target real}
  totalfracbits = 53; {number of bits in significand including implied bit}
  totalfracbytes = 7; {(totalfracbits + bitsperbyte - 1) div bitsperbyte}
  impliedbit = 16; {added into high order byte if non zero exponent}
  exponbias = 1023; {target representation exponent bias}
  pointoffset = 1; {position of binary point in high order byte (in bits from
                    leftmost bit)}

type
  digit = 0..9;
  hosthalfword = 0..255;
  unsigned = 0..65535;

  reel =
    packed record { M68000 packing order }
      s: 0..1; { sign bit }
      e: 0..2047; { exponent }
      f1: 0..15; { high order fraction }

      f: packed array [2..maxrealwords] of 0..65535;
    end;

  realblock = { a structure containing a decimal integer stack and a variable
               length binary fraction }
    record
      digitctr: minexpm1..maxexpm1; {counts integer digits if greater than
                                     zero, and leading zeroes if negative}
      decstring: packed array [1..maxexpm1] of char; {integer digit stack}
      binlen: 0..maxbinfrac; {length of remaining binary fraction}
      binfrac: packed array [1..maxbinfrac] of hosthalfword;
    end;



procedure scale(var rb: realblock;
                e: real;
                fraclimit: integer);

{ this procedure accepts a real number, unpacks it into a multiprecision
  binary string with binary point alignment on a "hosthalfword" boundary
  (inserting leading zeros before small fractions or appending trailing
  zeroes after large integers, as required), and finally converts any
  integer portion to ascii digits in the "decstring" field of a "realblock." 
  The length of the converted string is recorded in "digitctr", which will
  be less than 0 if no integer part exists.  The fractional residual is
  left in "binfrac" awaiting further conversion by "getnextdigit."
}

  var
    x: reel; {variable for loopholing a real into its constituent parts}
    shift: - bitsperbyte.. + bitsperbyte; {left shift count to align binary
                                           point}
    head, tail, point: 0..maxbinfrac;
    temp: unsigned;
    leadingzeroes: integer;
    i: integer;
    j: unsigned; {induction vars}
    carry: unsigned;
    product: unsigned; {0..maxproduct;}
    less_than_one: boolean;
    scaling: boolean;


  begin {scale}
    p_entlib;
    with rb do
      begin

      x := loophole(reel, e);

      if (x.e = 0) and (x.f1 = 0) and (x.f[2] = 0) and (x.f[4] = 0) and
         (x.f[4] = 0) then
        begin
        leadingzeroes := 0;
        shift := 0;
        end
      else
        begin
        leadingzeroes := x.e - (exponbias - pointoffset) - ord(x.e = 0);
        shift := (leadingzeroes + 3) mod bitsperbyte;
        if shift < 0 then shift := shift + bitsperbyte;
        end;

      point := 0;
      tail := ord(shift > 3);
      less_than_one := (leadingzeroes <= 0);

      {- - -  insert leading bytes of zero if no integer part exists  - - -}

      if less_than_one then {there is NO integer part}
        for i := 1 to ( - leadingzeroes) div bitsperbyte do
          begin
          tail := tail + 1;
          binfrac[tail] := 0;
          end

      else { there IS an integer part }
        point := (leadingzeroes + bitsperbyte - 1) div bitsperbyte;

      {- - -  unpack the mantissa into a byte string in "binfracs"  - - -}

      tail := tail + 1;
      head := tail; {marks the first byte of significance}
      binfrac[head] := x.f1;

      for i := 2 to maxrealwords do
        begin
        temp := x.f[i];
        for j := hostintsize downto 1 do
          begin
          binfrac[tail + j] := temp mod binbase;
          temp := temp div binbase;
          end; {for j}
        tail := tail + hostintsize;
        end; {for i}

      if x.e <> 0 then {set implied bit if non-zero exponent}
        binfrac[head] := binfrac[head] + impliedbit
      else {scan for start of denormal significance}
        while (binfrac[head] = 0) and (head < tail) do head := head + 1;

      {- - -  append trailing zeroes if no fractional part exists  - - -}

      while tail < point do
        begin
        tail := tail + 1;
        binfrac[tail] := 0;
        end {trailing zeroes} ;

{- - -  shift the fraction left to align binary point on a byte boundary - - -}

      if shift > 0 then
        begin {only perform shift if necessary}

        j := 1;
        repeat
          j := j + j;
          shift := shift - 1;
        until shift = 0;

        carry := 0;

        for i := head + totalfracbytes - 1 downto head do
          begin
          product := binfrac[i] * j + carry;
          binfrac[i] := product mod binbase;
          carry := product div binbase;
          end;

        if carry <> 0 then
          begin
          head := head - 1; {bump overflow into another byte}
          binfrac[head] := carry;
          end; {overflow adjustment}
        end; { of left shifting }

      {- - -  convert integer part, if any  - - -}

      digitctr := 0;

      while head <= point do
        begin {integer part exists}
        carry := 0;
        for i := head to point do
          begin
          product := carry * binbase + binfrac[i];
          binfrac[i] := product div decbase;
          carry := product mod decbase;
          end {for i} ;

        digitctr := digitctr + 1;
        decstring[digitctr] := chr(carry + ord('0'));
        if binfrac[head] = 0 then head := head + 1;
        end; {integer reduction}

      {- - -  integer conversion complete, move fraction left  - - -}

      binlen := 0;
      for i := point + 1 to tail do
        begin
        binlen := binlen + 1;
        binfrac[binlen] := binfrac[i];
        end {for i} ;

      {- - -  scale up a fraction which had no integer part  - - -}

      if less_than_one then
        begin
        scaling := true;
        digitctr := - 1;
        decstring[1] := '0';

        if (x.e <> 0) or (x.f1 <> 0) or (x.f[2] <> 0) or (x.f[4] <> 0) or
           (x.f[4] <> 0) then
          while scaling and (fraclimit < digitctr) do
            begin

            digitctr := digitctr - 1;
            carry := 0;

            for i := tail downto head do
              begin
              product := binfrac[i] * decbase + carry;
              binfrac[i] := product mod binbase;
              carry := product div binbase;
              end {for i} ;

            if (tail <> 0) and (binfrac[tail] = 0) then tail := tail - 1;

            if carry <> 0 then
              if head > 1 then
                begin {move MSB marker}
                head := head - 1;
                binfrac[head] := carry;
                end
              else
                begin {the fraction has now been scaled}
                binlen := tail;
                decstring[1] := chr(carry + ord('0'));
                scaling := false;
                end {non-zero carry} ;
            end {while} ;

        end; {point = 0}

      end {with rb} ;
    p_exitlb;
  end {scale} ;




function getnextdigit(var rb: realblock): char;

  var
    carry: digit;
    product: 0..maxproduct;
    i: 0..maxbinfrac;


  begin {getnextdigit}
    p_entlib;
    with rb do
      begin
      if digitctr < 0 then
        begin
        digitctr := digitctr + 1;
        if digitctr = 0 then getnextdigit := decstring[1]
        else getnextdigit := '0';
        end

      else if digitctr > 0 then
        begin
        getnextdigit := decstring[digitctr];
        digitctr := digitctr - 1;
        end

      else {digitctr = 0}
      if binlen = 0 then getnextdigit := '0'

      else
        begin {crank on the fraction to get another digit}
        carry := 0;

        for i := binlen downto 1 do
          begin
          product := binfrac[i] * decbase + carry;
          binfrac[i] := product mod binbase;
          carry := product div binbase;
          end {for i} ;

        if (binlen <> 0) and (binfrac[binlen] = 0) then binlen := binlen - 1;

        getnextdigit := chr(carry + ord('0'));
        end; {fraction cranking}
      end {with rb} ;
    p_exitlb;
  end {getnextdigit} ;




procedure p_dtoc1 { ( { procedure putf(ch: char); {receives chars of converted
                   value}
 {    value     : real;         {the real value to convert}
 {    total_width: integer;      {total field width for conversion}
 {                  ) } ;

  var
    prevch: char;
    ch: char;
    firstdigit: boolean;
    ninectr: integer;
    ExpValue: minexpon..maxexpon;
    rb: realblock;
    i: integer; {induction var}
    x: reel; { loophole var for real comparison }


  procedure putch(ch: char;
                  fill: char);


    begin
      p_entlib;
      putf(ch);

      if firstdigit then
        begin
        putf('.');
        firstdigit := false;
        end;

      while ninectr > 0 do
        begin
        putf(fill);
        ninectr := ninectr - 1;
        end;
      p_exitlb;
    end {putch} ;




  begin {p_dtoc1}
    p_entlib;
    x := loophole(reel, value); { to access sign bit without fpp comparison }

    if x.s <> 0 then putf('-')
    else putf(' ');

    if total_width <= ExpDigits + 6 then total_width := ExpDigits + 6;

    scale(rb, value, minexpm1); {get at least one digit}

    with rb do
      if digitctr > 0 then ExpValue := digitctr - 1
      else
        begin
        ExpValue := digitctr + 1;
        {force "getnextdigit" to omit leading zeroes}
        digitctr := - 1;
        end;

    prevch := getnextdigit(rb); {could be a nine!}
    firstdigit := true;
    ninectr := 0;

    for i := 1 to total_width - ExpDigits - 5 do
      begin
      ch := getnextdigit(rb);
      if ch = '9' then ninectr := ninectr + 1
      else
        begin {output 1 or more chars}
        putch(prevch, '9'); {its value won't change}
        prevch := ch;
        end;
      end {for i} ;

    if getnextdigit(rb) >= '5' then
      begin {round up}
      if prevch = '9' then
        begin {decimal overflow}
        ExpValue := ExpValue + 1;
        prevch := '0';
        end {decimal overflow} ;
      putch(succ(prevch), '0');
      end {round up}

    else {no rounding} putch(prevch, '9');

    putf('e');
    if x.e = 2047 then
      begin
      putf('+');
      putf('?');
      putf('?');
      putf('?');
      end
    else
      begin
      if ExpValue < 0 then putf('-')
      else putf('+');
      {representation dependent reduction (2 digits)}
      putf(chr(abs(ExpValue) div 100 + ord('0')));
      putf(chr(abs(ExpValue) div 10 mod 10 + ord('0')));
      putf(chr(abs(ExpValue) mod 10 + ord('0')));
      end;
    p_exitlb;
  end {p_dtoc1} ;




procedure p_dtoc2 { ( { procedure putf(ch: char); {receives chars of converted
                   value}
 {    value     : real;         {the real value to convert}
 {    total_width: integer;      {total field width for conversion}
 {    FracDigits: integer       {# of digits to right of dec. pt.}
 {                  ) } ;

  const
    space = ' ';
    zero = '0';
    one = '1';
    five = '5';
    nine = '9';

  var
    x: reel; {loophole the real}
    rb: realblock;
    ExpValue: minexpon..maxexpon;
    Spaces: integer;
    ch, prevch: char;
    decptctr: integer;
    ninectr: integer; {number of leading nines}
    zeroctr: integer; {number of leading zeroes}
    firstdigit, spacepending, lastdigit: boolean;
    i: integer; {induction var for number of digits to generate}


  procedure putch(ch: char;
                  fill: char);


    begin
      p_entlib;
      if spacepending then
        begin
        if ch <> one then putf(space);
        spacepending := false;
        end;

      if firstdigit then
        begin
        if x.s = 1 then
          begin
          if (zeroctr > 0) and lastdigit then putf(space)
          else putf('-');
          end {if x.s = 1} ;

        firstdigit := false;
        end;

      while zeroctr > 0 do
        begin
        zeroctr := zeroctr - 1;
        decptctr := decptctr - 1;
        if decptctr = 0 then putf('.');
        putf(zero);
        end {while zeroctr > 0} ;
      zeroctr := - 1;

      decptctr := decptctr - 1;
      if decptctr = 0 then putf('.');
      putf(ch);

      while ninectr > 0 do
        begin
        decptctr := decptctr - 1;
        if decptctr = 0 then putf('.');
        putf(fill);
        ninectr := ninectr - 1;
        end;
      p_exitlb;
    end {putch} ;




  begin {p_dtoc2}
    p_entlib;
    x := loophole(reel, value); { to access sign bit without fpp comparison }

    firstdigit := true;
    lastdigit := false;
    spacepending := false;
    zeroctr := 0;

    if total_width <= 0 then {the Standard says "it shall be an error"}
      total_width := 1; {but we'll retain compatibility with 2.0 for now}

    if FracDigits < 0 then {the Standard says "it shall be an error"}
    else if x.e = 2047 then
      begin
      {that's a nan - write question marks}
      putf(space);
      for i := 2 to total_width do
        if i = total_width - FracDigits then putf('.')
        else putf('?');
      end
    else
      with rb do
        begin {FracDigits >= 0, fixed point notation}

        scale(rb, value, - FracDigits - 2); {stop at significance or
                                             fracdigits}

        if digitctr > 0 then decptctr := digitctr + 1
        else decptctr := 2; {number of digits to left of decimal point}

        prevch := getnextdigit(rb); {could be a nine!}

        Spaces := total_width - FracDigits - decptctr - x.s;

        if FracDigits = 0 then {the Standard says "it shall be an error"}
          Spaces := Spaces + 1; {because decimal point will be suppressed}

        if (Spaces > 0) and (prevch = nine) then
          begin
          spacepending := true;
          Spaces := Spaces - 1;
          end;

        while Spaces > 0 do
          begin
          putf(space);
          Spaces := Spaces - 1;
          end;

        {- - -  commence output of digits  - - -}
        {do not print leading zeroes to avoid displaying the sign.}
        ninectr := 0;
        for i := 2 to (decptctr - 1) + FracDigits do
          begin
          {we start with 2 because we already have the 1st digit}
          {decptctr is decremented to omit the sign position}
          ch := getnextdigit(rb);

          if (prevch = zero) and (ch = zero) and (zeroctr >= 0) then
            begin
            {zeroctr >= 0 means that only zeroes has been received}
            zeroctr := zeroctr + 1;
            prevch := ch;
            end
          else if ch = nine then
            begin
            {the efective convert value is not zero, so flush zeroes if any}
            zeroctr := zeroctr - 1;
            if zeroctr >= 0 then putch(prevch, zero); {flush all zeroes, but
                                                        not last one}
            ninectr := ninectr + 1;
            end
          else
            begin {output 1 or more chars}
            putch(prevch, nine); {its value won't change}
            prevch := ch;
            end;
          end {for i} ;

        {- - -  rounding algorithm  - - -}

        lastdigit := true; {tell the putch-routine it's called last time}
        if getnextdigit(rb) >= five then
          begin {round up}
          if prevch = nine then
            begin {decimal overflow}
            decptctr := decptctr + 1;
            ninectr := ninectr + 1;
            prevch := zero;
            end {decimal overflow} ;

          putch(succ(prevch), zero);
          end {round up}

        else
          begin {no rounding}
          putch(prevch, nine);
          end {no rounding} ;

        end {fixed point notation} ;
    p_exitlb;
  end {p_dtoc2} ;
