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

  Char to Double Float convertion routine

 Last modified by KRIS on 26-Nov-1990 13:56:22
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Char to Double Float convertion routine'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback [b+]}

{$double}
%include opcom;


procedure p_ctod {(function nextch: char; var val: real; var err: realstatus)}
 ;

  const
    decbase = 10; {decimal base for conversions}
    binbase = 256; {binary base for conversions}
    maxproduct = 4095;

    radix = 10; {conversion radix for doubles}
    maxdigit = 9; {largest digit for given radix}

    bytesize = 256; {number of elements in target byte}
    maxbytevalue = 255; {greatest single value in byte}
    maxwordvalue = 65535; {greatest single value in word}
    maxinbuf = 310; {buffer up to entire line length of digits}
    maxusint = 65535; {largest unsigned (16-bit) integer}

    space = ' ';
    ordtab = 9;
    plus = '+';
    minus = '-';
    zero = '0';
    nine = '9';
    point = '.';
    largeE = 'E';
    smalle = 'e';

  type
    digitindex = 0..maxinbuf;
    unsigned = 0..maxusint;
    byte = 0..maxbytevalue;
    word = 0..maxwordvalue;

  var
    ch: char; {actual character read}
    digits: array [digitindex] of byte; {actual digits read}
    length: digitindex; {length of number read}
    leadingzeros: digitindex; {number of leading zeros (for real fraction)}
    fill: digitindex; {index used to fill the digit array}
    negative: boolean;


  procedure whiteout {skip leading whitespace} ;


    begin
      p_entlib;
      ch := nextch;
      while ch in [space, chr(ordtab)] do ch := nextch;
      p_exitlb;
    end;


  procedure readsign {read an optional initial sign} ;


    begin
      p_entlib;
      negative := false;
      if ch = minus then
        begin
        negative := true;
        ch := nextch;
        end
      else if ch = plus then ch := nextch;
      p_exitlb;
    end;


  procedure readdigits(skipzeros: boolean {skip and count leading zeros } );

{ Read digits into the digit array.  If "skipzeros" is true, then leading
  zeros are not significant.  They are counted in "leadingzeros" rather
  than inserted into the digit array.  Otherwise, all digit characters are
  converted to numerics and inserted into "digits" beginning at "fill" + 1.
}

    var
      digit: 0..maxdigit; {buffers the numeric value of "ch"}
      scanning: boolean;


    begin {readdigits}
      p_entlib;
      scanning := true;
      leadingzeros := 0;
      length := 0;

      while skipzeros and (ch = zero) do
        begin {scan leading zeros}
        leadingzeros := leadingzeros + 1;
        ch := nextch;
        end;

      while scanning do
        begin
        if (ch >= zero) and (ch <= nine) then digit := ord(ch) - ord(zero)
        else scanning := false;

        if scanning then
          begin
          length := length + 1;
          fill := fill + 1;
          digits[fill] := digit;
          ch := nextch;
          end
        end; {while}

      if length + leadingzeros = 0 then err := syntaxerror;
      p_exitlb;
    end {readdigits} ;

{ Scan a real number.
}


  procedure convertreal(var value1: real {resulting value} );

{ Interpret the digits read so far as the first part of a real number,
  scan the rest of the number, and return the real value.
}

    label
      1;

    const

      MaxDecExpDouble = 309;
      MinDecExpDouble = - 309;

      MaxBinExpDouble = 1023;
      MinBinExpDouble = - 1022;

      ExpBiasDouble = 1023;

      FracBitsDouble = 53;
      MaxFracBits = FracBitsDouble;

      FracBytesDouble = 7;
      MaxFracBytes = FracBytesDouble;

      maxexpval = 9999; {for input accumulation only}
      maxbinexp = 1080;
      binpower = 8;

      maxrealwords = 4; {number of words in real number}
      sign_bit = 32768;

    type
      halfrealsize = - 32767..32767; {this is real format dependent, rather
                                      than machine dependent }

      reel = {variant record kludge to allow bit manipulation on reals}
        packed record
          case boolean of
            false: (f: packed array [1..maxrealwords] of halfrealsize);
            true: (x: real); {double precision, of course}
        end;

    var
      realvalue: reel; {coerces the components of real into a "scalar" real}
      FracBits: 0..MaxFracBits;
      FracBytes: 0..MaxFracBytes;
      dpt: digitindex; {position of decimal point}
      exp: - maxexpval.. + maxexpval; {final value of exponent after scan}
      expvalue: 0..maxexpval; {range of exponent during input scan}
      binexp: - maxbinexp.. + maxbinexp;
      expneg: boolean; {true if exponent sign is negative}
      sticky_bit: boolean; {true if nonzero digits beyond buffer capacity}
      carry: byte;
      product: 0..maxproduct;
      power: byte;
      shifter: 0..maxusint;
      need_bits: - binpower..maxbinexp;
      temp: byte;
      head, i, j, top, tail: digitindex;
      tmp: integer;


    procedure get_fraction_byte(need_bits: byte);

{ Maintains the 2 indices, "top" and "tail", which delimit the fraction
  string, and "sticky_bit", which records the state of digits to the
  right of "tail", which are no longer participating in the conversion.
  In "need_bits" is the computed number of digits which are required to
  complete the conversion.  The output is 8 bits of binary stored in
  "carry".  This routine relocates the fraction string to the extreme
  right side of the digit buffer.
}

      var
        src, dst: word; {used to relocate the fraction string}


      begin
        p_entlib;
        while (digits[tail] = 0) and (top <= tail) do {zero suppress from
                                                       right}
          tail := tail - 1;

        carry := 0;

        if top + need_bits <= tail then
          begin
          tail := top + need_bits - 1; {adjust "tail"}
          sticky_bit := true; {since we're truncating non-zero digits}
          end;

        if tail >= top then
          begin
          dst := maxinbuf + 1; {plus one to accomodate pre-decrement}

          for src := tail downto top do
            begin
            dst := dst - 1;
            product := digits[src] * binbase + carry;
            digits[dst] := product mod decbase;
            carry := product div decbase;
            end;

          top := dst;
          tail := maxinbuf;
          end;
        p_exitlb;
      end {get_fraction_byte} ;


    begin {convertreal}
      p_entlib;
      FracBits := FracBitsDouble;
      FracBytes := FracBytesDouble;

      for i := 1 to maxrealwords do realvalue.f[i] := 0;
      exp := length; {number of significant digits}
      dpt := length; {local copy of digit length}

      binexp := 0;
      sticky_bit := false;

      { - - -   fraction part   - - - }

      if ch = point then
        begin
        ch := nextch;
        readdigits(exp = 0); {skip zeros if no integer part}
        exp := exp - leadingzeros; {nop if integer part exists}
        end; {fraction scan}

      { - - -   exponent part   - - - }

      if (ch = largeE) or (ch = smalle) then
        begin {exponent scan}
        ch := nextch;
        expneg := (ch = minus);

        if expneg or (ch = plus) then ch := nextch;

        if (ch < zero) or (ch > nine) then err := syntaxerror;

        expvalue := 0;
        while (ch >= zero) and (ch <= nine) do
          begin
          if expvalue < maxexpval div decbase then
            expvalue := expvalue * decbase + ord(ch) - ord(zero);
          ch := nextch
          end; {exponent digit scan}

        if expneg then exp := exp - expvalue
        else {not negative exponent} exp := exp + expvalue
        end; {exponent scan}

      dpt := fill;

{ dpt = total string length; exp = position of decimal point
}
      if dpt > 0 then
        begin {non-zero value}

        {-------- convert integer portion --------}

        if exp > 0 then
          begin

          if exp > MaxDecExpDouble then
            begin { obvious exponent overflow }
            err := overflowerror;
            goto 1;
            end;

          {detect string requiring trailing 0's; append as needed}

          while exp > dpt do
            begin
            dpt := dpt + 1;
            digits[dpt] := 0
            end;

          head := 1;
          tail := dpt;
          dpt := exp;

          { perform the integer conversion }

          for i := head to exp - 1 do
            begin
            carry := digits[i + 1];

            for j := i downto head do
              begin
              product := digits[j] * decbase + carry;
              digits[j + 1] := product mod binbase;
              carry := product div binbase;
              end; { for j }

            if carry = 0 then head := head + 1
            else digits[head] := carry;
            end; { for i }

          top := exp + 1;
          binexp := (exp - head + 1) * binpower;

          end { exp > 0 }

          { - - -  no integer part exists  - - - }

        else
          begin { exp <= 0 }

          if exp < MinDecExpDouble then
            begin { obvious exponent underflow }
            err := underflowerror;
            goto 1;
            end;

          top := 1;
          tail := dpt;
          binexp := 8;

          while exp <= 0 do
            begin {fraction scaling}

            get_fraction_byte(FracBits + 3 - (exp * 27) div 8);

            binexp := binexp - binpower; {adjust binary exponent}

            while (exp <> 0) and (carry <> 0) do
              begin
              top := top - 1;
              digits[top] := carry mod decbase;
              carry := carry div decbase;
              exp := exp + 1;
              end;

            if carry <> 0 then {force termination of scaling} exp := exp + 1;
            end; {fraction scaling}

          head := 1;
          dpt := 1; {store first binary byte to extreme left}
          digits[dpt] := carry;

          end { exp <= 0 } ;

{ The number now has at least one byte of binary integer.
  Make calls to "get_fraction_bytes" for additional bytes.
}
        temp := digits[head]; {the high order byte}
        need_bits := 0; {bits needed to normalize the high order}
        power := 1; {the multiplier required to normalize the result}

        while temp < binbase div 2 do
          begin
          need_bits := need_bits + 1;
          power := power + power;
          temp := temp + temp;
          end;

        binexp := binexp - need_bits;

        if dpt - head >= FracBytes then
          begin {we have excess integer bytes}
          dpt := head + FracBytes; {truncate the integer}

          { test excess precision for sticky bits }

          while not sticky_bit and (tail > dpt) do
            if digits[tail] = 0 then tail := tail - 1
            else sticky_bit := true;
          end {excess integer bytes}

        else
          begin {we need additional bytes, but the last byte produced may be
                 less than full precision}
          need_bits := (FracBytes - 1 + head - dpt) * binpower + need_bits + 1;
          while need_bits > 0 do
            begin
            get_fraction_byte(need_bits);
            dpt := dpt + 1;
            digits[dpt] := carry;
            need_bits := need_bits - binpower;
            end;

          while (digits[tail] = 0) and (top <= tail) do {zero suppress from
                                                         right}
            tail := tail - 1;

          if tail >= top then sticky_bit := true;
          end;

        {-------- normalize --------}

        if power > 1 then
          begin
          carry := 0;
          for i := dpt downto head do
            begin
            shifter := digits[i] * power + carry;
            digits[i] := shifter mod binbase;
            carry := shifter div binbase;
            end; {for i}
          end; {power > 1}

        sticky_bit := sticky_bit or (digits[dpt] <> 0);
        carry := 0;
        for i := dpt downto head do
          begin
          shifter := digits[i] * 32 + carry;
          digits[i + 1] := shifter mod binbase;
          carry := shifter div binbase;
          end; {for i}
        digits[head] := carry;

        {-------- rounding --------}

        if not sticky_bit then
          sticky_bit := digits[dpt] mod (binbase div 2) <> 0;

        if digits[dpt] >= binbase div 2 then
          begin {round bit = 1}

          carry := 1;
          i := dpt;

          while (carry <> 0) and (i <> head) do
            begin
            i := i - 1;
            if digits[i] = binbase - 1 then digits[i] := 0
            else
              begin
              digits[i] := digits[i] + 1;
              carry := 0;
              end;
            end;

          if i = head then
            if digits[head] = 32 then
              begin
              digits[head] := 16;
              binexp := binexp + 1;
              end;

          if not sticky_bit then {apply "nearest even" rule}
            digits[dpt - 1] := (digits[dpt - 1] div 2) * 2;
          end; {rounding}

        {-------- test exponent range --------}

        binexp := binexp - 1;

        if binexp < MinBinExpDouble then err := underflowerror
        else if binexp > MaxBinExpDouble then err := overflowerror;

{ if no errors up to now, then pack and deliver result, otherwise
  report proper error.  If we ever elect to give warnings instead
  of fatal errors, the following case statement could be expanded
  to allow underflow to return zero, overflow to return infinity,
  and syntaxerror to return a signalling "NaN".
}

      1:
        case err of

          noerror:
            with realvalue do
              begin {1st word is special due to exponent packing}
              tmp := (binexp + ExpBiasDouble - 1) * (binbase div 16);
              f[1] := tmp + digits[head];
              if negative then f[1] := f[1] or sign_bit;

              j := head + 1;
              for i := 2 to maxrealwords do
                begin
                tmp := digits[j] * binbase + digits[j + 1];
                if tmp > 32767 then tmp := tmp - 65536;
                f[i] := tmp;
                j := j + 2;
                end;
              end; {with realvalue}
          syntaxerror: ;
          underflowerror: ;
          overflowerror: ;
          end {case err} ;

        end {of non-zero value} ;

      value1 := realvalue.x;
      p_exitlb;
    end; {convertreal}


  begin {p_ctod}
    p_entlib;
    whiteout;

    readsign;

    fill := 0;

    err := noerror;

    readdigits(true); {fill digit array, skip leading zeros}

    convertreal(val);
    p_exitlb;
  end {p_ctod} ;
