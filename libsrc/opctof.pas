{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1982, 1983, 1984, 1985, 1986, 1987, 1988
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

  Pascal Real Number Input Conversion

 Last modified by KRIS on 26-Nov-1990 13:55:25
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}
{[f-]}

{$section=8,version=0045}
{$ident='Pascal Real Number Input Conversion'}
{$nomain,nopointercheck,norangecheck,noindexcheck,nowalkback}

%include opcom;


procedure p_ctof {(function nextch: char; var val: real; var err: convstatus)}
 ;

  label
    1;

  const
    maxinbuf = 116; { = max unbiased exp + 3 + number of fraction bits +
                     minimum decimal exponent == 127 + 3 + 24 - 38 }
    maxexpval = 9999; {for input accumulation only}
    maxbinexp = 156;
    expbias = 126;
    decbase = 10;
    binbase = 256;
    binpower = 8;

    maxbyte = 255;
    maxproduct = 2559; { decbase * binbase - 1 }
    maxfracbits = 24;
    maxfracbytes = 3;

    blank = ' ';
    plus = '+';
    minus = '-';
    period = '.';
    zero = '0';
    nine = '9';
    largeE = 'E';
    smalle = 'e';

  type
    byte = 0..maxbyte;
    unsigned = 0..65535;
    floatkludge =
      record
        case boolean of
          false: (r: real);
          true: (w: array [0..7] of 0..65535);
      end;

  var
    flt: floatkludge;
    ch: char; {ordinal value of current character}
    a: packed array [0..maxinbuf] of byte; {buffers number before conversion}
    dpt: 0..maxinbuf; {position of decimal point}
    exp: - maxexpval.. + maxexpval; {final value of exponent after scan}
    expvalue: 0..maxexpval; {range of exponent during input scan}
    binexp: - maxbinexp.. + maxbinexp;
    negative: boolean; {true if minus sign appeared in input string}
    expneg: boolean; {true if exponent sign is negative}
    sticky_bit: boolean; {true if nonzero digits beyond buffer capacity}
    carry: byte;
    product: 0..maxproduct;
    power: byte;
    shifter: unsigned;

    need_bits: - binpower..maxbinexp;
    temp: 0..maxbyte;
    head, i, j, top, tail: 0..maxinbuf;


  procedure get_fraction_byte(need_bits: byte);

{ Maintains the 2 indices, "top" and "tail", which delimit the fraction
  string, and "sticky_bit", which records the state of digits to the
  right of "tail", which are no longer participating in the conversion.
  "Need_bits" is the computed number of digits which are required to
  complete the conversion.  The output is 8 bits of binary stored in
  "carry".  This routine is free to relocate the fraction string to the
  extreme right of the digit buffer.
}

    var
      src, dst: byte; {used to relocate the fraction string}


    begin
      p_entlib;

      while (a[tail] = 0) and (top <= tail) do {zero suppress from right}
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
          product := a[src] * binbase + carry;
          a[dst] := product mod decbase;
          carry := product div decbase;
          end;

        top := dst;
        tail := maxinbuf;
        end;

      p_exitlb;
    end {get_fraction_byte} ;




  begin {p_ctof}
    p_entlib;

    dpt := 0;
    exp := 0;
    binexp := 0;
    sticky_bit := false;
    err := noerror; {initialize error condition}
    flt.r := 0.0;

    ch := nextch;

    while (ch = blank) do ch := nextch;

    { - - -   process sign   - - - }

    if ch = minus then
      begin
      negative := true;
      ch := nextch
      end {then}
    else
      begin
      negative := false;
      if ch = plus then ch := nextch
      end; {else}

    if (ch < zero) or (ch > nine) then err := syntaxerror;

    while ch = zero do ch := nextch;

    { - - -   integer part   - - - }

    while (ch >= zero) and (ch <= nine) do
      begin
      if dpt = maxinbuf then
        begin {buffer is full}
        exp := exp + 1; {count the integer digit}
        if ch <> zero then sticky_bit := true
        else {sticky retains previous value}
        end {buffer is full}
      else
        begin {insert integer digit}
        dpt := dpt + 1;
        a[dpt] := ord(ch) - ord(zero)
        end; {insert integer digit}
      ch := nextch
      end; {while}

    exp := exp + dpt;

    { - - -   fraction part   - - - }

    if ch = period then
      begin
      ch := nextch;
      if (ch < zero) or (ch > nine) then err := syntaxerror;

      if dpt = 0 then {no integer part}
        while ch = zero do
          begin
          exp := exp - 1;
          ch := nextch
          end
      else {already have significant digits} ;

      while (ch >= zero) and (ch <= nine) do
        begin
        if dpt = maxinbuf then {buffer is full}
          if ch <> zero then sticky_bit := true
          else {ignore excess fraction digits}
        else
          begin {insert fraction digit}
          dpt := dpt + 1;
          a[dpt] := ord(ch) - ord(zero)
          end; {insert fraction digit}
        ch := nextch
        end; {while}

      end; {fraction scan}

    { - - -   exponent part   - - - }

    if (ch = largeE) or (ch = smalle) then
      begin {exponent scan}
      ch := nextch;

      if ch = minus then
        begin
        expneg := true;
        ch := nextch
        end
      else
        begin
        expneg := false;
        if ch = plus then ch := nextch
        end;

      if (ch < zero) or (ch > nine) then err := syntaxerror;

      expvalue := 0;
      while (ch >= zero) and (ch <= nine) do
        begin
        if expvalue < maxexpval div 10 then
          expvalue := expvalue * 10 + ord(ch) - ord(zero)
        else {avoid overflow} ;
        ch := nextch
        end; {exponent digit scan}

      if expneg then exp := exp - expvalue
      else {not negative exponent} exp := exp + expvalue
      end; {exponent scan}

    while (exp < dpt) and (a[dpt] = 0) do { delete trailing fraction digits }
      dpt := dpt - 1;

{ calculate how many digits are REALLY required to perform an
  error-free conversion.  This is a function of "exp", of the
  maximum negative exponent, and of the number of bits in the
  fraction of the target representation (24, 53, etc.)
}

    if dpt > 0 then
      begin {non-zero value}

      if exp <= 0 then
        begin

        if exp < - 39 then
          begin { obvious exponent underflow }
          err := underflowerror;
          goto 1;
          end;

        top := 1;
        tail := dpt;
        binexp := 8;

        while exp <= 0 do
          begin {fraction scaling}

          get_fraction_byte(maxfracbits + 3 - (exp * 27) div 8);

          binexp := binexp - binpower; {adjust binary exponent}

          while (exp <> 0) and (carry <> 0) do
            begin
            top := top - 1;
            a[top] := carry mod 10;
            carry := carry div 10;
            exp := exp + 1;
            end;

          if carry <> 0 then {force termination of scaling} exp := exp + 1;
          end; {fraction scaling}

        head := 1;
        dpt := 1; {store first binary byte to extreme left}
        a[dpt] := carry;
        end { exp <= 0 }

        {-------- convert integer portion --------}

      else
        begin { exp > 0 }

        if exp > 39 then
          begin { obvious exponent overflow }
          err := overflowerror;
          goto 1;
          end;

        {detect string requiring trailing 0's; append as required}

        while exp > dpt do
          begin
          dpt := dpt + 1;
          a[dpt] := 0
          end;

        head := 1;
        tail := dpt;
        dpt := exp;

        { perform the integer conversion }

        for i := head to exp - 1 do
          begin
          carry := a[i + 1];

          for j := i downto head do
            begin
            product := a[j] * decbase + carry;
            a[j + 1] := product mod binbase;
            carry := product div binbase;
            end; { for j }

          if carry = 0 then head := head + 1
          else a[head] := carry;
          end; { for i }

        top := exp + 1;
        binexp := (exp - head + 1) * binpower;
        end { exp > 0 } ;

{ The number now has at least one byte of binary integer.
  Make calls to "get_fraction_bytes" for additional bytes.
}
      temp := a[head]; {the high order byte}
      need_bits := 0; {bits needed to normalize the high order}
      power := 1; {the multiplier required to normalize the result}

      while temp < binbase div 2 do
        begin
        need_bits := need_bits + 1;
        power := power + power;
        temp := temp + temp;
        end;

      binexp := binexp - need_bits;

      if dpt - head >= maxfracbytes then
        begin {we have excess integer bytes}
        dpt := head + maxfracbytes; {truncate the integer}

        { test excess precision for sticky bits }

        while not sticky_bit and (tail > dpt) do
          if a[tail] = 0 then tail := tail - 1
          else sticky_bit := true;
        end {excess integer bytes}

      else
        begin {we need additional bytes, but the last byte produced may be less
               than full precision}
        need_bits := (maxfracbytes - 1 + head - dpt) * binpower + need_bits +
                     1;
        while need_bits > 0 do
          begin
          get_fraction_byte(need_bits);
          dpt := dpt + 1;
          a[dpt] := carry;
          need_bits := need_bits - binpower;
          end;

        while (a[tail] = 0) and (top <= tail) do {zero suppress from right}
          tail := tail - 1;

        if tail >= top then sticky_bit := true;
        end;

      {-------- normalize --------}

      if power > 1 then
        begin
        carry := 0;
        for i := dpt downto head do
          begin
          shifter := a[i] * power + carry;
          a[i] := shifter mod binbase;
          carry := shifter div binbase;
          end; {for i}

        end; {power > 1}

      {-------- rounding --------}

      if not sticky_bit then sticky_bit := a[dpt] mod (binbase div 2) <> 0;

      if a[dpt] >= binbase div 2 then
        begin {round bit = 1}

        carry := 1;
        i := dpt;

        while (carry <> 0) and (i <> head) do
          begin
          i := i - 1;
          if a[i] = binbase - 1 then a[i] := 0
          else
            begin
            a[i] := a[i] + 1;
            carry := 0;
            end;
          end;

        if carry <> 0 then
          begin
          binexp := binexp + 1;
          a[head] := binbase div 2;
          end;

        if not sticky_bit then {apply "nearest even" rule}
          a[dpt - 1] := (a[dpt - 1] div 2) * 2;
        end; {rounding}

      {-------- test exponent range --------}

      if binexp < - 126 then
        begin
        err := underflowerror;
        goto 1;
        end
      else if binexp > + 128 then
        begin
        err := overflowerror;
        goto 1;
        end;

      {-------- packing --------}

      with flt do
        begin
        w[0] := (ord(negative) * binbase + (expbias + binexp)) * (binbase div
                2) + (a[head] - binbase div 2);

        i := head + 1;
        j := 1;

        while i < dpt do
          begin
          w[j] := a[i] * binbase + a[i + 1];
          i := i + 2;
          j := j + 1;
          end;

        end; {with Flt}

      end {of non-zero value}

      {components of Flt are unsigned, but overflow is still a problem}

    else flt.w[0] := ord(negative) * maxint + ord(negative);

    {-------- transfer --------}

    val := flt.r; {assign final result}
  1:
    p_exitlb;
  end; {p_ctof}
