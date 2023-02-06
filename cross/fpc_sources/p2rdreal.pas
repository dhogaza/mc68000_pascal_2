{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045 Level: 1
  Processor: All
  System: All

  Pascal-2 Real Number Parser

 Last modified by KRIS on 21-Nov-1990 15:18:10

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

unit p2rdreal;

interface

uses config;

const

  maxrealwords  =     4 ;       { maximum number of words per real }
  maxrealbytes  =     8 ;       { maximum number of bytes per real }
  maxrealbits   =    64 ;       { maximum number of bits per real }

type

  nextch_t = function(firstch: boolean): char;

  realarray     = packed array [1..maxrealwords] of word ;

  realstatus    = ( noerror , syntaxerror , underflowerr , overflowerr) ;

  realmodetype      = 0..maxusword ;


procedure p2ReadReal(nextch: nextch_t;
                     var result: realarray;
                     var errorcode: realstatus;
                     mode: realmodetype;
                     var isdouble: boolean);

implementation

const
  {[f-]}
  bitsperbyte   =     8 ;       { number of bits per binary byte }

  nibblesize    =    16 ;       { number of elements in nibble }
  bytesize      =   256 ;       { number of elements in byte }
{ wordsize      = 65536 }       { number of elements in word }

{ maxnibble     =    15 }       { nibblesize - 1 }
  maxbyte       =   255 ;       { bytesize - 1  }
  maxword       = 65535 ;       { wordsize - 1 }

  halfwordsize  = 32768 ;       { wordsize div 2 }
  halfmaxword   = 32767 ;       { maxword div 2 }
  {[f+]}

type

  {[f-]}
  byte          = 0..maxbyte ;
  word          = 0..maxword ;

  RealFormat    = ( DECformat
                  , IEEEformat
                  , INTELformat
                  , IBMformat
                  ) ;

  RealPrecision = ( SinglePrecision
                  , DoublePrecision
                  , QuadPrecision
                  ) ;

  RealRounding  = ( ToNearest
                  , ToZero
                  , ToPosInf
                  , ToNegInf
                  ) ;

  RealClass     = ( ZeroClass           { signed zero }
                  , NormalClass         { normalized operand }
                  , DenormalClass       { denormalized operand }
                  , InfClass            { signed Infinity }
                  , QNaNClass           { quiet NaN }
                  , SNaNClass           { signalling NaN }
                  ) ;
{[f+]}



procedure p2ReadReal(nextch: nextch_t;
                     var result: realarray;
                     var errorcode: realstatus;
                     mode: realmodetype;
                     var isdouble: boolean);

  label
    1;

  const
    MaxInBuf = 804; { to support IEEE double with denormals }

  {
    == (leading binary zeroes) + (precision+1) - (leading decimal zeroes)

    DECsingle   ==      127     +  25   -   38  ==        114
    DECdouble   ==      127     +  57   -   38  ==        146

    IEEEsingle  == ( 126 + 23)  +  25   -   45  ==        129
    IEEEdouble  == (1022 + 52)  +  54   -  323  ==        805

      buffer size for formats with 15-bit exponent fields :

    DECquad(H)  ==      16383    + 114  - 4932  ==      11565

    IEEEextend  == (16382 +  63) +  65  - 4950  ==      11560
    IEEEquad    == (16382 + 111) + 113  - 4964  ==      11642
  }

    decbase = 10; { decimal radix }
    binbase = 256; { binary radix }
    maxproduct = 2559; { decbase * binbase - 1 }

    MaxExpon = 9999; { maximum exponent accepted during input scan }
    ExpLimit = 999; { (MaxExpon - 9) div 10 }

  type

    BufferIndex = 0..MaxInBuf;
    DecExpType = - MaxExpon..MaxExpon;
    BinExpType = - halfmaxword..halfmaxword;

  var

    digits: packed array [BufferIndex] of byte;
    value: realarray; { local copy of final binary result }

    format: RealFormat; { }
    precision: RealPrecision; { }
    rounding: RealRounding; { }

    realbits: 1..maxrealbits; { total number of binary bits of significance }
    realbytes: 1..maxrealbytes; { number of bytes of significance }
    realwords: 1..maxrealwords; { number of words in target real }

    impliedbit: 0..1; { 1 if binary format uses "hidden bit" }
    pointoffset: 0..bitsperbyte; { 1 for IEEEformat, 0 for others }

    exponoffset: 0..bitsperbyte; { exponent offset within high byte }

    ch: char; { last char returned from "nextch" function }

    ExpValue: DecExpType; { value which follows "E" during scan }
    DecExp: DecExpType; { value of decimal exponent after scan }

    MaxDecExp: DecExpType; { maximum decimal exponent value }
    MinDecExp: DecExpType; { minimum decimal exponent value }

    MaxBinExp: BinExpType; { maximum unbiased exponent value }
    MinBinExp: BinExpType; { minimum unbiased exponent value }

    ExpBias: BinExpType; { exponent bias for this format }
    BinExp: BinExpType; { final unbiased binary exponent }

    NeedBits: BinExpType; { binary bits needed to finish conversion }

    ZeroBits: 0..bitsperbyte; { leading zero bits in high order byte }
    DenormBits: BinExpType; { number of bits to denormalize result by }

    negative: boolean; { true if minus sign appeared in input string }
    signed: boolean; { true if minus sign or plus sign encountered }
    expneg: boolean; { true if exponent sign is negative }
    StickyBit: boolean; { true if nonzero digits beyond buffer capacity }
    RoundBit: boolean; { true if the next bit past the LSB is non-zero }
    RoundUp: boolean; { true if rounding is applied to the fraction }
    Inexact: boolean; { true if the conversion is not exact }
    reversewds: boolean; { true if result is stored low word to hi word }
    denormalizing: boolean; { true if result may be gradually denormalized }

    product: 0..maxproduct;
    shifter: word;

    power: byte;
    carry: byte;
    temp: byte;

    dpt: BufferIndex; { index into digits[] when scanning input }
    i: BufferIndex; { temporary induction var }
    j: BufferIndex; { temporary induction var }

   { indices which delimit the integer and fraction parts of digits[] buffer }

    inthead: BufferIndex;
    inttail: BufferIndex;
    frachead: BufferIndex;
    fractail: BufferIndex;


  procedure getch;


    begin
if isdouble then
      ch := nextch(false);
    end { getch } ;


  function alfanumeric(ch: char): boolean;


    begin
      alfanumeric := (ch >= 'A') and (ch <= 'Z') or (ch >= 'a') and
                     (ch <= 'z') or (ch >= '0') and (ch <= '9');
    end { alfanumeric } ;


  function uppercase(ch: char): char;


    begin
      if (ch >= 'a') and (ch <= 'z') then
        uppercase := chr(ord(ch) - ord('a') + ord('A'))
      else uppercase := ch
    end { uppercase } ;


  function chEquals(c: char): boolean;


    begin
      if (uppercase(ch) = c) then
        begin
        getch;
        chEquals := true
        end
      else chEquals := false
    end { chEquals } ;


  procedure GetFractionByte(NeedBits: BinExpType);

{ Maintains the 2 indices, "frachead" and "fractail", which delimit the
  fraction string, and "StickyBit", which records the state of digits to
  the right of "fractail", which can no longer directly affect the result.
  "NeedBits" is the computed maximum number of decimal digits which are
  required to complete the binary conversion.  The output is one byte of
  binary stored in "carry".  This routine relocates the fraction string to
  the high end of the digit buffer.
}

    var
      src, dst: BufferIndex; { used to relocate the fraction string }


    begin

      while (digits[fractail] = 0) and (frachead <= fractail) do { zero
              suppress from right }
        fractail := fractail - 1;

      carry := 0;

      if (frachead + NeedBits <= fractail) then
        begin { adjust tail }
        fractail := frachead + NeedBits - 1;
        StickyBit := true { truncating non-zero digits }
        end; { adjust tail }

      if (fractail >= frachead) then
        begin
        dst := MaxInBuf;

        for src := fractail downto frachead do
          begin
          product := digits[src] * binbase + carry;
          digits[dst] := product mod decbase;
          carry := product div decbase;
          dst := dst - 1;
          end;

        frachead := dst + 1;
        fractail := MaxInBuf;
        end;

    end { GetFractionByte } ;


  procedure initialize;


    var
      i: integer;

    begin
    case (format) of

      IEEEformat, INTELformat:
        begin
        pointoffset := 1;
        denormalizing := true;
        if (format = INTELformat) then reversewds := true;

        case (precision) of
          SinglePrecision:
            begin
            realwords := 2;
            realbytes := 3;
            realbits := 24;

            ExpBias := 127;
            MaxBinExp := + 127;
            MinBinExp := - 126;
            MaxDecExp := + 39;
            MinDecExp := - 45;
            end { SinglePrecision } ;
          DoublePrecision:
            begin
            realwords := 4;
            realbytes := 7;
            realbits := 53;
            exponoffset := 4;

            ExpBias := 1023;
            MaxBinExp := + 1023;
            MinBinExp := - 1022;
            MaxDecExp := + 309;
            MinDecExp := - 323;
            end { DoublePrecision } ;
          end { case precision } ;
        end { IEEEformat } ;

      DECformat:
        begin
        pointoffset := 0;
        ExpBias := 128;
        MaxDecExp := + 39;
        MinDecExp := - 39;
        MaxBinExp := + 127;
        MinBinExp := - 127;
        case (precision) of
          SinglePrecision:
            begin
            realwords := 2;
            realbytes := 3;
            realbits := 24;
            end { SinglePrecision } ;
          DoublePrecision:
            begin
            realwords := 4;
            realbytes := 7;
            realbits := 56;
            end { SinglePrecision } ;
          end { case precision } ;
        end { DECformat } ;

      IBMformat:
        begin
        end { IBMformat } ;

      end { case format } ;

    for i := 1 to realwords do value[i] := 0;
    end;



  begin { p2ReadReal }

    isdouble := false; {default}

    { unpack the mode parameter word into constituent parts }

    rounding := RealRounding((mode mod nibblesize));

    precision := RealPrecision((mode div nibblesize) mod nibblesize);

    format := RealFormat((mode div bytesize) mod nibblesize);

    impliedbit := 1; { correct value for most formats }
    exponoffset := 7; { correct value for all single formats }
    reversewds := false; { correct value for all except iAPX86 }
    denormalizing := false; { correct value for non-IEEE formats }

    dpt := 0;
    DecExp := 0;
    BinExp := 0;
    StickyBit := false;
    errorcode := noerror;

    ch := nextch(true);

    while (ch = ' ') do getch;

    negative := (ch = '-');

    signed := negative or (ch = '+');

    if signed then getch;

    if (ch < '0') or (ch > '9') then
      begin
      errorcode := syntaxerror;

      if (format = IEEEformat) or (format = INTELformat) then

        begin { permit certain keywords }

        if chEquals('I') then
          if chEquals('N') then
            if chEquals('F') then

              begin { scan Infinity syntax }
              { permit alternate spellings }
              if chEquals('I') then
                if chEquals('N') then
                  if chEquals('I') then
                    if chEquals('T') then if chEquals('Y') then getch;

              { test for proper termination }
              end { aliases }

            else { ch <> 'F' }
          else { ch <> 'N' }
        else { ch <> 'I' }

        if chEquals('N') then
          if chEquals('A') then
            if chEquals('N') then

              begin { check NaN syntax }
              { permit parenthesized argument }
              if chEquals('(') then while not chEquals(')') do getch;

              { test for proper termination }
              end { check NaN syntax }

            else { ch <> 'N' }
          else { ch <> 'A' }
        else { ch <> 'N' }
        end { if format = IEEEformat } ;

      goto 1
      end { special operands } ;

    { - - -   scan integer part   - - - }

    while (ch = '0') do getch;

    while (ch >= '0') and (ch <= '9') do
      begin
      if (dpt < MaxInBuf) then
        begin { insert integer digit }
        dpt := dpt + 1;
        digits[dpt] := ord(ch) - ord('0')
        end { insert integer digit }
      else { dpt => MaxInBuf }
        begin { buffer is full }
        DecExp := DecExp + 1;
        if (ch <> '0') then StickyBit := true
        end; { buffer is full }
      getch
      end { while ch is digit } ;

    DecExp := DecExp + dpt;

    { - - -   scan fraction part   - - - }

    if chEquals('.') then
      begin
      if (ch < '0') or (ch > '9') then errorcode := syntaxerror;

      if (dpt = 0) then { no integer part }
        while chEquals('0') do DecExp := DecExp - 1;

      while (ch >= '0') and (ch <= '9') do
        begin
        if (dpt < MaxInBuf) then
          begin { insert fraction digit }
          dpt := dpt + 1;
          digits[dpt] := ord(ch) - ord('0')
          end { insert fraction digit }
        else { dpt => MaxInBuf }
        if (ch <> '0') then StickyBit := true;
        getch
        end { while ch is digit }

      end { scan fraction part } ;

    { - - -   scan exponent part   - - - }

    ch := uppercase(ch);

    if (ch = 'E') or (ch = 'D') then
      begin { exponent scan }

      { It's a double constant, set the flag and precision.
      }
      if ch = 'D' then
        begin
        isdouble := true;
        precision := DoublePrecision;
        end;

      getch;
      expneg := (ch = '-');
      if expneg or (ch = '+') then getch;

      if (ch < '0') or (ch > '9') then errorcode := syntaxerror;

      ExpValue := 0;
      while (ch >= '0') and (ch <= '9') do
        begin
        if (ExpValue <= ExpLimit) then
          ExpValue := ExpValue * 10 + ord(ch) - ord('0');
        getch
        end;

      if expneg then DecExp := DecExp - ExpValue
      else DecExp := DecExp + ExpValue
      end { exponent scan } ;

    { Now that 'E' or 'D' is passed, we can set the exponent limits.
    }
    initialize;


    {----- scanning is complete; commence conversion -----}

    if (dpt > 0) then
      begin { conversion of non-zero string }
      inthead := 1;

      while (DecExp < dpt) { if a fraction part exists }
            and (digits[dpt] = 0) do
        dpt := dpt - 1; { then delete trailing zeroes }

      if (DecExp > 0) then
        begin { convert integer portion }

        if (DecExp > MaxDecExp) then
          begin { obvious exponent overflow }
          errorcode := overflowerr;
          DecExp := MaxDecExp;
          digits[1] := ord('9');
          end;

        while (DecExp > dpt) do
          begin { append trailing zeroes }
          dpt := dpt + 1;
          digits[dpt] := 0
          end; { append trailing zeroes }

        inttail := DecExp;
        frachead := DecExp + 1;
        fractail := dpt;

        { perform the integer conversion }

        for i := inthead to inttail - 1 do
          begin
          carry := digits[i + 1];

          for j := i downto inthead do
            begin
            product := digits[j] * decbase + carry;
            digits[j + 1] := product mod binbase;
            carry := product div binbase;
            end { for j } ;

          if carry = 0 then inthead := inthead + 1
          else digits[inthead] := carry;
          end { for i } ;

        BinExp := (inttail - inthead + 1) * bitsperbyte;

        end { convert integer portion }

      else { DecExp <= 0 }
        begin { the value has no integer part }

        if (DecExp < MinDecExp) then
          begin { obvious exponent underflow }
          errorcode := underflowerr;
          goto 1
          end;

        frachead := 1;
        fractail := dpt;
        BinExp := bitsperbyte;

        while (DecExp <= 0) do
          begin { fraction scaling }

          { It may not be necessary for all of the decimal fraction digits
          to take part in the conversion -- the exact number of decimal
          digits needed is equivalent to the number of bits in the binary
          result INCLUDING leading zero bits (plus a few for rounding).
          Rather than multiply the decimal exponent by log(2)10 (3.3219...)
          to compute leading binary zeroes, a simpler approximation of 3 3/8
          (3.375) is used, taking care to avoid overflow on 16 bit hosts, even
          when developing extended format reals with 15 bit exponent values.
          }

          NeedBits := - DecExp - DecExp - DecExp; { 3 * abs(DecExp) }

          GetFractionByte(NeedBits div 8 + NeedBits + realbits + 3);

          BinExp := BinExp - bitsperbyte; { adjust binary exponent }

          while (DecExp <> 0) and (carry <> 0) do
            begin
            frachead := frachead - 1;
            digits[frachead] := carry mod decbase;
            carry := carry div decbase;
            DecExp := DecExp + 1;
            end;

          if (carry <> 0) then { force termination of scaling }
            DecExp := DecExp + 1;
          end; { fraction scaling }

        { store first converted binary byte in low order end of buffer }

        digits[1] := carry;
        inttail := 1;
        end { DecExp <= 0 } ;


{ The number now has at least one byte of converted binary integer.
  Truncate the binary integer if there are excess bytes, or call
  GetFractionBytes for additional bytes if there are too few.  Also,
  count the number of significant bits in the high order byte.
}
      ZeroBits := bitsperbyte; { number of leading zero bits }
      temp := digits[inthead]; { the high order byte }

      repeat
        ZeroBits := ZeroBits - 1;
        temp := temp div 2
      until temp = 0;

      BinExp := BinExp - ZeroBits;
      { BinExp is now the correct unbiased binary exponent }

      if (inttail - inthead >= realbytes) then
        begin { truncate excess bytes }
        inttail := inthead + realbytes;
        frachead := inttail + 1;
        end { truncate excess bytes }

      else { inttail - inthead < realbytes }
        begin { generate additional bytes }
        NeedBits := (realbytes - (inttail - inthead) - 1) * bitsperbyte +
                    ZeroBits + 1;
        repeat
          GetFractionByte(NeedBits);
          inttail := inttail + 1;
          digits[inttail] := carry;
          NeedBits := NeedBits - bitsperbyte;
        until (NeedBits <= 0);
        end; { generate additional bytes }

      while (not StickyBit) and (frachead <= fractail) do
        if (digits[fractail] = 0) then fractail := fractail - 1
        else StickyBit := true;

{ the binary significand is now a (realbytes + 1) byte string
  starting at digits[inthead] and ending at digits[inttail].
  "StickyBit" is true if the conversion is inexact thus far.
}

      {-------- normalize or denormalize --------}

      DenormBits := 0;
      if (BinExp <= MinBinExp) then
        begin
        errorcode := underflowerr;
        DenormBits := MinBinExp - BinExp;
        BinExp := MinBinExp;
        impliedbit := 0;

        if (not denormalizing) then
          begin
          for i := inthead to inthead + realbytes do digits[i] := 0;
          goto 1
          end
        end;

      { compute bit offset multiplier }

      shifter := (realbits + ZeroBits - DenormBits) mod bitsperbyte;
      power := 1;
      for i := 1 to shifter do power := power + power;

      { compute byte offset index }

      temp := (bitsperbyte - 1) - (realbits - 1) mod bitsperbyte;
      i := inttail - (DenormBits + temp) div bitsperbyte;

      { test the need for an extra right shift }

      if (ZeroBits < shifter) then i := i - 1;

      j := i;

      { scan any bytes being discarded from tail end }

      while (j < inttail) do
        begin
        j := j + 1;
        if (digits[j] <> 0) then StickyBit := true;
        end { while } ;

      { simultaneously shift bits left and bytes right }

      carry := 0;
      while (i >= inthead) do
        begin
        shifter := digits[i] * power + carry;
        digits[j] := shifter mod binbase;
        carry := shifter div binbase;
        i := i - 1;
        j := j - 1;
        end { while } ;

      { flush out remainder of high order, if any }

      while (j >= inthead) do
        begin
        digits[j] := carry;
        carry := 0;
        j := j - 1;
        end { while } ;

      {-------- test for rounding --------}

      RoundBit := (digits[inttail] >= (binbase div 2));

      if (not StickyBit) then
        StickyBit := (digits[inttail] mod (binbase div 2) <> 0);

      Inexact := (RoundBit) or (StickyBit);

      case (rounding) of

        ToNearest:
          RoundUp := (RoundBit) and (StickyBit or odd(digits[inttail - 1]));
        ToZero: RoundUp := false;

        ToPosInf: RoundUp := (not negative) and (Inexact);

        ToNegInf: RoundUp := (negative) and (Inexact);

        end { case (rounding) } ;

      {-------- apply rounding --------}

      if (RoundUp) then
        begin
        carry := 1;
        i := inttail - 1;

        while (carry <> 0) and (i > inthead) do
          begin
          if (digits[i] = binbase - 1) then digits[i] := 0
          else
            begin
            digits[i] := digits[i] + 1;
            carry := 0;
            end;
          i := i - 1;
          end;

        if (carry <> 0) then
          begin { round up high order byte }
          temp := 0;
          for i := 1 to ((realbits - 1) mod bitsperbyte + 1) do
            temp := temp + temp + 1;
          if (digits[inthead] = temp) then
            begin
            digits[inthead] := (temp div 2) + 1;
            BinExp := BinExp + 1;
            end
          else
            begin
            digits[inthead] := digits[inthead] + 1;
            carry := 0;
            end;
          end; { round up high order byte }

        end { rounding } ;

      {-------- test for exponent range error --------}

      if (BinExp - pointoffset > MaxBinExp) then
        begin
        errorcode := overflowerr;
        BinExp := MaxBinExp;
        if (format = IEEEformat) or (format = INTELformat) then
          begin
          BinExp := BinExp + 1;
          temp := 0
          end
        else temp := maxbyte;
        for i := inthead to inthead + realbytes do digits[i] := temp;
        goto 1;
        end;

      {-------- packing --------}

      shifter := (BinExp + ExpBias - pointoffset - impliedbit);

      for i := 1 to exponoffset do shifter := shifter + shifter; { shift
        exponent left }

      value[1] := shifter + digits[inthead];

      j := inthead;

      for i := 2 to realwords do
        begin
        j := j + 2;
        value[i] := digits[j - 1] * binbase + digits[j];
        end;

      end {of non-zero value } ;

    {---- append sign and deliver result ----}

  1:
    if negative then value[1] := value[1] + halfwordsize;

    if (reversewds) then
      begin
      j := realwords;
      for i := 1 to realwords do
        begin
        result[i] := value[j];
        j := j - 1;
        end;
      end

    else { not reverse words }

      for i := 1 to realwords do result[i] := value[i];

  end { p2ReadReal } ;

end.
