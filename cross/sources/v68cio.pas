{$nomain}
{$nopointer}
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

  Pascal-2 Compiler Header Processing Routines

 Last modified by KRIS on 21-Nov-1990 15:35:46

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

CONST
  maxlength = 4;
  maxbits = 79;
  maxheaderlength = 130;
  blockcodesize = 15;

TYPE
  word = 0..65535;
  index = 0..maxlength;
  number = PACKED ARRAY [index] OF word;
  bitindex = 0..maxbits;
  bitarray = PACKED ARRAY [bitindex] OF boolean;
  line = PACKED ARRAY [1..maxheaderlength] OF char;
  num = ARRAY [1..blockcodesize] OF number;
  codepointer = ^num;
  date =
    PACKED RECORD
      day: 0..31;
      month: 0..12;
      year: 0..127;
    END;


FUNCTION z_b134: codepointer;
  nonpascal;


PROCEDURE z_b133(VAR a, b: number);
  nonpascal;


PROCEDURE z_b129(VAR l: line;
                 VAR len, site1, site2: integer);
  external;


procedure z_b129;

  VAR
    k, bias, newbias: word;
    x, s: number;
    p: codepointer;
    i, j: integer;
    kludge:
      RECORD
        CASE boolean OF
          true: (n: number);
          false: (c: PACKED ARRAY [1..10] OF char);
      END;


  BEGIN
    len := 1;
    p := z_b134;
    bias := 0;

    FOR i := 0 TO maxlength DO s[i] := 0;
    s[maxlength] := 97;

    WITH kludge DO
      BEGIN
      FOR i := 1 TO blockcodesize - 2 DO
        BEGIN
        x := p^[i];
        x[0] := (x[0] + bias) MOD 65536;
        newbias := 0;
        FOR j := 0 TO maxlength DO
          BEGIN
          newbias := (newbias + x[j]) MOD 65536;
          END;
        n := s;
        z_b133(x, n);
        n[3] := (n[3] + bias) MOD 65536;
        bias := newbias;
        FOR k := 10 DOWNTO 1 DO
          BEGIN
          l[len] := chr(ord(c[(k - 1) + 2 * ord(odd(k))]) MOD 128);
          IF ord(l[len]) <> 0 THEN len := len + 1;
          END;
        END;
      n := s;
      x := p^[blockcodesize - 1];
      FOR j := 0 TO maxlength DIV 2 DO
        BEGIN
        k := x[j];
        x[j] := x[maxlength - j];
        x[maxlength - j] := k;
        END;
      z_b133(x, n);
      site1 := n[2];
      site2 := n[1];
      END;
  END;


PROCEDURE z_b130(VAR start, stop: integer);
  external;


procedure z_b130;

  VAR
    p: codepointer;
    i, j: integer;
    k, check: word;
    y, s: number;
    kludge:
      RECORD
        CASE boolean OF
          true: (n: number);
          false:
            (x:
               PACKED RECORD
                 sum: word;
                 startdate: word;
                 enddate: word;
                 x1, x2: word;
               END; );
      END;


  BEGIN
    WITH kludge DO
      BEGIN
      p := z_b134;

      FOR i := 0 TO maxlength DO s[i] := 0;
      s[maxlength] := 97;

      check := 0;
      FOR i := 1 TO blockcodesize - 1 DO
        FOR j := 0 TO maxlength DO check := (check + p^[i, j]) MOD 65536;
      n := s;
      y := p^[blockcodesize];
      z_b133(y, n);
      FOR j := 0 TO maxlength DIV 2 DO
        BEGIN
        k := n[j];
        n[j] := n[maxlength - j];
        n[maxlength - j] := k;
        END;
      start := x.startdate;
      IF ((check + x.sum) MOD 65536) <> 0 THEN stop := x.startdate - 1
      ELSE stop := x.enddate;
      END;
  END;
