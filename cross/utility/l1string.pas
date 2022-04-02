{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1985 by Oregon Software, 
  Inc.  All Rights Reserved.

  This computer program is the property of Oregon Software, Inc.
  of Portland, Oregon, U.S.A., and may be used
  and copied only as specifically permitted under written
  license agreement signed by Oregon Software, Inc.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

   Conformant array string routines.
   Release version: 0045 Level: 1  Date: 21-Nov-1990 17:19:37
   Processor: ALL  System: All
}

{

Declare string variables for this package with type declarations of the
form "packed array [0..n] of char", where n >= 1.  The length of the string
is stored in element 0, elements 1 to n are the characters of the string.
The routines will also accept parameters of type "packed array [1..n] of
char", in which case "n" is considered to be the length of the string.  In
particular, quoted strings in Pascal are considered to be of this type, and
may be intermixed with string variables when calling routines which don't
require "var" params.

The capabilities provided are:

Len(S) - a function giving the current length of string S;

Clear(S) - initializes string S to empty;

ReadString(F,S) - reads a value for string S from the text
  file F.  The string is terminated by Eoln(F) and a
  Readln(F) is performed.  String overflow (input data contains
  more characters than the target array can hold) results in truncation.

WriteString(F,S) - writes the string S to the text file F.

Concatenate(T,S) - appends string S to the target string T.
  The resulting value is string T.  Overflow results in
  truncation to StringMax characters.

Search(S,T,Start) - searches string S for the first
  occurrence of string T to the right of position Start
  (characters are numbered beginning with one).  The
  function Search() returns the position of the first
  character in the matching substring, or the value zero if
  the string T does not appear.

Insert(T,S,Start) - inserts the string S into the target
  string T at position Start.  Characters are shifted to the
  right as necessary.  Overflow produces a truncated target
  string;  a Start position which would produce a string
  which was not contiguous has no effect.

Assign(T,S) - Assign string S to the target string T.  Especially
useful for assigning a literal string to a variable string.

AssChar(T,C) - Assign character C to the target string T.
Especially useful for assigning a single character to a
variable string.

Equal(T,S) - Function Equal returns TRUE when T=S, returns
FALSE otherwise.

The Start and Span parameters in the following procedures
define a substring beginning at position Start (between
characters Start-1 and Start) with a length of Abs(Span).
If Span is positive, the substring is to the right of Start;
if negative, the substring is to the left.

DelString(S,Start,Span) - deletes the substring defined by
  Start, Span from the string S.

Substring(T,S,Start,Span) - the substring of string S
  defined by Start, Span is assigned to the target string T.

Leftstring(T,S,Last) - the substring of string S to the
  left of Last is assigned to target string T.

Rightstring(T,S,First) - the substring of string S to the
  right of First is assigned to target string T.
}
{[b+]}


procedure exitst(error: integer);

{ exit with status, forces abort of pascal program.
}

  external; {in the pascal library}


procedure abort(s: packed array [low..high: integer] of char);

{ abort program if an illegal combination of arguments has been
  detected.
}

  forward;


procedure writestring(var f: text; {an output file, we hope}
                      var s: packed array [slow..shigh: integer] of char);

 { Write a string variable to file "f". }

  var
    i: integer;


  begin {writestring}
    if (slow = 0) {funny string} then
      for i := 1 to ord(s[0]) do write(f, s[i])
    else if (slow = 1) {string literal} then
      for i := 1 to shigh do write(f, s[i])
    else abort('writestring')
  end {writestring} ;


procedure abort;


  begin {abort}
    write('Illegal string argument in "');
    writestring(output, s);
    writeln('"');
    exitst(4);
  end {abort} ;


function len(s: packed array [slow..shigh: integer] of char): integer;


  begin {len}
    if slow = 0 then len := ord(s[0])
    else if slow = 1 then len := shigh
    else abort('len');
  end {len} ;


procedure clear(var s: packed array [low..high: integer] of char);


  begin {clear}
    if low <> 0 then abort('clear')
    else s[0] := chr(0);
  end {clear} ;


procedure concatenate(var t: packed array [tlow..thigh: integer] of char;
                      s: packed array [slow..shigh: integer] of char);

  var
    i, slen, tlen: integer;


  begin {concatenate}
    if (tlow <> 0) or (slow <> 1) and (slow <> 0) then abort('concatenate')
    else
      begin
      if slow = 1 then slen := shigh
      else slen := ord(s[0]);
      tlen := ord(t[0]);
      if slen + tlen > thigh then slen := thigh - tlen;
      t[0] := chr(slen + tlen);
      for i := 1 to slen do t[i + tlen] := s[i];
      end;
  end {concatenate} ;


function search(s: packed array [slow..shigh: integer] of char;
                t: packed array [tlow..thigh: integer] of char;
                start: integer): integer;

  var
    i, j, tlen, slen: integer;
    uneq: boolean;


  begin {search}
    if (start < 1) or (slow <> 0) and (slow <> 1) or (tlow <> 0) and
       (tlow <> 1) then
      abort('search')
    else
      begin
      search := 0;
      if slow = 0 then slen := ord(s[0])
      else slen := shigh;
      if tlow = 0 then tlen := ord(t[0])
      else tlen := thigh;
      if (start + tlen <= slen + 1) and (tlen <> 0) then
        begin
        i := start - 1;
        repeat
          i := i + 1;
          j := 0;
          repeat
            j := j + 1;
            uneq := t[j] <> s[i + j - 1];
          until uneq or (j = tlen);
        until (not uneq) or (i = slen - tlen + 1);
        if uneq then search := 0
        else search := i;
        end;
      end;
  end {search} ;


procedure readstring(var f: text; { an input file }
                     var s: packed array [slow..shigh: integer] of char);

  var
    slen: integer {used to hold accumulated length of s, quicker access than
                   s[0]} ;


  begin {readstring}
    if slow <> 0 then abort('readstring')
    else
      begin
      slen := 0;
      while (not eoln(f)) and (slen < shigh) do
        begin
        slen := slen + 1;
        read(f, s[slen]);
        end;
      s[0] := chr(slen);
      readln(f);
      end;
  end {readstring} ;


procedure substring(var t: packed array [tlow..thigh: integer] of char;
                    s: packed array [slow..shigh: integer] of char;
                    start, span: integer);

  var
    i, slen: integer;


  begin {substring}
    if (tlow <> 0) or (slow <> 0) and (slow <> 1) then abort('substring')
    else
      begin
      if slow = 0 then slen := ord(s[0])
      else slen := shigh;
      if span < 0 then
        begin
        span := - span;
        start := start - span
        end;
      if start < 1 then
        begin
        span := span + start - 1;
        start := 1
        end;
      if start + span > slen + 1 then span := slen - start + 1;
      if thigh < span then abort('substring')
      else if span <= 0 then t[0] := chr(0)
      else
        begin
        for i := 1 to span do t[i] := s[start + i - 1];
        t[0] := chr(span);
        end;
      end;
  end {substring} ;


procedure delstring(var t: packed array [tlow..thigh: integer] of char;
                    start, span: integer);

  var
    i, limit, tlen: integer;


  begin {deletestring}
    if tlow <> 0 then abort('deletestring')
    else
      begin
      tlen := ord(t[0]);
      if span < 0 then
        begin
        span := - span;
        start := start - span
        end;
      limit := start + span;
      if start < 1 then start := 1;
      if limit > tlen + 1 then limit := tlen + 1;
      span := limit - start;
      if span > 0 then
        begin
        for i := 0 to tlen - limit do t[start + i] := t[limit + i];
        t[0] := chr(ord(t[0]) - span);
        end;
      end;
  end {deletestring} ;


procedure insert(var t: packed array [tlow..thigh: integer] of char;
                 s: packed array [slow..shigh: integer] of char;
                 p: integer);

  var
    i, j, tlen, slen: integer;


  begin {insert}
    if (tlow <> 0) or (slow <> 0) and (slow <> 1) then abort('insert')
    else
      begin
      tlen := ord(t[0]);
      if slow = 0 then slen := ord(s[0])
      else slen := shigh;
      if slen > 0 then
        if (p > 0) and (p <= tlen + 1) then
          begin
          if slen + tlen >= thigh then tlen := thigh
          else tlen := slen + tlen;
          for i := tlen downto p + slen do t[i] := t[i - slen];
          if tlen < p + slen then j := tlen
          else j := p + slen - 1;
          for i := p to j do t[i] := s[i - p + 1];
          t[0] := chr(tlen);
          end
        else abort('insert') { error: non-contiguous string } ;
      end;
  end {insert} ;


procedure assign(var t: packed array [tlow..thigh: integer] of char;
                 s: packed array [slow..shigh: integer] of char);

  var
    slen: integer;
    i: integer;


  begin {assign}
    if (tlow <> 0) and (tlow <> 1) or (slow <> 0) and (slow <> 1) then
      abort('assign - bad arguments');
    if slow = 0 then
      slen := ord(s[0])

   {User option here - the following code removes all trailing blanks
    from a literal string when inserting it into a variable string...
    This may be annoying to some users as you may wish to actually assign
    blanks to the end of a string in this manner.  To change the code
    comment out the following :

        else begin
         slen := shigh;
         while (s.....
         end;

    and replace the "else begin" with

       else slen := shigh;

   }
    else
      begin
      slen := shigh;
      while (s[slen] = ' ') and (slen > 1) do slen := slen - 1;
      end;
    if slen > thigh then abort('assign - destination string too short');
    for i := 1 to slen do t[i] := s[i];
    if tlow = 0 then t[0] := chr(slen)
    else if tlow = 1 then
      for i := slen + 1 to thigh do t[i] := ' ';
  end {assign} ;


procedure asschar(var t: packed array [tlow..thigh: integer] of char;
                  c: char);


  begin {assignchar}
    if tlow <> 0 then abort('assignchar');
    t[0] := chr(1);
    t[1] := c;
  end {assignchar} ;


function equal(s1: packed array [s1low..s1high: integer] of char;
               s2: packed array [s2low..s2high: integer] of char): boolean;

  var
    s1len, s2len: integer;
    eq: boolean;
    i: integer;


  begin {equal}
    if (s1low <> 0) and (s1low <> 1) or (s2low <> 0) and (s2low <> 1) then
      abort('equal - bad arguments')
    else
      begin
      if (s1low = 0) then s1len := ord(s1[0])
      else
        begin
        s1len := s1high;
        while (s1[s1len] = ' ') and (s1len > 1) do s1len := s1len - 1;
        end;
      if (s2low = 0) then s2len := ord(s2[0])
      else
        begin
        s2len := s2high;
        while (s2[s2len] = ' ') and (s2len > 1) do s2len := s2len - 1;
        end;
      if s1len <> s2len then equal := false
      else
        begin
        eq := true;
        for i := 1 to s1len do eq := eq and (s1[i] = s2[i]);
        equal := eq;
        end;
      end;
  end; {equal}


procedure leftstring(var t: packed array [tlow..thigh: integer] of char;
                     s: packed array [slow..shigh: integer] of char;
                     last: integer);

  var
    i: integer;


  begin
    if (last >= shigh) or (last < 2) then abort('leftstring')
    else
      begin
      for i := 1 to last do t[i] := s[i];
      t[0] := chr(last);
      end; {else}
  end; {leftstring}


procedure rightstring(var t: packed array [tlow..thigh: integer] of char;
                      s: packed array [slow..shigh: integer] of char;
                      first: integer);

  var
    i: integer;


  begin
    if (first < 1) or (first >= shigh) then abort('rightstring')
    else
      begin
      for i := first to shigh do t[(i + 1) - first] := s[i];
      t[0] := chr(shigh - first + 1);
      end; {else}
  end; {rightstring}
