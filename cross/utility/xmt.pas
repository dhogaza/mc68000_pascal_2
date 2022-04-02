{[R+,B+,L-]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF CONFIDENTIAL SOFTWARE:
  Copyright 1982 by Oregon Software, Inc.
  ALL RIGHTS RESERVED.

  This computer program is the proprietary property of Oregon
  Software, Inc. of Portland, Oregon, U.S.A., and may be used
  and copied only as specifically permitted under written
  license agreement signed by Oregon Software, Inc.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.
}

{

  Receive text files sent from host system

  Last modified by KRIS on 21-Nov-1990 17:21:16

  Purpose:

Update release version for PU-VV0-GS0 at 2.3.0.1

}

PROGRAM xmt;

{

XFR host to 68K transmission format


Each text line sent to the XMT program is formatted by the XFR program 
to add extra information to detect missing lines and parity errors.  
Three characters are added to the front of each line.  Each character
represents a 5 bit number selected from the range A..Z,0..5, with A 
being 0 and 5 being 31.

The first character is the line sequence character.  This is used to 
detect missed lines.  If a line is missed, there is no recovery, and the 
user must send the file again.  Note that lines will be missed if a line 
is sent before the 68K has issued a read.  There seems to be no 
typeahead on the Versados system.

The second character is the checksum byte.  The checksum is computed for 
the third through last characters on the line.  The checksum is 
calculated by taking the current checksum, multiplying by 3, adding in 
the current character, and reducing the result mod 32.  This will result 
in a checksum in the range 0..31, which is represented by a single 
character as described above.

The third character is the leading space count.  Our research has shown 
that up to half of a Pascal program consists of spaces inserted at the 
front of each source line to "line up" the statements.  These spaces are 
compressed out, and the number of spaces (5 bits worth) is represented 
by the third character.  This compression reduces the time spent sending 
Pascal programs over the link.

The remainder of the line, if any, consists of the text for the line, 
and it is terminated with a carriage return.  A "%" is used to mark the 
end of the file.

The XMT program sends special information up the link to the host to 
control the information received.  Each command is prefixed with a 
control-A character (chr(1)).  Following the control-A is a numeric 
digit describing what action to take.  The current codes are:

    0 -- Initialize
    1 -- Send next line
    2 -- Resend last line (due to checksum failure)
    3 -- Terminate processing (% encountered as first char on line)


}

  TYPE
    commandline = PACKED ARRAY [1..132] OF char;

  VAR
    f: text;
    lines, s, spacecount: integer;
    ch: char;
    done, fail: boolean;
    escape, sequence, checkch, spacech: char;
    checksum, i: integer;
    l: PACKED ARRAY [1..255] OF char;
    linelen: integer;
    map: PACKED ARRAY [1..32] OF char;
    filename: commandline;

{
  Get user's command line
}


  PROCEDURE p_getcmd(VAR line: PACKED ARRAY [l..h: integer] OF char;
                     VAR length: integer);
    EXTERNAL;


  BEGIN
    p_getcmd(filename, linelen);
    lines := 1;
    map := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ012345';
    done := false;
    escape := chr(1);
    rewrite(f, filename);
    writeln(escape, '0');
    writeln(escape, '1');
    REPEAT
      fail := false;
      linelen := 0;
      read(ch);
      IF ch = '%' THEN
        BEGIN
        done := eoln(input);
        IF NOT done THEN
          BEGIN
          linelen := linelen + 1;
          l[linelen] := ch;
          END;
        END;
      IF NOT done THEN
        BEGIN
        IF eoln(input) THEN fail := true
        ELSE sequence := ch;
        IF NOT fail THEN
          IF eoln(input) THEN fail := true
          ELSE read(checkch);
        WHILE NOT eoln(input) DO
          BEGIN
          read(ch);
          linelen := linelen + 1;
          l[linelen] := ch;
          END;
        readln(input);
        IF (map[(lines MOD 32) + 1] <> sequence) AND NOT fail THEN
          BEGIN
          done := true;
          writeln(chr(7), '*** Line dropped...Try again');
          END
        ELSE
          BEGIN
          checksum := 0;
          FOR i := 1 TO linelen DO
            checksum := (checksum * 3 + ord(l[i])) MOD 32;
          IF map[checksum + 1] <> checkch THEN fail := true;
          IF NOT fail THEN
            BEGIN
            spacech := l[1];
            IF spacech < 'A' THEN spacecount := ord(spacech) - ord('0')
            ELSE spacecount := ord(spacech) - ord('A');
            FOR s := 1 TO spacecount DO write(f, ' ');
            FOR i := 2 TO linelen DO write(f, l[i]);
            writeln(f);
            lines := lines + 1;
            writeln(escape, '1');
            END
          ELSE writeln(escape, '2');
          END;
        END;
    UNTIL done;
    writeln(escape, '3');
    writeln;
    writeln;
    writeln('XMIT -- ', lines - 1: 1, ' lines received');
  END.
