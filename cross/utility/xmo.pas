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

  Receive object modules from host system

  Last modified by KRIS on 21-Nov-1990 17:21:21

  Purpose:

Update release version for PU-VV0-GS0 at 2.3.0.1

}

PROGRAM xmo;

{

XFR host to 68K transmission format


Each object record sent to the XMO program is broken up into 8 64
character records by the XFR program.  Each of these 8 records is
prefixed by three bytes of control information which are used to detect
dropped lines and parity errors.  Each of the characters represents a 5
bit number selected from the range A..Z,0..5, with A being 0 and 5 being
31. 

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

The third character is used to mark an entire line of zeroes.  This will 
happen at the end of a record which is partially filled.  If the third 
control character is "Z", then the line is all zeroes.  If the third 
character is "A", then the line contains 64 bytes of data.

The remainder of the line consists of 64 hex bytes representing part of
the object record.  It takes two hex characters to represent a single
byte in the object record.  A "%" is used to indicate the end of the
object records.  When an entire module has been sent, XMO will print the
name of the object module and how many object records were loaded. The
module name is contained in the first object record. 

The XMO program sends special information up the link to the host to 
control the information received.  Each command is prefixed with a 
control-A character (chr(1)).  Following the control-A is a numeric 
digit describing what action to take.  The current codes are:

    0 -- Initialize
    1 -- Send next line
    2 -- Resend last line (due to checksum failure)
    3 -- Terminate processing (% encountered as first char on line)


}

  TYPE
    buf = PACKED ARRAY [1..256] OF char;
    commandline = PACKED ARRAY [1..132] OF char;

  VAR
    f: FILE OF buf;
    b: buf;
    modulename: PACKED ARRAY [1..10] OF char;
    bufptr, recordcount: integer;
    lines, s, spacecount: integer;
    ch: char;
    done, fail: boolean;
    escape, sequence, checkch, spacech: char;
    checksum, i: integer;
    l: PACKED ARRAY [1..255] OF char;
    linelen: integer;
    map: PACKED ARRAY [1..32] OF char;
    filename: commandline;


  PROCEDURE p_getcmd(VAR line: packed array [l..h:integer] of char;
                       VAR length: integer);
    EXTERNAL;


  FUNCTION xord(c: char): integer;


    BEGIN
      IF c <= '9' THEN xord := ord(c) - ord('0')
      ELSE xord := ord(c) - ord('A') + 10;
    END;


  BEGIN
    p_getcmd(filename, lines);
    lines := 1;
    bufptr := 1;
    recordcount := 0;
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
            IF l[1] = 'Z' THEN
              FOR i := 2 TO 66 DO l[i] := '0';
            i := 2;
            WHILE i < 65 DO
              BEGIN
              b[bufptr] := chr(xord(l[i + 1]) * 16 + xord(l[i]));
              bufptr := bufptr + 1;
              i := i + 2;
              END;
            IF bufptr >= 256 THEN
              BEGIN
              f^ := b;
              put(f);
              bufptr := 1;
              recordcount := recordcount + 1;
              IF recordcount = 1 THEN
                FOR i := 1 TO 10 DO modulename[i] := b[i + 2];
              END;
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
    write('XMIT -- ', recordcount: 1, ' records loaded for module ');
    FOR i := 1 TO 10 DO
      IF modulename[i] <> ' ' THEN write(modulename[i]);
    writeln(output);
  END.
