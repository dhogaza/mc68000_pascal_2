{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler SCAN-ANALYS Interface Declarations

 Last modified by KRIS on 21-Nov-1990 15:18:03
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

{ Pascal tokens -- identified by scanner }

type

  tokentype = (programsym, labelsym, constsym, typesym, varsym, sharedsym,
               proceduresym, functionsym, uparrow, arraysym, filesym,
               setsym, recordsym, stringsym, univsym, packedsym,
               originsym, usesym, definesym, beginsym, ifsym, casesym,
               whilesym, repeatsym, forsym, withsym,
               gotosym, eql, lss, gtr, neq, leq, geq, insym, plus, minus,
               orsym, star, slash, divsym, modsym, andsym, ofsym, endsym,
               elsesym, thensym, otherwisesym, dosym, untilsym, tosym,
               downtosym, notsym, at, nilsym, colon, dot, dotdot, comma,
               semicolon, becomes, lpar, rpar, lbrack, rbrack, intconst,
               realconst, dblrealconst, charconst, stringconst, ident, eofsym,
               lineinc, lineadd, newfile);

  { tokenrecord is the interface between scan and analys }

  { the Modula-2 and Pascal-2 scanner differ here, in that M-2 keeps
    the file index of the current source file in a global variable.

    This is impossible in the P-2 scanner, as we support the optional
    configuration of the compiler as four passes, with scanner output
    written to an intermediate file.  M-2 (due to the way definition
    modules are handled) requires the SCAN and ANALYS to run as one
    pass, thus gets a way with this little hack.
  }

  tokenrecord =
    record
      baseline: integer; {line the current file started on}
      line: integer; { line this token appeared on }
      fileindex: integer; { pointer into file name structure }
      filepos: integer; { getpos position of this token }
      left, right: columnindex; { where on the line it appeared }
      case token: tokentype of { the actual token }
        ident:
          (key: hashindex; { key uniquely identifies us }
           keypos: integer { location of name in string file } );
        intconst, charconst: (intvalue: integer {value or ord(value)} );
        realconst, dblrealconst: (realvalue: realarray {value} );
        stringconst:
          (pos: integer;
           len: columnindex) { position and length within stringfile }
    end;

  tempfileonetype = file of packed array [0..diskbufsize] of
      packed record
        case boolean of
          true: (byte: hostfilebyte); {integer data}
          false:
            (toke: tokentype); {represents "token" field of tokenrecord}
      end;

var
{ The following file declarations must be included in each pass,
  and must be the first variables declared in that pass.
}

  tempfileone: tempfileonetype; {interface file scan-analys, travrs-code}
  tempfiletwo: tempfiletwotype; {interface file analys-travrs}

{ Nexttoken is shared by scan and analys when bigcompilerversion is true,
  thereby avoiding intermediate file I/O.
}

  nexttoken: tokenrecord; {token being built by scanner}
