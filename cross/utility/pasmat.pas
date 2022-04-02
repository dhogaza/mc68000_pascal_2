{[b+,a+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1980, 1981, 1982, 1983 by Oregon Software, Inc.
  All Rights Reserved.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Pascal source code formatting utility
  Release version: 0045  Level: 1  Date: 21-Nov-1990 17:18:02
  Processor: ALL
  System: ALL
}

                    {------------------------------------*
                     | PASMAT: PAScal source code forMAT |
                     *-----------------------------------*}

program Pasmat(Input, Output, Source, Result);

{PASMAT: A Pascal Text Formatter.

 Pasmat is a program which formats a Pascal program (or fragment)
 according to standardized formatting rules.  It also converts the case
 of the identifiers and reserved words to your specification.

 A series of directives allow control over various aspects of the
 formatting.  Many of these are the result of strong differences of
 opinion amoung potential users of the formatter.

 Anyone studying this program should have a copy of the Pasmat users
 manual.

 The program was originally written by W. D. Thompson, who hashed it
 badly, and has been re-written piecemeal by M. S. Ball.  Almost the
 only remaining signs of Thompson are in the input lexical analyzer and
 some mild awkwardnesses in organization and layout.

 The formatter does an (almost) complete syntactic check of the program
 as it formats, and if it gets confused as to where it is, aborts and
 does not create an output element.  This avoids a problem which the
 previous formatter had of losing track of its parsing and producing
 complete garbage as an output element.  This would sometimes get
 substituted for the original element, and made recovery very
 difficult.  The extra checking costs a bit of time and code space,
 but seems worth it overall.  It also allows a very flexible formatting
 policy.

 There is a delayed output buffer to allow conditional modification of
 formatting decisions.  This allows the user to make tentative
 decisions and modify them later.  For instance, the user can note
 potential break points in a line and go back and use them when
 the line fills.  This facility is also used to allow short statements
 to follow case labels directly.  The statement is put on the next
 line, then if it would have fit can be moved back up to the line with
 the label.

 Comments are always difficult to handle in a Pascal formatter, and
 pasmat attempts to handle them in a way which provides the user with
 some control of their formatting.  The comment handling is completly
 separate from the normal formatting, and can be changed without
 affecting other areas.

 History:
   Original version perpetrated by W. D. Thompson 18 Oct 78

   Rewritten by M. S. Ball, 18 Oct 79, to make a system
   which people would be willing to use.

   Modified by M. S. Ball in Oct 80 to run using OMSI Pascal-2,
   and to process code for that compiler.  The "K", "N", and "A"
   directives were added at this time.

 For further data or bug reports:

   Oregon Software
   6915 SW Macadam Ave.
   Portland, OR 97219
   (503)245-2202

}


{*----------------------*
 | Labels and Constants |
 *----------------------*}

  label
    99;

  const

    TitleHeader = 'PASMAT r3.0'; {release.version} {revised 13 Feb 81}

    MaxLineLen = 132; {max output line length}
    Bufsize = 134; {output buffer size, > max_line_len}
    BufsizeP1 = 135; {buffer size +- 1}
    BufsizeM1 = 133;

    MaxWordLen = 9; {reserved words char size}
    NoResWords = 46; {number of reserved words}
    DefaultOutLine = 78; {default output line length}
    DefaultTabSpaces = 2; {logical indentation increments}
    DefaultCommentSpaces = 1; {spacing before and after comments}
    MaxBreakLevel = 4; {max number of break levels}

    Ff = 12; {ascii form feed character}
    Ht = 9; {ascii tab character}

    {identifier spelling constants}
    HashMax = 64; {size of hash table}
    HashLim = 63; {top entry in hash table}

    StringBlockSize = 512; {size of a block of the string table}
    StringBlockMax = 511; {max entry in a string block}
    StringIndexMax = 63; {max entry in the string index}

    TabInterval = 8; {standard tab interval}


{*-------*
 | Types |
 *-------*}

  type

    Symbols = (AndSy, ArraySy, BeginSy, CaseSy, ConstSy, DivSy, DoSy,
               DowntoSy, ElseSy, EndSy, ExternSy, FileSy, ForSy, FortranSy,
               ForwardSy, FunctionSy, GotoSy, IfSy, InSy, LabelSy, ModSy,
               NilSy, NonpascalSy, NotSy, OfSy, OrSy, OriginSy, OtherwiseSy,
               PackedSy, ProcedureSy, ProgramSy, RecordSy, RepeatSy, SetSy,
               StringSy, ThenSy, ToSy, TypeSy, UnivSy, UntilSy, VarSy,
               WhileSy, WithSy, Plus, Minus, Mult, Divide, Becomes, Period,
               Comma, Semicolon, Colon, Equal, RelOp, Pointer, Subrange,
               Apostrophy, OpenParen, CloseParen, OpenBrack, CloseBrack,
               Identifier, Number, StringConst, Comment, TextEnd, UseSy,
               DefineSy, SharedSy);

    {basic symbol enumeration}
    SetOfSyms = set of Symbols; {set ops on basic symbols}
    StringType = packed array [1..12] of Char; {identifier type for sirtag}
    WordType = packed array [1..MaxWordLen] of Char; {reserved}
    LenTableType = {index into reserved word table by length}
      record
        LowIndex, HiIndex: 1..NoResWords;
      end;

    LineIndex = 0..MaxLineLen;
    Actions = (Graphic, Spaces, BeginLine);
    BufferIndex = 0..BufsizeM1; {output buffer index}
    CharBuffer = array [BufferIndex] of
        record
          case ActionIs: Actions of
            Spaces, BeginLine: (Spacing: LineIndex);
            Graphic: (Character: Char)
        end;

    ColLog =
      record
        LogChar: Integer; {char_count at time of log}
        LogCol: LineIndex; {write_col at time of log}
        LogLine: Integer; {current_line at time of log}
      end;

    BreakLev = 0..MaxBreakLevel; {expression break priorities}

    AbortKind = (Syntax, Nesting, ComFormat); {error types}

    HashValue = 0..HashMax; {possible hash values}

    {string table description}

    StringBlockIndex = 0..StringIndexMax; {index table index}
    StringPieceIndex = 0..StringBlockMax; {index to chars in a piece}
    StringBlock = packed array [StringPieceIndex] of Char;

    {identifier spelling bookkeeping}
    IdPtr = ^IdDescr;

    IdDescr =
      packed record
        Next: IdPtr; {next id with this hash entry}
        Start: Integer; {start of identifier spelling in string table}
        Len: LineIndex; {length of identifier}
      end;



{*-----------*
 | Variables |
 *-----------*}

  var
    {Structured Constants}

    SpaceBefore, SpaceAfter: SetOfSyms; {individual symbol spacing}

    Alphanumerics: SetOfSyms; {alpha symbols}
    ResvWrd: array [1..NoResWords] of WordType; {reserved word table}
    ResSymbol: array [1..NoResWords] of Symbols; {symbols for resv_wrd}
    ResLen: array [2..MaxWordLen] of LenTableType; { length index}

    UpperCase: array [Char] of Char;
    LowerCase: array [Char] of Char; {case conversion tables}
    ProgSet, BlockBegSys, StatSet: SetOfSyms; {syntactic symbol types}

    Constants: SetOfSyms; {symbols which can be constants}
    HeadingBegSys: SetOfSyms; {symbols which begin a block heading}

    TypeBegSys: SetOfSyms; {type beginning symbols}
    ExprBegSys: SetOfSyms; {expression beginning symbols}
    RelOps: SetOfSyms; {relational operators}
    ArithOps: SetOfSyms; {arithmetic operators}

    {Formatting variables}

    Indent: Integer; {current number of indentation spaces}
    StatIndent: Integer; {indentation for major statement}
    WriteCol: Integer; {current output column}
    SymbolBreak: array [BreakLev] of
        record
          BufChar: Integer; {character in buffer}
          BreakCol: LineIndex; {output column}
        end;
    LastSym: Symbols; {last symbol processed}
    SymWritten: Boolean; {last symbol was written}
    IndentState: array [LineIndex] of LineIndex;
    IndentLevel: LineIndex; {these make a stack of indent levels}
    EndLine: Boolean; {last symbol ends the line}

    {miscellaneous}

    Result: Text; {output file}
    Source: Text; {input file}
    OutputLine: Integer; {line numbers for output}
    CurrentLine: Integer; {line number being written}
    InputLine: Integer; {input line number}

    {Formatting Control Values}

    OutLineLen: Integer; {current output line length}
    OneHalfLine: Integer; {significant point upon line}
    FiveEighthLine: Integer; {"}
    ThreeFourthLine: Integer; {"}

    TabSpaces: Integer; {spaces to indent for each level}
    ContinueSpaces: Integer; {spaces to indent continuation line}
    CommentSpaces: Integer; {spaces before statement comment}
    StatsPerLine: Integer; {statements per line}

    {flags to direct formatting}

    UcResWords: Boolean; {convert reserved words to UC}
    UcIdents: Boolean; {convert identifiers to UC}
    LitCopy: Boolean; {copy identifiers literally}
    PortabilityMode: Boolean; {eliminate underscores}
    Formatting: Boolean; {do formatting (otherwise, copy)}
    NewFormatting: Boolean; {start formatting at end of comment}
    Bunching: Boolean; {bunch statements on one line}
    ConvertToTabs: Boolean; {convert leading spaces to tabs}
    OtherwiseKluge: Boolean; {converts else to otherwise in case}
    FirstSpelling: Boolean; {convert equivalent ids to first spelling}
    NoNewLine: Boolean; {do not insert extra new-lines}
    NewNoNewline: Boolean; {start NoNewLine at end of comment}
    StandardRep: Boolean; {convert to standard representation}

    {lexical scanner variables}

    SymbolFound: Boolean; {success from lexical analysis}
    NewInputLine: Boolean; {true when no chars as yet on new line}
    BlankLines: Integer; {Count of blank lines read but not printed}
    EndFile: Boolean; {eof read}
    Ch: Char; {current character for lexical analysis}
    DoublePeriod: Boolean; {set if double period found}
    Column: Integer; {input column for last char input}
    TabColumn: Integer; {column at end of tab, for conversion to spaces}
    Sym: Symbols; {current basic symbol from lex}
    Symbol: array [LineIndex] of Char; {workspace for lex analysis}
    SymLen: 0..MaxLineLen; {index into WINDOW array}

    {output character buffering}

    Unwritten: CharBuffer; {unwritten characters}
    CharCount: Integer; {characters written so far}
    Oldest: BufferIndex; {oldest char in buffer}
    InitialBlanks: Integer; {initial blanks on a line}
    SavingBlanks: Boolean; {true if saving blanks to convert to tabs}

    {error handling variables}

    Overflows: 0..Maxint; {number of line overflows}
    FirstOverflow: 0..Maxint; {line where first overflow occured}
    ComOverflows: 0..Maxint; {number of comment overflows}
    FirstComOverflow: 0..Maxint; {line of first comment overflow}

    {identifier spelling variables}

    HashTable: array [HashValue] of IdPtr; {main hash table}

    StringIndex: array [StringBlockIndex] of ^StringBlock;
    {string table base array}

    StringTop: Integer; {last character in string table}


{*---------------------------------*
 | Read and Process Command String |
 *---------------------------------*}

  const
    DefaultExt = 'PAS'; {default filename extension}
    CSIprompt = 'PMT>'; {prompt to use if necessary}
    %include 'csicon'; {constants for csi processing}

  type
    ArgType = (UnknownArg, OutputFileArg, InputFileArg, OptionsArg,
               MalformedArg, MissingArg);
    SubArgType = 0..0;
    %include 'csityp'; {types for csi processing}

    ArgValuePtr = ^ArgValue;

  var
    {Operating system interface variables}

    InitialDirectives: Boolean; {set if initial directives provided}
    InitDirIndex: Integer; {initial directives index}
    InitDir: ArgValuePtr; {saves initial directives argument}
    OutputArg, TempArg: ArgValue; {filename buffers}
    error: Boolean; {success/failure exit status}
    OutputFlg: Boolean; {output file specified flag}

    %include 'csipro'; {procedures for csi processing}
    %include 'getcs'; {GetCS procedure}
    %include 'fixarg'; {FixFileArg csi procedure}
    %include 'fixout'; {FixOutputArg, FixTempOutput procedures}


  procedure exitst(status: Integer);
    external;


  procedure csi;

  {Read and process command line.
  }

    const
      {[f-]}
      ArgDefs = ArgDefTable (
        (('                ',  1,  0), 0, OptionalArg, NullArg),
        (('Output_File     ',  1, 11), 2, OptionalArg, FileArg),
        (('Input_File      ', 16, 10), 1, RequiredArg, FileArg),
        (('Options         ',  1,  7), 0, OptionalArg, StringArg),
        (('                ',  1,  0), 0, OptionalArg, NullArg),
        (('                ',  1,  0), 0, OptionalArg, NullArg));
      {[f+]}

    type
      ErrorMsg = (UnknownArgMsg, MalformedArgMsg, MissingArgMsg, NoInputMsg,
                  NoOutputMsg, FixupMsg, ExtraOutputMsg, ExtraInputMsg);

    var
      InputFlg: Boolean; {existence flags}
      InputArg: ArgValue; {filename buffer}
      flg: Integer; {file existence check value}
      j: iArgValue; {index conunter}
      DefExtVar: FileExt; {file extension variable}


    procedure SetupError(msg: ErrorMsg;
                         arg: ArgValue);


      begin
        case msg of
          UnknownArgMsg: write('Unexpected argument');
          MalformedArgMsg: write('Bad argument syntax');
          MissingArgMsg: write('Required argument missing');
          NoInputMsg: write('Can''t open input file');
          NoOutputMsg: write('Can''t open output file');
          ExtraOutputMsg: write('Extra output file');
          ExtraInputMsg: write('Extra input file');
          end;
        if arg.Len > 0 then write(' (', arg.txt: arg.Len, ')');
        writeln;
        error := true;
      end;


    procedure ProcessArg(arg: ArgValue;
                         typ: ArgType);

    {Process arguments returned by CSI procedures.
    }


      begin
        case typ of
          UnknownArg: SetupError(UnknownArgMsg, arg);
          OutputFileArg:
            begin
            if OutputFlg then SetupError(ExtraOutputMsg, arg);
            OutputArg := arg;
            OutputFlg := true;
            end;
          InputFileArg:
            begin
            if InputFlg then SetupError(ExtraInputMsg, arg);
            InputArg := arg;
            InputFlg := true;
            end;
          OptionsArg:
            begin
            InitialDirectives := true;
            new(InitDir);
            InitDir^ := arg;
            InitDirIndex := 1;
            end;
          MalformedArg: SetupError(MalformedArgMsg, arg);
          MissingArg: SetupError(MissingArgMsg, arg);
          end;
      end;


    begin
      InputFlg := false;
      OutputFlg := false;
      InitialDirectives := false;

      error := false;
      GetCS(ArgDefs, ProcessArg);
      if error then goto 99;

      for j := 1 to ExtLen do DefExtVar[j] := DefaultExt[j];
      FixFileArg(InputArg, ActualFile, DefExtVar, InputArg);
      reset(Source, InputArg.txt, , flg);
      if flg = - 1 then
        begin
        SetupError(NoInputMsg, InputArg);
        goto 99;
        end;

      if OutputFlg then
        FixFileArg(OutputArg, ActualFile, DefExtVar, OutputArg)
      else FixFileArg(InputArg, ActualFile, DefExtVar, OutputArg);
      FixOutputArg(OutputArg, TempArg);
      flg := 0;
      rewrite(Result, TempArg.txt, , flg);
      if flg = - 1 then
        begin
        SetupError(NoOutputMsg, OutputArg);
        goto 99;
        end;
    end; {csi}


{*--------------------------*
 | Initialize Set Constants |
 *--------------------------*}


  procedure InitSets;


    begin {initialize set constants}

      SpaceBefore := [AndSy, DivSy, DoSy, DowntoSy, InSy, ModSy, OfSy, OrSy,
                     ThenSy, ToSy, Plus, Minus, Mult, Divide, Becomes, Equal,
                     RelOp, UseSy, DefineSy];
      SpaceAfter := [AndSy, ArraySy, CaseSy, DivSy, DowntoSy, ForSy,
                    FunctionSy, GotoSy, IfSy, InSy, ModSy, NotSy, OfSy, OrSy,
                    PackedSy, ProcedureSy, ProgramSy, StringSy, ToSy, UntilSy,
                    WhileSy, WithSy, Plus, Minus, Mult, Divide, Becomes,
                    Comma, Semicolon, Colon, Equal, RelOp, Comment, UseSy,
                    DefineSy];
      Alphanumerics := [AndSy..WithSy, Identifier, Number];

      HeadingBegSys := [LabelSy, ConstSy, TypeSy, VarSy, ProcedureSy,
                       FunctionSy, SharedSy];
      BlockBegSys := HeadingBegSys + [BeginSy];
      StatSet := [BeginSy, IfSy, CaseSy, WhileSy, RepeatSy, ForSy, WithSy,
                 GotoSy, Number, Identifier];
      Constants := [Number, Identifier, StringConst, Plus, Minus, NilSy];
      ExprBegSys := Constants + [Pointer, NotSy, NilSy, OpenBrack, OpenParen];
      ArithOps := [Plus, Minus, Mult, Divide, DivSy, ModSy];
      RelOps := [Equal, RelOp, InSy];
      TypeBegSys := Constants + [ArraySy, FileSy, Pointer, RecordSy, SetSy,
                    StringSy, OpenParen] - [NilSy];
    end {init_sets} ;



                                        {*---------------------------*
                                         | initialize reserved words |
                                         *---------------------------*}


  procedure InitResvWrd;


    begin {[s=2] initialize reserved word array and length indices into
           reserved word array for length keyed search}

      ResLen[2].LowIndex := 1;            ResLen[2].HiIndex := 6;
      ResLen[3].LowIndex := 7;            ResLen[3].HiIndex := 16;
      ResLen[4].LowIndex := 17;           ResLen[4].HiIndex := 24;
      ResLen[5].LowIndex := 25;           ResLen[5].HiIndex := 30;
      ResLen[6].LowIndex := 31;           ResLen[6].HiIndex := 38;
      ResLen[7].LowIndex := 39;           ResLen[7].HiIndex := 41;
      ResLen[8].LowIndex := 41;           ResLen[8].HiIndex := 43;
      ResLen[9].LowIndex := 44;           ResLen[9].HiIndex := 46;
      ResvWrd[1] := 'do       ';          ResSymbol[1] := DoSy;
      ResvWrd[2] := 'if       ';          ResSymbol[2] := IfSy;
      ResvWrd[3] := 'in       ';          ResSymbol[3] := InSy;
      ResvWrd[4] := 'of       ';          ResSymbol[4] := OfSy;
      ResvWrd[5] := 'or       ';          ResSymbol[5] := OrSy;
      ResvWrd[6] := 'to       ';          ResSymbol[6] := ToSy;
      ResvWrd[7] := 'and      ';          ResSymbol[7] := AndSy;
      ResvWrd[8] := 'div      ';          ResSymbol[8] := DivSy;
      ResvWrd[9] := 'end      ';          ResSymbol[9] := EndSy;
      ResvWrd[10] := 'for      ';         ResSymbol[10] := ForSy;
      ResvWrd[11] := 'mod      ';         ResSymbol[11] := ModSy;
      ResvWrd[12] := 'nil      ';         ResSymbol[12] := NilSy;
      ResvWrd[13] := 'not      ';         ResSymbol[13] := NotSy;
      ResvWrd[14] := 'set      ';         ResSymbol[14] := SetSy;
      ResvWrd[15] := 'var      ';         ResSymbol[15] := VarSy;
      ResvWrd[16] := 'use      ';         ResSymbol[16] := UseSy;
      ResvWrd[17] := 'case     ';         ResSymbol[17] := CaseSy;
      ResvWrd[18] := 'else     ';         ResSymbol[18] := ElseSy;
      ResvWrd[19] := 'file     ';         ResSymbol[19] := FileSy;
      ResvWrd[20] := 'goto     ';         ResSymbol[20] := GotoSy;
      ResvWrd[21] := 'then     ';         ResSymbol[21] := ThenSy;
      ResvWrd[22] := 'type     ';         ResSymbol[22] := TypeSy;
      ResvWrd[23] := 'univ     ';         ResSymbol[23] := UnivSy;
      ResvWrd[24] := 'with     ';         ResSymbol[24] := WithSy;
      ResvWrd[25] := 'array    ';         ResSymbol[25] := ArraySy;
      ResvWrd[26] := 'begin    ';         ResSymbol[26] := BeginSy;
      ResvWrd[27] := 'const    ';         ResSymbol[27] := ConstSy;
      ResvWrd[28] := 'label    ';         ResSymbol[28] := LabelSy;
      ResvWrd[29] := 'until    ';         ResSymbol[29] := UntilSy;
      ResvWrd[30] := 'while    ';         ResSymbol[30] := WhileSy;
      ResvWrd[31] := 'downto   ';         ResSymbol[31] := DowntoSy;
      ResvWrd[32] := 'origin   ';         ResSymbol[32] := OriginSy;
      ResvWrd[33] := 'packed   ';         ResSymbol[33] := PackedSy;
      ResvWrd[34] := 'record   ';         ResSymbol[34] := RecordSy;
      ResvWrd[35] := 'repeat   ';         ResSymbol[35] := RepeatSy;
      ResvWrd[36] := 'string   ';         ResSymbol[36] := StringSy;
      ResvWrd[37] := 'define   ';         ResSymbol[37] := DefineSy;
      ResvWrd[38] := 'shared   ';         ResSymbol[38] := SharedSy;
      ResvWrd[39] := 'fortran  ';         ResSymbol[39] := FortranSy;
      ResvWrd[40] := 'forward  ';         ResSymbol[40] := ForwardSy;
      ResvWrd[41] := 'program  ';         ResSymbol[41] := ProgramSy;
      ResvWrd[42] := 'external ';         ResSymbol[42] := ExternSy;
      ResvWrd[43] := 'function ';         ResSymbol[43] := FunctionSy;
      ResvWrd[44] := 'otherwise';         ResSymbol[44] := OtherwiseSy;
      ResvWrd[45] := 'procedure';         ResSymbol[45] := ProcedureSy;
      ResvWrd[46] := 'nonpascal';         ResSymbol[46] := NonpascalSy;
    end {[s=1] init_resv_wrd} ;



                                        {*------------*
                                         | initialize |
                                         *------------*}


  procedure Initialize;

    var
      P: Integer;
      C: Char; {induction var}
      H: HashValue; {induction var}
      S: StringBlockIndex; {induction var}


    begin {initialize all global variables}
      InitSets;

      for C := Chr(0) to Chr(127) do
        begin
        LowerCase[C] := C;
        UpperCase[C] := C;
        end;
      for C := 'A' to 'Z' do
        begin
        LowerCase[C] := Chr(Ord(C) + Ord('a') - Ord('A'));
        UpperCase[Chr(Ord(C) + Ord('a') - Ord('A'))] := C;
        end;

      CharCount := 0;
      OutLineLen := DefaultOutLine;
      TabSpaces := DefaultTabSpaces;
      ContinueSpaces := (TabSpaces + 1) div 2;
      CommentSpaces := DefaultCommentSpaces;
      IndentLevel := 0;
      OneHalfLine := OutLineLen div 2;
      FiveEighthLine := 5 * OutLineLen div 8;
      ThreeFourthLine := 3 * OutLineLen div 4;
      StatsPerLine := 1;
      for P := 1 to OutLineLen do Symbol[P] := ' ';
      SymLen := 0;
      Indent := 0;
      StatIndent := 0;
      WriteCol := 0;
      SavingBlanks := false;
      Column := 0;
      TabColumn := 0;
      OutputLine := 1;
      CurrentLine := 0;
      InputLine := 1;
      NewInputLine := true;
      BlankLines := 0;
      Sym := Period;
      EndLine := false;
      EndFile := false;
      LastSym := Period;
      SymWritten := false;
      Ch := ' ';
      DoublePeriod := false;
      Formatting := true;
      NewFormatting := true;
      UcResWords := false;
      UcIdents := false;
      LitCopy := true;
      PortabilityMode := false;
      Bunching := false;
      ConvertToTabs := false;
      OtherwiseKluge := false;
      FirstSpelling := false;
      NoNewLine := false;
      NewNoNewline := false;
      StandardRep := true;
      Overflows := 0;
      ComOverflows := 0;
      InitResvWrd;
      for H := 0 to HashMax do HashTable[H] := nil;
      for S := 0 to StringIndexMax do StringIndex[S] := nil;
      StringTop := 0;
    end {initialize} ;


{*-----------------------------*
 | Terminate and Print Message |
 *-----------------------------*}


  procedure FinalData;


    begin {print summary data}
      if Overflows > 0 then
        begin
        write(Output, 'Token too wide for output at ', Overflows: 1,
              ' place');
        if Overflows > 1 then write(Output, 's, first error');
        writeln(Output, ' on line ', FirstOverflow: 1, '.');
        end;
      if ComOverflows > 0 then
        begin
        write(Output, 'Comment too wide for output at ', ComOverflows: 1,
              ' place');
        if ComOverflows > 1 then write(Output, 's, first');
        writeln(Output, ' on line ', FirstComOverflow: 1, '.');
        end;
      write(Output, OutputArg.txt: OutputArg.Len, ' formatting complete, ',
            OutputLine - 1: 1, ' line');
      if OutputLine > 2 then write('s');
      writeln(Output, ' output.');
    end; {final_data}


{*------------------*
 | Character output |
 *------------------*}


  procedure ClearBreaks;

    var
      i: BreakLev; {induction var}


    begin {clear out all symbol breaks}
      for i := 0 to MaxBreakLevel do SymbolBreak[i].BufChar := 0;
    end; {clear_breaks}


  procedure ResetCharCount;


    begin {reset the output character count to avoid overflow, taking care to
           preserve the actual buffer loc}
      if CharCount > BufsizeP1 then
        CharCount := CharCount mod Bufsize + 2 * Bufsize;
      ClearBreaks;
    end; {reset_char_count}


  procedure WriteA(Ch: Char);

    var
      i: LineIndex;


    begin {Write a character to the output buffer. If necessary (which it
           always is after the buffer is filled), write the previous contents
           of the buffer) }

      CharCount := CharCount + 1;
      Oldest := CharCount mod Bufsize;
      with Unwritten[Oldest] do
        begin
        if CharCount >= BufsizeP1 then
          if ActionIs = Graphic then
            begin
            if SavingBlanks then
              if Character = ' ' then InitialBlanks := InitialBlanks + 1
              else
                begin
                while ConvertToTabs and (InitialBlanks >= TabInterval) do
                  begin
                  write(Result, Chr(Ht));
                  InitialBlanks := InitialBlanks - TabInterval;
                  end;
                while InitialBlanks > 0 do
                  begin
                  write(Result, ' ');
                  InitialBlanks := InitialBlanks - 1;
                  end;
                SavingBlanks := false;
                write(Result, Character)
                end
            else write(Result, Character);
            end
          else if ActionIs = Spaces then
            begin
            if SavingBlanks then InitialBlanks := InitialBlanks + Spacing
            else for i := 1 to Spacing do write(Result, ' ');
            end
          else {action_is = begin_line}
            begin
            if CharCount > BufsizeP1 then writeln(Result);
            SavingBlanks := true;
            InitialBlanks := Spacing;
            OutputLine := OutputLine + 1;
            end;
        ActionIs := Graphic;
        Character := Ch;
        if Ch = Chr(Ht) then
          WriteCol := ((WriteCol + TabInterval) div TabInterval) * TabInterval
        else WriteCol := WriteCol + 1;
        end; {with}
    end; {write_a}


  procedure NewLine(Indent: LineIndex);


    begin {start a new line and indent it as specified}
      {fake a character, then change it}
      EndLine := false;
      WriteA(' ');
      with Unwritten[Oldest] do
        begin
        ActionIs := BeginLine;
        Spacing := Indent;
        end;
      WriteCol := Indent;
      CurrentLine := CurrentLine + 1;
    end; {new_line}


  procedure PrintLine(Indent: Integer);


    begin {print a line for formatting}
      if Formatting then
        begin
        while (BlankLines > 0) and (CurrentLine > 0) do
          begin
          NewLine(0);
          if NoNewLine then BlankLines := BlankLines - 1
          else BlankLines := 0;
          end;
        NewLine(Indent);
        end;
      BlankLines := 0;
      ClearBreaks;
    end; {print_line}


  procedure Space(N: Integer);


    begin {space n characters}
      if Formatting then
        begin
        WriteA(' ');
        with Unwritten[Oldest] do
          begin
          ActionIs := Spaces;
          if N >= 0 then Spacing := N
          else Spacing := 0;
          end;
        WriteCol := WriteCol + N - 1;
        end;
    end; {space}


  procedure FlushBuffer;

    var
      i: 0..BufsizeM1;


    begin {flush any unwritten buffer}
      for i := 0 to BufsizeM1 do WriteA(' ');
      writeln(Result);
    end; {flush_buffer}


  procedure FlushSymbol;

    var
      P: LineIndex; {induction var}


    begin {flush any accumulated characters in the buffer}
      if not SymWritten then for P := 1 to SymLen do WriteA(Symbol[P]);
    end; {flush_symbol}


  procedure throwaway(Ch: Char);


    begin {dummy procedure to throw away an output character}
    end; {throwaway}


                    {*-------------------------*
                     | INPUT/OUTPUT:  get char |
                     *-------------------------*}


  procedure GetChar;


    begin {read next character from input file}
      {The following is a kluge to read initial directives}
      if InitialDirectives then
        if InitDirIndex <= InitDir^.Len then
          begin
          Ch := InitDir^.txt[InitDirIndex];
          InitDirIndex := InitDirIndex + 1;
          end
        else
          begin
          InitialDirectives := false;
          dispose(InitDir);
          Ch := ']';
          end
          {End kluge}
      else if Column < TabColumn then
        begin
        Column := Column + 1;
        Ch := ' ';
        if not Formatting then WriteA(' ');
        end
      else if not Eof(Source) then
        if not eoln(Source) then
          begin {normal}
          Read(Source, Ch);
          if Ch = Chr(Ht) then
            begin {kluge in input tabs}
            TabColumn := ((Column + TabInterval) div TabInterval) *
                         TabInterval;
            Ch := ' ';
            end;
          if not Formatting then WriteA(Ch);
          Column := Column + 1;
          end {normal}
        else
          begin {eoln}
          if NewInputLine then BlankLines := BlankLines + 1
          else NewInputLine := true;
          Column := 0;
          TabColumn := 0;
          InputLine := InputLine + 1;
          readln(Source);
          if not Formatting then
            begin
            NewLine(0);
            ResetCharCount;
            end;
          Ch := ' ';
          end {eoln}
      else
        begin {eof}
        EndFile := true;
        Ch := ' ';
        end {eof}
    end {get_char} ;


{*----------------*
 | Error Handling |
 *----------------*}


  procedure LineOverflow;


    begin {token too long for output line, note it}
      Overflows := Overflows + 1;
      if Overflows = 1 then FirstOverflow := CurrentLine + 1;
    end; {line_overflow}


  procedure CommentOverflow;


    begin {block comment too long for output line, note it}
      ComOverflows := ComOverflows + 1;
      if ComOverflows = 1 then FirstComOverflow := CurrentLine;
    end; {comment_overflow}


  procedure Abort(Kind: AbortKind);

    var
      status: Boolean;


    begin {abort processing and do not create output element}
      FlushSymbol;
      WriteA(Ch);
      Close(Result);
      writeln(Output);
      if Kind = Syntax then write(Output, 'Syntax error detected, ')
      else if Kind = Nesting then
        write(Output, 'Too many indentation levels, ')
      else write(Output, 'Could not format comment, ');
      writeln(Output, OutputArg.txt: OutputArg.Len,
              ' processing aborted at input line ', InputLine: 1, '.');
      FixTempOutput(TempArg, OutputArg, false, status);
      if not status then writeln(Output, 'Can''t clean up temporary output');
      error := true;
      goto 99;
    end; {abort}


{*---------------------*
 | Indentation Control |
 *---------------------*}


  procedure IndentPlus(Delta: Integer);


    begin {increment indentation and check for overflow}
      if IndentLevel > MaxLineLen then Abort(Nesting);
      IndentLevel := IndentLevel + 1;
      IndentState[IndentLevel] := Indent;
      Indent := Indent + Delta;
      if Indent > OutLineLen then Indent := OutLineLen
      else if Indent < 0 then Indent := 0;
    end; {indent_plus}


  procedure Undent;


    begin {reset indent to the last value}
      Indent := IndentState[IndentLevel];
      IndentLevel := IndentLevel - 1;
    end; {undent}



  procedure SetSymbolBreak(Level: BreakLev);


    begin {mark a good spot to break a line}
      Space(0);
      with SymbolBreak[Level] do
        begin
        BufChar := CharCount;
        BreakCol := WriteCol;
        end;
    end; {set_symbol_break}


  procedure FormatLine(Indent: Integer);


    begin {Make a newline if allowed, otherwise mark this as a good break
           point.}
      if NoNewLine and not EndLine then SetSymbolBreak(MaxBreakLevel)
      else PrintLine(Indent);
    end;


{*---------*
 | Put_sym |
 *---------*}


  procedure PutSym;

    var
      Before: LineIndex; {spaces before this character}
      SymIndent: Integer; {indentation before this symbol}
      i: LineIndex; {induction var}
      L: BreakLev; {induction var}
      LastBreak: Integer; {last break character}


    function SpacesBefore(ThisSym, OldSym: Symbols): LineIndex;


      begin {determine the number of spaces before a symbol}
        if ((ThisSym in Alphanumerics) and (OldSym in Alphanumerics)) or
           (ThisSym in SpaceBefore) or (OldSym in SpaceAfter) then
          SpacesBefore := 1
        else SpacesBefore := 0;
      end; {spaces_before}


    begin {put_sym: put the current symbol to the output, taking care of
           spaces before the symbol. This also handles full lines, and tries
           to break lines at a convenient place}

      Before := SpacesBefore(Sym, LastSym);
      if EndLine or (Before + SymLen + WriteCol > OutLineLen) then
        begin {must handle an end of line}
        L := MaxBreakLevel;
        while (L > 0) and (SymbolBreak[L].BufChar = 0) do L := L - 1;
        with SymbolBreak[L] do
          if not EndLine and Formatting and (BufChar > 0) and
             (CharCount - BufChar < Bufsize) and
             (Before + SymLen + Indent + WriteCol - BreakCol <=
             OutLineLen) then
            begin
            with Unwritten[BufChar mod Bufsize] do
              begin
              ActionIs := BeginLine;
              Spacing := Indent
              end;
            WriteCol := WriteCol - BreakCol + Indent;
            CurrentLine := CurrentLine + 1;
            LastBreak := BufChar;
            end
          else
            begin {no good break spot, break it here}
            SymIndent := OutLineLen - SymLen;
            if SymIndent > Indent then SymIndent := Indent
            else if SymIndent < 0 then
              begin
              SymIndent := 0;
              LineOverflow
              end;
            PrintLine(SymIndent);
            LastBreak := CharCount;
            end;
        for L := 0 to MaxBreakLevel do
          with SymbolBreak[L] do if BufChar <= LastBreak then BufChar := 0;
        end; {if line overflow}
      if Unwritten[Oldest].ActionIs = BeginLine then Before := 0;
      if Before > 0 then
        with Unwritten[CharCount mod Bufsize] do
          if Formatting and (ActionIs = Spaces) then
            begin
            WriteCol := WriteCol - Spacing + Before;
            Spacing := Before;
            end
          else Space(Before);
      if Formatting then for i := 1 to SymLen do WriteA(Symbol[i]);
      LastSym := Sym;
      SymWritten := true;
      EndLine := false;
    end; {put_sym}



                                        {*-------------------------*
                                         | do_formatter_directives |
                                         *-------------------------*}


  procedure DoFormatterDirectives(procedure putch
                                       (C: Char));

    var
      OptChar: Char; {which option specified}


    procedure CopyAChar;


      begin {copy a character and get a new one}
        putch(Ch);
        GetChar;
      end; {copy_a_char}


    procedure SwitchDir(var Switch: Boolean);


      begin {read and set a switch directive, if char is not + or -, the value
             is unchanged}

        if Ch = '+' then
          begin
          Switch := true;
          CopyAChar
          end
        else if Ch = '-' then
          begin
          Switch := false;
          CopyAChar
          end;
      end; {switch_dir}


    procedure NumDir(var Value: Integer;
                     Min, Max: Integer {limits} );

      var
        TempVal: Integer; {value being accumulated}


      begin {read a numeric directive and set value. if the value is out of
             bounds it is set to the limit value}

        if Ch = '=' then CopyAChar;
        if (Ch >= '0') and (Ch <= '9') then
          begin
          TempVal := 0;
          while (Ch >= '0') and (Ch <= '9') do
            begin
            if TempVal <= (Maxint - 9) div 10 then
              TempVal := TempVal * 10 + (Ord(Ch) - Ord('0'));
            CopyAChar;
            end;
          if TempVal < Min then TempVal := Min;
          if TempVal > Max then TempVal := Max;
          Value := TempVal;
          end;
      end; {num_dir}


    begin {do_formatter_directives: read a formatter directive and set flags
           and value appropriately}
      CopyAChar;
      repeat
        if (Ch <> ']') and (Ch <> '}') and (Ch <> '*') then
          begin
          OptChar := Ch;
          CopyAChar;
          case OptChar of
            'a', 'A': SwitchDir(FirstSpelling);
            'b', 'B': SwitchDir(Bunching);
            'c', 'C': SwitchDir(ConvertToTabs);
            'f', 'F': SwitchDir(NewFormatting);
            'k', 'K': SwitchDir(OtherwiseKluge);
            'l', 'L': SwitchDir(LitCopy);
            'm', 'M': SwitchDir(StandardRep);
            'n', 'N': SwitchDir(NewNoNewline);
            'o', 'O':
              begin
              NumDir(OutLineLen, 1, MaxLineLen);
              OneHalfLine := OutLineLen div 2;
              FiveEighthLine := (5 * OutLineLen) div 8;
              ThreeFourthLine := (3 * OutLineLen) div 4;
              end;
            'p', 'P': SwitchDir(PortabilityMode);
            'r', 'R': SwitchDir(UcResWords);
            's', 'S': NumDir(StatsPerLine, 1, MaxLineLen);
            't', 'T':
              begin
              NumDir(TabSpaces, 0, MaxLineLen);
              ContinueSpaces := (TabSpaces + 1) div 2;
              end;
            'u', 'U': SwitchDir(UcIdents);
            otherwise;
            end; {case}
          end;
      until (Ch = ']') or (Ch = '}') or (Ch = '*');
      if Ch = ']' then CopyAChar;
    end; {do_formatter_directives}



{*------------------*
 | Comment Handling |
 *------------------*}


  procedure DoComment(Block: Boolean; {true if block comment}
                      InitCol: LineIndex; {starting column}
                      InitChar: Char {starting char} );

    var
      StatBreak: Integer; {character where line can be broken}
      StatBlanks: Boolean; {set if blank was last char}
      FirstInputLine: Boolean; {set if first input line}

{Handles all comments.

  Comments are split into two classes which are handled separately.

  Comments which begin a line are treated as "block comments" and
  are not formatted.  At most, it will be folded to fit on the
  output line.

  Comments which follow other statements on a line are formatted
  like any other statement.}



{*-------------------------*
 | Block Comment Character |
 *-------------------------*}


    procedure BlockComChar(Character: Char);


      begin {Write a character for a block comment. The comment formatting
             must be terminated with a call to adjust_block_comment. The
             comment is copied exactly, and if it will not fit within the
             out_line_len a message will be printed.}

        if EndFile then Abort(Syntax);
        if Formatting then
          if NewInputLine and (Character = ' ') then
            begin
            if WriteCol > OutLineLen then CommentOverflow;
            PrintLine(Column);
            FirstInputLine := false;
            NewInputLine := false;
            end
          else WriteA(Character);
      end; {block_com_char}



{*-----------------------------*
 | Statement Comment Character |
 *-----------------------------*}


    procedure BreakStatComment;

      var
        ExtraLen: Integer; {length from last break}
        ComIndent: Integer; {amount to indent the extra}


      begin {Break a statement comment at the last break. Assumes (stat_break
             <> 0) and (char_count - stat_break < bufsize)}

        ExtraLen := CharCount - StatBreak + 1;
        if WriteCol - ExtraLen > MaxLineLen then Abort(ComFormat)
        else
          begin {we can at least write it}
          if WriteCol - ExtraLen > OutLineLen then CommentOverflow;
          ComIndent := OutLineLen - ExtraLen;
          if ComIndent < 0 then ComIndent := 0
          else if ComIndent > Indent then ComIndent := Indent;
          with Unwritten[StatBreak mod Bufsize] do
            begin
            ActionIs := BeginLine;
            Spacing := ComIndent;
            end;
          CurrentLine := CurrentLine + 1;
          WriteCol := ComIndent + ExtraLen;
          end;
      end; {break_stat_comment}


    procedure StatComChar(Character: Char);


      begin {Take a statement character and format it. assumes that stat_break
             and stat_blank are initialized before the first character and
             are unchanged thereafter. The procedure adjust_stat_comment must
             be called after the comment is done}

        if EndFile then Abort(Syntax);
        if Formatting then
          if Character = ' ' then
            begin
            if not StatBlanks then
              begin
              if (WriteCol > OutLineLen) and (StatBreak <> 0) then
                BreakStatComment;
              WriteA(' ');
              StatBreak := CharCount;
              StatBlanks := true;
              end;
            end
          else
            begin
            WriteA(Character);
            StatBlanks := false;
            end;
      end; {stat_com_char}


{*------------------------*
 | Do compiler directives |
 *------------------------*}


    procedure DoCompilerDirectives(procedure putch
                                        (Ch: Char));


      begin {scan off compiler directives}
        while (Ch <> '[') and (Ch <> '*') and (Ch <> '}') do
          begin
          putch(Ch);
          GetChar;
          end;
      end; {do_compiler_directives}



{*----------------------*
 | Adjust Block Comment |
 *----------------------*}


    procedure AdjustBlockComment(Start: Integer);

      var
        ComLength: Integer; {length of comment if on one line}
        ComIndent: Integer; {amount to indent comment}


      begin {if the comment is all on one line, adjust it to line up with the
             indentation if possible, otherwise just try to fit it somehow.
             In any case, if the comment extends beyond the allowable length,
             bitch about it.}

        if Formatting then
          begin
          if FirstInputLine then
            begin
            ComLength := CharCount - Start;
            ComIndent := OutLineLen - ComLength;
            if ComIndent < 0 then ComIndent := 0
            else if ComIndent > StatIndent then ComIndent := StatIndent;
            Unwritten[Start mod Bufsize].Spacing := ComIndent;
            WriteCol := ComIndent + ComLength;
            end;
          if WriteCol > OutLineLen then CommentOverflow;
          end; {if formatting}
      end; {adjust_block_comment}


{*-------------------------*
 | Adjust Statment Comment |
 *-------------------------*}


    procedure AdjustStatComment;


      begin {called after the last character of a statment comment has been
             written to ensure that it all fits on a line}

        if Formatting then
          if WriteCol > OutLineLen then
            if StatBreak = 0 then
              if WriteCol <= MaxLineLen then CommentOverflow
              else Abort(ComFormat)
            else BreakStatComment;
      end; {adjust_stat_comment}


{*---------------*
 | Block Comment |
 *---------------*}


    procedure BlockComment(Column: LineIndex; {starting column}
                           InitChar: Char);

      var
        ComStart: Integer; {start of comment}


      begin {format a block comment: If the comment is all on one input line
             it will be indented to the current statement level unless it
             won't fit, in which case it is shifted left until it will fit.
             If any part of a block comment will not fit in the output line,
             the output line will be extended and a message printed.}

        if NoNewLine and not Block then
          IndentPlus(WriteCol + 1 - Column - Indent)
        else PrintLine(Column - 1);
        ComStart := CharCount;
        FirstInputLine := true;

        if StandardRep or (InitChar = '{') then BlockComChar('{')
        else
          begin
          BlockComChar('(');
          BlockComChar('*');
          end;
        GetChar;

        if Ch = '$' then DoCompilerDirectives(BlockComChar);
        if Ch = '[' then DoFormatterDirectives(BlockComChar);

        if InitChar = '/' then {We have a dumb comment, handle it}
          repeat
            while Ch <> '*' do
              begin
              BlockComChar(Ch);
              GetChar;
              end;
            GetChar;
            if (Ch <> '/') or not StandardRep then BlockComChar('*');
          until Ch = '/'
        else
          repeat
            while not (Ch in ['}', '*']) do
              begin
              BlockComChar(Ch);
              GetChar;
              end;
            if Ch = '*' then
              begin
              GetChar;
              if (Ch <> ')') or not StandardRep then BlockComChar('*');
              end;
          until Ch in ['}', ')'];

        if StandardRep or (Ch = '}') then BlockComChar('}')
        else BlockComChar(')');

        if Block then AdjustBlockComment(ComStart)
        else if NoNewLine then Undent;
      end; {block_comment}



                                        {*--------------*
                                         | stat_comment |
                                         *--------------*}


    procedure StatComment(InitChar: Char);


      begin {Format a statement comment: These are inserted in the line at the
             place found, and subsequent lines are indented to the start of
             the comment. If the start of the comment is too far to the
             right, it will be indented on the next line. Text will be moved
             as necessary to fill lines. All breaks will be at blanks, and if
             it is not possible to break a comment properly the output line
             will be extended and a message printed}

        {initialize stat_com_char}
        StatBreak := 0;
        StatBlanks := false;

        IndentPlus(WriteCol + CommentSpaces + 1 - Indent);
        if Indent > ThreeFourthLine then
          begin
          Undent;
          IndentPlus(TabSpaces);
          end;
        if WriteCol < OutLineLen - CommentSpaces - 1 then
          Space(CommentSpaces);

        if StandardRep or (InitChar = '{') then StatComChar('{')
        else
          begin
          StatComChar('(');
          StatComChar('*');
          end;
        GetChar;

        if Ch = '$' then DoCompilerDirectives(StatComChar);
        if Ch = '[' then DoFormatterDirectives(StatComChar);

        if InitChar = '/' then {We have a dumb comment, handle it}
          repeat
            while Ch <> '*' do
              begin
              StatComChar(Ch);
              GetChar;
              end;
            GetChar;
            if (Ch <> '/') or not StandardRep then StatComChar('*');
          until Ch = '/'
        else
          repeat
            while not (Ch in ['}', '*']) do
              begin
              StatComChar(Ch);
              GetChar;
              end;
            if Ch = '*' then
              begin
              GetChar;
              if (Ch <> ')') or not StandardRep then StatComChar('*');
              end;
          until Ch in ['}', ')'];

        if StandardRep or (Ch = '}') then StatComChar('}')
        else StatComChar(')');

        AdjustStatComment;
        Undent;
        BlankLines := 0;
        NewInputLine := false;
      end; {stat_comment}


{*------------------------------*
 | body of do_comment procedure |
 *------------------------------*}


    begin {do_comment}
      NewInputLine := false;
      if Block or NoNewLine then BlockComment(InitCol, InitChar)
      else StatComment(InitChar);
      Formatting := NewFormatting;
      NoNewLine := NewNoNewline;
      NewInputLine := false;
      GetChar;
      while (Ch = ' ') and not NewInputLine do GetChar;
      if Formatting and NewInputLine then EndLine := true;
      SymbolFound := false;
      LastSym := Comment;
    end; {do_comment}


{*--------------------------*
 | Lexical Scanner, Utility |
 *--------------------------*}


  procedure SymbolPut(ThisChar: Char);


    begin {ch to symbol}
      SymLen := SymLen + 1;
      Symbol[SymLen] := ThisChar;
      GetChar;
    end {symbol_put} ;

                                        {*------------*
                                         | print char |
                                         *------------*}


  procedure PrintChar;


    begin {print ASCII chars not belonging to Pascal}
      if WriteCol >= OutLineLen then PrintLine(Indent + ContinueSpaces);
      if Formatting then WriteA(Ch);
      GetChar;
    end {print_char} ;

                                        {*-------------*
                                         | scan_blanks |
                                         *-------------*}


  procedure ScanBlanks;


    begin {scan off blanks in the input}
      while (Ch = ' ') and not EndFile do GetChar;
    end;



{*-----------------*
 | String Constant |
 *-----------------*}


  procedure StringConstant;

    var
      StringEnd: Boolean;


    begin {character string to symbol}
      NewInputLine := false;
      SymbolFound := true;
      Sym := StringConst;
      StringEnd := false;
      repeat
        SymbolPut(Ch);
        if Ch = '''' then
          begin
          SymbolPut(Ch);
          StringEnd := Ch <> ''''
          end;
      until NewInputLine or StringEnd;
      if not StringEnd then Abort(Syntax);
    end {string constant} ;



{*-------------------------*
 | Test for Reserved Words |
 *-------------------------*}


  procedure TestResvWrd;

    var
      Id: WordType;
      Index: 1..NoResWords;
      P: 1..MaxWordLen;


    begin {test for reserved word}
      if (SymLen >= 2) and (SymLen <= MaxWordLen) then
        begin
        for P := 1 to MaxWordLen do
          if P > SymLen then Id[P] := ' '
          else Id[P] := LowerCase[Symbol[P]];
        with ResLen[SymLen] do
          begin {length index search}
          Index := LowIndex;
          while (ResvWrd[Index] <> Id) and (Index < HiIndex) do
            Index := Index + 1;
          end {length index search} ;
        if ResvWrd[Index] = Id then Sym := ResSymbol[Index]
        else Sym := Identifier;
        end
      else Sym := Identifier;
    end {test_resv_wrd} ;


{*-----------------------------*
 | Identifier or Reserved Word |
 *-----------------------------*}


  procedure AdjustSpelling;

    var
      ThisId: IdPtr; {Ref for current id}
      HashBase: HashValue; {hash value for this ident}
      ThisPiece: StringBlockIndex; {current piece of string table}
      ThisChar: StringPieceIndex; {character in current piece}
      j: LineIndex; {induction var}


    function HashIdent: HashValue;

      var
        i: LineIndex; {induction var}
        H: HashValue; {partial hash value}


      begin {hash the current identifier}
        H := 0;
        for i := 1 to SymLen do
          if Symbol[i] <> '_' then
            H := (H * 3 + Ord(UpperCase[Symbol[i]])) mod HashMax;
        HashIdent := H;
      end; {hash_ident}


    function SameIdent(P: IdPtr): Boolean;

      var
        i: Integer; {induction var on symbol characters}
        j: Integer; {count of characters in id}
        ThisPiece: StringBlockIndex; {current piece of string table}
        ThisChar: StringPieceIndex; {current character within the piece}


      begin {returns true if the identifier pointed to by p is the same as the
             current identifier}
        if P = nil then SameIdent := true
        else
          begin
          i := 0;
          j := 0;
          ThisPiece := (P^.Start - 1) div StringBlockSize;
          ThisChar := (P^.Start - 1) mod StringBlockSize;
          repeat
            if i < SymLen then
              repeat
                i := i + 1;
              until (Symbol[i] <> '_') or (i = SymLen);
            if j < P^.Len then
              repeat
                j := j + 1;
                if ThisChar = StringBlockMax then
                  begin
                  ThisPiece := ThisPiece + 1;
                  ThisChar := 0;
                  end
                else ThisChar := ThisChar + 1;
              until (j = P^.Len) or
                    (StringIndex[ThisPiece]^[ThisChar] <> '_');
          until ((j = P^.Len) and (i = SymLen)) or
                (UpperCase[Symbol[i]] <>
                UpperCase[StringIndex[ThisPiece]^[ThisChar]]);
          SameIdent := (j = P^.Len) and (i = SymLen) and
                       ((UpperCase[Symbol[i]] =
                       UpperCase[StringIndex[ThisPiece]^[ThisChar]]) or
                       (Symbol[i] = '_') or
                       (StringIndex[ThisPiece]^[ThisChar] = '_'));
          end;
      end; {same_id}


    begin {Adjust the spelling of the current identifier to the first spelling
           encountered for the same identifier. Identifiers are matched
           without regard to case or break-characters. If this is the first
           appearance of this identifier, the exact spelling is saved for
           future use. If it is not the first appearance, it is replaced with
           the spelling from the first appearance.}
      HashBase := HashIdent; {hash for current identifier}
      ThisId := HashTable[HashBase];
      while not SameIdent(ThisId) do ThisId := ThisId^.Next;
      if ThisId = nil then
        begin {Add this identifier to the table for future reference}
        new(ThisId);
        with ThisId^ do
          begin
          Next := HashTable[HashBase];
          HashTable[HashBase] := ThisId;
          Len := SymLen;
          Start := StringTop + 1;
          end;
        if StringTop = 0 then new(StringIndex[0]);
        ThisPiece := StringTop div StringBlockSize;
        ThisChar := StringTop mod StringBlockSize;
        for j := 1 to SymLen do
          begin
          if ThisChar = StringBlockMax then
            begin
            ThisPiece := ThisPiece + 1;
            new(StringIndex[ThisPiece]);
            ThisChar := 0;
            end
          else ThisChar := ThisChar + 1;
          StringTop := StringTop + 1;
          StringIndex[ThisPiece]^[ThisChar] := Symbol[j];
          end;
        end
      else
        with ThisId^ do
          begin
          ThisPiece := Start div StringBlockSize;
          ThisChar := Start mod StringBlockSize;
          SymLen := Len;
          for j := 1 to Len do
            begin
            Symbol[j] := StringIndex[ThisPiece]^[ThisChar];
            if ThisChar = StringBlockMax then
              begin
              ThisPiece := ThisPiece + 1;
              ThisChar := 0;
              end
            else ThisChar := ThisChar + 1;
            end;
          end;
    end; {adjust_spelling}


  procedure SetSymbolCase(Kind: Symbols);

    var
      LastUnderscore: Boolean; {true if last char underscore}
      i, j: LineIndex; {induction vars}


    begin {Convert a reserved word or identifier to the proper case}
      if Kind = Identifier then
        begin
        if PortabilityMode then
          begin
          j := 0;
          LastUnderscore := true;
          For i := 1 to SymLen Do
            if Symbol[i] = '_' then LastUnderscore := true
            else if LastUnderscore then
              begin
              LastUnderscore := false;
              j := j + 1;
              Symbol[j] := UpperCase[Symbol[i]];
              end
            else
              begin
              j := j + 1;
              Symbol[j] := LowerCase[Symbol[i]];
              end;
          for i := j + 1 to SymLen do Symbol[i] := ' ';
          SymLen := j;
          end
        else if FirstSpelling then AdjustSpelling
        else if not (LitCopy or PortabilityMode) then
          if UcIdents then
            for i := 1 to SymLen do Symbol[i] := UpperCase[Symbol[i]]
          else
            for i := 1 to SymLen do Symbol[i] := LowerCase[Symbol[i]];
        end
      else
        begin
        if PortabilityMode or (not LitCopy) then
          if UcResWords then
            for i := 1 to SymLen do Symbol[i] := UpperCase[Symbol[i]]
          else for i := 1 to SymLen do Symbol[i] := LowerCase[Symbol[i]];
        end;
    end; {set_symbol_case}


  procedure AlphaChar;


    begin {identifier or reserved word to symbol}
      NewInputLine := false;
      SymbolFound := true;
      while Ch in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$'] do SymbolPut(Ch);
      TestResvWrd;
      SetSymbolCase(Sym);
    end {alpha char} ;



{*--------*
 | Number |
 *--------*}


  procedure NumericChar;


    begin {unsigned number to symbol}
      NewInputLine := false;
      SymbolFound := true;
      Sym := Number;
      while (Ch >= '0') and (Ch <= '9') do {integer or fractional portion}
        SymbolPut(Ch);

      if Ch = '.' then
        begin
        SymbolPut(Ch);
        if Ch = '.' then
          begin {actually subrange, must fudge}
          SymLen := SymLen - 1; {erase period}
          DoublePeriod := true;
          end
        else
          while (Ch >= '0') and (Ch <= '9') do SymbolPut(Ch);
        end;

      if (Ch = 'E') or (Ch = 'e') or (Ch = 'D') or (Ch = 'd') then
        begin {exponential portion}
        if (Ch = 'E') or (Ch = 'e') then
          SymbolPut('E')
        else
          SymbolPut('D');
        if (Ch = '+') or (Ch = '-') then {sign} SymbolPut(Ch);
        while (Ch >= '0') and (Ch <= '9') do {characteristic} SymbolPut(Ch);
        end {exponential}
      else if (Ch = 'b') or (Ch = 'B') then SymbolPut('B')
      else if Ch = '#' then
        begin
        SymbolPut('#');
        while Ch in ['0'..'9', 'a'..'f', 'A'..'F'] do
          SymbolPut(UpperCase[Ch]);
        end;
    end {numeric char} ;



{*-------------------*
 | Special Character |
 *-------------------*}


  procedure SpecialChar;


    begin {operators or delimiters to symbol}
      SymbolFound := true; {untrue only for comments}
      NewInputLine := false;
      case Ch of {special symbols}
        '+':
          begin {plus}
          Sym := Plus;
          SymbolPut(Ch);
          end {plus} ;
        '-':
          begin {minus}
          Sym := Minus;
          SymbolPut(Ch);
          end {minus} ;
        '*':
          begin {multiply}
          Sym := Mult;
          SymbolPut(Ch);
          end {multiply} ;
        '.':
          begin {subrange or period}
          Sym := Period;
          SymbolPut(Ch);
          if DoublePeriod then
            begin {fudge a subrange}
            Symbol[2] := '.';
            SymLen := 2;
            Sym := Subrange;
            end
          else if Ch = '.' then
            begin {subrange}
            Sym := Subrange;
            SymbolPut(Ch);
            end
          else if Ch = ')' then
            begin {alternative right bracket}
            Sym := CloseBrack;
            if StandardRep then
              begin
              Symbol[1] := ']';
              GetChar;
              end
            else SymbolPut(Ch);
            end;
          DoublePeriod := false;
          end {subrange or period} ;
        ',':
          begin {comma}
          Sym := Comma;
          SymbolPut(Ch);
          end {comma} ;
        ';':
          begin {semicolon}
          Sym := Semicolon;
          SymbolPut(Ch);
          end {semicolon} ;
        ':':
          begin {becomes, or colon}
          Sym := Colon;
          SymbolPut(Ch);
          if Ch = '=' then
            begin {becomes}
            Sym := Becomes;
            SymbolPut(Ch);
            end {becomes}
          end {becomes, or colon} ;
        '=':
          begin {equals}
          Sym := Equal;
          SymbolPut(Ch);
          end {equals} ;
        '<':
          begin {less than, less equal, not equal}
          Sym := RelOp;
          SymbolPut(Ch);
          if (Ch = '=') or (Ch = '>') then SymbolPut(Ch);
          end {less than, less equal, not equal} ;
        '>':
          begin {greater equal, greater than}
          Sym := RelOp;
          SymbolPut(Ch);
          if Ch = '=' then SymbolPut(Ch);
          end {great than, or great equals} ;
        '^', '@':
          begin {pointer}
          Sym := Pointer;
          if StandardRep then SymbolPut('^')
          else SymbolPut(Ch);
          end {pointer} ;
        '''': StringConstant;
        ')':
          begin {close parenthesis}
          Sym := CloseParen;
          SymbolPut(Ch);
          end {close parenthesis} ;
        '[':
          begin {open bracket}
          Sym := OpenBrack;
          SymbolPut(Ch);
          end {open bracket} ;
        ']':
          begin {close bracket}
          Sym := CloseBrack;
          SymbolPut(Ch);
          end {close bracket} ;
        '#':
          begin
          {this option is added to take care of hexadecimal value 
          in case statement. This is special feature of MS-DOS}
          SymbolPut(Ch);		
          SymbolFound := false;
          end;
        end; {case}
    end {special_char} ;



{*------------------*
 | Start of Comment |
 *------------------*}


  procedure CommentChar;

    var
      InitChar: Char; {starting character}


    begin {possible start of comment}
      if (Ch = '(') or (Ch = '/') then
        begin {see if comment or just open paren}
        InitChar := Ch;
        SymbolPut(Ch);
        if Ch = '*' then
          begin
          SymLen := 0;
          DoComment(NewInputLine, Column - 1, InitChar);
          end
        else if (InitChar = '(') and (Ch = '.') then
          begin {alternate representation of left bracket}
          if StandardRep then
            begin
            Symbol[1] := '[';
            GetChar;
            end
          else SymbolPut(Ch);
          Sym := OpenBrack;
          SymbolFound := true;
          end
        else
          begin
          if InitChar = '(' then Sym := OpenParen
          else Sym := Divide;
          NewInputLine := false;
          SymbolFound := true;
          end;
        end
      else DoComment(NewInputLine, Column, Ch);
    end; {comment_char}


  procedure LexicalDirective;

    var
      Id: WordType;
      P: 1..MaxWordLen;
      OnNewline: Boolean; {was on a new line}
      quoted: Boolean;


    begin {Process a Pascal-2 lexical directive. The only ones are "%include"
           and "%page", and these are simply passed to the output. Any others
           are treated as an identifier.}
      OnNewline := NewInputLine;
      NewInputLine := false;

      repeat
        SymbolPut(Ch);
      until (Ch = ' ') or (Ch = ';');

      for P := 1 to MaxWordLen do
        if P > SymLen then Id[P] := ' '
        else Id[P] := LowerCase[Symbol[P]];

      SetSymbolCase(AndSy); {anything but identifier}

      if Id = '%include ' then
        begin
        SymbolPut(' ');
        while (Ch = ' ') do GetChar;
        if Ch = '''' then quoted := true
        else quoted := false;
        repeat
          SymbolPut(Ch);
        until (not quoted and ((Ch = ' ') or (Ch = ';'))) or (quoted and
              (Ch = '''')); {old and new Pascal-2}
        if (Ch = ';') then {old syle Pascal-2.0} SymbolPut(Ch)
        else if (Ch = '''') then
          begin {new style Pascal-2.1}
          SymbolPut(Ch);
          if Ch = ' ' then
            while Ch = ' ' do GetChar;
          if Ch = ';' then SymbolPut(';')
          end;
        end;
      if (Id = '%include ') or (Id = '%page    ') then
        begin
        Sym := Identifier;
        if OnNewline then FormatLine(StatIndent);
        PutSym;
        SymWritten := false;
        SymLen := 0;
        end
      else AlphaChar;
    end; {lexical_directive}



{*---------------------------*
 | Get Next Symbol (get_sym) |
 *---------------------------*}


  procedure GetSym;


    begin {extract next basic sym from text}
      SymLen := 0;
      SymbolFound := false;
      SymWritten := false;
      repeat
        if NoNewLine then EndLine := NewInputLine;
        if EndFile then
          begin
          Sym := TextEnd;
          SymbolFound := true
          end
        else if Ch = ' ' then ScanBlanks
        else
          begin
          case Ch of {lexical analysis}
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9': NumericChar;
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
            'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
            'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
            '$', '_':
              AlphaChar;
            ')', '*', '+', ',', '-', '.', ':', ';', '<', '=', '>', '[', ']',
            '^', '@', '''', '#':
              SpecialChar;
            '(', '{', '/': CommentChar;
            '!', '&', '?', '\', '`', '|', '~', '}', '"': PrintChar;
            '%': LexicalDirective;
            otherwise
              if Formatting and (Ch = Chr(Ff)) then
                begin
                PrintLine(0);
                PrintChar;
                Space(0);
                ClearBreaks;
                EndLine := true;
                end
              else GetChar;
            end
          end;
      until SymbolFound
    end {get_sym} ;


  procedure FudgeSymbol(WordLen: Integer; {length of word provided}
                        Word: WordType {word to substitute} );

    var
      i: 1..MaxWordLen; {induction var}


    begin {Fudges one reserved word into another. This is used to substitute a
           reserved word for the current symbol to improve compatibility
           between compilers.}
      for i := 1 to WordLen do
        if UcResWords then Symbol[i] := UpperCase[Word[i]]
        else Symbol[i] := LowerCase[Word[i]];
      SymLen := WordLen;
    end; {fudge_symbol}


{*-------------------------*
 | Parser Utility Routines |
 *-------------------------*}

                                        {*----------*
                                         | next_sym |
                                         *----------*}


  procedure NextSym;


    begin {output current sym and input next}
      if Sym <> TextEnd then
        begin {symbol}
        if not SymWritten then PutSym;
        GetSym;
        end {symbol}
    end {next_sym} ;

                                        {*-------*
                                         | check |
                                         *-------*}


  procedure Check(Fsym: SetOfSyms);


    begin {check if the next symbol is in fsym}
      if not (Sym in Fsym) then Abort(Syntax);
    end; {check}

                                        {*-----------*
                                         | Check sym |
                                         *-----------*}


  procedure CheckSym(Desired: Symbols);


    begin {abort if current symbol not desired, else next_sym}
      if Sym = Desired then NextSym
      else Abort(Syntax);
    end; {check_sym}

                                        {*-----------------*
                                         | Next on newline |
                                         *-----------------*}


  procedure NextOnNewline(Spacing, Delta: Integer);


    begin {space "spacing" lines, indent, put new symbol, and increment indent
           by "delta"}

      if NoNewLine then Spacing := 0
      else if (BlankLines > 0) or (CurrentLine = 0) then
        Spacing := Spacing - 1;
      repeat
        FormatLine(Indent);
        Spacing := Spacing - 1;
      until Spacing < 0;
      IndentPlus(Delta);
      StatIndent := Indent;
      NextSym;
    end; {next_on_newline}

                                        {*------------------*
                                         | Log symbol start |
                                         *------------------*}


  procedure LogSymbolStart(var Log: ColLog);


    begin {log the starting loc of the next symbol}
      with Log do
        begin
        LogChar := CharCount + 1;
        LogCol := WriteCol + 1;
        LogLine := CurrentLine;
        end;
    end; {log_symbol_start}



{*--------------------*
 | Statement bunching |
 *--------------------*}


  procedure Bunch(Start: ColLog; {start of statement}
                  var Success: Boolean);


    begin {move a statement up to the previous line if it will fit}
      with Start do
        if Formatting and (CharCount - LogChar < Bufsize) and
           (CharCount >= LogChar) and (LogLine + 1 = CurrentLine) and
           (WriteCol - Indent + LogCol < OutLineLen) then
          begin {move it up, adjusting things as we go}
          with Unwritten[LogChar mod Bufsize] do
            begin
            ActionIs := Spaces;
            Spacing := 1;
            WriteCol := WriteCol - Indent + LogCol + 1;
            end;
          CurrentLine := CurrentLine - 1;
          Success := true;
          end
        else Success := false;
    end; {bunch}

                                        {*------------------*
                                         | bunch_statements |
                                         *------------------*}


  procedure BunchStatement(Start: ColLog);

    var
      TabInt: Integer; {tab interval}
      NextTab: Integer; {next tab location}


    begin {see if we can put multiple statements on a line}
      if Formatting then
        with Start do
          begin
          TabInt := (OutLineLen - Indent) div StatsPerLine;
          if TabInt = 0 then TabInt := 1;
          if LogCol = Indent + 1 then LogCol := Indent;
          {fudge for start}
          NextTab := (LogCol - Indent + TabInt - 1) div TabInt * TabInt +
                     Indent;
          if (NextTab > Indent) and (LogLine + 1 = CurrentLine) and
             (CharCount - LogChar < Bufsize) and
             (NextTab + WriteCol - Indent <= OutLineLen) then
            begin {move up to prior line and fiddle pointers}
            with Unwritten[LogChar mod Bufsize] do
              begin
              ActionIs := Spaces;
              Spacing := NextTab - LogCol + 1;
              end;
            WriteCol := NextTab + WriteCol - Indent;
            CurrentLine := CurrentLine - 1;
            end;
          end;
    end; {bunch_statement}


  procedure TerminalSemicolon;


    begin {Parse a possible terminal semicolon at the end of a statement. This
           is done this way to make sure that it gets indented properly}
      if (Sym = Semicolon) and not SymWritten then PutSym;
    end; {terminal_semicolon}


{*-----------------------------*
 | Parser forward declarations |
 *-----------------------------*}


  procedure Statement;
    forward;


  procedure Expression;
    forward;


  procedure ExprList(BreakAt: Integer);
    forward;


  procedure ScanType;
    forward;


  procedure DoBlock;
    forward;



{*-----------------*
 | Identifier list |
 *-----------------*}


  procedure IdentList;


    begin {Scan a list of identifiers separated by commas. Formatting is
           allowed to continue if a comma is missing }
      while Sym = Identifier do
        begin
        NextSym;
        if Sym = OriginSy then
          begin
          NextSym;
          Expression;
          end;
        if (Sym in [UseSy, DefineSy]) then 
          begin
          NextSym;
          if Sym = StringConst then
            NextSym;
          end;
        if Sym = Comma then
          begin
          NextSym;
          SetSymbolBreak(0);
          end;
        end;
    end; {ident_list}



{*----------*
 | Constant |
 *----------*}


  procedure Constant;


    begin {scan a constant}
      if Sym in [Plus, Minus] then NextSym;
      Check(Constants - [Plus, Minus]);
      NextSym;
    end; {constant}


{*----------*
 | Variable |
 *----------*}


  procedure Variable;


    begin {scan off a variable, doesn't check much}
      while Sym in [Identifier, Period, Pointer, OpenBrack] do
        begin
        if Sym = OpenBrack then
          begin
          NextSym;
          ExprList(0);
          CheckSym(CloseBrack);
          end
        else NextSym;
        end;
    end; {variable}


{*---------------*
 | Constant list |
 *---------------*}


  procedure ConstList;


    begin {scan a list of constants, as for case labels}
      while Sym in Constants do
        begin
        Constant;
        if Sym = Comma then
          begin
          NextSym;
          SetSymbolBreak(0);
          end;
        end;
    end; {const_list}



{*--------*
 | Factor |
 *--------*}


  procedure Factor;


    begin {scan a factor in an expression, ignores precedence}
      if Sym = OpenParen then
        begin
        NextSym;
        ExprList(0); {hack to allow structured constants}
        CheckSym(CloseParen);
        if Sym = Comma then SetSymbolBreak(3);
        end
      else if Sym = OpenBrack then
        begin {set expression}
        NextSym;
        while Sym in ExprBegSys do
          begin
          ExprList(1);
          if Sym = Subrange then NextSym;
          end;
        CheckSym(CloseBrack);
        end
      else if Sym = Identifier then
        begin
        Variable;
        if Sym = OpenParen then
          begin
          PutSym;
          if WriteCol <= ThreeFourthLine then IndentPlus(WriteCol - Indent)
          else IndentPlus(0);
          NextSym;
          ExprList(3);
          CheckSym(CloseParen);
          Undent;
          end
        end
      else Constant;
    end; {factor}


{*------------*
 | Expression |
 *------------*}


  procedure Expression;


    begin {scan an expression}
      while Sym in ExprBegSys do
        begin
        if Sym in [Plus, Minus, NotSy, Pointer] then NextSym;
        Factor;
        if Sym in [AndSy, OrSy] then
          begin
          NextSym;
          SetSymbolBreak(3);
          end
        else if Sym in RelOps then
          begin
          NextSym;
          SetSymbolBreak(2);
          end
        else if Sym in ArithOps then
          begin
          NextSym;
          SetSymbolBreak(1);
          end;
        end; {while}
    end; {expression}


{*-----------------*
 | Expression list |
 *-----------------*}


  procedure ExprList;


    begin {scan a list of expressions}
      while Sym in ExprBegSys + [Comma] do
        begin
        if Sym in ExprBegSys then Expression;
        if (Sym = Comma) or (Sym = Colon) then
          begin
          NextSym;
          SetSymbolBreak(BreakAt);
          end;
        end;
    end; {expr_list}


{*----------------------------*
 | Statement List (stat_list) |
 *----------------------------*}


  procedure StatList;

    var
      StatTerms: SetOfSyms;
      StatStart: ColLog;
      FirstStat: Boolean;


    begin {process a list of statements}
      StatTerms := StatSet + [Semicolon];
      FirstStat := true;
      repeat
        LogSymbolStart(StatStart);
        Statement;
        {note: may or may not have semicolon}
        TerminalSemicolon;
        if (StatsPerLine > 1) and not FirstStat then
          BunchStatement(StatStart);
        {split like this so following comments don't screw up}
        if Sym = Semicolon then GetSym;
        FirstStat := false;
      until not (Sym in StatTerms);
    end; {state_list}


{*----------------------------*
 | Compound statement (begin) |
 *----------------------------*}


  procedure DoBegin(ProcBlock: Boolean);

    var
      Trim: Integer; {amount to indent}


    begin {handle a begin - end block, indenting if requested by setting
           proc_block true}

      ResetCharCount;
      if ProcBlock then Trim := TabSpaces
      else Trim := 0;
      NextOnNewline(0, Trim);
      StatList;
      Undent;
      FormatLine(Indent);
      CheckSym(EndSy);
    end; {do_begin}



{*-------------------------------*
 | Assignment and Procedure Call |
 *-------------------------------*}


  procedure DoAssignCall;


    begin {either assignment or call}
      FormatLine(Indent);
      IndentPlus(ContinueSpaces);
      Variable;
      if Sym = Becomes then
        begin
        NextSym;
        if WriteCol < ThreeFourthLine then IndentPlus(WriteCol - Indent + 1)
        else
          begin
          IndentPlus(0);
          SetSymbolBreak(0);
          end;
        Expression;
        TerminalSemicolon;
        Undent;
        end
      else if Sym = OpenParen then
        begin
        NextSym;
        if WriteCol <= ThreeFourthLine then IndentPlus(WriteCol - Indent)
        else IndentPlus(0);
        ExprList(3);
        CheckSym(CloseParen);
        TerminalSemicolon;
        Undent;
        end
      else TerminalSemicolon;
      Undent;
    end; {do_assign_call}



{*----------------*
 | Goto statement |
 *----------------*}


  procedure DoGoto;


    begin {goto statement}
      FormatLine(Indent);
      NextSym;
      CheckSym(Number);
      TerminalSemicolon;
    end; {do_goto}


{*-----------------*
 | While statement |
 *-----------------*}


  procedure DoWhile;

    var
      WhileStart: ColLog; {start of statement}
      StartLine, EndLine: Integer; {statement lines}
      Successful: Boolean; {bunching went}


    begin {while statement}
      ResetCharCount;
      FormatLine(Indent);
      NextSym;
      if WriteCol < ThreeFourthLine then IndentPlus(WriteCol - Indent + 1)
      else IndentPlus(ContinueSpaces);
      StartLine := CurrentLine;
      Expression;
      CheckSym(DoSy);
      Undent;
      IndentPlus(TabSpaces);
      EndLine := CurrentLine;
      LogSymbolStart(WhileStart);
      StatIndent := Indent;
      Statement;
      if Bunching and (StartLine = EndLine) then
        Bunch(WhileStart, Successful);
      Undent;
    end; {do_while}



{*----------------*
 | With statement |
 *----------------*}


  procedure DoWith;

    var
      StartLine, EndLine: Integer; {starting and ending lines of heading}
      WithStart: ColLog; {start of statement}
      Successful: Boolean; {bunching went}


    begin {with_statement}
      ResetCharCount;
      FormatLine(Indent);
      NextSym;
      if WriteCol < ThreeFourthLine then IndentPlus(WriteCol - Indent + 1)
      else IndentPlus(ContinueSpaces);
      StartLine := CurrentLine;
      ExprList(3);
      CheckSym(DoSy);
      Undent;
      IndentPlus(TabSpaces);
      StatIndent := Indent;
      EndLine := CurrentLine;
      LogSymbolStart(WithStart);
      Statement;
      if Bunching and (StartLine = EndLine) then
        Bunch(WithStart, Successful);
      Undent;
    end; {do_with}



{*--------------*
 | If statement |
 *--------------*}


  procedure DoIf(PrevElse: Boolean {set if previous sym was else} );

    var
      IfStart: ColLog; {start of if statement}
      StartLine, EndLine: Integer; {statement lines}
      Successful: Boolean; {bunching went}


    begin {if statement}
      ResetCharCount;
      if not PrevElse then FormatLine(Indent);
      NextSym;
      if WriteCol < ThreeFourthLine then IndentPlus(WriteCol - Indent + 1)
      else IndentPlus(ContinueSpaces);
      StartLine := CurrentLine;
      Expression;
      CheckSym(ThenSy);
      Undent;
      IndentPlus(TabSpaces);
      EndLine := CurrentLine;
      LogSymbolStart(IfStart);
      Statement;
      if Bunching and (StartLine = EndLine) then Bunch(IfStart, Successful);
      Undent;
      StatIndent := Indent;
      if Sym = ElseSy then
        begin
        FormatLine(Indent);
        NextSym;
        if Sym = IfSy then DoIf(true)
        else
          begin
          IndentPlus(TabSpaces);
          LogSymbolStart(IfStart);
          Statement;
          if Bunching then Bunch(IfStart, Successful);
          Undent;
          end;
        end;
    end; {do_if}



{*----------------*
 | Case statement |
 *----------------*}


  procedure DoCase;

    var
      CaseStart: ColLog; {start of case}
      Successful: Boolean; {bunching successful}
      LabStart, LabEnd: Integer; {label list lines}


    begin {case_statement}
      ResetCharCount;
      FormatLine(Indent);
      NextSym;
      if WriteCol < ThreeFourthLine then IndentPlus(WriteCol - Indent + 1)
      else IndentPlus(ContinueSpaces);
      Expression;
      CheckSym(OfSy);
      Undent;
      IndentPlus(TabSpaces);
      StatIndent := Indent;
      while not (Sym in [EndSy, OtherwiseSy, ElseSy]) do
        begin
        if Sym in Constants then
          begin
          FormatLine(Indent);
          LabStart := CurrentLine;
          ConstList;
          CheckSym(Colon);
          LabEnd := CurrentLine;
          IndentPlus(TabSpaces);
          LogSymbolStart(CaseStart);
          Statement;
          if Bunching and (LabStart = LabEnd) then
            Bunch(CaseStart, Successful);
          Undent;
          StatIndent := Indent;
          end; {if sym in constants}
        if Sym = Semicolon then NextSym;
        Check(Constants + [EndSy, Semicolon, OtherwiseSy, ElseSy]);
        end; {while}
      if (Sym = OtherwiseSy) or (Sym = ElseSy) then
        begin
        if OtherwiseKluge then FudgeSymbol(9, 'otherwise');
        NextOnNewline(0, TabSpaces);
        LogSymbolStart(CaseStart);
        StatList;
        if Bunching then Bunch(CaseStart, Successful);
        Undent;
        end;
      FormatLine(Indent);
      CheckSym(EndSy);
      Undent;
    end; {do_case}


{*------------------*
 | Repeat statement |
 *------------------*}


  procedure DoRepeat;


    begin {repeat statement}
      ResetCharCount;
      NextOnNewline(0, TabSpaces);
      StatList;
      Undent;
      StatIndent := Indent;
      FormatLine(Indent);
      CheckSym(UntilSy);
      if WriteCol < ThreeFourthLine then IndentPlus(WriteCol - Indent + 1)
      else IndentPlus(ContinueSpaces);
      Expression;
      TerminalSemicolon;
      Undent;
    end; {do_repeat}



{*---------------*
 | For statement |
 *---------------*}


  procedure DoFor;

    var
      StartLine, EndLine: Integer; {starting and ending lines of header}
      ForStart: ColLog; {start of controlled statement}
      Successful: Boolean; {bunching went}


    begin {for statement}
      ResetCharCount;
      NextOnNewline(0, ContinueSpaces);
      StartLine := CurrentLine;
      CheckSym(Identifier);
      CheckSym(Becomes);
      Expression;
      Check([ToSy, DowntoSy]);
      NextSym;
      Expression;
      CheckSym(DoSy);
      Undent;
      IndentPlus(TabSpaces);
      EndLine := CurrentLine;
      LogSymbolStart(ForStart);
      Statement;
      if Bunching and (StartLine = EndLine) then Bunch(ForStart, Successful);
      Undent;
    end; {do_for}


{*-----------*
 | Statement |
 *-----------*}


  procedure Statement;


    begin {handle a (possibly empty) statement}
      StatIndent := Indent;
      if Sym = Number then
        begin
        IndentPlus( - TabSpaces);
        FormatLine(Indent);
        NextSym;
        CheckSym(Colon);
        Undent;
        end;
      if Sym in (StatSet - [Number]) then
        case Sym of
          BeginSy: DoBegin(false);
          CaseSy: DoCase;
          ForSy: DoFor;
          GotoSy: DoGoto;
          Identifier: DoAssignCall;
          IfSy: DoIf(false);
          RepeatSy: DoRepeat;
          WhileSy: DoWhile;
          WithSy: DoWith;
          end; {case}
      StatIndent := Indent;
    end; {statement}



{*-----------------------*
 | Formal Parameter List |
 *-----------------------*}


  procedure Parameters;


    begin {format a formal parameter list: if they start less than halfway
           across the page, they are all lined up with the first parameter,
           on successive lines. If they start more than halfway across the
           page, they begin on the next line, indented double the usual
           (arbitrary)}

      if WriteCol > OneHalfLine then FormatLine(Indent + 2 * TabSpaces);
      NextSym;
      IndentPlus(WriteCol - Indent);
      while Sym in [Identifier, FunctionSy, ProcedureSy, VarSy] do
        begin
        if Sym in [FunctionSy, ProcedureSy] then
          begin
          IndentPlus(ContinueSpaces);
          NextSym;
          CheckSym(Identifier);
          if Sym = OpenParen then Parameters;
          end
        else
          begin
          if Sym <> Identifier then NextSym;
          if Sym <> Identifier then Abort(Syntax);
          IndentPlus(ContinueSpaces);
          IdentList;
          end;
        Undent;
        if Sym = Colon then
          begin
          NextSym;
          if Sym = UnivSy then NextSym;
          ScanType;
          end;
        if Sym = Semicolon then
          begin
          NextSym;
          FormatLine(Indent);
          end;
        end;
      CheckSym(CloseParen);
      TerminalSemicolon;
      Undent;
      StatIndent := Indent;
    end; {parameters}



{*------------*
 | Field list |
 *------------*}


  procedure FieldList;

    var
      InvarPart: Boolean; {true if there was an invarient part}
      labelstart, labelend: Integer; {lines for case label bunching}
      CaseStart: ColLog; {start of a variant}
      Successful: Boolean; {dummy param}


    begin {scan field list of type specification }
      InvarPart := false;
      while Sym = Identifier do
        begin
        InvarPart := true;
        IndentPlus(ContinueSpaces);
        IdentList;
        CheckSym(Colon);
        Undent;
        ScanType;
        if Sym = Semicolon then NextSym;
        if Sym = Identifier then FormatLine(Indent);
        end;
      if Sym = CaseSy then
        begin {case}
        if InvarPart then FormatLine(Indent);
        NextSym;
        IndentPlus(ContinueSpaces);
        if Sym = Identifier then NextSym
        else ScanType;
        if Sym = Colon then
          begin
          NextSym;
          ScanType
          end;
        CheckSym(OfSy);
        Undent;
        IndentPlus(TabSpaces);
        StatIndent := Indent;
        FormatLine(Indent);
        repeat {variant part}
          labelstart := CurrentLine;
          ConstList;
          CheckSym(Colon);
          labelend := CurrentLine;
          IndentPlus(TabSpaces);
          StatIndent := Indent;
          LogSymbolStart(CaseStart);
          FormatLine(Indent);
          CheckSym(OpenParen);
          IndentPlus(1); {compensate for paren}
          FieldList;
          Undent;
          CheckSym(CloseParen);
          Undent;
          StatIndent := Indent;
          if Sym = Semicolon then NextSym;
          if Bunching and (labelstart = labelend) then
            Bunch(CaseStart, Successful);
          if not (Sym in [EndSy, CloseParen]) then FormatLine(Indent);
        until not (Sym in Constants);
        Undent;
        StatIndent := Indent;
        end {case}
    end; {field_list}


{*-------------*
 | Record type |
 *-------------*}


  procedure RecordType(PackedStart: ColLog);


    begin {handle a record type, includes a kluge to move "packed" down to the
           next line}

      IndentPlus(TabSpaces);
      with PackedStart do
        if Formatting and (LogChar <> 0) and
           (CharCount - LogChar < Bufsize) then
          with Unwritten[LogChar mod Bufsize] do
            begin {note that this kluge assumes the logged point has become a
                   space so it can be changed to a newline}
            ActionIs := BeginLine;
            Spacing := Indent;
            WriteCol := Indent + WriteCol - LogCol;
            CurrentLine := CurrentLine + 1;
            end
        else FormatLine(Indent);
      NextSym;
      IndentPlus(TabSpaces);
      StatIndent := Indent;
      FormatLine(Indent);
      FieldList;
      Undent;
      FormatLine(Indent);
      CheckSym(EndSy);
      TerminalSemicolon;
      Undent;
    end; {record_type}


{*------------*
 | Array type |
 *------------*}


  procedure ArrayType;

    var
      Conformant: Boolean;


    begin {format an array type}
      Conformant := false;
      IndentPlus(TabSpaces);
      NextSym;
      SetSymbolBreak(0);
      CheckSym(OpenBrack);
      while Sym in Constants do
        begin
        Constant;
        if Sym = Subrange then
          begin
          NextSym;
          Constant;
          { For conformant arrays, check for ': typeid' here. }
          if Sym = Colon then
            begin
            Conformant := true;
            NextSym;
            CheckSym(Identifier);
            end;
          end;
        if (Sym = Comma) or (Sym = Semicolon) then
          begin
          NextSym;
          SetSymbolBreak(0);
          end;
        end; {while}
      CheckSym(CloseBrack);
      CheckSym(OfSy);
      ScanType;
      if not Conformant then TerminalSemicolon;
      Undent;
    end; {array_type}


{*------------------*
 | Enumeration type |
 *------------------*}


  procedure EnumType;


    begin {handle an enumeration type, align to the right of the opening
           parenthesis if there is room, otherwise use normal continuation}

      NextSym;
      if WriteCol <= ThreeFourthLine then IndentPlus(WriteCol - Indent)
      else IndentPlus(ContinueSpaces);
      IdentList;
      CheckSym(CloseParen);
      TerminalSemicolon;
      Undent;
    end; {enum_type}


{*-----------*
 | Scan type |
 *-----------*}


  procedure ScanType;

    var
      PackedStart: ColLog;


    begin {scan a type, formatting differs for each one}
      IndentPlus(ContinueSpaces);
      if Sym = PackedSy then
        begin {mark start of 'packed' - must actually be a space}
        LogSymbolStart(PackedStart);
        NextSym;
        end
      else PackedStart.LogChar := 0;
      Undent;
      Check(TypeBegSys);
      case Sym of
        OpenParen: EnumType;
        ArraySy: ArrayType;
        FileSy, SetSy:
          begin
          NextSym;
          CheckSym(OfSy);
          ScanType;
          end;
        Identifier, Number, Plus, Minus, StringConst:
          begin {simple or subrange}
          Constant;
          if Sym = Subrange then
            begin
            NextSym;
            Constant;
            end;
          end;
        Pointer:
          begin
          NextSym;
          ScanType;
          end;
        RecordSy: RecordType(PackedStart);
        StringSy:
          begin
          NextSym;
          If Sym = OpenBrack then
            begin
            NextSym;
            Constant;
            CheckSym(CloseBrack);
            end;
          end;
        end; {case}
      StatIndent := Indent;
    end; {scan_type}


{*-------------------*
 | Label Declaration |
 *-------------------*}


  procedure DoLabel;


    begin {label declaration}
      ResetCharCount;
      NextOnNewline(1, TabSpaces);
      FormatLine(Indent);
      while Sym = Number do
        begin
        NextSym;
        if Sym = Comma then NextSym;
        end; {while}
      CheckSym(Semicolon);
      Undent;
    end;



{*----------------------*
 | Constant Declaration |
 *----------------------*}


  procedure DoConst;

    var
      ConstStart: ColLog; {start of particular declaration}
      FirstConst: Boolean; {first constant in decl}


    begin {constant declaration}
      ResetCharCount;
      NextOnNewline(1, TabSpaces);
      FirstConst := true;
      while Sym = Identifier do
        begin
        LogSymbolStart(ConstStart);
        FormatLine(Indent);
        NextSym;
        CheckSym(Equal);
        ExprList(0); {hack to allow structured constants}
        if Sym = Semicolon then PutSym
        else Abort(Syntax);
        if (StatsPerLine > 1) and not FirstConst then
          BunchStatement(ConstStart);
        NextSym; {split so comments format right}
        FirstConst := false;
        end; {while}
      Undent;
      StatIndent := Indent;
    end; {do_const}


{*------------------*
 | Type Declaration |
 *------------------*}


  procedure DoType;


    begin {type_declaration}
      NextOnNewline(1, TabSpaces);
      while Sym = Identifier do
        begin
        ResetCharCount;
        FormatLine(Indent);
        NextSym;
        CheckSym(Equal);
        ScanType;
        CheckSym(Semicolon);
        end; {while}
      Undent;
      StatIndent := Indent;
    end; {do_type}


{*-----------------*
 | Var Declaration |
 *-----------------*}


  procedure DoVar;


    begin {var declaration}
      NextOnNewline(1, TabSpaces);
      while Sym = Identifier do
        begin
        ResetCharCount;
        FormatLine(Indent);
        IndentPlus(ContinueSpaces);
        Check([Identifier]);
        IdentList;
        CheckSym(Colon);
        Undent;
        ScanType;
        CheckSym(Semicolon);
        end; {while}
      Undent;
      StatIndent := Indent;
    end; {do_var}


{*---------*
 | Program |
 *---------*}


  procedure DoProgram;


    begin {program or processor}
      NextOnNewline(0, ContinueSpaces);
      CheckSym(Identifier);
      if Sym = OpenParen then
        begin
        NextSym;
        while Sym = Identifier do
          begin
          NextSym;
          if Sym = Comma then
            begin
            NextSym;
            SetSymbolBreak(0);
            end;
          end;
        CheckSym(CloseParen);
        end;
      CheckSym(Semicolon);
      Undent;
      IndentPlus(TabSpaces);
      DoBlock;
      if Sym = Period then NextSym;
      Undent;
    end; {do_program}



{*-----------------------*
 | Procedure Declaration |
 *-----------------------*}


  procedure DoProcedure;

    var
      StartSym: Symbols;


    begin {procedure}
      ResetCharCount;
      StartSym := Sym;
      NextOnNewline(2, ContinueSpaces);
      CheckSym(Identifier);
      if Sym = OpenParen then Parameters;
      if StartSym = FunctionSy then
        if Sym = Colon then
          begin {if function was declared forward, the second appearance has
                 no result type}
          CheckSym(Colon);
          CheckSym(Identifier);
          end;
      TerminalSemicolon;
      Undent;
      CheckSym(Semicolon);
      IndentPlus(TabSpaces);
      if Sym in [ExternSy, FortranSy, ForwardSy, NonpascalSy, Identifier] then
        begin
        FormatLine(Indent);
        NextSym;
        end
      else if Sym in BlockBegSys then DoBlock
      else Abort(Syntax);
      if Sym = Semicolon then
        begin
        PutSym;
        Undent;
        StatIndent := Indent;
        NextSym;
        end
      else Abort(Syntax);
    end; {procedure}



{*-------*
 | Block |
 *-------*}


  procedure DoBlock;


    begin {scan a block, including types, etc}
      StatIndent := Indent;
      while Sym in HeadingBegSys do
        begin {declarations}
        case Sym of
          LabelSy: DoLabel;
          ConstSy: DoConst;
          TypeSy: DoType;
          VarSy, SharedSy: DoVar;
          ProcedureSy, FunctionSy: DoProcedure;
          end;
        StatIndent := Indent;
        end; {while}
      if Sym = BeginSy then
        begin
        FormatLine(Indent);
        DoBegin(true);
        end;
    end; {do_block}



                    {*----------------------------*
                     | PROGRAM LOOP: process_text |
                     *----------------------------*}


  procedure ProcessText;

    var
      status: Boolean;


    begin {process text}
      ClearBreaks;
      if Sym = ProgramSy then DoProgram
      else if Sym in BlockBegSys then
        begin
        DoBlock;
        if Sym = Semicolon then NextSym;
        if Sym = Period then NextSym; {set of external procs}
        end
      else if Sym in StatSet then StatList;
      Check([TextEnd]);
      FlushBuffer;
      Close(Source);
      Close(Result);
      if MSDOSopsys then
        if not OutputFlg then FixBakOutput(OutputArg, true);
      FixTempOutput(TempArg, OutputArg, true, status);
      if not status then
        begin
        writeln(Output, 'Can''t clean up temporary output');
        error := true;
        end;
    end {process text} ;

                                        {*--------------------*
                                         | BEGIN PRETTY-PRINT |
                                         *--------------------*}


  begin {pretty-print}
    Initialize;
    csi;
    if InitialDirectives then
      begin
      DoFormatterDirectives(throwaway);
      Formatting := NewFormatting;
      NoNewLine := NewNoNewline;
      end
    else GetChar; {lead one char}
    GetSym; {lead one symbol}
    ProcessText;
    FinalData;
  99:
    if error then exitst(4);
  end {pasmat} .
