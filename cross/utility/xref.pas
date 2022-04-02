{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1980,1981,1982,1983,1984,1985,1986 by Oregon Software, Inc.
  All Rights Reserved.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  xref, cross reference generator
  Release version: 0045  Level: 1  Date: 21-Nov-1990 17:19:09
  Processor: ALL
  System: ALL
  Flavor: ~flavor~
}

{  Cross reference generator for Pascal
                                       N. Wirth   7 May 74
                                       J. Zaun    9 Nov 78
                                       M. Ball    9 Feb 81


   Originally written by N. Wirth for the CDC 6000 Implementation.

   Converted by J. Zaun to "paslist" to cater to the idiosyncracies
   of the Univac 1100 operating system and provide a way to get page
   ejects, etc into a listing.

   Further modified to cater to the idiosyncracies of the PDP-11
   operating systems and reduce space requirements.  This includes
   some blatant hacks to make things fit into a single word of a
   packed record.
}

label
  99;

const

  panic_space = 200; {bytes at which to quit}

  hash_max = 733; {size of hash table}

  blank12 = '            '; {12 blanks}
  id_length = 12; {12 char identifiers}
  ref_length = 6; {length of each reference}

  case_shift = - 32; {ASCII case shift offset}

  {[s=4] key word indexes, defined to allow binary search }
  and_key = 1;       arr_key = 2;       beg_key = 3;       cas_key = 4;
  con_key = 5;       def_key = 6;       div_key = 7;       do_key = 8;
  dwn_key = 9;       els_key = 10;      end_key = 11;      ext_key = 12;
  fil_key = 13;      for_key = 14;      fun_key = 15;      got_key = 16;
  if_key = 17;       in_key = 18;       lab_key = 19;      mod_key = 20;
  nil_key = 21;      non_key = 22;      not_key = 23;      of_key = 24;
  or_key = 25;       org_key = 26;      oth_key = 27;      pac_key = 28;
  prc_key = 29;      prg_key = 30;      rec_key = 31;      rep_key = 32;
  set_key = 33;      shr_key = 34;      str_key = 35;      thn_key = 36;
  to_key = 37;       typ_key = 38;      uni_key = 39;      unt_key = 40;
  use_key = 41;      var_key = 42;      whl_key = 43;      wth_key = 44;

  {[s=1] end key definitions}

type

  index = 0..hash_max; {hash table size}
  alpha = packed array [1..id_length] of char;

  kinds = (no_kind, decl_kind, asg_kind); {ref kinds}

  item_ptr = ^item;
  item =
    packed record
      line: 0..16383; {line no of ref, limits set to allow 2 word item}
      kind: kinds; {reference kind}
      next: item_ptr; {next reference for this entry}
    end;

  entries =
    packed record
      entri: alpha; {item name}
      last: item_ptr; {last reference to this item}
      next_indx: index; {next link in hash chain}
    end;

var

  {input variables}

  id: alpha; {Input identifier}
  id_nmbr, total: integer; {id counters}
  char_cnt, blank_cnt: integer; {character pos with id}
  line_nmbr: integer; {input line number}
  error_line: integer; {Line number of error}
  line_end: boolean; {TRUE at line_end}

  listing_wanted: boolean; {listing of input desired}

  {structured constants}

  r: array [and_key..wth_key] of alpha; {key word table}
  alpha_numerics, numerics, numbers, upper_cases: set of char;
  lower_cases: set of char;
  decls: set of and_key..wth_key; {start of decl}

  decl, def: boolean; {Tells when to mark refs as decl}

  {hash table variables}

  hash_table: array [index] of entries; {the hash table}
  indx, last_indx: index; {hash indexes}
  asg_ref: item_ptr; {line ref of last identitier}
  asg_ok: boolean; {determines which ref is asg ref}

  {output variables}

  done: boolean; {TRUE when done}
  error: boolean; {TRUE when table is full}
  flag_char: char; {char to print along left column}
  current_letter: char; {current xref letter}
  line_length: integer;


function space: integer;

{ Returns the amount of stack space remaining
}

  external;


{*---------------------------------*
 | Read and Process Command String |
 *---------------------------------*}

const
  InputExt = 'pas';
  OutputExt = 'crf';
  CSIprompt = 'XRF>'; {to use if prompting}
  min_line_length = 20; {shortest allowable line width}
  %include csicon;

type
  ArgType = (UnknownArg, InputFileArg, OutputFileArg, WidthArg, ListArg,
             NoListArg, MalformedArg, MissingArg);
  SubArgType = 0..0;
  %include csityp;


procedure exitst(i: Integer);
  external;

%include csipro;
%include getcs;
%include fixarg;
%include cnvnum;


procedure csi;

  const
    {[f-]}
      ArgDefs = ArgDefTable (
        (('                ',  1,  0), 0, OptionalArg, NullArg),
        (('Input_File      ', 11, 10), 1, RequiredArg, FileArg),
        (('Output_File     ',  2, 11), 2, OptionalArg, FileArg),
        (('Width           ',  1,  5), 0, OptionalArg, StringArg),
        (('List            ',  1,  4), 0, OptionalArg, NullArg),
        (('Nolist          ',  3,  6), 0, OptionalArg, NullArg),
        (('                ',  1,  0), 0, OptionalArg, NullArg),
        (('                ',  1,  0), 0, OptionalArg, NullArg));
      {[f+]}

  type
    ErrorMsg = (UnknownArgMsg, MalformedArgMsg, MissingArgMsg, ExtraOutputMsg,
                ExtraInputMsg, BadWidthMsg);

  var
    InputFlg, OutputFlg: (No, Yes, Unknown);
    InputArg, OutputArg: ArgValue;
    error: Boolean;
    iExtVar, oExtVar: FileExt;
    j: iArgValue;


  procedure SetupError(msg: ErrorMsg;
                       arg: ArgValue);


    begin
      case msg of
        UnknownArgMsg: write('Unexpected argument');
        MalformedArgMsg: write('Bad argument syntax');
        MissingArgMsg: write('Required argument missing');
        ExtraOutputMsg: write('Extra output file');
        ExtraInputMsg: write('Extra input file');
        BadWidthMsg: write('Width value out of range');
        end;
      if arg.Len > 0 then write(' (', arg.txt: arg.Len, ')');
      writeln;
      error := true;
    end;


  procedure ProcessArg(arg: ArgValue;
                       typ: ArgType);

    var
      numerror: boolean; {numeric error flag}


    begin
      case typ of
        UnknownArg: SetupError(UnknownArgMsg, arg);
        OutputFileArg:
          begin
          if OutputFlg <> Unknown then SetupError(ExtraOutputMsg, arg);
          OutputArg := arg;
          OutputFlg := Yes;
          end;
        InputFileArg:
          begin
          if InputFlg <> Unknown then SetupError(ExtraInputMsg, arg);
          InputArg := arg;
          InputFlg := Yes;
          end;
        ListArg: listing_wanted := true;
        NoListArg: listing_wanted := false;
        WidthArg:
          begin
          CnvNumericArg(arg, line_length, numerror);
          if numerror or (line_length < min_line_length) then
            SetupError(BadWidthMsg, arg);
          end;
        MalformedArg: SetupError(MalformedArgMsg, arg);
        MissingArg: SetupError(MissingArgMsg, arg);
        end;
    end;


  begin {csi}
    InputFlg := Unknown;
    OutputFlg := Unknown;

    error := false;
    GetCS(ArgDefs, ProcessArg);
    if error then exitst(4);
    for j := 1 to ExtLen do iExtVar[j] := InputExt[j];
    for j := 1 to ExtLen do oExtVar[j] := OutputExt[j];
    if OutputFlg = Yes then
      FixFileArg(OutputArg, ActualFile, oExtVar, OutputArg)
    else FixFileArg(InputArg, DefaultFile, oExtVar, OutputArg);
    FixFileArg(InputArg, ActualFile, iExtvar, InputArg);

    reset(Input, InputArg.txt);
    rewrite(Output, OutputArg.txt);
  end; {csi}



procedure initialize;

  var
    indx: index;


  begin
    {hash table variables}
    for indx := 0 to hash_max - 1 do hash_table[indx].entri := blank12;
    total := 0;
    id_nmbr := 0;
    line_nmbr := 0;
    last_indx := hash_max;

    {I/O related variables}
    done := false;
    error := false;
    current_letter := ' ';
    blank_cnt := id_length;

    {character sets}
    numbers := ['0'..'9'];
    upper_cases := ['A'..'Z', '$'];
    lower_cases := ['a'..'z'];
    alpha_numerics := lower_cases + ['_'] + upper_cases + numbers;
    numerics := ['b', 'B', 'E', 'e', 'D', 'd'] + numbers;

    {syntax variables}
    decl := true;
    def := false;
    asg_ok := true;
    flag_char := ' ';
    decls := [con_key, typ_key, var_key, shr_key, prc_key, prg_key, fun_key];

    {[s=2] key word table -- these entries MUST be in alphabetical order}

    r[and_key] := 'AND         ';        r[arr_key] := 'ARRAY       ';
    r[beg_key] := 'BEGIN       ';        r[cas_key] := 'CASE        ';
    r[con_key] := 'CONST       ';        r[def_key] := 'DEFINE      ';
    r[div_key] := 'DIV         ';        r[do_key] := 'DO          ';
    r[dwn_key] := 'DOWNTO      ';        r[els_key] := 'ELSE        ';
    r[end_key] := 'END         ';        r[ext_key] := 'EXTERNAL    ';
    r[fil_key] := 'FILE        ';        r[for_key] := 'FOR         ';
    r[fun_key] := 'FUNCTION    ';        r[got_key] := 'GOTO        ';
    r[if_key] := 'IF          ';         r[in_key] := 'IN          ';
    r[lab_key] := 'LABEL       ';        r[mod_key] := 'MOD         ';
    r[nil_key] := 'NIL         ';        r[non_key] := 'NONPASCAL   ';
    r[not_key] := 'NOT         ';        r[of_key] := 'OF          ';
    r[or_key] := 'OR          ';         r[org_key] := 'ORIGIN      ';
    r[oth_key] := 'OTHERWISE   ';        r[pac_key] := 'PACKED      ';
    r[prc_key] := 'PROCEDURE   ';        r[prg_key] := 'PROGRAM     ';
    r[rec_key] := 'RECORD      ';        r[rep_key] := 'REPEAT      ';
    r[set_key] := 'SET         ';        r[shr_key] := 'SHARED      ';
    r[str_key] := 'STRING      ';        r[thn_key] := 'THEN        ';
    r[to_key] := 'TO          ';         r[typ_key] := 'TYPE        ';
    r[uni_key] := 'UNIV        ';        r[unt_key] := 'UNTIL       ';
    r[use_key] := 'USE         ';        r[var_key] := 'VAR         ';
    r[whl_key] := 'WHILE       ';        r[wth_key] := 'WITH        ';

    {[s=1]}

    listing_wanted := false;
    line_length := 80;
    csi;

  end {initialize} ;


{*---------------------*
 |  I/O procedures     |
 *---------------------*   }


procedure new_line(ch: char);


  begin { Process a new input line, generating a line number if a listing is
         being generated. NOTE: The linenumber, etc is calculated to take
         exactly 8 characters, so tabs look right }
    line_nmbr := line_nmbr + 1;
    if listing_wanted then write(output, ch, line_nmbr: 6, ' ');
  end;


procedure sget;


  begin
    if eof(input) then goto 99
    else if eoln(input) then
      begin
      if listing_wanted then writeln;
      get(input);
      if not eof(input) then new_line(flag_char);
      line_end := true;
      end
    else
      begin
      line_end := false;
      get(input)
      end;
    if eof(input) then goto 99;
  end;


procedure copy_char;


  begin
    if listing_wanted then write(input^);
    sget;
  end;


 {*-----------------------------*
  | Lexical and Syntax Analysis |
  *-----------------------------* }


function reserved_wrd(word: alpha): boolean;

  var
    low, high, key: integer;
    key_ok: boolean;


  begin
    low := and_key;
    high := wth_key;
    repeat { binary search }
      key := (low + high) div 2;
      if r[key] <= word then low := key + 1;
      if r[key] >= word then high := key - 1;
    until low > high;
    key_ok := (r[key] = word);
    if key_ok then
      begin
      if key in decls then decl := true
      else if key = beg_key then decl := false
      else if (key = rec_key) or (key = cas_key) then def := false;
      end;
    reserved_wrd := key_ok;
  end { function } ;


procedure cross_ref;

  var
    indx, dst, i: index;
    ref: item_ptr;
    found: boolean;


  begin
    indx := 1;
    found := false;
    dst := 1;
    for i := 1 to 8 do indx := abs((indx * ord(id[i])) mod hash_max);
    total := total + 1;
    if space >= panic_space then
      begin
      new(ref);
      if asg_ok then asg_ref := ref;
      with ref^ do
        begin
        line := line_nmbr;
        next := nil;
        if decl and not def then kind := decl_kind
        else kind := no_kind;
        end;
      repeat
        with hash_table[indx] do
          begin
          if entri = id then
            begin {found}
            found := true;
            ref^.next := last;
            last := ref;
            end
          else if entri = blank12 then
            begin {new entri}
            found := true;
            id_nmbr := id_nmbr + 1;
            entri := id;
            last := ref;
            next_indx := last_indx;
            last_indx := indx;
            end
          else
            begin {collision}
            indx := (indx + dst) mod hash_max;
            dst := dst + 2;
            if dst >= hash_max then
              begin
              writeln;
              writeln('**** Too many unique identifiers.');
              error := true;
              error_line := line_nmbr;
              found := true
              end
            end
          end {with}
      until found;
      end
    else
      begin
      writeln;
      writeln('**** Too many references.');
      error := true;
      error_line := line_nmbr;
      end;
  end {cross_ref} ;


procedure identifier;


  begin
    if not error then
      begin
      char_cnt := 0;
      repeat
        if (char_cnt < id_length) then
          begin
          char_cnt := char_cnt + 1;
          if input^ in lower_cases then
            id[char_cnt] := chr(ord(input^) + case_shift)
          else id[char_cnt] := input^;
          end;
        copy_char
      until not (input^ in alpha_numerics);
      if char_cnt >= blank_cnt then blank_cnt := char_cnt
      else
        repeat
          id[blank_cnt] := ' ';
          blank_cnt := blank_cnt - 1
        until blank_cnt = char_cnt;
      if not reserved_wrd(id) then cross_ref;
      end
    else
      repeat
        copy_char
      until not (input^ in alpha_numerics);
  end {identifier} ;


procedure number;


  begin
    repeat
      copy_char;
    until not (input^ in numerics)
  end {number} ;


procedure stringconst;


  begin
    flag_char := 's';
    repeat
      copy_char;
    until (input^ = '''') or (line_end);
    flag_char := ' ';
    copy_char;
  end {stringconst} ;


procedure comment;


  begin
    flag_char := 'c';
    copy_char;
    while input^ <> '}' do copy_char;
    flag_char := ' ';
    copy_char;
  end {comment} ;


procedure comment1(termchar: char);


  begin
    copy_char;
    if input^ = '*' then
      begin
      flag_char := 'c';
      copy_char;
      repeat
        while input^ <> '*' do copy_char;
        copy_char;
      until input^ = termchar;
      flag_char := ' ';
      copy_char;
      end
  end {comment1} ;


procedure special_char;

  var
    got: boolean;


  begin
    got := false;
    if input^ = '[' then asg_ok := false
    else if input^ = ']' then asg_ok := true
    else if input^ = '=' then def := true
    else if input^ = ';' then def := false
    else if input^ = '%' then while not eoln do copy_char
    else if input^ = ':' then
      begin
      copy_char;
      got := true;
      if input^ = '=' then
        begin
        if not error then asg_ref^.kind := asg_kind;
        copy_char;
        end
      else def := true;
      end;
    if not got then
      begin
      copy_char
      end;
  end;


procedure scan_input;


  begin
    new_line(' ');
    while not (done or eof(input)) do
      begin
      if input^ in upper_cases + lower_cases then identifier
      else if input^ in numbers then number
      else if input^ = '''' then stringconst
      else if input^ = '{' then comment
      else if input^ = '(' then comment1(')')
      else if input^ = '/' then comment1('/')
      else special_char;
      end;
  end {print_listing} ;


{*----------------------*
 | hash table printing  |
 *----------------------* }


procedure print_entri(hash: entries);

  var
    out_count: integer;
    ch: char;
    i: integer;
    ref, last_ref, next_ref: item_ptr; {used to track entry list}


  begin
    with hash do
      begin
      if entri[1] <> current_letter then
        begin
        current_letter := entri[1];
        writeln;
        writeln('-', current_letter, '-');
        end;
      write(entri, ' ');
      out_count := id_length + 1;
      end;
    last_ref := nil;
    ref := nil;
    next_ref := hash.last;

    while next_ref <> nil do
      begin
      ref := next_ref;
      next_ref := ref^.next;
      ref^.next := last_ref;
      last_ref := ref;
      end;

    repeat
      if out_count > line_length - ref_length - 1 then
        begin {continue on next line}
        writeln;
        write(' ': id_length + 1);
        out_count := id_length + 1;
        end;
      out_count := out_count + ref_length + 1;
      with ref^ do
        begin
        if kind = decl_kind then ch := '*'
        else if kind = asg_kind then ch := '='
        else ch := ' ';
        write(line: ref_length, ch);
        ref := next
        end;
    until ref = nil;
    writeln;
  end {print_entri} ;


procedure print_cross_ref;

  var
    i, j, min: index;


  begin
    if listing_wanted then page(output);
    writeln('Cross reference:  * indicates definition, = indicates assignment'
            );
    i := last_indx;
    while i <> hash_max do
      begin
      min := i;
      j := hash_table[i].next_indx;
      while j <> hash_max do
        begin
        if hash_table[j].entri < hash_table[min].entri then min := j;
        j := hash_table[j].next_indx;
        end;
      print_entri(hash_table[min]);
      if min <> i then
        begin
        hash_table[min].entri := hash_table[i].entri;
        hash_table[min].last := hash_table[i].last;
        end;
      i := hash_table[i].next_indx;
      end; {while loop}
    writeln;
    writeln;
    if error then
      begin
      writeln('Memory capacity exceeded at line:', error_line: 5);
      writeln('Program too large for xref.');
      end;
    writeln('end xref', id_nmbr: 4, ' identifiers', total: 6,
            ' total references')
  end {print_cross_ref} ;

{ *-------------------*
  |   main program    |
  *-------------------* }


begin
  initialize;
  scan_input;
99:
  print_cross_ref;
end.
