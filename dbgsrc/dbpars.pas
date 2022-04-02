{[l-,b+]}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: all
  System: all

  Pascal-2 debugger parsing routines.

 Last modified by KRIS on 26-Nov-1990 13:48:40
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}



function p_exec(command: packed array [l..h: shortint] of char;
                len: shortint; {length of valid data}
                musttrypath: boolean; {use pathname if needed}
                mustask: boolean {ask a filename} ): shortint;
  external;


{************************************************************************

     ------ Command Line List

************************************************************************}


procedure d$discardcommands;


  begin
    commands := nil;
  end;


procedure d$makeactive {(p: commandpointer)} ;
  { Set up to interpret commands from a command line, by placing the
    line at the front of the active command line list.  Before doing
    so, the list is examined to make sure the command line is not
    already active (due to a bug or recursive macro). }

  const
    ourname = 'makeactive';
  var
    q: commandpointer;


  begin {d$makeactive}
    currentcontext := nil;
    if commands <> nil then
      begin
      if commands^.idx > commands^.len then commands := commands^.next;
      q := commands;
      while q <> nil do
        begin
        if q = p then choke(ourname);
        q := q^.next;
        end;
      end;
    p^.idx := 0;
    p^.next := commands;
    commands := p;
  end; {d$makeactive}


procedure d$deactivate {(p: commandpointer)} ;
  {Remove a command from the active list}

  var
    q: commandpointer;

  begin  {d$deactivate}
    if commands <> nil then
      if p = commands then commands := commands^.next
      else
        begin
        q := commands;
        while (q <> nil) and (q^.next <> p) do q := q^.next;
        if q <> nil then q^.next := p^.next;
        end;
  end;  {d$deactivate}


procedure d$cmderror {(msg: imessage)} ;
  { Report an error in the current command line.  The routine prints
    the error message and identifies the location of the error in the
    source line.  Also, further commands are deleted. }


  begin {d$cmderror}
    if not errorhappened then
      begin
      errorhappened := true;
      d$imesg(msg);
      writeln(out);
      writeln(out, commands^.com: commands^.len);
      writeln(out, ' ': (commands^.idx - 1), '^');
      d$discardcommands;
      end;
  end; {d$cmderror}



{************************************************************************

     ------ Command Input

************************************************************************}


procedure getcommandline(var f: text;
                         var c: commandline);

  { Read a command line from a file into a command line block; in the
    process fold case (except in strings). }

  var
    ch: char;
    quoteflag: boolean;


  procedure geterror(msg: imessage);


    begin
      errorhappened := true;
      d$imesg(msg);
      writeln(out);
      writeln(out, c.com: c.len);
      c.len := 0;
    end;


  begin { GetCommandLine }
    c.len := 0;
    c.idx := 0;
    c.next := nil;

    c.len := 0;
    while (c.len < stringsize) and not eoln(f) do
      begin
      read(f, ch);
      c.len := c.len + 1;
      c.com[c.len] := ch
      end;
    if not eoln(f) then geterror(linetoolong);
    readln(f);
  end; { GetCommandLine }


function currchpos: commandlineindex;
 { Get current position in command line. }


  begin
    if commands = nil then currchpos := 0
    else currchpos := commands^.idx;
  end;


procedure setchpos(pos: commandlineindex);
 { Set current position in command line. }


  begin
    commands^.idx := pos;
  end;


procedure nextch;
 { Advance to next position in command line. }


  begin
    commands^.idx := commands^.idx + 1;
  end;


function currch: char;
 { Get current character from command line. }

  var
    ch: char;

  begin
    if commands = nil then currch := eob
    else
      begin
      if commands^.idx > commands^.len then commands := commands^.next;
      if commands = nil then currch := eob
      else if commands^.idx >= commands^.len then currch := eol
      else 
        begin
        ch := commands^.com[commands^.idx + 1];
        if convertingcase and (ch in ['A'..'Z']) then
          currch := chr(ord(ch) + 32)
        else currch := ch;
        end;
      end;
  end;



{************************************************************************

     ------ Token Scan

************************************************************************}


procedure gettoken;
 { Scan current command line for next token. }

  var
    ch: char;
    startpos: commandlineindex;


  procedure tokerror(msg: imessage);


    begin
      d$cmderror(msg);
      token.typ := unknown;
    end;


  procedure checkreservedwords;
   { Compare an ident to the reserved words list. }

    var
      tokenidx: tokentype;


    begin
      for tokenidx := divtok to elsetok do
        if resvname[tokenidx] = token.identchars then token.typ := tokenidx;
    end;


  procedure getident;
   { Scan an identifier, then check to see if it is a resereved word. }

    var
      i: commandlineindex;
      ch: char;


    begin
      i := 0;
      repeat
        { HUH: long identifiers are truncated silently. }
        if i < msymbolname then
          begin
          i := i + 1;
          token.identchars[i] := currch;
          end;
        nextch;
      until not (currch in ['A'..'Z', 'a'..'z', '0'..'9', '_', '$']);
      for i := i + 1 to msymbolname do token.identchars[i] := ' ';
      token.typ := ident;
      checkreservedwords;
    end;


  procedure getstring;
   { Scan a quoted text string. }

    var
      i: commandlineindex;
      done: boolean;


    begin
      i := 0;
      done := false;
      repeat
        nextch;
        if currch = eol then
          begin
          tokerror(noclosingquote);
          done := true;
          end
        else if currch = '''' then
          begin
          nextch;
          if currch <> '''' then done := true;
          end;
        if not done then
          if i < stringsize then
            begin
            i := i + 1;
            token.s[i] := currch
            end;
      until done;
      token.len := i;
    end;


  procedure getintegerconstant;
   { Scan an integer constant. }

    label
      1;

    type
      idigits = 0..mdigits;

    var
      radix: unsignedint;
      val: unsignedint;
      digits: array [1..mdigits] of 0..15;
      n: idigits;


    procedure abort;


      begin
        tokerror(badinteger);
        goto 1;
      end;


    procedure collectdigits;

      var
        dig: set of char;
        null, zero: boolean;


      begin
        n := 0;
        null := true;
        zero := true;
        if radix <= 10 then dig := ['0'..'9']
        else dig := ['0'..'9', 'a'..'f'];
        while currch in dig do
          begin
          null := false;
          if not zero or (currch <> '0') then
            begin
            zero := false;
            if n = 32 then abort;
            n := n + 1;
            if currch in ['0'..'9'] then digits[n] := ord(currch) - ord('0')
            else digits[n] := ord(currch) - ord('a') + 10;
            end;
          nextch;
          end;
        if null then abort;
      end;


    procedure convertdigits;

      var
        i: idigits;


      begin
        val := 0;
        for i := 1 to n do
          begin
          if digits[i] >= radix then abort;
          if val > ((maxusint - digits[i]) div radix) then abort;
          val := val * radix + digits[i];
          end;
      end;


    begin { GetIntegerConstant }

      { Process radix indicator, collect digits, convert to integer. }

      radix := 10;
      collectdigits;
      if currch = 'b' then
        begin
        { HUH: is this format obsolete? }
        radix := 8;
        nextch;
        end
      else if currch = '#' then
        begin
        convertdigits;
        if not (val in [2, 8, 10, 16]) then abort;
        radix := val;
        nextch;
        collectdigits;
        end;
      convertdigits;

      { Set result, set extended flag if necessary. }

      token.i := val;
      token.extendedint := val > maxint;
    1:
    end; { GetIntegerConstant }


  procedure getrealconstant;
   { Scan a real constant, which is stored as a string. }

    var
      i: commandlineindex;


    begin { GetRealConstant }
      i := 1;
      while (currch in ['0'..'9']) and (i < 32) do
        begin
        token.treal[i] := currch;
        i := i + 1;
        nextch;
        end;
      if (currch = '.') and (i < 32) then
        begin
        token.treal[i] := currch;
        i := i + 1;
        nextch;
        end;
      while (currch in ['0'..'9']) and (i < 32) do
        begin
        token.treal[i] := currch;
        i := i + 1;
        nextch;
        end;
      if (currch = 'e') and (i < 32) then
        begin
        token.treal[i] := currch;
        i := i + 1;
        nextch;
        if (currch in ['+', '-']) and (i < 32) then
          begin
          token.treal[i] := currch;
          i := i + 1;
          nextch;
          end;
        while (currch in ['0'..'9']) and (i < 32) do
          begin
          token.treal[i] := currch;
          i := i + 1;
          nextch;
          end;
        end;
      token.treal[i] := ' ';
    end; { GetRealConstant }


  begin { GetToken }
    convertingcase := false;
    while currch = ' ' do nextch;
    case currch of
      '^':
        begin
        token.typ := uparrow;
        nextch
        end;
      '@':
        begin
        token.typ := at;
        nextch
        end;
      '+':
        begin
        token.typ := plus;
        nextch
        end;
      '-':
        begin
        token.typ := minus;
        nextch
        end;
      '*':
        begin
        token.typ := star;
        nextch
        end;
      '/':
        begin
        token.typ := slash;
        nextch
        end;
      '?':
        begin
        token.typ := ident;
        token.identchars := '?                               ';
        nextch;
        end;
      '!':
        begin
        token.typ := ident;
        token.identchars := '!                               ';
        nextch;
        end;
      '.':
        begin
        nextch;
        if currch = '.' then
          begin
          token.typ := dotdot;
          nextch
          end
        else if currch = ')' then
          begin
          token.typ := rbrack;
          nextch
          end
        else token.typ := dot;
        end;
      ',':
        begin
        token.typ := comma;
        nextch
        end;
      ';':
        begin
        token.typ := semicolon;
        nextch
        end;
      ':':
        begin
        nextch;
        if currch = '=' then
          begin
          token.typ := becomes;
          nextch
          end
        else token.typ := colon;
        end;
      '(':
        begin
        nextch;
        if currch = '.' then
          begin
          token.typ := lbrack;
          nextch
          end
        else token.typ := lpar;
        end;
      ')':
        begin
        token.typ := rpar;
        nextch
        end;
      '<':
        begin
        nextch;
        if currch = '=' then
          begin
          token.typ := lessequal;
          nextch;
          end
        else if currch = '>' then
          begin
          token.typ := notequal;
          nextch;
          end
        else token.typ := lessthan;
        end;
      '>':
        begin
        nextch;
        if currch = '=' then
          begin
          token.typ := greaterequal;
          nextch;
          end
        else token.typ := greaterthan;
        end;
      '=':
        begin
        token.typ := equal;
        nextch;
        end;
      '[':
        begin
        token.typ := lbrack;
        nextch
        end;
      ']':
        begin
        token.typ := rbrack;
        nextch
        end;
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
        begin
        convertingcase := true;
        startpos := currchpos;
        repeat
          nextch
        until not (currch in ['0'..'9']);
        if currch = 'E' then token.typ := realconst
        else
          begin
          token.typ := integerconst;
          if currch = '.' then
            begin
            nextch;
            if (currch <> '.') and (currch <> ')') then
              token.typ := realconst;
            end;
          end;
        if not errorhappened then setchpos(startpos);
        if token.typ = integerconst then getintegerconstant
        else getrealconstant;
        end;
      '''':
        begin
        token.typ := stringconst;
        convertingcase := true;
        getstring;
        end;
      'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
      'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
      'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '$':
        getident;
      otherwise
        begin
        if currch = eob then token.typ := endmark
        else
          begin
          if currch = eol then token.typ := semicolon
          else token.typ := unknown;
          nextch;
          end;
        end;
      end;
  end; { GetToken }


function getstoredcommands: commandpointer;

  { Read a sequence of debugger commands of the form:
        <CMD1; CMD2; ...;CMDN>
    from the current command line, and store them in a new command line
    block to be interpreted later.  Return a pointer to the block, NIL
    if no commands are present. }

  var
    c: commandpointer;
    i: commandlineindex;
    markpos: commandlineindex;
    quoteflag, done: boolean;
    prevch, ch: char;


  begin { GetStoredCommands }
    getstoredcommands := nil;
    if token.typ = lessthan then
      begin
      new(c);
      i := 0;
      quoteflag := false;
      done := false;
      ch := currch;
      prevch := ch;
      while not done and (ch <> eol) do
        begin
        if ch = '''' then quoteflag := not quoteflag
        else if not quoteflag then
          if ((prevch = '>') and (ch = ';')) then done := true;
        if not done then
          begin
          if ch = '>' then markpos := i;
          i := i + 1;
          c^.com[i] := ch;
          if ch <> ' ' then prevch := ch;
          nextch;
          ch := currch;
          end;
        end;
      if prevch = '>' then c^.len := markpos
      else c^.len := i;
      getstoredcommands := c;
      gettoken;
      end;
  end; { GetStoredCommands }


function booleanvalue(var bool: boolean): boolean;
 {Scan for identifiers TRUE and FALSE and return the boolean value}

  var
    i: integer;
    str: packed array [1..8] of char;


  begin {booleanvalue}
    booleanvalue := false;
    if token.typ = ident then
      begin
      for i := 1 to 8 do str[i] := token.identchars[i];
      if str = 'true    ' then
        begin
        bool := true;
        booleanvalue := true;
        end
      else if str = 'false   ' then
        begin
        bool := false;
        booleanvalue := true;
        end;
      gettoken;
      end;
  end; {booleanvalue}


function getstmt(var mdl: mdl_pointer;
                 var proc: proctreeptr;
                 var stmt: mapindex): boolean;

  { Scan input that identifies a statement, and return module index, procedure
    symbol table index, and statement map index.  The input is of one of
    the forms:
        NAME,STMT
        NAME
        STMT
    where NAME is the name of the procedure, and STMT is an integer.  If only
    STMT is given then the currentcontext procedure is default name, and if 
    only the NAME is given then 1 is the default STMT. }

  label
    1;

  var
    name: symbolname;
    stmtno: mapindex;
    status: boolean;


  procedure stmterror(msg: imessage);


    begin
      d$cmderror(msg);
      goto 1;
    end;


  begin { GetStmt }
    status := false;
    if currentcontext = nil then
      begin
      if d$locateaddr(currentpc, mdl, proc, stmt, false) then 
        begin
        if needcaching then proc^ := proctreefile^;
        end
      else currentcontext := d$firstvisibleframe;
      end;
    if currentcontext <> nil then
      begin
      mdl := currentcontext^.mdl;
      if needcaching then 
        begin
        if currentcontext^.proc <> nil then proc^ := currentcontext^.proc^
        else 
          begin
          d$freeproctreerec(proc);
          proc := nil;
          end;
        end
      else proc := currentcontext^.proc;
      end;

    if token.typ = lpar then gettoken;
    if token.typ = ident then
      begin
      name := token.identchars;
      gettoken;
      if token.typ = colon then
        begin
        if not locatemodident(name, mdl) then stmterror(nomodname);
        gettoken;
        if token.typ <> ident then stmterror(noprocname);
        name := token.identchars;
        gettoken;
        end;
      if not locateprocident(name, mdl, proc) then stmterror(procnotfound);
      if needcaching then proc^ := proctreefile^;
      if token.typ = comma then gettoken;
      end
    else if token.typ <> integerconst then stmterror(noprocname);

    if token.typ = integerconst then
      begin
      stmtno := token.i;
      gettoken;
      end
    else if (token.typ in [rpar, lessthan, semicolon]) then stmtno := 1
    else stmterror(stmtnoexpected);
    if not d$stmtlocate(mdl, proc, stmtno, stmt) then
      stmterror(stmtnotfound);
    if token.typ = rpar then gettoken;
    status := true;
  1:
    getstmt := status;
  end; { GetStmt }


function getframe(var f: stackpointer): boolean;

  { Scan input that identifies an execution frame, and return a pointer
    to the designated framelist entry.  The input is of the form:
        NUMBER
    where a positive NUMBER is the dynamic frame level, and a negative
    number is the relative frame level from the currentcontext. }

  label
    1;

  var
    i: integer;
    negate: boolean;


  procedure frameerror(msg: imessage);


    begin
      d$cmderror(msg);
      getframe := false;
      goto 1;
    end;


  begin { GetFrame }
    negate := false;
    if currentcontext = nil then currentcontext := d$firstvisibleframe;
    if errorhappened then goto 1;
    if token.typ = lpar then gettoken;
    if token.typ = minus then
      begin
      negate := true;
      gettoken;
      end;
    if token.typ <> integerconst then frameerror(numberexpected);
    i := token.i;
    if negate then i := - i;
    gettoken;
    if token.typ = rpar then gettoken;
    if (i < 0) then i := currentstackframe^.dynamiclevel + i;
    if (i < 0) or (i > currentstackframe^.dynamiclevel) then
      frameerror(nosuchframe);
    if (i = 0) then f := currentcontext^.mdl^.base_frame
    else
      begin
      f := currentstackframe;
      while (f <> nil) and (f^.dynamiclevel > i) do f := f^.dynamiclink;
      if f^.mdl = nil then frameerror(nosuchframe);
      end;
    getframe := true;
  1:
  end; { GetFrame }



{************************************************************************

     ------ Expression Evaluation

  The following routines evaluate command lines expressions using the 
  grammar:

    E  -> E  >|<|>=|<=|<>|= *T
         | *T
    *T -> *T +|-|or !T
         | !T
    !T -> !T *|/|div|mod|and F
         | F
    F  -> not|-|+|@ ident|constant
         | (E)
         | ident|constant

  The parsing process will construct a datatokens list that will be passed
  to the target supervisor.  Expressions will end up in reverse Polish
  when sent to the target.  In the recursive descent process, dataitems will
  be maintained with their own datatokenlists.  As these dataitems are
  operated on, their datatokenlists will be joined together with the
  current operator token appended to the resulting list.

************************************************************************}


procedure setregresidence {(var item: dataitem; storetyp: data_access_types;
                           regidx: regindex; lowframe: stackpointer;
                           highframe: stackpointer; var lost: boolean)} ;

    {Determine the residence of a register allocated varialble, which
     may be resident on the stack if the context is less than the
     current active frame.  The parameter "lost" informs whether the
     register location on the stack was found. }

  label
    1;

  var
    f: stackpointer;
    done, stacked, flg: boolean;
    r: regindex;
    tmpaddr: addressrec;
    rec: debugrecord;


  begin {setregresidence}

    lost := false;
    if embeddedjump and programdead and (lowframe = currentstackframe) then
      lost := true
    else if lowframe = highframe then
      begin
      adddatatoken(item, addressref);
      with item.dtok^ do
        begin
        store := storetyp;
        off := regidx;
        end;
      end
    else
      begin
      done := false;
      stacked := false;
      flg := false;
      f := lowframe;
      while not done and not lost do
        begin
        f := f^.uplink;
        if f^.proc = nil then lost := true
        else
          begin
          d$getobject(f^.mdl, f^.proc^.firstsym + 1, rec);
          case storetyp of
            gen_reg: flg := rec.genregssaved[regidx];
            ptr_reg: flg := rec.ptrregssaved[regidx];
            real_reg: flg := rec.realregssaved[regidx];
            end;
          end;
        if flg then
          begin
          lost := false;
          done := true;
          stacked := true;
          end
        else done := (f = highframe);
        end {while} ;

      if stacked then
        begin
        tmpaddr.addr := f^.database - rec.blocksize;
        if segmented then tmpaddr.segment := stacksegment;
        if maxptrregistermask > 0 then
          if targetmachine in [iapx86, i80386] then
	    begin
            for r := 0 to maxptrregistermask do
              if rec.ptrregssaved[r] then
                begin
                tmpaddr.addr := tmpaddr.addr - targetregistersize;
                if (storetyp = ptr_reg) and (r = regidx) then goto 1;
                end;
	    end
	  else
            for r := maxptrregistermask downto 0 do
              if rec.ptrregssaved[r] then
                begin
                tmpaddr.addr := tmpaddr.addr - targetregistersize;
                if (storetyp = ptr_reg) and (r = regidx) then goto 1;
                end;
        if maxgenregistermask > 0 then
	  if targetopsys = msdos then
	    begin
            for r := 0 to maxgenregistermask do
              if rec.genregssaved[r] then
                begin
                tmpaddr.addr := tmpaddr.addr - targetregistersize;
                if (storetyp = gen_reg) and (r = regidx) then goto 1;
                end;
	    end
	  else
            for r := maxgenregistermask downto 0 do
              if rec.genregssaved[r] then
                begin
                tmpaddr.addr := tmpaddr.addr - targetregistersize;
                if (storetyp = gen_reg) and (r = regidx) then goto 1;
                end;
        if maxrealregistermask > 0 then
            for r := maxrealregistermask downto 0 do
              if rec.realregssaved[r] then
                begin
                tmpaddr.addr := tmpaddr.addr - targetfpregistersize;
                if (storetyp = real_reg) and (r = regidx) then 
                  begin
                  item.wasfpreg := true;
                  goto 1;
                  end;
                end;
      1:
        adddatatoken(item, addressref);
        with item.dtok^ do
          begin
          store := dat_space;
          addr := tmpaddr;
          end;
        if not originright then
          begin
          adddatatoken(item, offset);
          item.dtok^.bitoff := bitspertransferunit - item.bitlen;
          end;
        end
      else if not lost then
        begin
        adddatatoken(item, addressref);
        with item.dtok^ do
          begin
          store := storetyp;
          off := regidx;
          end;
        if not originright then
          begin
          adddatatoken(item, offset);
          item.dtok^.bitoff := bitspertransferunit - item.bitlen;
          end;
        end;
      end;
  end; {setregresidence}



procedure experror(var item: dataitem;
                   msg: imessage);

    {When an error occurs during expression evaluation we emit the
     error message, then clear the datatokens for the given item and
     set the global flag errorhappened.}


  begin
    d$cmderror(msg);
    item.mdl := nil;
    item.frame := nil;
    item.index := noneindex;
    cleardatatokens(item);
    errorhappened := true;
  end;


procedure expression(var resultitem: dataitem);
  forward;


procedure createreferencetoken {(var item:dataitem;
                                 usebaseform, usescalar: boolean)};
  {Create a reference data token for ITEM}

  var
    form: debugrecord;
    i: symbolindex;

  begin  {createreferencetoken}
    adddatatoken(item, reference);
    with item.dtok^ do
      begin
      reflen := item.bitlen;
      signed := d$signed(item);
      if usescalar and (item.index <> realindex) and 
          (item.index <> doubleindex) then
        begin
        reftyp := scalars;
        refisstring := false;
        end
      else
        begin
        if usebaseform then d$getbaseform(item, form, i)
        else d$getform(item, form);
        reftyp := form.typ;
        refisstring := (form.typ = strings);
        if reftyp in [subranges, scalars, bools, chars] then
          begin
          lowerlim := lower(form);
          upperlim := upper(form);
          end
        else if reftyp in [reals, doubles] then
             {real registers stored on the stack need to be noted since
              they are stored in extended format}
            isfppreg := item.wasfpreg;
        end;
  
      end;
  end;  {createreferencetoken}



procedure factor(var resultitem: dataitem);

{  Parse 
    F  -> not|-|+|@ ident|constant
         | (E)
         | ident|constant

   Variable or function identifiers have datatokens generated that specify
   the location to find the value desired.  Constants are stored in a
   datapacket list (normally just one element to the list) which is carried
   around by the "atomic" datatoken.  The unary operators cause the 
   appropriate operator to be added to the datatoken list, in reverse Polish.
}

  var
    f: stackpointer;
    index: symbolindex;
    symbol: debugrecord;
    predef: predeftypes;
    negate: boolean;
    form: debugrecord;
    i: symbolindex;




  procedure indexexpression(f: stackpointer;
                            var resultform: debugrecord);

    {Parse an expression of the form [expression] or [expression..expression].
     The "evaloffset" datatoken is sent to instruct the target supervisor to 
     adjust the current address by the offset which is determined from the
     target stack.  If a '..' is encoutered then the resulting datatitem is
     given a new temporary form which uses as its index form another newly
     created temporary form which describes the given subrange. }

    var
      indexform: debugrecord;
      indexitem: dataitem;
      indextype: symbolindex;
      eltsize: integer;
      upperlim, lowerlim, origlowerlim, origupperlim: integer;


    procedure writeindexerror(val: integer;
                              err: imessage);

      {Write out the index value that caused an index error then
       emit the error message.
      }


      begin {writeindexerror}
        writeln(out, 'ord of index = ', val);
        d$cmderror(err);
      end; {writeindexerror}


    procedure indexsubrangeexpression;

      {Deal with an array subrange (e.g.  w X[I..J]).  A temporary form
       is created to describe that array subrange as a new type.
      }

      var
        packet: datapacketptr;
        err: imessage;


      begin {indexsubrangeexpression}
        gettoken;
        d$getobject(resultitem.mdl, resultform.indextype, indexform);
        origlowerlim := 
                  d$lowerbound(resultitem.mdl, resultitem.frame, resultform);
        origupperlim :=
                  d$upperbound(resultitem.mdl, resultitem.frame, resultform);
        packet := getdatapacket;
        packet^.next := nil;
        if not d$getvalue(indexitem, scalar, origlowerlim, origupperlim,
                          packet, err) then
          d$cmderror(err)
        else if err <> notanerror then writeindexerror(packet^.sc, err)
        else
          begin
          lowerlim := packet^.sc;
          expression(indexitem);
          if not errorhappened then
            if not compatible(resultitem.mdl, resultform.indextype, 
                              indexitem.mdl, indexitem.index) then
              experror(indexitem, indexnotcompatible)
            else
              begin
              if not d$getvalue(indexitem, scalar, origlowerlim, origupperlim,
                                packet, err) then
                d$cmderror(err)
              else if err <> notanerror then writeindexerror(packet^.sc, err)
              else
                begin
                upperlim := packet^.sc;
                if upperlim < lowerlim then d$cmderror(badexpression)
                else
                  begin
                  if indexform.typ = ints then 
                    begin
                    indexform.typ := subranges;
                    indexform.parenttype := intindex;
                    end;
                  with indexform do
                    if typ = scalars then
                      begin
                      lastord := upperlim - lowerlim;
                      firstscalar := firstscalar + 2 * lowerlim;
                      end
                    else if typ = subranges then
                      begin
                      lowerord := lowerlim;
                      upperord := upperlim;
                      end;
                  d$tmpobject(resultitem.mdl, indextype, indexform);
                  resultform.indextype := indextype;
                  if resultform.packedflag then
                    resultitem.bitlen := resultform.elementsize * (upperlim -
                                         lowerlim + 1)
                  else
                    resultitem.bitlen := resultform.elementsize *
                                         bitsperunit * (upperlim - lowerlim +
                                         1);
                  if resultform.bitaddress then
                    resultform.size := resultitem.bitlen
                  else resultform.size := resultitem.bitlen div bitsperunit;
                  if resultform.typ in [strings, conformantarrays] then 
                    resultform.typ := arrays;
                  d$tmpobject(resultitem.mdl, resultitem.index, resultform);
                  packet^.sc := lowerlim;
                  adddatatoken(resultitem, atomic);
                  resultitem.dtok^.packet := packet;
                  adddatatoken(resultitem, evaloffset);
                  with resultitem.dtok^ do
                    begin
                    if resultform.bitaddress then bitlength := indexform.size
                    else bitlength := indexform.size * bitsperunit;
                    lowerbnd := origlowerlim;
                    upperbnd := origupperlim;
                    if resultform.packedflag then
                      bitsperelt := resultform.elementsize
                    else bitsperelt := resultform.elementsize * bitsperunit;
                    end;
                  end;
                end
              end;
          end;
        if errorhappened then freedatapacket(packet);
      end; {indexsubrangeexpression}


    begin {indexexpression}
      if not (resultform.typ in [strings, arrays, conformantarrays]) then
        experror(resultitem, arrayexpected)
      else
        begin
        adddatatoken(resultitem, newlevel);
        indexitem.datatokenlist := nil;
        indexitem.dtok := nil;
        expression(indexitem);
        if errorhappened then cleardatatokens(indexitem)
        else
          begin
            if not compatible(resultitem.mdl, resultform.indextype, 
                              indexitem.mdl, indexitem.index) then
            experror(indexitem, indexnotcompatible)
          else if token.typ = dotdot then indexsubrangeexpression
          else
            begin
            if indexitem.store <> local then
              createreferencetoken(indexitem, false, false);
            d$getobject(resultitem.mdl, resultform.indextype, indexform);
            adddatatoken(indexitem, evaloffset);
            with indexitem.dtok^ do
              begin
              if resultform.bitaddress then bitlength := indexform.size
              else bitlength := indexform.size * bitsperunit;
              lowerbnd := 
                  d$lowerbound(resultitem.mdl, resultitem.frame, resultform);
              upperbnd := 
                  d$upperbound(resultitem.mdl, resultitem.frame, resultform);
              if resultform.packedflag then
                bitsperelt := resultform.elementsize
              else bitsperelt := resultform.elementsize * bitsperunit;
              end;
            resultitem.index := resultform.elementtype;
            if resultform.packedflag then
              resultitem.bitlen := resultform.elementsize
            else resultitem.bitlen := resultform.elementsize * bitsperunit;
            d$getform(resultitem, resultform);
            appendtokenlist(resultitem, indexitem);
            end;
          end;
        end;
    end; {indexexpression}



  procedure fieldspecifier(f: stackpointer;
                           var resultform: debugrecord);

   {Parse a field element of data expression.  A datatoken, "offset" is
    added to the list to instruct the target to adjust the current address. }

    var
      index: symbolindex;
      symbol: debugrecord;


    begin {fieldspecifier}
      if resultform.typ <> fields then experror(resultitem, recordexpected)
      else
        begin
        gettoken;
        if token.typ <> ident then experror(resultitem, fieldexpected)
        else
          begin
          if not locatefieldident(token.identchars, resultform.fieldid,
                                  resultform.fieldlevel, f,
				  resultform.firstfield,
				  resultform.lastfield, index) then
            experror(resultitem, notafield)
          else
            begin
            d$getobject(f^.mdl, index, symbol);
            adddatatoken(resultitem, offset);
            with resultitem.dtok^ do
              begin
              if resultform.packedflag then bitoff := symbol.offset
              else bitoff := symbol.offset * bitsperunit;
              end;
            resultitem.index := symbol.vartype;
            if resultform.bitaddress then resultitem.bitlen := symbol.length
            else resultitem.bitlen := symbol.length * bitsperunit;
            d$getform(resultitem, resultform);
            gettoken;
            end;
          end;
        end;
    end; {fieldspecifier}


  procedure identifiedvariable(f: stackpointer;
                               var resultform: debugrecord);
    { Dereference a pointer expression.  If it is a file pointer, then
      dereference twice. }


    begin {identifiedvariable}
      if not (resultform.typ in [ptrs, files]) then
        experror(resultitem, pointerexpected)
      else
        begin
        adddatatoken(resultitem, deref);
        if resultform.typ = ptrs then resultitem.index := resultform.ptrtype
        else
          begin
          adddatatoken(resultitem, deref);
          resultitem.index := resultform.filebasetype;
          end;
        d$getform(resultitem, resultform);
        if resultform.bitaddress then resultitem.bitlen := resultform.size
        else resultitem.bitlen := resultform.size * bitsperunit;
        gettoken;
        end;
    end; {identifiedvariable}



  procedure variableaccess(f: stackpointer;
                           var symbol: debugrecord;
                           var resultitem: dataitem);
    {  Create a dataitem for a variable, determine its location and create
       the datatokens to pass that information to the target supervisor.  }

    var
      resultform: debugrecord;
      lost: boolean;
      alloc: allockind;


    begin
      resultitem.mdl := f^.mdl;
      resultitem.frame := f;
      if symbol.namekind = funcname then resultitem.index := symbol.functype
      else resultitem.index := symbol.vartype;
      d$getform(resultitem, resultform);
      if resultform.bitaddress then resultitem.bitlen := resultform.size
      else resultitem.bitlen := resultform.size * bitsperunit;
      if symbol.namekind = funcname then alloc := normalalloc
      else alloc := symbol.varalloc;
      resultitem.wasfpreg := false;
      case alloc of
        genregister:
          begin
          resultitem.store := gen_reg;
          resultitem.regoff := genregindexes[symbol.offset];
          setregresidence(resultitem, gen_reg, genregindexes[symbol.offset],
                          f, currentstackframe, lost);
          if lost then experror(resultitem, cantfindreg);
          end;
        ptrregister:
          begin
          resultitem.store := ptr_reg;
          resultitem.regoff := ptrregindexes[symbol.offset];
          setregresidence(resultitem, ptr_reg, ptrregindexes[symbol.offset],
                          f, currentstackframe, lost);
          if lost then experror(resultitem, cantfindreg);
          end;
        realregister:
          begin
          resultitem.store := real_reg;
          resultitem.regoff := realregindexes[symbol.offset];
          setregresidence(resultitem, real_reg, realregindexes[symbol.offset],
                          f, currentstackframe, lost);
          if lost then experror(resultitem, cantfindreg);
          end;
        normalalloc, ownalloc, pointeralloc, absolute:
          begin
          resultitem.store := dat_space;
          adddatatoken(resultitem, addressref);
          with resultitem.dtok^ do
            begin
            store := dat_space;
            if (alloc = absolute) then
              addr.offset := symbol.offset
            else if (symbol.namekind <> funcname) and 
                (symbol.varalloc = normalalloc) and (f^.lexiclevel <= 1) then
              addr := globaldatabase
            else 
	      begin
	      if segmented then 
		if (symbol.varalloc = ownalloc) then
		  addr.segment := f^.mdl^.datasegment
		else addr.segment := stacksegment;
	      addr.addr := f^.database;
	      end;
            end;
          if (alloc <> absolute) then
            begin
            adddatatoken(resultitem, offset);
            with resultitem.dtok^ do
              begin
              if symbol.namekind = funcname then 
                bitoff := symbol.funcoffset * bitsperunit
              else
                bitoff := symbol.offset * bitsperunit;
              end;
            end;
          end;
        definealloc, usealloc, sharedalloc:
          begin
          resultitem.store := dat_space;
          adddatatoken(resultitem, addressref);
          with resultitem.dtok^ do
            begin
            store := dat_space;
            addr := resultitem.mdl^.externaladdress;
            end;
          if targetopsys = vdos then
            if (alloc = usealloc) or (alloc = definealloc) then
              begin
              adddatatoken(resultitem, offset);
              resultitem.dtok^.bitoff := 
                resultitem.mdl^.picoffset * bitsperunit;
              end;
          adddatatoken(resultitem, offset);
	  resultitem.dtok^.bitoff :=
	    (symbol.offset - 1) * bitsperunit * targetaddresssize;
	  adddatatoken(resultitem, deref);
          end;
         end {case} ;

      if not errorhappened then
        begin
        if (symbol.namekind in [varparam, confparam, varconfparam]) or
           (symbol.varalloc = pointeralloc) then
          adddatatoken(resultitem, deref);
        gettoken;

        while (token.typ in [lbrack, dot, uparrow]) and not errorhappened do
          case token.typ of
            lbrack:
              begin
              repeat
                gettoken;
                indexexpression(f, resultform);
              until (token.typ <> comma) or errorhappened;
              if token.typ <> rbrack then experror(resultitem, badindex);
              gettoken;
              end;
            dot: fieldspecifier(f, resultform);
            uparrow: identifiedvariable(f, resultform);
            end {case} ;
        end;

    end; {variableaccess}




  procedure predefined(predef: predeftypes;
                       var resultitem: dataitem);
   { Evaluate the predefined procedures or funtions. }

    var
      targetitem: dataitem;
      resultform: debugrecord;
      i: symbolindex;
      packet: datapacketptr;
      err: imessage;
      sigmask: dbset;
      sigmaskindex: symbolindex;
      sigitem: dataitem;
      sigform: debugrecord;

    procedure doloophole;
      {Evaluate loophole(variable, expression)
       Parse the target variable, then toss out the datatokenlist
       since all that is needed is the target's index and mdl, which
       are bound to the resulting dataitem.}

      begin  {doloophole}
        if token.typ <> ident then experror(resultitem, {} badexpression)
        else
          begin
          targetitem.datatokenlist := nil;
          targetitem.dtok := nil;
          factor(targetitem);
          cleardatatokens(targetitem);
          if errorhappened then cleardatatokens(resultitem)
          else if token.typ <> comma then
            experror(resultitem, commaexpected)
          else
            begin
            gettoken;
            expression(resultitem);
            if not errorhappened then
              begin
              if resultitem.index = realindex then
                resultitem.bitlen := targetitem.mdl^.realsize *
                                     bitsperunit;
              if targetitem.bitlen <> resultitem.bitlen then
                experror(resultitem, {} badexpression)
              else
                begin
                resultitem.index := targetitem.index;
                resultitem.mdl := targetitem.mdl;
                end;
              end {not errorhappened} ;
            end {typ = comma} ;
          end {typ = ident} ;
      end;  {doloophole}


    procedure dochr;
          { Evaluate chr(expression) }
      begin  {dochr}
        expression(resultitem);
        if not errorhappened then
          begin
          d$getbaseform(resultitem, resultform, i);
          if resultform.typ in [scalars, ints, bools, chars] then
            resultitem.index := charindex
          else experror(resultitem, badexpression);
          end;
      end;  {dochr}

    procedure doord;
      { Evaluate ord(expression) }
      begin  {doord}
        expression(resultitem);
        if not errorhappened then
          begin
          d$getbaseform(resultitem, resultform, i);
          if resultform.typ in [scalars, ints, bools, chars, ptrs] then
            resultitem.index := intindex
          else experror(resultitem, badexpression);
          end;
      end;  {doord}

    procedure doref;
          { Evaluate ref(expression) }
      begin  {doref}
        if token.typ <> ident then experror(resultitem, {} badexpression)
        else
          begin
          factor(resultitem);
          if not errorhappened then
            if (resultitem.store in 
                              [gen_reg, ptr_reg, real_reg, fpp_reg]) then
              experror(resultitem, norefonregstore)
            else if (resultitem.store <> dat_space) then 
              experror(resultitem, badexpression)
            else
              with resultitem do
                begin
                adddatatoken(resultitem, returnaddr);
                index := nilindex;
                bitlen := ptrsize * bitsperunit;
                store := local;
                end;
          end;
      end {doref};

    procedure dosigblock;
      {Evaluate sigblock()}
      begin
        sigitem.datatokenlist := nil;
        sigitem.dtok := nil;
        expression(sigitem);
{}{test for type}
        if not errorhappened then
          begin
          packet := getdatapacket;
          if not d$getvalue(sigitem, settype, 0, 0, packet, err) then
            d$cmderror(err);
          if not errorhappened then d$setsigblock(packet^.st);
          freedatapacket(packet);
          end;
        cleardatatokens(sigitem)
      end;  {psigblock}


    procedure dosigmask;
      {Evaluate sigmask()}
      begin  {dosigmask}
        d$getsigmask(sigmask);
        resultitem.mdl := nil;
        resultitem.frame := nil;
        resultitem.store := local;
        with sigform do
          begin
          kind := formdesc; 
          packedflag := false;
          bitaddress := false;
          typ := sets;
          basetype := intindex;
          end;
        d$tmpobject(resultitem.mdl, sigmaskindex, sigform);
        resultitem.index := sigmaskindex;
        adddatatoken(resultitem, atomic);
        with resultitem.dtok^ do
          begin
          packet := getdatapacket;
          packet^.next := nil;
          packet^.typ := settype;
          packet^.st := sigmask;
          packet^.len := setsize * bitsperunit;
          end;
      end;  {dosigmask}


    begin {predefined}
      gettoken;
      if token.typ <> lpar then experror(resultitem, lparenexpected)
      else
        begin
        gettoken;
        case predef of
          ploophole: doloophole;
          pchr: dochr;
          pord: doord;
          pref: doref;
          psigblock: dosigblock;
          psigmask: dosigmask;
          end {case};
        if token.typ <> rpar then experror(resultitem, rparenexpected);
        gettoken;
        end;
    end; {predefined}



  procedure setconstructor(var resultitem: dataitem);

      {Construct a set constant from the command line.  This is done by
       sending the target a list of datatokens, each of which adds a single
       element to the set.  A temporary form describing the set is also 
       added to the form cache}

    var
      membermdl: mdl_pointer;
      membertype: symbolindex;
      memberform, resultform: debugrecord;


    procedure memberdesignator;


      procedure memberexpression;
       { Evaulate a single element in the set. }

        var
          memberitem: dataitem;


        begin {memberexpression}
          adddatatoken(resultitem, newlevel);
          memberitem.datatokenlist := nil;
          memberitem.dtok := nil;
          expression(memberitem);
          if errorhappened then cleardatatokens(resultitem)
          else
            begin
            appendtokenlist(resultitem, memberitem);
            adddatatoken(resultitem, resetlevel);
            if membertype = noneindex then
              begin
              d$getbaseform(memberitem, memberform, membertype);
              if not (memberform.typ in 
                        [subranges, scalars, ints, bools, chars]) then
                experror(resultitem, badsetelement);
              membermdl := memberitem.mdl;
              end;
            if not errorhappened then
              if not compatible(membermdl, membertype, 
                                memberitem.mdl, memberitem.index) then
                experror(resultitem, elementnotcompatible);
            end;
        end; {memberexpression}


      begin {memberdesignator}
        memberexpression;
        if not errorhappened then
          if token.typ = dotdot then
            begin
            gettoken;
            memberexpression;
            if not errorhappened then
              begin
              adddatatoken(resultitem, addtoset);
              resultitem.dtok^.range := true;
              end;
            end
          else
            begin
            adddatatoken(resultitem, addtoset);
            resultitem.dtok^.range := false;
            end;
      end; {memberdesignator}


    begin {setconstructor}
      membermdl := nil;
      membertype := noneindex;
      adddatatoken(resultitem, makeset);
      gettoken;
      if token.typ <> rbrack then
        begin
        memberdesignator;
        while (token.typ = comma) and not errorhappened do
          begin
          gettoken;
          memberdesignator;
          end;
        if token.typ <> rbrack then experror(resultitem, commaexpected);
        end;

      if not errorhappened then
        begin
        with resultform do
          begin
          kind := formdesc;
          packedflag := true;
          bitaddress := true;
          size := d$min(upper(memberform) + 1, setsize * bitsperunit);
          typ := sets;
          basetype := membertype;
          end;
        resultitem.mdl := membermdl;
        resultitem.frame := nil;
        resultitem.store := local;
        d$tmpobject(resultitem.mdl, resultitem.index, resultform);
        gettoken;
        end;
    end; {setconstructor}



  procedure characterstring(var resultitem: dataitem);
      {Create a linked list of datapackets to hold the string, which is then
       carried to the target supervisor via the datatoken "atomic".  Temporary
       forms are also created to describe the string and its index. }

    var
      resultform, indexform: debugrecord;
      indextype: symbolindex;
      numpackets, base: integer;
      p: datapacketptr;
      i: integer;
      j: 1..datapacketsize;


    begin {characterstring}
      resultitem.mdl := nil;
      resultitem.frame := nil;
      resultitem.store := local;
      adddatatoken(resultitem, atomic);
      with resultitem.dtok^ do
        begin
        packet := getdatapacket;
        if token.len = 1 then
          begin
          resultitem.index := charindex;
          packet^.typ := scalar;
          packet^.next := nil;
          packet^.sc := ord(token.s[1]);
          packet^.len := dfltintsize * bitsperunit;
          resultitem.bitlen := packet^.len;
          end
        else
          begin
          numpackets := (token.len - 1) div datapacketsize + 1;
          base := 0;
          packet^.typ := stringtype;
          packet^.len := token.len * bitsperunit;
          resultitem.bitlen := packet^.len;
          packet^.next := nil;
          p := packet;
          for i := 1 to numpackets - 1 do
            begin
            for j := 1 to datapacketsize do p^.str[j] := token.s[base + j];
            base := i * datapacketsize;
            p^.next := getdatapacket;
            p := p^.next;
            p^.typ := stringtype;
            p^.next := nil;
            end;
          for j := 1 to (token.len mod datapacketsize) do
            p^.str[j] := token.s[base + j];
          with indexform do
            begin
            kind := formdesc;
            packedflag := false;
            bitaddress := false;
            size := dfltintsize;
            typ := subranges;
            lowerord := 1;
            upperord := token.len;
            extended := false;
            parenttype := intindex;
            end;
          d$tmpobject(resultitem.mdl, indextype, indexform);
          with resultform do
            begin
            kind := formdesc;
            packedflag := true;
            bitaddress := false;
            size := token.len;
            typ := arrays;
            elementtype := charindex;
            elementsize := bitsperunit * charsize;
            end;
          resultform.indextype := indextype;
          d$tmpobject(resultitem.mdl, resultitem.index, resultform);
          end;
        end;
      gettoken;
    end; {characterstring}



  procedure integerconstant(var resultitem: dataitem);
      { Create a datapacket to carry the integer to the target via the datatoken
        "atomic". }


    begin {integerconstant}
      resultitem.mdl := nil;
      resultitem.frame := nil;
      resultitem.store := local;
      resultitem.index := intindex;
      adddatatoken(resultitem, atomic);
      with resultitem.dtok^ do
        begin
        packet := getdatapacket;
        packet^.typ := scalar;
        packet^.next := nil;
        packet^.sc := token.i;
        packet^.len := dfltintsize * bitsperunit;
        resultitem.bitlen := packet^.len;
        end;
      gettoken;
    end; {integerconstant}


  procedure realconstant(var resultitem: dataitem);
      { Create a datapacket to carry the real to the target via the datatoken
        "atomic". }

    var
      i: 1..datapacketsize;


    begin {realconstant}
      resultitem.mdl := nil;
      resultitem.frame := nil;
      resultitem.store := local;
      resultitem.index := realindex;
      adddatatoken(resultitem, atomic);
      with resultitem.dtok^ do
        begin
        packet := getdatapacket;
        packet^.typ := realstring;
        packet^.next := nil;
        for i := 1 to datapacketsize do packet^.str[i] := token.treal[i];
        end;
      gettoken;
    end; {realconstant}




  procedure constantidentifier(f: stackpointer;
                               symbol: debugrecord;
                               var resultitem: dataitem);
      { Create a datapacket to carry the constant value to the target via 
        the datatoken "atomic". }

    var
      resultform: debugrecord;


    begin {constantidentifier}
      resultitem.mdl := f^.mdl;
      resultitem.frame := nil;
      resultitem.store := local;
      resultitem.index := symbol.consttype;
      d$getform(resultitem, resultform);
      adddatatoken(resultitem, atomic);
      with resultitem.dtok^ do
        begin
        packet := nil;
        case symbol.constform of
          ints, chars, bools, scalars:
            begin
            packet := getdatapacket;
            packet^.next := nil;
            packet^.typ := scalar;
            packet^.sc := symbol.i;
            packet^.len := resultitem.mdl^.intsize * bitsperint;
            end;
          reals, doubles:
            begin
            packet := getdatapacket;
            packet^.next := nil;
            if (symbol.constform = doubles) or (resultitem.mdl^.realsize = 8)
              then packet^.typ := doublereal
            else packet^.typ := singlereal;
            packet^.dr := symbol.r;
            end;
          ptrs:
            begin
            packet := getdatapacket;
            packet^.next := nil;
            packet^.typ := pointer;
            packet^.pt := nilpointervalue;
            end;
          {cannot write structured constants at the moment}
          otherwise experror(resultitem, nocanwrite);
          end;
        end;
      gettoken;
    end; {constantidentifier}




  procedure unaryexpression(var resultitem: dataitem; op: tokentype);

    var
      form: debugrecord;
      negate: boolean;
      i: symbolindex;

    begin  {unaryexpression}
      negate := token.typ = minus;
      gettoken;
      factor(resultitem);
      if not errorhappened then
        begin
        d$getbaseform(resultitem, form, i);
        case op of
          minus, plus:
            if (form.typ <> ints) and (form.typ <> reals) and 
              (form.typ <> doubles) then
                  experror(resultitem, badexpression)
	    else
	      if (hostopsys = msdos) and (form.typ = reals) or
		(form.typ = doubles) then 
		  experror(resultitem, norealexpr);
          nottok:
            if (form.typ <> bools) then
                  experror(resultitem, badexpression);
          end;	  

        if not errorhappened then
          begin
          if resultitem.store <> local then
            createreferencetoken(resultitem, false, false);
          case op of
            plus: {don't need to do anything};
            minus:
              begin
              adddatatoken(resultitem, operator);
              resultitem.dtok^.op := negateop;
              resultitem.dtok^.optyp := form.typ;
              if resultitem.mdl = nil then 
                resultitem.dtok^.fpcoprocessor := everfpcoprocessor
              else
                resultitem.dtok^.fpcoprocessor := resultitem.mdl^.fpcoprocessor;
              end;
            nottok:
              begin
              adddatatoken(resultitem, operator);
              resultitem.dtok^.op := notop;
              resultitem.dtok^.optyp := form.typ;
              if resultitem.mdl = nil then 
                resultitem.dtok^.fpcoprocessor := everfpcoprocessor
              else
                resultitem.dtok^.fpcoprocessor := resultitem.mdl^.fpcoprocessor;
              end;
            end;
          resultitem.store := local;
          end;
        end;
    end;  {unaryexpression}


  procedure derefexpression(var resultitem: dataitem);

    var
      form: debugrecord;
      i: symbolindex;

    begin  {derefexpression}
      gettoken;
      expression(resultitem);
      if not errorhappened then
        begin
        d$getbaseform(resultitem, form, i);
        if (i <> intindex) and (i <> nilindex) then 
          experror(resultitem, badexpression)
        else
          begin
          if resultitem.store <> local then
            createreferencetoken(resultitem, false, false);
          adddatatoken(resultitem, evaladdr);
          resultitem.bitlen := dfltintsize * bitsperunit;
          resultitem.index := intindex;
          resultitem.store := dat_space;
          end;
        end;
     end;  {derefexpression}




  begin {factor}
    case token.typ of
      lbrack: setconstructor(resultitem);
      stringconst: characterstring(resultitem);
      integerconst: integerconstant(resultitem);
      realconst: realconstant(resultitem);
      ident:
        begin
        if not locateident(token.identchars, f, index, predef) then
          experror(resultitem, illegalident)
        else
          begin
          if index = 0 then predefined(predef, resultitem)
          else
            begin
            d$getobject(f^.mdl, index, symbol);
            case symbol.namekind of
              constname, scalarname:
                constantidentifier(f, symbol, resultitem);
              funcname, varname, param, varparam, confparam, 
                varconfparam, boundid:
                variableaccess(f, symbol, resultitem);
              otherwise experror(resultitem, illegalident);
              end;
            end;
          end;
        end;
      lpar:
        begin
        gettoken;
        expression(resultitem);
        if not errorhappened then
          begin
          if not (token.typ in [rpar, comma, semicolon, colon]) then
            experror(resultitem, badexpression);
          if token.typ = rpar then gettoken;
          end;
        end;
      at:
        derefexpression(resultitem);
      minus, plus:
        unaryexpression(resultitem, token.typ);
      nottok:
        unaryexpression(resultitem, token.typ);
      otherwise experror(resultitem, badexpression);
      end;
  end; {factor}



procedure parseoperation(var resultitem: dataitem;
                          procedure expr(var item: dataitem));

  var
    form: debugrecord;
    rightitem: dataitem;
    operation: tokentype;
    i: symbolindex;

  begin  {parseoperation}
    operation := token.typ;
    gettoken;
    if resultitem.store <> local then
      createreferencetoken(resultitem, false, false);
    rightitem.datatokenlist := nil;
    rightitem.dtok := nil;
    expr(rightitem);
    if errorhappened then cleardatatokens(resultitem)
    else
      if not operatorcompatible(operation, resultitem, rightitem, form,
         i) then
        begin
        experror(resultitem, badexpression);
        cleardatatokens(rightitem);
        end
      else
        begin
	if hostopsys = msdos then 
	  if (i = realindex) or (i = doubleindex) then
	    experror(resultitem, norealexpr);
	if not errorhappened then
	  begin
          if ((i = realindex) or (i = doubleindex)) and 
              ((resultitem.index <> realindex) and
                (resultitem.index <> doubleindex)) then
            adddatatoken(resultitem, makereal);
          if rightitem.store <> local then
            createreferencetoken(rightitem, false, false);
          if ((i = realindex) or (i = doubleindex)) and 
              ((rightitem.index <> realindex) and
                (rightitem.index <> doubleindex)) then
            adddatatoken(rightitem, makereal);
          appendtokenlist(resultitem, rightitem);
          adddatatoken(resultitem, operator);
          with resultitem.dtok^ do
            begin
            optyp := form.typ;
            case operation of
              star: op := timesop;
              slash: op := intoop;
              divtok: op := divop;
              modtok: op := modop;
              andtok: op := andop;
              plus: op := addop;
              minus: op := subtractop;
              ortok: op := orop;
              equal: op := equalop;
              notequal: op := notequalop;
              lessthan: op := lessthanop;
              lessequal: op := lessequalop;
              greaterthan: op := greaterthanop;
              greaterequal: op := greaterequalop;
              intok: op := inop;
              end {case};
            if resultitem.mdl = nil then 
              resultitem.dtok^.fpcoprocessor := everfpcoprocessor
            else
              resultitem.dtok^.fpcoprocessor := resultitem.mdl^.fpcoprocessor;
            end {with resultitem.dtok^};
          resultitem.index := i;
          resultitem.bitlen := form.size * bitsperunit;
          resultitem.store := local;
          end {else};
        end {else};
  end;  {parseoperation} 



procedure bangterm(var resultitem: dataitem);
    {  Parse
          !T -> !T *|/|div|mod|and F
               | F
       With binary operators a dataitem is generated for the left and right
       operands, which are then checked for compatibility under the given
       operator, then the datatokenlist for the right is appended to the
       left, and finally the operator datatoken is added and the resuliting
       dataitem is modified to be of the appropriate type. }


  begin {bangterm}
    factor(resultitem);
    if errorhappened then cleardatatokens(resultitem)
    else while (token.typ in [star, slash, divtok, modtok, andtok]) do
      parseoperation(resultitem, factor);
  end; {bangterm}


procedure starterm(var resultitem: dataitem);
    {  Parse
          *T -> *T +|-|or !T
               | !T
       With binary operators a dataitem is generated for the left and right
       operands, which are then checked for compatibility under the given
       operator, then the datatokenlist for the right is appended to the
       left, and finally the operator datatoken is added and the resuliting
       dataitem is modified to be of the appropriate type. }

  var
    form: debugrecord;
    i: symbolindex;

  begin {starterm}
    bangterm(resultitem);
    if errorhappened then cleardatatokens(resultitem)
    else while (token.typ in [plus, minus, ortok]) do
      begin
      d$getbaseform(resultitem, form, i);
      if not (form.typ in [sets, ints, bools, reals, doubles]) then
        experror(resultitem, badexpression)
      else
        parseoperation(resultitem, bangterm);
     end;
  end; {starterm}


procedure expression {(var resultitem: dataitem)} ;

  { Parse
       E  -> E  >|<|>=|<=|<>|= *T
            | *T
    With binary operators a dataitem is generated for the left and right
    operands, which are then checked for compatibility under the given
    operator, then the datatokenlist for the right is appended to the
    left, and finally the operator datatoken is added and the resuliting
    dataitem is modified to be of the appropriate type. }


  begin {expression}
    starterm(resultitem);
    if errorhappened then cleardatatokens(resultitem)
    else while (token.typ in
            [equal, notequal, lessthan, lessequal, greaterthan, greaterequal,
            intok]) do
      begin
      parseoperation(resultitem, starterm);
      if not errorhappened then
        resultitem.index := boolindex;
      end;
  end; {expression}



{************************************************************************

     ------ dbwrite -- Write variable values

     The 'W' command writes the value of the specified variables.
     Several variables may be specified in one command if the names
     are separated by commas.  For example: W A,B,C,D.  Formatting
     control is available in the standard way.  For example,
     W I:-1 will print the value of the integer variable I in hex.
     The datatokenlist created by the expression evaluation is first
     sent to the target supervisor then d$wrtdata is called.

************************************************************************}


procedure dbwrite;

  label
    1;

  var
    item: dataitem;
    l, r: integer;
    err: imessage;
    success: boolean;
    packet: datapacketptr;
    location: commandlineindex;
    first: boolean;
    newline: boolean;


  procedure getfield(var i: integer);
   { Process a field specification }

    var
      negate: boolean;


    begin
      negate := false;
      if token.typ = colon then
        begin
        gettoken;
        if token.typ = minus then
          begin
          negate := true;
          gettoken;
          end;
        if token.typ <> integerconst then d$cmderror(numberexpected)
        else
          begin
          i := token.i;
          if negate then i := - i;
          gettoken;
          end;
        end;
    end; { GetField }


  begin { dbwrite }
    if currentcontext = nil then currentcontext := d$firstvisibleframe;
    first := true;
    repeat
      if not first then gettoken;
      first := false;
      item.datatokenlist := nil;
      item.dtok := nil;
      adddatatoken(item, commence);
      l := 1;
      r := 7;
      expression(item);
      if errorhappened then goto 1;
      adddatatoken(item, theend);
      packet := getdatapacket;
      packet^.next := nil;
      d$dataaccess(item.datatokenlist, packet, success, err, location);
      cleardatatokens(item);
      if not success then
        begin
        if commands <> nil then commands^.idx := location;
        d$cmderror(err);
        if err in [subscripttoohigh, subscripttoolow] then
          writeln(out, 'ord of index = ', packet^.sc);
        end
      else if err <> notanerror then d$iwarn(err);
      freedatapacket(packet);
      getfield(l);
      getfield(r);
      if errorhappened or interrupted then goto 1;
      d$wrtdata(item, l, r, newline);
      if errorhappened or interrupted then goto 1;
      if (token.typ = comma) and not newline then write(out, '  ')
      else writeln(out);
    until token.typ <> comma;
    if token.typ = rpar then gettoken;
  1:
  end; { dbwrite }



{************************************************************************

     ------ dbasn -- Assign a value to a variable

     A value is assigned to a variable.  The value may be a constant,
     or it may be another variable.  Assignment compatibility is
     checked before making the assignment.  The datatokenlists for the
     left and right operands are appended, then the "equals" operator
     is added before sending the list to the target supervisor.

************************************************************************}


procedure dbasn;

  label
    1;

  var
    leftitem, rightitem: dataitem;
    resultform: debugrecord;
    i: symbolindex;
    err: imessage;
    success: boolean;
    packet: datapacketptr;
    location: commandlineindex;


  procedure asnerror(msg: imessage);


    begin
      d$cmderror(msg);
      goto 1;
    end;


  begin { dbasn }
    if currentcontext = nil then currentcontext := d$firstvisibleframe;
    gettoken;
    leftitem.datatokenlist := nil;
    leftitem.dtok := nil;
    adddatatoken(leftitem, commence);
    factor(leftitem);
    if errorhappened then goto 1;
    if token.typ <> becomes then asnerror(illegalassign);
    createreferencetoken(leftitem, false, false);
    gettoken;
    rightitem.datatokenlist := nil;
    rightitem.dtok := nil;
    expression(rightitem);
    if errorhappened then goto 1;
    if not operatorcompatible(becomes, leftitem, rightitem, resultform, i) then
      asnerror(notcompatible);
    if rightitem.store <> local then
      createreferencetoken(rightitem, false, true);
    if ((leftitem.index = realindex) or (leftitem.index = doubleindex)) and 
      ((rightitem.index <> realindex) and (rightitem.index <> doubleindex)) then
      adddatatoken(rightitem, makereal);
    appendtokenlist(leftitem, rightitem);
    adddatatoken(leftitem, operator);
    leftitem.dtok^.op := equals;
    leftitem.dtok^.optyp := resultform.typ;
    adddatatoken(leftitem, theend);
    packet := getdatapacket;
    packet^.next :=nil;
    err := notanerror;
    d$dataaccess(leftitem.datatokenlist, packet, success, err, location);
    freedatapacket(packet);
    cleardatatokens(leftitem);
    if not success then
      begin
      if commands <> nil then commands^.idx := location;
      d$cmderror(err);
      if err in [subscripttoohigh, subscripttoolow] then
        writeln(out, 'ord of index = ', packet^.sc);
      end
    else if err <> notanerror then d$iwarn(err);
  1:
  end; { dbasn }


{***********************************************************************
***********************************************************************}

procedure dbproc;

  var
    resultitem: dataitem;
    err: imessage;
    packet: datapacketptr;
    location: commandlineindex;
    success: boolean;

  begin  {dbproc}
    if currentcontext = nil then currentcontext := d$firstvisibleframe;
    gettoken;
    resultitem.datatokenlist := nil;
    resultitem.dtok := nil;
    adddatatoken(resultitem, commence);
    factor(resultitem);
    err := notanerror;
    packet := getdatapacket;
    d$dataaccess(resultitem.datatokenlist, packet, success, err, location);
    freedatapacket(packet);
    cleardatatokens(resultitem);
    if not success then
      begin
      if commands <> nil then commands^.idx := location;
      d$cmderror(err);
      end
    else if err <> notanerror then d$iwarn(err);
  end;  {dbproc}




{************************************************************************

     ------ Breakpoints, Watched Variables, Macros

************************************************************************}


procedure clearbreak(addr: addressrec);
 { Remove a breakpoint by deleting its record in the breakpoints list
   and informing the target supervisor. }

  var
    b, bback: breakpointer;
    c, cback: commandpointer;


  begin
    b := breaklist;
    while (b <> nil) and (b^.addr.addr <> addr.addr) do
      begin
      bback := b;
      b := b^.next;
      end;
    if b = nil then d$cmderror(nosuchbreakpoint)
    else
      begin
      if b^.cmds <> nil then
        begin
        c := b^.cmds;
        while c <> nil do
          begin
          cback := c;
          c := c^.embedlink;
          d$deactivate(cback);
          dispose(cback);
          end;
        end;
      if b = breaklist then
        begin
        breaklist := breaklist^.next;
        dispose(b);
        end
      else
        begin
        bback^.next := b^.next;
        dispose(b);
        end;
      end;
    if not errorhappened then d$releasebreak(addr);
  end;


procedure dbbrk;

  { When the 'B' command is given with no other information, the
    breakpoint encountered at the current location is removed.
    Otherwise, the command has the forms: 
      B PROC,STMT 
      B PROC
      B STMT
    and this procedure sets a breakpoint at the location specified. 
    This requires a new record in the breakpoints list and informing 
    the target supervisor. }

  const
    ourname = 'dbbrk';
  var
    mdl: mdl_pointer;
    proc: proctreeptr;
    stmt: mapindex;
    stmtrec: stmtrecord;
    addr, entryaddr, exitaddr: addressrec;
    b, newb: breakpointer;
    breakaddr: addressrec;
    c: commandpointer;


  begin { dbbrk }
    if not framesupdated then d$frameupdate;
    if needcaching then proc := d$getproctreerec;
    if not errorhappened then
      begin
      if token.typ = semicolon then
        begin
        clearbreak(currentstackframe^.pc);
        if needcaching then d$freeproctreerec(proc);
        end
      else if getstmt(mdl, proc, stmt) then
        begin
        if not d$readmap(mdl, stmt, stmtrec) then choke(ourname);
        breakaddr := d$absaddr(mdl, stmtrec.pc);
        b := breaklist;
        while (b <> nil) and (b^.addr.addr <> breakaddr.addr) do
          b := b^.next;
        if b <> nil then
          begin
          if b^.cmds = nil then
            begin
            b^.cmds := getstoredcommands;
            if b^.cmds <> nil then
              begin
              b^.cmds^.embedlink := nil;
              b^.cmds^.next := nil;
              end;
            end
          else
            begin
            c := b^.cmds;
            while c^.embedlink <> nil do c := c^.embedlink;
            c^.embedlink := getstoredcommands;
            if c^.embedlink <> nil then
              begin
              c^.embedlink^.embedlink := nil;
              c^.embedlink^.next := nil;
              end;
            end;
          end
        else
          begin
          new(newb);
          newb^.addr := breakaddr;
          newb^.cmds := getstoredcommands;
          if newb^.cmds <> nil then
            begin
            newb^.cmds^.next := nil;
            newb^.cmds^.embedlink := nil;
            end;
          newb^.count := - 1;
          newb^.next := breaklist;
          newb^.mdl := mdl;
          newb^.proc := proc;
          newb^.stmt := stmt;
          breaklist := newb;
          entryaddr.addr := proc^.startpc;
          exitaddr.addr := proc^.endpc;
          d$setbreak(newb^.addr, entryaddr, exitaddr);
          end;
        end;
      end;
  end; { dbbrk }


procedure dbklr;

  { If no arguments are given, the 'K' command clears all breakpoints.
    If a command of the form: 
      K PROC,STMT 
      K PROC
      K STMT
    is given, then that specific breakpoint is cleared and the others 
    (if any) remain in effect. }

  const
    ourname = 'dbklr';
  var
    mdl: mdl_pointer;
    proc: proctreeptr;
    stmt: mapindex;
    stmtrec: stmtrecord;
    b, back: breakpointer;


  begin { dbklr }
    if token.typ = semicolon then
      begin
      b := breaklist;
      while b <> nil do
        begin
        back := b^.next;
        clearbreak(b^.addr);
        b := back;
        end;
      breaklist := nil;
      end
    else 
      begin
      if needcaching then proc := d$getproctreerec;
      if getstmt(mdl, proc, stmt) then
        begin
        if not d$readmap(mdl, stmt, stmtrec) then choke(ourname);
        clearbreak(d$absaddr(mdl, stmtrec.pc));
        end;
      if needcaching then if proc <> nil then d$freeproctreerec(proc);
      end;
  end; { dbklr }


procedure dbwtch;

  { When the 'W' command is given with no other information, it will
    clear all watched variables.  Otherwise, the command has the
    form: W variable and this procedure sets up to watch the variable
    specified.  The number of watched variables is limited to 256 and the
    set "watchidset" keeps tabs on which id numbers have already been
    allocated.  The target supervisor is sent a datatokenlist to describe
    the location being watched.
  }

  label
    1;

  var
    item: dataitem;
    startpos, stoppos: commandlineindex;
    w, wnext: watchpointer;
    err: imessage;
    success: boolean;
    packet: datapacketptr;
    i: 0..watchprobelimit;
    location: commandlineindex;


  procedure wtcherror(msg: imessage);


    begin
      d$cmderror(msg);
      goto 1;
    end;


  begin { dbwtch }
    if currentcontext = nil then currentcontext := d$firstvisibleframe;
    startpos := currchpos;
    gettoken;
    if token.typ = semicolon then
      begin
      w := watchlist;
      while w <> nil do
        begin
        if w^.cmds <> nil then 
          begin
          d$deactivate(w^.cmds);
          dispose(w^.cmds);
          end;
        wnext := w^.next;
        dispose(w);
        w := wnext;
        end;
      d$watchlistrelease;
      watchlist := nil;
      watching := false;
      watchidset := [];
      end
    else
      begin
      if token.typ = lpar then
        begin
        gettoken;
        startpos := startpos + 1;
        end;
      item.datatokenlist := nil;
      item.dtok := nil;
      adddatatoken(item, commence);
      factor(item);
      if item.store = local then 
        begin
        adddatatoken(item, evaladdr);
        item.store := dat_space;
        end;
      if errorhappened then goto 1;
      stoppos := currchpos;
      if token.typ = rpar then gettoken;
      i := 0;
      while (i <= watchprobelimit) and (i in watchidset) do i := i + 1;
      if (i > watchprobelimit) then wtcherror(toomanywatches)
      else watchidset := watchidset + [i];
      new(w);
      with w^ do
        begin
        if (item.store in [gen_reg, ptr_reg, real_reg, fpp_reg]) and
            (item.frame^.dynamiclevel < currentstackframe^.dynamiclevel) then
            {The register value may have been stored on the stack upon entry
             to a new procedure, which means that the stack location was given
             as the context address.  This token causes the context adddress to
             be the register.  The watch is ignored until that procedure is
             re-entered.}
          begin
          adddatatoken(item, watchreg);
          with item.dtok^ do
            begin
            regtype := item.store;
            regoffset := item.regoff;
            end;
          end;
        adddatatoken(item, watchtoken);
        with item.dtok^ do
          begin
          wlen := item.bitlen;
          ident := i;
          if item.frame = nil then level := 0
          else level := item.frame^.dynamiclevel;
          end;
        adddatatoken(item, theend);
        packet := getdatapacket;
        packet^.next := nil;
        d$dataaccess(item.datatokenlist, packet, success, err, location);
        cleardatatokens(item);
        if errorhappened then goto 1;
        if not success then
          begin
          commands^.idx := location;
          d$cmderror(err);
          freedatapacket(packet);
          goto 1;
          end
        else if err <> notanerror then d$iwarn(err);
        where := packet^;
        freedatapacket(packet);
        what := item;
        ident := i;
        if item.frame = nil then level := 0
        else level := item.frame^.dynamiclevel;
        store := item.store;
        regoff := item.regoff;
        name.len := 0;
        setchpos(startpos);
        while currchpos <> stoppos do
          begin
          if name.len < stringsize then
            begin
            name.len := name.len + 1;
            name.txt[name.len] := currch
            end;
          nextch;
          end;
        name.len := name.len - 1;
        if token.typ = rpar then gettoken;
        cmds := getstoredcommands;
        end;
      w^.next := watchlist;
      watchlist := w;
      watching := true;
      end;
  1:
    if errorhappened then cleardatatokens(item);
  end; { dbwtch }



{************************************************************************

     ------ dbdisp -- Display debugger information

     The 'D' command displays the following information:
         Breakpoints with any stored commands
         Macros
         Watched variables

************************************************************************}


procedure dbdisp;

  var
    b: breakpointer;
    w: watchpointer;
    m: macropointer;
    c: commandpointer;


  begin
    if breaklist <> nil then
      begin
      writeln(out);
      writeln(out, 'Breakpoints');
      writeln(out);
      b := breaklist;
      while b <> nil do
        with b^ do
          begin
          d$wrtlocation(mdl, proc, stmt, addr, false);
          c := cmds;
          while c <> nil do
            begin
            with c^ do writeln(out, '      <', com: len, '>');
            c := c^.embedlink;
            end;
          b := b^.next;
          end;
      end;

    if watchlist <> nil then
      begin
      writeln(out);
      writeln(out, 'Watched Variables');
      writeln(out);
      w := watchlist;
      while w <> nil do
        begin
        write(out, w^.ident: 4, ' ': 6);
        with w^.name do writeln(out, txt: len);
        if w^.cmds <> nil then
          with w^.cmds^ do writeln(out, '      <', com: len, '>');
        w := w^.next;
        end;
      end;

    if macrolist <> nil then
      begin
      writeln(out);
      writeln(out, 'Macros');
      writeln(out);
      m := macrolist;
      while m <> nil do
        begin
        if m^.cmds <> nil then
          begin
          d$wrtsymbolname(m^.name, 10);
          with m^.cmds^ do writeln(out, ' ', com: len);
          end;
        m := m^.next;
        end;
      end;
  end;




{************************************************************************

     DBCONT -- Continue program execution

     The 'C' command causes the user program to resume execution,
     unless the program has terminated.  If the user program stopped
     because it reached a breakpoint, an argument N may be specified
     with the 'C' command which will cause the program to continue
     without stopping at the current breakpoint until that breakpoint
     has been reached N times.  Each breakpoint has its own repetition
     count.

************************************************************************}


procedure dbcont;

  label
    1;

  var
    cnt: integer;
    addr: addressrec;
    b: breakpointer;


  begin { dbcont }
    if programdead then
      begin
      writeln(out, 'Program dead:  use g to restart');
      goto 1;
      end;
    cnt := 0;
    if token.typ <> semicolon then
      begin
      if token.typ = lpar then gettoken;
      if token.typ <> integerconst then d$cmderror(numberexpected);
      if token.i > 0 then cnt := token.i;
      gettoken;
      if token.typ = rpar then gettoken;
      end;

    if cnt <> 0 then
      begin
      if not framesupdated then d$frameupdate;
      if errorhappened then goto 1;
      b := breaklist;
      while (b <> nil) and (b^.addr.addr <> currentstackframe^.pc.addr) do
        b := b^.next;
      if b <> nil then b^.count := cnt
      else d$cmderror(nosuchbreakpoint)
      end;

    prompting := false;
    if firsttime then restarting := true;
  1:
  end; { dbcont }


procedure dbstep(inw: boolean {proceed} );

  label
    1;

  var
    f: stackpointer;


  procedure steperror(msg: imessage);


    begin
      d$cmderror(msg);
      goto 1;
    end;


  begin { dbstep }
    if programdead then
      begin
      writeln(out, 'Program dead:  use g to restart');
      goto 1;
      end;
    stepcount := 1;
    if token.typ = lpar then gettoken;
    if token.typ = integerconst then
      begin
      if token.i > 0 then stepcount := token.i;
      gettoken;
      if token.typ = rpar then gettoken;
      end;

    if not inw then proceeding := true
    else stepping := true;
    if firsttime then restarting := true;
    prompting := false;
  1:
  end; { dbstep }

{************************************************************************}
{                                                                        }
{    DBGO -- Restart user's program                                      }
{    DBQUIT -- Quit the debugging session                                }
{                                                                        }
{  For separate process debuggers anything between the G command and     }
{  the end of the command (eoln of semicolon) is considered to be        }
{  command line arguments for the target program, and is saved in        }
{  EXECARGS.  The old syntax that required arguments to be quoted and    }
{  separated by commas is still supported, which accounts for the        }
{  tortuous scan.                                                        }
{                                                                        }
{************************************************************************}


procedure dbgo;

  var
    done: boolean;
    parenpair: boolean;  {using the old syntax g(...), so skip closing paren}
    quoting: boolean;  {using the old syntax g '...','...', so skip quotes and
                        appropriate commas}
    inquote: boolean; {currently scanning in a quote pair}
    ch: char;
    idx: stringindex;


  begin { dbgo }

    if separateprocess then
      begin
      convertingcase := false;
      idx := 1;	
      ch := currch;
      while ch = ' ' do
        begin
        nextch;
        ch := currch;
        end;
      if ch = '(' then 
        begin
        parenpair := true;
        nextch;
        ch := currch;
        end
      else 
        begin
        parenpair := false;
        if ch = '''' then 
          begin
          quoting := true;
          inquote := true;
          nextch;
          ch := currch;
          end
        else
          begin
          quoting := false;
          inquote := false;
          end;
        end;
       
       done := (ch = eob) or (ch = eol) or (ch = ';'); 
       while not done do
         begin
         if quoting and ((ch = '''') or (not inquote and (ch = ','))) then 
           begin
           if (ch = '''') then inquote := not inquote;
           execargs.txt[idx] := ' ';
           end
         else if (ch = ')') and parenpair and not inquote then done := true
         else
           execargs.txt[idx] := ch;
         idx := idx + 1;
         nextch;
         ch := currch;
         done := (ch = eob) or (ch = eol) or (ch = ';'); 
         end;

       execargs.len := idx - 1;
       convertingcase := true;
       end;

    gettoken;

    if not (interrupted or errorhappened) then 
      begin
      prompting := false;
      restarting := true;
      if (tracing or history or watching) then firsttime := true;
      programdead := false;
      end;
  end; { dbgo }


procedure dbquit;


  begin {dbquit}
    quitting := true;
    prompting := false;
  end; {dbquit}



{************************************************************************

     ------ dbhelp -- Help message

     This routine is activated by a '?' command.  It prints a short
     help message on the user's terminal.

************************************************************************}


procedure dbhelp;


  begin { DbHelp }
    writeln(out);
    writeln(out, 'Pascal-2 Debugger command summary:');
    writeln(out);
    writeln(out, 'B -- Remove current breakpoint');
    writeln(out, 'B procedure,statement <commands> -- Set breakpoint');
    writeln(out, 'C -- Continue program execution');
    writeln(out, 'C n -- Continue n times through current breakpoint');
    writeln(out, 'D -- Display breakpoints, macros, and watched variables');
    writeln(out,
            'E n -- Enter context of stack frame n.  (Valid only for 1 line)')
            ;
    writeln(out, 'G -- Restart program at first statement');
    writeln(out, 'H TRUE or H FALSE -- Activate history mode');
    writeln(out,
            'H or H n -- Dump stack and display last n statements executed');
    writeln(out, 'K or K proc,stmt -- Remove all or specific breakpoints');
    writeln(out,
            'L proc, L proc,stmt,count, L stmt,count -- List source lines');
    writeln(out, 'M name <commands> -- Define macro');
    writeln(out, 'N or N n -- List variable names for stack frame n');
    writeln(out,
            'P or P n -- Single step n statements, not tracing procedures');
    writeln(out, 'Q -- Quit debugging and terminate program');
    writeln(out, 'S or S n -- Single step n statements');
    writeln(out, 'T TRUE or T FALSE -- Activate statement trace mode');
    writeln(out, 'V variable <commands> -- Watch value of a variable');
    writeln(out, 'W expression,expression,... -- Write value of variable');
    writeln(out, 'X name -- eXecute a macro');
    writeln(out, 'variable := expression -- assigns a value to a variable');
    writeln(out, 'DELETE will interrupt a running program');
    writeln(out,
           'Use semicolons (;) to separate multiple commands on the same line'
            );
  end; { dbhelp }



{************************************************************************

     ------ dbhist -- Print execution history and stack dump

     The 'H' command prints a list of the last 10 statements executed
     by the user program.  If a parameter N is given then that many
     statements in the execution history are printed.  The debugger
     retains history information about the last 50 statements executed,
     but the default is to print only the last 10 statements.  H(0)
     prints only the stack dump. H(-1) prints the stack dump with more
     complete information about addresses unknown to the debugger.  This
     last option is only available in the unix separate process
     environment.

************************************************************************}


procedure dbhist;

  label
    1;

  var
    item: dataitem;
    setupcnt, displaycnt: 0..maxhistory;
    setup, display, status, exists, complete: boolean;
    negate: boolean;


  procedure histerror(msg: imessage);


    begin
      d$cmderror(msg);
      goto 1;
    end;


  begin { dbhist }
    negate := false;
    display := false;
    displaycnt := 10;
    setup := false;
    complete := false;
    if token.typ = lpar then gettoken;
    if token.typ = minus then
      begin
      negate := true;
      gettoken;
      end;
    if token.typ = semicolon then display := true
    else if token.typ = integerconst then
      begin
      display := true;
      displaycnt := 0;
      if negate then token.i := - token.i;
      if token.i >= 0 then displaycnt := token.i;
      if token.i = - 1 then
        if separateprocess then complete := true
        else histerror(notoption);
      gettoken;
      end
    else if not booleanvalue(history) then histerror(booleanconstexpected);
    if token.typ = rpar then gettoken;

    if display then
      begin
      if (displaycnt > 0) then
        begin
        writeln(out);
        writeln(out, 'Program execution history:');
        writeln(out);
        d$wrthistory(displaycnt);
        end;
      writeln(out);
      writeln(out, 'Procedure execution stack');
      writeln(out);
      if currentcontext = nil then currentcontext := d$firstvisibleframe;
      d$wrtframes(complete, true, nil);
      writeln(out);
      end;
  1:
  end; { dbhist }



{************************************************************************

     ------ dbent -- Enter a new context level

     The 'E' command is used to examine data in stack frames which are
     not visible due to Pascal scope rules.  When the stack is dumped
     (via the 'H' command), the stack frames which are on the static
     chain are marked with stars ('*').  The variables in those frames
     are directly accessible.  The 'E' command must be used to access
     variables in the frames which are not marked with the start.  The
     format is E N where a positive N is the stack frame number to enter,
     and a negative N is a relative stack frame number from the current
     context.  Any commands following the 'E' command will be in the context of
     stack frame N.  Note that this command only has effect for one
     command line, after which the context reverts to the normal case
     (top of stack).  Therefore, using the 'E' command alone on a line
     does not make a whole lot of sense.

************************************************************************}


procedure dbent;

  var
    f: stackpointer;


  begin { dbent }
    if getframe(f) then currentcontext := f;
  end; { dbent }



{************************************************************************

     ------ dblist-- List portions of source program

************************************************************************}


procedure dblist;

  label
    1;
  const
    ourname = 'dblist';

  var
    f: stackpointer;
    mdl: mdl_pointer;
    proc: proctreeptr;
    stmt: mapindex;
    stmtno: integer;
    done: boolean;
    firstlineno, lastlineno, firstlinepos1, firstlinepos2: integer;
    linecount: integer;
    rec: stmtrecord;
    tempidentchars: symbolname;
    negate: boolean;


  procedure lsterror(msg: imessage);


    begin
      d$cmderror(msg);
      goto 1;
    end;


  begin { dblist }
    stmtno := 0;
    linecount := maxint;
    if needcaching then proc := d$getproctreerec;
    if currentcontext = nil then
      begin
      if d$locateaddr(currentpc, mdl, proc, stmt, false) then 
        begin
        if needcaching then proc^ := proctreefile^;
        end
      else currentcontext := d$firstvisibleframe;
      end;
    if currentcontext <> nil then
      begin
      mdl := currentcontext^.mdl;
      if needcaching then 
        begin
        if currentcontext^.proc <> nil then proc^ := currentcontext^.proc^
        else 
          begin
          d$freeproctreerec(proc);
          proc := nil;
          end;
        end
      else proc := currentcontext^.proc;
      end;

    if token.typ = semicolon then
      begin
      if (mdl = nil) or (proc = nil) then lsterror(procnotfound);
      end
    else
      begin
      if token.typ = lpar then gettoken;
      if token.typ = ident then
        begin
        tempidentchars := token.identchars;
        gettoken;
        if token.typ = colon then
          begin
          if not locatemodident(tempidentchars, mdl) then
            lsterror(nomodname);
          gettoken;
          if token.typ <> ident then lsterror(noprocname);
          tempidentchars := token.identchars;
          gettoken;
          end;
        if not locateprocident(tempidentchars, mdl, proc) then
          lsterror(procnotfound);
        if needcaching then proc^ := proctreefile^;
        if token.typ = comma then gettoken
        else if token.typ = integerconst then lsterror(commaexpected);
        end
      else if token.typ <> integerconst then lsterror(noprocname);

      if token.typ = integerconst then
        begin
        stmtno := token.i;
        linecount := 0;
        gettoken;
        if token.typ = comma then
          begin
          gettoken;
          if token.typ = minus then
            begin
            negate := true;
            gettoken;
            end
          else negate := false;
          if token.typ <> integerconst then lsterror(numberexpected);
          if negate then linecount := - token.i
          else linecount := token.i;
          gettoken;
          end;
        end;
      if token.typ = rpar then gettoken;
      end;
    if errorhappened then goto 1;

    if stmtno = 0 then
      begin
      if not d$stmtlocate(mdl, proc, 1, stmt) then lsterror(stmtnotfound);
      if not d$readmap(mdl, stmt, rec) then choke(ourname);
      firstlineno := rec.lineno;
      lastlineno := rec.lineno;
      firstlinepos1 := rec.filepos1;
      firstlinepos2 := rec.filepos2;
      done := false;
      while not done do
        begin
        stmt := stmt + 1;
        if not d$readmap(mdl, stmt, rec) then done := true
        else if rec.typ = plabrec then done := true
        else if rec.typ = stmntrec then
          if rec.lineno > lastlineno then lastlineno := rec.lineno;
        end;
      end
    else
      begin
      if not d$stmtlocate(mdl, proc, stmtno, stmt) then
        lsterror(stmtnotfound);
      if not d$readmap(mdl, stmt, rec) then choke(ourname);
      firstlineno := rec.lineno;
      firstlinepos1 := rec.filepos1;
      firstlinepos2 := rec.filepos2;
      if linecount > 0 then lastlineno := firstlineno + linecount - 1
      else if linecount < 0 then
        begin
        lastlineno := firstlineno;
        stmt := stmt + linecount + 1;
        if stmt <= 0 then stmt := 1;
        if firstlineno <= - linecount then firstlineno := 1
        else firstlineno := firstlineno + linecount + 1;
        if not d$readmap(mdl, stmt, rec) then choke(ourname);
        while rec.lineno < firstlineno do
          begin
          stmt := stmt + 1;
          if not d$readmap(mdl, stmt, rec) then choke(ourname);
          end;
        firstlineno := rec.lineno;
        firstlinepos1 := rec.filepos1;
        firstlinepos2 := rec.filepos2;

        end
      else {LineCount = 0}
        begin
        lastlineno := firstlineno;
        repeat
          stmt := stmt + 1;
          if not d$readmap(mdl, stmt, rec) then choke(ourname);
          if rec.typ = stmntrec then
            if rec.lineno > lastlineno then lastlineno := rec.lineno - 1;
        until rec.typ = stmntrec;
        end;
      end;

    if not errorhappened then
      d$wrtlist(mdl, firstlineno, lastlineno, firstlinepos1, firstlinepos2);
  1:
    if needcaching then d$freeproctreerec(proc);
    interrupted := false;
  end; { dblist }



{************************************************************************

     ------ Macros

************************************************************************}


procedure dbmac;

 { Define a macro (stored commands). }

  var
    back, m: macropointer;
    n: symbolname;
    found: boolean;


  begin { dbmac }
    if token.typ = lpar then gettoken;
    if token.typ <> ident then d$cmderror(macronameexpected)
    else
      begin
      n := token.identchars;
      gettoken;
      if token.typ = rpar then gettoken;
      back := nil;
      m := macrolist;
      found := false;
      while (m <> nil) and not found do
        begin
        found := m^.name = n;
        if not found then
          begin
          back := m;
          m := m^.next;
          end;
        end;
      if found then
        begin
        if m^.cmds <> nil then
          begin
          d$deactivate(m^.cmds);
          dispose(m^.cmds);
          m^.cmds := nil;
          end;
        if token.typ = lessthan then m^.cmds := getstoredcommands
        else
          begin
          if back = nil then macrolist := m^.next
          else back^.next := m^.next;
          dispose(m);
          m := nil;
          end
        end
      else if token.typ = lessthan then
        begin
        new(m);
        m^.next := macrolist;
        macrolist := m;
        m^.name := n;
        m^.cmds := getstoredcommands;
        end;
      end;
  end; { dbmac }


procedure dbexe;

 { Execute a macro. }

  var
    m: macropointer;
    n: symbolname;
    found: boolean;


  begin { dbexe }
    if token.typ = lpar then gettoken;
    if token.typ <> ident then d$cmderror(macronameexpected)
    else
      begin
      n := token.identchars;
      gettoken;
      if token.typ = rpar then gettoken;
      m := macrolist;
      found := false;
      while (m <> nil) and not found do
        begin
        found := m^.name = n;
        if not found then m := m^.next;
        end;
      if not found then d$cmderror(macronameexpected)
      else if m^.cmds <> nil then d$makeactive(m^.cmds);
      end;
  end; { dbexe }



{************************************************************************

     ------ dbname -- List variable names

     The 'N' command lists the names of variables and parameters in
     the current procedure.  If a parameter is given, then the names
     at that stack level are listed.  Use the 'H' command to find the
     stack levels.  For example, N 1 lists the names of the global
     level variables for the main module.  N 0 will list the names of the
     global level variables for the current context module.

************************************************************************}


procedure dbname;

  var
    f: stackpointer;
    max: symbolindex;
    pos: integer;
    rec: debugrecord;
    id, index, last: symbolindex;
    sym: symbolname;


  begin { dbname }
    if token.typ = semicolon then
      begin
      if currentcontext = nil then currentcontext := d$firstvisibleframe;
      f := currentcontext;
      end
    else if not getframe(f) then f := nil;

    if f <> nil then
      if (f^.proc <> nil) then
        begin
        d$getobject(f^.mdl, f^.proc^.firstsym + 1, rec);
        id := rec.id;
        index := f^.proc^.firstsym;
        last := f^.proc^.lastsym;
        pos := 1;
        while (index <= last + 1) and not interrupted do
          begin
          d$getobject(f^.mdl, index, rec);
          index := index + 1;
          if rec.kind = identdesc then sym := rec.identchars
          else if rec.kind = symboldesc then
            if rec.namekind in
               [varname, param, varparam, confparam, varconfparam,
               boundid] then
              if rec.name = id then
                begin
                max := msymbolname;
                while (sym[max] = ' ') and (max > 1) do max := max - 1;
                pos := pos + max + 1;
                if pos >= 78 then
                  begin
                  writeln(out);
                  pos := max + 1
                  end;
                d$wrtsymbolname(sym, max + 1);
                end;
          end;
        if pos > 1 then writeln(out);
        end;
    if interrupted then interrupted := false;
  end; { dbname }


{************************************************************************

     ------ dbtrce -- Trace control

     T TRUE turns on trace mode, while T FALSE turns it off.  In
     trace mode, each statement is identified with its source line
     before it is executed.

************************************************************************}


procedure dbtrce;

  label
    1;

  var
    item: dataitem;
    exists: boolean;
    settrace: boolean;


  procedure trceerror(msg: imessage);


    begin
      d$cmderror(msg);
      goto 1;
    end;


  begin { dbtrce }
    if token.typ = lpar then gettoken;
    if not booleanvalue(tracing) then trceerror(booleanconstexpected);
    if token.typ = rpar then gettoken;

  1:
  end; { dbtrce }


{************************************************************************

     ------ dbexec -- Spawn new task

************************************************************************}

  procedure dbexec;

    var
      comstr: stringdescr;
      i: stringindex;
      ch: char;

    begin
      convertingcase := false;
      i := 1;
      ch := currch;
      while (ch <> eob) and (ch <> eol) and (ch <> ';') do
	begin
	comstr.txt[i] := ch;
	i := i + 1;
        nextch;
	ch := currch;
	end;
      comstr.len := i - 1;
      d$spawn(comstr);
      convertingcase := true;
      gettoken;
    end;


procedure stripconditional;
  { Evalute the predicate for a conditional command then replace the current
    commands with those indicated by the value of the predicate.  Essentially
    the appropriate section of the currentcommands is trnsferred to the
    location "conditionalcommands", then "conditionalcoammands" is activated
    as the current commmand.  }

  var
    startpos, endpos, i: commandlineindex;
    predicateitem: dataitem;
    form: debugrecord;
    packet: datapacketptr;
    predicate: boolean;
    err: imessage;
    success: boolean;
    location: commandlineindex;


  begin {stripconditional}
    if currentcontext = nil then currentcontext := d$firstvisibleframe;
    gettoken;
    predicateitem.datatokenlist := nil;
    predicateitem.dtok := nil;
    expression(predicateitem);
    if token.typ <> thentok then
      begin
      d$cmderror(badexpression);
      errorhappened := true;
      end;
    if not errorhappened then
      if predicateitem.index <> boolindex then
        begin
        d$cmderror(badexpression);
        errorhappened := true;
        cleardatatokens(predicateitem);
        end
      else
        begin
        packet := getdatapacket;
        packet^.next := nil;
        d$getform(predicateitem, form);
        if not d$getvalue(predicateitem, scalar, lower(form), upper(form),
                          packet, err) then
          d$cmderror(err)
        else
          begin
          predicate := packet^.sc <> 0;
          with conditionalcommands^ do
            begin
            len := 0;
            idx := 0;
            next := nil;
            end;
          if predicate then
            begin
            startpos := currchpos + 1;
            repeat
              endpos := currchpos;
              gettoken;
            until (token.typ = elsetok) or (currch = eol);
            if currch = eol then endpos := commands^.len;
            for i := startpos to endpos do
              conditionalcommands^.com[i - startpos + 1] := commands^.com[i];
            conditionalcommands^.len := endpos - startpos + 1;
            conditionalcommands^.next := commands^.next;
            commands := conditionalcommands;
            end
          else
            begin
            repeat
              gettoken;
              startpos := currchpos + 1;
            until (token.typ = elsetok) or (currch = eol);
            if token.typ = elsetok then
              begin
              endpos := commands^.len;
              for i := startpos to endpos do
                conditionalcommands^.com[i - startpos +
                 1] := commands^.com[i];
              conditionalcommands^.len := endpos - startpos + 1;
              conditionalcommands^.next := commands^.next;
              commands := conditionalcommands;
              end;
            end;
          freedatapacket(packet);
          end;
        end;

  end; {stripconditional}



{************************************************************************

     ------ Interactive command processing

     Prompt the user and process debugger commands until the user
     requests that his (her?) program should resume execution via
     the 'C' or 'G' commands.  For debugger commands, only single
     letter commands are allowed, except in the case of variable
     assignment, which is done in the same way as in a user program.

************************************************************************}


procedure d$interactive;

  var
    startpos: commandlineindex;
    firstchar, secondchar: char;


  begin { d$interactive }
    prompting := true;
    while prompting do
      begin
      if interrupted then
        begin
        interrupted := false;
        d$discardcommands;
        end;
      errorhappened := false;
      startpos := currchpos;
      gettoken;
      if token.typ <> semicolon then
        begin
        if token.typ = endmark then
          begin
          write(out, prompt, ' ');
          break(out);
          if eof(inp) then d$finalend;
          if interrupted then
            begin
            readln(inp);
            write(out, prompt, ' ');
            break(out);
            if eof(inp) then d$finalend;
            end;
          getcommandline(inp, interactivecommands^);
          d$makeactive(interactivecommands);
          end
        else if token.typ = iftok then stripconditional
        else if (token.typ <> ident) then d$cmderror(notacommand)
        else {token.typ = ident}
          begin
          firstchar := token.identchars[1];
          secondchar := token.identchars[2];
          gettoken;
          if not errorhappened and
            not (token.typ in [uparrow, dot, becomes, lbrack]) and
             (secondchar = ' ') and
             (firstchar in
             ['b', 'c', 'd', 'e', 'g', 'h', 'k', 'l', 'm', 'n', 'p', 
              'q', 's', 't', 'v', 'w', 'x', '?', '!']) then
            begin
            case firstchar of
              'b': dbbrk;
              'c': dbcont;
              'd': dbdisp;
              'e': dbent;
              '!':
                begin
                setchpos(startpos);
                gettoken;
                dbexec;
                end;
              '?': dbhelp;
              'g': 
                begin
                setchpos(startpos);
                gettoken;
                dbgo;
                end;
              'h': dbhist;
              'k': dbklr;
              'l': dblist;
              'm': dbmac;
              'n': dbname;
              'p': dbstep(false);
              'q': dbquit;
              's': dbstep(true);
              't': dbtrce;
              'v':
                begin
                setchpos(startpos);
                gettoken;
                dbwtch;
                end;
              'w': dbwrite;
              'x': dbexe;
              end;
            end
          else
            begin
            if token.typ in [uparrow, dot, becomes, lbrack] then
              begin
              setchpos(startpos);
              dbasn;
              end
            else if token.typ = lpar then
              begin
              setchpos(startpos);
              dbproc;
              end
            else d$cmderror(notacommand);
            end;
          if token.typ <> semicolon then d$cmderror(commandnotterminated);
          end;
        end {<> semicolon} ;
      end {while} ;
    errorhappened := false;
  end; { d$interactive }
