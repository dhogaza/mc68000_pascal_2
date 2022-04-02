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

  Pascal-2 debugger interpreter initialization.

 Last modified by KRIS on 26-Nov-1990 13:47:08
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}


{*************************************************************************

     ------ Debugger Procedure Tree Initialization

  Each module has a binary tree of procedure descriptor records.  Each node
  of the tree is a record that maintains certain information about each
  procedure that is "visible" to the debugger.  This information includes
  the starting and ending pc's, index pointers into the symbol table
  and statement map and the lexical level of the procedure.  The tree is
  built on the basis of the start and end pc's so that fast binary searches
  can be made on the basis of a location in code.  The tree is constructed
  by first establishing an ordered linear list of records as derived from
  the symbol table and statement map (which thankfully is already 
  ordered).  Once the linear list is completed, it is converted into a
  balanced binary tree.

*************************************************************************}


procedure d$initproctree {(mdl: mdl_pointer)} ;

  { Establish a balanced binary tree of procedure records for each module.
    An ordered linear list is first procduced of all the procedures, then
    then function maketree in invoked to convert that list to a binary tree. }

  const
    ourname = 'initproctrees';
  var
    prochead, p, tmpp: proctreeptr;
    basep: proctreeptr;
    maprec: stmtrecord;
    symrec: debugrecord;
    done: boolean;
    cnt, treefilemax, i: integer;
    index: mapindex;
    tmpaddr: addressrec;


  function maketree(root: proctreeptr;
                    start, finish: integer): proctreeptr;

  { Convert a linear (ordered) list of procedure tree records into
    a balnced binary tree.  This routine does this by recursively
    "pulling" up the midpoint of the given list.}

    var
      mark, i: integer;
      p, markproc: proctreeptr;


    begin {maketree}
      if not needcaching then
        begin
        if start >= finish then maketree := root
        else
          begin
          mark := (start + finish - 1) div 2;
          p := root;
          markproc := root;
          for i := start to mark do
            begin
            markproc := p;
            p := p^.right.ptr;
            end;
          p^.left.ptr := root;
          markproc^.right.ptr := nil;
  
          p^.left.ptr := maketree(p^.left.ptr, start, mark);
          p^.right.ptr := maketree(p^.right.ptr, mark + 2, finish);
  
          maketree := p;
          end;
        end;
    end; {maketree}


  procedure maketreefile(root: proctreeptr;
                         start, finish, filepos: integer);

  { Convert a linear (ordered) list of procedure tree records into
    a balanced binary tree.  This routine does this by recursively
    "pulling" up the midpoint of the given list.  The tree is 
    "flattened" so that it can be stored in a linear file.
  }

    var
      mark, i: integer;
      p, markproc: proctreeptr;


    begin {maketreefile}
      if needcaching then
        begin
        if filepos > treefilemax then treefilemax := filepos;
        if start >= finish then
          begin
          seek(proctreefile, proctreefilemax + filepos);
          root^.left.index := 0;
          root^.right.index := 0;
          proctreefile^ := root^;
          put(proctreefile);
          if mdl^.base_frame^.proc <> root then dispose(root);
          end
        else
          begin
          mark := (start + finish - 1) div 2;
          p := root;
          markproc := root;
          for i := start to mark do
            begin
            markproc := p;
            p := p^.right.ptr;
            end;
          p^.left.ptr := root;
          markproc^.right.ptr := nil;
  
          if p^.left.ptr = nil then p^.left.index := 0
          else
            begin
            maketreefile(p^.left.ptr, start, mark, filepos * 2);
            p^.left.index := proctreefilemax + filepos * 2;
            end;
          if p^.right.ptr = nil then p^.right.index := 0
          else
            begin
            maketreefile(p^.right.ptr, mark + 2, finish, filepos * 2 + 1);
            p^.right.index := proctreefilemax + filepos * 2 + 1;
            end;

          seek(proctreefile, proctreefilemax + filepos);
          proctreefile^ := p^;
          put(proctreefile);
          if mdl^.base_frame^.proc <> p then dispose(p);
          end;
        end;
    end; {maketreefile}


  begin {d$initproctree}

    mdl^.proctreeinit := true;
    if mdl^.info > noinfo then
      begin
      new(mdl^.base_frame);
      mdl^.base_frame^.mdl := mdl;
      with mdl^.base_frame^ do
        begin
        dynamiclevel := 1;
        lexiclevel := 1;
        database := mdl^.data_base;
        staticlink := nil;
        dynamiclink := nil;
        proc := nil;
        end;
      if not mdl^.loaded then d$loadmdl(mdl);
      if not d$readmap(mdl, 1, maprec) then choke(ourname);
      if not d$readsym(mdl, maprec.recordnr, symrec) then
        choke(ourname);
      new(p);
      with p^, symrec do
        begin
        left.ptr := nil;
        right.ptr := nil;
        firstsym := firstname;
        lastsym := lastname;
        staticidx := name;
        procno := 1;
        tmpaddr := d$absaddr(mdl, maprec.pc);
        startpc := tmpaddr.addr;
        end;
      if symrec.level = 1 then mdl^.base_frame^.proc := p;

      prochead := p;
      cnt := 1;
      index := symrec.nextprocedure;
      done := false;

      while not done do
        begin

        if not d$readsym(mdl, index, symrec) then choke(ourname);
        if symrec.kind = symboldesc then
          begin
          if (symrec.namekind = procname) or (symrec.namekind = funcname) then
            begin
            if symrec.level = 0 then
              begin
              p^.lastsym := symrec.lastname;
              if mdl^.base_frame^.proc <> nil then
                mdl^.base_frame^.proc^.lastsym := symrec.lastname;
              p^.endpc := mdl^.code_end - 1;
              done := true;
              end {symrec.level = 0}
            else if symrec.firststmt <> 0 then
              begin
              if not d$readmap(mdl, symrec.firststmt - 1, maprec) then
                choke(ourname);
              tmpaddr := d$absaddr(mdl, maprec.pc);
              p^.endpc := tmpaddr.addr + 2;
              new(p^.right.ptr);
              p := p^.right.ptr;
              with p^, symrec do
                begin
                right.ptr := nil;
                left.ptr := nil;
                firstsym := firstname;
                lastsym := lastname;
                procno := symrec.firststmt;
                staticidx := symrec.name;
                if not d$readmap(mdl, procno, maprec) then
                  choke(ourname);
                tmpaddr := d$absaddr(mdl, maprec.pc);
                if separateprocess and (mdl = main_mdl) and
                   (symrec.level = 1) then
                  currentpc := tmpaddr;
                startpc := tmpaddr.addr;
                end;

                {Use the left ptr to temporarily establish the static link}
              tmpp := prochead;
              while tmpp <> nil do
                begin
                if (tmpp^.left.ptr = nil) and (tmpp^.staticidx = symrec.id) then
                  tmpp^.left.ptr := p;
                tmpp := tmpp^.right.ptr;
                end;

              cnt := cnt + 1;
              if symrec.level = 1 then mdl^.base_frame^.proc := p;
              end {symrec.firststmt <> 0}
            else if symrec.level = 1 then
              begin
              new(basep);
              with basep^, symrec do
                begin
                right.ptr := nil;
                left.ptr := nil;
                firstsym := firstname;
                lastsym := lastname;
                procno := 0;
                staticidx := 0;  {a nil static link}
                basep^.staticidx := 0;
                startpc := 0;
                endpc := 0;
                mdl^.base_frame^.proc := basep;
                end;
              end {symrec.level = 1};
            index := symrec.nextprocedure;
            end {procname or funcname}
          else index := index + 1
          end {symboldesc}
        else index := index + 1;
        end {while};

        {convert the static links from left ptr to an index}
      tmpp := prochead;
      while tmpp <> nil do 
        begin
          if tmpp^.left.ptr = nil then tmpp^.staticidx := 0
          else 
            begin
            tmpp^.staticidx := tmpp^.left.ptr^.firstsym;
            tmpp^.left.ptr := nil;
            end;
          tmpp := tmpp^.right.ptr;
        end;

      if needcaching then
        begin
        treefilemax := 2;
        while treefilemax <= cnt do treefilemax := treefilemax * 2;
        seek(proctreefile, proctreefilemax + 1);
        mdl^.proctreefileoff := proctreefilemax;
          {Initially pad the file out to the desired size so we can
           seek to the appropriate slots when constructing the "flat"
           binary tree.}
        for i := 1 to treefilemax do
          begin
          put(proctreefile);
          end;
        treefilemax := 0;
        maketreefile(prochead, 1, cnt, 1);
        proctreefilemax := proctreefilemax + treefilemax;
        end
      else mdl^.proctree := maketree(prochead, 1, cnt);
      end;

    if mdl^.base_frame <> nil then
      if mdl^.base_frame^.proc = nil then choke(ourname);

  end; {d$initproctree}



{*************************************************************************

     ------ Debugger Stack Frame Initialization

*************************************************************************}


procedure defaultframes;

  { Initialize the frame GlobalFrame which will be the default frame used
    initially and to determine the base stack frame. }

  var
    proc: proctreeptr;
    stmt: mapindex;
    mdl: mdl_pointer;
    maprec: stmtrecord;


  begin { defaultframes }

    new(globalstackframe);
    globalstackframe^.mdl := main_mdl;
    globalstackframe^.pc := nilpointervalue;
    globalstackframe^.proc := nil;
    if (main_mdl <> nil) then
      if (main_mdl^.base_frame <> nil) then
        if (main_mdl^.base_frame^.proc <> nil) then
      begin
      globalstackframe^.proc := main_mdl^.base_frame^.proc;
      if d$readmap(main_mdl, globalstackframe^.proc^.procno + 2, maprec) then
        if maprec.typ = stmntrec then
          globalstackframe^.nextstmtpc := d$absaddr(main_mdl, maprec.pc)
        else globalstackframe^.nextstmtpc := nilpointervalue;
      end;
    globalstackframe^.dynamiclink := nil;
    globalstackframe^.dynamiclevel := 1;
    globalstackframe^.staticlink := nil;
    globalstackframe^.lexiclevel := 1;
    globalstackframe^.uplink := nil;
    if main_mdl <> nil then globalstackframe^.database := main_mdl^.data_base
    else globalstackframe^.database := 0;
    currentstackframe := globalstackframe;
    if separateprocess then currentstackframe^.pc := currentpc;
    framesupdated := true;
  end; {defaultframes}



{*************************************************************************

     ------ Interpreter Initialization

*************************************************************************}


procedure initvars;

 { Initialize interpreter global variables. }

  var
    curr: formcachepointer;
    form: debugrecord;
    i: integer;


  begin {initvars}

    first_mdl_loaded := false;

    {initialize the step record}

    with steprec do
      begin
      procentrypc := 0;
      procexitpc := 0;
      stmtentrypc := 0;
      stmtexitpc := 0;
      segment := 0;
      prevaddr := 0;
      exitstep := false;
      proc := nil;
      stmt := 0;
      mdl := nil;
      end;

    { Initialize to control number of files opened at once. }

    num_mdls_open := 0;
    max_mdls_open := (maxfiles - ord(needcaching)) div 3 - 1 {sym+map+list} ;

    { Initialize Symbol Table Access Functions. }

    formcache := nil;
    for i := 1 to formcachesize do
      begin
      new(curr);
      with curr^ do
        begin
        next := formcache;
        mdl := nil;
        index := 0
        end;
      formcache := curr;
      end;
    lasttmpindex := maxint;

    with form do
      begin
      kind := formdesc;
      packedflag := false;
      bitaddress := false;
      size := dfltintsize;
      typ := ints
      end;
    d$getobject(nil, intindex, form);
    with form do
      begin
      size := singlerealsize;
      typ := reals
      end;
    d$getobject(nil, realindex, form);
    with form do
      begin
      size := doublerealsize;
      typ := doubles;
      end;
    d$getobject(nil, doubleindex, form);
    with form do
      begin
      size := charsize;
      typ := chars
      end;
    d$getobject(nil, charindex, form);
    with form do
      begin
      size := boolsize;
      typ := bools
      end;
    d$getobject(nil, boolindex, form);

    { Initialize Statement History List. }

    historytag := 1;
    historyidx := 1;
    historycount := 0;

    { Initialize the datatoken list. }

    datatokenpool := nil;
    datapacketfreelist := nil;
    proctreerecpool := nil;

    { Initialize command input. }

    eol := chr(10);
    eob := chr(0);
    prompt := '}';
    commands := nil;
    new(interactivecommands);
    new(conditionalcommands);

    { Initialize Breakpoints, Watched Variables, Macros. }

    macrolist := nil;
    breaklist := nil;
    watchlist := nil;
    watchfound := []; {watch ident numbers for vars which have been modified}
    watchidset := []; {watch ident numbers already in use}

    { Initialize the reserved identifiers and prdefined identifiers. }

    predefname[ploophole] := 'loophole                        ';
    predefname[pchr] := 'chr                             ';
    predefname[pord] := 'ord                             ';
    predefname[pref] := 'ref                             ';
    predefname[psigblock] := 'sigblock                        ';
    predefname[psigmask] := 'sigmask                         ';

    resvname[divtok] := 'div                             ';
    resvname[modtok] := 'mod                             ';
    resvname[nottok] := 'not                             ';
    resvname[andtok] := 'and                             ';
    resvname[ortok] := 'or                              ';
    resvname[intok] := 'in                              ';
    resvname[iftok] := 'if                              ';
    resvname[thentok] := 'then                            ';
    resvname[elsetok] := 'else                            ';

    { Initialize the execution condition flags. }

    interrupted := false;

    stepping := false;
    watching := false;
    tracing := false;
    history := false;
    quitting := false;

    firsttime := true;
    paused := true;

    if needcaching then 
      begin
      case hostopsys of
        vms:
          rewrite(proctreefile, 'proctree', '/temp/seek');
        unix, vdos, msdos:
          rewrite(proctreefile);
        end;
      proctreefilemax := 0;
      end;

  end; {initvars}


procedure d$initinterp;

 { Initialize the interpreter. }


  begin {initinterp}
    initvars;
    if main_mdl <> nil then d$initproctree(main_mdl);
    if embeddedjump then
      if (main_mdl = nil) or (main_mdl^.info = noinfo) then
        begin
        if main_mdl = nil then d$imesg(nomaindebug)
        else if main_mdl^.info = noinfo then d$imesg(nomainsymbols);
        d$finalend;
        end;
    defaultframes;
  end; {initinterp}
