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

  Pascal-2 debugger common symbol table and statement map access routines.

 Last modified by KRIS on 26-Nov-1990 13:47:19
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

{************************************************************************

     ------ Utility Routines

************************************************************************}


function d$max {(i, j: integer): integer} ;
 { Return the maximum value of two integers }

  begin
    if i > j then d$max := i
    else d$max := j
  end;


function d$min {(i, j: integer): integer} ;

  begin
    if i < j then d$min := i
    else d$min := j;
  end;


function lowercase(ch: char): char;

  begin
    if ch in ['A'..'Z'] then lowercase := chr(ord(ch) + 32)
    else lowercase := ch;
  end;


function d$addrinrange {(addr: addressrec; 
			 startrange, endrange: addressrange;
			 rangesegment: unsignedword): boolean} ;

  begin
    d$addrinrange := (startrange <= addr.addr) and (endrange > addr.addr) and
		     (not segmented or (rangesegment = addr.segment));
  end;


{************************************************************************

     ------ Symbol Table File, Statement Map File Access

************************************************************************}


function d$readmap
 {(mdl: mdl_pointer; index: mapindex; var rec: stmtrecord): boolean} ;
 { Return the map record for the given module and index. }


  begin
    if not mdl^.loaded then d$loadmdl(mdl);
    if (index <= 0) then d$readmap := false
    else
      begin
      seek(mdl^.mapfile, index);
      if eof(mdl^.mapfile) then d$readmap := false
      else
        begin
        rec := mdl^.mapfile^;
        d$readmap := true;
        end;
      end;
  end;


function d$readsym
 {(mdl: mdl_pointer; index: symbolindex; var rec: debugrecord): boolean} ;
 { Return the symbol record for the given module and index. }


  begin
    if not mdl^.loaded then d$loadmdl(mdl);
    if (index <= 0) then d$readsym := false
    else
      begin
      seek(mdl^.symfile, index);
      if eof(mdl^.symfile) then d$readsym := false
      else
        begin
        rec := mdl^.symfile^;
        d$readsym := true;
        end;
      end;
  end;

{*************************************************************************

     ------ Symbol Table Access

     The form cache is a list of form definitions (DebugRecords of type
     FormItem).  It contains records from several sources:
     - Permanent definitions of forms for the standard types Chars, Bools,
       Ints, and Reals; these are used to assign types to simple constants
       not associated with any particular module.  They are identified by a
       module value of NIL, and are never removed from the form cache.
     - Temporary definitions of forms for types created by the debugger;
       these are used for data items such as string and set constants.
       They are identified by having an index greater than LastTmpIndex,
       and may be reused whenever processing of a command is completed.
     - Ordinary definitions of forms taken from Symbol Table files.
       They may be reused at any time, since they can be recovered from
       the Symbol Table files.
     - Unused definitions.  They are identified by having an index of zero,
       and may be reused at any time.
    Records in the form cache are arranged in LRU order.

*************************************************************************}


procedure d$getobject
 {(mdl: mdl_pointer; index: symbolindex; var rec: debugrecord)} ;

  { Get a symbol table entry.
    The form cache is first scanned for the entry; if the entry is
    not in the cache, it is retrieved from the Symbol Table file,
    and, if it is a FormItem, it is placed in the form cache. }

  const
    ourname = 'getobject';
  var
    curr, prev, mark: formcachepointer;
    found: boolean;


  begin { d$getobject }
    curr := formcache;
    prev := nil;
    mark := nil;
    found := false;
    while (curr <> nil) and not found do
      begin
      if (mdl = curr^.mdl) and (index = curr^.index) then found := true
      else
        begin
        if (curr^.index = 0) or ((curr^.index < lasttmpindex) and
           (curr^.mdl <> nil)) then
          mark := prev;
        prev := curr;
        curr := curr^.next;
        end;
      end;

    if found then rec := curr^.rec
    else
      begin
      if (mdl <> nil) and (index < lasttmpindex) then
        if not d$readsym(mdl, index, rec) then choke(ourname);
      if rec.kind = formdesc then
        begin
        prev := mark;
        if prev = nil then choke(ourname);
        curr := prev^.next;
        curr^.mdl := mdl;
        curr^.index := index;
        curr^.rec := rec;
        end;
      end;

    if curr <> nil then
      begin
      if prev <> nil then
        begin
        prev^.next := curr^.next;
        curr^.next := formcache;
        formcache := curr;
        end;
      end;
  end; { d$getobject }


procedure d$tmpobject
 {(mdl: mdl_pointer; var index: symbolindex; var rec: debugrecord)} ;
 { Create a temporary object. }


  begin
    lasttmpindex := lasttmpindex - 1;
    index := lasttmpindex;
    d$getobject(mdl, index, rec);
  end;


procedure clrobject;
  { Clear all temporary objects out of form cache;
    a temporary object is removed by setting its index to zero, then
    moving it to the end of the cache. }

  var
    temps, curr, prev, temp: formcachepointer;


  begin { clrobject }
    curr := formcache;
    prev := nil;
    temps := nil;
    while curr <> nil do
      begin
      temp := curr;
      curr := curr^.next;
      if temp^.index < lasttmpindex then prev := temp
      else
        begin
        if prev = nil then formcache := curr
        else prev^.next := curr;
        temp^.index := 0;
        temp^.next := temps;
        temps := temp;
        end;
      end;
    if prev = nil then formcache := temps
    else prev^.next := temps;
    lasttmpindex := maxint;
  end; { clrobject }


procedure d$getform {(var item: dataitem; var form: debugrecord)} ;

 { Get the form corresponding to a DataItem. }


  begin {d$getform}
    d$getobject(item.mdl, item.index, form);
  end; {d$getform}


procedure d$getbaseform
 {(var item: dataitem; var form: debugrecord; var index: symbolindex)} ;


  begin {d$getbaseform}
    d$getform(item, form);
    index := item.index;
    if form.typ = subranges then
      begin
      index := form.parenttype;
      d$getobject(item.mdl, index, form)
      end;
  end; {d$getbaseform}


function d$signed(item: dataitem): boolean;

  var
    form: debugrecord;


  begin {d$signed}
    d$getform(item, form);
    d$signed := (form.typ = ints) or ((form.typ = subranges) and
                (form.lowerord < 0));
  end; {d$signed}


function d$extended(item: dataitem): boolean;

  var
    form: debugrecord;

  begin {d$extended}
    d$getform(item, form);
    d$extended := ((form.typ = subranges) and form.extended) or
      (form.typ = ptrs);
  end; {d$extended}


function d$isstring {(mdl: mdl_pointer; var form: debugrecord): boolean} ;
 { Determine if a form is a character string. }

  var
    tempform: debugrecord;


  begin { d$isstring }
    d$isstring := false;
    if form.packedflag and ((form.typ = arrays) or
       (form.typ = conformantarrays)) then
      begin
      d$getobject(mdl, form.indextype, tempform);
      if (form.typ = arrays) then
        begin
        if tempform.typ = subranges then
          begin
          d$getobject(mdl, tempform.parenttype, tempform);
          if tempform.typ = ints then
            begin
            d$getobject(mdl, form.elementtype, tempform);
            if tempform.typ = chars then d$isstring := true;
            end;
          end; {typ = subranges}
        end {typ = arrays}
      else
        begin {typ = conformantarrays}
        if tempform.typ = ints then
          begin
          d$getobject(mdl, form.elementtype, tempform);
          if tempform.typ = chars then d$isstring := true;
          end;
        end;
      end;
  end; { d$isstring }

function sizeof(var form: debugrecord): integer;
 { Determine the size of an object, based on the type of the object. }


  begin { sizeof }
    if not form.bitaddress then sizeof := form.size
    else sizeof := (form.size + bitsperunit - 1) div bitsperunit;
  end; { sizeof }


function lower(var form: debugrecord): integer;
 { Return the lowest ord of an object. }


  begin {lower}
    with form do
      if typ = ints then lower := - maxint
      else if typ = subranges then lower := lowerord
      else lower := 0;
  end; {lower}


function upper(var form: debugrecord): integer;
 { Return the highest ord of an object. }


  begin {upper}
    with form do
      case typ of
        ints: upper := targetmaxint;
        bools: upper := 1;
        chars: upper := 255;
        none: upper := 255;
        scalars: upper := lastord;
        subranges: upper := upperord;
        ptrs: upper := targetmaxaddr;
        otherwise upper := maxint
        end;
  end; {upper}


function d$upperbound {(mdl: mdl_pointer; f: stackpointer; var form:
                       debugrecord): integer} ;

  const
    upperboundoffset = 4;
    ourname = 'upperbound';

  var
    indexform: debugrecord;


  begin {d$upperbound}
    if form.typ = arrays then
      begin
      d$getobject(mdl, form.indextype, indexform);
      d$upperbound := upper(indexform);
      end
    else if form.typ = conformantarrays then
      d$upperbound := d$bound(mdl, f, form, upperboundoffset)
    else if form.typ = strings then 
      d$upperbound := sizeof(form) - 1
    else choke(ourname);
  end; {d$upperbound}


function d$lowerbound {(mdl: mdl_pointer; var form: debugrecord): integer} ;

  const
    lowerboundoffset = 2;
    ourname = 'lowerbound';

  var
    indexform: debugrecord;


  begin {d$lowerbound}
    if form.typ = arrays then
      begin
      d$getobject(mdl, form.indextype, indexform);
      d$lowerbound := lower(indexform);
      end
    else if form.typ = conformantarrays then
      d$lowerbound := d$bound(mdl, f, form, lowerboundoffset)
    else if form.typ = strings then d$lowerbound := 0
    else choke(ourname);
  end; {d$lowerbound}




{************************************************************************

     ------ Symbol Searching

************************************************************************}


function searchcontext(var nam: symbolname;
                       id: integer;
                       level: integer;
                       mdl: mdl_pointer;
                       first, last: symbolindex;
                       var index: symbolindex): boolean;

  { Search for a symbol with a particular name and id; the search scans
    a segment of a particular module's Symbol Table file.  The search
    starts from the last index and proceeds up a record at a time
    comparing the first character of the name, NAM, and the particular 
    ident name.  When that first character is matched then the offset
    "chainoffset" can be used to skip up the symbol table comparing
    names. }

  const
    ourname = 'searchcontext';
  var
    rec: debugrecord;
    found: boolean;
    initvalid: boolean;
    firstchar: char;
    chidx: char;
    chain: symbolindex;
    name: symbolname;
    i: isymbolname;

  function debughash(ch: char):debughashindex;

  { return hash value for debugger hash table.
  }

  begin {debughash}
    if ch = '$' then debughash := 26
    else if ch = '_' then debughash := 27
    else debughash := ord(lowercase(ch)) - ord('a');
  end   {debughash};


  begin
    initvalid := true;
    if mdl^.casesensitive then name := nam
    else for i := 1 to msymbolname do
      name[i] := lowercase(nam[i]);
    if (level = 1) and (mdl^.firstoccur[debughash(name[1])] <> 0) then
      begin
      firstchar := name[1];
      index := mdl^.firstoccur[debughash(firstchar)]
      end
    else index := last;
    found := false;
    while (index >= first) and not found do
      begin
      if not d$readsym(mdl, index, rec) then choke(ourname);
      if rec.kind = identdesc then 
        begin
        firstchar := rec.identchars[1];
        if initvalid and (level = 1) and 
            (mdl^.firstoccur[debughash(firstchar)] = 0) then
          mdl^.firstoccur[debughash(firstchar)] := index;
        if firstchar = name[1] then 
          begin
          chain := rec.chainoffset;
          initvalid := false;
          end
        else chain := index - 2;
        if rec.identchars = name then 
          begin
          if not d$readsym(mdl, index + 1, rec) then 
            choke(ourname);
          if (rec.kind = symboldesc) then
            if ((rec.name = id) or (rec.name < 2)) or 
               (rec.namekind = funcname) then
              found := true;
          end;
        if found then index := index + 1
        else index := chain;
        end
      else if rec.kind = symboldesc then index := index - 1
      else index := index - 2;
      end;
    searchcontext := found;
  end;





function locateident(var nam: symbolname;
                     var f: stackpointer;
                     var index: symbolindex;
                     var predefidx: predeftypes): boolean;

  { Search for an identifier (NAM).  The search starts at the current
    context level defined by CurrentContext and follows normal Pascal
    scope rules as the search proceeds through the static link of the
    frames. }

  var
    p: stackpointer;
    rec: debugrecord;
    found: boolean;


  begin { locateident }
    found := false;
    p := currentcontext;
    while p <> nil do
      begin
      if p^.proc <> nil then
        begin
        d$getobject(p^.mdl, p^.proc^.firstsym + 1, rec);
        if searchcontext(nam, rec.id, rec.level, p^.mdl, p^.proc^.firstsym,
                         p^.proc^.lastsym, index) then
          begin
          found := true;
          f := p;
          p := nil;
          end;
        end;
      if p <> nil then p := p^.staticlink;
      end;
    if not found then
      begin
      index := 0;
      predefidx := succ(firstpredef);
      while not found and (predefidx <> pnone) do
        if nam = predefname[predefidx] then found := true
        else predefidx := succ(predefidx);
      end;
    locateident := found;
  end; { locateident }


function locatefieldident(var nam: symbolname;
                          id: integer;
                          lev: integer;
			  f: stackpointer;
                          firstfield, lastfield: targetint;
                          var index: symbolindex): boolean;

  { Search for a field identifier (NAM) defined at a particular static nesting
    level (LEV) with a particular id (ID), within the symbol table context
    of a particular frame (F). }

  begin { locatefieldident }
    locatefieldident := searchcontext(nam, id, lev, f^.mdl,
                                 firstfield, lastfield, index);
  end; { locatefieldident }


function locateprocident(var nam: symbolname;
                         var mdl: mdl_pointer;
                         var proc: proctreeptr): boolean;

 { Search for a procedure identifier (NAM). If NEEDCACHING then the file
   pointer in PROCTREEFILE is left at the appropriate procedure record, 
   otherwise a pointer to the record in the procedure record tree is
   returned via PROC.
 }

  var
    found: boolean;
    name: symbolname;
    i: isymbolname;


  function searchtree(p: proctreeptr): proctreeptr;

    const
      ourname = 'searchtree';
    var
      rec: debugrecord;
      procptr: proctreeptr;


    begin {searchtree}
      if not needcaching then
        begin
        procptr := nil;
        if not d$readsym(mdl, p^.firstsym, rec) then choke(ourname);
        if rec.identchars = name then procptr := p
        else
          begin
          if p^.left.ptr <> nil then procptr := searchtree(p^.left.ptr);
          if (procptr = nil) and (p^.right.ptr <> nil) then
            procptr := searchtree(p^.right.ptr);
          end;
        searchtree := procptr;
        end;
    end; {searchtree}


  function searchtreefile(filepos: integer): boolean;

    const
      ourname = 'searchtreefile';
    var
      rec: debugrecord;
      found: boolean;
      rightidx: integer;

    begin {searchtreefile}
      if needcaching then
        begin
        found := false;
        seek(proctreefile, filepos);
        if not d$readsym(mdl, proctreefile^.firstsym, rec) then 
          choke(ourname);
        if rec.identchars = name then 
          found := true
        else
          begin
            {save right index since the file pointer will be moved}
          rightidx := proctreefile^.right.index;
          if proctreefile^.left.index <> 0 then 
            found := searchtreefile(proctreefile^.left.index);
          if not found and (rightidx <> 0) then
            found := searchtreefile(rightidx);
          end;
        searchtreefile := found;
        end;
    end; {searchtreefile}


  begin { locateprocident }
    found := false;
    if mdl <> nil then
      begin
      if mdl^.casesensitive then name := nam
      else for i := 1 to msymbolname do
        name[i] := lowercase(nam[i]);
      if not mdl^.loaded then d$loadmdl(mdl);
      if (mdl^.info > noinfo) then
        begin
        if needcaching then 
          found := searchtreefile(mdl^.proctreefileoff + 1)
        else 
          begin
          if mdl^.proctree <> nil then 
            proc := searchtree(mdl^.proctree)
          else proc := nil;
          if proc <> nil then found := true;
          end;
        end;
      end; {mdl <> nil}
    locateprocident := found;
  end; { locateprocident }


function locatemodident(nam: symbolname;
                        var mdl: mdl_pointer): boolean;
 { Search modules list for named module }

  var
    found: boolean;
    i: isymbolname;
    name: symbolname;


  begin { locatemodident }
    for i := 1 to msymbolname do
      name[i] := lowercase(nam[i]);
    mdl := mdl_list;
    found := false;
    while (mdl <> nil) and not found do
      begin
      found := true;
      i := 1;
      while (i <= msymbolname) and ((mdl^.nam[i] <> ' ') or
            (name[i] <> ' ')) and found do
        if mdl^.nam[i] in ['A'..'Z'] then
          if chr(ord(mdl^.nam[i]) + 32) <> name[i] then
            found := false
          else i := i + 1
        else if mdl^.nam[i] <> name[i] then found := false
        else i := i + 1;
      if not found then mdl := mdl^.next;
      end;
    locatemodident := found;
  end; { locatemodident }



{************************************************************************

     ------ Type Checking

************************************************************************}


function identical(mdl1: mdl_pointer;
                   index1: symbolindex;
                   mdl2: mdl_pointer;
                   index2: symbolindex): boolean;
 { Determine if two types are identical. }
  { There are three ways for types to be identical:
     - they have identical module and index values,
     - they have identical index values less than NoneIndex
       (they are the same required type in different modules),
     - at least one has index equal to NoneIndex
       (presumably an error has already been detected). }


  begin
    identical := ((index1 = index2) and ((mdl1 = mdl2) or
                 (index1 < noneindex))) or (index1 = noneindex) or
                 (index2 = noneindex);
  end;


function compatible(mdl1: mdl_pointer;
                    index1: symbolindex;
                    mdl2: mdl_pointer;
                    index2: symbolindex): boolean;
 { Determine if two types are compatible. }

  const
    ourname = 'compatible';
  var
    form1, form2: debugrecord;
    compat: boolean;


  procedure getbaseform(mdl: mdl_pointer;
                        var index: symbolindex;
                        var form: debugrecord);


    begin
      d$getobject(mdl, index, form);
      if form.typ = subranges then
        begin
        index := form.parenttype;
        d$getobject(mdl, index, form)
        end;
    end;


  begin { compatible }
    getbaseform(mdl1, index1, form1);
    getbaseform(mdl2, index2, form2);
    if identical(mdl1, index1, mdl2, index2) then compat := true
    else
      begin
      compat := false;
      if form1.typ = form2.typ then
        case form1.typ of
          arrays:
            begin
            if d$isstring(mdl1, form1) then
              if d$isstring(mdl2, form2) then
                if sizeof(form1) = sizeof(form2) then compat := true;
            end;
          sets:
            begin
            if form2.basetype = noneindex {empty set} then compat := true
            else 
              compat := compatible(mdl1, form1.basetype, mdl2, form2.basetype);
            end;
          ptrs:
            begin
            if (index1 = nilindex) or (index2 = nilindex) then compat := true
            else
              begin
              compat := compatible(mdl1, form1.ptrtype, mdl2, form2.ptrtype);
              end;
            end;
          strings:
            compat := true;
          subranges, fields, files, scalars: ;
          otherwise choke(ourname);
          end
      else if form1.typ = strings then
        if (form2.typ = arrays) and d$isstring(mdl2, form2) then
          if sizeof(form1) >= sizeof(form2) then compat := true;
      end;
    compatible := compat;
  end; { compatible }


function operatorcompatible(op: tokentype;
                            leftitem, rightitem: dataitem;
                            var resultform: debugrecord;
                            var symidx: symbolindex): boolean;

  { Determine if two expressions can ce operated on by the given oprator (OP).
    For the non relational operators, the resulting form and symbol table
    index is resturned.  For the relational operators, since it is known that
    the result has to be boolean, the form and index reflect the type under
    which the operation will be done.  e.g 1 < 4.5 must be accomplished as a
    real comparison. }

  var
    compat, same, intreal: boolean;
    leftform, rightform: debugrecord;
    leftidx, rightidx: symbolindex;


  begin {operatorcompatible}
    d$getbaseform(leftitem, leftform, leftidx);
    d$getbaseform(rightitem, rightform, rightidx);
    same := compatible(leftitem.mdl, leftidx, rightitem.mdl, rightidx);
    symidx := leftidx;
    intreal := ([leftform.typ, rightform.typ] <= [ints, reals, doubles]) 
                 and (leftform.typ <> rightform.typ);
    if intreal then 
      begin
      if doubles in [leftform.typ, rightform.typ] then
        begin
        d$getobject(nil, doubleindex, resultform);
        symidx := doubleindex;
        end
      else
        begin
        d$getobject(nil, realindex, resultform);
        symidx := realindex;
        end;
      end
    else resultform := leftform;

    case op of
      equal, notequal, becomes:
	if hostopsys = msdos then compat := same
	else compat := same or intreal;
       lessequal, greaterequal:
        compat := (same or intreal) and
                  (leftform.typ in [ints, bools, chars, 
                                    scalars, reals, doubles, sets]);
      lessthan, greaterthan:
        compat := (same or intreal) and
                  (leftform.typ in [ints, bools, chars,
                                    scalars, reals, doubles]);
      divtok, modtok:
        begin
        compat := same and (resultform.typ = ints);
        end;
      andtok, nottok, ortok:
        begin
        compat := same and (resultform.typ in [ints, bools]);
        end;
      slash:
        begin
        compat := intreal or (same and (leftform.typ in [reals, doubles]));
        if symidx <> realindex then
          begin
          d$getobject(nil, realindex, resultform);
          symidx := realindex;
          end;
        end;
      plus, minus, star:
        begin
        compat := intreal or (same and
                  (resultform.typ in [ints, reals, doubles, sets]));
        end;
      intok:
        begin
        compat := (rightform.typ = sets) and identical(leftitem.mdl,
                                                       leftitem.index,
                                                       rightitem.mdl,
                                                       rightform.basetype);
        resultform := rightform;             
        end;
      end {case} ;

    operatorcompatible := compat;
  end; {operatorcompatible}



{*************************************************************************

     ------ Statement Map Access

*************************************************************************}


function d$reladdr {(mdl: mdl_pointer; addr: addressrec): addressrange} ;


  begin
    d$reladdr := addr.addr - mdl^.code_start;
  end;


function d$absaddr {(mdl: mdl_pointer; addr: addressrange): addressrec} ;

  var
    tmpaddr: addressrec;


  begin
    tmpaddr.addr := addr + mdl^.code_start;
    if segmented then tmpaddr.segment := mdl^.codesegment;
    d$absaddr := tmpaddr;
  end;


function d$locateaddr
    {(addr: addressrec; var mdl: mdl_pointer; 
      var proc: proctreeptr;
      var stmt: mapindex; allinfowanted: boolean): boolean} ;

  { Locate the module, procedure, and statement containing an address;
    return NIL module pointer when the address is not within any known module,
    return zero procedure and statement values when the module containing
    the address has no associated map file. allinfowanted indicates whether
    the statement number is needed. If NEEDCACHING then the file
    pointer in PROCTREEFILE is left at the appropriate procedure record, 
    otherwise a pointer to the record in the procedure record tree is
    returned via PROC.
  }
 
  label
    1;

  const
    ourname = 'locateaddr';
  var
    idx: mapindex;
    treeidx: integer;
    found: boolean;
    p: proctreeptr;
    rec: stmtrecord;
    base: addressrange;
    offset: addressrange;


  begin { d$locateaddr }

    { Locate program module containing address. }

    d$locateaddr := false;
    mdl := mdl_list;
    if not needcaching then proc := nil;
    stmt := 0;
    found := false;
    while not found do
      begin
      if mdl = nil then goto 1;
      if d$addrinrange(addr, mdl^.code_start, mdl^.code_end, mdl^.codesegment)
      then found := true
      else mdl := mdl^.next;
      end;
    if mdl^.info = noinfo then goto 1;
    if not mdl^.loaded then d$loadmdl(mdl);

    { Binary search the proctree for records with desired address. }

    found := false;
    if needcaching then
      begin
      treeidx := mdl^.proctreefileoff + 1;
      while (treeidx <> 0) and not found do
        begin
        seek(proctreefile, treeidx);
        if (addr.addr < proctreefile^.startpc) then 
          treeidx := proctreefile^.left.index
        else if (addr.addr > proctreefile^.endpc) 
          then treeidx := proctreefile^.right.index
        else 
          found := true;
        end;
      end
    else
      begin
      p := mdl^.proctree;
      while (p <> nil) and not found do
        begin
        if (addr.addr < p^.startpc) then p := p^.left.ptr
        else if (addr.addr > p^.endpc) then p := p^.right.ptr
        else found := true;
        end;
      end;

    d$locateaddr := found;
    if not needcaching then
      if found then proc := p;

    if found and allinfowanted then
      begin
      offset := d$reladdr(mdl, addr);
      found := false;
      if needcaching then idx := proctreefile^.procno
      else idx := proc^.procno;
      if not d$readmap(mdl, idx + 1, rec) then choke(ourname);

      repeat
        idx := idx + 1;
        base := rec.pc;
        found := d$readmap(mdl, idx, rec);
      until not found or (rec.pc > offset);
      idx := idx - 1;
      repeat
        idx := idx - 1;
      until not d$readmap(mdl, idx, rec) or (rec.pc <> base);
      stmt := idx + 1;

      end;

  1: ;

  end; { d$locateaddr }


procedure d$stmtnumlocate
    {(addr: addressrec; mdl: mdl_pointer; 
      proc: proctreeptr;
      var stmt: mapindex)} ;
  { Given an absolute address (ADDR), module (MDL) and proctree entry (PROC),
    determine the statement number (STMT). }

  const
    ourname = 'stmtnumlocate';
  var
    offset: integer;
    found: boolean;
    idx: mapindex;
    rec: stmtrecord;
    base: integer;


  begin
    if proc = nil then stmt := 0
    else
      begin
      offset := d$reladdr(mdl, addr);
      found := false;
      idx := proc^.procno;
      if not d$readmap(mdl, idx + 1, rec) then choke(ourname);

      repeat
        idx := idx + 1;
        base := rec.pc;
        found := d$readmap(mdl, idx, rec);
      until not found or (rec.pc > offset);

      idx := idx - 1;
      repeat
        idx := idx - 1;
      until not d$readmap(mdl, idx, rec) or (rec.pc <> base);
      stmt := idx + 1;
      end;
  end;


function d$stmtlocate {(mdl: mdl_pointer; proc: symbolindex; stmt: integer;
                       var index: mapindex): boolean } ;

  { Locate the map file record for a statement, identified by a module,
    procedure and relative statement number. }

  var
    idx: mapindex;
    rec: stmtrecord;
    done: boolean;


  begin { d$stmtlocate }
    d$stmtlocate := false;
    if proc <> nil then
      begin
      if stmt <> 0 then
        begin
        index := proc^.procno + stmt;
        if d$readmap(mdl, index, rec) then
          if rec.typ = stmntrec then d$stmtlocate := true;
        end
      else
        begin
        index := proc^.procno + 1;
        done := false;
        while d$readmap(mdl, index, rec) and not done do
          if rec.typ = plabrec then done := true
          else if rec.exit then
            begin
            done := true;
            d$stmtlocate := true;
            end
          else index := index + 1;
      end;
    end;  {if proc <> nil}
  end; { d$stmtlocate }

{************************************************************************

     ------ Procedure Frame List

************************************************************************}


function d$firstvisibleframe {: stackpointer} ;
  { Search the frames list, starting from the current frame, for the first
    frames which is "visible" to the debugger, i.e. which was compiled
    with the debug option. }

  var
    f: stackpointer;
    done: boolean;


  begin {d$firstvisibleframe}
    if not framesupdated then d$frameupdate;
    if not errorhappened then
      begin
      f := currentstackframe;
      done := false;
      while (f <> nil) and not done do
        if (f^.proc <> nil) then done := true
        else f := f^.dynamiclink;
      d$firstvisibleframe := f;
      end;
  end; {d$firstvisibleframe}



{****************************************************************************

        Files management routines

  These routines manage the opening and closing of the debugger auxilliary
  files (the statement map, symbol table and listing file).  A queue is
  maintained to determine which files to close when the max is approached.

****************************************************************************}


procedure add_lmo_mdl(mdl: mdl_pointer);

  var
    new_lmo_mdl: plastmdlopened;


  begin {add_lmo_mdl}
    new(new_lmo_mdl);
    new_lmo_mdl^.next := nil;
    new_lmo_mdl^.mdl := mdl;
    if num_mdls_open > 0 then first_lmo^.next := new_lmo_mdl
    else last_lmo := new_lmo_mdl;
    first_lmo := new_lmo_mdl;
    new_lmo_mdl := nil;
    num_mdls_open := num_mdls_open + 1;
  end; {add_lmo_mdl}


procedure closemdl(var last_lmo: plastmdlopened);

  var
    disp_lmo: plastmdlopened;


  begin {closemdl}
    with last_lmo^.mdl^ do
      begin
      if info = allinfo then close(listfile);
      close(symfile);
      close(mapfile);
      loaded := false;
      end;
    disp_lmo := last_lmo; { set LastLUO to next in list }
    last_lmo := disp_lmo^.next;
    dispose(disp_lmo); { and dispose of pointer }
    disp_lmo := nil;
    num_mdls_open := num_mdls_open - 1;
  end; {closemdl}


procedure d$loadmdl { (mdl: mdl_pointer) } ;
 { Locate symbol table and statement map files for module. }

  const
    ourname = 'd$loadmdl';
  var
    i: isymbolname;
    flg: integer;
    addr: addressrec;


  begin { d$loadmdl }
    if not mdl^.loaded and (mdl^.info > noinfo) then
      begin
      if (num_mdls_open + 1) > max_mdls_open then closemdl(last_lmo);
      if mdl^.filenames <> nil then
        reset(mdl^.symfile, mdl^.filenames^.symfilename, , flg)
      else reset(mdl^.symfile, mdl^.nam, symext, flg);
      if flg = - 1 then choke(ourname);
      if mdl^.filenames <> nil then
        reset(mdl^.mapfile, mdl^.filenames^.mapfilename, , flg)
      else reset(mdl^.mapfile, mdl^.nam, mapext, flg);
      if flg = - 1 then choke(ourname);
      if mdl^.info = allinfo then
        begin
        if mdl^.filenames <> nil then
          reset(mdl^.listfile, mdl^.filenames^.listfilename, , flg)
        else reset(mdl^.listfile, mdl^.nam, listext, flg);
        if flg = - 1 then choke(ourname);
        end;
      add_lmo_mdl(mdl);
      mdl^.loaded := true;
      end; {if unloaded}
    if not mdl^.proctreeinit then d$initproctree(mdl);
  end; { d$loadmdl }

{*****************************************************************************

            Datatoken list maintenance routines.

******************************************************************************}



  function d$getproctreerec {: proctreeptr};

    begin  {d$getproctreerec}
      if proctreerecpool = nil then 
        begin
        new(proctreerecpool);
        proctreerecpool^.left.ptr := nil;
        end;
      d$getproctreerec := proctreerecpool;
      proctreerecpool := proctreerecpool^.left.ptr;
    end;  {d$getproctreerec}


  procedure d$freeproctreerec {(proc: proctreeptr)};
  
    begin  {d$freeproctreerec}
      proc^.left.ptr := proctreerecpool;
      proctreerecpool := proc;
    end;  {d$freeproctreerec}


  function getdatapacket: datapacketptr;

    begin  {getdatapacket}
      if datapacketfreelist = NIL then 
        begin
        new(datapacketfreelist);
        datapacketfreelist^.next := NIL;
        end;
      getdatapacket := datapacketfreelist;
      datapacketfreelist := datapacketfreelist^.next;
    end;  {getdatapacket}

  procedure freedatapacket(packet: datapacketptr);
  
    begin  {freedatapacket}
      packet^.next := datapacketfreelist;
      datapacketfreelist := packet;
    end;  {freedatapacket}


procedure adddatatoken(var item: dataitem; currtyp: datatokentypes);
    { Add a datatoken to the list in the dataitem (ITEM) given.  A pool
      is first checked for availability.  }

  var
    head: datatokenptr;


  begin {adddatatoken}
    head := datatokenpool;
    if head = nil then new(head)
    else datatokenpool := datatokenpool^.next;
    head^.next := nil;
    if item.dtok = nil then
      begin
      item.dtok := head;
      item.datatokenlist := head;
      end
    else
      begin
      item.dtok^.next := head;
      item.dtok := item.dtok^.next;
      end;
    with item.dtok^ do
      begin
      typ := currtyp;
      if commands <> nil then cmdloc := commands^.idx
      else cmdloc := 0;
      end;
  end; {adddatatoken}


procedure cleardatatokens(var item: dataitem);
 { Return the datatokens in the given list to the pool. }

  var
    d, dback: datatokenptr;
    p, pback: datapacketptr;


  begin {cleardatatokens}
    dback := nil;
    d := item.datatokenlist;
    while d <> nil do
      with d^ do
        begin
        if typ = atomic then
          if packet <> nil then
            begin
            p := packet;
            while p <> nil do
              begin
              pback := p;
              p := p^.next;
              freedatapacket(pback);
              end;
            end;
        dback := d;
        d := d^.next
        end;
    if dback <> nil then
      begin
      dback^.next := datatokenpool;
      datatokenpool := item.datatokenlist;
      end;
    item.datatokenlist := nil;
    item.dtok := nil;
  end; {cleardatatokens}


procedure appendtokenlist(var left, right: dataitem);
    { Append the datatokenlist assocciated with right to that
      associatted with left. }

  var
    d, dback: datatokenptr;


  begin {appendtokenlist}
    if left.datatokenlist = nil then
      left.datatokenlist := right.datatokenlist
    else
      begin
      d := left.datatokenlist;
      while d <> nil do
        begin
        dback := d;
        d := d^.next;
        end;
      dback^.next := right.datatokenlist;
      end;
    right.datatokenlist := nil;
    left.dtok := right.dtok;
    right.dtok := nil;
  end; {appendtokenlist}



{***************************************************************************

  	History List Maintenance

  The history list is an array of addresses, the array size determined 
  by MAXHISTORY.  This array is treated as a circular array, with each 
  new entry placed at the HISTORYTAG location.

***************************************************************************}


  procedure d$inithistory;
    {re-zero the history paramters}

    begin  {d$inithistory}
      historycount := 0;
      historytag := historyidx;
    end;  {d$inithistory}


  procedure d$addhistory {(addr: addressrec)};
    {add an entry to the history list}

    begin  {d$addhistory}
      historylist[historyidx] := addr;
      historycount := historycount + 1;
      if historyidx = maxhistory then historyidx := 1
      else historyidx := historyidx + 1;
    end;  {d$addhistory}


  function d$getprevhistory {(cnt: historyindex): addressrec };
    {return the "cnt'th" previous history entry}

    begin  {d$getprevhistory}
      d$getprevhistory := 
        historylist[(historytag + historycount - cnt) mod maxhistory];
    end;  {d$getprevhistory}


  function d$historycount {(cnt: historyindex) {: historyindex} ;
    {Return the allowed number of history entries where CNT is the
     desired number.
    }


    begin  {d$historycount}
      d$historycount := d$min(cnt, d$min(historycount, maxhistory));
    end; {d$historycount}


