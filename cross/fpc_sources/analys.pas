{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1985, 1986, 1987 Oregon Software, Inc.
  ALL RIGHTS RESERVED.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied, modified, transferred, and used only as
  provided under a signed license agreement with Oregon Software.
  Any support purchased from Oregon Software does not apply to
  user-modified programs.  All copies of this program must display
  this notice and all copyright notices.


  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Declaration Syntax/Semantic Analyzer

 Last modified by KRIS on 21-Nov-1990 15:19:40
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

{  Declarations module for syntax analyzer.

  This module contains the control routine for the syntax analyzer
  and the code to parse the declarations.  The external procedure
  "body" parses and generates intermediate file output for the bodies
  of the procedures.  This is split apart in this manner to allow
  the code for the body and declarations to be overlayed
}

unit analys;

interface

uses config, hdr, utils, scan, a_t, hdra, commona, mda, body;

procedure analys;

implementation

procedure block;

{ Syntactic routine to parse a block,  see the body for details.
}
  forward;

procedure enterblock(level: levelindex; {lex level of block}
                     bn: index; {name index for block name}
                     ref: proctableindex {procedure entry for block} );

{ Set up the display for a new block.  This is called prior to parsing
  any definitions for the new block, including parameters.  Most of
  the display is initialized to empty values.  The scope id is
  incremented for the new block.
}


  begin {enterblock}
    displaytop := level;
    blockref := ref; {global for 'panic' routine}
    with display[level] do
      begin
      if (lastid >= totalscopes) or (lastscope >= totalscopes) then
        fatal(manyscopes); {the last value of totalscopes is not used.}
      lastid := lastid + 1;
      blockid := lastid;
      lastscope := lastscope + 1;
      scopeid := lastscope;
      dbgscope := 0;
      firststmt := 0;
      laststmt  := 0;
      blockref := ref;
      blockkind := codeblock;
      blocksize := 0;
      paramsize := 0;
      oldundeftabletop := undeftabletop;
      labellist := labelflag;
      threshold := tabletop;
      blockname := bn;
      highestkey := 0;
      namesdeclared := 0;
      proctable[blockref].opensfile := false;
      end
  end {enterblock} ;


function create_type(typeindex:index) : unsignedword; forward;


procedure fixupparamoffsets(endofdefs: boolean {last chance} );

{ Parameters are allocated before variables but are addressed
  relative to the stack pointer.  This means that after a variable-
  declaration-part is parsed the parameter offsets must be adjusted
  to reflect any new variables allocated in this block.

  "Fixupparamoffsets" scans the parameters and makes necessary adjustments
  to the offsets.

  This routine assumes that parameters are allocated name
  entries immediately following the block name entry.

  For nonpascal routines (routines accessable via C or F77), parameter
  offsets are reversed since prior to being called from C they will
  have been pushed on the stack in reverse order.

  Nonpascal/Fortran type routine must reserve extra space in the
  local data area to store the function return value.

  If we are compiling for MS-windows, then we increment the blocksize
  with a word to reflect the saved datasegment.  The blocksize
  must be used (and not the returnlinksize), since all mapping to
  frame pointer relative offsets, including mapping for the
  debugger, use blocksize.
}

  var
    i: index; {induction var}
    t: index; {last parameter index}
    bn: index; {block name index}
    p: entryptr; {used for accessing procedure and parameters}
    newoffset: addressrange; {dummy to fix code gen bug on the PDP-11}
    startoffset: addressrange; {start of param offsets for non pascal; vms only;
                               it is four when the function returns something
                               huge; (well greater than quad, anyway)}

  begin {fixupparamoffsets}
    with display[level] do
      begin
      bn := blockname;
      if endofdefs then
        begin
        if targetopsys = msdos then
         if proctable[blockref].externallinkage and
            switcheverplus[windows] and (level > 1) then
           blocksize := blocksize + wordsize;
        blocksize := forcealign(blocksize + paramsize, stackalign, false)
                     - paramsize;
        if level = 1 then
          ownsize := forcealign(ownsize, targetintsize, false);
        end;

      if bigcompilerversion then p := @(bigtable[bn]);

      if targetopsys = vms then
        if (p^.funclen <= ptrsize) or (p^.funclen = 8) then
          startoffset := 0
        else
          startoffset := 4; {extra parameter for return value}
                              
      t := p^.paramlist;
{
      Fortran declared functions will have an extra parameter
      pushed that points to the function return value.
}
      if (proctable[blockref].calllinkage = fortrancall) and
         (proctable[blockref].registerfunction = 0) and    
         (p^.functype <> noneindex) then
        paramsize := paramsize + wordsize;

      i := bn + 1;
      while i <= t do
        begin
        if bigcompilerversion then p := @(bigtable[i]);
        if not p^.form then
          begin
{
          Non-Pascal declared routines will have reversed parameters.
}
          if (proctable[blockref].calllinkage = nonpascalcall) and 
            ((targetmachine = iAPX86) or (targetmachine = mc68000) or
             (targetmachine = i80386) or
             { (targetopsys = apollo) or }
             (targetmachine = ns32k) and (unixtarget = umax)) then
            newoffset := blocksize + p^.offset
          else if (proctable[blockref].calllinkage = nonpascalcall) and 
                  (targetopsys = vms) then
            newoffset := forcealign(p^.offset + startoffset, ptrsize, false)
          else {pascal2call/fortrancall}
            newoffset := paramsize + blocksize - forcealign(p^.offset +
                         p^.length, stackalign, false);
          
          if (targetopsys = vms) and 
             (proctable[blockref].calllinkage = nonpascalcall) then
            newoffset := ptrsize + newoffset { allow one for parm count}
          else if proctable[blockref].externallinkage then
            newoffset := newoffset + extreturnlinksize
          else newoffset := newoffset + returnlinksize;

          p^.offset := newoffset;
          if p^.namekind in [procparam, funcparam] then
            i := p^.nextparamlink;
          end;
        i := i + 1;
        end;

      end {with display};
  end {fixupparamoffsets} ;



procedure flushbuffers;

 { dispose of all software virtual memory buffers }

  var          
    i: integer; { induction variable }


  begin {flushbuffers}
    if not bigcompilerversion then
      for i := 1 to lastblocksin do
        with blocksin[i] do
          if not lowblock and (buffer <> nil) then dispose(buffer);
  end {flushbuffers} ;




procedure exitblock(level: levelindex {level of block being exited} );

{ Called to exit the block at "level".

  All forms defined in the block are disposed of, and any names
  declared in the block are removed from the name table and
  the key map.  Finally, the block id and table tops are reset to
  the value on entry to the block.
}

  var
    t: index; {top of debug file}
    i: index; {Induction variable}
    p: entryptr; {used for access to names}
    regok: boolean; { true if we can assign vars to registers in this block }
    localvar: localvartype; { for writing to file }


  begin {exitblock}
    { Since for UMAX our global registers cannot overlap Unix-destroyed
      registers, there is no need to restrict global allocation based on
      nonpascal references. }

    if ((targetmachine = ns32k) and (unixtarget = umax)) or
       (targetmachine = i80386) then
      regok := not anynonlocallabels and (not anyexternals or (level > 1))
    else
      regok := not anynonpascalcalls and not anynonlocallabels and
               (not anyexternals or (level > 1));

    tempvars := 0;

    with display[level] do
      begin
        genint(0);
        for i := oldtabletop + 1 to tabletop do
          begin
          if bigcompilerversion then p := @(bigtable[i]);
          { tell travrs about any var that may be assigned to a register }
          if not p^.form and (p^.namekind = varname) and
             regok and p^.registercandidate then
               possibletemp(p^.offset, p^.vartype, p^.dbgsymbol);
          end;

      { signal the end of local vars for this block }

      localvar.typ := none;
      write(locals, localvar);

      lastid := blockid - 1;

      { Now remove entries from the keymap,  all entries with
        name greater than the name at start of block are new.
        This is unnecessary at the global level, as we'll do nothing
        other than move on to the next compilation phase!
        well, we still need to take care of the symbol table: don't output
        it twice..
      }

      if (level > 1) or switcheverplus[symboltable] then
        begin
        while namesdeclared > 0 do
          begin
          i := highestkey;
          highestkey := highestkey - 1;
          t := keymap[i];
          while t > threshold do
            begin
            namesdeclared := namesdeclared - 1;
            if bigcompilerversion then p := @(bigtable[t]);
            t := p^.nextname;
            end;
          keymap[i] := t;
          end;

        {If parameters, oldtabletop has been fudged}

        tabletop := oldtabletop;
        undeftabletop := oldundeftabletop;
        end;

      end;

  end {exitblock} ;



{ procedure block(level:levelindex; blockref:proctableindex); }


procedure listundefprocs;

{ Examine the table of forward defined procedures and emit an error message
  for all which are still undefined.  Also emits errors for program
  parameters which are never defined in the variable section.
}

  var
    p, pp: entryptr; { used for access to name table entry }
    i: undefindex; { induction var for table scan }


  begin {listundefprocs}
    for i := display[level].oldundeftabletop + 1 to undeftabletop do
      begin
      if bigcompilerversion then p := @(bigtable[undeftable[i].tableref]);
      with p^, undeftable[i] do
        if form then
          begin
          { the only way a "form" entry gets put in this table is
            when generating tables for the debugger, and we need to
            update the type a pointer points to, after that type is
            known.  The "dbgsymbol" testing code is just bulletproofing. }
          if newdebugger then {get rid of "create_" call if not}
            begin
            end
          end
        else if namekind in [forwardproc, forwardfunc] then
          warnat(fwdundef, line, column)
        else if namekind = undefname then warnat(nameundef, line, column);
      end;
  end {listundefprocs} ;


procedure listundeftypes;

{ Examine the table of forward defined types,
  emiting an error message for all which are still undefined.
}

  var
    p: entryptr; {used for access to name table entry}
    i: undefindex; {induction var for table scan}


  begin {listundeftypes}
    for i := display[level].oldundeftabletop + 1 to undeftabletop do
      begin
      if bigcompilerversion then p := @(bigtable[undeftable[i].tableref]);
      with undeftable[i] do
        begin
        if p^.namekind = undeftypename then
          begin
          if not bigcompilerversion then blocksin[1].written := true;
          with p^ do
            if namekind = undeftypename then
              begin
              if (typeindex = noneindex) then
                warnat(typeundef, line, column);
              namekind := typename;
              end;
          end;
        end;
      end;
  end {listundeftypes} ;


procedure listundeflabels;

{ check the list of labels declared in the block just completed and
  emit an error message for any not defined.
}

  var
    p: labelptr; { used for tracing the label list }


  begin {listundeflabels}
    p := display[level].labellist;
    while p <> labelflag do
      with p^ do
        begin
        if definednest = 0 then warnat(labelundef, labelline, labelcolumn);
        p := nextlabel;
        end;
  end {listundeflabels} ;



procedure enterundef(where: index {name index of undef entry} );

{ Enter an undefined name (pointer type or forward proc or func) in
  the undeftable.  This is used to make sure that all forward defs
  are satisfied by the end of the block.  The line and column are
  saved for an error message.
}


  begin {enterundef}
    undeftabletop := undeftabletop + 1;
    if undeftabletop > undeftablesize then fatal(undeftablefull);
    with undeftable[undeftabletop] do
      begin
      tableref := where;
      line := lasttoken.line;
      column := (lasttoken.left + lasttoken.right) div 2;
      end;
  end {enterundef} ;


function newproc: proctableindex;

{ Allocates a new entry in the proctable, initializes its entries,
  and returns the index of the new entry.
}


  begin {newproc}
    proctabletop := proctabletop + 1;
    if proctabletop > proctablesize then fatal(proctablefull);
    with proctable[proctabletop] do
      begin
      charindex := 0;
      charlen := 0;
      globaldeath := false;
      externallinkage := false;
      any_calls := false; {unused by P-2}
      struct_calls := false;
      struct_ret := false;
      bodydefined := false;
      intlevelrefs := false;
      opensfile := false;
      realfunction := false;
      isprocparam := false;
      referenced := {false} travcode; {TRAVRS/CODE COMBINED}
      ownused := false;
      calllinkage := pascal2call;
      registerfunction := 0;
      backlink := 0; {to be complete}
      level := 1;
      levelspread := 0;
      end;
    newproc := proctabletop;
  end { newproc } ;



{ Symbol Table and Display Structure ----

  The symbol table and display work together to implement Pascal's
  name access rules.  They are only loosely connected, unlike many Pascal
  compilers where they are part of the same structure.

  Each unique identifier is assigned a unique number called a "key" by the
  source scanner.  These keys are used as an index into a keymap table
  which relates the key to a name table entry.  However, in Pascal,
  the same name may refer to different objects in different scopes.
  The compiler assigns unique identifiers to each scope, and the
  combination of scope id and key is sufficient to identify an
  object.

  The symbol table contains a separate name entry for each object, and
  all objects with the same key are linked on a chain rooted in the keymap.
  The scope id is stored in the name entry (called "name" of all things)
  and is used when searching for a particular object.  Thus the symbol
  table provides a complete mapping from scope id and key to a name
  entry.

  The display, on the other hand, keeps track of currently accessable
  scopes.  Thus, upon entering a scope, the display is assigned an
  id, and this is used for all items defined within this scope.  Each
  procedure and each record will have it's own scope id.  Only those
  scopes which are represented on the display will normally be
  accessable.  Thus, to implement a "with" it is only necessary to
  make a display entry containing the scope id of the record type, and
  The fields of the record may be found by searching for an identifier
  with the record's scope id.

  This approach completely decouples accessing rules from the
  symbol table.  The symbol table handles names, and the display
  handles access rules.

  There is an optimization which conserves space in the symbol
  table, as well as speeding access.  Since identifiers defined
  within a block are inaccessable after exit from that block,
  they can be removed from the symbol table at block exit.  This is not
  the case for record fields, or for parameters.

  Record fields are simply left in the symbol table, and parameters
  are removed from the keymap chain, but their entries are left in the
  name table.  They are no longer accessable through the key map,
  but can be found from the procedure entry.

  Parameters in forward definitions are treated somewhat differently, for
  practical reasons.  They have their scope id set to an impossible
  value to make them inaccessable.

  In addition, scope id's can be re-used after exit from a block,
  thus conserving space in the symbol table.
}


procedure searchlevel(var wherefound: index {result of search} );

{ Search the current level for the current token.  The result is returned
  in "wherefound".  If not found, returns zero.
}


  begin {searchlevel}
    searchsection(display[level].blockid, wherefound);
  end {searchlevel} ;



{ Name Table Entry Procedures --

  Procedures to enter identifiers into the name table.
}


procedure enterident(id: integer; {scope id for entry}
                     var newindex: index; {resulting name entry}
                     undefok: nametype {undefname, undeftypename, or noname to
                                        control redefs} );

{ Enter the current token as an identifier with scope id "id".  The
  resulting name index is returned in "newindex".  If undefok is set to
  be undefname or undeftypename, this could be a forward type definition
  from a prior pointer declaration, or a program statement parameter,
  and namekind must equal undefok. Other than this special case, if the
  identifier is already defined in this scope or has been used in this
  scope an error message is emitted.  If the token is not an identifier,
  an error message to that effect is emitted.
}

  var
    key: integer; {identifier key from scanner}
    p: entryptr; {used for access to name entry}
    t: index; {used for name search}


  begin {enterident}
    newindex := 0;
    probing := true;
    if token = ident then
      begin
      key := thistoken.key;
      search(t);
      if id <> display[displaytop].blockid then
        begin
        if bigcompilerversion then p := @(bigtable[t]);
        if (t <> 0) and (p^.namekind in [undefname, undeftypename]) and
           (p^.lastoccurrence >= lastscope) then
          warn(duplicateident);
        searchsection(id, t);
        end;
      if t <> 0 then
        begin
        if bigcompilerversion then p := @(bigtable[t]);
        if switcheverplus[multidef] and
           (p^.namekind = varname) and
           (p^.varalloc in [usealloc, definealloc]) then newindex := t
        else
          begin
          if (p^.lastoccurrence >= display[displaytop].scopeid) and
             (p^.name <> id) or (p^.name = id) and
             (undefok <> p^.namekind) then warn(duplicateident);
          if (p^.name = id) and (p^.namekind = undefok) then newindex := t;
          end;
        end;
      end
    else key := 0;
    probing := false;

    if newindex = 0 then
      begin
      if tabletop = tablesize then fatal(tablefull)
      else tabletop := tabletop + 1;
      display[displaytop].namesdeclared := display[displaytop].namesdeclared +
                                           1;
      if key > display[displaytop].highestkey then
        display[displaytop].highestkey := key;
      if bigcompilerversion then p := @(bigtable[tabletop]);
      p^.dbgsymbol := 0;
      p^.form := false;
      p^.nextname := keymap[key];
      p^.name := id;
      p^.namekind := noname;
      p^.typeindex := noneindex;
      p^.refdefined := false; {!!!}
      p^.lastoccurrence := lastscope;
      p^.vartype := noneindex;
      p^.sparelink := 0; { pdb kluge, see dumpname }
      p^.varalloc := normalalloc; { initialization needed for $multidef }
      if token = ident then
        begin
        p^.charindex := thistoken.keypos;
        p^.charlen := thistoken.right - thistoken.left + 1;
        end
      else
        begin
        p^.charindex := 0;
        p^.charlen := 0;
        end;
      keymap[key] := tabletop;
      newindex := tabletop;
      end;

    verifytoken(ident, novarerr);

  end {enterident} ;


procedure enterlocalident(var newindex: index; {resulting name entry}
                          undefok: nametype {see enterident} );

{ Enter current token as identifier in local scope
}


  begin {enterlocalident}
    enterident(display[level].blockid, newindex, undefok);
  end {enterlocalident} ;



procedure programheading;

{ Syntactic routine to parse a program-heading.

  Productions:
  program-heading = "program" identifier [ "(" identifier
        [* "," identifier *] ")" ] ";"

  This is simply checked for syntactic correctness and ignored.
}


  procedure oneprogramparam;

{ Process one program parameter.  All identifiers declared in the
  program header must be later declared as variables at the global
  level.  The only exception is input and output, which are already
  defined implicitly.  There is an interaction with files: all external
  files must be declared in the program statement.  However, program
  parameters need not be files.  We always give an error if a program
  parameter appears but is never declared as a variable, however the
  external file restriction is only enforced if the program is compiled
  "standard".
}

    var
      identindex: index; {index to program parameter, if it exists}
      idenp: entryptr; {used to refer to name entry}


    begin {oneprogramparam}
      if token = ident then
        begin
        searchlevel(identindex);
        if identindex = 0 then
          begin
          enterlocalident(identindex, noname);
          enterundef(identindex);
          if bigcompilerversion then idenp := @(bigtable[identindex]);
          idenp^.namekind := undefname;
          end
        else
          begin
          if bigcompilerversion then idenp := @(bigtable[identindex]);
          if idenp^.programdecl then warn(duplicateident)
          else if identindex = inputindex then inputdeclared := true
          else if identindex = outputindex then outputdeclared := true
          else warn(compilerwritererr);
          gettoken;
          end;
        idenp^.programdecl := true;
        end
      else
        begin
        warnbetween(novarerr);
        gettoken;
        end;
    end {oneprogramparam} ;


  begin {programheading}
    gettoken;
    proctable[0].charindex := thistoken.keypos;
    proctable[0].charlen := min(maxprocnamelen,
                                thistoken.right - thistoken.left + 1);
    verifytoken(ident, novarerr);
    if token = lpar then
      begin
      gettoken;
      oneprogramparam;
      while token in [comma, ident] do
        begin
        verifytoken(comma, nocommaerr);
        oneprogramparam;
        end;
      verifytoken(rpar, norparerr);
      end;
    verifytoken(semicolon, nosemierr);
  end {programheading} ;


function getdbgsymbol(i: index):p_symbolindex;

{ Map a compiler index to debugger record index when
  we know an entry has already been made, as in a
  name.
}
var p:entryptr; {The usual}

begin {getdbgsymbol}
if i = 0 then getdbgsymbol := 0
else begin
if bigcompilerversion then p := @(bigtable[i]);
getdbgsymbol := p^.dbgsymbol;
end;
end   {getdbgsymbol};


function create_type(typeindex:index) : unsignedword;

{ Creates an ODB symbol table entry for the given type.  Some
  special ones are simply defined (by create_simpleform) by
  a constant definition in dbghdr.  Ugly, in my opinion.
}

var
  tp : entryptr;
  p  : entryptr;
  paramindex : index;

begin {create_type}
  create_type := 0;
end   {create_type} ;

procedure gettyp(follow: tokenset; {legal following symbols}
                 var resulttype: index {resulting type desc} );
  forward;

{ Parses a type. See procedure definition for comments.
}


procedure onevar(id: integer; {Scope in which to enter ident}
                 varkind: nametype; {variable kind}
                 var where: index; {where in symtable it ends up}
                 sharedvar: boolean {if in 'shared' definition} );

{ Syntactic routine to parse a single component of a variable list, which can
  be part of a variable-list, parameter-list, or field-list.  "Varkind"
  determines the kind of variable list being parsed.

  production:

  x-identifier = identifier [use/define 'name' | origin nnn]

  The identifier is entered into the symbol table at scope "id".

  If the identifier is a parameter, it is initialized on entry to
  the scope, so is marked as "modified".
}

  var
    p: entryptr; {used to access name entry}
    assignedaddress: operand; {holds ORIGIN value or external name, if any}
    defineflag: boolean; {true if this is a DEFINE variable}
    varblock: 0..maxvarptrs; {for calculating block number of USE/DEFINE var}
    alias_needed: boolean; {true if string follows USE/DEFINE var}
    vartab_state: (new_entry, reuse_entry, no_entry); {used with multidef
                  switch to handle mutiple definitions}
    entry: integer; {Current vartableentry index}

  begin {onevar}
    {The following actually consumes the token}
    enterident(id, where, undefname);
    if bigcompilerversion then p := @(bigtable[where]);
    with p^ do
      begin
      if namekind <> undefname then programdecl := false;
      namekind := varkind;
      lastinsection := false;
      univparam := false;
      vartab_state := new_entry;

      if sharedvar and (token in [definesym, usesym, originsym]) then
        warn(badsharedvar);

      if sharedvar or (token in [definesym, usesym]) then
        begin
        if sharedvar then
          begin
          if not scanalys and switcheverplus[defineswitch] then
            warnbefore(baddefine);

          if switcheverplus[multidef] and
             (varalloc in [definealloc, usealloc]) then warn(badmultidef);

          varalloc := sharedalloc;
          {point to the identifer text in the stringtable}
          assignedaddress.cvalue.pos := charindex;
          assignedaddress.cvalue.len := charlen;
          alias_needed := false;
          end
        else {use/define}
          begin
          if (varkind <> varname) or (level <> 1) then
            warn(badusedefinevar);
          if token = usesym then
            begin
            if switcheverplus[multidef] and
               (varalloc in [usealloc, definealloc]) then
              vartab_state := no_entry
            else varalloc := usealloc;
            end
          else
            begin
            if switcheverplus[multidef] then
              begin
              if varalloc = definealloc then
                warn(duplicateident); { Don't allow multiple 'define's }
              if not (varalloc in [normalalloc, usealloc]) then
                warn(badmultidef);
              if varalloc = usealloc then vartab_state := reuse_entry;
              end;
            varalloc := definealloc;
            end;
          gettoken;
          if token in begconstset then
            begin
            constant(begtypset + [colon, comma], true, assignedaddress);
            alias_needed := true;

            with assignedaddress, cvalue do
              if typeindex = chartypeindex then
                begin
                if not scanalys then seekstringfile(consttablelimit);
                putbyte(intvalue);
                if scanalys then pos := stringfilecount - 1
                else pos := consttablelimit - 1;
                len := 1;
                end
              else if (representation <> arrays) or not stringconstflag then
                warnbefore(nostringerr);
            end
          else
            begin { point to the identifer text in the stringtable }
            if not scanalys and switcheverplus[defineswitch] then
              warnbefore(baddefine);
            alias_needed := false;
            assignedaddress.cvalue.pos := charindex;
            assignedaddress.cvalue.len := charlen;
            end;
          end; {use/define}

        if vartab_state <> no_entry then
          begin
          { If multidef is true, then allow a define to overlay the vartable
            entry for a use, but ignore a use after a define.
          }
          if vartab_state = reuse_entry then entry := sparelink
          else
            begin
            lastvartableentry := lastvartableentry + 1;
            entry := lastvartableentry;
            end;

          varblock := entry div (maxvarentries + 1) + 1;

          if varblock > lastvartableptr then
            begin
            if varblock > maxvarptrs then fatal(toomanyextvars)
            else
              new(vartable[varblock]);
            lastvartableptr := varblock;
            end;

          sparelink := entry;

          with vartable[varblock]^[entry mod (maxvarentries + 1)] do
            begin
            extvaralloc := varalloc;
            initialized := false; {never true for Pascal}
            referenced := false;
            aliased := alias_needed;
            charindex := assignedaddress.cvalue.pos;
            charlen := min(maxprocnamelen, assignedaddress.cvalue.len);
            size := 0;
            offset := 0;
            end;
          end; {vartab_state <> no_entry}
        end
      else if token = originsym then
        begin
        gettoken;
        constant(begtypset + [colon, comma], true, assignedaddress);
        if (varkind = varname) and
           (assignedaddress.cvalue.representation = ints) and
           not assignedaddress.cvalue.negated then
          begin
          {**note: following assignment also converts int to unsigned**}
          if switcheverplus[multidef] and
             (varalloc in [definealloc, usealloc]) then warn(badmultidef);
          varalloc := absolute;
          offset := assignedaddress.cvalue.intvalue;
          end
        else warnbefore(badorigin);
        end
      else if (varkind = varname) and (level = 1) and
              (switchcounters[own] > 0) then
        begin
        if switcheverplus[multidef] and
           (varalloc in [definealloc, usealloc]) then warn(badmultidef);
        varalloc := ownalloc;
        end
      else { It's none of the above -- must be normalalloc }
        begin
        if switcheverplus[multidef] and
           (varalloc in [definealloc, usealloc]) then warn(badmultidef);
        varalloc := normalalloc;
        end;

      registercandidate := varalloc = normalalloc; { until proven otherwise }
      nextparamlink := where;
      nestedmod := false;
      varianttag := false;
      knownvalid := (namekind in
                    [procparam, funcparam, param, varparam, boundid]);
      modified := (varalloc = absolute) or knownvalid or switcheverplus[test];
      parammodified := false;
      end;
  end {onevar} ;


procedure alloconevar(t: index; { name entry for variable }
                      f: index; { variable type }
                      varkind: nametype; {type of this variable}
                      var size: addressrange; {size of dataspace}
                      a: alignmentrange; {alignment requirement}
                      typelen: addressrange; {var length}
                      refparam: boolean; {param passed by reference}
                      packedresult: boolean {do packed allocation} );

{ Allocate one variable in a variable or field list.  "Size" is the current
  size of the dataspace in which the variable is being allocated, and is
  updated after the allocation.

  "Size" and "typelen" are in bits if this is a packed allocation, otherwise
  in addressing units.

  This procedure defines the packed allocation strategy, which is to allocate
  bitwise, but not to allow fields to cross word boundaries.
}

  var
    p: entryptr; {used for access to name entry}
    newoffset: addressrange; {offset of new variable}
    unusedspace: boolean; {we skipped over some space in packed field}
    overflowed: boolean; {true if data space is too large}


  begin {alloconevar}
    if bigcompilerversion then p := @(bigtable[t]);
    p^.namekind := varkind;

    if switcheverplus[multidef] and (p^.namekind = varname) and
       (p^.varalloc in [usealloc, definealloc]) and
       not identical(p^.vartype, f) then warn(badmultidef);

    p^.vartype := f;

    if refparam then
       begin
       if switcheverplus[multidef] and
          (p^.varalloc in [definealloc, usealloc]) then warn(badmultidef);
       p^.varalloc := pointeralloc;
       end;

    if p^.varalloc = absolute then
      begin
      if (targetmachine = pdp11) and (a <> 1) and (p^.offset <> - 1) and
         (p^.offset mod a <> 0) then
        warnbefore(badorigin)
      end
    else if p^.varalloc in [usealloc, definealloc, sharedalloc] then
      begin
      with vartable[p^.sparelink div (maxvarentries + 1) + 1]^
                   [p^.sparelink mod (maxvarentries + 1)] do
        begin
        if (p^.varalloc = definealloc) and (size = 0) then
          {we checked size to avoid multiple allocation with multidef}
          begin
          alloc(a, typelen, definesize, newoffset, overflowed);
          offset := newoffset;
          end;
        size := typelen;
        end;

      p^.offset := 0;
      end
    else
      begin
      if p^.varalloc = ownalloc then
        if packedresult then
          allocpacked(a, typelen, ownsize, newoffset, overflowed, unusedspace)
        else alloc(a, typelen, ownsize, newoffset, overflowed)
      else if packedresult then
        allocpacked(a, typelen, size, newoffset, overflowed, unusedspace)
      else alloc(a, typelen, size, newoffset, overflowed);
      p^.offset := newoffset;
      if overflowed then
        if varkind = fieldname then warnbefore(bigrecorderr)
        else warnbefore(bigblockerr);
      end;
    p^.length := typelen;
  end {alloconevar} ;


procedure variablelist(follow: tokenset; { legal following symbols }
                       f1: tokenset; {desired terminating symbols}
                       id: integer; { scope id }
                       dbgscope: p_symbolindex; {ditto for debugger}
                       var size: addressrange; {size of dataspace}
                       var align: alignmentrange; {alignment requirements}
                       varkind: nametype; {kind of idents in list}
                       notrecord: boolean; {Not record (for check)}
                       packedresult: boolean; {packed alloc desired}
                       sharedvar: boolean {if 'shared' definition} );

{ Syntactic routine to parse a group of variable lists of any flavor.

  Productions:

  x-list = [* ";" *] [* identifier [* "," identifier *]
        ":" type [ ";" ] *]  .

  Leading semicolons are accepted only within a record, as determined
  by the parameter "notrecord".

  This routine is complicated by being called in several contexts and
  with several purposes.  In particular, the routine is parsing an
  entire group of variable declarations, and attempts do as much error
  recovery locally as possible.  On the other hand, in some contexts
  the routine should stop after each declaration.  This is controlled
  by the internal set "startset" and the parameter "f1".

  "Startset" contains essentially all of the tokens which might appear
  in a variable list.  In particular, it contains all of the punctuation
  and all of the type beginning symbols.  "F1" contains all of the
  tokens on which the scan should stop after the first variable list
  is parsed.  For instance, in a field list, "f1" contains all of
  the tokens which might begin the next fields. The details
  of usage can be seen by examining the calling locations.

  All identifiers found are entered in the scope specified by "id", and
  given the kind "varkind".  On entry to the procedure, "size" is the
  current size of the data space, and "align" is the minimum alignment
  requirement for the variables.  Space allocated is reflected
  by "size", and the maximum alignment is accumulated and returned in
  "align".

  The case where a variable may already exist (due to previous naming
  as a program parameter) complicates things.  The variable list is
  linked upon "nextparamlink", in a special way (the next item in
  the list equals "nextparamlink + 1").  It is done this way due to
  the fact that "nextparamlink" is being overloaded, as it is really
  intended to link parameter lists, skipping over nested procedure
  parameters.
}

  var
    startset: tokenset; {All tokens to be found in a varlist}
    first: index; {first ident in a list}
    last: index; {last ident in a list}
    t: index; {induction var for allocation}
    p: entryptr; {used in name access}
    f: index; {type of a variable list}
    tptr, fptr: entryptr; {for access to the pointer}
    a: alignmentrange; {alignment for one varlist}
    typelen: addressrange; {length of one varlist element}
    varcount: integer; {number of variables in list}
    startpos: integer; {position of first var in source file}
    startline: integer; {line number of first var}

  begin {variablelist}

    align := unitsize;

    startset := [uparrow..stringsym, nilsym, ident, plus, minus, lpar,
                intconst..stringconst, comma, colon, semicolon];

    {skip semicolons if processing field list}
    if notrecord then verify([ident], follow, novarerr)
    else while token = semicolon do gettoken;

    while token in startset do
      begin {parse one variable list, making sure that it terminates on one of
             the tokens in f1}

      startset := startset - f1;

      startpos := thistoken.filepos;
      startline := thistoken.line;

      onevar(id, varkind, first, sharedvar);
      varcount := 1;
      last := first;
      while token in [ident, comma] do
        begin
        verifytoken(comma, nocommaerr);
        f := last;
        onevar(id, varkind, last, sharedvar);
        varcount := varcount + 1;
        if last - 1 <> f then
          begin
          if bigcompilerversion then fptr := @(bigtable[f]);
          fptr^.nextparamlink := last - 1;
          end;
        end;

      f := noneindex;
      verifytoken(colon, nocolonerr);
      gettyp(follow + startset, f);

      if bigcompilerversion then fptr := @(bigtable[f]);
      if notrecord and fptr^.containsfile then
        proctable[display[level].blockref].opensfile := true;

      t := first;
      repeat
        getallocdata(fptr, varkind, packedresult, size, typelen, a, align);
        alloconevar(t, f, varkind, size, a, typelen, false, packedresult);
        if bigcompilerversion then tptr := @(bigtable[t]);
        t := tptr^.nextparamlink + 1;
        if bigcompilerversion then fptr := @(bigtable[f]);
        varcount := varcount - 1;
      until varcount = 0;

      {must end on semicolon or in f1 for no error}
      if token = semicolon then gettoken
      else
        verify(f1, follow + begtypset + [comma, colon, semicolon],
               nosemiheaderr);
      end;
  end {variablelist} ;



procedure gettyp(follow: tokenset; {legal following symbols}
                 var resulttype: index {resulting type desc} );

{ Syntactic routine to parse a type.

  Productions:

  type-denoter = ( [ "packed" ] structured-type ) | simple-type |
        pointer-type  .

  structured-type = array-type | file-type | set-type | record-type  .

  The resulting form entry is returned in "resulttype"
}

  var
    packflag: boolean; {set if "packed" found}
    resulp: entryptr; {pointer for access to resulttype}


  procedure simpletyp;

{ Syntactic routine to parse a simple type.

  Productions:

  simple-type = type-identifier | subrange-type | enumerated-type  .

  subrange-type = constant ".." constant  .

  enumerated-type = "(" identifier [* "," identifier *] ")"  .
}

    var
      lowervalue, uppervalue: operand; {ends of a subrange}
      extended: boolean; {range doesn't fit in host integer}
      t: index; {type identifier index}
      p: entryptr; {used for name access}
      f: entryptr; {used for access to form data}
      latestord: integer; {ord of last scalar parsed in enumerated type}
      subrange: boolean; {true if subrange being parsed}


    procedure onescalar;

{ Syntactic routine to parse a single identifier in an enumeration type.

  Production:

  enumeration-constant = identifer  .

  The identifier is entered in the local scope (not the record scope, but
  the surrounding local scope).  Successive constants are assigned successive
  ordinal values.
}

      var
        t: index; {index of enumeration constant id}


      begin {onescalar}
        enterlocalident(t, noname);
        if bigcompilerversion then p := @(bigtable[t]);
        with p^, constvalue do
          begin
          namekind := constname;
          consttype := resulttype;
          representation := ints;
          intvalue := latestord;
          negated := false;
          end;

      end {onescalar} ;


    begin {simpletyp}
      resulttype := noneindex;
      if packflag then warn(cantpack);
      if token = lpar then
        begin {enumeration type, parse constants}
        gettoken;
        enterform(scalars, resulttype, resulp);
        latestord := 0;
        onescalar;
        while token in [comma, ident] do
          begin
          latestord := latestord + 1;
          verifytoken(comma, nocommaerr);
          onescalar;
          end;

        if bigcompilerversion then resulp := @(bigtable[resulttype]);
        with resulp^ do
          begin
          lastord := latestord;
          size := simplesize(lastord);
          align := min(scalaralign, size);
          end;
        verifytoken(rpar, norparerr);
        end
      else {not enumerated}
        begin
        verify(begconstset, follow, badtypesyntax);
        subrange := token in begconstset;
        if token = ident then
          begin
          search(t);
          if bigcompilerversion then p := @(bigtable[t]);
          if (t = 0) or (p^.namekind = undeftypename) and
             (p^.typeindex = noneindex) or (p^.namekind = noname) then
            begin {undefined, assume was meant to be type id.}
            warn(undefidenterr);
            subrange := false;
            gettoken;
            end
          else if p^.namekind in [undeftypename, typename] then
            begin
            resulttype := p^.typeindex;
            subrange := false;
            gettoken
            end
          end;
        if subrange then
          begin
          constant(follow + [dotdot], true, lowervalue);
          verifytoken(dotdot, nodotdoterr);
          constant(follow, true, uppervalue);
          extended := uppervalue.extended;
          if bigcompilerversion then f := @(bigtable[lowervalue.typeindex]);
          if (lowervalue.typeindex <> noneindex) and
             (uppervalue.typeindex <> noneindex) and
             ((lowervalue.typeindex <> uppervalue.typeindex) or
             not (f^.typ in [ints, chars, bools, scalars]) or (extended and
             ((switchcounters[standard] > 0) or lowervalue.cvalue.negated or
             ((lowervalue.cvalue.intvalue < 0) and
             (lowervalue.cvalue.intvalue > uppervalue.cvalue.intvalue)))) or
             (not extended and (((lowervalue.cvalue.intvalue < 0) and
             not lowervalue.cvalue.negated) or
             (lowervalue.cvalue.intvalue >
             uppervalue.cvalue.intvalue)))) then
            warnbefore(badsubrange)
          else
            begin
            enterform(subranges, resulttype, resulp);
            with resulp^ do
              begin
              lowerord := lowervalue.cvalue.intvalue;
              upperord := uppervalue.cvalue.intvalue;
              parentform := f^.typ;

{ The next section is parameterized to set the allocation size for
  variables, depending on the target machine.
 }
              case targetmachine of
                vax:
                  begin
                  size := defaulttargetintsize;
                  if not extended then
                    if lowerord < 0 then
                      if simplesize(max(abs(lowerord + 1),
                         abs(upperord))) = defaulttargetintsize then
                        size := defaulttargetintsize
                      else
                        size := simplesize(max(abs(lowerord + 1),
                                               abs(upperord)) * 2)
                    else size := simplesize(upperord);
                  if size = defaulttargetintsize - 1 then
                    size := defaulttargetintsize;
                  parenttype := lowervalue.typeindex;
                  if bigcompilerversion then p := @(bigtable[parenttype]);
                  if (switchcounters[pdp11data] > 0) and (p^.size > 1) then
                    align := pdpalign
                  else align := min(intalign, size);
                  if switchcounters[pdp11data] > 0 then
                    size := forcealign(size, align, false)
                  else
                    begin
                    { note: On the vax we simply allocate the parent size.
                      The strategy here is to minimize extend
                      operations at the expense of greater storage
                      requirements for unpacked subrange variables.
                     }
                    size := p^.size;
                    align := p^.align;
                    end;
                  end {vax} ;
                mc68000, pdp11:
                  begin
                  size := defaulttargetintsize;
                  if not extended then
                    if lowerord < 0 then
                      if simplesize(max(abs(lowerord + 1),
                         abs(upperord))) = defaulttargetintsize then
                        size := defaulttargetintsize
                      else
                        size := simplesize(max(abs(lowerord + 1),
                                               abs(upperord)) * 2)
                    else size := simplesize(upperord);
                  align := size;
                  parenttype := lowervalue.typeindex;
                  if bigcompilerversion then p := @(bigtable[parenttype]);
                  { the strategy here is to allocate 2 bytes if
                    it is subrange of a parenttype which is going
                    to be 4 bytes.
                  }
                  size := min(p^.size, forcealign(size, intalign, false));
                  align := p^.align;
                  end {mc68000, pdp11} ;
                iapx86:
                  begin
                  size := defaulttargetintsize;
                  if not extended then
                    if lowerord < 0 then
                      if simplesize(max(abs(lowerord + 1),
                         abs(upperord))) = defaulttargetintsize then
                        size := defaulttargetintsize
                      else
                        size := simplesize(max(abs(lowerord + 1),
                                               abs(upperord)) * 2)
                    else size := simplesize(upperord);
                  align := size;
                  parenttype := lowervalue.typeindex;
                  if bigcompilerversion then p := @(bigtable[parenttype]);
                  { the strategy here is to allocate 1 or 2 bytes if
                    it is subrange of a parenttype which is going
                    to be 4 bytes.
                  }
                  if switchcounters[bytealloc] <= 0 then {default}
                    begin
                    size := min(p^.size, forcealign(size, intalign, false));
                    align := p^.align;
                    end
                  else
                    begin
                    size := min(p^.size, forcealign(size, unitsize, false));
                    align := unitsize;
                    end;
                  if size = defaulttargetintsize - 1 then
                    size := defaulttargetintsize;
                  end {iapx86} ;
                ns32k:
                  begin
                  size := defaulttargetintsize;
                  if not extended then
                    if lowerord < 0 then
                      if simplesize(max(abs(lowerord + 1),
                         abs(upperord))) = defaulttargetintsize then
                        size := defaulttargetintsize
                      else
                        size := simplesize(max(abs(lowerord + 1),
                                               abs(upperord)) * 2)
                    else size := simplesize(upperord);
                  align := size;
                  parenttype := lowervalue.typeindex;
                  if bigcompilerversion then p := @(bigtable[parenttype]);
                  { the strategy here is to allocate 2 bytes if
                    it is subrange of a parenttype which is going
                    to be 4 bytes but to align 2 bytes on 2 byte bounds and
                    others on the parent alignment.
                  }
                  size := min(p^.size, forcealign(size, intalign, false));
                  if size = shortintsize then align := shortintalign
                  else align := p^.align;
                  end {ns32k } ;
                i80386:
                  begin
                  size := defaulttargetintsize;
                  if not extended then
                    if lowerord < 0 then
                      if simplesize(max(abs(lowerord + 1),
                         abs(upperord))) = defaulttargetintsize then
                        size := defaulttargetintsize
                      else
                        size := simplesize(max(abs(lowerord + 1),
                                               abs(upperord)) * 2)
                    else size := simplesize(upperord);
                  align := size;
                  parenttype := lowervalue.typeindex;
                  { the strategy here is to allocate 2 bytes if
                    it is subrange of a parenttype which is going
                    to be 4 bytes but to align 2 bytes on 2 byte bounds and
                    others on the parent alignment.
                  }
                  if size = shortintsize then
                    begin
                    align := shortintalign;
                    end
                  else
                    begin
                    if bigcompilerversion then p := @(bigtable[parenttype]);
                    size := min(p^.size, forcealign(size, intalign, false));
                    align := p^.align;
                    end;
                  end {i80386} ;
                otherwise
                  begin
                  write('Missing targetmachine selection');
                  compilerabort(inconsistent);
                  end;
                end; {case targetmachine of}
              extendedrange := extended;
              end; {with resulp}
            end;
          end;
        end;
      verify1(follow, badtypesyntax);
    end {simpletyp} ;


  procedure ptrtyp;

{ Syntactic routine to parse a pointer type.

  Production:

  pointer-type = "^" type-identifier  .

  This is complicated by the fact that pointer types can refer to types
  which are not yet defined.  In this case, the referenced type is entered
  as an undefined type name, and a reference to it is kept in the
  undeftable for checking.

  Since travrs needs to be able to group pointers according to type,
  each pointer type is assigned a unique key (generated from lastfilekey)
  which serves to identify it to travrs.  This key is not used further
  in analys.

}

    var
      t: index; { index for type identifier }
      p: entryptr; {used for name access to t}
      newtype: index; {points to type of type name if found}


    begin {ptrtyp}
      if packflag then warn(cantpack);
      gettoken;
      enterform(ptrs, resulttype, resulp);
      with resulp^ do
        begin
        ptrkey := lastfilekey;
        lastfilekey := lastfilekey - 1;
        size := ptrsize;
        align := ptralign;
        ptrtypename := 0;
        if token = ident then
          begin
          probing := true;
          search(t);
          probing := false;
          if bigcompilerversion then p := @(bigtable[t]);
          if (t = 0) or
             not (p^.namekind in [undeftypename, noname, typename]) then
            begin
            newtype := noneindex;
            if t <> 0 then
              if (p^.lastoccurrence = display[level].scopeid) then
                warn(notypenameerr);
            end
          else newtype := p^.typeindex;
          if (t = 0) or (lev < level) and
             (p^.lastoccurrence < display[level].scopeid) then
            begin
            enterlocalident(t, noname);
            enterundef(t);
            if bigcompilerversion then p := @(bigtable[t]);
            p^.namekind := undeftypename;
            p^.typeindex := newtype;
            end
          else gettoken;
          if bigcompilerversion then resulp := @(bigtable[resulttype]);
          resulp^.ptrtypename := t
          end
        else warnbetween(novarerr);
        end;
    end {ptrtyp} ;


  procedure arraytyp;

{ Syntactic routine to parse an array type.

  Productions:
  array-type = "array" "[" index-type [* "," index-type *] "]"
        "of" component-type  .

  component-type = type-denoter  .

  The real work is done in the procedure arraywork, which handles the
  nested array definition implied by the notation:

  [ index-type , index-type ]  .
}


    procedure arraywork(var resulttype: index {resulting array type} );

{ Syntactic routine to parse most of an array declaration.  The productions
  used are given above for "arraytyp".  As successive index types are found,
  this procedure is invoked recursively to treat it as:

  array [ index-type ] of array [index-type] of ...
}

      var
        es: addressrange; {tentative array element size}
        elttype: index; {type pointer for element}
        resulp: entryptr; {for access to resulttype}
        f: entryptr; {used to get access to type records}
        newindextype: index; {index type}
        t: 0..maxusint; {temp for unsigned arithmetic}
        indexmembers: addressrange; {members of the index type}
        isstring: boolean; {set if it is a string}


      begin {arraywork}
        gettyp(follow + [ofsym, comma, rpar, rbrack] + begsimplset,
               newindextype);
        if bigcompilerversion then f := @(bigtable[newindextype]);
        if not (f^.typ in [none, chars, bools, scalars, subranges]) then
          begin
          warnbefore(badindex);
          newindextype := noneindex;
          end;
        t := upper(f) - lower(f) + 1;
        if t > maxaddr then
          begin {we cannot address more then maxaddr elements}
          warnbefore(toomanyelements);
          end;
        indexmembers := t;
        isstring := (lower(f) = 1) and
                    ((upper(f) > 1) or (switchcounters[standard] <= 0));
        if isstring then isstring := getform(f) in [ints, none];
        if token in
           [comma, ident, nilsym, lpar, plus, minus,
           intconst..stringconst] then
          begin
          verifytoken(comma, nocommaerr);
          arraywork(elttype);
          end
        else
          begin
          if token = rpar then
            begin
            warn(norbrackerr);
            gettoken
            end
          else verifytoken(rbrack, norbrackerr);
          verifytoken(ofsym, nooferr);
          gettyp(follow, elttype)
          end;
        if bigcompilerversion then f := @(bigtable[elttype]);
        if isstring then isstring := f^.typ in [chars, none];
        enterform(arrays, resulttype, resulp);
        with resulp^ do
          begin
          indextype := newindextype;
          packedflag := packflag;
          elementtype := elttype;
          arraymembers := indexmembers;
          es := arraysizeof(f, packflag);
          if es = 0 then es := 1; {undefined kind}

          { determine if array size less than one word }

          bitaddress := packflag and (es < (packingunit * bitsperunit)) and
          {in case of overflow}
                        (indexmembers < (packingunit * bitsperunit) div es);

          if not bitaddress and packflag and
             (es < packingunit * bitsperunit) then
          { packed array > word but element < word }
            if es < bitsperunit then
              size := (indexmembers + (bitsperunit div es) - 1) div
                      (bitsperunit div es)
            else
              size := ((es + bitsperunit - 1) div bitsperunit) * indexmembers
          else
            begin
            es := arraysizeof(f, bitaddress);
            if not bitaddress and (es > 1) and
               (maxaddr div es < indexmembers) then
              begin
              indexmembers := 1; {prevent overflow}
              warnbefore(bigarrayerr);
              end;
            size := indexmembers * es;
            if packflag and not bitaddress then es := es * bitsperunit;
            end;
          elementsize := es;
          if bitaddress then align := es
          else if packflag and (es <= bitsperunit) then align := unitsize
          else align := alignmentof(f, false);
          containsfile := f^.containsfile;
          stringtype := isstring and packflag;
          end;
      end {arraywork} ;


    begin {arraytyp}
      gettoken;
      if token = lpar then
        begin
        warn(nolbrackerr);
        gettoken
        end
      else verifytoken(lbrack, nolbrackerr);
      arraywork(resulttype);
    end {arraytyp} ;


  procedure filetyp;

{ Syntactic routine to parse a file type.

  Production:

  file-type = "file" "of" component-type  .

  component-type = type-denoter  .
}

    var
      newfilebasetype: index; {pointer to type of file}
      f: entryptr; {access to newfilebasetype}


    begin {filetyp}
      gettoken;
      verifytoken(ofsym, nooferr);
      gettyp(follow, newfilebasetype);
      if bigcompilerversion then f := @(bigtable[newfilebasetype]);
      if f^.containsfile then warnbefore(nofilefile);
      enterform(files, resulttype, resulp);
      with resulp^ do
        begin
        filebasetype := newfilebasetype;
        packedflag := packflag;
        bitaddress := false;
        containsfile := true; { You had better believe it!!! }
        size := ptrsize;
        align := ptralign;
        filekey := lastfilekey;
        lastfilekey := lastfilekey - 1;
        end
    end {filetyp} ;


  procedure settyp;

{ Syntactic routine to parse a set type.

  Productions:

  set-type = "set" "of" base-type  .

  base-type = ordinal-type.

  Packed sets are allocated the exact number of bits needed, while
  ordinary sets are allocated an integral number of addressing units.
  If a set is not packed, and the size is greater than a word, the
  set is aligned on a word boundary.
}

    var
      m: integer; {number of members of the set}
      newbasetype: index; {type of set base}
      f: entryptr; {access to newbasetype}


    begin {settyp}
      gettoken;
      verifytoken(ofsym, nooferr);
      gettyp(follow, newbasetype);
      if bigcompilerversion then f := @(bigtable[newbasetype]);
      if not (f^.typ in [none, scalars, bools, chars, subranges]) then
        warnbefore(badsetbase)
      else if (lower(f) < 0) or (upper(f) > maxsetord) then
        warnbefore(bigsetbase);
      stripsubrange(newbasetype);
      if bigcompilerversion then f := @(bigtable[newbasetype]);
      if f^.typ = ints then m := maxsetord + 1
      else m := upper(f) + 1;
      enterform(sets, resulttype, resulp);
      with resulp^ do
        begin
        constructedset := false;
        basetype := newbasetype;
        packedflag := packflag;
        bitaddress := packflag;
        if packedflag then
          begin
          if (targetmachine = mc68000) and (m > bitsperunit) then
            size := forcealign(m, 1 { a byte }, true)
          else size := roundpackedsize(m, true);
          if size > bitsperunit then align := setalign * bitsperunit
          else align := 1;
          end
        else
          begin
          size := (m + bitsperunit - 1) div bitsperunit;
          if size = unitsize then align := unitsize
          else align := setalign;
          size := forcealign(size, align, false);
          end;
        end;
    end {settyp} ;



  procedure recordtyp;

{ Syntactic routine to parse a record type.

  Productions:

  record-type = "record" [ field-list [ ";" ] ] "end" .

  As far as the compiler is concerned, the record type simply
  defines a new scope, and all of the real work is done in the
  "fieldlist" routine.
}

    var
      id: integer; {scope id for this record}
      dbgscope: p_symbolindex; {index of record's debug entry}
      p: entryptr;

    procedure fieldlist(follow: tokenset; {legal following symbols}
                        tag: index; {tag field for this field, if any}
                        tagl: index; {list of label entries, if any}
                        lastv: index; {last variant parsed}
                        startsize: addressrange; {size at start of list}
                        var result: index {resulting type});

{ Syntactic routine to parse a fieldlist.

  Productions:

  field-list = fixed-part [ ";" variant-part ] | variant-part  .

  fixed-part = record-section [* ";" record-section *]  .

  record-section = identifier-list ":" type-denoter  .

  variant-part = "case" variant-selector "of" variant
        [* ";" variant *]  .

  variant-selector = [ identifier ":" ] type  .

  variant = case-constant-list ":" "(" [ fieldlist [ ";" ] ] ")" .

  case-constant-list = constant [* "," constant *]  .

  Each field-list is represented as a formentry with an index for the
  first and last field entries in the name table.  Each variant is
  treated as a separate fieldlist, and all variants for a fieldlist
  are linked through the "nextvariant" field in the formentry.  The
  "firstvariant" field is the start of this chain.  The labels for
  a particular variant are kept on a list rooted in "firstlabel", and
  a pointer to the tagfield is kept in "tagfield".

  Access to the fields is through the normal lookup using the "fieldid"
  as a scope id, or by sequential scan of the name entries when necessary,
  as for structured constants or writing debugger tables.  Note that
  not all name entries between "firstfield" and "lastfield" are field
  identifiers, only the ones with the proper scope id.

  "Tagl" is the list of case labels for this variant (or zero), and
  "lastv" is the last variant for linking (or zero).

}

      var
        resulp: entryptr; {access to result}
        localresult: tableentry; {local copy of "result" to simplify code}
        a: alignmentrange; {temp value of alignment for tagfield}
        latestlabel: index; {last variant label parsed}
        latestvariant: index; {last variant parsed}
        tagcount: unsignedint; {number of tags defined}
        tagmembers: unsignedint; {number of elements in tag type}
        lowesttag, highesttag: integer; {for checking that all tags defined}
        casetype: index; {type of tagfield}
        caseptr: entryptr; {access to casetype}
        f1: index; {temp for fieldlist within variant}
        f1ptr: entryptr; {access to f1}
        t: index; {tag field name entry (0 for undiscriminated)}
        p: entryptr; {used for name access}
        oldany: boolean; {old value of anyfile}
        firstdbglabel: p_symbolindex; {index of first variant label for debug}

      procedure onelabel;

{ Syntactic routine to parse a single variant label (case-constant in
  terms of the latest draft standard).

  Production:

  case-constant = constant.

  All labels for a given variant are chained together and the chain is
  rooted in the formentry for that variant.
}

        var
          t: index; {temp ptr of various uses}
          f: entryptr; {used for access to forms}
          labelval: operand; {constant label value}


        procedure checklabs(header: index {start of a list of labels} );

{ Check all of the labels on a particular label list to see if there are any
  with the same value as the current label.
}

          var
            f: entryptr; {used to access labels}


          begin {checklabs}
            while (header <> 0) do
              begin
              if bigcompilerversion then f := @(bigtable[header]);
              with f^ do
                begin
                if (labelval.cvalue.intvalue = varlabvalue) then
                  warnbefore(dupcaselabel);
                header := nextvarlab
                end;
              end;
          end {checklabs} ;


        begin {onelabel}

          tagcount := tagcount + 1;
          constant(follow + [comma, colon, lpar, rpar], true, labelval);
          with labelval.cvalue do
            if (intvalue < lowesttag) or (intvalue > highesttag) then
              warnnonstandard(badcasetags);
          checklabs(latestlabel);

          t := latestvariant;
          while (t <> 0) do
            begin
            if bigcompilerversion then f := @(bigtable[t]);
            with f^ do
              begin
              t := nextvariant;
              checklabs(firstlabel);
              end;
            end;

          enterform(variantlabs, t, f);
          with f^ do
            begin
            packedflag := packflag;
            bitaddress := packflag;
            nextvarlab := latestlabel;
            varlabtype := labelval.typeindex;
            varlabvalue := labelval.cvalue.intvalue;
            end;

          latestlabel := t;
          if (casetype <> noneindex) and (labelval.typeindex <> casetype) then
            warnbefore(badcaselab);

        end {onelabel} ;


      begin {fieldlist}
        oldany := anyfile;
        anyfile := false;
        enterform(fields, result, resulp);
        localresult := resulp^;
        with localresult do
          begin
          packedflag := packflag;
          bitaddress := packflag;
          fieldid := id;
          {link onto variant list}
          nextvariant := lastv;
          firstlabel := tagl;
          firstvariant := 0;
          tagfield := 0;
          typ := fields;
          firstfield := tabletop + 1;
          variablelist(follow + [rpar, casesym, endsym], [rpar, endsym], id,
                       dbgscope, startsize, a, fieldname, false, packflag, false);
          lastfield := tabletop + 1;
          repeat
            lastfield := lastfield - 1;
            if bigcompilerversion then p := @(bigtable[lastfield]);
          until (p^.name = fieldid) or (lastfield < firstfield);
          containsfile := anyfile;
          anyfile := oldany;

          {Now parse a variant-part}

          if token = casesym then
            begin
            gettoken;
            if (token = ident) and (nexttoken.token = colon) then
              begin
              onevar(id, fieldname, t, false);
              if bigcompilerversion then p := @(bigtable[t]);
              p^.varianttag := true;
              tagfield := t; { tagfield is in a packed record }
              gettoken;
              end;
            casetype := noneindex;
            if token in
               [uparrow..stringsym, nilsym, intconst..stringconst, plus,
               minus, lpar] then
              warnnonstandard(notypenameerr);
            gettyp(follow + [ofsym, endsym, ident, colon], casetype);
            if tagfield <> 0 then
              begin {allocate a tag field}
              if bigcompilerversion then caseptr := @(bigtable[casetype]);
              a := max(a, alignmentof(caseptr, packflag));
              alloconevar(tagfield, casetype, fieldname, startsize,
                          alignmentof(caseptr, packflag), sizeof(caseptr,
                          packflag), false, packflag);
              end;

            if bigcompilerversion then caseptr := @(bigtable[casetype]);
            lowesttag := lower(caseptr);
            highesttag := upper(caseptr);
            stripsubrange(casetype);
            if not (caseptr^.typ in
               [subranges, ints, chars, bools, scalars, none]) then
              warn(badcasetyp);
            verifytoken(ofsym, nooferr);
            latestvariant := 0;
            tagmembers := highesttag - lowesttag + 1;
            tagcount := 0;
            size := startsize;
            while token in
                  [comma, colon, lpar, semicolon, plus, minus, ident, nilsym,
                  intconst..stringconst] do
              begin {parse a single variant}
              if token in
                 [plus, minus, ident, nilsym, intconst..stringconst, comma,
                 colon, lpar] then
                begin
                latestlabel := 0;
                onelabel;
                while token in
                      [plus, minus, ident, nilsym, intconst..stringconst,
                      comma] do
                  begin
                  verifytoken(comma, nocommaerr);
                  onelabel;
                  end;
                verifytoken(colon, nocolonerr);
                verifytoken(lpar, nolparerr);
                fieldlist(follow + [comma, colon, rpar], tagfield,
                          latestlabel, latestvariant, size, f1);
                if bigcompilerversion then f1ptr := @(bigtable[f1]);
                containsfile := containsfile or f1ptr^.containsfile;
                f1ptr^.packedflag := packflag;
                f1ptr^.bitaddress := packflag;
                if f1ptr^.size > startsize then startsize := f1ptr^.size;
                a := max(a, alignmentof(f1ptr, packflag));
                latestvariant := f1;
                verifytoken(rpar, norparerr);
                end;
              if token = semicolon then gettoken
              else
                verify([endsym, rpar], follow + [comma, colon],
                       nosemiheaderr);
              end;
            firstvariant := latestvariant;
            if tagmembers <> tagcount then warnnonstandard(badcasetags);
            end;
          size := roundpackedsize(startsize, packflag);
          align := a;
          end;
        anyfile := oldany;
        if bigcompilerversion then resulp := @(bigtable[result]);
        resulp^ := localresult;
      end {fieldlist} ;


    begin {recordtyp}
      gettoken;
      if (lastid >= totalscopes) or (lastscope >= totalscopes) then
        fatal(manyscopes); {the last value of totalscopes is not used.}
      lastid := lastid + 1;
      lastscope := lastscope + 1;
      id := lastid;
      fieldlist(follow + [endsym], 0, 0, 0, 0, resulttype);
      verifytoken(endsym, noenderr);
    end {recordtyp} ;


  procedure stringtyp;

{ Process string type, a non-standard and (many of us think) ugly extension
  enthusiastically embraced by the unwashed masses of PC-land.

  Productions:

  string-type = "string" "[" constant "]".
}

    var
      value: operand; {value returned by constant}
      t: index; {temp index for entering string type}
      t1: entryptr; {Temp ptr for entering string type}


    begin {stringtyp}
      gettoken;
      if token = lpar then
        begin
        warn(nolbrackerr);
        gettoken
        end
      else verifytoken(lbrack, nolbrackerr);
      constant(follow + [rbrack], true, value);
      if (value.typeindex <> intindex) or (value.cvalue.intvalue <= 0) or
         (value.cvalue.intvalue > 255) then
        begin
        warnbefore(badstringindex);
        resulttype := noneindex;
        end
      else
        begin
        enterform(subranges, t, t1);
        with t1^ do
          begin
          size := targetintsize;
          align := intalign;
          parenttype := intindex;
          parentform := ints;
          lowerord := 0;
          upperord := value.cvalue.intvalue;
          end;
        enterform(strings, resulttype, resulp);
        with resulp^ do
          begin
          packedflag := true;
          bitaddress := true;
          containsfile := false;
          elementtype := chartypeindex;
          stringtype := false;
          arraymembers := value.cvalue.intvalue + 1;
          indextype := t;
          size := arraymembers div (bitsperunit div stringeltsize);
          if arraymembers mod (bitsperunit div stringeltsize) <> 0 then
            size := size + 1;
          size := size * bitsperunit;
          elementsize := stringeltsize;
          align := bitsperunit;
          end;
        end;
      if token = rpar then
        begin
        warn(norbrackerr);
        gettoken
        end
      else verifytoken(rbrack, norbrackerr);
    end {stringtyp} ;


  begin {gettyp}
    if token = packedsym then
      begin
      packflag := true;
      gettoken
      end
    else packflag := false;
    if token in begstructset then
      begin
      case token of
        uparrow: ptrtyp;
        arraysym: arraytyp;
        filesym: filetyp;
        setsym: settyp;
        recordsym: recordtyp;
        stringsym: stringtyp;
        end;
      end
    else simpletyp;
    verify1(follow, badtypesyntax);
    if bigcompilerversion then resulp := @(bigtable[resulttype]);
    anyfile := anyfile or resulp^.containsfile;
  end {gettyp} ;




procedure labeldefinition;

{ Syntactic routine to parse a label-declaration-part.

  Productions:

  label-declaration-part = "label" label [* "," label *] ";"  .

  Label declarations are recorded in labelentry's which are linked
  on a list rooted in "labellist" in the display entry.

  Each label has an internal label value assigned from a counter.

  The source line and column of the label are recorded to allow
  diagnostics if the label is not defined.

  Definednest is initialized to 0 (undefined)
  Maxlegalnest is initialized to maxint (any nest level)
}

  var
    t: labelptr; {temp ptr with various uses}
    value1: integer; {label value}


  procedure onelabeldef;

{ Process a single label definition, checking for duplicate label
  declarations and entering the declaration in the label list.

}


    begin {onelabeldef}
      if token <> intconst then warnbetween(nolabelerr)
      else
        begin
        value1 := thistoken.intvalue;
        if value1 > maxstandardlabel then warn(biglabelerr);
        with display[level] do
          begin
          searchlsection(value1, labellist, t);
          if t <> labelflag then warn(duplabeldef)
          else
            begin
            new(t);
            with t^ do
              begin
              labelvalue := value1;
              nextlabel := labellist;
              labellist := t;
              maxlegalnest := maxint;
              definednest := 0;
              internalvalue := lastlabel;
              { code generators need two entry points for Pascal labels }
              lastlabel := lastlabel - 2;
              nonlocalref := false;
              with thistoken do
                begin
                labelline := line;
                labelcolumn := (left + right) div 2;
                end;
              end
            end;
          end;
        gettoken;
        end;
    end {onelabeldef} ;


  begin {labeldefinition}
    gettoken;
    onelabeldef;
    while token in [comma, intconst] do
      begin
      verifytoken(comma, nocommaerr);
      onelabeldef
      end;
    verifytoken(semicolon, nosemiheaderr);
    verify1(neverskipset, baddeclerr);
  end {labeldefinition} ;


procedure constantdefinition;

{ Syntactic routine to parse a constant definition part.

  Productions:

  constant-definition-part = "const" constant-definition ";"
        [* constant-definition ";" *]

  constant-definition = identifier "=" constant  .

  Constants simply have their value entered in the name table.
}

  var
    t: index; { constant identifier entry}
    p, tp: entryptr; {used for nametable access}
    newvalue: operand; {value of the constant}
    startpos: integer; {position of first var in source file}
    startline: integer; {line number of first var}

  begin {constantdefinition}

    startpos := thistoken.filepos;
    startline := thistoken.line;
    gettoken;
    repeat
      enterlocalident(t, noname);
      verifytoken(eql, noeqlerr);
      constant(neverskipset, true, newvalue);
      if bigcompilerversion then p := @(bigtable[t]);
      with p^ do
        begin
        namekind := constname;
        consttype := newvalue.typeindex;
        constvalue := newvalue.cvalue;
        if consttype = nilindex then warnnonstandard(badconsterr);
        end;
      verifytoken(semicolon, nosemiheaderr);
      verify1(constfollowset + neverskipset, baddeclerr);
    until not (token in constfollowset);
  end {constantdefinition} ;


procedure typedefinition;

{ Syntactic routine to parse a type-definition-part.

  Productions:

  type-definition-part = "type" type-definition ";"
        [* type-definition ";" *]  .

  type-definition = identifier "=" type-denoter  .

  As each type is parsed its formentry is created in main store,
  and its nameentry contains the index of the formentry.
}

  var
    t: index; { name of type being defined }
    p: entryptr; { used for name access }
    f: index; {form entry for type being defined}
    startpos: integer; {position of first var in source file}
    startline: integer; {line number of first var}

  begin {typedefinition}

    gettoken;
    repeat
      startpos := thistoken.filepos;
      startline := thistoken.line;
      enterlocalident(t, undeftypename);
      verifytoken(eql, noeqlerr);
      gettyp([eql, ident, semicolon, uparrow..stringsym, nilsym,
             intconst..stringconst, plus, minus, lpar], f);
      if bigcompilerversion then p := @(bigtable[t]);
      with p^ do
        begin
        namekind := typename;
        typeindex := f;
        refdefined := false;
        end;
      verifytoken(semicolon, nosemiheaderr);
      verify1(typefollowset + neverskipset, baddeclerr);
    until not (token in typefollowset);
  end {typedefinition} ;


procedure vardefinition(sharedvar: boolean {if 'shared' section});

{ Syntactic routine to parse a variable-declaration-part.

  Productions:

  variable-declaration-part = "var" variable-declaration ";"
               [* variable-declaration ";" *]  .

  variable-declaration = identifier [* "," identifier *] ":"
        type-denoter  .

  Almost all of the work is actually done in "variablelist"
}

  var
    a: alignmentrange; {default alignment}


  begin {vardefinition}
    gettoken;
    with display[level] do
      variablelist(neverskipset, [], blockid, dbgscope, blocksize, a,
                   varname, true, false, sharedvar);
  end {vardefinition} ;




procedure changeparamids(firstparam: index; {entry of first parameter}
                         lastparam: index; {might be less than firstparam}
                         n: integer {scope id desired} );

{ Scan down the parameters of a procedure and change their scope to the
  specified scope.  This is used to change forward parameter declarations
  to the current scope, or to make them inaccessable after the declaration.
  Skips over items with name = deadscope, to avoid rejuvenating killed
  parameters nested within procedure parameters.
}

  var
    p: entryptr; {provides nametable access for parameters}
    i: index; {for stepping through parameters}


  begin {changeparamids}
    for i := firstparam to lastparam do
      begin
      if bigcompilerversion then p := @(bigtable[i]);
      if not p^.form and (p^.name <> deadscope) then p^.name := n;
      end;
  end {changeparamids} ;


procedure getfunctiontype(functiondefinition: boolean; {in func def}
                          forwardbody: boolean; {body of forward dec}
                          var returntype: index {func type} );

{ Syntactic routine to parse a function result type if necessary.

  Production:
  function-type = [ ":" type-identifier ]  .

  If we are parsing a function, and it is not a forward body for
  a previously defined function, this routine gets the result type.

  A procedure or forward function will have type "noneindex".

  If the type is not legal for a function, an error will be emitted.
}

  var
    f: entryptr; {for access to returntype}
    t: index; {for finding type identifier}
    i: levelindex; {for stepping through levels looking for it}

  begin {getfunctiontype}
    returntype := noneindex;
    if (functiondefinition and not forwardbody) or (token = colon) then
      begin
      if not functiondefinition then warn(badcolonerr)
      else if forwardbody then warn(dupfwdresult);
      verifytoken(colon, nocolonerr);
      if token = ident then
        begin
        i := displaytop; {Name must be found in enclosing scopes, not current}
        repeat
          i := i - 1;
          searchsection(display[i].blockid, t);
        until (i = 0) or (t <> 0);
        if bigcompilerversion then f := @(bigtable[t]);
        if f^.namekind = typename then returntype := f^.typeindex
        else warn(notypenameerr);
        gettoken;
        end
      else
        begin
        warn(notypenameerr);
        gettyp(neverskipset + [rpar], returntype);
        end;
      if bigcompilerversion then f := @(bigtable[returntype]);
      if (not (f^.typ in legalfunctypes)) and
         (switchcounters[standard] > 0) then
        warn(badfunctype);
      end;
  end {getfunctiontype} ;


procedure parameterdefinition(var paramsize: addressrange; {size of parms}
                              follow: tokenset {legal follow syms} );

{ Syntactic routine to parse a parameter list.

  Productions:

  formal-parameter-list = "(" formal-parameter-section
        [* ";" formal-parameter-section *] ")"  .

  Parameters are parsed and allocated at the top of the frame for
  the procedure, in such an order that they are pushed onto the stack
  in the order in which they are declared.

  Each value parameter is allocated the space for a value, each var
  parameter has space for a pointer to the parameter, and each
  routine parameter has space for the routine address and a pointer
  for the static link.
}


  procedure oneparampiece;

{ Syntactic routine to parse one formal parameter section.

  Productions:

  formal-parameter-section = value-parameter-specification |
        variable-parameter-specification |
        routine-parameter-specification  .

  value-parameter-specification = variable-list  .

  variable-parameter-specification = "var" variable-list  .

}


    procedure routineparam(routinekind: nametype {parameter kind} );

{ Syntactic routine to parse a routine-parameter-section.

  Productions:

  routine-parameter-section = ( "procedure" | "function " )
        [ parameter-list ] [ function-type ] ";"  .

  Parameters are assigned a new scope id, and will be unlinked from the
  key map on exit from the actual routine block.  They will, however,
  remain in the name table for checking when a routine is passed as a
  parameter.
}

      var
        routineindex: index; {entry for the parameter routine}
        p: entryptr; {used for name access}
        returntype: index; {type if function}
        intleveldummy: boolean; {actually has double use}
        sizedummy: addressrange; {dummy param to parameterdefinition}
        t: integer; {temp storage for last id}


      begin {routineparam}
        gettoken;
        sizedummy := 0;
        onevar(lastid, routinekind, routineindex, false);
        t := lastid;
        if (paramlistid + 1) >= totalscopes then fatal(manyscopes); {the last
          value of totalscopes is not used.}
        lastid := paramlistid + 1;
        paramlistid := lastid;
        if token = lpar then
          parameterdefinition(sizedummy,
                              [colon, rpar, semicolon] + begparamhdr);
        if bigcompilerversion then p := @(bigtable[routineindex]);
        p^.lastinsection := true;
        p^.offset := paramsize;
        p^.length := procparamsize;
        paramsize := paramsize + procparamsize;
        p^.nextparamlink := tabletop;
        intleveldummy := routinekind = funcparam;
        getfunctiontype(intleveldummy, false, returntype);
        p^.vartype := returntype;
        lastid := t;
        changeparamids(routineindex + 1, tabletop, deadscope);
        verify([rpar, semicolon], begparamhdr + neverskipset, badparamerr);
        if token = semicolon then gettoken;
      end {routineparam} ;


    procedure conformantparam(paramkind: nametype; {kind of parameter}
                              var result: index {resulting parameter type} );

{ Parse a conformant array parameter declaration.
  Boundid's are allocated after the parameter.
  If we are unable to parse a boundid, a dummy entry is used, since the
  pass gets very confused later if the boundid entries are not,
  in fact, boundid's.
}

      var
        packflag: boolean; {true if packed conformant array}
        highid, lowid: index; {high and low bound id's}
        thisindextype: index; {index type for this array}
        p: entryptr; {for access to names and forms}
        resulp: entryptr; {for access to result form}
        elttype: index; {element type}
        lasttabletop: index; {initial value of tabletop}


      begin {conformantparam}
        packflag := false;
        if (token = packedsym) or (token = arraysym) then
          begin
          if switchcounters[level0] > 0 then warnnonstandard(notlevel0);
          if token = packedsym then
            begin
            packflag := true;
            gettoken;
            end;
          if token = arraysym then
            begin
            gettoken;
            if token = lpar then
              begin
              warn(nolbrackerr);
              gettoken;
              end
            else verifytoken(lbrack, nolbrackerr);
            end
          end
        else verifytoken(semicolon, nosemiheaderr);

        lowid := 0;
        lasttabletop := tabletop;
        if token in [intconst, charconst] then
          begin
          warn(novarerr);
          gettoken;
          end
        else onevar(lastid, boundid, lowid, false);
        if lowid = 0 then lowid := nullboundindex;
        verifytoken(dotdot, nodotdoterr);
        highid := 0;
        lasttabletop := tabletop;
        if token in [intconst, charconst] then
          begin
          warn(novarerr);
          gettoken;
          end
        else onevar(lastid, boundid, highid, false);
        if highid = 0 then highid := nullboundindex;
        verifytoken(colon, nocolonerr);
        if token <> ident then warn(notypenameerr);
        gettyp(follow + [semicolon, rbrack, ofsym, arraysym], thisindextype);
        if bigcompilerversion then p := @(bigtable[thisindextype]);
        if not (p^.typ in [none, chars, bools, scalars, subranges, ints]) then
          warn(badindex);

        if token = rpar then
          begin
          warn(norbrackerr);
          gettoken;
          end
        else if token <> semicolon then
          begin
          verifytoken(rbrack, norbrackerr);
          verifytoken(ofsym, nooferr);
          end;

        if packflag and (token <> ident) then warn(badpackconform);
        if token in [semicolon, packedsym, arraysym] then
          conformantparam(paramkind, elttype)
        else
          begin
          if token <> ident then warn(notypenameerr);
          gettyp(nextparamhdr + [semicolon], elttype);
          end;

        enterform(conformantarrays, result, resulp);
        with resulp^ do
          begin
          packedflag := packflag;
          size := 0;
          align := ptralign;
          lowbound := lowid;
          highbound := highid;
          indextype := thisindextype;
          elementtype := elttype;
          stringtype := false;
          if bigcompilerversion then p := @(bigtable[elttype]);
          containsfile := p^.containsfile;
          elementsize := arraysizeof(p, packflag);
{ the following test should follow the wry logic in arraywork }
          if packflag and (elementsize > bitsperunit * packingunit) then
            elementsize := arraysizeof(p, false) * bitsperunit;
          end;
        if bigcompilerversion then p := @(bigtable[lowid]);
        p^.sparelink := result;
        if bigcompilerversion then p := @(bigtable[highid]);
        p^.lastinsection := true;
        p^.sparelink := 0; {used only by lowid}
      end {conformantparam} ;


    procedure allocboundids(paramtype: index {parameter type} );

{ Allocate the boundid's for a conformant parameter.  This is a separate
  action because they must follow the parameter itself in order for the
  call to work.
}

      var
        elttype: index; {elementtype for this conformant array schema}
        indextype: index; {indextype for this schema}
        p: entryptr; {for access to paramtype}
        highid, lowid: index; {boundid names}
        indexlen: addressrange; {length of indextype}
        a: alignmentrange; {alignment for this type}
        align: alignmentrange; {parameter alignment (dummy)}

      begin {allocboundids}
        align := intalign;
        repeat
          if bigcompilerversion then p := @(bigtable[paramtype]);
          elttype := p^.elementtype;
          indextype := p^.indextype;
          lowid := p^.lowbound;
          highid := p^.highbound;
          if bigcompilerversion then p := @(bigtable[indextype]);
          getallocdata(p, boundid, false, paramsize, indexlen, a, align);
          alloconevar(lowid, indextype, boundid, paramsize, a, indexlen,
                      false, false);
          alloconevar(highid, indextype, boundid, paramsize, a, indexlen,
                      false, false);
          paramtype := elttype;
          if bigcompilerversion then p := @(bigtable[paramtype]);
        until p^.typ <> conformantarrays;
      end {allocboundids} ;


    procedure oneparamlist(paramkind: nametype {kind of parameter list} );

{ Syntactic routine to parse a value or variable parameter section.

  Productions:

  variablelist = identifier [* "," identifier *] ":"
                 typeidentifier | conformantspecification  .

  This cannot be handled by the normal variables procedure because of
  the strange processing required for conformant arrays.
}

      var
        a: alignmentrange; {alignment if this var}
        align: alignmentrange; {"parameter" alignment}
        first, last: index; {limits of parameters}
        paramtype: index; {type of these paramters}
        paramptr, p: entryptr; {used for access to table entries}
        t: index; {induction var}
        typelen: addressrange; {space allocated for the var}
        univflag: boolean; {true if universal parameter}
        rp: boolean; {true if parameter passed by reference}

      begin {oneparamlist}
        align := unitsize;
        verify([ident], nextparamhdr, novarerr);

        onevar(lastid, paramkind, first, false);
        last := first;
        while token in [ident, comma] do
          begin
          verifytoken(comma, nocommaerr);
          onevar(lastid, paramkind, last, false);
          end;

        paramtype := noneindex;

        verifytoken(colon, nocolonerr);

        univflag := false;

        if token in [packedsym, arraysym] then
          begin
          if paramkind = varparam then paramkind := varconfparam
          else paramkind := confparam;
          conformantparam(paramkind, paramtype);
          end
        else if token in [ident, univsym] then
          begin
          if token = univsym then
            begin
            if paramkind <> varparam then warn(baduniv);
            gettoken;
            univflag := true;
            end;
          search(paramtype);
          if bigcompilerversion then paramptr := @(bigtable[paramtype]);
          if paramptr^.namekind <> typename then
            begin
            warn(notypenameerr);
            gettoken;
            paramtype := noneindex;
            end
          else gettyp(nextparamhdr + [semicolon, rpar], paramtype);
          end
        else warn(notypenameerr);

        if bigcompilerversion then paramptr := @(bigtable[paramtype]);
        if paramptr^.containsfile and (paramkind in [param, confparam]) then
          warn(novaluefile);

        for t := first to last do
          begin
          if bigcompilerversion then paramptr := @(bigtable[paramtype]);
          getallocdata(paramptr, paramkind, false, paramsize, typelen, a,
                       align);
          if (paramkind = param) then
            if (typelen > maxparambytes) and
               (paramptr^.typ in [sets, arrays, fields{, strings}]) then
              begin
              typelen := ptrsize;
              a := ptralign;
              rp := true;
              end
            else rp := false
          else rp := true;
          alloconevar(t, paramtype, paramkind, paramsize, a, typelen, rp,
                      false);
          if univflag then
            begin
            if bigcompilerversion then p := @(bigtable[t]);
            p^.univparam := true;
            end;
          end;
        if paramkind in [varconfparam, confparam] then
          allocboundids(paramtype);
        if bigcompilerversion then paramptr := @(bigtable[last]);
        paramptr^.lastinsection := true;
        paramptr^.nextparamlink := tabletop;
        if token = semicolon then gettoken
        else
          verify(nextparamhdr, follow + begtypset + [comma, colon, semicolon],
                 nosemiheaderr);

        paramsize := forcealign(paramsize, stackalign, false);
      end {oneparamlist} ;


    begin {oneparampiece}
      verify(begparamhdr, [rpar, semicolon, comma], badparamerr);
      case token of
        functionsym: routineparam(funcparam);
        varsym:
          begin
          gettoken;
          oneparamlist(varparam)
          end;
        ident: oneparamlist(param);
        proceduresym: routineparam(procparam);
        end;
    end {oneparampiece} ;


  begin {parameterdefinition}
    gettoken;
    oneparampiece;
    while token in
          ([comma, semicolon, functionsym, proceduresym, varsym, ident]) do
      begin
      if lasttoken.token <> semicolon then warnbetween(nosemiheaderr);
      oneparampiece;
      end;
    if lasttoken.token = semicolon then warnbefore(badparamerr);
    verifytoken(rpar, norparerr);
    verify1(neverskipset + follow, badparamerr);
  end {parameterdefinition} ;


procedure procdefinition;

{ Syntactic routine to parse a procedure or function definition.

  Productions:

  routine-declaration = ( "procedure" | "function" ) identifier
        [ formal-parameter-definition ] function-type ";"
        ( block | directive ) ";"

  The routine name is defined with the scope id of the enclosing block,
  and enters a new scope for the parameters.  This scope continues through
  the end of the procedure block (if any).  Each procedure defined is also
  described in a separate table called the "proctable" which saves basic
  data on the procedure.  This is kept in memory because it has frequent
  write access, and because it is larger than the other name entries, and
  would expand the nameentry unnecessarily.

  If the declaration is for a forward or external procedure, the parameters
  are saved in the name table with the scope id for the procedure, and this
  id will not be reused as there is no block exit.  When the forward body
  is encountered, the scope id's of these parameters is changed to the scope
  id of the body.  Upon exit from the body, there is no way to unlink these
  forward defined parameters as is done for normal routines, so they have
  their scope id changed to a special value which will not be used by
  normal scopes.  This effectively makes them inaccessable.

  When a function returns a real result, if the function does not use
  Pascal linkage, then this must be communicated to genblk to allow proper
  access to the value coming back in a register (i.e. is it r0 or f0?).
}

  var
    functiondefinition: boolean; {parsing a function}
    forwardbody: boolean; {proc was forward defined}
    procptr: entryptr; {provides access to procedure name entry}
    procindex: index; {procedure name entry}
    paramindex: index; {start of parameter entries}
    returntype: index; {function result type formentry}
    directive: standardids; {which directive, forward etc}
    directiveindex: index; {and symbol table index thereof}
    proctemp: proctableindex; {saves procref while block is compiled}
    funcline: integer; {line number where function defined}
    funccol: columnindex; {col where func defined (for not assigned msg)}
    f: entryptr; {used for access to returntype}
    hasparameters: boolean; {true if there are parameters}


  begin {procdefinition}
    hasparameters := false;
    functiondefinition := token = functionsym;
    gettoken;
    forwardbody := false;
    funcline := thistoken.line;
    funccol := (thistoken.left + thistoken.right) div 2;
    if token = ident then
      begin
      searchlevel(procindex);
      if procindex <> 0 then
        begin
        if bigcompilerversion then procptr := @(bigtable[procindex]);
        with procptr^ do
          begin
          forwardbody := namekind in [externalproc, externalfunc, forwardproc,
                         forwardfunc];
          if functiondefinition and
             (namekind in [externalproc, forwardproc]) then
            warn(fwdprocfuncerr)
          else if not functiondefinition and
                  (namekind in [externalfunc, forwardfunc]) then
            warn(fwdfuncprocerr);
          end;
        end;
      end;
    if not forwardbody then
      begin
      enterlocalident(procindex, noname);
      if bigcompilerversion then procptr := @(bigtable[procindex]);
      procptr^.procref := newproc;
      proctable[procptr^.procref].backlink := display[level].blockref;
      end
    else gettoken;
    if level = maxlevel then fatal(levelerr);
    if bigcompilerversion then procptr := @(bigtable[procindex]);
    enterblock(level + 1, procindex, procptr^.procref);
    if token = lpar then
      begin
      hasparameters := true;
      if forwardbody then warn(dupfwdparam);
      paramlistid := lastid;
      parameterdefinition(display[level + 1].paramsize, [colon]);
      end;
    paramindex := tabletop;
    getfunctiontype(functiondefinition, forwardbody, returntype);
    verifytoken(semicolon, nosemierr);
    if bigcompilerversion then procptr := @(bigtable[procindex]);
    with procptr^ do
      begin
      proctable[procref].globaldeath := false;
      proctable[procref].charindex := charindex;
      proctable[procref].charlen := min(maxprocnamelen, charlen);
      if bigcompilerversion then f := @(bigtable[returntype]);
{
      Reset the calling linkage to Pascal-2.  This is needed
      for non-pascal declared routines for which a body is present.
      Not needed on systems that support non-pascal procedure bodies.
      Remember, calllinkage is initialized to pascal2call in newproc.
      Interruptcall procedures are an exception.
}
      if not (((targetmachine = ns32k) and (unixtarget = umax)) or
              (targetmachine = i80386) or
              (targetopsys = vms) or
              (targetmachine = iapx86) or (targetopsys = apollo) or
              (proctable[procref].calllinkage = interruptcall)) then
        proctable[procref].calllinkage := pascal2call;
      if forwardbody then
        begin
        display[level + 1].paramsize := savedparamsize;
{
        Override the initial blocksize of this function if value must be
        returned in registers.  In that case we allocate the
        function space in the local variables area.
}
        if (targetmachine = iAPX86) or (targetmachine = i80386) or
           (targetopsys = apollo) or (targetopsys = vms) then
          display[level + 1].blocksize := proctable[procref].registerfunction;
        end
      else
        begin
        proctable[procref].realfunction :=
          ((f^.typ = reals) or (f^.typ = doubles)) and functiondefinition;
        proctable[procref].intlevelrefs := false;
        proctable[procref].level := level + 1;
        funclen := f^.size;
        functype := returntype;
        paramlist := paramindex
        end;
      if token = ident then
        begin
        search(directiveindex);
        if bigcompilerversion then f := @(bigtable[directiveindex]);
        if f^.namekind = directivename then
          begin
          directive := f^.procid;
          if directive = forwardid then
            begin
            if forwardbody then warn(dupforward)
            else enterundef(procindex);
            if functiondefinition then namekind := forwardfunc
            else namekind := forwardproc
            end
          else
            begin
            if forwardbody then warn(dupforward);
            { external, nonpascal, fortran, interrupt }
            if functiondefinition then namekind := externalfunc
            else namekind := externalproc;

            if directive = nonpascalid then
              proctable[procref].calllinkage := nonpascalcall
            else if directive = fortranid then
              proctable[procref].calllinkage := fortrancall
            else if not functiondefinition and (directive = interruptid) then
              begin
              proctable[procref].calllinkage := interruptcall;
              if hasparameters or (level <> 1) then warn(badinterruptproc);
              end;
            anyexternals := true;
            proctable[procref].externallinkage := true;
            proctable[procref].globaldeath := true;
            if level <> 1 then warn(badxdef);
            if targetmachine = iAPX86 then
              begin
              if functiondefinition and
                 ((directive = nonpascalid) and (funclen <= 4)) or
                 ((directive = fortranid) and (funclen <= 4) and
                  not proctable[procref].realfunction) then
                proctable[procref].registerfunction := funclen;
              end
            else if targetmachine = i80386 then
              begin
              if functiondefinition and
                 (directive = nonpascalid) and (funclen <= 4) then
                proctable[procref].registerfunction := funclen;
              end
            else if targetopsys = apollo then
              begin
              if functiondefinition and
                 (directive = nonpascalid) and (funclen <= 4) then
                proctable[procref].registerfunction := funclen;
              end
            else if targetopsys = vms then
              begin
              if functiondefinition and
                 (directive = nonpascalid) and ((funclen <= 4) or
                 (funclen = 8)) then
                proctable[procref].registerfunction := 
                  forcealign (funclen, 4, false);
              end;
            end;
          end
        else warn(baddirective);
        gettoken;
        with display[level + 1] do
          begin
          savedparamsize := paramsize;
          display[level].namesdeclared := display[level].namesdeclared +
                                          namesdeclared;
          display[level].highestkey := max(display[level].highestkey,
                                           highestkey);
          end;
        end
      else
        begin {no directive}
        if functiondefinition then namekind := funcname
        else namekind := procname;

        funcassigned := not functiondefinition;
        if forwardbody then changeparamids(procindex + 1, paramlist, lastid)
        else
          begin
          { bump lastscope as parameter list lives a reality separate than
            the procedure's block!
          }
          if lastscope >= totalscopes then fatal(manyscopes);
          lastscope := lastscope + 1;
          display[level + 1].scopeid := lastscope;
          end;
        display[level + 1].oldtabletop := tabletop;
        if procref <= cseregions then
          begin
          cseregiontable[procref, false].low := maxaddr;
          cseregiontable[procref, false].high := 0;
          cseregiontable[procref, true].low := maxaddr;
          cseregiontable[procref, true].high := 0;
          end;
        level := level + 1;
        block;
        if bigcompilerversion then procptr := @(bigtable[procindex]);
        level := level - 1;
        if not procptr^.funcassigned then
          warnat(nofuncass, funcline, funccol);
        proctable[procptr^.procref].bodydefined := true;
        directiveindex := procptr^.paramlist; {cache buffer...}
        if forwardbody then
          changeparamids(procindex + 1, directiveindex, deadscope);
        for directiveindex := procindex + 1 to directiveindex do
          begin
          if bigcompilerversion then f := @(bigtable[directiveindex]);
          if not f^.form then f^.modified := f^.parammodified;
          end;
        end;
      end;
    displaytop := level;
    verifytoken(semicolon, nosemiprocerr);
  end {procdefinition} ;



procedure block;

{ Syntactic routine to parse a block.  This also contains most of
  the rest of the syntax analyzer.

  Productions:
  block = [* label-declaration-part | constant-definition-part |
        type-definition-part | variable-declaration-part |
        procedure-and-function-declaration-part *]
        statement-part  .

}

  var
    firstsection: boolean; {set if first data declaration at this level}
    firstprocdefined: boolean; {false until a proc/func is defined at this
                                level}


  begin {block}
    verify1(begblockset, blockstarterr);
    firstsection := true;
    firstprocdefined := false;
    repeat
      if not firstsection then warnnonstandard(scrambledblkerr);
      if token = labelsym then labeldefinition;
      if token = constsym then constantdefinition;
      if token = typesym then typedefinition;
      if token = sharedsym then
        begin
        if level > 1 then warn(badsharedvar);
        vardefinition(true);
        end;
      if token = varsym then
        begin
        if (level > 1) and firstprocdefined then warn(scrambledblkerr);
        vardefinition(false);
        end;
      firstsection := false;
      while token in [functionsym, proceduresym] do
        begin
        listundeftypes;
        if not firstprocdefined then fixupparamoffsets(level > 1);
        firstprocdefined := true;
        procdefinition;
        end;
    until not (token in blockheadset);
    if not firstprocdefined or (level <= 1) then fixupparamoffsets(true);
    listundeftypes;
    listundefprocs;
    genstmt(begblk);
    genint(thistoken.line);
    with display[level] do
      begin
      genint(blockref);
      if level = 1 then globalsize := blocksize;
      genint(paramsize);
      genint(blocksize);
      genint(thistoken.baseline);
      end;
    loopfactor := 0; { not in a loop }
    anynonpascalcalls := false;
    anynonlocallabels := false;
    forsp := 0;
    nowdebugging := (switchcounters[debugging] > 0) or
                    (switchcounters[profiling] > 0);

    if (level > 1) or (switchcounters[mainbody] > 0) or (token <> eofsym) then
      begin
      if level = 1 then
        begin
        proctable[0].bodydefined := true; { use this entry for main body }
        if (switchcounters[mainbody] <= 0) and (token <> eofsym) then
          warn(extrastmterr);
        end;
      body.body;
      verifytoken(endsym, noenderr);
      end;
    verify1(neverskipset + [dot], blockenderr);

    listundeflabels;
    genstmt(endblk);
    exitblock(level);
  end {block} ;



procedure initanalys;

{ Initialize all tables, etc.
}

  type
    standardstring = packed array [1..9] of char; {used for standard ids}
    kludgerecord =
      record
        case boolean of
          false: (p: integer {targep} {nil pointer value} );
          true: (b: packed array [1..32] of hostfilebyte);
      end;

  var
    f: index; {used in creating forms}
    fptr: entryptr; {used to access forms being created}
    p: entryptr; {used to access name entries}
    i: integer; {Misc induction vars}
    kludge: kludgerecord; {for reversing bytes of NIL constant}
    b: hostfilebyte; {temp for reversing bytes}


  procedure enterdebuggerid(letters: standardstring; {standard id}
                            length: integer {length of id} );

{ Write a standard identifier to the debug file.
}

    var
      i: integer; {induction variable}


    begin {enterdebuggerid}
      if switcheverplus[symboltable] then
        begin
        if scanalys then p^.charindex := stringtabletop + 1
        else p^.charindex := stringtablelimit - stringfilecount + 1;
        p^.charlen := length;
        for i := 1 to length do
          begin
          if scanalys then
            begin
            stringtabletop := stringtabletop + 1;
            stringtable^[stringtabletop] := letters[i];
            end
          else
            begin
            stringtablelimit := stringtablelimit + 1;
            stringblkptr^[nextstringfile] := ord(letters[i]);
            putstringfile;
            end;
          end;
        end;
    end {enterdebuggerid} ;


  procedure enterstandardid(id: standardids; {key for std id}
                            n: nametype; {kind of entry}
                            l: addressrange; {length of the name}
                            f: index {type if meaningful} );

{ Make a name table entry for a standard identifier.  "id" specifies the
  standard id, and the other vars give necessary data
}

    var
      usearly, uslate: 0..maxusint; {force unsigned compares}
      i: integer; {intermediate check result}


    begin {enterstandardid}
      usearly := early;
      uslate := late;
      tabletop := tabletop + 1;
      case targetopsys of
        vdos: i := standardidtable[id];
        unix:
          case unixtarget of
            umax: i := standardidtable[id];
            otherwise
              i := standardidtable[id] * ord((today >= usearly) and
                                             (today <= uslate));
            end;
        otherwise
          i := standardidtable[id] * ord((today >= usearly) and
                                         (today <= uslate));
        end;
      keymap[i] := tabletop;
      if bigcompilerversion then p := @(bigtable[tabletop]);
      with p^ do
        begin
        form := false;
        name := lastid;
        lastoccurrence := lastscope;
        nextname := 0;
        namekind := n;
        charindex := 1;
        charlen := 0;
        with display[displaytop] do
          if n <> boundid then
            begin
            case targetopsys of
              vdos: namesdeclared := namesdeclared + 1;
              unix:
                case unixtarget of
                  umax: namesdeclared := namesdeclared + 1;
                  otherwise
                    namesdeclared := namesdeclared + 1 * ord((today >=
                                     usearly) and (today <= uslate));
                  end;
              otherwise
                namesdeclared := namesdeclared + 1 * ord((today >=
                                                         usearly) and
                                                         (today <= uslate));
              end;
            if i > highestkey then highestkey := i;
            end;
        case n of
          typename: typeindex := f;
          varname, boundid:
            begin
            offset := display[level].blocksize;
            length := l;
            if n = varname then
              display[level].blocksize := display[level].blocksize + length;
            vartype := f;
            knownvalid := true;
            modified := true;
            programdecl := false;
            univparam := false;
            varalloc := normalalloc;
            end;
          directivename, standardproc, standardfunc: procid := id;
          constname: consttype := f;
          end;
        end;
    end {enterstandardid} ;


  function rdup(i: integer): integer;

  { Round size.
  }

    begin {rdup}
      case hostmachine of
        mc68000: if odd(i) then rdup := i + 1 else rdup := i;
        iapx86: if odd(i) then rdup := i + 1 else rdup := i; {/nobytealloc}
        i80386: if odd(i) then rdup := i + 1 else rdup := i;
        otherwise rdup := i;
        end;
    end {rdup} ;


  begin {initanalys}

    { This code checks certain configuration parameters and reports any
      potential problems. }

    { If tablesize (a field width) is not at least as large as hashtablesize,
      then give up now.
    }
    if tablesize < hashtablesize then
      begin
      write('Tablesize is smaller than hashtablesize');
      compilerabort(inconsistent);
      end;

    { Check the sizes environment file components. }

{DRB
    if tableentriesperblock + 1 <>
       (diskbufsize + 1) div rdup(sizeof(tableentry)) then
      writeln('Environment files:  TABLEENTRIESPERBLOCK should be ',
             (diskbufsize + 1) div rdup(sizeof(tableentry)) - 1: 1);
}
    { End of special configuration checks}

    emitflag := true;
    emptysetgenerated := false;
    checkundefs := true;
    linearize := false;
    skipfactor := false;
    divfolded := false;

    lastlabel := 32766;
    nextintcode := 0;

    blockheadset := [labelsym..functionsym];
    begblockset := [labelsym..functionsym, beginsym];
    begparamhdr := [functionsym, proceduresym, varsym, ident];
    nextparamhdr := [functionsym, proceduresym, varsym, ident, rpar];
    begstmtset := [beginsym..gotosym, ident];
    begunsignedset := [nilsym, ident, intconst, charconst, realconst,
                      dblrealconst, stringconst];
    begconstset := [nilsym, ident, intconst, charconst, realconst,
                   dblrealconst, stringconst, plus, minus];
    begstructset := [uparrow..stringsym];
    begsimplset := [nilsym, ident, intconst, charconst, realconst,
                   dblrealconst, stringconst, plus, minus, lpar];
    begtypset := [uparrow..stringsym, nilsym, ident, intconst, charconst,
                 realconst, dblrealconst, stringconst, plus, minus, lpar];
    exprops := [eql..insym];
    sexprops := [plus..orsym];
    termops := [star..andsym];
    begfactset := [ident, intconst, realconst, dblrealconst, charconst,
                  stringconst, lbrack, lpar, notsym, at, nilsym];
    begexprset := [eql..andsym, ident, intconst, realconst, dblrealconst,
                  charconst, stringconst, lbrack, lpar, notsym, nilsym];
    legalfunctypes := [scalars, ints, reals, doubles, bools, chars, ptrs,
                      subranges, none];
    neverskipset := [labelsym..functionsym, beginsym..gotosym, ident,
                    semicolon, eofsym, endsym];
    constfollowset := [ident, semicolon, eql, plus, minus, lpar, nilsym,
                      intconst..stringconst];
    typefollowset := [ident, semicolon, eql, uparrow..stringsym, nilsym, plus,
                     minus, lpar, intconst..stringconst];

    { Map for binary operators }

    optable[eql] := eqop;
    optable[lss] := lssop;
    optable[gtr] := gtrop;
    optable[neq] := neqop;
    optable[leq] := leqop;
    optable[geq] := geqop;
    optable[insym] := inop;
    optable[plus] := plusop;
    optable[minus] := minusop;
    optable[orsym] := orop;
    optable[star] := mulop;
    optable[slash] := slashop;
    optable[divsym] := quoop;
    optable[modsym] := remop;
    optable[andsym] := andop;

    intstate := stmtstate;

    with nexttoken do
      begin
      left := 0;
      right := 0;
      line := 1
      end;

    tokencount := 0;
    tokenbufindex := 0;
    probing := false;
    sourcestringindex := 0;

    fewestblocks := lastblocksin;
    mostblocks := lastblocksin;

    for i := 0 to cseregions do
      begin
      with cseregiontable[i, false] do
        begin
          low := 0; high := maxaddr;
        end;
      with cseregiontable[i, true] do
        begin
          low := 0; high := maxaddr;
        end;
      end;

    for i := 0 to hashtablesize do keymap[i] := 0;

    lastvartableptr := 0;
    lastvartableentry := 0;

    scantoken;
    gettoken;

    nullboundindex := 0;
    inputdeclared := false;
    standardfilesreferenced := false;
    new(labelflag);

    lastdebugrecord := 0;

{
    If scanalys and using an environment file, then curstringblock
    and nextstringfile are taken from that file.
}
    if not (scanalys and switcheverplus[environswitch]) then
      begin
      curstringblock := stringtablelimit div (diskbufsize + 1) + 1;
      nextstringfile := stringtablelimit mod (diskbufsize + 1);
      end;

    if needcaching then
      begin
      seek(stringfile, curstringblock);
      stringfiledirty := false;
      end
    else
      begin
      stringblkptr := stringblkptrtbl[curstringblock];
      if stringblkptr = nil then
        begin
        new(stringblkptr);
        stringblkptrtbl[curstringblock] := stringblkptr;
        end;
      end;

    if not switcheverplus[environswitch] then

      begin

      case targetmachine of
      iapx86:
        begin
        if switcheverplus[largemodel] then ptrsize := longptrsize
        else ptrsize := defaultptrsize;
        if switcheverplus[walkback] or switcheverplus[debugging] then
          returnlinksize := extreturnlinksize;
        end;
      end;

      for i := 0 to debughashtablesize do debughashtable[i] := 0;

      { these values must change as more standard ids/types/consts are added }

      lastprocrecord := 0;
      undeftabletop := 0;
      proctabletop := 0;
      mainref := 0;

      globalsize := 0;
      ownsize := 0;
      definesize := 0;

      lastfilekey := maxint;

      anyfile := false;
      anyexternals := false;

      proctable[0].charindex := 0;
      proctable[0].opensfile := false;
      proctable[0].realfunction := false;
      proctable[0].globaldeath := false;
      proctable[0].isprocparam := false;
      proctable[0].bodydefined := false;
      proctable[0].intlevelrefs := false;
      proctable[0].externallinkage := false;
      proctable[0].referenced := true;
      proctable[0].ownused := false;
      proctable[0].calllinkage := pascal2call;
      proctable[0].registerfunction := 0;
      proctable[0].backlink := 0;
      proctable[0].charlen := 0;
      proctable[0].levelspread := 0;
      proctable[0].level := 1;

      lastid := 0;
      lastscope := 0;

      tabletop := 0;
      enterblock(0, 0, 0);
      level := 0;
      display[0].oldtabletop := 0;

      if switcheverplus[doublereals] then targetrealsize := doublesize
      else targetrealsize := singlesize;

      if switcheverplus[shortintegers] then
        begin
        targetintsize := shorttargetintsize;
        targetmaxint := shortmaxint;
        targetminint := shortminint;
        end
      else
        begin
        targetintsize := defaulttargetintsize;
        targetmaxint := defaulttargetmaxint;
        targetminint := defaulttargetminint;
        end;

      if targetmachine = iapx86 then
        begin
        if switcheverplus[largemodel] then ptrsize := longptrsize
        else ptrsize := defaultptrsize;
        if switcheverplus[walkback] or switcheverplus[debugging] then
          returnlinksize := extreturnlinksize;
        end;

      {fake entry for bad boundid's, will be overwritten by next call}
      enterstandardid(integerid, boundid, 0, nullboundindex);
      with display[displaytop] do namesdeclared := namesdeclared - 1;

      {define 'integer'}
      enterform(ints, f, fptr);
      with fptr^ do
        begin
        intindex := f;
        size := targetintsize;
        align := intalign;
        end;
      enterstandardid(integerid, typename, targetintsize, intindex);

      {define 'shortint' subrange}
      enterform(subranges, f, fptr);
      shortintindex := f;
      with fptr^ do
        begin
        size := shorttargetintsize;
        align := shortintalign;
        lowerord := shortminint;
        upperord := shortmaxint;
        parentform := ints;
        parenttype := intindex;
        end;
      enterstandardid(shortintid, typename, shorttargetintsize, shortintindex);

      {define dummy subrange for set building operations}
      enterform(subranges, f, fptr);
      with fptr^ do
        begin
        subrangeindex := f;
        size := targetintsize;
        align := intalign;
        lowerord := 0;
        upperord := maxsetord;
        parentform := ints;
        parenttype := intindex;
        parentform := ints;
        end;

      {define 'real'}
      enterform(reals, f, fptr);
      with fptr^ do
        begin
        realindex := f;
        size := targetrealsize;
        align := realalign;
        end;
      enterstandardid(realid, typename, targetrealsize, realindex);

      {define 'double'}
      enterform(doubles, f, fptr);
      with fptr^ do
        begin
        doubleindex := f;
        size := doublesize;
        align := realalign;
        end;

      if switcheverplus[doublereals] then doubleindex := realindex;

      enterstandardid(doubleid, typename, doublesize, doubleindex);

      {define 'char'}
      enterform(chars, f, fptr);
      chartypeindex := f;
      with fptr^ do
        begin
        size := charsize;
        align := charalign;
        end;
      enterstandardid(charid, typename, charsize, chartypeindex);

      {define 'boolean'}
      enterform(bools, f, fptr);
      with fptr^ do
        begin
        boolindex := f;
        size := scalarsize;
        align := scalaralign;
        end;
      enterstandardid(booleanid, typename, scalarsize, boolindex);

      {define 'text'}
      enterform(files, f, fptr);
      with fptr^ do
        begin
        containsfile := true;
        filebasetype := chartypeindex;
        textindex := f;
        size := ptrsize;
        align := ptralign;
        filekey := maxint;
        end;
      enterstandardid(textid, typename, ptrsize, textindex);
{***add this***}

      {define 'maxint'}
      enterstandardid(maxintid, constname, 0, intindex);
      enterdebuggerid('maxint   ', 6);
      p^.constvalue.representation := ints;
      p^.constvalue.intvalue := targetmaxint;
      p^.constvalue.negated := false;

      {define 'minint'}
      enterstandardid(minintid, constname, 0, intindex);
      enterdebuggerid('minint   ', 6);
      p^.constvalue.representation := ints;
      p^.constvalue.intvalue := targetminint;
      p^.constvalue.negated := true;

      {define 'true'}
      enterstandardid(trueid, constname, 0, boolindex);
      enterdebuggerid('true     ', 4);
      p^.constvalue.representation := ints;
      p^.constvalue.intvalue := ord(true);
      p^.constvalue.negated := false;

      {define 'false'}
      enterstandardid(falseid, constname, 0, boolindex);
      enterdebuggerid('false    ', 5);
      p^.constvalue.representation := ints;
      p^.constvalue.intvalue := ord(false);
      p^.constvalue.negated := false;

      {define 'write'}
      enterstandardid(writeid, standardproc, 0, 0);

      {define 'writeln'}
      enterstandardid(writelnid, standardproc, 0, 0);

      {define special mc68881 functions}
      if targetmachine = mc68000 then
        begin
        enterstandardid(facosid, standardfunc, 0, 0);
        enterstandardid(fasinid, standardfunc, 0, 0);
        enterstandardid(fatanid, standardfunc, 0, 0);
        enterstandardid(fatanhid, standardfunc, 0, 0);
        enterstandardid(fcoshid, standardfunc, 0, 0);
        enterstandardid(fetoxm1id, standardfunc, 0, 0);
        enterstandardid(fgetexpid, standardfunc, 0, 0);
        enterstandardid(fgetmanid, standardfunc, 0, 0);
        enterstandardid(fintid, standardfunc, 0, 0);
        enterstandardid(flog10id, standardfunc, 0, 0);
        enterstandardid(flog2id, standardfunc, 0, 0);
        enterstandardid(flognp1id, standardfunc, 0, 0);
        enterstandardid(fmodid, standardfunc, 0, 0);
        enterstandardid(fremid, standardfunc, 0, 0);
        enterstandardid(fscaleid, standardfunc, 0, 0);
        enterstandardid(fsgldivid, standardfunc, 0, 0);
        enterstandardid(fsglmulid, standardfunc, 0, 0);
        enterstandardid(fsinhid, standardfunc, 0, 0);
        enterstandardid(ftanid, standardfunc, 0, 0);
        enterstandardid(ftanhid, standardfunc, 0, 0);
        enterstandardid(ftentoxid, standardfunc, 0, 0);
        enterstandardid(ftwotoxid, standardfunc, 0, 0);
        enterstandardid(fmovecrid, standardfunc, 0, 0);
        enterstandardid(readfpcrid, standardfunc, 0, 0);
        end;

      {define 'sngl'}
      enterstandardid(snglid, standardfunc, 0, 0);

      {define 'dbl'}
      enterstandardid(dblid, standardfunc, 0, 0);

      {define 'sin'}
      enterstandardid(sinid, standardfunc, 0, 0);

      {define 'cos'}
      enterstandardid(cosid, standardfunc, 0, 0);

      {define 'exp'}
      enterstandardid(expid, standardfunc, 0, 0);

      {define 'sqrt'}
      enterstandardid(sqrtid, standardfunc, 0, 0);

      {define 'arctan'}
      enterstandardid(arctanid, standardfunc, 0, 0);

      {define 'ln'}
      enterstandardid(lnid, standardfunc, 0, 0);

      {define 'odd'}
      enterstandardid(oddid, standardfunc, 0, 0);

      {define 'abs'}
      enterstandardid(absid, standardfunc, 0, 0);

      {define 'sqr'}
      enterstandardid(sqrid, standardfunc, 0, 0);

      {define 'trunc'}
      enterstandardid(truncid, standardfunc, 0, 0);

      {define 'round'}
      enterstandardid(roundid, standardfunc, 0, 0);

      {define 'ord'}
      enterstandardid(ordid, standardfunc, 0, 0);

      {define 'chr'}
      enterstandardid(chrid, standardfunc, 0, 0);

      {define 'succ'}
      enterstandardid(succid, standardfunc, 0, 0);

      {define 'pred'}
      enterstandardid(predid, standardfunc, 0, 0);

      {define 'eof'}
      enterstandardid(eofid, standardfunc, 0, 0);

      {define 'eoln'}
      enterstandardid(eolnid, standardfunc, 0, 0);

      {define 'time'}
      enterstandardid(timeid, standardfunc, 0, 0);

      {define 'size'}
      enterstandardid(sizeid, standardfunc, 0, 0);

      {define 'bitsize'}
      enterstandardid(bitsizeid, standardfunc, 0, 0);

      {define 'upper'}
      enterstandardid(upperid, standardfunc, 0, 0);

      {define 'lower'}
      enterstandardid(lowerid, standardfunc, 0, 0);

      {define 'loophole'}
      enterstandardid(loopholeid, standardfunc, 0, 0);

      {define 'ref'}
      enterstandardid(refid, standardfunc, 0, 0);

      {define 'noioerror'}
      enterstandardid(noioerrorid, standardproc, 0, 0);

      {define 'ioerror'}
      enterstandardid(ioerrorid, standardfunc, 0, 0);

      {define 'iostatus'}
      enterstandardid(iostatusid, standardfunc, 0, 0);

      {define 'copy'}
      enterstandardid(copyid, standardfunc, 0, 0);

      {define 'concat'}
      enterstandardid(concatid, standardfunc, 0, 0);

      {define 'length'}
      enterstandardid(lengthid, standardfunc, 0, 0);

      {define 'pos'}
      enterstandardid(posid, standardfunc, 0, 0);

      {define 'seek'}
      enterstandardid(seekid, standardproc, 0, 0);

      {define 'read'}
      enterstandardid(readid, standardproc, 0, 0);

      {define 'readln'}
      enterstandardid(readlnid, standardproc, 0, 0);

      {define 'break'}
      enterstandardid(breakid, standardproc, 0, 0);

      {define 'new'}
      enterstandardid(newid, standardproc, 0, 0);

      {define 'dispose'}
      enterstandardid(disposeid, standardproc, 0, 0);

      {define 'pack', a singularly silly procedure}
      enterstandardid(packid, standardproc, 0, 9);

      {define 'unpack', whose parameterlist is incompatible with 'pack'!}
      enterstandardid(unpackid, standardproc, 0, 0);

      {define 'put'}
      enterstandardid(putid, standardproc, 0, 0);

      {define 'page'}
      enterstandardid(pageid, standardproc, 0, 0);

      {define 'get'}
      enterstandardid(getid, standardproc, 0, 0);

      {define 'reset'}
      enterstandardid(resetid, standardproc, 0, 0);

      {define 'rewrite'}
      enterstandardid(rewriteid, standardproc, 0, 0);

      {define 'close'}
      enterstandardid(closeid, standardproc, 0, 0);

      {define 'delete'}
      enterstandardid(deleteid, standardproc, 0, 0);

      {define 'rename'}
      enterstandardid(renameid, standardproc, 0, 0);

      {define 'insert'}
      enterstandardid(insertid, standardproc, 0, 0);

      {define 'str'}
      enterstandardid(strid, standardproc, 0, 0);

      {define 'val'}
      enterstandardid(valprocid, standardproc, 0, 0);

      {define 'deletestr'}
      enterstandardid(deletestrid, standardproc, 0, 0);

      {define special mc68881 procedures}
      if targetmachine = mc68000 then
        begin
        enterstandardid(fsincosid, standardproc, 0, 0);
        enterstandardid(setfpcrid, standardproc, 0, 0);
        end;

      {define 'forward'}
      enterstandardid(forwardid, directivename, 0, 0);

      {define 'external'}
      enterstandardid(externalid, directivename, 0, 0);

      {define 'nonpascal'}
      enterstandardid(nonpascalid, directivename, 0, 0);

      {define 'interrupt'}
      enterstandardid(interruptid, directivename, 0, 0);

      {define 'fortran'}
      if targetopsys = msdos then
        enterstandardid(fortranid, directivename, 0, 0);

      {define noneindex for undef typenames}
      enterform(none, f, fptr);
      with fptr^ do
        begin
        dbgsymbol := 0;
        noneindex := f;
        size := 0;
        align := unitsize;
        end;

      {define nilindex}
      enterform(ptrs, f, fptr);
      with fptr^ do
        begin
        ptrtypename := 0;
        nilindex := f;
        size := ptrsize;
        align := ptralign;
        end;
      nilvalue.operandkind := constoperand;
      nilvalue.cvalue.representation := ptrs;
      if reversebytes then
        begin
        kludge.p := niladdressvalue;
        for i := 1 to hostintsize div 2 do
          begin
          b := kludge.b[i];
          kludge.b[i] := kludge.b[hostintsize + 1 - i];
          kludge.b[hostintsize + 1 - i] := b;
          end;
        nilvalue.cvalue.ptrvalue := kludge.p;
        end
      else nilvalue.cvalue.ptrvalue := niladdressvalue;
      nilvalue.oprndlen := ptrsize;
      nilvalue.typeindex := nilindex;

      {define 'nil' for debugger}
      tabletop := tabletop + 1;
      if bigcompilerversion then p := @(bigtable[tabletop]);
      with p^ do
        begin
        form := false;
        name := 0;
        nextname := 0;
        namekind := constname;
        consttype := nilindex;
        constvalue := nilvalue.cvalue;
        end;
      enterdebuggerid('nil      ', 3);

      {define 'main' program}
      if bigcompilerversion then p := @(bigtable[0]);
      with p^ do
        begin
        name := 0;
        form := false;
        nextname := 0;
        namekind := procname;
        charindex := 1;
        charlen := 0;
        paramlist := 0;
        functype := noneindex;
        funclen := 0;
        dbgsymbol := 0;
        end;
      enterdebuggerid('main     ', 4);

      enterblock(1, 0, 0);

      display[1].blocksize := display[0].blocksize;
      display[1].oldtabletop := tabletop;
      level := 1;

      {define 'output'}
      enterstandardid(outputid, varname, ptrsize, textindex);
      outputindex := tabletop;
      outputdeclared := false;
      enterdebuggerid('output   ', 6);

      {define 'input'}
      inputoffset := display[level].blocksize;
      enterstandardid(inputid, varname, ptrsize, textindex);
      inputindex := tabletop;
      enterdebuggerid('input    ', 5);

      end; {not environment switch}

    { Save the number of bytes allocated for input and output.
    }
    globalfiles := display[1].blocksize;

    if not scanalys then
      while stringtablelimit mod stringroundoff <> 0 do
        begin
        stringtablelimit := stringtablelimit + 1;
        putstringfile;
        end;

    consttablelimit := stringtablelimit;
    stringfilebase := stringtablelimit; {save for writeenvirfile}

    assign(tempfiletwo, 'temptwo.tmp');
    rewrite(tempfiletwo);
    assign(locals, 'locals.tmp');
    rewrite(locals);

  end {initanalys} ;


procedure analys;

{ Syntactic routine to parse and generate intermediate file output for
  a Pascal program.

  Productions:

  program = [ program-heading  ";" ] block  .
  program-heading = "program" identifier " [ "(" identifier-list ")" ]  .

  This routine is responsible for program initialization, parsing
  of the entire program, and final cleanup.  The majority of the
  work is done by the routine "block"
}


  begin {analys}

    {the next lines are superfluous: initanalys does the work}
  {DRB
    if not needcaching then
      begin
      stringblkptr := stringblkptrtbl[1];
      if stringblkptr = nil then
        begin
        new(stringblkptr);
        stringblkptrtbl[1] := stringblkptr;
        end;
      end;
      }

    initanalys;

    if token = programsym then programheading
    else if switcheverplus[mainbody] then warnnonstandard(progexpected);

    repeat
      block;

      while token = endsym do
        begin
        warn(extraenderr);
        gettoken
        end;

      case token of
        eofsym:
          if switchcounters[mainbody] > 0 then warnbetween(doteoferr);
        dot:
          begin
          gettoken;
          verify1([eofsym], garbageerr)
          end;
        semicolon:
          begin
          warn(doteoferr);
          gettoken
          end;
        proceduresym, functionsym: warn(extraprocerr);
        otherwise fatal(extrastmterr)
        end;
      if token <> eofsym then enterblock(1, 0, 0);
    until token = eofsym;

    if intstate = opstate then genop(endexpr);
    genstmt(endall);
    exitblock(0);
    {DRB
    if emitflag then
      begin
      put(tempfiletwo);
      end;
      }

    dispose(labelflag);
  end {analys} ;
end.
