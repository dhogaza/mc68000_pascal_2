{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF CONFIDENTIAL SOFTWARE:
  Copyright 1977, 1978, 1979, 1980, 1981, 1982, 1985 by Oregon Software, Inc.
  ALL RIGHTS RESERVED.

  This computer program is the proprietary property of Oregon
  Software, Inc. of Portland, Oregon, U.S.A., and may be used
  and copied only as specifically permitted under written
  license agreement signed by Oregon Software, Inc.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Pascal-2 code generator.  

  Release version: 0045  Level: 1  Date: 21-Nov-1990 15:30:28  
  Processor: ~processor~  

  Last modified by KRIS on ~update~ 15:30:28 
   Purpose: 
  Update release version for PC-VV0-GS0 at 2.3.0.1 
}
{[a+]}

unit code;

interface

uses config, hdr, t_c, hdrc, utils, commonc, putcode;

procedure code;

procedure codeone;

procedure initcode;

procedure exitcode;

implementation

type
  getregtype = function : regindex {routine used to allocate a reg};


procedure copy_openarrays;
  forward;

procedure clearcontext;
  forward;

procedure sysfnintx;
  forward;


procedure gendouble(i: insttype; {instruction to generate}
                    src, dst: keyindex {operand descriptors} );
  forward;

function getareg: regindex;
  forward;

function getdreg: regindex;
  forward;

procedure markareg(r: regindex {register to clobber} );
  forward;

procedure fpgendouble(i: insttype; {instruction to generate}
                      src, dst: keyindex {operand descriptors});
  forward;

procedure initcode;

{ Initialize the code generation pass.
}

  var
    i: integer; {general purpose induction variable}
    procno: proctableindex;

  function rdup(i: integer): integer;

    begin {rdup}
      case hostmachine of
        mc68000: if odd(i) then rdup := i + 1 else rdup := i;
        iapx86: if odd(i) then rdup := i + 1 else rdup := i; {/nobytealloc}
        otherwise rdup := i;
        end;
    end {rdup} ;


  begin {initcode}

    monadicinsts :=
        [fbeq, fbne, fbgt, fbngt, fbge, fbnge, fblt, fbnlt, fble,
         fbnle, fbgl, fbngl, fbgle, fbngle, ftrap, ftst,
         beq, bge, bgt, bhi, bhs, ble, blo, bls, blt, bpl, bmi, bne,
         bra, bsr, bvc, bvs, bfclr, bfset, bftst, clr, ext, extb, jmp,
         jsr, move_to_ccr, neg, negx,
         notinst, pea, swap, trap, tst, unlk];

    dyadicinsts :=
        [fabs, facos, fadd, fasin, fatan, fatanh, fcmp, fcos, fcosh, fdiv,
         fetox, fetoxm1, fgetexp, fgetman, fint, fintrz, flog10, flog2,
         flogn, flognp1, fmod, fmove, fmovecr, fmove_to_fpcr, fmove_from_fpcr,
         fmovem, fmul,
         fneg, frem, fscale, fsgldiv, fsglmul, fsin, fsincos, fsinh,
         fsqrt, fsub, ftan, ftanh, ftentox, ftwotox,

         add, adda, addi, addq, addx, andinst, andi, asl, asr,
         bchg, bclr, bfexts, bfextu, bfins,
         bset, btst, chk, cmp, cmpa, cmpi, cmpm, dbeq,
         dbge, dbgt, dbhi, dbhs, dble, dblo, dbls, dblt, dbpl, dbmi, dbne,
         dbra, dbvc, dbvs, divs, divsl, divu, divul, eor, eori, exg, lea, link,
         lsl, lsr, move, movea, movem, moveq, muls, mulu, orinst,
         ori, rol, ror, roxl, roxr, sub, suba, subi, subq, subx];

    { note: only "rte", "rts", "trapcc" and "trapv" are not included in the
      above subsets.
    }

 
    { The "qualifiedinsts" set lists all instructions that must have
      a size provided for the assembler.
    }

    qualifiedinsts :=
        [adda, add, addi, addq, addx, andinst, andi, asl, asr,
         chk, chk2, clr, cmpa, cmp, cmpi, cmpm, divs, divsl, divu, divul,
         eor, eori, exg, ext, extb, lsl, lsr, move, movea, movem, muls, mulu,
         neg, negx, notinst, orinst, ori, rol, ror, roxl,
         roxr, suba, sub, subi, subq, subx, tst];
    shiftinsts := [asl, asr, lsl, lsr, rol, ror, roxl, roxr];

    shortinsts := [moveq, addq, subq, asl, asr, lsl, lsr, rol, ror, roxl,
                   roxr, trap];

    immedinsts := [addi, andi, cmpi, eori, ori, subi];

    branches := [beq, bge, bgt, bhi, ble, bls, blt, bpl, bmi, bne, blo, bhs,
                 bvc, bvs, bra];

    fpbranches := [fbeq, fbne, fbgt, fbngt, fbge, fbnge, fblt, fbnlt, fble,
                   fbnle, fbgl, fbngl, fbgle, fbngle];

    {[s=3] reversed branch table }                  reverse[beq] := beq;
    reverse[bne] := bne;    reverse[blt] := bgt;    reverse[bgt] := blt;
    reverse[bge] := ble;    reverse[ble] := bge;    reverse[blo] := bhi;
    reverse[bhi] := blo;    reverse[bls] := bhs;    reverse[bhs] := bls;
    reverse[nop] := nop;    reverse[bra] := bra;    reverse[bvs] := bvs;
    reverse[bvc] := bvc;

    reverse[fbeq] := fbeq;  reverse[fbne] := fbne;  reverse[fbgt] := fblt;
    reverse[fbngt] := fbnlt;  reverse[fbge] := fble;  reverse[fbnge] := fbnle;
    reverse[fblt] := fbgt;  reverse[fbnlt] := fbngt;  reverse[fble] := fbge;
    reverse[fbnle] := fbnge;  reverse[fbgl] := fbgl;  reverse[fbngl] := fbngl;
    reverse[fbgle] := fbgle;  reverse[fbngle] := fbngle;

    { inverted branch table }

    invert[beq] := bne;     invert[bne] := beq;     invert[blt] := bge;
    invert[bgt] := ble;     invert[bge] := blt;     invert[ble] := bgt;
    invert[blo] := bhs;     invert[bhi] := bls;     invert[bls] := bhi;
    invert[bhs] := blo;     invert[bvs] := bvc;     invert[bvc] := bvs;
    invert[nop] := bra;     invert[bra] := nop;

    invert[fbeq] := fbne;  invert[fbne] := fbeq;  invert[fbgt] := fbngt;
    invert[fbngt] := fbgt;  invert[fbge] := fbnge;  invert[fbnge] := fbge;
    invert[fblt] := fbnlt;  invert[fbnlt] := fblt;  invert[fble] := fbnle;
    invert[fbnle] := fble;  invert[fbgl] := fbngl;  invert[fbngl] := fbgl;
    invert[fbgle] := fbngle;  invert[fbngle] := fbgle;

    {[s=1]}

    {Initialize label book-keeping}

    labelnextnode := false;
    labeltable[0].nodelink := 0;
    startaddress := undefinedaddr;

    {Now initialize file variables}

    nokeydata := [endpseudocode, bad, blockentry, blockexit, jumpf, jumpt, jump,
		 pascallabel, savelabel, clearlabel, joinlabel, restorelabel,
		 sysroutine, caseelt, setfile, closerange, restoreloop,
                 saveactkeys];

    oneoperand := [endpseudocode, bad, blockexit, dovar, dounsvar, doint, doorigin,
		 pseudolabel, savelabel, clearlabel, joinlabel, restorelabel,
		 copyaccess, flt, pshaddr, pshstraddr, pshint, pshptr, pshreal,
		 pshstr, pshstruct, pshset, pshlitint, pshlitptr, pshlitreal,
		 copystack, fmt, wrint, wrreal, wrchar, wrst, wrbool, wrbin, wrxstr,
		 rdbin, rdint, rdchar, rdreal, rdst, rdxstr, stacktarget, ptrchk,
		 chrstr, arraystr, definelazy, setbinfile, setfile, closerange,
		 restoreloop];

    bitfieldinsts := [bfclr, bfexts, bfextu, bfins, bfset, bftst];

    mc68020 := switcheverplus[cpu68020]; {shorthand form}

    if mc68020 then mc68881 := switcheverplus[fpc68881]
    else mc68881 := false;

    aware := switcheverplus[awaremode];
    if targetopsys = vdos then pic_enabled := switcheverplus[pic] {shorthand form}
    else pic_enabled := false;

    coprocessor_id := 1; {temporary}

    case language of
      modula2, C: newobjectfmt := true;
      Pascal: newobjectfmt := false;
      end;

    dummyarg_ptr := 0;
    curstringblock := 0;
    nextstringfile := 0;
    level := 0;
    fileoffset := 0;
    formatcount := 0;
    lastmaprecord := 0;
    filenamed := false;
    definelazykluge := false; {indicates no-op}

    for procno := 0 to proctablesize do
      begin
      procmap[procno].addr := undefinedaddr;
      end;


    keytable[0].validtemp := false;

    lastnode := 0;

    testing := switcheverplus[test] and switcheverplus[outputmacro];

    sectionpc[codesect] := 0;
    sectionpc[diagsect] := 0;
    sectionno[codesect] := codesection;
    sectionno[diagsect] := diagsection;
    currentpc := 0;
    skip_macro_details := false;
    currentsect := codesect;
    highcode := 0;
    lastdiagpc := 0;
    lastdiagline := 1;
    everdiagnosing := false;
    totalputerr:=0;

    if switcheverplus[outputmacro] then initmac;

    stackcounter := keysize - 1; {fiddle consistency check}

    if peeping then for i := 0 to maxpeephole do peep[i] := 0;
    fixuphead := nil; { empty forward reference list }
    fixuptail := nil; { obviously no last node }
    nextESD := firstESD; { initialize ESDtable fill counter }

    if mc68881 then
      begin
      newesd.esdkind := ESDsupport;
      newesd.suppno := lib68881init;
      insertnewesd;
      end;
  end {initcode} ;



procedure initblock;

{ Initialize global variables for a new block.
}

  var
    i: integer; {general purpose induction variable}

  begin
    maxstackdepth := 0;
    paramlist_started := false; {reset switch}

    while (currentswitch <= lastswitch) and
          ((switches[currentswitch].mhi = gethi) and
          (switches[currentswitch].mlow <= getlow) or
          (switches[currentswitch].mhi < gethi)) do
      with switches[currentswitch] do
        begin
        switchcounters[s] := switchcounters[s] + v;
        currentswitch := currentswitch + 1;
        end;

    openarray_base := nil; { Modula2 only, but initialization needed for all }
    savereginst := 0;
    fpsavereginst := 0;
    savedebinst := 0;
    firststmt := 0;
    firstbr := nil;
    lastnode := 0;
    nextlabel := 0;
    forsp := 0;
    adjustdelay := false;
    oktostuff := true; {until proven otherwise}
    dontchangevalue := 0;
    settargetused := false;

    {Re-initialize virtual node storage}

    thrashing := false;
    for i := 1 to lastblocksin do
      with blocksin[i] do
        begin
        written := true;
        blkno := i - 1;
        end;

    contextsp := 1;
    context[1].keymark := 1;
    context[1].clearflag := false;
    context[1].lastbranch := 0;
    loopsp := 0;

    for i := 0 to 7 do
      begin
      with loopstack[0].aregstate[i] do
        begin
        active := false;
        killed := false;
        used := false;
        stackcopy := 0;
        end;

      with loopstack[0].dregstate[i] do
        begin
        active := false;
        killed := false;
        used := false;
        stackcopy := 0;
        end;

      with loopstack[0].fpregstate[i] do
        begin
        active := false;
        killed := false;
        used := false;
        stackcopy := 0;
        end;
      end;

    loopoverflow := 0;
    lastkey := 0;

    {initialize the keytable to empty}

    for i := lowesttemp to keysize do
      with keytable[i] do
        begin
        access := noaccess;
        properreg := i;
        properindxr := i;
        regvalid := false;
        indxrvalid := false;
        regsaved := false;
        indxrsaved := false;
        validtemp := false;
        tempflag := false;
        oprnd.scale := 1;
        oprnd.commonlong_reloc := unknown;
        end;

    keytable[keysize].refcount := 1;
    keytable[keysize - 1].refcount := 1;

    keytable[loopsrc].refcount := 255;
    keytable[loopsrc1].refcount := 255;
    keytable[loopdst].refcount := 255;

    {zero out all register data}
    for i := 0 to 7 do
      begin
      dregused[i]:=false;
      aregused[i]:=false;
      fpregused[i]:=false;
      dregisters[i] := 0;
      aregisters[i] := 0;
      fpregisters[i] := 0;
      end;

    { Prevent other code from using debuglink (currently A3) if debugging or
      profiling.  
    }
    if (switchcounters[debugging] > 0) or (switchcounters[profiling] > 0) then
      aregisters[debuglink] := 100000; {used throughout program by
                                       debugger/profiler}

    {initialize temp allocation vars}
    stackcounter := keysize - 1;
    stackoffset := 0;
    keytable[stackcounter].oprnd.offset := 0;
    keytable[keysize].oprnd.offset := 0;

  end {initblock} ;

procedure newnode;

{ Increment "lastnode", checking for instruction table overflow.  Sets
"lastptr" using cwriteaccess, to allow caller to easily fill in the
node.
}


  begin {newnode}
    lastnode := lastnode + 1;
    if lastnode = cnodetablesize then compilerabort(manynodes);
    lastptr := @(bignodetable[lastnode]);
  end {newnode} ;

function instlength(n: nodeindex {must refer to an instruction}) : integer;

{ Return the byte length of the given instruction.  This code assumes
  that all relevant instruction optimization has been done (i.e. adds
  have been changed to addi's or addq's where appropriate).
}

  var
    p: nodeptr; { for creadaccess to return pointer to current node }
    len: 0..10; { temporary length counter }
    inst: insttype; { temporary copy of instruction type }
    oplen: datarange; {operand length from instruction node}
    i: operandrange; { induction var for stepping thru operands }
    format: mc68881_source_specifiers;


  begin {instlength}
    if bigcompilerversion then p := @(bignodetable[n]);

    if p^.kind <> instnode then { filter the pretenders }

      if (p^.kind = stmtref) or (p^.kind = errornode) or
         (p^.kind = sectionnode) then len := 0
      else if p^.kind = labeldeltanode then len := word
      else if p^.kind = datanode then len := word
      else if p^.kind = adconnode then len := long
      else
        begin
        writeln('node ', n: 1, ' is not an instruction');
        compilerabort(inconsistent);
        end

    else
      begin { "conventional" instruction }
      inst := p^.inst;
      format := p^.fp_format;
      oplen := max(word, p^.oprndlength);

      if inst = nop then len := 0
      else if inst = movem then
        begin
        if bigcompilerversion then p := @(bignodetable[n + 2]);

        if p^.oprnd.m = relative then len := long + word else len := long;
        end
      else if inst = fmovem then len := long
      else if inst = link then len := word + oplen { 68020 allows long }
      else
        begin
        { Handle the strange ones here.
        }
        len := word;
        if mc68020 and (((inst = muls) or (inst = mulu)) and (oplen = long)) or
          (inst in [divsl, divul, chk2, bfclr, bfexts, bfextu, bfins,
          bfset, bftst]) or (inst in [fp_first..fp_last] - fpbranches) then
            len := long;

        for i := 1 to p^.oprndcount do
          begin
          if bigcompilerversion then p := @(bignodetable[n + i]);

          case p^.kind of
            relnode:
              if (inst = lea) or (inst in fpbranches) then len := len + word;
            labelnode: len := len + p^.labelcost;
            oprndnode:
              case p^.oprnd.m of
                nomode, areg, dreg, indr, autoi, autod, bit_field_const,
                twodregs, twofpregs, special_immediate:
                { no additional length } ;
                immediate, immediatequad, immediate_extended:
                  { Immediatequad is used for the 68881 only and oplen
                    reflects the length of the immediate operand, not the
                    length of the operation.
                  }
                  if inst in [fp_first..fp_last] then
                    case format of
                      word_integer, byte_integer: len := len + word;
                      long_integer, single_real: len := len + long;
                      double_real: len := len + quad;
                      extended_real: len := len + 12;
                      end
                  else if not (inst in shortinsts) then len := len + oplen;
                relative:
                  begin
                  len := len + word;
                  if mc68020 and ((p^.oprnd.offset > 32767) or
                     (p^.oprnd.offset < - 32768)) then
                    len := len + long;
                  end;
                pcindexed, indexed:
                  begin
                  len := len + word;
                  if mc68020 and ((p^.oprnd.offset > 127) or
                     (p^.oprnd.offset < - 128)) then
                    if (p^.oprnd.offset <= 32767) and
                       (p^.oprnd.offset >= - 32768) then
                      len := len + word
                    else
                      len := len + long;
                  end;
                absshort:
                  len := len + word;
                supportcall:
                  if targetopsys = vdos then
                    if switcheverplus[longlib] then
                      if mc68020 and pic_enabled then
                        len := len + long + word
                      else if pic_enabled then { 68000 pic }
                        len := len + word
                      else
                        len := len + long
                    else {not longlib}
                      len := len + word
                  else {unix}
                    if mc68020 and pic_enabled then
                      len := len + long + word
                    else
                      len := len + long;
                pcrelative, usercall:
                  len := len + p^.operandcost;
                abslong, commonlong, immediatelong, symbol,
                pic_own_immed, pic_usercall, pic_supportcall, pic_branch,
                pic_pcrelative:
                  len := len + long;
                pic_splat_pcrel:
                  len := len + word;
                bitindexed:
                  begin
                  write('bitindexed operand at node ', n + i: 1);
                  compilerabort(inconsistent);
                  end; { bitindexed }
                end; { case p^.oprnd.m }
            end; { case p^.kind }
          end { for i } ;
        end;
      if bigcompilerversion then p := @(bignodetable[n]);

      p^.computed_length := len;
      end; { instruction }

    instlength := len;
  end {instlength} ;
procedure insert(m: nodeindex; {instruction to insert after}
                 n: nodeindex {number of nodes to insert} );

{ Insert "n" nodes after node "m".  This attempts to overlay noop's
  if they exist, but it may move instructions if it needs to.  If it does
  move instructions, it will have to adjust the "savemark" fields of any
  temps allocated after instruction "m" and adjust labels which may point
  to them.

  This has the side effect of saving the current value of "lastnode" in
  "lastsaved" and setting "lastnode" to m.
}

  var
    i: nodeindex; {used to scan noop nodes}
    p: nodeptr; {used to access nodes being scanned}
    t: integer; {used to scan keytable}
    cp: brlinkptr; {used to follow brlink chain}
    bl: nodeindex; {used to follow brnodelink chain}


  procedure movenode(m: nodeindex; {source node}
                     n: nodeindex {destination node} );

{ Moves the contents of node "m" to node "n".
}

    var
      n1, n2: nodeptr; {used to get at contents of the nodes}


    begin
      if bigcompilerversion then n1 := @(bignodetable[m]);
      if bigcompilerversion then n2 := @(bignodetable[n]);
      n2^ := n1^;
    end {movenode} ;


  begin
    if m < lastnode then
      begin

      {first scan over any nop's that are there}

      i := m;

      if bigcompilerversion then p := @(bignodetable[i + 1]);

      if p^.kind <> instnode then
        begin
        write('attempt to insert before operand:', i: 1);
        compilerabort(inconsistent);
        end;

      repeat
        i := i + 1;
        if bigcompilerversion then p := @(bignodetable[i]);
      until (p^.inst <> nop) or (i >= m + n);

      {There are "n" nodes left which must be moved}
      n := m + n - i + 1;
      if (p^.inst <> nop) and (n > 0) then
        begin

        { modify any labels pointing to moved nodes }

        t := nextlabel;
        labeltable[0].nodelink := 0;
        while labeltable[t].nodelink > i do
          begin
          labeltable[t].nodelink := labeltable[t].nodelink + n;
          t := t - 1;
          end;

        { alter links in labelnodes referring to moved nodes }

        cp := firstbr;
        while cp <> nil do
          begin
          if bigcompilerversion then p := @(bignodetable[cp^.n + 1]);
          bl := p^.brnodelink;
          while bl <> 0 do
            begin
            if bl >= i then
              begin
              p^.brnodelink := p^.brnodelink + n;
              blocksin[1].written := true;
              end;
            if bigcompilerversion then p := @(bignodetable[bl + 1]);
            bl := p^.brnodelink;
            end;
          if cp^.n >= i then
            cp^.n := cp^.n + n;
          cp := cp^.nextbr;
          end;

        { now update the "instmark" of temp storage }

        for t := stackcounter to keysize do
          if keytable[t].instmark >= i then
            keytable[t].instmark := keytable[t].instmark + n;

        { and finally move the nodes }

        for t := lastnode downto i do movenode(t, t + n)
        end
      else n := 0;
      end;
    lastsaved := lastnode + n;
    lastnode := m;
  end {insert} ;


function findlabel(labno: integer {desired label number} ): labelindex;

{ Searches the label table for label "labno" and returns the index of
  the entry for that label.
}

  var
    l: labelindex; {induction var for search}


  begin {findlabel}
    l := nextlabel + 1;
    labeltable[0].labno := labno;
    labeltable[0].address := undefinedaddr;
    repeat
      l := l - 1
    until labeltable[l].labno = labno;
    findlabel := l;
  end {findlabel} ;


procedure definelabel(l: integer {label number to define} );

{ Define a label with label number "l" pointing to "lastnode".

  Labels are always kept sorted in node order.  Labels which are
  defined as code is initially emitted are naturally in node order,
  but those defined as a result of peep-hole optimizations may
  have to be sorted in.
}

  var
    t: labelindex; {induction var used in search for slot}
    t1: labelindex; {induction var used in moving labels}


  begin {definelabel}
    if l <> 0 then
      begin
      if nextlabel = labeltablesize then compilerabort(manylabels)
      else nextlabel := nextlabel + 1;
      t := nextlabel;
      labelnextnode := true;
      labeltable[0].nodelink := 0;
      while labeltable[t - 1].nodelink > lastnode do t := t - 1;
      for t1 := nextlabel downto t + 1 do
        labeltable[t1] := labeltable[t1 - 1];
      with labeltable[t] do
        begin
        labno := l;
        nodelink := lastnode + 1;
        stackdepth := stackoffset;
        address := undefinedaddr;
        end;
      end;
  end {definelabel} ;


procedure definelastlabel;

{ Define the label with number "lastlabel".  This is used by the code
  generator to generate new labels as needed.  Such "local" labels are
  defined from "maxint" down, while labels emitted by travrs are defined
  from 1 up.
}


  begin {definelastlabel}
    definelabel(lastlabel);
    lastlabel := lastlabel - 1;
  end {definelastlabel} ;

function getlabelnode(l: integer {label desired} ): nodeindex;

{ Returns the node index of the node with label "l".
}


  begin
    getlabelnode := labeltable[findlabel(l)].nodelink
  end {labelnode} ;



function eqinst(n, n1: nodeindex {nodes to compare} ): boolean;

{ Returns true if the instructions (including their operands) indexed
  by "n" and "n1" are equivalent.
}

  var
    p, p1: nodeptr; {point to nodes being compared}
    oprnds: operandrange; {operand counter}
    equal: boolean; {function result}


  begin
    if bigcompilerversion then
      begin
      p := @(bignodetable[n]);
      p1 := @(bignodetable[n1]);
      end;

    equal := ((p^.inst = p1^.inst) and (p^.oprndcount = p1^.oprndcount) and
             (p^.oprndlength = p1^.oprndlength)) or
             ((p^.kind = errornode) and (p1^.kind = errornode) and
             (p^.errorno = p1^.errorno));

    oprnds := p^.oprndcount;
    while equal and (oprnds > 0) do
      begin
      n := succ(n);
      n1 := succ(n1);

      if bigcompilerversion then
        begin
        p := @(bignodetable[n]);
        p1 := @(bignodetable[n1]);
        end;

      if p^.kind = p1^.kind then
        case p^.kind of
          oprndnode:
            if (p^.oprnd.m <> p1^.oprnd.m) or
               (p^.oprnd.reg <> p1^.oprnd.reg) or
               (p^.oprnd.indxr <> p1^.oprnd.indxr) or
               (p^.oprnd.offset <> p1^.oprnd.offset) or
               (p^.oprnd.offset1 <> p1^.oprnd.offset1) or
               (p^.oprnd.offset2 <> p1^.oprnd.offset2) or
               (p^.oprnd.scale <> p1^.oprnd.scale) or
               (p^.oprnd.commonlong_reloc <> p1^.oprnd.commonlong_reloc) then
              equal := false;
          labelnode:
            if (getlabelnode(p^.labelno) <> getlabelnode(p1^.labelno)) or
               (p^.stackdepth <> p1^.stackdepth) or
               (findlabel(p^.labelno) = 0) then
              equal := false;
          relnode:
            if p^.distance <> p1^.distance then
              equal := false;
          otherwise
            equal := false;
        end {case}
      else {not the same node kind}
        equal := false;
      oprnds := oprnds - 1;
      end {while} ;
    eqinst := equal;
  end {eqinst} ;

procedure refglobal(m: modes; {usercall, supportcall, etc.}
                    what: integer {which routine or other global} );

{ Look up a global reference in the reference list, and if absent,
  append it to the list.  Apollo only.
}

  var
    g: globalentryptr; {used to find or create the entry}
    found: boolean; {this one's already in the list}


  begin {refglobal}
    if targetopsys = apollo then
      begin
      g := firstref;
      found := false;
      while g <> nil do
        with g^ do
          if (m = mode) then
            if m = commonlong then
              if what = reloc then
                begin
                found := true;
                g := nil;
                end
              else g := nextglobal
            else if what = offset then
              begin
              found := true;
              g := nil;
              end
            else g := nextglobal
          else g := nextglobal;
      if not found then
        begin
        new(g);
        with g^ do
          begin
          nextglobal := nil;
          done := false;
          mode := m;
          if m = commonlong then
            begin
            offset := 0;
            reloc := what;
            end
          else
            begin
            offset := what;
            reloc := unknown;
            end;
          addr := 0;
          end {with} ;
        if lastref = nil then
          begin
          firstref := g;
          nextref := g;
          end
        else lastref^.nextglobal := g;
        lastref := g;
        end;
      end;
  end {refglobal} ;


procedure refsymbol(n: string8 {symbol name} );

{ Look up a symbol reference in the reference list, and if absent,
  append it to the list.  Apollo only.
}

  var
    g: globalentryptr; {used to find or create the entry}
    found: boolean; {this one's already in the list}


  begin {refsymbol}
    if targetopsys = apollo then
      begin
      g := firstref;
      found := false;
      while g <> nil do
        with g^ do
          if (mode = symbol) and (n = name) then
            begin
            found := true;
            g := nil;
            end
          else g := nextglobal;
      if not found then
        begin
        new(g);
        with g^ do
          begin
          nextglobal := nil;
          done := false;
          mode := symbol;
          name := n;
          reloc := unknown;
          addr := 0;
          end {with} ;
        if lastref = nil then
          begin
          firstref := g;
          nextref := g;
          end
        else lastref^.nextglobal := g;
        lastref := g;
        end;
      end;
  end {refsymbol} ;


procedure geninst(i: insttype; {instruction to generate}
                  l: operandrange; {number of operands}
                  olen: datarange {length of operands} );

{ Generate an instruction.

Actually, this creates a new node in the "nodefile" and initializes the
contents of the node according to data provided in the calling line.  The
actual instruction emission is done later from the node file.

If this instruction has been labeled, the "labelled" field is set.

All other fields not specified are cleared to zero, and will normally be
filled in by the calling procedure.  In particular, tempcount is set to
zero.
}


  begin {geninst}
    newnode;
    with lastptr^ do
      begin
      tempcount := 0;
      kind := instnode;
      inst := i;
      labelled := labelnextnode;
      labelnextnode := false;
      oprndcount := l;
      oprndlength := olen;
      end;
  end {geninst} ;

procedure genoprnd(o: operand {operand to generate} );

{ Generates the given operand.  If the operand contains an offset
dependent on the stack, tempcount is set appropriately.
}

  var
    tc: keyindex; {speedup for computations on tempcount field}
    vtptr: vartablerecptr; {used to access vartable entries}


  begin {genoprnd}
    newnode;

    if o.m = bitindexed then
      begin
      write('bitindexed operand at node ', lastnode: 1);
      compilerabort(inconsistent);
      end;

    with lastptr^ do
      begin
      if o.m in [relative, indexed, bitindexed] then
        if not blockusesframe and (o.reg = fp) then
          begin
          tempcount := keysize - stackcounter;
          o.reg := sp;
          o.offset := o.offset + stackoffset + blksize;
          end
        else if o.reg = sp then
          begin
          tc := keysize;
          while (o.offset < keytable[tc].oprnd.offset) and
                (tc > stackcounter) do
            tc := tc - 1;
          tempcount := tc - stackcounter;
          o.offset := o.offset + stackoffset;
          end
        else tempcount := 0
      else tempcount := 0;

      kind := oprndnode;
      operandcost := word; {only used if m = pcrelative or usercall}
      oprnd := o;

      if oprnd.m in [dreg, areg, fpreg, indr, autoi, autod, relative,
                     indexed, bitindexed, pcindexed, absshort,
                     immediate, pcrelative, twodregs, twofpregs] then
        oprnd.offset1 := 0;
      if targetopsys = apollo then
        with oprnd do
          if m in [supportcall, usercall, symbol, commonlong] then
            if m = supportcall then
              refglobal(supportcall, offset)
            else if m = usercall then
              begin
              if proctable[offset].externallinkage and
                    not proctable[offset].bodydefined then
                refglobal(usercall, offset);
              end
            else if m = symbol then
              refsymbol(name)
            else {must be commonlong}
            if commonlong_reloc > 0 then
              begin
              vtptr := getvartableptr(commonlong_reloc);
              if vtptr^.extvaralloc = usealloc then
                refglobal(commonlong, commonlong_reloc);
              end;
      end {with lastptr^} ;
  end {genoprnd} ;


procedure genlongword(data: unsigned);

{ Generates a longword of constant data.  Currently only used for mc68881
  double and extended constants that must be must be passed by pointer because
  there are no 64 or 96 bit immediate modes.
}


  begin {genlongword}
    newnode;
    lastptr^.kind := datanode;
    lastptr^.data := data;
  end; {genlongword}


procedure genlabeldelta(l1, l2: integer {base and offset labels} );

{ Generates a case table entry label, the 16-bit difference between l1 and
l2.
}


  begin {genlabeldelta}
    newnode;
    with lastptr^ do
      begin
      tempcount := 0;
      kind := labeldeltanode;
      tablebase := l1;
      targetlabel := l2;
      end;
  end {genlabeldelta} ;


procedure genlabel(l: integer {label number} );

{generate a labelnode to label "l".
}


  begin {genlabel}
    newnode;
    with lastptr^ do
      begin
      kind := labelnode;
      labelno := l;
      tempcount := keysize - stackcounter;
      stackdepth := stackoffset;
      labelcost := 0;
      brnodelink := 0;
      proclabel := false;
      end;
  end {genlabel} ;


procedure genbr(inst: insttype; {branch to generate}
                l: integer {label number} );

{ Generate a branch instruction to label "l".

The current stack level is stored in the node for later use
in case stack levels have to be equalized between the branch
point and the label definition point.  If stack adjustment has
been delayed, it is enabled again at this point.
}


  begin {genbr}
    if inst <> nop then
      begin
      geninst(inst, 1, 0);
      genlabel(l);
      if inst in fpbranches then lastptr^.labelcost := word;
      end;
    adjustdelay := false;
  end {Genbr} ;


procedure genrelbr(inst: insttype; {branch to generate}
                   reladd: integer {branch over reladd instructions} );

{ Generate a branch relative to the current location.
The relative argument is the number of instructions to skip over,
not nodes, to simplify peephole optimization routines.
}


  begin {genrelbr}
    geninst(inst, 1, 0);
    newnode;
    with lastptr^ do
      begin
      tempcount := 0;
      kind := relnode;
      distance := reladd;
      end;
  end {genrelbr} ;


procedure gendb(i: insttype; {db-style inst to gen}
                regkey: keyindex; {contains register portion of inst}
                l: integer {label to branch to} );

{ Gen a "db" instruction, decrement and branch register.
}


  begin {gendb}
    geninst(i, 2, word);
    genoprnd(keytable[regkey].oprnd);
    genlabel(l);
    lastptr^.labelcost := word;
  end {gendb} ;


Procedure gen1(i: insttype;
               datalen: datarange;
               dst: keyindex);

{ Generate a single operand instruction, using keytable[dst] as
  the destination.
}


  begin {gen1}
    geninst(i, 1, datalen);
    genoprnd(keytable[dst].oprnd);
  end {gen1} ;


procedure gen2(i: insttype;
               datalen: datarange;
               src, dst: keyindex);

{ Generate a double operand instruction, using keytable[src/dst] as
  the two operands.
}


  begin {gen2}
    geninst(i, 2, datalen);
    genoprnd(keytable[src].oprnd);
    genoprnd(keytable[dst].oprnd);
  end {gen2} ;


procedure gen_bf_inst(i: insttype;
                      datalen: datarange;
                      src, dst: keyindex;
                      offset: keyindex);

{ Generate a 68020 bit field instruction which may have 1 to 3 operands.
  If the src field is equal to lowesttemp-1, then it is omitted.  The offset
  field is either a bit_field_const or a dreg.  The width is always a constant.
  The operand node containing the offset always follows the source or
  destination node to which it applies.
}


  begin {gen_bf_inst}
    geninst(i, 2 + ord(src <> (lowesttemp - 1)), datalen);

    if src <> (lowesttemp - 1) then genoprnd(keytable[src].oprnd);

    if (i = bfexts) or (i = bfextu) then
      begin
      genoprnd(keytable[offset].oprnd);
      genoprnd(keytable[dst].oprnd);
      end
    else
      begin
      genoprnd(keytable[dst].oprnd);
      genoprnd(keytable[offset].oprnd);
      end;
  end {gen_bf_inst} ;


procedure genadcon(m: modes; {what kind of thing we're referring to}
                   what: integer; {which one, or its location}
                   where: commonlong_reloc_type {which section, if known} );

{ Generate an address constant.  Apollo only.
}

  var
    vtptr: vartablerecptr; {used to access vartable entries}


  begin {genadcon}
    if targetopsys = apollo then
      begin
      newnode;
      with lastptr^ do
        begin
        tempcount := 0;
        kind := adconnode;
        mode := m;
        sect := where;
        offset := what;
        if m in [supportcall, usercall, symbol, commonlong] then
          if m = supportcall then
            refglobal(supportcall, what)
          else if m = usercall then
            begin
            if proctable[what].externallinkage and
                  not proctable[what].bodydefined then
              refglobal(usercall, what);
            end
          else if m = symbol then
            refsymbol(name)
          else {must be commonlong}
          if where > 0 then
            begin
            vtptr := getvartableptr(where);
            if vtptr^.extvaralloc = usealloc then
              refglobal(commonlong, where);
            end;
        end {with lastptr^} ;
      end;
  end {genadcon} ;


procedure gensymboladcon(n: string8 {symbol name} );

{ Generate an address constant for a symbol reference.  Apollo only.
}


  begin {gensymboladcon}
    if targetopsys = apollo then
      begin
      newnode;
      with lastptr^ do
        begin
        tempcount := 0;
        kind := adconnode;
        mode := symbol;
        sect := unknown;
        name := n;
        refsymbol(n);
        end {with lastptr^} ;
      end;
  end {gensymboladcon} ;

procedure defglobal(m: modes; {usercall, supportcall, etc.}
                    what: integer; {which routine or other global}
                    sect: commonlong_reloc_type; {which section}
                    where: addressrange {where within the section} );

{ Append a global definition to the definition list.  Apollo only.
}

  var
    g: globalentryptr; {used to create the new entry}


  begin {defglobal}
    if targetopsys = apollo then
      begin
      new(g);
      with g^ do
        begin
        nextglobal := nil;
        done := false;
        mode := m;
        offset := what;
        reloc := sect;
        addr := where;
        end {with} ;
      if lastdef = nil then firstdef := g
      else lastdef^.nextglobal := g;
      lastdef := g;
      end;
  end {defglobal} ;


procedure defsymbol(sym: string8; {symbol name}
                    sect: commonlong_reloc_type; {which section}
                    where: addressrange {where within the section} );

{ Append a symbol definition to the definition list.  Apollo only.
}

  var
    g: globalentryptr; {used to create the new entry}


  begin {defsymbol}
    if targetopsys = apollo then
      begin
      new(g);
      with g^ do
        begin
        nextglobal := nil;
        done := false;
        mode := symbol;
        reloc := sect;
        name := sym;
        addr := where;
        end {with} ;
      if lastdef = nil then firstdef := g
      else lastdef^.nextglobal := g;
      lastdef := g;
      end;
  end {defsymbol} ;


procedure setcommonkey;

{ Check the key specified in the pseudoinstruction just read, and if
  it is a new key initialize its fields from the data in the pseudo-
  instruction.

  This uses the global "key", which is the operand for the latest
  pseudoinstruction.
}


  begin {setcommonkey}
    with keytable[key] do
      begin
      if key >= stackcounter then compilerabort(manykeys);
      if key > lastkey then
        begin
        access := noaccess;
        regsaved := false;
        properreg := key; {simplifies certain special cases}
        properindxr := key;
        validtemp := false;
        indxrsaved := false;
        regvalid := true;
        indxrvalid := true;
        packedaccess := false;
        joinreg := false;
        joinindxr := false;
        signed := true;
        signlimit := 0;
        knowneven := mc68020;
        high_word_dirty := false;
	{DRB:
        oprnd := operand(nomode, 0, 0, false, int, 1, unknown,
                         nomode, 0, 0, 0);}
        end
      else if (key <> 0) and (access <> valueaccess) then
        begin
        write('setcommonkey screwup:', key: 1,', ',lastkey:1);
        compilerabort(inconsistent);
        end;
      len := pseudoinst.len;
      refcount := pseudoinst.refcount;
      copycount := pseudoinst.copycount;
      copylink := 0;
      tempflag := false;
      instmark := lastnode + 1;
      end;
  end {setcommonkey} ;


procedure settemp(lth: datarange; {length of data referenced}
                  m: modes; {args are the same as for setvalue}
                  reg, indxr: regindex;
                  indxlong: boolean;
                  offset, offset1: integer;
                  scale: scale_range;
                  commonlong_reloc: commonlong_reloc_type);

{ Set up a temporary key entry with the characteristics specified.  This has
  nothing to do with runtime temp administration.  It strictly sets up a key
  entry.  Negative key values are used for these temp entries, and they are
  basically administered as a stack using "tempkey" as the stack pointer.
}


  begin {settemp}
    if tempkey = lowesttemp then compilerabort(interntemp);
    tempkey := tempkey - 1;
    with keytable[tempkey] do
      begin
      if m in [areg, abslong, immediatelong] then len := long
      else len := lth;
      refcount := 0;
      copycount := 0;
      copylink := 0;
      properreg := 0;
      properindxr := 0;
      access := valueaccess;
      tempflag := true;
      regsaved := false;
      indxrsaved := false;
      regvalid := true;
      indxrvalid := true;
      packedaccess := false;
      signed := true;
      signlimit := 0;
      knowneven := mc68020;
      high_word_dirty := false;
      oprnd.m := m;
      oprnd.reg := reg;
      oprnd.indxr := indxr;
      oprnd.indxlong := indxlong;
      oprnd.flavor := int_float;
      oprnd.offset := offset;
      oprnd.offset1 := offset1;
      oprnd.offset2 := 0;
      oprnd.scale := scale;
      oprnd.commonlong_reloc := commonlong_reloc;
      end;
  end {settemp} ;


procedure settempreg(lth: datarange; {length of data referenced}
                     m: modes; {normally dreg or areg}
                     reg: regindex {associated register} );

{ Shorthand call to settemp when only mode and register fields are
  meaningful.
}


  begin {settempreg}
    settemp(lth, m, reg, 0, false, 0, 0, 1, unknown);
  end; {settempreg}


procedure settempareg(reg: regindex {areg to set new key temp to});

{ Shorthand call to settemp when only mode and register fields are
  meaningful.
}


  begin {settempareg}
    settemp(long, areg, reg, 0, false, 0, 0, 1, unknown);
  end; {settempareg}


procedure settempdreg(lth: datarange; {length of data referenced}
                    reg: regindex {associated register} );

{ Shorthand call to settemp when only mode and register fields are
  meaningful.
}


  begin {settempdreg}
    settemp(lth, dreg, reg, 0, false, 0, 0, 1, unknown);
    keytable[tempkey].oprnd.flavor := int;
  end {settempdreg} ;


procedure settempfpreg(reg: regindex {associated register} );

{ Shorthand call to settemp when only mode and register fields are
  meaningful.
}


  begin {settempfpreg}
    settemp(long, fpreg, reg, 0, false, 0, 0, 1, unknown);
    keytable[tempkey].oprnd.flavor := float;
    keytable[tempkey].len := 12;
  end; {settempfpreg}


procedure settempimmediate(lth: datarange; {length of data referenced}
                           value: integer {literal value to set} );

{ Shorthand call to settemp for literal keytable entries.
}


  begin {settempimmediate}
    settemp(lth, immediate, 0, 0, false, value, 0, 1, unknown);
  end {settempimmediate} ;


procedure settempsymbol(sym: string8 {symbol name} );

{ Shorthand call to settemp for symbol references.
}


  begin {settempsymbol}
    settemp(long, symbol, 0, 0, false, 0, 0, 1, unknown);
    keytable[tempkey].oprnd.name := sym;
  end {settempsymbol} ;


procedure settempadcon(m: modes;
                       offset: integer;
                       commonlong_reloc: commonlong_reloc_type);

{ Generate an indirect reference through an address constant, for
  Apollo's weird addressing convention.  Besides being put into
  the node table, adcons are remembered in a list based in
  "firstind" and "lastind" so that duplicates can be reused.  No
  symbol adcon will be reused, since we aren't given the symbol,
  and we shouldn't be generating the same symbol more than once.
  Apollo only.
}

  var
    a: indirect_ref_ptr; {used to follow the adcon list}
    found: boolean; {loop stopper}


  begin {settempadcon}
    if targetopsys = apollo then
      begin
      a := firstind;
      found := false;
      if m <> symbol then
        while (a <> nil) and not found do
          with a^ do
            if (mode = m) and (off = offset) and
               (sec = commonlong_reloc) then
              found := true
            else a := next;
      if not found then
        begin
        new(a);
        with a^ do
          begin
          next := nil;
          if lastind = nil then
            begin
            firstind := a;
            nextind := a;
            end
          else lastind^.next := a;
          lastind := a;
          done := false;
          mode := m;
          sec := commonlong_reloc;
          off := offset;
          loc := nextadconloc;
          nextadconloc := nextadconloc + long;
          end {with} ;
        end;
      settempreg(long, relative, gp);
      keytable[tempkey].oprnd.offset := a^.loc;
      keytable[tempkey].knowneven := true;
      end;
  end {settempadcon} ;



{ Runtime environment utilities.

  Used to manipulate the temp stack at runtime.
}


procedure adjustoffsets(firstnode: nodeindex; {first node of scan}
                        change: boolean {true if offset is to change} );

{ Scan all instructions from "firstnode" to the global "lastnode" and
  adjust any which have any offsets dependent on the stack.  Also
  deallocates the temp.

  "Change" is set if there is an actual change in the stack depth for
  the range of instructions being scanned.

     The following stack-dependent items are adjusted:

     labels:       The stack depth for any labels is changed if "change"
                 is set.

     tempcount:    (in every node) Value <> 0 implies that this node is
                 dependent on the stack.  In this case, tempcount is
                 decremented and the operand is further modified as
                 described below.

     oprnd.offset: If tempcount was greater than zero, and the node is
                 an oprndnode, and "change" is true, the offset is
                 decremented by the number of bytes being removed
                 from the stack.  If tempcount was less than zero, this
                 is a pascallabel's attempt to adjust the stack pointer
                 based on the static link, so the offset is incremented
                 instead of decremented.  Also, tempcount is counted up
                 toward zero rather than down toward zero.

     stackdepth:   If this is a labelnode, and the conditions described
                 for oprnd.offset are met, stackdepth is decremented
                 similarly.  Tempcount is never negative for a labelnode.
}

  var
    newoffset: addressrange; {stack offset after adjustment}
    l: labelindex; {induction var to search for labels}
    delta: integer; {change in stack depth}
    i: integer; {induction var for misc uses}
    p: nodeptr; {used to access instruction nodes}


  begin {adjustoffsets}
    newoffset := - keytable[stackcounter + 1].oprnd.offset;
    delta := - keytable[stackcounter].oprnd.offset - newoffset;
    keytable[stackcounter].validtemp := false;

    {check all labels since firstnode and adjust stack depth}
    l := nextlabel;

    {make sure we stop}
    labeltable[0].nodelink := firstnode;

    if change then
      while labeltable[l].nodelink > firstnode do
        begin
        labeltable[l].stackdepth := labeltable[l].stackdepth - delta;
        l := l - 1;
        end;

    {now scan the nodes and adjust instructions}

    for i := firstnode to lastnode do
      begin
      if bigcompilerversion then p := @(bignodetable[i]);

      with p^ do
        if tempcount <> 0 then
          if tempcount < 0 then
            begin
            if needcaching then blocksin[1].written := true;
            tempcount := tempcount + 1;
            if change then
              if kind = oprndnode then oprnd.offset := oprnd.offset + delta
              else compilerabort(badadjust);
            end
          else
            begin
            if needcaching then blocksin[1].written := true;
            tempcount := tempcount - 1;
            if change then
              if kind = oprndnode then oprnd.offset := oprnd.offset - delta
              else if kind = labelnode then stackdepth := stackdepth - delta
              else compilerabort(badadjust);
            end;
      end;

    stackcounter := stackcounter + 1;

  end {adjustoffsets} ;

procedure aligntemps;

{ Make sure run-time stack aligns with the keytable model.
}


  begin {aligntemps}
    with keytable[stackcounter], oprnd do
      if offset <> - stackoffset then
        begin
        settempimmediate(long, - stackoffset - offset);
        settempareg(sp);
        gen2(sub, long, tempkey + 1, tempkey);
        stackoffset := - offset;
        end;
  end {aligntemps} ;


procedure newtemp(size: addressrange {size of temp to allocate} );

{ Create a new temp. Temps are allocated from the top of the keys,
  while expressions are allocated from the bottom.

  If our model shows a temp of the right size at the top of the
  stack, but the run-time stack has not yet allocated it, we will
  use that temp instead of allocating another.  This saves time
  and trouble in dealing with nested function calls.  The only
  time newtemp will be called with a mismatch between the model
  and the run-time stack is when we know that the new temp will
  be used to replace the previous temp.
}

  var
    length: addressrange; {actual length of stack temp}


  begin {newtemp}
    length := (size + (word - 1)) and ( - word);

    {align stack if necessary}
    if size <> keytable[stackcounter].len then
      aligntemps;

    if stackoffset + length > maxstackdepth
      then maxstackdepth := stackoffset + length;

    stackcounter := stackcounter - 1;
    if stackcounter <= lastkey then compilerabort(manykeys)
    else
      begin
      with keytable[stackcounter] do
        begin
        len := size;
        access := valueaccess;
        tempflag := false;
        validtemp := true;
        regvalid := true;
        indxrvalid := true;
        regsaved := false;
        indxrsaved := false;
        packedaccess := false;
        refcount := 0;
        instmark := lastnode + 1;
        high_word_dirty := false;
        oprnd.offset := - stackoffset - length;
        oprnd.m := relative;
        oprnd.reg := sp;
        oprnd.indxr := 0;
        oprnd.flavor := int_float;
        oprnd.offset1 := 0;
        oprnd.scale := 1;
        oprnd.commonlong_reloc := unknown;
        knowneven := true;
        signed := true;
        signlimit := 0;
        end;
      end;
  end {newtemp} ;

procedure delete(m: nodeindex; {first node to delete}
                 n: nodeindex {number of nodes to delete} );

{ Delete "n" instructions starting at node "m".  This is done by
  converting them to "nop's" which have no length and are not
  emitted.
}

  var
    i, j: nodeindex; {induction vars for deletion}
    count: operandrange; {number of operands accessed by deleted inst}
    p: nodeptr; {used to access nodes to be deleted}


  procedure unlinklabelnode(m: nodeindex {label node to unlink});

{ Search the brlink chain for a reference to this node, and if found, delete
  the reference.
}

    var
      cp1, cp2: brlinkptr; {used to follow brlink chain}
      p1, p2: nodeptr; {used to access label nodes}
      finished: boolean; {the only possible reference has been removed}


    begin {unlinklabelnode}
      finished := false;
      cp1 := firstbr;
      while (cp1 <> nil) and not finished do
        begin {search the brlinks}
        if bigcompilerversion then p1 := @(bignodetable[cp1^.n + 1]);

        if cp1^.n + 1 = m then
          begin
          cp1^.n := p1^.brnodelink;
          if cp1^.n = 0 then
            begin {delete the brlink}
            if cp1 = firstbr then firstbr := cp1^.nextbr
            else cp2^.nextbr := cp1^.nextbr;
            dispose(cp1);
            end;
          finished := true;
          end
        else
          begin
          while (p1^.brnodelink <> 0) and not finished do
            begin {search the nodes}
            p2 := p1;
            if p2^.brnodelink + 1 = m then
              begin
              blocksin[1].written := true;

              if bigcompilerversion then
                p1 := @(bignodetable[p2^.brnodelink + 1]);

              p2^.brnodelink := p1^.brnodelink;
              finished := true;
              end
            else if bigcompilerversion then
              p1 := @(bignodetable[p2^.brnodelink + 1]);
            end {while} ;
          cp2 := cp1;
          cp1 := cp2^.nextbr;
          end;
        end {while} ;
    end {unlinklabelnode} ;


  begin {delete}
    for i := 1 to n do
      begin
      if bigcompilerversion then p := @(bignodetable[m]);

      if p^.kind in [instnode, errornode] then
        count := p^.oprndcount
      else begin
        if p^.kind <> stmtref then
          begin
          write('attempt to delete non-inst node:', m: 1);
          compilerabort(inconsistent);
          end;
        p^.kind := instnode;
        p^.labelled := false;
        count := 0;
        end;
      p^.tempcount := 0;
      p^.inst := nop;
      p^.oprndcount := 0;
      m := m + 1;
      for j := 1 to count do
        begin
        if bigcompilerversion then p := @(bignodetable[m]);

        if p^.kind = labelnode then
          begin
          unlinklabelnode(m);

          if bigcompilerversion then p := @(bignodetable[m]);
          end
        else blocksin[1].written := true;
        p^.tempcount := 0;
        p^.kind := instnode;
        p^.labelled := false;
        p^.inst := nop;
        p^.oprndcount := 0;
        m := m + 1;
        end;
      end;
  end {delete} ;



procedure zaptemps(cnt: integer; {Number of temps do delete}
                   change: boolean {set if temp was never used} );

{ Pop "cnt" temps from the temp stack, adjusting instructions emitted
  since the temp was allocated.  If this temp was never used, ("change"
  set), the instruction which created it is deleted, and the offsets
  of any instructions depending on the stack are decremented.  Otherwise
  only the stack bookkeeping data of the instructions are decremented.
}

  var
    t: integer; {induction var for counting temps}


  begin {zaptemps}
    for t := cnt downto 1 do
      with keytable[stackcounter] do
        begin
        if change then delete(instmark, 1);
        adjustoffsets(instmark, change);
        end;
  end {zaptemps} ;


procedure returntemps(cnt: integer {number of temps to return} );

{ Return temps from the stack, decrementing the book-keeping fields of
  any instructions emitted since they were allocated
}


  begin {returntemps}
    zaptemps(cnt, false);
    stackoffset := - keytable[stackcounter].oprnd.offset;
  end {returntemps} ;


function uselesstemp: boolean;

{ True if the top temp on the tempstack is no longer needed.
}


  begin {uselesstemp}
    uselesstemp := (keytable[stackcounter].instmark >
                   context[contextsp].lastbranch) and
                   (keytable[stackcounter].refcount = 0);
  end {uselesstemp} ;


procedure adjusttemps;

{ Remove any temps which are no longer required.  In the process, the
  stack pointer must be adjusted, and any instructions emitted since the
  temp was allocated must be adjusted if they addressed anything relative
  to sp.

  If the temp was never used, the instruction which created it is deleted,
  and intermediate offsets must be modified because the stack pointer
  was never decremented.  Otherwise, the stack book-keeping field in the
  node is decremented but the offset itself is left alone.

  The global "adjustdelay" is used to delay adjusting temps by one instruction
  when the next instruction is going to be a "savecontext".  This avoids much
  useless stack adjustment in multi-branch statements.
}

  var
    oldstackoffset: addressrange; {stack offset prior to popping}
    adjustamount: integer; {amount to pop off stack}


  begin {adjusttemps}
    adjustamount := 0;
    if not adjustdelay then
      while uselesstemp do
        begin
        oldstackoffset := stackoffset;

        {first remove temps which were used}

        while uselesstemp and keytable[stackcounter].tempflag do
          returntemps(1);
        adjustamount := adjustamount + oldstackoffset - stackoffset;

        {now get rid of any unused temps}

        while uselesstemp and not keytable[stackcounter].tempflag do
          zaptemps(1, true);
        stackoffset := - keytable[stackcounter].oprnd.offset;
        end;
    if adjustamount <> 0 then
      begin
      settempimmediate(targetintsize, adjustamount);
      settempareg(sp);
      gen2(add, targetintsize, tempkey + 1, tempkey);
      end;
  end {adjusttemps} ;


procedure callsupport(bn: libroutines {support routine to call} );

 { Call the support library. }

  begin {callsupport}
    if switcheverplus[sharecode] then
      begin
      markareg(0);
      settempadcon(supportcall, ord(bn), unknown);
      settempareg(0);
      gen2(move, long, tempkey + 1, tempkey);
      keytable[tempkey].oprnd.m := indr;
      end
    else if pic_enabled and not mc68020 then
      begin
      settempareg(getareg);
      settemp(long, pic_splat_pcrel, 0, 0, false, 10, 0, 1, unknown);
      gen2(lea, long, tempkey, tempkey + 1);
      settemp(word, pic_supportcall, 0, 0, false, ord(bn), 6, 1, unknown);
      gen2(adda, long, tempkey, tempkey + 2);
      tempkey := tempkey + 2;
      keytable[tempkey].oprnd.m := indr;
      end
    else
      settemp(word, supportcall, 0, 0, false, ord(bn), 0, 1, unknown);
    gen1(jsr, 0, tempkey);
    paramlist_started := false; {reset the switch}
  end {callsupport} ;


{ Peep-hole optimization.

  The following procedures make optimizations based on the actual
  generated code.  They make local code improvement based on limited
  context.

  Specific optimizations are discussed in the procedures which make them.
}


procedure fixsimplestuff;

{ Make modifications to single instructions based on very local context.

  Optimizations made include:

  1.    Comparisons against zero are converted to test instructions.

  2.    If the instruction is a test, and the condition code is already
        set properly by the previous instruction, the test instruction
        will be deleted.  Note that test always clears the c and v bits,
	which requires some careful analysis to see if this is ok.

  3.    Consecutive adds or subtracts of literal data to the stack pointer
        are folded together.

  4.    A pop from the stack followed by a subtract literal from the sp
        (to reserve space) is converted to a move from the top-of-stack,
        and the subtraction operand is reduced by the size of the data item.

  5.    A move to the top of the stack preceded by a subtract literal from
        the sp (of at least the size of the item being moved) is changed to
        a push, and the subtraction operand is reduced by the size of the
        data item.

  6.    Moves of certain long immediate data to the stack or to an address
        register may be replaced by PEA or LEA, saving two bytes each.

  7.    A push to the stack preceeded by an add literal to the sp (to flush
        arguments) is converted to a move to the top-of-stack, and the
        addition operand is reduced by the size of the data item.

  8.    Adds and subtracts of a literal zero are deleted.

  9.    Adds and subtracts of literal data from -8 to +8 are replaced by
        the corresponding "addq" and "subq" instructions.

 10.    Moves of literal data from -128 to +127 to the data registers are
        replaced by the corresponding "moveq" instructions.

 11.    Moves of a literal zero to destinations other than address registers
        are changed to a clear to those destinations.

 12.    Relative address modes with a zero offset are changed to register
        deferred address modes.

 13.    Moves, adds, subs, and cmps involving an address register are
        changed to the appropriate "A" suffix forms.  If an immediate
        long is used unnecessarily, it is shortened to a word.

 14.    Instructions other than moves having immediate source operands
        are changed to the appropriate "I" suffix form.

 15.    Lea instructions which produce no effect ("lea (an),an") are
        deleted, and those of the form "lea (an),am" are changed to
        "movea.l an,am".

 16.    An unlabled FMOVE <ea>,FPn preceeded by an FMOVE FPn,<ea> for the
        same register and <ea> is deleted.

 17.    An unlabled MOVE <ea>,Rn preceeded by an MOVE Rn,<ea> for the
        same register and <ea> is deleted.

  The approach is simply to scan the list of instructions and change them
  as you go.
}

  var
    opct: operandrange; {counts number of operands required by instruction}
    i: nodeindex; {induction var for scanning nodes}
    ip: nodeptr; {used to access instruction node being changed}
    sp: nodeptr; {used to access source operand node }
    dp: nodeptr; {used to access destination operand node}

    xi: nodeindex; {index of previous instruction}
    xip, xsp, xdp: nodeptr; {pointers to previous instruction and operands}

    straightline: boolean; {true if no labels intervene between the previous
                            and the current instruction nodes}
    waslabelled: boolean; {used to remember whether straightline should be
                           cleared when we have deleted the current
                           instruction}
    firsttime: boolean; {xip, xdp, and xsp are uninitialized}
    tempoffset: integer; {temporary for arithmetic on operand node offsets}
    check_last_test: boolean; {the last instruction may be a deletable tst
			       instruction.}


  function lowprecisionlong(p: nodeptr): boolean;

{ This function returns true if the operand node referenced by "p" is an
  "immediatelong" mode with the high order word "offset1" being a proper
  sign extension to the low order word "offset", or is an "immediate" mode.
  This function is used mainly by addq/subq and moveq and lea optimizers.
}


    begin {lowprecisionlong}
      with p^, oprnd do
        lowprecisionlong := ((m = immediatelong) and ((offset < 0) and
                            (offset1 = - 1) or (offset >= 0) and
                            (offset1 = 0))) or ((m = immediate) and
                            (offset >= - 32768) and (offset <= 32767));
    end; {lowprecisionlong}


  function equaloperands(p, q: nodeptr): boolean;

{ Compare two operand nodes for equality.
}


    begin
      equaloperands := (p^.oprnd.m = q^.oprnd.m) and
                       (p^.oprnd.reg = q^.oprnd.reg) and
                       (p^.oprnd.indxr = q^.oprnd.indxr) and
                       (p^.oprnd.indxlong = q^.oprnd.indxlong) and
                       (p^.oprnd.offset = q^.oprnd.offset) and
                       (p^.oprnd.offset1 = q^.oprnd.offset1) and
                       (p^.oprnd.commonlong_reloc = q^.oprnd.commonlong_reloc);
    end; {equaloperands}


  procedure zapnode(p: nodeptr);

{ Change the operand node at "p" to an instruction node with NOP parameters.
}


    begin
      with p^ do
        begin
        tempcount := 0;
        kind := instnode;
        inst := nop;
        oprndcount := 0;
        labelled := false;
        oprndlength := 0;
        end;
    end; {zapnode}




  begin {fixsimplestuff}
    i := 0;
    xi := 0;
    firsttime := true;
    straightline := false;
    check_last_test := false;

    while i < lastnode do
      begin
      repeat
        i := i + 1;
        if bigcompilerversion then ip := @(bignodetable[i]);
        with ip^ do
          if (kind = instnode) and labelled or
             (kind in [labelnode, labeldeltanode, relnode, datanode]) then
            straightline := false;
      until (ip^.kind = instnode) and (ip^.inst <> nop) or (i > lastnode);
      waslabelled := ip^.labelled;

      with ip^ do
        if (kind = instnode) and (i <= lastnode) then
          begin
          blocksin[1].written := true;

          if inst in monadicinsts then
            opct := 1
          else if inst in dyadicinsts then
            opct := 2
          else
            opct := 0;

          { Consistency check: our expectation of the number of operands which
            an instruction should have vs. the number carried in the
            instruction node itself. }

          if (opct <> oprndcount) and (inst <> nop) and
             not (inst in bitfieldinsts) then
            begin
            writeln('Fixsimplestuff: bad opct in node ', i: 1);
            compilerabort(inconsistent)
            end;

          { Consistency check: quick, immediate, and address forms are
            generated at this stage, and should not be present earlier.
            Removed ADDA from this list so we can generate it in callsupport.
          }

          if inst in
             [addq, subq, moveq, addi, subi, cmpi, andi, ori, eori,
             suba, cmpa, movea] then
            begin
            writeln('Fixsimplestuff: bad inst in node ', i: 1);
            compilerabort(inconsistent)
            end;

          if opct = 0 then
            sp := ip
          else
            begin
            if bigcompilerversion then sp := @(bignodetable[i + 1]);

            if not (sp^.kind in [oprndnode, relnode, labelnode]) then
              begin
              writeln('Fixsimplestuff: missing src in node ', i + 1: 1);
              compilerabort(inconsistent)
              end;
            end;

          if opct < 2 then
            dp := sp
          else
            begin
            if bigcompilerversion then dp := @(bignodetable[i + 2]);

            if not (dp^.kind in [oprndnode, relnode, labelnode]) then
              begin
              writeln('Fixsimplestuff: missing dst in node ', i + 2: 1);
              compilerabort(inconsistent)
              end;
            end;

          if firsttime then
            begin
            xip := ip;
            xsp := sp;
            xdp := dp;
            firsttime := false;
            end;

	  { See if the last instruction was a deletable test.
          }
	  if check_last_test and (xip^.inst = tst) and
	     not (ip^.inst in [bge, bgt, bhi, ble, bls, blt, bvc, bvs]) then
	    begin
{	    if peeping then
	      begin
	      peep[2] := peep[2] + 1;
	      peep[0] := peep[0] + instlength(xi);
	      end;} {peeping}
	    delete(xi, 1);
	    end;

	  check_last_test := false;

          { 1. Check for comparisons against zero }

          if (inst = cmp) and (dp^.oprnd.m <> areg) then
            with sp^.oprnd do
              if (offset = 0) and ((m = immediate) or (m = immediatelong) and
                 (offset1 = 0)) then

                begin
                inst := tst; { change the opcode to a test }
                oprndcount := 1; { reflect the new operand count }
                sp^ := dp^; {slide destination back to source}
                zapnode(dp); {change destination operand to nop}
{                if peeping then
                  begin
                  peep[1] := peep[1] + 1;
                  peep[0] := peep[0] + max(word, oprndlength);
                  end; } {peeping}
                end;

          { 2. Check for test instructions which can be deleted }

          if straightline and (inst = tst) then
            if xip^.inst in
               [andinst, andi, clr, divs, divu, eor, eori, ext, move, moveq,
                notinst, orinst, ori, tst, bfins, bfexts, bfextu] then
              begin
              if ((xip^.inst in monadicinsts) and (equaloperands(sp, xsp))) or
                 ((xip^.inst in dyadicinsts) and (equaloperands(sp, xdp))) then
                begin
{                if peeping then
                  begin
                  peep[2] := peep[2] + 1;
                  peep[0] := peep[0] + instlength(i);
                  end;} {peeping}
                delete(i, 1);
                end;
	      end
	    else
	      check_last_test := (xip^.inst in
               [add, addi, addq, asl, asr, cmpa, cmp, cmpi, cmpm, lsl, lsr,
		move, moveq, muls, mulu, neg, rol, ror, roxl, roxr,
                sub, subi, subq]) and (((xip^.inst in monadicinsts) and
		equaloperands(sp, xsp)) or ((xip^.inst in dyadicinsts) and
		equaloperands(sp, xdp)));


          { 3. Check for consecutive arithmetic on the stack pointer }

          if straightline and (inst in [add, sub]) then
            if (sp^.oprnd.m in [immediate, immediatelong]) and
               (dp^.oprnd.m = areg) and (dp^.oprnd.reg = 7) then {test
                 previous inst}

              if (xip^.inst in [addq, adda, subq, suba]) and
                 (xsp^.oprnd.m = immediate) and (xdp^.oprnd.m = areg) and
                 (xdp^.oprnd.reg = 7) then

                begin {compute the net offset of the two insts}
                tempoffset := xsp^.oprnd.offset;
                if xip^.inst in [subq, suba] then
                  tempoffset := - tempoffset;
                if inst = add then
                  tempoffset := sp^.oprnd.offset + tempoffset
                else
                  tempoffset := sp^.oprnd.offset - tempoffset;
                if tempoffset < 0 then
                  if inst = add then
                    inst := sub
                  else
                    inst := add;
                sp^.oprnd.offset := abs(tempoffset);
{                if peeping then
                  begin
                  peep[3] := peep[3] + 1;
                  peep[0] := peep[0] + instlength(xi);
                  end;} {peeping}
                delete(xi, 1); {delete previous instruction}
                end

          { 4. Check for "sub  #xxx,sp" preceeded by "mov  (sp)+,<ea>"

               (we may demote the preceeding (sp)+ to (sp) and reduce
                the magnitude of the literal subtraction.  If the new value
                goes negative, it will be turned into an "addq" opcode by #8,
                and may subsequently be eliminated altogether by #6.
          }
              else {preceeding instruction was not in [addq, adda, subq, suba]}

              if straightline and (inst = sub) and (xip^.inst = move) and
                 (xsp^.oprnd.m = autoi) and (xsp^.oprnd.reg = 7) and
              {make sure the pop doesn't target the stack pointer: }
                 not ((xdp^.oprnd.m = areg) and
                 (xdp^.oprnd.reg = 7)) and not ((xdp^.oprnd.m = indexed) and
                 (xdp^.oprnd.offset + max(word, xip^.oprndlength) > 128)) then
                begin
                xsp^.oprnd.m := indr; {change address mode from pop to
                                       indirect}
                sp^.oprnd.offset := sp^.oprnd.offset - max(word,
                                    xip^.oprndlength);
                with xdp^.oprnd do
                  if (m in [indr, relative, indexed]) and (reg = 7) then
                    begin
                    offset := offset + max(word, xip^.oprndlength);

                    if m = indr then
                      m := relative;
{                    if peeping then
                      peep[4] := peep[4] + 1; }
                    end;
                end;

          { 5. Check for a two-step push to the stack }

          if inst = move then
            begin
            if ((dp^.oprnd.m = indr) or (dp^.oprnd.m = relative) and
               (dp^.oprnd.offset = 0)) and (dp^.oprnd.reg = 7) and
               (oprndlength > byte) then

                { We can't allow this optimization if this is a move of
                  a byte to the stack because the hardware pushes two
                  bytes with our byte in the low-order, not the high-order,
                  and the compiler likes to move several bytes in
                  sequence, causing the stack to get screwed up.  If
                  we could look ahead one instruction to be sure there
                  is not another move of a byte, then we could get away
                  with this.
                }
              begin
              tempoffset := xsp^.oprnd.offset - max(word, oprndlength);
              if straightline and (xip^.inst in [suba, subq]) and
                 (xdp^.oprnd.m = areg) and (xdp^.oprnd.reg = 7) and
                 lowprecisionlong(xsp) and (tempoffset >= 0) then
                begin {make it a push}
                dp^.oprnd.m := autod;
                with sp^.oprnd do
                  if (m in [indr, relative, indexed]) and (reg = 7) then
                    begin
                    offset := offset - max(word, oprndlength);
                    if m = indr then
                      m := relative;
                    end;
                if tempoffset = 0 then
                  delete(xi, 1)
                else
                  begin {update}
                  xsp^.oprnd.offset := tempoffset;
                  if xip^.inst = suba then
                    if tempoffset <= 8 then
                      xip^.inst := subq;
                  end; {update}
                end;
              end;

            { 6. Check for long immediate push of essentially short data }

            if (dp^.oprnd.m = autod) and (dp^.oprnd.reg = 7) then
              begin {push instruction}
              if (ip^.oprndlength = long) and lowprecisionlong(sp) then
                begin
                inst := pea;
                oprndcount := 1;
                sp^.oprnd.m := absshort;
                zapnode(dp); {change destination operand to nop}
{                if peeping then
                  begin
                  peep[5] := peep[5] + 1;
                  peep[0] := peep[0] + 2;
                  end;} {peeping}
                end

              { 7. Check for "mov  <ea>,-(sp)" preceeded by "add  #xxx,sp"

                   (we may be able to demote the preceeding add to a quick form,
                   or possibly eliminate it altogether).
              }
              else {not push of long immediate data}
              if straightline and (xip^.inst in [adda, addq]) then
                begin
                if (xsp^.oprnd.m = immediate) and (xdp^.oprnd.m = areg) and
                   (xdp^.oprnd.reg = 7) and
                {must be flushing as much or more space than is being pushed}
                   (xsp^.oprnd.offset >= max(word, oprndlength)) and
                   not ((sp^.oprnd.m = indexed) and
                   (sp^.oprnd.offset + max(word, oprndlength) > 128)) then

                  begin {reduce arithmetic on sp in previous node}
                  dp^.oprnd.m := indr; {change address mode from push to
                                        indirect}
                  tempoffset := xsp^.oprnd.offset - max(word, oprndlength);
                  with sp^.oprnd do
                    if (m in [indr, relative, indexed]) and (reg = 7) then
                      begin
                      offset := offset + max(word, oprndlength);
                      if m = indr then
                        m := relative;
                      end;
                  if tempoffset = 0 then
                    begin
                    delete(xi, 1);
{                    if peeping then
                      peep[0] := peep[0] + 2; }
                    end
                  else
                    begin {update}
                    xsp^.oprnd.offset := tempoffset;
                    if xip^.inst = adda then
                      if tempoffset <= 8 then
                        begin
                        xip^.inst := addq;
{                        if peeping then
                          peep[0] := peep[0] + 2; }
                        end;
                    end; {update}
{                  if peeping then
                    peep[6] := peep[6] + 1; }
                  end; {reduce arithmetic}
                end;
              end; {push instruction}
            end; {move}

          { 8. Check for adds and subtracts of a literal zero }

          if inst in [add, sub] then
            with sp^.oprnd do
              if (offset = 0) and ((m = immediate) or (m = immediatelong) and
                 (offset1 = 0)) then
                begin
{                if peeping then
                  begin
                  peep[7] := peep[7] + 1;
                  peep[0] := peep[0] + instlength(i);
                  end;} {peeping}
                delete(i, 1);
                end

          { 9. Check for adds and subtracts of literal data from -8 to +8 }

              else {in [add,sub] but not 0}
              if (offset >= - 8) and (offset <= + 8) and
                 lowprecisionlong(sp) then
                begin
                m := immediate; {drive immediatelong to immediate for
                                 instlength}
                if offset < 0 then
                  begin { invert both value and opcode }
                  offset := - offset;
                  if inst = add then
                    inst := subq
                  else
                    inst := addq
                  end
                else if inst = add then
                  inst := addq
                else
                  inst := subq;
{                if peeping then
                  begin
                  peep[8] := peep[8] + 1;
                  peep[0] := peep[0] + max(word, oprndlength);
                  end;} {peeping}
                if dp^.oprnd.m = areg then
                  oprndlength := word;
                end;

          { 10. Check for moves of literal data from -128 to +127 to D-regs }

          if inst = move then
            with sp^.oprnd do
              if (dp^.oprnd.m = dreg) and (offset >= - 128) and
                 (offset <= 127) and lowprecisionlong(sp) then
                begin
{                if peeping then
                  begin
                  peep[9] := peep[9] + 1;
                  peep[0] := peep[0] + max(word, oprndlength);
                  end;} {peeping}
                inst := moveq;
                m := immediate; {drive immediatelong to immediate for
                                 instlength}
                oprndlength := byte;
                end;

          { 11. Check for moves of a literal zero to <ea>s other than A-regs }

          if inst = move then
            with sp^.oprnd do
              if (offset = 0) and ((m = immediate) or (m = immediatelong) and
                 (offset1 = 0)) and (dp^.oprnd.m <> areg) then
                begin
{                if peeping then
                  begin
                  peep[10] := peep[10] + 1;
                  peep[0] := peep[0] + max(word, oprndlength);
                  end;} {peeping}
                inst := clr; { change the opcode to a clear }
                oprndcount := 1; { reflect the new operand count }
                sp^ := dp^; { slide destination operand up one node }
                zapnode(dp); {change destination operand to nop}
                end;

          { 12. Check for relative address modes with a zero offset }

          if opct >= 1 then { a source operand is present }
            with sp^, oprnd do
              if (kind = oprndnode) and (m = relative) and (offset = 0) then
                begin
{                if peeping then
                  begin
                  peep[11] := peep[11] + 1;
                  peep[0] := peep[0] + 2;
                  end;} {peeping}
                m := indr;
                end;

          if opct = 2 then { a destination operand is present }
            with dp^, oprnd do
              if (kind = oprndnode) and (m = relative) and (offset = 0) then
                begin
{                if peeping then
                  begin
                  peep[11] := peep[11] + 1;
                  peep[0] := peep[0] + 2;
                  end;} {peeping}
                m := indr;
                end;

          { 13. Check for moves, adds, subs, and cmps involving an A-reg }

          if inst in [move, cmp, add, sub] then
            if dp^.oprnd.m = areg then

              begin
              inst := pred(inst); { select the "xxxA" form of mnemonic }
              if lowprecisionlong(sp) then
                begin {we can shorten the operation}
{                if peeping then
                  begin
                  peep[12] := peep[12] + 1;
                  if oprndlength = long then
                    peep[0] := peep[0] + 2;
                  end;} {peeping}
                sp^.oprnd.m := immediate;
                oprndlength := word;
                end;
              end;

          { 14. Check for immediate source operands in non-move instructions }

          if inst in [add, cmp, sub, andinst, eor, orinst] then
            with sp^.oprnd do
              if (m = immediate) or (m = immediatelong) then

                begin
                inst := succ(inst); { select the "xxxI" form of mnemonic }
{                if peeping then
                  begin
                  peep[13] := peep[13] + 1;
                  peep[0] := peep[0] + 2;
                  end;} {peeping}
                end;

          { 15. Check for instructions of the form LEA (AN),AM }

          if (inst = lea) and (sp^.oprnd.m = indr) then
            begin
{            if peeping then
              peep[14] := peep[14] + 1; }
            if (sp^.oprnd.reg = dp^.oprnd.reg) then
              begin
              delete(i, 1);
{              peep[0] := peep[0] + 2; }
              end
            else
              begin { change lea to movea }
              inst := movea;
              oprndlength := long;
              sp^.oprnd.m := areg;
              end;
            end;

          { 16. Check for 68881 instructions in the pattern:

                      FMOVE   FPn,<ea>
                      FMOVE   <ea>,FPn

                Remove the redunant fetch if the second instruction is
                unlabeled and <ea> is not abslong (i.e., origined).
          }

          if mc68881 and straightline and (xip^.inst = fmove) and
             (ip^.inst = fmove) and
             (xsp^.oprnd.m = fpreg) and (dp^.oprnd.m = fpreg) and
             (dp^.oprnd.reg = xsp^.oprnd.reg) and
             (xdp^.oprnd.m <> fpreg) and (sp^.oprnd.m <> fpreg) and
             equaloperands(xdp, sp) and (sp^.oprnd.m <> abslong) then
            begin
            delete(i, 1);  { remove the FMOVE }
{            if peeping then
              begin
              peep[16] := peep[16] + 1;
              peep[0] := peep[0] + instlength(xi);
              end;} {peeping}
            end;

          { 17. Check for move instructions in the pattern:

                      MOVE   Dn,<ea>
                      MOVE   <ea>,Dn

                Remove the redunant fetch if the second instruction is
                unlabeled and <ea> is not abslong (i.e., origined).
          }

          if straightline and (xip^.inst = move) and
             (ip^.inst = move) and
             (xsp^.oprnd.m = dreg) and (dp^.oprnd.m = dreg) and
             (dp^.oprnd.reg = xsp^.oprnd.reg) and
             (xdp^.oprnd.m <> dreg) and (sp^.oprnd.m <> dreg) and
             equaloperands(xdp, sp) and (sp^.oprnd.m <> abslong) then
            begin
            delete(i, 1);  { remove the MOVE }
{            if peeping then
              begin
              peep[17] := peep[17] + 1;
              peep[0] := peep[0] + instlength(xi);
              end;} {peeping}
            end;

          if inst <> nop then {we didn't just delete it}
            begin {retain the previous instruction}
            xi := i;
            xip := ip;
            xsp := sp;
            xdp := dp;
            straightline := true;
            end
          else
            straightline := not waslabelled;

          end; { with ip }

      i := i + opct; { adjust to last (operand) node accessed }
      end; { while i < lastnode }

  end {fixsimplestuff} ;


procedure fixcmps;

{ A special case optimization which is useful primarily as a result
  of the comparison pattern generated by case statement branch
  trees.  It may also pick up a few others by chance.

  Note:  This is one of a class of peephole optimizations which scan
  forward in the code tree.  More might be added to make more use of
  autoincrement and autodecrement modes on addressing registers.
}

    var
      i, j: nodeindex; {for scanning the list}
      m, n: nodeptr; {for getting at nodes}

    begin
      i := 0;
      while i < lastnode do
	begin
	repeat
	  i := i + 1;
	  if bigcompilerversion then n := @(bignodetable[i]);
	until (n^.kind = instnode) or (i = lastnode);

	if n^.kind = instnode then
	  if (n^.inst = cmp) or (n^.inst = cmpi) then
	    begin
	    j := i;
	    repeat
	      repeat
		j := j + 1;
		if bigcompilerversion then m := @(bignodetable[j]);
	      until (m^.kind = instnode) or (j >= lastnode);
	      if not m^.labelled and eqinst(i, j) then delete(j, 1);
	    until m^.labelled or not (m^.inst in [nop, beq..bvs]) or
		  (j >= lastnode);
	    end;
	end;
    end; {fixcmps}



procedure stuffregisters;

{ This peephole routine attempts to fill unused address registers
  with address expressions, and unused data registers with operands.
  This can save both space and time, since the resulting register
  addresses save at least one word per substitution as well as
  at least one memory cycle.  The basic algorithm is to scan the
  generated code for common, easily-replaced address modes and
  immediate operands, which are saved and weighted by the number of
  occurences.  The most-used addresses and operands are then stuffed
  into unused registers and the generated code is rescanned, replacing
  the winning address modes with the shorter register addresses.
}

  const
    maxoprndkept = 25; {maximum number of candidates tracked}
    maxcountkept = 1023; {maximum reference count kept per candidate}
    firstusecost = 1; {number of words lost by stuffing for one use}
    wordusegain = 1; {advantage per subsequent use of stuffing a word}
    longusegain = 2; {advantage per subsequent use of stuffing a long}

  type
    operandentry =
      packed record {info for one address or operand}
        value: - firstusecost..maxcountkept; {value of stuffing this operand}
        newreg: regindex; {replacement register filled by assignregisters}
        oprndlength: datarange; {data size from instruction}
        oprnd: operand; {address or operand filled by findcandidates}
      end;
    operandtableindex = 0..maxoprndkept;
    operandtable = array [operandtableindex] of operandentry;
    
  var
    addresses: operandtable; {list of addresses}
    operands: operandtable; {list of operands}
    freeaddrregs: integer; {number of free address registers to work with}
    freedataregs: integer; {number of free data registers to work with}
    freefpregs: integer; {number of free floating-point registers to work with}
    lastaddr: operandtableindex; {last entry in addresses}
    lastoprnd: operandtableindex; {last entry in operands}
    datasize: datarange; {data size from last instruction node}
    stuffimmediateok: boolean; {instruction can use register instead}
    suffixIinstruction: boolean; {instruction is of suffix I form}
    leainstruction: boolean; {instruction is lea}
    slused: boolean; {static link is being used}
    dbgused: boolean; {debugging register is in use}
    pic_own_used: boolean; { pic and own used together }


  procedure countfreeregisters;

{ Uses global regused arrays to determine how many free registers
  are available for our use in this procedure.  The special code
  for sl is due to the fact that genblk does not mark sl as used
  unless it is actually destroyed, to minimize unneeded saving
  and restoring of this special register.
}

    var
      i: regindex; {induction for scanning regused arrays}


    begin
      freeaddrregs := 0;
      for i := 0 to sl do
        if not aregused[i] then
          freeaddrregs := freeaddrregs + 1;

      slused := proctable[blockref].intlevelrefs;
      if slused and not aregused[sl] then
        freeaddrregs := freeaddrregs - 1;


      { PIC code uses pic_own_base for the own section address.
      }
      pic_own_used := pic_enabled and (ownsize > 0) and proctable[blockref].ownused;

      dbgused := (switchcounters[debugging] > 0) or
                 (switchcounters[profiling] > 0);

      if {(dbgused and not aregused[debuglink]) or}
         (pic_own_used and not aregused[pic_own_base]) then
        freeaddrregs := freeaddrregs - 1;
      freedataregs := 0;
      freefpregs := 0;
      for i := 0 to 7 do
        begin
        if not dregused[i] then
          freedataregs := freedataregs + 1;
        if not fpregused[i] then
          freefpregs := freefpregs + 1;
        end;
    end {countfreeregisters} ;


  function goodaddress(p: nodeptr): boolean;

{ Returns true if the current node contains an 'interesting'
  address mode.  If so, addresses[0] contains the address.
}


    begin
      if (p^.oprnd.m = commonlong) and pic_enabled then goodaddress := false
      else if (p^.oprnd.m = relative) and ((p^.oprnd.reg = gp) or
         (p^.oprnd.reg = fp) or slused and (p^.oprnd.reg = sl)) or
         (p^.oprnd.m in
         [pcrelative, absshort, commonlong, supportcall, usercall]) then
        begin
        goodaddress := true;
        addresses[0].oprnd := p^.oprnd;
        addresses[0].oprndlength := datasize;
        end
      else
        goodaddress := false;
    end {goodaddress} ;


  function goodoperand(p: nodeptr): boolean;

{ Returns true if the current node contains an 'interesting'
  mode (immediate).  If so, operands[0] contains the operand.
}


    begin
      if stuffimmediateok and (p^.oprnd.m in [immediate, immediatelong]) then
        begin
        goodoperand := true;
        operands[0].oprnd := p^.oprnd;
        operands[0].oprndlength := datasize;
        end
      else
        goodoperand := false;
    end {goodoperand} ;


  function addressesmatch(var known, trial: operandentry): boolean;

{ Compares addresses for equality.
}


    begin
      addressesmatch := ((known.oprnd.m = trial.oprnd.m) and
                        (known.oprnd.reg = trial.oprnd.reg) and
                        (known.oprnd.indxr = trial.oprnd.indxr) and
                        (known.oprnd.offset = trial.oprnd.offset) and
                        (known.oprnd.offset1 = trial.oprnd.offset1) and
                        (known.oprnd.scale = trial.oprnd.scale) and
                        (known.oprnd.commonlong_reloc = 
                         trial.oprnd.commonlong_reloc));
    end {addressesmatch} ;


  function operandsmatch(var known, trial: operandentry): boolean;

{ Compares immediate operands for equality in only the portion
  common to both operands.
}

    var
      minlen: integer; {length of shorter operand}
      matched: boolean; {function result}


    begin
      minlen := min(trial.oprndlength, known.oprndlength);
      if minlen = byte then
        matched := ((trial.oprnd.offset and 255) = (known.oprnd.offset and
                   255))
      else
        begin
        matched := (trial.oprnd.offset = known.oprnd.offset);
        if matched and (minlen = long) and
           (trial.oprnd.offset1 <> known.oprnd.offset1) then
          matched := false;
        end;
      operandsmatch := matched;
    end {operandsmatch} ;


  procedure findcandidates;

{ Scan generated code and record popular addresses and operands in the
  address and operand lists.
}

    var
      t: nodeindex; {induction for scanning nodetable}
      p: nodeptr; {pointer to current node}


    procedure addaddress;

{ Add an address to the address list.  If this is a duplicate address,
  merely bump the value.  Otherwise, add a new entry, if the table is
  not full.
}

      var
        i: operandtableindex; {induction for scanning address list}


      begin
        i := lastaddr;
        while not addressesmatch(addresses[i], addresses[0]) do
          i := i - 1;
        if i > 0 then
          with addresses[i] do
	    if oprnd.m = commonlong then
	      if value + longusegain > maxcountkept then value := maxcountkept
	      else value := value + longusegain
	    else
	      if value + wordusegain > maxcountkept then value := maxcountkept
	      else value := value + wordusegain
        else if lastaddr < maxoprndkept then
          begin
          addresses[0].value := - firstusecost;
          lastaddr := lastaddr + 1;
          addresses[lastaddr] := addresses[0];
          end;
      end {addaddress} ;


    procedure addoperand;

{ Add an operand to the operand list.  If this is a duplicate operand,
  merely bump the value.  Otherwise, add a new entry, if the table is
  not full.  If the new operand and the operand found in the list
  are of different lengths but match in the portion common to both
  (the low order part of the longer operand), then the longer of the
  two operands is saved.  This is done to allow a single data register
  to be used for more than one immediate value, where possible.  It
  can be done because instructions using only a portion of a register
  don't see the rest of it.
}

      var
        i: operandtableindex; {induction for scanning operand list}


      begin {addoperand}
        i := lastoprnd;
        while not operandsmatch(operands[i], operands[0]) do
          i := i - 1;
        if i > 0 then
          with operands[i] do
            begin
            if operands[0].oprndlength > oprndlength then
              begin
              oprndlength := operands[0].oprndlength;
              oprnd := operands[0].oprnd;
              end;
	    if operands[0].oprnd.m = commonlong then
	      if value + longusegain > maxcountkept then value := maxcountkept
	      else value := value + longusegain
	    else
	      if value + wordusegain > maxcountkept then value := maxcountkept
	      else value := value + wordusegain
            end
        else if lastoprnd < maxoprndkept then
          begin
          operands[0].value := - firstusecost;
          lastoprnd := lastoprnd + 1;
          operands[lastoprnd] := operands[0];
          end;
      end {addoperand} ;


    begin {findcandidates}
      lastaddr := 0;
      lastoprnd := 0;

      for t := stuffreginst to lastnode do
        begin
        if bigcompilerversion then p := @(bignodetable[t]);

        if p^.kind = instnode then
          begin
          datasize := p^.oprndlength;
          stuffimmediateok := not (p^.inst in [link] + shortinsts);
          end
        else if p^.kind = oprndnode then
          if goodaddress(p) then
            addaddress
          else if goodoperand(p) then
            addoperand;
        end;
    end {findcandidates} ;


  procedure adjustoperands;

{ Adjust 'value' of operands before sorting to reflect the increased
  benefit when the register can be loaded by a MOVEQ instruction.
}

    var
      i: operandtableindex; {induction for operand table}


    begin
      for i := 1 to lastoprnd do
        with operands[i], oprnd do
          if (offset >= - 128) and (offset <= 127) and
             ((m <> immediatelong) or (offset < 0) and (offset1 = - 1) or
             (offset >= 0) and (offset1 = 0)) then
	    if value < maxcountkept then value := value + 1;
    end; {adjustoperands}


  procedure sorttable(var table: operandtable;
                      tabletop: operandtableindex);

{ Sort found addresses or operands descending on the 'value' of the
  address or operand.  A simple bubble sort is used -- if 'maxaddrkept'
  is made large, this could be replaced by a more efficient sort.
}

    var
      sorted: boolean; {true if list is sorted}
      temp: operandentry; {for swapping list elements}
      i, top: operandtableindex; {for sweeping list}


    begin
      if tabletop > 1 then
	begin
        top := tabletop - 1;

        repeat
          sorted := true;
          for i := 1 to top do
            if table[i].value < table[i + 1].value then
              begin
              temp := table[i];
              table[i] := table[i + 1];
              table[i + 1] := temp;
              sorted := false;
              top := i - 1;
              end;
        until sorted;
        end;
    end {sorttable} ;


  procedure assignaddressregisters;

{ Assign the free address registers to the best addresses.  Since the
  address list has been sorted by value, the entries 1..lastaddr
  contain the most-frequently recorded addresses.
}

    var
      i: operandtableindex; {induction for scanning address list}
      r: regindex; {register to assign}


    begin
      lastaddr := min(lastaddr, freeaddrregs);
      addresses[0].value := maxcountkept;
      while addresses[lastaddr].value <= 0 do
        lastaddr := lastaddr - 1;
      r := 0;
      for i := 1 to lastaddr do
        begin
        while aregused[r] do
          r := r + 1;
        if {((r = debuglink) and dbgused) or}
           ((r = pic_own_base) and pic_own_used) then
          r := r + 1;
        if (r = sl) and slused then
          begin
          write('stuffregisters screwup');
          compilerabort(inconsistent);
          end;
        addresses[i].newreg := r;
        aregused[r] := true;
        end;
    end {assignaddressregisters} ;


  procedure assigndataregisters;

{ Assign the free data registers to the best operands.  Since the
  operand list has been sorted by value, the entries 1..lastoprnd
  contain the most-frequently recorded operands.
}

    var
      i: operandtableindex; {induction for scanning operand list}
      r: regindex; {register to assign}


    begin
      lastoprnd := min(lastoprnd, freedataregs);
      operands[0].value := maxcountkept;
      while operands[lastoprnd].value <= 0 do
        lastoprnd := lastoprnd - 1;
      r := 0;
      for i := 1 to lastoprnd do
        begin
        while dregused[r] do
          r := r + 1;
        operands[i].newreg := r;
        dregused[r] := true;
        end;
    end {assigndataregisters} ;


  procedure assignfpregisters;

{ Assign the free floating-point registers to the best operands.  Since the
  operand list has been sorted by value, the entries 1..lastoprnd
  contain the most-frequently recorded operands.
}

    var
      i: operandtableindex; {induction for scanning operand list}
      r: regindex; {register to assign}


    begin
      if mc68881 then
        begin
        lastoprnd := min(lastoprnd, freefpregs);
        operands[0].value := maxcountkept;
        while operands[lastoprnd].value <= 0 do
          lastoprnd := lastoprnd - 1;
        r := 0;
        for i := 1 to lastoprnd do
          begin
          while fpregused[r] do
            r := r + 1;
          operands[i].newreg := r;
          fpregused[r] := true;
          end;
        end;
    end {assignfpregisters} ;


  procedure replacereferences;

{ Replace favored addresses and operands in generated code with the
  shorter register equivalent.
}

    var
      newreg: regindex; {register to replace}
      t: nodeindex; {for scanning node table}
      ti: nodeindex; {for accessing instruction node}
      p: nodeptr; {points to current operand node}
      pi: nodeptr; {points to current instruction node}


    function replacingaddress: boolean;

{ Returns true and sets newreg if the current address should be
  replaced.
}

      var
        i: operandtableindex; {for scanning addresses}


      begin
        i := lastaddr;
        while not addressesmatch(addresses[i], addresses[0]) do
          i := i - 1;
        if i > 0 then
          begin
          replacingaddress := true;
          newreg := addresses[i].newreg;
          end
        else
          replacingaddress := false;
      end {replacingaddress} ;


    function replacingoperand: boolean;

{ Returns true and sets newreg if the current operand should be
  replaced.
}

      var
        i: operandtableindex; {for scanning operands}


      begin {replacingoperand}
        i := lastoprnd;
        while not operandsmatch(operands[i], operands[0]) do
          i := i - 1;
        if i > 0 then
          begin
          replacingoperand := true;
          newreg := operands[i].newreg;
          end
        else
          replacingoperand := false;
      end {replacingoperand} ;


    begin {replacereferences}
      for t := stuffreginst to lastnode do
        begin
        if bigcompilerversion then p := @(bignodetable[t]);

        if p^.kind = instnode then
          begin
          ti := t;
          datasize := p^.oprndlength;
          stuffimmediateok := not (p^.inst in [link] + shortinsts);
          suffixIinstruction := p^.inst in
                                ([addi, andi, cmpi, eori, ori, subi]);
          leainstruction := (p^.inst = lea);
          end
        else if p^.kind = oprndnode then
          begin
          if goodaddress(p) then
            begin
            if replacingaddress then
              begin
              with p^.oprnd do
                begin
                m := indr;
                reg := newreg;
                indxr := 0;
                offset := 0;
                offset1 := 0;
                blocksin[1].written := true;
                end;
              if leainstruction then
                begin {LEA (An),Am is silly looking}
                p^.oprnd.m := areg;

                if bigcompilerversion then pi := @(bignodetable[ti]);

                pi^.inst := movea;
                pi^.oprndlength := long;
                end;
              end;
            end
          else if goodoperand(p) then
            if replacingoperand then
              begin
              with p^.oprnd do
                begin
                m := dreg;
                reg := newreg;
                indxr := 0;
                offset := 0;
                offset1 := 0;
                blocksin[1].written := true;
                end;
              if suffixIinstruction then
                begin

                if bigcompilerversion then pi := @(bignodetable[ti]);

                pi^.inst := pred(pi^.inst);
                end;
              end;
          end;
        end;
    end {replacereferences} ;


  procedure loadregisters;

{ Load the free registers with addresses and operands from the
  address and operand lists.
}

    var
      i: operandtableindex; {induction for scanning address list}
      regoprnd: operand; {operand describing register to load}


    begin
      insert(stuffreginst, (lastaddr + lastoprnd) * 3);
      with regoprnd do
        begin
        indxr := 0;
        offset := 0;
        offset1 := 0;
        end;

      regoprnd.m := areg;
      for i := 1 to lastaddr do
        with addresses[i] do
          begin
          geninst(lea, 2, long);
          genoprnd(oprnd);
          regoprnd.reg := newreg;
          genoprnd(regoprnd);
          end;

      regoprnd.m := dreg;
      for i := 1 to lastoprnd do
        with operands[i], oprnd do
          begin
          if (offset >= - 128) and (offset <= 127) and
             ((m <> immediatelong) or (offset < 0) and (offset1 = - 1) or
             (offset >= 0) and (offset1 = 0)) then
            begin
            geninst(moveq, 2, byte);
            m := immediate;
            end
          else
            geninst(move, 2, oprndlength);
          genoprnd(oprnd);
          regoprnd.reg := newreg;
          genoprnd(regoprnd);
          end;

      lastnode := lastsaved;
    end {loadregisters} ;


  begin {stuffregisters}
    countfreeregisters;
    if freeaddrregs + freedataregs > 0 then
      begin
      findcandidates;
      if lastaddr + lastoprnd > 0 then
        begin
        adjustoperands;
        sorttable(addresses, lastaddr);
        sorttable(operands, lastoprnd);
        assignaddressregisters;
        assigndataregisters;
        replacereferences;
        loadregisters;
        end;
      end;
  end {stuffregisters} ;



procedure fixstack;

{ Scan the list of generated instructions and check for branches
  whose stack depth is not the same as the stack depth at their
  destination.  Such branches must have stack adjust instructions
  added before the branch to equalize stack levels.

  If the branch is unconditional, the stack adjust is simply inserted
  before the branch.  For a conditional branch, the code is changed
  from:
        bcon    label

  to:
        bncon   lab1
        adjust stack
        br      label
  lab1:
}

  var
    t: nodeindex; {used to scan instruction list}
    n: nodeptr; {used to access instruction nodes}
    addednodes: 1..5; {number of nodes added}
    tempinst: insttype; {holds inverted branch}
    stackadj: integer; {difference in stack between branch and label}


  begin {fixstack}
    t := 0;
    repeat
      tempkey := loopcount - 1;
      repeat
        t := t + 1;
        if bigcompilerversion then n := @(bignodetable[t]);
      until (t = lastnode) or (n^.kind = labelnode);

      if n^.kind = labelnode then
        begin
        stackadj := n^.stackdepth -
                    labeltable[findlabel(n^.labelno)].stackdepth;
        if findlabel(n^.labelno) > 0 then
          if stackadj <> 0 then
            begin
            t := t - 1;

            if bigcompilerversion then n := @(bignodetable[t]);

            addednodes := 3; {we will generate an addi/subi #dif,sp}
            labelnextnode := n^.labelled;
            n^.labelled := false;

            if n^.inst <> bra then
              begin
              tempinst := invert[n^.inst];
              n^.inst := bra;
              addednodes := addednodes + 2; {we will add an extra branch}
              end;
            insert(t - 1, addednodes);
            if addednodes > 3 then
              genrelbr(tempinst, 2);
            settempimmediate(word, abs(stackadj));
            settempareg(sp);
            if stackadj < 0 then
              gen2(sub, word, tempkey + 1, tempkey)
            else
              gen2(add, word, tempkey + 1, tempkey);
            lastnode := lastsaved;
            t := t + addednodes + 1;
            end;
        end;
    until t >= lastnode;
  end {fixstack} ;



procedure assignlabeladdresses;

{ After all changes to the code have been made, but before branch/jump
  resolution, this goes through the label table and the code and assigns
  instruction counter values to each label.
}

  var
    t: nodeindex; {induction var for scanning code list}
    next: nodeindex; {computed as next instruction following t}
    n: nodeptr; {used to access each code node to get it's length}
    l: labelindex; {induction var for scanning labels}
    currentpc: addressrange; {used to keep track of the pc value}


  begin
    currentpc := highcode;
    l := 1;
    t := 1;
    while l <= nextlabel do
      begin {find addresses for each label}
      while t <> labeltable[l].nodelink do
        begin {scan to the next node, acccumulating code size}
        if bigcompilerversion then n := @(bignodetable[t]);

        if n^.kind = instnode then
          next := t + n^.oprndcount + 1
        else
          next := t + 1;
        currentpc := currentpc + instlength(t);
        t := next;
        end;
      labeltable[l].address := currentpc;
      l := l + 1;
      end;
    labeltable[0].address := undefinedaddr;
  end {assignlabeladdresses} ;


procedure fixaddressing;

{ Determine the final form for branches and pcrelative (that is, psect-
  relative) addresses.  The M68000 has two forms of branches.  Short
  (one-word) branches can reach instructions within 127 bytes of the
  PC (of course, instructions must start on even bytes).  Long branches
  (two-word) can reach any instruction within 32767 bytes, i.e. any
  destination within a given Pascal block.  Short branches are indicated
  by a "labelnode" with "labelcost" zero, long branches have "labelcost"
  set to the length of a word, two bytes, which is the cost (in space)
  of a long vs. short branch.  Forward branches may have to reach beyond
  32K, so they are changed to jumps, with "labelcost" set to four for an
  absolute address. 

  "pcrelative" really means psect-relative, and if the code is long
  enough, constant references near the end of the program may not be
  able to reach with pcrelative mode.  An operandcost of four (long)
  is used to indicate that a relocatable address should be generated
  instead of an offset from the pc.

  The approach is to assume initially that short addresses can be used.
  The instruction list is scanned, and any addresses which cannot reach
  their targets are converted to the appropriate longer form, and label
  addresses are adjusted appropriately.  This, of course, may cause
  other branches or pcrelative addresses to be unable to reach their
  targets, so the process is repeated until there is no change on a
  complete scan of the code list.

  Additionally, transfers to the next instruction are deleted, as a branch
  to the next instruction (offset = 0) indicates the long form, and would
  be silly anyway.

  NOTE:  Datanodes are ignored because they are part of the highcode
  (ie constants) count and are not considered instructions.
}

  var
    t: nodeindex; {induction var for scanning code list}
    ti: nodeindex; {induction var for finding instruction node}
    n: nodeptr; {used to access nodes as they are scanned}
    ni: nodeptr; {used to access instruction node to change bra to jmp}
    l: labelindex; {used for scanning the label table}
    l1: labelindex; {scans following labels to increment addresses}
    currentpc: addressrange; {current pc of this node}
    backward: boolean; {branch is to lower address}
    brlen: addressrange; {distance the branch goes}
    bias: integer; {change due to this branch change}
    register: integer; {Temporary for 68000 pic}


  begin {fixaddressing}
    repeat
      l := 1;
      currentpc := highcode;
      bias := 0;
      t := 1;

      {Make one scan through the instruction list}

      while t <= lastnode do
        begin
        while (labeltable[l].nodelink <= t) and (l <= nextlabel) do
          l := l + 1; {skip labels before this instruction}
        if bigcompilerversion then n := @(bignodetable[t]);
        with n^ do
          begin
          if kind in [instnode, labeldeltanode] then
            currentpc := currentpc + instlength(t)
          else if (kind = labelnode) and (labelcost < long) then
            begin {candidate label reference}
            l1 := findlabel(labelno);
            backward := (labeltable[l1].address < (currentpc - labelcost));
            brlen := labeltable[l1].address - (currentpc - labelcost);

            if (l1 = 0) {forward reference to some other block}
               or (labelcost = 0) and (not backward and (brlen > 127) or
               backward and ((hostintsize > word) and (brlen < - 128) or
               (hostintsize <= word) and (brlen < maxaddr - 127))) or
               not backward and (brlen > 32767) or backward and
               (brlen < - 32767 - 1) or (brlen = 0) then
              begin {some kind of a change}
              if bigcompilerversion then n := @(bignodetable[t]);

              if (l1 <> 0) and (brlen = 0) then
                begin
                bias := - word;
                delete(t - 1, 1);
                end
              else
                begin {change to longer form}
                if (l1 = 0) {forward, out of block} or not backward and
                   (brlen > 32767) or backward and ((hostintsize <= word) and
                   (brlen <= 32767) or (hostintsize > word) and
                   (brlen < - 32767 - 1)) then
                  begin {change to long}
                  labelcost := long;

                  if not (mc68020 and pic_enabled) then { 68020 has long branches}
                    begin
                    ti := t;

                    repeat
                      ti := ti - 1;
                      if bigcompilerversion then ni := @(bignodetable[ti]);
                    until ni^.kind = instnode;

                    if bigcompilerversion then ni := @(bignodetable[ti]);

                    if pic_enabled then
                      begin  { MC68000 24-bit pic does not allow branches in
                               procedure longer than 32k. }
                      write('Procedure is too large for PIC');
                      compilerabort(inconsistent);
                      end;
                    if ni^.inst = bra then
                      begin
                      ni^.computed_length := 6;
                      ni^.inst := jmp;
                      end
                    else if ni^.inst in branches then
                      begin
                      write('Fixaddressing: long conditional branch in node ',
                            ti: 1);
                      compilerabort(inconsistent);
                      end;
                    end; {not (mc68020 and pic_enabled)}
                  end {change to long}
                else
                  labelcost := word;
                bias := labelcost;
                end {change to longer form} ;
              {adjust addresses of following labels}
              currentpc := currentpc + bias;
              for l1 := l to nextlabel do
                labeltable[l1].address := labeltable[l1].address + bias;
              end; {some kind of a change}
            end {candidate label reference}
          else if (kind = oprndnode) and (operandcost < long) and
                  ((oprnd.m = pcrelative) or (oprnd.m = usercall)) then
            begin {candidate operand node}
            if (oprnd.m = pcrelative) and
               (oprnd.offset - (currentpc - word) < - 32768) or
               (oprnd.m = usercall) and not (not mc68020 and pic_enabled) and
               (proctable[oprnd.offset].externallinkage and
               not proctable[oprnd.offset].bodydefined or
               (procmap[oprnd.offset].addr = undefinedaddr) or
               (procmap[oprnd.offset].addr + oprnd.offset1 - (currentpc -
               word) < - 32768)) then
              begin {change to long form}
              if bigcompilerversion then n := @(bignodetable[t]);

              if not mc68020 and pic_enabled then
                begin
                { Convert the LEA that was put out in dostructx to a two
                  instruction sequence to compute the constant's address.
                }
                n^.oprnd.m := pic_pcrelative; { change mode to pic_pcrelative }
                n^.oprnd.offset1 := 6;
                if bigcompilerversion then n := @(bignodetable[t - 1]);
                n^.inst := adda;
                if bigcompilerversion then n := @(bignodetable[t + 1]);
                register := n^.oprnd.reg;
                currentpc := currentpc - long;
                insert(t - 2, 3);
                settempareg(register);
                settemp(long, pic_splat_pcrel, 0, 0, false, 10, 0, 1, unknown);
                gen2(lea, long, tempkey, tempkey + 1);
                tempkey := tempkey + 2;
                if bigcompilerversion then n := @(bignodetable[t - 1]);
                currentpc := currentpc + instlength(t - 1);
                bias := 6; { ADDA is 6 bytes long; new LEA same length as old }
                lastnode := lastsaved;
                t := t + 3; { Advance past new instruction }
                end
              else if mc68020 and pic_enabled then
                begin
                n^.operandcost := long + word; { 8 bytes total }
                bias := long;
                end
              else
                begin
                n^.operandcost := long; { 6 bytes total }
                bias := word;
                end;

              { Adjust addresses of following labels.
              }
              currentpc := currentpc + bias;
              for l1 := l to nextlabel do
                labeltable[l1].address := labeltable[l1].address + bias;
              end {change to long form} ;
            end {candidate operand node} ;
          end {with n^} ;

          t := t + 1;
        end {while t <= lastnode} ;
    until bias = 0;

  { This is saved for a consistency check later.
  }
  last_pc := currentpc;
  end {fixaddressing} ;



procedure mergebranchtails;

{ This routine performs a valuable optimization for reducing code
  size.  Consider the case of a conditional statement, where
  we have at least two streams of instructions which transfer to
  the same label.  One of the streams ends in an unconditional branch
  to a label, while the other directly preceeds the label.  In the
  case of a multiway conditional, such as a case statement,  there
  may be many such instruction streams converging on a single node.

  Such strings are called "branch tails" (for historical reasons).

  If the final instructions in two branch tails are identical,  one
  of the sequences of identical instructions can be deleted, and
  the branch changed to point to the start of the identical instructions
  in the other stream.  This is called "merging branch tails", and can
  save much space in many practical situations.

  The algorithm used takes one stream as a reference, and compares
  it with each other stream leading into the same node.  It takes
  the stream with the longest match and changes the reference stream
  to jump to it, then deletes the tail of the reference string.  This
  is repeated for each stream leading into the node.

  The algorithm uses an auxiliary data structure built by the code
  generator in the routine "jumpx" as branches are generated.
  Each label has an entry in a linked list with a pointer to a list
  of branches to that label.  The branches are chained in the
  "brnodelink" field of the label node.  Note that it is possible to
  have more than one label on a node.
}

  var
    longestmatch: nodeindex; {beginning of the longest matching tail}
    longestkill: nodeindex; {beginning of the longest tail in ref stream}
    t: nodeindex; {temp node index with several uses}
    p, p1: nodeptr; {used to access nodes for various reasons}
    l: labelindex; {label table entry for label being used}
    longestcount: integer; {max number of instructions that matched}
    longestkillcount: integer; {max number of instructions to be deleted}
    cp: brlinkptr; {used to chain down aux branch list}


  procedure matchtails(t: nodeindex; {branch tail to check for match}
                       t1: nodeindex {tail to be shortened} );

{ Scan backwards from node t1, comparing it to the nodes prior to t,
  until a label is found on the tail from t1 or the tails don't match.

  If the length of matching tails is longer than the prior longest match,
  the data is saved for later use in linking tails.
}

    var
      p, p1: nodeptr; {used to access nodes}
      labelpassed: boolean; {true if a label has been passed by t1}
      lastpassed: boolean; {used to provide delay in setting labelpassed}
      count: integer; {number of instructions that matched}
      killcount: integer; {number of instructions to be deleted}
      t0: nodeindex; {start of the first branch tail}


    begin {matchtails}
      count := - 1;
      killcount := - 1;
      lastpassed := false;
      t0 := min(t, t1);
      repeat
        labelpassed := lastpassed;
        count := count + 1;
        killcount := killcount + 1;
        repeat
          {skip nop's to the prior instruction on t1 tail}
          t1 := t1 - 1;
          if bigcompilerversion then
            p1 := @(bignodetable[t1]);
          if (p1^.kind = instnode) and (p1^.inst = nop) and
             not p1^.labelled or (p1^.kind = stmtref) then
            killcount := killcount + 1;
        until ((p1^.kind = instnode) and ((p1^.inst <> nop) or
              p1^.labelled)) or (p1^.kind = errornode);
        lastpassed := p1^.labelled;
        repeat
          {go to prior instruction on the t tail}
          t := t - 1;
          if bigcompilerversion then
            p := @(bignodetable[t]);
        until ((p^.kind = instnode) and (p^.inst <> nop)) or
              (p^.kind = errornode);
      until labelpassed or (t = t1) or not eqinst(t, t1);
      if (count >= longestcount) and (t0 <= max(t, t1)) then
        begin {we have the longest chain so far}
        longestcount := count;
        longestkillcount := killcount;
        longestmatch := t;
        longestkill := t1;
        end;
    end {matchtails} ;


  begin {mergebranchtails}
    while (firstbr <> nil) and (nextlabel < labeltablesize - 1) do
      begin {repeat for each label}
      while (firstbr^.n <> 0) and (nextlabel < labeltablesize - 1) do
        begin {repeat for each branch to this label}
        longestcount := 0;
        longestkillcount := 0;
        cp := firstbr;
        while cp <> nil do
          begin {check for branches leading to this node}
          if (findlabel(cp^.l) <> 0) and
             (getlabelnode(cp^.l) = getlabelnode(firstbr^.l)) then
            begin
            t := cp^.n;
            while t <> 0 do
              begin
              matchtails(t + 1, firstbr^.n + 1);
              if bigcompilerversion then
                p := @(bignodetable[t + 1]);
              t := p^.brnodelink;
              end;
            end;
          cp := cp^.nextbr;
          end;

        { now check the inline stream up to this node }
        longestcount := longestcount - 1;
        longestkillcount := longestkillcount - 1;
        l := findlabel(firstbr^.l);

        if bigcompilerversion then
          begin
          p := @(bignodetable[firstbr^.n]);
          p1 := @(bignodetable[firstbr^.n + 1]);
          end;

        if (l <> 0) and (labeltable[l].stackdepth = p1^.stackdepth) and
           not p^.labelled then
          matchtails(labeltable[l].nodelink, firstbr^.n);

        if longestcount > 0 then
          begin {adjust the branch and delete extra instructions}
          t := lastnode;
          lastnode := longestmatch;
          repeat
            lastnode := lastnode + 1;
            if bigcompilerversion then
              p := @(bignodetable[lastnode]);
          until ((p^.kind = instnode) and (p^.inst <> nop)) or
                (p^.kind = errornode);
          l := nextlabel;
          labeltable[0].nodelink := lastnode;
          labeltable[0].labno := lastlabel;
          labeltable[0].stackdepth := stackoffset;
          while labeltable[l].nodelink <> lastnode do
            l := l - 1;
          if l = 0 then
            begin
            lastnode := lastnode - 1;
            blocksin[1].written := true;
            p^.labelled := true;
            definelastlabel;
            end;
          if bigcompilerversion then
            p := @(bignodetable[firstbr^.n + 1]);

          p^.labelno := labeltable[l].labno;
          p^.stackdepth := labeltable[l].stackdepth;
          lastnode := t;
          t := longestkill;
          repeat
            t := t + 1;
            if bigcompilerversion then
              p := @(bignodetable[t]);
          until (p^.kind = instnode) or (p^.kind = stmtref) or
                (p^.kind = errornode);
          delete(t, longestkillcount);
          end;
        if bigcompilerversion then
          p := @(bignodetable[firstbr^.n + 1]);
        firstbr^.n := p^.brnodelink
        end;
      cp := firstbr^.nextbr;
      dispose(firstbr);
      firstbr := cp;
      end;
  end {mergebranchtails} ;


procedure proctrailer;

{ Generate code for a block exit.

  At this point we know what registers we used, so we can set up the
  register save and restore.
}

  var
    i: regindex; {induction var for scanning registers}
    savemask, restoremask: unsigned; {16 bit masks for movem instructions}
    fpsavemask, fprestoremask: unsigned; {16 bit masks for fmovem instructions}
    savebit, restorebit: unsigned; {current register bit}
    fpsavebit, fprestorebit: unsigned; {current register bit for fpregs}
    tempptr, saveregptr: nodeptr; {points to save register mask}
    blockcost: integer; {max bytes allocated on the stack}
    fptemp, sptemp: keyindex;
    regcost, fpregcost: integer; {bytes allocated to save registers on stack}
    noframe_instr: insttype; {instruction to use if noframe is used}
    noframe_size: datarange; {size of frame if noframe is used}
    total_frame_size: integer; { blksize for unix; blockcost + blksize for
                                 Vdos.  Makes code more readable. }
    reg: integer; { temp for dummy register }
    

  begin {proctrailer}

    { If we need an a-register later for PIC procedure calls, it must be
      reserved now so it will get in the save/restore mask.
    }
    if not mc68020 and pic_enabled and (proctable[blockref].opensfile or
      switcheverplus[debugging] or switcheverplus[profiling])
    then reg := getareg;

    {do the register save and restore}

    savemask := 0;
    restoremask := 0;
    savebit := &100000;
    restorebit := 1;
    regcost := 0;
    fpsavemask := 0;
    fprestoremask := 0;
    fpsavebit := 1;  { fmovem mask is opposite of movem!!! }
    fprestorebit := &200;
    fpregcost := 0;
    if pic_enabled and (ownsize > 0) and proctable[blockref].ownused then
      aregused[pic_own_base] := true;
    with keytable[stackcounter + 1].oprnd do
      begin
      offset := keytable[stackcounter].oprnd.offset;
      for i := 0 to 7 do
        begin
        if dregused[i] then
          begin
          savemask := savemask + savebit;
          restoremask := restoremask + restorebit;
          regcost := regcost + long;
          end
        else
          offset := offset + long;
        savebit := savebit div 2;
        restorebit := restorebit * 2;
        end;

      for i := 0 to sl do
        begin
        if aregused[i] then
          begin
          savemask := savemask + savebit;
          restoremask := restoremask + restorebit;
          regcost := regcost + long;
          end
        else
          offset := offset + long;
        savebit := savebit div 2;
        restorebit := restorebit * 2;
        end;

      if mc68881 then
        for i := 0 to 7 do
          begin
          if fpregused[i] then
            begin
            fpsavemask := fpsavemask + fpsavebit;
            fprestoremask := fprestoremask + fprestorebit;
            fpregcost := fpregcost + 12;
            end
          else
            offset := offset + 12;
          fpsavebit := fpsavebit * 2;  { opposite of movem!! }
          fprestorebit := fprestorebit div  2;
          end;

      if switchcounters[stackcheck] > 0 then
        blockcost := maxstackdepth + long - offset +
                     keytable[stackcounter].oprnd.offset
      else
        blockcost := 0;
      end;

    { See if there is enough space on the stack for the frame linkage,
      all the registers to be saved and (if stackcheck is true) everything
      else that will be pushed on the stack.
    }
    insert(linkentryinst, prologuelength);
    settempareg(fp);
    fptemp := tempkey;

    total_frame_size := blockcost + blksize;
    if not blockusesframe then
      begin
      if total_frame_size > 0 then
        begin
	noframe_instr := subq;
	if total_frame_size > 8 then
	  noframe_instr := suba;
	noframe_size := word;
	if total_frame_size > 32767 then
	  noframe_size := long;
        settempareg(sp);
	settempimmediate(noframe_size, total_frame_size);
	gen2(noframe_instr, noframe_size, tempkey, tempkey + 1);
        tempkey := tempkey + 2;
        end
      end
    else
      if (total_frame_size > 32767) and not mc68020 then
        {big big stack frame!}
        begin {must simulate link instruction with 32 bit constant offset}
        settempreg(long, autod, sp);
        gen2(move, long, fptemp, tempkey);
        settempareg(sp);
        gen2(move, long, tempkey, fptemp);
        settempimmediate(long, total_frame_size);
        gen2(suba, long, tempkey, tempkey + 1);
        linkentryinst := lastnode - 1;
        tempkey := tempkey + 4;
        end
      else
        begin
        if (total_frame_size > 32767) and mc68020 then
          begin
          settempimmediate(long, - total_frame_size);
          gen2(link, long, fptemp, tempkey);
          end
        else
          begin
          settempimmediate(word, - total_frame_size);
          gen2(link, word, fptemp, tempkey);
          end;
        linkentryinst := lastnode;
        tempkey := tempkey + 2;
        end;

    if switchcounters[stackcheck] > 0 then
      begin
      { CMPA will not generate an overflow with 32 bits so we must set the
        overflow flag.
      }
      settempreg(long, indr, gp);
      settempareg(sp);
      sptemp := tempkey;
      gen2(cmpa, long, tempkey + 1, sptemp);

      { On MC68000 there is no other way to cause an overflow trap than to
        load the CCR and do a TRAPV.  On the MC68020 we can do a TRAPcc to
        cause a trap if cc is low or same.  TRAPV and TRAPcc use the same
        vector (7).
      }
      if false {mc68020} then { Can't do this until the library is changed }
        geninst(trapcc, 0, 0)
      else
        begin
        genrelbr(bhi, 2);
        settempimmediate(word, 2); { overflow }
        gen1(move_to_ccr, word, tempkey);
        tempkey := tempkey + 1;
        geninst(trapv, 0, 0);
        end;

      { Now remove what we will push on later.
      }
      settempimmediate(long, blockcost); {kludge value, prevent inst deletion}
      if blockcost > 32767 then
        gen2(adda, long, tempkey, sptemp)
      else
        gen2(adda, word, tempkey, sptemp);
      tempkey := tempkey + 3;
      end;
    lastnode := lastsaved;

    { Adjust stack offsets to reflect actual space required by saved registers.
    }

    adjustoffsets(savereginst, true);

    { Update save mask and prepare to generate restore instruction.
    }

    if restoremask = 0 then
      delete(savereginst, 1)
    else
      begin
      if bigcompilerversion then
        saveregptr := @(bignodetable[savereginst + 1]);

      saveregptr^.oprnd.offset := savemask;
      end;

    if mc68881 then
      if fprestoremask = 0 then
        delete(fpsavereginst, 1)
      else
        begin
        if bigcompilerversion then
          saveregptr := @(bignodetable[fpsavereginst + 1]);

        saveregptr^.oprnd.offset := fpsavemask;
        end;

    if proctable[blockref].opensfile then
      begin
      settemp(long, relative, sp, 0, false, (fpregcost - 96) * ord(mc68881) +
              regcost - 13 * long, 0, 1, unknown);
      gen1(pea, long, tempkey);
      tempkey := tempkey + 1;
      settempimmediate(long, blksize);
      settempreg(long, autod, sp);
      gen2(move, long, tempkey + 1, tempkey);
      tempkey := tempkey + 2;
      callsupport(libcloseinrange);
      settempimmediate(word, 8);  { Clean up stack }
      settempareg(sp);
      gen2(adda, word, tempkey + 1, tempkey);
      end;
    if switcheverplus[debugging] or switcheverplus[profiling] then
      begin
      if bigcompilerversion then
        saveregptr := @(bignodetable[savedebinst + 1]);

      saveregptr^.oprnd.offset := left;
      callsupport(libdebugger_exit) {debugger procedure exit}
      end;

    { If there were open arrays in this procedure, restore the stack pointer
      that was saved on the stack after all the open arrays, then restore
      the registers.  The saved stack pointer is required because the
      length of the open arrays on the stack is not known at compile-time.
    }
    if language = modula2 then
      if openarray_base <> nil then
        begin
        settempareg(sp);
        settemp(long, indr, sp, 0, false, 0, 0, 1, unknown);
        gendouble(move, tempkey, tempkey + 1);
        tempkey := tempkey + 2;
        end;

    { The register restore has been moved here because of PIC.
    }
    if mc68881 and (fprestoremask <> 0) then
      begin
      settempimmediate(word, fprestoremask);

{ This won't work until buildmovem is altered to allow relative movems.

      if (language = modula2) and proctable[blockref].needsframeptr then
        begin
        settemp(long, relative, fp, 0, false, - blksize - regcost, 0, 1,
                unknown);
        end
      else} settempreg(long, autoi, sp);

      fpgendouble(fmovem, tempkey, tempkey + 1);
      tempkey := tempkey + 2;
      end;

    if restoremask <> 0 then
      begin
      settempimmediate(word, restoremask);

{ This won't work until buildmovem is altered to allow relative movems.

      if (language = modula2) and proctable[blockref].needsframeptr then
        begin
        settemp(long, relative, fp, 0, false, - blksize, 0, 1,
                unknown);
        end
      else} settempreg(long, autoi, sp);

      gen2(movem, long, tempkey, tempkey + 1);
      tempkey := tempkey + 2;
      end;

    if not blockusesframe then
      begin
      if total_frame_size > 0 then
        begin
	noframe_instr := addq;
	if total_frame_size > 8 then
	  noframe_instr := adda;
	noframe_size := word;
	if total_frame_size > 32767 then
	  noframe_size := long;
        settempareg(sp);
	settempimmediate(noframe_size, total_frame_size);
	gen2(noframe_instr, noframe_size, tempkey, tempkey + 1);
        tempkey := tempkey + 2;
        end
      end
    else
      begin {generate exit code}
      settempareg(fp);
      gen1(unlk, long, tempkey);
      tempkey := tempkey + 1;
      end;

    { Put out an RTE instead of an RTS if this is an interrupt procedure.
    }
    if proctable[blockref].calllinkage = interruptcall then geninst(rte, 0, 0)
    else geninst(rts, 0, 0);

  end {proctrailer} ;


procedure maintrailer;

{ Generate code for a main program trailer.  This generates debugging 
  initialization code if needed.
}


  begin
    mainsymbolindex := left;
    case language of
      pascal: callsupport(libexit);
      modula2: callsupport(libmexit);
      end;
  end; {maintrailer}


procedure cmaintrailer;

{ Generate trailer code for the main program.
}
  begin
    callsupport(libcexit);
    proctrailer;
  end; {cmaintrailer}


procedure putblock;

{ After all code has been generated for a block, this procedure generates
  cleanup code (with "proctrailer"), calls the various peep-hole optimization
  routines, and finally calls the appropriate routines to output the code.
  The order in which these routines are called is not random, and is designed
  to produce the greatest possible effect.  The other alternative is to call
  each routine multiple times until no change is noted, an expensive approach.
}

  var
    r: regindex;
    regssaved : array [0..23] of boolean;
    pc_addr   : integer;

  begin {PutBlock}
    { save procedure symbol table index }
    procsym := pseudoinst.oprnds[1];

    { Clean up stack, and make sure we did so
    }

    if mc68881 then context[contextsp].lastbranch := fpsavereginst + 3
    else context[contextsp].lastbranch := savereginst + 3;

    adjusttemps;

    { One more temp is needed for 68881 code.  Proctrailer kills it.
      Another temp is used by Modula2 for openarrays.  Proctrailer kills
      it too.
    }
    if stackcounter < keysize - (2 + ord(mc68881) +
       ord(openarray_base <> nil)) then
      begin
      compilerabort(undeltemps);
      writeln('Excess temps: ', keysize - 2 - stackcounter: 1, ', Bytes left: ',
             stackoffset + keytable[stackcounter].oprnd.offset: 1);
      end;

    { peephole optimizations }
    if not (switcheverplus[debugging] or switcheverplus[profiling]) then
      mergebranchtails; {note --- mergebranchtails must be FIRST}
    fixstack; { line up stack offsets of branches and labels }
    fixsimplestuff; { one-instruction peephole optimization }
    if oktostuff then stuffregisters; {take advantage of unused registers}

    if level = 1 then maintrailer
    else if (language = c) and (blockref = 0) then cmaintrailer
    else proctrailer;

    assignlabeladdresses;
    definelabel(maxint);
    fixaddressing; { branch/jump and pcrelative address resolution }

    { fill in debugger symbol table entry fields }
    if switcheverplus[symboltable] then
      begin
      end;

    { write output code }
    if switcheverplus[outputmacro] or
       switcheverplus[outputobj] then putcode.putcode;

       {DRB
    if currentpc <> last_pc then
      begin
      writeln('Phase error, pass 1 = ', last_pc:-4, ', pass 2 = ',
              currentpc:-4);
      compilerabort(inconsistent);
      end;
      }
  end { PutBlock } ;



{ The following 4 procedures handle the pseudo operators doint, doptr,
  doreal, and dolevel, where the level is local or global.  Placed here
  to shorten genblk a bit.
}


procedure dointx;

{ Access a constant integer operand.  The value is in oprnds[1].
  This simply sets up the key for the value, with literal mode.
}


  begin {dointx}
    keytable[key].access := valueaccess;
    keytable[key].len := pseudoinst.len;
    keytable[key].signed := true;
    keytable[key].oprnd.m := immediate;
    keytable[key].oprnd.offset := pseudoinst.oprnds[1];
    keytable[key].knowneven := mc68020 or not odd(keytable[key].oprnd.offset);
  end {dointx} ;


procedure dofptrx;

{ Access a constant function pointer.  The procref is in oprnds[1].
}


  begin {dofptrx}
    keytable[key].access := valueaccess;
    keytable[key].len := pseudoinst.len;
    keytable[key].signed := false;
    keytable[key].oprnd.m := usercall;
    keytable[key].oprnd.offset := pseudoinst.oprnds[1];
    keytable[key].knowneven := true;
  end {dofptrx} ;


procedure dorealx;

{ Access a constant real operand.  The value is given in oprnds[1]
  and oprnds[2].  Travrs scans and generates all constants prior to
  generating code, so these may be inserted in the code stream directly
  and an absolute reference to them inserted in the key.
}

  const
    word_zero = false;
    word_one = true;

  var
    kluge: record
      case boolean of
        false: (i: integer);
        true: (damn: packed array [boolean] of - 32767..32767);
      end;


  begin {dorealx}
    with pseudoinst do
      if oprnds[3] <> 1 then
        begin
        write('Invalid real number intermediate code');
        compilerabort(inconsistent);
        end
      else with keytable[key].oprnd do
        begin
        m := immediatelong;
        reg := 0;
        indxr := 0;
        flavor := float;

        if len = quad then {double precision}
          if mc68881 then
            begin
            m := immediatequad;
            kluge.i := oprnds[1];
            offset1 := (kluge.damn[word_zero] * 256) * 256
              + (kluge.damn[word_one] and 65535);
              { The "and" defeats sign extension }
            kluge.i := oprnds[2];
            offset := (kluge.damn[word_zero] * 256) * 256
              + (kluge.damn[word_one] and 65535);
{            begin
            genlongword(offset);
            genlongword(offset1);
            offset := highcode;
            offset1 := 0;
            highcode := highcode + 8;
            m := pcrelative;
}            end
          else
            begin
            kluge.i := oprnds[1];
            offset1 := (kluge.damn[word_zero] * 256) * 256
              + (kluge.damn[word_one] and 65535);
              { The "and" defeats sign extension }
            kluge.i := oprnds[2];
            offset := (kluge.damn[word_zero] * 256) * 256
              + (kluge.damn[word_one] and 65535);
            end
        else {single precision}
          begin
          kluge.i := oprnds[1];
          offset := kluge.damn[true];
          offset1 := kluge.damn[false];
          end;
        end;        
  end {dorealx} ;


procedure dostaticlevels(ownflag: boolean {true says own sect def} );

{ Generate a reference to the data area for the level specified in
  opernds[1].  This is a direct reference to the global area for level 1,
  and a reference relative to sp for the local frame.  There is another
  procedure, dolevelx, in genblk which handles intermediate level references.
  These two cases (global+current vs. intermediate levels) are split up
  purely to save space and to facilitate inclusion of blockcodex in this
  overlay.
}

  var
    reg: regindex; {reg for indirect reference}


  begin
    keytable[key].access := valueaccess;
    with keytable[key], oprnd do
      begin
      if ownflag then
        begin
        if pic_enabled then
          begin
          m := relative;
          reg := pic_own_base;
          end
        else
          begin
          m := commonlong;
          commonlong_reloc := own_section;
          end;
        offset := 0;
        end
      else if left = 0 then
        begin
        m := abslong;
        offset := 0;
        end
      else if left = 1 then
        begin
        m := relative;
        offset := globalbase;
        reg := gp;
        end
      else if left = level then
        begin
        m := relative;
        offset := - blksize;
        reg := fp;
        end;
      knowneven := true;
      len := long;
      end;
  end {dostaticlevels} ;


procedure blockcodex;

{ Generate code for the beginning of a block.

  If this is a procedure block, standard procedure entry code is generated.
}

  var
    I: regindex; {induction var for initializing context stack}


  procedure load_own_reg;

  { Generate code to load register A3 for PIC code.

      MOVE.L   #G-P_OWN,A3
      ADDA.L   4(A5),A3

    The special mode PIC_OWN_IMMED is a hack to generate #G-P_OWN.  Location
    4(A5) is loaded by the library init code to point to the own section
    WHICH MUST BE IN SECTION 15.  The move loads the distance from the start
    of section 15 to our own section.
  }
    begin
    settempareg(debuglink);
    settemp(long, pic_own_immed, 0, 0, false, 0, 0, 1, unknown);
    gen2(move, long, tempkey, tempkey + 1);
    keytable[tempkey].oprnd.m := relative;
    keytable[tempkey].oprnd.offset := 4;
    keytable[tempkey].oprnd.reg := gp;
    gen2(add, long, tempkey, tempkey + 1);
    tempkey := tempkey + 2;
    end; { load_own_reg }


  procedure prochdr;

{ Generate code for a procedure entry, and log data about it in the proctable.

  This generates the indirect moves prior to the actual entry for static
  link tracing, and also generates nop's to be filled with register save
  instructions after we know which registers are used.

  The stack check code is designed to be "safe", i.e. to guarantee that the
  code itself won't trash memory outside the assigned stack space.  To do
  this we check for the space used by all parameters passed to interior
  procedures as well as a worst-case check for all the registers that
  might be saved by this procedure.  This check is slightly on the conservative
  side (by up to about 50 bytes) but if the user's program is that close to
  grief, who cares!?!?  Cost is two words (one instruction) over the older,
  minimum check.
}

    var
      i: integer; {general use induction variable}


    begin
      procmap[blockref].addr := currentpc;
      with proctable[blockref] do
        begin
        if intlevelrefs then
        {generate indirect moves before entry}
          begin
          settempreg(long, indr, sl);
          settempareg(sl);
          for i := 1 to levelspread - 1 do
            begin
            procmap[blockref].addr := procmap[blockref].addr + word;
            gen2(move, long, tempkey + 1, tempkey);
            end;
          end;
        end;
      blocklabelnode := lastnode + 1;
      stackoffset := 0;
      newtemp(13 * long);
      stackoffset := 13 * long; {potentially 13 registers to be saved}

      if mc68881 then
        begin
        newtemp(96);
        stackoffset := stackoffset + 96; {potentially 8 extended fpregs saved}
        end;

      if language = modula2 then
        if openarray_base <> nil then
          begin
          newtemp(4);
          stackoffset := stackoffset + 4; { one longword pointer saved }
          end;

      linkentryinst := lastnode;
      for i := 1 to prologuelength do geninst(nop, 0, 0);
      settempimmediate(word, 0);
      settempreg(long, autod, sp);
      savereginst := lastnode + 1;
      gen2(movem, long, tempkey + 1, tempkey);

      if mc68881 then
        begin
        fpsavereginst := lastnode + 1;
        fpgendouble(fmovem, tempkey + 1, tempkey);
        end;

      if language = modula2 then
        if openarray_base <> nil then copy_openarrays;

      if switcheverplus[debugging] or switcheverplus[profiling] then
        begin
        tempkey := loopcount - 1;
        settempimmediate(word, level);
        settempreg(word, autod, sp);
        gen2(move, word, tempkey + 1, tempkey);
        settempimmediate(word, 257); {large enough the peephole won't change it}
        settempreg(word, autod, sp);
        savedebinst := lastnode + 1;
        gen2(move, word, tempkey + 1, tempkey);
        callsupport(libdebugger_entry);
        settempimmediate(word, 4); {arguments to pop}
        settempareg(sp);
        gen2(add, word, tempkey + 1, tempkey);
        end;
      stuffreginst := lastnode;
      for i := 1 to maxstuffnodes do geninst(nop, 0, 0);

        if pic_enabled and (ownsize > 0) and proctable[blockref].ownused then
          load_own_reg;
    end {prochdr} ;


  procedure mainhdr;

{ Emit code to enter main program.  Calls Pascal Runtime initialization
  routine with the amount of global storage required passed on the stack.
}

  var
    i: integer; {general use induction variable}
    initcall: libroutines; {support routine to call for initialization}


    begin {mainhdr}
      main_import_offset := currentpc; { for modula2 }
      blocklabelnode := lastnode + 1;

      if switchcounters[mainbody] > 0 then
        begin
        procmap[blockref].addr := currentpc;
        with newesd do
          begin
          esdkind := esdglobal;
          glbsize := globalsize;
          end;
        insertnewesd;

        newesd.esdkind := esdbegin;  {Make an XDEF BEGIN$ in the object file}
        insertnewesd;
        startaddress := currentpc;
        geninst(lea, 2, 4);
        newnode;
        with lastptr^ do
          begin
          tempcount := 0;
          kind := relnode;
          if pic_enabled and not mc68020 then distance := 3
          else distance := 1;
          end;
        settempareg(sp);
        genoprnd(keytable[tempkey].oprnd);
        if switcheverplus[debugging] then initcall := libdebugger_init
        else if switcheverplus[profiling] then initcall := libprofilerinit
        else initcall := libinitialize;

        if pic_enabled and not mc68020 then
          begin
          settempareg(6); { A6 is a suitable register; Vdos passes
                            useful information in the  others }
          settemp(long, pic_splat_pcrel, 0, 0, false, 10, 0, 1, unknown);
          gen2(lea, long, tempkey, tempkey + 1);
          settemp(word, pic_supportcall, 0, 0, false, ord(initcall), 6, 1,
                  unknown);
          gen2(adda, long, tempkey, tempkey + 2);
          tempkey := tempkey + 2;
          keytable[tempkey].oprnd.m := indr;
          end
        else settemp(word, supportcall, 0, 0, false, ord(initcall), 0, 1,
                     unknown);

        gen1(jmp, 0, tempkey);
        stuffreginst := lastnode;
        for i := 1 to maxstuffnodes do geninst(nop, 0, 0);

        if pic_enabled and (ownsize > 0) and proctable[blockref].ownused then
          load_own_reg;
        end;
    end {mainhdr} ;


  procedure cmainhdr;

{ Generate main program code for a c program.  Since the C main program
  can be called as a function from within the main code we have to be
  a bit careful here, and actually do most of the work in prochdr.  The
  only "initialization" code is a single call to an init procedure.
}
    begin
      prochdr;
      callsupport(libcinit);
    end; {cmainhdr}


  begin {blockcodex}
    { If generating PIC for the 68020, pic_own_base (currently A3) is needed
      only if there is an own section pointer.
    }
    if pic_enabled and (ownsize > 0) and proctable[blockref].ownused then
      aregisters[pic_own_base] := 100000;
    if level = 1 then mainhdr else prochdr;

    context[1].lastbranch := lastnode;
    context[1].firstnode := lastnode;
    context[1].keymark := lastkey + 1;
    for i := 0 to 7 do context[1].abump[i] := false;
    for i := 0 to 7 do context[1].dbump[i] := false;
    for i := 0 to 7 do context[1].fpbump[i] := false;
    context[0] := context[1];
    lastdreg := 7 - left;
    lastfpreg := 7 - target;
{    if (switchcounters[debugging] > 0) or (switchcounters[profiling] > 0) then
      right := right + 1; }
{    if proctable[blockref].intlevelrefs then lastareg := sl - right - 1
    else }
    lastareg := sl - right;
    lineoffset := pseudoinst.len;
    nowdiagnosing := switchcounters[walkback] > 0;
  end {blockcodex} ;



procedure blockentryx;

{ Called at the beginning of every block, after a "blockentry" pseudo-op
  has been read.

  This just sets up to generate code and saves data about the block.
}


  begin {blockentryx}
    initblock;
    with pseudoinst do
      begin
      blockref := oprnds[1];
      paramsize := oprnds[2];
      blksize := oprnds[3];
      end;
    level := proctable[blockref].level;
    blockusesframe := switcheverplus[framepointer]
	or ((language = modula2) and proctable[blockref].needsframeptr);
  end {blockentryx} ;


procedure blockexitx;

{ Finish up after one procedure block.
}
  var
    i: 0..7;
    anyfound: boolean;

  begin {blockexitx}
    if (level <> 1) or (switchcounters[mainbody] > 0) then putblock;
    if (blockref = 0) or (level = 1) then
      mainsymbolindex := pseudoinst.oprnds[1];

    { Complain about any registers that have a non-zero reference count at the
      end of the procedure.  This is not a fatal condition because we will
      only generate code that uses more registers than it should, but it
      is usually correct.
    }
    if switcheverplus[test] then
      begin
      anyfound := false;
      if (pic_enabled and (ownsize > 0) and proctable[blockref].ownused) or
        (switchcounters[debugging] > 0) or (switchcounters[profiling] > 0) then
        aregisters[3] := 0;
      for i := 0 to lastareg do
        if aregisters[i] <> 0 then anyfound := true;

      for i := 0 to lastdreg do
        if dregisters[i] <> 0 then anyfound := true;

      if mc68881 then
        for i := 0 to lastfpreg do
          if fpregisters[i] <> 0 then anyfound := true;

      if anyfound then
        begin
        write('Found registers with non-zero use counts');
        compilerabort(inconsistent); { Display procedure name }

        for i := 0 to lastareg do
          if aregisters[i] <> 0 then
            write('  A', i:1, ' = ', aregisters[i]:1);

        for i := 0 to lastdreg do
          if dregisters[i] <> 0 then
            write('  D', i:1, ' = ', dregisters[i]:1);

        if mc68881 then
          for i := 0 to lastfpreg do
            if fpregisters[i] <> 0 then
              write('  FP', i:1, ' = ', fpregisters[i]:1);

        writeln;
        end;
      end;
  end   {blockexitx};


procedure flushbuffers;

{ dispose of software virtual memory buffers on heap
}

  var
    i: integer; {induction var for scanning blocksin}


  begin
    for i := 1 to lastblocksin do
      with blocksin[i] do 
	if (not lowblock) and (buffer <> nil) then dispose(buffer);
  end; {flushbuffers}


procedure exitcode;

{ Clean up after code generation.
}

  var
    i: integer;

  begin {exitcode}
    fixdefines;
    if not everdiagnosing and
      (switcheverplus[debugging] or switcheverplus[profiling]) then initdiags;
    if switcheverplus[outputmacro] then fixmac;
    flushbuffers;
  end; {exitcode}

procedure seekstringfile(n: integer {byte to access});

{ Do the equivalent of a "seek" on the string file.  This sets the
  file and "nextstringfile" to access byte "n" of the stringfile.
}

  var
    newblock: 1..maxstringblks; { block to which seeking }


  begin {seekstringfile}
    newblock := n div (diskbufsize + 1) + 1;
    if newblock <> curstringblock then
      begin
      curstringblock := newblock;
      stringblkptr := stringblkptrtbl[newblock];
      if stringblkptr = nil then
        begin
        write('unexpected end of stringtable ');
        compilerabort(inconsistent);
        end;
      end;
    nextstringfile := n mod (diskbufsize + 1);
  end {seekstringfile} ;


function get_stringfile_byte(loc: integer): integer;

{ Return a byte from the string table.
}
  begin
  { Adjust loc if constant is in the structured constant section.
  }
  if loc >= stringfilecount then
    loc := stringtablelimit + loc - stringfilecount;

  curstringblock := loc div (diskbufsize + 1) + 1;
  stringblkptr := stringblkptrtbl[curstringblock];
  nextstringfile := loc mod (diskbufsize + 1);
  get_stringfile_byte := stringblkptr^[nextstringfile]; 
  end;


procedure debugkey(k :keyindex);

begin
  with keytable[k] do
    begin
      write('key is ',k:1, ', access is ');
      case access of
        valueaccess: write('valueaccess');
	noaccess: write('noaccess');
        branchaccess: write('branchaccess');
        end;
      writeln(' and refcount is ',refcount:1);	
    end;
end;


function equivaddr(l, r: keyindex): boolean;

{ True if the addresses accessed for key l and key r are the same.
}


  begin {equivaddr}
    with keytable[l].oprnd, keytable[r] do
      if (access <> valueaccess) or (keytable[l].access <> valueaccess) or
         (packedaccess <> keytable[l].packedaccess) or (oprnd.m <> m) or
         (oprnd.reg <> reg) or (oprnd.offset <> offset) then
        equivaddr := false
      else if not (m in [dreg, areg, fpreg, indr, autoi, autod, twodregs,
                         twofpregs]) and
         ((oprnd.offset <> offset) or (oprnd.offset1 <> offset1) or
         (oprnd.indxr <> indxr) or
         (oprnd.commonlong_reloc <> commonlong_reloc)) then equivaddr := false
      else equivaddr := true;
  end {equivaddr} ;


function equivaccess(l, r: keyindex): boolean;

{ True if the addresses accessed for key l and key r are the same.
}


  begin {equivaccess}
    equivaccess := equivaddr(l,r) and (keytable[l].regvalid =
                   keytable[r].regvalid) and (keytable[l].indxrvalid =
                   keytable[r].indxrvalid);
  end {equivaccess} ;


procedure bumptempcount(k: keyindex; {key of temp desired}
                        delta: integer {amount to adjust ref count} );

{ Increment the reference of any temp containing the value for key "k".
  If there is no temp assigned, this is a no-op
}


  begin
    with keytable[k] do
      begin
      if regsaved and (properreg >= stackcounter) then
        with keytable[properreg] do
          begin
          if (delta < - refcount) then { overflow is rarely a problem }
            begin
            write('BUMPTEMPCOUNT, refcount underflow');
            compilerabort(inconsistent);
            end;
          refcount := refcount + delta;
          end;

      if indxrsaved and (properindxr >= stackcounter) then
        with keytable[properindxr] do
          begin
          if (delta < - refcount) then { overflow is rarely a problem }
            begin
            write('BUMPTEMPCOUNT, refcount underflow');
            compilerabort(inconsistent);
            end;
          refcount := refcount + delta;
          end;
      end;
  end {bumptempcount} ;


function pushing(k: keyindex {expression to check}) : boolean;

{ True if the value of "k" is being pushed onto the stack.
}


  begin
    with keytable[k], oprnd do
      pushing := (m = relative) and (reg = sp) and
                 (stackoffset + len + offset <= 0);
  end {pushing} ;


function popping(k: keyindex {expression to check}) : boolean;

{ True if the value of "k" is being popped off the stack.
}


  begin
    with keytable[k], oprnd do
      popping := (m = relative) and (reg = sp) and uselesstemp and
                 ( - offset = stackoffset) and
                 (keytable[stackcounter + 1].oprnd.offset - offset <=
                  len + 1);
  end {popping} ;



procedure fixaccess(oprndlen: datarange; {instruction operand length}
                    k: keyindex; {key holding operand}
                    var oprnd: operand {operand to change} );

{ Changes relative-sp mode to (sp)+ or -(sp) where possible.
}


  begin {fixaccess}
    oprndlen := max(word, oprndlen);
    oprnd := keytable[k].oprnd;
    with oprnd do
      if popping(k) and
         (stackoffset - oprndlen =
         - keytable[stackcounter + 1].oprnd.offset) then
        begin
        returntemps(1);
        m := autoi;
        offset := 0;
        end
      else if pushing(k) and
              (stackoffset + oprndlen =
              - keytable[stackcounter].oprnd.offset) then
        begin
        stackoffset := stackoffset + oprndlen;
        m := autod;
        offset := 0;
        end;
  end {fixaccess} ;



procedure gensingle(i: insttype; {instruction to generate}
                    dst: keyindex {keytable descriptor of operand} );

{ Generate a single operand instruction.  Differs from "gen1" (defined
  previously) in that operand length is calculated from the operand, and
  stack pops and pushes are calculated using "fixaccess".
}

  var
    dstoprnd: operand; {holds result of fixaccess}


  begin {gensingle}
    geninst(i, 1, keytable[dst].len);
    fixaccess(keytable[dst].len, dst, dstoprnd);
    genoprnd(dstoprnd);
    keytable[dst].oprnd.flavor := int;
  end {gensingle} ;


procedure fpgensingle(i: insttype; {instruction to generate}
                      src: keyindex {operand descriptor} );

{ Generate a single operand f.p. instruction.  Currently this is only used for
  FTST.
}

  var
    l: datarange; {calculated operand length}
    srcoprnd: operand; {modified operands returned by fixaccess}


  begin {fpgensingle}
    l := keytable[src].len; { instlength uses this for immediate constants }
    geninst(i, 1, l);

    { Insert the 68881 floating-point format in the instruction node.
    }
    if (i <> fmove) and (keytable[src].oprnd.m = fpreg) then
      lastptr^.fp_format := extended_real
    else if keytable[src].oprnd.flavor = float then
      if l = 12 then lastptr^.fp_format := extended_real
      else if l = quad then lastptr^.fp_format := double_real
      else lastptr^.fp_format := single_real
    else { It's an int_float }
      begin
      write('Illegal 68881 data type in fpgensingle');
      compilerabort(inconsistent);
      end;

    fixaccess(l, src, srcoprnd);
    genoprnd(srcoprnd);
  end {fpgensingle} ;

procedure gendouble(i: insttype; {instruction to generate}
                    src, dst: keyindex {operand descriptors} );

{ Generate a double operand instruction.  Like gensingle, calculates operand
  length and calls fixaccess to generate stack pop and push modes.
}

  var
    l: datarange; {calculated operand length}
    srcoprnd, dstoprnd: operand; {modified operands returned by fixaccess}


  begin {gendouble}
    l := min(keytable[src].len, keytable[dst].len);
    geninst(i, 2, l);
    fixaccess(l, src, srcoprnd);
    genoprnd(srcoprnd);
    fixaccess(l, dst, dstoprnd);
    genoprnd(dstoprnd);
    keytable[src].oprnd.flavor := int;
    keytable[dst].oprnd.flavor := int;
  end {gendouble} ;

procedure gensimplemove(src, dst: keyindex {move src to dst} );

{ Generate a move of a simple operand.
}


  begin
    if not equivaddr(src, dst) then gendouble(move, src, dst);
  end {gensimplemove} ;


procedure fpgendouble(i: insttype; {instruction to generate}
                      src, dst: keyindex {operand descriptors});

{ Generate a double operand f.p. instruction.  Like gensingle, calculates
  operand length and calls fixaccess to generate stack pop and push modes.
}

  var
    l: datarange; {calculated operand length}
    dstl, srcl: datarange; {dst, src lengths}
    srcoprnd, dstoprnd: operand; {modified operands returned by fixaccess}


  begin {fpgendouble}
    srcl := keytable[src].len; { instlength uses this for immediate constants }
    dstl := keytable[dst].len;
    geninst(i, 2, srcl);

    { Insert the 68881 floating-point format in the instruction node.
    }
    if (i in [fmovecr, fmovem]) or
       ((i = fmove) and (keytable[src].oprnd.m = fpreg) and 
         (keytable[dst].oprnd.m = fpreg)) or
       ((i <> fmove) and (keytable[src].oprnd.m = fpreg)) then
      begin
      lastptr^.fp_format := extended_real;
      l := 12;
      end
    else if (i = fmove) and (keytable[dst].oprnd.m <> fpreg) then
      begin
      if keytable[dst].oprnd.flavor = float then
        if dstl = 12 then lastptr^.fp_format := extended_real
        else if dstl = quad then lastptr^.fp_format := double_real
        else lastptr^.fp_format := single_real
      else if keytable[dst].oprnd.flavor = int then
        begin
        if dstl = byte then lastptr^.fp_format := byte_integer
        else if dstl = word then lastptr^.fp_format := word_integer
        else if dstl > word then lastptr^.fp_format := long_integer;
             { includes 3 byte integers }
        end
      else { It's an int_float }
        begin
        write('Illegal 68881 data type in fpgendouble');
        compilerabort(inconsistent);
        end;
      l := dstl;
      end
    else if keytable[src].oprnd.flavor = float then
      if srcl = 12 then lastptr^.fp_format := extended_real
      else if srcl = quad then lastptr^.fp_format := double_real
      else lastptr^.fp_format := single_real
    else if keytable[src].oprnd.flavor = int then
      begin
      if srcl = byte then lastptr^.fp_format := byte_integer
      else if srcl = word then lastptr^.fp_format := word_integer
      else if srcl > word then lastptr^.fp_format := long_integer;
           { includes 3 byte integers }
      end
    else { It's an int_float }
      begin
      write('Illegal 68881 data type in fpgendouble');
      compilerabort(inconsistent);
      end;

    with keytable[src], oprnd do
      if m = fpreg then len := srcl;

    with keytable[dst], oprnd do
      if m = fpreg then len := dstl;
	
    fixaccess(keytable[src].len, src, srcoprnd);
    genoprnd(srcoprnd);
    fixaccess(l, dst, dstoprnd);
    genoprnd(dstoprnd);
  end {fpgendouble} ;

procedure genfpmove(src, dst: keyindex {move src to dst} );

{ Generate a floating-point move of a simple operand.
}


  begin
    if not equivaddr(src, dst) then fpgendouble(fmove, src, dst);
  end {genfpmove} ;

procedure generror(err: integer);

{ Generate an error node to pass more precise data to the postmortem analyzer
}


  begin
    newnode;
    with lastptr^ do
      begin
      tempcount := 0; {for node dump only}
      kind := errornode;
      errorno := err;
      oprndcount := 0;
      end;
  end; {generror}

function signedoprnds : boolean;

{ True if both left and right operands of the current operation are
  signed.  Picks up the operands from the globals "left" and "right".
}


  begin
    signedoprnds := keytable[left].signed and keytable[right].signed;
  end {signedoprnds} ;


function bytelength(k: keyindex {operand to examine} ): datarange;

{ Returns the operand length in bytes, regardless of whether the operand
  is packed or not.  Note that a byte length of 3 is a special case that is
  rounded up to long.
}

  var
    length: integer;

  begin
    with keytable[k],oprnd do
      if packedaccess then
        begin
        length := (len + bitsperunit - 1) div bitsperunit;
        if length = 3 then bytelength := long else bytelength := length;
        end
      else if (m <> immediate) then
        bytelength := len
      else if (offset >= - 128) and (offset <= 127) then
        bytelength := byte
      else if (offset >= - 32767 - 1) and (offset <= 32767) then
        bytelength := word
      else bytelength := long;
  end {bytelength} ;


function roundbytes(bytelen: addressrange): addressrange;

{ Round given byte length to a power-of-two.
}


  begin
    if bytelen > word then roundbytes := long
    else roundbytes := bytelen;
  end {roundbytes} ;


procedure setlongvalue(i:integer);

{ Set a keytable entry to the value of i.  The global variable "right"
  is set to the entry's index, since this routine is designed to be called
  by routines which handle pseudoops of the form xxxlitptr and xxxlitreal.
}


  begin
    settemp(len, immediate, 0, 0, false, i, 0, 1, unknown);
    right := tempkey;
  end {setlongvalue} ;

procedure adjustregcount(k: keyindex; {operand to adjust}
                         delta: integer {amount to adjust count by});

{ Adjusts the register reference count for any registers used in the
  specified operand.  If a register pair is used, both registers will
  be adjusted by the same amount.
}


  begin
    with keytable[k], oprnd do
      if access = valueaccess then
        case m of
          dreg:
            if regvalid then
              dregisters[reg] := dregisters[reg] + delta;
          pcindexed:
            if indxrvalid then
              dregisters[indxr] := dregisters[indxr] + delta;
          areg, indr, autoi, autod, relative:
            if regvalid then
              aregisters[reg] := aregisters[reg] + delta;
          fpreg:
            begin
            if regvalid then
              fpregisters[reg] := fpregisters[reg] + delta;
            end;
          indexed, bitindexed:
            begin
            if indxrvalid then
              dregisters[indxr] := dregisters[indxr] + delta;
            if regvalid then
              aregisters[reg] := aregisters[reg] + delta;
            end;
          twodregs:
            begin
            if indxrvalid then
              dregisters[indxr] := dregisters[indxr] + delta;
            if regvalid then
              dregisters[reg] := dregisters[reg] + delta;
            end;
          twofpregs:
            begin
            if indxrvalid then
              fpregisters[indxr] := fpregisters[indxr] + delta;
            if regvalid then
              fpregisters[reg] := fpregisters[reg] + delta;
            end;
          otherwise
          end;
  end {adjustregcount} ;


procedure setbr(inst: insttype {branch instruction used} );

{ Sets the operand data for an operand which is accessed by a branch.
  That is, only the condition code is used.  The type of conditions tested
  for are implicit in the branch instruction.

  This uses the global "key", which is the operand for the latest
  pseudoinstruction.
}


  begin
    adjustdelay := true;
    with keytable[key] do
      begin
      access := branchaccess;
      brinst := inst
      end;
  end {setbr} ;


procedure setvalue(m: modes; {hardware operand mode}
                   reg: regindex; {register field, if any}
                   indxr: regindex; {index register field, if any}
                   indxlong: boolean; {true if index register is long}
                   offset: addressrange; {immediate operand or fixed offset}
                   offset1: addressrange {extension for 32 bit fixed operand}
		   );

{ Sets the current operand to be a value access and sets reference counts
  according to the data in the current pseudo-instruction.

  This uses the global "key", which is the operand for the latest
  pseudoinstruction.
}


  begin
    with keytable[key] do
      begin
      access := valueaccess;
      oprnd.m := m;
      oprnd.reg := reg;
      oprnd.indxr := indxr;
      oprnd.indxlong := indxlong;
      oprnd.offset := offset;
      oprnd.offset1 := offset1;
      oprnd.scale := 1;
      oprnd.commonlong_reloc := unknown;

      if m = fpreg then oprnd.flavor := float
      else if m = dreg then oprnd.flavor := int
      else oprnd.flavor := int_float;

      high_word_dirty := false;
      instmark := lastnode + 1;
      adjustregcount(key, refcount);
      bumptempcount(key, refcount);
      end;
  end {setvalue} ;


procedure setkeyvalue(k: keyindex);

{call setvalue with fields from keytable[k]
}


  begin
    with keytable[k], oprnd do
      begin
      keytable[key].packedaccess := packedaccess;
      setvalue(m, reg, indxr, indxlong, offset, offset1);
      keytable[key].oprnd.flavor := flavor;
      keytable[key].oprnd.scale := scale;
      keytable[key].oprnd.commonlong_reloc := commonlong_reloc;
      keytable[key].signed := signed;
      keytable[key].signlimit := signlimit;
      keytable[key].knowneven := knowneven;
      keytable[key].high_word_dirty := high_word_dirty;
      end;
  end {setkeyvalue} ;


procedure setallfields(k: keyindex);

{similar to setkeyvalue but also copies properaddress, packedrecord,
 etc.  Can only be used if we are copying a keyvalue and not changing
 regset, mode, or offset, as in dovarx.
}


  begin
    with keytable[k] do
      begin
      keytable[key].regsaved := regsaved;
      keytable[key].indxrsaved := indxrsaved;
      keytable[key].regvalid := regvalid;
      keytable[key].indxrvalid := indxrvalid;
      keytable[key].properreg := properreg;
      keytable[key].properindxr := properindxr;
      keytable[key].tempflag := tempflag;
      setkeyvalue(k);
      end;
  end {setallfields} ;

function savedreg(r: regindex {register to save}) : keyindex;

{ Save the given volatile data register on the runtime stack.  This
  routine is quite clever about the process since it attempts to reuse
  an existing copy of the register if possible.  If not, the contents
  of the register are pushed onto the stack.  This is coded as a function
  to simplify the coding of "savekey".  Normally, one would write this
  as a procedure with a var param, but one cannot pass a packed field as a
  var param.
}

  var
    i: keyindex; {induction var used to search keytable}
    found: boolean; {set true when we find an existing saved copy}
    passed_reg: boolean; {set true when we pass the same register but not
                          necessarily "found".}
    dreglen: word..long; {length of longest item in dreg}


  begin {savedreg}
    i := lastkey;
    found := false;
    dreglen := word;
    passed_reg := false;

    with context[contextsp] do
      while not found and (i >= keymark) do
        begin
        with keytable[i], oprnd do
          if (access = valueaccess) and (refcount > 0) then
            begin
            if (m in [dreg, twodregs]) and (r = reg) and regvalid then
              begin
              passed_reg := true;

              if regsaved and keytable[properreg].validtemp and
                ((properreg >= stackcounter) or (properreg <= lastkey)) then
                begin
                found := true;
                savedreg := properreg;
                end
              else if len > dreglen then
                dreglen := len;
              end
            else if (m in [pcindexed, indexed, bitindexed, twodregs]) and
                    (r = indxr) and indxrvalid then
              begin
              passed_reg := true;

              if indxrsaved and keytable[properindxr].validtemp then
                begin
                found := true;
                savedreg := properindxr;
                end
              else if (m = twodregs) and (len > dreglen) then dreglen := len
              else if (m = indexed) and indxlong then dreglen := long;
              end;
            end;
        i := i - 1;
        end;
    if not found then
      begin
{      if dontchangevalue > 0 then
        begin
        write('Can''t save register in a parameter list');
        compilerabort(inconsistent);
        end; }

      { We didn't pass an occurance of this register, even one that couldn't
        be used, so we must assume long.
      }
      if not passed_reg then dreglen := long;
                                                
      aligntemps;
      newtemp(dreglen);
      settempdreg(dreglen, r);
      gensimplemove(tempkey, stackcounter);
      tempkey := tempkey + 1;
      savedreg := stackcounter;
      end;
  end {savedreg} ;


function savefpreg(r: regindex {register to save}) : keyindex;

{ Save the given volatile f.p. register on the runtime stack.  This
  routine is quite clever about the process since it attempts to reuse
  an existing copy of the register if possible.  If not, the contents
  of the register are pushed onto the stack.  This is coded as a function
  to simplify the coding of "savekey".  Normally, one would write this
  as a procedure with a var param, but one cannot pass a packed field as a
  var param.
}

  var
    i: keyindex; {induction var used to search keytable}
    found: boolean; {set true when we find an existing saved copy}


  begin {savefpreg}
    i := lastkey;
    found := false;

    with context[contextsp] do
      while not found and (i >= keymark) do
        begin
        with keytable[i], oprnd do
          if (access = valueaccess) then
            if (m in [fpreg, twofpregs]) and (r = reg) and regvalid then
              begin
              if regsaved and keytable[properreg].validtemp then
                begin
                found := true;
                savefpreg := properreg;
                end;
              end;
        i := i - 1;
        end;
    if not found then
      begin
{      if dontchangevalue > 0 then
        begin
        write('Can''t save register in a parameter list');
        compilerabort(inconsistent);
        end; }

      aligntemps;
      newtemp(12); { Always save an fpreg as an extended to prevent rounding. }
      settempfpreg(r);
      keytable[stackcounter].len := 12;
      keytable[stackcounter].oprnd.flavor := float;
      genfpmove(tempkey, stackcounter);
      tempkey := tempkey + 1;
      savefpreg := stackcounter;
      end;
  end {savefpreg} ;

function saveareg(r: regindex): keyindex;

{ Save an address register.  Much like "savedreg" above, but simpler.
}

  var
    i: keyindex; {running induction var}


  begin {saveareg}
    i := lastkey;
    with context[contextsp] do
      begin
      while (i >= keymark) and not ((keytable[i].access = valueaccess) and
            (keytable[i].oprnd.m in
            [areg, indexed, bitindexed, relative, indr, autoi, autod]) and
            (keytable[i].oprnd.reg = r) and keytable[i].regvalid and
            keytable[i].regsaved and
            keytable[keytable[i].properreg].validtemp) do
        i := i - 1;
      if i < keymark {no safe copy found} then
        begin
{        if dontchangevalue > 0 then
          begin
          write('Can''t save register in a parameter list');
          compilerabort(inconsistent);
          end; }

        aligntemps;
        newtemp(long);
        settempareg(r);
        gensimplemove(tempkey, stackcounter);
        tempkey := tempkey + 1;
        saveareg := stackcounter;
        end
      else saveareg := keytable[i].properreg;
      end;
  end {saveareg} ;



procedure savekey(k: keyindex {operand to save} );

{ Save all volatile registers required by given key.  Calls upon
  "savedreg" and "saveareg" to do all the work.
}


  begin
    if k > 0 then
      with keytable[k] do
        if access = valueaccess then
          begin
          bumptempcount(k, - refcount);
          with oprnd do
            case m of
              fpreg, twofpregs:
                begin
                if (m = twofpregs) and indxrvalid and not indxrsaved and
                   (indxr <= lastfpreg) then
                  begin
                  properindxr := savefpreg(indxr);
                  indxrsaved := true;
                  end;

                if regvalid and not regsaved and (reg <= lastfpreg) then
                  begin
                  properreg := savefpreg(reg);
                  regsaved := true;
                  end;
                end;

              dreg, twodregs:
                begin
                if (m = twodregs) and indxrvalid and not indxrsaved and
                   (indxr <= lastdreg) then
                  begin
                  properindxr := savedreg(indxr);
                  indxrsaved := true;
                  end;
                if regvalid and not regsaved and (reg <= lastdreg) then
                  begin
                  properreg := savedreg(reg);
                  regsaved := true;
                  end;
                end;

              pcindexed:
                if indxrvalid and not indxrsaved and (indxr <= lastdreg) then
                  begin
                  properindxr := savedreg(indxr);
                  indxrsaved := true;
                  end;

              areg, indr, autoi, autod, relative, indexed, bitindexed:
                begin
                if (m in [indexed, bitindexed]) and indxrvalid and
                   not indxrsaved and (indxr <= lastdreg) then
                  begin
                  properindxr := savedreg(indxr);
                  indxrsaved := true;
                  end;
                if regvalid and not regsaved and (reg <= lastareg) then
                  begin
                  properreg := saveareg(reg);
                  regsaved := true;
                  end;
                end;
              otherwise
              end;
          bumptempcount(k, refcount);
          end;
  end {savekey} ;

  procedure saveactivekeys;

    var
      i: keyindex; {for stepping through active portion of keytable}


    begin {saveactivekeys}
     if dontchangevalue <= 0 then
      begin
      for i := context[contextsp].keymark to lastkey do
      with keytable[i] do
        if (refcount > 0) and not (regsaved and indxrsaved)
        then savekey(i);
      end;
    end {saveactivekeys} ;

procedure saveresult;

{ Save the result of an arithmetic operation, as described by
  keytable[key].  Called by routines that leave their results
  in the dedicated arithmetic registers.  The result can be in
  D register 4, or both D registers 4 and 3.

  Because certain arithmetic operations are simulated using
  dedicated registers, which might (very likely) be needed by
  the next operation, this routine (called following the call
  to setvalue) makes sure the key gets saved, even if it has
  only one reference remaining (the savekey at the end of the
  main loop of genblk doesn't know about this special case, and
  only saves the key value if there are two or more references
  left).

  If the only reference is a move or push, we don't bother, as
  the result is being saved at its destination.
}


  begin {saveresult}
    if not ((pseudobuff.op in [movint, movreal]) and
            (pseudobuff.oprnds[2] = key) or
            (pseudobuff.op in [pshint, pshreal]) and
            (pseudobuff.oprnds[1] = key)) then
      begin
      adjusttemps;
      if key > lastkey then
        lastkey := key;
      savekey(key);
      end;
  end {saveresult} ;

procedure setd4result;

{ set result of an operation to d4.
  Used by 32-bit integer and floating point emulator.
}

begin {setd4result}
  setvalue(dreg, 4, 0, false, 0, 0);
  saveresult;
end   {setd4result};

procedure markareg(r: regindex {register to clobber} );

{ Mark an address register used.  Since such a register is just about
  to be modified,  any operand which depends on its current value must
  be saved.  This is done by scanning the keytable for operands which
  use this register.  If the operand is within the current context,
  the value is saved in a temp.  In any case, the "join" flag is
  set so it will be invalidated at the join context at the end
  of a conditional construct.

  For each operand saved, a scan of unsaved keys is made to set
  any keys with equivalent access to the same temp location.
  In addition we scan the for stack looking for an occurrence of
  the register being used to hold the address of the control
  variable of a for loop, and if found, we set tempflag in its
  stack copy.  This use varies from the normal use of tempflag,
  since the stack item has no ordinary key to be the properreg of.
}

  var
    i, savedr: keyindex; {induction vars for keytable scan}
    saved: boolean; {set true when the register has been saved}
    j: loopindex;


  begin {markareg}
    aregused[r] := true;
    if r <= lastareg then
      begin
      saved := false;
      aregisters[r] := 0;
      context[contextsp].abump[r] := false;

      for j := loopsp downto 1 do loopstack[j].aregstate[r].killed := true;

      with context[contextsp] do
        for i := lastkey downto 1 do
          with keytable[i], oprnd do
            if (access = valueaccess) and
               (m in [areg, indr, autoi, autod, relative, indexed, bitindexed])
	       and (r = reg) and regvalid then
              begin
              if i >= keymark then {current context}
                begin
                if not regsaved and (refcount > 0) then
                  begin
                  regsaved := true;
                  if not saved then
                    begin
                    savedr := saveareg(r);
                    saved := true;
                    end;
                  properreg := savedr;
                  keytable[savedr].refcount := keytable[savedr].refcount +
                                               refcount
                  end;
                regvalid := false;
                end;
              joinreg := true;
              end;
      end;
  end {markareg} ;


procedure markdreg(r: regindex {register to clobber} );

{ Mark a data register used.  Since such a register is just about to be
  modified,  any operand which depends on its current value must be
  saved.  This is done by scanning the keytable for operands which
  use this register.  If the operand is within the current context,
  the value is saved in a temp.  In any case, the "join" flag is
  set so it will be invalidated at the join context at the end
  of a conditional construct.

  For each operand saved, a scan of unsaved keys is made to set
  any keys with equivalent access to the same temp location.
}

  var
    i, savedr: keyindex; {induction vars for keytable scan}
    saved: boolean; {true if the register has already been saved}
    j: loopindex;


  begin {markdreg}
    dregused[r] := true;
    if r <= lastdreg then
      begin
      saved := false;
      dregisters[r] := 0;
      context[contextsp].dbump[r] := false;

      for j := loopsp downto 1 do loopstack[j].dregstate[r].killed := true;

      with context[contextsp] do
        for i := lastkey downto 1 do
          with keytable[i], oprnd do
            if (access = valueaccess) and (m in [dreg, twodregs]) and
               (r = reg) and regvalid then
              begin
              if i >= keymark then {current context}
                begin
                if not regsaved and (refcount > 0) then
                  begin
                  regsaved := true;
                  if not saved then
                    begin
                    savedr := savedreg(r);
                    saved := true;
                    end;
                  properreg := savedr;
                  keytable[savedr].refcount := keytable[savedr].refcount +
                                               refcount
                  end;
                regvalid := false;
                end;
              joinreg := true;
              end
            else if (access = valueaccess) and
                    (m in [pcindexed, indexed, bitindexed, twodregs]) and
                    (r = indxr) and indxrvalid then
              begin
              if i >= keymark then
                begin
                if not indxrsaved and (refcount > 0) then
                  begin
                  indxrsaved := true;
                  if not saved then
                    begin
                    savedr := savedreg(r);
                    saved := true;
                    end;
                  properindxr := savedr;
                  keytable[savedr].refcount := keytable[savedr].refcount +
                                               refcount
                  end;
                indxrvalid := false;
                end;
              joinindxr := true;
              end;
      end;
  end {markdreg} ;


procedure markfpreg(r: regindex {register to clobber} );

{ Mark an f.p. register used.  Since such a register is just about to be
  modified,  any operand which depends on its current value must be
  saved.  This is done by scanning the keytable for operands which
  use this register.  If the operand is within the current context,
  the value is saved in a temp.  In any case, the "join" flag is
  set so it will be invalidated at the join context at the end
  of a conditional construct.

  For each operand saved, a scan of unsaved keys is made to set
  any keys with equivalent access to the same temp location.
}

  var
    i, savefpr: keyindex; {induction vars for keytable scan}
    saved: boolean; {true if the register has already been saved}
    j: loopindex;


  begin {markfpreg}
    fpregused[r] := true;
    if r <= lastfpreg then
      begin
      saved := false;
      fpregisters[r] := 0;
      context[contextsp].fpbump[r] := false;

      for j := loopsp downto 1 do loopstack[j].fpregstate[r].killed := true;

      with context[contextsp] do
        for i := lastkey downto 1 do
          with keytable[i], oprnd do
            if (access = valueaccess) and (m in [fpreg, twofpregs]) and
               (r = reg) and regvalid then
              begin
              if i >= keymark then {current context}
                begin
                if not regsaved and (refcount > 0) then
                  begin
                  regsaved := true;
                  if not saved then
                    begin
                    savefpr := savefpreg(r);
                    saved := true;
                    end;
                  properreg := savefpr;
                  keytable[savefpr].refcount := keytable[savefpr].refcount +
                                               refcount
                  end;
                regvalid := false;
                end;
              joinreg := true;
              end
            else if (access = valueaccess) and (m = twofpregs) and
               (r = indxr) and indxrvalid then
              begin
              if i >= keymark then {current context}
                begin
                if not indxrsaved and (refcount > 0) then
                  begin
                  indxrsaved := true;
                  if not saved then
                    begin
                    savefpr := savefpreg(r);
                    saved := true;
                    end;
                  properindxr := savefpr;
                  keytable[savefpr].refcount := keytable[savefpr].refcount +
                                               refcount
                  end;
                indxrvalid := false;
                end;
              joinindxr := true;
              end;
      end;
  end {markfpreg} ;


procedure reserve_dreg(k: keyindex; { key to check }
                      r: regindex  { register needed } );

{ Similar to markdreg except it will not mark the register if k is already
  using that register.
}
  begin
  with keytable[k], oprnd do
    if regvalid and (refcount >= 1) and (m = dreg) and (reg = r) then
      { it's fine }
    else markdreg(r); { blast the sucker! }
  end; { reserve_dreg }


{ Register allocation procedures }


function countdreg: integer;

      { Returns lowest register usage count of any data register.
       Register count is increased if register is seen to be useful
       beyond the next join point. This situation is recorded in the
       dbump field of the markstack when the context is first entered
       via a savelabel.
       }

  var
    cnt: integer;
    r: regindex;


  begin {countdreg}
    cnt := maxint;
    for r := 0 to lastdreg do
      if dregisters[r] + ord(context[contextsp].dbump[r]) < cnt then
        cnt := dregisters[r] + ord(context[contextsp].dbump[r]);
    countdreg := cnt;
  end {countdreg} ;


function bestdreg(reg: regindex {data register to check} ): boolean;

{ Returns true if reg is the "best" data register to step on.
}


  begin {bestdreg}
    bestdreg := (reg <= lastdreg) and
                (dregisters[reg] + ord(context[contextsp].dbump[reg]) <=
                countdreg);
  end {bestdreg} ;


function getdreg: regindex;

{ Return the least worthwhile data register.  If necessary, the current
  contents of the selected register is flushed via markdreg.
}

  var
    cnt: integer;
    r: regindex;


  begin {getdreg}
    cnt := countdreg;
    if targetopsys = apollo then
      begin
      r := lastdreg;
      while dregisters[r] + ord(context[contextsp].dbump[r]) <> cnt do
        r := r - 1;
      end
    else
      begin
      r := 0;
      while dregisters[r] + ord(context[contextsp].dbump[r]) <> cnt do
        r := r + 1;
      end;
    markdreg(r);
    getdreg := r;
  end {getdreg} ;


function countfpreg: integer;

{ Returns lowest register usage count of any f.p. register.
  Register count is increased if register is seen to be useful
  beyond the next join point. This situation is recorded in the
  fpbump field of the markstack when the context is first entered
  via a savelabel.
}

  var
    cnt: integer;
    r: regindex;


  begin {countfpreg}
    cnt := maxint;
    for r := 0 to lastfpreg do
      if fpregisters[r] + ord(context[contextsp].fpbump[r]) < cnt then
        cnt := fpregisters[r] + ord(context[contextsp].fpbump[r]);
    countfpreg := cnt;
  end {countfpreg} ;


function bestfpreg(reg: regindex {f.p. register to check} ): boolean;

{ Returns true if reg is the "best" data register to step on.
}


  begin {bestfpreg}
    bestfpreg := (reg <= lastfpreg) and
                (fpregisters[reg] + ord(context[contextsp].fpbump[reg]) <=
                countfpreg);
  end {bestfpreg} ;


function getfpreg: regindex;

{ Return the least worthwhile f.p. register.  If necessary, the current
  contents of the selected register is flushed via markfpreg.
}

  var
    cnt: integer;
    r: regindex;


  begin {getfpreg}
    cnt := countfpreg;
    r := 0;
    while fpregisters[r] + ord(context[contextsp].fpbump[r]) <> cnt do
      r := r + 1;
    markfpreg(r);
    getfpreg := r;
  end {getfpreg} ;


function countareg: integer;

{ Returns lowest register usage count of any address register.
  Register count is increased if register is seen to be useful
  beyond the next join point. This situation is recorded in the
  dbump field of the markstack when the context is first entered
  via a savelabel.
}

  var
    cnt: integer;
    r: regindex;


  begin {countareg}
    cnt := maxint;
    for r := 0 to lastareg do
      if aregisters[r] + ord(context[contextsp].abump[r]) < cnt then
        cnt := aregisters[r] + ord(context[contextsp].abump[r]);
    countareg := cnt;
  end {countareg} ;


function bestareg(reg: regindex {address reg to check} ) : boolean;

{ Returns true if address register r is the "best" address register
  available to step on.
}


  begin {bestareg}
    bestareg := (reg <= lastareg) and
                (aregisters[reg] + ord(context[contextsp].abump[reg]) <=
                countareg);
  end {bestareg} ;



function getareg : regindex;

{ Return the least worthwhile address register.  If necessary, the current
  contents of the selected register is flushed via markareg.
}

  var
    cnt: integer;
    r: regindex;


  begin {getareg}
    cnt := countareg;
    if targetopsys = apollo then
      begin
      r := lastareg;
      while aregisters[r] + ord(context[contextsp].abump[r]) <> cnt do
        r := r - 1;
      end
    else
      begin
      r := 0;
      while aregisters[r] + ord(context[contextsp].abump[r]) <> cnt do
        r := r + 1;
      end;
    markareg(r);
    getareg := r;
  end {getareg} ;


procedure allowmodify(var k: keyindex; {operand to be modified}
                      forcecopy: boolean {caller can force temp} );

{ Makes sure that the operand "k" can be modified.  If the operand was
  generated before the last conditional jump, it must not be modified, so
  a copy of the key is made in the temporary
  key area and the value of "k" modified accordingly.  This temporary
  key can be used in generating the current operand.  The boolean "forcecopy"
  forces this routine to create a copy of the key.
}


  begin
    if forcecopy or (k >= 0) and
       ((keytable[k].instmark < context[contextsp].lastbranch) or
       (stackoffset <> - keytable[stackcounter].oprnd.offset) or
       keytable[k].high_word_dirty) then
      begin
      if tempkey = lowesttemp then compilerabort(interntemp);
      tempkey := tempkey - 1;
      keytable[tempkey] := keytable[k];
      keytable[tempkey].refcount := 0;
      keytable[tempkey].copycount := 0;
      keytable[tempkey].regsaved := false;
      keytable[tempkey].indxrsaved := false;
      k := tempkey
      end;
  end {allowmodify} ;


procedure lock(k: keyindex {operand to lock} );

{ Make sure that operand "k" will not be deallocated by setting
  reference counts to an impossibly high value.
}


  begin
    adjustregcount(k, 100);
    bumptempcount(k, 100);
  end {lock} ;


procedure unlock(k: keyindex {operand to unlock} );

{ Undoes the effects of "lock" so normal deallocation can be done.
}


  begin
    bumptempcount(k, - 100);
    adjustregcount(k, - 100);
  end {unlock} ;


procedure changevalue(var key1: keyindex; {key to be changed}
                      key2: keyindex {source of key data} );

{ Effectively assigns the contents of key2 to key1.  This is complicated
  by the same things as allowmodify, and by the need to adjust reference
  counts.  If the key will be referenced later, it is saved.
}


  begin
    allowmodify(key1, dontchangevalue > 0);
    with keytable[key1] do
      begin
      adjustregcount(key1, - refcount);
      bumptempcount(key1, - refcount);
      regsaved := keytable[key2].regsaved or (refcount <= 0);
      indxrsaved := keytable[key2].indxrsaved or (refcount <= 0);
      regvalid := keytable[key2].regvalid;
      indxrvalid := keytable[key2].indxrvalid;
      properreg := keytable[key2].properreg;
      properindxr := keytable[key2].properindxr;
      packedaccess := keytable[key2].packedaccess;
      high_word_dirty := keytable[key2].high_word_dirty;
      oprnd := keytable[key2].oprnd;
      bumptempcount(key1, refcount);
      adjustregcount(key1, refcount);
      end;
    savekey(key1);
  end {changevalue} ;

function fix_effective_addr(k: keyindex): keyindex;

{ LEA and PEA can't have an areg or dreg as an argument so this function
  forces the use of the stack copy.
}

  begin {fix_effective_addr}
  with keytable[k], oprnd do
    if (m = dreg) or (m = areg) then
      begin
      keytable[properreg].tempflag := true;
      fix_effective_addr := properreg;
      if not regsaved then
        begin
        write('fix_effective_addr screw-up');
        compilerabort(inconsistent);
        end;
      end
    else
      fix_effective_addr := k;
  end; {fix_effective_addr}


procedure forcerelative(var k: keyindex; {force key to be of relative mode}
                        needareg: boolean; {true if a-reg based mode needed}
                        indexedok: boolean; {true if indexed mode will suffice}
                        offsetbias: integer; {amount which will bias offset}
                        shortoffset: boolean {true if need an 8-bit offset} );

{ Force keytable[k].oprnd.m to be "relative" or "indexed" (optional) mode.
  "offsetbias" is the value we will end up indexing by.  In addition, if
  "shortoffset" is true, we will require the offset word to have an 8-bit
  representation (signed), useful in setting up the hardware "indexed"
  mode.  Uses "changevalue" to make the change.

  "bitindexed" is considered equivalent to "indexed", in this case.
  "indexedok" forces "shortoffset" true -- simplifies calling code.
}

  var
    tempoffset: addressrange; {for reaching long relative addresses}


  begin {forcerelative}
    with keytable[k], oprnd do
      begin
      if (m = indexed) and indexedok then
        shortoffset := true;

      if ((m <> indexed) or not indexedok) and
         not (m in [immediate, immediatequad, abslong, pcrelative, commonlong,
                    bitindexed, relative]) or
         not (m in [abslong, commonlong]) and
         not mc68020 and (abs(offset + offsetbias) + len > 32767) or
         needareg and (m in [abslong, pcrelative, commonlong]) or
         (targetopsys = apollo) and switcheverplus[sharecode] and
         (m = commonlong) or
         shortoffset and ((m in [abslong, pcrelative, commonlong]) or
          (not mc68020 and (abs(offset + offsetbias) > 127))) then
          { ^^^ (mc68020 only)  Must be careful in last case not to prevent
            an LEA for a pcrelative operand when caller expects it.  i.e.
            accessbit when argument is a constant.
          }
        begin
        adjustregcount(k, - refcount);

        { For Versados the global section is acquired dynamicly, so we must
          always reference it using the gp register.  For unix the global
          section is static, so we use the gp register as an optimization
          to allow shorter instructions.
        }
        settempareg(getareg);
        adjustregcount(k, refcount);
        if (targetopsys = apollo) and switcheverplus[sharecode] and
           (m = commonlong) then
          begin
          settempadcon(m, offset, commonlong_reloc);
          gen2(move, long, tempkey, tempkey + 1);
          tempkey := tempkey + 1;
          end
        else if ((offset > 32767) or (offset < -32768)) and not (m in
                 [abslong, commonlong]) then
          begin
          if (targetopsys = unix) and (reg = gp) and (m = relative) then
            begin
            { Unix code is never pic, so we may address variables in the
              global section directly.  On the 68020 this is faster than
              relative mode with a long displacement.
            }
            settemp(long, commonlong, 0, 0, false, offset, 0, 1,
                    global_section);
            gen2(lea, long, tempkey, tempkey + 1);
            tempkey := tempkey + 1;
            end
          else if not mc68020 then
            begin
            tempoffset := offset;
            offset := 0;
            settempimmediate(long, tempoffset);
            gen2(lea, long, fix_effective_addr(k), tempkey + 1);
            gendouble(add, tempkey, tempkey + 1);
            offset := tempoffset;
            tempkey := tempkey + 1;
            end
          else {mc68020 only -- generate long displacement}
            gen2(lea, long, fix_effective_addr(k), tempkey);
          end
        else
          gen2(lea, long, fix_effective_addr(k), tempkey);
        keytable[tempkey].oprnd.m := relative;
        changevalue(k, tempkey);
        end;
      end;
  end {forcerelative} ;


procedure make_immediate(var k: keyindex; { the key to change }
                         compflag: boolean {complement constant if true} );

{ Pull a constant out of the constant section and make it immediate.
}

  var
    i: integer; { loop counter }
    constant: unsigned;

  begin { make_immediate }
  with keytable[k],oprnd do
    begin
    constant := get_stringfile_byte(offset);

    for i := 2 to len do
      constant := constant * 256 + get_stringfile_byte(offset + i - 1);

    if compflag then constant := not constant;

    settempimmediate(len, constant);
    k := tempkey;
    end;
  end; { make_immediate }


procedure popstack(n: integer {number of items to physically pop} );

{ Return temps and adjust the stack pointer to reflect the fact.
}

  var
    oldoffset: integer; {used in calculating space vacated by params}


  begin {popstack}
    oldoffset := - keytable[stackcounter].oprnd.offset;
    returntemps(n);
    if oldoffset - stackoffset <> 0 then
      begin
      settempareg(sp);
      settempimmediate(long, oldoffset - stackoffset);
      gendouble(add, tempkey, tempkey + 1);
      end;
  end {popstack} ;


procedure callandpop(entry: libroutines; args: integer); 

{ Calls support library routine, and returns arguments afterwards.
}

begin {callandpop}
  callsupport(entry);
  popstack(args);
end {callandpop} ;


function is_sp(r: regindex): boolean;

{ Returns true if argument is sp or if /noframe and argument is fp.
}

begin
  if (r = sp) or (not blockusesframe and (r = fp)) then
    is_sp := true
  else is_sp := false;
end;



{ Internal loop generation.

  These routines are used for building internal loops.  For example,
  a block copy, or a set operation on long sets may be implemented as
  a loop.  These routines are complicated by the variety of special
  cases which must be handled.

  A loop may be a two operand loop, such as a copy, or a three operand
  loop such as a set operation.  Also, a loop may be popping one of its
  arguments from the stack, as in copying a temp value to its destination,
  or it may be pushing the destination on the stack.  Both cases are
  handled as special cases.  As a further consideration, it may be
  cheaper to replicate the operation in line then to initialize the
  loop.

  Loop arguments are addressed using four special keys in the temporary
  key area.

  loopsrc:      Describes the main source operand.

  loopsrc1:     Describes the second source operand for three operand
                loops, otherwise is the same as the destination.

  loopdst:      Describes the destination operand.

  loopcount:    Describes the loop counter, if any.

  These are set up with the proper addressing modes to step through
  the operands in the desired direction.  This direction is chosen to
  minimize the instructions generated.  A loop will decrement the
  registers only if the destination operand is being pushed onto the
  stack a word at a time.  Otherwise it goes up.

  To allow multiple references to the same operand without multiple
  increments or decrements certain global booleans (loopupflag, popflag,
  loopdownflag) are set by this routine.  Other routines use this information
  to determine the proper addressing mode to be used for the first, middle,
  and last references within a loop.

  The "initloop" routine decides whether to implement an actual
  loop, based on data provided in the call.

  The basic template for implementing a loop is:

        initloop(.... pieces .....)
        for count := 1 to pieces do
          begin
          generate loop code
          bumploop
          end
        finishloop
}


procedure initloop(src: keyindex; {main source operand}
                   src1: keyindex; {secondary source operand}
                   dst: keyindex; {destination operand}
                   maxsize: integer; {max size of an operand}
                   maxpieces: integer; {max pieces to generate inline}
                   var loop: boolean; {set if an actual loop is generated}
                   var pieces: integer {number of inline operations to gen});

{ Set up for an internal loop.  If "src", "src1" and "dst" are distinct,
  this is assumed to be a three operand loop.  If "src1" is equivalent to
  "dst" it is assumed to be a two address loop.

  First the piece size to be used is computed.  If byte addressing is
  needed for any operand the piece size is one, otherwise maxpiecesize.
  The global "piecesize" is set correctly for the first operation.
  Bumploop is smart enough to reduce the piecesize if there is a remainder.

  If the total number of iterations needed is less than maxpieces, "loop"
  is false and "pieces" is set to the number of iterations required.  This
  produces inline code.  Otherwise "loop" will be true and "pieces" will be
  one.

  The loop direction is chosed depending on the destination usage and piece
  size, and the special loop keys are set up even if the code is to be
  generated inline.

  A new label will be defined after any setup code is done.
}

  var
    twoaddress: boolean; {set if two address loop}
    len: addressrange; {length of operand handled by the loop}
    bumplen: addressrange; {length to bump offsets by (0 unless downloop)}
    sizecounter: datarange; {used to calculate diminishing size}
    lengthremainder: addressrange; {used to calculate remaining size
                                    after each diminishing move}


  procedure loadcount;

{ Set up the keytable entry for "loopcount", get a register for it, and
  load the count value into the register.  This will be called only if
  "loop" is true.
}

    var
      count: integer;  { temporary for loop count }

    begin
      count := len div piecesize;

      with keytable[loopcount], oprnd do
        begin
        if count - 1 > $FFFF then len := long else len := word;
        access := valueaccess;
        indxr := 0;
        offset := 0;
        offset1 := 0;
        if loop then
          begin
          m := dreg;
          reg := getdreg;
          regvalid := true;
          if len = long then settempimmediate(long, count)
          else settempimmediate(word, count - 1);
          gendouble(move, tempkey, loopcount);
          lock(loopcount);
          end
        else
          begin
          m := nomode;
          reg := 0
          end;
        end;
    end {loadcount} ;


  procedure oneloopsetup(var k: keyindex {one actual loop operand} );

{ Setup one actual loop operand.  The operand is being used, so the
  register count is decremented.  In addition, if code is to be generated
  inline, the mode is forced to an indexed mode so we can increment
  the offset at compile time.
}


    begin
      adjustregcount(k, - 1);
      if not loop and (pieces > 1) then
        forcerelative(k, false, true, len, false);
    end {oneloopsetup} ;


  procedure oneloopelt(k: keyindex; {actual loop operand}
                       loopkey: keyindex {special temp loop operand} );

{ Set up one of the special loop operands from an actual loop operand.
  The keytable entry is set up even for inline code, but the operand is
  placed in a register only if a loop is actually being generated.
}


    begin
      with keytable[loopkey], oprnd do
        begin
        len := piecesize;
        access := valueaccess;
        regsaved := false;
        indxrsaved := false;
        regvalid := true;
        indxrvalid := true;
        if not loop or is_sp(keytable[k].oprnd.reg) and loopdownflag then
          oprnd := keytable[k].oprnd
        else
          begin
          m := areg;
          reg := keytable[k].oprnd.reg;
          indxr := 0;
          offset := 0;
          offset1 := 0;
          if (not popping(k) or (piecesize < word)) then
            begin
            if (keytable[k].oprnd.m in [pcrelative, abslong, commonlong]) or
                not bestareg(reg) then
              reg := getareg
            else markareg(reg);
            if not mc68020 and (keytable[k].oprnd.offset > 32767) and not 
               (keytable[k].oprnd.m in [abslong, commonlong]) then
              begin
              len := long; {for address computation herein}
              if reg <> keytable[k].oprnd.reg then
                begin
                settempareg(keytable[k].oprnd.reg);
                gensimplemove(tempkey, loopkey);
                end;
              settempimmediate(long, keytable[k].oprnd.offset);
              gendouble(add, tempkey, loopkey);
              len := piecesize; {reset length}
              end
            else
              gen2(lea, long, fix_effective_addr(k), loopkey);
            end;
          m := indr;
          end;
        end;
      lock(loopkey);
    end {oneloopelt} ;


  procedure loopup;

{ Set up for a loop which counts up.  To simplify inline code generation,
  if we are popping from the stack the "len" field in the special loop
  keys is set to zero for anything addressed relative to the stack pointer.
  This disables the compile-time incrementation of the offsets, since it
  is taken care of at run time by the increment to sp.

  The routine assumes that any operand being popped will be passed
  as "src".
}


    procedure oneloopup(k: keyindex; {actual loop operand}
                        loopkey: keyindex {special loop operand} );

{ Set up a single loop operand for a loop which counts up.  This would
  be very simple except that it checks for the special case of an operand
  being popped off the stack and sets it up to use (sp)+ rather than
  a separate register for an address.  This also sets "popflag"
}


      begin
        oneloopsetup(k);
        oneloopelt(k, loopkey);
        if popping(k) and (piecesize >= word) then
          begin
          popflag := true;
          keytable[loopkey].oprnd.m := autoi;
          end;
      end {oneloopup} ;


    begin {loopup}
      loopupflag := true;
      lock(src);
      if not twoaddress then lock(src1);
      oneloopup(dst, loopdst);
      unlock(src);
      oneloopup(src, loopsrc);
      if not twoaddress then begin
       unlock(src1);
       oneloopup(src1, loopsrc1);
       end;
      loadcount;
      if popflag then
        begin {disable compile-time incrementation for an actual pop}
        if is_sp(keytable[loopsrc].oprnd.reg) then keytable[loopsrc].len := 0;
        if is_sp(keytable[loopsrc1].oprnd.reg) then
          keytable[loopsrc1].len := 0;
        if is_sp(keytable[loopdst].oprnd.reg) then keytable[loopdst].len := 0;
        stackoffset := stackoffset - piecesize;
        end;
    end {loopup} ;


  procedure loopdown;

{ Set up for a loop which decrements.  This will be called only when the
  destination is being pushed onto the stack and the piece size is a word
  size.  Thus, it assumes that the stack will be being pushed and sets
  up operands accordingly.
}

    var
      bias: integer; {bias to be used to src operands}


    procedure oneloopdown(k: keyindex; {actual source operand}
                          loopkey: keyindex {special loop operand} );


      begin
        oneloopsetup(k);
        with keytable[k].oprnd do
          settemp(len, m, reg, indxr, indxlong, offset, offset1, scale,
                  commonlong_reloc);
        k := tempkey;
        if loop or (pieces > 1) then
          forcerelative(k, false, true, len, false);
        {compensate for auto decrement to follow}
        keytable[k].oprnd.offset := keytable[k].oprnd.offset + len;
        oneloopelt(k, loopkey);
        if not loop or is_sp(keytable[loopkey].oprnd.reg) then
          with keytable[loopkey], oprnd do
            begin {we must fudge, since it may be relative to sp}
            if is_sp(reg) then
              begin
{              offset := keytable[k].oprnd.offset + bias;}
              len := 0;
              end
            else len := - piecesize;
            offset := offset - piecesize;
            end;
      end {oneloopdown} ;


    begin {loopdown}
      loopdownflag := true;
      {Compute bias for src if needed}
      bias := - max(word, len);
      oneloopdown(src, loopsrc);
      if not twoaddress then oneloopdown(src1, loopsrc1);
      {we know we are pushing the destination operand}
      keytable[loopdst].regsaved := false;
      keytable[loopdst].oprnd.m := autod;
      keytable[loopdst].oprnd.reg := sp;
      keytable[loopdst].oprnd.indxr := 0;
      keytable[loopdst].oprnd.offset := 0;
      keytable[loopdst].oprnd.offset1 := 0;
      loadcount;
    end {loopdown} ;


  begin {initloop}

    twoaddress := equivaccess(dst, src1);
    popflag := false;
    loopupflag := false;
    loopdownflag := false;

    {get length of operation and see if we can use word/long operations}

    len := min(min(maxsize, keytable[src].len), keytable[dst].len);

    { Pick the largest piecesize that results in an integral number of
      move instructions.
    }
    if not mc68020 and not (keytable[src].knowneven and
       keytable[dst].knowneven) then

      { If we are going to push the item on the stack backwards and the length
        is odd and the source offset is even, then we can treat is as if it
        were even.
      }
      if pushing(dst) and odd(len) and (len > 1) and
         keytable[src].knowneven then
        piecesize := min(maxsize, long)
      else piecesize := 1
    else piecesize := min(maxsize, long);

    if (len = 3) and (piecesize > 2) then piecesize := 2;

    while piecesize > len do piecesize := piecesize div 2;

    { Now see if we are going to build an actual loop.
    }

    loop := (len div piecesize) > maxpieces;

    if loop then pieces := 1
    else pieces := len div piecesize;

    lengthremainder := len mod piecesize;

    { Now calculate the number of moves it will take.  i.e. to move a 7 byte
      structure we may be able (if piecesize is long) to use a long move a
      word move and a byte move.
    }
    sizecounter := piecesize;
    while sizecounter > 1 do
      begin
      sizecounter := sizecounter div 2;
      if (lengthremainder >= sizecounter) then
        begin
        pieces := pieces + 1;
        lengthremainder := lengthremainder - sizecounter;
        end;
      end;

    if loop then loopdatalength := len mod piecesize + piecesize
    else loopdatalength := len;

    adjustregcount(src, 1);
    if not twoaddress then adjustregcount(src1, 1);
    adjustregcount(dst, 1);

    { Decide which direction we are going.
    }
    if pushing(dst) then { possibly down }
      if ((piecesize = 1) or odd(len)) and (len > piecesize) then

        { -(sp) with byte op doesn't work, so bump sp down and use another
          register for loop.
        }
        begin
        settempimmediate(long, - keytable[stackcounter].oprnd.offset -
                         stackoffset);
        settempareg(sp);
        gendouble(sub, tempkey + 1, tempkey);
        stackoffset := - keytable[stackcounter].oprnd.offset;
        loopup;
        end
      else loopdown { -(sp) of words is just great }
    else loopup; { normal case, go up }

    if loop then definelastlabel;

    if twoaddress then
      begin
      keytable[loopsrc1] := keytable[loopsrc];
      lock(loopsrc1);
      end;
  end {initloop} ;


procedure bumploop(dbinst: insttype; {inst to finish loop}
                   var loop: boolean {value returned by initloop} );

{ Called in the middle of a loop to increment the offsets of the operands.

  This will actually be used only if the loop is generated inline.  It is
  called anyway, but the result is ignored.
}

  var
    newpiecesize: addressrange;

  begin {bumploop}
    if loop then
      begin
      if keytable[loopcount].len = long then
        begin
        if dbinst = dbne then genrelbr(beq, 2);
        settempimmediate(long, 1);
        gendouble(sub, tempkey, loopcount);
        genbr(bne, lastlabel + 1);
        end
      else gendb(dbinst, loopcount, lastlabel + 1);
      loop := false;
      end;

    loopdatalength := loopdatalength - piecesize;

    newpiecesize := piecesize;

    while newpiecesize > loopdatalength do
      newpiecesize := newpiecesize div 2;

    if (loopdownflag or popflag) and (piecesize > loopdatalength) then
      begin
      with keytable[loopsrc], oprnd do
        if len <> 0 then
          if loopdownflag then len := - newpiecesize
          else if is_sp(reg) then len := newpiecesize;
      with keytable[loopsrc1], oprnd do
        if len <> 0 then
          if loopdownflag then len := - newpiecesize
          else if is_sp(reg) then len := newpiecesize;
      with keytable[loopdst], oprnd do
        if len <> 0 then
          if loopdownflag then len := - newpiecesize
          else if is_sp(reg) then len := newpiecesize;
      end;

    { If the len is 0 and we are looping down (dst is autod) and the source is
      the stack and the piecesize just got smaller, then we must add the amount
      reduced to the offset because of the 68k's byte ordering.
    }
    with keytable[loopsrc], oprnd do
      if len <> 0 then
        begin
        offset := offset + len;
        if not loopdownflag then len := newpiecesize;
        end
      else if loopdownflag and is_sp(reg) and (newpiecesize <> piecesize) then
        offset := offset + piecesize - newpiecesize;

    with keytable[loopsrc1], oprnd do
      if len <> 0 then
        begin
        offset := offset + len;
        if not loopdownflag then len := newpiecesize;
        end
      else if loopdownflag and is_sp(reg) and (newpiecesize <> piecesize) then
        offset := offset + piecesize - newpiecesize;

    with keytable[loopdst], oprnd do
      if len <> 0 then
        begin
        offset := offset + len;
        if not loopdownflag then len := newpiecesize;
        end;

    piecesize := newpiecesize;
  end; {bumploop}


procedure finishloop;

{ Clean up after a loop, setting regoffset[sp] if we have been changing
  the stack pointer.  If we are actually looping, this routine generates
  the decrement and branch for the loop.
}


  begin
    unlock(loopsrc);
    unlock(loopsrc1);
    unlock(loopdst);
    unlock(loopcount);

    if popflag then stackoffset := - keytable[stackcounter + 1].oprnd.offset
    else if loopdownflag then
           stackoffset := - keytable[stackcounter].oprnd.offset;
  end {finishloop} ;


procedure firstreference(k: keyindex {loop address counter});

{ Makes the first reference to a loop counter in the code for a loop.
  @areg is changed to -(areg) if we are looping down.
}


  begin
    with keytable[k].oprnd do
      if loopdownflag and (m = indr) then m := autod;
  end {firstreference} ;


procedure middlereference(k: keyindex {loop address counter} );

{ Makes an auto decrement address mode into a register deferred address
  mode.  This is called after the first reference to a loop address
  counter to make the decrement occur once prior to all references within
  the loop.
}


  begin
    with keytable[k].oprnd do
      if loopdownflag and (m = autod) then m := indr;
  end {middlereference} ;


procedure lastreference(k: keyindex {loop address counter} );

{ Change register deferred mode to auto indirect if looping up.
}


  begin
    with keytable[k].oprnd do
      if loopupflag and (m = indr) then m := autoi
      else if loopdownflag and (m = autod) then m := indr;
  end {lastreference} ;


procedure onlyreference(k: keyindex {loop address counter} );

{ Calls first and last reference, of course! Used for one-reference
  loops, such as block moves.
}


  begin
    with keytable[k].oprnd do
      if (m = indr) then
        if loopdownflag then m := autod
        else if loopupflag then m := autoi;
  end {onlyreference} ;


{ Data movement routines }

procedure genblockmove(src, dst: keyindex {move operands} ;
                       minpiecesize: integer {minimum size chunk to move} );

{ Generate a block move.  Less than four words are moved inline,
  otherwise a loop is generated.  Double precision real constants are a
  special case, and are handled here by breaking the quadword into two
  longwords of immediate data.
}

  var
    loop: boolean; {set if a loop is actually used}
    pieces: integer; {number of internal instructions to generate}
    i: integer; {induction var for internal generation}
    source: keyindex; {tempkeys used to split double reals}
    temp: unsigned; { holds structured constant }
    carray: packed array [1..4] of uns_byte; { holds bytes of constant }
    length: integer; {length of move}
    oldlen: integer; {length of source, before possible truncation}


  begin {genblockmove}
    { The caller knows more than we do if the minimum piece size is more
      than a byte.  Setting knowneven allows initloop to use word or long
      moves.
    }
    if minpiecesize > byte then
      begin
      keytable[left].knowneven := true;
      keytable[right].knowneven := true;
      end;

    length := min(keytable[src].len, keytable[dst].len);

    if (keytable[src].oprnd.m in [immediate, immediatelong, immediatequad]) then
      if length <= long then

        { This check protects structured constant moves of word or long 
          length that are not word aligned.
        }
        if (minpiecesize = byte) and not (keytable[src].knowneven and
           keytable[dst].knowneven) then
          with keytable[src], oprnd do
            begin
            temp := offset;

            for i := 1 to length do
              begin
              carray[i] := temp mod $100;
              temp := temp div $100;
              end;

            { If going to the stack, force space to be allocated first
              because a series -(sp) moves of bytes won't work.
            }
            aligntemps;

            settempimmediate(byte, 0);

            for i := length downto 1 do
              begin
              keytable[tempkey].oprnd.offset := carray[i];
              gendouble(move, tempkey, dst);
              with keytable[dst].oprnd do offset := offset + 1;
              end;

            tempkey := tempkey + 1;
            end
        else gensimplemove(src, dst)
      else
        begin {it's a double precision real constant}
        with keytable[src].oprnd do
          if m = immediatequad then
            begin
            temp := offset1;
            settemp(long, immediatelong, 0, 0, false,
                    temp mod 65536, temp div 65536,
                    1, unknown);
            temp := offset;
            settemp(long, immediatelong, 0, 0, false,
                    temp mod 65536, temp div 65536,
                    1, unknown);
            source := tempkey;
            end
          else {immediatelong}
            begin
            temp := offset1;
            settemp(long, immediatelong, 0, 0, false,
                    temp mod 65536, temp div 65536,
                    1, unknown);
            temp := offset;
            settemp(long, immediatelong, 0, 0, false,
                    temp mod 65536, temp div 65536,
                    1, unknown);
            source := tempkey;
            end;

        if pushing(dst) then
          begin
          settemp(long, autod, sp, 0, false, 0, 0, 1, unknown);
          gen2(move, long, source, tempkey);
          gen2(move, long, source + 1, tempkey);
          stackoffset := -keytable[stackcounter].oprnd.offset;
          end
        else
          with keytable[dst].oprnd do
            begin
            forcerelative(dst, false, true, long, false);
            settemp(long, m, reg, indxr, indxlong, offset, offset1, scale,
                    commonlong_reloc);
            gen2(move, long, source + 1, tempkey);
            keytable[tempkey].oprnd.offset :=
              keytable[tempkey].oprnd.offset + long;
            gen2(move, long, source, tempkey);
            end;
        end
    else if not equivaddr(src, dst) then
      begin
      oldlen := keytable[src].len;
      keytable[src].len := length;
      initloop(src, dst, dst, maxint, 4, {long, minpiecesize,} loop, pieces);
      onlyreference(loopsrc);
      onlyreference(loopdst);
      for i := 1 to pieces do
        begin
        gen2(move, piecesize, loopsrc, loopdst);
        bumploop(dbra, loop);
        end;
      finishloop;
      keytable[src].len := oldlen;
      end;
  end {genblockmove} ;


{ Reference count book-keeping
}


procedure dereference(k: keyindex {operand} );

{ Reduce all appropriate reference counts for this key.  This is called
  when a particular reference is completed.
}


  begin
    if k > 0 then
      begin
      with keytable[k] do
        begin
        if refcount = 0 then
          begin
          write('DEREFERENCE, refcount < 0');
          compilerabort(inconsistent);
          end;

        refcount := refcount - 1;
        end;
      bumptempcount(k, - 1);
      adjustregcount(k, - 1);
      end;
  end {dereference} ;


procedure derefboth;

{ Reduce the reference counts on the global left and right operands.
  This is called after generating a binary operation.
}


  begin
    dereference(left);
    dereference(right);
  end {derefboth} ;


procedure rereference(k: keyindex {operand} );

{ Increase all appropriate reference counts for this key.  This is called
  when we need to compensate for an extra dereference.
}


  begin {rereference}
    if k > 0 then
      begin
      with keytable[k] do refcount := refcount + 1;
      adjustregcount(k, 1);
      bumptempcount(k, 1);
      end;
  end {rereference} ;

procedure makeaddressable(var k: keyindex);

{ Force addressability of specified key. Also permanently makes the new
  address mode available subject to restrictions of allowmodify. A key
  becomes unaddressed when one of the mark routines clears regvalid or
  indxrvalid. Makeaddressable reloads the missing register(s) and clears
  the marked reg/indxr status.
}

  var
    restorereg, restoreindxr: boolean;
    t: keyindex;


  procedure recall_areg(regx: regindex; properregx: keyindex);

    { Unkill an areg if possible.
    }
    begin
      with loopstack[loopsp] do
        if (thecontext = contextsp) and (loopoverflow = 0) and
           (thecontext <> contextdepth - 1) and
           (aregstate[regx].stackcopy = properregx) then
          aregstate[regx].killed := false;
    end;


  procedure recall_dreg(regx: regindex; properregx: keyindex);

    { Unkill a dreg if possible.
    }
    begin
      with loopstack[loopsp] do
        if (thecontext = contextsp) and (loopoverflow = 0) and
           (thecontext <> contextdepth - 1) and
           (dregstate[regx].stackcopy = properregx) then
          dregstate[regx].killed := false;
    end;


  procedure recall_fpreg(regx: regindex; properregx: keyindex);

    { Unkill a fpreg if possible.
    }
    begin
      with loopstack[loopsp] do
        if (thecontext = contextsp) and (loopoverflow = 0) and
           (thecontext <> contextdepth - 1) and
           (fpregstate[regx].stackcopy = properregx) then
          fpregstate[regx].killed := false;
    end;


  begin {makeaddressable}
    with keytable[k], oprnd do
      begin
      restorereg := not regvalid;
      restoreindxr := not indxrvalid;
      if restorereg then keytable[properreg].tempflag := true;
      if restoreindxr then keytable[properindxr].tempflag := true;
      end;
    if restorereg or restoreindxr then allowmodify(k, false);
    with keytable[k], oprnd do
      begin
      adjustregcount(k, - refcount);
      case m of
        areg, indr, autoi, autod, relative:
          if restorereg then
            begin
            reg := getareg;
            recall_areg(reg, properreg);
            settempareg(reg);
            gensimplemove(properreg, tempkey);
            end;
        fpreg, twofpregs:
          begin
          if restorereg then
            begin
            reg := getfpreg;
            recall_fpreg(reg, properreg);
            settempfpreg(reg);
            genfpmove(properreg, tempkey);
            end;

          if (m = twofpregs) and restoreindxr then
            begin
            fpregisters[reg] := fpregisters[reg] + 1000;
            indxr := getfpreg;
            recall_fpreg(indxr, properindxr);
            fpregisters[reg] := fpregisters[reg] - 1000;
            settempfpreg(indxr);
            genfpmove(properindxr, tempkey);
            end;
          end;

        dreg, twodregs:
          begin
          if restorereg then
            begin
            reg := getdreg;
            recall_dreg(reg, properreg);
            settempdreg(keytable[properreg].len, reg);
            gensimplemove(properreg, tempkey);
            end;
          if (m = twodregs) and restoreindxr then
            begin
            dregisters[reg] := dregisters[reg] + 1000;
            indxr := getdreg;
            recall_dreg(indxr, properindxr);
            dregisters[reg] := dregisters[reg] - 1000;
            settempdreg(keytable[properindxr].len, indxr);
            gensimplemove(properindxr, tempkey);
            end;
          end;

        pcindexed, indexed, bitindexed:
          begin
          if restorereg then
            begin
            reg := getareg;
            recall_areg(reg, properreg);
            settempareg(reg);
            gensimplemove(properreg, tempkey);
            end;
          if restoreindxr then
            begin
            indxr := getdreg;
            recall_dreg(indxr, properindxr);
            settempdreg(keytable[properindxr].len, indxr);
            gensimplemove(properindxr, tempkey);
            end;
          end;
        otherwise
        end;

      regvalid := true;
      indxrvalid := true;
      joinreg := false;
      joinindxr := false;
      regsaved := regsaved and keytable[properreg].validtemp;
      indxrsaved := indxrsaved and keytable[properindxr].validtemp;
      adjustregcount(k, refcount);
      end;
  end {makeaddressable} ;


procedure address(var k: keyindex);

{ Shorthand concatenation of a dereference and makeaddressable call }


  begin
    dereference(k);
    makeaddressable(k);
  end {address} ;


procedure addressboth;

 { address both operands of a binary pseudoop }


  begin
    address(right);
    lock(right);
    address(left);
    unlock(right);
  end {addressboth} ;


{ General register operand loading routines.
}


function stackcovers: boolean;

{ True if the right operand is the top temp on the stack and the left operand
  is the second temp on the stack.  The operands are taken from the globals
  "left" and "right".
}


  begin
    stackcovers := equivaddr(stackcounter + 1, left) and equivaddr(right,
                   stackcounter);
  end {stackcovers} ;


function loadeddreg(k: keyindex; {operand to check}
                    regneeded: boolean {must be in a register} ): boolean;

{ True if the operand specified is loaded in a volatile location.  If
  "regneeded" is set, this must be a data register.  In any case, it must be
  available for modification.
}


  begin
    with keytable[k], oprnd do
      loadeddreg := (access = valueaccess) and (target <> 0) and
                    (equivaccess(k, target) and
                    (keytable[key].refcount <= 1) and (not regneeded or
                    (m = dreg))) or ((m = dreg) and (reg <= lastdreg) and
                    (dregisters[reg] + ord(context[contextsp].dbump[reg]) <=
                    countdreg));
  end {loadeddreg} ;


procedure loaddreg(src: keyindex; {operand to load}
                   other: keyindex; {other operand to avoid}
                   regneeded: boolean {set if must be in register} );

{ Load "src", which must be an integer of some sort, into a modifiable
  location.  If "regneeded" is set, this must be a register.  "Other" is
  provided so that the other operand of a binary operation is not disturbed.

  If possible, the operand will be loaded into the target.
  The entry for the global "key" is set to the results of the load.
}

  var
    mayusetarget: boolean;

  begin
    lock(other);
    with keytable[src], oprnd do
      if loadeddreg(src, regneeded) then
        begin {already loaded}
        if m = dreg then markdreg(reg);
        setkeyvalue(src);
        end
      else
        begin {not loaded, find the best place}

        {see if we can load it into the target}

        if other > 0 then
          mayusetarget := not equivaccess(other, target)
        else mayusetarget := true;

        if (keytable[key].refcount = 1) and (keytable[target].len = len) and
           { (keytable[target].refcount <= 1) and }
           (target > 0) and mayusetarget and
           (target <= lastkey) and not keytable[target].packedaccess and
           (not regneeded or (keytable[target].oprnd.m = dreg)) then
          with keytable[target], oprnd do
            begin
            if m = dreg then markdreg(reg);
            lock(src);
            setallfields(target);
            makeaddressable(key);
            changevalue(target, key);
            unlock(src);
            end
        else
          begin {nothing special, just load it}
          tempkey := tempkey - 1;
          keytable[tempkey] := keytable[src];
          src := tempkey;
          setvalue(dreg, getdreg, 0, false, 0, 0);
          keytable[key].signed := keytable[src].signed;
          end;
        if keytable[src].oprnd.m = fpreg then genfpmove(src, key)
        else gensimplemove(src, key);
        end;
    unlock(other);
  end {loaddreg} ;

function loadedfpreg(k: keyindex; {operand to check}
                     regneeded: boolean {must be in a register}): boolean;

{ True if the operand specified is loaded in a volatile location.  If
  "regneeded" is set, this must be an f.p. register.  In any case, it must be
  available for modification.
}


  begin
    with keytable[k], oprnd do
      loadedfpreg := (access = valueaccess) and (target <> 0) and
                     (equivaccess(k, target) and
                     (keytable[key].refcount <= 1) and (not regneeded or
                     (m = fpreg))) or ((m = fpreg) and (reg <= lastfpreg) and
                     (fpregisters[reg] + ord(context[contextsp].fpbump[reg]) <=
                     countfpreg));
  end {loadedfpreg} ;


procedure loadfpreg(src: keyindex; {operand to load}
                    other: keyindex; {other operand to avoid}
                    regneeded: boolean {set if must be in register});

{ Load "src", which must be a single or double real, into a modifiable
  location.  If "regneeded" is set, this must be a register.  "Other" is
  provided so that the other operand of a binary operation is not disturbed.

  If possible, the operand will be loaded into the target.
  The entry for the global "key" is set to the results of the load.
}


  begin
    lock(other);
    with keytable[src], oprnd do
      if loadedfpreg(src, regneeded) then
        begin {already loaded}
        if m = fpreg then markfpreg(reg);
        setkeyvalue(src);
        end
      else
        begin {not loaded, find the best place}

        {see if we can load it into the target}

        if (keytable[key].refcount = 1) and (keytable[target].len = len) and
           { (keytable[target].refcount <= 1) and }
           (target > 0) and
           (target <= lastkey) and not keytable[target].packedaccess and
           (not regneeded or (keytable[target].oprnd.m = fpreg)) then
          with keytable[target], oprnd do
            begin
            if m = fpreg then markfpreg(reg);
            setkeyvalue(target);
            end
        else
          begin {nothing special, just load it}
          tempkey := tempkey - 1;
          keytable[tempkey] := keytable[src];
          src := tempkey;
          setvalue(fpreg, getfpreg, 0, false, 0, 0);
          end;
        genfpmove(src, key);
        end;
    unlock(other);
  end {loadfpreg} ;


function loadedareg(k: keyindex; {operand to check}
                    regneeded: boolean {must be in a register}) : boolean;

{ True if the operand specified is loaded in a volatile location.  If
  "regneeded" is set, this must be an address register.  In any case,
  it must be available for modification.
}


  begin
    with keytable[k], oprnd do
      loadedareg := (access = valueaccess) and (target <> 0) and
                    (equivaccess(k, target) and
                    (keytable[key].refcount <= 1) and (not regneeded or
                    (m = areg))) or ((m = areg) and bestareg(reg));
  end {loadedareg} ;


procedure loadareg(src: keyindex; {operand to load}
                   other: keyindex; {other operand to avoid}
                   regneeded: boolean {set if must be in register} );

{ Load "src", which must be a pointer of some sort, into a modifiable
  location.  If "regneeded" is set, this must be a register.  "Other" is
  provided so that the other operand of a binary operation is not disturbed.

  If possible, the operand will be loaded into the target.
  The entry for the global "key" is set to the results of the load.
}


  begin
    lock(other);
    with keytable[src], oprnd do
      if loadedareg(src, regneeded) then
        begin {already loaded}
        if m = areg then markareg(reg);
        setkeyvalue(src);
        end
      else
        begin {not loaded, find the best place}

        {see if we can load it into the target}

        if (keytable[key].refcount = 1) and
           (keytable[target].refcount <= 1) and (target > 0) and
           (target <= lastkey) and not keytable[target].packedaccess and
           (not regneeded or (keytable[target].oprnd.m = areg)) then
          with keytable[target], oprnd do
            begin
            markareg(reg);
            setkeyvalue(target);
            end
        else
          begin {nothing special, just load it}
          tempkey := tempkey - 1;
          keytable[tempkey] := keytable[src];
          src := tempkey;
          setvalue(areg, getareg, 0, false, 0, 0);
          end;
        gensimplemove(src, key);
        end;
    unlock(other);
  end {loadareg} ;


{ Set loading routines }


function loadedset(k: keyindex {operand to check} ): boolean;

{ True if "k" (which is assumed to be a set) is loaded in a
  modifiable location.
}


  begin
    with keytable[k], oprnd do
      loadedset := equivaccess(k, target) and (keytable[key].refcount <= 1) or
                   (m = relative) and (reg = sp) and
                   (keytable[properreg].refcount = 0);
  end {loadedset} ;


procedure loadset(src: keyindex {Operand to load} );

{ Get a set operand in a modifiable location,  either the target
  or in a temp on the stack.

  The entry for the global "key" is set to reflect this location.
}


  begin
    settargetused := false;

    with keytable[src], oprnd do
      if loadedset(src) then setallfields(src)
      else if (target > 0) then
        begin
        setallfields(target);
        settargetused := true;
        end
      else
        begin
        newtemp(len);
        keytable[stackcounter].tempflag := true;
        keytable[key].properreg := stackcounter;
        keytable[key].regsaved := true;
        setkeyvalue(stackcounter);
        end;
    savekey(key);
  end {loadset} ;



procedure clearsp(n: integer {words to clear} );

{ Allocate and clear "n" words on the top of the stack.
}

  var
    t: integer; {induction variable}


  begin
    aligntemps;

    for t := n div 2 downto 1 do
      begin
      newtemp(long);
      gensingle(clr, stackcounter);
      end;
    if odd(n) then
      begin
      newtemp(word);
      gensingle(clr, stackcounter);
      end;
  end {clearsp} ;


procedure shiftit(inst: insttype; {shift instruction to use}
                  len: integer; {byte, word, long}
                  amount: integer; {literal number of bits to shift}
                  k: keyindex {who to shift} );

{ Shift "k" by a literal amount.  Very simple other than the screwy
  max shift amount of 8 per shift instruction imposed by architecture.
}

  var
    i: 0..3; {induction var for shifting in chunks of 8}


  begin
    settempimmediate(word, 8);
    for i := 1 to amount div 8 do gen2(inst, len, tempkey, k);
    settempimmediate(word, amount mod 8);
    if amount mod 8 <> 0 then gen2(inst, len, tempkey, k);
    tempkey := tempkey + 2;
  end {shiftit} ;


procedure extend(var k: keyindex; {operand to be sign extended}
                 newlen: addressrange {desired length} );

{ Extend an operand to "newlen", with the proper sign.  In this
  implementation, newlen <= long.

  This is divided into two cases, for signed and unsigned operands, and
  the two are handled almost independently.  This is not too obvious from
  the code, where things are rather interwoven.
}

  var
    newkey: keyindex;
    newreg: regindex;
    newoffset1: integer;

  begin
    if (keytable[k].len < newlen) then
      begin
      with keytable[k], oprnd do
        if m = immediate then
          if newlen = long then
            begin
            if offset >= 0 then newoffset1 := 0
            else newoffset1 := - 1;
            settemp(long, immediatelong, 0, 0, false, offset, newoffset1, 1,
                    unknown);
            end
          else settemp(word, immediate, 0, 0, false, offset, 0, 1, unknown)
        else
          begin
          { If it's not in a dreg already we can't allocate more bytes so
            we must stuff it into a new dreg.  Also if the high word contains
            the remainder from a divide, we must use another register.
          }

          if (m <> dreg) or (high_word_dirty and
             (dregisters[reg] + ord(context[contextsp].dbump[reg]) > 0)) then
            begin
            lock(k);
            newreg := getdreg;
            unlock(k);
            end
          else newreg := reg;

          settempdreg(long, newreg);

          { If we allocated a new register for an unsigned value, it's cheaper
            to clear it first.
          }

          if (newreg <> reg) and not signed then gen1(clr, long, tempkey);

          gensimplemove(k, tempkey);

          if signed then
            if (len = byte) and (newlen = long) and mc68020
            then gen1(extb, long, tempkey) { EXTB goes from byte to long }
            else
              begin
              if len = byte then gen1(ext, word, tempkey);
              if newlen = long then gen1(ext, long, tempkey);
              end
          else if newreg = reg then { If not already fixed up above }
            begin
            if len = byte then settempimmediate(long, $FF)
            else settempimmediate(long, $FFFF);
            gendouble(andinst, tempkey, tempkey + 1);
            tempkey := tempkey + 1;
            end;
          end;
      newkey := tempkey;
      allowmodify(k, dontchangevalue > 0);
      keytable[k].len := newlen;
      keytable[k].signed := true; {no longer crammed-full operand}
      changevalue(k, newkey);
      if newlen = long then keytable[k].high_word_dirty := false;
      end;
  end {extend} ;


procedure shrink(var k: keyindex; {item to shrink}
                 newlen: integer {size to squeeze to} );

{ Reference small piece of long or word sized item.  Case only arises
  during assignment to a packed record where the target is smaller than
  the source.  Since byte zero of a word or long is the most significant
  byte, we need to do some fiddling to access the least significant byte
  of the item.
}

  var
    keep_on_stack: boolean; {there is a stack copy - remember it}


  begin {shrink}
    if (keytable[k].len > newlen) and
       not (keytable[k].oprnd.m in [dreg, areg]) then
      if keytable[k].oprnd.m in [immediate, immediatelong, relative, indexed,
                                 pcrelative, pcindexed, bitindexed, abslong,
                                 commonlong]
         then
        begin
        with keytable[k], oprnd do
          keep_on_stack := (m = relative) and (reg = sp) and regsaved;
        allowmodify(k, true);
        with keytable[k], oprnd do
          if m = immediatelong then
            begin
            if newlen <= word then m := immediate;
            end
          else if m <> immediate then
            begin
            if not regsaved then regsaved := keep_on_stack;
            forcerelative(k, false, true, len - newlen, false);
            keytable[k].oprnd.offset := keytable[k].oprnd.offset + len -
                                          newlen;
            keytable[k].knowneven :=
              mc68020 or not odd(keytable[k].oprnd.offset);
            end;
        keytable[k].len := newlen;
        if newlen <= keytable[k].signlimit then keytable[k].signed := false;
        end
      else
        begin
        settempdreg(newlen, getdreg);
        gensimplemove(k, tempkey);
        changevalue(k, tempkey);
        keytable[k].len := newlen;
        if newlen <= keytable[k].signlimit then keytable[k].signed := false;
        end;
  end {shrink} ;



procedure unpack(var k: keyindex; {operand to unpack}
                 finallen: integer {length desired} );

{ Make a source reference to a possibly packed operand.  If the operand
  is not packed, it is simply addressed.  If it is packed, code is
  generated to make an unpacked copy of the operand.  If there are further
  references to this unpacked field it is saved.

  Note: this code is currently implemented for 16-bit unpacking only.
}

  var
    newvalue: keyindex; {contains description of unpacked result}
    piecesize: integer; {how big a chunk to pick out before unpacking}
    piece: keyindex; {describes the byte/word chunk}
    resultsigned: boolean; {the resulting value can be treated as signed}
    offset_key: keyindex;


  begin {unpack}
    address(k);
    if keytable[k].packedaccess then
      begin
      if mc68020 then
        begin
        if keytable[k].oprnd.m = indexed then
          forcerelative(k, false, true, 0, false);
        lock(k);

        { See if our caller has a preference about where we put our result.
        }
        if use_preferred_key and (keytable[preferred_key].oprnd.m = dreg) then
          newvalue := preferred_key
        else if not bftst_needed then
          begin
          settempdreg(long, getdreg);
          newvalue := tempkey;
          end;

        use_preferred_key := false;

        with keytable[k], oprnd do
          begin
          resultsigned := signed or (len < long * bitsperbyte);
          if m = bitindexed then
            begin
            if offset1 <> 0 then
              { This handles the case where a packed array does not
                begin on an addressable boundary.  i.e. a packed record
                containing a 2 bit integer and an array of 2 bit integers.
              }
              begin
              settempimmediate(long, offset1);
              settempdreg(len, indxr);
              gendouble(add, tempkey + 1, tempkey);
              end
            else settempdreg(len, indxr);
            offset_key := tempkey;
            settemp(len, relative, reg, 0, false, offset, offset1, scale, 
                    unknown);
            end
          else if (m in [relative, indexed]) then
            begin
            settemp(len, bit_field_const, 0, 0, false, 0,
                    offset1 mod bitsperbyte, 1, unknown);
            offset_key := tempkey;
            settemp(len, m, reg, indxr, indxlong,
                    offset + offset1 div bitsperbyte, 0, scale,
                    commonlong_reloc);
            end
          else
            begin
            settemp(len, bit_field_const, 0, 0, false, 0, offset1, 1, unknown);
            offset_key := tempkey;
            settemp(len, m, reg, indxr, false, offset, 0, scale,
                    commonlong_reloc);
            end;
          if bftst_needed then
            gen_bf_inst(bftst, len, lowesttemp - 1, tempkey, offset_key)
          else if signed then
            gen_bf_inst(bfexts, len, tempkey, newvalue, offset_key)
          else gen_bf_inst(bfextu, len, tempkey, newvalue, offset_key);
          end {with} ;
        unlock(k);

        if not bftst_needed then
          begin
          allowmodify(k, dontchangevalue > 0);
          keytable[k].len := long;
          changevalue(k, newvalue);
          keytable[k].signed := resultsigned;
          keytable[k].len := long; { BFEXT gives 32-bit result }
          end;
        end
      else {not mc68020}
        begin
        lock(k);

        { See if our caller has a preference about where we put our result.
        }
        if use_preferred_key and (keytable[preferred_key].oprnd.m = dreg) then
          newvalue := preferred_key
        else
          begin
          settempdreg(long, getdreg);
          newvalue := tempkey;
          end;

        use_preferred_key := false;

        with keytable[k], oprnd do
          begin
          keytable[tempkey].signed := signed;
          piecesize := roundbytes((offset1 + len - 1) div 8 + 1);

          resultsigned := signed or (len < max(finallen, piecesize) *
                          bitsperbyte);

          if not signed and (piecesize < finallen) then
            gen1(clr, long, newvalue);
          if m = bitindexed then
            begin
            settemp(piecesize, relative, reg, 0, false, offset, 0, 1, unknown);
            piece := tempkey;
            end
          else piece := k;

          { Handle a word on an odd byte boundary if the front end ever gives
            us one.
          }
          if odd(keytable[piece].oprnd.offset) and (piecesize = word) then
            begin
            gen2(move, byte, piece, newvalue);
            shiftit(lsl, word, 8, newvalue);
            keytable[piece].oprnd.offset := keytable[piece].oprnd.offset + 1;
            gen2(move, byte, piece, newvalue);
            keytable[piece].oprnd.offset := keytable[piece].oprnd.offset - 1;
            end
          else gen2(move, piecesize, piece, newvalue);

          if m = bitindexed then
            begin
            settempdreg(piecesize, indxr);
            gen2(lsl, piecesize, tempkey, newvalue);
            end;
          shiftit(lsl, piecesize, offset1, newvalue);
          if signed then shiftit(asr, piecesize, piecesize * 8 - len, newvalue)
          else shiftit(lsr, piecesize, piecesize * 8 - len, newvalue);
          if signed and (piecesize < finallen) then
            begin
            if piecesize = 1 then gen1(ext, word, newvalue);
            if finallen = long then gen1(ext, long, newvalue);
            end;
          end;
        unlock(k);
        allowmodify(k, dontchangevalue > 0);
        keytable[k].len := max(finallen, piecesize);
        keytable[k].signed := resultsigned;
        changevalue(k, newvalue);
        end
      end
    else {not packedaccess}
      begin
      use_preferred_key := false;
      extend(k, finallen);
      end;

    bftst_needed := false;
  end {unpack} ;


procedure unpackboth(len: integer {desired length} );

{ Unpack both operands (from the globals "l" and "r")
}


  begin
    unpack(left, len);
    lock(left);
    unpack(right, len);
    unlock(left);
  end {unpackboth} ;


procedure shrinkboth(len: integer {desired length} );

{ Shrink both operands (from the globals "left" and "right").
}


  begin
    shrink(left, len);
    lock(left);
    shrink(right, len);
    unlock(left);
  end {shrinkboth} ;

procedure unpackshrink(var k: keyindex; {keytable reference}
                       len: integer {desired length} );

{ shrink both operands (from the globals "left" and "right")
}


  begin
    unpack(k, len);
    shrink(k, len);
  end {unpackshrink} ;

procedure unpkshkboth(len: integer {desired length} );

{ shrink both operands (from the globals "left" and "right")
}


  begin
    unpack(left, len);
    shrink(left, len);
    lock(left);
    unpack(right, len);
    shrink(right, len);
    unlock(left);
  end {unpkshkboth} ;



procedure pack(packedleft: boolean);

{ Packs the operand in the global "right" into the global "left".
}

  var
    shiftpart: keyindex; {contains packed array bitshift value}
    mask: unsigned; {mask values built here}
    i: integer; {induction var}
    shortcut: boolean; {true if "right" is packed but needn't be unpacked}
    rshift: integer; {if shortcut, amount "right" is shifted}
    oldrlen: integer; {if shortcut, stores temporarily modified length of
                       "right"}
    piecesize: integer; {size of unpacked "chunk"}
    piecekey: keyindex; {defines the new piece to be manipulated}
    packedarray: boolean; {true if target (left) is packed array }
    offset_key, src_key: keyindex;
    boolcnst: boolean; { 68020 only; true if packedarray of booleans is set
                         via constant }
    m020: boolean; { if true generate code for the mc68020 }


  begin {pack}
    m020 := mc68020 and not packedleft;

    if equivaccess(left, right) then derefboth
    else
      begin
      shortcut := false;
      address(left);

      packedarray := keytable[left].oprnd.m = bitindexed;

      if not m020 and packedarray then
        with keytable[left], oprnd do
          begin
          settemp(len, relative, reg, 0, false, offset, offset1, 1, unknown);
          left := tempkey;
          settempreg(word, dreg, indxr);
          shiftpart := tempkey;
          lock(shiftpart);
          end;
      lock(left);

      with keytable[left], oprnd do
        begin
        piecesize := roundbytes(max((len + offset1 - 1) div 8 + 1,
          (keytable[right].len + keytable[right].oprnd.offset1 - 1) div 8 + 1));
        if packedleft then rshift := (piecesize - keytable[right].len) * 8
         else rshift := (piecesize * 8) - len;

        if not m020 and not packedarray and
           (keytable[right].oprnd.m <> bitindexed) and
           keytable[right].packedaccess and (len <= keytable[right].len) and
           (roundbytes((len + offset1 - 1) div 8 + 1) = roundbytes((
            keytable[right].len + keytable[right].oprnd.offset1 - 1) div 8 + 1))
        then
          begin
          address(right);
          oldrlen := keytable[right].len;
          with keytable[right] do
            begin
            len := piecesize;
            rshift := oprnd.offset1;
            end;
          shortcut := true;
          end
        else unpack(right, piecesize);

        mask := 0;
        for i := 1 to len do mask := (mask div 2) or ( - maxint - 1);
        for i := 1 to (hostintsize - piecesize) * 8 + offset1 do
          mask := mask div 2;

        if keytable[right].oprnd.m = immediate then
          begin
          with keytable[right], oprnd do
            settemp(len, m, reg, indxr, indxlong, offset, offset1, scale,
                    commonlong_reloc);
          right := tempkey;

          { Restrict this for the 68020.  We need the unmodified value, except
            in the case where and/or can be used to insert a constant.
          }
          if not m020 or (not packedarray and
            (keytable[right].oprnd.m = immediate)) then
            begin
            for i := 1 to piecesize * 8 - offset1 - len do
              keytable[right].oprnd.offset := keytable[right].oprnd.offset * 2;
            keytable[right].oprnd.offset := keytable[right].oprnd.offset and
                                            mask;
            end;
          end;

        if (not m020 and packedarray and (keytable[right].oprnd.offset <> 0))
           or (keytable[right].oprnd.m <> immediate) then
          begin
          keytable[key].len := min(hostintsize, keytable[right].len); { hack! }
          loaddreg(right, 0, true);
          piecekey := key;
          end
        else
          begin
          piecekey := right;
          keytable[piecekey].len := piecesize;
          end;

        allowmodify(piecekey, dontchangevalue > 1);

        if m020 then
          begin
          lock(piecekey);

          boolcnst := (keytable[left].len = 1) and
                      (keytable[right].oprnd.m = immediate); {packedarray only}

          if not packedarray and (keytable[piecekey].oprnd.m = immediate) then
            begin
            if keytable[piecekey].oprnd.offset and mask <> mask then
              begin
              if piecesize = long then settempimmediate(long, not mask)
              else
                begin
                if not packedarray and (piecesize = 1) then
                  settempimmediate(word, (not mask) and 255)
                else settempimmediate(word, (not mask) and 65535);
                end;
              gen2(andinst, piecesize, tempkey, left);
              end;
            if keytable[piecekey].oprnd.offset <> 0 then
              gen2(orinst, piecesize, piecekey, left);
            end
          else
            begin {we can't use and/or}
            if m = indexed then
              forcerelative(left, false, true, 0, false); {destination operand}

            if not boolcnst and (keytable[piecekey].oprnd.m <> dreg) then
              begin
              settempdreg(keytable[piecekey].len, getdreg);
              gensimplemove(piecekey, tempkey);
              src_key := tempkey;
              end
            else src_key := piecekey;

            { We have blown our enclosing with statement if we did
              the above forcerelative on left, so here's a new one.
            }
            with keytable[left], oprnd do
              begin
              if m = bitindexed then
                begin
                if offset1 <> 0 then
                  { This handles the case where a packed array does not
                    begin on an addressable boundary.  i.e. a packed record
                    containing a 2 bit integer and an array of 2 bit integers.
                  }
                  begin
                  settempimmediate(long, offset1);
                  settempdreg(len, indxr);
                  gendouble(add, tempkey + 1, tempkey);
                  end
                else settempdreg(len, indxr);
                offset_key := tempkey;
                settemp(len, relative, reg, 0, false, offset, offset1, 1,
                        unknown);
                end
              else if (m in [relative, indexed]) then
                begin
                settemp(long, bit_field_const, 0, 0, false, 0,
                        offset1 mod bitsperbyte, 1, unknown);
                offset_key := tempkey;
                settemp(len, m, reg, indxr, indxlong,
                        offset + offset1 div bitsperbyte, 0, scale,
                        commonlong_reloc);
                end
              else
                begin
                settemp(long, bit_field_const, 0, 0, false, 0, offset1, 1,
                        unknown);
                offset_key := tempkey;
                settemp(len, m, reg, indxr, false, offset, 0, scale,
                        commonlong_reloc);
                end;
              end {with} ;

            { If field is 1 bit wide and set to 1 use BFSET, if it's an
              immediate constant of any width and zero, use BFCLR, else
              load value with BFINS.
            }
            if boolcnst and (keytable[right].oprnd.offset <> 0) then
              gen_bf_inst(bfset, keytable[left].len, lowesttemp - 1, tempkey,
                          offset_key)
            else if (keytable[right].oprnd.offset = 0) and
                    (keytable[right].oprnd.m = immediate) then
              gen_bf_inst(bfclr, keytable[left].len, lowesttemp - 1,
                          tempkey, offset_key)
            else gen_bf_inst(bfins, keytable[left].len, src_key, tempkey,
                             offset_key);
            end;
          unlock(piecekey);
          end
        else { not m020 }
          begin
          lock(piecekey);

          if (keytable[right].oprnd.m <> immediate) or
             (keytable[right].oprnd.offset and mask <> mask) then
            begin
            if piecesize = long then settempimmediate(long, not mask)
            else
              begin
              if not packedarray and (piecesize = 1) then
                settempimmediate(word, (not mask) and 255)
              else settempimmediate(word, (not mask) and 65535);
              end;
            if packedarray then
              begin
              settempreg(word, dreg, getdreg);
              gendouble(move, tempkey + 1, tempkey);
              gendouble(ror, shiftpart, tempkey);
              end;
            gen2(andinst, piecesize, tempkey, left);
            end;
  
          if keytable[right].oprnd.m <> immediate then
            begin
            mask := 0;
            for i := 1 to len do mask := (mask div 2) or ( - maxint - 1);
            for i := 1 to (hostintsize - piecesize) * 8 + rshift do
              mask := mask div 2;
            if piecesize = long then settempimmediate(long, mask)
            else
              begin
              if piecesize = byte then mask := mask and 255;
              settempimmediate(word, mask and 65535);
              end;
            if shortcut or signed then
              gen2(andinst, piecesize, tempkey, piecekey);
            if rshift < offset1 then
              shiftit(lsr, piecesize, offset1 - rshift, piecekey)
            else shiftit(lsl, piecesize, rshift - offset1, piecekey);
            end;
  
          if (keytable[piecekey].oprnd.m <> immediate) or
             (keytable[piecekey].oprnd.offset <> 0) then
            begin
            if packedarray then gen2(lsr, piecesize, shiftpart, piecekey);
            gen2(orinst, piecesize, piecekey, left);
            end;
          unlock(piecekey);
          end;

        if shortcut then keytable[right].len := oldrlen;
        end;

      unlock(left);
      if not m020 and packedarray then unlock(shiftpart);
      end;
  end {pack} ;



procedure makestacktarget;

{ Create a slot on the stack for the current key
}


  begin {makestacktarget}
    if pseudobuff.op <> pshaddr then saveactivekeys;
    { ^^^ This is a hack to fix TR3845, where a register was saved in the
      middle of a parameter list for STR.  The real fix is to have the
      front-end pump out a makeroom for sysroutines.
    }
    newtemp(len);
    keytable[stackcounter].tempflag := true;
    keytable[key].regsaved := true;
    keytable[key].properreg := stackcounter;
    setkeyvalue(stackcounter);
  end {makestackstarget} ;


procedure forcebranch(k: keyindex; {operand to test}
                      newsignedbr: insttype; {branch to generate}
                      newunsignedbr: insttype {unless operand is unsigned} );

{ Force key "k" to a branch reference and dereference.  This is called
  when "k" is a boolean, or when any scalar field is being compared to zero.
  It leaves the key set to a "branchaccess" operand.
}

  var
    mask: unsigned; {mask values built here}
    piecesize: integer; {size of unpacked "chunk"}
    i: integer; {"for" index}
    newkey: keyindex;

  begin
    with keytable[k], oprnd do
      if access = valueaccess then
        begin
        if {((newsignedbr = bne) or (newsignedbr = beq)) and packedaccess and
           (m <> bitindexed) then}
          false then { To hell with it!  It causes too many problems }
          begin {compare in place without extraction}
          piecesize := roundbytes((len + offset1 - 1) div 8 + 1);
          mask := 0;

          for i := 1 to len do mask := (mask div 2) or ( - maxint - 1);

          for i := 1 to (hostintsize - piecesize) * 8 + offset1 do
            mask := mask div 2;

          settempimmediate(piecesize, mask);

          { If the key can't be destroyed, then make a copy.
          }
          if refcount > 1 then
            begin
            settempdreg(piecesize, getdreg);
            gensimplemove(k, tempkey);
            gen2(andinst, piecesize, tempkey + 1, tempkey);
            newkey := tempkey;
            end
          else
             begin
             gen2(andinst, piecesize, tempkey, k);
             newkey := k;
             end;

          dereference(k);
          end
        else
          begin
          bftst_needed := true;
          unpack(k, 1);
          newkey := k;
          end;

        { For 68020 the BFTST instruction is used for packed fields.
        }
        if not (mc68020 and packedaccess) then
          if (not mc68020 and (m in [pcrelative, pcindexed, immediate,
             immediatelong])) or
             (mc68020 and (m in [immediate, immediatelong])) then
               { "tst" does not have all the addressing modes, but the 68020
                 has PC-relative.
                }
            begin
            settempdreg(long, getdreg);
            gensimplemove(newkey, tempkey);
            end
          else gensingle(tst, newkey);

        if signed then setbr(newsignedbr)
        else setbr(newunsignedbr);
        end
      else {is already branch access}
        begin
        dereference(k);
        setbr(brinst);
        end;
  end {forcebranch} ;


{ Routines to access operands}


procedure dolevelx;

{ Generate a reference to the data area for the level specified in
  opernds[1].  This is the  static link in linkreg for the surrounding
  frame, or a single indirect reference through "target" for intermediate
  frames.  The necessary operand for this reference is set up by prior
  pseudo-ops. Note that the procedure 'dostaticlevels' handles global
  and local levels. Level 0 is a dummy which points to absolute indexes.
  This is used as the base for ORIGIN variables.
}


  begin
    address(target);
    if left = 0 then setvalue(abslong, 0, 0, false, 0, 0)
    else if left = level - 1 then setvalue(relative, sl, 0, false, 0, 0)
    else
      begin
      settempareg(getareg);
      gensimplemove(target, tempkey);
      setvalue(relative, keytable[tempkey].oprnd.reg, 0, false, 0, 0);
      end;
    keytable[key].knowneven := true;
  end {dolevelx} ;


procedure dostructx;

{ Set up an operand to access a structured constant (including strings).
  This is simply a reference to the string data area.
}


  begin {dostructx}
    if not mc68020 and pic_enabled then

      { This incredible hack is to allow > 32k byte references to constants
        for 68000 pic.  We fake up a short reference to the constant section
        using an LEA so that there is an areg to use later.  Fixaddressing will
        measure the distance and convert our LEA into a pair of instructions
        to compute the pointer in the old LEA's register.  This is a compromise
        of several very gross solutions and has the benefit of not requiring
        recompution of the pointer as refcounts are used on the register
        allocated for the LEA.
      }

      begin
      settempareg(getareg);
      settemp(long, pcrelative, 0, 0, false, pseudoinst.oprnds[1], 0, 1,
              unknown);
      gen2(lea, long, tempkey, tempkey + 1);
      setvalue(relative, keytable[tempkey + 1].oprnd.reg, 0, false, 0, 0);
      end
    else if (targetopsys = apollo) and not switcheverplus[sharecode] then
      begin
      setvalue(commonlong, 0, 0, false, pseudoinst.oprnds[1], 0);
      keytable[key].oprnd.commonlong_reloc := pure_section;
      end
    else setvalue(pcrelative, 0, 0, false, pseudoinst.oprnds[1], 0);

    keytable[key].knowneven := mc68020 or not odd(pseudoinst.oprnds[1]);
  end {dostructx} ;


procedure dovarx(s: boolean {signed variable reference} );

{ Defines the left operand as a variable reference and sets the
  result "key" to show this.

  This is used by the "dovar" and "dounsvar" pseudo-ops.
}


  begin
    setallfields(left);
    dereference(left);
    with keytable[key] do
      begin
      signed := s;
      if packedaccess then signlimit := (len - 1) div bitsperunit + 1
      else signlimit := len;
      end;
  end {dovarx} ;


procedure doextx;

{ Defines the left operand as a variable reference and sets the
  result "key" to show this.

  This is for "use", "define" and "shared" variables.
}


  var
    register: regindex;
    vtptr: vartablerecptr;


  begin
    vtptr := getvartableptr(pseudoinst.oprnds[1]);

    with vtptr^, keytable[key] do
      begin
      if pic_enabled and (extvaralloc = sharedalloc) then
        begin
        register := getareg;
        settempareg(register);
        settemp(long, relative, gp, 0, false, 4, 0, 1, unknown);
        gen2(move, long, tempkey, tempkey + 1);
        keytable[tempkey].oprnd.m := commonlong;
        keytable[tempkey].oprnd.offset := 0;
        keytable[tempkey].oprnd.commonlong_reloc := pseudoinst.oprnds[1];
        keytable[tempkey].oprnd.reg := register;
        gen2(add, long, tempkey, tempkey + 1);
        setvalue(relative, register, 0, false, 0, 0);
        end
      else { not (pic and sharedalloc) }
        begin
        oprnd.m := commonlong;
        oprnd.offset := 0;
        oprnd.commonlong_reloc := pseudoinst.oprnds[1];
        access := valueaccess;
        end;
      end;
  end {doextx} ;



procedure checkx(checkingrange: boolean; {true if range, not index check}
                 error: integer {error number} );

{Generate code for subrange and array bounds checking.  Value to be checked
 is in keytable[left], right and target contain the lower and upper
 bounds.  Rangechecking leaves the value unchanged, while indexchecking
 must leave the value with the lower bounds subtracted.
}

  var
    lower, upper: keyindex; {bounds}
    literalops: boolean; {literal operands}
    signedop: boolean; {set if operands are signed}
    upperoffset: integer; {upper range limit if literal}

  begin {checkx}
    lower := right;
    upper := target;
    target := 0; {to keep "loaddreg" from becoming confused}
    literalops := (keytable[lower].oprnd.m = immediate) and
                  (keytable[upper].oprnd.m = immediate);
    signedop := keytable[lower].signed and keytable[upper].signed;
    len := max(len, word);
    unpack(left, len);
    keytable[key].len := len;
    if (keytable[left].oprnd.m = dreg) and (checkingrange or
       literalops and (keytable[lower].oprnd.offset = 0) and
       ((len = long) or (keytable[upper].oprnd.offset <= 32767))) then
      setkeyvalue(left)
    else loaddreg(left, 0, true);
    keytable[key].knowneven := keytable[left].knowneven;
    if literalops then
      begin
      dereference(lower);
      dereference(upper);
      upperoffset := keytable[upper].oprnd.offset -
                     keytable[lower].oprnd.offset;

      if (upperoffset > 32767) and mc68020 then extend(key, long);

      gendouble(sub, lower, key);

      if (upperoffset > 32767) and mc68020 then
        begin
        settempimmediate(long, upperoffset);
        gendouble(chk, tempkey, key);
        end
      else if (upperoffset > 0) and (upperoffset <= 32767) and (len = word)
          then
        begin
        settempimmediate(word, upperoffset);
        gendouble(chk, tempkey, key)
        end
      else
        begin
        settempimmediate(len, upperoffset);
        gendouble(cmp, tempkey, key);
        genrelbr(bls, ord(switcheverplus[sharecode]) + 1);

        case targetopsys of
          unix, apollo:
            begin
            if checkingrange then callsupport(librangetrap)  { range error }
            else callsupport(libsubscripttrap);  { subscript error }
            end;
          vdos:
            begin
            settempimmediate(word, rangetrap);
            gensingle(trap, tempkey);
            end;
          end;
        labelnextnode := true;
        end;
      generror(error);
      if checkingrange then gendouble(add, lower, key);
      end
    else if (language = modula2) and (keytable[lower].oprnd.m = immediate) then
      begin { literal lower bound and a non-literal upper bound }
      lock(key);
      unpack(upper, len);
      gendouble(cmp, upper, key);
      genbr(bls, lastlabel);

      case targetopsys of
        unix, apollo:
          begin
          if checkingrange then callsupport(librangetrap)  { range error }
          else callsupport(libsubscripttrap);  { subscript error }
          end;
        vdos:
          begin
          settempimmediate(word, rangetrap);
          gensingle(trap, tempkey);
          end;
        end;

      definelastlabel;
      unlock(key);
      generror(index_error);
      labelnextnode := true;
      end
    else
      begin {non-literal bounds, do a direct check}
      lock(key);
      unpack(upper, len);
      gendouble(cmp, upper, key);
      if signedop then genbr(bgt, lastlabel)
      else genbr(bhi, lastlabel);
      unpack(lower, len);
      if checkingrange then gendouble(cmp, lower, key)
      else gendouble(sub, lower, key);
      if signedop then genrelbr(bge, ord(switcheverplus[sharecode]) + 1)
      else genrelbr(bhs, ord(switcheverplus[sharecode]) + 1);
      definelastlabel;

      case targetopsys of
        unix, apollo:
          begin
          if checkingrange then callsupport(librangetrap)  { range error }
          else callsupport(libsubscripttrap);  { subscript error }
          end;
        vdos:
          begin
          settempimmediate(word, rangetrap);
          gensingle(trap, tempkey);
          end;
        end;

      unlock(key);
      if checkingrange then generror(range_error)
      else generror(index_error);
      labelnextnode := true;
      end;
  end {checkx} ;



procedure ptrchkx;

{Generate code to valididate a pointer.  'new' routine arranges that
 the longword preceeding the returned heap entry contains the value
 of the pointer itself, i.e. that the comparision cmpa -4(ptr),ptr
 should always indicate equality.

 If this pointer check is followed by a definelazy op, we set our result
 to left, since definelazy needs to pass the address of the file variable,
 rather than the value.  Since we necessarily put the pointer in a register
 for the value also, we pass the number of the register that we assigned in
 "definelazyklugereg".  When definelazykluge is false the register value
 is ignored.
}

  var
    ptrkey: keyindex;   { Holds pointer }


  begin {ptrchkx}
    if pseudobuff.op <> definelazy then dereference(left);
    makeaddressable(left); 
	{ if makeaddressable is above dereference some cases unix/68, 
          maybe using regindrsaved, don't get dereferenced }

    if keytable[left].oprnd.m <> areg then
      begin
      settempareg(getareg);
      gensimplemove(left, tempkey);
      end
    else settempareg(keytable[left].oprnd.reg);
   
    ptrkey := tempkey;

    if pseudobuff.op = definelazy then
      begin
      dereference(left);
      setkeyvalue(left);
      definelazyklugereg := keytable[tempkey].oprnd.reg;
      definelazykluge := true;
      end
    else setkeyvalue(tempkey);

{
The above code was commented out and code similar to the Vax was used
when we were trying to solve the dereference problem mentioned above.
Both seem to work ok so we will keep the vax stuff for now.



    address(left);
    with keytable[left], oprnd do
      if m <> areg then begin
        if pseudobuff.op = definelazy then lock(left);
        settempareg(getareg);
        if pseudobuff.op = definelazy then unlock(left);
 	gensimplemove(left, tempkey);
        end
      else
	settempareg(reg);	
    ptrkey := tempkey;
    if pseudobuff.op = definelazy then begin
      setkeyvalue(left);
      definelazyklugereg := keytable[tempkey].oprnd.reg;
      definelazykluge := true;
      end
    else
      setkeyvalue(tempkey); 
}  
    
    settemp(long, relative, keytable[tempkey].oprnd.reg, 0, false, - long, 0,
            1, unknown);

    { A "feature" of the Masscomp's virtual memory hardware allows fetches
      from any address and returns a zero for "illegal" addresses.  Thus -4
      from zero (NIL) is zero and our usual pointer check fails to detect
      the NIL pointer.  The only solution, other than changing the nil pointer
      value to 1 which causes library problems, is to generate code to make
      sure that the pointer is not zero.
    }
    case targetopsys of
      unix:
        case unixtarget of
          masscomp:
            if language = modula2 then
              begin
              writeln('PTRCHK VS. MASSCOMP'); compilerabort(inconsistent);
              end
            else
              begin
              settempimmediate(long, 0);
              gendouble(cmp, tempkey, ptrkey);
              tempkey := tempkey + 1;
              genrelbr(beq, 2);  {branch to the pointer trap call}
              end;
          otherwise {do nothing}
          end;
      otherwise {do nothing}
      end;

    if language = modula2 then
      begin
      settempimmediate(long, 0);
      gen2(cmp, long, tempkey, ptrkey);
      genrelbr(bne, ord(switcheverplus[sharecode]) + 1);
      end
    else
      begin
      gen2(cmp, long, tempkey, ptrkey);
      genrelbr(beq, ord(switcheverplus[sharecode]) + 1);
      end;

    case targetopsys of
      unix, apollo:
        begin
        case unixtarget of
          masscomp:
            labelnextnode := true;
          otherwise {do nothing}
          end;
        callsupport(libpointertrap);  { pointer check }
        end;
      vdos:
        begin
        settempimmediate(word, ptrtrap);
        gensingle(trap, tempkey);
        end;
      end;

    labelnextnode := true;
  end {ptrchkx} ;



procedure indxindrx;

{ Do an indirect reference on a pointer value contained in oprnds[1] (left).
}


  begin {indxindrx}
    address(left);
    with keytable[left].oprnd do
      if m = areg then setvalue(relative, reg, 0, false, 0, 0)
      else
        begin
        settempareg(getareg);
        gensimplemove(left, tempkey);
        setvalue(relative, keytable[tempkey].oprnd.reg, 0, false, 0, 0);
        end;
    keytable[key].knowneven := mc68020;
  end {indxindrx} ;


procedure indxx;

{ Index the pointer reference in oprnds[1] (left) by the constant offset
  in oprnds[2].  The result ends up in "key".
}


  var
    reloc: commonlong_reloc_type;


  begin {indxx}
    address(left);

    with keytable[left], oprnd do
      if (m <> relative) and
         ((refcount > 1) or (pseudoinst.oprnds[2] <> 0)) or
         switcheverplus[sharecode] and
         (m = commonlong) then
        forcerelative(left, false, true, pseudoinst.oprnds[2], false);

    keytable[key].len := long;

    with keytable[left], oprnd do
      begin
      reloc := commonlong_reloc;

      if not (m in [commonlong, abslong])
         and (abs(offset + pseudoinst.oprnds[2]) > 32767) then

        { Versados always references the global section off of gp, unix
          may address it directly.
        }
	if (m = relative) and (reg = gp) and (targetopsys = unix) then
          begin
	  setvalue(commonlong, 0, 0, false, offset + pseudoinst.oprnds[2], 0);
          reloc := global_section;
          end
	else if not mc68020 then
          begin
          lock(left);
	  setvalue(areg, getareg, 0, false, 0, 0);
          unlock(left);
	  settemp(len, m, reg, indxr, indxlong, 0, offset1, scale,
                  commonlong_reloc);
	  gen2(lea, long, tempkey, key);
	  settempimmediate(long, pseudoinst.oprnds[2] + offset);
	  gendouble(add, tempkey, key);
	  keytable[key].oprnd.m := relative;
	  end
        else {mc68020 only -- generate long displacement}
          setvalue(m, reg, indxr, indxlong, offset + pseudoinst.oprnds[2], 
                   offset1)
      else
        setvalue(m, reg, indxr, indxlong, offset + pseudoinst.oprnds[2], 
                 offset1);
      keytable[key].oprnd.scale := scale;
      keytable[key].oprnd.commonlong_reloc := reloc;
      keytable[key].packedaccess := packedaccess;
      keytable[key].oprnd.offset1 := offset1;
      keytable[key].knowneven :=
        mc68020 or ((m in [commonlong, indexed, relative, abslong]) and
        knowneven and not odd(offset + pseudoinst.oprnds[2]));
      end;
  end {indxx} ;


procedure aindxx;

{ Generate code for an array access.  Oprnds[1] is a pointer to the base
  of the array and oprnds[2] is the index expression.  The index expression
  is adjusted by earlier passes for any offset changes due to the range
  check algorithm.  The result is a pointer to the array element.  The length
  field is the scale factor used by the 68020.
}


  begin {aindxx}
    address(left);
    lock(left);
    with keytable[right] do
      if not packedaccess and not signed and (len = word) then
        unpack(right, long)
      else unpack(right, word);
    unlock(left);
    lock(right);
    forcerelative(left, switcheverplus[sharecode], false, 0, true);
    unlock(right);

    if keytable[right].oprnd.m <> dreg then
      begin
      settempdreg(keytable[right].len, getdreg);
      gensimplemove(right, tempkey);
      changevalue(right, tempkey);
      end;

    with keytable[left], oprnd do
      begin
      setvalue(indexed, reg, keytable[right].oprnd.reg, 
               keytable[right].len = long, offset, 0);
      keytable[key].knowneven :=
        mc68020 or (knowneven and keytable[right].knowneven);
      end;

    keytable[key].len := long; {The common front end changed the length
                                because of implicit multiplies}
    keytable[key].oprnd.scale := len;
  end {aindxx} ;



procedure pindxx;

{ Index the address value in oprnds[1] (left) by the bit offset in oprnds[2]
  (right).  Note that the len field in this case is a bit length, not a word
  length.

  The result is left in "key".
}


  begin
    address(left);
    with keytable[left].oprnd do
      if offset1 + pseudoinst.oprnds[2] > 8 then
        forcerelative(left, false, true, 1, false);
    setkeyvalue(left);
    with keytable[key], oprnd do
      begin
      packedaccess := true;
      offset := offset + (offset1 + pseudoinst.oprnds[2]) div 8;
      offset1 := (offset1 + pseudoinst.oprnds[2]) mod 8;
      end;
  end {pindxx} ;


procedure paindxx;

{ Generate code for a packed array reference.  This generates a packed
  array access pointer which consists of a word or byte address and
  a bit address in consecutive registers.

  Oprnds[1] (left) contains the base address of the array, and oprnds[2]
  (right) contains the index expression.
}

  var
    shiftamount: integer; {holds log2(bitlength)}
    newoffset: integer; {general temp}
    i: integer; {FOR index}
    newdreg, newareg: regindex; {we will form bitindexed mode here}
    bitlen: datarange; {length in bits of indexed value}
    lea_needed: boolean; { needed for 68000 and noframe }


  begin {paindxx}
    lea_needed := false;
    bitlen := keytable[key].len;
    address(left);
    if keytable[left].oprnd.m = bitindexed then
      with keytable[left], oprnd do
        begin
        lock(left);
        unpack(right, word);
        unlock(left);
        settempdreg(word, indxr);
        if bestdreg(indxr) then setkeyvalue(left)
        else
          begin
          setvalue(m, reg, getdreg, false, offset, offset);
          keytable[key].oprnd.scale := scale;
          keytable[key].oprnd.commonlong_reloc := commonlong_reloc;
          settempdreg(word, keytable[key].oprnd.indxr);
          gensimplemove(tempkey + 1, tempkey);
          end;
        for i := 1 to bitlen {1, 2 or 4!} do gendouble(add, right, tempkey);
        end
    else if mc68020 then
      begin
      lock(right);
      forcerelative(left, true, false, 0, false);
      unlock(right);
      lock(left);
      unpack(right, long);

      with keytable[right],oprnd do
        if (m = dreg) and bestdreg(reg) then
          begin
          markdreg(reg);
          newdreg := reg;
          end
        else newdreg := getdreg;

      with keytable[left], oprnd do
        begin
        setvalue(bitindexed, reg, newdreg, indxlong, offset, offset1);
        keytable[key].signed := signed;
        end;

      unlock(left);
      newoffset := len;
      shiftamount := 0;

      { Determine if the multiplier is a power of two.
      }
      while not odd(newoffset) do
        begin
        shiftamount := shiftamount + 1;
        newoffset := newoffset div 2;
        end;

      settempdreg(long, newdreg);

      with keytable[right].oprnd do
        if not ((m = dreg) and (reg = newdreg)) then
          gensimplemove(right, tempkey);

      shiftit(lsl, long, shiftamount, tempkey);
      keytable[left].oprnd.scale := 1;
      end
    else { this is for the 68000 }
      begin
      lock(left);
      unpack(right, long);
      unlock(left);
      lock(right);
      forcerelative(left, true, false, 0, false);
      unlock(right);
      with keytable[right].oprnd do
        if (m = dreg) and bestdreg(reg) then
          begin
          markdreg(reg);
          newdreg := reg;
          end
        else newdreg := getdreg;
      with keytable[left].oprnd do
        begin
        if bestareg(reg) then newareg := reg
        else newareg := getareg;
        setvalue(bitindexed, newareg, newdreg, false, offset, offset1);

        { Our noframe hack in geninst will miss this reference to fp unless
          it is relative mode.  This code causes an lea to be generated
          instead of a move.
        }
        if not blockusesframe and (reg = fp) then
          begin
          settemp(long, relative, fp, 0, false, 0, 0, 1, unknown);
          lea_needed := true;
          end
        else settempareg(reg);
        end;
      shiftamount := 0;
      newoffset := len;
      while newoffset > 1 do
        begin
        newoffset := newoffset div 2;
        shiftamount := shiftamount + 1;
        end;
      settempimmediate(word, 3 - shiftamount);
      settempareg(newareg);

      if lea_needed then gen2(lea, long, tempkey + 2, tempkey)
      else gensimplemove(tempkey + 2, tempkey);

      settempdreg(long, newdreg);
      gensimplemove(right, tempkey);
      gen2(ror, long, tempkey + 2, tempkey);
      keytable[tempkey].len := word;
      gendouble(add, tempkey, tempkey + 1);
      gensingle(swap, tempkey);
      keytable[tempkey + 2].oprnd.offset := 3;
      gendouble(rol, tempkey + 2, tempkey);
      end;
    keytable[key].packedaccess := true;
  end {paindxx} ;



procedure regtempx;

{ Generate a reference to a local variable permanently assigned to an
  integer register.  "pseudoinst.oprnds[3]" contains the number of the
  temp register assigned, and oprnds[1] is the variable access being
  so assigned.
}


  begin
    address(left);
    setvalue(dreg, 8 - pseudoinst.oprnds[3], 0, false, 0, 0);
    dregused[8 - pseudoinst.oprnds[3]] := true;
  end {regtempx} ;


procedure realtempx;

{ Generate a reference to a local variable permanently assigned to an
  fp register.  "pseudoinst.oprnds[3]" contains the number of the
  temp register assigned, and oprnds[1] is the variable access being
  so assigned.
}


  begin
    address(left);
    setvalue(fpreg, 8 - pseudoinst.oprnds[3], 0, false, 0, 0);
    fpregused[8 - pseudoinst.oprnds[3]] := true;
  end {realtempx} ;


procedure ptrtempx;

{ Generate a reference to a pointer variable which has been assigned to
  a register.  pseudoinst.oprnds[3] is the index of the temp assigned,
  and oprnds[1] is the variable access.
}


  var
    register: regindex;


  begin {ptrtempx}
    address(left);

    { The debugger/profiler and the own section pointer for PIC use A3
      which is the second pointer temp, this code adjusts the register
      number.  If there is a static link in use then there are no pointer
      registers left and indicates a bug in travrs.
    }
    if (targetopsys = vdos) and
      ((pic_enabled and (ownsize > 0) and proctable[blockref].ownused) or
      (switcheverplus[debugging] or switcheverplus[profiling])) then
      begin
      register := gp - pseudoinst.oprnds[3] + 1;

      if proctable[blockref].intlevelrefs then
        begin
        write('Attempt to allocate too many ptrtemps');
        compilerabort(inconsistent);
        end;
      end
    else register := gp - pseudoinst.oprnds[3];

    setvalue(areg, register, 0, false, 0, 0);
    aregused[register] := true;
  end {ptrtempx} ;



procedure stacktargetx;

{ Sets up a key to be used as a target for code generation when the actual
  target is being pushed on the stack.  This makes targeting work with
  temps being generated.

  The sequence is:

        stacktarget     skey
        expression code
        push            skey
}


  begin
    if not paramlist_started then
      begin
      paramlist_started := true;
      saveactivekeys; {since no makeroom was called for this parameter list}
      end;
    makestacktarget;
    dontchangevalue := dontchangevalue + 1;
  end {stacktargetx} ;


procedure addrx;

{ Generate the address of oprnds[1].  Used to make a pointer for array
  access, etc.
}


  begin {addrx}
    address(left);
    setvalue(areg, getareg, 0, false, 0, 0);
    with keytable[left].oprnd do
      if switcheverplus[sharecode] and
         (m in [commonlong, supportcall, usercall]) then
        begin
        settempadcon(m, offset, commonlong_reloc);
        gensimplemove(tempkey, key);
        end
      else
        gen2(lea, long, fix_effective_addr(left), key);
  end {addrx} ;



procedure movx(packedleft: boolean; {true if bits get packed from left end
                                     of word, not right end}
               regmode: modes; {should be areg or dreg}
               getreg: getregtype {routine used to allocate a reg} );

{ Generate code to move the integer/pointer described by the right
  operand to the left operand.  The operand is first copied to a register
  if this seems wise, and "pack" is called if necessary.
}

  var
    newreg: regindex;


  begin {movx}
    if keytable[right].oprnd.m = usercall then
      begin
      settempreg(len, areg, getareg);
      gendouble(lea, right, tempkey);
      changevalue(right, tempkey);
      end;

    with keytable[left], oprnd do
      if packedaccess then pack(packedleft)
      else
        begin
        { If "use_preferred_key" is true "unpack" will be encouraged to use
          "preferred_key" as its destination.
        }
        use_preferred_key := true;
        preferred_key := left;
        unpackshrink(right, len);
        lock(right);
        address(left);
        unlock(right);
        if len > long then genblockmove(right, left, long)
        else if not equivaccess(right, left) and
           ((keytable[right].oprnd.m = regmode) or (refcount > 2)) and
           (dontchangevalue = 0) { must guard this because C allows
                                   assignments in parameter lists } then
          begin {load into a register first, then move}
          lock(left);
          settempreg(len, regmode, 0 {fill in later} );
          if m = regmode then newreg := reg
          else if keytable[right].oprnd.m = regmode then
            begin
            newreg := keytable[right].oprnd.reg;
            keytable[tempkey].high_word_dirty :=
              keytable[right].high_word_dirty;
            end
          else newreg := getreg();
          unlock(left);
          keytable[tempkey].oprnd.reg := newreg;
          gensimplemove(right, tempkey);
          gensimplemove(tempkey, left);
          if (regmode = dreg) and (newreg <= lastdreg) or (regmode = areg) and
             (newreg <= lastareg) then
            changevalue(left, tempkey);
          end
        else gensimplemove(right, left);
        end;

    if (right > 0) and
       (keytable[right].oprnd.m in [immediate, immediatelong]) then
      setkeyvalue(right)
    else
      begin
      setkeyvalue(left);
      keytable[key].len := keytable[left].len;
      end;
  end {movx} ;


procedure fpmovx;

{ Generate code to move the 68881 floating point number described by the
  right operand to the left operand.  If the move is memory to memory,
  then an fp register will be allocated and the move will go through the
  68881 to allow it to complain about bad numbers.
}

  begin {fpmovx}
    with keytable[right].oprnd do if flavor = int_float then
      flavor := float;

    keytable[left].oprnd.flavor := float;

    { If neither operand is in an fp register, then **don't** force the move to
      go through the 68881.
    }
    if (keytable[left].oprnd.m <> fpreg) and
       (keytable[right].oprnd.m <> fpreg) then
      begin
      if (keytable[left].oprnd.flavor = float) and
         (keytable[right].oprnd.flavor = float) and
         (keytable[left].len = keytable[right].len) then
        begin
        movx(false, dreg, @getdreg); { This is faster and perhaps not that much
                                      worse (NAN traps, normalization, ect.)
                                      than going through the 68881. }
        { Gendouble will change the flavor.  Set it back.
        }
        keytable[left].oprnd.flavor := float;
        keytable[right].oprnd.flavor := float;
        end
      else
        begin { must float an integer or convert real to double or double
                to real. }
        addressboth;
        settempfpreg(getfpreg);
        genfpmove(right, tempkey);
        genfpmove(tempkey, left);
        end;
      end
    else
      begin
      addressboth;
      genfpmove(right, left);
      end;
    setkeyvalue(left);
  end {fpmovx} ;


procedure movlitintx;

{ Generate literal (immediate data) integer move.
}


  begin {movlitintx}
    right := tempkey;
    movx(false, dreg, @getdreg);
  end {movlitintx} ;


procedure movlitrealx;

{ Generate literal (immediate data) real move - always 0.0.
}


  begin {movlitrealx}
    right := tempkey;
    keytable[right].oprnd.m := immediatelong;

    if mc68881 then 
      if (keytable[left].oprnd.m = fpreg) and (keytable[left].regvalid) then
        begin
        { The result is going to the 68881, so we can generate an internal
          constant move.
        }
        settemp(0, special_immediate, 0, 0, false, 15, 0, 1, unknown);
        address(left);
        fpgendouble(fmovecr, tempkey, left);
        keytable[key].oprnd.flavor := float;
        end
      else fpmovx
    else movx(false, dreg, @getdreg);
  end {movlitrealx} ;


procedure movlitptrx;

{ Move the value of NIL into variable described by keytable[left].
}


  begin
    setlongvalue(niladdressvalue);
    movx(false, areg, @getareg);
  end {movlitptrx} ;


procedure movstructx(guaranteed: boolean; {must be even}
                     register_ok: boolean {true if pascal string type} );

{ Generate code to move a structure.
}

  var
    temp: boolean;
    llen: integer;
    len_hack: boolean;

  begin
    len_hack := false;

    { In Modula2, a constant "string" can be moved to an array of greater
      length.  This code will set left's length to the pseudo-op's length
      if right is a constant.  Left's length will be reset at the end
      of this routine.
    }
    if language = modula2 then
      if (pseudoinst.op <> movset) and
         (keytable[right].oprnd.m = pcrelative) then
        begin
        len_hack := true;
        llen := keytable[left].len;
        keytable[left].len := len;
        end;

    if settargetused then
      begin { Prevents redundant set moves. }
      settargetused := false;
      derefboth;
      end
    else
      if register_ok and 
         (keytable[left].packedaccess or (len = byte) or ((len = word) or
         (len = long)) and (guaranteed or keytable[left].knowneven and
         (keytable[right].len <> 3) and { handle string[2] case }
         (keytable[right].knowneven or (keytable[right].oprnd.m = immediate))))
         then
        begin
        if pseudoinst.op = movset then
          temp := false
        else
          temp := not keytable[right].packedaccess;
        movx(temp, dreg, @getdreg);
        end
      else
        begin
        unpackboth(1);
        if len > 0 then
          if guaranteed and (keytable[left].len > byte) then
            genblockmove(right, left, word)
          else genblockmove(right, left, byte);
        end;
    setkeyvalue(left);

    if language = modula2 then
      if len_hack then keytable[left].len := llen;
  end {movstructx} ;


procedure movstrx;

{ Generate code to move a string.  If the max(src) will fit in the max(dst)
  then just let movstructx handle it.
}

  const
    maxsimplesize = 32; {an arbitrary minimum length for movc3 instr}

  var
    lengthreg: keyindex; { where to find length }
    tlen: 0..maxstrlen; {for diddling length fields}
    src, dst: keyindex;


  begin {movstrx}
    if equivaddr(left, right) then derefboth
    else if (keytable[left].len >= keytable[right].len) then
      movstructx(false, false)
    else
      begin
      addressboth;
      aligntemps;
      lock(right);
      lock(left);

      settempdreg(word, getdreg);
      lengthreg := tempkey;

      if keytable[right].oprnd.m = pcrelative then
        begin { it's a constant string, handle any truncation now }
        if keytable[right].len > keytable[left].len then
          settempimmediate(word, keytable[left].len - 1)
        else settempimmediate(word, get_stringfile_byte(keytable[right].oprnd.
                              offset) - 1);
        gensimplemove(tempkey, lengthreg);
        end
      else
        begin
        gensingle(clr, lengthreg);
        keytable[lengthreg].len := byte;
          { DBRA needs a word, but string length is a byte }
        tlen := keytable[right].len;
        keytable[right].len := byte;
        gensimplemove(right, lengthreg); { pick up current src length }
        keytable[right].len := tlen;
        settempimmediate(byte, keytable[left].len - 1); { max(dst) length }
        gendouble(cmp, tempkey, lengthreg);
        genbr(bls, lastlabel);
        gensimplemove(tempkey, lengthreg); { use max(dst) as new dst length }
        definelastlabel;
        end;

      tlen := keytable[left].len;
      keytable[left].len := byte;
      gensimplemove(lengthreg, left); { plug in new length }
      keytable[left].len := tlen;

      settempimmediate(byte, 1);
      gendouble(sub, tempkey, lengthreg); { DBRA goes n + 1 times }
      tempkey := tempkey + 1;
      genbr(blo, lastlabel - 1); { null src case }

      { Generate relative pointers to first byte of string for src and dst.
      }
      settempareg(getareg);
      dst := tempkey;
      with keytable[left].oprnd do offset := offset + 1;
      gen2(lea, long, left, dst);
      with keytable[left].oprnd do offset := offset - 1;
      unlock(left);

      lock(dst);
      settempareg(getareg);
      src := tempkey;
      with keytable[right].oprnd do offset := offset + 1;
      gen2(lea, long, right, src);
      with keytable[right].oprnd do offset := offset - 1;
      unlock(dst);

      definelastlabel; { top of loop }
      keytable[src].oprnd.m := autoi;
      keytable[dst].oprnd.m := autoi;
      gen2(move, byte, src, dst); { move the string }
      gendb(dbra, lengthreg, lastlabel + 1); { decrement and branch }

      definelastlabel; { target of null case }
      unlock(right);
      end;
  end {movstrx} ;


procedure movcstructx;

{ Generate code to move a structure whose size isn't known until
  runtime.  The size is specified by "target".
}

  var
    count: keyindex; {byte count for the move}


  begin
    count := target;
    target := 0; {to avoid confusing load routines}
    unpack(count, word);
    keytable[key].len := keytable[count].len;
    loaddreg(count, 0, true);
    settempimmediate(word, 1);
    gendouble(sub, tempkey, key);
    lock(key);
    addressboth;
    unlock(key);
    if (keytable[left].oprnd.m <> areg) or (keytable[left].refcount > 0) then
      begin
      lock(right);
      settempareg(getareg);
      gen2(lea, long, fix_effective_addr(left), tempkey);
      left := tempkey;
      unlock(right);
      end;
    if (keytable[right].oprnd.m <> areg) or
       (keytable[right].refcount > 0) then
      begin
      lock(left);
      settempareg(getareg);
      gen2(lea, long, fix_effective_addr(right), tempkey);
      right := tempkey;
      unlock(left);
      end;
    keytable[right].oprnd.m := autoi;
    keytable[left].oprnd.m := autoi;
    definelastlabel;
    gen2(move, byte, right, left);
    gendb(dbra, key, lastlabel + 1);
  end; {movcstructx}

procedure chrstrx;

{ Convert a character into an extended string.  The length byte is set to
  one and the data byte set to the value of the character.
}


  begin {chrstrx}
    unpack(left, byte);
    shrink(left, byte);
    makestacktarget;
    aligntemps;
    settempimmediate(byte, 1);
    tempkey := tempkey - 1;
    keytable[tempkey] := keytable[key];
    keytable[tempkey].len := byte;
    gendouble(move, tempkey + 1, tempkey);
    with keytable[tempkey].oprnd do offset := offset + 1; {el kludge}
    gendouble(move, left, tempkey);
  end {chrstrx} ;


procedure arraystrx;

{ Convert a packed array [1..n] of chars into an extended string.  The
  length byte is set to n, data bytes to chars 1..n
}


  begin {arraystrx}
    unpack(left, byte);
    makestacktarget;
    aligntemps;
    settempimmediate(byte, keytable[left].len);
    tempkey := tempkey - 1;
    keytable[tempkey] := keytable[key];
    gendouble(move, tempkey + 1, tempkey);

    with keytable[tempkey], oprnd do
      begin
      offset := offset + 1; {el kludge}
      knowneven := mc68020;
      { ^^^ perhaps the length should be at 1(sp) and the text start at 2(sp)
        so we can use word or long moves.
      }
      end;

    genblockmove(left, tempkey, byte);
  end {arraystrx} ;



procedure createfalsex;

{ Create false constant prior to conversion of comparison to value.
  If a target exists, the value is built there; if not, in a register.
  The target might be a packed field, which explains the apparent
  complexity of this procedure.

  There is a kludge to guarantee that we catch all short-circuit
  evaluations destined to be moved to the stack (as part of a parameter
  list), even when travrs' targetting logic fails to pick it up due
  to future, "dangerous" references to operands.
}


  begin {createfalsex}
    if (stackoffset <> - keytable[stackcounter].oprnd.offset) and
       (keytable[stackcounter].len <= long) then
      begin
      keytable[key].regsaved := true;
      keytable[key].properreg := stackcounter;
      if keytable[stackcounter].len = long then keytable[key].len := long;
      setkeyvalue(stackcounter)
      end
    else if target <= 0 then setvalue(dreg, getdreg, 0, false, 0, 0)
    else setallfields(target);
    makeaddressable(key);
    tempkey := tempkey - 1;
    keytable[tempkey] := keytable[key];
    left := tempkey;
    settempimmediate(word, 0);
    key := 0;
    movlitintx;
    key := pseudoinst.key;

    if not ((stackoffset <> -keytable[stackcounter].oprnd.offset) and
           (keytable[stackcounter].len > long) and (dontchangevalue > 1)) then
      savekey(key);
  end {createfalsex} ;


procedure createtruex;

{ Create the value 'true'.  This operator must follow a createfalse operator,
  whose key is passed as our left operand.  Code is straightforward for
  a packed field: a single bit is assigned.  Unpacked fields are subjected
  to a slight hack: an increment instruction (remember, 1 = 0 + 1 !) is used
  instead of a mov #1, to save a word + a cycle.  The assignment to lastbranch
  allows this value to be popped off the stack if the temp register grabbed
  by the previous createfalse instruction.  This assignment is safe because
  createfalse and createtrue instructions bracket a closed region of code and
  no branch instructions can escape this region.
}


  begin {createtruex}
    setallfields(left);
    if (keytable[left].oprnd.m = dreg) and not (keytable[left].regvalid) then
      begin
      dereference(left);
      left := keytable[left].properreg;
      end
    else address(left);
    adjusttemps;
    keytable[key].len := keytable[left].len;
    context[contextsp].lastbranch := keytable[left].instmark - 1;
    settempimmediate(keytable[key].len, 1);
    right := tempkey;
    if keytable[left].packedaccess then
      begin
      with keytable[left], oprnd do
        settemp(len, m, reg, indxr, indxlong, offset, offset1, scale,
                commonlong_reloc);
      left := tempkey;
      key := 0;
      pack(false);
      key := pseudoinst.key;
      end
    else gendouble(add, right, left);
  end {createtruex} ;


procedure createtempx;

{ Create a temp to hold the value of a conditional expression.
  the logic here is very similar to that of createfalse except that
  we can't afford to modify the target by this operation unless the
  next operation is a simple assignment.  It's a pity, since we can
  generate better code for structure assignments if we could.
}
  begin
    aligntemps;
    if (left = ord(ints)) or (left = ord(reals)) and
       (len <= long) and not mc68881 then
      setvalue(dreg, getdreg, 0, false, 0, 0)
    else if (left = ord(ptrs)) or (left = ord(fptrs)) then
      setvalue(areg, getareg, 0, false, 0, 0)
    else if (left = ord(reals))  and mc68881 then
      setvalue(fpreg, getfpreg, 0, false, 0, 0)
    else
      begin
      newtemp(len);
      aligntemps;
      keytable[stackcounter].tempflag := true;
      keytable[key].regsaved := true;
      keytable[key].properreg := stackcounter;
      setkeyvalue(stackcounter);
      end;
  end {createtempx} ;


procedure jointempx;

{ Make the common temp available
}
  begin
    context[contextsp].lastbranch := keytable[left].instmark - 1;
    dereference(left);
    setallfields(left);
  end;


procedure cmpx(signedbr, unsignedbr: insttype; {branch on result}
               regmode: modes; {areg or dreg}
               getreg: getregtype {getareg or getdreg} );

{ Compare scalars or pointers in keytable[left] and keytable[right].
  Keytable[key] is set by setbr to the appropriate branch.
}

  var
    brinst: insttype; {the actual branch to use}


  begin {cmpx}
    if keytable[left].signed = keytable[right].signed then
      len := min(len, max(bytelength(left), bytelength(right)));

    if len = 3 then len := 4;

    { Shrinking operands is only safe if the sign is set back to the original
      sign for that length.  The field "signlimit" provides that function.
    }
    unpkshkboth(len);

    if signedoprnds then brinst := signedbr
    else brinst := unsignedbr;

    if keytable[left].oprnd.m = regmode then gendouble(cmp, right, left)
    else if keytable[right].oprnd.m = regmode then
      begin
      gendouble(cmp, left, right);
      brinst := reverse[brinst];
      end
    else
      begin
      lock(right);
      settempreg(keytable[left].len, regmode, getreg());
      unlock(right);
      gensimplemove(left, tempkey);
      changevalue(left, tempkey);
      gendouble(cmp, right, left);
      end;
    setbr(brinst);
  end {cmpx} ;


procedure cmplitintx(signedbr, unsignedbr: insttype; {branch instructions}
                     unsignedzerobr: insttype {special branch for 0 compare} )
                     ;

{ Generate comparison with a literal integer.  If this is zero, no comparison
  need be done.
}

  var
    packingmod: shortint;

  begin
    if pseudoinst.oprnds[2] <> 0 then
      begin
      right := tempkey;

      if keytable[left].packedaccess then packingmod := bitsperbyte
      else packingmod := 1;

      if not keytable[left].signed then { left is unsigned }
        begin
        if (keytable[left].len = packingmod) and
           (keytable[right].oprnd.offset >= 0) and
           (keytable[right].oprnd.offset <= 255) then
          begin
          keytable[right].signed := false;
          keytable[right].len := 1;
          { Front-end is overly conservative here and makes the cmp word }
          keytable[key].len := 1;
          len := 1;
          end
        else if (keytable[left].len = word * packingmod) and
                (keytable[right].oprnd.offset >= 0) and
                (keytable[right].oprnd.offset <= $ffff) then
          begin
          keytable[right].signed := false;
          keytable[right].len := word;
          { Front-end is overly conservative here and makes the cmp long }
          keytable[key].len := word;
          len := word;
          end
        end
      else if not keytable[left].packedaccess then keytable[right].len := len;

      cmpx(signedbr, unsignedbr, dreg, @getdreg)
      end
    else forcebranch(left, signedbr, unsignedzerobr);
  end {cmplitintx} ;


procedure cmplitptrx(brinst: insttype);

{Generate code for comparision with NIL.
}


  begin
    setlongvalue(niladdressvalue);
    cmpx(brinst, brinst, areg, @getareg);
  end {cmplitptrx} ;



procedure cmpsetinclusion(left, right: keyindex {operands} );

{ Generate a test for pure set inclusion, used by geqset and leqset.  The
  only difference is in the order of the operands.  The actual test is to
  nand the left operand with a copy of the right operand and check for zero.
}

  var
    loop: boolean; {true if loop actually generated}
    pieces: integer; {number of loop pieces to generate inline}
    oldlastbranch: nodeindex;


  begin {cmpsetinclusion}
    { Sets are always knowneven.
    }
    keytable[left].knowneven := true;
    keytable[right].knowneven := true;

    len := min(keytable[left].len, keytable[right].len);
    unpackboth(len);
    oldlastbranch := context[contextsp].lastbranch;
    context[contextsp].lastbranch := cnodetablesize;
    initloop(left, right, right, pseudoinst.len, 1, loop, pieces);
    settempdreg(piecesize, getdreg);
    onlyreference(loopsrc);
    gen2(move, piecesize, loopsrc, tempkey);
    gen1(notinst, piecesize, tempkey);
    onlyreference(loopdst);
    gen2(andinst, piecesize, loopdst, tempkey);
    bumploop(dbne, loop);
    finishloop;
    context[contextsp].lastbranch := oldlastbranch;
    setbr(beq);
  end {cmpsetinclusion} ;


procedure cmpstructx(brinst: insttype {branch condition} );

{ Generate code for structured variable comparison.  Only strings may be
  compared for other than equality, but at this level we don't care.
}

  var
    loop: boolean; {true if loop actually generated}
    pieces: integer; {number of loop cores to generate inline}
    i: integer; {induction var for building inline code}
    lab: integer; {exit label for the loop}
    b: insttype; {computed brinst, possibly reverse[brinst]}
    oldlastbranch: nodeindex; {old value of context[contextsp].lastbranch}


  procedure onecmp;


    begin
      if keytable[loopsrc].oprnd.m = dreg then
        gen2(cmp, piecesize, loopdst, loopsrc)
      else if keytable[loopdst].oprnd.m = dreg then
        begin
        gen2(cmp, piecesize, loopsrc, loopdst);
        b := reverse[brinst];
        end
      else if keytable[loopsrc].oprnd.m <> autoi then
        begin
        gen2(move, piecesize, loopsrc, tempkey);
        gen2(cmp, piecesize, loopdst, tempkey);
        end
      else if keytable[loopdst].oprnd.m <> autoi then
        begin
        gen2(move, piecesize, loopdst, tempkey);
        gen2(cmp, piecesize, loopsrc, tempkey);
        b := reverse[brinst];
        end
      else gen2(cmpm, piecesize, loopdst, loopsrc);
    end {onecmp} ;


  begin {cmpstructx}
    unpackboth(1);
    len := min(keytable[left].len, keytable[right].len);
    b := brinst;
    lab := lastlabel;
    lastlabel := lastlabel - 1;
    oldlastbranch := context[contextsp].lastbranch;
    context[contextsp].lastbranch := cnodetablesize;
    initloop(left, right, right, pseudoinst.len, 2, loop, pieces);
    context[contextsp].lastbranch := lastnode + 1;

    if not ((keytable[loopsrc].oprnd.m = autoi) and
            (keytable[loopdst].oprnd.m = autoi)) and
       not ((keytable[loopsrc].oprnd.m = dreg) or
            (keytable[loopdst].oprnd.m = dreg)) then
      settempdreg(piecesize, getdreg);

    onlyreference(loopsrc);
    onlyreference(loopdst);
    onecmp;

    for i := 2 to pieces do
      begin
      genbr(bne, lab);
      bumploop(dbne, loop);
      onecmp;
      end;

    if pieces = 1 then bumploop(dbne, loop);

    finishloop;

    if pieces > 1 then definelabel(lab);

    context[contextsp].lastbranch := oldlastbranch;
    setbr(b);
  end {cmpstructx} ;


procedure cmpstrx(brinst: insttype {beq, etc etc} );

{ Generate code to compare an extended string.
}

  var
    tlen: 0..maxstrlen; {temp used while fiddling lengths}
    register: keyindex; {d-reg to hold loop count and length}
    arg1, arg2: keyindex; {a-regs to point to arguments}
    left_len, right_len: keyindex; {used for constant strings}
    len_byte: integer;
    left_const: boolean; { True if left is a constant string }

  begin {cmpstrx}
    saveactivekeys; { This routine needs lots of registers }
    addressboth;

    { If either left or right are constant strings, then create an immediate
      constant to put in left_len or right_len.  If not a constant, these
      are copies of left or right to simplify the code below.
    }
    if keytable[left].oprnd.m = pcrelative then
      begin { constant string }
      settempimmediate(word, get_stringfile_byte(keytable[left].oprnd.offset));
      left_len := tempkey;
      left_const := true;
      end
    else
      begin
      left_len := left;
      left_const := false;
      end;

    if keytable[right].oprnd.m = pcrelative then
      begin { constant string }
      settempimmediate(word, get_stringfile_byte(keytable[right].oprnd.offset));
      right_len := tempkey;
      end
    else right_len := right;

    lock(right);
    lock(left);
    settempdreg(word, getdreg);
    register := tempkey;

    { Since the length is an unsigned byte and the DBRA counter needs a word,
      this register must be cleared to do a unsigned extension unless left_len
      is a constant.
    }
    if not left_const then gensingle(clr, register);
    keytable[register].len := byte;

    { Compute the min of the two strings.
    }
    gendouble(move, left_len, register);
    gendouble(cmp, right_len, register);
    genbr(bls, lastlabel);
    gendouble(move, right_len, register);
    definelastlabel;
    settempimmediate(byte, 1);
    gendouble(sub, tempkey, register);  { DBRA goes n+1 times }
    tempkey := tempkey + 1;
    genbr(blo, lastlabel - 1);

    settempareg(getareg);
    arg1 := tempkey;
    with keytable[left].oprnd do offset := offset + 1;
    gen2(lea, long, left, arg1);
    with keytable[left].oprnd do offset := offset - 1;

    unlock(left);
    lock(arg1);
    settempareg(getareg); { may take left's register }
    arg2 := tempkey;
    with keytable[right].oprnd do offset := offset + 1;
    gen2(lea, long, right, arg2);
    with keytable[right].oprnd do offset := offset - 1;
    unlock(arg1);

    definelastlabel;
    keytable[arg1].oprnd.m := autoi;
    keytable[arg2].oprnd.m := autoi;
    gen2(cmpm, byte, arg2, arg1);  { do the cmp }

    { Exit loop if no longer equal or decrement and branch.  The second test
      is needed in case the loop did not go to completion and preserves
      condition codes.
    }
    gendb(dbne, register, lastlabel + 1); { decrement and branch }
    genbr(bne, lastlabel - 1);  

    { Strings match through min of lengths.  Now find out which is longer and
      call that one "greater".
    }
    definelastlabel;
    makeaddressable(left_len); { We may need as many as four aregisters. }
    gendouble(move, left_len, register);
    gendouble(cmp, right_len, register);
    definelastlabel;
    setbr(brinst);
    unlock(right);
  end {cmpstrx} ;


{ Routines to load operands on the top of the stack }

function loadedstack(k: keyindex; {operand to check}
                     stackkey: keyindex {candidate location} ): boolean;

{ True if the operand "k" is in a modifiable location and is in the
  location of "stackkey".  "Stackkey" is assumed to be a temporary
  location on the stack.
}

  begin
    loadedstack := (keytable[stackkey].refcount = 0)
                   and (keytable[stackkey].instmark - 1 >
                        context[contextsp].firstnode)
                   and (keytable[stackkey].oprnd.m = relative)
                   and (keytable[stackkey].oprnd.reg = sp)
                   and equivaccess(k, stackkey);
  end {loadedstack} ;


procedure loadstack(src: keyindex {operand to load} );

{ Get a set or string operand into a modifiable location, either
  the target or a temp on the stack.

  The entry for the global "key" is set to reflect this location.
}


  begin {loadstack}
    if loadedstack(src, stackcounter) then setallfields(src)
    else if (target > 0) and (keytable[key].refcount <= 1) then
      setallfields(target)
    else
      with keytable[src], oprnd do
        begin
        newtemp(pseudoinst.len);
        keytable[stackcounter].tempflag := true;
        keytable[key].properreg := stackcounter;
        keytable[key].regsaved := true;
        setkeyvalue(stackcounter);
        end;
  end {loadstack} ;


procedure pushone(k: keyindex {operand to push} );

{ Push "k" onto the top of the stack if it is not already there.
  The operand must be a double real.

  * not used for 68881 *
}

  begin
    if not loadedstack(k, stackcounter) then
      with keytable[k] do
        begin
        aligntemps;
        newtemp(quad);
        keytable[stackcounter].tempflag := true;
        genblockmove(k, stackcounter, long);
        end;
    keytable[stackcounter].refcount := 1;
  end {pushone} ;


procedure pushboth(commute: boolean {true if operands can be commuted} );

{ Push the global operands "left" and "right" onto the stack.  This code
  attempts to avoid unnecessary movement if one of the operands is already
  on the stack.  The operands must be double reals.

  * not used for 68881 *
}


  begin
    if loadedstack(right, stackcounter + 1)
       and loadedstack(left, stackcounter)
       or not equivaddr(left, right)
       and loadedstack(right, stackcounter)
       and not loadedstack(left, stackcounter + 1) then
      begin
      { Swap reals on top of stack }
      pushone(left);
      if not commute then callsupport(libdswap);
      end
    else if not loadedstack(left, stackcounter + 1)
        or not loadedstack(right, stackcounter) then
      begin
      lock(right);
      pushone(left);
      unlock(right);
      pushone(right);
      end;
  end {pushboth} ;

procedure settos(args: integer {original number of arguments} );

{ Set the top of stack on return from a floating point or similar routine
  or instruction.  It pops the arguments, then allocates a new temp whose
  location will just happen to be the same as the answer's.  The entry for the
  global "key" will be set up to reflect this result.
}


  begin {settos}
    returntemps(args);
    stackoffset := - keytable[stackcounter - 1].oprnd.offset
                   - keytable[stackcounter - 1].len;
    makestacktarget;
    stackoffset := - keytable[stackcounter].oprnd.offset;
  end   {settos};




procedure pshlitrealx;

{ Push a literal real onto the top of the stack, always 0.0.
}


  begin
    keytable[tempkey].oprnd.m := immediatelong;
    gensimplemove(tempkey, key);
    dontchangevalue := dontchangevalue - 1;
    keytable[key].oprnd.flavor := float;
  end {pshlitrealx} ;


procedure pshlitintx;

{ Push a literal integer onto the top of the stack.  This involves fudging the
  "tempkey" offset since the value is in "pseudoinst.oprnds[1]" instead of
  "right" in the movelit pseudo-ops.
}


  begin
    keytable[tempkey].oprnd.offset := pseudoinst.oprnds[1];
    gensimplemove(tempkey, key);
    dontchangevalue := dontchangevalue - 1;
  end {pshlitintx} ;


procedure pshx;

{ Push a simple value onto the stack.
}


  begin
    if pseudoinst.op = pshreal then
      with keytable[left].oprnd do if flavor = int_float then flavor := float;

    if mc68881 and (pseudoinst.op = pshreal) then
      begin
      if (keytable[left].oprnd.m = fpreg) or
        (keytable[left].oprnd.flavor = int) or
        (keytable[left].len <> len) { a real to double cvt }
      then
        begin
        address(left);
        keytable[key].oprnd.flavor := float;

        if keytable[left].oprnd.m <> fpreg then
          begin { float it }
          settempfpreg(getfpreg);
          genfpmove(left, tempkey);
          keytable[tempkey].len := keytable[key].len;
          genfpmove(tempkey, key);
          end
        else genfpmove(left, key);
        end
      else if len = quad then
        begin {known to be a double precision real on an even boundary}
        address(left);
        genblockmove(left, key, long);
        keytable[left].oprnd.flavor := float;
        end
      else
        begin
        unpackshrink(left, len);
        gensimplemove(left, key);
        keytable[left].oprnd.flavor := float;
        end;
      end
    else if len = quad then
      begin {known to be a double precision real on an even boundary}
      address(left);
      genblockmove(left, key, long);
      end
    else
      begin
      unpackshrink(left, len);
      gensimplemove(left, key);
      end;

    dontchangevalue := dontchangevalue - 1;
  end {pshx} ;


procedure pshfptrx;

{ Push a function pointer onto the stack
}
  begin
    settemp(long, usercall, 0, 0, false, pseudoinst.oprnds[1], 0, 1, unknown);
    gen1(pea, long, tempkey);
    stackoffset := stackoffset + long;
    dontchangevalue := dontchangevalue - 1;
  end; {pshfptrx}


procedure pshlitptrx;

{ Push a "nil" onto the stack.
}


  begin {pshlitptrx}
    setlongvalue(niladdressvalue);
    gensimplemove(right, key);
    dontchangevalue := dontchangevalue - 1;
  end {pshlitptrx} ;


procedure pshaddrx;

{ Push address of left operand onto the stack.
}


  begin {pshaddrx}
    address(left);
    with keytable[left].oprnd do
      if switcheverplus[sharecode] and
         (m in [commonlong, supportcall, usercall]) then
        begin
        settempadcon(m, offset, commonlong_reloc);
        gensimplemove(tempkey, key);
        end
      else
        begin
        gen1(pea, long, fix_effective_addr(left));
        stackoffset := - keytable[stackcounter].oprnd.offset;
        end;
    dontchangevalue := dontchangevalue - 1;
  end {pshaddrx} ;


procedure pshstraddrx;

{ Push a string's address on the stack for support library routines.  The
  support library expects a string to be passed as a pair (len, @(data)).
  In this way, extended strings as well as standard, packed array [1..n] of
  char strings can be passed to the same routine.

  In order to simplify code generation for write, reset, etc a dummy stack
  entry of length 0 is allocated so that the genblk routines see consistent
  numbers of stack slots.
}


  begin {pshstraddrx}
    with keytable[stackcounter] do
      begin
      len := len - defaulttargetintsize;
      oprnd.offset := oprnd.offset + defaulttargetintsize;
      address(left);
      with keytable[left], oprnd do
        begin
        len := len - 1;
        offset := offset + 1;
        if switcheverplus[sharecode] and
           (m = commonlong) then
          begin
          settempadcon(m, offset, commonlong_reloc);
          gensimplemove(tempkey, key);
          end
        else
          begin
          gen1(pea, long, fix_effective_addr(left));
          stackoffset := - keytable[stackcounter].oprnd.offset;
          end;
        len := len + 1;
        offset := offset - 1;
        end;
      { Push a longword zero in the stack and move the length byte into the
        low order byte on the stack.
      }
      oprnd.offset := oprnd.offset - defaulttargetintsize;
      len := len - long + defaulttargetintsize;
      gensingle(clr, stackcounter);
      stackoffset := - keytable[stackcounter].oprnd.offset;
      tempkey := tempkey - 1;
      keytable[tempkey] := keytable[left];
      keytable[tempkey].len := byte;
      oprnd.offset := oprnd.offset + long - byte; { point to low order byte }
      gensimplemove(tempkey, stackcounter);
      oprnd.offset := oprnd.offset - long + byte; { point to low order byte }
      len := len + long;
      end;
    dontchangevalue := dontchangevalue - 1;
    newtemp(0);
    keytable[stackcounter].refcount := 1;
  end {pshstraddrx} ;


procedure pshstrx;

{ Push a string value onto the stack.
}

  var
    constlen, oldlen: integer;


  begin {pshstrx}
    address(left);
    aligntemps;

    with keytable[left], oprnd do
      if m = pcrelative then
        begin
        constlen := get_stringfile_byte(offset) + 1;

        if constlen <= keytable[key].len then
          begin { Keep from moving too many bytes. }
          oldlen := len;
          len := constlen;
          genblockmove(left, key, byte);
          len := oldlen;
          end
        else
          begin { Handle truncation here. }
          { Move the old string length and the correct number of bytes }
          genblockmove(left, key, byte);
          settempimmediate(byte, keytable[key].len - 1);
          { Move the new string length over the old length }
          gen2(move, byte, tempkey, key);
          end;
        end
      else genblockmove(left, key, byte);

    dontchangevalue := dontchangevalue - 1;
  end {pshstrx} ;


procedure pshstructx;

{ Push a structure value onto the stack.
}


  begin {pshstructx}
    unpack(left, len);
    genblockmove(left, key, byte);
    dontchangevalue := dontchangevalue - 1;
  end {pshstructx} ;


procedure pshprocx;

{ Push a procedure descriptor onto the stack.  This consists of pushing
  the address of the procedure as well as the proper environment pointer.
  "right" is the procedure index to be pushed.  The "left" operand is
  the environment pointer, which is pushed onto the stack following the
  procedure address.
}

  var
    temp: integer;


  begin {pshprocx}
    address(left);
    if keytable[left].oprnd.m = commonlong then { own section }
      begin
      settempreg(long, autod, sp);
      gensingle(clr, tempkey);
      stackoffset := stackoffset + long;
      end
    else if not mc68020 and (keytable[left].oprnd.m = relative) and
        (abs(keytable[left].oprnd.offset) > $7FFF) then
      begin
      temp := keytable[left].oprnd.offset;
      keytable[left].oprnd.offset := 0;
      gen1(pea, long, fix_effective_addr(left));
      stackoffset := stackoffset + 2 * long; { fake up for add }
      settempimmediate(long, temp);
      gen2(add, long, tempkey, stackcounter);
      keytable[left].oprnd.offset := temp;
      stackoffset := stackoffset - long; { gets added in again below }
      end
    else
      begin
      gen1(pea, long, fix_effective_addr(left));
      stackoffset := stackoffset + long;
      end;

    if switcheverplus[sharecode] then
      begin
      settempadcon(usercall, pseudoinst.oprnds[2], unknown);
      settempreg(long, autod, sp);
      gensimplemove(tempkey + 1, tempkey);
      end
    else if pic_enabled and not mc68020 then
      begin
      settempareg(getareg);
      settemp(long, pic_splat_pcrel, 0, 0, false, 10, 0, 1, unknown);
      gen2(lea, long, tempkey, tempkey + 1);
      settemp(word, pic_usercall, 0, 0, false, pseudoinst.oprnds[2],
              6, 1, unknown);
      gen2(adda, long, tempkey, tempkey + 2);
      keytable[tempkey + 2].oprnd.m := indr;
      gen1(pea, long, tempkey + 2);
      end
    else
      begin
      settemp(word, usercall, 0, 0, false, pseudoinst.oprnds[2],
              0, 1, unknown);
      gen1(pea, long, tempkey);
      end;
    stackoffset := stackoffset + long;

    if proctable[pseudoinst.oprnds[2]].intlevelrefs then aregused[sl] := true;

    dontchangevalue := dontchangevalue - 1;
  end {pshprocx} ;



procedure jumpx(lab: integer; {label to jump to}
                picbr: boolean {if true generate 68000 pic branch} );

{ Generate an unconditional branch to "lab".  The only non-obvious thing here
  is linking the jump into the list of branch links

  Note that all branches to a particular label are linked through the
  brnodelink field of the label node.
}

  var
    p: brlinkptr; {for searching list of branch links}
    found: boolean; {for stopping search}


  begin
    if picbr and pic_enabled and not mc68020 then
      begin
      settempareg(getareg);
      settemp(long, pic_splat_pcrel, 0, 0, false, 10, 0, 1, unknown);
      gen2(lea, long, tempkey, tempkey + 1);
      settemp(word, pic_branch, 0, 0, false, lab, 6, 1, unknown);
      gen2(adda, long, tempkey, tempkey + 2);
      tempkey := tempkey + 2;
      keytable[tempkey].oprnd.m := indr;
      gensingle(jmp, tempkey);
      end
    else genbr(bra, lab);

    if (targetopsys = unix) and (unixtarget = Tandy) then
      {do nothing, space saving kludge}
    else
      begin
      p := firstbr;
      found := false;
      while not found and (p <> nil) do
        if p^.l = lab then found := true
        else p := p^.nextbr;
      if not found and
          not switcheverplus[debugging] and (tailmerging in genset) then
        begin
        new(p);
        p^.l := lab;
        p^.nextbr := firstbr;
        p^.n := 0;
        firstbr := p;
        end;
      if p <> nil then
        begin
        lastptr^.brnodelink := p^.n;
        p^.n := lastnode - 1;
        end;
      end {else};
  end {jumpx} ;


procedure jumpcond(inv: boolean {invert the sense of the branch} );

{ Used to generate a jump true or jump false on a condition.  If the key is
  not already a condition, it is forced to a "bne", as it is a boolean
  variable.
}


  begin
    forcebranch(right, bne, bne);
    if inv then genbr(invert[keytable[key].brinst], pseudoinst.oprnds[1])
    else genbr(keytable[key].brinst, pseudoinst.oprnds[1]);
    if findlabel(pseudoinst.oprnds[1]) = 0 then
      context[contextsp].lastbranch := lastnode;
  end {jumpcond} ;


{ Pascal label and goto routines }


procedure pascallabelx;

{ Generate a pascal label.  The complication arises from the need to reset
  the stack pointer and static link to the current block.  If the current
  block is the main program (level=1), then there is no static link, and
  sp is reset to a value which was saved at initialization.

  If the current block is not global, we use the static link to restore
  the stack pointer, so we always have intlevelrefs true.  In this case
  we adjust the sp by the difference between the current stackoffset and
  the static link. Inline code jumps around this stack adjustment stuff.

  In addition, all registers are marked as used in this routine if the
  label is the target of a non-local goto.  This is necessary since the
  come-from routine might have trashed registers not actually used in the
  gone-to routine.  Our solution, while gross, does work.

  The goto location is responsible for setting the static link and/or
  sp properly.
}

  var
    t: integer; {amount to fudge sp (no static link at level 2)}
    p: nodeptr; {points stack decrement value}


  begin {pascallabelx}
    clearcontext;
    if pseudoinst.oprnds[2] <> 0 then
      begin
      oktostuff := false;
      for t := 0 to 7 do dregused[t] := true;
      for t := 0 to sl do aregused[t] := true;
      if mc68881 then
        for t := 0 to 7 do fpregused[t] := true;

      if level > 1 then
        begin {generate code to adjust sp}
        jumpx(pseudoinst.oprnds[1], false);
        definelabel(pseudoinst.oprnds[1] - 1);
        if level = 2 then t := 0
        else t := long;
        settempareg(sp);

        case targetopsys of
          unix, apollo:
            settemp(long, relative, sl, 0, false, t - stackoffset, 0,
                    1, unknown);
          vdos:
            settemp(long, relative, sl, 0, false, - stackoffset, 0, 1,
                    unknown);
          end {case} ;

        gen2(lea, long, tempkey, tempkey + 1);

        if bigcompilerversion then p := @(bignodetable[lastnode - 1]);

        p^.tempcount := stackcounter - keysize; {note: negative tempcount!}

        if blockusesframe then
          begin
          settemp(long, relative, sl, 0, false, blksize + t, 0, 1, unknown);
          settempareg(fp);
          gen2(lea, long, tempkey + 1, tempkey);
          end;

        if level > 2 then
          begin
          settempreg(long, indr, sl);
          settempareg(sl);
          gen2(move, long, tempkey + 1, tempkey);
          end;
        end;
      end;
    definelabel(pseudoinst.oprnds[1]);
  end {pascallabelx} ;


procedure pascalgotox;

{ Generate a pascal goto.  This requires tracing down the static chain to 
  the proper level, or if the target is the global level, restoring the
  stack from the value saved at initialization.
}

  var
    i: integer; {induction var for tracing static chain}

  procedure closerange;
    begin
    if proctable[blockref].opensfile then
      begin
      settemp(long, relative, fp, 0, false, - blksize, 0, 1, unknown);
      gen1(pea, long, tempkey);
      tempkey := tempkey + 1;
      settempimmediate(long, blksize);
      settempreg(long, autod, sp);
      gen2(move, long, tempkey + 1, tempkey);
      tempkey := tempkey + 2;
      callsupport(libcloseinrange);
      settempimmediate(word, 8);  { Clean up stack }
      settempareg(sp);
      gen2(adda, word, tempkey + 1, tempkey);
      end;
    end;

  begin
    clearcontext;
    if pseudoinst.oprnds[2] = level then jumpx(pseudoinst.oprnds[1], false)
    else
      begin
      if (targetopsys = vdos) or (targetopsys = apollo) then
        begin
        if (switchcounters[profiling] > 0) or (switchcounters[debugging] > 0)
           then
          callsupport(libdebugger_goto);
        end
      else if switchcounters[profiling] > 0 then
        callsupport(libdebugger_goto);
      { This call is a profiler/debugger entry point for a non-local goto. }

      if pseudoinst.oprnds[2] > 1 then
        begin
        settempreg(long, indr, sl);
        settempareg(sl);
        for i := level downto pseudoinst.oprnds[2] + 2 do
          gen2(move, long, tempkey + 1, tempkey);
        closerange;
        jumpx(pseudoinst.oprnds[1] - 1, true);
        end
      else
        begin
        closerange;

        case targetopsys of
          unix:
            settempsymbol('p_glbstk');
          vdos:
            settemp(long, relative, gp, 0, false, 8, 0, 1, unknown);
          apollo:
            if switcheverplus[sharecode] then
              begin
              settempadcon(symbol, 0, unknown);
              lastind^.symbolname := 'p_glbstk';
              settempareg(0);
              gen2(move, long, tempkey + 1, tempkey);
              keytable[tempkey].oprnd.m := indr;
              end
            else
              settempsymbol('p_glbstk');
          end {case} ;
        settempareg(sp);
        gen2(move, long, tempkey + 1, tempkey);

        { Clear the frame pointer so walkback will not blow up.
        }
        if blockusesframe then
          begin
          if targetopsys = apollo then
            if switcheverplus[sharecode] then
              begin
              settempadcon(symbol, 0, unknown);
              lastind^.symbolname := 'p_glbfrm';
              settempareg(0);
              gen2(move, long, tempkey + 1, tempkey);
              keytable[tempkey].oprnd.m := indr;
              end
            else
              settempsymbol('p_glbfrm')
          else
            settempimmediate(long, 0);
          settempareg(fp);
          gen2(move, long, tempkey + 1, tempkey);
          end;

        jumpx(pseudoinst.oprnds[1], true);
        end;
      end;
  end {pascalgotox} ;

procedure addstrx;

{ Concatenate two strings.  If possible we target the operation to
  its final resting place, otherwise on the stack.  Front end passes
  the maximum possible length of the operation as the length of the
  concatenate operator.  If we must allocate space on the stack, we
  use this constant rather than attempt to dynamically generate the
  proper length.  This simplifies stack cleanup, producing slightly
  faster code at the expense of potentially greater stack space.

  Expressions of the form x := x + y are optimized to avoid the overhead
  of moving data to itself.

  Great pains were taken to optimize concatenations where one operand
  is a constant.  No attempt was made to handle the case where both
  operands are constant because this should be handled in the front-end.
}

  var
    leftlengthreg: keyindex; {holds length operand in register}
    leftlengthkey: keyindex; {holds key pointing to left's length}

    rightlengthreg: keyindex; {holds length operand in register}
    rightlengthkey: keyindex; {holds key pointing to right's length}

    remainderreg: keyindex; { holds comparand }
    counterreg: keyindex; { if not twoaddress, holds counter register for left }
    srcreg, destreg: keyindex; { used to point to start of string text }
    tlen: 0..maxstrlen; { for diddling length fields }
    twoaddress: boolean; { true if x := x + y form }
    left_const: boolean; { true if not twoaddress and left is constant }
    left_length: integer; { Length of left if a constant after 
                                  truncatation, if needed }
    right_const: boolean; { true if right is constant }
    right_length: integer; { length of right if a constant }


  procedure load_length(key, dst: keyindex);

    var
      tlen: integer;

    begin
    if keytable[key].oprnd.m = immediate then
      gensimplemove(key, dst)
    else
      begin
      tlen := keytable[key].len;
      keytable[key].len := word;
      gensingle(clr, dst);
      keytable[key].len := byte;
      gensimplemove(key, dst);
      keytable[key].len := tlen;
      end;
    end;


  begin {addstrx}
    saveactivekeys; { This routine needs lots of registers }

    if equivaddr(right, target) then target := 0
    else if target <> 0 then keytable[key].len := keytable[target].len;
    addressboth;
    lock(left);

    lock(right);
    loadstack(left);

    { Set flags to generate better code.
    }
    twoaddress := equivaddr(left, key);
    left_const := not twoaddress and (keytable[left].oprnd.m = pcrelative);

    if left_const then
      left_length := get_stringfile_byte(keytable[left].oprnd.offset)
    else left_length := keytable[left].len - 1;

    { Set up a key that points to left's length field if not a constant or
      to an immediate if it is a constant.  Saves a lot of checking.
    }
    if not twoaddress then
      begin
      if left_const then
        begin
        settempimmediate(word, left_length);
        leftlengthreg := tempkey;
        leftlengthkey := tempkey;
        end
      else { not left_const }
        begin
        with keytable[left],oprnd do
          settemp(byte, m, reg, indxr, false, offset, 0, scale,
                  commonlong_reloc);
        leftlengthkey := tempkey;
        lock(leftlengthkey);

        settempdreg(word, getdreg);
        leftlengthreg := tempkey;
        lock(leftlengthreg);
        end;
      end
    else { twoaddress }
      begin
      settempdreg(word, getdreg);
      leftlengthreg := tempkey;
      lock(leftlengthreg);
      end;

    aligntemps;
    forcerelative(key, true, not twoaddress, 0, false);
    lock(key);

    settempareg(getareg);
    destreg := tempkey;

    if twoaddress then
      begin { Compute the address in key where right will start }
      load_length(left, leftlengthreg);

      with keytable[key].oprnd do
        begin
        if ((m = relative) and mc68020) or
           ((m = relative) and (abs(offset + 1) <= 127)) then
          begin { convert relative to indexed }
          settemp(byte, indexed, reg, keytable[leftlengthreg].oprnd.reg, false,
                  offset + 1, 0, scale, commonlong_reloc);
          gen2(lea, long, tempkey, destreg);
          tempkey := tempkey + 1;
          end
        else
          begin
          offset := offset + 1;
          gen2(lea, long, key, destreg);
          offset := offset - 1;
          gen2(adda, word, leftlengthreg, destreg);
          end;
        end; { with }
      end
    else {not twoaddress}
      begin
      settempdreg(word, getdreg);
      counterreg := tempkey;
      lock(counterreg);

      if keytable[key].len - 1 < left_length then { must truncate }
        if left_const then
          begin

          { It's a constant string, handle any truncation now.  Store the
            length in leftlengthreg and the DBRA trip count in counterreg.
          }
          left_length := keytable[key].len - 1;
          settempimmediate(word, left_length);
          leftlengthreg := tempkey;
          settempimmediate(word, left_length - 1); { -1 for DBRA }
          gensimplemove(tempkey, counterreg);
          tempkey := tempkey + 1;
          end
        else { not left_const }
          begin
          load_length(left, leftlengthreg);
          settempimmediate(long, keytable[key].len - 1);
          gendouble(cmp, tempkey, leftlengthreg);
          genbr(bls, lastlabel);
          gensimplemove(tempkey, leftlengthreg);
          tempkey := tempkey + 1;
          definelastlabel;
          gensimplemove(leftlengthreg, counterreg);
          end
      else { no truncation needed }
        begin
        if left_const then
          begin
          left_length := keytable[leftlengthkey].oprnd.offset;
          settempimmediate(word, left_length - 1);
          gensimplemove(tempkey, counterreg);
          tempkey := tempkey + 1;
          end
        else { not left_const }
          begin
          load_length(leftlengthkey, leftlengthreg);
          gensimplemove(leftlengthreg, counterreg);
          end;
        end;

      if not left_const then unlock(leftlengthkey);

      if not left_const or (left_const and (left_length > 0)) then
        begin

        { Generate relative pointers to first byte of string for dst.
          The move loop below may be a nop at runtime, so the dst pointer
          must be set up here.
        }
        with keytable[key].oprnd do offset := offset + 1;
        gen2(lea, long, key, destreg);
        with keytable[key].oprnd do offset := offset - 1;

        if not left_const then
          begin
          settempimmediate(word, 1);
          gendouble(sub, tempkey, counterreg); { DBRA goes n + 1 times }
          tempkey := tempkey + 1;
          genbr(blo, lastlabel - 1); { null src case }
          end;

        { Generate relative pointers to first byte of string for src.
        }
        lock(destreg);
        settempareg(getareg);
        srcreg := tempkey;
        with keytable[left].oprnd do offset := offset + 1;
        gen2(lea, long, left, srcreg);
        with keytable[left].oprnd do offset := offset - 1;
        unlock(destreg);

        definelastlabel; { top of loop }
        keytable[srcreg].oprnd.m := autoi;
        keytable[destreg].oprnd.m := autoi;
        gen2(move, byte, srcreg, destreg); { move the string }
        gendb(dbra, counterreg, lastlabel + 1); { decrement and branch }

        definelastlabel; { target of null case }
        end; { not left_const or (left_const and (left_length > 0)) }
      unlock(counterreg);
      end; { not twoaddress }

    unlock(left);
    lock(destreg);

    unlock(right);

    { Set up a key that points to right's length field if not a constant or
      to an immediate if it is a constant.  This is the same as for left above.
    }
    if keytable[right].oprnd.m = pcrelative then
      begin
      right_const := true;
      right_length := get_stringfile_byte(keytable[right].oprnd.offset);
      settempimmediate(word, right_length);
      end
    else
      begin
      right_const := false;
      right_length := keytable[right].len - 1;
      with keytable[right],oprnd do
        settemp(byte, m, reg, indxr, false, offset, 0, scale,
                commonlong_reloc);
      end;

    rightlengthkey := tempkey;
    lock(rightlengthkey);
    lock(right);

    { Allocate register to hold right's length.
    }
    settempdreg(word, getdreg);
    rightlengthreg := tempkey;
    lock(rightlengthreg);

    { The following test implements the string overflow check that we
      include for compatibility with MS-DOS' TURBO Pascal strings.
    }
    if left_length + right_length > maxusbyte then
      begin
      load_length(rightlengthkey, rightlengthreg);

      { The pointer to left may have been destroyed, so use the length in
        the register.
      }
      if not left_const then leftlengthkey := leftlengthreg;

      gen2(add, byte, leftlengthkey, rightlengthreg);
      genbr(bhs, lastlabel);
      callsupport(libstrovr);
      definelastlabel;
      end;

    { If sum of max(left) and max(right) will fit in max(key) then we don't
      need a runtime check.  The length of "key" includes the length byte.
    }
    if left_length + right_length > keytable[key].len - 1 then
      begin
      load_length(rightlengthkey, rightlengthreg); { Pick up right's length
                                                     byte }
      settempdreg(word, getdreg);
      remainderreg := tempkey;
      settempimmediate(word, keytable[key].len - 1);
      gensimplemove(tempkey, remainderreg);
      tempkey := tempkey + 1;
      gendouble(sub, leftlengthreg, remainderreg);
      gendouble(cmp, remainderreg, rightlengthreg);
      genbr(bls, lastlabel);
      gensimplemove(remainderreg, rightlengthreg);
      definelastlabel;

      { Compute the new length.
      }
      tlen := keytable[key].len;
      keytable[key].len := byte;
      unlock(key);

      if not twoaddress then gendouble(move, leftlengthreg, key);
      gendouble(add, rightlengthreg, key);

      keytable[key].len := tlen;
      end
    else { no truncation needed }
      begin
      tlen := keytable[key].len;
      keytable[key].len := byte;
      unlock(key);

      { Compute the new length.
      }
      if not twoaddress then gendouble(move, leftlengthreg, key);

      if right_const then
        begin
        gendouble(add, rightlengthkey, key);
        settempimmediate(word, right_length - 1); { - 1 for DBRA }
        gensimplemove(tempkey, rightlengthreg); { Pick up right's length byte }
        tempkey := tempkey + 1;
        end
      else
        begin
        load_length(rightlengthkey, rightlengthreg); { Pick up right's length
                                                       byte }
        gendouble(add, rightlengthreg, key);
        end;

      keytable[key].len := tlen;
      end;

    unlock(rightlengthkey);

    if not right_const or (right_const and (right_length > 0)) then
      begin
      { If the right operand was not a constant or if it was a constant
        and a compare was generated above, then we must generate a
        subtract.  In this second case we could reload the register
        with a new constant, but a subtract is just as good.
        FR6179w  STH  1-17-90
      }
      if not right_const or (right_const and
        (left_length + right_length > keytable[key].len - 1)) then
        begin
        settempimmediate(byte, 1);
        gendouble(sub, tempkey, rightlengthreg); { DBRA goes n + 1 times }
        tempkey := tempkey + 1;
        genbr(blo, lastlabel - 1); { null src case }
        end;

      { Generate relative pointer to first byte of string for right.
      }
      settempareg(getareg);
      srcreg := tempkey;
      with keytable[right].oprnd do offset := offset + 1;
      gen2(lea, long, right, srcreg);
      with keytable[right].oprnd do offset := offset - 1;

      definelastlabel; { top of loop }
      keytable[srcreg].oprnd.m := autoi;
      keytable[destreg].oprnd.m := autoi;
      gen2(move, byte, srcreg, destreg); { move the string }
      gendb(dbra, rightlengthreg, lastlabel + 1); { decrement and branch }
      end;

    definelastlabel; { target of null case }
    unlock(destreg);
    unlock(rightlengthreg);
    unlock(right);

    if twoaddress or not left_const then unlock(leftlengthreg);

    tempkey := loopcount - 1; { put back all the temps }
  end; {addstrx}



procedure makeroomx;

{ Make space for a function or procedure call. The len operand is the
  literal length of the needed space for the return value, and is 0 for
  procedures, since only functions can really create a value.
  Also used to make space for return values from read operations.
}


  begin
    saveactivekeys;

{    if (pseudoinst.oprnds[3] = 0) and
       not proctable[pseudoinst.oprnds[1]].pascallinkage then
      begin
      markareg(0);
      markareg(1);
      markdreg(0);
      markdreg(1);
      end;
}
    newtemp(len);
    aligntemps;
    if key <> 0 then setvalue(relative, sp, 0, false, - stackoffset, 0);
    keytable[stackcounter].knowneven := true;
    keytable[stackcounter].refcount := 1;
    dontchangevalue := dontchangevalue + 1;
  end {makeroomx} ;


procedure callroutinex(s: boolean {signed function value} );

{ Generate a call to a user procedure. There are two possibilies:  if
  target is non-zero, then keytable[left] is a procedure parameter,
  and we call the routine by loading the static link register with the
  passed environment pointer and call the routine.  If target is 0, we
  are calling an explicit routine, the left'th procedure (named Pleft).
  Proctable contains interesting information as to whether or not the
  procedure is external and uses pascal linkage.

  We use the offset1 field of the usercall node to indicate how
  many "movl (A4),A4" instructions to use on entry to the called
  routine.  This causes the generated routine's address to be offset by
  - offset1 * 2.

  ** UNIX **
  If the procedure was declared non-Pascal, we must copy the parameter
  list to pass parameters right to left, and make sure each parameter is
  one longword, except reals, which must be double.  If the call is to a
  function, we also have to copy the function return value from register 0
  (and maybe 1) to the stack location reserved for it.
}

  var
    linkreg: boolean; {true if we build a static link}
    levelhack: integer; {if linkreg then we are going up levelhack levels}
    slkey: keyindex; {tempkey holding static link descriptor}
    framekey: keyindex; {tempkey holding address of base of current frame}
    spkey: keyindex; {tempkey holing stack pointer descriptor}
    param: integer; {parameter count for creating unix standard list}
    notcopied: 0..1; {1 if last parameter was already the right size}
    i: keyindex; {"from" key for copying parameters}
    inreg: boolean; {no result on the stack - result is in a register}

  const
    reverse_params = false; { true if nonpascal parameters are to be reversed
                              here, false if front-end does it. }

  begin {callroutinex}
    paramlist_started := false; {reset the switch}
    settemp(long, relative, fp, 0, false, - blksize - long, 0, 1, unknown);
    framekey := tempkey;
    settempareg(sl);
    slkey := tempkey;
    settempareg(sp);
    spkey := tempkey;

    levelhack := 0;
    notcopied := 0;
    inreg := false;

    if pseudoinst.oprnds[3] <= 0 then
      begin
      linkreg := proctable[pseudoinst.oprnds[1]].intlevelrefs and
                 (proctable[pseudoinst.oprnds[1]].level > 2);

      if linkreg then
        begin
        levelhack := level - proctable[pseudoinst.oprnds[1]].level;
        if levelhack < 0 then
          begin
          if level = 2 then keytable[framekey].oprnd.offset := - blksize;

          { Handle long relative offset (big frame).  68020 uses long mode rel.
          }
          if not mc68020 and (abs(keytable[framekey].oprnd.offset) > $7FFF)
          then
            begin
            settempareg(fp);
            gen2(move, long, tempkey, slkey);
            settempimmediate(long, keytable[framekey].oprnd.offset);
            gen2(add, long, tempkey, slkey);
            end
          else gen2(lea, long, framekey, slkey);

          aregused[sl] := true;
          end;
        end;

      if proctable[pseudoinst.oprnds[1]].calllinkage <> pascal2call then
        begin {nonpascal}
        markareg(0);
        markareg(1);
        markdreg(0);
        markdreg(1);

        if targetopsys = apollo then
          begin
          if len > long then
            begin
            newtemp(long);
            settemp(long, relative, sp, 0, false, keytable[stackcounter +
                    pseudoinst.oprnds[2] + 1].oprnd.offset, 0, 1, unknown);
            gen1(pea, long, tempkey);
            stackoffset := - keytable[stackcounter].oprnd.offset;
            end;
          end
        else
          begin
          if (pseudoinst.oprnds[2] > 0) and
             ((keytable[stackcounter].len = long) or
              { (keytable[stackcounter].oprnd.flavor = floatx) and }
              (keytable[stackcounter].len = quad)) then
            notcopied := 1;

          if reverse_params then
            for param := 1 to pseudoinst.oprnds[2] - notcopied do
              begin {reverse (and extend) parameters on stack}
              i := stackcounter + param * 2 + notcopied - 2;
              if (targetopsys = unix) and
                 { (keytable[i].oprnd.flavor = floatx) and }
                 (keytable[i].len = quad) then
                newtemp(quad)
              else newtemp(long);
              if keytable[stackcounter].len = quad then
                genblockmove(i, stackcounter, long)
              else if keytable[i].len = long then
                gensimplemove(i, stackcounter)
              else if keytable[i].len <= word then
                begin
                extend(i, long);
                gensimplemove(i, stackcounter);
                end
              else if keytable[i].len < long then {it's three bytes long!}
                begin
                settempimmediate(long, 0);
                gensimplemove(tempkey, stackcounter);
                with keytable[stackcounter].oprnd do
                  begin
                  offset := offset + long - keytable[i].len;
                  genblockmove(i, stackcounter, byte);
                  offset := offset - long + keytable[i].len;
                  end;
                end
              else { len > long }
                begin
                gen1(pea, long, fix_effective_addr(i));
                stackoffset := stackoffset + long;
                end;
              tempkey := spkey;
              end {for} ;
          end {not apollo} ;
        end {nonpascal} ;

      { now make sure any allocated stack space will be reserved }
      aligntemps; {important for a non-pascal function without parameters}

      if switcheverplus[sharecode] then
        begin
        if proctable[pseudoinst.oprnds[1]].externallinkage then
          settempadcon(usercall, pseudoinst.oprnds[1], unknown)
        else
          settempadcon(usercall, pseudoinst.oprnds[1], impure_section);
        settempareg(getareg);
        gensimplemove(tempkey + 1, tempkey);
        keytable[tempkey].oprnd.m := relative;
        keytable[tempkey].oprnd.offset := - max(0, levelhack * word);
        end
      else if pic_enabled and not mc68020 then
        begin
        settempareg(getareg);
        settemp(long, pic_splat_pcrel, 0, 0, false, 10, 0, 1, unknown);
        gen2(lea, long, tempkey, tempkey + 1);
        settemp(word, pic_usercall, 0, 0, false, pseudoinst.oprnds[1],
                6 + word * max(0, levelhack), 1, unknown);
        gen2(adda, long, tempkey, tempkey + 2);
        tempkey := tempkey + 2;
        keytable[tempkey].oprnd.m := indr;
        end
      else settemp(word, usercall, 0, 0, false, pseudoinst.oprnds[1],
                   - word * max(0, levelhack), 1, unknown);
      gensingle(jsr, tempkey);
      end
    else { It's a procedure parameter }
      begin
      address(left);
      if keytable[left].len = quad then
	begin
	linkreg := true;
	tempkey := tempkey - 1;
	keytable[tempkey].oprnd := keytable[left].oprnd;
	keytable[tempkey].oprnd.offset := keytable[tempkey].oprnd.offset + long;
	keytable[tempkey].len := long;
	end
      else linkreg := false;
      lock(left);
      settempareg(getareg);
      gensimplemove(left, tempkey);
      keytable[tempkey].oprnd.m := indr;
      unlock(left);
      if linkreg then
	begin
	gensimplemove(tempkey + 1, slkey);
	aregused[sl] := true;
	end;
      gensingle(jsr, tempkey);
      end;

    if (pseudoinst.oprnds[3] <= 0) and
       (proctable[pseudoinst.oprnds[1]].calllinkage <> pascal2call) then
      begin
      if targetopsys = apollo then
        begin
        if len > long then
          popstack(pseudoinst.oprnds[2] + 1)
        else
          begin
          popstack(pseudoinst.oprnds[2]);
          if len > 0 then
            begin {convert to register return value}
            keytable[stackcounter].refcount := 0;
            keytable[stackcounter].tempflag := false;
            adjusttemps; {in case of value conformant array, etc.}
            setvalue(dreg, 0, 0, false, 0, 0);
            savekey(key);
            inreg := true;
            end;
          end;
        end
      else
        begin
        if reverse_params then
          popstack(max(pseudoinst.oprnds[2] * 2 - notcopied, 0))
        else popstack(pseudoinst.oprnds[2]);

        if keytable[stackcounter].len > 0 then
          if keytable[stackcounter].len = quad then
            with keytable[stackcounter].oprnd do
              begin
              settempreg(long, dreg, 1);
              offset := offset + 4;
              gensimplemove(tempkey, stackcounter);
              keytable[tempkey].oprnd.reg := 0;
              offset := offset - 4;
              gensimplemove(tempkey, stackcounter);
              end
          else
            begin
            { this won't handle 3-byte function results }
            settempreg(long, dreg, 0);
            if (pseudobuff.op = movint) and (pseudobuff.oprnds[2] = key) and
               (keytable[key].refcount = 1) then
              begin
              setkeyvalue(tempkey);
              inreg := true;
              end
            else gensimplemove(tempkey, stackcounter);
            end;
        end;
      end
    else popstack(pseudoinst.oprnds[2]);

    with keytable[stackcounter] do
      begin
      refcount := 0;
      tempflag := not inreg;
      context[contextsp].lastbranch := max(0, min(context[contextsp].lastbranch,
                                                  instmark - 1));
      end {with} ;

    with keytable[key] do
      begin
      if not inreg then
        begin
        keytable[key].properreg := stackcounter;
        keytable[key].tempflag := true;
        keytable[key].regsaved := true;

        { loopholefnx is using movb instructions to move function return
          values off of the stack because knowneven is not being set true
          here.   So use setkeyvalue instead of setvalue because setkeyvalue
          sets knowneven.
        }
        setkeyvalue(stackcounter);
        end;
      signed := s;
      end {with} ;

    { Restore the static link register, if needed.
    }
    if linkreg and ((pseudoinst.oprnds[3] > 0) or (level > 2) and
       (levelhack <> 0)) then
      begin
      aregused[sl] := true;

      { Handle long relative offset (big frame).  68020 uses long mode rel.
      }
      if not mc68020 and (abs(keytable[framekey].oprnd.offset) > $7FFF) then
        begin
        settempimmediate(long, keytable[framekey].oprnd.offset);
        gen2(move, long, tempkey, slkey);
        settemp(long, indexed, fp, sl, true, 0, 0, 1, unknown);
        gen2(move, long, tempkey, slkey);
        end
      else gendouble(move, framekey, slkey);
      end;

    dontchangevalue := dontchangevalue - 1;
  end {callroutinex} ;


{ Case statement generation.

  The general scheme is to generate a case branch followed directly by
  as many caseelt's as needed.  Tying a caseelt to the code for that case
  is done by the labels generated
}


procedure casebranchx;

{ Generate code for a case branch.  The pseudoinstruction field have the
  following meanings:

  target:       Case expression

  len:          Default label

  refcount:          0, no error check; 1, default label is error

  oprnds[1]:    Lower bound of cases

  oprnds[2]:    Upper bound of cases

  The code generated is:

     ;move selection expression into Dn
             sub.w   #lower,Dn       ;skew to range 0..(upper-lower)
             cmpi.w  #(upper-lower),Dn       ;range test
     if "otherwise" exists, or no error checking (refcount = 0) then:
             bls.?   <otherwiselimb> ;condition = (C+Z); short/long branch
     else no "otherwise" and error checking on (refcount = 1):
             bhi.s   templabel       ;condition = not(C+Z)
             jsr     caseerror       ;"case selector matches no label"
         templabel:                  ;target of short branch around error
     ;fi
             add.w   Dn,Dn           ;make word address
             move.w  6(PC,Dn.w),Dn   ;fetch 16-bit offset, reusing Dn
             jmp     2(PC,Dn.w)      ;dispatch to selected case limb
     table:  <word offsets of form "caselimb - table">

     note: this schema presumes that a data reference to program space
     is allowed by the host operating system (VERSAdos now allows it).

}

  var
    t: keyindex; {case expression}
    default: integer; {default label}
    span: unsignedint; {number of elements in branch table}
    errordefault: boolean; {true if error label defines error}


  begin {casebranchx}
    t := target;
    target := 0;
    errordefault := (pseudoinst.refcount <> 0);
    unpack(t, word);
    default := len;
    key := tempkey; {using multarget as a scratch key}
    keytable[tempkey].len := keytable[t].len;
    loaddreg(t, 0, true);
    with keytable[key] do
      begin
      adjusttemps;
      settempimmediate(targetintsize, pseudoinst.oprnds[1]);
      gendouble(sub, tempkey, key);
      span := pseudoinst.oprnds[2] - pseudoinst.oprnds[1];
      if span <> 0 then
        begin
        settempimmediate(targetintsize, span); 
        gendouble(cmp, tempkey, key);
        end;
      if errordefault then
        begin
        if pseudoinst.oprnds[2] = pseudoinst.oprnds[1] then
          genrelbr(beq, ord(switcheverplus[sharecode]) + 1)
        else genrelbr(bls, ord(switcheverplus[sharecode]) + 1);

        definelabel(default);

        case targetopsys of
          unix, apollo:
            callsupport(libcasetrap);  { case error }
          vdos:
            begin
            settempimmediate(word, casetrap);
            gensingle(trap, tempkey);
            end;
          end;

        labelnextnode := true;
        end
      else if pseudoinst.oprnds[2] = pseudoinst.oprnds[1] then
        genbr(bne, default)
      else genbr(bhi, default);
      keytable[key].len := word;
      if pseudoinst.oprnds[2] <> pseudoinst.oprnds[1] then
        gendouble(add, key, key);
      settemp(word, pcindexed, 0, keytable[key].oprnd.reg, false, 6, 0, 1,
              unknown);
      gensimplemove(tempkey, key);
      keytable[tempkey].oprnd.offset := 2;
      gensingle(jmp, tempkey);

      { Kludge strictly for Sys V assemblers.  They need a special
        pseudo instruction of the form "swbeg &span", where span is
        the number of entries in the case branch table, for some
        obscure, barely documented reason.  So we save the value in
        the normally unused offset2 field of the pcindexed operand.
      }

      lastptr^.oprnd.offset2 := span + 1; {was calculated zero-based}

      end;
    definelastlabel;
  end {casebranchx} ;


procedure caseeltx;

{ Generate oprnds[2] references to label oprnds[1].
  These will be placed in the constant psect.
}

  var
    i: integer; {induction var}


  begin
    for i := 1 to pseudoinst.oprnds[2] do
      genlabeldelta(lastlabel + 1, pseudoinst.oprnds[1]);
  end {caseeltx} ;


procedure caseerrx;

{ Generate a case error trap
}
  begin
    case targetopsys of
      unix:
	callsupport(libcasetrap);  { case error }
      vdos:
	begin
	settempimmediate(word, casetrap);
	gensingle(trap, tempkey);
	end;
      end;
  end; {caseerrx}



procedure stmtbrkx;

{ Generate a statement break for the debugger and runtime diagnostics
}


  begin
    case targetopsys of
      unix:
        if (switchcounters[profiling] > 0) then
          callsupport(libdebugger_step);
      vdos:
        if (switchcounters[debugging] > 0) or (switchcounters[profiling] > 0)
           then
          begin
          { The debugger used to load A3 with the debugger step entry point and 
            A3 was considered a global register.  This doesn't work when some
            modules are compiled debug and some aren't.  The current solution is
            to set A3 aside unused as before, generate the calls with references
            to the debugger and let stuffregisters load A3 and fixup the calls.
          }
          callsupport(libdebugger_step)

{          if pic_enabled then callsupport(libdebugger_step)
          else
            begin
            settempreg(long, indr, debuglink);
            gensingle(jsr, tempkey);
            end;
}
          end;
      apollo:
        if (switchcounters[debugging] > 0) or (switchcounters[profiling] > 0)
           then
          callsupport(libdebugger_step);
      end {case} ;

    if firststmt = 0 then firststmt := pseudoinst.oprnds[1];
    newnode;
    with lastptr^ do
      begin
      tempcount := 0; {for node dump only}
      kind := stmtref;
      stmtno := pseudoinst.oprnds[1];
      current_stmt := pseudoinst.oprnds[1];
      sourceline := pseudoinst.oprnds[2];
      current_line := pseudoinst.oprnds[2] - lineoffset;
      flags := pseudoinst.oprnds[3];
      filename := len;
      if targetopsys = unix then
        if (switchcounters[debugging] > 0) and (flags and 4 <> 0) then
          callsupport(libdbgtrap);
      end;
  end; {stmtbrkx}


procedure dummyargx;

  { The special 68881 functions and procedures that have more than one argument
    use this pseudo-op to pass all their arguments.  The keys are stacked here
    and dealt with in the sysfunction.
  }

  begin {dummyargx}
    dummyarg_stack[dummyarg_ptr] := left;
    dummyarg_ptr := dummyarg_ptr + 1;
    keytable[key].refcount := 0; { keeps setcommonkey happy }
  end; {dummyargx}


procedure dummyarg2x;

  { The special 68881 functions that look like arithmetic operations (fmod,
    frem, fsgldiv, fsglmul) use this pseudo-op to pass both arguments.
    The keys are stacked here and dealt with in the sysfunction.
  }

  begin {dummyarg2x}
    dummyarg_stack[0] := left;
    dummyarg_stack[1] := right;
    dummyarg_ptr := 2;
    keytable[key].refcount := 0; { keeps setcommonkey happy }
  end; {dummyarg2x}

procedure copy_openarrays;

{
  This routine implements callee copy of open array parameters for Modula-2. 

  The input to this routine is a linked list of openarray parameters to
  copy.  A node in the list contains these fields:

    dimensions -     the number of dimensions
    param_offset -   the offset off the frame pointer off the caller's
                     copy of the array
    element_length - the element size of the array

  The high bounds (zero based) for each dimension are pushed on the
  stack below the base pointer.  This routine generates code to copy
  each the open array parameter to the stack below the locals and
  then alters the base pointer to point to the new stack location.
}

  var
    dim: shortint; { for loop index on dimensions }
    counterreg, srcreg, destreg, temp_reg1, temp_reg2: keyindex;
    shortcut: boolean;
    op_length: unsigned; { length of the move operand }
    top_of_stack_reg: keyindex;
    p, tp: openarraynodeptr;
    temp_reg1_allocated: boolean; { flag to prevent extra register allocation }
    i: shortint; { for loop index }

  begin {copy_openarrays}
    if language = modula2 then
      { This is the new "improved", "faster", "better" library routine copy
        hack.  The inline code has been left in for when "they" decide to
        go back to inline. :-)
      }
      if true then
        begin
	{ No "address" is needed because the arguments are constants.
	}

        { All routines destroy d0, d1, a0 and a1.  A2 is used to hold
          our stack pointer.
        }
        markdreg(0);
        markareg(0);
        markareg(1);
        markareg(2); 

	{ Create a pointer in an aregister to the top of the stack.  This
	  will be pushed onto the stack after the last open array and used
	  in the procedure's epilog to remove the open arrays from the
	  stack.
	}
	settempareg(2);
	top_of_stack_reg := tempkey;
	lock(top_of_stack_reg);
	settempareg(sp);
	gen2(move, long, tempkey, top_of_stack_reg);

	p := openarray_base;

	with p^ do
	  while p <> nil do
	    begin
	    settemp(long, relative, fp, 0, false,
		    param_offset - blksize, 0, 1, unknown);
	    settempareg(0);
	    gen2(lea, long, tempkey + 1, tempkey);

            if (dimensions = 1) and ((element_length = 1) or
               (element_length = 2) or (element_length = 4)) then
              begin
              case element_length of
                1: callsupport(libmcopy1);
                2: callsupport(libmcopy2);
                4: callsupport(libmcopy4);
                end;
              end
            else { must use the general case }
              begin
              settempimmediate(long, dimensions);
              settempdreg(long, 0);
              gen2(move, long, tempkey + 1, tempkey);

              settempimmediate(long, element_length);
              markdreg(1);
              settempdreg(long, 1);
              gen2(move, long, tempkey + 1, tempkey);

              callsupport(libmcopymd);
              end;

	    { Advance the pointer and dispose of this entry.
	    }
	    tp := p;
	    p := nextnode;
	    dispose(tp);
            tempkey := top_of_stack_reg; { Remove excess temps }
	    end;

	{ Push the pointer to remove the open arrays.
	}
	settempreg(long, autod, sp);
	gen2(move, long, top_of_stack_reg, tempkey);
	unlock(top_of_stack_reg);

	tempkey := loopcount - 1; { put back all the temps }
        end
      else { not used }
	begin
	{ No "address" is needed because the arguments are constants.
	}

	{ Create a pointer in an aregister to the top of the stack.  This
	  will be pushed onto the stack after the last open array and used
	  in the procedure's epilog to remove the open arrays from the
	  stack.
	}
	settempareg(getareg);
	top_of_stack_reg := tempkey;
	lock(top_of_stack_reg);
	settempareg(sp);
	gen2(move, long, tempkey, top_of_stack_reg);
	tempkey := tempkey + 1;

	if mc68020 then settempdreg(long, getdreg)
	else
	  begin
	  markdreg(4);
	  settempdreg(long, 4);
	  end;

	counterreg := tempkey;
	lock(counterreg);

	temp_reg1_allocated := false;
	p := openarray_base;

	with p^ do
	  while p <> nil do
	    begin
	    shortcut := false;

	    { Decide if we can move words or longs or just bytes.
	    }
	    if mc68020 and ((element_length = word) or
			    (element_length = long)) then
	      begin
	      if dimensions = 1 then shortcut := true;
	      op_length := element_length;
	      end
	    else
	      begin
	      if (element_length = 1) and (dimensions = 1) then
		begin
		shortcut := true;
		op_length := element_length;
		end
	      else
		begin
		settempimmediate(long, element_length);
		gen2(move, long, tempkey, counterreg); { load element size }
		tempkey := tempkey + 1;
		op_length := byte;
		end;
	      end;

	    { Only allocate this scratch register if needed, but once allocated
	      each open array will use the same scratch register.
	    }
	    if not temp_reg1_allocated then
	      begin
	      if mc68020 then
		begin
		if (op_length = 1) or not shortcut then
		  settempdreg(long, getdreg);
		end
	      else
		begin
		markdreg(3);
		settempdreg(long, 3);
		end;

	      temp_reg1 := tempkey;
	      lock(temp_reg1);
	      temp_reg1_allocated := true;
	      end;

	    if shortcut then temp_reg2 := counterreg
	    else temp_reg2 := temp_reg1;

	    for dim := 1 to dimensions do
	      begin
	      settemp(long, relative, fp, 0, false,
		      param_offset - dim * long - blksize, 0, 1, unknown);
	      gen2(move, long, tempkey, temp_reg2);
	      tempkey := tempkey + 1;

	      if not(mc68020 and shortcut and (op_length > 1)) then
		begin
		settempimmediate(byte, 1);
		gen2(add, long, tempkey, temp_reg2);
		tempkey := tempkey + 1;
		end;

	      { Save a multiply if possible.
	      }
	      if not shortcut then
		if mc68020 then
		  if (dim = 1) and ((element_length = 1) or (op_length = word) or
		     (op_length = long)) then
		    gen2(move, long, temp_reg1, counterreg)
		  else gen2(mulu, long, temp_reg1, counterreg)
		else {mc68020}
		  if element_length = 1 then gen2(move, long, temp_reg1, counterreg)
		  else callsupport(libunsmult);
	      end; {for}

	    { Generate a relative pointer to the source.
	    }
	    settempareg(getareg);
	    srcreg := tempkey;
	    settemp(long, relative, fp, 0, false, param_offset - blksize, 0, 1,
		    unknown);
	    gen2(move, long, tempkey, srcreg);
	    tempkey := tempkey + 1;

	    { If the operand size is not byte, advance to the end of the
	      source so we can use auto decrement.  If the operand size
	      is byte we must use auto increment.
	    }
	    if op_length > 1 then
	      begin
	      if mc68020 then
		begin
		settemp(long, indexed, keytable[srcreg].oprnd.reg,
			keytable[counterreg].oprnd.reg, true,
			ord(shortcut) * op_length, { save add 1 ... sub 1 }
			0, op_length, unknown);
		gen2(lea, long, tempkey, srcreg);
		tempkey := tempkey + 1;
		end
	      else
		for i := 1 to op_length do
		  gen2(adda, long, counterreg, srcreg);

	      settempareg(getareg);
	      srcreg := tempkey;
	      settempareg(sp);
	      destreg := tempkey;
	      end
	    else
	      { Make a hole on the stack.  Remember that the stack must always
		remain byte aligned.
	      }
	      begin
	      lock(srcreg);
	      settempareg(getareg);
	      unlock(srcreg);
	      destreg := tempkey;
	      gen2(move, long, counterreg, temp_reg1); {temp_reg1 is d3 for 68000}
	      { Add one to round }
	      settempimmediate(long, 1);
	      gen2(add, long, tempkey, temp_reg1);
	      { Clear low order bit }
	      settempimmediate(long, $fffe);
	      gen2(andinst, word, tempkey, temp_reg1);
	      settempareg(sp);
	      gen2(sub, long, temp_reg1, tempkey);
	      gen2(move, long, tempkey, destreg);
	      tempkey := tempkey + 3;
	      end;

	      if not(mc68020 and shortcut and (op_length > 1)) then
		begin
		settempimmediate(long, 1);
		gen2(sub, long, tempkey, counterreg);
		tempkey := tempkey + 1;
		end;

	    { Generate the loop
	    }
	    definelastlabel; { top of loop }

	    if op_length = 1 then { byte moves must use auto increment }
	      begin
	      keytable[srcreg].oprnd.m := autoi;
	      keytable[destreg].oprnd.m := autoi;
	      end
	    else { all others may use auto decrement }
	      begin
	      keytable[srcreg].oprnd.m := autod;
	      keytable[destreg].oprnd.m := autod;
	      end;

	    gen2(move, op_length, srcreg, destreg); { move the array }
	    gendb(dbra, counterreg, lastlabel + 1); { decrement and branch }

	    settempimmediate(long, $10000);
	    gen2(sub, long, tempkey, counterreg);
	    tempkey := tempkey + 1;
	    genbr(bhs, lastlabel + 1);

	    { Plug the pointer in the parameter list to point to the new
	      stack copy.
	    }
	    settempareg(sp);
	    settemp(long, relative, fp, 0, false, param_offset - blksize, 0, 1,
		    unknown);
	    gen2(move, long, tempkey + 1, tempkey);

	    { Advance the pointer and dispose of this entry.
	    }
	    tp := p;
	    p := nextnode;
	    dispose(tp);

	    if temp_reg1_allocated then tempkey := temp_reg1
	    else tempkey := counterreg;
	    end;

	unlock(temp_reg1);
	unlock(counterreg);

	{ Push the pointer to remove the open arrays.
	}
	settempreg(long, autod, sp);
	gen2(move, long, top_of_stack_reg, tempkey);
	unlock(top_of_stack_reg);

	tempkey := loopcount - 1; { put back all the temps }
	end;
  end; {copy_openarrays}


procedure openarrayx;

{
  This routine is part of the implementation of callee copy of open array
  parameters for Modula-2.  Each open array parameter is added to a linked
  list which is processed in blockcodex.

  Left is a constant that is the offset off the frame pointer off the caller's
  copy of the array.  Right is a constant that is the number of dimensions.
  The length of the pseudo-op is the element size of the array.
}

  var
    t: openarraynodeptr;

  begin {openarrayx}
    t := openarray_base;
    new(openarray_base);
    openarray_base^.nextnode := t;

    with openarray_base^ do
      begin
      param_offset := pseudoinst.oprnds[1];
      dimensions := pseudoinst.oprnds[2];
      element_length := len;
      end;
  end; {openarrayx}

{ File operations.  Calls to standard i/o procedures are translated into
  special operators rather than procedure calls.  These procedures implement
  these special file operators.

  To understand these procedures it is necessary to understand the support
  library structure.  File variables are allocated with only a pointer
  on the stack, and the file control block and buffer are allocated by
  the support library.  The status bits for eoln and eof are at specified
  locations in the file control block.  The current buffer variable is
  accessed through a pointer in the first word of the file control block.

  The standard files "input" and "output" are always allocated as the first
  two words in the global area.  The address of the file is pushed on the 
  stack.

  Formatting arguments are provided to the support library by the compiler.

  For other than text files, "read" and "write" are converted into the
  appropriate assignment with a "get" or "put".
}


procedure fmtx;

{ Push a format value onto the stack.  This is an expression "e" in the
  val:e:e form of a write-argument.  A count of the number of format
  parameters is kept since some of them are defaulted, and in the case
  of real output the format depends on the number of arguments.
}


  begin
    pshx;
    formatcount := formatcount + 1;
  end {fmtx} ;


procedure setinpx;

{ Make the location of the standard file "input" available in the current
  key.
}


  begin {setinpx}
    if targetopsys = apollo then
      begin
      if switcheverplus[sharecode] then
        begin
        settempadcon(commonlong, globalbase + long, global_section);
        settempareg(getareg);
        gensimplemove(tempkey + 1, tempkey);
        keytable[tempkey].oprnd.m := indr;
        setkeyvalue(tempkey);
        end
      else
        begin
        setvalue(commonlong, 0, 0, false, globalbase + long, 0);
        keytable[tempkey].oprnd.commonlong_reloc := global_section;
        end;
      end
    else
      setvalue(relative, gp, 0, false, globalbase + long, 0);
  end {setinpx} ;


procedure setfileaddrx;

{ Make the location of the standard file "output" available in the current
  key.
}


  begin {setfileaddrx}
    if targetopsys = apollo then
      begin
      if switcheverplus[sharecode] then
        begin
        settempadcon(commonlong, globalbase, global_section);
        settempareg(getareg);
        gensimplemove(tempkey + 1, tempkey);
        keytable[tempkey].oprnd.m := indr;
        setkeyvalue(tempkey);
        end
      else
        begin
        setvalue(commonlong, 0, 0, false, globalbase, 0);
        keytable[tempkey].oprnd.commonlong_reloc := global_section;
        end;
      end
    else
      setvalue(relative, gp, 0, false, globalbase, 0);
  end {setfileaddrx} ;


procedure closerangex;

{Close files in structure referenced by top two elements on the stack.
 This is called strictly by dispose, and leaves the top of the stack alone.

 Note: generated code only works if "targetintsize" is long.
}

  var
    stackloc: keyindex;


begin {closerangex}
  stackloc := stackcounter;
  if switcheverplus[sharecode] then saveactivekeys;
  settempareg(getareg);
  gensimplemove(stackloc + 1, tempkey);
  newtemp(long);
  keytable[tempkey].oprnd.m := indr;
  gensimplemove(tempkey, stackcounter);
  newtemp(long);
  gensimplemove(stackloc, stackcounter);
  callandpop(libcloseinrange, 2);
end {closerangex} ;


procedure setfilex;

{ Flags beginning of file processing of some sort or another.  "Filenamed"
  is used to differentiate between implicit and explicit files for read/write.
  "Filestkcnt" is used for clearing the stack for reset/rewrite.
}

begin {setfilex}
  filenamed := true;
  filestkcnt := stackcounter;
  dontchangevalue := dontchangevalue + 1;
end {setfilex} ;


procedure setbinfilex;

{ Set target/source for move portion of a binary read/write file operation.
  Net effect is to provide a target equal to the address of the file variable,
  with the reference count bumped by one.
}

begin {setbinfilex}
  setfilex;
  keytable[key].properreg := stackcounter;
  setkeyvalue(stackcounter);
end {setbinfilex} ;


procedure copystackx;

{ Make a copy on the stack of the file address already pushed, if any.  This
  is used to make the int/char/real read routines look like Pascal functions.
}

begin {copystackx}
  saveactivekeys;

  if filenamed and (stackcounter <> filestkcnt) then
    begin
    aligntemps;
    newtemp(long);
    keytable[stackcounter].refcount := 1;
    keytable[stackcounter].tempflag := true;
    gensimplemove(filestkcnt, stackcounter);
    end;
end {copystackx} ;


procedure definelazyx;

{ Checks "defined" bit in file status word.  If not set calls "get".
  File to check is in "left".
}

begin {definelazyx}
  address(left);

  if definelazykluge then
    begin
    settempareg(definelazyklugereg);
    setkeyvalue(tempkey);
    definelazykluge := false;
    end
  else
    begin
    lock(left);
    loadareg(left, 0, true);  { put the filevar in a register }
    unlock(left);
    keytable[key].len := long;
    settempareg(keytable[key].oprnd.reg);
    end;

  with keytable[tempkey], oprnd do begin
    m := relative;
    offset := long;
    end;

  if switcheverplus[sharecode] then saveactivekeys;
  settempimmediate(word, lazybit); 
  gendouble(btst, tempkey, tempkey + 1);
  genbr(bne, lastlabel);
  aligntemps; {just to be sure}
  newtemp(long);
  keytable[stackcounter].refcount := 1;
  gen1(pea, long, fix_effective_addr(left));
  stackoffset := - keytable[stackcounter].oprnd.offset;
  lock(key);
  callandpop(libdefinebuf, 1);
  unlock(key);
  definelastlabel;
end {definelazyx} ;


procedure calliosupport(libroutine: libroutines; {support routine to call}
                        args: integer {number of stacked params} );

{ Calls support library routine to do I/O.  Determines whether to call
  the explicit-file or standard input/output entrypoint, and returns
  the proper number of parameter entries afterwards.

  Depends on "filenamed".
}


  begin {calliosupport}
    if filenamed then callandpop(libroutine, args + 1)
    else callandpop(succ(libroutine), args);
  end {calliosupport} ;


{ Formatted I/O Routines.}


procedure dumpstack;

  var i: integer;


  begin {dumpstack}
    for i := stackcounter to keysize - 2 do 
      with keytable[i], oprnd do begin
        write('sp ', keysize - i - 1 : 1);
        write(', ref ', refcount:1, ', len ', len:1);
        write(', temp ', tempflag, ', instmark=', instmark:1);
        write(', offset ', offset:1);
        writeln;
        end;
    writeln;
  end {dumpstack} ;


procedure rdxstrx;

{ Read an extended string.  Called as:

        procedure rdxstrg(var result: string; length: integer);

  The file argument is either already pushed or is defaulted.
}


  begin {rdxstrx}
    if filenamed then callandpop(libreadxstring, 2)
    else callandpop(libreadxstringi, 2);
  end {rdxstrx} ;


procedure rdintcharx(libroutine: libroutines; {support routine to call}
                     length: datarange);

{ Read a character, integer, or real, used by "rdint", "rdchar" and
  "rdreal".  This is called as a function with one argument
  and leaves the result on the stack.

  The file argument is on the stack.
}


  begin {rdintcharx}
    if len = quad then
      if libroutine = libreadreal then libroutine := libreaddouble;
    if filenamed then callandpop(libroutine, 1)
    else callsupport(succ(libroutine));
    right := stackcounter;
    len := length;
    keytable[right].tempflag := true;
    keytable[right].len := len;
{    dontchangevalue := 0; { no longer in a parameter list }}

    if mc68881 and (pseudoinst.op = rdreal) then
      begin
      keytable[right].oprnd.flavor := float;
      fpmovx;
      end
    else movx(false, dreg, @getdreg);
  end; {rdintcharx}


procedure wrrealx;

{ Calls one of two procedures to write a real number to the text file.
  The arguments are assumed to be placed on the stack already.  If no
  format parameters are present, a default TotalFieldWidth is generated,
  which results in a floating point notation call (wrrl1).  The presence of
  two format parameters will result in a fixed point notation call (wrrl2).

  The calling sequence is:

        procedure wrrl1(var f: text; val: double; field1: integer);
  or
        procedure wrrl2(var f: text; val: double; field1, field2: integer);

}


  begin
    if formatcount = 0 then
      begin
      if len = quad then settempimmediate(defaulttargetintsize, 21)
      else settempimmediate(defaulttargetintsize, 13);
      aligntemps;
      newtemp(defaulttargetintsize);
      gendouble(move, tempkey, stackcounter);
      end;
    if len = quad then
      if formatcount <= 1 then
        if filenamed then callandpop(libwritedouble1, 2)
        else callandpop(libwritedouble1o, 2)
      else if filenamed then callandpop(libwritedouble2, 3)
      else callandpop(libwritedouble2o, 3)
    else if formatcount <= 1 then
      if filenamed then callandpop(libwritereal1, 2)
      else callandpop(libwritereal1o, 2)
    else if filenamed then callandpop(libwritereal2, 3)
    else callandpop(libwritereal2o, 3);
    formatcount := 0;
  end {wrrealx} ;


procedure wrcommon(libroutine: libroutines; {formatting routine to call}
                   deffmt: integer {default width if needed} );

{ Calls a formatted write routine for most data types.  This is used for
  "wrint", "wrchar" and "wrbool".  The default field width is set if not
  otherwise specified (determined by "formatcount").  The calling
  sequence is:

        procedure writeit(var f: text; val: entrytype; fieldwidth: integer);

}


  begin
    if formatcount = 0 then
      begin
      settempimmediate(defaulttargetintsize, deffmt);
      aligntemps; {just to be sure}
      newtemp(defaulttargetintsize);
      gendouble(move, tempkey, stackcounter);
      end;
    if filenamed then callandpop(libroutine, 2)
    else callandpop(succ(libroutine), 2);
    formatcount := 0;
  end {wrcommon} ;


procedure wrstx(stdstring: boolean {true if packed array[1..n] kind} );

{ Write a string to a text file.  The necessary data is assumed to be
  pushed on the stack.  The calling sequence is:

        procedure wrstrg(var f: text; var val: string; len, field: integer);

  Note that in this case the default width is the string width.
}


  begin
    if formatcount = 0 then
      begin
      settempreg(defaulttargetintsize, indr, sp);
      aligntemps; {just to be sure}
      newtemp(defaulttargetintsize);
{      gendouble(move, tempkey, stackcounter);}
      if stdstring then gensimplemove(stackcounter + 1, stackcounter)
      else gensimplemove(stackcounter + 2, stackcounter);
      end;
    if filenamed then callandpop(libwritestring, 3)
    else callandpop(libwritestringo, 3);
    formatcount := 0;
  end {wrstx} ;


procedure clearcontext;

{ Clear the current context.  That is, forget where everything is in the
  current context.  Any registers containing temps are saved.
  This is called at labels when the context at this point is unpredictable.
}

  var
    i: regindex; {induction var for killing variables}


  begin
    for i := 0 to lastdreg do if dregused[i] then markdreg(i);
    for i := 0 to lastareg do if aregused[i] then markareg(i);
    if mc68881 then
      for i := 0 to lastfpreg do if fpregused[i] then markfpreg(i);

{ Preserve innermost for loop induction register if this is the first
  clear at the current level (we know the first one is derived from the
  for loop itself, and therefore is under our control).
}

    if (forsp > 0) and forstack[forsp].firstclear then
      begin
      forstack[forsp].firstclear := false;
      with forstack[forsp], keytable[forkey] do
        if not regvalid then
          begin
          regvalid := true;
          adjustregcount(forkey, refcount);
          end;
      end;

    context[contextsp].clearflag := true;
    context[contextsp].lastbranch := lastnode;
  end {clearcontext} ;


procedure enterloop;

{ Enter a loop construct.  Travrs has internally done a 'clearcontext'
  operation and has issued a 'clearlabel' or 'fortop' pseudoop.  All
  registers are empty at this point except those generated by 'with'
  statements and for loop indices.  We track these registers, and
  if they are spoiled by code within the loop, restore them at the
  bottom.

  Since loops may be nested, we use a special stack of limited size
  to do the required tracking.  If the stack is full, we simply do
  a clearcontext and increment the overflow counter.
}

  var
    i: regindex; {for stepping through volatile registers}


  begin {enterloop}
    if loopsp < loopdepth then
      begin
      if forsp > 0 then forstack[forsp].firstclear := false;

      loopsp := loopsp + 1;

      with loopstack[loopsp] do
        begin
        { Save the bump fields so we can restore them properly.
        }
        thecontext := contextsp;
        abump := context[contextsp].abump;
        dbump := context[contextsp].dbump;
        fpbump := context[contextsp].fpbump;
        savefirstnode := context[contextsp].firstnode;
        savelastbranch := context[contextsp].lastbranch;

        for i := 0 to lastareg do
          with aregstate[i] do
            begin
            reload := 0; { not reloaded yet }
            killed := false;
            used := (aregisters[i] > 0);
            if used then context[contextsp].abump[i] := true;
            active := context[contextsp].abump[i];
            if active then
              begin
              stackcopy := saveareg(i);
              keytable[stackcopy].refcount := keytable[stackcopy].refcount +
                                              1;
              end;
            end; {with}

        for i := 0 to lastdreg do
          with dregstate[i] do
            begin
            reload := 0; { not reloaded yet }
            killed := false;
            used := (dregisters[i] > 0);
            if used then context[contextsp].dbump[i] := true;
            active := context[contextsp].dbump[i];
            if active then
              begin
              stackcopy := savedreg(i);
              keytable[stackcopy].refcount := keytable[stackcopy].refcount +
                                              1;
              end;
            end; {with}

        if mc68881 then
          for i := 0 to lastfpreg do
            with fpregstate[i] do
              begin
              reload := 0; { not reloaded yet }
              killed := false;
              used := (fpregisters[i] > 0);
              if used then context[contextsp].fpbump[i] := true;
              active := context[contextsp].fpbump[i];
              if active then
                begin
                stackcopy := savefpreg(i);
                keytable[stackcopy].refcount := keytable[stackcopy].refcount +
                                                1;
                end;
              end; {with}
        end; {with loopstack}
      context[contextsp].clearflag := true;
      context[contextsp].lastbranch := lastnode;
      context[contextsp].firstnode := lastnode; { prevent popping stack
                                                  in loop }
      end
    else
      begin
      clearcontext;
      loopoverflow := loopoverflow + 1;
      end;
  end {enterloop} ;


procedure reloadloop;
  {
  We can't do the restore of loop registers at the bottom of a repeat loop
  since the until may kill restored registers, therefore we restore at
  the top and then delete any unneeded restores at the bottom. Since we
  can't tell the difference from while loops they work the same way.
  }

  var
    i: regindex;
    r: keyindex;


  begin
    if loopoverflow = 0 then
      begin
      with loopstack[loopsp] do
        begin
        settempareg(0); {dummy}
        r := tempkey;

        for i := 0 to lastareg do
          with aregstate[i] do
            if active then
              begin
              keytable[r].oprnd.reg := i;
              reload := lastnode + 1;
              gensimplemove(stackcopy, r);
              end;

        keytable[r].oprnd.m := dreg;

        for i := 0 to lastdreg do
          with dregstate[i] do
            if active then
              begin
              keytable[r].oprnd.reg := i;
              reload := lastnode + 1;
              gensimplemove(stackcopy, r);
              end;

        if mc68881 then
          begin
          keytable[r].oprnd.m := fpreg;

          for i := 0 to lastfpreg do
            with fpregstate[i] do
              if active then
                begin
                keytable[r].oprnd.reg := i;
                reload := lastnode + 1;
                genfpmove(stackcopy, r);
                end;
          end; {mc68881}
        end; {with loopstack}
      end;
  end {reloadloop} ;


{ We now begin the higher-level code generation routines.  These routines
  are called from the main generation procedure based on the most recent
  pseudo-op read.  In general, there is a procedure for each pseudo-op,
  and if the pseudo-op has the name "op", the corresponding procedure
  has the name "opx".  There are a few cases when a single procedure
  is used by more than one pseudo-op, in which case the name of the
  procedure will reflect this.

  Most of these routines take operands from the globals "left" and "right",
  and leave the result in the global "key".
}

{ These procedures all define labels, with the label number in the global
  "pseudoinst.oprnds[1]", which is equivalent to operand[1].
}


procedure savelabelx;

{ Define a label and save the current context for later restoration.
  This is called at the beginning of each branching structure, and
  corresponds to the same operation in "travrs".

  The context is saved in the "markstack" data structure.
}

  var
    i: integer; {induction var for scanning keys and registers.}


  begin
    adjustdelay := false;
    context[contextsp].lastbranch := lastnode;
    definelabel(pseudoinst.oprnds[1]);
    saveactivekeys;
    contextsp := contextsp + 1;

    with context[contextsp] do
      begin
      clearflag := false;
      keymark := lastkey + 1;
      firstnode := lastnode;
      lastbranch := lastnode;
      dbump := context[contextsp - 1].dbump;
      abump := context[contextsp - 1].abump;
      for i := 0 to lastdreg do if dregisters[i] > 0 then dbump[i] := true;
      for i := 0 to lastareg do if aregisters[i] > 0 then abump[i] := true;

      if mc68881 then
        begin
        fpbump := context[contextsp - 1].fpbump;
        for i := 0 to lastfpreg do if fpregisters[i] > 0 then fpbump[i] := true;
        end;
      end;

    for i := context[contextsp - 1].keymark to lastkey do
      adjustregcount(i, - keytable[i].refcount);
  end {savelabelx} ;


procedure restorelabelx;

{ Define a label and restore the previous environment.  This is used at the
  end of one branch of a branching construct.  It gets rid of any temps
  generated along this branch and resets to the previous context.
}

  var
    i: keyindex; {used to scan keys to restore register counts}


  begin
    definelabel(pseudoinst.oprnds[1]);
    context[contextsp].lastbranch := context[contextsp].firstnode;

    while lastkey >= context[contextsp].keymark do
      with keytable[lastkey] do
        begin
        bumptempcount(lastkey, - refcount);
        adjustregcount(lastkey, - refcount);
        refcount := 0;
        lastkey := lastkey - 1;
        end;

    adjusttemps;
    contextsp := contextsp - 1;
    for i := context[contextsp].keymark to lastkey do
      adjustregcount(i, keytable[i].refcount);
    adjustdelay := (pseudobuff.op = savelabel);
    context[contextsp].lastbranch := context[contextsp].firstnode;
  end {restorelabelx} ;


procedure joinlabelx;

{ Define a label and adjust temps at the end of a forking construction.
  Temps which are used along any branch of the fork have the "join" flag
  set, and at this point such temps are flagged as used.  This corresponds
  to the "join" construction in travrs.
}

  var
    i: keyindex; {induction var for scanning keys}


  begin
    definelabel(pseudoinst.oprnds[1]);
    for i := context[contextsp].keymark to lastkey do
      with keytable[i] do
        begin
        adjustregcount(i, - refcount);
        if joinreg then regvalid := false;
        if joinindxr then indxrvalid := false;
        adjustregcount(i, refcount);
        end;
  end {joinlabelx} ;


procedure clearlabelx;

{ Define a label at the head of a loop.  All CSE's except 'with'
  expressions and for loop indices were purged by travrs.  The
  routine 'enterloop' enters bookkeeping information which allows
  registers to be used within the loop.  This scheme depends upon
  'restoreloopx' properly restoring registers to their state as of
  loop entry.
}


  begin
    saveactivekeys;
    enterloop;
{    clearcontext;}
    definelabel(pseudoinst.oprnds[1]);
    reloadloop;
  end {clearlabelx} ;


procedure pseudolabelx;

{ Define a pseudo-code label.  This is the basic label definition routine.
}


  begin
    definelabel(pseudoinst.oprnds[1]);
  end {pseudolabelx} ;


procedure restoreloopx;
  {
  Restore necessary registers at the bottom of a loop. This is necessary
  because code at the top of the loop may depend upon these register
  values.
  }

  var
    i: regindex;
    tempreg: keyindex;


  begin
    if loopoverflow > 0 then loopoverflow := loopoverflow - 1
    else
      begin
      with loopstack[loopsp] do
        begin
        { get bump field back to what it should be }
        context[thecontext].abump := abump;
        context[thecontext].dbump := dbump;

        if mc68881 then
          context[thecontext].fpbump := fpbump;

        context[thecontext].firstnode := savefirstnode;
        context[thecontext].lastbranch := savelastbranch;

        { seem like we should only restore registers that were used? hmm. }
        settempareg(0); {dummy}
        tempreg := tempkey;

        for i := 0 to lastareg do
          with aregstate[i] do
            if active then
              begin
              if killed then
                begin
                keytable[stackcopy].tempflag := true;
                { if not restored at top of loop (i.e. for loop) do it now }
                if reload = 0 then
                  begin
                  markareg(i); { tell current user }
                  keytable[tempreg].oprnd.reg := i;
                  gensimplemove(stackcopy, tempreg);
                  end;
                end
              else if reload <> 0 then delete(reload, 1);
              keytable[stackcopy].refcount := keytable[stackcopy].refcount -
                                              1;
              end;

        keytable[tempreg].oprnd.m := dreg;

        for i := 0 to lastdreg do
          with dregstate[i] do
            if active then
              begin
              if killed then
                begin
                keytable[stackcopy].tempflag := true;
                { if not restored at top of loop (i.e. for loop) do it now }
                if reload = 0 then
                  begin
                  markdreg(i); { tell current user }
                  keytable[tempreg].oprnd.reg := i;
                  gensimplemove(stackcopy, tempreg);
                  end;
                end
              else if reload <> 0 then delete(reload, 1);
              keytable[stackcopy].refcount := keytable[stackcopy].refcount -
                                              1;
              end;

        if mc68881 then
          begin
          keytable[tempreg].oprnd.m := fpreg;

          for i := 0 to lastfpreg do
            with fpregstate[i] do
              if active then
                begin
                if killed then
                  begin
                  keytable[stackcopy].tempflag := true;
                  { if not restored at top of loop (i.e. for loop) do it now }
                  if reload = 0 then
                    begin
                    markfpreg(i); { tell current user }
                    keytable[tempreg].oprnd.reg := i;
                    genfpmove(stackcopy, tempreg);
                    end;
                  end
                else if reload <> 0 then delete(reload, 1);
                keytable[stackcopy].refcount := keytable[stackcopy].refcount -
                                                1;
                end; {active}
          end; {mc68881}
        end; {with loopstack}
      loopsp := loopsp - 1;
      end;
  end {restoreloopx} ;



procedure copyaccessx;

{ Make a copy at the current context level of the keytable entry
  for the operand in oprnds[1].  This allows modifications to the
  local key without affecting the outer context key.  If the flag
  "clearflag" is set in the local context mark,  The properaddress of
  the key is copied into the local key, as we assume that the volatile
  copy of the key may not exist at this point, and we must use the
  non-volatile copy in "properaddress".

  The refcount of the key being copied (and all intermediate copies)
  is reduced by the difference between the local refcount and
  copycount.  This number is the number of references in the new
  local context.
}

  var
    delta: integer; {difference between refcount and copycount}
    useproperaddress: boolean; {true if copy is logically within a loop}


  begin
    { Because of hoisting, we may have a copy operator appearing
      before the clearlabel, defeating the purpose of the context
      clearflag.  Fortunately, travrs warns us by passing a flag
      in the len field.
    }
    useproperaddress := (len <> 0) or context[contextsp].clearflag;

    with keytable[key] do
      begin {copy the key}
      len := keytable[left].len;
      copylink := left;
      delta := refcount - copycount;
      end;

    with keytable[left], oprnd do
      begin
      keytable[key].regsaved := regsaved;
      keytable[key].indxrsaved := indxrsaved;
      keytable[key].regvalid := regvalid;
      keytable[key].indxrvalid := indxrvalid;
      keytable[key].properreg := properreg;
      keytable[key].properindxr := properindxr;
      keytable[key].tempflag := tempflag;
      keytable[key].packedaccess := packedaccess;

      { Point to the properaddress if clearcontext}
      if useproperaddress then
        begin
        if joinreg then keytable[key].regvalid := false;
        if joinindxr then keytable[key].indxrvalid := false;
        end;

      with loopstack[loopsp] do
        begin
        with aregstate[reg] do
          if keytable[key].regvalid and active then used := true;
        with dregstate[reg] do
          if keytable[key].regvalid and active then used := true;
        with fpregstate[reg] do
          if keytable[key].regvalid and active then used := true;

        with aregstate[indxr] do
          if keytable[key].indxrvalid and active then used := true;
        with dregstate[indxr] do
          if keytable[key].indxrvalid and active then used := true;
        with fpregstate[indxr] do
          if keytable[key].indxrvalid and active then used := true;
        end; { loopstack[loopsp] }

      setvalue(m, reg, indxr, indxlong, offset, offset1);
      keytable[key].oprnd.flavor := flavor;
      keytable[key].oprnd.scale := scale;
      keytable[key].oprnd.commonlong_reloc := commonlong_reloc;
      keytable[key].signed := signed;
      keytable[key].signlimit := signlimit;
      keytable[key].knowneven := knowneven;
      keytable[key].high_word_dirty := high_word_dirty;
      end;

    { Now decrement refcounts }
    repeat
      with keytable[left] do
        begin
        refcount := refcount - delta;
        copycount := copycount - delta;
        bumptempcount(left, - delta);
        left := copylink;
        end;
    until left = 0;
  end {copyaccessx} ;



procedure defforindexx(sgn, { true if signed induction var }
                       lit: boolean { true if constant starter value } );

{ Define a for-loop induction variable's starting value. There are two
  cases - a global register induction variable and a stack induction
  variable. The stack case will actually delay pushing the variable
  until we are inside the loop body, which saves a word.  Indeed, in
  many cases the push is not needed at all.
}


  begin {defforindexx}
    saveactivekeys;
    address(right);

    if lit then
      begin
      settempimmediate(len, pseudoinst.oprnds[1]);
      left := tempkey
      end
    else
      begin
      lock(right);
      unpackshrink(left, len);
      unlock(right);
      end;

    keytable[key].signed := sgn;

    { Allocate a register unless this is a permanently assigned register
      variable.  If target <> 0, we must preserve the running index in
      the actual variable, if not, we'll issue a "savekey" in "fortopx"
      to save the running index on the stack.  Often, this can later be
      deleted as with any other stack temp, making register-only loops
      quite common.
    }

    forsp := forsp + 1;
    with forstack[forsp], keytable[key] do
      begin
      nonvolatile := target <> 0;
      globaldreg := keytable[right].oprnd.m = dreg;
    if not globaldreg then
      begin
      settempdreg(long, getdreg);
      setkeyvalue(tempkey);    {destroys signed field}
      keytable[key].regsaved := true;
      if nonvolatile then
        begin
        keytable[right].validtemp := true;

        { if this "nonvolatile" variable is greater than 32KB from the
          frame base and we're in 68K mode, it will actually be relative
          to some volatile A register.  In this case, we need to force
          an extra reference to it so that the stack copy will be preserved
          and restored via the enter/exitloop mechanism.  It will be
          dereferenced in fortopx and should be a NOP in other cases.
        }
        rereference(right);

        keytable[key].properreg := right;
        end;
      end
    else setkeyvalue(right);

    keytable[key].signed := sgn;

    { We make it long if it's free, or if it might be used as an index.
    }
      originalreg := oprnd.reg;
      litinitial := lit;
      forkey := key;
      firstclear := true;
      savedlen := len;
      initval := pseudoinst.oprnds[1];
      end;

    gensimplemove(left, key);

    dontchangevalue := 0;

  end {defforindexx} ;


procedure fortopx(signedbr, unsignedbr: insttype { proper exit branch });

{ Start a for loop, top or bottom.  Branch arguments determine if we
  are going up or down.  If we have constant limits we do not generate a
  cmp/brfinished pair at this point. If the induction var is on the
  stack we will force storage of the original loaded register for the
  value onto the stack after the comparison (if there is one).
}

  var
    branch: insttype;
    regkey: keyindex; {descriptor of for-index register}


  begin {fortopx}
    with forstack[forsp] do
      begin
      if keytable[forkey].signed then branch := signedbr
      else branch := unsignedbr;
      pseudolabelx;
      settempdreg(long, originalreg);
      regkey := tempkey;
      keytable[regkey].len := savedlen;

      if target <> 0 then
        begin
        makeaddressable(target);
        shrink(target, keytable[forkey].len);
        gendouble(cmp, target, regkey);
        genbr(branch, pseudoinst.oprnds[2]);
        end;

      if nonvolatile and not globaldreg
      then gensimplemove(regkey, keytable[forkey].properreg)
      else
        begin
        keytable[forkey].regsaved := false;
        savekey(forkey);
        end;

      enterloop;

      { see defforindexx for an explaination of this }
      if nonvolatile and not globaldreg then
        dereference(keytable[forkey].properreg);
      end;

  end {fortopx} ;


procedure forbottomx(improved: boolean; { true if cmp at bottom }
                     incinst, { add or sub }
                      signedbr, unsignedbr: insttype {branch to top});

{ Finish a for loop. If improved is true, we inc/dec and compare at
  this point. If improved is false, we inc/dec and branch to comparison
  at the top of the loop. We pop off induction variable to save a word
  if the loop is finished. The code at the top of the loop will re-push
  the value if we are not finished.
}

  var
    sgn: boolean;
    needcompare: boolean; {need to generate a comparison at end of loop}
    branch: insttype;
    maxvalue: unsigned; {"cmp" instruction works if limit value < maxvalue}
    i: 1..4; {induction var}
    byvalue: unsigned; { BY value (always "1" for Pascal) }


  begin {forbottom}
    byvalue := len;
    context[contextsp].lastbranch := context[contextsp].firstnode;
    adjusttemps;

    with forstack[forsp] do
      begin
      sgn := keytable[forkey].signed;
      dereference(forkey);
      if sgn then branch := signedbr
      else branch := unsignedbr;
      with keytable[forkey], oprnd do
        begin
        maxvalue := 127;
        for i := 2 to len do maxvalue := maxvalue * 256 + 255;
        if not sgn then maxvalue := maxvalue * 2 + 1;
        if not regvalid or (m <> dreg) or (reg <> originalreg) then
          begin
          settempdreg(max(len, word), originalreg);
          keytable[properreg].tempflag := true;
          gensimplemove(properreg, tempkey);
          forkey := tempkey;
          if loopoverflow = 0 then
            loopstack[loopsp].dregstate[originalreg].killed := false;
          end;
        end;
      keytable[forkey].len := savedlen;
      restoreloopx;

      { The following tests determine how we detect the last iteration through
        the loop when the initial and final values are both constant.  We add
        or subtract the step value then compare with the final value, and loop
        if we've not passed by the final value.  Normally, we issue a compare
        instruction, but if the add or subtract causes overflow (or carry, in
        the unsigned case) the compare won't work. On the bright side, not
        only does the compare not work but is is not needed, and a simple
        branch on no overflow (or carry) is sufficient.

        Below, "needcompare" is set true if overflow (carry) will not occur
        when we "pass over" the final value, in which case we'll issue the
        compare.
      }
      with keytable[target].oprnd, pseudoinst do
        begin
        if improved then
          if incinst = add then
            needcompare := (unsignedint(maxvalue - offset) >= byvalue)
                           or (unsignedint(maxvalue - initval)
                               mod byvalue < unsignedint(maxvalue - offset)) 
          else
            if sgn then
              needcompare := (unsignedint(offset + maxvalue + 1) >=
                             byvalue) or
                             (initval mod byvalue < offset + maxvalue + 1)
            else needcompare := (unsignedint(offset) >= byvalue) or
                                (unsignedint(initval) mod
                                 byvalue < offset)
        else needcompare := false;
        end;

      settempimmediate(targetintsize, byvalue); { "for" step for Modula-2 }
      gendouble(incinst, tempkey, forkey);

      if needcompare then
        begin
        address(target);
        if tempkey = lowesttemp then compilerabort(interntemp);
        tempkey := tempkey - 1;
        keytable[tempkey] := keytable[target];
        target := tempkey;

        { Change comparisons against 1 to be comparisions against 0.
        }

        with keytable[target].oprnd do
          if offset = 1 then
            begin
            if incinst = sub then
              begin
              offset := 0;
              if sgn then branch := bgt else branch := bhi;
              end;
            end
          else if offset = -1 then
            if sgn and (incinst = add) then
              begin
              offset := 0;
              branch := blt;
              end;

          with keytable[forkey], oprnd do
            begin
            shrink(target, keytable[forkey].len);
            gendouble(cmp, target, forkey);
            genbr(branch, pseudoinst.oprnds[1]);
            end;
          end {if needcompare}
        else if sgn then genbr(bvc, pseudoinst.oprnds[1])
        else genbr(bhs, pseudoinst.oprnds[1])
      end; {with forstack[forsp]}

    if not needcompare then dereference(target);

    pseudoinst.oprnds[1] := pseudoinst.oprnds[2];
    forsp := forsp - 1;
    joinlabelx;
    context[contextsp].lastbranch := context[contextsp].firstnode;
  end {forbottomx} ;


procedure forcheckx(up: boolean {we are going up} );

{ Generate range check operations for entry to a for loop.  This is
  surprisingly complicated, and the main job done in the outer procedure
  is deciding the form of the code to be generated.  The actual code is
  generated by the appropriate inner procedure

  This assumes that the initial value has been assigned to the controlled
  variable.  The controlled variable is obtained from the forstack, the
  final value is in "left", the lower bound in "pseudoinst.oprnds[2]",
  and the upper bound in "pseudoinst.oprnds[3]".
}

  var
    regkey: keyindex;
    s: boolean; {signed branches should be generated}
    finalzero: boolean; {final value is constant zero}
    t: integer; {for swapping limit values}
    brl: insttype; {blt or blo or bgt or bhi, depending on s and up}
    brle: insttype; {etc. ad nauseum}
    brg: insttype;
    brge: insttype;


  procedure onepart(brinst: insttype; {branch to generate}
                    key1, key2: keyindex; {operands to compare}
                    l: integer {branch target} );

{ Generate compare followed by branch.  Keytable[key1] had better be
  mode dreg.
}


    begin {onepart}

      if keytable[key1].oprnd.m <> dreg then
        begin
        write('for check error');
        compilerabort(inconsistent);
        end;

      gendouble(cmp, key2, key1);
      genbr(brinst, l);

    end {onepart} ;


  procedure fullygrosscheck(br1, br2: insttype);

{ Generate the long form (no constant limit or initial value) of for
  loop check code.  The name expresses my feelings precisely.
}


    begin {fullygrosscheck}

      settempimmediate(keytable[key].len, pseudoinst.oprnds[2]);
      settempimmediate(keytable[key].len, pseudoinst.oprnds[3]);

      {bounds a}
      onepart(br1, regkey, tempkey + 1, lastlabel);

      {bounds b}
      onepart(br2, left, tempkey, lastlabel - 1);

    end {fullygrosscheck} ;


  begin

    with forstack[forsp] do
      begin
      settempdreg(len, originalreg);
      regkey := tempkey;
      s := keytable[forkey].signed;
      unpack(left, len);
      with keytable[left].oprnd do
        finalzero := (m = immediate) and (offset = 0);

      setallfields(left);

      {[s=2] initialize screwy branches}
      if up then
        begin
        if s then
          begin
          brl := blt;                       brle := ble;
          brg := bgt;                       brge := bge;
          end
        else {not s}
          begin
          brl := blo;                       brle := bls;
          brg := bhi;                       brge := bhs;
          end;
        end
      else {not up}
        begin
        if s then
          begin
          brl := bgt;                       brle := bge;
          brg := blt;                       brge := ble;
          end
        else {not s}
          begin
          brl := bhi;                       brle := bhs;
          brg := blo;                       brge := bls;
          end;
        end;
      {[s=1]}

      {generate zero-trip check}
      onepart(brg, regkey, left, lastlabel - 1);

      if (keytable[left].oprnd.m <> dreg) and
         (keytable[left].oprnd.m <> immediate) then
        begin
        lock(regkey);
        settempdreg(keytable[left].len, getdreg);
        gensimplemove(left, tempkey);
        unlock(regkey);
        left := tempkey;
        end;

      if litinitial then
        begin
        if up then settempimmediate(len, pseudoinst.oprnds[3])
        else settempimmediate(len, pseudoinst.oprnds[2]);
        onepart(brle, tempkey + 1, tempkey, lastlabel - 1)
        end {if litinitial}
      else {not litinitial}
      if keytable[left].oprnd.m = immediate then
        begin
        if up then settempimmediate(len, pseudoinst.oprnds[2])
        else settempimmediate(len, pseudoinst.oprnds[3]);
        onepart(brge, regkey, tempkey, lastlabel - 1);
        end {if keytable[left].oprnd...}
      else {must generate long, gross form: three comparisons!}
      if (pseudoinst.oprnds[2] = 0) and not s then
        onepart(brle, regkey, tempkey, lastlabel - 1)
      else if up then fullygrosscheck(brl, brle)
      else
        begin
        t := pseudoinst.oprnds[2];
        pseudoinst.oprnds[2] := pseudoinst.oprnds[3];
        pseudoinst.oprnds[3] := t;
        fullygrosscheck(brl, brle);
        end;
      end;

    definelastlabel;

    case targetopsys of
      unix, apollo:
        callsupport(librangetrap);  { range error }
      vdos:
        begin
        settempimmediate(word, rangetrap);
        gensingle(trap, tempkey);
        end;
      end;

    generror(range_error);
    definelastlabel;

  end; {forcheckx}


procedure forerrchkx;

{ Handle the case where one of the limits is constant, but out of range
  for the for variable.  This is legal only if the loop is never
  executed,  but since it is legal we have to handle it.

  Here "left" is the final value, and "pseudoinst.oprnds[2]"
  is non-zero for a downto loop.
}

  var
    regkey: keyindex; {descriptor of induction register}


  begin {forerrchkx}
    unpack(left, len);
    setallfields(left);
    settempdreg(targetintsize, forstack[forsp].originalreg);
    regkey := tempkey;
    gendouble(cmp, key, regkey);
    if keytable[regkey].signed then
      begin
      if pseudoinst.oprnds[2] = 0 then genbr(bgt, lastlabel)
      else genbr(blt, lastlabel)
      end
    else
      begin
      if pseudoinst.oprnds[2] = 0 then genbr(bhi, lastlabel)
      else genbr(blo, lastlabel)
      end;

    case targetopsys of
      unix, apollo:
        callsupport(librangetrap);  { range error }
      vdos:
        begin
        settempimmediate(word, rangetrap);
        gensingle(trap, tempkey);
        end;
      end;

    generror(range_error);
    definelastlabel;
  end {forerrchkx} ;


procedure movintx;

{ Move an integer.  The integer may be in the 68881.
}

  begin
  if mc68881 and (keytable[right].oprnd.m = fpreg) then
    begin
    addressboth;
    fpgendouble(fmove, right, left);
    end
  else movx(false, dreg, @getdreg);
  end;


procedure arithcommon(commute: boolean; {commutative operation?}
                      kill_d4: boolean; {controls killing of d4}
                      kill_d3: boolean; {controls killing of d3}
                      libentry_s: libroutines; {support routine for signed}
                      libentry_u: libroutines {support routine for unsigned} );

{ Does common code for arithmetic and comparisions.
}

  var
    t: keyindex; {temp for switching commutative operands}
    d3key: keyindex; {temp for locking D3}
    d4key: keyindex; {temp for locking D4}
    rightfirst: boolean; {load right operand first}


  begin {arithcommon}
    saveactivekeys;

    if commute and (keytable[right].oprnd.m = dreg) and
       (keytable[right].oprnd.reg = 4) and keytable[right].regvalid then
      begin
      t := left;
      left := right;
      right := t;
      end;

    with keytable[left].oprnd, keytable[right] do
      begin
      dereference(right);
      rightfirst := (oprnd.m = dreg) and (oprnd.reg = 4) and regvalid or
                    popping(right) and not ((m = dreg) and (reg = 3) and
                    keytable[left].regvalid);
      refcount := refcount + 1;
      bumptempcount(right, 1);
      adjustregcount(right, 1);
      end;

    if rightfirst then
      begin
      if not ((keytable[right].oprnd.m = dreg) and
         (keytable[right].oprnd.reg = 3) and
         keytable[right].regvalid) then
        begin
        if keytable[right].refcount <= 1 then
          begin
          markdreg(3);
          lock(left);
          dregisters[3] := dregisters[3] - 1000; {make it attractive to unpack}
          unpack(right, long);
          dregisters[3] := dregisters[3] + 1000;
          end
        else
          begin
          lock(left);
          unpack(right, long);
          unlock(left);
          markdreg(3);
          lock(left);
          end;
        end
      else
        begin
        lock(left);
        unpack(right, long);
        end;

      unlock(left);
      reserve_dreg(right, 3); {in case we missed it before}
      settempdreg(long, 3);
      gensimplemove(right, tempkey);
      d3key := tempkey;
      lock(d3key);
      end;

    if not ((keytable[left].oprnd.m = dreg) and
       (keytable[left].oprnd.reg = 4) and
       keytable[left].regvalid) then
      begin
      if keytable[left].refcount <= 1 then
        begin
        markdreg(4);
        dregisters[4] := dregisters[4] - 1000; {make it attractive to unpack}
        unpack(left, long);
        dregisters[4] := dregisters[4] + 1000;
        end
      else
        begin
        unpack(left, long);
        markdreg(4);
        end;
      end
    else unpack(left, long);

    reserve_dreg(left, 4); {in case we missed it before}
    settempdreg(long, 4);
    gensimplemove(left, tempkey);
    d4key := tempkey;

    if rightfirst then unlock(d3key)
    else
      begin
      if not ((keytable[right].oprnd.m = dreg) and
         (keytable[right].oprnd.reg = 3) and
         keytable[right].regvalid) then
        begin
        if keytable[right].refcount <= 1 then
          begin
          markdreg(3);
          lock(d4key);
          dregisters[3] := dregisters[3] - 1000; {make it attractive to unpack}
          unpack(right, long);
          dregisters[3] := dregisters[3] + 1000;
          end
        else
          begin
          lock(d4key);
          unpack(right, long);
          unlock(d4key);
          markdreg(3);
          lock(d4key);
          end;
        end
      else
        begin
        lock(d4key);
        unpack(right, long);
        end;

      unlock(d4key);
      reserve_dreg(right, 3); {in case we missed it before}
      settempdreg(long, 3);
      gensimplemove(right, tempkey);
      d3key := tempkey;
      end;

    if kill_d4 and (keytable[d4key].oprnd.m = dreg) and
       (keytable[d4key].oprnd.reg = 4) then
      markdreg(4); { Result is going in D4 -- must kill operand in D4 }

    if kill_d3 and (keytable[d3key].oprnd.m = dreg) and
       (keytable[d3key].oprnd.reg = 3) then
      markdreg(3); { Result is going in D3 -- must kill operand in D3 }

    if signedoprnds then callsupport(libentry_s)
    else callsupport(libentry_u);
  end {arithcommon} ;


procedure realarithmeticx(commute: boolean; {commutative operation?}
                          realentry: libroutines; {support routine if single}
                          doubentry: libroutines; {support routine if double}
                          mc68881_inst: insttype {68881 inst} );

{ Generate code for a real operation.  We will use the 68881 if the user
  gave us the switch otherwise it is done by simulation.

  Pseudo-ops handled are addreal, subreal, mulreal, divreal.
}


  var
    t: keyindex; {temp for switching commutative operands}

  begin {realarithmeticx}
    if mc68881 then
      begin
      addressboth;

      with keytable[left].oprnd do if flavor = int_float then
        flavor := float;

      with keytable[right].oprnd do if flavor = int_float then
        flavor := float;

      if not equivaccess(left, target) and loadedfpreg(right, true) and
        (mc68881_inst <> fdiv) then
        begin
        loadfpreg(right, left, true);
        if mc68881_inst = fsub then
          begin
          fpgendouble(fneg, key, key);
          fpgendouble(fadd, left, key);
          end
        else fpgendouble(mc68881_inst, left, key);
        end
      else
        begin
        if equivaccess(right, target) then target := 0;
        loadfpreg(left, right, true);
        fpgendouble(mc68881_inst, right, key);
        end;
      keytable[key].oprnd.flavor := float;
      end
    else if len = quad then
      begin
      addressboth;
      pushboth(commute);
      settos(2);
      callsupport(doubentry);
      end
    else
      begin
      arithcommon(commute, true, false, realentry, realentry);
      setd4result;
      end;
  end {realarithmeticx} ;


procedure movrealx;
  begin
  if mc68881 then fpmovx
  else movx(false, dreg, @getdreg);
  end;

procedure cmprealx(brinst: insttype; {true branch}
                   double_call: libroutines; {routine numbers}
                   mc68881_inst: insttype);

{ Process comparisons of real numbers.
}

  const
    single_call = libfcmp; {same entry point for all single comparisions}

  begin {cmprealx}
    if mc68881 then
      begin
      addressboth;

      with keytable[left].oprnd do if flavor = int_float then
        flavor := float;

      with keytable[right].oprnd do if flavor = int_float then
        flavor := float;

      if (keytable[left].oprnd.m = fpreg) then
        begin
        fpgendouble(fcmp, right, left);
        brinst := mc68881_inst;
        end
      else if (keytable[right].oprnd.m = fpreg) and
              (keytable[left].oprnd.m <> fpreg) then
        begin
        fpgendouble(fcmp, left, right);
        brinst := reverse[mc68881_inst];
        end
      else { Neither operand is in an fpreg. }
        begin
        settempfpreg(getfpreg);
        genfpmove(left, tempkey);
        fpgendouble(fcmp, right, tempkey);
        brinst := mc68881_inst;
        end;
      end
    else if len = quad then
      begin
      addressboth;
      pushboth(false);
      callsupport(double_call);
      returntemps(2);
      end
    else
      arithcommon(false, false, false, single_call, single_call);
    setbr(brinst);
  end {cmprealx} ;


procedure cmplitrealx(brinst: insttype; {true branch}
                      double_call: libroutines; {routine numbers}
                      mc68881_inst: insttype);

{ Process comparisons with literal 0.
}

  begin {cmplitrealx}
    setlongvalue(0);
    cmprealx(brinst, double_call, mc68881_inst);
  end {cmplitrealx} ;


procedure unaryrealx(inst: insttype {'bchg' for neg, 'bclr' for abs} );

{ Complement or clear the leftmost bit of the left operand.

  * not used for 68881 *
}


  begin {unaryrealx}
    address(left);
    if keytable[left].oprnd.m = immediatelong then
      begin {just fold the constant value}
      setkeyvalue(left);
      with keytable[key].oprnd do
        if inst = bclr then offset1 := (offset1 and (not 32768))
        else offset1 := not (offset1 and 32768) and (offset1 or 32768); {xor}
      {believe it or not, this works for both single and double}
      end
    else if len > long then {double precision}
      begin
      if (keytable[key].refcount <= 1) and (target > 0) and
         (target <= lastkey) and (keytable[target].refcount <= 1) then
        begin
        if not equivaccess(left, target) then
          genblockmove(left, target, long);
        setkeyvalue(target);
        end
      else
        begin
{        aligntemps;}
        pushone(left);
        settos(1);
        end;

      settempimmediate(word, 7);
      gendouble(inst, tempkey, key);
      end
    else {single precision}
      begin
      loaddreg(left, 0, false);
      if keytable[key].oprnd.m = dreg then
        settempimmediate(word, 31)
      else settempimmediate(word, 7);
      gendouble(inst, tempkey, key);
      end;
  end {unaryrealx} ;


procedure callsinglesim(n: libroutines {simulation routine to call} );

{ Load left operand into d4, then call the single-argument math routine
  "n".
}


  begin {callsinglesim}
    saveactivekeys;
    markdreg(4);
    with keytable[left] do
      if (oprnd.m <> dreg) or (oprnd.reg <> 4) then
        begin
        settempdreg(len, 4);
        gensimplemove(left, tempkey);
        end;

    if (keytable[left].oprnd.m = dreg) and
       (keytable[left].oprnd.reg = 4) then
      markdreg(4); { Result is going in D4 -- must kill operand in D4 }

    callsupport(n);
    setd4result;
  end {callsinglesim} ;


procedure postrealx;

{ Generate a post-increment of a real value.
}
  var
    oldkey: keyindex;

  begin
    if mc68881 then
      begin
      addressboth;
      dereference(left); {extra for all reflexives}
      if keytable[left].oprnd.flavor = int_float then
	keytable[left].oprnd.flavor := float;

      if keytable[key].refcount > 0 then
	if (target <> 0) then
	  begin
	  if keytable[target].oprnd.flavor = int_float then
	    keytable[target].oprnd.flavor := float;
	  genfpmove(left, target);
	  setkeyvalue(target)
	  end
	else
	  begin
	  settempreg(len, fpreg, getfpreg);
	  genfpmove(left, tempkey);
	  setkeyvalue(tempkey);
	  end;
      if keytable[left].oprnd.m = fpreg then fpgendouble(fadd, right, left)
      else
        begin
        settempreg(len, fpreg, getfpreg);
        genfpmove(left, tempkey);
        fpgendouble(fadd, right, tempkey);
        genfpmove(tempkey, left);
        end;
      end
    else
      begin
      addressboth;
      if (keytable[key].refcount > 0) and (len = quad) then
        begin
        aligntemps;
        newtemp(len);
        oldkey := stackcounter;
        if len = quad then genblockmove(left, oldkey, long)
        else gensimplemove(left, oldkey);
        end
      else oldkey := left;
      if len = quad then
        begin
        pushboth(true);
        callsupport(libdadd);
        returntemps(2);
        stackoffset := - keytable[stackcounter - 1].oprnd.offset -
                       keytable[stackcounter - 1].len;
        newtemp(quad);
        stackoffset := - keytable[stackcounter].oprnd.offset;
        dereference(left);
        genblockmove(stackcounter, left, long);
        returntemps(1);
        keytable[oldkey].tempflag := true;
        keytable[key].regsaved := true;
        keytable[key].properreg := stackcounter;
        setkeyvalue(oldkey);
        savekey(key);
        end
      else
        begin {single precision}
        reserve_dreg(left, 4);
        settempreg(long, dreg, 4);
        gensimplemove(left, tempkey);
        reserve_dreg(right, 3);
        settempreg(long, dreg, 3);
        gensimplemove(right, tempkey);
        if keytable[key].refcount > 0 then
          begin
          lock(tempkey);
          lock(tempkey + 1);
          settempreg(long, dreg, getdreg);
          gensimplemove(tempkey + 2, tempkey);
          setkeyvalue(tempkey);
          tempkey := tempkey + 1;
          unlock(tempkey + 1);
          unlock(tempkey);
          end;
        markdreg(4);
        callsupport(libfadd);
        settempreg(len, dreg, 4);
        dereference(left);
        gensimplemove(tempkey, left);
        end;
      end;
  end; {postrealx}


procedure fltx;

{ Convert an integer to a real
}


  begin {fltx}
    if mc68881 then
      begin
      if keytable[left].packedaccess then
        begin
        unpack(left, long);
        loaddreg(left, 0, true);
        keytable[key].oprnd.flavor := int;
        end
      else if (keytable[left].len = long) and not keytable[left].signed then
        { Adjust an unsigned long that has the sign bit set.
        }
        begin
        address(left);
        keytable[left].oprnd.flavor := int;
        settempfpreg(getfpreg);
        genfpmove(left, tempkey);
        genbr(fbge, lastlabel);
        settempimmediate(12, 0);

        with keytable[tempkey].oprnd do
          begin
          flavor := float;
          m := immediate_extended;
          offset2 := $401f0000;
          offset1 := $80000000;
          offset := 0;
          end;

        fpgendouble(fadd, tempkey, tempkey + 1);
        setkeyvalue(tempkey + 1);
        keytable[key].oprnd.flavor := float;
        definelastlabel;
        end
      else
        begin
        { If safe, delay popping to the next 68881 instruction.
        }
        address(left);

        if (pseudoinst.refcount = 1) and popping(left) then setallfields(left)
        else setkeyvalue(left);

        keytable[key].oprnd.flavor := int;

        with keytable[key], oprnd do
          if m = immediate then len := long
          else len := keytable[left].len;
        end;
      end
    else
      begin
      saveactivekeys;
      reserve_dreg(left, 4);
      dregisters[4] := dregisters[4] - 1000; {make it attractive to unpack}
      unpack(left, long);
      dregisters[4] := dregisters[4] + 1000;

      if (keytable[left].oprnd.m = dreg) and
         (keytable[left].oprnd.reg = 4) then
        markdreg(4); { Result is going in D4 -- must kill operand in D4 }

      if len = quad then
        begin
        aligntemps;
        with keytable[left] do
          if (oprnd.m <> dreg) or (oprnd.reg <> 4) then
            begin
            settempdreg(len, 4);
            gensimplemove(left, tempkey);
            end;

        makestacktarget;
        stackoffset := - keytable[stackcounter].oprnd.offset;

        if (keytable[left].len = long) and not keytable[left].signed then
          callsupport(libdfloat_uns)
        else callsupport(libdfloat);
        end
      else
        if (keytable[left].len = long) and not keytable[left].signed then
          callsinglesim(libffloat_uns)
        else callsinglesim(libffloat);
      end;
  end {fltx} ;



procedure mulintx;

{ Generate code for an integer multiply.  This differs from add/sub etc.
  because hardware multiplies require 16-bit operands (no byte multiplies)
  and the result is always formed in a register.  If either of the
  operands is longer than 16 bits, a library routine is called.

  When mc68020 switch is used 32 bit multiplies are done inline.
}

  var
    finallen: datarange; {length of result, being fudged}
    s: boolean; {signed operands}
    length: integer; {max of length of two operands}


  begin {mulintx}
    length := max(bytelength(left), bytelength(right));

    if (length <= word) or mc68020 then
      begin
      finallen := keytable[key].len;

      if length = byte then length := word; {there is no multiply byte}

      keytable[key].len := length; {68020 may use long here}
      unpkshkboth(length);
      s := signedoprnds;

      if loadeddreg(left, true) then
        begin
        loaddreg(left, right, true);
        if s then gendouble(muls, right, key)
        else gendouble(mulu, right, key);
        end
      else
        begin
        loaddreg(right, left, true);
        if s then gendouble(muls, left, key)
        else gendouble(mulu, left, key);
        end;
      keytable[key].len := finallen;
      keytable[key].signed := s;
      end
    else
      begin
      arithcommon(true, true, false, libimult, libunsmult);
      keytable[key].signed := signedoprnds;
      setd4result;
      end;
    keytable[key].knowneven := keytable[left].knowneven and
			       keytable[right].knowneven;
  end {mulintx} ;


procedure unaryintx(inst: insttype {instruction to generate} );

{ Generate code for a unary operand on an integer.  This is used for
  "negint", "compint" as well as internally.
}


  begin
    unpackshrink(left, len);
    if (target > 0) and (keytable[target].len > len) then
      target := 0;
    with keytable[left] do
      begin
      loaddreg(left, 0, false);
      gensingle(inst, key);
      keytable[key].signed := keytable[left].signed;
      end;
  end {unaryintx} ;


procedure incdec(inst: insttype; {add or sub}
                 negflag: boolean {true if preliminary "neg" desired} );

{ Generate add/sub #1, left.  Handles compbool, incint and decint pseudoops.
}


  begin {incdec}
    unpackshrink(left, len);
    if (target > 0) and (keytable[target].len > len) then
      target := 0;
    loaddreg(left, 0, false);
    if negflag then gensingle(neg, key);
    settempimmediate(len, 1);
    gendouble(inst, tempkey, key);
    keytable[key].signed := keytable[left].signed;
  end {incdec} ;


procedure postintptrx(isptr: boolean);

{ Generate post increment instructions for integers and pointers.
}
  var
    oldleft: keyindex;
    newkey: keyindex;

  begin
    oldleft := left;
    unpack(left, len);
    if keytable[key].refcount > 0 then
      begin
      if target <> 0 then
	begin
	gensimplemove(left, target);
	newkey := target;
	setkeyvalue(target)
	end
      else
	begin
	lock(left);
	if isptr then settempreg(len, areg, getareg)
	else settempreg(len, dreg, getdreg);
	unlock(left);
	gensimplemove(left, tempkey);
	newkey := tempkey;
	end;
      lock(newkey);
      end;
    settempimmediate(len, pseudoinst.oprnds[2]);
    gendouble(add, right, left);
    right := left;
    left := oldleft;
    with keytable[right] do
      begin
      refcount := refcount + 1;
      bumptempcount(right, 1);
      adjustregcount(right, 1);
      end;
    movx(false, dreg, @getdreg);
    if keytable[key].refcount > 0 then
      begin
      unlock(newkey);
      setkeyvalue(newkey);
      end;
  end;



procedure divintx(correctrem: boolean {need to correct remainder} );

{ Generate code for a divide operation.  Since the divide operators
  provide both a quotient and remainder, and since both are frequently
  needed, this is handled with a special kluge in analys.  The "div" or
  "mod" is compiled into a "divint" followed by a "getquo" or "getrem".
  The "divint" does the actual divide, while the "getxxx" operator extracts
  the desired result.

  We use the offset1 field here to pass information to getrem/getquo.  If
  we called the library routine, we indicate that we've already set up the
  desired result by passing (lowestemp-1), which we do here if there is only 
  one reference to our key;  if there are two references (the only other
  possibility), we pass twodregs mode.  If we generate code in line (word
  operations only for 68000, also long for the 68020), and there will be a
  need to correct the remainder, in case it can go negative, we will be
  referring to our right operand from the getrem, so we pass the right key
  number in offset1, and before we reference the right operand, we bump its
  reference counts, so it won't be popped or discarded prematurely.
}

  var
    s: boolean; {signed operands}
    indexreg: keyindex; {for acquiring a second register}


  begin {divintx}
    { Handle a word divide for the 68000 or 68020.
    }
    if (keytable[key].refcount = 1) and (pseudobuff.refcount > 1) then
      target := 0;

    if len <= word then
      begin
      with keytable[left], oprnd do
        if (m = immediate) and (offset < 0) then
          begin
          dereference(left);
          settemp(long, immediatelong, 0, 0, false, offset, 0, 1, unknown);
          left := tempkey;
          end
        {Note: specialized to 16 bit integers}
        else if refcount > 1 then
          begin
          unpackshrink(left, word);
          allowmodify(left, true);
          extend(left, long);
          end
        else
          unpack(left, long);
      lock(left);
      unpackshrink(right, word);
      unlock(left);
      s := signedoprnds;
      keytable[key].len := long;
      loaddreg(left, right, true);

      if ((keytable[key].refcount > 1) or (pseudobuff.op = getrem)) then
        begin
        { Getrem will have to generate code to add right to correct the
          remainder.
        }
        if s and correctrem then
          begin
          keytable[right].refcount := keytable[right].refcount + 1;
          adjustregcount(right, 1);
          bumptempcount(right, 1);
          keytable[key].oprnd.offset1 := right;
          end
        else { Indicate that there is no need to correct the remainder. }
          keytable[key].oprnd.offset1 := lowesttemp - 1;
        end;
      keytable[key].len := word;

      if s then gen2(divs, word, right, key)
      else gen2(divu, word, right, key);

      if ((keytable[key].refcount > 1) or (pseudobuff.op = getrem)) then
        begin
        { This is the only case where a word length register contains something
          we must save in the upper word.  Extend now will extend in place
          even when there are further uses, so high_word_dirty will prevent it.
        }
        keytable[key].high_word_dirty := true;
        lastkey := key;
        savekey(key);
        keytable[keytable[key].properreg].high_word_dirty := true;
        end;
      end
    else { len = long }
      begin
      if mc68020 then
        begin
        unpkshkboth(long);
        loaddreg(left, right, true);

        { Twodregs is a hack to pass two registers to putcode.  "Lowesttemp
          - 1" is a flag to getremquo that the desired result is already
          setup.  The field "reg" contains the quotient register and the field
          "indxr" contains the remainder register.  If the quotient register
          and the remainder register are the same then no remainder will be
          generated.
        }
        with keytable[key], oprnd do
          if (refcount = 1) and (pseudobuff.op = getquo) then
            keytable[key].oprnd.offset1 := lowesttemp - 1
          else 
            begin
            lock(key);
            lock(right);
            indexreg := getdreg;
            unlock(right);
            unlock(key);
            adjustregcount(key, -refcount);
            bumptempcount(key, - refcount);
            indxr := indexreg;
            m := twodregs;
            bumptempcount(key, refcount);
            adjustregcount(key, refcount);
            offset1 := lowesttemp - 1;
            end;

        s := signedoprnds;

        { This is a hack to adjust the MOD at runtime.
        }
        if s and correctrem and ((keytable[key].refcount > 1) or
            (pseudobuff.op = getrem)) then
          begin {getrem will have to generate code to add right}
          keytable[right].refcount := keytable[right].refcount + 1;
          adjustregcount(right, 1);
          bumptempcount(right, 1);
          if s then gendouble(divsl, right, key)
          else gendouble(divul, right, key);
          keytable[key].oprnd.offset1 := right;
          end
        else if s then gendouble(divsl, right, key)
        else gendouble(divul, right, key);
        end
      else
        begin
        arithcommon(false, true, true, libidiv, libunsdiv);
        s := signedoprnds;

        if keytable[key].refcount = 1 then
          if pseudobuff.op = getquo then
            setvalue(dreg, 4, 0, false, 0, lowesttemp - 1)
          else setvalue(dreg, 3, 0, false, 0, lowesttemp - 1)
        else setvalue(twodregs, 4, 3, false, 0, 0);
        end;
      end;
    keytable[key].signed := s;
  end {divintx} ;


procedure getremquox(remflag: boolean {true if "getrem"} );

{ Extract remainder or quotient word or longword generated above.
}

  var
    divisor: keyindex; {divisor key, for correcting remainder}

  begin {getremquox}
    keytable[key].high_word_dirty := false;  { default setting }
    address(left);
    divisor := keytable[left].oprnd.offset1;

    if keytable[left].oprnd.m <> twodregs then

      { The word divide instruction returns the quotient in the low half
        of the register and the remainder in the high half.  We must generate
        code to fetch the remainder and correct it if necessary.
      }
      if remflag and (len <= word) then
        begin
        keytable[key].high_word_dirty := true;
        keytable[left].oprnd.offset1 := 0;
        keytable[key].len := long;
        keytable[left].len := long;
        loaddreg(left, 0, true);
        keytable[key].len := word;
        keytable[left].len := word;
        gensingle(swap, key);

        if divisor <> lowesttemp - 1 then
          begin
          lock(key);
          unpackshrink(divisor, word);
          unlock(key);
          gensingle(tst, key);
          genrelbr(bge, 1);
          gendouble(add, divisor, key);
          labelnextnode := true;
          end;
        end
      else
        begin
        extend(left, len);
        shrink(left, len);
        setkeyvalue(left);
        keytable[key].oprnd.offset1 := 0;
        end
    else { keytable[left].oprnd.m = twodregs }
      begin
      allowmodify(left, true);
      with keytable[left], oprnd do
        begin
        m := dreg;
        if remflag then
          begin
          reg := indxr;
          properreg := properindxr;
          regsaved := indxrsaved;
          regvalid := indxrvalid;
          joinreg := joinindxr;
          end;
        indxr := 0;
        properindxr := 0;
        indxrsaved := false;
        indxrvalid := true;
        joinindxr := false;
        end;

      { Generate code to correct the remainder generated by the 68020
        divide instruction.  For the 68000 long divide, the library routine
        handles this.
      }
      if mc68020 and (divisor <> lowesttemp - 1) and
         remflag then
        begin
        keytable[left].oprnd.offset1 := 0;
        setkeyvalue(left);
        lock(key);
        unpackshrink(divisor, long);
        unlock(key);
        gensingle(tst, key);
        genrelbr(bge, 1);
        gen2(add, long, divisor, key); { gendouble could pop the stack }
        labelnextnode := true;
        end
      else
        begin
        keytable[left].oprnd.offset1 := 0;
        setkeyvalue(left);
        end;
      end;
    saveresult;
  end {getremquox} ;



procedure integerarithmetic(inst: insttype {simple integer inst} );

{ Generate code for a simple binary, integer operation (add, sub, etc.).
  The operation is assumed to be commutative unless "inst" is a subtract.
}


  begin {integerarithmetic}
    unpkshkboth(len);
    if (target > 0) and (keytable[target].len > len) then
      target := 0;

    { The equivaccess check below prevents us from forgeting a target
      already in use.
    }
    if not equivaccess(left, target) and
       (loadeddreg(right, true) or loadeddreg(right, false) and
       (keytable[left].oprnd.m in [immediate, dreg])) then
      begin
      loaddreg(right, left, not (keytable[left].oprnd.m in [immediate, dreg]));
      if inst = sub then
        begin
        gensingle(neg, key);
        gendouble(add, left, key);
        end
      else gendouble(inst, left, key);
      end
    else
      begin
      loaddreg(left, right,
               not (keytable[right].oprnd.m in [immediate, dreg]));
      gendouble(inst, right, key);
      end;
    keytable[key].signed := signedoprnds;
    if len > 1 then keytable[key].knowneven := true;
  end {integerarithmetic} ;


procedure xorintx;

{ Generate code for an exclusive or instruction.  This instruction,
  unlike ALL other logical instructions, requires that the first
  operand be in a D register or that it be immediate.
}

  begin
    unpkshkboth(len);
    if (target > 0) and (keytable[target].len > len) then
      target := 0;
    if not equivaccess(right, target) and
       not (keytable[left].oprnd.m in [immediate, dreg]) and
       loadeddreg(right, true) then
      begin
      loaddreg(left, right, false);
      gendouble(eor, right, key);
      end
    else
      begin
      if not (keytable[left].oprnd.m in [immediate, dreg]) then
	begin
	settempreg(len, dreg, getdreg);
	gendouble(move, left, tempkey);
	left := tempkey;
	end;
      loaddreg(right, left, false);
      gendouble(eor, left, key);
      end;
    keytable[key].signed := signedoprnds;
    if len > 1 then keytable[key].knowneven := true;
  end; {xorintx}


procedure addptrx;

{ Generate an add of an integer value to a pointer value.  We do this in
  an A register rather than a D register, which is why the code is
  different from normal integer addition
}
  begin
    unpackboth(len);

    { The equivaccess check below prevents us from forgeting a target
      already in use.
    }
    if not equivaccess(left, target) and
       (loadedareg(right, true) or loadedareg(right, false) and
       (keytable[left].oprnd.m in [immediate, dreg])) then
      begin
      loadareg(right, left,
               not (keytable[left].oprnd.m in [immediate, dreg]));
      gendouble(add, left, key);
      end
    else
      begin
      loadareg(left, right,
               not (keytable[right].oprnd.m in [immediate, dreg]));
      gendouble(add, right, key);
      end;
    keytable[key].signed := false;
    keytable[key].knowneven := keytable[left].knowneven and
			       keytable[right].knowneven
  end; {addptrx}


procedure shiftlintx(backwards: boolean);

{ Shift the operand by the distance given in oprnds[2].
}

  var
    shiftfactor: integer; {amount to shift}
    shiftinst: insttype; {either asl, asr, or lsr}
    knowneven: boolean; {true if result is known to be even.  Left shifts will
                         always give an even result; we can't tell for right
                         shifts. }


  begin
    unpackshrink(left, len);
    lock(left);
    unpackshrink(right, word);
    unlock(left);
    if keytable[left].signed then shiftinst := asl
    else shiftinst := lsl;
    if keytable[right].oprnd.m = immediate then
      begin
      shiftfactor := keytable[right].oprnd.offset;
      knowneven := shiftfactor > 0;
      if shiftfactor < 0 then backwards := not backwards;
      if backwards then
	if shiftinst = asl then shiftinst := asr
	else shiftinst := lsr;
      shiftfactor := abs(shiftfactor);
      loaddreg(left, 0, (shiftfactor > 1) or (len <> word));
      settempimmediate(len, 8);
      while shiftfactor >= 8 do
	begin
	gendouble(shiftinst, tempkey, key);
	shiftfactor := shiftfactor - 8;
	end;
      if shiftfactor > 0 then
	begin
	settempimmediate(len, shiftfactor);
	gendouble(shiftinst, tempkey, key);
	end;
      keytable[key].knowneven := mc68020 or knowneven;
      end
    else
      begin
      if backwards then
	if shiftinst = asl then shiftinst := asr
	else shiftinst := lsr;
      loaddreg(left, right, true);
      settempreg(keytable[right].len, dreg, getdreg);
      gensimplemove(right, tempkey);
      gen2(shiftinst, len, tempkey, key);
      end;

    keytable[key].signed := keytable[left].signed;
  end {shiftlintx} ;


procedure cvtrdx;

{ Convert a 4 byte real to an 8 byte real.
}


  begin {cvtrdx}

  if mc68881 then
    begin
    address(left);

    { If safe, delay popping to the next 68881 instruction. }
    if (pseudoinst.refcount = 1) and popping(left) then setallfields(left)
    else setkeyvalue(left);

    keytable[key].len := long; { The next operation will convert it. }
    end
  else
    begin
    saveactivekeys;
    reserve_dreg(left, 4);
    dregisters[4] := dregisters[4] - 1000; {make it attractive to unpack}
    unpack(left, long);
    dregisters[4] := dregisters[4] + 1000;
{    aligntemps;}

    with keytable[left] do
      if (oprnd.m <> dreg) or (oprnd.reg <> 4) then
        begin
        settempdreg(len, 4);
        gensimplemove(left, tempkey);
        end;

    makestacktarget;
    stackoffset := - keytable[stackcounter].oprnd.offset;
    callsupport(libcvtrd);
    end;
  end; {cvtrdx}


procedure cvtdrx;

{ Convert an 8 byte real to a 4 byte real.  This is the sngl function.
}


  begin {cvtdrx}
  address(left);

  if mc68881 then
    begin
    { If safe, delay popping to the next 68881 instruction. }
    if (pseudoinst.refcount = 1) and popping(left) then setallfields(left)
    else setkeyvalue(left);
    keytable[key].len := keytable[left].len;
    end
  else
    begin
    saveactivekeys;
    reserve_dreg(left, 4);
    pushone(left);
    returntemps(1);
    callsupport(libcvtdr);
    setd4result;

    if (keytable[left].oprnd.m = dreg) and
       (keytable[left].oprnd.reg = 4) then
      markdreg(4); { Result is going in D4 -- must kill operand in D4 }
    end;
  end; {cvtdrx}


procedure castrealx;

{ Convert one size of a real number to another.  For the mc68881 this
  is simple, we just load it into a register and size differences
  disappear.  Otherwise we call a routine to do the work.
}
  var
    temp: keyindex;

  begin
    if mc68881 then
      begin
      address(left);
      if keytable[left].oprnd.flavor = int_float then
	keytable[left].oprnd.flavor := float;
      if (keytable[left].oprnd.m = fpreg) and (keytable[left].len <= len) then
	setkeyvalue(left)
      else
	begin
	if target <> 0 then temp := target
	else
	  begin
	  if loadeddreg(left, true) then settempfpreg(keytable[left].oprnd.reg)
	  else settempfpreg(getfpreg);
	  temp := tempkey;
	  keytable[temp].len := len;
	  end;
	genfpmove(left, tempkey);
	setkeyvalue(tempkey);
	end;
      end
    else
      begin
      aligntemps;
      makestacktarget;
      aligntemps;
      address(left);
      newtemp(keytable[left].len);
      keytable[stackcounter].tempflag := true;
      genblockmove(left, stackcounter, long);
      if len = quad then callandpop(libfd, 1)
      else callandpop(libdf, 1);
      with keytable[stackcounter] do
        begin
        tempflag := true;
        instmark := lastnode + 1;
        end;
      end;
  end;


procedure castrealintx;

{ Convert a real value into an integer value.  Ansi C does not specify
  whether this rounds or truncates:  we choose truncation as the easiest
  to handle.
}
  begin
    pseudoinst.oprnds[2] := ord(truncid);
    sysfnintx;
  end; {castrealx}


procedure castintx;

{ Cast an int or pointer value to a different size
}
  begin
    unpackshrink(left, len);
    setallfields(left);
    keytable[key].signed := right = 0;
  end; {castintx}


procedure setarithmetic(inst: insttype; {to form result}
                        compflag: boolean {complement right side if true} );

{ Generate code for the binary set operators "+", "-", and "*".
  Short sets, 4 bytes or less, are handled by "integerarithmetic"
  unless compflag is true.  The usual loop logic is used to govern
  straightline or loop implementation of the operator.
}

  var
    pieces: integer; {number of inline loop elements to generate}
    i: integer; {induction var for generating loop code}
    loop: boolean; {set true if loop required}
    threeoperands: boolean; {set true if not of form x:=x op y }
    backwards: boolean; { True if right is already on the stack and becomes the
                         destination }
    eor_reg: regindex; { scratch reg for eor instruction }


  begin {setarithmetic}
    { Sets are always knowneven.
    }
    keytable[left].knowneven := true;
    keytable[right].knowneven := true;

    len := min(keytable[left].len, keytable[right].len);
    keytable[key].len := len; { Required when target is shorter than key. }

    if ((len = byte) or (len = word) or (len = long)) then
      begin
      if keytable[left].oprnd.m = pcrelative then
        begin
        dereference(left);
        make_immediate(left, false);
        end;

      if keytable[right].oprnd.m = pcrelative then
        begin
        dereference(right);
        make_immediate(right, compflag);
        end
      else
        if compflag then
          begin
          unpack(right, len);
          settempreg(len, dreg, getdreg);
          gensimplemove(right, tempkey);
          gen1(notinst, len, tempkey);
          right := tempkey;
{          keytable[right].refcount := 1;     * not needed *
          adjustregcount(right, 1); }
          end;

      if inst = eor then xorintx
      else integerarithmetic(inst);
      end
    else
      begin
      addressboth;

      { If the right side is on the stack and it is the target, then don't
        pop it off.
      }
      with keytable[right].oprnd do
        if (m = relative) and (reg = sp) and equivaccess(right, target) then
          begin
          backwards := true;
          loadset(right);
          threeoperands := false;
          settargetused := false;
          end
        else
          begin
          backwards := false;
          loadset(left);

          if (target > 0) and not equivaccess(left, target) then
            begin
            lock(left);
            lock(right);
            makeaddressable(target);
            unlock(right);
            unlock(left);
            if key > lastkey then lastkey := key; { Cause markareg to affect
                                                    "key" also. }
            threeoperands := true;
            end
          else threeoperands := not equivaccess(left, key);
          end;

      if backwards then
        initloop(left, right, key, maxint, 3, loop, pieces)
      else initloop(right, left, key, maxint, 3, loop, pieces);

      if threeoperands and (inst = eor) then
        begin
        settempdreg(piecesize, getdreg);
        eor_reg := tempkey;
        lock(eor_reg);
        settempdreg(piecesize, getdreg);
        unlock(eor_reg);
        end
      else settempdreg(piecesize, getdreg);

      for i := 1 to pieces do
        begin
        onlyreference(loopsrc);
        gen2(move, piecesize, loopsrc, tempkey);

        if compflag then
          if backwards then gen1(notinst, piecesize, loopdst)
          else gen1(notinst, piecesize, tempkey);

        onlyreference(loopdst);

        if threeoperands then
          begin
          onlyreference(loopsrc1);

          if inst = eor then
            begin
            gen2(move, piecesize, loopsrc1, eor_reg);
            gen2(inst, piecesize, eor_reg, tempkey);
            end
          else gen2(inst, piecesize, loopsrc1, tempkey);

          gen2(move, piecesize, tempkey, loopdst);
          end
        else gen2(inst, piecesize, tempkey, loopdst);
        bumploop(dbra, loop);
        end;
      finishloop;
      end;

    { In some cases we generate redundant set moves when this routine has
      correctly used the target.  The flag "settargetused" prevents that.
    }
    if settargetused and (pseudobuff.op <> movset) then settargetused := false;
  end {setarithmetic} ;

function accessbit(k: keyindex; {describes set operand}
                   forcedreg: boolean {true sez a dreg is required})
                   : keyindex;

{ Generate setup code to access a single bit in a set.  The bit desired
  is assumed to be accessed by keytable[left].

  The function returns the descriptor to be used with the following bit
  operator, which must be of the form "gendouble(Bop, left, <result>)",
  where Bop will be a btst or bset instruction.

  In general, these bit operations are a pain in the butt and by far the
  most poorly designed instructions in the machine.
}

  var
    twoimmediates: boolean;

  begin {accessbit}
    { The immediate check below is required because with kludge records and
      constant propagation it is possible to try to generate:
        btst #xxx,#yyy
      which is an illegal 68k instruction.  The fix is to force the operand
      into a register.
    }
    if (keytable[left].oprnd.m = immediate) and
       (keytable[k].oprnd.m = immediate) then twoimmediates := true
    else twoimmediates := false;

    lock(k);
{    unpackshrink(left, word);  This call was moved outside of accessbit
                                because makeaddressable was causing
                                a register restore to occur inside a loop
                                in setinsert.
}

    if (keytable[left].oprnd.m <> dreg) and
       (forcedreg or twoimmediates or
       (keytable[left].oprnd.m <> immediate)) then
      begin
      settempreg(keytable[left].len, dreg, getdreg);
      gensimplemove(left, tempkey);
      changevalue(left, tempkey);
      end;

    unlock(k);

    with keytable[left].oprnd, keytable[k] do
    if (len > byte) and (oprnd.m = dreg) then
      if m = immediate then
        begin
        settempimmediate(len, (len - offset div 8 - 1) * 8 + offset mod 8);
        changevalue(left, tempkey);
        end
      else if m = dreg then
        begin
        aligntemps; {just to be sure}
        newtemp(len);
        keytable[stackcounter].tempflag := true;
        gensimplemove(k, stackcounter);
        changevalue(k, stackcounter);
        end;

    if (keytable[k].len <= byte) or (keytable[k].oprnd.m = dreg) then
      accessbit := k

    else if keytable[left].oprnd.m = immediate then
      begin
      with keytable[k].oprnd do
{?????}
        forcerelative(k, true, true {false},
                      offset + keytable[left].oprnd.offset div 8, true);

      with keytable[k].oprnd do
        settemp(word, m, reg, indxr, indxlong,
                offset + keytable[left].oprnd.offset div 8, 0, scale,
                commonlong_reloc);
      accessbit := tempkey;
      end
    else if twoimmediates then accessbit := k
    else
      begin
      lock(left);
      forcerelative(k, false, true, 0, true);

      with keytable[k].oprnd do
        begin
        lock(k); { So we don't loose the old index reg }
        settemp(word, indexed, reg, getdreg, false, offset, 0, 1, unknown);
        unlock(k);
        end;
      unlock(left);
      accessbit := tempkey;
      settempreg(word, dreg, keytable[tempkey].oprnd.indxr);
      gensimplemove(left, tempkey);
      settempimmediate(word, 3);
      gendouble(lsr, tempkey, tempkey + 1);
      end;
  end {accessbit} ;


procedure dosetx;

{ Define the set base for a set insertion.  This is the constant part of
  a set constructor, which may be followed by zero or more setinsert
  pseudo-operations.  Sets "settarget" to the desired target of the
  set insertion operators.  Some special things:  If "settarget" has
  "packedaccess" set, or represents a data register, we create a stack
  temp.  Otherwise we just copy the operand information stored in
  keytable[left].
}


  begin {dosetx}
    firstsetinsert := true;
    address(left);
    if (keytable[left].oprnd.m = dreg) or keytable[left].packedaccess then
      begin
      makestacktarget;
      settarget := key;
      end
    else
      begin
      setkeyvalue(left);
      settarget := target;
      end;
  end {dosetx} ;



procedure fix_set_addressing(which: keyindex; {which key to diddle}
                             bitkey: keyindex {result of accessbit});

{ This routine fixes up the index register in the case that left is not
  immediate and right was indexed or scaled.  This information is lost
  by accessbit. 
}

  var
    tempreg: regindex;

  begin {fix_set_addressing}
    with keytable[which], oprnd do
      if (keytable[left].oprnd.m <> immediate) and (m = indexed) then
	begin
	tempreg := indxr;

	{ On the MC68020, there may be a scale factor on the index register in
	  which case we must manually perform the scaling here.
	}
	if mc68020 and (scale > 1) then
	  begin
	  settempdreg(2 + 2 * ord(indxlong), indxr);

	  if not ((refcount <= 1) and (indxr <= lastdreg) and
	    (dregisters[indxr] + ord(context[contextsp].dbump[indxr]) <=
	    countdreg)) then
	    begin
	    lock(left);
	    lock(bitkey);
	    lock(right);
	    tempreg := getdreg;
	    settempdreg(long, tempreg);
	    unlock(left);
	    unlock(bitkey);
	    unlock(right);
	    gensimplemove(tempkey + 1, tempkey);
	    end;

	  { floor(x/4)+1 maps 2,4,8 to 1,2,3 }
	  settempimmediate(long, scale div 4 + 1);
	  gendouble(asl, tempkey, tempkey + 1);
	  end;

	{ If right is indexed, then we must add in the index here.
	}
	if len > 1 then
	  begin
	  settempdreg(2 + 2 * ord(indxlong), tempreg);
	  settempdreg(2 + 2 * ord(keytable[bitkey].oprnd.indxlong),
		      keytable[bitkey].oprnd.indxr);
	  gendouble(add, tempkey + 1, tempkey);
	  end;
	end;
  end; {fix_set_addressing}


procedure insetx;

{ Implement the set "in" operator.  Tests bit corresponding to left
  operand in set described by right operand.
}

  var
    checkrange: boolean; {true if left operand might be outside range
                          0..255 and if set is full-sized}
    bitkey: keyindex; {result of accessbit}

  begin {insetx}
    unpack(right, 1);
    if language = modula2 then
      begin
      if (keytable[left].oprnd.m = immediate) and
         (keytable[left].oprnd.offset < keytable[right].len * bitsperunit) then
        checkrange := false
      else
        checkrange := (keytable[left].len > 1) or (keytable[right].len < 32);
      end
    else { pascal }
      begin
      checkrange := false;
      if (keytable[right].len = 32) then
        with keytable[left], oprnd do
          if len > 1 then
            if (m <> immediate) or (offset < 0) or (offset > 255) then
              checkrange := true;
      end;

    lock(right);
    unpackshrink(left, word);
    unlock(right);

    bitkey := accessbit(right, checkrange);

    if checkrange then
      begin
      case language of
        pascal:
          begin
          settempimmediate(long, 32);
          with keytable[bitkey].oprnd do
            if indxlong then settempdreg(long, indxr)
            else settempdreg(word, indxr);
	  gendouble(cmp, tempkey + 1, tempkey);
	  genrelbr(blt, 2);
	  settempimmediate(long, 0);
	  gendouble(move, tempkey, tempkey + 1);
          end;
        modula2:
          begin
	  if (keytable[right].len = 1) or (keytable[right].oprnd.m = dreg) then
	    settempimmediate(long, keytable[right].len * bitsperunit)
	  else settempimmediate(long, keytable[right].len);

	  if (keytable[right].len = 1) or (keytable[right].oprnd.m = dreg) then
	    settempdreg(word, keytable[left].oprnd.reg)
	  else
	    with keytable[bitkey].oprnd do
	      if indxlong then settempdreg(long, indxr)
	      else settempdreg(word, indxr);
	  gendouble(cmp, tempkey + 1, tempkey);
	  genrelbr(blt, 2);
	  settempimmediate(word, 4); { zero condition code }
	  gen1(move_to_ccr, word, tempkey);
          end;
        end; {case}
      genbr(bra, lastlabel);
      labelnextnode := true;
      end;

    fix_set_addressing(right, bitkey);

    gendouble(btst, left, bitkey);

    if checkrange then definelastlabel;

    setbr(bne);
  end {insetx} ;


procedure setbit(value: boolean);

{ Implement the Modula2 INCL and EXCL functions.  Sets or clears the bit
  indicated by right in the set in left.
}

  var
    checkrange: boolean; {true if left operand might be outside range
                          0..255 and if set is full-sized}
    bitkey: keyindex; {result of accessbit}
    kind: standardids;


  begin {setbit}
    if language = modula2 then
      begin
      right := dummyarg_stack[0];
      left := dummyarg_stack[1];

      unpack(right, 1);
      lock(right);
      unpackshrink(left, word);
      unlock(right);

      bitkey := accessbit(right, false);

      fix_set_addressing(right, bitkey);

      if value then gendouble(bset, left, bitkey)
      else gendouble(bclr, left, bitkey);

      dummyarg_ptr := 0;
      end;
  end {setbit} ;


procedure setinsertx;

{ Insert an element (or range) into a set.  The constant part of any
  such insertion is set up by "dosetx".  This attempts to do the
  insertion in place if possible.

  "Target" will be the constant part of the set constructor, oprnds[1]
  is the element to insert or the first element if oprnds[2] is non-
  zero.  "Settarget" is assumed to be the target of the whole operation.

  The bit is inserted with a "btst" using information computed
  by "accessbit".  The range specification is implemented as a loop.
}


  var
    temp: integer; { holds refcount temporarily }
    bitkey: keyindex; {Result of accessbit}

  begin {setinsertx}
    with keytable[target] do
      if firstsetinsert then
        if (settarget <> 0) and not equivaccess(settarget, left) and
           not equivaccess(settarget, right) then {We can do it in place}
          begin
          genblockmove(target, settarget, min(len, word));
          adjustregcount(target, - keytable[target].refcount);
          keytable[target].oprnd := keytable[settarget].oprnd;
          adjustregcount(target, keytable[target].refcount);
          target := settarget;
          end
        else
          begin {do it in a temporary location}
          with keytable[target] do
            if regsaved then { kill off this temp first }
              begin
              keytable[properreg].refcount := 0;
              regsaved := false;
              end;

          newtemp(len);
          keytable[stackcounter].tempflag := true;
          dereference(target);
          genblockmove(target, stackcounter, min(len, word));
          rereference(target);
          keytable[target].oprnd := keytable[stackcounter].oprnd;
          keytable[target].properreg := stackcounter;
          keytable[target].regsaved := true;
          keytable[target].knowneven := true;
          keytable[target].regvalid := true;
          bumptempcount(target, keytable[target].refcount);
          end;

    firstsetinsert := false;

    if right <> 0 then
      begin
      unpack(left, word);
      lock(left);
      settempdreg(keytable[left].len, getdreg);
      unlock(left);
      gensimplemove(left, tempkey);
{      adjustregcount(left, - keytable[left].refcount);}
      keytable[tempkey].len := keytable[left].len;
      left := tempkey;
{      keytable[left].oprnd := keytable[tempkey].oprnd;}
{      rereference(left);}
      unpack(right, byte);
      lock(right);
      genbr(bra, lastlabel - 1);
      definelastlabel;
      end
    else
      begin
      lock(target);
      unpackshrink(left, word);
      unlock(target);
      end;

    bitkey := accessbit(target, false);

    fix_set_addressing(target, bitkey);

    gendouble(bset, left, bitkey);

    if right <> 0 then
      begin
      settempimmediate(keytable[left].len, 1);
      gendouble(add, tempkey, left);
      definelastlabel;
      gendouble(cmp, right, left);
      genbr(ble, lastlabel + 2);
      unlock(right);
      end;
  end {setinsertx} ;

procedure sysroutinex;

{ Generate code for a system routine.  This usually results in a call
  to a support library routine, but some of them are special.
}


    procedure openx(libroutine: libroutines {support routine to call} );

{ Call a routine which can open a file.  This is reset or rewrite.
  Some of the arguments can be defaulted, so a count must be kept and
  the omitted ones zeroed.
}


      begin {openx}
        clearsp(2 * (5 - (filestkcnt - stackcounter)));
        callandpop(libroutine, filestkcnt - stackcounter + 2);
      end {openx} ;


    procedure fsincosfn;

    { Generate the 68881 fsincos instruction.  It has three arguments:
      source, sine and cosine.
    }

      var
        sinekey, cosinekey: keyindex;

      begin {fsincosfn}
        address(dummyarg_stack[0]);
        address(dummyarg_stack[1]);
        address(dummyarg_stack[2]);
        lock(dummyarg_stack[0]);

        if (tempkey - 2) <= lowesttemp then compilerabort(interntemp);
        tempkey := tempkey - 1;
        sinekey := tempkey;
        keytable[sinekey] := keytable[dummyarg_stack[1]];
        tempkey := tempkey - 1;
        cosinekey := tempkey;
        keytable[cosinekey] := keytable[dummyarg_stack[2]];

        if (keytable[sinekey].oprnd.m <> fpreg) then
          begin
          with keytable[sinekey], oprnd do
            begin
            m := fpreg;
            lock(cosinekey);
            reg := getfpreg;
            flavor := float;
            unlock(cosinekey);
            end;
          end;

        if (keytable[cosinekey].oprnd.m <> fpreg) then
          begin
          with keytable[cosinekey], oprnd do
            begin
            m := fpreg;
            lock(sinekey);
            reg := getfpreg;
            flavor := float;
            unlock(sinekey);
            end;
          end;

        settemp(keytable[dummyarg_stack[0]].len, twofpregs,
                keytable[sinekey].oprnd.reg,
                keytable[cosinekey].oprnd.reg, false, 0, 0, 1, unknown);

        with keytable[dummyarg_stack[0]].oprnd do if flavor = int_float then
             flavor := float;

        keytable[dummyarg_stack[1]].oprnd.flavor := float;
        keytable[dummyarg_stack[2]].oprnd.flavor := float;
        fpgendouble(fsincos, dummyarg_stack[0], tempkey);

        if keytable[dummyarg_stack[2]].oprnd.m <> fpreg then
          genfpmove(cosinekey, dummyarg_stack[2]); {cosine}

        if keytable[dummyarg_stack[1]].oprnd.m <> fpreg then
          genfpmove(sinekey, dummyarg_stack[1]); {sine}

        unlock(dummyarg_stack[0]);
        dummyarg_ptr := 0;
      end; {fsincosfn}


    procedure setfpcrfn;

    { Generate the 68881 fmove-to-fpcr instruction.  It has two arguments:
      control register constant and value.
    }


      begin {setfpcrfn}
        address(dummyarg_stack[0]);
        address(dummyarg_stack[1]);
        keytable[dummyarg_stack[0]].oprnd.m := special_immediate;
        fpgendouble(fmove_to_fpcr, dummyarg_stack[1], dummyarg_stack[0]);
        dummyarg_ptr := 0;
      end; {setfpcrfn}


  begin {sysroutinex}
    if switcheverplus[sharecode] then saveactivekeys;
    case standardids(pseudoinst.oprnds[1]) of
      pageid: calliosupport(libpage, 0);
      putid: callandpop(libput, 1);
      getid: callandpop(libget, 1);
      breakid: callandpop(libbreak, 1);
      seekid: callandpop(libseek, 2);
      closeid: callandpop(libclose, 1);
      resetid: openx(libreset);
      rewriteid: openx(librewrite);
      packid: callandpop(libpack, 11);
      unpackid: callandpop(libunpack, 11);
      newid: callandpop(libnew, 2);
      disposeid: callandpop(libdispose, 2);
      renameid: callandpop(librename, 3);
      noioerrorid: callandpop(libnoioerror, 1);
      deleteid: callandpop(libdelete, 1);
      writeid, readid:
        if filenamed then popstack(1);
      writelnid: calliosupport(libwriteln, 0);
      readlnid: calliosupport(libreadln, 0);
      insertid: callandpop(libinsert, 4);
      deletestrid: callandpop(libdeletestr, 4);
      valprocid:
        if target = 0 then callandpop(libvalint, 3)
        else if (target = 2) or switcheverplus[doublereals] then
          callandpop(libvaldouble, 3)
        else callandpop(libvalreal, 3);
      strid:
        if target = 0 then
          begin
          if formatcount = 0 then callandpop(libstrint0, 3)
          else callandpop(libstrint1, 4);
          end
        else if (target = 2) or switcheverplus[doublereals] then
          begin
          if formatcount = 0 then callandpop(libstrdouble0, 3)
          else if formatcount = 1 then callandpop(libstrdouble1, 4)
          else callandpop(libstrdouble2, 5);
          end
        else
          begin
          if formatcount = 0 then callandpop(libstrreal0, 3)
          else if formatcount = 1 then callandpop(libstrreal1, 4)
          else callandpop(libstrreal2, 5);
          end;
      setfpcrid: setfpcrfn;
      fsincosid: fsincosfn;
      inclid: setbit(true);
      exclid: setbit(false);
      end;
    filenamed := false;
    formatcount := 0;
    dontchangevalue := 0;
    paramlist_started := false; {reset switch}
  end {sysroutinex} ;



procedure loopholefnx;

{ Generate code for a loophole function.  This actually generates code
  only in the cases where the argument is in a register (a or d), or
  immediate modes, or the operand must be aligned on a word boundary.
}


  begin
    unpackshrink(left, len);
    with keytable[left].oprnd do
      if m = fpreg then
        begin
        makestacktarget;
        keytable[key].oprnd.flavor := float;
        genfpmove(left, key);
        end
      else if m in [immediate, immediatelong, areg, dreg] then
        {this handles the non-structure reference}
        begin
        makestacktarget;

        if len <= long then gensimplemove(left, key)
        else genblockmove(left, key, byte);

        keytable[key].signed := keytable[left].signed;
        end
      else if not mc68020 and (len > 1) and
              (m in [relative, indexed, pcrelative]) and
              (not keytable[left].knowneven or odd(offset)) then
        {this handles the case where the operand is not word-aligned}
        begin
        makestacktarget;
        genblockmove(left, key, byte);
        keytable[key].signed := keytable[left].signed;
        end
      else setallfields(left);

    if mc68881 then keytable[key].oprnd.flavor := int_float; { Can't be sure }
    keytable[key].signed := (pseudoinst.oprnds[2] = 0);
  end; {loopholefnx}



procedure sysfnstringx;

{ Generate code for a system function with string arguments.
}


  procedure simplestringfn(libroutine: libroutines; {support routine to call}
                           n: integer {number of stack temps to pop} );


    begin {simplestringfn}
      callandpop(libroutine, n);
      with keytable[stackcounter] do
        begin
        tempflag := true;
        refcount := 0;
        end;
      with keytable[key] do
        begin
        regsaved := true;
        properreg := stackcounter;
        end;
      setkeyvalue(stackcounter);
    end {simplestringfn} ;


  begin {sysfnstringx}
    if switcheverplus[sharecode] then saveactivekeys;
    case standardids(right) of
      posid: simplestringfn(libpos, 2);
      copyid: simplestringfn(libcopy, 3);
      end;
    dontchangevalue := dontchangevalue - 1;
  end {sysfnstringx} ;


procedure sysfnintx;

{ Generate code for a system routine with scalar (non-real) argument.
  These functions are all generated inline.
}


    procedure callsimplefn(libroutine: libroutines {routine to call} );

{ Call a simple function, with one argumument, reserving space on the
  stack as though it had been declared in Pascal.
}


      begin {callsimplefn}
        makestacktarget;
        keytable[key].len := len;
        aligntemps;
        address(left);
        newtemp(keytable[left].len);
        gen1(pea, long, fix_effective_addr(left));
        stackoffset := keytable[stackcounter].oprnd.offset;
        callandpop(libroutine, 1);
        with keytable[stackcounter] do
          begin
          tempflag := true;
          instmark := lastnode + 1;
          end;
      end {callsimplefn} ;


  procedure truncround(kind: standardids { round, trunc, or fint } );

{ Generate code for a truncate or round instruction.
  This is currently implemented with a support library routine.
}


    begin
      address(left);

      if mc68881 then
        begin
        keytable[left].oprnd.flavor := float;

        if kind = roundid then
          begin
          { FINT does round-to-nearest and does not work for values with an
            odd integer part and a 0.5 fraction, so this code will add 0.5
            for positive values (or subtract 0.5 for negatives) and truncate.
          }
          loadfpreg(left, 0, true);
          settempimmediate(long, $3F000000);
          keytable[tempkey].oprnd.flavor := float;
          lock(left);
          settempfpreg(getfpreg);
          unlock(left);
          genfpmove(tempkey + 1, tempkey);
          fpgensingle(ftst, key);
          genrelbr(fbge,1);
          fpgendouble(fneg, tempkey, tempkey);
          labelnextnode := true;
          fpgendouble(fadd, tempkey, key);
          left := key;
          end
        else setvalue(fpreg, getfpreg, 0, false, 0, 0);

        if kind = fintid then fpgendouble(fint, left, key)
        else fpgendouble(fintrz, left, key);

        keytable[key].oprnd.flavor := int;

        { The result of the FINT or FINTRZ is an integer stored in an fpreg
          and must be moved here into the target or a dreg.  This is again
          because a real number is considered an "int" until a real operation
          is performed on it and then it is changed to a "float".  We must
          do an adjustregcount on "key" because the next pseudo-op will never
          get to derefernce this key.
        }
        adjustregcount(key, - keytable[key].refcount);

        if (target > 0) and (keytable[key].refcount <= 1) then
          begin
          with keytable[target], oprnd do if flavor = int_float then
            flavor := int;
          genfpmove(key, target);
          setallfields(target);
          keytable[key].len := keytable[target].len;
          end
        else loaddreg(key, 0, true);
        end
      else if keytable[left].len = quad then
        begin
        saveactivekeys;
        reserve_dreg(left, 4);
        pushone(left);
        returntemps(1);
        if kind = roundid then callsupport(libdround)
        else callsupport(libdtrunc);
        setd4result;

        if (keytable[left].oprnd.m = dreg) and
           (keytable[left].oprnd.reg = 4) then
          markdreg(4); { Result is going in D4 -- must kill operand in D4 }
        end
      else if kind = roundid then callsinglesim(libfround)
      else callsinglesim(libftrunc);
    end {truncround} ;


  procedure absintx;

{ Generate code for abs(x)
}


    begin
      unpackshrink(left, len);
      loaddreg(left, 0, false);
      gensingle(tst, key);
      genrelbr(bpl, 1);
      gensingle(neg, key);
      labelnextnode := true;
    end {absintx} ;


  procedure oddx;

{ Generate code for odd(x), just mask off the last bit.
}


    begin
      unpack(left, 1);
      keytable[key].len := keytable[left].len;
      if keytable[left].oprnd.m = dreg then setkeyvalue(left)
      else loaddreg(left, 0, true);
      settempimmediate(word, 0);
      gendouble(btst, tempkey, key);
      adjustregcount(key, - keytable[key].refcount);
      setbr(bne);
    end {oddx} ;


  procedure eolneofx(whichbit: addressrange {mask to choose condition bit} );

{ Generate code for an eoln or eof call.  This checks the status bit in the
  file control table.  This code is dependent on being preceded
  by a "definelazy" that sets the key to a register holding the filevar.
}


    begin
      dereference(left);
      settempareg(keytable[left].oprnd.reg);
      with keytable[tempkey], oprnd do begin
        m := relative;
        offset := long;
        end;
      settempimmediate(word, whichbit);
      gendouble(btst, tempkey, tempkey + 1);
      adjustregcount(key, - keytable[key].refcount);
      setbr(bne);
    end {eolnx} ;


  procedure capx;

  { Modula-2 CAP function.  Make room for the result and push the value of
    the argument on the stack.
  }

    begin
      if language = modula2 then
        begin
        makestacktarget;
        keytable[key].len := len;
        aligntemps;
        unpackshrink(left, len);
        newtemp(len);
        gensimplemove(left, stackcounter);
        callandpop(libcap, 1);
        with keytable[stackcounter] do
          begin
          tempflag := true;
          instmark := lastnode + 1;
          end;
        end;
    end;


  begin {sysfnintx} ;
    if switcheverplus[sharecode] then saveactivekeys;
    case standardids(pseudoinst.oprnds[2]) of
      oddid: oddx;
      eolnid: eolneofx(eolnbit);
      eofid: eolneofx(eofbit);
      roundid, truncid, fintid:
        truncround(standardids(pseudoinst.oprnds[2]));
      succid: incdec(add, false);
      predid: incdec(sub, false);
      sqrid:
        if not mc68020 and (bytelength(left) > word) then
          begin
          saveactivekeys;
          reserve_dreg(left, 4);
          reserve_dreg(left, 3);
          dregisters[4] := dregisters[4] - 1000; {make it attractive}
          unpack(left, long);
          dregisters[4] := dregisters[4] + 1000;
          settempdreg(long, 4);

          if (keytable[left].oprnd.m <> dreg) or
             (keytable[left].oprnd.reg <> 4) then
            gensimplemove(left, tempkey);

          if (keytable[left].oprnd.m <> dreg) or
             (keytable[left].oprnd.reg <> 3) then
            begin
            settempdreg(long, 3);
            gensimplemove(tempkey + 1, tempkey);
            end;

          if (keytable[left].oprnd.m = dreg) and
             (keytable[left].oprnd.reg = 4) then
            markdreg(4); { Result is going in D4 -- must kill operand in D4 }

          callsupport(libimult);
          setd4result;
          end
        else
          begin
          { This code handles either a 68000 word length sqr or a 68020
            long square.
          }
{          unpackshrink(left, word + (ord(mc68020) * (len - 2)));}
          unpackshrink(left, keytable[left].len);

          { The above length hack is necessary because the front-end may throw
            in an implicit word to long conversion on the 68000 which makes the
            multiply fail.  68020 mode should allow either a word or long.
          }
          loaddreg(left, 0, true);
          keytable[key].signed := keytable[left].signed;
          keytable[key].len := keytable[left].len;
          if keytable[key].signed then gendouble(muls, key, key)
          else gendouble(mulu, key, key);
          keytable[key].len := long;
          end;
      absid: absintx;
      ioerrorid: callsimplefn(libioerror);
      iostatusid: callsimplefn(libiostatus);

      readfpcrid:

      { 68881 Move from system control register.
      }
        begin
        if (keytable[key].refcount = 1) and (keytable[target].len = len) and
           (target > 0) and (target <= lastkey) and
           not keytable[target].packedaccess and
           (keytable[target].oprnd.m = dreg) then
          setvalue(dreg, keytable[target].oprnd.reg, 0, false, 0, 0)
        else setvalue(dreg, getdreg, 0, false, 0, 0);

        keytable[left].oprnd.m := special_immediate;
        fpgendouble(fmove_from_fpcr, left, key);
        { The resulting flavor is int }
        end;

      capid: capx; { modula2 CAP function }
      end;
  end {sysfnintx} ;

procedure fpfunction(inst: insttype);

{ Call the 68881 and perform the indicated function.
}

  begin {fpfunction}
    address(left);

    with keytable[left].oprnd do if flavor = int_float then
      flavor := float;

    if loadedfpreg(left, true) and (target = 0) then
      begin
      setkeyvalue(left);
      fpgendouble(inst, key, key);
      end
    else if (target > 0) and (keytable[target].oprnd.m = fpreg) and
            (keytable[key].refcount <= 1) then
      begin
      fpgendouble(inst, left, target);
      setkeyvalue(target);
      end
    else
      begin
      settempfpreg(getfpreg);
      fpgendouble(inst, left, tempkey);
      setkeyvalue(tempkey);
      end;

    keytable[key].oprnd.flavor := float;
  end; {fpfunction}


procedure fpfunc2(op: standardids);

{ Call the 68881 and perform the indicated function.
}

  var
    inst: insttype;

  begin {fpfunc2}
    case op of
      fmodid: inst := fmod;
      fremid: inst := frem;
      fscaleid: inst := fscale;
      fsgldivid: inst := fsgldiv;
      fsglmulid: inst := fsglmul;
      end;

    address(dummyarg_stack[0]);
    address(dummyarg_stack[1]);

    if (target <> 0) and (equivaccess(dummyarg_stack[1], target)) then
      target := 0;

    with keytable[dummyarg_stack[0]].oprnd do if flavor = int_float then
         flavor := float;

    loadfpreg(dummyarg_stack[0], dummyarg_stack[1], true);

    with keytable[dummyarg_stack[1]].oprnd do if flavor = int_float then
         flavor := float;

    { Args are reversed here so args in the source are in a logical sequence.
    }
    fpgendouble(inst, dummyarg_stack[1], key);
    dummyarg_ptr := 0;
  end; {fpfunc2}


procedure fmovecrfn;

{ Generate the FMOVECR 68881 instruction.
}

  begin
    if (target <> 0) and (keytable[target].oprnd.m = fpreg) and
      keytable[target].regvalid then
      setvalue(fpreg, keytable[target].oprnd.reg, 0, false, 0, 0)
    else setvalue(fpreg, getfpreg, 0, false, 0, 0);

    keytable[left].oprnd.m := special_immediate;
    fpgendouble(fmovecr, left, key);
  end; {fmovecrfn}


procedure negrealx;

{ Negate a real number.
}

  begin
  if mc68881 then fpfunction(fneg)
  else unaryrealx(bchg);
  end;


procedure sysfnrealx;

{ Generate code for a system function with a real argument.
}

  procedure callrealfn(libroutine: libroutines {system routine number} );

{ Call a system real arithmetic function.
}


    begin {callrealfn}
      makestacktarget;
      aligntemps;
      address(left);
      newtemp(len);
      genblockmove(left, stackcounter, long);

      if len = quad then
        case libroutine of
          libarctan: libroutine := libdarctan;
          libcos: libroutine := libdcos;
          libexp: libroutine := libdexp;
          libln: libroutine := libdln;
          libsin: libroutine := libdsin;
          libsqrt: libroutine := libdsqrt;
          end;
      callandpop(libroutine, 1);

      with keytable[stackcounter] do
        begin
        tempflag := true;
        instmark := lastnode + 1;
        end;
    end {callrealfn} ;

  procedure square_real;

{ Square a real number, single or double.
}


    begin
      if mc68881 then 
        begin
        address(left);

        with keytable[left].oprnd do if flavor = int_float then
          flavor := float;

        if loadedfpreg(left, true) and (target = 0) then
          begin
          setkeyvalue(left);
          fpgendouble(fmul, key, key);
          end
        else if (target > 0) and (keytable[target].oprnd.m = fpreg) and
                (keytable[key].refcount <= 1) then
          begin
          genfpmove(left, target);
          fpgendouble(fmul, target, target);
          setkeyvalue(target);
          end
        else
          begin
          settempfpreg(getfpreg);
          genfpmove(left, tempkey);
          fpgendouble(fmul, tempkey, tempkey);
          setkeyvalue(tempkey);
          end;

        keytable[key].oprnd.flavor := float;
        end
      else
        begin
        address(left);
        if len = quad then
          begin
          pushone(left);
          settos(1);
          callsupport(libdsqr);
          end
        else callsinglesim(libfsqr);
        end;
    end; {square_real}


  procedure timefn;
  { Call the library time function.  Same for 68881 and non-68881.
  }

    begin
      makestacktarget;
      aligntemps;
      if len = quad then callsupport(libdtime)
      else callsupport(libtime);
    end {timex} ;


    procedure fsincos2fn;

    { Generate the 68881 fsincos instruction when there is a sin/cos cse.
    }

      begin {fsincos2fn}
        address(left);

        if keytable[key].refcount = 2 then
          begin
          settempfpreg(getfpreg);
          lock(tempkey);
          settemp(len, twofpregs, keytable[tempkey].oprnd.reg, getfpreg,
                  false, 0, 0, 1, unknown);
          unlock(tempkey + 1);

          with keytable[left].oprnd do if flavor = int_float then
               flavor := float;

          fpgendouble(fsincos, left, tempkey);
          setkeyvalue(tempkey);
          end
        else setallfields(left);
      end; {fsincos2fn}


    procedure fsincospt2;

    { If the mode of left is twofpregs then fetch the correct result of
      in FSINCOS instruction.  If the mode is not twofpregs, then the
      FSINCOS was not needed and we must do the FSIN or FCOS here.
    }

      begin
      if keytable[left].oprnd.m = twofpregs then
        begin
        if standardids(pseudoinst.oprnds[2]) = sinid then 
          begin
          address(left);
          setvalue(fpreg, keytable[left].oprnd.indxr, 0, false, 0, 0)
          end
        else
          begin
          address(left);
          setvalue(fpreg, keytable[left].oprnd.reg, 0, false, 0, 0)
          end;
        end
      else
        if standardids(pseudoinst.oprnds[2]) = sinid then 
          fpfunction(fsin)
        else fpfunction(fcos);
      end; {fsincospt2}


  begin {sysfnrealx}
    if mc68881 then
      case standardids(pseudoinst.oprnds[2]) of
        fsincos2id: fsincos2fn;
        sinid, cosid: fsincospt2;
        absid: fpfunction(fabs);
        expid: fpfunction(fetox);
        lnid: fpfunction(flogn);
        sqrtid: fpfunction(fsqrt);
        arctanid: fpfunction(fatan);
        sqrid: square_real;
        timeid: timefn;

        facosid: fpfunction(facos);
        fasinid: fpfunction(fasin);
        fatanid: fpfunction(fatan);
        fatanhid: fpfunction(fatanh);
        fcoshid: fpfunction(fcosh);
        fetoxm1id: fpfunction(fetoxm1);
        fgetexpid: fpfunction(fgetexp);
        fgetmanid: fpfunction(fgetman);
        flog10id: fpfunction(flog10);
        flog2id: fpfunction(flog2);
        flognp1id: fpfunction(flognp1);
        fsinhid: fpfunction(fsinh);
        ftanid: fpfunction(ftan);
        ftanhid: fpfunction(ftanh);
        ftentoxid: fpfunction(ftentox);
        ftwotoxid: fpfunction(ftwotox);
        fmovecrid: fmovecrfn;

        fmodid, fremid, fscaleid, fsgldivid, fsglmulid:
          fpfunc2(standardids(pseudoinst.oprnds[2]));
        end
    else
      begin
      if switcheverplus[sharecode] then saveactivekeys;
      case standardids(pseudoinst.oprnds[2]) of
        absid: unaryrealx(bclr);
        sinid: callrealfn(libsin);
        cosid: callrealfn(libcos);
        expid: callrealfn(libexp);
        lnid: callrealfn(libln);
        sqrtid: callrealfn(libsqrt);
        arctanid: callrealfn(libarctan);
        sqrid: square_real;
        timeid: timefn;
        end {case} ;
      end;
  end {sysfnrealx} ;

procedure genone;

{ Generate code for one pseudoop.  Called by genblk for small compilers,
  called by travrs directly for large compilers.
}

  begin {genone}
    bftst_needed := false; { Used by forcebranch and unpack -- 68020 only }
    use_preferred_key := false; {code generator flag}

    tempkey := loopcount - 1;
    with keytable[tempkey], oprnd do
      begin
      signed := true;
      refcount := 0;
      len := pseudoinst.len;
      signlimit := 0;
      packedaccess := false;
      regvalid := true;
      indxrvalid := true;
      regsaved := false;
      indxrsaved := false;
      knowneven := mc68020 or not odd(pseudoinst.oprnds[2]);
      m := immediate;
      reg := 0;
      indxr := 0;
      offset := pseudoinst.oprnds[2];
      offset1 := 0;
      flavor := int_float;
      end;

    case pseudoinst.op of
      stmtbrk: stmtbrkx;
      copyaccess: copyaccessx;
      blockentry, blockexit, doint, doreal, doptr, dofptr, blockcode:
        { handled by overlay } ;
      clearlabel: clearlabelx;
      savelabel: savelabelx;
      restorelabel: restorelabelx;
      joinlabel: joinlabelx;
      pseudolabel: pseudolabelx;
      pascallabel: pascallabelx;
      pascalgoto: pascalgotox;
      defforlitindex: defforindexx(true, true);
      defforindex: defforindexx(true, false);
      defunsforlitindex: defforindexx(false, true);
      defunsforindex: defforindexx(false, false);
      fordntop: fortopx(blt, blo);
      fordnbottom: forbottomx(false, sub, bge, bhs);
      fordnimproved: forbottomx(true, sub, bge, bhs);
      foruptop: fortopx(bgt, bhi);
      forupbottom: forbottomx(false, add, ble, bls);
      forupimproved: forbottomx(true, add, ble, bls);
      forupchk: forcheckx(true);
      fordnchk: forcheckx(false);
      forerrchk: forerrchkx;
      casebranch: casebranchx;
      caseelt: caseeltx;
      caseerr: caseerrx;
      dostruct: dostructx;
      doset: dosetx;
      dolevel: dolevelx;
      dovar: dovarx(true);
      dounsvar, doptrvar, dofptrvar: dovarx(false);
      doext: doextx;
      indxchk: checkx(false, index_error);
      rangechk: checkx(true, range_error);
      congruchk: checkx(true, index_error);
      regtemp: regtempx;
      ptrtemp: ptrtempx;
      realtemp: realtempx;
      indxindr: indxindrx;
      indx: indxx;
      aindx: aindxx;
      pindx: pindxx;
      paindx: paindxx;
      createfalse: createfalsex;
      createtrue: createtruex;
      createtemp: createtempx;
      jointemp: jointempx;
      addr: addrx;
      setinsert: setinsertx;
      inset: insetx;
      movint, returnint: movintx;
      movptr, returnptr, returnfptr: movx(false, areg, @getareg);
      movlitint: movlitintx;
      movlitptr: movlitptrx;
      movreal, returnreal: movrealx;
      movlitreal: movlitrealx;
      movstruct, returnstruct: movstructx(false, true);
      movstr: movstrx;
      movcstruct: movcstructx;
      movset: movstructx(true, true);
      addstr: addstrx;
      addint: integerarithmetic(add);
      subint, subptr: integerarithmetic(sub);
      mulint: mulintx;
      stddivint: divintx(true);
      divint: divintx(false);
      getquo: getremquox(false);
      getrem: getremquox(true);
      shiftlint: shiftlintx(false);
      shiftrint: shiftlintx(true);
      negint: unaryintx(neg);
      incint: incdec(add, false);
      decint: incdec(sub, false);
      orint: integerarithmetic(orinst);
      andint: integerarithmetic(andinst);
      xorint: xorintx;
      addptr: addptrx;
      compbool: incdec(add, true);
      compint: unaryintx(notinst);
      addreal: realarithmeticx(true, libfadd, libdadd, fadd);
      subreal: realarithmeticx(false, libfsub, libdsub, fsub);
      mulreal: realarithmeticx(true, libfmult, libdmult, fmul);
      divreal: realarithmeticx(false, libfdiv, libddiv, fdiv);
      negreal: negrealx;
      addset: setarithmetic(orinst, false);
      subset: setarithmetic(andinst, true);
      mulset: setarithmetic(andinst, false);
      divset: setarithmetic(eor, false);
      stacktarget: stacktargetx;
      makeroom: makeroomx;
      callroutine: callroutinex(true);
      unscallroutine: callroutinex(false);
      sysfnstring: sysfnstringx;
      sysfnint: sysfnintx;
      sysfnreal: sysfnrealx;
      castreal: castrealx;
      castrealint: castrealintx;
      castint, castptr: castintx;
      loopholefn, castptrint, castintptr, castfptrint, castintfptr:
	loopholefnx;
      sysroutine: sysroutinex;
      chrstr: chrstrx;
      arraystr: arraystrx;
      flt: fltx;
      pshint, pshptr: pshx;
      pshfptr: pshfptrx;
      pshlitint: pshlitintx;
      pshlitptr, pshlitfptr: pshlitptrx;
      pshlitreal: pshlitrealx;
      pshreal: pshx;
      pshaddr: pshaddrx;
      pshstraddr: pshstraddrx;
      pshproc: pshprocx;
      pshstr: pshstrx;
      pshstruct, pshset: pshstructx;
      fmt: fmtx;
      setbinfile: setbinfilex;
      setfile: setfilex;
      closerange: closerangex;
      copystack: copystackx;
      rdint: rdintcharx(libreadint, defaulttargetintsize);
      rdchar: rdintcharx(libreadchar, byte);
      rdreal: rdintcharx(libreadreal, len);
      rdst:
        if filenamed then callandpop(libreadstring, 2)
        else callandpop(libreadstringi, 2);
      rdxstr: rdxstrx;
      rdbin: callsupport(libget);
      wrbin: callsupport(libput);
      wrint: wrcommon(libwriteint, 12);
      wrchar: wrcommon(libwritechar, 1);
      wrst: wrstx(true);
      wrxstr: wrstx(false);
      wrbool: wrcommon(libwritebool, 5);
      wrreal: wrrealx;
      jump: jumpx(pseudoinst.oprnds[1], false);
      jumpf: jumpcond(true);
      jumpt: jumpcond(false);

      eqreal: cmprealx(beq, libdeql, fbngl);
      neqreal: cmprealx(bne, libdeql, fbgl);
      lssreal: cmprealx(blt, libdlss, fblt);
      leqreal: cmprealx(ble, libdlss, fble);
      geqreal: cmprealx(bge, libdgtr, fbge);
      gtrreal: cmprealx(bgt, libdgtr, fbgt);

      eqint: cmpx(beq, beq, dreg, @getdreg);
      eqptr, eqfptr: cmpx(beq, beq, areg, @getareg);
      neqint: cmpx(bne, bne, dreg, @getdreg);
      neqptr, neqfptr: cmpx(bne, bne, areg, @getareg);
      leqint, leqptr: cmpx(ble, bls, dreg, @getdreg);
      geqint, geqptr: cmpx(bge, bhs, dreg, @getdreg);
      lssint, lssptr: cmpx(blt, blo, dreg, @getdreg);
      gtrint, gtrptr: cmpx(bgt, bhi, dreg, @getdreg);

      eqstruct: cmpstructx(beq);
      neqstruct: cmpstructx(bne);
      leqstruct: cmpstructx(ble);
      geqstruct: cmpstructx(bge);
      lssstruct: cmpstructx(blt);
      gtrstruct: cmpstructx(bgt);

      eqstr: cmpstrx(beq);
      neqstr: cmpstrx(bne);
      leqstr: cmpstrx(ble);
      geqstr: cmpstrx(bge);
      lssstr: cmpstrx(blt);
      gtrstr: cmpstrx(bgt);

      eqlitreal: cmplitrealx(beq, libdeql, fbngl);
      neqlitreal: cmplitrealx(bne, libdeql, fbgl);
      lsslitreal: cmplitrealx(blt, libdlss, fblt);
      leqlitreal: cmplitrealx(ble, libdlss, fble);
      gtrlitreal: cmplitrealx(bgt, libdgtr, fbgt);
      geqlitreal: cmplitrealx(bge, libdgtr, fbge);

      eqlitptr, eqlitfptr: cmplitptrx(beq);
      neqlitptr, neqlitfptr: cmplitptrx(bne);

      eqlitint: cmplitintx(beq, beq, beq);
      neqlitint: cmplitintx(bne, bne, bne);
      leqlitint: cmplitintx(ble, bls, beq);
      geqlitint: cmplitintx(bge, bhs, bra);
      lsslitint: cmplitintx(blt, blo, nop);
      gtrlitint: cmplitintx(bgt, bhi, bne);

      eqset: cmpstructx(beq);
      neqset: cmpstructx(bne);
      geqset: cmpsetinclusion(left, right);
      leqset: cmpsetinclusion(right, left);

      postint: postintptrx(false);
      postptr: postintptrx(true);
      postreal: postrealx;

      ptrchk: ptrchkx;
      definelazy: definelazyx;
      restoreloop: restoreloopx;
      startreflex: dontchangevalue := dontchangevalue + 1;
      endreflex: dontchangevalue := dontchangevalue - 1;
      cvtrd: cvtrdx;
      cvtdr: cvtdrx; { SNGL function }
      dummyarg: dummyargx;
      dummyarg2: dummyarg2x;
      openarray: openarrayx;
      saveactkeys: saveactivekeys;
      otherwise
        begin
        write('Not yet implemented: ', ord(pseudoinst.op): 1);
        compilerabort(inconsistent);
        end;
      end;
    if key > lastkey then lastkey := key;
    with keytable[key] do
      if refcount + copycount > 1 then savekey(key);

    adjusttemps;

    while (keytable[lastkey].refcount = 0) and
          (lastkey >= context[contextsp].keymark) do
      begin
      keytable[lastkey].access := noaccess;
      lastkey := lastkey - 1;
      end;

    { This prevents stumbling on an old key later.
    }
{    key := lastkey;

    while key >= context[contextsp].keymark do
      begin
      if keytable[key].refcount = 0 then keytable[key].access := noaccess;
      key := key - 1;
      end;}
  end; {genone}


procedure genblk;

{ Generate code for a block.

  This is the main code generator driver.  It reads pseudo-instructions from
  the pseudofile, sets up default global variables for key, len, etc,
  and sets tempkey on the assumption that we have a literal operand in
  oprnds[2].  This last is a hack to reduce space in the pass.

  It then calls a specific routine to handle each pseudo-op.  Finally
  it saves the current key if it has a refcount > 1 and adjusts the
  keytable to a possibly higher key.  It also returns any temps which
  are now unused.
}


  begin {genblk}
    while pseudobuff.op <> blockexit do
      begin
      setcommonkey;
      genone;
      end;
  end {genblk} ;

procedure codeselect;

{ Generate code for one of the pseudoops handled by this part of the
  code generator.
}

  begin {codeselect}
    tempkey := loopcount - 1;
    setcommonkey;
    case pseudoinst.op of
      blockentry: blockentryx;
      blockcode: blockcodex;
      blockexit: blockexitx;
      doint, doptr: dointx;
      doreal: dorealx;
      dolevel:
        if (left > 1) and (left < level) then genone
        else dostaticlevels(false);
      doown: dostaticlevels(true);
      dofptr: dofptrx;
     otherwise
        genone;
      end;
    if (key > lastkey) and
      (pseudoinst.op in [doint, doptr, dofptr, doreal, dolevel, doown])
    then lastkey := key;
  end; {codeselect}


procedure dumppseudo;


  begin {dumppseudo}
    with pseudoinst do
      begin
        if (op = blockentry) then
          writeln;
        case op of
          blockentry: write('blockentry': 15);
          addint: write('addint': 15);
          addptr: write('addptr': 15);
          addr: write('addr': 15);
          addreal: write('addreal': 15);
          addset: write('addset': 15);
          addstr: write('addstr': 15);
          aindx: write('aindx': 15);
          andint: write('andint': 15);
          arraystr: write('arraystr': 15);
          bad: write('bad': 15);
          blockcode: write('blockcode': 15);
          blockexit: write('blockexit': 15);
          callroutine: write('callroutine': 15);
          casebranch: write('casebranch': 15);
          caseelt: write('caseelt': 15);
          caseerr: write('caseerr': 15);
          castfptrint: write('castfptrint': 15);
          castint: write('castint': 15);
          castintfptr: write('castintfptr': 15);
          castintptr: write('castintptr': 15);
          castptr: write('castptr': 15);
          castptrint: write('castptrint': 15);
          castreal: write('castreal': 15);
          castrealint: write('castrealint': 15);
          chrstr: write('chrstr': 15);
          clearlabel: write('clearlabel': 15);
          closerange: write('closerange': 15);
          commafake: write('commafake': 15);
          compbool: write('compbool': 15);
          compint: write('compint': 15);
          congruchk: write('congruchk': 15);
          copyaccess: write('copyaccess': 15);
          copystack: write('copystack': 15);
          createfalse: write('createfalse': 15);
          createtemp: write('createtemp': 15);
          createtrue: write('createtrue': 15);
          cvtdr: write('cvtdr': 15);
          cvtrd: write('cvtrd': 15);
          dataadd: write('dataadd': 15);
          dataaddr: write('dataaddr': 15);
          dataend: write('dataend': 15);
          datafaddr: write('datafaddr': 15);
          datafield: write('datafield': 15);
          datafill: write('datafill': 15);
          dataint: write('dataint': 15);
          datareal: write('datareal': 15);
          datastart: write('datastart': 15);
          datastore: write('datastore': 15);
          datastruct: write('datastruct': 15);
          datasub: write('datasub': 15);
          decint: write('decint': 15);
          defforindex: write('defforindex': 15);
          defforlitindex: write('defforlitindex': 15);
          definelazy: write('definelazy': 15);
          defunsforindex: write('defunsforindex': 15);
          defunsforlitindex: write('defunsforlitindex': 15);
          divint: write('divint': 15);
          divreal: write('divreal': 15);
          divset: write('divset': 15);
          doext: write('doext': 15);
          dofptr: write('dofptr': 15);
          dofptrvar: write('dofptrvar': 15);
          doint: write('doint': 15);
          dolevel: write('dolevel': 15);
          doorigin: write('doorigin': 15);
          doown: write('doown': 15);
          doptr: write('doptr': 15);
          doptrvar: write('doptrvar': 15);
          doreal: write('doreal': 15);
          doretptr: write('doretptr': 15);
          doset: write('doset': 15);
          dostruct: write('dostruct': 15);
          dotemp: write('dotemp': 15);
          dounsvar: write('dounsvar': 15);
          dovar: write('dovar': 15);
          dummyarg: write('dummyarg': 15);
          dummyarg2: write('dummyarg2': 15);
          endpseudocode: write('endpseudocode': 15);
          endreflex: write('endreflex': 15);
          eqfptr: write('eqfptr': 15);
          eqint: write('eqint': 15);
          eqlitfptr: write('eqlitfptr': 15);
          eqlitint: write('eqlitint': 15);
          eqlitptr: write('eqlitptr': 15);
          eqlitreal: write('eqlitreal': 15);
          eqptr: write('eqptr': 15);
          eqreal: write('eqreal': 15);
          eqset: write('eqset': 15);
          eqstr: write('eqstr': 15);
          eqstruct: write('eqstruct': 15);
          flt: write('flt': 15);
          fmt: write('fmt': 15);
          fordnbottom: write('fordnbottom': 15);
          fordnchk: write('fordnchk': 15);
          fordnimproved: write('fordnimproved': 15);
          fordntop: write('fordntop': 15);
          forerrchk: write('forerrchk': 15);
          forupbottom: write('forupbottom': 15);
          forupchk: write('forupchk': 15);
          forupimproved: write('forupimproved': 15);
          foruptop: write('foruptop': 15);
          geqint: write('geqint': 15);
          geqlitint: write('geqlitint': 15);
          geqlitptr: write('geqlitptr': 15);
          geqlitreal: write('geqlitreal': 15);
          geqptr: write('geqptr': 15);
          geqreal: write('geqreal': 15);
          geqset: write('geqset': 15);
          geqstr: write('geqstr': 15);
          geqstruct: write('geqstruct': 15);
          getquo: write('getquo': 15);
          getrem: write('getrem': 15);
          gtrint: write('gtrint': 15);
          gtrlitint: write('gtrlitint': 15);
          gtrlitptr: write('gtrlitptr': 15);
          gtrlitreal: write('gtrlitreal': 15);
          gtrptr: write('gtrptr': 15);
          gtrreal: write('gtrreal': 15);
          gtrstr: write('gtrstr': 15);
          gtrstruct: write('gtrstruct': 15);
          incint: write('incint': 15);
          incstk: write('incstk': 15);
          indx: write('indx': 15);
          indxchk: write('indxchk': 15);
          indxindr: write('indxindr': 15);
          inset: write('inset': 15);
          joinlabel: write('joinlabel': 15);
          jointemp: write('jointemp': 15);
          jump: write('jump': 15);
          jumpf: write('jumpf': 15);
          jumpt: write('jumpt': 15);
          jumpvfunc: write('jumpvfunc': 15);
          kwoint: write('kwoint': 15);
          leqint: write('leqint': 15);
          leqlitint: write('leqlitint': 15);
          leqlitptr: write('leqlitptr': 15);
          leqlitreal: write('leqlitreal': 15);
          leqptr: write('leqptr': 15);
          leqreal: write('leqreal': 15);
          leqset: write('leqset': 15);
          leqstr: write('leqstr': 15);
          leqstruct: write('leqstruct': 15);
          loopholefn: write('loopholefn': 15);
          lssint: write('lssint': 15);
          lsslitint: write('lsslitint': 15);
          lsslitptr: write('lsslitptr': 15);
          lsslitreal: write('lsslitreal': 15);
          lssptr: write('lssptr': 15);
          lssreal: write('lssreal': 15);
          lssstr: write('lssstr': 15);
          lssstruct: write('lssstruct': 15);
          makeroom: write('makeroom': 15);
          modint: write('modint': 15);
          movcstruct: write('movcstruct': 15);
          movint: write('movint': 15);
          movlitint: write('movlitint': 15);
          movlitptr: write('movlitptr': 15);
          movlitreal: write('movlitreal': 15);
          movptr: write('movptr': 15);
          movreal: write('movreal': 15);
          movset: write('movset': 15);
          movstr: write('movstr': 15);
          movstruct: write('movstruct': 15);
          mulint: write('mulint': 15);
          mulreal: write('mulreal': 15);
          mulset: write('mulset': 15);
          negint: write('negint': 15);
          negreal: write('negreal': 15);
          neqfptr: write('neqfptr': 15);
          neqint: write('neqint': 15);
          neqlitfptr: write('neqlitfptr': 15);
          neqlitint: write('neqlitint': 15);
          neqlitptr: write('neqlitptr': 15);
          neqlitreal: write('neqlitreal': 15);
          neqptr: write('neqptr': 15);
          neqreal: write('neqreal': 15);
          neqset: write('neqset': 15);
          neqstr: write('neqstr': 15);
          neqstruct: write('neqstruct': 15);
          openarray: write('openarray': 15);
          orint: write('orint': 15);
          paindx: write('paindx': 15);
          pascalgoto: write('pascalgoto': 15);
          pascallabel: write('pascallabel': 15);
          pindx: write('pindx': 15);
          postint: write('postint': 15);
          postptr: write('postptr': 15);
          postreal: write('postreal': 15);
          preincptr: write('preincptr': 15);
          pseudolabel: write('pseudolabel': 15);
          pshaddr: write('pshaddr': 15);
          pshfptr: write('pshfptr': 15);
          pshint: write('pshint': 15);
          pshlitfptr: write('pshlitfptr': 15);
          pshlitint: write('pshlitint': 15);
          pshlitptr: write('pshlitptr': 15);
          pshlitreal: write('pshlitreal': 15);
          pshproc: write('pshproc': 15);
          pshptr: write('pshptr': 15);
          pshreal: write('pshreal': 15);
          pshretptr: write('pshretptr': 15);
          pshset: write('pshset': 15);
          pshstr: write('pshstr': 15);
          pshstraddr: write('pshstraddr': 15);
          pshstruct: write('pshstruct': 15);
          ptrchk: write('ptrchk': 15);
          ptrtemp: write('ptrtemp': 15);
          rangechk: write('rangechk': 15);
          rdbin: write('rdbin': 15);
          rdchar: write('rdchar': 15);
          rdint: write('rdint': 15);
          rdreal: write('rdreal': 15);
          rdst: write('rdst': 15);
          rdxstr: write('rdxstr': 15);
          realtemp: write('realtemp': 15);
          regtemp: write('regtemp': 15);
          restorelabel: write('restorelabel': 15);
          restoreloop: write('restoreloop': 15);
          returnfptr: write('returnfptr': 15);
          returnint: write('returnint': 15);
          returnptr: write('returnptr': 15);
          returnreal: write('returnreal': 15);
          returnstruct: write('returnstruct': 15);
          savelabel: write('savelabel': 15);
          saveactkeys: write('saveactkeys': 15);
          setbinfile: write('setbinfile': 15);
          setfile: write('setfile': 15);
          setinsert: write('setinsert': 15);
          shiftlint: write('shiftlint': 15);
          shiftrint: write('shiftrint': 15);
          stacktarget: write('stacktarget': 15);
          startreflex: write('startreflex': 15);
          stddivint: write('stddivint': 15);
          stdmodint: write('stdmodint': 15);
          stmtbrk: write('stmtbrk': 15);
          subint: write('subint': 15);
          subptr: write('subptr': 15);
          subreal: write('subreal': 15);
          subset: write('subset': 15);
          sysfnint: write('sysfnint': 15);
          sysfnreal: write('sysfnreal': 15);
          sysfnstring: write('sysfnstring': 15);
          sysroutine: write('sysroutine': 15);
          temptarget: write('temptarget': 15);
          unscallroutine: write('unscallroutine': 15);
          wrbin: write('wrbin': 15);
          wrbool: write('wrbool': 15);
          wrchar: write('wrchar': 15);
          wrint: write('wrint': 15);
          wrreal: write('wrreal': 15);
          wrst: write('wrst': 15);
          wrxstr: write('wrxstr': 15);
          xorint: write('xorint': 15);
          otherwise write('unknown op:', ord(op): 15);
        end;
        if not (op in nokeydata) then
          write(':', len: 6, ' (', key: 3, ',', refcount: 3, ',', copycount: 3, ') ')
        else write(':',' ':21);
        { Only one operand }
        if op in oneoperand then writeln(oprnds[1]:5)
        else writeln(oprnds[1]:5, oprnds[2]:5, oprnds[3]:5);
    end
  end {dumppseudo} ;

procedure codeone;

{ Routine called by directly by travrs to generate code for one
  pseudoop for big compiler version.
}

  begin {codeone}
    if travcode then
      begin
      key := pseudoinst.key;
      len := pseudoinst.len;
      left := pseudoinst.oprnds[1];
      right := pseudoinst.oprnds[2];
      target := pseudoinst.oprnds[3];
      codeselect;
      end;
  end {codeone};


procedure code;

{ Driver routine for code generator.  Most of the code generation is
  actually done in the overlay "genblk".

  This routine is also responsible for initialization and termination
  actions.
}
  var
    i: integer;

  begin {code}
    if not travcode then
      begin
      initcode;
      while pseudobuff.op <> endpseudocode do
	begin
	repeat
	  codeselect;
	until (pseudoinst.op = blockcode) or (pseudobuff.op = endpseudocode);

	if pseudoinst.op = blockcode then
	  begin
	  genblk;
	  tempkey := loopcount - 1;
	  blockexitx;
	  end;
	end;

      exitcode;
      end;
  end {code} ;

end.
