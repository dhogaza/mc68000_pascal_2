{$nomain}
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

    { This code checks certain configuration parameters and reports any
      potential problems. }

    { Complain if caching and the nodes per block value is wrong or if not
      caching and the entire nodetable can not fit in the array maxblocksin.
    }
    if needcaching then
      begin
      if codemaxnodeinblock + 1 > (diskbufsize + 1) div rdup(size(node)) then
        writeln('CODE (caching) CODEMAXNODEINBLOCK should be ',
          (diskbufsize + 1) div rdup(size(node)) - 1:1);
      end
    else if not bigcompilerversion then
      if cmaxblocksin * (codemaxnodeinblock + 1) < cnodetablesize then
        writeln('CODE (non-caching) CMAXBLOCKSIN should be ',
          (cnodetablesize + codemaxnodeinblock) div (codemaxnodeinblock + 1)
          : 1, ' or CODEMAXNODEINBLOCK should be ', (cnodetablesize +
          cmaxblocksin - 1) div cmaxblocksin : 1);

    { End of special configuration checks}

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
    if targetopsys = vdos then $pic := switcheverplus[pic] {shorthand form}
    else $pic := false;

    coprocessor_id := 1; {temporary}

    case language of
      modula2, C: newobjectfmt := true;
      Pascal: newobjectfmt := false;
      end;

    dummyarg_ptr := 0;
    curstringblock := 0;
    nextstringfile := 0;
    nextpseudofile := 0;
    getpseudobuff;
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

    if needcaching then
      begin {initialize the node buffers in global buffer array}
      for i := 1 to maxblockslow do
        with blocksin[i] do
          begin {set up for a direct mapping of nodeindex to block}
          blkno := i - 1;
          written := true;
          lowblock := true;
          buffer := ref(blockslow[i]);
          end;
      i := maxblockslow + 1;
      while (i <= cmaxblocksin) and (space > crequiredspace) do
        with blocksin[i] do
          begin
          blkno := i - 1;
          written := true;
          lowblock := false;
          new(buffer);
          i := i + 1;
          end;
      end
    else
      begin {Initialize the node buffers in the heap}
      i := 1;
      while (i <= cmaxblocksin) do
        with blocksin[i] do
          begin
          blkno := i - 1;
          written := true;
          lowblock := false;
          buffer := nil;
          i := i + 1;
          end;
      end;
 
    lastblocksin := i - 1;
    thrashing := false;
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

    if switcheverplus[outputobj] then initobj;

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
      if bigcompilerversion then n1 := ref(bignodetable[m])
      else creadaccess(m, n1);
      if bigcompilerversion then n2 := ref(bignodetable[n])
      else cwriteaccess(n, n2);
      n2^ := n1^;
    end {movenode} ;


  begin
    if m < lastnode then
      begin

      {first scan over any nop's that are there}

      i := m;

      if bigcompilerversion then p := ref(bignodetable[i + 1])
      else creadaccess(i + 1, p);

      if p^.kind <> instnode then
        begin
        write('attempt to insert before operand:', i: 1);
        abort(inconsistent);
        end;

      repeat
        i := i + 1;
        if bigcompilerversion then p := ref(bignodetable[i])
        else creadaccess(i, p);
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
          if bigcompilerversion then p := ref(bignodetable[cp^.n + 1])
          else creadaccess(cp^.n + 1, p);
          bl := p^.brnodelink;
          while bl <> 0 do
            begin
            if bl >= i then
              begin
              p^.brnodelink := p^.brnodelink + n;
              blocksin[1].written := true;
              end;
            if bigcompilerversion then p := ref(bignodetable[bl + 1])
            else creadaccess(bl + 1, p);
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
      p := ref(bignodetable[n]);
      p1 := ref(bignodetable[n1]);
      end
    else
      begin
      creadaccess(n, p);
      creadaccess(n1, p1);
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
        p := ref(bignodetable[n]);
        p1 := ref(bignodetable[n1]);
        end
      else
        begin
        creadaccess(n, p);
        creadaccess(n1, p1);
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
        if bigcompilerversion then ip := ref(bignodetable[i])
        else creadaccess(i, ip);
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
            abort(inconsistent)
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
            abort(inconsistent)
            end;

          if opct = 0 then
            sp := ip
          else
            begin
            if bigcompilerversion then sp := ref(bignodetable[i + 1])
            else cwriteaccess(i + 1, sp); { get source operand node }

            if not (sp^.kind in [oprndnode, relnode, labelnode]) then
              begin
              writeln('Fixsimplestuff: missing src in node ', i + 1: 1);
              abort(inconsistent)
              end;
            end;

          if opct < 2 then
            dp := sp
          else
            begin
            if bigcompilerversion then dp := ref(bignodetable[i + 2])
            else cwriteaccess(i + 2, dp); { get destination operand node }

            if not (dp^.kind in [oprndnode, relnode, labelnode]) then
              begin
              writeln('Fixsimplestuff: missing dst in node ', i + 2: 1);
              abort(inconsistent)
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
	  if bigcompilerversion then n := ref(bignodetable[i])
	  else creadaccess(i, n);
	until (n^.kind = instnode) or (i = lastnode);

	if n^.kind = instnode then
	  if (n^.inst = cmp) or (n^.inst = cmpi) then
	    begin
	    j := i;
	    repeat
	      repeat
		j := j + 1;
		if bigcompilerversion then m := ref(bignodetable[j])
		else creadaccess(j, m);
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
      pic_own_used := $pic and (ownsize > 0) and proctable[blockref].ownused;

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
      if (p^.oprnd.m = commonlong) and $pic then goodaddress := false
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
        matched := ((trial.oprnd.offset and 377B) = (known.oprnd.offset and
                   377B))
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
        if bigcompilerversion then p := ref(bignodetable[t])
        else creadaccess(t, p);

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
          abort(inconsistent);
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
        if bigcompilerversion then p := ref(bignodetable[t])
        else creadaccess(t, p);

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

                if bigcompilerversion then pi := ref(bignodetable[ti])
                else cwriteaccess(ti, pi);

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

                if bigcompilerversion then pi := ref(bignodetable[ti])
                else cwriteaccess(ti, pi);

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
        if bigcompilerversion then n := ref(bignodetable[t])
        else creadaccess(t, n);
      until (t = lastnode) or (n^.kind = labelnode);

      if n^.kind = labelnode then
        begin
        stackadj := n^.stackdepth -
                    labeltable[findlabel(n^.labelno)].stackdepth;
        if findlabel(n^.labelno) > 0 then
          if stackadj <> 0 then
            begin
            t := t - 1;

            if bigcompilerversion then n := ref(bignodetable[t])
            else cwriteaccess(t, n);

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
        if bigcompilerversion then n := ref(bignodetable[t])
        else creadaccess(t, n);

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
        if bigcompilerversion then n := ref(bignodetable[t])
        else creadaccess(t, n);
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
              if bigcompilerversion then n := ref(bignodetable[t])
              else cwriteaccess(t, n);

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

                  if not (mc68020 and $pic) then { 68020 has long branches}
                    begin
                    ti := t;

                    repeat
                      ti := ti - 1;
                      if bigcompilerversion then ni := ref(bignodetable[ti])
                      else creadaccess(ti, ni);
                    until ni^.kind = instnode;

                    if bigcompilerversion then ni := ref(bignodetable[ti])
                    else cwriteaccess(ti, ni);

                    if $pic then
                      begin  { MC68000 24-bit pic does not allow branches in
                               procedure longer than 32k. }
                      write('Procedure is too large for PIC');
                      abort(inconsistent);
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
                      abort(inconsistent);
                      end;
                    end; {not (mc68020 and $pic)}
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
               (oprnd.m = usercall) and not (not mc68020 and $pic) and
               (proctable[oprnd.offset].externallinkage and
               not proctable[oprnd.offset].bodydefined or
               (procmap[oprnd.offset].addr = undefinedaddr) or
               (procmap[oprnd.offset].addr + oprnd.offset1 - (currentpc -
               word) < - 32768)) then
              begin {change to long form}
              if bigcompilerversion then n := ref(bignodetable[t])
              else cwriteaccess(t, n);

              if not mc68020 and $pic then
                begin
                { Convert the LEA that was put out in dostructx to a two
                  instruction sequence to compute the constant's address.
                }
                n^.oprnd.m := pic_pcrelative; { change mode to pic_pcrelative }
                n^.oprnd.offset1 := 6;
                if bigcompilerversion then n := ref(bignodetable[t - 1])
                else cwriteaccess(t - 1, n); { change inst to ADDA }
                n^.inst := adda;
                if bigcompilerversion then n := ref(bignodetable[t + 1])
                else creadaccess(t + 1, n); { pick up destination register }
                register := n^.oprnd.reg;
                currentpc := currentpc - long;
                insert(t - 2, 3);
                settempareg(register);
                settemp(long, pic_splat_pcrel, 0, 0, false, 10, 0, 1, unknown);
                gen2(lea, long, tempkey, tempkey + 1);
                tempkey := tempkey + 2;
                if bigcompilerversion then n := ref(bignodetable[t - 1])
                else cwriteaccess(t - 1, n); { update length }
                currentpc := currentpc + instlength(t - 1);
                bias := 6; { ADDA is 6 bytes long; new LEA same length as old }
                lastnode := lastsaved;
                t := t + 3; { Advance past new instruction }
                end
              else if mc68020 and $pic then
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
            p1 := ref(bignodetable[t1])
          else
            creadaccess(t1, p1);

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
            p := ref(bignodetable[t])
          else
            creadaccess(t, p);
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
                p := ref(bignodetable[t + 1])
              else
                creadaccess(t + 1, p);
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
          p := ref(bignodetable[firstbr^.n]);
          p1 := ref(bignodetable[firstbr^.n + 1]);
          end
        else
          begin
          creadaccess(firstbr^.n, p);
          creadaccess(firstbr^.n + 1, p1);
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
              p := ref(bignodetable[lastnode])
            else
              creadaccess(lastnode, p);
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
            p := ref(bignodetable[firstbr^.n + 1])
          else
            cwriteaccess(firstbr^.n + 1, p);

          p^.labelno := labeltable[l].labno;
          p^.stackdepth := labeltable[l].stackdepth;
          lastnode := t;
          t := longestkill;
          repeat
            t := t + 1;
            if bigcompilerversion then
              p := ref(bignodetable[t])
            else
              creadaccess(t, p);
          until (p^.kind = instnode) or (p^.kind = stmtref) or
                (p^.kind = errornode);
          delete(t, longestkillcount);
          end;
        if bigcompilerversion then
          p := ref(bignodetable[firstbr^.n + 1])
        else
          creadaccess(firstbr^.n + 1, p);
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
    if not mc68020 and $pic and (proctable[blockref].opensfile or
      switcheverplus[debugging] or switcheverplus[profiling])
    then reg := getareg;

    {do the register save and restore}

    savemask := 0;
    restoremask := 0;
    savebit := 100000B;
    restorebit := 1;
    regcost := 0;
    fpsavemask := 0;
    fprestoremask := 0;
    fpsavebit := 1;  { fmovem mask is opposite of movem!!! }
    fprestorebit := 200B;
    fpregcost := 0;
    if $pic and (ownsize > 0) and proctable[blockref].ownused then
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
        saveregptr := ref(bignodetable[savereginst + 1])
      else cwriteaccess(savereginst + 1, saveregptr);

      saveregptr^.oprnd.offset := savemask;
      end;

    if mc68881 then
      if fprestoremask = 0 then
        delete(fpsavereginst, 1)
      else
        begin
        if bigcompilerversion then
          saveregptr := ref(bignodetable[fpsavereginst + 1])
        else cwriteaccess(fpsavereginst + 1, saveregptr);

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
        saveregptr := ref(bignodetable[savedebinst + 1])
      else cwriteaccess(savedebinst + 1, saveregptr);

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
      abort(undeltemps);
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
      seek(debugfile, procsym);
      debugfile^.firststmt := lastmaprecord + 1;
      debugfile^.entryaddress := procmap[blockref].addr;

      { initialize the register save masks to "don't save"}

      debugfile^.ptrregssaved :=
	ptrregmask (false, false, false, false, false, false, false, false);
      debugfile^.genregssaved := 
	genregmask (false, false, false, false, false, false, false, false);
      debugfile^.realregssaved := 
	realregmask (false, false, false, false, false, false, false, false);

      if level > 1 then begin
	for r := 0 to sl do debugfile^.ptrregssaved[r] := aregused[r];
	for r := 0 to 7 do debugfile^.genregssaved[r] := dregused[r];
	if mc68881 then
	  for r := 0 to 7 do debugfile^.realregssaved[r] := fpregused[r];
	end;
      put(debugfile);
      end;

    { write output code }
    if switcheverplus[outputmacro] or
       switcheverplus[outputobj] then putcode;

    if currentpc <> last_pc then
      begin
      writeln('Phase error, pass 1 = ', last_pc:-4, ', pass 2 = ',
              currentpc:-4);
      abort(inconsistent);
      end;
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
        abort(inconsistent);
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
        if $pic then
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

        if $pic and (ownsize > 0) and proctable[blockref].ownused then
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
          if $pic and not mc68020 then distance := 3
          else distance := 1;
          end;
        settempareg(sp);
        genoprnd(keytable[tempkey].oprnd);
        if switcheverplus[debugging] then initcall := libdebugger_init
        else if switcheverplus[profiling] then initcall := libprofilerinit
        else initcall := libinitialize;

        if $pic and not mc68020 then
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

        if $pic and (ownsize > 0) and proctable[blockref].ownused then
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
    if $pic and (ownsize > 0) and proctable[blockref].ownused then
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
      if ($pic and (ownsize > 0) and proctable[blockref].ownused) or
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
        abort(inconsistent); { Display procedure name }

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
    if everdiagnosing then fixdiags;
    if switcheverplus[outputmacro] then fixmac;
    if switcheverplus[outputobj] then 
      begin
      fixobj;
      if totalputerr > 0 then
        begin
        write('Consistency checks detected -- see assembler listing');
        abort(inconsistent);
        end;
      end;

{    if peeping and switcheverplus[test] then begin {peephole statistics}
{      writeln('Peephole opts gained ', peep[0]:1, ' bytes');
      for i := 1 to maxpeephole do writeln(i:3, peep[i]:10, ' times');
      end; {peephole statistics}

    flushbuffers;
  end; {exitcode}


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
	  unpackpseudofile;
	  codeselect;
	until (pseudoinst.op = blockcode) or (pseudobuff.op = endpseudocode);

	if pseudoinst.op = blockcode then
	  begin
	  genblk;
	  unpackpseudofile;
	  tempkey := loopcount - 1;
	  blockexitx;
	  end;
	end;

      exitcode;
      end;
  end {code} ;
