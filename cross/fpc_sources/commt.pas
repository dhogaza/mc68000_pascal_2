{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987, 1988, 1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 

  %W% %G% %U%

  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Tree Processing Common Routines

 Last modified by KRIS on 21-Nov-1990 15:22:51
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}
{$nomain,nopointercheck}

{ Tree build/improve/walk }
{ Virtual Memory System for Expression Nodes.

  These routines implement the virtual memory tree used for expressions.
  Nodes are stored on the file "nodefile" in blocks.  A maximum of
  "tmaxblocksin" of these blocks are kept in buffers in main memory.
  The array "blocksin" is used to keep track of the blocks in memory,
  and to implement a least recently used update strategy.

  When a node is requested, the list of blocks in memory is searched,
  and if the node is in one of them that block is moved to the head
  of the list (blocksin[1]).  All blocks between the former position
  and the head of the list are moved one position toward the foot of
  the list.  If the block is not already in memory, the last block on the
  list is made available by writing its contents to the file (if they have
  been modified), and it is moved to the start of the list and loaded
  from the file.  Finally a pointer to the desired node is generated
  using the "ref" function.  Of course, if the block is not in memory and
  there are less than "tmaxblocksin" blocks already in memory, a new
  buffer is created without writing an old one.
}


procedure accessblk(blockwanted: blockindex {index of block to access} );

{ Make the block with index "blockwanted" available at the top of
  "blocksin".  If not already in core, the contents of the file
  block with that index are read in.  If the least recently used block
  must be written ("written" set and no more blocks allowed in core),
  it is written to the appropriate place in the file.

  This procedure is used locally from the virtual memory package.

  Although the code within "accessblk" will work with tmaxblocksin
  equal to 1, the compiler code assumes that tmaxblocksin >= 2, and
  will not work without this condition being met.
}

  var

    i: 1..tmaxblocksin; {induction var for search and copy}
    temp: blockmap; {temp storage for moving block to head of list}


  begin {accessblk}
    if not bigcompilerversion and needcaching then
      begin
      if not thrashing and switcheverplus[test] then
        writeln('travrs thrashing state reached');
      thrashing := true;

      {find this block or last block in memory}

      i := 1;
      repeat
        i := i + 1
      until (blocksin[i].blkno = blockwanted) or (i = lastblocksin);

      {now make room for blockwanted at head of the list}

      temp := blocksin[i];
      for i := i downto 2 do blocksin[i] := blocksin[i - 1];

      {write out old block if necessary, read blockwanted or create new buffer}

      with temp do
        begin
        if blockwanted <> blkno then
          begin
          if written then
            begin
            seek(cache, blkno + 1);
            cache^ := buffer^.physical;
            put(cache);
            end;
          written := false;
          seek(cache, blockwanted + 1);
          buffer^.physical := cache^;
          end;
        blkno := blockwanted;
        end;

      {update list}

      blocksin[1] := temp;
      end;
  end {accessblk} ;


procedure treadaccess
                     {i: nodeindex; (node wanted)
                      var p: nodeptr (provides access to desired node) } ;

{ Make virtual element "i" available for read-only access
  and return a pointer to it.

  The element must not be changed, as it will not be written
  back to the file after use.  Note that the element is only
  to be accessable for two consecutive calls to "readaccess"
  or "writeaccess".

  Note:  There are places in the code which assume the side-effect
  of placing the block containing the element in blocksin[1].  Thus,
  this general approach must be maintained.
}

  var
    blockno: unsignedword; {1..tnodetablesize + 1}


  begin
    if bigcompilerversion then writeln('treadaccess called!')
    else
      begin
      blockno := i div (nodesperblock + 1) + 1;
      if needcaching then
        begin
        if thrashing or (blockno >= lastblocksin) then
          begin
          if blocksin[1].blkno <> i div (nodesperblock + 1) then
            accessblk(i div (nodesperblock + 1));
          p := ref(blocksin[1].buffer^.logical[i mod (nodesperblock + 1)]);
          end
        else
          with blocksin[i div (nodesperblock + 1) + 1] do
            begin
            case hostmachine of
              pdp11: ;
              otherwise
                if buffer = nil then new(buffer);
              end;
            p := ref(buffer^.logical[i mod (nodesperblock + 1)]);
            end;
        end
      else {nocaching}
        begin
        if blockno > tmaxblocksin then abort(manynodes);
        { temp consistency check }
        if i > lastnode then abort(manynodes);
        with blocksin[blockno] do
          begin
          if buffer = nil then new(buffer);
          p := ref(buffer^.logical[i mod (nodesperblock + 1)]);
          end;
        end;
      end;
  end {treadaccess} ;


procedure twriteaccess
                      {i: nodeindex; (node to access)
                       var p: nodeptr (provides access to the node) } ;

{ Make virtual element "i" available for access and modification, and
  return a pointer to it.

  The block in which the element resides is marked as modified, so that
  any changes will be written to the file if the block is no longer
  in memory.  Note that access is only guaranteed for two consecutive
  calls to "readaccess" or "writeaccess".
}

  var
    blockno: unsignedword; {1..tnodetablesize + 1}


  begin {twriteaccess}
    if bigcompilerversion then writeln('twriteaccess called!')
    else
      begin
      if needcaching then
        begin
        treadaccess(i, p);
        blocksin[1].written := true;
        end
      else {no caching}
        begin
        { temp consistency check }
        if i > lastnode then abort(manynodes);
        blockno := i div (nodesperblock + 1) + 1;
        if blockno > tmaxblocksin then abort(manynodes);
        with blocksin[blockno] do
          begin
          if buffer = nil then new(buffer);
          p := ref(buffer^.logical[i mod (nodesperblock + 1)]);
          written := true;
          end;
        end;
      end;
  end {twriteaccess} ;


procedure flushbuffers;

{ Return all buffers used in the virtual memory system to the
  heap.  This assumes than no further use is to be made of the
  entire virtual space, as modified buffers are not written.
}

  var
    i: 1..tmaxblocksin;


  begin {flushbuffers}
    if not bigcompilerversion then
      begin
      for i := 1 to lastblocksin do
        with blocksin[i] do
          if not lowblock and (buffer <> nil) then dispose(buffer);
      end;
  end {flushbuffers} ;


procedure tdecreasebuffers;

{ If available memory is running low relenquish a cache buffer.
}

  var
    i: 0..tmaxblocksin; {induction variable}


  begin {tdecreasebuffers}
    if needcaching then
      begin
      if not newok(size(links)) and (space < trequiredspace) or
         (space < tpanicspace) then
        begin
        if lastblocksin = maxblockslow then
          begin
          if space < tpanicspace then abort(outofmem);
          end
        else
          begin
          i := lastblocksin;
          while blocksin[i].lowblock do i := i - 1;
          with blocksin[i] do
            begin
            if written then
              begin
              seek(cache, blkno + 1);
              write(cache, buffer^.physical);
              end;
            dispose(buffer);
            lastblocksin := lastblocksin - 1;
            for i := i to lastblocksin do blocksin[i] := blocksin[i + 1];
            if lastblocksin < fewestblocks then fewestblocks := lastblocksin;
            end;
          end;
        end;
      end;
    if space < tpanicspace then abort(outofmem);
  end {tdecreasebuffers} ;


procedure tincreasebuffers;

{ Add a new virtual buffer if space has gotten large due to the
  dispose routine.
}


  begin {tincreasebuffers}
    if needcaching then
      begin
      while (lastblocksin < tmaxblocksin) and ((space > texcessivespace) or
            newok(size(diskblock))) do
        begin
        if not thrashing and switcheverplus[test] then
          writeln('travrs thrashing state reached');
        thrashing := true;
        lastblocksin := lastblocksin + 1;
        with blocksin[lastblocksin] do
          begin
          new(buffer);
          lowblock := false;
          written := false;
          blkno := - 1;
          end;
        end;
      end;
  end {tincreasebuffers} ;


function newlabel;

{ Create a new pseudo-code label.  Labels created in travrs are assigned
  from the low numbers, while those created later in codegen are
  assigned from the high end (maxint) down.  This helps avoid conflict.

  The label is not defined by this routine, it simply generates unique
  labels for later definition.
}


  begin {newlabel}
    lasttravrslabel := lasttravrslabel + 1;
    newlabel := lasttravrslabel
  end {newlabel} ;


procedure checkconst
                    {node: nodeindex; (node to check)
                     var constflag: boolean; (true if node is const)
                     var i: integer (constant value if const) } ;

{ Check node "node" to see if it is constant, and set "constflag" and
  "i" if it is.  This is used in dead code elimination.
}

  var
    ptr: nodeptr; {used to access node}


  begin {checkconst}
    if bigcompilerversion then ptr := ref(bignodetable[node])
    else treadaccess(node, ptr);
    with ptr^ do
      if (action in [visit, revisit]) and (op = intop) then
        begin
        constflag := true;
        i := oprnds[1];
        end
      else constflag := false;
  end {checkconst} ;


procedure estimateloop
                      {stmt: nodeindex;
                       var fixed: boolean;
                       var overflow: boolean;
                       var runcount: unsignedint} ;

{    Purpose:
      Figure out if possible how many iterations a for loop will execute.
      Note : assumes that unsigned arithmetic works properly.

    Inputs:
      stmt : index to forhdr stmt.

    Outputs:
      fixed : true if constant loop.
      overflow : true if loop runs for 0..maxusint iterations.
      runcount : if fixed then number of iterations.

    Algorithm:
      Simply examine the for loop limits

    Sideeffects:
      none.

    Last Modified: 7/16/85

}

  var
    ptr: nodeptr; {used to access index node}
    initialvalue, finalvalue: integer; {initial and final values}
    uinitialvalue, ufinalvalue: unsignedint; {initial and final values unsigned}
    currentstmt: node; { copy of the stmt node }
    constfinal: boolean; { true if to/downto is constant }


  begin {estimateloop}
    fixed := false;
    overflow := false;
    runcount := 0; {temp }
    if bigcompilerversion then ptr := ref(bignodetable[stmt])
    else treadaccess(stmt, ptr);
    currentstmt := ptr^;
    with currentstmt do
      begin

      if bigcompilerversion then ptr := ref(bignodetable[expr1])
      else treadaccess(expr1, ptr);
      if ptr^.op in [forupchkop, fordnchkop, forerrchkop] then
        checkconst(ptr^.oprnds[1], constfinal, finalvalue)
      else checkconst(expr1, constfinal, finalvalue);

      if bigcompilerversion then ptr := ref(bignodetable[expr2])
      else treadaccess(expr2, ptr);
      if constfinal and ((ptr^.op = defforlitindexop) or
         (ptr^.op = defunsforlitindexop)) then
        begin
        fixed := true;
        initialvalue := ptr^.oprnds[3];
        uinitialvalue := initialvalue;
        ufinalvalue := finalvalue;

        if stmtkind = foruphdr then
          begin
          if ptr^.op = defunsforlitindexop then
            begin
            if (uinitialvalue = 0) and (ufinalvalue = maxusint) then
              begin
              overflow := true;
              runcount := maxusint; { at least! }
              end
            else
              begin
              if ufinalvalue >= uinitialvalue then
                runcount := ufinalvalue - uinitialvalue + 1
              else runcount := 0;
              end;
            end
          else
            begin
            { assumes two's complement representation }
            if (initialvalue < - maxint) and (finalvalue = maxint) then
              begin
              overflow := true;
              runcount := maxusint; { at least! }
              end
            else
              begin
              if initialvalue <= finalvalue then
                begin
                if initialvalue >= 0 then
                  runcount := finalvalue - initialvalue + 1
                else
                  begin
                  if finalvalue < 0 then
                    runcount := finalvalue - initialvalue + 1
                  else
                    begin
                    runcount := - initialvalue;
                    runcount := runcount + finalvalue + 1;
                    end;
                  end;
                end
              else runcount := 0;
              end;
            end;
          end
        else
          begin
          { down loop }
          if ptr^.op = defunsforlitindexop then
            begin
            if (uinitialvalue = maxusint) and (ufinalvalue = 0) then
              begin
              overflow := true;
              runcount := maxusint; { at least! }
              end
            else
              begin
              if ufinalvalue <= uinitialvalue then
                runcount := uinitialvalue - ufinalvalue + 1
              else runcount := 0;
              end;
            end
          else
            begin
            { assumes two's complement representation }
            if (initialvalue = maxint) and (finalvalue < - maxint) then
              begin
              overflow := true;
              runcount := maxusint; { at least! }
              end
            else
              begin
              if initialvalue >= finalvalue then
                begin
                if finalvalue >= 0 then
                  runcount := initialvalue - finalvalue + 1
                else
                  begin
                  if initialvalue < 0 then
                    runcount := initialvalue - finalvalue + 1
                  else
                    begin
                    runcount := - finalvalue;
                    runcount := runcount + initialvalue + 1;
                    end;
                  end;
                end
              else runcount := 0;
              end;
            end;
          end; { down loop }
        end;
      end; {with}
  end; {estimateloop}


procedure putpseudofile;

{ Do the equivalent of a "put" on the pseudofile.  It is assumed that
  the next pseudocode element has been stored in pseudofile^[nextpseudofile].
  If the buffer is full, it is actually written, otherwise the
  global index "nextpseudofile" is incremented.
}


  begin
    if nextpseudofile = diskbufsize then
      begin
      nextpseudofile := 0;
      put(tempfileone);
      end
    else nextpseudofile := nextpseudofile + 1;
  end {putpseudofile} ;


procedure genpseudo
                     {o: pseudoop; (operator)
                      l: addressrange; (operand length)
                      n: keyindex; (key for this node)
                      r: refcountrange; (reference count)
                      c: refcountrange; (copy count)
                      i, j, k: integer (operands)} ;

{ Generate a pseudocode output to the pseudofile.  Logically,
  this file consists of fixed length records, with unused fields
  filled with zeros.  The fields have the following use.

  op              Operator
  len             Length of operand (if needed)
  key             Key for this node, labels the node.
  refcount        Current ref count (decremented as copies made)
  copycount       Total copies made of this node
  oprnds          Operands, uses depend on op.

  In actual practice, many of the fields are zero for any particular
  operator, and a considerable reduction in I/O may be achieved by
  writing only the fields which are used.

}


  procedure putint(i: integer {value to write} );

{ Put an integer to the pseudofile in a compressed format.  If the
  integer will fit into a single byte, it is so written, otherwise the
  largest value which will fit is used as an escape and the integer
  follows in full format.
}

    var
      { This fudges an integer into bytes.  The constant "32" is }
      { simply a large enough number to include all probable systems. }
      fudge:
        record
          case boolean of
            true: (int: integer);
            false: (byte: packed array [1..32] of hostfilebyte);
        end;
      j: 1..32; {induction var}


    begin
      if (i >= 0) and (i < hostfilelim) then
        tempfileone^[nextpseudofile].byte := i
      else
        begin
        tempfileone^[nextpseudofile].byte := hostfilelim;
        fudge.int := i;
        for j := 1 to hostintsize * hostfileunits do
          begin
          putpseudofile;
          tempfileone^[nextpseudofile].byte := fudge.byte[j];
          end;
        end;
      putpseudofile;
    end;


  begin {genpseudo}

    if travcode then
      begin
      pseudoinst := pseudobuff;
      with pseudobuff do
        begin
        len := l;
        op := o;
        key := n;
        refcount := r;
        copycount := c;
        oprnds[1] := i;
        oprnds[2] := j;
        oprnds[3] := k;
        if emitpseudo then codeone;
        emitpseudo := true;
        end;
      end;

    if not travcode or switcheverplus[test] then
      begin
      if putlow = maxint then
        begin
        putlow := 0;
        puthi := puthi + 1;
        end
      else putlow := putlow + 1;

      tempfileone^[nextpseudofile].op := o;
      putpseudofile;
      { No key data}
      if not (o in
         [endpseudocode, bad, blockentry, blockexit, jumpf, jumpt, jump,
         pascallabel, savelabel, clearlabel, joinlabel, restorelabel,
         sysroutine, caseelt, setfile, closerange, restoreloop,
         saveactkeys]) then
        begin
        putint(l);
        putint(n);
        putint(r);
        putint(c);
        end;

      putint(i);
      { only one operand }
      if not (o in
         [endpseudocode, bad, blockexit, dovar, dounsvar, doint, doorigin,
         pseudolabel, savelabel, clearlabel, joinlabel, restorelabel,
         copyaccess, flt, pshaddr, pshstraddr, pshint, pshptr, pshreal, pshstr,
         pshstruct, pshset, pshlitint, pshlitptr, pshlitreal, copystack, fmt,
         wrint, wrreal, wrchar, wrst, wrbool, wrbin, wrxstr, rdbin, rdint,
         rdchar, rdreal, rdst, rdxstr, stacktarget, ptrchk, chrstr, arraystr,
         definelazy, setbinfile, setfile, closerange, restoreloop]) then
        begin
        putint(j);
        putint(k);
        end;
      end;
  end; {genpseudo}


procedure genrealop
                   {o: pseudoop; (operator)
                    l: addressrange; (operand length)
                    n: keyindex; (key for this node)
                    r: refcountrange; (reference count)
                    c: refcountrange; (copy count)
                    val: realarray (real value)} ;

{ Generate a pseudo instruction with a real value as an operand
}

  var
    rsize: integer;
    i: 1..30;
    kludge: {for converting to ints}
      packed record
        case boolean of
          true: (r: realarray; );
          false: (i: array [1..30] of integer; );
      end;


  begin
    rsize := size(realarray);
    kludge.r := val;
    i := 1;
    while rsize > 0 do
      begin
      genpseudo(o, l, n, r, c, kludge.i[i], kludge.i[i + 1], kludge.i[i + 2]);
      i := i + 3;
      rsize := rsize - 3 * size(integer);
      end;
  end; {genrealop}


procedure increfcount {n: nodeindex; (node to increment) deadcode: boolean; inc:
                       shortint (amount to increment) } ;

{ Change the effective reference count for a node by "inc".  This
  may have to chain down a sequence of nodes to get the actual most
  recent node which should be changed.
}

  var
    p: nodeptr;


  begin
    if not (deadcode and (removedeadcode in genset)) and
       (n <> 0) {or (inc < 0)} then
      begin
      if bigcompilerversion then p := ref(bignodetable[n])
      else treadaccess(n, p);
      if p^.action = visit then
        if (p^.op < intop) and (p^.op > newunsvarop) then
          while p^.slink <> 0 do
            begin
            n := p^.slink;
            if bigcompilerversion then p := ref(bignodetable[n])
            else treadaccess(n, p);
            end;
      if bigcompilerversion then p := ref(bignodetable[n])
      else twriteaccess(n, p);
      p^.refcount := p^.refcount + inc;
      if p^.action = visit then
        if p^.op = commaop then increfcount(p^.oprnds[2], deadcode, inc);
      end;
  end; {increfcount}
