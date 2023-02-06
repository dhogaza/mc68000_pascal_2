{[b+,l+]}
{$nomain}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: 68000
  System: UNIX

  Pascal-2 Code generator (common routines)

 Last modified by KRIS on 21-Nov-1990 15:27:50
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}


{ Virtual Memory System for Instruction Nodes.
  These routines implement the virtual memory tree used for instructions.
  Nodes are stored on the file "nodefile" in blocks.  A maximum of
  "cmaxblocksin" of these blocks are kept in buffers in main memory.
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
  there are less than "cmaxblocksin" blocks already in memory, a new
  buffer is created without writing an old one.

  Since measurements indicated that this is a major time consumer
  in this pass, the mechanism is effectively bypassed if the blocks
  will all fit in core.  In this case,  there is a direct mapping
  from index to buffers in "blocksin".  Once any buffer has been
  written (indicated by the global "thrashing"), the system reverts
  to the search of blocksin to locate a buffer.

  At the point where thrashing becomes true,  all blocks are
  assumed to be written.
}


procedure accessblk{blockwanted: integer (index of desired block) };

{ Make the block with index "blockwanted" available at the top of
  "blocksin".  If not already in core, the contents of the file
  block with that index are read in.  If the least recently used block
  must be written ("written" set and no more blocks allowed in core),
  it is written to the appropriate place in the file.

  This procedure is used locally from the virtual memory package.

  Although the code within "accessblk" will work with cmaxblocksin
  equal to 1, the compiler code assumes that cmaxblocksin >= 2, and
  will not work without this condition being met.
}

  var
    i: 0..cmaxblocksin; {induction var}
    temp: blockmap; {used to swap buffer entries}


  begin {accessblk}
    if needcaching then
      begin
      if not thrashing and switcheverplus[test] then
        writeln('code thrashing state reached');

      thrashing := true; {we have written a block}

      {find this block or last block in memory}

      i := 1;
      repeat
        i := i + 1
      until (blocksin[i].blkno = blockwanted) or (i = lastblocksin);

      {make room for blockwanted at head of list}

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

      blocksin[1] := temp;
    end;
  end {accessblk} ;


procedure creadaccess{i: nodeindex; (node wanted)
                     var p: nodeptr (provides access to desired node) };

{ Make virtual element "i" available for read-only access
  and return a pointer to it.

  The element must not be changed, as it will not be written
  back to the file after use.  Note that the element is only
  to be accessable for (cmaxblocksin-1) calls to "creadaccess"
  or "cwriteaccess".

  Note: There are places in the code where it is assumed that
  the most recently accessed block is in blocksin[1], or that
  we are not yet thrashing.
}

  var
    blockno: integer; { 1..cnodetablesize + 1}
    byteno: integer; { 1..cnodetablesize + 1}


  begin {creadaccess}
    if bigcompilerversion then writeln('creadaccess called!')
    else
      begin
      blockno := i div (nodesperblock + 1) + 1;
      byteno := i mod (nodesperblock + 1);

      if needcaching then
        begin
        if hostmachine = pdp11 then
          begin
          if blocksin[1].blkno <> (blockno - 1) then
            accessblk(blockno - 1);
          p := ref(blocksin[1].buffer^.logical[byteno]);
          end
        else
          begin
          if thrashing or (blockno >= lastblocksin) then
            begin {we must do a search to find the block}
            if blocksin[1].blkno <> (blockno - 1) then
              accessblk(blockno - 1);
            p := ref(blocksin[1].buffer^.logical[byteno]);
            end
          else {we can compute the block number directly}
            with blocksin[blockno] do
              begin
              if buffer = nil then
                begin
                if not newok(size(nodeblock)) then abort(outofmem);
                new(buffer);
                end;
              p := ref(buffer^.logical[byteno]);
              end;
          end;
        end
      else {no cashing}
        begin
        if blockno > cmaxblocksin then abort(manynodes);
        with blocksin[blockno] do
          begin
          if buffer = nil then
            begin
            if not newok(size(nodeblock)) then abort(outofmem);
            new(buffer);
            end;
          p := ref(buffer^.logical[byteno]);
          end;
        end;
      end;
  end {creadaccess} ;


procedure cwriteaccess{i: nodeindex; (node to access)
                      var p: nodeptr (provides access to the node) };

{ Make virtual element "i" available for access and modification, and
  return a pointer to it.

  The block in which the element resides is marked as modified, so that
  any changes will be written to the file if the block is no longer
  in memory.  Note that access is only guaranteed for (cmaxblocksin-1)
  calls to "creadaccess" or "cwriteaccess".
}

  var
    blockno: integer; { 1..cnodetablesize + 1 }


  begin {cwriteaccess}
    if bigcompilerversion then writeln('cwriteaccess called!')
    else
      begin
      if needcaching then
        begin
        creadaccess(i, p);
        blocksin[1].written := true;
        end
      else
        begin
        blockno := i div (nodesperblock + 1) + 1;

        if blockno > cmaxblocksin then abort(manynodes);

        with blocksin[blockno] do
          begin
          if buffer = nil then
            begin
            if not newok(size(nodeblock)) then abort(outofmem);
            new(buffer);
            end;
          p := ref(buffer^.logical[i mod (nodesperblock + 1)]);
          written := true;
          end;
        end;
      end;
  end {cwriteaccess} ;


function getvartableptr{(i: integer (index in vartable)): vartablerecptr};

{ Returns a pointer to the ith vartable entry.
}


  begin {getvartableptr}
  getvartableptr := ref(vartable[i div (maxvarentries + 1) + 1]^
                                [i mod (maxvarentries + 1)]);
  end {getvartableptr} ;


{ Pseudo file handling procedures.

  Since many of the fields in a pseudo-instruction are unused for many
  of the instructions,  the pseudo-file is packed to eliminate the
  redundant fields.  

  This code gets and unpacks the pseudo-file into pseudocode elements.
}


procedure getpseudobuff;

{ Get and unpack the next element in the pseudofile, leaving the result
  in "pseudobuff".
}


  procedure gettempfile;

{ Do the equivalent of a get on the pseudofile, treating the file as a
  file of bytes.  The pseudo-file is actually tempfileone, and the next
  byte is accessed as tempfileone^[nextpseudofile].
}


    begin {gettempfile}
      if not travcode then
        begin
        if nextpseudofile = diskbufsize then
          begin
          nextpseudofile := 0;
          get(tempfileone);
          end
        else nextpseudofile := nextpseudofile + 1;
        end;
    end {gettempfile} ;


  function getint: integer;

{ Get an integer value from the file.  This is made into a function to
  allow returning the value into subranges of integer.  Integers are encoded
  in a single byte unless they won't fit.
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


    begin {getint}
      if not travcode then
        begin
        fudge.int := tempfileone^[nextpseudofile].byte;
        gettempfile;
        if fudge.int = hostfilelim then
          for j := 1 to hostintsize * hostfileunits do
            begin
            fudge.byte[j] := tempfileone^[nextpseudofile].byte;
            gettempfile;
            end;
        getint := fudge.int;
        end;
    end {getint} ;


  begin {getpseudobuff}
    if not travcode then
      begin
      if getlow = maxint then
	begin
	getlow := 0;
	gethi := gethi + 1;
	end
      else getlow := getlow + 1;

      pseudobuff.op := tempfileone^[nextpseudofile].op;
      gettempfile;

      with pseudobuff do
	begin
	len := 0;
	key := 0;
	refcount := 0;
	copycount := 0;
	{ No key data}
	if not (op in nokeydata) then
	  begin
	  len := getint;
	  key := getint;
	  refcount := getint;
	  copycount := getint;
	  end;

	oprnds[1] := getint;
	oprnds[2] := 0;
	oprnds[3] := 0;
	{ only one operand }
	if not (op in oneoperand) then
	  begin
	  oprnds[2] := getint;
	  oprnds[3] := getint;
	  end;
	end;
      end;
  end {getpseudobuff} ;


procedure unpackpseudofile;

{ Get the next pseudo-instruction in "pseudobuf" and distribute some
  of the fields into other global variables.
}


  begin {unpackpseudofile}
    if not travcode then
      begin
      pseudoinst := pseudobuff;
      if pseudoinst.op <> endpseudocode then getpseudobuff;

      key := pseudoinst.key;
      len := pseudoinst.len;
      left := pseudoinst.oprnds[1];
      right := pseudoinst.oprnds[2];
      target := pseudoinst.oprnds[3];
      end;
  end {unpackpseudofile} ;


procedure getreal {var rval: realarray};

{ Get a real number from the pseudo instruction.  We are making the assumption
  here (as elsewhere) that integers are at least 32 bits long so we can
  fit the entire real into one pseudoinstruction.
}

  var
    kluge:
      record case boolean of
	true: (rval: realarray);
	false: (ival: packed array [1..3] of integer;);
	end;
  begin
    kluge.ival[1] := pseudoinst.oprnds[1];
    kluge.ival[2] := pseudoinst.oprnds[2];
    kluge.ival[3] := pseudoinst.oprnds[3];
    rval := kluge.rval;
  end;


procedure realtoints
		    {rval: realarray; (value to convert)
		     len: natural; (length desired)
		     var i1, i2: integer (resulting integers)} ;
		    
{ Convert a real value, stored as an extended value, into two
  integers.  This assumes (obviously) that all real values will
  fit into two integers.  It further assumes that integers are
  are 32 bits long and we can combine two 16 bit values into
  a single 32 bit value easily.  However, it makes no assumptions
  about the order in which these values are stored.
}
    var
      dumm: boolean; {dummy error flag}
      tr: unsigned;

    begin
{      if len = quad then fptodouble(rval, rval, dumm)
      else fptosingle(rval, rval, dumm); }
      tr := rval[1];
      i1 := tr * 65536 + rval[2];
      tr := rval[3];
      i2 := tr * 65536 + rval[4];
    end; {realtoints}


procedure refglobal
                   {m: modes; (usercall, supportcall, etc.)
                    what: integer (which routine or other global) };

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
        if not newok(size(globalentry)) then abort(outofmem);
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


procedure refsymbol
                   {n: string8 (symbol name) };

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
        if not newok(size(globalentry)) then abort(outofmem);
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


procedure defglobal
                   {m: modes; (usercall, supportcall, etc.)
                    what: integer; (which routine or other global)
                    sect: commonlong_reloc_type; (which section)
                    where: addressrange (where within the section) };

{ Append a global definition to the definition list.  Apollo only.
}

  var
    g: globalentryptr; {used to create the new entry}


  begin {defglobal}
    if targetopsys = apollo then
      begin
      if not newok(size(globalentry)) then abort(outofmem);
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


procedure defsymbol{sym: string8; (symbol name)
                    sect: commonlong_reloc_type; (which section)
                    where: addressrange (where within the section) };

{ Append a symbol definition to the definition list.  Apollo only.
}

  var
    g: globalentryptr; {used to create the new entry}


  begin {defsymbol}
    if targetopsys = apollo then
      begin
      if not newok(size(globalentry)) then abort(outofmem);
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


procedure newnode;

{ Increment "lastnode", checking for instruction table overflow.  Sets
"lastptr" using cwriteaccess, to allow caller to easily fill in the
node.
}


  begin {newnode}
    lastnode := lastnode + 1;
    if lastnode = cnodetablesize then abort(manynodes)
    else if bigcompilerversion then lastptr := ref(bignodetable[lastnode])
    else cwriteaccess(lastnode, lastptr);
  end {newnode} ;


procedure geninst{i: insttype; (instruction to generate)
                  l: operandrange; (number of operands)
                  olen: datarange (length of operands) };

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


procedure genoprnd{o: operand (operand to generate) };

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
      abort(inconsistent);
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


procedure genlongword{data: unsigned};

{ Generates a longword of constant data.  Currently only used for mc68881
  double and extended constants that must be must be passed by pointer because
  there are no 64 or 96 bit immediate modes.
}


  begin {genlongword}
    newnode;
    lastptr^.kind := datanode;
    lastptr^.data := data;
  end; {genlongword}


procedure genlabeldelta{l1, l2: integer (base and offset labels) };

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


procedure genlabel{l: integer (label number) };

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


procedure genbr{inst: insttype; (branch to generate)
                l: integer (label number) };

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


procedure genrelbr{inst: insttype; (branch to generate)
                   reladd: integer (branch over reladd instructions) };

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


procedure gendb{i: insttype; (db-style inst to gen)
                regkey: keyindex; (contains register portion of inst)
                l: integer (label to branch to) };

{ Gen a "db" instruction, decrement and branch register.
}


  begin {gendb}
    geninst(i, 2, word);
    genoprnd(keytable[regkey].oprnd);
    genlabel(l);
    lastptr^.labelcost := word;
  end {gendb} ;


Procedure gen1{i: insttype;
               datalen: datarange;
               dst: keyindex};

{ Generate a single operand instruction, using keytable[dst] as
  the destination.
}


  begin {gen1}
    geninst(i, 1, datalen);
    genoprnd(keytable[dst].oprnd);
  end {gen1} ;


procedure gen2{i: insttype; (the instruction to generate)
               datalen: datarange; (length of operation in bytes)
               src, dst: keyindex (keytable indices of operands) };

{ Generate a double operand instruction, using keytable[src/dst] as
  the two operands.
}


  begin {gen2}
    geninst(i, 2, datalen);
    genoprnd(keytable[src].oprnd);
    genoprnd(keytable[dst].oprnd);
  end {gen2} ;


procedure gen_bf_inst{i: insttype; (the instruction to generate)
                      datalen: datarange; (length of operation in bits)
                      src, dst: keyindex (keytable indices of operands)
                      offset: keyindex (offset of bit field from base address)};

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


procedure genadcon
                  {m: modes; (what kind of thing we're referring to)
                   what: integer; (which one, or its location)
                   where: commonlong_reloc_type (which section, if known) };

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


procedure gensymboladcon
                        {n: string8 (symbol name) };

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
      if key >= stackcounter then abort(manykeys);
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
        oprnd := operand(nomode, 0, 0, false, int, 1, unknown,
                         nomode, 0, 0, 0);
        end
      else if (key <> 0) and (access <> valueaccess) then
        begin
        write('setcommonkey screwup:', key: 1,', ',lastkey:1);
        abort(inconsistent);
        end;
      len := pseudoinst.len;
      refcount := pseudoinst.refcount;
      copycount := pseudoinst.copycount;
      copylink := 0;
      tempflag := false;
      instmark := lastnode + 1;
      end;
  end {setcommonkey} ;


procedure settemp{lth: datarange; (length of data referenced)
                  m: modes; (args are the same as for setvalue)
                  reg, indxr: regindex;
                  indxlong: boolean;
                  offset, offset1: integer
                  scale: scale_range;
                  commonlong_reloc: commonlong_reloc_type};

{ Set up a temporary key entry with the characteristics specified.  This has
  nothing to do with runtime temp administration.  It strictly sets up a key
  entry.  Negative key values are used for these temp entries, and they are
  basically administered as a stack using "tempkey" as the stack pointer.
}


  begin {settemp}
    if tempkey = lowesttemp then abort(interntemp);
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


procedure settempreg{lth: datarange; (length of data referenced)
                     m: modes; (normally dreg, areg or fpreg)
                     reg: regindex (associated register) };

{ Shorthand call to settemp when only mode and register fields are
  meaningful.
}


  begin {settempreg}
    settemp(lth, m, reg, 0, false, 0, 0, 1, unknown);
  end; {settempreg}


procedure settempareg{reg: regindex (areg to set new key temp to) };

{ Shorthand call to settemp when only mode and register fields are
  meaningful.
}


  begin {settempareg}
    settemp(long, areg, reg, 0, false, 0, 0, 1, unknown);
  end; {settempareg}


procedure settempdreg{lth: datarange; (length of data referenced)
                    reg: regindex (associated register) };

{ Shorthand call to settemp when only mode and register fields are
  meaningful.
}


  begin {settempdreg}
    settemp(lth, dreg, reg, 0, false, 0, 0, 1, unknown);
    keytable[tempkey].oprnd.flavor := int;
  end {settempdreg} ;


procedure settempfpreg{reg: regindex (fpreg to set new key temp to) };

{ Shorthand call to settemp when only mode and register fields are
  meaningful.
}


  begin {settempfpreg}
    settemp(long, fpreg, reg, 0, false, 0, 0, 1, unknown);
    keytable[tempkey].oprnd.flavor := float;
    keytable[tempkey].len := 12;
  end; {settempfpreg}


procedure settempimmediate{lth: datarange; (length of data referenced)
                           value: integer (literal value to set) };

{ Shorthand call to settemp for literal keytable entries.
}


  begin {settempimmediate}
    settemp(lth, immediate, 0, 0, false, value, 0, 1, unknown);
  end {settempimmediate} ;


procedure settempsymbol{sym: string8 (symbol name) };

{ Shorthand call to settemp for symbol references.
}


  begin {settempsymbol}
    settemp(long, symbol, 0, 0, false, 0, 0, 1, unknown);
    keytable[tempkey].oprnd.name := sym;
  end {settempsymbol} ;


procedure settempadcon{m: modes;
                       offset: integer;
                       commonlong_reloc: commonlong_reloc_type} ;

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
        if not newok(size(indirect_ref)) then abort(outofmem);
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


procedure delete{m: nodeindex; (first node to delete)
                 n: nodeindex (number of nodes to delete) };

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
        if bigcompilerversion then p1 := ref(bignodetable[cp1^.n + 1])
        else creadaccess(cp1^.n + 1, p1);

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
                p1 := ref(bignodetable[p2^.brnodelink + 1])
              else creadaccess(p2^.brnodelink + 1, p1);

              p2^.brnodelink := p1^.brnodelink;
              finished := true;
              end
            else if bigcompilerversion then
              p1 := ref(bignodetable[p2^.brnodelink + 1])
            else creadaccess(p2^.brnodelink + 1, p1);
            end {while} ;
          cp2 := cp1;
          cp1 := cp2^.nextbr;
          end;
        end {while} ;
    end {unlinklabelnode} ;


  begin {delete}
    for i := 1 to n do
      begin
      if bigcompilerversion then p := ref(bignodetable[m])
      else cwriteaccess(m, p);

      if p^.kind in [instnode, errornode] then
        count := p^.oprndcount
      else begin
        if p^.kind <> stmtref then
          begin
          write('attempt to delete non-inst node:', m: 1);
          abort(inconsistent);
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
        if bigcompilerversion then p := ref(bignodetable[m])
        else creadaccess(m, p);

        if p^.kind = labelnode then
          begin
          unlinklabelnode(m);

          if bigcompilerversion then p := ref(bignodetable[m])
          else cwriteaccess(m, p);
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


procedure newtemp{size: addressrange (size of temp to allocate) };

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
    if stackcounter <= lastkey then abort(manykeys)
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


{ Runtime environment utilities.

  Used to manipulate the temp stack at runtime.
}


procedure adjustoffsets{firstnode: nodeindex; (first node of scan)
                        change: boolean (true if offset is to change) };

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
      if bigcompilerversion then p := ref(bignodetable[i])
      else creadaccess(i, p);

      with p^ do
        if tempcount <> 0 then
          if tempcount < 0 then
            begin
            if needcaching then blocksin[1].written := true;
            tempcount := tempcount + 1;
            if change then
              if kind = oprndnode then oprnd.offset := oprnd.offset + delta
              else abort(badadjust);
            end
          else
            begin
            if needcaching then blocksin[1].written := true;
            tempcount := tempcount - 1;
            if change then
              if kind = oprndnode then oprnd.offset := oprnd.offset - delta
              else if kind = labelnode then stackdepth := stackdepth - delta
              else abort(badadjust);
            end;
      end;

    stackcounter := stackcounter + 1;

  end {adjustoffsets} ;


procedure zaptemps{cnt: integer; (Number of temps to delete)
                   change: boolean (set if temp was never used) };

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


procedure returntemps{cnt: integer (number of temps to return) };

{ Return temps from the stack, decrementing the book-keeping fields of
  any instructions emitted since they were allocated
}


  begin {returntemps}
    zaptemps(cnt, false);
    stackoffset := - keytable[stackcounter].oprnd.offset;
  end {returntemps} ;


function uselesstemp {: boolean };

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



procedure definelabel{l: integer (label number to define) };

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
      if nextlabel = labeltablesize then abort(manylabels)
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


function findlabel{labno: integer (desired label number) : labelindex };

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


procedure callsupport{bn: libroutines (support routine to call) };

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
    else if $pic and not mc68020 then
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


procedure supname{libroutine: libroutines;
                  var s: packed array[lo..hi: integer] of char};

  var
    t: packed array [1..10] of char;
    i: integer;

  begin {supname}
    case libroutine of
      lib68881init:     t := 'p_68881   ';
      libarctan:        t := 'p_fatn    ';
      libbreak:         t := 'p_break   ';
      libcap:           t := 'p_cap     ';  { for Modula-2}
      libcasetrap:      t := 'p_caserr  ';
      libcexit:         t := 'p_cexit   ';  { for C }
      libmexit:         t := 'p_mexit   ';  { for Modula-2 }
      libcidiv:         t := 'p_cidiv   ';  { for C }
      libcinit:         t := 'p_centry  ';  { for C }
      libminit:         t := 'p_minit   ';  { for Modula-2 }
      libclose:         t := 'p_close   ';
      libcloseinrange:  t := 'p_clsrng  ';
      libconnect:       t := 'p_connect ';
      libcopy:          t := 'p_copy    ';
      libcos:           t := 'p_fcos    ';
      libcvtdr:         t := 'p_cvtdf   ';
      libcvtrd:         t := 'p_cvtfd   ';
      libdadd:          t := 'p_dadd    ';
      libdarctan:       t := 'p_datn    ';
      libdbgtrap:       t := 'p_dbgtrp  ';
      libdcos:          t := 'p_dcos    ';
      libddiv:          t := 'p_ddiv    ';
      libdefinebuf:     t := 'p_define  ';
      libdelete:        t := 'p_delete  ';
      libdeql:          t := 'p_deql    ';
      libdexp:          t := 'p_dexp    ';
      libdfloat:        t := 'p_dfloat  ';
      libdufloat:       t := 'p_dufloat ';  { for C }
      libdfloat_uns:    t := 'p_dfltu   ';
      libdispose:       t := 'p_dispos  ';
      libdgtr:          t := 'p_dgtr    ';
      libdln:           t := 'p_dln     ';
      libdlss:          t := 'p_dlss    ';
      libdmult:         t := 'p_dmul    ';
      libdround:        t := 'p_dround  ';
      libdsin:          t := 'p_dsin    ';
      libdsqr:          t := 'p_dsqr    ';
      libdsqrt:         t := 'p_dsqrt   ';
      libdsub:          t := 'p_dsub    ';
      libdswap:         t := 'p_dswap   ';
      libdtime:         t := 'p_dtime   ';
      libdtrunc:        t := 'p_dtrunc  ';
      libdf:            t := 'p_libdf   ';  { for C }
      libfd:            t := 'p_libfd   ';  { for C }
      libexit:          t := 'p_exit    ';
      libexp:           t := 'p_fexp    ';
      libfadd:          t := 'p_fadd    ';
      libfcmp:          t := 'p_fcmp    ';
      libfiletrap:      t := 'p_filerr  ';
      libfdiv:          t := 'p_fdiv    ';
      libffloat:        t := 'p_ffloat  ';
      libfufloat:       t := 'p_fufloat ';  { for C }
      libffloat_uns:    t := 'p_ffltu   ';
      libfmult:         t := 'p_fmul    ';
      libfree:          t := 'p_free    ';
      libfround:        t := 'p_fround  ';
      libfsqr:          t := 'p_fsqr    ';
      libfsub:          t := 'p_fsub    ';
      libftrunc:        t := 'p_ftrunc  ';
      libget:           t := 'p_get     ';
      libhalt:          t := 'p_halt    ';  { for Modula-2}
      libidiv:          t := 'p_idiv    ';
      libimult:         t := 'p_imul    ';
      libinitialize:    t := 'p_initio  ';
      libioerror:       t := 'p_ioerro  ';
      libiostatus:      t := 'p_iostat  ';
      libiotransfer:    t := 'p_iotrans ';  { for Modula-2}
      libln:            t := 'p_fln     ';
      libmcopy1:        t := 'm_copy1   ';  { for Modula-2}
      libmcopy2:        t := 'm_copy2   ';  { for Modula-2}
      libmcopy4:        t := 'm_copy4   ';  { for Modula-2}
      libmcopymd:       t := 'm_copymd  ';  { for Modula-2}
      libnew:           t := 'p_new     ';
      libnewprocess:    t := 'p_newprc  ';  { for Modula-2}
      libnoioerror:     t := 'p_noioer  ';
      libpack:          t := 'p_pack    ';
      libpage:          t := 'p_page    ';
      libpageo:         t := 'p_page_o  ';
      libpointertrap:   t := 'p_badptr  ';
      libpos:           t := 'p_pos     ';
      libprofilerdump:  t := 'p_prdump  ';
      libprofilerinit:  t := 'p_prinit  ';
      libprofilerstmt:  t := 'p_prstmt  ';
      libput:           t := 'p_put     ';
      librangetrap:     t := 'p_subrng  ';
      libreadchar:      t := 'p_rdc     ';
      libreadchari:     t := 'p_rdc_i   ';
      libreaddouble:    t := 'p_rdd     ';
      libreaddoublei:   t := 'p_rdd_i   ';
      libreadint:       t := 'p_rdi     ';
      libreadinti:      t := 'p_rdi_i   ';
      libreadln:        t := 'p_rdln    ';
      libreadlni:       t := 'p_rdln_i  ';
      libreadreal:      t := 'p_rdf     ';
      libreadreali:     t := 'p_rdf_i   ';
      libreadstring:    t := 'p_rds     ';
      libreadstringi:   t := 'p_rds_i   ';
      libreadxstring:   t := 'p_rdxs    ';
      libreadxstringi:  t := 'p_rdxs_i  ';
      librealloc:       t := 'p_realloc ';
      librename:        t := 'p_rename  ';
      libreset:         t := 'p_reset   ';
      librewrite:       t := 'p_rewrit  ';
      libscan:          t := 'p_scan    ';  { for Modula-2}
      libseek:          t := 'p_seek    ';
      libsin:           t := 'p_fsin    ';
      libsqrt:          t := 'p_fsqrt   ';
      libstrovr:        t := 'p_strovr  ';
      libsubscripttrap: t := 'p_subscr  ';
      libtell:          t := 'p_tell    ';
      libtime:          t := 'p_ftime   ';
      libtransfer:      t := 'p_trans   ';  { for Modula-2}
      libunpack:        t := 'p_unpack  ';
      libunsdiv:        t := 'p_udiv    ';
      libunsmod:        t := 'p_umod    ';
      libunsmult:       t := 'p_umul    ';
      libwritebool:     t := 'p_wtb     ';
      libwriteboolo:    t := 'p_wtb_o   ';
      libwritechar:     t := 'p_wtc     ';
      libwritecharo:    t := 'p_wtc_o   ';
      libwritedouble1:  t := 'p_wtd1    ';
      libwritedouble1o: t := 'p_wtd1_o  ';
      libwritedouble2:  t := 'p_wtd2    ';
      libwritedouble2o: t := 'p_wtd2_o  ';
      libwriteint:      t := 'p_wti     ';
      libwriteinto:     t := 'p_wti_o   ';
      libwriteln:       t := 'p_wtln    ';
      libwritelno:      t := 'p_wtln_o  ';
      libwritereal1:    t := 'p_wtf1    ';
      libwritereal1o:   t := 'p_wtf1_o  ';
      libwritereal2:    t := 'p_wtf2    ';
      libwritereal2o:   t := 'p_wtf2_o  ';
      libwritestring:   t := 'p_wts     ';
      libwritestringo:  t := 'p_wts_o   ';
      libdebugger_goto: t := 'p_dbggto  ';
      libdebugger_init: t := 'p_dbgint  ';
      libdebugger_entry:t := 'p_dbgent  ';
      libdebugger_exit: t := 'p_dbgext  ';
      libdebugger_step: t := 'p_dbstmt  ';
      libown:           t := 'p_own     ';
      libstrint0:       t := 'p_stri0   ';
      libstrint1:       t := 'p_stri1   ';
      libvalint:        t := 'p_vali    ';
      libstrreal0:      t := 'p_strf0   ';
      libstrreal1:      t := 'p_strf1   ';
      libstrreal2:      t := 'p_strf2   ';
      libvalreal:       t := 'p_valf    ';
      libstrdouble0:    t := 'p_strd0   ';
      libstrdouble1:    t := 'p_strd1   ';
      libstrdouble2:    t := 'p_strd2   ';
      libvaldouble:     t := 'p_vald    ';
      libinsert:        t := 'p_ins     ';
      libdeletestr:     t := 'p_delstr  ';
      otherwise
        begin
        write('Unexpected library name (', ord(libroutine):1, ')');
        abort(inconsistent);
        end;
      end;

    if language = modula2 then
      {avoid collision w/names in Pascal or C libraries}
      t[1] := 'm';

    for i := 1 to 10 do
      if i < hi then s[i] := t[i];

    for i := 11 to hi do s[i] := ' ';
  end {supname} ;


function instlength{n: nodeindex (must refer to an instruction) : integer};

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
    if bigcompilerversion then p := ref(bignodetable[n])
    else creadaccess(n, p);

    if p^.kind <> instnode then { filter the pretenders }

      if (p^.kind = stmtref) or (p^.kind = errornode) or
         (p^.kind = sectionnode) then len := 0
      else if p^.kind = labeldeltanode then len := word
      else if p^.kind = datanode then len := word
      else if p^.kind = adconnode then len := long
      else
        begin
        writeln('node ', n: 1, ' is not an instruction');
        abort(inconsistent);
        end

    else
      begin { "conventional" instruction }
      inst := p^.inst;
      format := p^.fp_format;
      oplen := max(word, p^.oprndlength);

      if inst = nop then len := 0
      else if inst = movem then
        begin
        if bigcompilerversion then p := ref(bignodetable[n + 2])
        else creadaccess(n + 2, p);

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
          if bigcompilerversion then p := ref(bignodetable[n + i])
          else creadaccess(n + i, p);

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
                      if mc68020 and $pic then
                        len := len + long + word
                      else if $pic then { 68000 pic }
                        len := len + word
                      else
                        len := len + long
                    else {not longlib}
                      len := len + word
                  else {unix}
                    if mc68020 and $pic then
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
                  abort(inconsistent);
                  end; { bitindexed }
                end; { case p^.oprnd.m }
            end; { case p^.kind }
          end { for i } ;
        end;
      if bigcompilerversion then p := ref(bignodetable[n])
      else cwriteaccess(n, p);

      p^.computed_length := len;
      end; { instruction }

    instlength := len;
  end {instlength} ;
