{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from
  Oregon Software does not apply to user-modified programs.  All copies
  of this program must display this notice and all copyright notices.

  @(#)walk.pas	4.1 7/13/90 15:44:53

  Release version: 0042 Level: 7
  Processor: All
  System: All

  Pascal-2 Compiler Tree Walker

 Last modified by PASCALS on 25-Apr-1990 13:23:28
 Purpose:
other blunder from merge process; this one in pushretaddr for vax.

}

{ Tree walking and pseudocode generation
  This piece of travrs is responsible for walking the tree built for
  a block and generating pseudocode for it.  Information about the
  tree structure is passed using "keys".  These keys directly correspond
  to the active portion of the tree.  As each node is scanned, a key is
  assigned to it and passed to the codegen pass.  Further references
  to the node are made using this key.

  The pseudo code generated is an augmented form of triples, with additional
  information passed to aid in keeping track of the tree form and common
  expressions.  Additional data includes operand length, a reference
  count and copycount, and sometimes additional operands.

  To simplify the bookkeeping problem for keys in the code generation
  pass, keys are assigned in order from 0 up, and are reused only
  when all higher number keys are unused.  That is, even though a key
  may have a zero refcount, it is reused only if all higher keys
  also have a zero refcount.  This allows codegen to keep track of
  active keys very simply.

  The tree walking routines keep track of targets and cost, and attempt
  to generate the simplist form of code.  Also, booleans are converted
  to shortcircuit evaluation in this phase only if they involve
  actual relations which are tested.  Simple boolean operations on
  boolean variables are done using the machine code for that operation.

}


unit walk;

interface

uses config, hdr, hdrt, a_t, t_c, utils, foldcom, commont, code;

procedure walk;

implementation

function getlabel(blk: basicblockptr): labelrange;
{
    Purpose:
      return the blocklabel of basic block pointed to by blk.

    Inputs:
      blk : pointer to block we want label of

    Outputs:
      getlabel : the label of the block.

    Algorithm:
      If jumping to a block that is empty, then search successor lists
      until reaching final destination, then if the block has a blocklabel
      assigned the return it else assign a new label and return it.

    Sideeffects:
      basic block is modified.

    Last Modified: 1/21/86

}


  begin {getlabel}
    { get the final destination of the jump }

    while not blk^.forcelabel and (blk^.beginstmt = 0) and
          (blk^.successor <> nil) do
      blk := blk^.successor^.suc;
    with blk^ do
      begin
      if blocklabel = 0 then blocklabel := newlabel;
      getlabel := blocklabel;
      end;
  end {getlabel} ;


function exitlabel(blk: basicblockptr): labelrange;
{
    Purpose:
      return the blocklabel of basic block pointed to by blk.

    Inputs:
      blk : pointer to block we want label of

    Outputs:
      exitlabel : the label of the block.

    Algorithm:
      NOTE: Used only by for statements because of the "free" labeling done
      by genblk. In genblk fortop labels controlled block for free and
      forbottom labels itself for free, because of this we can't jump
      to final destination without screwing up block labels.  Otherwise
      blocks can be labelled twice or labels never generated.
      If genblk were changed to label code only on "label" pseudoops
      the world would be a better place.

      If the block has a blocklabel assigned the return it else
      assign a new label and return that.

    Sideeffects:
      basic block is modified.

    Last Modified: 1/21/86

}


  begin {exitlabel}
    if blk = nil then exitlabel := 0
    else
      with blk^ do
        begin
        if blocklabel = 0 then blocklabel := newlabel;
        exitlabel := blocklabel;
        end;
  end {exitlabel} ;


procedure walkvalue(root: nodeindex; {root of tree to walk}
                    var key: keyindex; {resulting key}
                    targetkey: keyindex {target (0 if none)} );
  forward;

{ Walk a tree or subtree building a value.  See the forward body for
  further data
}


procedure walkboolean(root: nodeindex; {root of tree to walk}
                      var key: keyindex; {resulting key}
                      tlabel: labelrange; {lab for true result}
                      flabel: labelrange {lab if false result} );
  forward;

{ Walk a tree for a boolean expression, doing short circuit evaluation
  if possible, and generating code to jump to the false label if the
  condition is not met.  The true label is used for the short circuit
  evaluation of logical connectives.  For more details see the
  forward body.
}


procedure walknode(root: nodeindex; {root of tree to walk}
                   var key: keyindex; {resulting key}
                   targetkey: keyindex; {target (0 if none)}
                   counting: boolean {decr ref counts} );
  forward;

{ Walk an expression tree rooted in "root".  If "counting" is set, the
  reference count for that node will be decremented.  For details see
  the forward body.
}


procedure clearkeys;

{ Clear all reusable keys.  A key is reusable when it has a reference
  count of zero, and all keys greater than it are reusable.  This allows
  the code generator to keep track of useful keys with a simple pointer
  to the key for the last pseudocode read.
}

  var
    done: boolean; {true when no more keys reusable}
    ptr: nodeptr; {used for access to key nodes}


  begin
    if oktoclear then
      with context[contextsp] do
        begin
        done := high < low;
        while not done do
          begin
          if bigcompilerversion then ptr := @(bignodetable[keytable[high]]);
          if (keytable[high] = 0) or (ptr^.refcount = 0) then high := high - 1
          else done := true;
          done := done or (high < low);
          end;
        end;
  end {clearkeys} ;


procedure definelabel(l: labelrange {label to define} );

{ Generate pseudocode to define a label with number "l".  The label
  will refer to the next pseudocode generated.
}


  begin
    genpseudo(pseudolabel, 0, 0, 0, 0, l, 0, 0);
  end {definelabel} ;


procedure definesavelabel(l: labelrange {label to define} );

{ Begin a new context level, and emit pseudocode which both defines a
  label and informs the code generator that a new context level is
  being entered.  This is the code generator analog of "savecontext",
  and handles context overflow by emitting a pseudolabel rather
  than savelabel to prevent overflow in the code generator.
}


  begin
    clearkeys;
    if contextsp = contextdepth then
      begin
      overflowdepth := overflowdepth + 1;
      definelabel(l);
      end
    else
      begin
      genpseudo(savelabel, 0, 0, 0, 0, l, 0, 0);
      contextsp := contextsp + 1;
      with context[contextsp] do
        begin
        high := context[contextsp - 1].high;
        low := high + 1;
        origlow := low;
        end;
      end;
  end {definesavelabel} ;


procedure definerestorelabel(l: labelrange {label to define} );

{ End a context level, restoring prior keys, and emit pseudocode which
  both defines a label and informs the code generator to restore a
  previously saved context.  If we are in a context overlow situation,
  a pseudolabel operator is emitted and no context operation performed.
}


  begin
    {The following is a dead code elimination kludge}
    {please explain. }
    if overflowdepth <= 0 then
      with context[contextsp] do low := origlow;
    clearkeys;
    if overflowdepth > 0 then
      begin
      overflowdepth := overflowdepth - 1;
      definelabel(l);
      end
    else
      begin
      genpseudo(restorelabel, 0, 0, 0, 0, l, 0, 0);
      contextsp := contextsp - 1;
      end;
  end {definerestorelabel} ;


procedure defineclearlabel(l: labelrange {label to define} );

{ Emit pseudocode which both defines a label and informs the code generator
  that all keys in the current context are invalid and should be cleared.
}


  begin
    genpseudo(clearlabel, 0, 0, 0, 0, l, 0, 0);
    clearkeys;
  end {defineclearlabel} ;


procedure definejoinlabel(l: labelrange {label to define} );

{ Emit pseudocode which both defines a label and informs the code generator
  that contexts just joined, and keys should be updated accordingly.
}


  begin
    genpseudo(joinlabel, 0, 0, 0, 0, l, 0, 0);
    clearkeys;
  end {joinlabel} ;



{ Actual tree walking procedures -
}


procedure shortvisit(root: nodeindex; {tree to visit}
                     inpushaddr: boolean {true sez part of a pshaddr} );

{ This procedure visits all nodes in the tree rooted in "root" and
  generates pseudocode for those nodes with ref count > 1.  This is
  used for components of a short-circuit expression to make sure
  that all common subexpressions which are used later get generated
  no matter what portion of the expression is actually executed at
  runtime.
}

  var
    j: 1..3; {induction for operand scan}
    k: keyindex; {dummy argument to walknode}
    ptr: nodeptr; {used for access to root node}
    op: operatortype; {operator for this node}
    oprndptr: nodeptr; {used for access one level down}
    newroot: nodeindex; {used in rem/quo hack}


  begin
    if bigcompilerversion then ptr := @(bignodetable[root]);
    newroot := root;
    op := ptr^.op;
    if ptr^.action = visit then
      begin
      if (op in [quoop, remop]) and (ptr^.refcount = 1) then
        begin
        newroot := ptr^.oprnds[1];
        if bigcompilerversion then ptr := @(bignodetable[ptr^.oprnds[1]]);
        end;
      if not ptr^.local and ((ptr^.refcount > 1) or (ptr^.op = mulop) and
         (ptr^.form = ints) or
         (op in
         [pushcvalue, divop, quoop, remop, paindxop, chrstrop, arraystrop, commaop]) or
         switcheverplus[largemodel] and ((ptr^.op = filebufindrop) or
         (ptr^.op = indrop))) then
        walknode(root, k, 0, false)
      else
        begin
        inpushaddr := inpushaddr or (op = pushaddr) or (op = pushstraddr);
        if inpushaddr and (language <> c) then
          for j := 1 to 3 do
            if ptr^.nodeoprnd[j] then
              begin
              if bigcompilerversion then
                oprndptr := @(bignodetable[ptr^.oprnds[j]]);
              if (oprndptr^.op in
                 [originop, call, callparam, unscall, unscallparam]) or
                 (oprndptr^.form in [sets, strings]) then
                begin
                walknode(ptr^.oprnds[j], k, 0, false);
                if bigcompilerversion then ptr := @(bignodetable[newroot]);
                end;
              end;
        if (ptr^.op = rd) and (targetmachine in [iAPX86, i80386, ns32k]) then
          begin
{
            Walk this early since work registers may die
}
          walknode(ptr^.oprnds[2], k, 0, false);
          end;
        for j := 1 to 3 do
          if ptr^.nodeoprnd[j] then
            begin
            shortvisit(ptr^.oprnds[j], inpushaddr);
            if bigcompilerversion then ptr := @(bignodetable[newroot]);
            end;
        if (ptr^.op < intop) and (ptr^.slink <> 0) then
          shortvisit(ptr^.slink, false);
        end;
      end;
  end {shortvisit} ;


procedure unnestsets(root: nodeindex {tree to visit} );

{ This procedure visits all nodes in the tree rooted in "root" and
  generates pseudocode for any bldset nodes. This is done so that the
  code generator doesn't have to keep track of nested set targets.
  In the rare (!) case that a set element involves another set, we
  walk the nested set here.  Called only by bldsetnode.
}

  var
    j: 1..3; {induction for operand scan}
    k: keyindex; {dummy argument to walknode}
    ptr: nodeptr; {used for access to root node}


  begin {unnestsets}
    if bigcompilerversion then ptr := @(bignodetable[root]);
    if ptr^.action = visit then
      if ptr^.op = bldset then walknode(root, k, 0, false)
      else
        begin
        for j := 1 to 3 do
          if ptr^.nodeoprnd[j] then
            begin
            unnestsets(ptr^.oprnds[j]);
            if bigcompilerversion then ptr := @(bignodetable[root]);
            end;
        if (ptr^.op < intop) and (ptr^.slink <> 0) then unnestsets(ptr^.slink);
        end;
  end {unnestsets} ;


function targetpresent(p: nodeindex {node to check} ): boolean;

{ True if the target node is used in the expression represented by "p".
  This simply checks "p" and its operands (or class representative for
  a copy operation) for the target flag.
}

  var
    cnt: 0..3; {induction var for checking operands}
    found: boolean; {set if target found}
    ptr: nodeptr; {used to access nodes}


  begin
    found := false;
    if bigcompilerversion then ptr := @(bignodetable[p]);
    if ptr^.action = recopy then found := targetpresent(ptr^.oldlink)
    else
      begin
      found := ptr^.target;
      cnt := 0;
      while (cnt < 3) and not found do
        begin
        cnt := cnt + 1;
        if ptr^.nodeoprnd[cnt] then
          begin
          found := targetpresent(ptr^.oprnds[cnt]);
          end;
        end;
      if not found and (ptr^.op < intop) and (ptr^.slink <> 0) then
        found := targetpresent(ptr^.slink);
      end;
    targetpresent := found;
  end {targetpresent} ;


function newkey: keyindex;

{ Return the next highest key.  If none available, give panic error
  and stop compilation.
}


  begin
    if context[contextsp].high = keysize then compilerabort(manykeys)
    else newkey := context[contextsp].high + 1;
  end {newkey} ;


procedure walknode(root: nodeindex; {root of tree to walk}
                   var key: keyindex; {resulting key}
                   targetkey: keyindex; {target (0 if none)}
                   counting: boolean {decr ref counts} );

{ Walk the expression tree rooted in "root", generating code as needed.
  The key for the root node is returned in "key".  "targetkey" is
  passed on to the code generator to aid in generating good code.  In
  the case of an assignment, this is the key for the target.
  If "counting" is set, the refcount of the root is decremented.
  This is the normal action unless some common expressions are being
  precomputed, as by "shortvisit".

  If there are multiple nodes linked through "slink", these are walked
  sequentially.

  There are several possible states for a node, and "walknode" takes
  appropriate action for the particular node state.  The states are:

  visit:        The node is an expression, and has not been walked.

  revisit:      As visit, but the node has been walked, just pick up the
                key from the keytable.

  copy:         The node is a copy of a common expression on entering
                a new context, and the copy has not been generated.

  recopy:       Same as copy, but the node has been walked, so just
                pick up the key from the keytable.

  The routines "visitnode" and "copynode" are used to handle nodes which
  must be walked.

  The variables "refcount", "copycount" and "len" are global to
  the internal routines which handle individual nodes, and give
  the parameters of the root node.
}

  var
    rootp: nodeptr; {used to access root}
    nextroot: nodeindex; {next node along slink path}
    refcount: refcountrange; {refcount for root node}
    copycount: refcountrange; {copy count for root node}
    len: addressrange; {length for root node}


  procedure mapkey;

{ Find or create a key for the root node.  The keytable is simply
  a table of node indices, and each context has a high and low index
  into this table.  The table is searched for the index of the root node,
  and if not found it is added to the high end of the table.

  The result is left in the argument (to walknode) "key".
}

    var
      i: keyindex; {induction var for search}


    begin {mapkey}
      with context[contextsp] do
        begin
        i := 0;
        key := newkey;
        keytable[key] := root;
        repeat
          i := i + 1;
        until (keytable[i] = root);
        if i = key then high := i
        else key := i;
        end;
    end {mapkey} ;


  procedure visitnode;

{ Walk a node in the state "visit".  This routine contains many small
  node-specific routines which generate pseudocode for the nodes as
  they are walked.  This is the routine which actually generates the
  majority of the expression code.

  Data on the left and right operands of binary (or unary) operators
  is kept local to this routine to reduce accesses to the tree for
  the internal procedures.
}

    var
      lkey, rkey: keyindex; {keys for left and right operands}
      rootp: nodeptr; {used to access root node}
      l, r: integer; {left and right operand nodes, or literals}


    function cost(p: nodeindex {node to check} ): shortint;

{ Returns the cost of the operation represented by node p.  This is
  the number of temporary locations needed to compute the operation
  and all of its sub-trees.
}

      var
        ptr: nodeptr; {used to access node}


      begin
        if bigcompilerversion then ptr := @(bignodetable[p]);
        cost := ptr^.cost;
      end {cost} ;


    procedure walkboth;

{ Walk both operands, called for binary operations.

  The operand with the largest cost is walked first, as we know that
  the other operand can then be computed with at most one more register
  used (worst case when costs are equal).  If the target is present in
  the second node to be walked, the target key is not passed to the
  first node, since it will not be able to make use of it.  Only the
  final calculation in an expression can change the target.
}

      var
        lefttarget, righttarget: nodeindex; {target to pass to left or right
                                             operand}


      begin {walkboth}
        if (targetkey <> 0) and (l <> r) and targetpresent(l) then
          righttarget := 0
        else righttarget := targetkey;
        if (targetkey <> 0) and (l <> r) and targetpresent(r) then
          lefttarget := 0
        else lefttarget := targetkey;
        if cost(l) >= cost(r) then
          begin
          walkvalue(l, lkey, lefttarget);
          if lefttarget <> 0 then righttarget := 0;
          walkvalue(r, rkey, righttarget);
          end
        else
          begin
          walkvalue(r, rkey, righttarget);
          if righttarget <> 0 then lefttarget := 0;
          walkvalue(l, lkey, lefttarget);
          end;
        mapkey;
      end {walkboth} ;


{ Node-specific walking routines.
  This series of routines take node-specific actions to generate pseudo-
  code for expressions.  They are called from the large case statement
  which makes up the majority of "visitnode".
}


    procedure indxnode(p: pseudoop {operator for root node} );

{ Walk and generate code for "indx" or "pindx" operations.

  These operations compute the address of a variable, and this routine
  checks to see if the variable should be assigned to a register.
  Analys made an estimate of the best local vars to assign to registers,
  and if the operation describes a local var the displacement is
  checked against the list from analys and assigned to the temp register
  if it matches.
}

      var
        lp: nodeptr; {used to access left operand}
        possibletemp: boolean; {true if local var and possible temp loc}
        offset: addressrange; {variable offset if possibletemp}
        j: 0..regtablelimit; { var for temp search }
        third: integer; {third operand, zero or temp number}
        paramflag: boolean; {true if indexing parameter level}


      begin
        if bigcompilerversion then lp := @(bignodetable[l]);
        third := 0;
        offset := rootp^.oprnds[2];
        paramflag := l = localparamnode;
        with lp^ do
          if action = revisit then
            possibletemp := (p = indx) and (op = levop) and (oprnds[1] = level)
          else possibletemp := false;

        walknode(l, lkey, 0, true);

        if possibletemp then
          begin
            { This hashes the var's offset, really should be a function call.
              However it would be called in several high bandwidth places and
              places an unneeded speed penalty on this phase.
              This code is replicated in procedures: doreference, killasreg
              and dodefine in travrs.
            }
          j := (offset div targetintsize) mod (regtablelimit + 1);
          while ((regvars[j].offset <> offset) or
                (regvars[j].parameter <> paramflag)) and
                (regvars[j].worth >= 0) do
            j := (j + 1) mod (regtablelimit + 1);
          if regvars[j].regid <> 0 then
            begin
            { assigned to a register }
            third := regvars[j].regid;
            case regvars[j].regkind of
              genreg, bytereg: p := regtemp;
              realreg: p := realtemp;
              ptrreg: p := ptrtemp;
              end;
            end;
          end;

        mapkey;
        genpseudo(p, len, key, refcount, copycount, lkey, offset, third);

      end {indxnode} ;


    procedure vindxnode;

{ Walk and generate code for "vindx" operations.

  This is very similar to "indx" except that it is used for local
  variables in C functions.  Such functions allow variables local to
  inner blocks, so the blocksize is unknown until function exit.
  This routine computes the actual address from that data.
  Also, any variable references with 'vindx" is a register candidate.
  Note that there is an arbitrary "offset" used only to identify the variable,
  and a real offset which is the location in the block.

  These operations compute the address of a variable, and this routine
  checks to see if the variable should be assigned to a register.
  Analys made an estimate of the best local vars to assign to registers,
  and if the operation describes a local var the displacement is
  checked against the list from analys and assigned to the temp register
  if it matches.
}

      var
        lp: nodeptr; {used to access left operand}
        offset: addressrange; {actual variable offset}
        trav_offset: addressrange; {traverse offset for temp check}
        j: 0..regtablelimit; { var for temp search }
        third: integer; {third operand, zero or temp number}
        p: pseudoop; {the operation to generate}


      begin
        if bigcompilerversion then lp := @(bignodetable[l]);
        third := 0;
        case targetmachine of
          vax:
{to be verified ... esp. the P2 linkage case}
            begin
            if newtravrsinterface then
              with new_proctable[blockref div (pts + 1)]^
                                [blockref mod (pts + 1)] do
                if (lp^.oprnds[2] = 0) and
                   (calllinkage = nonpascalcall) then
                  offset := rootp^.oprnds[2] - final_block_size
                else offset := rootp^.oprnds[2]
            else
              with proctable[blockref] do
                if (lp^.oprnds[2] = 0) and
                   (calllinkage = nonpascalcall) then
                  offset := rootp^.oprnds[2] - final_block_size
                else offset := rootp^.oprnds[2];
            end;
          otherwise
            offset := final_block_size - rootp^.oprnds[2];
        end {case};
        trav_offset := rootp^.oprnds[3];
        p := indx;

        walknode(l, lkey, 0, true);

        if trav_offset <> 0 then
          begin
          { This hashes the var's offset, really should be a function call.
            However it would be called in several high bandwidth places and
            places an unneeded speed penalty on this phase.
            This code is replicated in procedures: doreference, killasreg
            and dodefine in travrs.
          }
          j := (trav_offset div targetintsize) mod (regtablelimit + 1);
          while (regvars[j].offset <> trav_offset) and
                (regvars[j].worth >= 0) do
            j := (j + 1) mod (regtablelimit + 1);
          if regvars[j].regid <> 0 then
            begin
            { assigned to a register }
            third := regvars[j].regid;
            case regvars[j].regkind of
              genreg, bytereg: p := regtemp;
              realreg: p := realtemp;
              ptrreg: p := ptrtemp;
              end;
            end;
          end;

        mapkey;
        genpseudo(p, len, key, refcount, copycount, lkey, offset, third);

      end {vindxnode} ;


    procedure openarraynode;

{ walk and generate code for copying an open array value parameter's value
into newly-opened space in the procedure's stack frame. }


      begin {openarraynode}

        genpseudo(openarray, len, 0, 0 {refcount} , 0 {copycount} ,
                  rootp^.oprnds[1] {offset} , rootp^.oprnds[2], 0);

      end {openarraynode} ;


    procedure orderednode(op: operatortype; {root operator}
                          form: types {operand form} );

{ Walk and generate code for an operator whose right operand should
be walked first irregardless of which operand has higher cost.
Currently, the only operators which fit into this catagory cannot
make use of targetting information, so the operands are walked
with target = 0.
}


      begin {orderednode}
        walkvalue(r, rkey, 0);
        walkvalue(l, lkey, 0);
        mapkey;
        genpseudo(map[op, form], len, key, refcount, copycount, lkey, rkey,
                  targetkey);
      end {orderednode} ;


    procedure binarynode(op: operatortype; {root operator}
                         form: types {operand type} );

{ Walk and generate code for a normal binary operation "op" with
  operand type "form".  The array "map" converts the operator and
  type into a pseudocode operator.

  ***PDP11***
  The special targets multarget and divtarget are passed to the
  operand trees to facilitate good code generation if these
  operations require special action or register allocation.
}

      var
        oldtarget: keyindex; {target key for root node}


      begin
        oldtarget := targetkey;
        if targetmachine = pdp11 then targetkey := max(multarget, targetkey);
        walkboth;
        genpseudo(map[op, form], len, key, refcount, copycount, lkey, rkey,
                  oldtarget);
      end {binarynode} ;


    procedure incnode(op: operatortype; {root operator}
                      form: types; {operand type}
                      op3: integer {third operand} );

{ Walk and generate code for a pre or pos increment operation with
  operand type "form".  The array "map" converts the operator and
  type into a pseudocode operator.

  If the third operand is negative, it is passed along in place of the
  target
}


      begin
        if op3 >= 0 then op3 := targetkey;
        walkboth;
        genpseudo(map[op, form], len, key, refcount, copycount, lkey, rkey,
                  op3);
      end {incnode} ;


    procedure reflexivenode(op: operatortype; {root operator}
                            form: types {operand type} );

{ Walk a reflexive operator.  We fake this as an ordinary operator
  but the left hand side is always the target.  We then fake a binary
  operation followed by a move.  The reference count of the left side
  was incremented when building especially to allow this.
  Some casts may well be necessary to keep things in the proper form.
  These are computed from the type of the operation and the type
  of the lvalue.
}

      var
        target: keyindex; {dummy target operand}
        tkey: keyindex; {temp for result}
        ckey: keyindex; {cast lvalue key}
        lp: nodeptr; {for access to the left side}
        lform: types; {type of lvalue}
        llen: addressrange; {length of lvalue}
        use_casts: boolean; {we have to insert casts}


      begin
        target := 0;
        if bigcompilerversion then lp := @(bignodetable[l]);
        llen := lp^.len;
        lform := types(rootp^.oprnds[3]);
        walknode(l, lkey, 0, true);
        genpseudo(startreflex, 0, 0, 0, 0, 0, 0, 0);
        use_casts := (lform <> form) or (lform = reals) and (llen <> len);
        if use_casts then
          begin
          ckey := newkey;
          context[contextsp].high := ckey;
          keytable[ckey] := 0;
          genpseudo(castmap[form, lform], len, ckey, 1, 0, lkey, 0, 0);
          end
        else ckey := lkey;
        walkvalue(r, rkey, 0);
        tkey := newkey;
        context[contextsp].high := tkey;
        keytable[tkey] := 0;
        genpseudo(map[op, form], len, tkey, 1, 0, ckey, rkey, ckey);
        if use_casts then
          begin
          ckey := newkey;
          context[contextsp].high := ckey;
          keytable[ckey] := 0;
          genpseudo(castmap[lform, form], llen, ckey, 1, 0, tkey, 0, 0);
          end
        else ckey := tkey;
        mapkey;
        genpseudo(map[moveop, lform], llen, key, refcount, copycount, lkey,
                  ckey, targetkey);
        genpseudo(endreflex, 0, 0, 0, 0, 0, 0, 0);
      end; {reflexivenode}


    procedure reflexdivnodes(op: operatortype; {root operator}
                             form: types {operand type} );

{ Walk a reflexive divide operator.  This would be the same as any
  other reflexive operator except that integer divide and mod are
  handled specially.
}

      var
        target: keyindex; {dummy target operand}
        tkey, tdkey: keyindex; {temps for results}


      begin
        if form = ints then
          begin
          target := 0;
          walknode(l, lkey, target, true);
          genpseudo(startreflex, 0, 0, 0, 0, 0, 0, 0);
          target := 0;
          walkvalue(r, rkey, target);
          tdkey := newkey;
          context[contextsp].high := tdkey;
          keytable[tdkey] := 0;
          genpseudo(divint, len, tdkey, 1, 0, lkey, rkey, lkey);
          tkey := newkey;
          context[contextsp].high := tkey;
          keytable[tkey] := 0;
          genpseudo(map[op, ints], len, tkey, 1, 0, tdkey, 0, lkey);
          mapkey;
          genpseudo(movint, len, key, refcount, copycount, lkey, tkey,
                    targetkey);
          genpseudo(endreflex, 0, 0, 0, 0, 0, 0, 0);
          end
        else reflexivenode(op, form);
      end; {reflexdivnodes}


    procedure commanode;

{ Walk a comma operator.  This just walks and discards the left
  operand.  The result is the right operand.
}

      var
        target: keyindex; {target, ignored}
        deref: boolean; {set true if we must deref subtree}


      begin
        target := 0;
        deref := rootp^.refcount > 0;
        walknode(l, lkey, target, false);
        if deref then walkvalue(r, rkey, targetkey)
        else walknode(r, rkey, targetkey, false);
        mapkey;
        genpseudo(commafake, len, key, refcount, copycount, rkey, 0, 0);
      end; {commanode}


    procedure litopnode(op: operatortype; {root operator}
                        form: types {operand form} );

{ Walk and generate code for an operator with one literal operand.
  Only the left operand is an expression, the right is the literal
  value in the right operand location.

  Note: only values which can be represented in a single integer
  value may be used as operands for literal operations.
}


      begin
        walkvalue(l, lkey, targetkey);
        mapkey;
        genpseudo(map[op, form], len, key, refcount, copycount, lkey, r,
                  targetkey);
      end {litopnode} ;


    procedure movelitnode(form: types {operand form} );

{ Walk and generate code for "movelit".  This only differs from
  a normal litopnode in that we always emit zero as the reference
  count when language is Pascal.
}


      begin
        walkvalue(l, lkey, targetkey);
        mapkey;
        if language = pascal then
          genpseudo(map[movelit, form], len, key, 0, 0, lkey, r, targetkey)
        else
          genpseudo(map[movelit, form], len, key, refcount, copycount, lkey, r,
                    targetkey);
      end {movelitnode} ;


    procedure mulintnode(form: types {operand type} );

{ Walk and generate code for a multiply operation.
}

      var
        oldtarget: keyindex; {target key for root node}


      begin
        oldtarget := targetkey;
        walkboth;
        genpseudo(map[mulop, form], len, key, refcount, copycount, lkey, rkey,
                  oldtarget);
      end {mulintnode} ;


    procedure divintnode(op: operatortype; {divop or stddivop}
                         form: types {operand type} );

{ Walk and generate code for an integer divide operation.
}

      var
        oldtarget: keyindex; {target key for root node}


      begin
        oldtarget := targetkey;
        if targetmachine = pdp11 then targetkey := divtarget;
        walkboth;
        genpseudo(map[op, form], len, key, refcount, copycount, lkey, rkey,
                  oldtarget);
      end {divintnode} ;


    procedure unaryaddrnode(op: operatortype {root operator} );

{ Walk and generate code for a unary operator which generates an
  At the moment, the only ones are "indrop" and "filebufindrop".  The form
  and length are implicit in the operation.
}


      begin
        walknode(l, lkey, targetkey, true);
        mapkey;
        genpseudo(map[op, ints], len, key, refcount, copycount, lkey, 0,
                  targetkey);
      end {unaryaddrnode} ;


    procedure unarynode(op: operatortype; {root operation}
                        form: types {operand type} );

{ Walk and generate code for a unary operator.  Only the left operand
  is significant.
}


      begin
        if targetmachine = pdp11 then targetkey := max(multarget, targetkey);
        walkvalue(l, lkey, targetkey);
        mapkey;
        genpseudo(map[op, form], len, key, refcount, copycount, lkey, 0,
                  targetkey);
      end {unarynode} ;


    procedure castnode(op: operatortype; {root operation}
                       form: types {operand type} );

{ Walk and generate code for a cast operator.  The left operand contains
  the value being cast while the right is a flag for an unsigned result.
}


      begin {castnode};
        if targetmachine = pdp11 then targetkey := max(multarget, targetkey);
        walkvalue(l, lkey, targetkey);
        mapkey;
        genpseudo(map[op, form], len, key, refcount, copycount, lkey, r,
                  targetkey);
      end {castnode};


    procedure groupnode;

{ This is exactly like a unary node except that it generates no
  code itself.  This is for the unary "+", which is used only to
  group operands.
}


      begin
        walknode(l, lkey, targetkey, true);
        key := lkey;
      end;


    procedure setbinfilenode;

{ Walk and generate code for setbinfile operator.  Only the left operand
  is significant and must be prewalked to pull out odd nested function
  calls, believe it or not!
}


      begin
        shortvisit(l, true);
        walkvalue(l, lkey, targetkey);
        mapkey;
        genpseudo(setbinfile, len, key, refcount, copycount, lkey, 0,
                  targetkey);
      end {unarynode} ;


    procedure unmappedunarynode(op: operatortype; {root operation}
                                form: types {operand type} );

{ Walk and generate code for a unary operator that does not need its own key.
  Same as unarynode, except no mapkey.  Only the left operand is significant.
}


      begin {unmappedunarynode}
        if targetmachine = pdp11 then targetkey := max(multarget, targetkey);
        walkvalue(l, lkey, targetkey);
        genpseudo(map[op, form], len, 0, 0, 0, lkey, 0, targetkey);
      end {unmappedunarynode} ;


    procedure copystacknode;

{ Process special stack copying operator.  Different, in that we walk
  this operator in a prefix rather than postfix fashion, to guarantee
  that the operator is the first thing emitted in the expression stream
  requiring it.  This operator only occurs within I/O parameter lists
  and is used to copy the explicit file parameter, if it exists.
}


      begin
        if targetmachine = pdp11 then targetkey := max(multarget, targetkey);
        walkvalue(l, key, targetkey);
        if len <> 0 then genpseudo(makeroom, len, 0, 0, 0, 0, 0, 0);
        genpseudo(copystack, 0, 0, 0, 0, 0, 0, 0);
      end {copystacknode} ;


    procedure checknode(op: operatortype {root operator} );

{ Walk and generate code for a rangecheck operation.  This is similar
  to a unary operator except two additional range operands are in
  oprnds[2] and oprnds[3].
}

      var
        low, high: integer; {range limits}
        lowkey, highkey: keyindex; {range limits if expressions}
        exprlimit: boolean; {the limits are expresions}


      begin
        exprlimit := rootp^.nodeoprnd[2];
        low := rootp^.oprnds[2];
        high := rootp^.oprnds[3];
        walkvalue(l, lkey, 0);
        if exprlimit then
          begin
          walkvalue(low, lowkey, 0);
          walkvalue(high, highkey, 0);
          low := lowkey;
          high := highkey;
          end;
        mapkey;
        genpseudo(map[op, ints], len, key, refcount, copycount, lkey, low,
                  high);
      end {checknode} ;


    procedure movenode(op: operatortype; {root operator}
                       form: types {operand type} );

{ Walk and generate code for a "moveop". This is a simple binary operator
  except that the left operand is always walked before the right,
  and the left key is the target for the right operands.
}

      var
        size: nodeindex; {size of this move if cmoveop}
        skey: keyindex; {key for size if cmoveop}


      begin
        size := rootp^.oprnds[3];
        walknode(l, lkey, 0, true);
        walkvalue(r, rkey, lkey);
        if op = cmoveop then walkvalue(size, skey, 0)
        else skey := 0;
        mapkey;
        if language = pascal then {binary writes have screwed refcounts}
          genpseudo(map[op, form], len, key, 0, 0, lkey, rkey, skey)
        else
          genpseudo(map[op, form], len, key, refcount, copycount, lkey, rkey,
                    skey);
      end {movenode} ;


    procedure literalnode(op: operatortype {root operator} );

{ Generate pseudocode for a literal.  Here all operands are either part
  of the literal or pointers into the string area.
}


      begin
        mapkey;
        with rootp^ do
          begin
          genpseudo(map[op, ints], len, key, refcount, copycount, oprnds[1],
                    oprnds[2], oprnds[3]);
          if op = fptrop then
            if newtravrsinterface then
              new_proctable[oprnds[1] div (pts + 1)]^[oprnds[1] mod (pts +
               1)].referenced := true
            else proctable[oprnds[1]].referenced := true;
          end;
      end {literalnode} ;


    procedure crealnode;

{ Generate a series of "doreal" pseudoops containing a real number split
  into integers.  The most significant part is put out first, and a piece
  count is transmitted along with the data.  Note that the code generator
  depends on these being in a sequence with no intervening operators.
}


      begin {crealnode}
        mapkey;
        with rootp^ do
          genrealop(doreal, len, key, refcount, copycount, realvalue);
      end; {crealnode}


    procedure realnode;

{ Generate a series of "doreal" pseudoops containing a real number split
  into integers.  The most significant part is put out first, and a piece
  count is transmitted along with the data.  Note that the code generator
  depends on these being in a sequence with no intervening operators.
}

      var
        piece: 0..maxrealwords; {piece count}
        l: addressrange; {length of node}
        refc, copy: refcountrange; {ref and copy counts}
        nextpiece: nodeindex; {next piece being generated}
        nextp: nodeptr; {access to next piece to generate}


      begin
        mapkey;
        with rootp^ do
          begin
          l := len;
          refc := refcount;
          copy := copycount;
          end;
        piece := 1;
        nextp := rootp;
        repeat
          with nextp^ do
            begin
            genpseudo(doreal, l, key, refc, copy, oprnds[1], oprnds[2], piece);
            action := revisit;
            nextpiece := oprnds[3];
            end;
          piece := piece + 1;
          if nextpiece <> 0 then
            if bigcompilerversion then nextp := @(bignodetable[nextpiece]);
        until nextpiece = 0;
      end; {realnode}


    procedure ownnode;

{ Walk and generate code for a "ownop".  This passes globalsize as
  an offset to future indices off level 0.  A first-class, but should-work
  kludge, easier than defining a new magic level.
}


      begin {ownnode}
        mapkey;
        genpseudo(doown, ptrsize, key, refcount, copycount, 0, 0, 0);
        if newtravrsinterface then
          new_proctable[blockref div (pts + 1)]^[blockref mod (pts +
           1)].ownused := true
        else proctable[blockref].ownused := true;
      end {ownnode} ;


    procedure extnode;

{ Generate code for a "extop", which just passes through the index into the
  ext table.  Note: for the 8086 this should probably be split into
  "seg" and "offset" portions.  This is left as an exercise for the
  reader.
}


      begin
        with rootp^ do
          begin
          mapkey;
          genpseudo(doext, ptrsize, key, refcount, copycount, oprnds[1], 0, 0);
          end;
      end; {extnode}


    procedure originsegnode(o: pseudoop);

{ Walk and generate code for an "originop" or "segop".  This operation
  establishes addressing for an absolute address.
}


      begin {originsegnode}
        with rootp^ do
          begin
          mapkey;
          genpseudo(o, ptrsize, key, refcount, copycount, oprnds[1], 0, 0);
          end;
      end {originsegnode} ;


    procedure levnode;

{ Walk and generate code for a "levop".  This operation establishes
  addressing for a lex level.  It may or may not have an operand tree
  in the third operand.
}


      begin
        with rootp^ do
          begin
          if (oprnds[1] = 0) or (oprnds[1] = level) then rkey := 0
          else walknode(oprnds[3], rkey, 0, true);
          mapkey;
          genpseudo(dolevel, ptrsize, key, refcount, copycount, oprnds[1],
                    oprnds[2], rkey);
          end;
      end {levnode} ;


    procedure callnode(op: operatortype);

{ Walk and generate code for a procedure call.  The right operand is
  the parameter list, and the call pseudocode does not further use them.

  A "shortvisit" is done prior to evaluating the parameters so that any
  temporaries will be left on the stack before the call setup is done.
  This avoids conflicts with the use of the stack for parameters.
}

      var
        p: proctableindex; {temp for proc name}


      begin
        shortvisit(r, false);
        walknode(r, rkey, 0, true);
        mapkey;
        if bigcompilerversion then rootp := @(bignodetable[root]);
        if op = unscall then
          genpseudo(unscallroutine, len, key, refcount, copycount,
                    rootp^.oprnds[1], rootp^.oprnds[3], - ord(rootp^.form))
        else
          genpseudo(callroutine, len, key, refcount, copycount,
                    rootp^.oprnds[1], rootp^.oprnds[3], - ord(rootp^.form));
        p := rootp^.oprnds[1];
        if newtravrsinterface then
          new_proctable[p div (pts + 1)]^[p mod (pts + 1)].referenced := true
        else proctable[p].referenced := true;
        if language <> c then clearkeys;
      end {callnode} ;


    procedure callparamnode(op: operatortype);

{ Walk and generate code for a call to a parameter procedure.  This
  is similar to "callnode" except that the left operand evaluates to
  the procedure descriptor.
}


      begin {callparamnode}
        walknode(l, lkey, 0, true);
        shortvisit(r, false);
        walknode(r, rkey, 0, true);
        mapkey;
        if bigcompilerversion then rootp := @(bignodetable[root]);
        if op = unscallparam then
          genpseudo(unscallroutine, len, key, refcount, copycount, lkey,
                    rootp^.oprnds[3], ord(rootp^.form))
        else
          genpseudo(callroutine, len, key, refcount, copycount, lkey,
                    rootp^.oprnds[3], ord(rootp^.form));
        if language <> c then clearkeys;
      end {callparamnode} ;


    procedure linkednode(op: operatortype; {root operator}
                         form: types {operand type} );

{ Process the file variable setup operator for read/write procedure.
  Similar to "stacknode" except no stacktarget is generated.
}


      begin
        walknode(l, lkey, 0, true);
        walknode(r, rkey, key, true);
        mapkey;
        genpseudo(map[op, form], len, key, refcount, copycount, rkey, 0, 0);
      end {linkednode} ;


    procedure pushfinalnode;

{ Walk and generate code for the final value of a for statement.  This
  generates a stack target to hold the value, then generates the expression,
  then the push to save the value.
}


      begin
        mapkey;
        genpseudo(stacktarget, len, key, refcount, copycount, 0, 0, 0);
        walkvalue(l, lkey, key);
        genpseudo(pshint, len, key, refcount, copycount, lkey, 0, 0);
      end {pushfinalnode} ;


    procedure pushprocnode;

{ Push a procedure descriptor on the stack.  This is used to generate
  an actual procedure parameter.
}

      var
        third: proctableindex; {third root operand (procedure number)}


      begin
        third := rootp^.oprnds[3];
        walknode(l, lkey, 0, true);
        mapkey;
        genpseudo(stacktarget, len, key, refcount, copycount, 0, 0, 0);
        walknode(r, rkey, 0, true);
        genpseudo(pshproc, len, key, 0, 0, rkey, third, 0);
        if newtravrsinterface then
          new_proctable[third div (pts + 1)]^[third mod (pts +
           1)].referenced := true
        else proctable[third].referenced := true;
      end {pushprocnode} ;


    procedure pushcvaluenode(form: types);

{ Push an array value on the stack for a value conformant array parameter.
}


      begin
        walkvalue(l, lkey, targetkey);
        mapkey;
        genpseudo(stacktarget, len, key, refcount, copycount, 0, 0, 0);
        genpseudo(map[pushcvalue, form], len, key, refcount, copycount, lkey, 0,
                  0);
      end; {pushcvaluenode}


    procedure pushretnode;

{ Walk a node which "pushes" the return address of a structure.
}


      begin
        walknode(l, lkey, 0, true);
        mapkey;
        case targetmachine of
          vax:
            genpseudo(stacktarget, len, key, refcount, copycount, 0, 0, 0);
          i80386:
            genpseudo(ptrtemp, len, key, refcount, copycount, 0, 0, - 3);
          otherwise
            genpseudo(ptrtemp, len, key, refcount, copycount, 0, 0, - 2);
          end;
        walkvalue(r, rkey, key);
        genpseudo(map[pushret, ptrs], len, key, 0, 0, rkey, 0, 0);
      end; {pushretnode}


    procedure stacknode(op: operatortype; {root operator}
                        form: types {operand form} );

{ Walk and generate code for a node which pushes a value on the stack.
  Left operand is the link to other parameters, right operand is the
  value to be pushed.
}


      begin {stacknode}
        walknode(l, lkey, 0, true);
        mapkey;
        genpseudo(stacktarget, len, key, refcount, copycount, 0, 0, 0);
        if op in [pushlitvalue, pushfptr] then
          genpseudo(map[op, form], len, key, 0, 0, r, 0, 0)
        else
          begin
          walkvalue(r, rkey, key);
          genpseudo(map[op, form], len, key, 0, 0, rkey, 0, 0);
          end;

        if language <> c then clearkeys;
      end {stacknode} ;


    procedure pushaddrnode(op: operatortype;
                           form: types);

{ Walk and generate code for a node which pushes a value on the stack.
  Left operand is the link to other parameters, right operand is the
  address to be pushed.
}

      var
        rptr: nodeptr; {point to right operand}
        walkfirst: boolean; {set true if we must walk operand before emitting
                             stacktarget}


      begin {pushaddrnode}
        walknode(l, lkey, 0, true);
        if bigcompilerversion then rptr := @(bignodetable[r]);
        walkfirst := (rptr^.op in
                     [copystackop, call, callparam, unscall, unscallparam]) or
                     (form = strings);
        if walkfirst then walkvalue(r, rkey, key);
        mapkey;
        genpseudo(stacktarget, len, key, refcount, copycount, 0, 0, 0);
        if not walkfirst then walkvalue(r, rkey, key);
        genpseudo(map[op, form], len, key, 0, 0, rkey, 0, 0);
        if language <> c then clearkeys;
      end {pushaddrnode} ;


    procedure dummyargnode(op: operatortype;
                           form: types);

{ Walk and generate code for a dummy argument node.  Left operand is the link
  to other dummy argument nodes, right operand is the address of the arg.
}


      begin
        walknode(l, lkey, 0, true);
        walkvalue(r, rkey, 0);
        mapkey;
        genpseudo(map[op, form], len, key, 0, 0, rkey, 0, 0);
        if language <> c then clearkeys;
      end {dummyargnode} ;




    procedure seteltnode;

{ Walk and generate code for a single set element. This is not a
  true triple, but sets the value into a previously defined set target.
  This approach is needed because there are an unknown number of
  operators applied to a given target set.  Note that the same pseudo-
  code operator is used for both single and paired set elements.
}


      begin
        walknode(l, lkey, targetkey, true);
        walkvalue(r, rkey, 0);
        genpseudo(setinsert, len, 0, 0, 0, rkey, 0, targetkey);
        clearkeys;
      end {seteltnode} ;


    procedure setpairnode;

{ Walk and generate code for a set pair element.  This is very similar
  to the previous procedure.
}


      begin
        walknode(l, lkey, targetkey, true);
        walkvalue(r, lkey, 0);
        walkvalue(rootp^.oprnds[3], rkey, 0);
        genpseudo(setinsert, len, 0, 0, 0, lkey, rkey, targetkey);
        clearkeys;
      end {setpairnode} ;


    procedure bldsetnode;

{ This operator defines a set for use by the set insert operators.
  The right node defines the set const, the left has the inserts.
}


      begin
        unnestsets(l);
        walknode(r, rkey, 0, true);
        mapkey;
        if walkdepth > 2 then targetkey := 0;
        genpseudo(doset, len, key, refcount, copycount, rkey, 0, targetkey);
        walknode(l, lkey, key, true);
        clearkeys;
      end {bldsetnode} ;


    procedure wrnode;

{ Walk and generate code for a write operator.  This is a standard unary
  operator except that the operand has a shortvisit first to make sure
  that any common expressions are put on the stack before the write
  operand.
}


      begin {wrnode}
        shortvisit(l, false);
        refcount := refcount - 1;
        unarynode(wr, rootp^.form);
        clearkeys;
      end {wrnode} ;


    procedure rdnode;

{ Walk and generate code for a read operator.  This is similar
  to a write node.
}


      begin
        shortvisit(l, false);
        refcount := refcount - 1;
        if rootp^.form = arrays then unarynode(rd, arrays)
        else
          begin
          walknode(l, lkey, 0, true);
          walknode(r, rkey, 0, true);
          mapkey;
          if bigcompilerversion then rootp := @(bignodetable[root]);
          genpseudo(map[rd, rootp^.form], len, key, refcount, copycount, rkey,
                    0, 0);
          end;
        clearkeys;
      end {rdnode} ;


    procedure reservenode;

{ Walk and generate a reserrve stack node.  The only operand is the
  length of the function result
}


      begin
        mapkey;
        with rootp^ do
          genpseudo(makeroom, oprnds[1], key, 0, 0, oprnds[2], oprnds[3], 0);
      end {reservenode} ;


    procedure tempnode;

{ Walk and generate code for a known register temporary.  This is
  primarily used for specifying the registers for return values.
  The index, which is in oprnds[1], is negated and passed in the
  third operand.  It is interpreted by the code generator in a
  machine-dependent manner.
}


      begin
        mapkey;
        with rootp^ do
          case targetmachine of
            mc68000:
              genpseudo(map[op, form], len, key, refcount, copycount, 0, 0,
                        - oprnds[1]);
            otherwise
              genpseudo(regtemp, len, key, refcount, copycount, 0, 0,
                        - oprnds[1]);
            end;
      end; {tempnode}


    procedure varnode(psop: pseudoop {pseudo op to generate} );

{ Walk and generate code for a variable or unsigned variable.  The
  level access is in operand three, while operands 1 and 2 are the
  level and displacement and are ignored as far as code goes.
}

      var
        threekey: keyindex; {key for level access tree}


      begin
        walknode(rootp^.oprnds[3], threekey, 0, true);
        mapkey;
        genpseudo(psop, len, key, refcount, copycount, threekey, 0, 0);
      end {varnode} ;


    procedure sysfnnode(op: operatortype; {root operator}
                        form: types {result form} );

{ Walk and generate code for a system function.  This is a unary node
  with the argument receiving a shortvisit to get common expressions on
  the stack prior to the argument.  The function index is in operand 1.
  Note that such functions may change type, in which case targeting is
  not applicable.
}

      var
        rp: nodeptr; {used to access right operand}


      begin
        shortvisit(r, false);

        if not (switcheverplus[fpc68881] and (standardids(rootp^.oprnds[1]) in
           [facosid, fasinid, fatanid, fatanhid, fcoshid, fetoxm1id, fgetexpid,
           fgetmanid, fintid, flog10id, flog2id, flognp1id, fmodid, fremid,
           fscaleid, fsgldivid, fsglmulid, fsinhid, ftanid, ftanhid, ftentoxid,
           ftwotoxid, fmovecrid, absid, sqrid, sinid, cosid, expid, lnid,
           sqrtid, arctanid, truncid, roundid, snglid, dblid, acosid, asinid,
           tanid, coshid, tanhid, log10id, fabsid, labsid, sinhid,
           logid])) then
          begin
          if bigcompilerversion then rp := @(bignodetable[r]);
          if form <> rp^.form then targetkey := 0;
          end;

        walkvalue(r, rkey, 0);
        mapkey;
        genpseudo(map[op, form], len, key, refcount, copycount, rkey,
                  rootp^.oprnds[1], targetkey);

      end {sysfnnode} ;


    procedure defforindexnode(notforlit: boolean {intl val is expr} );

{ Walk and generate code for a for index node.  The target field of the
  emitted pseudoop tells the code generator if a memory copy must be kept.

  This node contains the initial value expression (or literal) in operand 3,
  and if the initial value is not a literal, this expression is walked.
}

      var
        initialkey, varkey: keyindex; {initial expr key }
        initialvalue: integer; { first operand for pseudo-op}
        varp: nodeptr; {point to the defined variable}


      begin {defforindexnode}
        with rootp^ do
          begin

          walkvalue(oprnds[2], varkey, 0);

          if notforlit then
            begin
            walknode(oprnds[3], initialkey, 0, false {already decremented} );
            initialvalue := initialkey;
            end
          else initialvalue := oprnds[3];

          mapkey;

          genpseudo(map[op, ints], len, key, refcount, copycount, initialvalue,
                    varkey, ord((oprnds[1] = 0) or nowdebugging or nowwalking));

          end;
      end {defforindexnode} ;


{ Walk a conditional expression node.
  We assign a temp for the value, then walk the two sides, moving
  each to the temp.  The result is the temp.
}


    procedure questnode;

{ The condition is the first operand, while the two expressions are
  the second and third operands.
}

      var
        oldinv: boolean; {local save for "inverted"}
        oldt, oldf: integer; {local saves for "truelabel", "falselabel"}
        oldtused, oldfused: boolean; {local saves for "trueused", "falseused"}
        tlabel, flabel, jlabel: integer; {new labels for converting to value}
        k: nodeindex; {new key for building value}
        tk: integer; {temp for searching list of nodes}
        targ: nodeindex; {temp target}
        cond: nodeindex; {the condition expression}
        p: nodeptr; {for access to left and right nodes}
        form: types; {form for this operation}
        oldclearok: boolean; {old value of oktoclear}
        val_used: boolean; {the value is used}
        next: integer; {for tracing through comma ops}


      begin
        oldclearok := oktoclear;
        oktoclear := false;
        cond := l; {first get the names right}
        l := r;
        r := rootp^.oprnds[3];
        val_used := rootp^.refcount > 0;
        form := rootp^.form;

        if not val_used then
          begin
          increfcount(l, false, - 1);
          increfcount(r, false, - 1);
          end;

        oldinv := inverted;
        oldt := truelabel;
        oldf := falselabel;
        oldtused := trueused;
        oldfused := falseused;
        tlabel := newlabel;
        flabel := newlabel;
        jlabel := newlabel;
        if (targetkey <> 0) and (keytable[targetkey] <> 0) and
           targetpresent(root) then
          targetkey := 0;
        if val_used then
          begin
          k := newkey;
          context[contextsp].high := k;
          keytable[k] := 0;
          genpseudo(createtemp, len, k, 3, 0, ord(form), 0, targetkey);
          end;
        walkboolean(cond, key, tlabel, flabel);
        definelabel(tlabel);
        targ := k;
        if val_used then
          begin
          walkvalue(l, lkey, targ);
          tk := newkey;
          context[contextsp].high := tk;
          keytable[tk] := 0;
          genpseudo(map[moveop, form], len, tk, 0, 0, k, lkey, k);
          end
        else walknode(l, lkey, 0, false);
        genpseudo(jump, 0, 0, 0, 0, jlabel, 0, 0);
        definelabel(flabel);
        targ := k;
        if val_used then
          begin
          walkvalue(r, rkey, targ);
          tk := newkey;
          context[contextsp].high := tk;
          keytable[tk] := 0;
          genpseudo(map[moveop, form], len, tk, 0, 0, k, rkey, k);
          end
        else walknode(r, rkey, 0, false);
        definelabel(jlabel);
        if val_used then
          begin
          mapkey;
          genpseudo(jointemp, len, key, refcount, copycount, k, ord(form), 0);
          end;
        inverted := oldinv;
        truelabel := oldt;
        falselabel := oldf;
        trueused := oldtused;
        falseused := oldfused;
        oktoclear := oldclearok;
      end; {questnode}


{ Short Circuit Boolean Evaluation
  The following three routines handle short circuit evaluation of
  boolean expressions.  This takes advantage of the fact that it is
  frequently possible to terminate evaluation of a boolean expression
  as soon as the value of one of the operands is known.  For instance,
  if the first operand of an "and" operation is false, the entire expression
  is false and there is no need to evaluate the second half.

  This has the added advantage in a machine with condition codes that
  the condition code need not be converted to a boolean value, since the
  condition code is normally checked directly by conditional jumps.

  Short circuit evaluation is implemented using three global variables,
  "truelabel", which is the label to use if the expression is true,
  "falselabel", similar for the expression false, and "inverted", which
  indicates that a "not" has been walked and the condition is inverted.
  Since boolean expressions can be nested, these global variables are
  frequently saved and restored by the tree walking routines.
}


    procedure shortand;

{ Walk and generate code for a boolean "and", using shortcircuit
  evaluation.  In this case, control passes to the falselabel if
  the left operand is false, and to the code for the right operand
  if it is true.  Falselabel remains constant for the evaluation of
  the and, while truelabel is changed for the left operand.
}

      var
        nextlabel: labelrange; {label for right operand eval}
        t: labelrange; {local storage for truelabel}
        oldinv: boolean; {local storage for inverted}
        oldused: boolean; {local value for "trueused"}


      begin
        nextlabel := newlabel;
        t := truelabel;
        truelabel := nextlabel;
        oldused := trueused;
        trueused := false;
        oldinv := inverted;
        if language = pascal then shortvisit(r, false);
        walknode(l, lkey, 0, true);
        if inverted then genpseudo(jumpt, 0, 0, 0, 0, falselabel, lkey, 0)
        else genpseudo(jumpf, 0, 0, 0, 0, falselabel, lkey, 0);
        inverted := oldinv;
        truelabel := t;
        if trueused then definelabel(nextlabel);
        falseused := true;
        trueused := oldused;
        walknode(r, key, 0, true);
      end {shortand} ;


    procedure shortor;

{ Walk and generate code for a boolean "or" using shortcircuit
  evaluation.  In this case, control passes to the truelabel if the
  left operand is true, and to the evaluation of the right operand
  if it is false.  Truelabel remains constant for the evaluation of
  the or, while falselabel is changed for the left operand.
}

      var
        nextlabel: labelrange; {label for right operand eval}
        f: labelrange; {local storage for falselabel}
        oldinv: boolean; {local storage for inverted}
        oldused: boolean; {local value for falseused}


      begin
        nextlabel := newlabel;
        f := falselabel;
        falselabel := nextlabel;
        oldused := falseused;
        falseused := false;
        oldinv := inverted;
        if language = pascal then shortvisit(r, false);
        walknode(l, lkey, 0, true);
        if inverted then genpseudo(jumpf, 0, 0, 0, 0, truelabel, lkey, 0)
        else genpseudo(jumpt, 0, 0, 0, 0, truelabel, lkey, 0);
        inverted := oldinv;
        if falseused then definelabel(nextlabel);
        falseused := oldused;
        trueused := true;
        falselabel := f;
        walknode(r, key, 0, true);
      end {shortor} ;


    procedure shortnot;

{ Walk and generate code for a boolean "not", using shortcircuit
  evaluation.  This simply reverses "truelabel" and "falselabel"
  while the operand is being evaluated, then complements "inverted"
  to invert the final condition.
}

      var
        t: labelrange; {temp for inverting labels}
        b: boolean; {temp for inverting used variables}


      begin
        t := truelabel;
        truelabel := falselabel;
        falselabel := t;
        b := trueused;
        trueused := falseused;
        falseused := b;
        walknode(l, key, 0, true);
        t := truelabel;
        truelabel := falselabel;
        falselabel := t;
        b := trueused;
        trueused := falseused;
        falseused := b;
        inverted := not inverted;
      end {shortnot} ;



    begin {visitnode}
      walkdepth := walkdepth + 1;
      lkey := 0;
      rkey := 0;
      key := 0;
      if bigcompilerversion then rootp := @(bignodetable[root]);
      with rootp^ do
        begin
        l := oprnds[1];
        r := oprnds[2];
        case op of
          indxchkop, cindxchkop, rangechkop, forupchkop, fordnchkop,
          forerrchkop, congruchkop:
            checknode(op);
          call: callnode(op);
          callparam: callparamnode(op);
          unscall: callnode(op);
          unscallparam: callparamnode(op);
          reserve: reservenode;
          pushaddr, pushstraddr: pushaddrnode(op, form);
          bldfmt: stacknode(op, none);
          pushproc: pushprocnode;
          pushfinal: pushfinalnode;
          pushvalue, pushlitvalue, pushfptr: stacknode(op, form);
          pushcvalue: pushcvaluenode(form);
          pushret: pushretnode;
          sysfn: sysfnnode(op, form);
          rd: rdnode;
          wr: wrnode;
          fptrop, ptrop, intop, structop, jumpvfuncop: literalnode(op);
          dummyargop: dummyargnode(op, form);
          realop, doubleop:
            case language of
              c: crealnode;
              otherwise realnode;
              end;
          ownop: ownnode;
          extop: extnode;
          originop: originsegnode(doorigin);
          segop: originsegnode(doseg);
          levop: levnode;
          varop: varnode(dovar);
          unsvarop: varnode(dounsvar);
          groupop: groupnode;
          tempop: tempnode;
          moveop, cmoveop, returnop: movenode(op, form);
          andop:
            if relation then shortand
            else binarynode(op, form);
          orop:
            if relation then shortor
            else binarynode(op, form);
          notop:
            if relation then shortnot
            else unarynode(op, form);
          modop, kwoop, stdmodop, {new and undefined for other than ints}
          divop, stddivop:
            if form = ints then divintnode(op, form)
            else binarynode(op, form);
          mulop:
            if form = ints then mulintnode(form)
            else binarynode(mulop, form);
          aindxop, paindxop, inop: orderednode(op, form);
          plusop, minusop, slashop, shiftlop, eqop, neqop, lssop, gtrop, leqop,
          geqop, xorop, shiftrop, dummyarg2op:
            binarynode(op, form);
          openarrayop: openarraynode;
          float, float1, chrstrop, chrstrop1, arraystrop, arraystrop1,
          float_double, real_to_dbl, dbl_to_real, castfptrop, castintop,
          castptrop, castrealop:
            begin
            if targetopsys <> vms then targetkey := 0;
            castnode(op, form);
            end;
          indrop, filebufindrop: unaryaddrnode(op);
          movelit: movelitnode(form);
          eqlit, neqlit, lsslit, gtrlit, leqlit, geqlit, loopholeop:
            litopnode(op, form);
          incop, decop, addrop, negop, remop, quoop, ptrchkop, definelazyop,
          compop:
            unarynode(op, form);
          setbinfileop: setbinfilenode;
          setfileop, closerangeop: unmappedunarynode(op, form);
          copystackop: copystacknode;
          defforindexop, defunsforindexop: defforindexnode(true);
          defforlitindexop, defunsforlitindexop: defforindexnode(false);
          indxop: indxnode(indx);
          pindxop: indxnode(pindx);
          vindxop, parmop: vindxnode;
          setelt: seteltnode;
          setpair: setpairnode;
          bldset: bldsetnode;
          addeqop, andeqop, muleqop, oreqop, shiftleqop, shiftreqop, subeqop,
          xoreqop:
            reflexivenode(op, form);
          preincop, postincop: incnode(op, form, rootp^.oprnds[3]);
          diveqop, modeqop: reflexdivnodes(op, form);
          commaop: commanode;
          questop: questnode;
          otherwise
            begin
            write('travrs walk error ', ord(op): 3);
            compilerabort(walkerror);
            end;
          end;
        end;
      walkdepth := walkdepth - 1;
    end {visitnode} ;


  procedure copynode;

{ Walk a copy node and generate code to bring this temp into the current
  context.
}

    var
      oldkey: keyindex; {key for common expression}
      rootp: nodeptr; {used to access root node}


    begin
      if bigcompilerversion then rootp := @(bignodetable[root]);
      { hoisting may have removed all the references. check it out }
      if rootp^.refcount <> 0 then
        begin
        walknode(rootp^.oldlink, oldkey, 0, true);
        mapkey;
        if not nowdebugging and (key >= context[contextsp].low) then
          context[contextsp].low := key + 1;
          { there is a kludge here:  we tell the code generator if copy
            was hoisted out of the loop, so that copyaccessx can take it
            into account }
        genpseudo(copyaccess, ord(currentblock^.loophdr), key, refcount,
                  copycount, oldkey, 0, 0);
        end;
    end {copynode} ;


  begin {walknode}
    key := 0;
    while root <> 0 do
      begin
      if bigcompilerversion then rootp := @(bignodetable[root]);
      refcount := rootp^.refcount;
      copycount := rootp^.copycount;
      len := rootp^.len;
      if rootp^.action in [visit, revisit] then
        if rootp^.op < intop then nextroot := rootp^.slink
        else nextroot := 0
      else nextroot := 0;
      with rootp^ do
        case action of
          visit:
            begin
            action := revisit;
            visitnode
            end;
          copy:
            begin
            action := recopy;
            copynode
            end;
          revisit, recopy: mapkey;
          end;
      if counting then
        begin
        if bigcompilerversion then rootp := @(bignodetable[root]);
        with rootp^ do refcount := refcount - 1;
        end;
      root := nextroot;
      end;
  end {walknode} ;


procedure walkboolean(root: nodeindex; {root of tree to walk}
                      var key: keyindex; {resulting key}
                      tlabel: labelrange; {lab for true result}
                      flabel: labelrange {lab if false result} );

{ Do a shortcircuit evaluation of a boolean expression using "tlabel"
  and "flabel" as truelabel and falselabel.
}


  begin
    inverted := false;
    trueused := false;
    falseused := false;
    truelabel := tlabel;
    falselabel := flabel;
    if language = pascal then
      begin
      shortvisit(root, false);
      genpseudo(saveactkeys, 0, 0, 0, 0, 0, 0, 0);
      end;
    walknode(root, key, 0, true);
    if inverted then genpseudo(jumpt, 0, 0, 0, 0, flabel, key, 0)
    else genpseudo(jumpf, 0, 0, 0, 0, flabel, key, 0);
    falseused := true;
  end {walkboolean} ;


procedure walkvalue(root: nodeindex; {root of tree to walk}
                    var key: keyindex; {resulting key}
                    targetkey: keyindex {target (0 if none)} );

{ Walk an expression, returning a value.  If the result is a boolean
  relation, the shortcircuit boolean evaluation is converted into a
  value on the stack.
}

  var
    rootp: nodeptr; {used to access root node}


  procedure convertrelation;

{ Convert relational expression to a value.  Note: booleans are
  assumed to be one unit long here.
}

    var
      oldinv: boolean; {local save for "inverted"}
      oldt, oldf: labelrange; {local saves for "truelabel", "falselabel"}
      oldtused, oldfused: boolean; {local saves for "trueused", "falseused"}
      tlabel, flabel: labelrange; {new labels for converting to value}
      k, k1: keyindex; {new key for false portion of value}
      oldclearok: boolean; {old value of oktoclear}
      valsize: addressrange; {the size of the value, different for diff. lang}


    begin {convertrelation}
      oldclearok := oktoclear;
      oktoclear := false;
      oldinv := inverted;
      oldt := truelabel;
      oldf := falselabel;
      oldtused := trueused;
      oldfused := falseused;
      if language = c then valsize := defaulttargetintsize
      else valsize := unitsize;
      tlabel := newlabel;
      flabel := newlabel;
      if (targetkey <> 0) and targetpresent(root) then targetkey := 0;
      k := newkey;
      context[contextsp].high := k;
      keytable[k] := 0;
      genpseudo(createfalse, valsize, k, 1, 0, 0, 0, targetkey);
      walkboolean(root, k1, tlabel, flabel);
      if trueused then definelabel(tlabel);
      key := newkey;
      context[contextsp].high := key;
      keytable[key] := root;
      if keytable[k1] = root then keytable[k1] := 0; { use value, not relation,
                                                       in future }
      genpseudo(createtrue, valsize, key, 1, 0, k, 0, 0);
      definelabel(flabel);
      inverted := oldinv;
      truelabel := oldt;
      falselabel := oldf;
      trueused := oldtused;
      falseused := oldfused;
      oktoclear := oldclearok;
    end {convertrelation} ;


  begin {walkvalue}
    if bigcompilerversion then rootp := @(bignodetable[root]);
    if rootp^.relation then convertrelation
    else walknode(root, key, targetkey, true);
  end {walkvalue} ;


procedure walknodelist(p: nodeindex {start of node chain} );

{ Walk each node in a chain linked by "prelink".  This is used to walk the
  "preconditions" of a basic block, or for initially writing the contents
  of context[1], which contains common and constant nodes.
}

  var
    p1: nodeindex; {used to store prelink during node walking}
    ptr: nodeptr; {used to access a node}
    key: keyindex; {dummy parameter to walknode}


  begin
    while p <> 0 do
      begin
      if bigcompilerversion then ptr := @(bignodetable[p]);
      p1 := ptr^.prelink;
      walknode(p, key, 0, false);
      p := p1;
      end;
  end {walknodelist} ;



{ Statement walking routines
  These routines walk the statement tree, inserting labels for control flow
  and walking expressions as needed.

  As the statements are walked, they are disposed of, since they don't
  contribute any common expressions, and need not be kept.
}


procedure walkstmtlist(firststmt: nodeindex {start of statement list} );

{ Walk each statement in a list using "walkstmt", and dispose of the
  statement header afterwards.  All of the real work is done in "walkstmt".
}

  var
    p: nodeptr; { for access to statement node }
    newcontrolstmt: boolean; { used to prevent extra readaccess call }
    s: nodeindex; { local copy of firststmt }


  procedure walkstmt(stmt: nodeindex {statement to walk} );

{ Walk a single statement.  The actual walking is done by statement-specific
  routines which are local to this routine.  The statement routines may
  recursively call "walkstmtlist" to walk nested constructs.
  Fortunately the fields in this statement will not change at this point
  so one call to readaccess, and an assignment to currentstmt covers a
  multitude of readaccess calls!
}

    var
      expr1key, expr2key: keyindex; {keys for expressions in headers}
      currentstmt: node; { current stmt node }
      p: nodeptr;


    procedure genstmtbrk;

{ Generate a statement break for the debugger.  This pseudocode informs
  the debugger about a change in statements.
}


      begin
        if (targetopsys = vms) and (currentstmt.textline < 0) and
           nowdebugging then
          exit_stmtno := currentstmt.stmtno
        else if currentstmt.stmtno <> 0 then
          if currentstmt.stmtno <> 0 then
            genpseudo(stmtbrk, currentstmt.srcfileindx, 0, 0, 0,
                      currentstmt.stmtno, abs(currentstmt.textline),
                      ord(controlstmt) + ord(branchstmt) * 2 +
                      ord(targetstmt) * 4);
        controlstmt := false;
        branchstmt := false;
        targetstmt := false;
      end {genstmtbrk} ;


    procedure walkuntil;

{ Walk a until statement.  This has a label at the top for repetition,
  and one following the boolean expression for termination.  The context
  is cleared at the start of the loop, since we do not scan the
  loop looking for still valid expressions.  If the boolean expression is
  a constant true, it is replaced with a jump, if a constant false
  it is simply eliminated.
}


      begin
        { temporarily don't put this out old p2 didn't }
          {
          genstmtbrk;
          }
        with currentstmt do
          begin
          { kludge for constant repeats }
          if expr1 = 0 then
            genpseudo(jump, 0, 0, 0, 0, getlabel(falseblock), level, 0)
          else
            begin
              { looplabel is below any hoisting in the loop which is where
                we want to go if present. }
            if falseblock^.looplabel = 0 then
              walkboolean(expr1, expr1key, getlabel(trueblock),
                          getlabel(falseblock))
            else
              walkboolean(expr1, expr1key, getlabel(trueblock),
                          falseblock^.looplabel);
            end;
          { safe to check for loop restores now }
          if falseblock^.clearop then
            genpseudo(restoreloop, 0, 0, 0, 0, 0, 0, 0);
          if has_break then genpseudo(pascallabel, 0, 0, 0, 0, 0, 0, 0);
          end;
      end {walkuntil} ;


    procedure walkwhileif;

{ Walk a while/if statement.
}


      begin
        genstmtbrk;
        with currentstmt do
          walkboolean(expr1, expr1key, getlabel(trueblock),
                      getlabel(falseblock));
      end {walkwhile} ;


    procedure walkcase;

{ Walk a case statement.  This evaluates the selector expression,
  then generates a casebranch followed by an entry for each
  case label, in numeric order.  This is simply generated by tracing
  the orderedlink of the labels.  Finally each statement in the case
  is generated, proceeded by a label and followed by a jump to the exit
  label.  If the selector is constant, only the specified case is
  actually generated.  Labels and label headers are disposed of as
  they are used.

  This is slightly complicated when the case expression is a relation,
  rather than value-bearing expression.  Rather than trying to generate
  hot-shit code, as only test programs have such constructs which are
  entirely equivalent to an "if" statement, we just force the inefficient
  jump-table path.
}

      var
        p, p1: nodeindex; {used to trace and dispose of lists}
        default: labelrange; {default label or 1 if is an error}
        errordefault: boolean; {true if default gives an error}
        casekey: keyindex; {key for case expression}
        ptr1, ptr2: nodeptr; { to access nodes }
        currentlabel: labelrange; { current case label for stmt block }
        reskey: keyindex; {result of compare for split cases}
        firstgrouprec: nodeindex; {first split group for the case}
        n: nodeptr; {for access to expression}
        relationcase: boolean; {case expression is a relation}


      procedure genjumptable(firstlab, lastlab: nodeindex; {lab limits}
                             ordered: boolean {this is a contig ordered group}
                             );

{ Generate code for a case jump table.  If there is only one label
  for the table, a direct test for equality plus a jump is used instead
  of the more complicated case.
}

        var
          fp: nodeptr; {for access to firstlab}
          lp: nodeptr; {for access to lastlab}
          p, p1: nodeptr; {for tracing lists}
          locdefault: labelrange; {label for a local default}
          done: boolean; {loop termination condition}
          thislab: nodeindex; {for tracing the chain of labels}


        begin {genjumptable}
          if bigcompilerversion then fp := @(bignodetable[firstlab]);
          if bigcompilerversion then lp := @(bignodetable[lastlab]);

          if not relationcase and ((fp^.caselabellow = lp^.caselabelhigh) or
             (fp^.caselabellow = fp^.caselabelhigh) and
             (lp^.caselabellow = lp^.caselabelhigh) and
             (fp^.orderedlink = lastlab)) then
            begin
            genpseudo(eqlitint, targetintsize, reskey, 1, 0, casekey,
                      fp^.caselabellow, 0);

            genpseudo(jumpt, 0, 0, 0, 0, fp^.stmtlabel, reskey, 0);
            if fp^.caselabellow <> lp^.caselabelhigh then
              begin
              genpseudo(eqlitint, targetintsize, reskey, 1, 0, casekey,
                        lp^.caselabelhigh, 0);
              genpseudo(jumpt, 0, 0, 0, 0, lp^.stmtlabel, reskey, 0);
              end;
            if errordefault then genpseudo(caseerr, 0, 0, 0, 0, 0, 0, 0)
            else genpseudo(jump, 0, 0, 0, 0, default, 0, 0);
            end
          else if ordered then
            begin
            locdefault := newlabel;
            genpseudo(lsslitint, targetintsize, reskey, 1, 0, casekey,
                      fp^.caselabellow, 0);
            genpseudo(jumpt, 0, 0, 0, 0, locdefault, reskey, 0);
            genpseudo(leqlitint, targetintsize, reskey, 1, 0, casekey,
                      lp^.caselabelhigh, 0);
            genpseudo(jumpt, 0, 0, 0, 0, lp^.stmtlabel, reskey, 0);
            definelabel(locdefault);
            if errordefault then genpseudo(caseerr, 0, 0, 0, 0, 0, 0, 0)
            else genpseudo(jump, 0, 0, 0, 0, default, 0, 0);
            end
          else
            begin
            if errordefault then locdefault := newlabel
            else locdefault := default;
            genpseudo(casebranch, locdefault, 0, ord(errordefault), 0,
                      fp^.caselabellow, lp^.caselabelhigh, casekey);
            thislab := firstlab;
            p := fp;
            repeat
              if bigcompilerversion then
                p1 := @(bignodetable[p^.orderedlink]);
              genpseudo(caseelt, 0, 0, 0, 0, p^.stmtlabel,
                        p^.caselabelhigh - p^.caselabellow + 1, 0);
              done := thislab = lastlab;
              if not done then
                if p1^.caselabellow - p^.caselabelhigh > 1 then
                  genpseudo(caseelt, 0, 0, 0, 0, locdefault,
                            p1^.caselabellow - p^.caselabelhigh - 1, 0);
              thislab := p^.orderedlink;
              p := p1;
            until done;
            end;
        end; {genjumptable}


      procedure casesplit(first, last: integer {index of split groups} );

{ Generate code for a single split portion of a case statement.  If
  the test is non-terminal, the routine is invoked recursively to
  generate code for the two branches of the tree.  The root of the
  subtree is always taken as the middle of the specified elements.

  Note that numbered elements are located by a linear scan of the
  list on the assumption that most such lists will be short.
}

        var
          middle: integer; {middle of the tree}
          thisgroup: nodeindex; {middle group}
          lastlabel: labelrange; {label associated with "last" direction branch}
          firstlabel: labelrange; {label associated with "first" direction
                                   branch}
          p, lp, hp: nodeptr; {for access to nodes}


        begin
          middle := (first + last + 1) div 2;
          thisgroup := firstgrouprec;
          if bigcompilerversion then p := @(bignodetable[thisgroup]);
          while p^.groupno <> middle do
            begin
            thisgroup := p^.nextstmt;
            if bigcompilerversion then p := @(bignodetable[thisgroup]);
            end;

          if middle < last then
            begin {there is a right-hand branch}
            if bigcompilerversion then hp := @(bignodetable[p^.highestlabel]);
            if bigcompilerversion then hp := @(bignodetable[hp^.orderedlink]);
            lastlabel := newlabel;
            genpseudo(geqlitint, targetintsize, reskey, 1, 0, casekey,
                      hp^.caselabelhigh, 0);
            genpseudo(jumpt, 0, 0, 0, 0, lastlabel, reskey, 0);
            end;
          if middle > first then
            begin {there is a left-hand branch}
            if bigcompilerversion then lp := @(bignodetable[p^.lowestlabel]);
            firstlabel := newlabel;
            genpseudo(lsslitint, targetintsize, reskey, 1, 0, casekey,
                      lp^.caselabellow, 0);
            genpseudo(jumpt, 0, 0, 0, 0, firstlabel, reskey, 0);
            end;
          with p^ do genjumptable(lowestlabel, highestlabel, ordered);
          if middle < last then
            begin
            definelabel(lastlabel);
            casesplit(middle + 1, last);
            end;
          if middle > first then
            begin
            definelabel(firstlabel);
            casesplit(first, middle - 1);
            end;
        end; {casesplit}


      begin {walkcase}
        genstmtbrk;
        with currentstmt do
          begin
          errordefault := (language <> c) and (casedefptr = nil) and
                          (switchcounters[rangecheck] > 0);

          { always need a default label }
          if errordefault then default := newlabel
          else if casedefptr = nil then default := getlabel(joinblock)
          else default := getlabel(casedefptr);

          if errordefault and (elements = 0) then
            genpseudo(caseerr, 0, 0, 0, 0, 0, 0, 0)
          else if elements <> 0 then
            begin
            if bigcompilerversion then n := @(bignodetable[selector]);
            relationcase := n^.relation;
            walkvalue(selector, casekey, 0);
            reskey := newkey;
            firstgrouprec := firstgroup;
            casesplit(1, groupcount);
            end
          else if language = C then
            begin
            walkvalue(selector, casekey, 0);
            genpseudo(dovar, 4, newkey, 0, 0, casekey, 0, 0); {this sink
stinks}
            end;
          end;
      end {walkcase} ;


    procedure walkfortop;

{ Walk a for statement.  This is complicated by the
  desire to generate good code for the case when we know that the loop will
  be executed at least once.  In this case, we can put the test at the
  bottom only and avoid a jump.

  This optimization can be done if both limits are constant, and the
  first is less than the second.

  Note too, that the final value is pushed on the stack, so has a shortvisit
  to save common expressions before this.  Also, the for expressions are
  kept around until the end of the loop by walking the nodes with "counting"
  false.  They are then walked with "counting" true at the end of the
  loop.  This generates no code, just fixes reference counts.

  Note that the index variable is defined after the final value.  This
  is a kludge which produced better code on most stack machines.
  Unfortunately, it complicates the generation of for range checks.
}

      var
        ptr: nodeptr; {used to access index node}
        constinitial, constfinal: boolean; {initial and final const}
        unsigned: boolean; {true if unsigned controlled var}
        initialvalue, finalvalue: integer; {initial and final values}
        checkexists: boolean; {for check is found}
        actualexpr1: nodeindex; {final value minus check (if any)}


      begin {walkfortop}
        genstmtbrk;
        constinitial := false;
        labelnext := false; { genblk labels controlled blk for free }
        with currentstmt do
          begin

          if bigcompilerversion then ptr := @(bignodetable[expr1]);
          if ptr^.op in [forupchkop, fordnchkop, forerrchkop] then
            begin
            checkexists := true;
            actualexpr1 := ptr^.oprnds[1];
            end
          else
            begin
            checkexists := false;
            actualexpr1 := expr1;
            end;

          shortvisit(actualexpr1, false);
          checkconst(actualexpr1, constfinal, finalvalue);
          if bigcompilerversion then ptr := @(bignodetable[expr2]);
          if (ptr^.op = defforlitindexop) or
             (ptr^.op = defunsforlitindexop) then
            begin
            constinitial := true;
            initialvalue := ptr^.oprnds[3];
            unsigned := ptr^.op = defunsforlitindexop;
            end
          else walkvalue(ptr^.oprnds[3], expr2key, 0);
          walknode(actualexpr1, expr1key, 0, false);
          if expr1key > expr2key then clearkeys; {reclaim what we can}
          walknode(expr2, expr2key, 0, false);
          if checkexists then walknode(expr1, expr1key, 0, false);

          forsp := forsp + 1;
          with forstack[forsp] do
            begin
            forkey1 := expr1key;
            forkey2 := expr2key;
            if currentstmt.stmtkind = foruphdr then
              begin
              { why does this care about debugging?? }
              if constfinal and constinitial and not nowdebugging and
                 ((initialvalue <= finalvalue) and not (unsigned and
                 (initialvalue < 0) and (finalvalue >= 0)) or unsigned and
                 (initialvalue >= 0) and (finalvalue < 0)) then
                begin {no chance of a zero trip loop}
                genpseudo(foruptop, forstepsize, 0, 0, 0, exitlabel(trueblock),
                          0, 0);
                improved := true;
                downloop := false;
                end
              else
                begin
                genpseudo(foruptop, forstepsize, 0, 0, 0, exitlabel(trueblock),
                          exitlabel(falseblock), expr1key);
                improved := false;
                downloop := false;
                end;
              end
            else
              begin
              if constfinal and constinitial and not nowdebugging and
                 ((initialvalue >= finalvalue) and not (unsigned and
                 (initialvalue >= 0) and (finalvalue < 0)) or unsigned and
                 (initialvalue < 0) and (finalvalue >= 0)) then
                begin {no chance of a zero trip loop}
                genpseudo(fordntop, forstepsize, 0, 0, 0, exitlabel(trueblock),
                          0, 0);
                improved := true;
                downloop := true;
                end
              else
                begin
                genpseudo(fordntop, forstepsize, 0, 0, 0, exitlabel(trueblock),
                          exitlabel(falseblock), expr1key);
                improved := false;
                downloop := true;
                end;
              end; { down loop }
            end; {with forstack}
          end; {with currentstmt}
      end {walkfortop} ;


    procedure walkforbot;
      { produce the code for the bottom of a forloop.
        Most of the work was done at the top of the loop, this is just
        clean up.
      }

      var
        ptr: nodeptr; { for access to for loop top }


      begin { walkforbot }
        if bigcompilerversion then
          ptr := @(bignodetable[currentstmt.looptop^.beginstmt]);
        with ptr^, forstack[forsp] do
          begin
          if improved then
            begin
            if downloop then
              genpseudo(fordnimproved, forstepsize, 0, 0, 0,
                        exitlabel(trueblock), exitlabel(falseblock), forkey1)
            else
              genpseudo(forupimproved, forstepsize, 0, 0, 0,
                        exitlabel(trueblock), exitlabel(falseblock), forkey1);
            end
          else
            begin
            if downloop then
              genpseudo(fordnbottom, forstepsize, 0, 0, 0, exitlabel(trueblock),
                        exitlabel(falseblock), forkey1)
            else
              genpseudo(forupbottom, forstepsize, 0, 0, 0, exitlabel(trueblock),
                        exitlabel(falseblock), forkey1);
            end;
            { this is a kludge to prevent the exit block from being labelled
              twice, once by forbottom in genblk and once by normal block
              labeling here.
            }
          falseblock^.blocklabel := 0;
          walknode(expr1, forkey1, 0, true);
          walknode(expr2, forkey2, 0, true);
          clearkeys;
          end; {with}
        forsp := forsp - 1;
      end {walkforbot} ;


    procedure walkcforbot;

{ Walk the bottom of a C for statement.  This has a single expression
  then a jump to the top of the loop
}

      var
        dummykey: keyindex;


      begin
        with currentstmt do
          begin
          walknode(expr1, dummykey, 0, false);
          clearkeys;
          if trueblock^.clearop then
            genpseudo(restoreloop, 0, 0, 0, 0, 0, 0, 0);
          if trueblock^.looplabel = 0 then
            genpseudo(jump, 0, 0, 0, 0, getlabel(trueblock), level, 0)
          else genpseudo(jump, 0, 0, 0, 0, trueblock^.looplabel, level, 0);
          if has_break then genpseudo(pascallabel, 0, 0, 0, 0, 0, 0, 0);
          end;
      end; {walkcforbot}


    procedure walkwith;

{ Walk a with statement.  This is very simple since the action is all taken
  care of in the expressions.
}


      begin
        genstmtbrk;
        with currentstmt do
          begin
          walknode(expr1, expr1key, 0, false);
          end;
      end {walkwith} ;


    procedure walksimple;

{ Walk a simple statement (assignment or procedure call).  There is
  nothing to it.
}


      begin
        genstmtbrk;
        walknode(currentstmt.expr1, expr1key, 0, false);
        clearkeys;
      end {walksimple} ;


    procedure walklabel;

{ Generate code for a pascal label
}


      begin
        { Labels aren't actually stmts.  At least old travrs didn't think so }
        { genstmtbrk; }
        genpseudo(pascallabel, 0, 0, 0, 0, currentstmt.labelno,
                  ord(currentstmt.nonlocalref), 0);
        targetstmt := targetstmt or currentstmt.nonlocalref;
        if currentstmt.nonlocalref then anynonlocalgotos := true;
      end {walklabel} ;


    procedure walkgoto;

{ Generate code for a goto statement.
}

      var
        ptr: nodeptr;
        p: proctableindex;


      begin {walkgoto}

        if bigcompilerversion then ptr := @(bignodetable[root^.beginstmt]);
        p := ptr^.procref;
        if newtravrsinterface then
          branchstmt := currentstmt.labellevel <>
                        new_proctable[p div (pts + 1)]^[p mod (pts + 1)].level
        else branchstmt := currentstmt.labellevel <> proctable[p].level;
        genstmtbrk;
        with currentstmt do
          genpseudo(pascalgoto, 0, 0, 0, 0, labelno, labellevel, 0);
      end {walkgoto} ;


    procedure walksyscall;

{ Walk and generate code for a system call.  This is pretty standard, it
  does a shortvisit on the args to generate common subexpressions,
  then walks the args and generates a "sysroutine".  "new" is slightly
  different because it has a length argument along with it.
}

      var
        ptr: nodeptr; {used to access expr node for new call}
        targetflag: 0..2; {flag to pass the type of number used}


      begin
        genstmtbrk;
        with currentstmt do
          begin
          shortvisit(expr1, false);
          targetflag := 0;
          if (expr2 = ord(valprocid)) or (expr2 = ord(strid)) then
            begin
            if bigcompilerversion then ptr := @(bignodetable[expr1]);
            if expr2 = ord(valprocid) then
              if bigcompilerversion then ptr := @(bignodetable[ptr^.slink]);
            if ptr^.form = reals then targetflag := 1
            else if ptr^.form = doubles then targetflag := 2;
            end;
          walknode(expr1, expr1key, 0, true);
          if expr2 = ord(getid) then
            genpseudo(sysroutine, 0, 0, 0, 0, expr2, expr1key, 0)
          else genpseudo(sysroutine, 0, 0, 0, 0, expr2, 0, targetflag);
          end;
        clearkeys;
      end {walksyscall} ;


    procedure walkblk;

{ Walk a block.  First all constant expressions in context[1] are
  generated, then code for the block.
}

      var
        i: hashindex; {induction var for generating context[1]}


      begin {walkblk}
        for i := 0 to nodehashsize do walknodelist(context[1].opmap[i]);

        genpseudo(blockcode, currentstmt.fileline, 0, 0, 0, regtemps, ptrtemps,
                  realtemps);
        genstmtbrk;
        contextsp := 2;
        overflowdepth := 0;
        with context[2] do
          begin
          high := context[1].high;
          low := high + 1;
          end;
      end {walkblk} ;


    procedure walkbrkcont(needstmtbrk: boolean);

{ Walk a break or continue statement.  This just generates a transfer to
  the proper block;
}


      begin
        if needstmtbrk then genstmtbrk;
        genpseudo(jump, 0, 0, 0, 0, getlabel(currentstmt.targblock), level, 0);
      end; {walkbrkcont}


    procedure walkreturn;

{ Walk a return statement.  Just walk the statement and transfer control
  to the exit block (stored in "trueblock");
}

      var
        dumkey: keyindex; {for fake walk}


      begin
        genstmtbrk;
        with currentstmt do
          begin
          walknode(expr1, dumkey, 0, false);
          clearkeys;
          genpseudo(jump, 0, 0, 0, 0, getlabel(trueblock), 0, 0);
          end;
      end; {walkreturn}


    begin {walkstmt}

      inverted := false;
      if bigcompilerversion then p := @(bignodetable[stmt]);
      currentstmt := p^;
      with currentstmt do
        begin
        case stmtkind of
          blkhdr: walkblk;
          rpthdr, loophdr: genstmtbrk;
          untilhdr: walkuntil;
          whilehdr, ifhdr: walkwhileif;

          whilebothdr, loopbothdr:
            begin
            if currentstmt.looptop^.clearop then
              genpseudo(restoreloop, 0, 0, 0, 0, 0, 0, 0);
              { looplabel is below any hoisting in the loop which is where
                we want to go if present }
            if currentstmt.looptop^.looplabel = 0 then
              genpseudo(jump, 0, 0, 0, 0, getlabel(currentstmt.looptop), level,
                        0)
            else
              genpseudo(jump, 0, 0, 0, 0, currentstmt.looptop^.looplabel, level,
                        0);
            if has_break then genpseudo(pascallabel, 0, 0, 0, 0, 0, 0, 0);
            end;

          casehdr: walkcase;
          foruphdr, fordnhdr: walkfortop;
          forbothdr: walkforbot;
          withhdr: walkwith;
          simplehdr: walksimple;
          labelhdr: walklabel;
          gotohdr: walkgoto;
          loopbrkhdr: walkbrkcont(true);
          swbrkhdr: walkbrkcont(false);
          cswbrkhdr: walkbrkcont(true);
          loopconthdr: walkbrkcont(true);
          cforhdr: walkwhileif;
          cforbothdr: walkcforbot;
          returnhdr: walkreturn;
          syscallhdr: walksyscall;
          caseerrhdr: genpseudo(caseerr, 0, 0, 0, 0, 0, 0, 0);
          nohdr: { dead statement } ;
          otherwise writeln('ouch!!', ord(stmtkind))
          end;
        end;
    end {walkstmt} ;


  begin {walkstmtlist}
    controlstmt := true;
    branchstmt := false;
    targetstmt := false;
    while firststmt <> 0 do
      begin
      s := firststmt;
      if bigcompilerversion then p := @(bignodetable[firststmt]);
      firststmt := p^.nextstmt;
      if p^.stmtkind in [rpthdr, whilehdr, foruphdr, fordnhdr, cforhdr] then
        controlstmt := true;
      { do this now to avoid readaccess call }
      newcontrolstmt := not (p^.stmtkind in [withhdr, simplehdr, syscallhdr]);
      walkstmt(s);
      controlstmt := newcontrolstmt;
      end;
  end {walkstmtlist} ;


procedure walk;

{ Walk the tree for a block.  This sets up global variables, emits
  block entry code, and calls "walkstmtlist" to actually walk the block
  code.
}

  var
    blocksize: addressrange;


  begin {walk}
    trueused := false;
    falseused := false;
    oktoclear := true;
    contextsp := 1;
    with context[1] do
      begin
      low := 1;
      high := 0;
      end;
    if bigcompilerversion then ptr := @(bignodetable[root^.beginstmt]);
    with ptr^ do
      begin
      if final_block_size = 0 then blocksize := bs
      else blocksize := final_block_size - ps;
      genpseudo(blockentry, 0, 0, 0, 0, procref, ps, blocksize);
      end;
    { now walk the block in dfo }
    currentblock := root;
    labelnext := true;
    repeat
      oktolabel := labelnext;
      labelnext := true;
      { walk the statements for this block }
      { do the context operations for this block }

      { clear op always signify loops there are deferred }

      with currentblock^ do
        begin
        if restoreop then
          begin
          if saveop then
            begin
            definerestorelabel(0);
            definesavelabel(blocklabel);
            end
          else definerestorelabel(blocklabel);
          if joinop then definejoinlabel(0);
          end
        else if saveop then definesavelabel(blocklabel)
        else if (blocklabel <> 0) and oktolabel then
          begin
          { this prevents stmtblock of forloop from being labelled twice }
          if loophdr then
            begin
            { if block has any hoists label it now }
            if looplabel <> 0 then definelabel(blocklabel);
            end
          else definelabel(blocklabel);
          end;
        if not isdead then walknodelist(precode);

        { label loops as required }
        if loophdr and not isdead then
          begin
          if clearop then
            begin
            { while or repeat stmt, must label it }
            if looplabel <> 0 then defineclearlabel(looplabel)
            else defineclearlabel(getlabel(currentblock))
            end
          else
            begin
            { for loop, label it if not labelled already }
            if looplabel = 0 then definelabel(blocklabel);
            end;
          end;

        if not isdead then
          begin
          if beginstmt <> 0 then
            begin
            walkstmtlist(beginstmt);
            if bigcompilerversion then ptr := @(bignodetable[beginstmt]);
            needjump := not (ptr^.stmtkind in
                        [forbothdr, whilebothdr, untilhdr, cforbothdr, gotohdr,
                        swbrkhdr, loopbrkhdr, loopconthdr]);
            end
          else needjump := true;
          { if the current block only has one successor better jump to it }
          { unless it is next or bottom of a for loop }
          if successor <> nil then
            begin
            if needjump and (successor^.snext = nil) and
               (successor^.suc <> dfolist) and not successor^.suc^.isdead then
              genpseudo(jump, 0, 0, 0, 0, getlabel(successor^.suc), level, 0);
            end;
          end;
        end; {with}
      currentblock := currentblock^.dfolist;
    until currentblock = nil;

    if not newdebugger then
      begin
      if nowdebugging then
        genpseudo(stmtbrk, 0, 0, 0, 0, 0, 0, ord(controlstmt) + 8);
      end
    else if (targetopsys = vms) and (exit_stmtno <> 0) then
      genpseudo(stmtbrk, 0, 0, 0, 0, exit_stmtno, 0, ord(controlstmt) + 8);

    genpseudo(blockexit, 0, 0, 0, 0, symbolrecord, 0, 0);

    if travcode then
      begin
      pseudoinst := pseudobuff;
      if emitpseudo then codeone; {this allows skipping codeone while
                                    debugging}
      emitpseudo := false;
      end;

    clearkeys;
    contextsp := 1;
    clearkeys;

    { dispose of allocated memory }
    repeat
      currentblock := root^.dfolist;
      { dispose of the successor links for this block }
      lnk := root^.successor;
      dispose(root);
      blockblocks := blockblocks - 1;
      root := currentblock;
      while lnk <> nil do
        begin
        lnk2 := lnk;
        lnk := lnk^.snext;
        dispose(lnk2);
        end;
    until root = nil;
    if blockblocks <> 0 then
      writeln('warning  ', blockblocks: 1, ' blocks not released in block ',
              blockref: 1);

  end {walk} ;

end.
