{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from
  Oregon Software does not apply to user-modified programs.  All copies
  of this program must display this notice and all copyright notices.

  %W% %G% %U%

  Release version: 0045 Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Tree Improver (Optimizer)

 Last modified by KRIS on 21-Nov-1990 15:25:42
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

{
  Graph improving procedures. This section deals with improving" the
  graph just produced. Lots of possibilites exist here, the important
  ones are: more folding ( by rearranging expressions), live variable
  analysis with register assignments and hoisting invariant expressions
  out of loops.


}

unit improve;

interface

uses config, hdr, hdrt, a_t, utils, foldcom, commont;

procedure improve;

implementation

procedure assignregs;
{
    Purpose:
      Based on their lifetimes and worth assign local variables to registers

    Inputs:
      Uses global arrays regvars to determine lifetime.

    Outputs:
      Update regvars with register assignments.

    Algorithm:

      For each register type call allocateregs with proper parameters.

    Sideeffects:
      None

    Last Modified: 7/12/85

}

  type
    regallocset = set of regalloctype;

  var
    reglife: array [reghashindex] of region;
    regioncount: - 1..regtablelimit; { number of active regions - 1 }
    i: reghashindex; { induction var }
    frameneeded: boolean; { does procedure require FP? }


  function disjoint(var life: region): boolean;
{
    Purpose:
      determine if the lifetimes of life and reglife are disjoint.


    Inputs:
      life - lifetime region of a local var.

    Outputs:
      true iff lifetimes are disjoint else false.

    Algorithm:


    Sideeffects:
      None

    Last Modified: 7/12/85

}

    type
      clip = (above, below, right, left);
      clipset = set of clip;

    var
      i: reghashindex; {induction vars }
      code1, code2, code3, code4: clipset; { clipping for rectangle's sides }


    procedure clipping(lon: nodeindex;
                       fon: fonrange;
                       var clips: clipset);
     { Sutherland's clipping algorithm. See any computer graphics book. }


      begin
        if (lon < life.lonmin) then clips := [left]
        else if (lon > life.lonmax) then clips := [right]
        else clips := [];
        if (fon < life.fonmin) then clips := clips + [below]
        else if (fon > life.fonmax) then clips := clips + [above];
      end;


    begin {disjoint}
      disjoint := true;
      i := 0;
      while i <= regioncount do
        begin
        with reglife[i] do
          begin
        { in order to be disjoint all 4 lines must be "off screen" where
        one lifetime is the screen. Also cannot be surrounder.
        }
          clipping(lonmin, fonmin, code1);
          clipping(lonmin, fonmax, code2);
          clipping(lonmax, fonmin, code3);
          clipping(lonmax, fonmax, code4);
          { if ( offscreen ) or ( surround ) then ... }
          if ((code1 * code2 = []) or (code1 * code3 = []) or
             (code2 * code4 = []) or (code3 * code4 = [])) or
             ((code1 * code2 = [left]) and (code1 * code3 = [below]) and
             (code2 * code4 = [above]) and (code3 * code4 = [right])) then
            begin
            disjoint := false;
            i := regioncount;
            end;
          i := i + 1;
          end; {with}
        end;
    end {disjoint} ;


  procedure allocateregs(maxregs: shortint;
                         acceptable: regallocset;
                         var regcount: shortint);
{
    Purpose:
      Allocate maxregs to vars in acceptable class.

    Inputs:
      maxregs : number of registers of this type available to allocate.
      acceptable : variable type ok to allocate to this register.

    Outputs:
      regcount : number of registers allocated.

    Algorithm:
        0. Init the register lifetime to empty.
        1. Find the most valuable var that has a disjoint lifetime with
           register. If one exists then assign it to the register and
           add the vars lifetime to the register's lifetime and repeat.

           In order to be interesting, the worth must exceed the cost
           of saving and restoring the register, plus the cost of any
           load.

    Sideeffects:
      none.

}

    var
      i, j: shortint; {induction vars }
      bestvar: reghashindex; { best var to assign this pass }
      bestworth: shortint; { most useful var with disjoint register life }
      allocated: boolean; { true if a register allocated on this path }
      varalloc: allockind; {kind of allocation}


    begin {allocateregs}
      for i := regcount + 1 to maxregs do
        begin
        regioncount := - 1;
        allocated := false;
        { find a register to assign }
        repeat
          bestworth := 0;
          j := 0;
          while j <= regtablelimit do
            begin
            with regvars[j] do
              begin
              if registercandidate and
                 (worth - 3 * ord(parameter) > bestworth) and
                 (regkind in acceptable) then
                begin
                if disjoint(varlife) then
                  begin
                  bestworth := worth - 3 * ord(parameter);
                  bestvar := j;
                  end;
                end;
              end;
            j := j + 1;
            end;
          if regioncount < 0 then bestworth := max(bestworth - 2, 0);
          if (bestworth > 0) then
            begin
            {found one to allocate }
            allocated := true;
            regvars[bestvar].worth := 0;
            regvars[bestvar].regid := i;
            regioncount := regioncount + 1;
            reglife[regioncount] := regvars[bestvar].varlife;
            { update debug file }
            with regvars[bestvar] do
              if debugrecord <> 0 then
                begin
                case regvars[bestvar].regkind of
                  ptrreg: varalloc := ptrregister;
                  genreg, bytereg: varalloc := genregister;
                  realreg: varalloc := realregister;
                  end;
		  {DRB  dbg_alloc(debugrecord, varalloc, regid)}
                end;
            end;
        until bestworth = 0;
        if allocated then regcount := regcount + 1;
        end;
    end {allocateregs} ;


  begin {assignregs}
    if switcheverplus[tblock] and (tblocknum = blockref) then
      begin
      { show register lifetimes }
      writeln('  off typ worth lmin lmax fmin fmax');
      for i := 0 to regtablelimit do
        if regvars[i].registercandidate then
          begin
          with regvars[i], varlife do
            writeln(offset: 5, ord(regkind): 3, worth: 7, lonmin: 5, lonmax: 5,
                    fonmin: 5, fonmax: 5);
          end;
      end;
    { assign general registers }
    case targetmachine of
      ns32k:
        begin
        { static link comes out of general regs }
        if newtravrsinterface then
          if new_proctable[blockref div (pts + 1)]^[blockref mod (pts +
             1)].intlevelrefs then
            regtemps := 1 { static link gets first register}
          else regtemps := 0
        else if proctable[blockref].intlevelrefs then
          regtemps := 1 { static link gets first register}
        else regtemps := 0;
        allocateregs(assignreg, [genreg, ptrreg], regtemps);
        { no specific ptr reg on 32k }
        ptrtemps := 0;
        realtemps := 0;
        allocateregs(assignrealreg, [realreg], realtemps);
        end;
      vax:
        begin
        if newtravrsinterface then
          with new_proctable[blockref div (pts + 1)]^[blockref mod (pts + 1)] do
            begin
            { static link comes out of general regs }
            if intlevelrefs then regtemps := 1
            else regtemps := 0;
 { static link gets first register, or ap if non pascal; check paramsize,
   later...
 }
            end
        else
          with proctable[blockref] do
            begin
            if intlevelrefs then regtemps := 1
            else regtemps := 0;
            end;

        allocateregs(assignreg, [genreg], regtemps);
        { no specific ptr or real regs on the VAX }
        ptrtemps := 0;
        realtemps := 0;
        end;
      iAPX86:
        begin
        regtemps := 0;
        {static link must be register bx}
        {static link gets the only available register}
        if newtravrsinterface then
          if not new_proctable[blockref div (pts + 1)]^[blockref mod (pts +
             1)].intlevelrefs then
            begin
            allocateregs(assignreg, [genreg], regtemps);
            end
          else if not proctable[blockref].intlevelrefs then
            begin
            allocateregs(assignreg, [genreg], regtemps);
            end;
        { no specific ptr or real regs on the iAPX-86 }
        ptrtemps := 0;
        realtemps := 0;
        end;
      i80386:
        begin
        regtemps := 0;
        {static link must be register ebx}
        if newtravrsinterface then
          with new_proctable[blockref div (pts + 1)]^[blockref mod (pts + 1)] do
            begin
            if intlevelrefs then
              regtemps := 1 { static link gets only register}
            else regtemps := 0;
            frameneeded := needsframeptr;
            end
        else
          with proctable[blockref] do
            begin
            if intlevelrefs then
              regtemps := 1 { static link gets only register}
            else regtemps := 0;
            frameneeded := needsframeptr;
            end;
        allocateregs(1 {assignreg} , [genreg, bytereg], regtemps);

        {allocate register ebp - the frame pointer - if not used}
        if not switcheverplus[framepointer] and not frameneeded then
          allocateregs(2, [genreg], regtemps);
        { no specific ptr or real regs }
        ptrtemps := 0;
        realtemps := 0;
        end;
      mc68000:
        begin
          { Static link comes out of general regs.
          }
        ptrtemps := 0;
        if newtravrsinterface then
          with new_proctable[blockref div (pts + 1)]^[blockref mod (pts + 1)] do
            begin
            if intlevelrefs then ptrtemps := 1;
            if (register_return and not switcheverplus[structstatic] and
               (struct_calls or struct_ret)) or (language = c) and
               (switcheverplus[debugging] or switcheverplus[targdebug]) then
              ptrtemps := ptrtemps + 1;
            end
        else
          with proctable[blockref] do
            begin
            if intlevelrefs then ptrtemps := 1;
            if (register_return and not switcheverplus[structstatic] and
               (struct_calls or struct_ret)) or (language = c) and
               (switcheverplus[debugging] or switcheverplus[targdebug]) then
              ptrtemps := ptrtemps + 1;
            end;

          { For Versados the debugger/profiler uses one dedicated register.
            If generating 68000 pic and there is an own section, then a
            dedicated register is used here too.  If both are true the
            debugger loses and debugger steps are three instructions long.
          }
        if (targetopsys = vdos) and ((switcheverplus[pic] and (ownsize > 0)) or
           switcheverplus[debugging] or switcheverplus[profiling]) then
          ptrtemps := ptrtemps + 1;

        allocateregs(assignptrreg, [ptrreg], ptrtemps);
        regtemps := 0;
        allocateregs(assignreg, [genreg], regtemps);
        realtemps := 0;
        if switcheverplus[fpc68881] then
          allocateregs(assignrealreg, [realreg], realtemps);
        end;
      end;
    if switcheverplus[tswitch1] and switcheverplus[details] then
      begin
      writeln('register assignments for block ', blockref: 1);
      for i := 0 to regtablelimit do
        with regvars[i], varlife do
          begin
          if regid <> 0 then
            begin
            writeln('id ', regid: 2, ' typ ', ord(regkind): 1, ' off ', offset:
                    1, ' lon ', lonmin: 1, ' ', lonmax: 1, ' fon ', fonmin: 1,
                    ' ', fonmax: 1);
            end;
          end;
      end;

  end {assignregs} ;


procedure loops;
{
    Purpose:
      Hoist invariant expressions out of loops.

    Inputs:
      Flow graph

    Outputs:
      Modified flow graph.

    Algorithm:
      The basic blocks are walked in reverse order. Whenever we back in to
      a loop we begin examining all of the expressions within the loop
      to determine if they are invariant. When an expression is determined
      to be invariant it is hooked to the header block of the loop. As
      usual things are more complicated than this sounds. The two major
      complications are common subexpressions shared by two loops and
      the need to insert copy operations so that nested contexts can
      reference the expression that since being hoisted is no longer within
      it's context. The first complication also manages to solve the
      problem for hoisting invariants multiple levels of loops.

      We hoist hoist any expression that is invariant and "safe". Some
      expressions are invariant but may not be safe such as:
      if p <> nil then foo := p^;
      p^ may be invariant but if it is hoisted the a segment fault may
      occur which would not happen if hoisting did not occur. Operators
      are divided into safe and unsafe classes, addition, subtraction are
      safe array subscripting, pointer dereferencing, division are not
      safe. (On machines where overflow traps occur on addition/subtraction
      even they are not safe). Invariant safe expressions are always hoisted,
      Invariant unsafe expressions are only hoisted if the basic block
      containing the expression will execute if the closest loop containing
      it will run if execution happens to reach it.

      As a side effect this hoisting can cause slower execution when
      expressions are hoisted out of loops which rarely if ever execute.
      This is a rare occurrence according to the literature and is
      ignored.

    Sideeffects:
      Many

    Last Modified: 7/3/85

}

  var
    blk: basicblockptr; { for walking blocks }
    ptr: nodeptr; { for access to stmt node }
    nextblk: basicblockptr; { next block to examine }
    anyinvars: boolean; { used for testing purposes }


  procedure examineloop(loopblk, bottomblk: basicblockptr);
{
    Purpose:
      Examine loop for invariant expressions

    Inputs:
      loopblk : ptr of the block describing loophdr of loop to examine.
      bottomblk : ptr to final block in the loop.

    Outputs:
      modified graph

    Algorithm:


    Sideeffects:
      invariants marked.

    Last Modified: 9/9/85

}

    var
      p: nodeindex; { for walking hoist chains }
      ptr, ptr2: nodeptr; { for access to nodes }
      stmt: nodeindex; { current stmt in basic block }
      lasthoist: nodeindex; { index of last invariant hoisted in this loop }
      currenthoist: nodeindex; { index of current invariant from inner loop }
      nexthoist: nodeindex; { index of next invariant from inner loop }
      previoushoist: nodeindex; { index of previous invariant from inner loop }
      current: basicblockptr; { basic block being examined }
      innerloop: basicblockptr; { basic block containing an innerloop }
      secondlevel: boolean; { true if examining expressions that have already
                             been hoisted in some other loop }
      finalwrite: nodeindex; { index of 1st write outside loop's writes }
      temp: addressrange; { for building write list }
      fixnode: nodeindex; { index of node that points to pushfinal }
      newoprnd: nodeindex; { index of pushfinal oprnd }
      finalexpr: nodeindex; { index of for final expression }
      initexpr: nodeindex; { index of for index variable }
      dominating: boolean; { true if current block dominates loopblks exit and
                            loopblk executes }


    procedure definecontext(outerblk, innerblk: basicblockptr);
{
    Purpose:
      Find the context stack of the basicblock "innerblk".

    Inputs:
      outerblk : the basic block defining the outermost context
      innerblk : the basic block we wish to find the context for.

    Outputs:
      none : just side effects

    Algorithm:
      Starting at outerblk, we walk dfolist until we reach innerblock.
      At each saveop we increment the context stack and save the block
      pointer. At each restoreop we decrement the context stack.

    Sideeffects:
      modifies context[1]..context[n] for use by hoistit.

    Last Modified: 11/26/86

}

      var
        overflow: shortint; { depth of context overflow }


      begin {definecontext}
        contextsp := 0;
        overflow := 0;
        context[0].firstblock := outerblk;

        repeat
          if outerblk^.restoreop then
            begin
            if overflow = 0 then contextsp := contextsp - 1
            else overflow := overflow - 1;
            end;
          if outerblk^.saveop then
            begin
            if contextsp = contextdepth then overflow := overflow + 1
            else
              begin
              contextsp := contextsp + 1;
              context[contextsp].firstblock := outerblk;
              end;
            end;
          outerblk := outerblk^.dfolist;
        until outerblk = innerblk^.dfolist;

      end; {definecontext}


    procedure hoistit(expr: nodeindex);
{
    Purpose:
      Link expression into the precode chain of loopblk and mark all of
      the graph below it as hoisted by loopblk.

    Inputs:
      expr : expression to be hoisted.

    Outputs:
      modified graph.

    Algorithm:
      If a copy is present must search the subgraph for the nodes which point
      to copies and make them point directly to the copied expression.

    Sideeffects:
      modified graph.

    Last Modified: 6/27/85

}

      var
        ptr: nodeptr; { for access to node }
        copied: nodeptr; { for building copy node }
        lastcopy: nodeindex; { previous copynode in copy chain }
        refc, copyc: refcountrange; { ref count of hoisted expression }
        newexpr: nodeindex; { the node to hoist if copies needed }
        sp: contextindex; { induction var }


      procedure flood(expr: nodeindex;
                      forceflood: boolean);
{
    Purpose:
      Mark expression rooted in expr as hoisted.

    Inputs:
      expr : index of node to mark as hoisted.

    Outputs:
      modified graph

    Algorithm:
      recursive descent of graph.

    Sideeffects:
      modified graph.

    Last Modified: 7/6/85

}

        var
          i: 1..3; {induction vars }
          ptr: nodeptr; { for node access }
          operands: operandarray; { local copy of oprnd field }
          nodeoperands: nodeoperandarray; { local copy of nodeoprnd field }


        begin {flood}

          if bigcompilerversion then ptr := @(bignodetable[expr]);
          if (ptr^.hoistedby = nil) or forceflood then
            begin
            ptr^.hoistedby := loopblk;
            if ptr^.action = visit then
              begin
              operands := ptr^.oprnds;
              nodeoperands := ptr^.nodeoprnd;
              for i := 1 to 3 do
                begin
                if nodeoperands[i] then flood(operands[i], false);
                end
              end
            else flood(ptr^.oldlink, false);
            end;

        end {flood} ;


      procedure removecopies(depth: contextindex; {depth to which we remove 'em}
                             expr: nodeindex);
{
    Purpose:
      Remove copies in expr which are at least as deep as the target context
      depth to which we will hoist.

    Inputs:
      depth: how deep to go, presumed > 0.
      expr : index of expression graph to search.

    Outputs:
      modified graph

    Algorithm:
      recursive descent of graph.

    Sideeffects:
      modified graph.

    Last Modified: 7/6/85

}

        var
          i: 1..3; {induction vars }
          d: contextindex; { running depth count along a particular oprnd }
          copylink: nodeindex; { running index to copy node's target }
          exprptr: nodeptr; { for access to original node }
          ptr: nodeptr; { for walking copy links }
          operands: operandarray; { local copy of oprnd field }
          nodeoperands: nodeoperandarray; { local copy of nodeoprnd field }


        begin {removecopies}

          if bigcompilerversion then exprptr := @(bignodetable[expr]);

          { should always be a visit node }

          if exprptr^.action = visit then
            begin
            operands := exprptr^.oprnds;
            nodeoperands := exprptr^.nodeoprnd;
            for i := 1 to 3 do
              begin
              if nodeoperands[i] then
                begin
                if bigcompilerversion then
                  ptr := @(bignodetable[operands[i]]);
                if ptr^.action = copy then
                  begin
                  blocksin[1].written := true;
                  d := depth;
                  repeat
                    d := d - 1;
                    copylink := ptr^.oldlink;
                    if bigcompilerversion then exprptr^.oprnds[i] := copylink;
                    ptr^.refcount := ptr^.refcount - 1;
                    if bigcompilerversion then
                      ptr := @(bignodetable[copylink]);
                    ptr^.copycount := ptr^.copycount - 1;
                  until (d = 0) or (ptr^.action = visit);
                  if d > 0 then removecopies(d, copylink);
                  end
                else removecopies(depth, operands[i]);
                end;
              end; {for}
            end
          else
            begin
            writeln('removecopies bad node found');
            compilerabort(inconsistent);
            end;

        end {removecopies} ;


      procedure onecopy(sp: contextindex; { current context level}
                        c: nodeindex {where to build copy node} );

        { Build one copy access node in chain leading to hoisted expr
        }


        begin {onecopy}

          if bigcompilerversion then ptr := @(bignodetable[c]);
          copied^.copycount := refc;

          with ptr^ do
            begin
            refcount := refc;
            copycount := copyc;
            action := copy;
            oldlink := lastcopy;
            directlink := newexpr;
            hoistedby := loopblk;
            prelink := context[sp].firstblock^.precode;
            context[sp].firstblock^.precode := c;
            end;
          lastcopy := c;
          copied := ptr;
        end {onecopy} ;


      begin {hoistit}
        if bigcompilerversion then ptr := @(bignodetable[expr]);

        { first see if it's been hoisted already or not a true invariant }

        if not ((ptr^.action = visit) and
           (ptr^.op in
           [defforlitindexop, defforindexop, forindexop, endexpr, defunsforlitindexop,
           defunsforindexop])) and (secondlevel or (ptr^.hoistedby = nil)) then
          begin
            { Two complications arise here. First we must walk the expression
              to remove the references to the
              copy operations since now the expression will be higher in the
              graph than the copy. In this case we should also search the
              precode chain for an expression that matches this one otherwise
              we can produce much slower "optimized" code.

              Second if the block being hoisted out of is a nested context
              we must replace the original node with a copy operation.
            }
          if contextsp > 0 then removecopies(contextsp, expr);

            { expr must be hooked to the precode of loopblk. If we are within
              nested context(s) we must build the proper copy chains. We then
              convert the original node to a copy and build a new node as the
              actual expression.
            }
          if contextsp > 0 then
            begin

            { Build a duplicate of expr, this is the node we will hoist }

            if lastnode + contextsp >= tnodetablesize then compilerabort(manynodes);

            lastnode := lastnode + 1;
            newexpr := lastnode;

            if bigcompilerversion then copied := @(bignodetable[newexpr]);

            copied^ := ptr^;
            copied^.prelink := 0;
            lastcopy := newexpr;
            refc := ptr^.refcount;
            copyc := ptr^.copycount;

            for sp := 1 to contextsp - 1 do
              begin
              lastnode := lastnode + 1;
              onecopy(sp, lastnode);
              end;
            onecopy(contextsp, expr);
            expr := newexpr;
            end; {contextsp > 0}

          if lasthoist = 0 then loopblk^.precode := expr
          else
            begin
            if bigcompilerversion then ptr := @(bignodetable[lasthoist]);
            ptr^.prelink := expr;
            end;
          lasthoist := expr;

          { count the number of hoists that occur }

          if switcheverplus[test] then
            begin
            if bigcompilerversion then ptr := @(bignodetable[expr]);
            if not anyinvars then
              begin
              anyinvars := true;
              end;
            end;
          hoistone := hoistone + 1;
          end;

        { now mark all nodes in expression as hoisted }

        flood(expr, true);
      end; {hoistit}


    function simpleinvar(oprnd: nodeindex; {pushfinal's oprnd}
                         other: nodeindex {deffor... op} ): boolean;

{   Purpose:
      Examine the expression indexed by oprnd and see if it is a simple
      variable which is invariant in the current loop, but is NOT the
      loop variable.

    Inputs:
      oprnd: index of the expression to examine.
      other: index of the deffor operator, for comparison.

    Outputs:
      simplevar: true if oprnd is a simple invariant variable reference.

    Algorithm:
      Examine the writes for the current loop.

    Sideeffects:
      none

    Last Modified: 9/26/86 (Rick)
}

      var
        ptr: nodeptr; { for access to nodes }
        varlev: levelindex; { level of var op }
        off: addressrange; { where in frame of var }
        written: boolean; { true if found on write list }


      begin {simpleinvar}
        simpleinvar := false;
        if bigcompilerversion then ptr := @(bignodetable[oprnd]);
        if ptr^.action = copy then
          begin
          { Point to the real operand }
          oprnd := ptr^.directlink;
          if bigcompilerversion then ptr := @(bignodetable[oprnd]);
          end {copy} ;
        if ((ptr^.op = varop) or (ptr^.op = unsvarop)) and
           not loopblk^.deadlevels[ptr^.oprnds[1]] then
          begin
          varlev := ptr^.oprnds[1];
          off := ptr^.oprnds[2];
            { A simple variable points to an indxop, which points
              to a levop}
          if bigcompilerversion then ptr := @(bignodetable[ptr^.oprnds[3]]);
          if ptr^.op in [indxop, vindxop] then
            begin
            if bigcompilerversion then ptr := @(bignodetable[ptr^.oprnds[1]]);
              { Globalop's and localop's are converted to levop's
                by buildexpr }
            if (ptr^.op = levop) then
              begin
              { First, check to see if this is the loop variable }
              if bigcompilerversion then ptr := @(bignodetable[other]);
              if (level <> varlev) or (ptr^.oprnds[2] <> off) then
                begin
                  { It isn't the loop variable, so now check to see
                    if it's written within the loop }
                p := loopblk^.writes;
                written := false;
                while not written and (p <> 0) and (p <> finalwrite) do
                  begin
                  if bigcompilerversion then ptr := @(bignodetable[p]);
                  if (ptr^.oprnds[1] = varlev) and (ptr^.oprnds[2] = off) then
                    written := true;
                  p := ptr^.looplink;
                  end {while} ;
                simpleinvar := not written;
                end {not loop var} ;
              end {levop} ;
            end {indxop} ;
          end {varop} ;
      end {simpleinvar} ;


    procedure examineexpression(expression: nodeindex;
                                hoistsubtrees: boolean);
      forward;
{
    Purpose:
      Examine the oprnd indexed by expression and find any invariant
      subexpressions it contains and hoist them.

    Inputs:
      expression : index of the expression operand to be examined.
        This expression cannot be hoisted because it is either unsafe
        or not applicable.

      hoistsubtrees: if true, we may hoist any subtrees which are invariant.

    Outputs:
      modified graph.

    Algorithm:
      Examines each operand of the node "expression". For each
      invariant it finds it hoists it. If the expressions has sequential
      expressions connected to it each of those expressions are examined.

    Sideeffects:
      graph is changed.

    Last Modified: 7/22/85

}


    function invariantoprnd(oprnd: nodeindex;
                            var worthit: boolean;
                            hoistsubtrees: boolean): boolean;
{
    Purpose:
      Examine the oprnd indexed by oprnd and see if it is invariant in
      the current loop context.
      And hoistable.

    Inputs:
      oprnd : index of the expression operand to be examined.
      copyfound : true if earlier caller found a copy node in this subgraph.
      hoistsubtrees: true if we should hoist invariant subtrees

    Outputs:
      returns true if oprnd is invariant and can be hoisted.
      copyfound : true if oprnd was a copy node.
      worthit : true if oprnd is invariant and worth ( or legal ) to
                be hoisted.

    Algorithm:
      If oprnd is not hoistable type of operator calls examineexpression
      to search for invariants beneath this operator. Otherwise recursively
      examines each operand of the node "oprnd" if all of it's
      subexpressions are invariant then returns true.

    Sideeffects:
      none.

    Last Modified: 7/22/85

}

      var
        i: 0..contextdepth; {induction var }
        ptr: nodeptr; { for node access }
        operands: operandarray; { local copy of oprnd field }
        nodeoperands: nodeoperandarray; { local copy of nodeoprnd field }
        copyfound: boolean; {true if this node was a copy node}
        musthoist: boolean; {true if we must hoist common subexpression}
        invar: boolean; { true if oprnd is invariant }
        isvar: boolean; { true if op is var/unsvar }
        blockhoist: boolean; {true if intermediate copies block hoisting}
        hoist: array [1..3] of boolean; { true if subexpression hoistable}
        aindxkludge: boolean; { true if aindx should be hoisted anyway}


      function varisinvar: boolean;
{
    Purpose:
      determine if the (uns)varop is invariant.

    Inputs:
      expr : copy of the varop node.

    Outputs:
      true : if the variable is invariant in this context.

    Algorithm:
      Obvious!

    Sideeffects:
      Who me?

    Last Modified: 6/27/85

}

        var
          currentvar: nodeindex; { for walking write chains }
          found: boolean; { true if expression found on write chains }
          ptr: nodeptr; { for access to expression nodes }


        begin {varisinvar}
            { Only called on pass 2, when invariant flag is not enough to
              determine if invariant in this context or when target flag
              is on and marked invariant.
            }
          if loopblk^.deadlevels[operands[1]] then
            begin
            varisinvar := false;
            if bigcompilerversion then ptr := @(bignodetable[oprnd]);
            ptr^.invariant := false; { warn others early }
            end
          else
            begin

            { must check against the writes }

            currentvar := loopblk^.writes;
            found := false;
            while (currentvar <> finalwrite) and not found do
              begin
              if bigcompilerversion then ptr := @(bignodetable[currentvar]);
              if (operands[1] = ptr^.oprnds[1]) and
                 (operands[2] = ptr^.oprnds[2]) then
                found := true;
              currentvar := ptr^.looplink;
              end;
            if found then
              begin
              varisinvar := false;
              if bigcompilerversion then ptr := @(bignodetable[oprnd]);
              ptr^.invariant := false;
              end
            else varisinvar := true;
            end;
        end {varisinvar} ;


      begin {invariantoprnd}

        if bigcompilerversion then ptr := @(bignodetable[oprnd]);

        worthit := false;
        aindxkludge := false;
        copyfound := false;

        i := contextsp;
        while (i > 0) and (ptr^.action = copy) do
          begin
          copyfound := true; {a few redundant assignments here won't hurt}
          oprnd := ptr^.oldlink;
          if bigcompilerversion then ptr := @(bignodetable[oprnd]);
          i := i - 1;
          end;

        invar := ptr^.action = copy;
        blockhoist := copyfound and (ptr^.refcount > ptr^.copycount);
        musthoist := hoistsubtrees and not copyfound and (contextsp > 0) and
                     (ptr^.hoistedby <> loopblk) and (ptr^.refcount > 1);

        if ptr^.action = visit then
          begin
          if ((ptr^.op = paindxop) or (ptr^.op = aindxop)) and
             not dominating and (oprnd <> currenthoist) then
            begin
                { this is a kludge, it is ok to hoist array indexing as
                  long as we make sure that the possible bad address is
                  not used until the actual reference point. That is for
                  "invarop" + array[invar] the addition operator cannot
                  be hoisted but both operands can.
                }
            aindxkludge := true;
            end;

          { see if this expression is not a hoistable type or unsafe }

          if {was not ptr^.local and -- I don't know what this was supposed to
              fix, but I know what it breaks; you really cannot hoist those.
              a.o. no backend propagates correctly a key with a branchaccess
              (see copyaccess); hoisting a call also seems kind of peculiar. }
           (((ptr^.form = bools) and ptr^.relation) or
           (ptr^.op in
           [forindexop, forupchkop, fordnchkop, forerrchkop, moveop, cmoveop, movelit, lssop, leqop, neqop, eqop, gtrop,
           geqop, lsslit, leqlit, eqlit, neqlit, gtrlit, geqlit, withop,
           setfileop, setfileaddrop, setinput, newset, ptrchkop,
                  { could do this if travrs kept better track get/put don't
                   mark var as written }
                  { could do bldset, setelt, setpair if genblk would make
                    sure stack temp not popped during loop, someday... }
           bldset, setelt, setpair, bldnil, bldfmt, inop, pushaddr, pushvalue,
           pushcvalue, pushfinal, pushlitvalue, call, unscall, callparam,
           copystackop, unscallparam, reserve, pushproc, rd, wr, switchstack,
           closerangeop, definelazyop, filebufindrop,
           setbinfileop, sysfn, dummyargop]) or
           (ptr^.op in [rangechkop, indxchkop]) and (nowwalking or
           nowdebugging) or not dominating and
           (ptr^.op in
           [divop, stddivop, remop, quoop, kwoop, modop, stdmodop, rangechkop, indxchkop]) or
           ((targetopsys = vdos) and switcheverplus[pic] and
           (ptr^.op in [extop]))) then
            examineexpression(oprnd, hoistsubtrees and not copyfound)
          else
            begin
            if (ptr^.hoistedby = loopblk) or (ptr^.hoistedby = root) then
              invar := true
            else
              begin {must check}
              if ptr^.op in [defforindexop, defforlitindexop, defunsforindexop,
                 defunsforlitindexop] then
                begin
                { must determine if we are in this for loop or deeper }
                if bigcompilerversion then
                  ptr := @(bignodetable[loopblk^.beginstmt]);
                invar := ptr^.expr2 <> oprnd;
                end
              else {ordinary operator}
                begin
                operands := ptr^.oprnds;
                nodeoperands := ptr^.nodeoprnd;
                isvar := (ptr^.op = varop) or (ptr^.op = unsvarop);
                if not dominating and (ptr^.op = indrop) then
                  begin {uservar^ can't be invariant, internal indrs can be}
                  if language = c then invar := false
                  else
                    begin
                    if bigcompilerversion then
                      ptr := @(bignodetable[ptr^.oprnds[1]]);
                    invar := (ptr^.action = visit) and (ptr^.op <> unsvarop);
                    end
                  end
                else invar := true;
                for i := 1 to 3 do
                  begin
                  hoist[i] := false;
                  if nodeoperands[i] then
                    begin
                    if not invariantoprnd(operands[i], hoist[i],
                                          hoistsubtrees and not copyfound) then
                      invar := false;
                    end;
                  end;

              { Varop's require additional checking to determine invariantness }

                if invar and isvar then invar := varisinvar;
                    {
                      If entire expression wasn't hoistable, hoist worthwhile
                      invariant subexpressions.
                    }
                if hoistsubtrees and not copyfound then
                  if not invar then
                    begin
                    for i := 1 to 3 do if hoist[i] then hoistit(operands[i]);
                    end
                  else if aindxkludge or musthoist then
                    begin
                    { can't let this go any further }
                    invar := not aindxkludge;
                    hoistit(oprnd);
                    end;
                worthit := invar and not (copyfound or musthoist);
                end; {ordinary operator}
              end; {else must check}
            end; {hoistable}
          end; {action = visit}
        invariantoprnd := invar and not blockhoist;
      end {invariantoprnd} ;


    procedure examineexpression(expression: nodeindex;
                                hoistsubtrees: boolean);

      var
        i: 1..3; {induction var }
        ptr: nodeptr; { for node access }
        operands: operandarray; { local copy of oprnd field }
        nodeoperands: nodeoperandarray; { local copy of nodeoprnd field }
        valuable: boolean; { true if invariant worth hoisting }


      begin {examineexpression}
        repeat
          if bigcompilerversion then ptr := @(bignodetable[expression]);

          if ptr^.action = visit then
            begin

            operands := ptr^.oprnds;
            nodeoperands := ptr^.nodeoprnd;
              {
                this expression may contain several parallel expressions
                which should be examined also
              }
            if ptr^.op < intop then expression := ptr^.slink
            else expression := 0;
            for i := 1 to 3 do
                {
                  Examine each operand to determine if expression is
                  invariant.  If it is hoist it.
                }
              if nodeoperands[i] then
                begin
                if invariantoprnd(operands[i], valuable, hoistsubtrees) then
                  if valuable and hoistsubtrees then hoistit(operands[i]);
                end;
            end
          else expression := 0;
        until expression = 0;
      end {examineexpression} ;


    procedure examineinvariant(var expression: nodeindex;
                               previous: nodeindex);
{
    Purpose:
      Examine an expression found to be invariant in an inner loop and
      hoist any part of it found to be invariant in this loop.

    Inputs:
      expression : invariant expression to be examined.
      previous : index of previous invariant on precode list.

    Outputs:
      expression : 0 if entire expression was found to be invariant
        otherwise unchanged.

    Algorithm:
      Examine expression to determine if it is invariant. If entire
      expression is invariant insert in precode chain of loopblk,
      remove from precode chain of hoistedby and replace it with a
      copy operator.
      Otherwise link any invariant subexpressions found onto loopblk's
      precode list and insert the appropriate copy operations.

    Sideeffects:
      modified graph.

    Last Modified: 7/3/85

}

      var
        ptr: nodeptr; { for access to node }
        valuable: boolean;


      begin {examineinvariantoprnd}

        if bigcompilerversion then ptr := @(bignodetable[expression]);

        secondlevel := true;

        if (ptr^.hoistedby = innerloop) and invariantoprnd(expression, valuable,
                                                           false) then
          begin
          if valuable then
            begin
            if previoushoist = 0 then innerloop^.precode := nexthoist
            else
              begin
              if bigcompilerversion then
                ptr := @(bignodetable[previoushoist]);
              ptr^.prelink := nexthoist;
              end;
            if bigcompilerversion then ptr := @(bignodetable[expression]);
            ptr^.prelink := 0;
            hoistit(expression);
            expression := 0;
            end
          end;

        if expression <> 0 then {try to hoist bits of it, at least}
          examineexpression(expression, true);

        secondlevel := false;

      end {examineinvariantoprnd} ;


    begin {examineloop}

      { see if this loop is too deeply nested, and ignore it if so}

      if not (loopblk^.deadloop or loopblk^.isdead) then
        begin
          { find the last node in the precode chain, loops rarely have any already
            unless they are within a with statement.
           }

        p := loopblk^.precode;
        lasthoist := 0;
        currenthoist := 0;
        while p <> 0 do
          begin
          if loopblk^.looplabel = 0 then loopblk^.looplabel := newlabel;
          if bigcompilerversion then ptr := @(bignodetable[p]);
          lasthoist := p;
          p := ptr^.prelink;
          end;

        { find the first write outside of this loop }

        if loopblk^.lastwrite <> 0 then
          begin
          if bigcompilerversion then
            ptr := @(bignodetable[loopblk^.lastwrite]);
          finalwrite := ptr^.looplink;
          end
        else finalwrite := 0;

        current := bottomblk;

        { examine each basic block within the loop }

        while current <> loopblk^.rdfolist do
          begin

          { define the context of the current block }

          definecontext(loopblk, current);

          { examine each statement in this block }
          if not current^.isdead and (current^.beginstmt <> 0) then
            begin
            stmt := current^.beginstmt;
            if bigcompilerversion then ptr := @(bignodetable[stmt]);

            { see if we just backed into a new loop }

            if (current <> bottomblk) and
               (ptr^.stmtkind in
               [forbothdr, cforbothdr, whilebothdr, untilhdr, loopbothdr]) then
              begin
              if ptr^.stmtkind = untilhdr then innerloop := ptr^.falseblock
              else innerloop := ptr^.looptop;
              examineloop(innerloop, current);

              { examine the expressions hoisted out of inner loop(s) }

              definecontext(loopblk, current);

              if innerloop^.dominates and (innerloop^.precode <> 0) then
                begin

                { see if we can hoist any invariants from innerloop }

                dominating := loopblk^.willexecute;
                previoushoist := 0;
                currenthoist := innerloop^.precode;
                repeat
                  if bigcompilerversion then
                    ptr := @(bignodetable[currenthoist]);
                  nexthoist := ptr^.prelink;
                  if ptr^.action = visit then
                    examineinvariant(currenthoist, previoushoist);

                  { if entire expression wasn't hoisted make it previous}

                  if currenthoist <> 0 then previoushoist := currenthoist;
                  currenthoist := nexthoist;
                until currenthoist = 0;
                end; {precode <> 0}
              current := innerloop;
              end {current <> bottomblk}

              { if this block doesn't dominate loop exit ignore it }

            else if (current = loopblk) or current^.dominates then
              begin

              { examine each stmts expressions }

              if current = loopblk then dominating := current^.dominates
              else dominating := loopblk^.willexecute and current^.dominates;
              repeat
                if bigcompilerversion then ptr := @(bignodetable[stmt]);
                { get next stmt while it's cheap }
                stmt := ptr^.nextstmt;
                secondlevel := false;
                if (ptr^.stmtkind in
                   [ifhdr, whilehdr, withhdr, simplehdr, untilhdr,
                   syscallhdr]) and (ptr^.expr1 <> 0) then
                  examineexpression(ptr^.expr1, true)

                else if ptr^.stmtkind = casehdr then
                  begin
                  examineexpression(ptr^.selector, true);
                  end;
              until stmt = 0;
              end; {else}
            end; {beginstmt<>0}

          current := current^.rdfolist;
          end; {while}

          { finally if this loop is a for loop and the final limit is a
            simple invariant var then no need to "pushfinal" it just
            reference it.
          }
        if loopblk^.beginstmt <> 0 then
          begin
          fixnode := loopblk^.beginstmt;
          if bigcompilerversion then ptr := @(bignodetable[fixnode]);
          if (ptr^.stmtkind = foruphdr) or (ptr^.stmtkind = fordnhdr) then
            begin
            finalexpr := ptr^.expr1;
            initexpr := ptr^.expr2;
            if bigcompilerversion then ptr := @(bignodetable[ptr^.expr2]);
            temp := ptr^.len; {length of index variable}
            if bigcompilerversion then ptr := @(bignodetable[finalexpr]);
            if ptr^.op in [forupchkop, fordnchkop, forerrchkop] then
              begin
              fixnode := finalexpr;
              if bigcompilerversion then
                ptr := @(bignodetable[ptr^.oprnds[1]]);
              if ptr^.op = pushfinal then
                begin
                newoprnd := ptr^.oprnds[1];
                if bigcompilerversion then ptr := @(bignodetable[newoprnd]);
                if simpleinvar(newoprnd, initexpr) and (temp = ptr^.len) then
                  begin
                  { delete the pushfinal op }
                  if bigcompilerversion then ptr2 := @(bignodetable[fixnode]);
                  ptr2^.oprnds[1] := newoprnd;
                  end;
                end;
              end
            else if ptr^.op = pushfinal then
              begin
              newoprnd := ptr^.oprnds[1];
              if bigcompilerversion then ptr := @(bignodetable[newoprnd]);
              if simpleinvar(newoprnd, initexpr) and (temp = ptr^.len) then
                begin
                { delete the pushfinal op }
                if bigcompilerversion then ptr2 := @(bignodetable[fixnode]);
                ptr2^.expr1 := newoprnd;
                end;
              end;
            end; { forloop}
          end { <> 0 } ;

        if (lasthoist > 0) and (loopblk^.looplabel = 0) then
          loopblk^.looplabel := newlabel;
        end;
    end {examineloop} ;


  begin {loops}
    anyinvars := false;

    { walk the blocks until we find a loophdr }

    blk := tail;
    repeat
      { this test will be unneeded when empty basic blocks are removed }
      if not blk^.isdead and (blk^.beginstmt <> 0) then
        begin
        if bigcompilerversion then ptr := @(bignodetable[blk^.beginstmt]);
        if (ptr^.nodeform = stmtnode) and
           (ptr^.stmtkind in
           [loopbothdr, whilebothdr, forbothdr, cforbothdr, untilhdr]) then
          begin
          { get next blk now while its cheap }
          if ptr^.stmtkind = untilhdr then nextblk := ptr^.falseblock^.rdfolist
          else nextblk := ptr^.looptop^.rdfolist;
          examineloop(nextblk^.dfolist, blk);
          blk := nextblk;
          end
        else blk := blk^.rdfolist;
        end
      else blk := blk^.rdfolist;
    until blk = nil;

  end {loops} ;


procedure smooth;
{
    Purpose:
      Smooth out size of expression.

    Inputs:
      none.

    Outputs:
      none.

    Algorithm:
      For each basic block, examine each statement and call
      smoothexpr to all the work.

    Sideeffects:
      Graph is changed.

    Last Modified: 11/12/85

}

  var
    ptr: nodeptr; { for access to nodes }
    stmt: nodeindex; { current stmt in basic block }
    blk: basicblockptr; { block being examined }


  procedure smoothexpr(expression: nodeindex;
                       initial: addressrange);
{
    Purpose:
      Convert the length of all arithmetic operators in an
      expression the size of the widest length contained in
      the expression.

    Inputs:
      expression : index of the expression to be examined.

    Outputs:
      none.

    Algorithm:
      Walk all of the nodes of the expression recording the
      max and min values of the length field. If after walking
      the entire expression min <> max the rewalk the expression
      converting length to max. Complicated by the fact that
      some pseudoops cannot have their length fields changed
      ( e.g. varop ) and others ( e.g. aindxop ) have bogus lengths.
      Whenever an aindxop is seen the index expression is set to
      be targetintsize regardless of max and min size of the expression.

    Sideeffects:
      Graph is changed.

    Last Modified: 11/12/85

}

    var
      maxlen, minlen: addressrange; { size ranges of expression }


    procedure getsize(expr: nodeindex);
{
    Purpose:
      Find the maximum and minimum size of expression rooted in expr

    Inputs:
      expr : expression to determine size for.

    Outputs:
      none.

    Algorithm:
      walk expression nodes recording max an min values.

    Sideeffects:
      maxlen and minlen in enclosing procedure modified.

    Last Modified: 11/12/85

    Note: this could have been coded to take maxlen and minlen as
    var parameters but this simply wastes stack space.

}

      var
        i: 1..3; { induction var }
        operands: operandarray; { local copy of oprnd field }
        nodeoperands: nodeoperandarray; { local copy of nodeoprnd field }
        ptr: nodeptr; { for access to node }


      begin {getsize}
        if bigcompilerversion then ptr := @(bignodetable[expr]);
        if (ptr^.action = visit) then
          begin
          operands := ptr^.oprnds;
          nodeoperands := ptr^.nodeoprnd;
          if (ptr^.op < intop) then
            begin
            if ptr^.op = pushvalue then
              begin
              { expressions pushed to stack should be computed at stackalign
                size. At least on 32k it is desirable.
              }
              maxlen := stackalign;
              end;
            { must smooth as parallel expression }
            if ptr^.slink <> 0 then smoothexpr(ptr^.slink, 0);
            end
          else
            begin
            if (ptr^.op in
               [plusop, minusop, mulop, divop, stddivop, remop, quoop, kwoop,
               modop, stdmodop, negop, shiftlop, incop, decop]) and
               (ptr^.form in [ints, scalars, subranges]) then
              begin
              if ptr^.len > maxlen then maxlen := ptr^.len;
              if ptr^.len < minlen then minlen := ptr^.len;
              end
            else if (ptr^.op = aindxop) or (ptr^.op = setelt) or
                    (ptr^.op = paindxop) then
              begin
              smoothexpr(operands[2], targetintsize);
              { prevent separate expression from influencing this expression}
              nodeoperands[2] := false;
              end
            else if ptr^.op = setpair then
              begin
              smoothexpr(operands[3], 0);
              nodeoperands[3] := false;
              smoothexpr(operands[2], 0);
              nodeoperands[2] := false;
              end
            else if (not (ptr^.form in [ints, subranges, scalars])) and
                    (ptr^.op in
                    [orop, andop, eqop, neqop, notop, plusop, minusop, mulop,
                    divop, stddivop, remop, quoop, kwoop, modop, stdmodop,
                    negop, shiftlop, incop, decop]) then
              begin
              smoothexpr(operands[1], 0);
              nodeoperands[1] := false;
              smoothexpr(operands[2], 0);
              nodeoperands[2] := false;
              end;
            end;
          for i := 1 to 3 do
            begin
            if nodeoperands[i] then
              begin
              getsize(operands[i]);
              end;
            end;
          end { action=visit}
        else if ptr^.action = copy then
          begin
          getsize(ptr^.directlink);
          end;
      end; {getsize}


    procedure setsize(expr: nodeindex);
{
    Purpose:
      Set the size of the expression to maxlen

    Inputs:
      expr : expression to set size for.

    Outputs:
      none.

    Algorithm:
      walk expression nodes setting len for appropriate ops.

    Sideeffects:
      graph is modified.

    Last Modified: 11/12/85

    Note: this could have been coded to take maxlen as a parameter
    but this simply wastes stack space.

}

      var
        i: 1..3; { induction var }
        operands: operandarray; { local copy of oprnd field }
        nodeoperands: nodeoperandarray; { local copy of nodeoprnd field }
        ptr: nodeptr; { for access to node }


      begin {setsize}
        if bigcompilerversion then ptr := @(bignodetable[expr]);
        if (ptr^.action = visit) then
          begin
          if (ptr^.op in
             [plusop, minusop, mulop, divop, stddivop, remop, quoop, kwoop,
             modop, stdmodop, negop, shiftlop, incop, decop]) and
             (ptr^.form in [ints, subranges, scalars]) then
            begin
            ptr^.len := maxlen;
            if not bigcompilerversion then blocksin[1].written := true;
            end;
          operands := ptr^.oprnds;
          nodeoperands := ptr^.nodeoprnd;
          { don't follow certain expressions }
          if (ptr^.op = aindxop) or (ptr^.op = setelt) or
             (ptr^.op = paindxop) then
            nodeoperands[2] := false
          else if ptr^.op = setpair then
            begin
            nodeoperands[3] := false;
            nodeoperands[2] := false;
            end
          else if (not (ptr^.form in [ints, subranges, scalars])) and
                  (ptr^.op in [orop, andop, eqop, neqop, notop]) then
            begin
            nodeoperands[1] := false;
            nodeoperands[2] := false;
            end;
          for i := 1 to 3 do
            begin
            if nodeoperands[i] then
              begin
              setsize(operands[i]);
              end;
            end;
          end { action=visit}
        else if ptr^.action = copy then
          begin
          setsize(ptr^.directlink);
          end;
      end; {setsize}


    begin {smoothexpr}
      maxlen := initial;
      minlen := targetintsize;

      getsize(expression);
      { only smoothing integer expressions }
      { temp let codegen blowup if smooth wrong thing }
      if (maxlen > minlen) { ( maxlen <= targetintsize ) } then
        setsize(expression);
    end {smoothexpr} ;


  begin {smooth}
    blk := root;
    repeat
      { examine each stmts expressions }
      stmt := blk^.beginstmt;
      while stmt <> 0 do
        begin
        if bigcompilerversion then ptr := @(bignodetable[stmt]);
        { get next stmt while it's cheap }
        stmt := ptr^.nextstmt;
        if ptr^.stmtkind in
           [ifhdr, whilehdr, withhdr, simplehdr, untilhdr, syscallhdr] then
          begin
          smoothexpr(ptr^.expr1, 0);
          end
        else if ptr^.stmtkind = casehdr then
          begin
          smoothexpr(ptr^.selector, 0);
          end
        end;
      blk := blk^.dfolist;
    until blk = tail;
  end {smooth} ;


procedure improve;
{
    Purpose:
      Optimize the flow graph.

    Inputs:
      A flow graph rooted in "root"

    Outputs:
      An improved flow graph.

    Algorithm(s):
      calls various improving procedures

    Sideeffects:
      Graph is changed.

    Last Modified: 7/3/85

}


  begin {improve}
    assignregs;
    { smooth before hoisting }
    case targetmachine of
      iAPX86, mc68000:; { Smoothing causes poor code for these machines. }
      otherwise smooth;
      end;
    if (hoisting in genset) and not irreducible then loops;
  end {improve} ;

end.
