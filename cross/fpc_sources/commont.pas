
{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987 Oregon Software, Inc.
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

  Pascal-2 Compiler Tree Processing Common Routine Declarations

 Last modified by KRIS on 21-Nov-1990 15:23:00
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

unit commont;

interface

uses config, hdr, hdrt, a_t, t_c, code;

{ Tree build/improve/walk }

function newlabel: labelrange;

{ Create a new pseudo-code label.
}


procedure checkconst(node: nodeindex; {node to check}
                     var constflag: boolean; {true if node is const}
                     var i: integer {constant value if const} );

{ Check node "node" to see if it is constant, and set "constflag" and
  "i" if it is.  This is used in dead code elimination.
}


procedure estimateloop(stmt: nodeindex;
                       var fixed: boolean;
                       var overflow: boolean;
                       var runcount: unsignedint);

{ Figure out if possible how many iterations a for loop will execute.
}


procedure genpseudo(o: pseudoop; {operator}
                    l: addressrange; {operand length}
                    n: keyindex; {key for this node}
                    r: refcountrange; {reference count}
                    c: refcountrange; {copy count}
                    i, j, k: integer {operands} );

{ Generate a pseudo instruction
}


procedure genrealop(o: pseudoop; {operator}
                    l: addressrange; {operand length}
                    n: keyindex; {key for this node}
                    r: refcountrange; {reference count}
                    c: refcountrange; {copy count}
                    val: realarray {real value} );

{ Generate a pseudo instruction with a real value as an operand
}

  procedure increfcount(n: nodeindex; {node to increment}
                        deadcode: boolean;
                        inc: shortint {amount to increment} );

{ Change the effective reference count for a node by "inc".  This
  may have to chain down a sequence of nodes to get the actual most
  recent node which should be changed.
}


implementation

function newlabel: labelrange;

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



procedure checkconst(node: nodeindex; {node to check}
                     var constflag: boolean; {true if node is const}
                     var i: integer {constant value if const} );

{ Check node "node" to see if it is constant, and set "constflag" and
  "i" if it is.  This is used in dead code elimination.
}

  var
    ptr: nodeptr; {used to access node}


  begin {checkconst}
    if bigcompilerversion then ptr := @(bignodetable[node]);
    with ptr^ do
      if (action in [visit, revisit]) and (op = intop) then
        begin
        constflag := true;
        i := oprnds[1];
        end
      else constflag := false;
  end {checkconst} ;



procedure estimateloop(stmt: nodeindex;
                       var fixed: boolean;
                       var overflow: boolean;
                       var runcount: unsignedint);

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
    if bigcompilerversion then ptr := @(bignodetable[stmt]);
    currentstmt := ptr^;
    with currentstmt do
      begin

      if bigcompilerversion then ptr := @(bignodetable[expr1]);
      if ptr^.op in [forupchkop, fordnchkop, forerrchkop] then
        checkconst(ptr^.oprnds[1], constfinal, finalvalue)
      else checkconst(expr1, constfinal, finalvalue);

      if bigcompilerversion then ptr := @(bignodetable[expr2]);
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

{DRB free pascal version supports travcode mode only}
procedure genpseudo(o: pseudoop; {operator}
                    l: addressrange; {operand length}
                    n: keyindex; {key for this node}
                    r: refcountrange; {reference count}
                    c: refcountrange; {copy count}
                    i, j, k: integer {operands} );


  begin {genpseudo}
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
  end; {genpseudo}


procedure genrealop(o: pseudoop; {operator}
                    l: addressrange; {operand length}
                    n: keyindex; {key for this node}
                    r: refcountrange; {reference count}
                    c: refcountrange; {copy count}
                    val: realarray {real value} );

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
    rsize := maxrealwords;
    kludge.r := val;
    i := 1;
    while rsize > 0 do
      begin
      genpseudo(o, l, n, r, c, kludge.i[i], kludge.i[i + 1], kludge.i[i + 2]);
      i := i + 3;
      rsize := rsize - 3 * sizeof(integer);
      end;
  end; {genrealop}


  procedure increfcount(n: nodeindex; {node to increment}
                        deadcode: boolean;
                        inc: shortint {amount to increment} );

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
      if bigcompilerversion then p := @(bignodetable[n]);
      if p^.action = visit then
        if (p^.op < intop) and (p^.op > newunsvarop) then
          while p^.slink <> 0 do
            begin
            n := p^.slink;
            if bigcompilerversion then p := @(bignodetable[n]);
            end;
      if bigcompilerversion then p := @(bignodetable[n]);
      p^.refcount := p^.refcount + inc;
      if p^.action = visit then
        if p^.op = commaop then increfcount(p^.oprnds[2], deadcode, inc);
      end;
  end; {increfcount}
end.
