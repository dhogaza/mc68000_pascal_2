{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from
  Oregon Software does not apply to user-modified programs.  All copies
  of this program must display this notice and all copyright notices.


  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Block Body Syntax/Semantic Analyzer

 Last modified by KRIS on 21-Nov-1990 15:21:10
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1
}

unit body;

interface

uses config, hdr, utils, scan, hdra, a_t, commona, mda, foldcom;

procedure body;

implementation

type dorangeproc = procedure (left, right: range;
                              var result: range;
			      op: binaryfoldop;
                              var mayoverflow: boolean);

{ Machine dependent routines for Body.

  These routines will possibly have to be rewritten for different machines.
}

procedure genrealvalue(op: operatortype; {type of real}
                       r: realarray);

{ Generates a real operand.  This is machine dependent, and must be
  conditionalized, for example, for a crosscompiler
}

  var
    kludge: {convert real to integer}
      record
        case integer of
          1: (b: packed array [1..32] of hostfilebyte);
          2: (i: array [1..16] of integer);
          3: (r: realarray);
      end;
    i: 1..32; {induction var for writing}


  begin {genrealvalue}
    kludge.r := r;
    genop(op);
    case targetmachine of
      iapx86: {THIS SHOULD MATCH TRAVRS}
        begin {just write bytes to the stringfile}
        if scanalys then genint(stringfilecount)
        else genint(consttablelimit - (stringtablelimit - stringfilecount));
        genint(0);
        for i := 1 to targetrealsize * hostfileunits do putbyte(kludge.b[i]);
        end;
      otherwise
        begin
        for i := 1 to maxrealwords div (hostfileunits * hostintsize) do
          genint(kludge.i[i]);
        end;
      end;
  end {genrealvalue} ;


procedure storagelimit(unsigned: boolean; {this is an unsigned value}
                       length: addressrange; {length of field}
                       packedfield: boolean; {a packed field}
                       var limit: integer);

{ Finds the maximum value that the field defined by the arguments will hold.
  This may well be greater than the range.
}

  var
    { these locals are for speed purposes, locals will go to registers
    and the loop will run faster ( sorry iapx..) }
    tlimit: integer; {temp to hold intermediate limit value}
    tlength: addressrange; {temp to hold intermediate length value}


  begin {storagelimit}
    tlength := length;
    if not packedfield then tlength := tlength * bitsperunit;
    { 6/4/85 SFG Found a case where  this ran away on an undeclared
    var. Got real tired of waiting for exit from the while loop!
    new stuff from oregon has yet another way to fix this.
    }
    if not unsigned { and ( tlength <> 0 )} then tlength := tlength - 1;
    { this solves looping problem also }
    if tlength > targetintsize * bitsperunit then
      tlength := targetintsize * bitsperunit;
    tlimit := 0;
    while tlength > 0 do
      begin
      tlimit := tlimit * 2 + 1;
      tlength := tlength - 1;
      end;
    limit := tlimit;
  end {storagelimit} ;


procedure setconstrange(value1: integer; {value to use}
                        negated: boolean; {value was negated}
                        var r: operand_range {resulting range} );

{ Set up a constant operand_range.
}


  begin {setconstrange}
    with r.optimistic do
      begin
      minlimit := value1;
      maxlimit := value1;
      extended := (value1 < 0) and not negated;
      end;
    r.pessimistic := r.optimistic;
  end {setconstrange} ;


procedure settyperange(t: entryptr; {type containing range}
                       var r: range {resulting range} );

{ Set up a range according to the range of t.
}


  begin {settyperange}
    r.minlimit := lower(t);
    r.maxlimit := upper(t);
    r.extended := t^.extendedrange;
  end {settyperange} ;


procedure setstoragerange(t: entryptr; {variable or field in question}
                          len: addressrange; {length of field}
                          packedfield: boolean; {field is packed}
                          var r: range {resulting range} );

{ Set up a range according to what the storage will hold
}

  var
    ov: boolean; {overflow dummy argument}


  begin {setstoragerange}
    with r do
      begin
      ov := unsigned(t, len, packedfield);
      storagelimit(ov, len, packedfield, maxlimit);
      if unsigned(t, len, packedfield) then minlimit := 0
      else minlimit := - maxlimit - 1;
      extended := t^.extendedrange;
      end;
  end {setstoragerange} ;


procedure setvarrange(len: addressrange; {range of variable}
                      packedfield: boolean; {this is a packed field}
                      trustrange: boolean {trust the type range data} );

{ Set the range for a variable reference.  If we know that the variable
  contains only legal values, both optimistic and pessimistic estimates
  are set from the type data.  If the variable might be uninitialized,
  the pessimistic estimate is set to the limits which can be contained
  in the storage assigned to it's value.

  The type of the variable is taken from resulttype, and the result is
  placed in oprndstk[sp].value_range
}


  begin {setvarrange}
    with oprndstk[sp].value_range do
      begin
      settyperange(resultptr, optimistic);
      if trustrange then pessimistic := optimistic
      else setstoragerange(resultptr, len, packedfield, pessimistic);
      end;
  end {setvarrange} ;



{ The following routines calculate the possible range which various
  operations may return.  Conceptually simple, the job is made quite
  difficult by the existence of "extended" range arithmetic as well
  as signed integer arithmetic, and the desire to cleanly capture
  overflow conditions rather than cause machine traps.
}


procedure binaryrange(var left, right: operand_range; {arguments}
                      extended: boolean; {an extended range}
		      signedop: binaryfoldop;
		      unsignedop: binaryfoldop;
		      dorange: dorangeproc;
                      var result: operand_range;
                      var mayoverflow: boolean);

{ Apply the appropriate operation to compute a binary range.
  This is broken out as a rather strange procedure to save a bit
  of space.  The "left" and "right" operands are passed as variable
  parameters solely to save space at the call.
}

  var
    ov: boolean; {dummy overflow operation}


  begin {binaryrange}
    if extended then
      begin
      dorange(left.optimistic, right.optimistic, result.optimistic, unsignedop,
              mayoverflow);
      dorange(left.pessimistic, right.pessimistic, result.pessimistic,
              unsignedop, ov);
      end
    else
      begin
      dorange(left.optimistic, right.optimistic, result.optimistic, signedop,
              mayoverflow);
      dorange(left.pessimistic, right.pessimistic, result.pessimistic, signedop,
              ov);
      end;
  end {binaryrange} ;


procedure modrange(left, right: range; {operands}
                   var result: range; {result}
		   op: binaryfoldop;
                   var mayoverflow: boolean);

  { Compute the resulting range for a "mod" operator.  In most cases, the
    most you can say is that the range is determined by the divisor range.
    If the dividend is constant, we can be a bit more explicit.
  }

  var
    ov: boolean; {dummy overflow operator}


  begin {modrange}
    with right do
      mayoverflow := (minlimit = 0) or not extended and (minlimit < 0);
    with left do
      if maxlimit = minlimit then
        begin
        op(maxlimit, right.maxlimit, result.maxlimit, ov);
        result.minlimit := result.maxlimit;
        end
      else
        begin
        result.maxlimit := max(right.maxlimit - 1, 0);
        result.minlimit := 0;
        end;
    result.extended := right.extended;
  end {modrange} ;


procedure divrange(left, right: range; {operands}
                   var result: range; {result}
		   op: binaryfoldop;
                   var mayoverflow: boolean);

{ Compute the resulting range for a divide operation.  This is
  extremely complicate, as each of the 9 possible sign combinations
  must be considered separately.
}

  var
    ov: boolean; {dummy for range divides}
    temp: integer; {temp for negating}
    maxmax, maxmin, minmax, minmin: integer; {cross terms}
    rightpos, rightneg: boolean; {right is always ...}


  begin {divrange}
    mayoverflow := false;
    if (right.minlimit = 0) then
      begin
      right.minlimit := 1;
      mayoverflow := true;
      end
    else if (right.maxlimit = 0) then
      begin
      right.maxlimit := - 1;
      mayoverflow := true;
      end;
    op(left.maxlimit, right.maxlimit, maxmax, ov);
    op(left.maxlimit, right.minlimit, maxmin, ov);
    op(left.minlimit, right.maxlimit, minmax, ov);
    op(left.minlimit, right.minlimit, minmin, ov);
    rightpos := right.extended or (right.minlimit > 0);
    rightneg := not right.extended and (right.maxlimit < 0);
    with left do
      if extended or (minlimit >= 0) then
        begin {dividend always positive}
        if rightpos then
          begin {divisor always positive}
          result.maxlimit := maxmin;
          result.minlimit := minmax;
          end
        else if rightneg then
          begin {divisor always negative}
          result.maxlimit := minmin;
          result.minlimit := maxmax;
          end
        else
          begin {divisor crosses zero}
          result.maxlimit := maxlimit;
          negate(maxlimit, result.minlimit, ov);
          end;
        end
      else if (maxlimit < 0) then
        begin {dividend always negative}
        if rightpos then
          begin {divisor always positive}
          result.maxlimit := maxmax;
          result.minlimit := minmin;
          end
        else if rightneg then
          begin {divisor always negative}
          result.maxlimit := minmax;
          result.minlimit := maxmin;
          mayoverflow := mayoverflow or (minlimit < targetminint) and
                         (right.maxlimit = - 1);
          end
        else
          begin {divisor crosses zero}
          negate(minlimit, result.maxlimit, ov);
          result.minlimit := minlimit;
          mayoverflow := true;
          end
        end
      else
        begin {dividend crosses zero}
        if rightpos then
          begin {divisor always positive}
          result.maxlimit := maxmin;
          result.minlimit := minmin;
          end
        else if rightneg then
          begin {divisor always negative}
          result.maxlimit := minmax;
          result.minlimit := maxmax;
          end
        else
          begin {divisor crosses zero}
          negate(minlimit, temp, ov);
          result.maxlimit := max(maxlimit, temp);
          negate(maxlimit, temp, ov);
          result.minlimit := min(temp, minlimit);
          mayoverflow := true;
          end;
        end;
    result.extended := left.extended or right.extended;
  end {divrange} ;


procedure addrange(left, right: range; {operands}
                   var result: range; {result}
		   op: binaryfoldop;
                   var mayoverflow: boolean);

{ Adjust the range of an operand as a result of an addition.
  If the operation may generate an overflow, the variable "mayoverflow"
  is set.
}

  var
    overflow: boolean;


  begin {addrange}
    with result do
      begin
      op(left.minlimit, right.minlimit, minlimit, overflow);
      op(left.maxlimit, right.maxlimit, maxlimit, mayoverflow);
      mayoverflow := mayoverflow or overflow;
      extended := left.extended or right.extended;
      end;
  end {addrange} ;


procedure subrange(left, right: range; {operands}
                   var result: range; {result}
		   op: binaryfoldop;
                   var mayoverflow: boolean);

{ Adjust the range of an operand as a result of a subtracton.
  If the operation may generate an overflow, the variable "mayoverflow"
  is set.
}

  var
    overflow: boolean;


  begin {subrange}
    with result do
      begin
      op(left.minlimit, right.maxlimit, minlimit, overflow);
      op(left.maxlimit, right.minlimit, maxlimit, mayoverflow);
      mayoverflow := mayoverflow or overflow;
      extended := left.extended or right.extended;
      end;
  end {subrange} ;


procedure mulrange(left, right: range; {operands}
                   var result: range; {result}
		   op: binaryfoldop;
                   var mayoverflow: boolean);

{ Compute the resulting range for an integer multiply.  This is
  a relatively complicated job, since different signs on the
  limits cause different terms to dominate.
}

  var
    maxmax, maxmin, minmax, minmin: integer; {temp results}
    ov: boolean; {operation overflowed}


  begin {mulrange}
    op(left.maxlimit, right.maxlimit, maxmax, mayoverflow);
    op(left.maxlimit, right.minlimit, maxmin, ov);
    mayoverflow := mayoverflow or ov;
    op(left.minlimit, right.maxlimit, minmax, ov);
    mayoverflow := mayoverflow or ov;
    op(left.minlimit, right.minlimit, minmin, ov);
    mayoverflow := mayoverflow or ov;
    if (not left.extended and (left.minlimit <= 0) and (left.maxlimit > 0)) or
       (not right.extended and (right.minlimit <= 0) and
       (right.maxlimit > 0)) then
      begin {one of them crosses zero}
      result.maxlimit := max(maxmax, minmin);
      if (left.extended or right.extended) and mayoverflow then
        result.minlimit := 0
      else result.minlimit := min(maxmin, minmax);
      end
    else if left.extended or (left.minlimit > 0) then
      if right.extended or (right.minlimit > 0) then
        begin
        result.maxlimit := maxmax;
        if (left.extended or right.extended) and mayoverflow then
          result.minlimit := 0
        else result.minlimit := minmin;
        end
      else
        begin {right must be always negative}
        result.maxlimit := minmax;
        if left.extended and mayoverflow then result.minlimit := 0
        else result.minlimit := maxmin;
        end
    else {must be < 0}
    if right.extended or (right.minlimit > 0) then
      begin
      result.maxlimit := maxmin;
      result.minlimit := minmax;
      end
    else
      begin
      result.maxlimit := minmin;
      result.minlimit := maxmax;
      end;
    result.extended := left.extended or right.extended;
  end {mulrange} ;


procedure andrange(var left, right: range; {operands, var to save space}
                   var result: range {resulting range} );

{ Compute the resulting range for an integer AND
}

  var
    minbits: integer; {value with fewest bits in representation}
    ov: boolean; {dummy overflow result}


  begin {andrange}
    if (left.maxlimit = left.minlimit) and
       (right.maxlimit = right.minlimit) then
      begin
      result.maxlimit := left.maxlimit and right.maxlimit;
      result.minlimit := result.maxlimit;
      end
    else if left.extended or right.extended or (left.minlimit >= 0) or
            (right.minlimit >= 0) then
      begin
      if (left.minlimit < 0) then minbits := right.maxlimit
      else if (right.minlimit < 0) then minbits := left.maxlimit
      else minbits := min(left.maxlimit, right.maxlimit);
      storagelimit(true, bits(minbits), true, result.maxlimit);
      result.minlimit := 0;
      end
    else
      begin
      storagelimit(false, targetintsize, false, result.maxlimit);
      result.minlimit := - result.maxlimit - 1;
      end;
    result.extended := left.extended or right.extended;
  end {andrange} ;


procedure orrange(var left, right: range; {operands, var to save space}
                  var result: range {resulting range} );

{ Compute the resulting range for an integer OR
}

  var
    maxbits: integer; {value with most bits in representation}
    ov: boolean; {dummy overflow argument}


  begin {orrange}
    if (left.maxlimit = left.minlimit) and
       (right.maxlimit = right.minlimit) then
      begin
      result.maxlimit := left.maxlimit or right.maxlimit;
      result.minlimit := result.maxlimit;
      end
    else if left.extended or right.extended or (left.minlimit >= 0) and
            (right.minlimit >= 0) then
      begin
      if (left.maxlimit < 0) then maxbits := left.maxlimit
      else if (right.maxlimit < 0) then maxbits := right.maxlimit
      else maxbits := max(left.maxlimit, right.maxlimit);
      storagelimit(true, bits(maxbits), true, result.maxlimit);
      result.minlimit := 0;
      end
    else
      begin
      storagelimit(false, targetintsize, false, result.maxlimit);
      result.minlimit := - result.maxlimit - 1;
      end;
    result.extended := left.extended or right.extended;
  end {orrange} ;




procedure checkboolean;

{ Emit an error message if the result type of the current expression is
  not boolean.  Emit no message if the result type is undefined, to avoid
  redundant messages.
}


  begin {checkboolean}
    if (resultform <> none) and (resultform <> bools) then
      warnbefore(booleanexpected);
  end {checkboolean} ;


function computecost(i: integer { operand stack index } ): integer;

{ Compute the cost of the operand specified by "i".
}


  begin {computecost}
    if oprndstk[i].operandkind = constoperand then computecost := 0
    else computecost := oprndstk[i].cost;
  end {computecost} ;

{ The following group of procedures are utilities used in intermediate
  file generation and expression analysis.
}


procedure genlit(i: integer {value to generate} );

{ generate a literal with value "i".
}


  begin {genlit}
    genop(lit);
    genint(i)
  end {genlit} ;


function constcheck(i: integer {operand stack index} ): boolean;

{ True if operand i is a constant.
}


  begin {constcheck}
    constcheck := oprndstk[i].operandkind = constoperand
  end {constcheck} ;


procedure bumpsp;

{ Push a new operand on the stack if there is room.
}


  begin {bumpsp}
    if sp = oprnddepth then fatal(compilerwritererr);
    sp := sp + 1;
  end {bumpsp} ;


procedure genoprnd;

{ Make sure that the intermediate file outputs needed to access the
  top operand on the stack are generated, then pop the top operand.
  It is necessary to generate output only if the top operand is a
  constant, as the expression parsing routines will have generated
  access code for all other cases.
}

  var
    kludge: {convert to bytes or ints}
      record
        case integer of
          1: (b: packed array [0..setvaluebytes] of hostfilebyte);
          2: (s: setvalueblock);
          3: (i: array [0..10] of integer);
      end;
    i: integer; {general use induction var}
    newlim: integer; {new string file limit}
    f: entryptr; {for access to a form entry}
    emptyset: boolean; {true if set const is empty set}
    setcount: integer; {address in string table of set}


  begin {genoprnd}
    if sp = - 1 then fatal(compilerwritererr);
    with oprndstk[sp] do
      if operandkind = constoperand then
        with cvalue do
          case representation of
            ptrs:
              begin
              genop(ptrop);
              genint(niladdressvalue);
              end;
            ints, chars, bools, scalars:
              begin
              genop(intop);
              genint(intvalue);
              end;
            reals: genrealvalue(realop, realvalue.realbuffer);
            doubles: genrealvalue(doubleop, realvalue.realbuffer);
            strings, arrays, fields:
              begin
              if bigcompilerversion then f := @(bigtable[typeindex]);
              if f^.disposable and (typeindex = tabletop) then
                begin
                lastdebugrecord := lastdebugrecord - 2;
                tabletop := tabletop - 2;
                end;
              genop(structop);
              genint(pos);
              genint(oprndlen);
              end;
            sets:
              begin
              emptyset := setvalue^ = [];
              if emptyset and emptysetgenerated then setcount := emptysetcount
              else
                begin
                if scanalys then
                  begin
                  newlim := forcealign(stringfilecount,
                                       setalign * hostfileunits, false);
                  while newlim > stringfilecount do putbyte(0);
                  setcount := stringfilecount;
                  end
                else
                  begin
                  newlim := forcealign(consttablelimit,
                                       setalign * hostfileunits, false);
                  while newlim > consttablelimit do putbyte(0);
                  setcount := consttablelimit - stringtablelimit +
                              stringfilecount;
                  end;
                if emptyset then
                  begin
                  emptysetgenerated := true;
                  emptysetcount := setcount;
                  for i := 0 to setvaluebytes do putbyte(0);
                  end
                else
                  begin
                  kludge.s := setvalue^;
                  for i := 0 to oprndlen * hostfileunits - 1 do
                    putbyte(kludge.b[i]);
                  end;
                end;
              genop(structop);
              genint(setcount);
              genint(oprndlen);
              dispose(setvalue);
              end;
            otherwise; {in case of syntax errors}
            end;
    sp := sp - 1;
  end {genoprnd} ;


procedure debugstmt(s: stmttype;
                    line: integer;
                    filepos: integer;
                    fileindex: integer);


  begin {debugstmt}
    genstmt(s);

    { Only need to do sourcestringindex once.  If source name changes,
      and somebody sets to nonzero value, it'll be put out again.
    }
    if sourcestringindex <> fileindex then
      begin
      sourcestringindex := fileindex;
      genint(fileindex);
      end
    else genint(0);

    if newdebugger then
      begin
      if (switcheverplus[debugging]) and (line <> 0) then
        begin
        if display[level].firststmt = 0 then
          display[level].firststmt := display[level].laststmt;
        genint(display[level].laststmt);
        end
      else genint(line);
      end;

    genint(line);

  end {debugstmt} ;


procedure newexprstmt(s: stmttype { statement to generate } );

{ Begin a new statement which has an expression as part of its structure.
}


  begin {newexprstmt}
    debugstmt(s, thistoken.line, thistoken.filepos, thistoken.fileindex);
    intstate := opstate;
  end {newexprstmt} ;


procedure getexprstmt(s: stmttype { statement starting } );

{ Begin a new statement with expression and get the next token.
}


  begin {getexprstmt}
    newexprstmt(s);
    gettoken;
  end {getexprstmt} ;


procedure genoprndstmt;

{ Terminate the expression(s) being compiled and prepare for the
  next statement.
}


  begin {genoprndstmt}
    genoprnd;
    genop(endexpr);
    intstate := stmtstate;
  end {genoprndstmt} ;


procedure pushint(i: integer {value to push} );

{ Push an integer constant onto the operand stack.
}


  begin {pushint}
    bumpsp;
    with oprndstk[sp] do
      begin
      typeindex := intindex;
      oprndlen := targetintsize;
      operandkind := constoperand;
      cvalue.representation := ints;
      cvalue.intvalue := i;
      cvalue.negated := false;
      extended := false;
      setconstrange(i, (i < 0), value_range);
      end
  end {pushint} ;


procedure pushdummy;

{ Push a dummy operand on the stack.  This is strictly a place holder,
  and will generate no intermediate file output.
}


  begin {pushdummy}
    pushint(0);
    oprndstk[sp].operandkind := exproperand;
    oprndstk[sp].cost := 0;
  end {pushdummy} ;


procedure checkrange(subject: operand; {does this fit?}
                     believeit: boolean; {if true, use optimistic estimate}
                     var outofbounds: boolean; {definitely out of bounds}
                     var lowermaybe: boolean; {lower range needs a check}
                     var uppermaybe: boolean {upper range needs a check} );

{ Check the range associated with "subject" and see how it fits with the
  type.  The boolean variables are used to determine runtime range
  checking.  The "believeit" flag determines how much we believe the
  range data.
  ****self hosted version
}

  var
    r: range; {range to check against}
    usmin, usmax: unsignedint; {unsigned values for check}
    l, u: integer; {lower and upper type limits}
    typeptr: entryptr; {for access to typeindex data}


  begin {checkrange}
    with subject, value_range do
      begin
      if believeit then r := optimistic
      else r := pessimistic;
      if bigcompilerversion then typeptr := @(bigtable[typeindex]);
      l := lower(typeptr);
      u := upper(typeptr);
      if typeptr^.extendedrange or r.extended then
        begin
        usmin := optimistic.minlimit;
        usmax := optimistic.maxlimit;
	{DRB
        outofbounds := (usmax < l) or (usmin > u);
        usmin := r.minlimit;
        usmax := r.maxlimit;
        lowermaybe := usmin < l;
        uppermaybe := usmax > u;
	}
        outofbounds := (usmax < longword(l)) or (usmin > longword(u));
        usmin := r.minlimit;
        usmax := r.maxlimit;
        lowermaybe := usmin < longword(l);
        uppermaybe := usmax > longword(u);
        end
      else
        begin
        outofbounds := (optimistic.maxlimit < l) or (optimistic.minlimit > u);
        lowermaybe := r.minlimit < l;
        uppermaybe := r.maxlimit > u;
        end;
      end;
  end {checkrange} ;


procedure newresulttype(newtype: index);

{ Set the value of resulttype to newtype and make it available in newptr.
}


  begin {newresulttype}
    resulttype := newtype;
    if bigcompilerversion then resultptr := @(bigtable[resulttype]);
    resultform := getform(resultptr);
  end {newresulttype} ;


procedure pushconstant(follow: tokenset);

{ Parse and push a constant operand onto the stack.
}


  begin {pushconstant}
    bumpsp;
    constant(follow, true, oprndstk[sp]);
    newresulttype(oprndstk[sp].typeindex);
    with oprndstk[sp], cvalue do
      if representation = ints then
        setconstrange(intvalue, negated, value_range);
  end {pushconstant} ;


procedure newstringtype(var newtype: index; {returns index of new string type}
                        newform: types; {arrays or strings}
                        len: addressrange {number of chars in string} );

{ Build a new type describing a string.  This can be either a standard
  "packed array [1..len] of char" string or extended "string[len]".
  Note from above description that "len" refers to data bytes, not including
  the length byte for an extended string, to be allocated.
}

  var
    t: index; {holds subrange}
    t1: entryptr; {for filling in table entry}


  begin {newstringtype}
    enterform(subranges, t, t1);
    with t1^ do
      begin
      size := targetintsize;
      align := intalign;
      parenttype := intindex;
      parentform := ints;
      if newform = arrays then lowerord := 1
      else
        begin
        lowerord := 0;
        len := len + 1;
        upperord := len;
        end;
      end;
    enterform(newform, newtype, t1);
    with t1^ do
      begin
      packedflag := true;
      bitaddress := true;
      containsfile := false;
      disposable := true;
      elementtype := chartypeindex;
      stringtype := newform = arrays;
      arraymembers := len;
      indextype := t;
      size := len div (bitsperunit div stringeltsize);
      if len mod (bitsperunit div stringeltsize) <> 0 then size := size + 1;
      size := size * bitsperunit;
      elementsize := stringeltsize;
      align := stringalign;
      end;
  end {newstringtype} ;


function range_length(r: range {return length of this range in bytes} ):
 addressrange;

{ Computes the number of bytes required to compute result in the given
  range.  This is machine dependent as in most cases we don't want to
  compute lengths not supported by the machine.
}

  var
    l: addressrange;
    result: addressrange; {hold the result}


  begin {range_length}

    case targetmachine of
      {a virtual "nop" routine on the vax, because all integers
       are stored in a "long", and there is full 32-bit support.
       The alternative code shrinks short operations to 16 bits.

       note: might want to use shorter range for comparisions at a
              later date.
      }
      vax, pdp11: range_length := targetintsize;
      iapx86:
        begin
        with r do
          if extended then result := targetintsize
          else
            begin
            l := bits(max(abs(minlimit + 1), abs(maxlimit)));
            {allow for sign bit and unsigned 16-bit operations}
            if (minlimit < 0) or (switchcounters[truncatesw] <= 0) then
              l := l + 1;
            if l <= 16 then result := 2
            else result := 4;
            end;
        range_length := min(result, targetintsize);
        end;
      i80386:
        begin
        with r do
          if extended then result := targetintsize
          else
            begin
            l := bits(max(abs(minlimit + 1), abs(maxlimit)));
            {allow for sign bit and unsigned 16-bit operations}
            if (minlimit < 0) or (switchcounters[truncatesw] <= 0) then
              l := l + 1;
            if l <= 16 then result := 2
            else result := 4;
            end;
        range_length := min(result, targetintsize);
        end;
      otherwise {mc68000, ns32k}
        begin
        with r do
          if extended then result := targetintsize
          else
            begin
            l := bits(max(abs(minlimit + 1), abs(maxlimit)));
            if (targetmachine = mc68000) or (minlimit < 0) then l := l + 1;
            {allow for sign bit and signed 16-bit relative mode on 68K}
            if l <= 16 then result := 2
            else result := 4;
            end;
        range_length := min(result, targetintsize);
        end;
      end;
  end {range_length} ;

{ This group of routines is responsible for generating unary and binary
  operations.

  The generation routines do some operations on constant operands at compile
  time.  This is commonly called "constant folding".  They also check for
  special cases when one operand is constant.  For instances, multiplications
  by powers of 2 can be converted into shifts.

  The folding routines are logically local to the genbinary and genunary
  procedures, but are moved out to the global level for greater efficiency.
  This is a space saving hack which removes the need to trace static links
  to access variables.  Also, this means that a number of variables which
  are logically private to these routines are moved out to the global
  level.  These are marked in the declarations.
}


function getintvalue(i: integer {operand stack index} ): integer;

{ Return the integer value of operand i, which is assumed to
  be an integer constant.
}


  begin {getintvalue}
    if oprndstk[i].cvalue.representation = ints then
      getintvalue := oprndstk[i].cvalue.intvalue
    else getintvalue := 1;
  end {getintvalue} ;


function getrealvalue(i: integer {operand stack index} ): real;

{ Return the real value of operand i, which is assumed to be a
  real constant in host format.
}


  begin {getrealvalue}
    if (oprndstk[i].cvalue.representation = reals) or
       (oprndstk[i].cvalue.representation = doubles) then
      getrealvalue := oprndstk[i].cvalue.realvalue.realbinary
    else getrealvalue := 1.0;
  end {getrealvalue} ;


function getrealbuffer(i: integer {operand stack index} ): realarray;

{ Return the real value of operand i, which is assumed to be a
  real constant in any supported format format.
}


  begin {getrealbuffer}
    if (oprndstk[i].cvalue.representation = reals) or
       (oprndstk[i].cvalue.representation = doubles) then
      getrealbuffer := oprndstk[i].cvalue.realvalue.realbuffer
    else getrealbuffer[1] := 1;
  end {getrealbuffer} ;


procedure power2check(n: integer; { number to check }
                      var power2: boolean; {true if n = power of 2}
                      var power2value: integer {resulting power} );

{ Find out if n is an even power of 2, and return the exponent if so.
  ****self hosted version
}


  begin {power2check}
    power2value := 0;
    while (n > 0) and not odd(n) do
      begin
      n := n div 2;
      power2value := power2value + 1;
      end;
    power2 := (n = 1);
  end {power2check} ;


procedure foldcommon;

{ Common part of binary folding routines.  Pops the stack and sets isconst
  for the returned value.
}


  begin {foldcommon}
    sp := sp - 1;
    oprndstk[sp].operandkind := constoperand;
  end {foldcommon} ;


procedure returnint(intvalue: integer; {value to return}
                    negated: boolean {value has been negated} );

{ Leave an integer constant "intvalue" on the stack in the place of two
  operands.  Used in constant operation folding.
}


  begin {returnint}
    foldcommon;
    oprndstk[sp].cvalue.representation := ints;
    oprndstk[sp].cvalue.intvalue := intvalue;
    oprndstk[sp].cvalue.negated := negated;
    oprndstk[sp].extended := (intvalue < 0) and not negated;
    setconstrange(intvalue, negated, oprndstk[sp].value_range);
  end {returnint} ;


procedure returnreal(realvalue: real {value to return} );

{ Leave a real constant "realvalue" on the stack in the place of two
  operands.  Used in constant operation folding.
}


  begin {returnreal}
    foldcommon;
    oprndstk[sp].cvalue.representation := reals; {!!!}
    oprndstk[sp].cvalue.realvalue.realbinary := realvalue;
  end {returnreal} ;


procedure returnoprnd(i: integer {operand stack index} );

{ Leave operand i on the stack in the place of two operands.
  Used in constant operation folding.
}

  var
    o: operand; {temp storage for top of stack}


  begin {returnoprnd}
    o := oprndstk[i];
    foldcommon;
    oprndstk[sp] := o;
    oprndstk[sp].value_range := result_range;
  end {returnoprnd} ;


procedure returnresult(overflowed: boolean);

{ Return the value computed in result_range and give an error
  if overflowed is true.
}


  begin {returnresult}
    with result_range.optimistic do
      returnint(maxlimit, (maxlimit < 0) and not divide_extended);
    if overflowed then warnbefore(overflow);
  end {returnresult} ;


procedure dumpconst(constantlen: addressrange; {length of const to be dumped}
                    dumplen: boolean {dump length in stringfile if true} );

{ Dump a constant which is stored as an integer into the string file.
  This is required in two cases: when a structured constant is subjected
  to selection ("[", ".") or when a char or structured constant is
  converted to a string.  If "dumplen" is true the length itself becomes
  the first byte.
}

  var
    i, j, k: integer; {induction var for putting structured constant}
    tlim: integer; {temp value of consttablelimit}
    kludge: {conversion to string file}
      record
        case boolean of
          true: (b: packed array [1..hostintsize] of hostfilebyte);
          false: (i: integer);
      end;


  begin {dumpconst}
    with oprndstk[sp], cvalue do
      if representation = ints then
        begin
        kludge.i := intvalue;
        if dumplen then representation := strings
        else
          begin
          if scanalys then
            begin
            tlim := forcealign(stringfilecount, intalign * hostfileunits,
                    false);
            while tlim > stringfilecount do putbyte(0);
            end
          else
            begin
            tlim := forcealign(consttablelimit, intalign * hostfileunits,
                    false);
            while tlim > consttablelimit do putbyte(0);
            end;
          representation := arrays;
          end;
        if scanalys then pos := stringfilecount
        else pos := consttablelimit - stringtablelimit + stringfilecount;

      { This is a structured constant, so the bytes
        may have to be reversed. }

        i := 1;
        j := hostintsize * hostfileunits;
        if dumplen then
          begin
          putbyte(constantlen);
          oprndlen := oprndlen + 1;
          end;
        if constantlen < hostintsize * hostfileunits then
          if hostintlowbytefirst then j := constantlen {do left part}
          else i := j + 1 - constantlen; {do right part}
        if reversebytes then for k := j downto i do putbyte(kludge.b[k])
        else for k := i to j do putbyte(kludge.b[k]);
        end;
  end {dumpconst} ;



procedure genbinary(op: operatortype; {operation to generate}
                    form: types {type of operands} );
  forward;

{ Generate binary operation, see body for details
}


procedure foldneg;

{ Fold a negate operation if the operand is constant.
}

  var
    temp: integer; {used for negation}
    ov: boolean; {set if overflow can occur}


  begin {foldneg}
    foldedunary := false;
    with result_range.optimistic do
      begin
      negate(minlimit, temp, ov);
      negate(maxlimit, minlimit, ov);
      maxlimit := temp;
      end; 
    with result_range.pessimistic do
      begin
      negate(minlimit, temp, ov);
      negate(maxlimit, minlimit, ov);
      maxlimit := temp;
      end;
    if oconst then
      if (unaryform = reals) or (unaryform = doubles) then
        begin
        oprndstk[sp].cvalue.realvalue.realbuffer := negaterealconst(
                                                    getrealbuffer(sp));
        foldedunary := true;
        end
      else
        begin
        if oextended then 
          if (oprndstk[sp].cvalue.intvalue <> targetminint) or 
             ((oprndstk[sp].cvalue.intvalue = targetminint) and 
               oprndstk[sp].cvalue.negated) 
          then ov := true
          else
            with result_range do begin  {oops; - (maxint + 1)}
            ov := false;
            pessimistic.extended := false;
            optimistic.extended  := false;
            pessimistic.maxlimit := targetminint;
            pessimistic.minlimit := targetminint;
            optimistic.maxlimit  := targetminint;
            optimistic.minlimit  := targetminint;
            oprndstk[sp].cvalue.negated := true;
            oprndstk[sp].value_range := result_range;
            end
        else
          with oprndstk[sp].cvalue do
            begin
            intvalue := result_range.optimistic.maxlimit;
            negated := not negated;
            end;
        if ov then warnbefore(overflow);
        foldedunary := true;
        end;
  end {foldneg} ;


procedure foldrem;

{ Fold the remainder operation.  This operation always comes immediately
  after a divide operation, and extracts the remainder portion of the
  result.  It uses the global "divfolded" which is true if the divide
  could be folded, either to a constant or changed into a shift.
}

  var
    mayoverflow: boolean; {overflow might happen}


  begin {foldrem}
    if freemodwithdiv then
      begin
      mayoverflow := false;
      binaryrange(divide_range, oprndstk[sp].value_range, divide_extended,
                  @remainder, @usremainder, @modrange, result_range, mayoverflow);

      foldedunary := foldedbinary;
      if foldedunary then
        if constcheck(sp - 1) then
          begin
          with result_range.optimistic do
            returnint(maxlimit, (maxlimit < 0) and not divide_extended);
          if mayoverflow then warnbefore(badmodop);
          end
        else
          begin
          with oprndstk[sp].cvalue do
            storagelimit(true, - intvalue, true, intvalue);
          genbinary(andop, unaryform);
          end;
      divfolded := false;
      end;
  end {foldrem} ;


procedure foldquo;

{ Fold a quotient operation.  This operation is always generated immediately
  after a div operation, and extracts the quotient part of the result.  The
  global "divfolded" indicates that the div operation has been folded, either
  to a constant or changed into a shift.
}

  var
    mayoverflow: boolean; {an operation can overflow}


  begin {foldquo}
    if freemodwithdiv then
      begin
      binaryrange(divide_range, oprndstk[sp].value_range, divide_extended,
                  @divide, @usdivide, @divrange, result_range, mayoverflow);

      foldedunary := foldedbinary;
      if foldedunary then
        if constcheck(sp - 1) then
          begin
          with result_range.optimistic do
            returnint(maxlimit, (maxlimit < 0) and not divide_extended);
          if mayoverflow then warnbefore(overflow);
          end
        else if getintvalue(sp) = 0 then returnoprnd(sp - 1)
        else
          begin
          oprndstk[sp].value_range := result_range; {a hack for shiftlop}
          genbinary(shiftlop, unaryform);
          end;
      divfolded := false;
      end;
  end {foldquo} ;


procedure foldmod;

{ Fold the mod operation.  This operation always comes immediately
  after a divide operation, and extracts the remainder portion of the
  result.  It uses the global "foldedbinary" which is true if the divide
  could be folded, either to a constant or changed into a shift.
}

  var
    mayoverflow: boolean; {overflow might happen}
    power2: boolean; { true if divisor ispower of 2}
    power2value: integer; { divisor exponant value }
    typeptr: entryptr; {for access to typeindex data}
    rangenonneg: boolean; {range appears to be nonnegative}


  begin {foldmod}
    if not freemodwithdiv then
      begin
      mayoverflow := false;
      binaryrange(oprndstk[l].value_range, oprndstk[r].value_range, oextended,
                  @remainder, @usremainder, @modrange, result_range, mayoverflow);
      if (lconst and rconst) then returnresult(mayoverflow)
      else if rconst then
        begin
        power2check(getintvalue(r), power2, power2value);
        foldedbinary := false;
        with oprndstk[l] do
          begin
          if bigcompilerversion then typeptr := @(bigtable[typeindex]);
          rangenonneg := value_range.optimistic.minlimit >= 0;
          rangenonneg := rangenonneg or unsigned(typeptr, oprndlen, false);
          end;
        if power2 and rangenonneg then
          begin
          returnoprnd(l);
          pushint( - power2value);
          with oprndstk[sp].cvalue do
            storagelimit(true, - intvalue, true, intvalue);
          binaryop := andop;
          end
        end
      else foldedbinary := false;
      if not foldedbinary then newlen := targetintsize;
      end;
  end {foldmod} ;


procedure foldkwo;

{ Fold a quotient operation.  This operation is always generated immediately
  after a div operation, and extracts the quotient part of the result.  The
  global "foldedbinary" indicates that the div operation has been folded,
  either to a constant or changed into a shift.
}

  var
    mayoverflow: boolean; {an operation can overflow}
    power2: boolean; { true if divisor ispower of 2}
    power2value: integer; { divisor exponant value }
    typeptr: entryptr; {for access to typeindex data}
    rangenonneg: boolean; {range appears to be nonnegative}


  begin {foldkwo}
    if not freemodwithdiv then
      begin
      binaryrange(oprndstk[l].value_range, oprndstk[r].value_range, oextended,
                  @divide, @usdivide, @divrange, result_range, mayoverflow);

      if (lconst and rconst) then returnresult(mayoverflow)
      else if rconst and (getintvalue(r) = 1) then returnoprnd(l)
      else if rconst then
        begin
        power2check(getintvalue(r), power2, power2value);
        foldedbinary := false;
        with oprndstk[l] do
          begin
          if bigcompilerversion then typeptr := @(bigtable[typeindex]);
          rangenonneg := value_range.optimistic.minlimit >= 0;
          rangenonneg := rangenonneg or unsigned(typeptr, oprndlen, false);
          end;
        if power2 and rangenonneg then
          begin
          returnoprnd(l);
          { is this right?? 5/14/85}
          oprndstk[sp].value_range := result_range; {a hack for shiftlop}
          pushint( - power2value);
          binaryop := shiftlop;
          end
        else foldedbinary := false;
        end
      else foldedbinary := false;
      if not foldedbinary then newlen := targetintsize;
      end;
  end {foldkwo} ;


procedure foldchrarraystr;

{ Turn a character constant or quoted 'Pascal string' into a string,
  if possible.  Note that packed arrays of chars within structured
  constants are NOT folded at this time, as the length byte emitted
  by scan for true string constants is not present in such structures.
  This is a kludge, but strings are a kludge in Pascal (standard and
  extended both) and I didn't invent them!
}


  begin {foldchrarraystr}
    newstringtype(resulttype, strings, olen);
    newresulttype(resulttype);
    olen := olen + 1;
    if constcheck(sp) then
      begin
      dumpconst(olen - 1, true);
      with oprndstk[sp], cvalue do
        if representation = arrays then
          if stringconstflag then
            begin
            pos := pos - 1;
            oprndlen := oprndlen + 1;
            end
          else foldedunary := false;
      end
    else foldedunary := false;
  end {foldchrarraystr} ;


procedure foldfloat;

{ Float an integer constant if this is possible.  This sets the result
  type and size to real no matter what.
}

  var
    tival: integer; {temp storage for integer value}


  begin {foldfloat}
    newresulttype(realindex);
    olen := targetrealsize;
    oprndstk[sp].oprndlen := targetrealsize;
    if not realfolding then foldedunary := false
    else if oconst then
      begin
      tival := getintvalue(sp);
      oprndstk[sp].cvalue.representation := reals;
      oprndstk[sp].cvalue.realvalue.realbinary := tival;
      end
    else foldedunary := false;
  end {foldfloat} ;


procedure fold_double_float;

{ Float an integer constant if this is possible.  This sets the result
  type and size to double no matter what.
}

  var
    tival: integer; {temp storage for integer value}


  begin {fold_double_float}
    newresulttype(doubleindex);
    olen := doublesize;
    oprndstk[sp].oprndlen := doublesize;
    if not realfolding then foldedunary := false
    else if oconst then
      begin
      tival := getintvalue(sp);
      oprndstk[sp].cvalue.representation := doubles;
      oprndstk[sp].cvalue.realvalue.realbinary := tival;
      end
    else foldedunary := false;
  end {fold_double_float} ;


procedure foldpush;

{ Change a push to a pushlit if possible.  This applies to literal
  zero of any flavor, including nil pointers, or to integer literals.
}

  var
    i: integer; {value if foldable}


  begin {foldpush}
    if unaryform = ints then olen := max(olen, stackalign);
    foldedunary := oconst;
    if oconst then
      case unaryform of
        reals:
          begin
          if realfolding then foldedunary := getrealvalue(sp) = 0.0
          else foldedunary := false;
          i := 0;
          end;
        ints, scalars, bools, chars: i := getintvalue(sp);
        ptrs: i := niladdressvalue;
        otherwise foldedunary := false;
        end;
    if foldedunary then
      begin
      genlit(i);
      genop(pushlitvalue);
      genint(olen);
      genint(ocost);
      genform(unaryform);
      oprndstk[sp].operandkind := exproperand;
      oprndstk[sp].cost := 0;
      end;
  end {foldpush} ;


procedure foldnot;

{ Fold a boolean or integer "not" operation if possible.
}

  var
    ov: boolean; {overflow has happened}
    temp: integer; {used in "not-ing" the range}


  begin {foldnot}
    foldedunary := oconst;
    with oprndstk[sp], cvalue do
      if oconst then
        begin
        if unaryform = bools then intvalue := 1 - intvalue
        else intvalue := not intvalue;
        setconstrange(intvalue, negated, result_range);
        end
      else if unaryform <> bools then
        begin
        with result_range.optimistic do
          begin
          temp := not minlimit;
          minlimit := not maxlimit;
          maxlimit := temp;
          end;
        with result_range.pessimistic do
          begin
          temp := not minlimit;
          minlimit := not maxlimit;
          maxlimit := temp;
          end;
        end;
  end {foldnot} ;


procedure foldsetelt;

{ Fold a single set member-designator by inserting the bit into
  a constant set if the operand is constant.
}

  var
    i: integer; {constant member value}


  begin {foldsetelt}
    foldedunary := oconst;
    if foldedunary then
      begin
      with oprndstk[sp - 1].cvalue do
        if (representation = sets) and (getintvalue(sp) >= 0) and
           (getintvalue(sp) <= maxsetord) then
          setvalue^ := setvalue^ + [getintvalue(sp)]
        else warnbefore(bigsetbase);
      oprndstk[sp].operandkind := exproperand;
      oprndstk[sp].cost := 0;
      end;
  end {foldsetelt} ;


procedure foldchk(adjustlow: boolean; {adjust lower bound to 0}
                  chkerror: warning; {message to issue if error}
                  generate: boolean {generate the check} );

{ Do the preliminary work for a range check.  If the operand being checked
  is constant, the check is done at compile time, with the error
  message provided by "chkerror".  If "adjustlow" is true, the
  constant value will be changed by the value of the low bound to give
  a range of 0..(high-low).  This is used for index range checks
  only.  If the operand is not constant, literal high and low values
  are written to the intermediate file for use by the rangechkop
  which will be generated by the main "genunary" routine.
}

  var
    outofbounds: boolean; {can't possibly be in range}
    upperneeded: boolean; {upper limit check needed}
    lowerneeded: boolean; {lower limit check needed}
    typeptr: entryptr; {for access to type}
    lowerbound: integer; {lower bound of typeindex}
    upperbound: integer; {upper bound of typeindex}


  begin {foldchk}
    if bigcompilerversion then typeptr := @(bigtable[oprndstk[sp].typeindex]);
    lowerbound := lower(typeptr);
    upperbound := upper(typeptr);
    if typeptr^.typ in [subranges, chars, bools, ints, scalars] then
      begin
      checkrange(oprndstk[sp], false, outofbounds, lowerneeded, upperneeded);
      if oconst then
        begin
        if outofbounds then warnbefore(chkerror);
        if adjustlow then
          oprndstk[sp].cvalue.intvalue := oprndstk[sp].cvalue.intvalue -
                                          lowerbound;
        end
      else if generate and (upperneeded or lowerneeded) or adjustlow and
              (lowerbound <> 0) then
        begin
        foldedunary := false;
        pushint(lowerbound);
        genoprnd;
        pushint(upperbound);
        genoprnd;
        if adjustlow then
          with result_range do
            begin
            optimistic.minlimit := optimistic.minlimit - lowerbound;
            optimistic.maxlimit := optimistic.maxlimit - lowerbound;
            pessimistic := optimistic;
            end;
        end;
      end
    else foldedunary := generate;
  end {foldchk} ;


procedure foldunary;

{ Check the operands of those unary operations which can possibly be
  folded and fold the operation if possible.  Note: in the case of
  range checks, this may result in some intermediate file code being
  generated for the range limit values if the operand is not a constant.
}


  begin {foldunary}
    foldedunary := true;
    case unaryop of
      pushvalue: foldpush;
      setelt: foldsetelt;
      notop: foldnot;
      negop: foldneg;
      remop: foldrem;
      quoop: foldquo;
      float: foldfloat;
      float_double: fold_double_float;
      chrstrop, arraystrop: foldchrarraystr;
      indxchkop: foldchk(true, indexerror, switchcounters[indexcheck] > 0);
      congruchkop: foldchk(false, indexerror, switchcounters[indexcheck] > 0);
      rangechkop: foldchk(false, rangeerror, switchcounters[rangecheck] > 0);
      otherwise foldedunary := false
      end;
  end {foldunary} ;

          
procedure genunary(op: operatortype; {operation to generate}
                   form: types {type of operand} );

{ Generate a unary operation.  The operands are first checked to
  see if they can be folded.  If not, the operation is generated with
  the form:
        op(length, cost, form)
  The operand stack is adjusted to reflect the result of the operation.
}


  begin {genunary}

    if sp >= 0 then
      begin

      unaryop := op;
      unaryform := form; {ugh}
      result_range := oprndstk[sp].value_range;

      oconst := constcheck(sp);
      ocost := max(1, computecost(sp));
      olen := oprndstk[sp].oprndlen;
      oextended := oprndstk[sp].extended;

      foldunary;

      if not (op in
         [bldfmt, filebufindrop, float, indrop, indxop, ptrchkop, pushaddr,
         pushvalue, pushfinal, pushproc, paindxop, pindxop, call, callparam,
         unscall, unscallparam, chrstrop, arraystrop]) and (form = ints) then
        begin
        olen := range_length(result_range.optimistic);
        if (not foldedunary and (oprndstk[sp].oprndlen > olen)) or
           (switchcounters[truncatesw] > 0) then
          olen := oprndstk[sp].oprndlen;
        end;

      if not foldedunary then
        begin
        genoprnd;
        sp := sp + 1;
        genop(op);
        genint(olen);
        genint(ocost);
        genform(form);
        end;

      with oprndstk[sp] do
        begin
        typeindex := resulttype;
        value_range := result_range;
        if not foldedunary then
          begin
          operandkind := exproperand;
          cost := ocost;
          oprndlen := olen;
          end;
        end;
      end;
    if bigcompilerversion then resultptr := @(bigtable[resulttype]);
  end {genunary} ;


procedure foldintplusminus(sign: integer {1 if add, -1 if sub} );

{ Fold integer addition or subtraction.  This can be done if both
  operands are constant, or if either has constant value zero.  If
  overflow would result, an error message is generated.  Also,
  if one of the operands has constant value one, the operation is
  converted into an increment or decrement operation.

  **** NOTE ****  if the global "linearize" is set, a constant
  right operand will be placed into the linear factor for use in
  array accessing.
}

  var
    mayoverflow: boolean; {overflow is possible this op}
    ov: boolean; {dummy overflow}


  begin {foldintplusminus}
    if sign > 0 then
      binaryrange(oprndstk[l].value_range, oprndstk[r].value_range, oextended,
                  @add, @usadd, @addrange, result_range, mayoverflow)
    else
      binaryrange(oprndstk[l].value_range, oprndstk[r].value_range, oextended,
                  @subtract, @ussubtract, @subrange, result_range, mayoverflow);

    if (lconst and rconst) then returnresult(mayoverflow)
    else if rconst and linearize then
      begin
      linearfactor := linearfactor + sign * getintvalue(r);
      returnoprnd(l);
      end
    else if lconst and (getintvalue(l) = 0) and (sign > 0) then returnoprnd(r)
    else if rconst and (getintvalue(r) = 0) then returnoprnd(l)
    else if rconst and (getintvalue(r) = 1) then
      begin
      returnoprnd(l);
      if sign = 1 then genunary(incop, binaryform)
      else genunary(decop, binaryform);
      end
    else if lconst and (getintvalue(l) = 1) and (sign > 0) then
      begin
      returnoprnd(r);
      genunary(incop, binaryform)
      end
    else
      begin
      newlen := targetintsize;
      foldedbinary := false;
      end;
  end {foldintplusminus} ;


procedure foldrealplusminus(sign: integer {1 if add, -1 if sub} );

{ Fold addition or subtraction for real operands.  This is possible if
  both operands are constant, or if either operand is constant zero.
  The case where the left operand is constant zero and the operation is
  subtraction could be folded to a negate operation, but this is not
  done at the present.
}


  begin {foldrealplusminus}
    if not realfolding then foldedbinary := false
    else if (lconst and rconst) then
      returnreal(getrealvalue(l) + getrealvalue(r) * sign)
    else if lconst and (getrealvalue(l) = 0.0) and (sign > 0) then
      returnoprnd(r)
    else if rconst and (getrealvalue(r) = 0.0) then returnoprnd(l)
    else foldedbinary := false;
  end {foldrealplusminus} ;


procedure foldstringplus;

{ Fold string concatenation operator.

  Actually, we don't fold at all at this point but we do create the
  new type created by the operation!
}


  begin {foldstringplus}
    foldedbinary := false;
    newlen := min(maxstrlen + 1,
                  oprndstk[l].oprndlen + oprndstk[r].oprndlen - 1);
    newstringtype(resulttype, strings, newlen - 1); {newstringtype adds 1}
  end {foldstringplus} ;


procedure foldplusminus(sign: integer {1 if add, -1 if sub} );

{ Fold addition and subtraction.
}


  begin {foldplusminus}
    if binaryform = strings then foldstringplus
    else if (binaryform = ints) or (binaryform = subranges) then
      foldintplusminus(sign)
    else if binaryform = reals then foldrealplusminus(sign) {!!!}
    else foldedbinary := false;
  end {foldplusminus} ;


procedure foldintmul;

{ Fold an integer multiply if possible.  This can be done if both operands
  are constant, or if either is constant one.  Also, if an operand is
  a constant power of two the operation can be converted into a shift.
  Overflow is possible, and will result in a compile-time message.

  There are several optimizations which are possible but not yet
  incorporated into the routine.  For instance, negative constant
  multipliers could be handled.  Also, the case of the left being
  constant is not handled.
}

  var
    power2: boolean; {set if operand is power of 2}
    power2value: integer; {resulting shift distance}
    mayoverflow: boolean; {operation might overflow}
    ov: boolean; {dummy overflow value}


  begin {foldintmul}
    binaryrange(oprndstk[l].value_range, oprndstk[r].value_range, oextended,
                @multiply, @usmultiply, @mulrange, result_range, mayoverflow);

    if (lconst and rconst) then returnresult(mayoverflow)
    else if lconst and (getintvalue(l) = 1) then returnoprnd(r)
    else if rconst and (getintvalue(r) = 1) then returnoprnd(l)
    else if rconst then
      begin
      power2check(getintvalue(r), power2, power2value);
      if power2 then
        begin
        returnoprnd(l);
        pushint(power2value);
        binaryop := shiftlop;
        foldedbinary := false;
        end
      else foldedbinary := false;
      end
    else foldedbinary := false;
    if not foldedbinary then newlen := targetintsize;
  end {foldintmul} ;


procedure foldrealmul;

{ Fold a real multiply.  This is possible if both operands are constant,
  or if either is constant one.
}


  begin {foldrealmul}
    if not realfolding then foldedbinary := false
    else if (lconst and rconst) then
      returnreal(getrealvalue(l) * getrealvalue(r))
    else if lconst and (getrealvalue(l) = 1.0) then returnoprnd(r)
    else if rconst and (getrealvalue(r) = 1.0) then returnoprnd(l)
    else foldedbinary := false;
  end {foldrealmul} ;


procedure foldmul;

{ Fold a multiply operation if possible.
}


  begin {foldmul}
    if (binaryform = ints) or (binaryform = subranges) then foldintmul
    else if binaryform = reals then foldrealmul
    else foldedbinary := false;
  end {foldmul} ;


procedure folddiv;

{ Check the operands of a div operand and see if they can be folded.
  Since this operation is always followed by a remainder or quotient
  operation to extract the needed part of the answer, it does not
  actually do the folding.  It sets the global flag "divfolded" which
  informs the "genunary" routine that folding is possible.  If the
  divisor is an even power of two, and the left operand is a positive
  subrange (i.e. 1..10), that power is substituted in
  that operand, and will be converted to a shift or and operation.
}

  var
    power2: boolean; {true if divisor is a power of 2}
    power2value: integer; {divisor exponent value}
    typeptr: entryptr; {for access to typeindex data}
    rangenonneg: boolean; {range appears to be nonnegative}


  begin {folddiv}
    if freemodwithdiv then
      begin
      if rconst and not lconst then
        begin
        power2check(getintvalue(r), power2, power2value);
        with oprndstk[l] do
          begin
          if bigcompilerversion then typeptr := @(bigtable[typeindex]);
          rangenonneg := value_range.optimistic.minlimit >= 0;
          if power2 and (rangenonneg or unsigned(typeptr, oprndlen, false)) then
            oprndstk[r].cvalue.intvalue := - power2value
          else foldedbinary := false;
          divfolded := foldedbinary;
          end;
        end
      else foldedbinary := lconst and rconst;
      if not foldedbinary then newlen := targetintsize;
      divide_range := oprndstk[l].value_range;
      divide_extended := oextended;
      end;
  end {folddiv} ;


procedure foldslash;

{ Fold a real divide.  This will be possible if both are constant, or if
  the left is constant zero, or the right is constant one.
  If this would cause an error, it is left until runtime for it to occur.
}


  begin {foldslash}
    if not realfolding then foldedbinary := false
    else if rconst and (getrealvalue(r) = 0.0) then warnbefore(overflow)
    else if (lconst and rconst) then
      returnreal(getrealvalue(l) / getrealvalue(r))
    else if rconst and (getrealvalue(r) = 1.0) then returnoprnd(l)
    else foldedbinary := false;
  end {foldslash} ;


procedure foldcmp(op: operatortype {operation being folded} );

{ Fold moves and compares.  If both operands are constant (obviously
  not a move), the comparison is folded into a boolean constant so
  that travrs can "deaden" any code that depends on NOT the result.
  If only one operand is constant, the move or compare operation is
  folded to a movelit or cmplit of some sort.
}

  var
    i: integer; {constant values for compare}
    uns: boolean; {use unsigned comparisons on integers}
    f: entryptr; {to get at a form entry}
    temp: operand; {used for swapping operands}
    result: boolean; { result of folded compare }


  begin {foldcmp}
    foldedbinary := false;
    if rconst and lconst and
       (binaryform in
       [bools, chars, ints, ptrs, reals, doubles, scalars, subranges]) then
      begin
      case binaryform of
        bools, chars, ints, scalars, subranges:
          begin
          uns := oprndstk[l].value_range.optimistic.extended or
                 oprndstk[r].value_range.optimistic.extended;
          if uns then uscompareint(getintvalue(l), getintvalue(r), result, op)
          else compareint(getintvalue(l), getintvalue(r), result, op);
          foldedbinary := true;
          end;
        ptrs:
          begin
          result := op = eqlit; {both NIL}
          foldedbinary := true;
          end;
        reals, doubles:
          if realfolding then
            begin
            comparereal(getrealvalue(l), getrealvalue(r), result, op);
            foldedbinary := true;
            end;
        otherwise {foldedbinary := false} ;
        end {case binaryform} ;
      if foldedbinary then
        begin
        returnint(ord(result), false);
        setconstrange(ord(result), false, result_range);
        newresulttype(boolindex);
        binaryform := bools;
        oprndstk[sp].oprndlen := unitsize;
        end;
      end;

    { Correct for mismatched length in case of the empty set }
    if binaryform = sets then
      newlen := min(oprndstk[l].oprndlen, oprndstk[r].oprndlen);

    if (rconst <> lconst) then
      begin
      foldedbinary := true;
      if ((binaryform = strings) or (binaryform = arrays)) then
        newlen := resultptr^.arraymembers
        {???}
      else if binaryform = ints then
        if range_length(oprndstk[l].value_range.optimistic) >
           range_length(result_range.optimistic) then
          result_range := oprndstk[l].value_range;
      {???}
      if not rconst and
         (binaryform in
         [bools, chars, ints, ptrs, reals, doubles, scalars, subranges]) then
        begin {reverse the operands}
        temp := oprndstk[l];
        oprndstk[l] := oprndstk[r];
        oprndstk[r] := temp;
        lconst := false;
        rconst := true;
        {and reverse any comparisons}
        case binaryop of
          lssop:
            begin
            op := gtrlit;
            binaryop := gtrop;
            end;
          leqop:
            begin
            op := geqlit;
            binaryop := geqop;
            end;
          gtrop:
            begin
            op := lsslit;
            binaryop := lssop;
            end;
          geqop:
            begin
            op := leqlit;
            binaryop := leqop;
            end;
          otherwise {no change} ;
          end {case} ;
        end;
      case binaryform of
        bools, chars, ints, scalars, subranges: i := getintvalue(r);
        ptrs: i := niladdressvalue;
        reals, doubles:
          begin
          if realfolding then foldedbinary := getrealvalue(r) = 0.0
          else foldedbinary := false;
          i := 0;
          end;
        otherwise foldedbinary := false;
        end;
      if foldedbinary then
        begin
        genlit(i);
        returnoprnd(l);
        genunary(op, binaryform);
        end;
      end;
  end {foldcmp} ;


procedure foldmove;

{ Fold moves, folding to a movelit if the right operand is constant.
}

  var
    i: integer; {constant values for move}
    f: entryptr; {to get at a form entry}


  begin {foldmove}
    foldedbinary := false;

    { Correct for mismatched length in case of the empty set }
    if binaryform = sets then
      newlen := min(oprndstk[l].oprndlen, oprndstk[r].oprndlen);

    if rconst then
      begin
      foldedbinary := true;
      {???}
      if binaryform = ints then
        if range_length(oprndstk[l].value_range.optimistic) >
           range_length(result_range.optimistic) then
          result_range := oprndstk[l].value_range;
      {???}
      case binaryform of
        bools, chars, ints, scalars, subranges: i := getintvalue(r);
        ptrs: i := niladdressvalue;
        reals, doubles:
          begin
          if realfolding then foldedbinary := getrealvalue(r) = 0.0
          else foldedbinary := false;
          i := 0;
          end;
        otherwise foldedbinary := false;
        end;
      if foldedbinary then
        begin
        genlit(i);
        returnoprnd(l);
        genunary(movelit, binaryform);
        end;
      end;
  end {foldmove} ;


procedure foldsetpair;

{ Fold a set constructor of the form a..b into a constant set.  This is
  possible only if both are constant.
}

  var
    i: integer; {induction var}


  begin {foldsetpair}
    foldedbinary := lconst and rconst;
    if foldedbinary then
      begin
      with oprndstk[sp - 2].cvalue do
        if (getintvalue(sp - 1) >= 0) and (getintvalue(sp - 1) <= maxsetord) and
           (getintvalue(sp) >= 0) and (getintvalue(sp) <= maxsetord) then
          setvalue^ := setvalue^ + [getintvalue(sp - 1)..getintvalue(sp)]
        else warnbefore(bigsetbase);
      sp := sp - 1;
      oprndstk[sp].operandkind := exproperand;
      oprndstk[sp].cost := 0;
      end;
  end {foldsetpair} ;


procedure foldand;

{ Fold integer and boolean 'and' operator.
}

  var
    i: integer; {constant value}


  begin {foldand}
    andrange(oprndstk[l].value_range.optimistic,
             oprndstk[r].value_range.optimistic, result_range.optimistic);
    andrange(oprndstk[l].value_range.pessimistic,
             oprndstk[r].value_range.pessimistic, result_range.pessimistic);

    if lconst and rconst then returnresult(false)
    else if lconst then
      begin
      i := getintvalue(l);
      if (binaryform = bools) and (i = 1) or (binaryform = ints) and
         (i = - 1) then
        returnoprnd(r)
      else foldedbinary := false;
      end
    else if rconst then
      begin
      i := getintvalue(r);
      if (binaryform = bools) and (i = 1) or (binaryform = ints) and
         (i = - 1) then
        returnoprnd(l)
      else foldedbinary := false;
      end
    else foldedbinary := false;
  end {foldand} ;


procedure foldor;

{ Fold integer and boolean 'or' operator.
}


  begin {foldor}
    orrange(oprndstk[l].value_range.optimistic,
            oprndstk[r].value_range.optimistic, result_range.optimistic);
    orrange(oprndstk[l].value_range.pessimistic,
            oprndstk[r].value_range.pessimistic, result_range.pessimistic);
    if lconst and rconst then returnresult(false)
    else if lconst and (binaryform in [bools, ints]) and
            (getintvalue(l) = 0) then
      returnoprnd(r)
    else if rconst and (binaryform in [bools, ints]) and
            (getintvalue(r) = 0) then
      returnoprnd(l)
    else foldedbinary := false;
  end {foldor} ;


procedure foldbinary;

{ Fold a binary operation if this is possible.  Only constant operands
  may be discarded, since intermediate code for expressions is generated
  on the fly.  This limits folding possibilities slightly, since expressions
  like "a and false" (with "a" boolean) can not be discarded.
}


  begin {foldbinary}
    foldedbinary := true;
    case binaryop of
      plusop: foldplusminus(1);
      minusop: foldplusminus( - 1);
      mulop: foldmul;
      divop, stddivop: folddiv;
      stdmodop, modop: foldmod;
      kwoop: foldkwo;
      slashop: foldslash;
      moveop: foldmove;
      lssop: foldcmp(lsslit);
      leqop: foldcmp(leqlit);
      gtrop: foldcmp(gtrlit);
      geqop: foldcmp(geqlit);
      eqop: foldcmp(eqlit);
      neqop: foldcmp(neqlit);
      setpair: foldsetpair;
      andop: foldand;
      orop: foldor;
      otherwise foldedbinary := false
      end
  end {foldbinary} ;



procedure genbinary(op: operatortype; {operation to generate}
                    form: types {type of operands} );

{ Generate intermediate file output for a binary operation.  If possible,
  the operation will be folded.  The operand stack is updated to reflect
  the result of the operation.
}

  var
    lrange, rrange: addressrange; {size of operands, based on range}
    comparision: boolean; {use different length analysis if op is comparision}
    same_signedness: boolean; {true if l and r both unsigned/signed}


  begin {genbinary}

    if sp >= 1 then
      begin
      l := sp - 1;
      r := sp;
      lconst := constcheck(l);
      rconst := constcheck(r);
      c1 := computecost(l);
      c2 := computecost(r);
      result_range := oprndstk[r].value_range;
      newlen := max(oprndstk[l].oprndlen, oprndstk[r].oprndlen);
      if c1 = c2 then newcost := c1 + 1
      else newcost := max(c1, c2);
      oextended := oprndstk[l].extended or oprndstk[r].extended;
      binaryop := op; {may be modified by foldbinary}
      binaryform := form; {may be modified by foldbinary}

      foldbinary;

      if (binaryform = ints) and
         not (binaryop in [indxop, aindxop, addrop, indrop] {addressing
           operators} ) then
        begin
        lrange := range_length(oprndstk[l].value_range.optimistic);
        rrange := range_length(oprndstk[r].value_range.optimistic);
        if switchcounters[truncatesw] <= 0 then
          begin
          newlen := range_length(result_range.optimistic);
          if divfolded or not foldedbinary then
            begin
            if lrange > newlen then newlen := lrange;
            if rrange > newlen then newlen := rrange;
            end;
          end
        else {truncate} newlen := max(lrange, rrange);
        end;
      if not foldedbinary then
        begin
        genoprnd;
        genoprnd;
        if lconst then genop(switchstack);
        sp := sp + 1;
        genop(binaryop);
        genint(newlen);
        genint(newcost);
        genform(binaryform);
        end;

      with oprndstk[sp] do
        begin
        typeindex := resulttype;
        value_range := result_range;
        if not foldedbinary then
          begin
          oprndlen := newlen;
          operandkind := exproperand;
          cost := newcost;
          end;
        end;
      end;
    if bigcompilerversion then resultptr := @(bigtable[resulttype]);
  end {genbinary} ;


procedure setdefaulttargetintsize;

{ Used to force operand to be of length "defaulttargetintsize".  In
  particular, integer arguments to many support library routines must
  be extended to this length.
}


  begin {setdefaulttargetintsize}
    oprndstk[sp].oprndlen := defaulttargetintsize;
  end {setdefaulttargetintsize} ;


procedure genpushint(i: integer);

{ Cause i to be pushed at runtime.
}


  begin {genpushint}
    pushint(i);
    genunary(pushvalue, ints);
  end {genpushint} ;


procedure genpushdefaultint(i: integer);

{ Cause i to be pushed at runtime.
}


  begin {genpushdefaultint}
    pushint(i);
    setdefaulttargetintsize;
    genunary(pushvalue, ints);
  end {genpushdefaultint} ;


procedure genpushbool(b: boolean);

{ Cause b to be pushed at runtime.
}


  begin {genpushbool}
    pushint(ord(b));
    oprndstk[sp].typeindex := boolindex;
    oprndstk[sp].oprndlen := 1;
    genunary(pushvalue, bools);
  end {genpushbool} ;


procedure computeresult(maybestring: boolean {force char to string?});

{ Check the two operands of a binary operation for type compatibility.
  If one side is real or double and the other integer, the integer operand is
  converted to real or double before the check.  Real is also promoted to
  double when mixed.

  The conversion is complicated by the fact that the code to access the
  left operand was generated before the decision could be made.  This
  is handled by issuing a "switchstack" operator in the intermediate
  file, doing the float, and issuing another "switchstack" to restore
  the state.

  The maybestring flag is a kludge to help in cases like 'a' + 'b', i.e.
  two chars which the programmer imagines to be strings.  It is true when
  ever the operands MIGHT be allowable as strings.
}

  type
    castreal = record
      case boolean of
        true:  (b: realarray);
        false: (s: real)
    end;

  var
    lefttype, righttype: index; {formentry's for operands}
    f: entryptr; {used for access to formentries}
    leftshortstring, rightshortstring: boolean; {left or right is/are short
                                                 'standard' strings}
    leftstringtype: boolean; {left type is packed array [1..n] of char}
    leftform, rightform: types; {operand types}
    tival: integer; {temporary integer value}
    bscast: castreal;

  begin {computeresult}
    if sp > 0 then lefttype := oprndstk[sp - 1].typeindex
    else lefttype := noneindex;
    if sp >= 0 then righttype := oprndstk[sp].typeindex
    else righttype := noneindex;
    newresulttype(noneindex);
    if bigcompilerversion then f := @(bigtable[lefttype]);
    leftform := getform(f);
    leftstringtype := (leftform = arrays) and f^.stringtype;
    leftshortstring := leftstringtype and (f^.arraymembers = 1);
    if bigcompilerversion then f := @(bigtable[righttype]);
    rightform := getform(f);
    rightshortstring := (rightform = arrays) and f^.stringtype and
                        (f^.arraymembers = 1);
    if leftshortstring and (rightform = chars) and
       (switchcounters[standard] <= 0) then
      begin
      newstringtype(resulttype, arrays, 1);
      newresulttype(resulttype);
      oprndstk[sp].typeindex := resulttype;
      end
    else if rightshortstring and (leftform = chars) and
            (switchcounters[standard] <= 0) then
      begin
      newstringtype(resulttype, arrays, 1);
      newresulttype(resulttype);
      oprndstk[sp - 1].typeindex := resulttype;
      end
    else
      begin
      if (maybestring and ((leftform = chars) or leftstringtype) or
         (leftform = strings)) and (rightform = chars) then
        begin
        genunary(chrstrop, strings);
        righttype := resulttype;
        rightform := strings;
        end;
      if (maybestring and ((leftform = chars) or leftstringtype) or
         (leftform = strings)) and (rightform = arrays) and
         f^.stringtype then
        begin
        genunary(arraystrop, strings);
        righttype := resulttype;
        rightform := strings;
        end;
      if (rightform = strings) and (leftstringtype or
         (leftform = chars)) then
        begin
        sp := sp - 1;
        if not constcheck(sp) and not constcheck(sp + 1) then
          genop(switchstack);
        if leftstringtype then genunary(arraystrop, strings)
        else genunary(chrstrop, strings);
        if not constcheck(sp) and not constcheck(sp + 1) then
          genop(switchstack);
        sp := sp + 1;
        end
      else if (leftform = ints) and (rightform = reals) then
        begin
        if not constcheck(sp - 1) or not realfolding then
          begin
          if not constcheck(sp) and not constcheck(sp - 1) then
            genop(switchstack);
          sp := sp - 1;
          genunary(float, ints);
          sp := sp + 1;
          if not constcheck(sp) then genop(switchstack);
          end
        else if realfolding then
          with oprndstk[sp - 1], cvalue do
            begin
            tival := intvalue;
            typeindex := realindex;
            representation := reals;
            realvalue.realbinary := tival;
            end;
        newresulttype(realindex);
        end
      else if (leftform = reals) and (rightform = ints) then
        begin
        genunary(float, ints);
        newresulttype(realindex);
        end
      else if (leftform = ints) and (rightform = doubles) then
        begin
        if not constcheck(sp - 1) or not realfolding then
          begin
          if not constcheck(sp) and not constcheck(sp - 1) then
            genop(switchstack);
          sp := sp - 1;
          genunary(float_double, ints);
          sp := sp + 1;
          if not constcheck(sp) then genop(switchstack);
          end
        else if realfolding then
          with oprndstk[sp - 1], cvalue do
            begin
            tival := intvalue;
            typeindex := doubleindex;
            representation := doubles;
            realvalue.realbinary := tival;
            end;
        newresulttype(doubleindex);
        end
      else if (leftform = doubles) and (rightform = ints) then
        begin
        genunary(float_double, ints);
        newresulttype(doubleindex);
        end
      else if (leftform = reals) and (rightform = doubles) then
        begin
        if not constcheck(sp - 1) or not realfolding then
          begin
          if not constcheck(sp) and not constcheck(sp - 1) then
            genop(switchstack);
          sp := sp - 1;
          oprndstk[sp].oprndlen := doublesize;
          genunary(real_to_dbl, reals);
          sp := sp + 1;
          if not constcheck(sp) then genop(switchstack);
          end
        else
          with oprndstk[sp - 1], cvalue do
            begin
            bscast.b := realvalue.realbuffer;
            realvalue.realbinary := bscast.s;
            typeindex := doubleindex;
            representation := doubles;
            end;
        newresulttype(doubleindex);
        end
      else if (leftform = doubles) and (rightform = reals) then
        begin
        oprndstk[sp].oprndlen := doublesize;
        genunary(real_to_dbl, reals);
        newresulttype(doubleindex);
        end
      else if compatible(lefttype, righttype) then newresulttype(lefttype)
      else warnbefore(typesincomp)
      end;
  end {computeresult} ;


procedure gencheck(op: operatortype; {operator to generate}
                   checktype: index {type for check} );

{ If the top of the operand stack can be checked, check it.
}

  var
    generate: boolean; {actually generate a check}
    checkptr: entryptr; {for access to checktype entry}


  begin {gencheck}
    if bigcompilerversion then checkptr := @(bigtable[checktype]);
    with checkptr^ do
      if typ = subranges then
        generate := (lowerord <> 0) or (upperord <> maxusint)
      else generate := typ in [bools, chars, scalars];
    if generate then
      begin
      oprndstk[sp].typeindex := checktype;
      genunary(op, ints);
      end;
  end {gencheck} ;


procedure setshorttargetintsize;

{ Used to force operand to be of length "shorttargetintsize".  Certain
  support library routines (string intrinsics, at least) don't need full
  integers.  Checking code is emitted to ensure that nothing naughty goes
  on behind our backs.
}


  begin {setshorttargetintsize}
    gencheck(rangechkop, shortintindex);
    oprndstk[sp].oprndlen := shorttargetintsize;
  end {setshorttargetintsize} ;


function checkforstack(varindex: index; {variable to check}
                       var where: forstackindex {for level} ): boolean;

{ Check a variable to see if it is being used as the controlled variable
  in a for statement.  As for statements are encountered, the index of the
  controlled variable is stored in a stack in the compiler.  This allows the
  compiler to check for the assignment rules, and to allocate this variable
  to a register for the duration of the for statement.
}

  var
    i: forstackindex; {induction var for search}


  begin {checkforstack}
    forstack[0].forindex := varindex;
    i := forsp;
    while forstack[i].forindex <> varindex do i := i - 1;
    where := i;
    checkforstack := (i <> 0);
  end {checkforstack} ;


procedure getlevel(lev: levelindex; {level to reference}
                   param: boolean {true if parameter} );

{ Generate the intermediate file code for a reference to the level
  requested.  The operand stack is set up with an address operand.

  Code generated:

        get-level = "globalop" | "originop" |
                ( "localop"
                  [* "levop(level, [0 | blockref])" *] )  .

  Note: origin code depends on level 0 being equal to a dummy
  absolute 0 address reference
}

  var
    i: levelindex; {induction var for levop generation}


  begin {getlevel}
    if lev = 0 then
      begin
      genop(levop);
      genint(0);
      genint(0);
      end
    else if lev = 1 then genop(globalop)
    else
      begin
      if not param or (lev < level) then genop(localop);
      for i := level - 1 downto lev + 1 do
        begin
        genop(levop);
        genint(i);
        genint(0);
        end;
      if param or (lev < level) then
        begin
        genop(levop);
        genint(lev);
        if param then genint(blockref)
        else genint(0);
        end;
      end;
    bumpsp;
    with oprndstk[sp] do
      begin
      typeindex := intindex;
      oprndlen := ptrsize;
      extended := false;
      operandkind := exproperand;
      cost := 0
      end;
  end {getlevel} ;

procedure setnowunpacking(packedflag: boolean; {packed structure?}
                          offset: addressrange; {start of field}
                          var nowunpacking: boolean {result} );

{ Check a field being accessed and set "nowunpacking" if we need to
  generate unpacking code.  It is possible for the structure to be packed,
  but the particular field falls on a word boundry, and no unpacking code
  is necessary.

  This routine uses the global "resultptr" to determine size.
}


  begin {setnowunpacking}
    nowunpacking := packedflag and ((offset mod bitsperunit <> 0) or
                    ((offset + sizeof(resultptr, true)) mod bitsperunit <> 0));
  end {setnowunpacking} ;


procedure startunpacking(nowunpacking: boolean; {unpacking now?}
                         var constpart: addressrange; {const address}
                         var unpacking: boolean {been unpacking?} );

{ If we are just beginning to unpack a field, generate the constant part
  of the word address and set the "unpacking" flag.  The constpart will
  now be a bit address, so is set to zero.
}


  begin {startunpacking}
    if nowunpacking and not unpacking then
      begin
      if constpart <> 0 then
        begin
        genlit(constpart);
        oprndstk[sp].oprndlen := unitsize;
        genunary(indxop, ints);
        end;
      constpart := 0;
      unpacking := true;
      end;
  end {startunpacking} ;


procedure lastindex(var unpacking: boolean; {unpacking a field}
                    constpart: addressrange; {addr const part}
                    var len: addressrange {operand length} );

{ Generates the last "indxop" in accessing a variable.

  Output generated:

        "lit(constpart)"
        ( pindxop(len, cost, 'ints')" |
          indxop(len, cost, 'ints')" )  .
}

  var
    power2: boolean;
    power2dummy: integer;


  begin {lastindex}
    if not unpacking and (resultptr^.typ = subranges) then {we have an
                                                           inconveniently-sized
                                                            scalar entity}
      begin
      power2check(len, power2, power2dummy);
      if not power2 then
        begin
        len := len * bitsperunit;
        startunpacking(true, constpart, unpacking);
        end;
      end;
    if unpacking then
      begin
      genlit(constpart);
      oprndstk[sp].oprndlen := len;
      genunary(pindxop, ints);
      end
    else
      begin
      genlit(constpart);
      oprndstk[sp].oprndlen := len;
      genunary(indxop, ints);
      end;
  end {lastindex} ;


procedure updatelabelnest;

{ Called when exiting a nested control structure to update maxlegalnest
  field of all labels defined or referenced at the current nesting level.
  The effect is to make all future references at this level to such a
  label illegal.  We also adjust jumpoutnest if a label was referenced
  that has not yet been defined, since it's too late for it to be defined
  in this nesting level.
}

  var
    p: labelptr; { used to traverse the list of labels active in this block }


  begin {updatelabelnest}
    p := display[level].labellist;
    while p <> labelflag do
      with p^ do
        begin
        if (nest = maxlegalnest) and (definednest = 0) then
          jumpoutnest := min(jumpoutnest, nest - 1);
        if (nest = maxlegalnest) or (nest = definednest) then
          maxlegalnest := nest - 1;
        p := nextlabel;
        end;
  end {updatelabelnest} ;


procedure statement(follow: tokenset {legal following symbols} );

{ Syntactic routine to parse a statement.

  Productions:

  statement = [ label ":" ] ( empty-statement | assignment-statement |
        procedure-statement | goto-statement | compound-statement |
        if-statement | case-statement | repeat-statement |
        while-statement | for-statement | with-statement  .

  The main statement routine looks at the first token of the statement and
  calls the proper statement-specific routine to parse that statement kind.
}


  procedure expression(follow: tokenset; {legal following sym}
                       arrayopt: boolean {true if array opt wanted} );
    forward;

{ Parse an expression in which the first factor is possibly already parsed.
  See the body for details.
}


  procedure variable(variantok: boolean; {true if case selector ok}
                     packedok: boolean; {packed ok in context}
                     forindexallowed: boolean; {for index ok in context}
                     newflag: boolean; {newvarop to travrs}
                     parsing: boolean; {true if parsing a variable}
                     varindex: index);
    forward;

{ Parse a variable.  See the body for details
}


  procedure genvalsize(elttype: index; {element to get size of}
                       eltsize: addressrange {size of fixed element} );

{ Generate code for the size of a value.  If the type is a conformant
  array parameter, this involves generating code to compute the size.
  If it is an ordinary value, it is just taken from the form entry.

  Note that for a conformant array parameter, the number of elements
  is pushed before the element size.  This allows folding of the
  element size if possible.  The other order allows more common
  expressions, but multiple dimensions on arrays are rare, so this
  is less important.
}

    var
      lowid: index; {index of lowbound for a conformant array}
      et: index; {element type id}
      es: addressrange; {element size in addressing units}
      f: entryptr; {for access to elementtype}
      packedaccess: boolean; {this is a packed conformant array}


    begin {genvalsize}
      packedaccess := false;
      if bigcompilerversion then f := @(bigtable[elttype]);
      with f^ do
        if typ = conformantarrays then
          begin
          lowid := lowbound;
          et := elementtype;
          es := elementsize;
          if packedflag and (es > 0) then
            if es < bitsperunit then
              begin
              packedaccess := true;
              es := bitsperunit div es;
              end
            else es := es div bitsperunit;
          variable(true, false, false, false, false, highbound);
          variable(true, false, false, false, false, lowid);
          genbinary(minusop, ints);
          genunary(incop, ints);
          genvalsize(et, es);
          if packedaccess then
            begin
            case targetmachine of
              ns32k: genbinary(kwoop, ints);
              otherwise
                begin
                genbinary(divop, ints);
                genunary(quoop, ints);
                end;
              end;
            end
          else genbinary(mulop, ints);
          end
        else pushint(eltsize);
    end {genvalsize} ;


  procedure selector(variantok: boolean; {true if case selector ok}
                     nowvariant: boolean; {true if now a case selector}
                     packedok: boolean; {packed field is ok}
                     var unpacking: boolean; {unpacking a field?}
                     var constpart: addressrange; {const part of addr}
                     var len: addressrange; {operand length}
                     var off: addressrange; {offset for travrs}
                     var varlev: levelindex; {varlev for travrs}
                     var ownvar: boolean {own variable flag for travrs} );

{ Syntactic routine to parse a selector.

  Productions:

  selector = ( array-selector | field-selector | ptr-selector )
        [* selector *]  .

  array-selector = "[" expression [* "," expression *] "]"  .

  field-selector = "." field-identifier  .

  ptr-selector = "^"  .

  Output generated:

        selector = indexing | field | ptr-ref .

        field = [ "lit(constantpart)" "indxop(len, cost, 'int')" ]  .

        ptr-ref = "lit(constantpart)" "indxop(len, cost, 'int')
                "varop('ptrsize', lev, off, ownvar)" "indrop(len, cost, 'int')"
                [ "indrop(len, cost, 'int')" ]  .

  Indexing will be described in the routine "onearrayindex".

  This routine parses all variable selectors, and has a few places
  where non-obvious output is generated to make "travrs" work better.
  In particular, the "varop(size, lev, off, ownvar)" includes the "lev",
  "off" and "ownvar" fields entirely to allow travers to invalidate references
  to that variable.  The "off" field is the start of the variable, so that
  for an array reference the entire array is invalidated by a reference
  to any element of the array.

  This routine is concerned with "off" only because pointers and files
  have a fake offset associated with the type, and all references to
  pointers with the same type are invalidated.  As this routine
  generates code to de-reference pointers, it may also have to change
  the "lev" and "off" values to those for the referenced type.

  In addition, files are implemented as pointers to a file control
  table, so buffer variable references have an additional "indrop" added.
}

    var
      textfile: boolean; {set true if "^" applied to text file}
      nowunpacking: boolean; { true if now unpacking }
      indexwanted: index; { index type, used by onearrayindex}
      arrayelement: index; { element type, used by onearrayindex}
      p, p1: entryptr; {used for access to name entry}
      fieldindex: index; {field selected}
      newoff: integer; {new "off" for pointer or file refs}


    procedure onearrayindex;

{ Syntactic routine to parse a single array index.   The syntax is
  trivial, it simply parses an expression.  If the next token is a
  comma, it is effectively changed to "]" "[" to keep the parsing
  of the array indexing going.

  The variables "linearize" and "linearfactor" are used to simplify
  the optimization of array indices of the form a[i+1].  If "linearize"
  is set true, then constant terms are added into "linearfactor" so
  they can be included in the constant offset of the array being
  indexed.  "Linearize" is manipulated to be on only when parsing
  an array index.

  "Linearize" and "linearfactor" are local to the routine "statement",
  which is global to all expression parsing routines.  This is because
  they are needed by genbinary, which is outside the "expression" routine,
  and it would be very inconvenient to have to pass them as parameters
  on all calls.  The price paid is some rather hard to understand code
  to manipulate them.

  Output generated:

        indexing = expression [ "lit(min)" "lit(max)"
                "indxchkop(len, cost, form)" ]
                ( ( "lit(min)" "minusop(len, cost, 'int')"
                "paindxop(len, cost, form)" ) |
                ( "lit(size)" "mulop(len, cost, 'int')"
                "aindxop(len, cost, form)" ) )  .

  The above is not quite complete, as multiplies and adds may be folded,
  but it gives the right idea.

}

      var
        eltsperunit: 0..bitsperunit; {array elts per addressing unit}
        oldlinear: integer; {local save for linearfactor}
        eltsize: integer; {size of an array element}
        alreadyunpacking: boolean; {true if we were unpacking at entry}
        outofbounds: boolean; {set of index out of bounds}
        dum1, dum2: boolean; {dummy arguments for range check}
        indextypeptr: entryptr; {for access to index type entry}
        lowerbound: integer; {lower bound of the array index}

        conformant: boolean; {the array is conformant}
        lowid: index; {lower bound location for a conformant array}
        highid: index; {upper bound location for a conformant array}
        upperout, lowerout: boolean; {bounds could be out}
        power2: boolean; {multiplier is a power of two}
        power2value: integer; {which power of two it is}
        tmaxautoindex: integer; {temp to hold maxautoindex constant}


      begin {onearrayindex}
        case targetmachine of
          mc68000:
          { The mc68020 has scaling, but the mc68000 does not }
            if switcheverplus[cpu68020] then tmaxautoindex := maxautoindex
            else tmaxautoindex := 0;
          otherwise tmaxautoindex := maxautoindex;
          end;
        indexwanted := noneindex;
        arrayelement := noneindex;
        eltsize := 1;
        conformant := resultptr^.typ = conformantarrays;
        if resulttype <> noneindex then
          with resultptr^ do
            if (typ in [strings, arrays]) or conformant then
              begin
              arrayelement := elementtype;
              indexwanted := indextype;
              eltsize := elementsize;
              lowid := lowbound;
              highid := highbound;
              end
            else warn(arrayexpected);
        gettoken;
        nowunpacking := false;
        alreadyunpacking := unpacking;
        if resultptr^.packedflag then
          if eltsize < bitsperunit then nowunpacking := true
          else eltsize := eltsize div bitsperunit;
        startunpacking(nowunpacking, constpart, unpacking);
        linearize := (switchcounters[indexcheck] <= 0) and not unpacking and
                     not conformant and (varlev <> 0);
        oldlinear := linearfactor;
        linearfactor := 0;
        expression(follow + [comma, rbrack, rpar], linearize);
        if not compatible(indexwanted, resulttype) then
          warnbefore(indexincomp);
        linearize := false;
        oprndstk[sp].typeindex := indexwanted;
        if bigcompilerversion then indextypeptr := @(bigtable[indexwanted]);
        lowerbound := lower(indextypeptr);
        if not conformant and (switchcounters[indexcheck] > 0) then
          genunary(indxchkop, ints)
        else
          begin
          checkrange(oprndstk[sp], false, outofbounds, lowerout, upperout);
          if outofbounds then warnbefore(indexerror);
          if unpacking or conformant then
            begin
            if conformant then
              begin
              lev := varlev;
              if switchcounters[indexcheck] > 0 then
                begin
                genoprnd;
                variable(true, false, false, false, false, lowid);
                genoprnd;
                variable(true, false, false, false, false, highid);
                genunary(cindxchkop, ints);
                end
              else
                begin
                variable(true, false, false, false, false, lowid);
                genbinary(minusop, ints);
                end
              end
            else
              begin
              pushint(lowerbound);
              genbinary(minusop, ints);
              end
            end
          else if abs(constpart + (linearfactor - lowerbound) * eltsize) >
                  maxaddr then
            begin
            pushint(linearfactor - lowerbound);
            genbinary(plusop, ints);
            end
          else constpart := constpart + (linearfactor - lowerbound) * eltsize;
          end;
        linearfactor := oldlinear;
        if token = comma then token := lbrack
        else verifytoken(rbrack, badindexerr);
        if constcheck(sp) and not conformant then
          begin
          sp := sp - 1;
          with oprndstk[sp + 1], cvalue do
            if unpacking then
              begin
              eltsperunit := bitsperunit div eltsize;
              if not alreadyunpacking then
                begin
                genlit(constpart + intvalue div eltsperunit);
                oprndstk[sp].oprndlen := eltsize;
                genunary(indxop, ints);
                constpart := 0;
                intvalue := intvalue mod eltsperunit;
                end;
              constpart := constpart + intvalue * eltsize;
              end
            else constpart := constpart + intvalue * eltsize;
          end
        else if unpacking then
          begin
          genoprnd;
          oprndstk[sp].oprndlen := eltsize;
          genunary(paindxop, ints);
          end
        else
          begin
          if not conformant then power2check(eltsize, power2, power2value)
          else power2 := false;
          if power2 and (power2value <= tmaxautoindex) then
            begin
            oprndstk[sp].oprndlen := eltsize;
            oprndstk[sp - 1].oprndlen := eltsize;
            end
          else
            begin
            genvalsize(arrayelement, eltsize);
            genbinary(mulop, ints);
            oprndstk[sp].oprndlen := unitsize;
            oprndstk[sp - 1].oprndlen := unitsize;
            end;
          genbinary(aindxop, ints);
          end;
        newresulttype(arrayelement);
        len := eltsize;
      end {onearrayindex} ;


    begin {selector}
      repeat
        if resultptr^.packedflag and not packedok then
          begin
          warn(varparamerr);
          packedok := true;
          end;
        p1 := resultptr; {we modify resultptr within "with" statement}
        if token = lbrack then
          begin
          onearrayindex;
          while token = lbrack do onearrayindex;
          nowvariant := false;
          end
        else if token = dot then
          begin
          gettoken;
          with p1^ do
            if token = ident then
              begin
              if typ = fields then
                begin
                searchsection(fieldid, fieldindex);
                if fieldindex <> 0 then
                  begin
                  if bigcompilerversion then p := @(bigtable[fieldindex]);
                  newresulttype(p^.vartype);
                  nowvariant := p^.varianttag;
                  setnowunpacking(packedflag, p^.offset, nowunpacking);
                  if nowunpacking then
                    begin
                    if not unpacking then
                      begin
                      constpart := constpart + p^.offset div (bitsperunit *
                                   packingunit) * packingunit;
                      startunpacking(nowunpacking, constpart, unpacking);
                      constpart := constpart + p^.offset mod (bitsperunit *
                                   packingunit);
                      end
                    else constpart := constpart + p^.offset;
                    end
                  else if packedflag and not unpacking then
                    constpart := constpart + p^.offset div bitsperunit
                  else constpart := constpart + p^.offset;
                  if packedflag and not unpacking then
                    len := p^.length div bitsperunit
                  else len := sizeof(resultptr, unpacking);
                  end
                else
                  begin
                  warn(undefidenterr);
                  newresulttype(noneindex);
                  end;
                end
              else if typ <> none then warnbefore(recordexpected);
              gettoken
              end
            else warnbetween(novarerr);
          end
        else if token = uparrow then
          begin
          textfile := resulttype = textindex;
          nowvariant := false;
          with p1^ do
            begin
            if typ = ptrs then
              begin
              if bigcompilerversion then p := @(bigtable[ptrtypename]);
              newresulttype(p^.typeindex);
              newoff := ptrkey;
              end
            else if typ = files then
              begin
              newresulttype(filebasetype);
              newoff := filekey;
              end
            else if typ <> none then
              begin
              warn(ptrexpected);
              newresulttype(noneindex);
              end;
            lastindex(unpacking, constpart, len);
            genop(unsvarop);
            genint(len);
            genint(varlev);
            genint(off);
            genint(ord(ownvar));
            oprndstk[sp].oprndlen := ptrsize;
            if switchcounters[nilcheck] > 0 then genunary(ptrchkop, ints);
            if (typ = files) then
              begin
              genunary(definelazyop, files);
              genunary(filebufindrop, ints); {can't be a cse}
              end;
            genunary(indrop, ints);
            constpart := 0;
            varlev := 0;
            unpacking := false;
            off := newoff;
            ownvar := false;
            len := sizeof(resultptr, false);
            gettoken
            end;
          end;
      until not (token in [lbrack, dot, uparrow]);
      if nowvariant and not variantok then warnnonstandard(novarianttag);
    end {selector} ;


  procedure parameterlist(procindex: index {procedure name entry} );
    forward;

{ Syntactic routine to parse a parameter list, see body for details.
}


  procedure illegalident(varindex: index {index of illegal identifier} );

{ Called when an illegal identifier is found to try to do something sensible
  with the input.  This routine pushes a dummy result to keep the operand
  calculations going, then tries to determine from the next token whether
  it was intended as a variable reference or a procedure reference and
  act accordingly.  There is no worry about the code going out, since code
  emission is turned off by the error.
}

    var
      {all of the following are dummy parameters}
      len, constpart, dummy1: addressrange;
      unpacking: boolean;
      dummy2: levelindex;


    begin {illegalident}
      pushint(0);
      newresulttype(noneindex);
      oprndstk[sp].operandkind := varoperand;
      oprndstk[sp].typeindex := noneindex;
      oprndstk[sp].cvalue.representation := none;
      if varindex = 0 then warn(undefidenterr)
      else warn(wantvarname);
      gettoken;
      if token in [lbrack, dot, uparrow] then
        selector(true, false, true, unpacking, constpart, len, dummy1, dummy2,
                 unpacking)
      else if token = lpar then parameterlist(0);
      newresulttype(noneindex);
      oprndstk[sp].typeindex := noneindex;
      oprndstk[sp].cvalue.representation := none;
    end {illegalident} ;


  procedure illegalassign;

{ Attempt to make some reasonable recovery from a garbled assignment
}


    begin {illegalassign}
      if token = ident then illegalident(varindex)
      else
        begin
        warn(badassign);
        if token in
           [intconst, realconst, dblrealconst, charconst, stringconst] then
          gettoken;
        end;
      if token in [becomes, eql] then
        begin
        if token = eql then warn(nobecomeserr);
        gettoken;
        expression(follow, false);
        genoprnd;
        end
    end {illegalassign} ;


  procedure variable(variantok: boolean; {true if case selector ok}
                     packedok: boolean; {packed ok in context}
                     forindexallowed: boolean; {for index ok in context}
                     newflag: boolean; {newvarop to travrs}
                     parsing: boolean; {true if parsing a variable}
                     varindex: index);

{ Syntactic routine to parse a variable reference.

  Productions:

  variable = identifier [ selection ]  .

  This generates the basic code for a variable access, calling on
  "selector" to handle any qualification, indexing, etc.

  This is complicated by the fact that an apparent variable reference
  could be a field within a with, or a function result.  Also, a variable
  could be being used as a for index, in which case it is accessed in a
  special manner.


  This routine sets the (relatively) global "resulttype" to the type of
  the variable being accessed.
  If "parsing" is false, we are only generating references for an
  existing variable entry, and no parsing will take place.
}

    var
      unpacking: boolean; {we are continuing to unpack}
      nowunpacking: boolean; {either unpacking or should start}
      var_is_valid: boolean; {variable known to contain a legal value}
      ownvar: boolean; {variable is an "own" variable}
      constpart: addressrange; {constant part of var address}
      varlev: levelindex; {level of variable}
      i: forstackindex; {which for index, if it is an index}
      varptr: entryptr; {used to access var name entry}
      off: addressrange; {offset for use by travrs (see selector)}
      len: addressrange; {length of value}


    begin {variable}
      unpacking := false;
      varlev := lev;
      ownvar := false;
      if bigcompilerversion then varptr := @(bigtable[varindex]);
      with varptr^ do
        begin
        if checkforstack(varindex, i) then
          begin
          if not forindexallowed then warn(modifiedfor);
          newresulttype(vartype);
          bumpsp;
          with oprndstk[sp] do
            begin
            typeindex := vartype;
            extended := resultptr^.extendedrange;
            oprndlen := length;
            operandkind := varoperand;
            cost := 0;
            setvarrange(targetintsize, false, true);
            end;
          genop(forindexop);
          genint(i);
          if parsing then gettoken;
          end
        else
          begin {not a for index}
          var_is_valid := false;
          case namekind of
            varname, fieldname, param, varparam, funcparam, procparam,
            confparam, varconfparam, boundid:
              begin
              var_is_valid := (namekind in [param, boundid]) or knownvalid;
              newresulttype(vartype);
              if varlev > level then
                with display[varlev] do
                  begin {within a with}
                  if withpacking and not packedok then
                    begin
                    warn(varparamerr);
                    packedok := true;
                    end;
                  pushdummy;
                  genop(withop);
                  genint(varlev - level);
                  varlev := withlevel;
                  off := withoffset;
                  setnowunpacking(withpacking, offset, nowunpacking);
                  if nowunpacking then
                    begin
                    if not unpacking then
                      begin
                      constpart := offset div (bitsperunit * packingunit) *
                                   packingunit;
                      startunpacking(nowunpacking, constpart, unpacking);
                      end;
                    constpart := constpart + offset mod (bitsperunit *
                                 packingunit);
                    end
                  else if withpacking then constpart := offset div bitsperunit
                  else constpart := offset;
                  if withpacking and not unpacking then
                    len := length div bitsperunit
                  else len := length;
                  end
              else
                begin {not within a with}
                constpart := offset;
                if (varlev = level) then
                  begin {local variable}
                  if not (varalloc in [usealloc, definealloc, sharedalloc]) and
                     not (modified or newflag) and checkundefs and
                     not (anyexternals and (level = 1)) then
                    warn(unassigned);
                  end
                else
                  begin {not local variable}
                  if (varlev > 1) then
                    begin
                    proctable[display[level].blockref].intlevelrefs := true;
                    if (varlev > 2) and (varalloc <> absolute) then
                      constpart := constpart + staticlinkoffset;
                    end;
                  registercandidate := false;
                  if newflag then nestedmod := true;
                  end;
                if (varalloc = normalalloc) and (varlev = 1) and
                   ((varindex = outputindex) or (varindex = inputindex)) then
                  standardfilesreferenced := true;
                if varalloc = ownalloc then
                  begin
                  genop(ownop);
                  ownvar := true;
                  bumpsp;
                  with oprndstk[sp] do
                    begin
                    typeindex := intindex;
                    oprndlen := ptrsize;
                    extended := false;
                    operandkind := exproperand;
                    cost := 0
                    end;
                  end
                else if varalloc in
                        [absolute, usealloc, definealloc, sharedalloc] then
                  begin
                  varlev := 0;
                  newflag := true;
                  if varalloc = absolute then {ORIGIN}
                    begin
                    if (targetmachine = iAPX86) or (targetmachine = i80386) then
                      begin
                      genop(originop);
                      genint(offset div 65536); {segment part}
                      constpart := offset mod 65536; {retain offset part}
                      end
                    else
                      begin
                      genop(levop);
                      genint(0);
                      genint(0);
                      end;
                    end
                  else
                    begin {USE/DEFINE/SHARED external variable}
                    vartable[sparelink div (maxvarentries + 1) +
                     1]^[sparelink mod (maxvarentries + 1)].referenced := true;
                    if (targetmachine = iAPX86) or (targetmachine = i80386) then
                      begin
                      if switcheverplus[largemodel] then
                        begin
                        genop(segop);
                        genint(sparelink);
                        end;
                      end;
                    genop(extop);
                    genint(sparelink);
                    end;
                  bumpsp;
                  with oprndstk[sp] do
                    begin
                    typeindex := intindex;
                    oprndlen := ptrsize;
                    extended := false;
                    operandkind := varoperand;
                    cost := 0
                    end;
                  end
                else
                  getlevel(varlev,
                           namekind in
                           [param, varparam, funcparam, procparam, confparam,
                           varconfparam, boundid]);
                len := sizeof(resultptr, unpacking);
                off := constpart;

                { This fixes a 68k problem where a small packed array (of
                  boolean for instance) when moved as a unit, is moved to
                  the least significant end instead of the most significant
                  end of the byte that contains it.  This had some nasty
                  effects in the native compiler itself, including
                  suppressing hoisting!  The fix is to treat the array as
                  a packed entity.
                }
                if packinghightolow then
                  begin
                  setnowunpacking(resultptr^.bitaddress, 0, nowunpacking);

                  if nowunpacking then
                    begin
                    if not unpacking then
                      startunpacking(nowunpacking, constpart, unpacking);
                    constpart := 0;
                    end;

                  len := sizeof(resultptr, unpacking);
                  end;

                end;
              if varalloc = pointeralloc then
                begin
                genlit(constpart);
                genunary(indxop, ints);
                genunary(indrop, ints);
                len := sizeof(resultptr, false); { false because varparams can't
                                                  be packed element }
                oprndstk[sp].oprndlen := len;
                constpart := 0;
                end;
              if parsing then
                begin
                gettoken;
                if token in [lbrack, dot, uparrow] then
                  selector(variantok, varianttag, packedok, unpacking,
                           constpart, len, off, varlev, ownvar);
                end;
              if namekind in [funcparam, procparam] then len := procparamsize;
              end;
            forwardfunc, funcname:
              begin
              varlev := varlev + 1;
              if (targetopsys=vms) and
                 (proctable[display[varlev].blockref].calllinkage =
                     nonpascalcall) and
                 (proctable[display[varlev].blockref].registerfunction = 0)
              then
                begin
                getlevel(varlev, true);
                genlit(4);
                genunary(indxop, ints);
                genunary(indrop, ints);
                constpart := 0;
                end
              else
                begin
                getlevel(varlev, false);
                with display[varlev] do
                  begin
                  constpart := blocksize + paramsize;
                  if proctable[blockref].externallinkage then
                    constpart := constpart + extreturnlinksize
                  else constpart := constpart + returnlinksize;
                  end;
                end;
{
              Override offset of function value if value must be
              returned in registers.  In that case we allocate the
              function space in the local variables area.
}
              if proctable[display[varlev].blockref].registerfunction <> 0 then
                constpart := 0;
              if (varlev <> level) and (varlev > 1) then
                begin
                proctable[display[level].blockref].intlevelrefs := true;
                if varlev > 2 then constpart := constpart + staticlinkoffset;
                end;
              off := constpart;
              newresulttype(functype);
              len := sizeof(resultptr, false);
              if parsing then gettoken;
              end;
            otherwise
              begin
              illegalident(varindex);
              if parsing then gettoken;
              end;
            end;
          setvarrange(len, unpacking, var_is_valid);
          lastindex(unpacking, constpart, len);
          if unsigned(resultptr, len, unpacking) then
            if newflag then genop(newunsvarop)
            else genop(unsvarop)
          else if newflag then genop(newvarop)
          else genop(varop);
          genint(len);
          genint(varlev);
          if newflag and (varlev <= 1) then
            with proctable[display[level].blockref] do
              begin
              globaldeath := true;
              if display[level].blockref <= cseregions then
                with cseregiontable[display[level].blockref, ownvar] do
                  begin
                  if off < low then low := off;
                  if off > high then high := off;
                  end;
              end;
          genint(off);
          genint(ord(ownvar));
          oprndstk[sp].oprndlen := sizeof(resultptr, false);
          oprndstk[sp].extended := resultptr^.extendedrange;
          oprndstk[sp].operandkind := varoperand;
          lev := varlev;
          end;
        end;
    end {variable} ;


  procedure modifyvariable(variantok: boolean; {true if case selector ok}
                           packedok: boolean {packed ok in context} );

{ Parse a variable which will be modified.  The actual parsing is done by
  "variable", this routine just does special actions for variables which
  will be modified.
}

    var
      resultindex: index; {used as temp, and dummy arg}
      p: entryptr; {used for name table access}


    begin {modifyvariable}
      search(resultindex);
      if resultindex = 0 then illegalident(resultindex)
      else
        begin
        if bigcompilerversion then p := @(bigtable[resultindex]);
        with p^ do
          if namekind in
             [varname, fieldname, param, varparam, confparam,
             varconfparam] then
            begin
            modified := true;
            parammodified := true;
            if (nest = 1) and nolabelsofar then knownvalid := true;
            variable(variantok, packedok, false, true, true, resultindex)
            end
          else illegalident(resultindex);
        end
    end {modifyvariable} ;


  function filedeclared: boolean;

  { Returns true unless "standard" is specified and token is input
    or output and is not declared in the program heading.
  }

    var
      fileindex: index; {to get to the file name}
      f: entryptr; {to point there once we've found it}


    begin
      filedeclared := true;
      if (switchcounters[standard] > 0) and (token = ident) then
        begin
        search(fileindex);
        if (fileindex = inputindex) or (fileindex = outputindex) then
          begin
          if bigcompilerversion then f := @(bigtable[fileindex]);
          filedeclared := f^.programdecl;
          end;
        end;
    end {filedeclared} ;


  procedure fileparam(pushit: boolean; {parameter must be pushed}
                      emitlen: boolean; {element length is needed, too}
                      textfileneeded: boolean {must be a text file} );

{ Parses a file parameter to a procedure.  This is assumed to be the first
  parameter in all cases, as it parses the initial left parenthesis too.

  If "emitlen" is set, the length of a file element in file size units will
  be pushed onto the stack after the file operator.
}

    var
      f: entryptr; {for access to file base type}
      fileident: index; {for checking program statement declaration}


    begin {fileparam}
      if pushit then genop(bldnil);
      verifytoken(lpar, nolparerr);
      if token = ident then
        begin
        if not filedeclared then warnnonstandard(filenotdeclared);
        modifyvariable(true, true);
        if not (resultform in [files, none]) then warnbefore(nofilevar)
        else
          begin
          if textfileneeded and (resulttype <> textindex) then
            warnbefore(nottextfile);
          if pushit then genunary(pushaddr, resultform);
          if emitlen and (resultform = files) then
            if resulttype = textindex then genpushdefaultint( - 1)
            else
              begin
              if bigcompilerversion then
                f := @(bigtable[resultptr^.filebasetype]);
              genpushdefaultint(forcealign(sizeof(f, false), alignmentof(f,
                                           false),
                                false) * (bitsperunit div bitsperfileunit));
              end;
          end;
        end
      else warnbetween(novarerr);
    end {fileparam} ;


  procedure stringtarget;

{ Parse and generate a string target parameter for intrinsic string
  functions.   The argument must be a string variable, and the address
  and length are both passed as parameters.
}

    var
      varlen: addressrange;


    begin {stringtarget}
      modifyvariable(true, true);
      if not (resultform in [strings, none]) then warnbefore(nostringerr);
      varlen := oprndstk[sp].oprndlen - 1;
      oprndstk[sp].oprndlen := ptrsize;
      genunary(pushaddr, strings);
      pushint(varlen);
      oprndstk[sp].oprndlen := shorttargetintsize;
      genunary(pushvalue, ints);
    end {stringtarget} ;


  procedure stringsource;

{ Parse a string source parameter for intrinsic string params.  Parameter
  is passed by reference but any string-look-alike is allowed including
  single chars, packed arrays of char, and literal constants.
}


    begin {stringsource}
      expression(follow + [comma, colon, rpar], false);
      if resultform = chars then genunary(chrstrop, chars)
      else if (resultform = arrays) and resultptr^.stringtype then
        genunary(arraystrop, arrays)
      else if not (resultform in [strings, none]) then warnbefore(nostringerr);
      oprndstk[sp].oprndlen := ptrsize;
      genunary(pushaddr, strings);
    end {stringsource} ;


  procedure parsecolons(maxcolons: integer {colons allowed in context} );

{ This routine is used for procedure arguments to parse off any write-args,
  even if the procedure is not write.  if the number of colons found exceeds
  "maxcolons", an error message is issued.

  Productions:

  write-parameter = expression [ ":" expression [ ":" expression ] ] .

}

    var
      coloncount: integer; {count of colons in this writearg}


    begin {parsecolons}
      coloncount := 0;
      if maxcolons > 0 then genoprnd;
      while token = colon do
        begin
        coloncount := coloncount + 1;
        if coloncount > maxcolons then warn(illegalformat);
        gettoken;
        expression(follow + [comma, rpar, colon], false);
        if resultform <> ints then warnbefore(badformat);
        setdefaulttargetintsize;
        genunary(bldfmt, ints); ;
        end
    end {parsecolons} ;


  procedure parseextraargs;

{ Syntactic error recovery routine to parse any arguments left after the
  expected number for a standard procedure.  It will accept almost
  anything that looks vaguely like an argument and try to parse it.
  It will issue an error message if it finds anything to do.
}


    begin {parseextraargs}
      parsecolons(0);
      if token in
         [comma, eql..andsym, ident, intconst..stringconst, lbrack, lpar,
         notsym, nilsym] then
        begin
        warn(toomanyargs);
        repeat
          verifytoken(comma, nocommaerr);
          expression(follow + [rpar, comma, colon], false);
          genoprnd;
          parsecolons(0)
        until not (token in
              [comma, eql..andsym, ident, intconst..stringconst, lbrack, lpar,
              notsym, nilsym]);
        end;
      verifytoken(rpar, norparerr);
    end {parseextraargs} ;


  procedure parameterlist(procindex: index {procedure name entry} );

{ Syntactic procedure to parse a parameter list.

  Productions:

  actual-parameter-list = "(" actual-parameter
        [* "," actual-parameter *] ")"  .

  Each actual-parameter found is parsed by "oneactualparameter".

  If there are more or fewer parameters found than expected,
  an error is emitted.
}

    var
      paramcount: integer; {parameters found}
      paramindex: integer; {next formal parameter index}
      maxparams: integer; {max param (end of param index)}
      alreadywarned: boolean; {message already issued, don't duplicate}
      p: entryptr; {used for access to proc entry}
      calltype: index; {used for function param return type}

      lastconfactual: index; {type of last actual conformant parameter}


    procedure oneactualparam;

{ Syntactic routine to parse an actual parameter:

  Productions:

  actual-parameter = expression | variable-access |
        procedure-identifier | function-identifier  .

  The parameter parsed is compared to the corresponding formal parameter
  for compatibility.  Routine parameters have their argument lists
  checked as well.  Parameters are pushed onto the stack in the order
  they are read.
}

      var
        nextparam: index; {next formal param for this procedure}
        namekind: nametype; {namekind of the formal parameter}
        actualstdstring: boolean; {true if actual param is a standard string}
        actualindex: index; {name for actual parameter}
        actualptr: entryptr; {used for access to actual param}
        p: entryptr; {used for access to formal param}
        paramptr: entryptr; {for type of formal param}
        formaltype: index; {type of formal parameter}
        formallen: addressrange; {size of formal parameter}


      procedure callparam;

{ A routine parameter has been parsed, so the parameter lists must be
  checked for compatibility, and the routine address and static link
  set up to be passed.  If the actual parameter is itself a procedural
  parameter, the parameter lists are still compared, and the value is
  passed directly.
}


        function congruent(actual, lastactual: index; {limits of actual list}
                           formal,
         lastformal: index {same for formal list} ): boolean;

{ True if the actual procedure's parameter list (defined by "actual"
  and "lastactual" is congruent to the formal procedure's parameter
  list (defined by "formal" and "lastformal").  "actual" and "formal"
  point to the procedure name, and "lastactual" and "lastformal" are
  the last parameter entries for the procedures.

  Two parameter lists are congruent if each parameter section is of
  the same kind (var, value, procedure, function), has the same number
  of parameters, and the same type.  If the parameter is a conformant
  array, the two schemas must be "equivalent".
}

          var
            c: boolean; {local value of "congruent"}
            procpar: boolean; {set if procedure parameter}
            ap, fp: entryptr; {access to "actual" and "formal"}
            nexta, nextf: index; {links to next parameters}


          function equivtypes(formalt, actualt: index {type names} ): boolean;

{ True if the types are the same or are equivalent conformal array
  schemas
}

            var
              at, ft: entryptr; {for access to type entries}


            begin {equivtypes}
              equivtypes := false;
              if identical(formalt, actualt) then equivtypes := true
              else
                begin
                if bigcompilerversion then at := @(bigtable[actualt]);
                if bigcompilerversion then ft := @(bigtable[formalt]);
                if (at^.typ = conformantarrays) and
                   (ft^.typ = conformantarrays) then
                  equivtypes := (at^.packedflag = ft^.packedflag) and
                                identical(at^.indextype, ft^.indextype) and
                                equivtypes(at^.elementtype, ft^.elementtype);
                end;
            end {equivtypes} ;


          begin {congruent}
            c := (lastactual - actual) = (lastformal - formal);
            actual := actual + 1; {point to first parameter}
            formal := formal + 1;
            while c and (actual <= lastactual) do
              begin
              if bigcompilerversion then ap := @(bigtable[actual]);
              if bigcompilerversion then fp := @(bigtable[formal]);
              nexta := ap^.nextparamlink;
              nextf := fp^.nextparamlink;
              c := (ap^.namekind = fp^.namekind) and
                   (ap^.lastinsection = fp^.lastinsection);
              if c and ap^.lastinsection then
                begin
                procpar := ap^.namekind in [procparam, funcparam];
                c := equivtypes(ap^.vartype, fp^.vartype);
                if c and procpar then
                  c := congruent(actual, nexta, formal, nextf);
                end;
              actual := nexta + 1;
              formal := nextf + 1;
              end; {while}
            congruent := c;
          end {congruent} ;


        begin {callparam}
          if actualptr^.namekind in [procparam, funcparam] then
            begin
            if not congruent(actualindex, actualptr^.nextparamlink, paramindex,
                             p^.nextparamlink) then
              warn(paramtypeerr);
            variable(true, false, true, false, true, actualindex);
            oprndstk[sp].oprndlen := procparamsize;
            genunary(pushvalue, arrays);
            end
          else
            begin
            if not congruent(actualindex, actualptr^.paramlist, paramindex,
                             p^.nextparamlink) or
               ((proctable[actualptr^.procref].calllinkage <>
               interruptcall) and
               ((proctable[actualptr^.procref].calllinkage <> pascal2call) and
               not switcheverplus[windows])) then
              warn(paramtypeerr);
            proctable[actualptr^.procref].isprocparam := true;
            getlevel(lev, false);
            if (level > 2) and (lev = level) then
              begin
              genlit( - staticlinkoffset);
              genunary(indxop, ints);
              end;
            if lev > 1 then
              proctable[display[level].blockref].intlevelrefs := true;
            genlit(actualptr^.procref);
            oprndstk[sp].oprndlen := procparamsize;
            genunary(pushproc, resultptr^.typ);
            gettoken
            end;
        end {callparam} ;


      function conformable(formal, actual: index {parameters} ): boolean;

{ True if the actual parameter is conformable to the formal conformant
  array parameter.
}

        var
          formalptr, actualptr: entryptr; {access to formal and actual}
          formalindex, actualindex: index; {index types}
          formalelt, actualelt: index; {element types}
          c: boolean; {partial value of conformable}


        begin {conformable}
          if bigcompilerversion then formalptr := @(bigtable[formal]);
          if bigcompilerversion then actualptr := @(bigtable[actual]);
          if (formalptr^.typ = conformantarrays) and
             (actualptr^.typ in [conformantarrays, arrays, strings]) then
            begin
            formalindex := formalptr^.indextype;
            actualindex := actualptr^.indextype;
            formalelt := formalptr^.elementtype;
            actualelt := actualptr^.elementtype;
            c := formalptr^.packedflag = actualptr^.packedflag;
            c := c and compatible(formalindex, actualindex) and
                 conformable(formalelt, actualelt);
            if c and (actualptr^.typ in [arrays, strings]) then
              begin
              if bigcompilerversion then
                formalptr := @(bigtable[formalindex]);
              if bigcompilerversion then
                actualptr := @(bigtable[actualindex]);
              c := (lower(actualptr) >= lower(formalptr)) and
                   (upper(actualptr) <= upper(formalptr));
              end;
            conformable := c;
            end
          else conformable := identical(formal, actual);
        end {conformable} ;


      procedure conformantparam(paramtype: index; {index of parameter
                                                   description}
                                pushbounds: boolean; {push the bounds for this
                                                      one}
                                valueparam: boolean {this is a value parameter}
                                );

{ Parse a conformant parameter.  The bounds are passed only for the first
  in a group of conformant parameters.  If the actual parameter is itself
  a conformant parameter, a runtime check on the bounds is required to make
  sure that it will fit in the bounds of the formal parameter.
}

        var
          paramptr: entryptr; {for access to parameter type data}
          actualptr: entryptr; {for access to actual parameter bounds}
          at1, pt1: index; {for tracking down lists}
          highid, lowid: index; {bound id's if conformant}
          actualisconformant: boolean; {actual parameter is also conformant}
          desiredindex: index; {Index type of formal parameter}
          indexptr: entryptr; {for access to actual index type}
          boundidsize: addressrange; {for determining size of boundid}
          boundidtype: index; {the subrange used to declare the array}


        procedure pushbound(which: index);

{ Push a boundid for a conformant parameter
}


          begin {pushbound}
            variable(true, false, false, false, false, which);
            if resulttype <> desiredindex then
              begin
              oprndstk[sp].typeindex := desiredindex;
              genunary(congruchkop, resultform);
              end;
          end {pushbound} ;


        begin {conformantparam}
          if oprndstk[sp].oprndlen = 0 then oprndstk[sp].oprndlen := 1
          else if valueparam then genunary(pushcvalue, resultform);

          oprndstk[sp].oprndlen := ptrsize;
          genunary(pushaddr, resultform);

          if not identical(resulttype, lastconfactual) then
            warnbefore(confinconsistent);

          if pushbounds then lastconfactual := noneindex
          else lastconfactual := resulttype;
          actualisconformant := resultptr^.typ = conformantarrays;

          if conformable(paramtype, resulttype) then
            begin
            if pushbounds then
              begin
              if bigcompilerversion then paramptr := @(bigtable[paramtype]);
              actualptr := resultptr;
              while (actualptr^.typ in [arrays, conformantarrays, strings]) and
                    (paramptr^.typ = conformantarrays) do
                begin
                paramcount := paramcount + 2;
                pt1 := paramptr^.elementtype;
                at1 := actualptr^.elementtype;
                if bigcompilerversion then
                  indexptr := @(bigtable[paramptr^.lowbound]);
                boundidsize := indexptr^.length;
                boundidtype := indexptr^.vartype;
                highid := actualptr^.highbound;
                lowid := actualptr^.lowbound;
                desiredindex := paramptr^.indextype;
                if bigcompilerversion then
                  indexptr := @(bigtable[actualptr^.indextype]);
                if actualisconformant then pushbound(lowid)
                else pushint(lower(indexptr));
                oprndstk[sp].typeindex := boundidtype;
                oprndstk[sp].oprndlen := boundidsize;
                genunary(pushvalue, getform(indexptr));
                genoprnd;

                if actualisconformant then pushbound(highid)
                else pushint(upper(indexptr));
                oprndstk[sp].typeindex := boundidtype;
                oprndstk[sp].oprndlen := boundidsize;
                genunary(pushvalue, getform(indexptr));
                genoprnd;

                if bigcompilerversion then paramptr := @(bigtable[pt1]);
                if bigcompilerversion then actualptr := @(bigtable[at1]);
                end;
              end;
            end
          else warnbefore(paramtypeerr);

          newresulttype(lastconfactual);
          oprndstk[sp].oprndlen := ptrsize;
        end {conformantparam} ;


      begin {oneactualparam}
        paramcount := paramcount + 1;
        newresulttype(noneindex);
        if (paramindex > maxparams) then
          begin
          if not alreadywarned then
            begin
            warn(toomanyargs);
            alreadywarned := true
            end;
          paramindex := maxparams
          end;
        if bigcompilerversion then p := @(bigtable[paramindex]);
        namekind := p^.namekind;
        nextparam := p^.nextparamlink + 1;
        if alreadywarned or (namekind in [param, confparam]) or
           (paramindex = 0) then
          begin
          expression(follow + [comma, colon, rpar], false);
          actualstdstring := (resultform = arrays) and resultptr^.stringtype;
          if not alreadywarned then
            begin
            if bigcompilerversion then p := @(bigtable[paramindex]);
            formaltype := p^.vartype;
            if bigcompilerversion then paramptr := @(bigtable[formaltype]);
            formallen := p^.length;
            if namekind = param then
              begin
              if (resultform = ints) and (getform(paramptr) = reals) then
                begin
                newresulttype(realindex);
                genunary(float, ints);
                end
              else if (resultform = ints) and (getform(paramptr) = doubles) then
                begin
                newresulttype(doubleindex);
                genunary(float_double, ints);
                end
              else if (resultform = reals) and
                      (getform(paramptr) = doubles) then
                begin
                newresulttype(doubleindex);
                oprndstk[sp].oprndlen := doublesize;
                genunary(real_to_dbl, reals);
                end
              else if (getform(paramptr) = strings) and
                      (resultform = chars) then
                genunary(chrstrop, strings)
              else if (getform(paramptr) = strings) and actualstdstring then
                genunary(arraystrop, strings)
              else
                begin
                if (switchcounters[standard] <= 0) and
                   (getform(paramptr) = arrays) and paramptr^.stringtype and
                   (resultform = chars) then
                  begin
                  newstringtype(resulttype, arrays, 1);
                  newresulttype(resulttype);
                  oprndstk[sp].typeindex := resulttype;
                  end;
                if not compatible(resulttype, formaltype) then
                  warnbefore(paramtypeerr);
                end;
              gencheck(rangechkop, formaltype);
              end
            else if (namekind = confparam) and
                    (resultform = conformantarrays) then
              warnbefore(badconfactual);
            if namekind = param then
              if p^.varalloc = pointeralloc then
                begin
                if p^.modified and
                   (oprndstk[sp].operandkind <> exproperand) then
                  genunary(pushcvalue, resultform);
                oprndstk[sp].oprndlen := ptrsize;
                genunary(pushaddr, resultform);
                end
              else
                begin
                oprndstk[sp].oprndlen := formallen;
                genunary(pushvalue, resultform);
                end
            else conformantparam(p^.vartype, p^.lastinsection, true);
            end;
          end
        else if token = ident then
          begin
          search(actualindex);
          if actualindex = 0 then illegalident(actualindex)
          else
            begin
            if bigcompilerversion then actualptr := @(bigtable[actualindex]);
            case namekind of
              varparam:
                begin
                modifyvariable(false, false);
                if bigcompilerversion then p := @(bigtable[paramindex]);
                if not (p^.univparam or identical(p^.vartype, resulttype)) then
                  warnbefore(paramtypeerr);
                oprndstk[sp].oprndlen := ptrsize;
                genunary(pushaddr, resultform);
                if token in begexprset then warn(varparamerr);
                end;
              procparam:
                if not (actualptr^.namekind in
                   [forwardproc, externalproc, procname, procparam]) then
                  begin
                  warn(badprocparam);
                  illegalident(actualindex)
                  end
                else callparam;
              funcparam:
                if not (actualptr^.namekind in
                   [forwardfunc, externalfunc, funcname, funcparam]) then
                  begin
                  warn(badfuncparam);
                  illegalident(actualindex)
                  end
                else
                  begin
                  newresulttype(p^.vartype);
                  if actualptr^.namekind = funcparam then
                    calltype := actualptr^.vartype
                  else calltype := actualptr^.functype;
                  if not identical(resulttype, calltype) then
                    warn(paramtypeerr);
                  callparam;
                  end;
              varconfparam:
                begin
                modifyvariable(true, false);
                conformantparam(p^.vartype, p^.lastinsection, false);
                if token in begexprset then warn(varparamerr);
                end;
              end;
            end
          end
        else warn(novarerr);
        paramindex := nextparam;
        parsecolons(0);
        genoprnd;
        verify1([rpar, comma] + follow, badparamerr);
      end {oneactualparam} ;


    begin {parameterlist}
      lastconfactual := noneindex;
      alreadywarned := false;
      if bigcompilerversion then p := @(bigtable[procindex]);
      with p^ do
        if (procindex = 0) or
           not (namekind in
           [funcparam, procparam, externalfunc, procname, funcname, externalproc, forwardfunc,
           forwardproc]) then
          begin
          maxparams := procindex;
          alreadywarned := true;
          end
        else if (namekind in [funcparam, procparam]) then
          maxparams := nextparamlink
        else maxparams := paramlist;
      paramcount := 0;
      paramindex := procindex + 1;
      if token = lpar then
        begin
        gettoken;
        oneactualparam;
        while token = comma do
          begin
          gettoken;
          oneactualparam;
          end;
        verifytoken(rpar, norparerr);
        end;
      if (paramindex <= maxparams) and not alreadywarned then
        warnbefore(toofewargs);
      genlit(paramcount);
    end {parameterlist} ;


  procedure procedurecall(where: index {proc name index} );

{ Syntactic routine to parse a procedure call:

  Productions:

  procedure-statement = identifer [ actual-parameter-list ]  .

  On parsing the call space is saved for the function value (if any),
  the parameters are put on the stack by "parameterlist", and the call
  is generated.

  This routine also book-keeps any cross-level references needed
}

    var
      ftype: index; {function type}
      plen: addressrange; {function result size}
      bref: proctableindex; {current procedure ref index}
      pref: proctableindex; {procedure ref index}
      p: entryptr; {provides access to procedure name/type entry}
      l: levelindex; {calling proc level}


    begin {procedurecall}
      gettoken;
      if bigcompilerversion then p := @(bigtable[where]);
      with p^ do
        begin
        ftype := functype;
        pref := procref;
        end;
      if bigcompilerversion then p := @(bigtable[ftype]);
      plen := sizeof(p, false);
      genlit(pref);
      genop(reserve);
      genint(plen);
      bref := display[level].blockref;
      l := level;
      with proctable[pref] do
        begin
        if (l - lev > levelspread) and (lev <> 1) then levelspread := l - lev;
        if (lev <> 1) and (not bodydefined or intlevelrefs) and (l > 1) then
          proctable[bref].intlevelrefs := true;
        if globaldeath or not bodydefined then
          begin
          proctable[bref].globaldeath := true;
          if (bref <= cseregions) and (pref <= cseregions) then
            begin
            with cseregiontable[bref, false] do
              begin
              if cseregiontable[pref, false].low < low then
                low := cseregiontable[pref, false].low;
              if cseregiontable[pref, false].high > high then
                high := cseregiontable[pref, false].high;
              end;
            with cseregiontable[bref, true] do
              begin
              if cseregiontable[pref, true].low < low then
                low := cseregiontable[pref, true].low;
              if cseregiontable[pref, true].high > high then
                high := cseregiontable[pref, true].high;
              end;
            end;
          end;
        if calllinkage <> pascal2call then anynonpascalcalls := true;

        { Interrupt linkage routines may not be called.
        }
        if calllinkage = interruptcall then warn(badinterruptproc);
        end;
      parameterlist(where);
      newresulttype(ftype);
      pushdummy;
      oprndstk[sp].typeindex := resulttype;
      oprndstk[sp].oprndlen := plen;
      setvarrange(plen, false, false);
      if bigcompilerversion then p := @(bigtable[ftype]);
      if unsigned(p, plen, false) then genunary(unscall, resultform)
      else genunary(call, resultform);
    end {procedurecall} ;


  procedure paramcall(where: index {procedure name index} );

{ Syntactic routine to parse a parameter procedure call.  The
  syntax is the same as for an ordinary procedure call, but the
  actual call is different.
}

    var
      ftype: index; {function type pointer}
      plen: addressrange; {function result size}
      p: entryptr; {provides access to procedure type entry}


    begin {paramcall}
      variable(true, false, true, false, true, where);
      ftype := resulttype;
      if bigcompilerversion then p := @(bigtable[ftype]);
      plen := sizeof(p, false);
      genop(reserve);
      genint(plen);
      parameterlist(where);
      newresulttype(ftype);
      oprndstk[sp].oprndlen := plen;
      setvarrange(oprndstk[sp].oprndlen, false, false);
      if unsigned(resultptr, plen, false) then
        genunary(unscallparam, resultform)
      else genunary(callparam, resultform);
      with proctable[display[level].blockref] do
        begin
        globaldeath := true;
        intlevelrefs := true;
        if display[level].blockref <= cseregions then
          begin
          with cseregiontable[display[level].blockref, false] do
            begin
            low := 0;
            high := maxaddr;
            end;
          with cseregiontable[display[level].blockref, true] do
            begin
            low := 0;
            high := maxaddr;
            end;
          end;
        end;
    end {paramcall} ;


  procedure parsetagparams(var currentrecord: index {starting point} );

{ Parse a stream of tag variants, updating currentrecord as we march
  along.  Used by "new", "dispose", and "sizeof".
}

    var
      currentptr: entryptr; {points to currentrecord}
      tagtype: index; {index of currentrecord's tag, if any}
      f, p: entryptr; {the better to point with...}
      labvalue: operand; {the given tag labels}


    begin {parsetagparams}
      while token in
            [comma, ident, nilsym, lpar, plus, minus, intconst..stringconst] do
        begin
        verifytoken(comma, nocommaerr);
        constant([comma, rpar], true, labvalue);
        if bigcompilerversion then currentptr := @(bigtable[currentrecord]);
        if getform(currentptr) = fields then
          begin
          tagtype := noneindex;
          if currentptr^.tagfield <> 0 then
            begin
            if bigcompilerversion then p := @(bigtable[currentptr^.tagfield]);
            tagtype := p^.vartype
            end
          else if currentptr^.firstvariant <> 0 then
            begin
            if bigcompilerversion then
              f := @(bigtable[currentptr^.firstvariant]);
            if f^.firstlabel <> 0 then
              begin
              if bigcompilerversion then f := @(bigtable[f^.firstlabel]);
              tagtype := f^.varlabtype;
              end;
            end;
          if not compatible(labvalue.typeindex, tagtype) then
            warnbefore(badcaselab);
          searchvariants(currentrecord, labvalue);
          end
        else if currentrecord <> noneindex then
          begin
          warnbefore(nofieldtype);
          currentrecord := noneindex
          end;
        end;
    end {parsetagparams} ;


  procedure factor;

{ Syntactic routine to parse a factor.

  Productions:

  factor = variable-access | unsigned-constant | function-designator |
        set-constructor | "(" expression ") | "not" factor |
        "@" variable-access  .

  As the factor is parsed, intermediate output is generated to compute it.
  The global "resulttype" is set to the type of the factor.
}

    var
      unpacking: boolean; {true if unpacking a record or array}
      setisconst: boolean; {true if [setexpression] is constant}
      varindex: index; {index of var if found}
      varptr, p: entryptr; {provides access to var nameentry if needed}
      constpart, constantlen, off: addressrange;
      {"selector" args for const struct}
      ownvar: boolean; {false as only const structs are handled here}
      varlev: levelindex; {dummy arg to "selector"}
      setelementtype: index; {elt type for set constructor}
      settype: index; {created set type for set constructor}
      setptr: entryptr; {used to access settype}
      setentry: tableentry; {local copy of settype entry}
      baseptr: entryptr; {used to access base type of set}


    procedure standardfunctions(procid: standardids {std func no.} );

{ Parse and generate standard functions.  Syntax varies slightly, but
  is different for different functions.
}

      var
        paramform: types; {type of the parameter}


      procedure beginparams;

{ Parse a single parameter. generating intermediate code for the
  expression, and push the stack for the result
}


        begin {beginparams}
          verifytoken(lpar, nolparerr);
          expression(follow + [rpar, comma, colon], false);
        end {beginparams} ;

      procedure stringexpr;

{ Parse one expression for concat, and convert to string if necessary.
}


        begin {stringexpr}
          expression(follow + [comma, rpar], false);
          if resultform = chars then genunary(chrstrop, strings)
          else if (resultform = arrays) and resultptr^.stringtype then
            genunary(arraystrop, strings)
          else if resultform <> strings then warnbefore(paramtypeerr);
        end {stringexpr} ;

      procedure finishparams(legaltypes: typeset; {types allowed}
                             newtype: index {function result type} );

{ Set up the returned value with the result type, and parse any extra
  arguments.  On entry to this procedure, "resulttype" is set to the
  type of the operand by the expression parser.  This is changed as
  necessary to "newtype".
}


        begin {finishparams}
          if not (resultform in legaltypes) then
            begin
            warnbefore(badfunctionarg);
            newresulttype(noneindex);
            end
          else newresulttype(newtype);
          paramform := resultform;
          oprndstk[sp].typeindex := newtype;
          oprndstk[sp].oprndlen := sizeof(resultptr, false);
          parseextraargs;
        end {finishparams} ;


      procedure time;

{ Process the standard routine "time".
}


        begin {time}
          pushdummy;
          newresulttype(realindex);
          paramform := reals;
          oprndstk[sp].oprndlen := targetrealsize;
          oprndstk[sp].typeindex := realindex;
          genop(bldnil);
        end {time} ;


      procedure iofunction(finaltype: index {type of value returned} );

{ Parse the file functions "ioerror" and "iostatus".
}


        begin {iofunction}
          fileparam(false, false, false);
          finishparams([none, files], finaltype);
          if finaltype = intindex then setdefaulttargetintsize;
        end {iofunction} ;


      procedure transcendentals;

{ Parse the transcendental functions.  This may require that an integer
  argument be converted to real.
}


        begin {transcendentals}
          beginparams;
          if resultform = ints then
            begin
            newresulttype(realindex);
            genunary(float, ints)
            end;
          finishparams([none, reals, doubles], resulttype);
        end {transcendentals} ;


      procedure sincosfn;

      { Handle the sin and cos functions.  They are complicated by the
        68881 fsincos instruction that returns both the sin and cos.
      }


        begin {sincosfn}
          if (targetmachine = mc68000) then
            if switcheverplus[fpc68881] then
              begin
              genlit(ord(fsincos2id));
              transcendentals;
              genunary(sysfn, paramform);
              end
            else transcendentals
          else transcendentals;
        end {sincosfn} ;


      procedure oddfunction;

{ Process the odd function.  No special action.
}


        begin {oddfunction}
          beginparams;
          finishparams([ints, none], boolindex);
          setvarrange(unitsize, false, true);
        end {oddfunction} ;


      procedure absfunction;

{ Process an "abs" function.  The result is of the same
  type as the input.
}


        procedure absrange(operand: range; {operand having abs taken}
                           var result: range {resulting range} );

{ Compute resulting range for the "abs" function
}

          var
            temp: integer; {used to negate values}
            ov: boolean; {dummy overflow flag}


          begin {absrange}
            result := operand;
            with operand do
              if not result.extended and (minlimit < 0) then
                begin
                negate(minlimit, temp, ov);
                result.maxlimit := max(temp, maxlimit);
                if (maxlimit < 0) then negate(maxlimit, result.minlimit, ov)
                else result.minlimit := 0;
                end;
          end {absrange} ;


        begin {absfunction}
          beginparams;
          with oprndstk[sp] do
            if resultform = ints then
              begin
              absrange(value_range.optimistic, value_range.optimistic);
              absrange(value_range.pessimistic, value_range.pessimistic);
              end;
          finishparams([none, ints, reals, doubles], resulttype);
        end {absfunction} ;


      procedure sqrfunction;

{ Process a "sqr" function.  The result is of the same type as the input.
}


        procedure sqrrange(operand: range; {operand being squared}
		           op: binaryfoldop;
                           var result: range {resulting range} );

{ Compute resulting range for the "sqr" function
}

          var
            temp: integer; {used to square values}
            ov: boolean; {dummy overflow flag}


          begin {sqrrange}
            with operand do
              begin
              op(maxlimit, maxlimit, result.maxlimit, ov);
              op(minlimit, minlimit, temp, ov);
              result.maxlimit := max(temp, result.maxlimit);
              if (minlimit < 0) then result.minlimit := 0
              else result.minlimit := temp;
              result.extended := extended;
              end;
          end {sqrrange} ;


        begin {sqrfunction}
          beginparams;
          with oprndstk[sp] do
            if resultform = ints then
              if extended then
                begin
                sqrrange(value_range.optimistic, @usmultiply,
                         value_range.optimistic);
                sqrrange(value_range.pessimistic, @usmultiply,
                         value_range.pessimistic);
                end
              else
                begin
                sqrrange(value_range.optimistic, @multiply,
                         value_range.optimistic);
                sqrrange(value_range.pessimistic, @multiply,
                         value_range.pessimistic);
                end;
          finishparams([none, ints, reals, doubles], resulttype);
        end {sqrfunction} ;


      procedure truncround;

{ Process a trunc or round function.  No special processing.
}


        begin {truncround}
          beginparams;
          finishparams([none, reals, doubles], intindex);
          setdefaulttargetintsize;
          setvarrange(defaulttargetintsize, false, true);
        end {truncround} ;


      procedure ordfunction;

{ Parse an ord function.  No special action here, but no "sysfn" is
  generated, just the operand.
}


        begin {ordfunction}
          beginparams;
          finishparams([none, ints, bools, chars, scalars], intindex);
        end {ordfunction} ;


      procedure chrfunction;

{ Parse a chr function.  Similar to ord, but leaves type char as the
  result.
}


        begin {chrfunction}
          beginparams;
          finishparams([none, ints], chartypeindex);
        end {chrfunction} ;


      procedure succpred;

{ Parse a succ or pred function.  No special action.
}


        begin {succpred}
          beginparams;
          pushint(1);
          if procid = succid then genbinary(plusop, ints)
          else genbinary(minusop, ints);
          finishparams([none, scalars, ints, chars, bools], resulttype);
        end {succpred} ;



      procedure lengthfunction;

{ Parse length function.  This returns the length of a string expression.
  Since the string length is stored as the first byte of a string, it is
  exactly equivalent to ord(stringexpr[0]).
}

        var
          oldcheckundefs: boolean; {save/restore checkundefs flag}


        procedure setlength(l: addressrange);

         { Set length to the constant value l }


          begin {setlength}
            genunary(deleteop, none);
            parseextraargs;
            sp := sp - 1;
            pushint(l);
            resultform := ints;
            resulttype := intindex;
          end {setlength} ;


        begin {lengthfunction}
          verifytoken(lpar, nolparerr);
          oldcheckundefs := checkundefs;
          checkundefs := false;
          expression(follow + [comma, rpar], false);
          checkundefs := oldcheckundefs;
          if resultform = chars then setlength(1)
          else if (resultform = arrays) and resultptr^.stringtype then
            setlength(resultptr^.arraymembers)
          else
            begin
            finishparams([none, strings], intindex);
            with oprndstk[sp] do
              begin
              oprndlen := charsize;
              value_range.optimistic.maxlimit := 255;
              value_range.pessimistic.maxlimit := 255;
              end;
            genop(unsvarop);
            genint(charsize);
            genint(0);
            genint(0);
            genint(0);
            end;
        end {lengthfunction} ;



      procedure eofeoln;

{ Parse a file function.  This has a default argument of the file "input"
  if no argument is provided.
}


        begin {eofeoln}
          if token <> lpar then
            begin
            getlevel(1, false);
            genlit(inputoffset);
            genunary(indxop, ints);
            genop(varop);
            genint(ptrsize);
            genint(1);
            genint(inputoffset);
            genint(0);
            oprndstk[sp].typeindex := textindex;
            oprndstk[sp].oprndlen := ptrsize;
            paramform := bools;
            if switchcounters[nilcheck] > 0 then genunary(ptrchkop, ints);
            genunary(definelazyop, files);
            standardfilesreferenced := true;
            if not inputdeclared then warnnonstandard(inputnotdeclared);
            end
          else
            begin
            beginparams;
            if not (resultform in [none, files]) then warnbefore(nofilevar)
            else if (procid = eolnid) and (resulttype <> textindex) then
              warnbefore(nottextfile);
            if resultform = files then
              begin
              if switchcounters[nilcheck] > 0 then genunary(ptrchkop, ints);
              genunary(definelazyop, files);
              end;
            finishparams([none, files], boolindex);
            end;
          newresulttype(boolindex);
          setvarrange(unitsize, false, true);
        end {eofeoln} ;


      procedure firstypeparam(varok: boolean; {true allows type name or var}
                              var restype: index {type found} );

{ Parse a type parameter for the functions "size" and "loophole".
}

        var
          typeident: index; {name block for type ident}
          p: entryptr; {used to access type name block}
          oldemitflag: boolean; {so we can turn of intcode emission}
          oldcheck: boolean; {and the unassigned value check}


        begin {firstypeparam}
          verifytoken(lpar, nolparerr);
          restype := noneindex;
          if token = ident then
            begin
            search(typeident);
            if typeident = 0 then warn(undefidenterr)
            else
              begin
              if bigcompilerversion then p := @(bigtable[typeident]);
              if p^.namekind in [typename, undeftypename] then
                begin
                restype := p^.typeindex;
                gettoken;
                end
              else if p^.namekind in
                      [varname, fieldname, param, varparam, boundid] then
                begin
                oldemitflag := emitflag;
                oldcheck := checkundefs;
                checkundefs := false;
                emitflag := false;
                variable(true, false, true, false, true, typeident);
                sp := sp - 1;
                emitflag := oldemitflag;
                checkundefs := oldcheck;
                restype := resulttype;
                end
              else
                begin
                warn(notypenameerr);
                gettoken;
                end;
              end;
            end
          else verifytoken(ident, notypenameerr);
        end {firstypeparam} ;


      procedure sizefunction;

{ Process the size and bitsize functions.  These always return a
  constant which is the size of the type identifier used as a pseudo-
  parameter.
}

        var
          thistype: index;
          f: entryptr; {access to thistype}


        begin {sizefunction}
          firstypeparam(true, thistype);
          parsetagparams(thistype);
          if bigcompilerversion then f := @(bigtable[thistype]);
          pushint(sizeof(f, procid = bitsizeid));
          with oprndstk[sp] do
            begin
            extended := cvalue.intvalue < 0;
            setconstrange(cvalue.intvalue, false, value_range);
            end;
          newresulttype(intindex);
          parseextraargs;
        end {sizefunction} ;


      procedure lowerupperfunction;

{ Process the lower and upper functions, returning the first value of a
  given type, or of an expression.
}

        var
          thistype: index; {index to the resulting type}
          f: entryptr; {and pointer to same}
          setflag: boolean; {set true if upper/lower of a set}


        begin {lowerupperfunction}
          setflag := false;
          firstypeparam(true, thistype);
          if bigcompilerversion then f := @(bigtable[thistype]);
          if f^.typ in [sets, arrays] then
            begin {royal kludge per customer request}
            if f^.typ = arrays then thistype := f^.indextype
            else
              begin
              setflag := true;
              thistype := f^.basetype;
              end;
            if bigcompilerversion then f := @(bigtable[thistype]);
            end;
          if not (f^.typ in [none, bools, scalars, ints, subranges, chars]) then
            warnbefore(badfunctionarg);
          if procid = lowerid then
            if setflag then pushint(max(0, lower(f)))
            else pushint(lower(f))
          else if setflag then pushint(min(maxsetord, upper(f)))
          else pushint(upper(f));
          with oprndstk[sp] do
            begin
            typeindex := thistype;
            oprndlen := min(targetintsize, sizeof(f, false));
            setconstrange(cvalue.intvalue, false, value_range);
            end;
          newresulttype(thistype);
          parseextraargs;
        end {lowerupperfunction} ;

      procedure loopholefunction;

{ Implement the "loophole" function.  This is a general type transfer
  function that changes the type of a variable, though it leaves the
  representation alone.
}

        var
          newtype: index;
          newptr: entryptr;
          newform: types;
          newsize: addressrange;
          newunsigned: boolean;


        begin {loopholefunction}
          firstypeparam(false, newtype);
          if bigcompilerversion then newptr := @(bigtable[newtype]);
          newsize := sizeof(newptr, false);
          newform := getform(newptr);
          newunsigned := unsigned(newptr, newsize, false);
          verifytoken(comma, nocommaerr);
          expression(follow + [comma, rpar, colon], false);
          genoprnd;
          sp := sp + 1;
          oprndstk[sp].operandkind := exproperand;
          oprndstk[sp].cost := 0;
          if not ((newform in [none, ints, bools, chars, scalars, ptrs]) and
             (resultform in [none, ints, bools, chars, scalars, ptrs]) or
             (newsize = sizeof(resultptr, false))) then
            warnbefore(typesincomp);
          finishparams([resultform], newtype);
          setvarrange(newsize, false, false);
          genlit(ord(newunsigned));
          genunary(loopholeop, newform);
        end {loopholefunction} ;


      procedure reffunction;

{ Implement the 'ref' function, which builds a pointer to an item.
  A special hack (aren't all hacks special?) enables this new pointer
  type to bypass normal type checking, which requires two pointers to
  be identical to be type compatible.  Since we are building an anonymous
  type, it will not be identical to anything.  Thus the hack, which involves
  the setting of a special bit in the form 'refdefined'.
}

        var
          varindex: index;
          varptr: entryptr;


        begin {reffunction}
          verifytoken(lpar, nolparerr);
          if token = ident then
            begin
            search(varindex);
            if bigcompilerversion then varptr := @(bigtable[varindex]);
            modifyvariable(true, false);
            end
          else
            begin
            warnbetween(novarerr);
            gettoken;
            pushint(0);
            end;
          oprndstk[sp].oprndlen := ptrsize;
          genunary(addrop, ptrs);
          if tabletop = tablesize then fatal(tablefull)
          else tabletop := tabletop + 1;
          if bigcompilerversion then varptr := @(bigtable[tabletop]);
          with varptr^ do
            begin
            dbgsymbol := 0;
            form := false;
            namekind := typename;
            typeindex := resulttype;
            refdefined := true;
            charindex := 0;
            charlen := 0;
            end;
          enterform(ptrs, resulttype, resultptr);
          with resultptr^ do
            begin
            ptrtypename := tabletop - 1;
            ptrkey := lastfilekey;
            lastfilekey := lastfilekey - 1;
            size := ptrsize;
            align := ptralign;
            end;
          oprndstk[sp].typeindex := resulttype;
          verifytoken(rpar, norparerr);
          newresulttype(resulttype);
        end {reffunction} ;


      procedure copy;

{ Parse "copy(stringsource, pos, num)".  Why isn't this called "substring"?
  Ask Borland someday!
}

        var
          strlen: addressrange;


        begin {copy}
          genlit(0);
          genop(reserve);
          genint(maxstrlen + 1);
          verifytoken(lpar, nolparerr);
          expression(follow + [comma, colon, rpar], false);
          if resultform = chars then genunary(chrstrop, chars)
          else if (resultform = arrays) and resultptr^.stringtype then
            genunary(arraystrop, arrays)
          else if not (resultform in [strings, none]) then
            warnbefore(nostringerr);
          strlen := oprndstk[sp].oprndlen;
          oprndstk[sp].oprndlen := ptrsize;
          genunary(pushaddr, strings);
          verifytoken(comma, nocommaerr);
          expression(follow + [comma], false);
          if not (resultform in [none, ints]) then warn(badfunctionarg);
          setshorttargetintsize;
          genunary(pushvalue, ints);
          verifytoken(comma, nocommaerr);
          expression(follow + [rpar], false);
          if not (resultform in [none, ints]) then warn(badfunctionarg);
          setshorttargetintsize;
          genunary(pushvalue, ints);
          parseextraargs;
          newstringtype(resulttype, strings, strlen);
          newresulttype(resulttype);
          paramform := strings;
{ Must remove the parameters from the evaluation stack, since copy is a
  function.  Genunary expects a description of the copy operator on the
  stack, not the first parameter, so this hack is only partly correct. }
          sp := sp - 2;
          oprndstk[sp].oprndlen := strlen;
        end {copy} ;


      procedure concat;

{ Parse concat(s1,s2,...sn) function.  Any number of arguments are allowed,
  but must be strings!
}


        begin {concat}
          verifytoken(lpar, nolparerr);
          stringexpr;
          repeat
            verifytoken(comma, nocommaerr);
            stringexpr;
            genbinary(plusop, strings);
          until not (token in follow + [comma]);
          verifytoken(rpar, norparerr);
        end {concat} ;


      procedure pos;

{ Parse "pos(string1, string2)".
}


        begin {pos}
          genlit(0);
          genop(reserve);
          genint(shorttargetintsize);
          verifytoken(lpar, nolparerr);
          stringsource;
          verifytoken(comma, nocommaerr);
          stringsource;
          verifytoken(rpar, norparerr);
          newresulttype(intindex);
{ Must remove the parameters from the evaluation stack, since pos is a
  function.  Genunary expects a description of the pos operator on the
  stack, not the parameter, so this hack is only partly correct. }
          sp := sp - 1;
          oprndstk[sp].oprndlen := shorttargetintsize;
          paramform := strings;
        end {pos} ;


      procedure snglfn;

      { Convert double to single
      }


        begin {snglfn}
          if switcheverplus[doublereals] then warnbefore(badcvtfunc);

          beginparams;

          if resultform = ints then
            begin
            newresulttype(realindex);
            finishparams([none, reals], realindex);
            genunary(float, ints);
            end
          else
            begin
            finishparams([none, doubles], realindex);
            genunary(dbl_to_real, doubles);
            end;
        end; {snglfn}


      procedure dblfn;

      { Convert single to double
      }


        begin {dblfn}
          if switcheverplus[doublereals] then warnbefore(badcvtfunc);

          beginparams;

          if resultform = ints then
            begin
            newresulttype(doubleindex);
            finishparams([none, doubles], doubleindex);
            genunary(float_double, ints);
            end
          else
            begin
            finishparams([none, reals], doubleindex);
            genunary(real_to_dbl, reals);
            end;
        end; {dblfn}


      procedure mc68881_realfn1(procid: standardids {func no.} );

      { Process a special 68881 inline function that has one real or double
        argument.  An integer argument is converted to real.
      }


        begin
          if targetmachine = mc68000 then { allows dead coding }
            if switcheverplus[fpc68881] then
              begin
              beginparams;
              finishparams([none, reals, doubles], resulttype);
              end
            else illegalident(varindex)
          else illegalident(varindex);
        end; {mc68881_realfn1}


      procedure mc68881_realfn2(procid: standardids {func no.} );

      { Process a special 68881 inline function that has two real or double
        arguments.  If the two arguments are in the set real, double or
        integer, but are not the same type, the arg(s) will be converted
        according to the same rules used for assignments.
      }

        var
          firstform: types; {form of first argument}


        begin
          if targetmachine = mc68000 then { allows dead coding }
            if switcheverplus[fpc68881] then
              begin
              verifytoken(lpar, nolparerr);
              expression(follow + [comma], false);

              if resultform = ints then
                begin
                newresulttype(realindex);
                genunary(float, ints)
                end;

              firstform := resultform;

              if not (resultform in [none, reals, doubles]) then
                warn(paramtypeerr);

              verifytoken(comma, nocommaerr);
              expression(follow + [rpar], false);

              if resultform = ints then
                begin
                newresulttype(realindex);
                genunary(float, ints)
                end;

              if not (resultform in [none, reals, doubles]) or
                 (resultform <> firstform) then
                warn(paramtypeerr);

              computeresult(false);
              genbinary(dummyarg2op, resultform);
              paramform := resultform;
              parseextraargs;
              end
            else illegalident(varindex)
          else illegalident(varindex);
        end; {mc68881_realfn2}


      procedure mc68881_fint;

      { Process the special 68881 inline function FINT.  The argument is
        real or double, the result is integer.
      }


        begin
          if targetmachine = mc68000 then { allows dead coding }
            if switcheverplus[fpc68881] then
              begin
              beginparams;
              finishparams([none, reals, doubles], intindex);
              paramform := ints;
              end
            else illegalident(varindex)
          else illegalident(varindex);
        end; {mc68881_fint}


      procedure mc68881_intfn(procid: standardids {func no.} );

      { Process a special 68881 inline function with a constant integer
        argument.
      }


        begin
          if (targetmachine = mc68000) then { allows dead coding }
            if switcheverplus[fpc68881] then
              begin
              verifytoken(lpar, nolparerr);
              pushconstant(follow + [rpar]);
              if procid = fmovecrid then finishparams([none, ints], realindex)
              else finishparams([none, ints], intindex)
              end
            else illegalident(varindex)
          else illegalident(varindex);
        end; {mc68881_intfn}


      begin {standardfunctions}
        if not (procid in
           [refid, loopholeid, sizeid, bitsizeid, ordid, chrid, succid, predid,
           lengthid, concatid, snglid, dblid, upperid, lowerid]) then
          genlit(ord(procid));
        gettoken;
        case procid of
          posid: pos;
          copyid: copy;
          concatid: concat;
          sinid, cosid: sincosfn;
          expid, lnid, sqrtid, arctanid: transcendentals;
          lengthid: lengthfunction;
          timeid: time;
          oddid: oddfunction;
          absid: absfunction;
          sqrid: sqrfunction;
          truncid, roundid: truncround;
          ordid: ordfunction;
          chrid: chrfunction;
          succid, predid: succpred;
          eofid, eolnid: eofeoln;
          sizeid, bitsizeid: sizefunction;
          lowerid, upperid: lowerupperfunction;
          loopholeid: loopholefunction;
          refid: reffunction;
          ioerrorid: iofunction(boolindex);
          iostatusid: iofunction(intindex);
          snglid: snglfn;
          dblid: dblfn;

          facosid, fasinid, fatanid, fatanhid, fcoshid, fetoxm1id, fgetexpid,
          fgetmanid, flog10id, flog2id, flognp1id, fsinhid, ftanid, ftanhid,
          ftentoxid, ftwotoxid:
            mc68881_realfn1(procid);

          fmodid, fremid, fscaleid, fsgldivid, fsglmulid:
            mc68881_realfn2(procid);

          fmovecrid, readfpcrid: mc68881_intfn(procid);

          fintid: mc68881_fint;

          otherwise warn(compilerwritererr)
          end;
        if not (procid in
           [refid, sizeid, bitsizeid, ordid, chrid, loopholeid, succid, predid,
           lengthid, concatid, snglid, dblid, upperid, lowerid]) then
          genunary(sysfn, paramform);
      end {standardfunctions} ;


    procedure setexpression;

{ Parse an expression which is part of a set constructor.  This checks
  that the expression is compatible with prior expressions in the
  set constructor, and that it is a legal base type for a set.

  This kludge is necessary because there is no way to tell the type of
  a constructed set except by guessing from the  expressions included.

  The basetype is set to "noneindex" before the first expression is
  parsed, and this is compatible with any type.
}


      begin {setexpression}
        expression(follow + [dotdot, comma, rbrack], false);
        setisconst := setisconst and (oprndstk[sp].operandkind = constoperand);
        with setentry do
          begin
          if not compatible(basetype, resulttype) then
            warnbefore(badsetexpression)
          else
            begin
            stripsubrange(resulttype);
            basetype := resulttype;
            end;
          if basetype = intindex then basetype := subrangeindex;
          if bigcompilerversion then baseptr := @(bigtable[basetype]);
          if not (getform(baseptr) in
             [ints, chars, bools, scalars, subranges, none]) then
            warnbefore(badsetbase)
          else gencheck(rangechkop, basetype);
          end;
      end {setexpression} ;


    begin {factor}
      newresulttype(noneindex);
      if token in begfactset then
        case token of
          stringconst:
            begin
            if scanalys then
              begin
              thistoken.pos := stringfilecount + 1;
              dumpstr(thistoken.len + 1, curstringbuf, true);
              end;
            pushconstant(follow);
            if bigcompilerversion then resultptr := @(bigtable[resulttype]);
            resultptr^.align := 0;
            resultptr^.disposable := true;
            end;
          intconst, charconst, realconst, dblrealconst, nilsym:
            pushconstant(follow);
          ident:
            begin
            search(varindex);
            if varindex = 0 then illegalident(varindex)
            else
              begin
              if bigcompilerversion then varptr := @(bigtable[varindex]);
              with varptr^ do
                case namekind of
                  noname: illegalident(varindex);
                  constname, scalarname, typename:
                    begin
                    pushconstant(follow);
                    if token in [lbrack, dot, uparrow] then
                      begin
                      if token = lbrack then warnnonstandard(arrayexpected)
                      else if token = dot then warnnonstandard(recordexpected);
                      constpart := 0;
                      unpacking := false;
                      constantlen := sizeof(resultptr, false);
                      dumpconst(constantlen, false);
                      genoprnd;
                      sp := sp + 1;
                      oprndstk[sp].operandkind := varoperand;
                      oprndstk[sp].cost := 0;
                      ownvar := false;
                      selector(true, false, true, unpacking, constpart,
                               constantlen, off, varlev, ownvar);
                      lastindex(unpacking, constpart, constantlen);
                      if unsigned(resultptr, constantlen, unpacking) then
                        genop(unsvarop)
                      else genop(varop);
                      with oprndstk[sp].value_range do
                        begin
                        settyperange(resultptr, optimistic);
                        pessimistic := optimistic;
                        end;
                      genint(constantlen);
                      genint(0);
                      genint(lastfilekey);
                      genint(0);
                      lastfilekey := lastfilekey - 1;
                      oprndstk[sp].oprndlen := sizeof(resultptr, false);
                      end;
                    end;
                  varname, fieldname, param, varparam, confparam, varconfparam,
                  boundid:
                    variable(true, true, true, false, true, varindex);
                  forwardfunc, externalfunc, funcname: procedurecall(varindex);
                  funcparam: paramcall(varindex);
                  procname, forwardproc, externalproc, procparam, standardproc:
                    begin
                    warn(badprocfunc);
                    illegalident(varindex);
                    end;
                  undefname, undeftypename: illegalident(varindex);
                  standardfunc: standardfunctions(procid);
                  end;
              end;
            end;
          notsym:
            begin
            gettoken;
            factor;
            if switchcounters[standard] > 0 then checkboolean
            else if not (resultform in [ints, bools, none]) then
              warnbefore(badarithtype);
            genunary(notop, resultform);
            end;
          lpar:
            begin
            gettoken;
            expression(follow + [rpar], false);
            verifytoken(rpar, norparerr);
            end;
          lbrack:
            begin
            gettoken;
            enterform(sets, settype, setptr);
            setentry := setptr^;
            setisconst := true;
            bumpsp;
            with oprndstk[sp] do
              begin
              oprndlen := 0;
              typeindex := settype;
              extended := false;
              operandkind := constoperand;
              cvalue.representation := sets;
              new(cvalue.setvalue);
              cvalue.setvalue^ := [];
              end;
            with setentry do
              begin
              size := (maxsetord + 1) div bitsperunit;
              basetype := noneindex;
              constructedset := true;
              genop(newset);
              while token in
                    [dotdot, comma, eql..andsym, ident, intconst..stringconst,
                    lpar, notsym, nilsym] do
                begin
                setexpression;
                if token = dotdot then
                  begin
                  setelementtype := resulttype;
                  gettoken;
                  setexpression;
                  genbinary(setpair, ints);
                  end
                else genunary(setelt, ints);
                if token <> rbrack then verifytoken(comma, nocommaerr);
                genoprnd;
                end;
              if bigcompilerversion then baseptr := @(bigtable[basetype]);
              if getform(baseptr) in
                 [ints, chars, bools, scalars, subranges] then
                begin
                size := ((upper(baseptr) + bitsperunit) div bitsperunit);
                if not packedflag and (size > unitsize) then
                  size := forcealign(size, setalign, false);
                end;
              oprndstk[sp].oprndlen := size;
              end;
            if bigcompilerversion then setptr := @(bigtable[settype]);
            setptr^ := setentry;
            newresulttype(settype);
            verifytoken(rbrack, norbrackerr);
            genunary(bldset, sets);
            {kludge: can't make it const as genoprnd will re-emit the structop,
             can't leave it exproperand as param push will allow modification.
             Could have made a special operandtype for this but seemed like
             too much trouble.
            }
            if setisconst then oprndstk[sp].operandkind := varoperand;
            end
          end
      else
        begin
        warnbetween(nooprnderr);
        bumpsp;
        oprndstk[sp].operandkind := exproperand;
        oprndstk[sp].typeindex := noneindex
        end
    end {factor} ;


  procedure expression(follow: tokenset; {legal following sym}
                       arrayopt: boolean {true if array opt wanted} );

{ Syntactic routine to parse an expression (with mods).

  Productions:

  expression = simple-expression [ relational-operator
        simple-expression ]  .

  relational-operator = "=" | "<>" | "<" | ">" | "<=" |
        ">=" | "in"  .

  The funny part about this routine is that an initial factor may have
  been parsed already when the routine is called.  This is indicated
  by setting "skipfactor" in the call.  This kludge is necessary because
  the first argument to a write procedure may be just a factor, and
  may be an entire expression. (File defaults are a pain.)

  The "linearize" parameter is set only when computing an array subscript,
  and informs the constant folder that we would like to incorporate any
  additive constants into the constant part of the array address.
}

    var
      lefttype: index; {type of the left operand}
      leftptr: entryptr; {pointer to left node, for the 'in' operator}
      op: tokentype; { relational operator (if any) }
      opseen: boolean; {set if there is actually an operator}
      exprstdstring: boolean; {set if operands are 'standard' strings}


    procedure term;

{ Syntactic routine to parse a term.

  Productions:

  term = factor [* multiplying-operator factor *]  .

  multiplying-operator = "*" | "/" | "div" | "mod" | "and"  .

  If the global "skipfactor" is set in "expression", the first call to
  factor is assumed to have been made prior to the call to term.  This
  parameter is reset to avoid skipping more factors.

  On machines where doing a divide gives a free mod the "div" and "mod"
  operators are split into two parts in the intermediate
  file.  A binary operator "divop" performs the division, and unary operators
  "remop" or "quoop" pick either the remainder or the quotient.  This takes
  advantage of the "almost universal" machine characteristic of generating
  both pieces on a divide, and allows the results to be used as common
  subexpressions.
  On those machines not of this "universe" ( i.e. ns32k )
  "div" and "mod" are treated just like any other binary operator.
}

      var
        op: tokentype; {operator (if found)}
        specialdiv: boolean; {may need to correct remainder for mod}
        gendone: boolean; { true if gen op already emitted }
        f: entryptr; {used to get lower bound for div/mod}


      begin {term}
        if not skipfactor then factor;
        skipfactor := false;
        while token in termops do
          begin
          gendone := false;
          if (token = slash) and (resultform = ints) then
            begin
            newresulttype(realindex);
            genunary(float, ints)
            end;
          op := thistoken.token;
          gettoken;
          factor;
          computeresult(false);
          if (op = andsym) and (switchcounters[standard] > 0) then checkboolean
          else
            begin
            case op of
              andsym:
                if not (resultform in [ints, bools, none]) then
                  warnbefore(badarithtype);
              star:
                if not (resultform in [sets, ints, reals, doubles, none]) then
                  warnbefore(badarithtype);
              divsym, modsym:
                begin

                case targetmachine of
                  ns32k:
                    begin
                    { the 32k almost never gets a free mod with a div.
                      If you wanted to get fancy you could figure out
                      here which operations that happens on and pass the
                      operators like the other machines. I can't believe
                      that it is worth it since the free case is rare
                      and this code would execute always. Only real advantage
                      is in readaccess/writeaccess which is irrelevant on
                      a vm based 32k.
                    }
                    specialdiv := true;
                    with oprndstk[sp - 1] do
                      if (value_range.pessimistic.minlimit >= 0) or
                         extended then
                        with oprndstk[sp] do
                          if (value_range.pessimistic.minlimit >= 0) or
                             extended then
                            specialdiv := false
                          else
                            begin
                            if bigcompilerversion then
                              f := @(bigtable[oprndstk[sp - 1].typeindex]);
                            if (lower(f) >= 0) or oprndstk[sp - 1].extended then
                              begin
                              if bigcompilerversion then
                                f := @(bigtable[oprndstk[sp].typeindex]);
                              if (lower(f) >= 0) or oprndstk[sp].extended then
                                specialdiv := false;
                              end;
                            end;
                    gendone := true;
                    if (op = modsym) then
                      begin
                      if specialdiv then genbinary(stdmodop, ints)
                      else genbinary(modop, ints);
                      end
                    else genbinary(kwoop, ints);
                    end;
                  otherwise
                    begin
                    { "universal machines"}
                    specialdiv := true;
                    with oprndstk[sp - 1] do
                      if (value_range.pessimistic.minlimit >= 0) or
                         extended then
                        with oprndstk[sp] do
                          if (value_range.pessimistic.minlimit >= 0) or
                             extended then
                            specialdiv := false
                          else
                            begin
                            if bigcompilerversion then
                              f := @(bigtable[oprndstk[sp - 1].typeindex]);
                            if (lower(f) >= 0) or oprndstk[sp - 1].extended then
                              begin
                              if bigcompilerversion then
                                f := @(bigtable[oprndstk[sp].typeindex]);
                              if (lower(f) >= 0) or oprndstk[sp].extended then
                                specialdiv := false;
                              end;
                            end;
                    if specialdiv then genbinary(stddivop, ints)
                    else genbinary(divop, ints);
                    end; {universal}
                  end; {case}
                if not (resultform in [ints, none]) then
                  warnbefore(badarithtype);
                end;
              slash:
                if not (resultform in [reals, doubles, none]) then
                  warnbefore(badarithtype);
              end;
            end;
          if not gendone then
            begin
            if op in [divsym, modsym] then genunary(optable[op], resultform)
            else genbinary(optable[op], resultform);
            end;
          end;
      end {term} ;


    procedure simpleexpression;

{ Syntactic routine to parse a simple-expression.

  Productions:

  simple-expression = [ sign ] term [* adding-operator term *]  .

  adding-operator = "+" | "-" | "or"  .

  No special action is taken.
}

      var
        op: tokentype; {operator if found}
        signed: boolean; {set if initial sign found}
        negate: boolean; {set if initial sign was minus}


      begin {simpleexpression}
        signed := (token in [plus, minus]) and not skipfactor;
        if signed then
          begin
          negate := (token = minus);
          gettoken;
          end;
        term;
        if signed then
          begin
          if resultform = sets then warnbefore(signedseterr)
          else if not (resultform in [ints, reals, doubles, none]) then
            warnbefore(badarithtype);
          if negate then genunary(negop, resultform);
          end;
        while token in sexprops do
          begin
          op := thistoken.token;
          gettoken;
          term;
          computeresult(op = plus);
          if (op = orsym) then
            begin
            if ((switchcounters[standard] > 0) and (resultform = ints) or
               not (resultform in [bools, ints, none])) then
              warnbefore(badarithtype)
            end
          else if not ((resultform in [ints, reals, doubles, sets, none]) or
                  (op = plus) and (resultform = strings)) then
            warnbefore(badarithtype);
          linearize := arrayopt;
          genbinary(optable[op], resultform);
          end;
      end {simpleexpression} ;


    begin {expression}
      simpleexpression;
      while (token in begexprset) and ((token <> ident) or
            (thistoken.line = lasttoken.line)) do
        begin
        lefttype := resulttype;
        if token in exprops then
          begin
          op := thistoken.token;
          opseen := true;
          gettoken
          end
        else
          begin
          opseen := false;
          op := eofsym;
          warnbetween(nooperr)
          end;
        simpleexpression;
        if op = insym then
          begin
          if bigcompilerversion then leftptr := @(bigtable[lefttype]);
          if (resultform <> sets) or
             not (leftptr^.typ in
             [none, ints, chars, scalars, bools, subranges]) or not compatible(lefttype,
                                                         resultptr^.basetype)
             then
            warnbefore(badinoprnds);
          end
        else
          begin
          computeresult(false);
          if resultform = arrays then
            begin
            if not resultptr^.stringtype then warnbefore(badreloprnds);
            end
          else
            begin
            case op of
              lss, gtr:
                begin
                if resultform = sets then warnbefore(nostrictinclusion)
                else if not (resultform in
                        [sets, ints, reals, doubles, bools, chars, scalars,
                        strings, none]) then
                  warnbefore(badarithtype)
                end;
              eql, neq:
                if not (resultform in
                   [sets, ptrs, ints, reals, doubles, bools, chars, scalars,
                   strings, none]) then
                  warnbefore(badarithtype);
              leq, geq:
                if not (resultform in
                   [sets, ints, reals, doubles, bools, chars, scalars, strings,
                   none]) then
                  warnbefore(badarithtype);
              otherwise {do nothing} ;
              end;
            end;
          end;
        if opseen then genbinary(optable[op], resultform)
        else if sp >= 0 then sp := sp - 1;
        newresulttype(boolindex);
        with oprndstk[sp], value_range do
          begin
          typeindex := resulttype;
          oprndlen := scalarsize;
          { Set new result range however if we already have the result
            due to folding don't throw it away!
          }
          if operandkind <> constoperand then
            begin
            settyperange(resultptr, optimistic);
            pessimistic := optimistic;
            end;
          end;
        verify1(follow + [rpar] + begexprset, badexprerr);
        end;
      while not (rpar in follow) and (token = rpar) do
        begin
        warn(badrparerr);
        gettoken
        end;
      verify1(follow, badexprerr);
    end {expression} ;


  procedure standardprocedures(procid: standardids {which std proc} );

{ Parse a standard procedure.  The syntax is similar to that for a user
  procedure, so will not be repeated here.

  This is complicated by the "default" file parameter for read and write,
  and the variety of types which these procedures accept.  Also, new and
  dispose require special processing for the optional tag arguments.
}


    procedure pushstringparam(extendedstring: boolean {arrays or strings} );

{ Push a string parameter onto the operand stack.  This actually generates
  code to push a reference to the string, followed by a the string length.

  Both standard Pascal strings (packed array [1..n] of char) and extended
  strings (string[n]) are allowed.

  This is used to handle string parameters to "write" and for the optional
  file specification parameters to reset and rewrite.
}

      var
        stringlen: integer; {length of the string}


      begin {pushstringparam}
        if extendedstring then
          begin
          oprndstk[sp].oprndlen := ptrsize + defaulttargetintsize;
          genunary(pushstraddr, strings);
          end
        else
          begin
          stringlen := resultptr^.arraymembers;
          if constcheck(sp) then dumpconst(stringlen, false);
          oprndstk[sp].oprndlen := ptrsize;
          genunary(pushaddr, ints);
          genpushdefaultint(stringlen);
          end;
      end {pushstringparam} ;


    procedure ioprocedure(textfileneeded: boolean {must be a text file} );

{ Parse simple, one argument file procedures.
}


      begin {ioprocedure}
        fileparam(true, false, textfileneeded);
        parseextraargs;
      end {ioprocedure} ;


    procedure seek;

{ Process a "seek" procedure. First parameter is file, but otherwise
  nothing special.
}


      begin {seek}
        fileparam(true, false, false);
        verifytoken(comma, nocommaerr);
        expression(follow + [comma, rpar], false);
        if not (resultform in [none, ints]) then warn(badfunctionarg);
        setdefaulttargetintsize;
        genunary(pushvalue, ints);
        parseextraargs;
      end {seek} ;


    procedure getname(blankallowed: boolean {true sez blank field ok} );

{ Get a file name parameter for reset or rewrite.  This must be a string of
  some flavor, or blank (i.e. reset(f,'x',,i) ).
}


      begin {getname}
        verifytoken(comma, nocommaerr);
        if blankallowed and (token = comma) then
          begin
          sp := sp + 1;
          oprndstk[sp] := nilvalue;
          genunary(pushvalue, ptrs);
          genpushdefaultint(0);
          end
        else
          begin
          expression(follow + [comma, rpar, intconst..stringconst], false);
          if (resultform = arrays) and resultptr^.stringtype or
             (resultform = strings) then
            pushstringparam(resultform = strings)
          else if (resultform <> none) then warnbefore(nostringerr);
          end;
      end {getname} ;


    procedure rename;

{ Process the "rename" procedure.  Both the file and string parameters
  are required.
}


      begin {rename}
        fileparam(true, false, false);
        getname(false);
        parseextraargs;
      end {rename} ;


    procedure resetrewrite;

{ Process a "reset" or "rewrite" procedure.  This has to deal with the
  optional parameters for file specifications.
}


      begin {resetrewrite}
        fileparam(true, true, false);
        genunary(setfileop, none);
        if token in
           [comma, eql..andsym, ident, intconst..stringconst, lbrack, lpar,
           notsym, nilsym] then
          begin
          warnnonstandard(filenameerr);
          getname(true);
          if token in
             [comma, eql..andsym, ident, intconst..stringconst, lbrack, lpar,
             notsym, nilsym] then
            getname(true);
          if token in [comma, ident] then
            begin
            verifytoken(comma, nocommaerr);
            modifyvariable(true, false);
            if not identical(resulttype, intindex) then
              warnbefore(paramtypeerr);
            oprndstk[sp].oprndlen := ptrsize;
            genunary(pushaddr, ints);
            end;
          end;
        parseextraargs;
      end {resetrewrite} ;


    procedure newdispose;

{ Parse a "new" or "dispose" call.  The optional arguments are used to chain
  down the tree of variants to get the size of the specified variant.  The
  final size result is passed to the routine as a second argument.
}

      var
        n: index; {name entry, from search}
        p: entryptr; {used to get currentrecord from name table}
        currentrecord: index; {current part of record spec}
        currentptr: entryptr; {used to access currentrecord}
        f: entryptr; {used for general form access}
        tagtype: index; { tagfield type }
        callnew: boolean; {true if new instead of dispose}
        containedfile: boolean; {true if parent record contains file}


      begin {newdispose}
        containedfile := false;
        callnew := procid = newid;
        genop(bldnil);
        verifytoken(lpar, nolparerr);
        newresulttype(noneindex);
        if token = ident then
          begin
          if callnew then modifyvariable(true, true)
          else
            begin
            search(n);
            if n <> 0 then
              begin
              if bigcompilerversion then p := @(bigtable[n]);
              if p^.namekind in
                 [funcname, forwardfunc, externalfunc, funcparam] then
                factor
              else modifyvariable(true, true);
              end
            else illegalident(n);
            end;
          if not (resultform in [none, ptrs]) then warnbefore(noptrvar);
          genunary(pushaddr, ptrs);
          end
        else warnbetween(novarerr);
        currentrecord := noneindex;
        with resultptr^ do
          if typ = ptrs then
            begin
            if bigcompilerversion then p := @(bigtable[ptrtypename]);
            currentrecord := p^.typeindex;
            if p^.namekind = undeftypename then
              begin
              if not bigcompilerversion then blocksin[1].written := true;
              p^.lastoccurrence := display[level].scopeid;
              end;
            if bigcompilerversion then
              currentptr := @(bigtable[currentrecord]);
            containedfile := currentptr^.containsfile;
            end;
        parsetagparams(currentrecord);
        if bigcompilerversion then currentptr := @(bigtable[currentrecord]);
        pushint(sizeof(currentptr, false));
        oprndstk[sp].oprndlen := defaultptrsize;
        genunary(pushvalue, ints);
        parseextraargs;
        if not callnew and containedfile then genunary(closerangeop, none);
      end {newdispose} ;

    procedure filehack(filetype: index;
                       reading: boolean);

{ Generate intcode to access one file element.  The vast number of indirect
  operations is due to the fact that we placed the address of the file var
  on the stack when it was processed for the forthcoming "get/put" operation.
  Thus, we have mem[sp]->filevar->buffptr->data, three indirects in all.
}

      var
        f: entryptr; {for access to filetype}


      begin {filehack}
        genunary(setbinfileop, files);
        genunary(indrop, files);
        if reading then genunary(definelazyop, files);
        genunary(indrop, files);
        genunary(indrop, files);
        genop(newvarop);
        bumpsp;
        if bigcompilerversion then f := @(bigtable[filetype]);
        newresulttype(f^.filebasetype);
        with oprndstk[sp] do
          begin
          typeindex := resulttype;
          oprndlen := sizeof(resultptr, false);
          genint(oprndlen);
          operandkind := exproperand;
          cost := 0;
          extended := resultptr^.extendedrange;
          end;
        genint(0);
        genint(0);
        genint(0);
      end {filehack} ;


    procedure gencopystack(reservelen: integer {stack space to reserve for
                                                result value} );

{ Generate special code to copy the top element of the runtime stack,
  which is assumed by this routine to be the explicitly named file
  argument to "read" or "write".  Complicated by the fact that the
  travrs operand stack (for building nodes) might or might not already
  contain the read/write argument.
}


      begin {gencopystack}
        if not constcheck(sp) then genop(switchstack);
        genop(copystackop);
        genint(reservelen);
        genint(0);
        genform(ptrs);
        if not constcheck(sp) then genop(switchstack);
      end {gencopystack} ;


    procedure readprocedure;

{ Process a read procedure call.  The first argument may be a file, in
  which case that file is used, otherwise the file "input" is used.
}

      var
        filetype: index;
        f: entryptr; {for access to filetype}
        readflag: boolean; {true if "read", rather than "readln"}


      procedure readparams;

{ Parse read parameters.  These look like any other var parameters,
  except that many types of parameters are allowed.
}

        var
          readform: types; {type of parameter}
          readfile: boolean; {true if read(filevar, ...) form}
          restoresp: - 1..oprnddepth; {for restoring operand sp after each read}
          filetype: index;
          readfiledeclared: boolean; {set true if file was declared in program
                                      header}


        procedure genoneread;

{ Generate the intermediate file output for one read parameter.  This
  is done with a special operation "rd".
}

          var
            resultlen: integer; {length of result from read}


          begin {genoneread}
            if readform <> files then
              begin
              if readform in [ints, reals, doubles, chars] then
                begin
                if readform = ints then resultlen := defaulttargetintsize
                else resultlen := sizeof(resultptr, false);
                gencopystack(resultlen);
                end
              else if readfile and ((token <> rpar) or not readflag) then
                gencopystack(0);

              if not (readform in [none, ints, reals, doubles, chars]) then
                begin
                if ((readform <> arrays) or not resultptr^.stringtype) and
                   (readform <> strings) or (switchcounters[standard] > 0) then
                  warnbefore(badreadtype)
                else pushstringparam(false);
                end;
              end;
            genunary(rd, readform);
          end {genoneread} ;


        procedure onereadparam;

{ Parse one read parameter.  This must be a variable.
}


          begin {onereadparam}
            sp := restoresp;
            if token = ident then
              begin
              if filetype <> textindex then filehack(filetype, true);
              modifyvariable(true, true);
              readform := resultform;
              if filetype <> textindex then
                begin
                if bigcompilerversion then f := @(bigtable[filetype]);
                if not compatible(resulttype, f^.filebasetype) then
                  warnbefore(typesincomp);
                genop(switchstack);
                genbinary(moveop, readform);
                readform := files;
                end;
              parsecolons(0);
              verify1([rpar, comma, ident], badparamerr)
              end
            else
              begin
              warnbetween(novarerr);
              newresulttype(noneindex);
              end;
          end {onereadparam} ;


        begin {readparams}
          sp := - 1;
          readfile := false;
          restoresp := sp;
          verifytoken(lpar, nolparerr);
          filetype := textindex;
          readfiledeclared := filedeclared;
          onereadparam;
          if resultform = files then
            begin
            if not readfiledeclared then warnbefore(filenotdeclared);
            restoresp := sp;
            filetype := resulttype;
            if (filetype <> textindex) and (procid = readlnid) then
              warnbefore(nottextfile);
            if (procid = readid) and (token = rpar) then warn(noreadarg);
            genunary(pushaddr, ptrs);
            if filetype = textindex then genunary(setfileop, none);
            readfile := true;
            end
          else
            begin
            if not inputdeclared then warnnonstandard(inputnotdeclared);
            standardfilesreferenced := true;
            genoneread;
            end;
          while token in [comma, ident] do
            begin
            verifytoken(comma, nocommaerr);
            onereadparam;
            if resultform = files then warnbefore(badreadtype);
            genoneread;
            end;
          parseextraargs;
        end {readparams} ;


      begin {readprocedure}
        genop(bldnil);
        readflag := procid = readid;
        if readflag and (token <> lpar) then warnbetween(noreadarg)
        else if token = lpar then readparams
        else
          begin
          if not inputdeclared then warnnonstandard(inputnotdeclared);
          standardfilesreferenced := true;
          end;
      end {readprocedure} ;


    procedure writeprocedure;

{ Process a write procedure.  This is complicated by the optional file
  argument (as in read) and by the optional field width arguments set
  by colons.  As in the read procedure, each write argument is passed
  with a special argument.
}

      var
        writeflag: boolean; {true if write instead of writeln}
        filetype: index;
        f: entryptr; {for access to filetype}


      procedure writeparams;

{ Parse the parameters to a write procedure.
}

        var
          restoresp: - 1..oprnddepth; {used to restore operand stack}
          writefile: boolean; {true if write(f,...) form}
          writefiledeclared: boolean; {set true if file was declared in program
                                       header}


        procedure onewriteparam(varread: boolean {var already read?} );

{ Parse a single parameter to a write procedure.  The flag "varread" is
  set if a variable has been read to check for the default file parameter.
}

          var
            writeform: types; {form of argument}
            writelen: addressrange; {length of write parameter}
            stringflag: boolean; {used to check for string argument}
            basetype: index; {pointer to base type}

          begin {onewriteparam}
            if filetype <> textindex then filehack(filetype, false);
            skipfactor := varread;
            expression(follow + [comma, rpar, colon], false);
            stripsubrange(resulttype);
            newresulttype(resulttype);
            writeform := resultform;
            if writeform = ints then setdefaulttargetintsize;
            writelen := oprndstk[sp].oprndlen;

            if filetype = textindex then
              begin
              {set stringflag before upcoming genunary modifies resulttype}
              if writeform = arrays then stringflag := resultptr^.stringtype
              else stringflag := writeform = strings;

              if ((token <> rpar) or not writeflag) then gencopystack(0);

              case writeform of
                bools, chars, ints:
                  begin
                  genunary(pushvalue, writeform);
                  parsecolons(1);
                  end;
                none, reals, doubles:
                  begin
                  genunary(pushvalue, writeform);
                  parsecolons(2);
                  end;
                arrays, strings:
                  begin
                  if not stringflag then warnbefore(badwritearg)
                  else pushstringparam(writeform = strings);
                  parsecolons(1)
                  end;
                otherwise
                  begin
                  warnbefore(badwritearg);
                  parsecolons(maxint)
                  end
                end;

              end
            else
              begin
              if bigcompilerversion then f := @(bigtable[filetype]);
              basetype := f^.filebasetype;
              if bigcompilerversion then f := @(bigtable[basetype]);
              if ((getform(f) = ints) and ((writeform = reals) or
                 (writeform = doubles))) or
                 not compatible(basetype, resulttype) then
                warnbefore(typesincomp);
              genbinary(moveop, resultform);
              writeform := files;
              end;
            genop(wr);
            genint(writelen);
            genint(0);
            genform(writeform);
            sp := restoresp;
          end {onewriteparam} ;


        begin {writeparams}
          writefile := false;
          restoresp := - 1;
          verifytoken(lpar, nolparerr);
          filetype := textindex;
          if token = ident then
            begin
            writefiledeclared := filedeclared;
            factor;
            if resultform = files then
              begin
              if not writefiledeclared then warnbefore(filenotdeclared);
              writefile := true;
              filetype := resulttype;
              verify1([comma, rpar], badparamerr);
              genunary(pushaddr, ptrs);
              restoresp := sp;
              if filetype = textindex then genunary(setfileop, none);
              if (filetype <> textindex) and (procid = writelnid) then
                warnbefore(nottextfile);
              if writeflag and (token = rpar) then warn(nowritearg)
              end
            else
              begin
              if not outputdeclared then warnnonstandard(outputnotdeclared);
              onewriteparam(true);
              standardfilesreferenced := true;
              end
            end
          else
            begin
            if not outputdeclared then warnnonstandard(outputnotdeclared);
            onewriteparam(false);
            standardfilesreferenced := true;
            end;
          while token in
                [comma, eql..andsym, ident, intconst..stringconst, lbrack,
                lpar, notsym, nilsym] do
            begin
            verifytoken(comma, nocommaerr);
            onewriteparam(false)
            end;
          parseextraargs;
        end {writeparams} ;


      begin {writeprocedure}
        genop(bldnil);
        writeflag := (procid = writeid);
        if not writeflag and (token <> lpar) then
          begin
          if not outputdeclared then warnnonstandard(outputnotdeclared);
          standardfilesreferenced := true;
          end;
        if writeflag then verify([lpar], begexprset, nolparerr);
        if token in begexprset then writeparams;
      end {writeprocedure} ;


    procedure pusharrayparam(packedarray, {which kind of array}
                              assigning: boolean; {true if assigning new value}
                             var indxtype: index; {type of index}
                             var elttype: index {type of element} );

{ Push one array parameter for the intrinsic routines pack/unpack.
  Items pushed include the address of the array, lower/upper bounds,
  and size.  If packedarray is true, ord(bitaddress) is pushed as well.
}

      var
        varindex: index; {used to search variable}
        eltsize: addressrange; {size of one array element}
        packedelt: boolean; {set true if element is < bitsperunit}
        packing: boolean; {set if a packed array}
        highid, lowid: index; {id's for bound identifiers}
        f: entryptr; {used to get index and element data}


      begin {pusharrayparam}

        if token = ident then search(varindex);
        if assigning or (token <> ident) or (varindex = 0) then
          modifyvariable(true, true)
        else variable(true, true, false, true, true, varindex);

        if resultform in [arrays, conformantarrays] then
          begin
          with resultptr^ do
            begin
            if packedflag <> packedarray then warnbefore(badparamerr);
            elttype := elementtype;
            indxtype := indextype;
            eltsize := elementsize;
            packedelt := packedflag;
            packing := packedarray;
            if resultform = conformantarrays then
              begin
              highid := highbound;
              lowid := lowbound;
              end
            else if bigcompilerversion then f := @(bigtable[indextype]);
            end;
          oprndstk[sp].oprndlen := ptrsize;
          genunary(pushaddr, ints);
          if resultform = conformantarrays then
            begin
            variable(true, false, false, false, false, lowid);
            setdefaulttargetintsize;
            genunary(pushvalue, ints);
            variable(true, false, false, false, false, highid);
            setdefaulttargetintsize;
            genunary(pushvalue, ints);
            end
          else
            begin
            genpushdefaultint(lower(f));
            genpushdefaultint(upper(f));
            end;
          if packedelt and (eltsize > bitsperunit) then
            begin
            packedelt := false;
            eltsize := eltsize div bitsperunit;
            end;
          genpushdefaultint(eltsize);
          if packing then
            begin
            genpushbool(packedelt);
            if bigcompilerversion then f := @(bigtable[elttype]);
            genpushbool(unsigned(f, eltsize, packedelt));
            end;
          end
        else
          begin
          warnbefore(arrayexpected);
          elttype := noneindex;
          end;
      end {pusharrayparam} ;


    procedure pack;

{ Compile code for the pack procedure.  Since nobody in his right
  mind would use this silly thing except for compatibility with
  existing, archaic implementations, no effort has been made to
  make this procedure particularly efficient.  Indeed, a general
  packing routine is called at runtime, and is guaranteed to be
  less efficient than the equivalent code expressed in source form
  as a for-loop.
}

      var
        indextype, lefttype, righttype: index; {types of left and right arrays}


      begin {pack}
        indextype := 0; { Causes problems if not initialized and type is "none"}
        lefttype := 0;
        righttype := 0;
        genop(bldnil);
        verifytoken(lpar, nolparerr);
        pusharrayparam(false, false, indextype, lefttype);
        verifytoken(comma, nocommaerr);
        expression(follow + [comma, rpar], false);
        if not compatible(indextype, resulttype) then warnbefore(badparamerr);
        setdefaulttargetintsize;
        genunary(pushvalue, ints);
        verifytoken(comma, nocommaerr);
        pusharrayparam(true, true, indextype, righttype);
        if not identical(lefttype, righttype) then warnbefore(badparamerr);
        verifytoken(rpar, norparerr);
      end {pack} ;


    procedure unpack;

{ Compile code for intrinsic procedure unpack.  Calls runtime for help.
}

      var
        indextype, lefttype, righttype: index; {types of left and right arrays}


      begin {unpack}
        indextype := 0; { Causes problems if not initialized and type is "none"}
        lefttype := 0;
        righttype := 0;
        genop(bldnil);
        verifytoken(lpar, nolparerr);
        pusharrayparam(true, false, indextype, lefttype);
        verifytoken(comma, nocommaerr);
        pusharrayparam(false, true, indextype, righttype);
        if not identical(lefttype, righttype) then warnbefore(badparamerr);
        verifytoken(comma, nocommaerr);
        expression(follow + [comma, rpar], false);
        if not compatible(indextype, resulttype) then warnbefore(badparamerr);
        setdefaulttargetintsize;
        genunary(pushvalue, ints);
        verifytoken(rpar, norparerr);
      end {unpack} ;


    procedure page;

{ Compile standard procedure "page(filename)".  It has a non-standard
  form in that "output" is default.
}


      begin {page}
        if token = lpar then
          begin
          ioprocedure(true);
          genunary(setfileop, none);
          end
        else
          begin
          if not outputdeclared then warnnonstandard(outputnotdeclared);
          standardfilesreferenced := true;
          end;
      end {page} ;


    procedure insert;

{ Parse standard procedure "insert(stringexpr, stringvar, pos)".
}


      begin {insert}
        genop(bldnil);
        verifytoken(lpar, nolparerr);
        stringsource;
        verifytoken(comma, nocommaerr);
        stringtarget;
        verifytoken(comma, nocommaerr);
        expression(follow + [rpar], false);
        if not (resultform in [none, ints]) then warn(badfunctionarg);
        setshorttargetintsize;
        genunary(pushvalue, ints);
        parseextraargs;
      end {insert} ;


    procedure deletestr;

{ Parse "deletestr(st, pos, num)"
}


      begin {deletestr}
        genop(bldnil);
        verifytoken(lpar, nolparerr);
        stringtarget;
        verifytoken(comma, nocommaerr);
        expression(follow + [comma], false);
        if not (resultform in [none, ints]) then warn(badfunctionarg);
        setdefaulttargetintsize;
        genunary(pushvalue, ints);
        verifytoken(comma, nocommaerr);
        expression(follow + [rpar], false);
        if not (resultform in [none, ints]) then warn(badfunctionarg);
        setdefaulttargetintsize;
        genunary(pushvalue, ints);
        parseextraargs;
      end {deletestr} ;


    procedure val;

{ Parse "val(str, value, errorpointer)".
}


      begin {val}
        genop(bldnil);
        verifytoken(lpar, nolparerr);
        stringsource;
        verifytoken(comma, nocommaerr);
        modifyvariable(true, true);
        if not (identical(resulttype, intindex) or identical(resulttype,
           realindex) or identical(resulttype, doubleindex)) then
          warn(paramtypeerr);
        oprndstk[sp].oprndlen := ptrsize;
        genunary(pushaddr, resultform);
        verifytoken(comma, nocommaerr);
        modifyvariable(true, true);
        if not identical(resulttype, intindex) then warn(paramtypeerr);
        oprndstk[sp].oprndlen := ptrsize;
        genunary(pushaddr, ints);
        parseextraargs;
      end {val} ;


    procedure str;

{ Parse "str(value, stringtarget)".
}


      begin {str}
        genop(bldnil);
        verifytoken(lpar, nolparerr);
        expression(follow + [comma, colon], false);
        if not (resultform in [none, reals, doubles, ints]) then
          warn(badfunctionarg);
        if resultform = ints then setdefaulttargetintsize;
        genunary(pushvalue, resultform);
        if resultform = ints then parsecolons(1)
        else parsecolons(2);
        verifytoken(comma, nocommaerr);
        stringtarget;
        parseextraargs;
      end {str} ;


    procedure setfpcrproc;


      begin {setfpcrproc}
        if targetmachine = mc68000 then { allows dead coding }
          if switcheverplus[fpc68881] then
            begin
            genop(bldnil);
            verifytoken(lpar, nolparerr);
            pushconstant([comma, rpar, semicolon]);

            if resultform <> ints then
              begin
              warnbefore(badfunctionarg);
              newresulttype(noneindex);
              end;

            genunary(dummyargop, resultform);
            verifytoken(comma, nocommaerr);
            expression(follow + [rpar], false);

            if not (resultform in [none, ints]) then warn(paramtypeerr);

            genunary(dummyargop, resultform);
            parseextraargs;
            end
          else illegalident(varindex)
        else illegalident(varindex);
      end; {setfpcrproc}


    procedure fsincosproc;

      { Parse an fsincos procedure.  There are three arguments:  The first is
        the input argument, the second is the cosine and the third is the sine.
        The second and third arguments are output parameters.
      }

      var
        firstform: types; {form of first argument}


      begin {fsincosproc}
        if targetmachine = mc68000 then { allows dead coding }
          if switcheverplus[fpc68881] then
            begin
            genop(bldnil);
            verifytoken(lpar, nolparerr);
            expression(follow + [comma], false);

            if resultform = ints then
              begin
              newresulttype(realindex);
              genunary(float, ints)
              end;

            genunary(dummyargop, resultform);
            firstform := resultform;

            if not (resultform in [none, reals, doubles]) then
              warn(paramtypeerr);

            verifytoken(comma, nocommaerr);
            modifyvariable(false, false);

            if not (resultform in [none, reals, doubles]) or
               (resultform <> firstform) then
              warn(paramtypeerr);

            genunary(dummyargop, resultform);

            verifytoken(comma, nocommaerr);
            modifyvariable(false, false);

            if not (resultform in [none, reals, doubles]) or
               (resultform <> firstform) then
              warn(paramtypeerr);

            genunary(dummyargop, resultform);
            parseextraargs;
            end
          else illegalident(varindex)
        else illegalident(varindex);
      end; {fsincosproc}


    begin {standardprocedures}
      newexprstmt(syscall);
      genint(ord(procid));
      gettoken;
      case procid of
        readid, readlnid: readprocedure;
        writeid, writelnid: writeprocedure;
        newid: newdispose;
        disposeid: newdispose;
        pageid: page;
        putid, getid, breakid, closeid, deleteid, noioerrorid:
          ioprocedure(false);
        seekid: seek;
        renameid: rename;
        resetid, rewriteid: resetrewrite;
        packid: pack;
        unpackid: unpack;
        deletestrid: deletestr;
        insertid: insert;
        strid: str;
        valprocid: val;
        fsincosid: fsincosproc;
        setfpcrid: setfpcrproc;
        otherwise warn(compilerwritererr)
        end;
    end {standardprocedures} ;


  procedure assignlabel;

{ Assign a label to the current statement.  This must search the labellist
  at the current level to make sure that it is declared.  It also may have
  to check and see if the label has been the target of an illegal goto.
}

    var
      t: labelptr; {Label entry}


    begin {assignlabel}
      checkundefs := false;
      nolabelsofar := false;
      searchlabels(thistoken.intvalue, t);
      if (t = labelflag) or (lev <> level) then warn(labnotpredef)
      else
        with t^ do
          begin
          if definednest <> 0 then warn(badlabeldef)
          else if nest > maxlegalnest then
            warnat(badlabelnest, labelline, labelcolumn);
          definednest := nest;
          maxlegalnest := maxint;
          if nonlocalref then anynonlocallabels := true;
          end;
      genstmt(deflab);
      genint(t^.internalvalue);
      genint(lev);
      genint(ord(t^.nonlocalref));
      gettoken;
      if token = becomes then illegalassign
      else verifytoken(colon, nocolonerr);
    end {assignlabel} ;


  procedure badelseclause;

{ Generate an error message for a bad else clause, then recover.
  This is such a common error it is worth special handling.
}


    begin {badelseclause}
      warn(badelseerr);
      gettoken;
      statement(follow)
    end {badelseclause} ;


  procedure assignment;

{ Syntactic routine to parse an assignment statement.

  Productions:

  assignment-statement = variable ":=" expression  .

  This routine must transform integer expressions to real if the
  target is real, and may issue a range check for a subrange
  assignment.

  The left hand side variable is marked as modified.
}

    var
      varptr: entryptr; {Provides access to LHS variable}
      lefttype: index; {LHS type}
      leftform: types; {for compatibility checking}
      leftstdstring: boolean; {for char-to-std-string conversion}


    begin {assignment}
      variable(true, true, false, true, true, varindex);
      lefttype := resulttype;
      leftform := resultform;
      leftstdstring := (resultform = arrays) and resultptr^.stringtype;
      if token = eql then
        begin
        warn(nobecomeserr);
        gettoken
        end
      else verifytoken(becomes, nobecomeserr);
      expression(follow, false);
      if resultptr^.containsfile then warnbefore(dontassignfile)
      else if (leftform = reals) and (resultform = ints) then
        genunary(float, ints)
      else if (leftform = ints) and (resultform = reals) then
        warnbefore(badrealtoint)
      else if (leftform = doubles) and (resultform = ints) then
        genunary(float_double, ints)
      else if (leftform = ints) and (resultform = doubles) then
        warnbefore(badrealtoint)
      else if (leftform = doubles) and (resultform = reals) and
              not switcheverplus[doublereals] then
        begin
        oprndstk[sp].oprndlen := doublesize;
        genunary(real_to_dbl, reals);
        end
      else if (leftform = reals) and (resultform = doubles) then
        warnbefore(baddbltoreal)
      else if (leftform = strings) and (resultform = chars) then
        genunary(chrstrop, strings)
      else if (leftform = strings) and (resultform = arrays) and
              resultptr^.stringtype then
        genunary(arraystrop, strings)
      else
        begin
        if (switchcounters[standard] <= 0) and leftstdstring and
           (resultform = chars) then
          begin
          newstringtype(resulttype, arrays, 1);
          newresulttype(resulttype);
          oprndstk[sp].typeindex := resulttype;
          end;
        if not compatible(lefttype, resulttype) then warnbefore(badassignment);
        end;
      gencheck(rangechkop, lefttype);
      if leftform = conformantarrays then
        begin
        genvalsize(lefttype, 1);
        genoprnd;
        genbinary(cmoveop, arrays);
        end
      else genbinary(moveop, leftform);
      genoprndstmt;
      if bigcompilerversion then varptr := @(bigtable[varindex]);
      with varptr^ do
        if namekind in
           [varname, fieldname, param, varparam, confparam, varconfparam] then
          begin
          modified := true;
          parammodified := true;
          if (nest = 1) and nolabelsofar then knownvalid := true;
          end;
    end {assignment} ;


  procedure compoundstatement;

{ Syntactic routine to parse a compound statement.

  Productions:

  compound-statement = "begin" statement [* ";" statement *] "end"  .

  This does very little, just parses a statement sequence
}


    begin {compoundstatement}
      gettoken;
      nest := nest + 1;
      statement(follow + [semicolon, endsym, otherwisesym] - [elsesym]);
      while not (token in [labelsym..functionsym, endsym, eofsym]) do
        begin
        if token = semicolon then gettoken
        else verify1([semicolon], nosemierr);
        statement([semicolon, endsym, otherwisesym] + follow - [elsesym])
        end;
      updatelabelnest;
      nest := nest - 1;
      verifytoken(endsym, noenderr);
    end {compoundstatement} ;


  procedure ifstatement;

{ Syntactic routine to parse an if statement.

  Productions:

  if-statement = "if" expression "then" statement
        [ "else" statement ]  .

  This must make a modification in the normal checking for undefined
  variable references.  This is because in a loop, it is possible for the
  variables to be initialized by another branch of the if on an earlier
  pass through the loop.  Thus if we are in a looping construct the
  checking is disabled

}

    var
      oldcheck: boolean; {old value of checkundefs}


    begin {ifstatement}
      getexprstmt(begif);
      expression(follow + [elsesym, thensym, dosym], false);
      checkboolean;
      genoprndstmt;
      oldcheck := checkundefs;
      checkundefs := (loopfactor = 0);
      verifytoken(thensym, nothenerr);
      nest := nest + 1;
      statement(follow + [elsesym]);
      genstmt(endthen);
      if token = elsesym then
        begin
        updatelabelnest;
        gettoken;
        genstmt(begelse);
        statement(follow);
        genstmt(endelse);
        end;
      updatelabelnest;
      nest := nest - 1;
      checkundefs := oldcheck;
    end {ifstatement} ;


  procedure casestatement;

{ Syntactic routine to parse a case statement.

  Productions:

  case-statement = "case" expression "of"
        case-list-element [* ";" case-list-element *] [ ";" ]
        [ ( "otherwise" | "else" ) statement [ ";" ] ] "end"  .

  A list of used case labels is kept to allow checking for duplicate
  labels.

  A problem similar to that for "if" exists for undefined variable checks.
}

    type
      caselabptr = ^caselabentry; {used to keep track of case labels}
      caselabentry =
        record
          next: caselabptr;
          value1: integer
        end;

    var
      oldcheck: boolean; {old value of checkundef}
      latestlabel: caselabptr; {start of case label list}
      p1: caselabptr; {used when deleting case label list}
      casetype: index; {type of case expression}


    procedure caselabel;

{ Read a single case label, check it against previously read case labels,
  then add it to the list of labels read.

  Output generated:

  case-label = "caselab(value1)"
}

      var
        p: caselabptr; {used to trace label list}
        p1: caselabptr; {used to generate new label entry}
        lab: operand; {constant label value}


      begin {caselabel}
        new(p1);
        with p1^ do
          begin
          next := latestlabel;
          constant(follow + begconstset + [comma, colon, elsesym, otherwisesym],
                   true, lab);
          value1 := lab.cvalue.intvalue;
          if not compatible(casetype, lab.typeindex) then
            warnbefore(badcaselabeltype);
          p := latestlabel;
          while (p <> nil) do
            begin
            if p^.value1 = lab.cvalue.intvalue then warnbefore(dupcaselabel);
            p := p^.next;
            end;
          genstmt(caselab);
          genint(lab.cvalue.intvalue);
          end;
        latestlabel := p1;
      end {caselabel} ;


    procedure onecase;

{ Syntactic routine to parse a case-element.

  Production:

  case-list-element = constant [* "," constant *] ":"
        statement  .
}


      begin {onecase}
        if token in begconstset then
          begin
          caselabel;
          while token in
                [comma, ident, plus, minus, nilsym, intconst..stringconst] do
            begin
            verifytoken(comma, nocommaerr);
            if token in begconstset then caselabel
            else warn(caselabelerr)
            end;
          verifytoken(colon, nocolonerr);
          statement(follow + [semicolon, intconst, realconst, dblrealconst,
                    charconst, ident, endsym, elsesym, otherwisesym]);
          updatelabelnest;
          genstmt(endcaseelt);
          end;
      end {onecase} ;


    begin {casestatement}
      getexprstmt(begcase);
      expression(follow + [ofsym], false);
      genoprndstmt;
      if not (resultform in [ints, chars, bools, scalars, subranges, none]) then
        warnbefore(badcasetype);
      verifytoken(ofsym, nooferr);
      casetype := resulttype;
      latestlabel := nil;
      oldcheck := checkundefs;
      checkundefs := (loopfactor = 0);
      nest := nest + 1;
      onecase;
      while token in
            [semicolon, ident, nilsym, plus, minus, intconst..stringconst] do
        begin
        verifytoken(semicolon, nosemierr);
        onecase
        end;
      if token in [elsesym, otherwisesym] then
        begin
        warnnonstandard(caseelseerr);
        gettoken;
        if token = colon then
          begin
          warn(caselabelerr);
          gettoken;
          end;
        genstmt(casedef);
        statement(follow + [semicolon, endsym]);
        updatelabelnest;
        if token = semicolon then gettoken
        end;
      nest := nest - 1;
      while latestlabel <> nil do
        begin
        p1 := latestlabel^.next;
        dispose(latestlabel);
        latestlabel := p1;
        end;
      verifytoken(endsym, noenderr);
      genstmt(endcase);
      checkundefs := oldcheck;
    end {casestatement} ;


  procedure whilestatement;

{ Syntactic routine to parse a while statement.

  Production:

  while-statement = "while" expression "do" statement  .
  loopfactor is used to determine if checking for undefinded
  vars should happen.

}

    var
      oldcheck: boolean; {old value of checkundefs}


    begin {whilestatement}
      loopfactor := loopfactor + 1;
      getexprstmt(begwhile);
      expression(follow + [dosym], false);
      checkboolean;
      genoprndstmt;
      verifytoken(dosym, nodoerr);
      nest := nest + 1;
      oldcheck := checkundefs;
      checkundefs := false;
      statement(follow);
      checkundefs := oldcheck;
      updatelabelnest;
      nest := nest - 1;
      genstmt(endwhile);
      loopfactor := loopfactor - 1;
    end {whilestatement} ;


  procedure repeatstatement;

{ Syntactic routine to parse a repeat statement.

  Production:

  repeat-statement = "repeat" statement [* ";" statement *]
        "until" expression  .

  Loopfactor is manipulated in a manner similar to while.
}


    begin {repeatstatement}
      loopfactor := loopfactor + 1;
      gettoken;
      debugstmt(begrpt, lasttoken.line, lasttoken.filepos, lasttoken.fileindex);
      nest := nest + 1;
      statement(follow + [untilsym, semicolon]);
      while token in [semicolon, beginsym..gotosym, ident] do
        begin
        verifytoken(semicolon, nosemierr);
        statement(follow + [untilsym, semicolon])
        end;
      verifytoken(untilsym, nountilerr);
      genstmt(endrpt);
      intstate := opstate;
      expression(follow, false);
      checkboolean;
      genoprndstmt;
      updatelabelnest;
      nest := nest - 1;
      loopfactor := loopfactor - 1;
    end {repeatstatement} ;


  procedure forstatement;

{ Syntactic routine to parse a for statement:

  Production:

  for-statement = "for" variable ":=" expression
        ( "to" | "downto" ) final-value "do" statement  .

  The controlled variable must be an entire variable local to the
  current block.  Within the for statement, the controlled variable must
  not be used in any context where it can be modified.  Also, the
  for variable has an undefined state if the for loop exits normally.

  Since it is also highly desirable to keep the controlled variable
  in a register for the duration of the loop, controlled variables
  are handled in a special manner.

  The controlled variable is placed in a stack, and as long as it
  is on that stack it will be handled specially.  Also, for limits
  are passed to travrs differently if they are constant than if
  they are expressions.
}

    var
      oldcheck: boolean; {old value of checkundefs}
      oldjumpoutnest: integer; {old value of jumpoutnest}
      upflag: boolean; {true if this if "for" "to"}
      forlen: addressrange; {length of controlled var}
      localflag: boolean; {true if var unused by interior procs}
      t: forstackindex; {used for searching for stack}
      forvar: index; {index of controlled var}
      forvarptr: entryptr; {provides access to cont. var entry}
      fortype: index; {type of for var}
      fortypeptr: entryptr; {for access to fortype data}
      initconst, initout: boolean; {initial value constant, out of range}
      initlcheck, inithcheck: boolean; {set if low or high needs checking}
      initcol: columnindex; {column of initial expression}
      initline: integer; {text line of initial expression}
      initval: integer; {initial constant value}
      usinitval: unsignedint; {unsigned version of initial constant value}
      initrange: range; {range of initial value}
      finallen: integer; {length of final value}
      finalconst: boolean; {final value is constant}
      finalval: integer; {final constant value}
      usfinalval: unsignedint; {unsigned version of final constant value}
      finalout: boolean; {final value out of range}
      finlcheck, finhcheck: boolean; {set if low or high needs checking}
      finalrange: range; {range of final value}
      lowerbound, upperbound: integer; {for type range}
      extendedfor: boolean; {an extended range for statement}
      unsignedfor: boolean; {an unsigned for statement}


    begin {forstatement}
      loopfactor := loopfactor + 1;
      getexprstmt(begfor);
      forvar := 0;
      fortype := noneindex;
      lowerbound := 0;
      upperbound := 0;
      unsignedfor := false;
      extendedfor := false;
      if token = ident then
        begin
        search(forvar);
        if forvar = 0 then warn(undefidenterr)
        else
          begin
          if checkforstack(forvar, t) then warn(modifiedfor);
          if bigcompilerversion then forvarptr := @(bigtable[forvar]);
          with forvarptr^ do
            if namekind in [varname, param, varparam] then
              begin
              if bigcompilerversion then fortypeptr := @(bigtable[vartype]);

              { We don't support for loop indexes that are origined, declared
                USE, DEFINE or SHARED, or OWN.  OWN is allowed if the global
                section is not split.
              }
              if ((varalloc = ownalloc) and (globalsize > globalfiles)) or
                 (varalloc in
                 [absolute, usealloc, definealloc, sharedalloc]) then
                warn(unsupportedforvardecl);

              if not (fortypeptr^.typ in
                 [none, ints, chars, scalars, bools, subranges]) then
                warn(badfortype)
              else
                begin
                fortype := vartype;
                lowerbound := lower(fortypeptr);
                upperbound := upper(fortypeptr);
                extendedfor := fortypeptr^.extendedrange;
                unsignedfor := unsigned(fortypeptr, length, false);
                end;
              if (namekind <> varname) or (lev <> level) then warn(badforvar)
              else if nestedmod then warnnonstandard(badfornestref);
              forlen := length;
              localflag := registercandidate and ((level > 1) or
                           not anyexternals);
              end
            else warn(wantvarname);
          end;
        variable(true, true, false, true, true, forvar);
        end
      else warnbetween(missingforindex);
      if token = eql then
        begin
        warn(nobecomeserr);
        gettoken
        end
      else
        begin
        verify([becomes], follow + begexprset, nobecomeserr);
        if token = becomes then gettoken;
        end;

      expression(follow + [downtosym, tosym, untilsym, dosym], false);
      if not compatible(fortype, resulttype) then warnbefore(badforlimit);

      oprndstk[sp].typeindex := fortype;
      initconst := constcheck(sp);
      if initconst then initval := getintvalue(sp);
      checkrange(oprndstk[sp], false, initout, initlcheck, inithcheck);
      initrange := oprndstk[sp].value_range.optimistic;
      if initout then
        with lasttoken do
          begin
          initcol := (left + right) div 2;
          initline := line;
          end;

      if initconst then
        begin
        genlit(initval);
        sp := sp - 1;
        if unsignedfor then genop(defunsforlitindexop)
        else genop(defforlitindexop)
        end
      else
        begin
        genoprnd;
        if unsignedfor then genop(defunsforindexop)
        else genop(defforindexop)
        end;
      genint(forlen);
      genint(ord(localflag));
      upflag := (token = tosym);
      if token in [downtosym, tosym] then gettoken
      else warnbetween(nodowntoerr);
      expression(follow + [dosym], false);
      if not compatible(fortype, resulttype) then warnbefore(badforlimit);
      finalconst := constcheck(sp);
      oprndstk[sp].typeindex := fortype;
      oprndstk[sp].oprndlen := max(forlen, oprndstk[sp].oprndlen);
      if finalconst then
        begin
        finallen := forlen;
        finalval := getintvalue(sp)
        end
      else
        begin
        finallen := oprndstk[sp].oprndlen;
        genunary(pushfinal, ints);
        end;

      finalrange := oprndstk[sp].value_range.optimistic;
      oprndstk[sp].typeindex := fortype;
      checkrange(oprndstk[sp], false, finalout, finlcheck, finhcheck);

      genoprnd;
      if finalconst and initconst then
        begin
        usinitval := initval;
        usfinalval := finalval;
        if upflag and (extendedfor and (usinitval <= usfinalval) or
           not extendedfor and (initval <= finalval)) or not upflag and
           (extendedfor and (usinitval >= usfinalval) or not extendedfor and
           (initval >= finalval)) then
          begin
          if initout then warnat(rangeerror, initline, initcol);
          if finalout then warnbefore(rangeerror);
          end;
        end
      else if (switchcounters[rangecheck] > 0) and (upflag and (initlcheck or
              finhcheck) or not upflag and (inithcheck or finlcheck)) then
        begin
        if initout or finalout then
          begin
          genlit(ord(not upflag));
          genop(forerrchkop);
          end
        else
          begin
          genlit(lowerbound);
          genlit(upperbound);
          if upflag then genop(forupchkop)
          else genop(fordnchkop);
          end;
        genint(finallen);
        genint(1);
        genform(ints);
        end;

      genop(endexpr);
      intstate := stmtstate;
      if upflag then genstmt(forup)
      else genstmt(fordn);
      genint(1);
      verifytoken(dosym, nodoerr);
      forsp := forsp + 1;
      with forstack[forsp] do
        begin
        containedgoto := false; {hopefully remains false!}
        forindex := forvar;
        if bigcompilerversion then fortypeptr := @(bigtable[fortype]);
        settyperange(fortypeptr, forrange);
        if upflag then
          begin
          if finalrange.maxlimit >= initrange.minlimit then
            begin
            if not finhcheck then forrange.maxlimit := finalrange.maxlimit;
            if not initlcheck then forrange.minlimit := initrange.minlimit;
            end
          end
        else
          begin
          if finalrange.minlimit <= initrange.maxlimit then
            begin
            if not finlcheck then forrange.minlimit := finalrange.minlimit;
            if not inithcheck then forrange.maxlimit := initrange.maxlimit;
            end
          end;
        end;
      if bigcompilerversion then forvarptr := @(bigtable[forvar]);
      with forvarptr^ do
        if namekind in [varname, fieldname, param, varparam] then
          modified := true;
      oldcheck := checkundefs;
      checkundefs := false;
      nest := nest + 1;
      oldjumpoutnest := jumpoutnest;
      jumpoutnest := nest;
      statement(follow);
      checkundefs := oldcheck;
      updatelabelnest;
      genstmt(endfor);
      genint(ord(jumpoutnest < nest));
      jumpoutnest := min(jumpoutnest, oldjumpoutnest);
      nest := nest - 1;
      loopfactor := loopfactor - 1;
      if not (forstack[forsp].containedgoto and
         (switchcounters[standard] > 0)) and (nest = 1) then
        begin
        if bigcompilerversion then forvarptr := @(bigtable[forvar]);
        if forvarptr^.namekind = varname then forvarptr^.modified := false;
        end;
      forsp := forsp - 1;
    end {forstatement} ;


  procedure withstatement;

{ Syntactic routine to parse a with-statement.

  Production:

  with-statement = "with" variable [* ";" variable *] "do"
        statement  .

  As each variable is parsed, the scope id is pushed onto the display
  to let its fields be found in the normal search sequence.  These are
  popped at the end of the statement.  Also, the address of the record
  is written to the intermediate file.
}

    var
      withcount: integer; {number of variables}
      i: integer; {induction variable}


    procedure onewith;

{ Parse a single variable in a with statement.  This actually pushes the
  display and generates the output.
}

      var
        p: entryptr; {access with variable}
        off: addressrange; {offset of with variable}
        i: index; {index of with variable}
        l: levelindex; {level of with variable}


      begin {onewith}
        newexprstmt(begwith);
        if token = ident then
          begin
          search(i);
          if bigcompilerversion then p := @(bigtable[i]);
          if p^.namekind in [varname, fieldname, param, varparam] then
            off := p^.offset
          else off := 0;
          l := lev;
          modifyvariable(true, true);
          genlit(0);
          genunary(indxop, ints);
          if resultform = fields then
            if displaytop < maxlevel then
              begin
              displaytop := displaytop + 1;
              with display[displaytop] do
                begin
                scopeid := display[displaytop - 1].scopeid;
                blockid := resultptr^.fieldid;
                blockkind := withblock;
                withpacking := resultptr^.packedflag;
                withoffset := off;
                withlevel := l;
                end;
              withcount := withcount + 1;
              genoprndstmt;
              end
            else warnbefore(levelerr)
          else if resulttype <> noneindex then warnbefore(norecordident)
          end
        else warnbetween(norecordident);
      end {onewith} ;


    begin {withstatement}
      gettoken;
      withcount := 0;
      onewith;
      while token in [comma, ident] do
        begin
        verifytoken(comma, nocommaerr);
        onewith;
        end;
      verifytoken(dosym, nodoerr);
      statement(follow);
      displaytop := displaytop - withcount;
      for i := withcount downto 1 do genstmt(endwith);
    end {withstatement} ;


  procedure gotostatement;

{ Syntactic routine to parse a goto statement.

  production:

  goto-statement = "goto" label  .

  This also has to check if the goto is legal, though if the
  label is not yet defined this may have to wait for the label to
  be defined.

}

    var
      lab: labelptr; {label entry}
      gotoline: integer; {line on which goto appeared}
      gotofilepos: integer; {file position of goto token}
      gotofileindex: integer; {fileindex of goto token}


    begin {gotostatement}
      forstack[forsp].containedgoto := true;
      gotoline := thistoken.line;
      gotofilepos := thistoken.filepos;
      gotofileindex := thistoken.fileindex;
      gettoken;
      if token = intconst then
        begin
        searchlabels(thistoken.intvalue, lab);
        with lab^ do
          begin
          if lab = labelflag then warn(labnotpredef)
          else
            begin
            if definednest = 0 then maxlegalnest := min(nest, maxlegalnest)
            else if (nest > maxlegalnest) or (nest < definednest) then
              warnat(badlabelnest, labelline, labelcolumn)
            else if (nest > definednest) and (lev = level) then
              jumpoutnest := min(jumpoutnest, definednest);
            if lev <> level then
              begin
              nonlocalref := true;
              maxlegalnest := 1;
              end;
            end;
          if (lev > 1) and (lev <> level) then
            proctable[display[level].blockref].intlevelrefs := true;
          debugstmt(gotolab, gotoline, gotofilepos, gotofileindex);
          genint(internalvalue);
          genint(lev);
          end;
        gettoken
        end
      else warnbetween(badlabelerr);
    end {gotostatement} ;


  begin {statement}
    sp := - 1;
    if token = intconst then assignlabel;
    if token in [intconst, charconst, realconst, dblrealconst, stringconst] then
      illegalassign
    else if token = ident then
      begin
      search(varindex);
      if varindex = 0 then illegalassign
      else
        begin
        if bigcompilerversion then varptr := @(bigtable[varindex]);
        case varptr^.namekind of
          standardproc: standardprocedures(varptr^.procid);
          varname, fieldname, param, varparam, confparam, varconfparam:
            begin
            newexprstmt(simple);
            assignment;
            end;
          forwardproc, externalproc, procname:
            begin
            newexprstmt(simple);
            procedurecall(varindex);
            genoprndstmt
            end;
          procparam:
            begin
            newexprstmt(simple);
            paramcall(varindex);
            genoprndstmt
            end;
          funcname:
            begin
            if not bigcompilerversion then blocksin[1].written := true;
            varptr^.funcassigned := true;
            newexprstmt(simple);
            if (lev >= level) or (varindex <> display[lev + 1].blockname) then
              begin
              warn(badfuncassign);
              illegalassign
              end
            else assignment;
            end;
          otherwise illegalassign
          end;
        end;
      end
    else
      case token of
        beginsym: compoundstatement;
        ifsym: ifstatement;
        casesym: casestatement;
        whilesym: whilestatement;
        repeatsym: repeatstatement;
        forsym: forstatement;
        withsym: withstatement;
        gotosym: gotostatement;
        elsesym:
          if not (elsesym in follow) then badelseclause;
        otherwisesym:
          if not (otherwisesym in follow) then badelseclause;
        otherwise;
        end;
  end {statement} ;


procedure body;

{ Syntactic routine to parse a block.

  Productions:

  body = "begin" statement [* ";" statement *] "end"

  This is broken out as an external procedure to allow overlaying
  code with the declaration processing code which is contained
  in "analys".

  It is called using "callo"
}


  begin {body}
    if switcheverplus[defineswitch] then warn(bodyfounderr);
    nest := 1;
    jumpoutnest := maxint;
    nolabelsofar := true;

    if token in [ifsym..gotosym, ident] then warn(nobeginerr)
    else verifytoken(beginsym, nobeginerr);
    statement(neverskipset);
    while not (token in [labelsym..functionsym, endsym, eofsym]) do
      begin
      if token = semicolon then gettoken
      else verify1([semicolon], nosemierr);
      statement(neverskipset);
      end;

  end {body} ;

end.
