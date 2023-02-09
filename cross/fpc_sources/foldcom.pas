{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Constant Folding Routines

 Last modified by KRIS on 21-Nov-1990 15:21:00
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

unit foldcom;

interface

uses config, hdr, a_t;

type

  binaryfoldop = procedure (left, right: integer; {left and right operands}
                              var result: integer; {result}
                              var overflow: boolean {set if overflow occurred} );

  unaryfoldop = procedure (operand: integer; {operand to negate}
                                var result: integer; {result}
                                var overflow: boolean {set if overflow results} );

procedure add(left, right: integer; {left and right operands}
              var result: integer; {result}
              var overflow: boolean {set if overflow occurred} );

procedure usadd(left, right: integer; {left and right operands}
                var result: integer; {result}
                var overflow: boolean {set if overflow occurred} );

{ Unsigned add of two target integers.  If the operation overflows,
  "overflow"  is set, and "result" will be set to the max value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}


procedure negate(operand: integer; {operand to negate}
                 var result: integer; {result}
                 var overflow: boolean {set if overflow results} );

{ Negate a target integer.  If it overflows, the result will be set to
  the limit at the appropriate sign.
  ****Self hosted version
}

procedure subtract(left, right: integer; {operands}
                   var result: integer; {result}
                   var overflow: boolean {set if overflow results} );

{ Subtract two target integers.  If the operation overflows, "overflow" will
  be set, and "result" will be set to the max value possible.
  ****Self hosted version
 }


procedure ussubtract(left, right: integer; {operands}
                     var result: integer; {result}
                     var overflow: boolean {set if overflow results} );

{ Unsigned subtract.  The result is required to be unsigned as well
  or overflow will be set.
  ****Self hosted version
}


procedure multiply(left, right: integer; {operands}
                   var result: integer; {result}
                   var overflow: boolean {set if overflow results} );

{ Multiply two target integers.  If the operation overflows, "overflow"
  will be set and "result" will be set to the max signed value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}


procedure usmultiply(left, right: integer; {operands}
                     var result: integer; {result}
                     var overflow: boolean {set if overflow results} );

{ Unsigned equivalent of the multiply routine
  ****self hosted version
}


procedure divide(left, right: integer; {operands}
                 var result: integer; {result}
                 var overflow: boolean {set if result overflows} );

{ Divide two target integers.  If the operation overflows, "overflow"
  will be set and "result" will be set to the max signed value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}

procedure usdivide(left, right: integer; {operands}
                   var result: integer; {result}
                   var overflow: boolean {set if result overflows} );

{ Unsigned version of divide.
  *****16 bit 68K on a PDP11.  The PDP11 doesn't really have an unsigned
  divide instruction, so we have to fake it.
}


procedure remainder(left, right: integer; {operands}
                    var result: integer; {result}
                    var overflow: boolean {set if result overflows} );

{ Take the mod for target integers;  If "right" is zero, the result
  will be zero, and "overflow" will be set.
  ****self hosted version
}


procedure usremainder(left, right: integer; {operands}
                      var result: integer; {result}
                      var overflow: boolean {set if result overflows} );
{ Unsigned version of remainder
  ****self hosted version
}


procedure comparereal(left, right: real;
                      var result: boolean;
                      op: operatortype);

{ Fold the comparison of a pair of real operands. }


procedure uscompareint(left, right: unsignedint;
                       var result: boolean;
                       op: operatortype);

{ Fold the compare of a pair of unsigned integer operands. }


procedure compareint(left, right: integer;
                     var result: boolean;
                     op: operatortype);

implementation

procedure add(left, right: integer; {left and right operands}
              var result: integer; {result}
              var overflow: boolean {set if overflow occurred} );

{ Add two target integers.  If the operation overflows, "overflow" will
  be set, and "result" will be set to the max value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}

  begin {add}
    overflow := ((left >= 0) = (right >= 0)) and ((left >= 0) and
                (targetmaxint - right < left) or (left < 0) and
                ( - targetmaxint - 1 - right > left));
    if overflow then
      if left >= 0 then result := targetmaxint
      else result := - targetmaxint - 1 {two's complement}
    else result := left + right;
  end {add} ;



procedure usadd(left, right: integer; {left and right operands}
                var result: integer; {result}
                var overflow: boolean {set if overflow occurred} );

{ Unsigned add of two target integers.  If the operation overflows,
  "overflow"  is set, and "result" will be set to the max value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}

  var
    usleft, usright: unsignedint;


  begin {usadd}
    usleft := left;
    usright := right;
    overflow := maxusint - usleft < usright;
    if overflow then result := maxusint
    else result := usleft + usright;
  end {usadd} ;


procedure negate(operand: integer; {operand to negate}
                 var result: integer; {result}
                 var overflow: boolean {set if overflow results} );

{ Negate a target integer.  If it overflows, the result will be set to
  the limit at the appropriate sign.
  ****Self hosted version
}


  begin {negate}
    overflow := operand = ( - targetmaxint - 1);
    if overflow then result := targetmaxint
    else result := - operand;
  end {negate} ;



procedure subtract(left, right: integer; {operands}
                     var result: integer; {result}
                     var overflow: boolean {set if overflow results} );

{ Subtract two target integers.  If the operation overflows, "overflow" will
  be set, and "result" will be set to the max value possible.
  ****Self hosted version
 }


  begin {subtract}
    overflow := ((left >= 0) <> (right >= 0)) and ((left >= 0) and
                (targetmaxint + right < left) or (left < 0) and
                ( - targetmaxint - 1 + right > left));
    if overflow then
      if left >= 0 then result := targetmaxint
      else result := - targetmaxint - 1 {two's complement}
    else result := left - right;
  end {subtract} ;


procedure ussubtract(left, right: integer; {operands}
                     var result: integer; {result}
                     var overflow: boolean {set if overflow results} );

{ Unsigned subtract.  The result is required to be unsigned as well
  or overflow will be set.
  ****Self hosted version
}

  var
    usleft, usright: unsignedint;


  begin {ussubtract}
    usleft := left;
    usright := right;
    overflow := usleft < usright;
    if overflow then result := 0
    else result := usleft - usright;
  end {ussubtract} ;



procedure multiply(left, right: integer; {operands}
                   var result: integer; {result}
                   var overflow: boolean {set if overflow results} );

{ Multiply two target integers.  If the operation overflows, "overflow"
  will be set and "result" will be set to the max signed value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}


  begin {multiply}
    if left = -1 then overflow := right = (- targetmaxint - 1)
    else if right = -1 then overflow := left = (- targetmaxint - 1)
    else if (left <> 0) and (right <> 0) then
      overflow := ((left > 0) = (right > 0)) and ((left > 0) and
                  (targetmaxint div left < right) or (left < 0) and
                  (targetmaxint div left > right)) or
                  ((left > 0) <> (right > 0)) and ((left > 0) and
                  (( - targetmaxint - 1) div left > right) or (left < 0) and
                  (( - targetmaxint - 1) div right > left))
    else overflow := false;
    if overflow then
      if (left > 0) = (right > 0) then result := maxint
      else result := - maxint - 1
    else result := left * right;
  end {multiply} ;


procedure usmultiply(left, right: integer; {operands}
                     var result: integer; {result}
                     var overflow: boolean {set if overflow results} );

{ Unsigned equivalent of the multiply routine
  ****self hosted version
}

  var
    usright, usleft: unsignedint;


  begin {usmultiply}
    usright := right;
    usleft := left;
    if (usright = 0) or (usleft = 0) then overflow := false
    else overflow := maxusint div usright < usleft;
    if overflow then result := maxusint
    else result := usleft * usright;
  end {usmultiply} ;

procedure divide(left, right: integer; {operands}
                 var result: integer; {result}
                 var overflow: boolean {set if result overflows} );

{ Divide two target integers.  If the operation overflows, "overflow"
  will be set and "result" will be set to the max signed value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}


  begin {divide}
    overflow := (right = 0) or (left = ( - targetmaxint - 1)) and
                (right = - 1);
    if overflow then
      if (left > 0) or (right = - 1) then result := targetmaxint
      else result := - targetmaxint - 1
    else result := left div right;
  end {divide} ;


procedure usdivide(left, right: integer; {operands}
                   var result: integer; {result}
                   var overflow: boolean {set if result overflows} );

{ Unsigned version of divide.
  *****16 bit 68K on a PDP11.  The PDP11 doesn't really have an unsigned
  divide instruction, so we have to fake it.
}

  var
    usleft, usright: unsignedint;


  begin {usdivide}
    usleft := left;
    usright := right;
    overflow := usright = 0;
    if overflow then result := maxusint
    else if usright > targetmaxint then
      if usright > usleft then result := 0
      else result := 1
    else result := usleft div usright;
  end {usdivide} ;

procedure remainder(left, right: integer; {operands}
                    var result: integer; {result}
                    var overflow: boolean {set if result overflows} );

{ Take the mod for target integers;  If "right" is zero, the result
  will be zero, and "overflow" will be set.
  ****self hosted version
}

  begin {remainder}
    overflow := right = 0;
    if overflow or (left = ( - targetmaxint - 1)) and ((right = - 1) or
       (right = ( - targetmaxint - 1))) then
      result := 0
    else if (right = ( - targetmaxint - 1)) then result := left
    else if left < 0 then
      if right < 0 then result := ( - left) mod ( - right)
      else result := - ( - left) mod right
    else if right < 0 then result := - (left mod ( - right))
    else result := left mod right;
    { Now that we have the arithmetic remainder, we can "correct"
      it to conform to the Pascal definition of "mod". }
    if true {we want the positive modulus} then
      if result < 0 then result := result + abs(right);
  end {remainder} ;



procedure usremainder(left, right: integer; {operands}
                      var result: integer; {result}
                      var overflow: boolean {set if result overflows} );
{ Unsigned version of remainder
  ****self hosted version
}

  var
    usright, usleft: unsignedint;


  begin {usremainder}
    usright := right;
    usleft := left;
    overflow := usright = 0;
    if overflow then result := 0
    else result := usleft mod usright;
  end {usremainder} ;

procedure comparereal(left, right: real;
                      var result: boolean;
                      op: operatortype);

{ Fold the comparison of a pair of real operands. }

  begin {comparereal}
   if realfolding then
    begin
    case op of
      lsslit: result := left < right;
      leqlit: result := left <= right;
      gtrlit: result := left > right;
      geqlit: result := left >= right;
      eqlit: result := left = right;
      neqlit: result := left <> right;
      end;
   end
  end {comparereal} ;

procedure uscompareint(left, right: unsignedint;
                       var result: boolean;
                       op: operatortype);

 { Fold the compare of a pair of unsigned integer operands. }


  begin
    { NOTE: it seems at first glance that we should take into
    account whether the other operand is signed and compensate
    However if this were done at runtime the code generator
    would generate an unsigned compare which would give different
    results than what we would do here. For consistency give the
    runtime result here. E.G.  -1 < x::0..maxusint-1 but a
    runtime this would be false since -1 = maxusint and we do
    a simple unsigned compare.
    }
    case op of
      lsslit: result := left < right;
      leqlit: result := left <= right;
      gtrlit: result := left > right;
      geqlit: result := left >= right;
      eqlit: result := left = right;
      neqlit: result := left <> right;
      end;
  end; {uscompareint}

procedure compareint(left, right: integer;
                     var result: boolean;
                     op: operatortype);

  begin
    case op of
      lsslit: result := left < right;
      leqlit: result := left <= right;
      gtrlit: result := left > right;
      geqlit: result := left >= right;
      eqlit: result := left = right;
      neqlit: result := left <> right;
      end;
  end; {compareint}

  end.
