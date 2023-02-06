{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045  Level 1
  Processor: All
  System: All

  Pascal-2 Compiler Constant Folding Routine Declarations

 Last modified by KRIS on 21-Nov-1990 15:21:06
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}


procedure add(left, right: integer; {left and right operands}
              var result: integer; {result}
              var overflow: boolean {set if overflow occurred} );
  external;

{ Add two target integers.  If the operation overflows, "overflow" will
  be set, and "result" will be set to the max value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}


procedure usadd(left, right: integer; {left and right operands}
                var result: integer; {result}
                var overflow: boolean {set if overflow occurred} );
  external;

{ Unsigned add of two target integers.  If the operation overflows,
  "overflow"  is set, and "result" will be set to the max value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}


procedure negate(operand: integer; {operand to negate}
                 var result: integer; {result}
                 var overflow: boolean {set if overflow results} );
  external;

{ Negate a target integer.  If it overflows, the result will be set to
  the limit at the appropriate sign.
  ****Self hosted version
}


procedure subtract(left, right: integer; {operands}
                   var result: integer; {result}
                   var overflow: boolean {set if overflow results} );
  external;

{ Subtract two target integers.  If the operation overflows, "overflow" will
  be set, and "result" will be set to the max value possible.
  ****Self hosted version
 }


procedure ussubtract(left, right: integer; {operands}
                     var result: integer; {result}
                     var overflow: boolean {set if overflow results} );
  external;

{ Unsigned subtract.  The result is required to be unsigned as well
  or overflow will be set.
  ****Self hosted version
}


procedure multiply(left, right: integer; {operands}
                   var result: integer; {result}
                   var overflow: boolean {set if overflow results} );
  external;

{ Multiply two target integers.  If the operation overflows, "overflow"
  will be set and "result" will be set to the max signed value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}


procedure usmultiply(left, right: integer; {operands}
                     var result: integer; {result}
                     var overflow: boolean {set if overflow results} );
  external;

{ Unsigned equivalent of the multiply routine
  ****self hosted version
}


procedure divide(left, right: integer; {operands}
                 var result: integer; {result}
                 var overflow: boolean {set if result overflows} );
  external;

{ Divide two target integers.  If the operation overflows, "overflow"
  will be set and "result" will be set to the max signed value possible.
  ****Self hosted version
  This will have to be changed if the target integers are not the same
  size as the host integers;
}


procedure usdivide(left, right: integer; {operands}
                   var result: integer; {result}
                   var overflow: boolean {set if result overflows} );
  external;

{ Unsigned version of divide.
  *****16 bit 68K on a PDP11.  The PDP11 doesn't really have an unsigned
  divide instruction, so we have to fake it.
}


procedure remainder(left, right: integer; {operands}
                    var result: integer; {result}
                    var overflow: boolean {set if result overflows} );
  external;

{ Take the mod for target integers;  If "right" is zero, the result
  will be zero, and "overflow" will be set.
  ****self hosted version
}


procedure usremainder(left, right: integer; {operands}
                      var result: integer; {result}
                      var overflow: boolean {set if result overflows} );
  external;

{ Unsigned version of remainder
  ****self hosted version
}


procedure comparereal(left, right: real;
                      var result: boolean;
                      op: operator);

  external;

{ Fold the comparison of a pair of real operands. }


procedure uscompareint(left, right: unsignedint;
                       var result: boolean;
                       op: operator);
  external;

{ Fold the compare of a pair of unsigned integer operands. }


procedure compareint(left, right: integer;
                     var result: boolean;
                     op: operator);
  external;
