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
  Processor: ~processor~
  System: ~system~

  Machine independent floating point package.

 Last modified by KRIS on 21-Nov-1990 15:23:32
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

{ 

  The Compiler internal IEEE extended precision floating point calculations
  package.

  The Compiler converts all floating point constants to IEEE extended
  (80-bit) format using type "realarray".  This type consists of 5 16-bit
  unsigned integers, in most-to-least significant order.  That is, the
  80-bit value is formed by concatenating r[1], r[2], ..., r[5] in that
  order.

  When a value needs to be converted to IEEE single or double precision,
  it is also kept in a variable of type "realarray".  The same arrangement
  holds.  A single-precision number is formed by concatenating r[1],r[2].
  A double-precision number is formed by concatenating r[1], ..., r[4]
  after conversion.

  There is no way to know whether a value of type realarray contains
  an Extended, Double, or Single precision value -- this must be kept
  track of in some other way.
}

{DRB this was used by the C/C++ version of the compiler, which has been
 lost to time ... kept here as a historical reference.
 
 The external references are never called by existing code, folding was
 done in the C/C++ front end.}

unit fpcalc;

interface

uses config, hdr, utils, error;

procedure fpadd(x, y: realarray;
                var z: realarray;
                var err: boolean);
{ Compute:   z := x + y;
  If any error (overflow, NaN, etc), 'err' is set and z is not valid.
}
  external;


procedure fpsub(x, y: realarray;
                var z: realarray;
                var err: boolean);
{ Compute:   z := x - y;
  Invert the sign of y and add.
  If any error (overflow, NaN, etc), 'err' is set and z is not valid.
}
  external;


procedure fpmul(x, y: realarray;
                var z: realarray;
                var err: boolean);
{ Compute:   z := x * y;
  If any error (overflow, NaN, etc), 'err' is set and z is not valid.
}
  external;


procedure fpdiv(x, y: realarray;
                var z: realarray;
                var err: boolean);
{ Compute:   z := x / y;
  If any error (overflow, NaN, etc), 'err' is set and z is not valid.
}
  external;



procedure fpnegate(x: realarray;
                   var y: realarray);
{ Compute:   y := -x;
}
  external;


procedure doubletofp(d: realarray;
                     var x: realarray);

{ Convert IEEE double occupying first 8 bytes of d to extended in x
}
  external;


procedure singletofp(s: realarray;
                     var x: realarray);

{ Convert IEEE single occupying first 4 bytes of d to extended in x
}
  external;


procedure inttofp(i: integer;
                  isunsigned: boolean;
                  var x: realarray);
{ Convert possibly unsigned integer i to floating point.
}
  external;


procedure fptodouble(x: realarray;
                     var d: realarray;
                     var err: boolean;
                     var inexact: boolean);
{ Convert floating point x to IEEE double format.
  If overflow or other error occurs, set err, and d is not valid.
  If no error, 'inexact' means precision was lost.
}
  external;


procedure fptosingle(x: realarray;
                     var s: realarray;
                     var err: boolean;
                     var inexact: boolean);
{ Convert floating point x to IEEE single format.
  If overflow or other error occurs, set err, and s is not valid.
  If no error, 'inexact' means precision was lost.
}
  external;


procedure fptodecdouble(x: realarray;
                     var d: realarray;
                     var err: boolean;
                     var inexact: boolean);
{ Convert floating point x to DEC double format.
  If overflow or other error occurs, set err, and d is not valid.
  If no error, 'inexact' means precision was lost.
}
  external;


procedure fptodecsingle(x: realarray;
                     var s: realarray;
                     var err: boolean;
                     var inexact: boolean);
{ Convert floating point x to DEC single format.
  If overflow or other error occurs, set err, and s is not valid.
  If no error, 'inexact' means precision was lost.
}
  external;


procedure fptoint(x: realarray;
                  var i: integer;
                  tounsigned: boolean;
                  var err: boolean;
                  var inexact: boolean);
{ Convert floating point x to an integer i, possibly unsigned.
  If overflow or other error occurs, set err, and i is not valid.
  If no error, 'inexact' means precision was lost.
}
  external;


procedure fpinfinity(var x: realarray);

{ Return a positive infinith
}
  external;

function fpcomp(x, y: realarray): integer;
{ Compare x to y, return +1, 0, -1 for greater, equal, less.
  We assume that neither x nor y are infinities or NaN's.
}

implementation


function fpcomp(x, y: realarray): integer;
{ Compare x to y, return +1, 0, -1 for greater, equal, less.
  We assume that neither x nor y are infinities or NaN's.
}

  begin
  compilerabort(inconsistent);
  fpcomp := 0;
  end;

end.
