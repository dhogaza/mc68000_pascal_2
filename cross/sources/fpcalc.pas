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

  External declarations for Compiler internal IEEE extended precision
  floating point calculations package.

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

function fpcomp{x, y: realarray}{: integer};
{ Compare x to y, return +1, 0, -1 for greater, equal, less.
  We assume that neither x nor y are infinities or NaN's.
}
  begin
  abort(inconsistent);
  fpcomp := 0;
  end;
