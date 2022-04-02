{[l-,b+]}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1984 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045  Level: 1
  Processor: m68000
  System: all

  Pascal-2 debugger target dependent parameters.

 Last modified by KRIS on 21-Nov-1990 17:20:37
 Purpose:
Update release version for PU-VV0-GS0 at 2.3.0.1

}




const

  targetmachine = mc68000;

  {}

  targetmaxint = 2147483647; {max value of integer on vax}
  targetmaxaddr = 16#FFFFFFFF; {max value of address on vax}
  targetaddresssize = 4; {# of units in address}
  targetregistersize = 4; {# of units in a register}
  targetfpregistersize = 12; {# of units in a floating point register}

  defaultaddressradix = 16; {default radix for writing address values}

  regmax = 15; {max value of register set}

  fpc_available = true;  {yes, we use mc68881}

  { Constants used to refer to return values from p_prctyp. }
  mc68_error      = 0;
  mc68000_mc68010 = 1;
  mc68020_only    = 2;
  mc68020_mc68881 = 3;

type

  regindex = 0..regmax;

  registerarray = array [0..regmax] of integer;

const

  maxgenregistermask = 7; { max number of genearal registers }
  maxptrregistermask = 7; { max number of pointer registers }
  maxrealregistermask = 7; { max number of real registers }

  {mapping from compiler generated register offset to target general register}

  genregindexes = registerarray(0, 7, 6, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0);

  {mapping from compiler generated register offset to target general register}

  ptrregindexes = registerarray(0, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                0);

  {mapping from compiler generated register offset to target real register}

  realregindexes = registerarray(0, 7, 6, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                0);

type

  targetint = - targetmaxint..targetmaxint; {range on integer on vax}
  targetaddressrange = 0..targetmaxaddr; {range of addresses on vax}
  targetaddress = targetaddressrange; {range of addresses on vax}
