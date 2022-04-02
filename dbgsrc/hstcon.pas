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

  Pascal-2 debugger host machine dependent parameters.

 Last modified by KRIS on 26-Nov-1990 13:51:38
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

const

  hostmachine = mc68000;

  fastread = false; {use p_rdsfst to speed profile list generation (VMS only)}

  {}
  reversebytes = false; {true if host/transfer byte-order is backwards}
  hostintlowbytefirst = false; {true if host integers have low order byte at
                               lowest address}
  packinghightolow = true; {true if the first field to be packed goes in the
                             high order of the integer containing it}
  originright = false; { bit zero is at left of word }

  {}
  maxaddr = 16#FFFFFFFF; {Max value of an address on the vax}
  shortmaxint = 32767; {max value for 16 bit integer on any machine}
  maxusint = 4294967295; {Max value of an address on the vax}
  maxusword = 65535; {Max value for a 16-bit value}
  maxusbyte = 255; {Max value for an 8-bit value}
  niladdressvalue = 0; {numeric value of NIL}

  singlerealsize = 4; {size of a single precision real}
  doublerealsize = 8; {size of a double precision real}
  maxrealwords = 4; {maximum number of words per real}

  dfltintsize = 4; {units per integer}
  addresssize = 4; {units per address}
  wordsize = 2; {units per word}
  setsize = 32; {units per set}
  filesize = 4; {units per file ptr}
  ptrsize = 4; {units per pointer}
  boolsize = 1; {units per boolean}
  charsize = 1; {units per character}

  transferunitsize = 4; {units per transfer unit}
  datapacketlength = 8; {number of transfer units to a data packet}
  datatransferlength = 9; {number of transfer units to a transfer array}

  bitsperunit = 8;
  bitsperword = 16;
  bitsperint = 32;
  bitsperaddressableunit = 16; {# of bits to the lowest granularity
                               addressable}
  addressableunitsize = 2; {# of units to the lowest granularity addressable }

  bitsperaddress = 32;
  bitspertransferunit = 32;

  mdigits = 32;
  longzero = 0;
  longunit = 1;

  { trapOp is now a variable since it may vary from system to system }
  trapopsize = 16;		{ # of bits in TRAP opcode }
  tracebitmask = 16#FF3FFFFF;   {used to turn of the trace bit and trace pending
                                 bit}


type

  transferunit = integer; {the type used to transfer data between the debugger
                           and the image being debugged}

  sysint = integer;
  intptr = ^integer;

  unsignedint = 0..maxusint; {range for unsigned values}
  unsignedword = 0..maxusword; {range for unsigned words}
  unsignedbyte = 0..maxusbyte; {range for unsigned bytes}
  addressrange = 0..maxaddr; {supported addressing space (bytes)}

  singlerealkludge = integer; {for manipulating single precision reals in a
                               double precision environment}
  extendedrealarray = packed array [1..3] of integer;

%include trgcon; {target dependent parameters}
%include opscon; { operating system dependent parameters }

type

  { Transfer record for scalar atomics }

  scalarkludgerecord =
    record
      case 0..2 of
        0: (i: integer);
        1: (fillb: packed array [1..3] of char;
            b: boolean);
        2: (fillc: packed array [1..3] of char;
            c: char);
    end;

  mask = packed array [0..bitsperint] of unsignedint;


  addressrec = { used to store addresses } 
    packed record 
      case 0..3 of 
        0: 
          (addr: addressrange); 
        1: 
          (offset, segment: unsignedword); { for segmented architecures } 
        2: 
          (ptr: ^integer); 
        3: 		{ r0 thru r3 are there to allign regoff for 68000 }
          (r0: unsignedword; r1,r2,r3,regoff: regindex); 
    end; 

  stuffedregisters = record
    fp5, fp6, fp7: extendedrealarray;
    d5, d6, d7, a3, a4: addressrange;
    end;      

  savedregisters = record
    r: stuffedregisters;
    fp, sp: addressrange;
    pc: addressrec;
    end;

const

  {  The following structured constants are used to shift and mask data
     pieces. }


  BitMask = mask (
    16#00000001, 16#00000002, 16#00000004, 16#00000008,
    16#00000010, 16#00000020, 16#00000040, 16#00000080,
    16#00000100, 16#00000200, 16#00000400, 16#00000800,
    16#00001000, 16#00002000, 16#00004000, 16#00008000,

        16#00010000, 16#00020000, 16#00040000, 16#00080000,
        16#00100000, 16#00200000, 16#00400000, 16#00800000,
        16#01000000, 16#02000000, 16#04000000, 16#08000000,
        16#10000000, 16#20000000, 16#40000000, 16#80000000, 0);


  FieldMask = mask (0,
    16#00000001, 16#00000003, 16#00000007, 16#0000000f,
    16#0000001f, 16#0000003f, 16#0000007f, 16#000000FF,
    16#000001FF, 16#000003FF, 16#000007FF, 16#00000FFF,
    16#00001FFF, 16#00003FFF, 16#00007FFF, 16#0000FFFF,

        16#0001FFFF, 16#0003FFFF, 16#0007FFFF, 16#000FFFFF,
        16#001FFFFF, 16#003FFFFF, 16#007FFFFF, 16#00FFFFFF,
        16#01FFFFFF, 16#03FFFFFF, 16#07FFFFFF, 16#0FFFFFFF,
        16#1FFFFFFF, 16#3FFFFFFF, 16#7FFFFFFF, 16#FFFFFFFF);

