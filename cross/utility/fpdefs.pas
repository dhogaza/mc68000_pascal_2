
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1985, 1986, 1987 Oregon Software, Inc.
  ALL RIGHTS RESERVED.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied, modified, transferred, and used only as
  provided under a signed license agreement with Oregon Software.
  Any support purchased from Oregon Software does not apply to
  user-modified programs.  All copies of this program must display
  this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: All
  System: All
  Flavor: All

  User definitions for 68881.

 Last modified by KRIS on 21-Nov-1990 17:21:33
 Purpose:
Update release version for PU-VV0-GS0 at 2.3.0.1

}


{$68881}

{ FMOVECR constant names
}
const
  pi        = 16#00;
  logtenof2 = 16#0b;
  e         = 16#0c;
  logtwoofe = 16#0d;
  logtenofe = 16#0e;
  zero      = 16#0f;
  lnof0     = 16#30;
  lnof10    = 16#31;
  tento0    = 16#32;
  tento1    = 16#33;
  tento2    = 16#34;
  tento4    = 16#35;
  tento8    = 16#36;
  tento16   = 16#37;
  tento32   = 16#38;
  tento64   = 16#39;
  tento128  = 16#3a;
  tento256  = 16#3b;
  tento512  = 16#3c;
  tento1024 = 16#3d;
  tento2048 = 16#3e;
  tento4096 = 16#3f;

{ 68881 control register fields and constants.  See Motorola 68881 manual
  for details.
}
const

  { Control register names
  }
  fpiar = 1;
  fpsr  = 2;
  fpcr  = 4;

  { Rounding precision constants
  }
  rp_extended = 0;
  rp_single   = 1;
  rp_double   = 2;

  { Rounding mode constants
  }
  rm_to_nearest = 0;
  rm_toward_zero = 1;
  rm_toward_minus_infinity = 2;
  rm_toward_plus_infinity = 3;


type
  two_bits = 0..3;

  fpcr_rec = packed record
    filler: 0..16#ffff;
    bsun: boolean;  { branch/set on unordered }
    snan: boolean;  { signalling not a number }
    operr: boolean; { operand error }
    ovfl: boolean;  { overflow }
    unfl: boolean;  { underflow }
    dz: boolean;    { divide by zero }
    inex2: boolean; { inexact operation }
    inex1: boolean; { inexact decimal input }
    prec: two_bits; { rounding precision }
    rnd: two_bits;  { rounding mode }
    end;

  fpsr_rec = packed record

    { floating-point condition code byte
    }
    filler: 0..15;
    negative: boolean;  { negative }
    zero: boolean;      { zero }
    infinity: boolean;  { infinity }
    nan: boolean;       { not a number or unordered }

    { quotient byte
    }
    sign: boolean;      { sign of quotient }
    quotient: 0..16#7f; { unsigned quotient }

    { exception status byte
    }
    bsun: boolean;      { branch/set on unordered }
    snan: boolean;      { signalling not a number }
    operr: boolean;     { operand error }
    ovfl: boolean;      { overflow }
    unfl: boolean;      { underflow }
    dz: boolean;        { divide by zero }
    inex2: boolean;     { inexact operation }
    inex1: boolean;     { inexact decimal input }

    { accrued exception byte
    }
    ae_iop: boolean;    { invalid operation }
    ae_ovfl: boolean;   { overflow }
    ae_unfl: boolean;   { underflow }
    ae_dz: boolean;     { divide by zero }
    ae_inex: boolean;   { inexact }
    end;
