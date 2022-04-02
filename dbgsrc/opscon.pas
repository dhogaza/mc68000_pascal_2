{[l-,b+]}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 
  
  
  Release version: 0045  Level: 1
  Processor: MC68000
  System: VERSADOS

  Operating system dependent constants for the Pascal-2 debugger.

 Last modified by KRIS on 26-Nov-1990 13:51:32
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

const

  hostopsys = vdos;

  targetopsys = vdos;

  embeddedjump = true; {does compiler generate jumps to the debugger}
  separateprocess = false; {does the debugger run as a separate process}
  segmented = false; { if the executable image segmented }
  remote = false;

  needcaching = false;

  unitnamelength = 8;

  errornum = 0;

  symext = '.st';
  mapext = '.sm';
  listext = '.ls';
  symdmpext = '.sd';
  mapdmpext = '.md';
  mapsymext = '.mp';
  profilext = '.pr';
  csisymext = 'sy';
  csimapext = 'sm';
  csilistext = 'ls';

  maxfilesopen = 10;

  librarynoerror = 0;

type

  {Compiler switches passed from the compiler via the diagnostic block}

  compilerswitches = (sw_main, sw_own, sw_doublereal, sw_case, 
                      sw_short, sw_68881, sw_ununsed7, sw_unused8, 
                      sw_unused9, sw_unused10, sw_ununsed11, sw_unused12,
                      sw_unused13, sw_unused14, sw_ununsed15, sw_unused16); 

  unit_nam = packed array [1..unitnamelength] of char;

  unitinfoblock =
    packed record
      tablelen: unsignedword;
      codestart: addressrange;
      codelen: unsignedint;
      module_name: unit_nam;
      data_base: addressrange;
      switches: packed array [compilerswitches] of boolean;
         { compiler directives }
      externtablelen: unsignedint; 
      msize: shortint;
    end;

  unitinfoptr = ^unitinfoblock;
