{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1983, 1985, 1986 by Oregon Software, Inc.
  All Rights Reserved.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Release version: 0045  Level: 1  Date: 21-Nov-1990
  Processor: M68000
  System: versaDOS
 }

{CSI constant declarations}

  ExtLen = 2;
  mArgName = 16;
  mArgValue = 132;

  PROTKopsys = false;
  VMSopsys = false;
  RSXopsys = false;
  RSTSopsys = false;
  RT11opsys = false;
  VDOSopsys = true;  
  MSDOSopsys = false;

  procedure emt(i: integer); external; {only used for RSTS}

