{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1980, 1981, 1982 by Oregon Software, Inc.
  All Rights Reserved.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  CSI constant declarations
  Release version: 2.1E  Level: 2  Date: 22-Oct-1985 14:22:27
  Processor: VAX
  System: VMS
}

  ExtLen = 3;
  mArgName = 16;
  mArgValue = 255;

procedure emt(i:integer); external;  

const

  PROTKopsys = false;  
  VMSopsys = true;
  RSXopsys = false;
  RSTSopsys = false;
  RT11opsys = false;
  MSDOSopsys = false;
  VDOSopsys = false;
