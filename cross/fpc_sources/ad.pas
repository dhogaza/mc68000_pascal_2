{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045 Level: 1
  Processor: All
  System: All

  Pascal-2 ANALYS/BODY Output Dumper (Debugging Tool)

 Last modified by KRIS on 21-Nov-1990 15:36:00

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

program ad;

uses config, a_t;

begin {ad}
  assign(tempfiletwo, 'temptwo.tmp');
  reset(tempfiletwo);
  assign(locals, 'locals.tmp');
  reset(locals);

  repeat
    read(tempfiletwo, tempfilebuf);
  until (tempfilebuf.intcode = stmt) and (tempfilebuf.s = endall);

  close(tempfiletwo);
  close(output);
end {ad} .
