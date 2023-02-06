{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Tree Dumper (Debugging Aid)

 Last modified by KRIS on 21-Nov-1990 15:25:38
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}


procedure opentree;

{opens the tree dump file for debugging travrs
}


  begin
    case hostopsys of
      vdos: rewrite(dump, 'tree.tm');
      otherwise rewrite(dump, 'tree.tmp');
      end;
  end;


procedure closetree;

{closes the tree dump file for debugging travrs
}


  begin
    close(dump);
  end;
