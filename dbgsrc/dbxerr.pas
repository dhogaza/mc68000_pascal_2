{[l-,b+]}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1984 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0001  Level: 1
  Processor: vax
  System: vms

  Debugger executive error handling routines.

 Last modified by BRUCE on 23-May-1986 15:51:41
 Purpose:


}


procedure writepascalerror(errnum: integer);
 {nothing for now}


  begin
  end;


procedure processerror(errmsg: xtmessage;
                         errnum: integer;
                         var err: imessage);

 {Process an error reported by the target supervisor.}


  begin {processerror}
    case errmsg of
      systemerror:
        begin
        end;
      pascalerror:
        begin
        writepascalerror(errnum);
        programdead := true;
        end;
      termination:
        begin
        end;
      inactive:
        begin
        end;
      processerr:
        begin
        end;
      otherwise
        if (errmsg > firstcommanderror) and (errmsg < lastcommanderror) then
        {convert to integpreter command errors}
          case errmsg of
            incompatsize: err := illegalassign;
            indexhigh: err := subscripttoohigh;
            indexlow: err := subscripttoolow;
            realformaterr: err := badreal;
            stackerr: err := expstackerr;
            stackoverflow: err := expstackoverflo;
            stackunderflow: err := expstackunderflo;
            subrangerr: err := badsubrange;
            watcherr: err := watchnotfound;
            watchlimit: err := watchlimitreached;
            end
        else choke('processerr');
      end;
  end; {processerror}
