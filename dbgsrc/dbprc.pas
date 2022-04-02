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
  Processor: all
  System: all

  Procedure declarations for Pascal-2 debugger.

 Last modified by KRIS on 26-Nov-1990 13:51:45
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}



procedure enable_cc;
  external;


procedure d$recint;
  external;


procedure d$restart;
  external;


procedure d$continue;
  external;


procedure d$trapcontinue(inward: boolean);
  external;


procedure d$singlestep;
  external;


procedure d$tracesteps(startpc, endpc: addressrange;
                       var returnpc: addressrec);
  external;


procedure d$dataaccess(datatokenlist: datatokenptr;
                       var packet: datapacketptr;
                       var success: boolean;
                       var err: imessage;
                       var location: commandlineindex);
  external;


procedure d$setbreak(addr, entryaddr, exitaddr: addressrec);
  external;


procedure d$releasebreak(addr: addressrec);
  external;


procedure d$initinterp;
  external;


procedure d$interpreter;
  external;


procedure d$updatewatchlist;
  external;


function d$min(i, j: integer): integer;
  external;


function d$max(i, j: integer): integer;
  external;


function d$addrinrange(addr: addressrec; 
		       startrange, endrange: addressrange;
		       rangesegment: unsignedword): boolean;
  external;


procedure d$watchrelease(watchid: watchidrange);
  external;


procedure d$watchlistrelease;
  external;


procedure d$wrtnonpascaladdr(addr: addressrec);
  external;


procedure d$getframes(var framelist: framelistptr;
                      var level: integer);
  external;


procedure d$setrunconditions;
  external;


procedure d$finalend;
  external;


procedure d$quit;
  external;


procedure d$imesg(msg: imessage);
  external;


procedure d$iwarn(msg: imessage);
  external;


procedure setpos(var f: text;
                 pos: integer;
                 dummy: integer);
  external;


function d$readmap(mdl: mdl_pointer;
                   index: mapindex;
                   var rec: stmtrecord): boolean;
  external;


function d$readsym(mdl: mdl_pointer;
                   index: symbolindex;
                   var rec: debugrecord): boolean;
  external;


procedure d$getobject(mdl: mdl_pointer;
                      index: symbolindex;
                      var rec: debugrecord);
  external;


procedure d$tmpobject(mdl: mdl_pointer;
                      var index: symbolindex;
                      var rec: debugrecord);
  external;


procedure d$getform(var item: dataitem;
                    var form: debugrecord);
  external;


procedure d$getbaseform(var item: dataitem;
                        var form: debugrecord;
                        var index: symbolindex);
  external;


procedure d$dereference(var item: dataitem);
  external;


function d$upperbound(mdl: mdl_pointer;
                      f: stackpointer;
                      var form: debugrecord): integer;
  external;


function d$lowerbound(mdl: mdl_pointer;
                      f: stackpointer;
                      var form: debugrecord): integer;
  external;


function d$bound(mdl: mdl_pointer;
                 f: stackpointer;
                 var form: debugrecord;
                 boundoffset: integer): integer;
  external;



function d$reladdr(mdl: mdl_pointer;
                   addr: addressrec): addressrange;
  external;


function d$absaddr(mdl: mdl_pointer;
                   addr: addressrange): addressrec;
  external;


function d$locateaddr(addr: addressrec;
                       var mdl: mdl_pointer;
                       var proc: proctreeptr;
                       var stmt: mapindex;
                       allinfowanted: boolean): boolean;
  external;


procedure d$stmtnumlocate(addr: addressrec;
                          mdl: mdl_pointer;
                          proc: proctreeptr;
                          var stmt: mapindex);
  external;


function d$stmtlocate(mdl: mdl_pointer;
                      proc: proctreeptr;
                      stmt: integer;
                      var index: mapindex): boolean;
  external;


function d$firstvisibleframe: stackpointer;
  external;


function d$isstring(mdl: mdl_pointer;
                    var form: debugrecord): boolean;
  external;


procedure d$cmderror(msg: imessage);
  external;


procedure d$makeactive(p: commandpointer);
  external;


procedure d$deactivate(p: commandpointer);
  external;


procedure d$discardcommands;
  external;


procedure d$wrtlist(mdl: mdl_pointer;
                    first, last, firstpos1, firstpos2: integer);
  external;


procedure d$wrtlocation(mdl: mdl_pointer;
                        proc: proctreeptr;
                        stmt: mapindex;
                        addr: addressrec;
                        complete: boolean);
  external;


procedure d$wrthistory(cnt: integer);
  external;


procedure d$wrtframes(complete, verbose: boolean;
                      limitframe: stackpointer);
  external;


procedure d$wrtdata(resultitem: dataitem;
                    l, r: integer;
                    var newline: boolean);
  external;


procedure d$wrtwtch(w: watchpointer);
  external;


procedure writeversion;
  external;


procedure d$regwtchresidence(w: watchpointer);
  external;


procedure d$resetwatchlist;
  external;


function d$getvalue(var item: dataitem;
                    gettyp: datatypes;
                    util1, util2: integer;
                    var packet: datapacketptr;
                    var err: imessage): boolean;
  external;


procedure d$wrtsymbolname(var sym: symbolname;
                          width: integer);
  external;


procedure d$wrtaddress(val: addressrec);
  external;


procedure d$initproctree(mdl: mdl_pointer);
  external;


procedure d$loadmdl(mdl: mdl_pointer);
  external;


procedure d$frameupdate;
  external;


procedure d$initialize;
  external;


procedure d$interactive;
  external;


procedure d$execution;
  external;


procedure d$inithistory;
  external;


procedure d$addhistory(addr: addressrec);
  external;


function d$getprevhistory(cnt: historyindex): addressrec;
  external;


function d$historycount(cnt: historyindex): historyindex;
  external;


function d$getproctreerec: proctreeptr;
  external;


procedure d$freeproctreerec(proc: proctreeptr);
  external;


procedure d$inittarget(var units: unitptr); 
  external;

procedure d$signoff;
  external;


procedure d$csi;
  external;


function d$maxfilesopen: integer;
  external;


function d$beingdebugged: boolean;
  external;

procedure d$spawn(var commandline: stringdescr);
  external;

procedure d$setsigblock(var sigblock: dbset);
  external;

procedure d$getsigmask(var sigmask: dbset);
  external;
