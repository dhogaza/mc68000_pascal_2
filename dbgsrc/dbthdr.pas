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

  Target supervisor common header file

 Last modified by KRIS on 26-Nov-1990 13:50:48
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

%include dbghdr;
%include dbxthdr;

const

  dbgdiagtablesize = 52;  { Size of the debugger diagnostic section }

  maxstack = 10; { Max depth of the stack used in the expression evaluation
                  machine. }

    {Support for readreal library call on 68k/unix libraries}

  IEEE_single	= 16#28E0 ;
  IEEE_double	= 16#4BE0 ;

type


  { The data_array is used for transfering data between local use and memory }

  data_array = array [1..datatransferlength] of transferunit;

  genregset = array [regindex] of integer;
  ptrregset = array [regindex] of integer;
  realregset = array [regindex] of integer;

  stackindex = 1..maxstack;

  { Save context for expression evaluation machine. }

  contextrec =
    record
      storeclass: data_access_types;
      addr: addressrec;
      offset: integer;
    end;

  { Watched variables }

  watchingrecptr = ^watchingrec;

  watchingrec =
    record
      next: watchingrecptr; { link to next record }
      ident: watchidrange; { identifier for this probe }
      level: integer; { dynamic level of the variable }
      value: data_array; { stored original value of the variable }
      firstmask, lastmask: transferunit; { mask the first and last transfer
                                          units if variable is off of
                                          addressable boundaries }
      watchlength: integer; { actual number watched }
      addr: addressrec; { address of the variable }
      store: data_access_types; { how stored }
      offset: addressrange;  {offset in value}
      valueset: boolean;
      toolarge: boolean; {the value being watched is bigger than max allowed}
    end;

    {support for 68k readreal library call}
  realstatus	= (r_noerror, r_underflowerr, r_overflowerr, r_syntaxerr) ;


var

  active: boolean;  {is debugged process active; always true on embedded}
  processerror: boolean;  {has debugged process hit error condition;
                           always false on embedded}

  watchfound: watchidtype; {which watched variables were modified}
  previouspc: addressrec; {saved pc of last instruction}

    { stack segment (for segmented machines) }
  stacksegment: unsignedword;

  { Stack maintenance for the expression evaluation machine }

  datapacketfreelist: datapacketptr; {pointer to pool of free data packets}
  context: array [stackindex] of contextrec; { context stack }
  packetstack: array [stackindex] of datapacketptr; { data stack }
  contextindex, { context stack index }
   packetindex: 0..maxstack; { data stack index }
  setpacket: datapacketptr; { special packet for building sets }
  lastsetitem: integer; { previous set element appended }
  firsttime: boolean;

  { Current context parameters }

  currentaddress: addressrec;
  currentoffset: integer;
  currentstorageclass: data_access_types;
  currentlocaldata: data_array;

  { watched variable list }

  watchinglist: watchingrecptr;

  { quick frame list }

  framelisthead: framelistptr;

  { status of saved fpp registers }

  fppregsaved: boolean;  {have the fpp registers been saved yet}
  fppinuse: boolean;  {is the math coprocessor in use with this program}



procedure p_ctof(function nextch: char;
                 var val: singlerealkludge;
                 var err: sysint);
  external;


procedure p_ctod(function nextch: char;
                 var val: real;
                 var err: sysint);
  external;


procedure p_ftoc2(procedure putf(ch: char);
                  value: singlerealkludge;
                  totalwidth: sysint;
                  fracdigits: sysint);
  external;


procedure p_dtoc2(procedure putf(ch: char);
                  value: real;
                  totalwidth: sysint;
                  fracdigits: sysint);
  external;




function d$sglconv(i: integer; fpp: boolean): singlerealkludge;
  external;


function d$sglnegate(x: singlerealkludge; fpp: boolean): singlerealkludge;
  external;


function d$sgladd(x, y: singlerealkludge; fpp: boolean): singlerealkludge;
  external;


function d$sglsub(x, y: singlerealkludge; fpp: boolean): singlerealkludge;
  external;


function d$sglmult(x, y: singlerealkludge; fpp: boolean): singlerealkludge;
  external;


function d$sglinto(x, y: singlerealkludge; fpp: boolean): singlerealkludge;
  external;

function d$dblconv(i: integer; fpp: boolean): realarray;
  external;

function d$dblnegate(x: realarray; fpp: boolean): realarray;
  external;

function d$dbladd(x, y: realarray; fpp: boolean): realarray;
  external;

function d$dblsub(x, y: realarray; fpp: boolean): realarray;
  external;

function d$dblmult(x, y: realarray; fpp: boolean): realarray;
  external;

function d$dblinto(x, y: realarray; fpp: boolean): realarray;
  external;

function d$dblless(x, y: realarray; fpp: boolean): integer;
  external;

function d$dbllesseq(x, y: realarray; fpp: boolean): integer;
  external;

function d$dblgret(x, y: realarray; fpp: boolean): integer;
  external;

function d$dblgreteq(x, y: realarray; fpp: boolean): integer;
  external;

procedure d$fppsav;
  external;


procedure d$fppres;
  external;


procedure d$putsgl(var source: extendedrealarray;
                    var destination: data_array);
  external;


procedure d$putdbl(var source: extendedrealarray;
                    var destination: data_array);
  external;


procedure d$getsgl(var destination: extendedrealarray;
                    var source: data_array);
  external;


procedure d$getdbl(var destination: extendedrealarray;
                    var source: data_array);
  external;


procedure readreal (
		function nextch	: char ; 
		var	result	: realarray ;
		var	status	: realstatus ;
			mode	: unsignedword ) ;
external ;
