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

  Header file for Pascal-2 profiler.

 Last modified by KRIS on 26-Nov-1990 13:52:44
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

%include dbprprg;



%include arccon;

const

  dbgdiagtablesize = 52;  {Size of the debugger diagnostic section}


type

  unix_flavors = (nohost, uniplus, uniplusiii, uniplusv, masscomp, xenix,
                  tandy, sun, munix, regulus, wicat, candd, perpos, callan,
                  ti, venix, uniflex, vmev2, uniplusv2, sys5coff, uspare68k1,
                  uspare68k2, uspare68k3, pcix, inix86, inix286, atxenix,
                  uspare2, uspare3, ultrix, morebsd, uspare4, uspare5,
                  uspare6);

  %include hstcon;
  %include dbghdr;
  %include dbxthdr;


type

  pclist = array [1..3000] of addressrange;
  pcpointer = ^pclist;

  countlist = array [1..16000] of unsignedint;
  countpointer = ^countlist;

  { procedure tree record }

  proctreeptr = ^proctreerec;

  proctreerec =
    record
      left, right: proctreeptr; { left and right pointers }
      startpc, endpc: addressrange; { the starting and ending addresses for
                                     this procedure }
      procno: mapindex; { the first map statement for the procedure }
      symid: mapindex;
      lines: mapindex;  { # of lines in this procedure }
      pcmap: pcpointer; { array of pc's for lines in procedure }
      counts: countpointer; { buckets for lines execution counts }
      total: unsignedint; { total number of executions this procedure }
      calls: unsignedint; { total number of calls }
    end;

  { Module descriptor record }
      
  mdl_pointer = ^mdl_record;

  mdl_record = record
    next: mdl_pointer; {next module in list }
    code_start, code_end: addressrange; { start and end boudaries in code }
    nam: symbolname; { name of module }
      { auxilliary files }
    symfile: file of debugrecord;
    mapfile: file of stmtrecord;
    listfile: text;
    proctree: proctreeptr; { procedure tree pointer }
    count: unsignedint; { total execution count for this module }
    stmt: mapindex;
    mainsym: mapindex;
    info: (unloaded, noinfo, syminfo, allinfo); 
      { state of files for this modules }
    datasegment, codesegment: unsignedword; { for segmented architecures }
    end;

  { Stack frame records }

  frame_pointer = ^frame_record;

  frame_record = record
    uplink, downlink: frame_pointer; 
    fp: addressrange; { frmae pointer }
    proc: proctreeptr; { pointer to node in procedure tree }
    stmt: mapindex; { index into statement map }
    end;


  {  List of modules with files opened. }

  plastmdlopened = ^lastmdlopened;
  lastmdlopened =
    record
      next: plastmdlopened; { next record }
      mdl: mdl_pointer;
    end;


  actiontype = (initialize, procedureentry, procedureexit, statement, error, 
                nonlocalgoto, terminated, procedurecall);

  line = packed array [1..132] of char;
  lineptr = ^line;


var

  entrypoint: addressrec;

    {These flags are used at entry into the debugger to determine whether or
     not to return immediately to the target program.}
  notskipping: boolean; {If false then debugger will only be entered upon
                         error, termination or interrupt}
  looking: boolean; {If false then debugger will not be entered at statement}
  terminalcondition: boolean; {has target program terminated}

  register: savedregisters; {registers saved at entry to debugger}

    {Do not touch the declarations up to this point without also modifying
     opdbg.}

  interrupted: boolean;
  stepping: boolean;
  justproceeding: boolean;
  initialized: boolean;
  first_time: boolean;
  out, inp: text;
{warning: do not touch the variables down to here without also 
 modifying opdbg.mar}
  levelchanged: boolean;
  running: boolean;
  
  mdl_list: mdl_pointer;
  main_mdl: mdl_pointer;
  currentstackframe: frame_pointer;
  mainproc: proctreeptr;

  totalprocs: integer;
  laststmtindex: integer;
  profilefile: text;
  { Module list maintenance }

  first_lmo, last_lmo: plastmdlopened; { ptrs to LastUnitOpened queue }
  num_mdls_open: integer; { number of modules with open files }
  max_mdls_open: integer; { max number of modules allowed to have files open
                           at a time }
  maxfiles: integer; { max # of files allowed to be open open }
  first_mdl_loaded: boolean; { unit info from files loaded for first unit }


procedure d$rego; 
  external;

function d$globaladdr: addressrec;
  external;

function p_getdia: unitinfoptr;
  external;

function p_stdia: addressrec;
  external;

function p_enddia: addressrec;
  external;

procedure getpos(var f: text;
                 var block, byte: integer);
  external;

function p_inew(bytesize: addressrange): intptr;
  external;

procedure d2$dbg2(action: actiontype);
  external;

procedure d$pro2(action: actiontype);
  external;

procedure p_rdsfst(var f: text;
                   var p: lineptr;
                   var l2: integer);
  external;

procedure p_stdio(var inp, out: text);
  external;


procedure initproctree(mdl: mdl_pointer);
  forward;


procedure d$clearexit;
  external;

