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

  Pascal-2 debugger host to target common definitions.

 Last modified by KRIS on 26-Nov-1990 13:50:57
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

const

  stringsize = 128; { max number of characters in dbstring }

  msymbolname = 32; { max number of characters in a symbol }

  datapacketsize = 32; { number of units in a data packet }

  watchprobelimit = 255; { max number of watching probes allowed }

  watchlimitsize = 32; { max number of units watched in a watched variable }

type

  { String storage }

  dbset = packed set of 0..255;

  isymbolname = 0..msymbolname; { index for symbol names }
  stringindex = 1..stringsize; { index for dbstring }

  datapacketindex = 1..datapacketsize;
  datapacketstring = packed array [datapacketindex] of char;

  dbstring = packed array [stringindex] of char;

  stringdescr =
    record
      txt: dbstring;
      len: integer;
    end;

  pstringdescr = ^stringdescr;

  pstringlist = ^stringlist;
  stringlist =
    record
      next: pstringlist;
      str: stringdescr;
    end;

  { the quick frame list as produced by the target }

  framelistptr = ^framelistrec;
  framelistrec =
    record
      fp: addressrange; { framepointer }
      pc: addressrec; { current pc or return pc }
      next, prev: framelistptr; { links for the list }
    end;

  { Save the complete filenames for the symbol table file, the statement map
    file and the listing file. }

  filenamesptr = ^filenamesrec;

  filenamesrec =
    record
      mapfilename, symfilename, listfilename: dbstring;
    end;

  { A list of unit records is maintained for every compilation unit in the
    executable file.  In an embedded jump model, these are derived from
    the information passed from the compiler via the diagnostics section.
    In a separate process debugger where the executable file is read, 
    the executable symbol table is used to first build a list of all
    module, then those not visible to the debugger are culled out. }

  unitptr = ^unitrecord;

  unitrecord =
    record
      next: unitptr;
      longlink: unitptr; {used for the complete list of all
                          compilation units with separate process debuggers}
      code_start, code_end: addressrange; { address boundaries }
      nam: symbolname; { linker symbolic name }
      doublereal: boolean; { $double switch used in module }
      shortinteger: boolean; { $shortint switch used in module }
      casesensitive: boolean; { $case switch used in module }
      mainunit: boolean;  {is this the main module}
      fpcoprocessor: boolean; {was fp co-processor used}
      data_base: addressrange; { base of own data }
      externaladdress: addressrec; { address of external variables }
      case boolean of
        true: (datasegment, codesegment: unsignedword); 
		{ used in segmented architectures }
        false: (picoffset: addressrange);
    end;

  { watching parameters }

  watchidrange = 0..watchprobelimit; { range of watched variable ids }
  watchidtype = set of watchidrange; { set used to record ids in use }

  { where to look for data }

  data_access_types = (ins_space, dat_space, gen_reg, ptr_reg, real_reg,
                       fpp_reg, local, watchedlocal);

  { types of data as stored in a data packet }

  datatypes = (scalar, singlereal, doublereal, realstring, singlestring,
               doublestring, settype, stringtype, pointer, nonetype, storeref,
               operators);

  { opertators used in the target expression evaluator }

  operatortypes = (equals, negateop, notop, andop, orop, inop, addop,
                   subtractop, timesop, intoop, divop, modop, equalop,
                   notequalop, lessthanop, lessequalop, greaterthanop,
                   greaterequalop);

  { data packets used for transferring data between host and target }

  datapacketptr = ^datapacket;

  datapacket =
    record
      next: datapacketptr; { link to next packet }
      len: integer; { how many bits are used }
      fil: packed array [1..3] of char;
      case typ: datatypes of
        scalar: (sc: integer);
        realstring, singlestring, doublestring, stringtype:
          (str: datapacketstring);
        singlereal, doublereal:
          (sr: singlerealkludge; { single machine representation }
           dr: realarray;
           fppregsav: boolean); { double machine representation }
        pointer: (pt: addressrec);
        settype: (st: dbset);
        nonetype:
          (dnone: packed array [1..datapacketlength] of transferunit);
        storeref:
        { information describing where to fetch a value }
          (addr: addressrec;
           off: integer;
           lower, upper: integer;
           xisstring: boolean;
           signed: boolean;
           makereal: boolean;
           isfppreg: boolean;
           storetyp: types;
           store: data_access_types);
        operators:
          (op: operatortypes;
           optyp: types;
           fpp: boolean);
    end;

  { Data Tokens - Data tokens are used by the interpreter to communicate
    with the target supervisor about any data references.  This includes
    data retrieval, upkeep of watched variables and expression evaluation.
  }

  datatokentypes = (commence, addressref, offset, deref, newlevel, resetlevel,
                    atomic, eval, evaladdr, evaloffset, returnvalue, addtoset,
                    makeset, makereal, operator, watchtoken, updatewatch,
                    watchreg, reference, returnaddr, theend);

  datatokenptr = ^datatokenrec;

  datatokenrec =
    record
      next: datatokenptr; { link to the next datatoken }
      cmdloc: integer; { current index in the command line }
      case typ: datatokentypes of
        commence, { start token - clean slates }
        newlevel, { enter a new context }
        resetlevel, { restore the previous context }
        makereal, { convert an integer to a real }
        deref, { use the current context to evaluate a new address }
        evaladdr, { use the top of the stack to evaluate a new address }
        eval: { evaluate the stack }
          ();
        addressref: { set the current context address }
          (case store: data_access_types of
             dat_space, ins_space, watchedlocal: (addr: addressrec);
             gen_reg, ptr_reg, real_reg, fpp_reg:
               (off: integer;
                depth: integer));
        offset: (bitoff: integer); { adjust the current address by bitoff }
        operator:
          (op: operatortypes;
           optyp: types;
           fpcoprocessor: boolean);
        reference: { establish a reference of the given type }
          (refisstring: boolean;
           reflen: integer;
           signed: boolean;
           case reftyp: types of
             subranges, scalars, bools, chars:
               (upperlim, lowerlim: integer);
             reals, doubles: (isfppreg: boolean));
        atomic: { an immediate value is given in the datapacket }
          (packet: datapacketptr); { a datapacket is arriving }
        evaloffset: { evaluate the current context then restore context and
                     adjust the offset accordingly }
          (bitlength, bitsperelt, lowerbnd, upperbnd: integer);
        returnvalue: { return the immediate value from the top of stack }
          (case evaltyp: datatypes of
             scalar: (upperord, lowerord: integer;
                      extended: boolean);
             singlestring, doublestring: (left, right: integer);
             settype, stringtype, nonetype: (returnlen: integer));
        addtoset: { add val to the current set at top of stack }
          (val: integer;
           range: boolean);
        watchtoken: { establish a watch based on the current context }
          (ident, wlen, level: integer;
           lost: boolean);
        updatewatch:
          (watchid: integer); { update the current value for the watched
                               variable }
        watchreg:             {ensure that watch is on register - this is
                               needed where the original watch is put on the
                               stack where reg value is stored}
          (regtype: data_access_types;
           regoffset: integer);
    end;

  { Host - target error messages }

  xtmessage = (noerror, firstruntimeerror, { delimiter for the runtime errors
                                            }
               systemerror, { operating system has detected an error }
               pascalerror, { pascal runtime error detected }
               termination, { program has terminated }
               inactive, {program is not in an active state }
               incompletestack, {stack frame list was not completed}
               lastruntimeerror, { delimeter for runtime errors }
               firstcommanderror, { delimeter for command line errors }
               incompatsize, { incompatible sizes for assignment }
               subrangerr, { subrange exceeded }
               illegaladdress, { illegal address }
               indexhigh, { array subscript too high }
               indexlow, { array subscript too low }
               realformaterr, { error reading real number }
               watchlimit, { limit reached for watching variable }
               stackoverflow, { target stack has overflowed }
               stackunderflow, { target stack has under flowed }
               stackerr, { consistency problem in target stack }
               watcherr, { watched variable record not found }
               xtnoexecfilemsg, { Lookup failed on executable file }
               xtnotdebugcompiled, { Program not compiled with debug switch }
               xtfpcerror, { Program uses fp co-processor where none detected }
               lastcommanderror, { delimeter for command line errors }
               firstconsistencyerror, lastconsistencyerror,
               firstprocesstraceerror, { delimeter for process tracing errors
                                        }
               processerr, { error ocurred when tracing process }
               cantreadtext, { unable to read process text }
               cantwritetext, { unable to write process text }
               lastprocesstraceerror); { delimeter for process tracing errors
                                        }



procedure xt_start(execargs: stringdescr;
                          var errmsg: xtmessage;
                          var errnum: integer);
  external;


procedure xt_stop(var errmsg: xtmessage;
                         var errnum: integer);
  external;


procedure xt_continue(var currentpc: addressrec;
                             var breakpointhit: boolean;
                             var exited: boolean;
                             var errmsg: xtmessage;
                             var errnum: integer);
  external;


procedure xt_tracesteps(startpc, endpc: addressrange;
                        var currentpc: addressrec;
                        var returnpc: addressrec;
                        var breakpointhit: boolean;
                        var exited: boolean;
                        var errmsg: xtmessage;
                        var errnum: integer);
  external;


procedure xt_singlestep(var watchpointhit: boolean;
                        var errmsg: xtmessage;
                        var errnum: integer);
  external;


procedure xt_intrrpt;
  external;


procedure xt_dataaccess(var datatokenlist: datatokenptr;
                        var packet: datapacketptr;
                        var errmsg: xtmessage;
                        var errnum: integer);
  external;


procedure xt_setbreak(addr: addressrec;
                      entryaddr, exitaddr: addressrec;
                      var errmsg: xtmessage;
                      var errnum: integer);
  external;


procedure xt_settmpbreak(addr: addressrec;
                         var errmsg: xtmessage;
                         var errnum: integer);
  external;


procedure xt_releasebreak(addr: addressrec;
                          var errmsg: xtmessage;
                          var errnum: integer);
  external;


procedure xt_settrapmod(var errmsg: xtmessage;
                        var errnum: integer);
  external;


procedure xt_wtchrelease(watchid: watchidrange;
                      var errmsg: xtmessage;
                      var errnum: integer);
  external;


procedure xt_wtchlstrelease(var errmsg: xtmessage;
                          var errnum: integer);
  external;


procedure xt_setrunconditions(xstep, xhistory, xtrace, xproceed: boolean);
  external;


procedure tx_interpreter(xcurrentpc: addressrec;
                         xbreakpointhit: boolean;
                         xwatchpointhit: boolean;
                         xnewlevel: boolean;
                         xterminalcondition: boolean;
                         xcontrol_c: boolean;
                         xerrorencountered: boolean;
                         xinitialize: boolean);
  external;


procedure tx_updatewatch(xcurrentpc: addressrec);
  external;


procedure xt_getframes(var framelist: framelistptr;
                       var level: integer;
                       var errmsg: xtmessage;
                       var errnum: integer);
  external;


procedure xt_getwatchinfo(var xwatchfound: watchidtype;
                          var xpreviouspc: addressrec);
  external;


procedure xt_inittarget(var execfilename: pstringdescr; var unitlist: unitptr;
                         testing: boolean;
                         var globaladdr: addressrec;
        		 var stackseg: unsignedword;
                         var errmsg: xtmessage;
                         var errnum: integer);

  external;

procedure xt_linksymbol(addr: addressrec;
                        var symbol: stringdescr;
                        var errmsg: xtmessage;
                        var errnum: integer);
  external;

procedure xt_setsigblock(var newsigblock: dbset);
  external;

procedure xt_getsigmask(var sigmask: dbset);
  external;

procedure exitst(status: integer);
  external;


procedure p_term;
  external;


procedure choke(info: packed array [low..high: integer] of char);
  external;
