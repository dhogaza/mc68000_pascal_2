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

  Pascal-2 debugger common header file.

 Last modified by KRIS on 26-Nov-1990 13:52:00
 Purpose:
Update release version for PL-GS0-GS0 at 2.3.0.1

}

%include dbprg;

  %include arccon;
  %include hstcon;
  %include dbghdr;
  %include dbxthdr;

const

  maxhistory = 50; { max number of statements maintained in history }

  nilpointervalue = addressrec(0, 0); { value of nil pointer as addressrec }
  nulladdress = addressrec(0, 0);

  maxerrmsglen = 60; { max number of characters in an error message }

  { Symbol Table Access }

  intindex = 1; { integer form index }
  realindex = 4; { real form index }
  doubleindex = 5; {double real index}
  charindex = 6; { char form index }
  boolindex = 7; { boolean form index }
  textindex = 8; { text file form index }
  nilindex = 9; { NIL form index }
  noneindex = 9; { error form index }

  formcachesize = 100; { Size of form cache }

  constsize = 128; { units in a constant }

  firstidentchar = 'A';
  lastidentchar = '\';


%include csicon;


  type

  { command string interpreter parameters }

    argtype = (unknownarg, execfilearg, inputdirarg, testarg, 
               versionarg, malformedarg, missingarg);
    subargtype = 0..1;

    %include csityp;

    pargvalue = ^argvalue;
    pargvaluelist = ^argvaluelist;
    argvaluelist =
      record
        next: pargvaluelist;
        arg: argvalue;
        end;

  shortstring = packed array [1..16] of char;

  symbolindex = integer; { symbol table index }

  { Procedure tree records contain symbol table and statement map indexes for
    each procedure and are contructed into a balanced binary tree based on 
    the entry and exit addresses of the procedure. }

  proctreeptr = ^proctreerec;

  leftrightrec = 
    record
      case boolean of
	true: (index: integer); {file indeces}
	false: (ptr: proctreeptr) {pointer}
    end;

  proctreerec =
    record
      firstsym, lastsym: symbolindex; { the first and last symbols for this
                                       procedure }
      startpc, endpc: addressrange; { the starting and ending addresses for
                                     this procedure }
      procno: mapindex; { the first map statement for the procedure }
      staticidx: integer; { the record index of the static nesting procedure }
      left, right: leftrightrec; {left and right file indeces/pointers}
    end;

  proctreefiletype = packed file of proctreerec;

  {  The module records will maintain needed information for every compilation
     unit that is compiled with the debug option. }

  charfile = packed file of char;

  symindexarray = array [debughashindex] of symbolindex;

  mdl_pointer = ^mdl_record;

  stackpointer = ^stackframe;

  mdl_record =
    record
      next: mdl_pointer; { next module }
      code_start, code_end: addressrange; { boundary addresses }
      nam: symbolname; { name of the module }
      data_base: addressrange; { base of own data }
      main_sym: symbolindex; { symbol table index of the level 1 procedure }
      base_frame: stackpointer; { base stack frame for the module }
      symfile: file of debugrecord; { sybol table file }
      mapfile: file of stmtrecord; {statement map file }
      listfile: text {charfile} ; { listing file }
      filenames: filenamesptr; { used for UNIX only }
      {describe the current state of the auxilliary files}
      info: (unloaded, {no attempt has been made to open the files}
             noinfo, {the files were not found for this module}
             syminfo, {statement map and symbol table open, listing not found
                       }
             allinfo); {all three files open}
      datasegment, codesegment: unsignedword; { for segmented architecures }
      externaladdress: addressrec;
      loaded: boolean; {are files currently open}
      proctreeinit: boolean; { has the procedure tree been initialized }
      casesensitive: boolean; {was $case switch used}
      fpcoprocessor: boolean; {is floating point co-processor in use}
      realsize: 0..8; { size of reals for the module }
      intsize: 0..4; { size of integers for the module }
      firstoccur: symindexarray; { index of first occurrence in symbol table }
      picoffset: addressrange;  { offset of external variables in pic environ }
      case boolean of
        true: (proctreefileoff: integer);
                { offset into the file of the procedure records tree }
        false: (proctree: proctreeptr); 
                { pointer into the procedure records tree }
    end;

  { The stack frames list gives a picture of the current stack in the target
     program.  The information stored here fro every procedure, including a
     link into the procedure tree record, allows the interpreter quick access
     for information needed for such things as data retrieval. }

  {stackpointer = ^stackframe;}
  stackframe =
    record
      dynamiclink: stackpointer;
      staticlink: stackpointer;
      uplink: stackpointer; { the link up to the next available unused record}
      mdl: mdl_pointer; { module of this frame's code }
      database: addressrange; { address of base of data for the frame }
      dynamiclevel: integer; { number of frames in }
      lexiclevel: integer; { lexical level }
      pc: addressrec; { either current pc (for the current stack frame) or
                       return pc }
      fp: addressrange; { frame pointer }
      nextstmtpc: addressrec; { pc of the next statement to be executed }
      proc: proctreeptr; { procedure record for frame }
    end;

  { Data Items are used in the parsing process to both maintain a current 
    picture of data (either variable, constant, or the result of an expression)
    and to store the datatokenlist that will be used to instruct the target
    supervisor on retrieval of the data. }

  dataitem =
    record
      mdl: mdl_pointer; { module of form for data item }
      frame: stackpointer; { frame containing item }
      index: symbolindex; { index of form for data item }
      typ: datatypes; { the current type of the data item }
      datatokenlist, dtok: datatokenptr; { the head of the datatokens list and
                                          the current datatoken }
      bitlen: integer; {length in bits of the item }
      store: data_access_types; { how is item stored }
      regoff: regindex; { offset into register set (if register store) }
      wasfpreg: boolean;  {is this an fp reg allocated var that is stored on
                           stack}
    end;

  { Command Input }

  commandbuf = dbstring;
  commandlineindex = 0..stringsize;

  commandpointer = ^commandline;
  commandline =
    record
      next: commandpointer; { next command line block }
      embedlink: commandpointer; 
                            { next embedded command line for a control point}
      len: commandlineindex; { length of command line}
      idx: commandlineindex; { current index in command line }
      com: commandbuf; { text of command line }
    end;

  { Breakpoints, Watched Variables, Macros }

  watchpointer = ^watch;
  watch =
    record
      next: watchpointer; { link to next watch }
      ident: watchidrange; { number to identify the probe }
      level: integer; { dynamic level of base frame for probe }
      cmds: commandpointer; { stored commands }

      name: stringdescr; { name of variable being watched }
      what: dataitem; { dat descriptor for variable being watched }
      where: datapacket; { location to watch }
      store: data_access_types; { how stored }
      regoff: regindex; { register offset, if register }
    end;

  { breakpoints }

  breakpointer = ^breakrec;
  breakrec =
    record
      next: breakpointer; { link to next breakpoint }
      addr: addressrec; { identifying address }
      segment: unsignedword; { for segmented architectures }
      mdl: mdl_pointer; { module of breakpoint }
      stmt: mapindex; { statement number }
      proc: proctreeptr; { procedure record }
      count: integer; { count of times to continue }
      cmds: commandpointer; { stored commands }
    end;

  { stepping record (for patching type debuggers) }

  steprecord =
    record
      stmtentrypc, stmtexitpc: addressrange; { boundary addresses of the
                                              statement }
      procentrypc, procexitpc: addressrange; { boundary addresses of the
                                              procedure }
      prevaddr: addressrange; { last address stepped from }
      segment: unsignedword; { for segmented architecures }
      mdl: mdl_pointer; { current module }
      stmt: mapindex; { index for statement in the statement record file }
      proc: proctreeptr; { procedure record }
      exitstep: boolean; { is this the exit statement for the procedure }
    end;

  { Interpreter Error Messages }

  imessage = (notanerror,
              arrayexpected, { Variable is not an array }
              badexpression, { Invalid variable or expression }
              badindex, { ']' or ',' must follow index expression }
              badinteger, { Invalid integer constant }
              badreal, { Invalid real constant }
              badsetelement, { Set elements must be non-real scalar type }
              badsubrange, { Variable subrange exceeded }
              booleanconstexpected, { TRUE or FALSE expected here }
              cantassignvariable, { Can't assign variable }
              cantexec, { Unable to execute command }
              cantfindreg, { Can't access stored register value from stack }
              cantreadprocess, { Can't read child process text }
              cantwriteprocess, { Can't write child process text }
              commaexpected, { Comma expected here }
              commandnotterminated, { Command not properly terminated }
              constassign, { Can't assign a value to a constant }
              elementnotcompatible, { Set element is incompatible or out of
                                     range }
              expstackerr, { Expression evaluation stack error }
              expstackoverflo, { Expression evaluation stack underflow }
              expstackunderflo, { Expression evaluation stack overflow }
              fieldexpected, { Field identifier expected here }
              fpcerror, { Program compiled with floating point co-processor 
                            switch - fpc not detected on current machine }
              illegaladdr, { Illegal address }
              illegalassign, { Invalid command or assignment }
              illegalident, { Invalid identifier }
              illegalscalar, { Illegal value for scalar variable }
              inactiveprocess, { Process is inactive }
              indexnotcompatible, { Index is incompatible or out of range }
              lparenexpected, { Left paren expected here }
              linetoolong, { Line too long }
              macronameexpected, { Macro name expected }
              nilpointer, { Attempted reference through a NIL pointer }
              nocanwrite, { Can't write variables of this type }
              noclosingquote, { No closing quote for literal string }
              noexecfilemsg, { Lookup failed on executable file }
              nomaindebug, { Main module not compiled with debug switch }
              nomainsymbols,
                { Can't locate symbol table for main program unit }
              nomodname, { Module name expected here }
              noprocname, { Procedure name expected here }
	      norealexpr, { Real expressions not allowed }
              norefonregstore, { ref() invalid on register stored variable }
              nostackframes, { Can't identify stack frames }
              nosuchbreakpoint, { No breakpoint is set at this location }
              nosuchframe, { No such stack frame }
              notcompatible, { Assignment operands of incompatible types }
              notacommand, { Invalid debugger command }
              notafield, { This identifier is not a field name }
              notavariable, { This identifier is not a variable }
              notdebugcompiled, { Program not compiled with debug switch }
              notoption, { This option not available }
              numberexpected, { Number expected here }
              ipanicmsg, { Unexpected condition }
              pointerexpected, { Pointer expected here }
              processterminated, { Process terminated }
              processtrace, { Error tracing process }
              procnotfound, { Can't find this procedure }
              rparenexpected, { Right parenthesis expected here }
              recordexpected, { Variable of type record expected here }
              stmtnoexpected, { Statement number expected here }
              stmtnotfound, { No such statement in this procedure }
              stringexpected, { Character string expected here }
              subscripttoohigh, { Array subscript too large }
              subscripttoolow, { Array subscript too small }
              toomanywatches, { Too many watched variables }
              variableexpected, { Variable name expected here }
              watchlimitreached, { Size limit reached for watched variable }
              watchnotfound); { Unable to find watched variable data }

  { Symbol Table Access }

  formcachepointer = ^formcacherecord;
  formcacherecord =
    record
      next: formcachepointer; { pointer to next record }
      mdl: mdl_pointer; { program unit }
      index: symbolindex; { index }
      rec: debugrecord; { form being cached }
    end;

  { Statement history }

  historyindex = 1..maxhistory;

  historyentry = array [historyindex] of addressrec;

  { Command Line Tokens }

  tokentype = (ident, uparrow, plus, minus, star, slash, divtok, modtok,
               nottok, andtok, ortok, intok, iftok, thentok, elsetok, at, dot,
               dotdot, comma, semicolon, colon, becomes, lpar, rpar, lbrack,
               rbrack, integerconst, realconst, stringconst, lessthan,
               greaterthan, lessequal, greaterequal, equal, notequal, endmark,
               unknown);

  tokenitem =
    record
      case typ: tokentype of
        ident: (identchars: symbolname);
        integerconst:
          (i: integer;
           extendedint: boolean);
        realconst: (treal: dbstring);
        stringconst:
          (s: dbstring;
           len: integer)
    end;

  predeftypes = (firstpredef, ploophole, pchr, pord, pref, psigblock, 
                 psigmask, pnone);

  { Macros }

  macropointer = ^macro;
  macro =
    record
      next: macropointer; { next macro }
      cmds: commandpointer; { stored commands }
      name: symbolname; { macro name }
    end;

  {  List of modules with files opened. }

  plastmdlopened = ^lastmdlopened;
  lastmdlopened =
    record
      next: plastmdlopened; { next record }
      mdl: mdl_pointer;
    end;

var
  inp, out: text; {don't move these}
  interrupted: boolean; { interrupt detected while running }

  { Program state descriptors }

  firsttime: boolean; { program has not yet been executed }
  paused: boolean; { the program has paused awaiting user input }
  programdead: boolean; { program execution cannot be continued }
  exited: boolean; { program has exited execution }
  term: boolean; { program has terminated }
  runtimeerror: boolean; { an exeception has been detected }

  { Current actions being taken on the program }

  stepping: boolean; { in a single-stepping mode }
  proceeding: boolean; { in a proceeding mode }
  history: boolean; { history is activated }
  tracing: boolean; { in a tracing mode }
  watching: boolean; { at least one watched variable is active }
  quitting: boolean; { quit the debugging session }
  restarting: boolean; { restart the program }
  stepcount: integer; { number of single steps to proceed }
  steplevel: integer; { the dynamic level at start of single-step }

  { The program state as reported by the target supervisor }

  currentpc: addressrec; {current pc of the program}
  previouspc: addressrec; {pc of previous statement}
  watchfound: watchidtype; { set of watched variables that have been modified}
  breakpointhit: boolean; {has program encoutered a breakpoint}
  watchpointhit: boolean; {has a watched variable changed value}

  globaldatabase: addressrec; {base address for the default global data area}

  { Interpreter descriptors }

  prompting: boolean; { Prompting the user for commands }
  errorhappened: boolean; { An error occurred during parsing }
  framesupdated: boolean; {is the stackframelist up-to-date}
  currentcontext: stackpointer; { the current stack frame }

  {  Module list }

  mdl_list: mdl_pointer; { head of the modules list }
  main_mdl: mdl_pointer; { module containing the main block }

  { Breakpoints list }

  breaklist: breakpointer; { head of the breakpoints list }

  { Watched variables list }

  watchlist: watchpointer; { head of the watched variables list }
  watchidset: watchidtype; { set of currently active watched variables }

  { Macros list }

  macrolist: macropointer; { list of macros }

  { History list }

  historylist: historyentry; { statement history list }
  historyidx: historyindex; { current index in the history list }
  historytag: historyindex; 
                 { start index in history list of the current history }
  historycount: 0..maxhistory; { count of valid statement history records }

  { Stack frame list }

  currentstackframe: stackpointer; { current active frame }
  globalstackframe: stackpointer; { frame of the main block }

  { Quick stack frame list }

  baselevelframe: framelistptr; {pointer to level 1 "quick" frame}
  framelistlevel: integer; {number of active frames}

  { Data token list }

  datatokenpool: datatokenptr; { Pointer to pool used for for memory
                                maintenance}
  datapacketfreelist: datapacketptr;  { pointer to data packet pool }

  { Proc tree record pool }

  proctreerecpool: proctreeptr;

  { Current step parameters record }

  steprec: steprecord;

  { Symbol Table Access }

  formcache: formcachepointer; { form cache }
  lasttmpindex: integer; { index counter for temporary forms }

  { Command Input }

  prompt: char; { input prompt }
  eol: char; { end-of-line character (constant) }
  eob: char; { end-of-buffer character (constant) }
  convertingcase: boolean;
  commands: commandpointer; { list of active command lines }
  interactivecommands: { interactive command line }
  commandpointer;
  conditionalcommands: commandpointer; { receptacle for resulting commands of
                                        a conditional command }
  execargs: stringdescr; { arguments to executable file (for separate process)
                          }

  { Tokens }

  token: tokenitem; { the current token }
  predefname: array [predeftypes] of symbolname; {predefined symbols}
  resvname: array [divtok..elsetok] of symbolname; {reserved symbols}

  { Module list maintenance }

  first_lmo, last_lmo: plastmdlopened; { ptrs to LastUnitOpened queue }
  num_mdls_open: integer; { number of modules with open files }
  max_mdls_open: integer; { max number of modules allowed to have files open
                           at a time }
  maxfiles: integer; { max # of files allowed to be open open }
  first_mdl_loaded: boolean; { unit info from files loaded for first unit }

  stacksegment: unsignedword; { segment containing the stack }
  datasegment: unsignedword; { segment containing the global data }

  { The procedure tree file - used if needcaching is true }

  proctreefile: proctreefiletype;
  proctreefilemax: integer;  {the current number of records kept in the file}

 
  execname: pstringdescr; { name of executable file }
  testflg: boolean;
  includes: pargvaluelist; { directories to search for symbol/map files }

  active: boolean; { sub-process is active }
  terminated: boolean; { sub-process has terminated }
  unexpectedsignal: boolean; { an unexpected signal was encountered }

  everfpcoprocessor: boolean;  {was fp co-processor used in any module; 
				used for desk calculator emulation}
