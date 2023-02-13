{[l-,b+]*******************************************************}
{                                                              }
{                    P a s c a l - I I                         }
{                                                              }
{**************************************************************}

{ NOTICE OF COPYRIGHT AND OWNERSHIP OF CONFIDENTIAL SOFTWARE:
  Copyright 1977, 1978, 1979, 1980, 1981, 1982, 1985 by Oregon Software, Inc.
  ALL RIGHTS RESERVED.

  This computer program is the proprietary property of Oregon
  Software, Inc. of Portland, Oregon, U.S.A., and may be used
  and copied only as specifically permitted under written
  license agreement signed by Oregon Software, Inc.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Configuration parameters for the Versados native compiler. 

  Release version: 0045  Level: 1  Date: 21-Nov-1990 15:15:48 
  Processor: MC68000 

  Last modified by KRIS on 21-Nov-1990 15:15:48

   Purpose:
  Update release version for PC-VV0-GS0 at 2.3.0.1

}



unit config;

{ Configuration parameters for this particular host and target combination.
}


interface

{ Definition of possible host/target operating systems and machine
  architectures.  These definitions are used to conditionalized
  routines in the front end for each machine.
}

type

  shortint = integer; {DRB fpc defaults to 8 bits}
  integer = longint; {DRB fpc defaults to 16 bits}

  languages = (pascal, c, modula2, fortran77);
  architectures = (pdp11, vax, iapx86, mc68000, ns32k, i80386);
  opsystems = (vms, rsx, rsts, rt, unix, msdos, vdos, cpp, apollo);
  unixflavors = (NoHost, UniPlus, UniPlusIII, UniPlusV, Masscomp, Xenix,
                 Tandy, Sun, HP9300, Munix, Regulus, Wicat, CandD, Perpos, Ctix, Nti,
                 Venix, UniFlex, VMEV2, UniPlusV2, Ncr, LMI, gmf, domainix,
                 pcix, inix86, inix286, MSxenix, umax, atxenix, ultrix,
                 morebsd, NEC, uspare5, uspare6, last_host);

{ get configuration parameters for this particular host and
  target combination.  'config' defines such things as wordlength,
  unit size (byte or word), whether or not disk cache is needed,
  etc.
}

const
  bigcompilerversion = true; {runs in fewer passes if 'true', but is bigger}
  scanalys = true; {phases scan and analys run together if 'true'}
  travcode = true; {phases travrs and code run together if 'true'}
  hostmachine = iapx86;
  targetmachine = mc68000;
  hostopsys = unix;
  targetopsys = vdos;

  unixtarget = nohost;
  defunixtarget = nohost;
  oldproctable = true; {we are using old-style procedure table}
  language = pascal;
  newdebugger = false;
  register_return = false;
  newscaninterface = false; {we are using old-style string table and tokens}
  newtravrsinterface = false; {we are using old-style proctable and
                               case table interface}
{ Machine dependent parameters
  The machine dependent parameters here apply to more than one pass.
  There are also parameters in analys and scan which will have to be
  modified when the compiler is moved
}

  reversebytes = true; {if target/host byte-order is backwards}
  hostintlowbytefirst = true; {host integers have low order byte
                               at lowest address}
  packinghightolow = true; {the first field to be packed goes in the
                            high order of the integer containing it}
  modworks = false; {true if host mod operator always returns positive number
                     as per the standard. Hashing algorithms in
                     scan:identifier and travrs:hash save one instruction if
                     this is true}

  unitsize = 1; {size of an addressing unit}
  wordsize = 4; {size of a word in addressing units}
  machinesize = 4; {size of a natural unit in addressing units}
  singlesize = 4; {size of a real in addressing units}
  doublesize = 8; {size of a double real in addressing units}

  longdoubsize = 8; {size of a long real (C)}
  defaulttargetrealsize = singlesize; {default real size is single precision}
  defaulttargetintsize = 4; {target integer size in HOST addressing units}
  shorttargetintsize = 2; {16 bit integer size in TARGET addressing units}
  defaultptrsize = 4; {size of a pointer in addressing units}
  farptrsize = 4; {no difference for the 68k}
  longptrsize = defaultptrsize; {only different on the iapx86}
  longsize = 4; {size of a target long}

  hostintsize = 4;  {host integer size in HOST addressing units}
  maxintarray = 1; {number of integral host integers per target integer}

  maxaddr = $FFFFFFFF; { 32 bits is max value of address on M68020 }
  defaulttargetmaxint = $7FFFFFFF; { max value for integer on M68000 }
  defaulttargetminint = $80000000; { min value for an integer on the M68000 }
  shortmaxint = $7FFF; {Max value for a short integer}
  shortminint = $FFFF8000; {min value for 16 bit integer}
  maxusbyte = $FF; {maximum value for an 8-bit byte unit}
  maxusword = $FFFF; {Max value for a 16-bit value}
  maxusint = $FFFFFFFF; { max unsigned integer for this implementation }
  maxlong = defaulttargetmaxint; {max value in a long}
  maxautoindex = 3; {Highest power of two used with indexed mode.
                     Procedure "onearrayindex" in body uses a zero for 68000}
  maxexpon = + 39; {max exponent which can possibly fit in a real}
  minexpon = - 39; {min exponent which can possibly fit in a real}
  maxdoubleexpon = + 309; {max exponent which can fit in a double prec. real}
  mindoubleexpon = - 309; {min exponent which can fit in a double prec. real}
  niladdressvalue = 1; {numeric value of NIL}
  maxword = shortmaxint; {maximum value for a 16-bit signed word}
  minword = - maxword; {minimum value for a 16-bit signed word}

  hostfileunits = 1; {host computer file units per addressing unit}
  maxrealwords = 4; {number of words in a double precision real number}
  hostfilelim = 255; {max integer in host file unit}

  maxlevel = 15; { Max procedure nesting level }

  { NOTE: these are set up for using the old-style string table.  The
    new style uses new to create blocks of chars on the heap.  The
    correct figures would be:  stringblockmax = ???, stringtablesize = 200000,
    stringspan = 1023.
  }
  stringtablesize = 200000; {max characters in all identifiers}
  stringblocksmax = 0; {max blocks -1 for string table}
  stringspan = stringtablesize; {bytes per block - 1 in string table}

  hashtablesize = 12497; { max number of distinct identifiers }
  hashspan = 255; {hash entries per block - 1 in hash table}
  hashblocksmax = 19; {max blocks - 1 for hash table}
  proctablespan = 0; {proc entries per block of proctable}
  procblocksmax = 0; {max blocks in proctable}

  { Proctablesize should be set to the max number of procedures,
    irregardless of new/old-style proctable.  Old_proctablesize
    should be set 0 if new-style proctable used, proctablesize
    if old-style.
  }
  proctablesize = 1023; { max number of procedures }
  old_proctablesize = proctablesize; { max number in old-style proctable}
  vartablesize = 4096; {max number of external and static refs}
  cseregions = 0 {proctablesize}; {max number of cse regions saved}
  undeftablesize = proctablesize; { max number of forward procedures/functions }
  maxswitchvalue = 127; { i.e. LISTALL = 127 $LIST comments }
  switchtablesize = 200; { max number of switches allowed in program }
  errortablesize = 200; { max number of errors before we give up hope }
  listtablesize = 200; { max number of listing control switches }
  maxprocnamelen = 15; {max characters in assembly proc name}
  maxnamestring = 16; {max characters in a name string}

  { Use/define stuff. }
  maxvarentries = 63; {max number of entries less one in an external var block}
  maxvarptrs = 32; {number of pointers in vartable}

  defdatasection = 15; { default data section (used by csi) }

  maxdebugrecord = 14; {max number-1 of debug records in a diskblock}
  debugfileptrmax = 999; {max number-1 of debug file pointers}
  debugfilemax = 15000; {max number of debug records}

  maxstmtrecord = 31; {max number-1 of stmt records in a diskblock}
  stmtfileptrmax = 999; {max number-1 of stmt file pointers}
  stmtfilemax = 32000; {max number of stmt records}

  { Define the maximum numbers of scalars and reals to assign to registers }

  assignreg = 3;
  assignptrreg = 2;
  assignrealreg = 3;  { 68020/68881 only }

  maxtrackvar = 30; {maximum number of register candidates for a block}
  regtablelimit = 31; {last entry in register candidate hash table. MUST be
                       greater than maxtrackvar. Also hashing works fastest
                       if this is 2**N -1 type of number}

  { Define the register mask, used by the Unix debugger }

  maxgenregistermask = 7;
  maxptrregistermask = 7;
  maxrealregistermask = 7;

  suffixlength = 4; {max length of a filename suffix}
  maxextension = 4; {max length of a default extension}
  cmdlinelength = 132; {max length of a command line}
  filenamelen = 100; {max length of a filename}

  sourcedepth = 8; {source + 7 levels of include}
  linelen = 160; {max length of input line}
  list_line_width = 132; {max width of a list output line}
  max_string_len = linelen;

  exitstatus = $10000004; {2^28+4 severe error, no message: VMS exit value}
  diskbufsize = 511; { number of bytes in a disk buffer }
  doublediskbuflimit = diskbufsize; {size of cache blocks}
  worddiskbufsize = 255; { number of words in a disk buffer }
  keysize = 500; { number of concurrent active nodes in travrs/code tree}

  realfolding = false; {turns on real folding}
  rangetracking = true; {turns on range tracking}
  maxparambytes = maxaddr {defaultptrsize}; {value params longer than this are
                                   passed by reference rather than copied}
  optimizeinvariants = true; {turns on removal of invariants from loops}


  {disk cache parameters}

  {Virtual memory sizing constants}
  {these are all HOST, not target dependent}

  { Three possibilities exist:

    1. bigcompiler = false, needcaching = true.

       This is the "wimp computer" case.  Data structures are false, and
       the various maxnodeinblock and lowblock constants must be hand
       configured so that each block fits in a disk block and that global
       space is most efficiently used.

    2. bigcompiler = false, needcaching = false.

       Caching is not done, but the structure is maintained and dynamically
       extended as needed.  Basically a compromise, for systems which don't
       support an extremely large address space.  "memory overflow" may
       occur before the entire structure is filled and maximum program size
       may be limited by available memory space.

       In this case, blocksin structure is still used but each block does
       not need to fit in a disk block.

    3. bigcompiler = true, needcaching = false.

       Caching is not done, and read/writeaccess calls are replaced by
       use of the "ref" function directly into a large array.  Used for
       systems with lots of (usually virtual) memory.  FAST!
  }

  needcaching = false;

  maxstringblks = 400; {max number of string blocks}
  tablesize = 16383; {max number of types and names}
  tnodetablesize = 12000; {maximum nodes in tree}
  cnodetablesize = 16383; {max number of nodes per block}

  { Number of nodes per virtual memory block for each pass:  These
    numbers are defined by the size of tableentry (hdra), node (hdrt),
    and node (hdrc) if needcaching is true.  If needcaching is false,
    they are arbitrary, so we make them some 2^n-1 for fast code.
    If bigcompilerversion is true, however, we only use one block,
    and the value can be anything that's big enough. }

  { The following stuff is meaningful only if bigcompiler = false, and
    should be "1" otherwise.
  }


  analysmaxnodeinblock = 1 {63};
  travrsmaxnodeinblock = 1 {256};
  codemaxnodeinblock = 1 {259};

  {number of static virtual memory blocks for each pass}

  lowanalysblocks = 1 {9};
  lowtravrsblocks = 1 {18};
  lowcodeblocks = 1;

  {make these "2" if bigcompiler, calculate otherwise}

  amaxblocksin = 2 {131}; { max number of blocks in heap+maxblockslow }
  tmaxblocksin = 2 {64}; {max blocks allocated to node file buffers}
  cmaxblocksin = 2 {127}; {max blocks ever allowed}

  { These three constants are meaningful only if bigcompiler = true.
    Make them minimal otherwise.
  }

  bigtablesize = tablesize; {minimum for this one is 1}
  bigtnodetablesize = tnodetablesize; {minimum for this one is 1}
  bigcnodetablesize = cnodetablesize; {minimum for this one is 4}

{ Blocking definitions for environment files }

  tableentriesperblock = 16;
  proctableentriesperblock = 15;
  hashtableentriesperblock = 72; {based on 6 bytes per entry}
  switchesperblock = 101; {based on two bytes per entry}

{ Scan parameters }

{DRB  fastread = true;}
  fastread = false;   {use the fast library routine p_rdsfst}
  stringroundoff = 2; {string length alignment constant}

{ Analys parameters }

  charsetsize = 256; {number of elements in the character set}
  maxsetord = 255; {max ord allowed in a set}
  setvaluebytes = 31; {maximum set size (bytes), for set values}
  setalign = 2; {alignment requirement for a set}
  intalign = 2; {alignment for an integer}
  shortintalign = 2; {alignment for a short integer}

  charsize = unitsize; {size of a character}
  charalign = unitsize; {alignment for a character}

  longintsize = 4; {size of largest integer}
  shortintsize = 2; {size of a medium size integer}

  scalarsize = unitsize; {size of a small scalar}
  scalaralign = unitsize; {alignment for small scalars}
  realalign = 2; {alignment of real values}
  doublealign = 2; {alignment of double real values}
  ptralign = 2; {alignment requirements for a pointer}
  defreturnlinksize = 8; {size of a procedure return link in addressing units
                         (includes dynamic link on 68k)}
  defextreturnlinksize = 8; {size of a procedure return link in addressing
                            units (includes dynamic link on 68k) for externals}
  procparamsize = 8; {size of a procedure parameter in addressing units}
  staticlinkoffset = 4; {position of static link in saved registers area}
  stackalign = 2; {alignment requirement for the stack}
  bitsperunit = 8; {bits per address unit}
  bitsperfileunit = 8; {bits per file allocation unit}
  packingunit = 2; {maximum size used for packing}
  maxbit = 15; {One less than "bits-per-word"}
  maxalign = 16; {largest alignment value possible}
  pdpalign = 2; {pdp-11 compatible alignment value}
  stringeltsize = 8; {bits per string element}
  stringalign = 2; {alignment for a string}

  max_bitfield = 32; {max size of a bitfield (C)}
  unsignedprefered = false; {prefer to unpack unsigned fields}
  freemodwithdiv = true; {true if some flavor of div returns a remainder}

{ Virtual memory sizing constants }
{ These are all HOST, not target dependent }

  apanicspace = 500; { if memory gets this low, ouch! }
  arequiredspace = 2500; { minimum of 2500 bytes of stack }
  aexcessivespace = 3000; { but we need no more than this }

{ symbol table size parameters for analys }

  magicblocksize = 32; { see 'decreasebuffers' for an explanation }
  totalscopes = 2047; {max number of scopes allowed in whole program}
  deadscope = totalscopes; {max scope id, indicates not accessable}

  oprnddepth = 32; {size of operand stack (arbitrary)}
  fordepth = 32; {size of for stack, number of nested for loops}

{ Travrs parameters }

{ The following values are used determine the costs of alternative
  case constructs.  For example, they are used to decide when to
  implement the case as a jump table and when to implement it as tests
  and branches.  You can determine them on the basis of time, space, or
  some combination thereof.  If space is the criterion, the size of the
  instructions required in addressing units are the obvious values.
}

  splitcost = 7; {The average cost of a compare and jump}
  errdefaultcost = 4; {The cost of an error trap}
  casedefaultcost = 4; {The cost of a jump to the default case}
  casetablecost = 22; {overhead cost of a case jump table}
  caselabcost = 2; {cost of each label in a jump table}
  contextdepth = 20; {maximum number of nested contexts}
  maxwithdepth = 32; {maximum number of nexted "withed" records}

{ Virtual memory sizing constants }
{ These are all HOST, not target dependent }

  trequiredspace = 3500; {required stack space in bytes}
  texcessivespace = 4000; {if this much space, create more node buffers}
  tpanicspace = 500; {if only 500 bytes, we are indeed in trouble}

  nodehashsize = 67; {number of buckets in node hash table}

  maxrefcount = 1023; {max number of refs to a node}
  maxcost = 127; {max cost value considered}

  divtarget = - 2; {result is a future dividend}
  multarget = - 1; {result is a future multiplicand}

{ Code parameters }

  {The following constants size the pass and the max block size for compiling}

  labeltablesize = 1000; {max number of labels per block}
  lowesttemp = - 20; {lowest temp key which can be allocated}

{ Virtual memory sizing constants }
{ These are all HOST, not target dependent }

  crequiredspace = 2000; {min stack + heap space after virtual mem. (words)}
  oktouse = 1000; {amount of remaining space we can use for branch chains}

  {predefined data stored off gp.  Used to communicate with support
   library.
  }

  stacklimit = 0; {used for stack overflow checking}
  filevar = 4; {current file stored here for support lib i/o calls}
  restorestack = 8; {set to initial value of stack}
  supportlibdata = 12; {points to support lib private data}
  globalbase = 16; {offset of first global variable}

  {Entry points for read and write support routines.  Name is P$nn.}
  {note:  these use default files, entry + 2 is specified file}

  rdln = 16; {readln for a text file}
  rdstg = 12; {read a string from a text file}
  rdnm = 4; {read an integer from a text file}
  rdch = 0; {read a character from a text file}
  rdrl = 8; {read a real number from a text file}
  rddr = 11; {read a double real number for a text file}
  wrln = 36; {writeln for a text file}
  wrnm = 24; {write an integer to a text file}
  wrch = 20; {write a character to a text file}
  wrrl = 28; {write a real number to a text file}
  wrdr = 29; {write a double real number to a text file}
  wrstrg = 32; {write a string to a text file}
  wrbo = 110; {write a boolean variable to a text file}
{ constant definitions to support the PutObj module:
}

  firstESD = 17; { first available External Symbol Definition index }
  lastESD = 256; { this value is 1 greater than the maximum assignable index
                  so that nextESD will never be out of ESDrange }
  {note: a load module can have at most 238 external definitions/references! }

  oursection = 13; { section in which Pascal code resides }
  diagsection = 14; { section for diagnostic code }
  linknamesize = 10; { max length of an external name }
  linknameused = 10; { length of external names actually used }
  maxtempbuffer = 32; { number of words in tempbuffer }
  maxrelfile = 255; { number of words in relfilebuffer }

  checkmsg = ' consistency checks detected'; { output only if internal errors
                                              }

type
{ unsignedint = unsigned (extended) integer on TARGET
  addressrange = HOST dependent definition (watch out for variant records!)
  targetint = signed integer on TARGET
  targetaddress = address on TARGET
}

  {unsignedint = 0..maxusint; {range for unsigned values}
  addressrange = 0..maxaddr; {supported addressing space (bytes)}
  targetaddress = addressrange;
  unsignedword = 0..maxusword;}

  {DRB for free pascal}
  unsignedint = longword;
  addressrange = longword;
  targetaddress = longword;
  unsignedword = word;

  targetint = integer;

  realarray = packed array [1..maxrealwords] of unsignedword;

  alignmentrange = 0..maxalign; {possible alignment values}

implementation

end.
