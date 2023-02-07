{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Syntax/Semantic Analysis Common Routines

 Last modified by KRIS on 21-Nov-1990 15:18:37
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

unit comma;

interface

uses config, hdr, utils, at, scan;

const

  {Analys pass sizing parameters}

  { where hardware vm is used, blockslow is made as small as possible }

  maxblockslow = lowanalysblocks; { number of blocks in low-space }
  entriesperblock = analysmaxnodeinblock; {entries per physical file block -
                                           1}
  targetzero = 0; {target machine value of zero}
  targetone = 1; {target machine value of one}

type
  tokenlengthtable = array [tokentype] of 0..10; {defines token lengths}

  alignmentrange = 0..maxalign; {possible alignment values}

  {the following type has moved to hdr.pas}
  {addressrange = 0..maxaddr;} {possible address values}

  bitrange = 0..maxbit; {possible bit indices within a word}

  { Structures to describe types and labels}

  entryptr = ^tableentry;
  index = 0..tablesize;
  scoperange = 0..deadscope;
  totalscoperange = 0..totalscopes;

  labelptr = ^labelentry;

  labelentry =
    packed record
      labelvalue: pascallabelrange; {user supplied value for label}
      internalvalue: labelrange; {corresponding internal value}
      nextlabel: labelptr; {previous label defined in this block}
      maxlegalnest: integer; {used for error checking}
      definednest: integer; {nesting level, 0 if not yet defined}
      labelline: integer; {line of label decl (for error)}
      labelcolumn: columnindex; {column of label decl (for error)}
      nonlocalref: boolean; {true if ref'd by non-local goto}
    end;

  setvalueblock = set of 0..maxsetord; {holds set const}

  range =
    record
      minlimit, maxlimit: integer; {limits of values}
      extended: boolean; {maxlimit is > maxint}
    end;

  operand_range =
    record
      optimistic: range; {assumes all vars in range}
      pessimistic: range; {assumes any value allowed by storage}
    end;

  const_descr =
    record {describes a constant value}
      case representation: types of {values for different types}
        ints:
          (intvalue: integer; {ord(value)}
           negated: boolean {value was negated} );
        ptrs: (ptrvalue: integer {targetptr} {referenced operand} );
        reals, doubles:
          (realvalue:
             record {actual real value}
               case boolean of
                 false: (realbinary: double); {computable}
                 true: (realbuffer: realarray); {just storage}
             end);
        arrays, fields, strings:
          (pos, len: integer; {location in string file}
           stringconstflag: boolean {true if 'string'} );
        sets: (setvalue: ^setvalueblock);
    end; {const_descr}

  operandtype = (constoperand, varoperand, exproperand);

  operand =
    record {describes expression operand}
      typeindex: index; {type of this operand}
      oprndlen: addressrange; {length of operand in units}
      value_range: operand_range; {range of values for the operand}
      extended: boolean; {set if thiy is an extended range}
      case operandkind: operandtype of {true if constant operand}
        constoperand: (cvalue: const_descr; {value of the constant} );
        varoperand, exproperand:
          (cost: integer {estimated registers to compute operand value} )
    end;

  tableentry =
    packed record {symbol table entry}
      dbgsymbol: p_symbolindex; {debugger form (PDB) or table (ODB) index}
      case form: boolean of
        true:
          (size: addressrange; {size of type in units or bits}
           align: alignmentrange; {alignmentrequirements}
           packedflag: boolean; {true if user said "packed"}
           bitaddress: boolean; {true if size is bits, not units}
           containsfile: boolean; {true if type has a file component}
           extendedrange: boolean; {true if upper bound > maxint}
           disposable: boolean; {true if type can be quickly disposed of}
           case typ: types of
             subranges:
               (lowerord, upperord: integer; {user said lowerord..upperord}
                parentform: types; {"typ" value of the parent}
                parenttype: index {type of lowerord, upperord} );
             fields:
               (fieldid: integer; {scope id for symbol table search}
                nextvariant: index; {next variant record at this level}
                firstlabel: index; {head of label chain describing this
                                    variant}
                firstvariant: index; {first subvariant defined by case at
                                      level}
                tagfield: index; {name entry of tagfield, 0 if none}
                firstfield: index; {index of first field in symbol table}
                lastfield: index {index of last field in record} );
             variantlabs:
               (nextvarlab: index; {points to next label for this variant}
                varlabtype: index; {type of variant label}
                varlabvalue: integer {ord(variant value)} );
             arrays, conformantarrays, strings:
               (indextype: index; { user said array [indextype] }
                elementtype: index; { of elementtype }
                highbound, lowbound: index; {bound id's if conformant}
                stringtype: boolean; { set if a string type }
                arraymembers: addressrange; {members in the indextype}
                elementsize: addressrange {effective size of element} );
             sets:
               (constructedset: boolean; {declared or [i,j..k] ?}
                basetype: index {user said set of basetype} );
             files:
               (filebasetype: index; {user defined file of filebasetype}
                filekey: integer {used to identify file componants to travrs}
               );
             ptrs:
               (ptrtypename: index; {user defined ^ptrtypename}
                ptrkey: integer {used to identify ptr components to travrs} );
             scalars: (lastord: integer {highest value of scalar} ); );
        false:
          (nextname: index; {pointer to previous incarnation of same name}
           name: scoperange; {id of block in which declared}
           charlen: 0..linelen; {length of character string defining name}
           charindex: integer; {start of name in string file}
           lastoccurrence: totalscoperange; {last scope which accessed this
                                             unit}
           case namekind: nametype of
             typename, undeftypename:
               (typeindex: index; {points to defined type}
                refdefined: boolean {true if defined by ref function} );
             constname:
               (consttype: index; {type for this constant}
                constvalue: const_descr {value for this constant} );
             varname, fieldname, param, varparam, procparam, funcparam,
             confparam, varconfparam, boundid, undefname:
               (modified: boolean; {becomes true when value assigned}
                nestedmod: boolean; {true if modified by nested procedure}
                parammodified: boolean; {becomes true when param value changes}
                knownvalid: boolean; {set when value is known to be in range}
                programdecl: boolean; {set when declared in program header}
                varianttag: boolean; {set true if record variant tag}
                registercandidate: boolean; { true if travrs can assign to a
                                             reg}
                univparam: boolean; {true if universal parameter}
                lastinsection: boolean; {last in a parameter section}
                varalloc: allockind; {kind of allocation made}
                nextparamlink: index; {if parameter, points to next param in
                                       list}
                offset: unsignedint; {address of item within block}
                length: addressrange; {length of item}
                vartype: index; {name's type}
                sparelink: index; {for boundid, array for which it's a bound;
                                   for use/shared vars index into vartable;
                                   for fields used to pass info to debugger} );
             standardproc, standardfunc, directivename:
               (procid: standardids {which standard procedure} );
             procname, funcname, forwardproc, forwardfunc, externalproc,
             externalfunc:
               (functype: index; {function result type}
                funclen: addressrange; {length of resulting value}
                paramlist: index; {Index of last parameter}
                funcassigned: boolean; {true if function value defined}
                procref: proctableindex; {index into global proc data for
                                          routine}
                savedparamsize: addressrange {size of parameters} ); );
    end;

  undefindex = 0..undeftablesize;

  tokenset = set of tokentype;
  typeset = set of types;

  {The following define the virtual memory for the name table}

  nameblock =
    record
      case boolean of
        false: (physical: doublediskblock);
        true:
          (logical: array [0..entriesperblock] of tableentry; {one symbol table
             block} );
    end;

  nameblockptr = ^nameblock;

  blockindex = - 1..amaxblocksin;

  blockmap =
    record {Used to map block number into buffer pointer}
      blkno: blockindex; { index for seek }
      written: boolean; {set if block has been modified}
      lowblock: boolean; {set if buffer not on heap}
      buffer: nameblockptr {points to block if exists, otherwise nil}
    end;

  undefentry =
    record {used to record forward routines and type definitions}
      tableref: index; {symbol table entry for name}
      line: integer; {Source line on which occurs}
      column: columnindex {source column at which it occurs}
    end;

  blocktype = (codeblock, withblock); {types of scope}

  displayentry =
    record {used to describe a scope level}
      dbgscope: p_symbolindex; {ODB sym file entry for this scope}
      blockid: scoperange; {unique id for name searching}
      scopeid: totalscoperange; {monotonic increasing block count for scope
                                 checking}
      case blockkind: blocktype of
        withblock:
          (withoffset: addressrange; {offset of with variable}
           withpacking: boolean; {this is a packed type}
           withlevel: levelindex {level of with variable} );
        codeblock:
          (blocksize: addressrange; {size of local variables for block}
           paramsize: addressrange; {size of parameters for block}
           firststmt, {for debugger support}
           laststmt : integer; {values returned by stf_stmt}
           blockref: proctableindex; {global procedure table index}
           labellist: labelptr; {all labels defined at this level}
           oldtabletop: index; {top of table at entry, for restore at end}
           threshold: index; {last name on entry, for restore at end}
           blockname: index; {name entry for the block}
           oldundeftabletop: undefindex; {to restore undef table at end}
           namesdeclared: hashindex; {number of names defined in this block}
           highestkey: hashindex; {key of highest name declared} )
    end;

  intstates = (opstate, stmtstate); {intermediate code is statement or expr}

  forstackindex = 0..fordepth;

  debughashtabletype = array [debughashindex] of targetint;

 { declarations for kluged type to allow writing to environment file }
  proctableblock = array [0..proctableentriesperblock] of proctableentry;
  tableblock = array [0..tableentriesperblock] of tableentry;

  envirtype = (en_analys_var_block, en_keytable_block, en_proctable_block,
	       en_symboltable_block, en_disk_block);
  envirrecord =
    record
      case envirtype of
	en_analys_var_block:
	 (eproctabletop: proctableindex;
          eundeftabletop: undefindex;
	  elastfilekey: integer;
	  eanyexternals: boolean;
	  elastid: 0..deadscope;
	  elastscope: 0..totalscopes;
	  etabletop: index;
	  edisplay0, edisplay1: displayentry;
	  eblockref: proctableindex;
	  elevel: levelindex;
	  edisplaytop: levelindex;
          elastdebugrecord: integer;
          elastprocrecord: integer;
	  enullboundindex, eintindex, esubrangeindex, erealindex, edoubleindex,
	   echartypeindex, eboolindex, etextindex, enoneindex,
	   enilindex: index;
	  econsttablelimit: integer;
          estringfilebase: integer;
          etargetrealsize: integer;
          etargetintsize: integer;
          etargetmaxint: integer;
          etargetminint: integer;
          eptrsize: integer;
          eglobalsize: addressrange;
          eownsize: addressrange;
          edefinesize: addressrange;
          eanyfile: boolean;
          einputoffset: index;
          edebughashtable: debughashtabletype;
          enilvalue: operand;
          einputindex: index;
          eoutputindex: index );
	  en_keytable_block:
          (ekeyblock: packed array [0..worddiskbufsize] of shortint);
	en_proctable_block: (eproctable: proctableblock);
	en_symboltable_block: (esymboltable: tableblock);
	en_disk_block: (ediskblock: diskblock);
    end;

var

  nooverflow: integer; {kludge for lib$int_over(on/off)}
  glboverflow, glbov: boolean; {needed for function return}
  legalfunctypes: typeset; {types which a function can return}
  neverskipset, { These tokens are NEVER skipped by parser }
   begconstset, { Legal tokens which start a signed constant }
   blockheadset, { Begblockset - [beginsym] }
   begblockset, { Legal tokens which start a block }
   begparamhdr, { Legal tokens which start a param }
   nextparamhdr, { Legal tokens which start next param }
   begstmtset, { Legal tokens which start a stmt }
   begunsignedset, { Legal tokens which start unsigned consts }
   begsimplset, { Legal tokens which begin simple types }
   begstructset, { Legal tokens which start a structured type }
   begtypset, { Legal tokens which start a type def }
   begfactset, { Legal tokens which start a factor }
   constfollowset, { Tokens which may follow a constant decl }
   typefollowset, { Tokens which may follow a type decl }
   begexprset, { Legal and illegal tokens which start an expression }
   exprops, { Expression operators (relational ops) }
   sexprops, { Simple expression operators (adding ops) }
   termops: tokenset { Term operators (multiplying ops) } ;

  {token records bracketing the current, having several helps error recovery}
  thistoken, lasttoken: tokenrecord;

  token: tokentype; {current token}
  tokenbufindex: 0..diskbufsize; {current token file entry}

  dbgsourceindex: unsignedint;
  sourcestringindex: unsignedint; {pos in stringtable of source file name}

  display: array [levelindex] of displayentry; {compile time display}

  {the following are entries for standard types}
  intindex, shortintindex, realindex, doubleindex, chartypeindex,
    boolindex, noneindex, nilindex, textindex, inputindex, outputindex,
    subrangeindex: index;

  nullboundindex: index; {undefined boundid}

  emptysetgenerated: boolean; {true if '[]' already emitted}
  emptysetcount: integer; {where it is, if it is}
  inputdeclared, outputdeclared: boolean; {true if declared in program stmt}

  optable: array [eql..andsym] of operatortype; {maps tokens into operators}

  oprndstk: array [0..oprnddepth] of operand; {stack for expression
                                               evaluation}
  sp: - 1..oprnddepth; {top of operand stack}

  keymap: array [hashindex] of index; {Index into symboltable by name}

  stringfilebase: integer; {top of stringfile when we enter analys}

  undeftable: array [undefindex] of undefentry; {forward reference table}

  debughashtable: debughashtabletype;
  lastdebugrecord: integer; {last record written in debugger file}
  lastprocrecord: integer; {last procedure record written in debugger file}
  lastfilekey: integer; {used to generate unique ids for file and ptr types}
  tabletop: index; {last entry in symboltable}
  undeftabletop: undefindex; {last entry in forward def table}
  displaytop: levelindex; {top of display stack}
  labelflag: labelptr; {used to mark end of form and label lists}

  lastid: scoperange; {last named scope created}
  lastscope: totalscoperange; {last marker for scope checking}
  level: levelindex; { current block level }
  lev: levelindex; { Returned by search -- level of item found }

  tokencount: integer; {count of tokens read}

  nilvalue: operand; {value of reserved word nil}

  intstate: intstates; {state of intermediate file, operator or statement}
  emitflag: boolean; {set if intfile to be emitted, reset on error}
  checkundefs: boolean; {set if valid to check for undef var usage}
  nolabelsofar: boolean; {set if no labels encountered yet}
  anyfile: boolean; {used in record parsing to see if contains file}
  anyexternals: boolean; {set if any externals in entire compilation unit}
  anynonlocallabels: boolean; {set if any non-local labels in this block}
  nextintcode: 0..diskbufsize; {intfile buffer pointer}
  paramlistid: integer; {scope id for last param list}
  nowdebugging: boolean; {current block has debugging code}

  {The following are used by the constant folding routines}

  quoflag: boolean; {true if div is for a quotient operation, not rem}
  divfolded: boolean; {tells "remop" or "quoop" folding that "divop" folded}
  divide_extended: boolean; {divide left operand was extended}
  divide_range: operand_range; {range of left operand of a div}
  linearize: boolean; {true if constants folding into array base addr}
  linearfactor: integer; {saves const from genbinary to array index}
  skipfactor: boolean; {true sez factor already read when expression called}

  varindex: index; {index of latest variable parsed}
  varptr: entryptr; {pointer to name entry for varindex}
  resulttype: index; {type of current operation being parsed}
  resultptr: entryptr; {pointer to type block of resulttype}
  resultform: types; {form for resulttype}
  result_range: operand_range; {range for current operation}

  forstack: array [forstackindex] of
      record
        containedgoto: boolean; {true says for loop contained goto statement}
        forindex: index; {controlled vars for for loops}
        forrange: range;
      end;

  forsp: forstackindex; {top of forstack}

  loopfactor: integer; {non-zero if within loop}

  {The following are used by genunary and genbinary}
  foldedunary: boolean; {set if unary operation successfully folded}
  oconst: boolean; {set if unary op operand is constant}
  ocost, olen: integer; {operation cost and result length for unary op}
  oextended: boolean; {set if the operation is extended range}
  lconst, rconst: boolean; {left or right operand constant for binary op}
  foldedbinary: boolean; {binary folding attempt was successful}
  l, r: 0..oprnddepth; {operand indices for folding binary op}
  c1, c2, newcost: integer; {used for computing costs of binary op}
  newlen: addressrange; {result length for binary operation}
  unaryform, binaryform: types; {operation result types for unary, binary op}
  unaryop, binaryop: operatortype; {op being generated for genunary, genbinary}

  nest: integer; {statement nesting depth for goto checking}
  jumpoutnest: integer; {outermost nesting level for jumps out of for loops}
  probing: boolean; {set if tentative probe of symbol table, not real usage}

  anynonpascalcalls: boolean; {set if block contains any non-pascal calls}

  fewestblocks, mostblocks: 0..amaxblocksin; {monitor virt mem scheme}

{ Gross kludge to enable placement of structured constants in analys
  overlay: }

  structfollow: tokenset; {tokens which can follow structured constant}
  structtype: index; {type of structured constant}
  structvalue: operand; {the returned value}
  tempvars: integer; {number of locals available for register assignment}

  lastblocksin: 1..amaxblocksin; {last block actually allocated}
  thrashing: boolean; {set true when sym table spills onto disk}
  bigtable: array [0..bigtablesize] of tableentry; {symboltable if
                                                    bigcompiler}
  blocksin: array [1..amaxblocksin] of blockmap; {name blocks in memory}
  blockslow: array [1..maxblockslow] of nameblock;

procedure adecreasebuffers;

procedure aincreasebuffers;

procedure gettoken;

procedure warnbefore(err: warning {Error message number} );

{ Generate an error message at the center of the last token.
}

procedure warnbetween(err: warning {Error message number} );

{ Generate an error message half way between the last token and the
  current token.
}

procedure warn(err: warning {Error message number} );

{ Generate an error message in the middle of the current token.
}

procedure warnnonstandard(err: warning {Error message number} );

{ Generate a warning only if the standard switch is set.
  Used to warn of non-standard features.
}

procedure fatal(err: warning {Error message number} );

{ Generate a fatal warning which will terminate the compilation.
}

procedure putbyte(a: integer {value of byte to access} );

{ Write the byte "a" to the next location in the string file.  This
  is assumed to be added to the constant table, and the global
  "consttablelimit" is incremented as a result.
}

procedure genform(f: types {form to emit} );

{ If no errors found so far, emit a form to the intermediate file.
}


procedure genint(i: integer {value to emit} );

{ If no errors found so far, emit an integer value to the intermediate file.
  Since each intermediate file element is only in the range 0..255 (one byte),
  multiple elements are used.

  Note that only unsigned integers are emitted.
}


procedure genop(o: operatortype {operator to emit} );

{ If no errors are found so far, emit an operator to the intermediate file.
}


procedure genstmt(s: stmttype {statement to emit} );

{ If no errors are found so far, emit a statement to the intermediate file.
}

procedure verify(set1: tokenset; {acceptable tokens}
                 set2: tokenset; {tokens to finish skip}
                 err: warning {Message if token not ok} );

{ Check that the current token is in set1, emit an error message
  "err" if not, and skip until the token is in set1 + set2 +
  neverskipset.  The error message will be placed as close
  to the estimated location of the token as possible.
}

procedure verify1(set1: tokenset; {acceptable tokens}
                  err: warning {message if token not ok} );

{ Same as verify, except no separate skip set is provided.
}

procedure verifytoken(tok: tokentype; {acceptable token}
                      err: warning {message if token not ok} );

{ Check for a given token (tok) and skip it if found.  If not
  found, emit an error message set by "err".  This is used for
  redundant tokens in the syntax, where parsing can continue if it
  is missing.
}

implementation

procedure warnbefore (err: warning {Error message number} ) ;

{ Generate an error message at the center of the last token.
}


  begin {warnbefore}
    emitflag := false;
    with lasttoken do warnat(err, line, (left + right) div 2)
  end {warnbefore} ;


procedure warnbetween(err: warning {Error message number} );

{ Generate an error message half way between the last token and the
  current token.
}


  begin {warnbetween}
    emitflag := false;
    with lasttoken do
      if line = thistoken.line then
        warnat(err, line, (right + thistoken.left + 1) div 2)
      else warnat(err, line, min(right + 1, linelen))
  end {warnbetween} ;


procedure warn(err: warning {Error message number} );

{ Generate an error message in the middle of the current token.
}


  begin {warn}
    emitflag := false;
    with thistoken do warnat(err, line, (left + right) div 2)
  end {warn} ;


procedure warnnonstandard(err: warning {Error message number} );

{ Generate a warning only if the standard switch is set.
  Used to warn of non-standard features.
}


  begin {warnnonstandard}
    if (switchcounters[standard] > 0) then warn(err)
  end {warnnonstandard} ;


procedure fatal(err: warning {Error message number} );

{ Generate a fatal warning which will terminate the compilation.
}


  begin {fatal}
    fatalflag := true;
    warn(err);
  end {fatal} ;

{ Virtual Memory System }

{ Virtual Memory System for Instruction Nodes.
  These routines implement the virtual memory tree used for instructions.
  Nodes are stored on the file "nodefile" in blocks.  A maximum of
  "amaxblocksin" of these blocks are kept in buffers in main memory.
  The array "blocksin" is used to keep track of the blocks in memory,
  and to implement a least recently used update strategy.

  When a node is requested, the list of blocks in memory is searched,
  and if the node is in one of them that block is moved to the head
  of the list (blocksin[1]).  All blocks between the former position
  and the head of the list are moved one position toward the foot of
  the list.  If the block is not already in memory, the last block on the
  list is made available by writing its contents to the file (if they have
  been modified), and it is moved to the start of the list and loaded
  from the file.  Finally a pointer to the desired node is generated
  using the "ref" function.  Of course, if the block is not in memory and
  there are less than "amaxblocksin" blocks already in memory, a new
  buffer is created without writing an old one.

  Since measurements indicated that this is a major time consumer
  in this pass, the mechanism is effectively bypassed if the blocks
  will all fit in core.  In this case,  there is a direct mapping
  from index to buffers in "blocksin".  Once any buffer has been
  written (indicated by the global "thrashing"), the system reverts
  to the search of blocksin to locate a buffer.

  At the point where thrashing becomes true,  all blocks are
  assumed to be written.
}

procedure areadaccess{i: index; (node wanted)
                      var p: entryptr (provides access to desired node ) } ;

{ Make virtual element "i" available for read-only access
  and return a pointer to it.

}

  begin {areadaccess}
    if bigcompilerversion then writeln('areadaccess called!');
  end {areadaccess} ;


procedure awriteaccess {i: index; (node to access)
                       var p: entryptr (provides access to the node) } ;

{ Make virtual element "i" available for access and modification, and
  return a pointer to it.

}

  begin {awriteaccess}
    if bigcompilerversion then write('awriteaccess called!');
  end {awriteaccess} ;


procedure adecreasebuffers;

{ If available memory is running low relinquish a cache buffer.
}

  begin {adecreasebuffers}
  end {adecreasebuffers} ;


procedure aincreasebuffers;

{ Add a new virtual buffer if space has gotten large due to the
  dispose routine.
}

  begin {aincreasebuffers}
  end {aincreasebuffers} ;

procedure gettoken;

{ Read next lexical token

  This is one of the most popular procedures in ANALYS.  What it
  does is read the "next" token.  Several global level variables
  are altered by this routine:
      thistoken -- Receives the "next" token from the token file
      lasttoken -- Receives previous value of thistoken
      token     -- Receives tokentype of thistoken
      nexttoken -- Receives tokentype of what will be the next
                   token read from the token file.  This provides
                   a one token "look ahead" for ANALYS.

  If scanalys is true, SCAN's scantoken routine is
  called directly, otherwise tempfileone is read.
}


  begin {gettoken}
    lasttoken := thistoken;
    thistoken := nexttoken;
    token := thistoken.token;
    if token <> eofsym then scantoken;
  end {gettoken} ;



{ String File Processing

  The string file contains constant data collected by scan and analys.
  It is organized in blocks, each consisting of:

    array [0..diskbufsize] of byte

  The string file is always accessed as stringblkptr^[nextstringfile];

  The following routines manipulate this file.
}

procedure putbyte(a: integer {value of byte to access});

{ Write the byte "a" to the next location in the string file.  This
  is assumed to be added to the constant table, and the global
  "consttablelimit" or "stringfilecount" is incremented as a result.
}


  begin {putbyte}
    stringfilecount := stringfilecount + 1;
    stringblkptr^[nextstringfile] := a;
    putstringfile;
  end {putbyte} ;



{ Intermediate File Output

  DRB not true for FPC
  The intermediate file is the interface to the next pass (travrs),
  and consists of blocks of undiscriminated variant records.  The
  type of the next record is determined strictly by context.

  The following routines write the different kinds of values to this
  file.

  As with all files in the compiler, this one is blocked, and the next
  element is always accessed as:

    tempfiletwo^[nextintcode]

  The global flag "emitflag", which is set false if errors are detected,
  controls the actual emission of output to this file.
}

procedure genform (f: types {form to emit});

{ If no errors found so far, emit a form to the intermediate file.
}


  begin {genform}
    if emitflag then
      begin
      tempfilebuf.intcode := form;
      tempfilebuf.f := f;
      write(tempfiletwo, tempfilebuf);
      end;
  end {genform} ;


procedure genint(i: integer {value to emit} );

{ If no errors found so far, emit an integer value to the intermediate file.
  Since each intermediate file element is only in the range 0..255 (one byte),
  multiple elements are used.

  Note that only unsigned integers are emitted.
}

  var
    { This fudges an integer into bytes.  The constant "32" is }
    { simply a large enough number to include all probable systems. }
    fudge:
      record
        case boolean of
          true: (int: integer);
          false: (byte: packed array [1..32] of hostfilebyte);
      end;
    j: 1..32; {induction var}


  begin {genint}
    if emitflag then
      tempfilebuf.intcode := literal;
      if (i >= 0) and (i < hostfilelim) then
        begin
        tempfilebuf.b := i;
        write(tempfiletwo, tempfilebuf);
        end
      else
        begin
        tempfilebuf.b := hostfilelim;
        write(tempfiletwo, tempfilebuf);;
        fudge.int := i;
        for j := 1 to hostintsize * hostfileunits do
          begin
          tempfilebuf.b := fudge.byte[j];
          write(tempfiletwo, tempfilebuf);;
          end;
        end;
  end {genint} ;


procedure genop (o: operatortype {operator to emit} );

{ If no errors are found so far, emit an operator to the intermediate file.
}


  begin {genop}
    if emitflag then
      begin
      tempfilebuf.intcode := op;
      tempfilebuf.o := o;
      write(tempfiletwo, tempfilebuf);
      end;
  end {genop} ;


procedure genstmt (s: stmttype {statement to emit} );

{ If no errors are found so far, emit a statement to the intermediate file.
}


  begin {genstmt}
    if emitflag then
      begin
      if intstate = opstate then
        begin
        genop(endexpr);
        intstate := stmtstate
        end;
      tempfilebuf.intcode := stmt;
      tempfilebuf.s := s;
      write(tempfiletwo, tempfilebuf);
      end
  end {genstmt} ;




procedure verify(set1: tokenset; {acceptable tokens}
                 set2: tokenset; {tokens to finish skip}
                 err: warning {Message if token not ok} );

{ Check that the current token is in set1, emit an error message
  "err" if not, and skip until the token is in set1 + set2 +
  neverskipset.  The error message will be placed as close
  to the estimated location of the token as possible.
}

  var
    skipset: tokenset; {Tokens which end skipping}


  begin {verify}
    if not (token in set1) then
      begin
      skipset := set1 + set2 + neverskipset;
      if token in skipset then warnbetween(err)
      else
        begin
        warn(err);
        while not (token in skipset) do gettoken
        end
      end
  end {verify} ;


procedure verify1(set1: tokenset; {acceptable tokens}
                  err: warning {message if token not ok} );

{ Same as verify, except no separate skip set is provided.
}


  begin {verify1}
    verify(set1, [], err);
  end {verify1} ;


procedure verifytoken(tok: tokentype; {acceptable token}
                      err: warning {message if token not ok} );

{ Check for a given token (tok) and skip it if found.  If not
  found, emit an error message set by "err".  This is used for
  redundant tokens in the syntax, where parsing can continue if it
  is missing.
}


  begin {verifytoken}
    if token = tok then gettoken
    else warnbetween(err)
  end {verifytoken} ;




procedure enterform
                   {newtyp: types; (type for this form)
                    var where: index; (new entry)
                    var whereptr: entryptr (for access to new entry) } ;

{ Enter a new formentry at the current level.  This also
  gets a dbgsymbol for use with the debugger, and sets the type
  to be newtyp.
}


  begin {enterform}
    if tabletop = tablesize then fatal(tablefull)
    else tabletop := tabletop + 1;
    where := tabletop;
    if bigcompilerversion then whereptr := ref(bigtable[tabletop])
    else awriteaccess(tabletop, whereptr);
    with whereptr^ do
      begin
      if not newdebugger and (newtyp in
         [subranges, scalars, fields, arrays, sets, files, ptrs, ints, bools,
         chars, reals, doubles, conformantarrays, strings]) then
        begin
        lastdebugrecord := lastdebugrecord + 1;
        dbgsymbol := lastdebugrecord;
        end
      else dbgsymbol := 0;
      form := true;
      typ := newtyp;
      containsfile := false;
      packedflag := false;
      bitaddress := false;
      extendedrange := false;
      disposable := false;
      end;
  end {enterform} ;




procedure searchsection
                       {id: integer; (scope id for search)
                        var wherefound: index (resulting name index) } ;

{ Search the symbol table for the current token identifier in scope
  "id".  If found, the name index will be placed in "wherefound".  If not
  found, zero will be returned. If the identifier found proves to be
  a constant or type identifier we will update "lastoccurrence" within
  the symbol table entry to allow enforcement of scope restrictions.
}

  var
    p: entryptr; {used for name table access}
    twherefound: index; {temp for wherefound during procedure}


  begin {searchsection}
    twherefound := keymap[thistoken.key];
    if bigcompilerversion then p := ref(bigtable[twherefound])
    else areadaccess(twherefound, p);
    while (twherefound <> 0) and (p^.name <> id) do
      begin
      twherefound := p^.nextname;
      if bigcompilerversion then p := ref(bigtable[twherefound])
      else areadaccess(twherefound, p);
      end;
    if not probing and (twherefound <> 0) and
       (p^.lastoccurrence < display[displaytop].scopeid) and
       (p^.namekind in
       [procname, funcname, constname, typename, standardproc, standardfunc, undeftypename, fieldname, undefname]) then
      begin
      if not bigcompilerversion then blocksin[1].written := true;
      p^.lastoccurrence := display[displaytop].scopeid;
      end;
    wherefound := twherefound;
  end {searchsection} ;




procedure searchlsection
                        {value1: integer; (label value)
                         labellist: labelptr; (root of label list)
                         var wherefound: labelptr (result of search) } ;

{ Search a list of labels starting at "labellist" for a label with the
  value "value".  The result is returned in "wherefound".  If the label
  is not in the list, the returned entry will be "labelflag".

  "Labelflag" is set to the desired value to simplify the search algorithm.
}


  begin {searchlsection}
    labelflag^.labelvalue := value1;
    wherefound := labellist;
    while wherefound^.labelvalue <> value1 do
      wherefound := wherefound^.nextlabel;
  end {searchlsection} ;


procedure searchlabels
                      {value1: integer; (label value)
                       var wherefound: labelptr (result of search ) } ;

{ Search all available scopes for a label with "value", returning the
  result in "wherefound."  The result will be set to "labelflag"
  if the label cannot be found.
}

  var
    i: levelindex; {induction var for level search}


  begin {searchlabels}
    i := level;
    repeat
      searchlsection(value1, display[i].labellist, wherefound);
      i := i - 1;
    until (i = 0) or (wherefound <> labelflag);
    lev := i + 1;
  end {searchlabels} ;


procedure search {var wherefound: index (result of search) } ;

{ Search all available scopes for the current token.  The result is
  returned in "wherefound", with zero indicating no find.  The global
  variable "lev" is set to the level where the token was found.
}

  var
    i: levelindex; {induction var for level search}
    t: index; {temp result of search for each level}


  begin {search}
    i := displaytop + 1;
    repeat
      i := i - 1;
      searchsection(display[i].blockid, t);
    until (i = 0) or (t <> 0);
    lev := i;
    wherefound := t;
  end {search} ;


procedure searchvariants
                        {var currentrecord: index; (record to search)
                         labvalue: operand (varnt label value) } ;

{ Search a record variant part starting at "currentrecord" for a
  variant with a label of "labvalue" and set "currentrecord" to that
  variant.  If there is no variant with the desired label,
  "currentrecord" is unmodified, and an error message is emitted.
}

  var
    t: index; {used to trace variant chain}
    t1: index; {used to trace label chain}
    t2: index; {holds last value of t for later use}
    ptr, ptr1: entryptr; {used to access variant and label chains}
    found: boolean; {set if label found, controls search}


  begin {searchvariants}
    found := false;
    if bigcompilerversion then ptr := ref(bigtable[currentrecord])
    else areadaccess(currentrecord, ptr);
    t := ptr^.firstvariant;
    while (t <> 0) and not found do
      begin
      if bigcompilerversion then ptr := ref(bigtable[t])
      else areadaccess(t, ptr);
      with ptr^ do
        begin
        t2 := t;
        t := nextvariant;
        t1 := firstlabel;
        while (t1 <> 0) and not found do
          begin
          if bigcompilerversion then ptr1 := ref(bigtable[t1])
          else areadaccess(t1, ptr1);
          with ptr1^ do
            begin
            found := (labvalue.cvalue.intvalue = varlabvalue);
            t1 := nextvarlab;
            end;
          end;
        end;
      end;
    if found then currentrecord := t2
    else warnbefore(badtagerr);
  end {searchvariants} ;



{ Utilities for use with types}


procedure stripsubrange {var objectindex: index (form to be stripped) } ;

{ Convert a subrange type to the base type for use in an expression.
}

  var
    ptr: entryptr;


  begin {stripsubrange}
    if bigcompilerversion then ptr := ref(bigtable[objectindex])
    else areadaccess(objectindex, ptr);
    with ptr^ do if (typ = subranges) then objectindex := parenttype;
  end {stripsubrange} ;


function lower {f: entryptr (form to check) } {: integer} ;

{ Returns the lower bound of "f".  This is meaningful only for
  scalar types.
}


  begin {lower}
    with f^ do
      if typ = ints then lower := targetminint
      else if typ = subranges then lower := lowerord
      else lower := 0;
  end {lower} ;


function upper {f: entryptr (form to check) } {: integer} ;

{ Returns the upper bound of "f".  This is meaningful only for
  scalar types.
}


  begin {upper}
    with f^ do
      case typ of
        ints: upper := targetmaxint;
        bools: upper := 1;
        chars: upper := charsetsize - 1;
        none: upper := 255;
        scalars: upper := lastord;
        subranges: upper := upperord;
        otherwise upper := targetmaxint
        end
  end {upper} ;




function bits {i: integer (value to find size of) } {: integer} ;

{ Returns the number of bits needed to contain the value of i.
}

  var
    b: integer; {Accumulates number of bits}
    value: unsignedint; {Temp so can use a register and shift inst}


  begin {bits}
    if i < 0 then bits := targetintsize * bitsperunit
    else
      begin
      value := i;
      b := 1;
      while value > 1 do
        begin
        b := b + 1;
        value := value div 2;
        end;
      bits := b;
      end;
  end {bits} ;


function sizeof
               {f: entryptr; (Form to get size of)
                packedresult: boolean (set if packed value) }
 {: addressrange} ;

{ Returns the amount of storage needed to contain a value of the type
  specified by "f".  If "packedresult" is set, this is in bits, otherwise
  it is in addressing units.
}

  var
    lowerf: integer; { temp holding lower(f) }
    magnitude: addressrange; {absolute value of max number of bits}


  begin {sizeof}
    if packedresult = f^.bitaddress then sizeof := f^.size
    else if packedresult then
      case f^.typ of
        chars, bools, scalars, subranges, none:
          begin
          if (targetmachine = iapx86) and (f^.size > wordsize) then
            sizeof := defaulttargetintsize * bitsperunit
          else
            begin
            lowerf := lower(f);
            if (lowerf < 0) then
              begin
              magnitude := max(abs(upper(f)), abs(lowerf + 1));
              if magnitude = 0 then sizeof := 1 {handles the case of -1..0}
              else sizeof := bits(magnitude) + 1; {the normal case}
              end
            else sizeof := bits(upper(f));
            end;
          end
        otherwise
          if maxaddr div bitsperunit < f^.size then sizeof := maxaddr
          else sizeof := f^.size * bitsperunit;
        end
    else sizeof := (f^.size + bitsperunit - 1) div bitsperunit;
  end {sizeof} ;




function forcealign
                   {size: addressrange; (value to align)
                    alignment: addressrange; (requirement)
                    packedresult: boolean (size is in bits) }
 {: addressrange} ;

{ Forces "size" to the next higher multiple of "alignment".
  Used to overcome limitations built into much contemporary hardware.
}


  begin {forcealign}
    if packedresult then alignment := alignment * bitsperunit;
    if alignment > 1 then
      size := ((size + alignment - 1) div alignment) * alignment;
    forcealign := size;
  end {forcealign} ;


function unsigned
                 {f: entryptr; (type to check)
                  len: addressrange; (space actually allocated for var)
                  packedelement: boolean (set if packed var) } {: boolean} ;

{ Returns true if the values of type "f" are unsigned.
  If "len" is not equal to the space required for the value, it is being
  allocated a space larger than required, and should be treated as signed
  or unsigned for unpacking, depending on the global "unsignedprefered".
}


  begin {unsigned}
    if not packedelement then len := len * bitsperunit;
    unsigned := not (f^.typ in [subranges, ints, bools, chars, scalars]) or
                (lower(f) >= 0) and (unsignedprefered or (len = sizeof(f,
                true))) or f^.extendedrange;
  end {unsigned} ;


function simplesize {i: integer (value to find size of) } {: integer} ;

{ Returns the size in multiples of addressing units needed to contain
  the value of i.
}

  var
    b: integer; {bits to contain i}
    t: integer; {used to accumulate size in units}


  begin {simplesize}
    b := bits(i);
    t := 1;
    while b > t * bitsperunit do t := t + 1;
    simplesize := t;
  end {simplesize} ;




function negaterealconst {realbuffer: realarray (real constant value) }
 {: realarray} ;

{ Function to negate a real constant, independent of the host
}

  const
    halfword = 32768; {for constant negating}

  var
    signidx: 1..maxrealwords; {index to the signpart}


  begin {negaterealconst}
    case targetmachine of
      vax, pdp11:
        begin
        if realbuffer[1] <> 0 then
          if realbuffer[1] >= halfword then
            realbuffer[1] := realbuffer[1] - halfword
          else realbuffer[1] := realbuffer[1] + halfword;
        end;
      mc68000:
        begin
        if realbuffer[1] >= halfword then
          realbuffer[1] := realbuffer[1] - halfword
        else realbuffer[1] := realbuffer[1] + halfword;
        end;
      iapx86, i80386, ns32k:
        begin
        if switcheverplus[doublereals] then signidx := maxrealwords
        else signidx := maxrealwords div 2;
        if realbuffer[signidx] >= halfword then
          realbuffer[signidx] := realbuffer[signidx] - halfword
        else realbuffer[signidx] := realbuffer[signidx] + halfword;
        end;
      end;
    negaterealconst := realbuffer;
  end {negaterealconst} ;


procedure constant
                  {follow: tokenset; (legal following symbols)
                   dumpvalue: boolean; (true says dump string)
                   var value1: operand (resulting constant) } ;

{ Syntactic routine to parse a constant.

  productions:

  constant = [ sign ] (unsigned-number | constant-identifier) |
         character-string | structured-constant  .

  If the constant is a simple constant, the actual value is returned in
  "value", otherwise a pointer to the string file is returned.  Constant
  structures are added to the "consttable" portion of the string file
  if they are longer than an integer value, otherwise they are returned
  as an integer value.
}

  var
    negate: boolean; {set if neg sign found}
    sign: boolean; {set if any sign found}
    t: index; {temp index for constant identifier}
    p: entryptr; {temp access to constant identifier entry}
    t1: entryptr; {Temp for index type for string constant}
    unsvalue: unsignedint; {temp for unsigned operation}


  begin {constant}
    {init the descriptor}
    with value1, cvalue do
      begin
      typeindex := noneindex;
      operandkind := constoperand;
      representation := ints; {the most common case}
      negated := false;
      intvalue := 0; {this field will be used, even if it is a bad constant}
      extended := false;
      end;
    negate := (token = minus);
    sign := negate or (token = plus);
    if sign then gettoken;
    verify(begunsignedset, follow, badconsterr);
    if token in begunsignedset then
      begin
      if token = nilsym then
        begin
        value1 := nilvalue;
        gettoken
        end
      else if token = ident then
        begin {either constant or constant structure}
        structtype := noneindex;
        search(t);
        if bigcompilerversion then p := ref(bigtable[t])
        else areadaccess(t, p);
        if t = 0 then
          begin
          warn(undefidenterr);
          gettoken;
          end
        else
          with p^ do
            if namekind = constname then
              begin
              value1.typeindex := consttype;
              if bigcompilerversion then t1 := ref(bigtable[consttype])
              else areadaccess(consttype, t1);
              value1.oprndlen := sizeof(t1, false);
              value1.cvalue := constvalue;
              gettoken;
              end
            else if (namekind = typename) then
              begin
              { Note:  this is a kluge to get structconst into an overlay}
              structtype := p^.typeindex;
              structfollow := follow;
              if hostopsys <> msdos then ovrlay(xcstruct)
              else cstruct;
              value1 := structvalue;
              { Note: end of kluge, normally a simple call would suffice}
              end
            else
              begin
              warn(badconsterr);
              gettoken;
              end;
        end
      else {not nil or an identifier}
        with value1, cvalue do
          begin
          case token of
            intconst:
              begin
              typeindex := intindex;
              intvalue := thistoken.intvalue;
              negated := false; {is default}
              oprndlen := targetintsize;
              end;
            realconst:
              begin
              typeindex := realindex;
              representation := reals;
              realvalue.realbuffer := thistoken.realvalue;
              oprndlen := targetrealsize;
              end;
            dblrealconst:
              begin
              typeindex := doubleindex;
              representation := doubles;
              realvalue.realbuffer := thistoken.realvalue;
              oprndlen := doublesize;
              end;
            charconst:
              begin
              typeindex := chartypeindex;
              intvalue := thistoken.intvalue;
              negated := false; {is default!}
              oprndlen := charsize;
              end;
            stringconst:
              begin {Must make type entry for the string}
              representation := arrays;
              stringconstflag := true;
              len := thistoken.len;
              if scanalys and (thistoken.pos < 0) and dumpvalue
              then
                begin
                pos := stringfilecount + 1;
                if scanalys then  { make sure it's deadcoded }
                  dumpstr(len + 1, curstringbuf, true);
                end
              else pos := thistoken.pos;
              enterform(subranges, t, t1);
              with t1^ do
                begin
                size := targetintsize;
                align := intalign;
                parenttype := intindex;
                parentform := ints;
                lowerord := 1;
                upperord := len
                end;
              enterform(arrays, typeindex, t1);
              with t1^ do
                begin
                packedflag := true;
                bitaddress := true;
                containsfile := false;
                elementtype := chartypeindex;
                stringtype := true;
                arraymembers := len;
                indextype := t;
                size := len div (bitsperunit div stringeltsize);
                if len mod (bitsperunit div stringeltsize) <> 0 then
                  size := size + 1;
                oprndlen := size;
                size := size * bitsperunit;
                elementsize := stringeltsize;
                align := stringalign;
                end;
              end
            end {case} ;
          gettoken;
          end;
      with value1, cvalue do
        if sign and (typeindex <> realindex) and (typeindex <> intindex) then
          warn(badconsterr)
        else if negate then
          if typeindex = realindex then
            realvalue.realbuffer := negaterealconst(realvalue.realbuffer)
          else {non real}
            begin
            if intvalue <> - targetmaxint - 1 then intvalue := - intvalue;
            negated := not negated;
            end;
      end;
    with value1, cvalue do
      if representation = ints then
        begin
        unsvalue := intvalue;
        extended := (unsvalue > targetmaxint) and not negated;
        end;
  end {constant} ;




function getform {objecttype: entryptr (desired form) } {: types} ;

{ Get the basic form associated with a type.
}


  begin {getform}
    with objecttype^ do
      if typ = subranges then getform := parentform
      else getform := typ;
  end {getform} ;


function identical {left, right: index (types to compare) } {: boolean} ;

{ True if two types are identical, or if either is undefined (to avoid
  redundant messages).
}


  begin {identical}
    identical := (left = right) or (left = noneindex) or (right = noneindex);
  end {identical} ;


function compatible {left, right: index (types to compare) } {: boolean} ;

{ True if the types represented by the two input forms are compatible
  as defined by the Pascal standard.  If either input is undefined,
  they are assumed to be compatible to eliminate redundant error
  messages.
}

  var
    lptr, rptr: entryptr; { used for access to symbol table }
    c: boolean; {temporary value of compatible}


  begin {compatible}
    stripsubrange(left);
    stripsubrange(right);
    if identical(left, right) then compatible := true
    else
      begin
      compatible := false;
      if bigcompilerversion then lptr := ref(bigtable[left])
      else areadaccess(left, lptr);
      if bigcompilerversion then rptr := ref(bigtable[right])
      else areadaccess(right, rptr);
      if lptr^.typ = rptr^.typ then
        case lptr^.typ of
          strings: compatible := true;
          arrays:
            compatible := lptr^.stringtype and rptr^.stringtype and
                          (lptr^.arraymembers = rptr^.arraymembers);
          sets:
            compatible := compatible(lptr^.basetype, rptr^.basetype) and
                          ((lptr^.packedflag = rptr^.packedflag) or
                          lptr^.constructedset or rptr^.constructedset);
          ptrs:
            if (left = nilindex) or (right = nilindex) then
              compatible := true
            else
              begin {Allow compatibility between pointer types and pointers
                     created with the address operator if the base types are
                     the same. Also forstall error messages if either pointer
                     base type is undef. }
              if bigcompilerversion then
                lptr := ref(bigtable[lptr^.ptrtypename])
              else areadaccess(lptr^.ptrtypename, lptr);
              if bigcompilerversion then
                rptr := ref(bigtable[rptr^.ptrtypename])
              else areadaccess(rptr^.ptrtypename, rptr);

              c := (lptr^.typeindex = noneindex) or
                   (rptr^.typeindex = noneindex);
              if rptr^.refdefined or lptr^.refdefined then
                c := c or compatible(rptr^.typeindex, lptr^.typeindex);
              compatible := c;
              end;
          end;
      end;
  end {compatible} ;




function alignmentof
                    {f: entryptr; (form to check)
                     packedresult: boolean (result is packed) }
 {: alignmentrange} ;

{ Compute the alignment requirement of a type.  This function is needed
  strictly because the alignment of a subrange is kluged to the parent
  type to give better code generation on certain types of machines.  This
  kluge causes trouble with packed types, so is deleted if the result
  is to be used in a packed structure.
}


  begin {alignmentof}
    if packedresult = f^.bitaddress then alignmentof := f^.align
    else if packedresult then alignmentof := f^.align * bitsperunit
    else alignmentof := (f^.align + bitsperunit - 1) div bitsperunit;
  end {alignmentof} ;


procedure seekstringfile {n: integer (byte to access) } ;

{ Do the equivalent of a "seek" on the string file.  This sets the
  file and "nextstringfile" to access byte "n" of the stringfile.
}

  var
    newblock: 1..maxstringblks; { block to which seeking }


  begin {seekstringfile}
    newblock := n div (diskbufsize + 1) + 1;
    if newblock <> curstringblock then
      begin
      if needcaching then
        begin
        if stringfiledirty then put(stringfile);
        stringfiledirty := false;
        seek(stringfile, newblock);
        end
      else
        begin
        stringblkptr := stringblkptrtbl[newblock];
        if stringblkptr = nil then
          begin
          new(stringblkptr);
          stringblkptrtbl[newblock] := stringblkptr;
          end;
        end;
      curstringblock := newblock;
      end;
    nextstringfile := n mod (diskbufsize + 1);
  end {seekstringfile} ;


procedure getstringfile;

{ Do the equivalent of a get on the stringfile.

  The string file contains constant data collected by SCAN and
  ANALYS.  It is organized as blocks containing arrays 0..
  diskbufsize of bytes.  The string file is always accessed
  as 

    stringfile^[nextstringfile]    if caching is enabled

    stringblkptr^[nextstringfile]  if caching is disabled
}


  begin {getstringfile}
    if nextstringfile = diskbufsize then
      begin
      nextstringfile := 0;
      curstringblock := curstringblock + 1;
      if needcaching then get(stringfile)
      else stringblkptr := stringblkptrtbl[curstringblock];
      end
    else nextstringfile := nextstringfile + 1;
  end {getstringfile} ;

function do_hash{charindex:integer; charlen:integer}{ : integer};

{perform a hashing of the entry of length "charlen" at the location "charindex"
 in the stringtable, using the function concealed inside the debugger package.}

  function nextch:char;
  begin {nextch}
    nextch := stringtable^[charindex];
    charindex := charindex + 1
  end {nextch} ;

begin {do_hash}
if newdebugger then do_hash := p_debughash(charlen, nextch);
end {do_hash} ;


