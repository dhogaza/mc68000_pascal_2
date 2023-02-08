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

  Pascal-2 Compiler Syntax/Semantic Analysis Global Declarations

 Last modified by KRIS on 21-Nov-1990 15:18:54
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

unit hdra;

interface

uses config, hdr, scan, a_t;

const

  {Analys pass sizing parameters}

  { where hardware vm is used, blockslow is made as small as possible }

  maxblockslow = lowanalysblocks; { number of blocks in low-space }
  entriesperblock = analysmaxnodeinblock; {entries per physical file block -
                                           1}


  const

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

  { Must be in the same place in analys/travrs }

  toklengths: tokenlengthtable;
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

  implementation
  end.
