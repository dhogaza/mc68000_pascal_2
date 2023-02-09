{[b+,o=80]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1987, 1988, 1989, 1990 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 

  %W% %G% %U%

  Release version: 0045  Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Tree Processing Global Declarations

 Last modified by KRIS on 21-Nov-1990 15:23:05
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

unit hdrt;

interface

uses config, hdr, a_t, t_c;

const

  pts = proctablespan; {shorter local name}
  debugtree = false;
  maxloopdepth = 10;
  maxexprstack = 100;
  nodesperblock = travrsmaxnodeinblock; {nodes per physical file block - 1}
  maxblockslow = lowtravrsblocks;           
                     {min blocks for node file buffers, allocated statically
                      to use some otherwise unused space in the global area}

type

  actions = (visit, copy, revisit, recopy); { Possible node states }

  { Define all possible Pascal statement header/trailer which exist in tree }

  stmthdrtype = (blkhdr, gotohdr, labelhdr, whilehdr, rpthdr, ifhdr, foruphdr,
                 fordnhdr, withhdr, casehdr, caselabhdr, casegroup, simplehdr,
                 syscallhdr, untilhdr, forbothdr, whilebothdr, cforhdr,
                 cforbothdr, returnhdr, loopbrkhdr, loopconthdr, swbrkhdr,
                 cswbrkhdr, loopbothdr, loophdr, caseerrhdr, nohdr);

  nodeptr = ^node; { Pointer to tree element }
  nodeindex = 0..tnodetablesize; {index to tree element}

  basicblockptr = ^basicblock;
  linkptr = ^links;

  links =
    record
      pre: basicblockptr; { ptr to blocks predeccessor }
      pnext: linkptr; { ptr to next in predecessor chain }
      suc: basicblockptr; { ptr to blocks successor }
      snext: linkptr; { ptr to next in successor chain }
    end;

  oprndindex = 1..3;
  operandarray = array [oprndindex] of integer;
  nodeoperandarray = packed array [oprndindex] of boolean;
  levelarray = packed array [levelindex] of boolean;

  stackindex = 0..maxexprstack; {index to operand stack}
  contextindex = 0..contextdepth; {index to context stack}
  maphashindex = 0..nodehashsize; {index to node hash table}
  reghashindex = 0..regtablelimit; {index to var hash table}

  nodeformtype = (stmtnode, exprnode);
  fonrange = shortint; {range for fon's}
  refcountrange = 0..maxrefcount; {range for refcounts}

  node =
    packed record {describes a node in the expression tree}
      case nodeform: nodeformtype of
        exprnode:
          (refcount: refcountrange; {number of active references to this node}
           copycount: refcountrange; {number of copies mode of this node}
           valid: boolean; {set if node is valid for cse search}
           mustinvalidate: boolean; {invalidate node after next statement}
           join: boolean; {invalidate node at next join}
           target: boolean; {this node is an assignment target}
           relation: boolean; {subnodes contain a relational operator}
           local: boolean; {local to a conditional expression}
           nodeoprnd: nodeoperandarray; {set if oprnds[i] is a link}
           looplink: nodeindex; { link to other vars in this loop/ read or
                                 writes}
           cost: 0..maxcost; {cost of computing this node}
           prelink: nodeindex; { link of nodes that must be hoisted }
           hoistedby: basicblockptr; { this node is hoisted by this block }
           case action: actions of
             visit, revisit:
               (op: operatortype; { Operator from Analys }
                form: types; {type of operands}
                hasvalue: boolean; { true if we know the value of expression }
                invariant: boolean; { true if expression is invariant }
                ownvar: boolean; {true if expression is own variable}
                regcandidate: boolean; {true if var is a register candidate}
                deepestvalid: contextindex; {deepest level where node is valid}
                slink: nodeindex; { Search link -- next node on hash chain }
                len: addressrange; {length of operation result}
                case {isrealnode:} boolean of
                  true: (realvalue: realarray);
                  false:
                    (oprnds: operandarray; {tree nodes or literal integers}
                     value: integer; { value propogated here } ); );
             copy, recopy:
               (oldlink: nodeindex; {link to next outer node being copied}
                directlink: nodeindex {link to parent node being copied} ));
        stmtnode:
          (nextstmt: nodeindex; { Next statement in this basic block }
           textline: integer; { Textline where this statement starts }
           stmtno: integer; { statement counter value when encountered }
           srcfileindx: integer; { codegen access to file name }
           case stmtkind: stmthdrtype of

             blkhdr:
               (procref: proctableindex; { Reference into global proc table }
                bs, ps: addressrange; { Local storage and parameter storage }
                blkbody: nodeindex; { Point to first statement in block }
                blkexit: basicblockptr; {exit block for return}
                fileline: integer {line in actual file where block starts} );

             casehdr:
               (selector: nodeindex; { selector expression }
                elements: nodeindex; { index to caselabhdrs in numeric order}
                joinblock: basicblockptr; {ptr to stmtblock that defines tail}
                firstgroup: nodeindex; {first case group produced by split}
                groupcount: shortint; {number of groups produced by split}
                casedefptr: basicblockptr; {default statement blk, if any} );

             caselabhdr:
               (caselabellow, caselabelhigh: integer; {label values}
                stmtblock: basicblockptr; { ptr to stmt block for this label }
                stmtlabel: labelrange; { label of stmt block }
                orderedlink: nodeindex; {used to link case label table} );

             casegroup:
               (highestlabel, lowestlabel: nodeindex; {limiting entries}
                ordered: boolean; {this is a contiguous ordered group}
                groupno: shortint; {which group this is} );

             forbothdr, whilebothdr, loopbothdr, whilehdr, rpthdr, ifhdr,
             foruphdr, fordnhdr, withhdr, simplehdr, syscallhdr, untilhdr,
             cforbothdr, returnhdr, nohdr:
               (looptop: basicblockptr; { top of for/while }
                expr1, expr2: nodeindex; {Expressions used by statement}
                has_break: boolean; {loop body contains a break}
                trueblock, falseblock: basicblockptr; {blocks for statement}
                forstepsize: integer; { for step for Modula-2 } );

             loopbrkhdr, loopconthdr, swbrkhdr, cswbrkhdr:
               (targblock: basicblockptr; {target block for this statement} );

             gotohdr, labelhdr:
               (labelno: labelrange; {label value or internal label number}
                nonlocalref: boolean; {true if label ref'd by non-local goto}
                labellevel: levelindex; {For non-local gotos} ); );
    end;

{ Note about has_break: when the loop has a break statement, travrs does 
  a clear context at the end of the loop, but does not say anything to
  the back-end; the back end assumes that the loop keeps the active keys
  'alive' which is done by inserting load at the bottom (or top) of the loop.
  The break gives the loop an exit which does not go through the load 
  operations. The current fix if to issue a dummy pascal label at the end
  of such a loop. This leads to poor code... We only need to know if the loop
  has a break statement at the bottom of the loop.
}

  basicblock =
    packed record
  { basic block description }
      dfolist: basicblockptr; { ptr to next block in depth first order }
      rdfolist: basicblockptr; { ptr to next block in reverse dfo }
      successor: linkptr; { direct successor of this block }
      predeccessor: linkptr; { direct predeccessor of this block }
      precode: nodeindex; { copy nodes and hoisted code for this block}
      beginstmt: nodeindex; { link to first statement of this block }
      defmin: fonrange; { lowest definition number in this block}
      blocklabel: labelrange; { internal label number for this block }
      isdead: boolean; { true if block is unreachable }
      visited: boolean; { true if block visited }
      clearop: boolean; { true if clear needed here }
      joinop: boolean; { true if join op needed here }
      saveop: boolean; { true if save op needed here }
      restoreop: boolean; { true if restore op need here }
      dominates: boolean; { true if block dominates loop exit }
      loophdr: boolean; { true if 1st block of a loop }
      forcelabel: boolean; { true if forced to use this label even if empty }
      looplabel: labelrange; { label of code below any hoisted }
      reads: nodeindex; { chain of invariant for this loop }
      writes: nodeindex; { chain of non-invariant for this loop }
      lastwrite: nodeindex; { tail of chain of non-invariant for this loop }
      deadlevels: levelarray; { the dead levels }
      deadloop: boolean; {true if loop so deeply nested that we gave up}
      willexecute: boolean; { true if loop will execute if flow reaches}
    end;

  nodeblock =
    record {Kluge to use the same file for all virtual memories}
      case boolean of
        false: (physical: doublediskblock {physical disk block} );
        true:
          (logical: array [0..nodesperblock] of node {local use of file} );
    end;

  nodeblockptr = ^nodeblock; {used to access disk block buffers}
  blockindex = - 1..tmaxblocksin;

  blockmap =
    record {maps incore buffers to virtual nodes on disk}
      blkno: blockindex; {virtual block number}
      written: boolean; {set if block is modified since read}
      lowblock: boolean; {true if in global core, not disposed of}
      buffer: nodeblockptr; {actual data buffer}
    end;

  stackrecord =
    record {holds operands as the stack is being built}
      context_mark: nodeindex; {last node in this expression}
      relation: boolean; { Node contains a relational op ('=') }
      uniqueoprnd: boolean; { At least one operand is unique -- can't be CSE }
      case litflag: boolean of {literal value?}
        true: (i: integer {actual value} );
        false:
          (p: integer; {operand node}
           l: contextindex {operand context level} )
    end;

  workingnode =
    record {Holds data on node being built}
      op: operatortype; {operator for node}
      form: types; {operator type}
      ownvar: boolean; {true sez own variable}
      len: addressrange; {length of operands}
      cost: shortint; {cost of this node}
      {up to three operands per node}
      case {isrealnode:} boolean of
        true: (rval: realarray; );
        false: (oprndlist: array [oprndindex] of stackrecord; );
    end;

  contextrecord =
    record {context stack element, defines current statement context}
      searchlevel: contextindex; { Lowest level with valid node }
      joinflag: boolean; {must kill the tree at the next join point}
      dominates: boolean; { true if this context dominates exit }
      origlow: keyindex; {dead code kluge to reset low, shouldn't be needed
                          now!}
      firstblock: basicblockptr; { Point to basic block header for this level}
      opmap: array [maphashindex] of nodeindex; {for searching nodes}
      low, high: keyindex; {used to assign keys during walk}
    end;
  region =
    record
      lonmin: nodeindex; { minimum node that refs var }
      lonmax: nodeindex; { maximum node that refs var }
      fonmin: fonrange; { flow order minimum value }
      fonmax: fonrange; { flow order maximum value }
    end;

  regalloctype = (genreg, ptrreg, realreg, bytereg); { register allocation type
                                                      }

  simplevars =
    record
      offset: addressrange; { offset of local var }
      size: addressrange; {size of variable}
      regkind: regalloctype; { register allocation type }
      regid: 0..255; { register number assigned }
      registercandidate: boolean; { true if not object of pushaddr or ref }
      parameter: boolean; {true if this is a parameter}
      debugrecord: integer; { for fixing debug symbol file }
      varlife: region; { lifetime region }
      worth: shortint; { value of this var }
    end;

var

  truelabel, falselabel: labelrange; {for short circuit boolean evaluaton}
  trueused, falseused: boolean; {set if truelabel, falselabel used in eval}

  lasttravrslabel: labelrange; {last label used}

  emitpseudo: boolean; {1-op delay due to pseudobuff/pseudoinst interface}

  root: basicblockptr; {root block for the current procedure}
  tail: basicblockptr; {tail block for the current procedure}

  regtemps: shortint; {number of int vars actually reg allocated}
  ptrtemps: shortint; {number of ptr vars actually reg allocated}
  realtemps: shortint; {number of real vars actually reg allocated}

  level: levelindex; { Lexical level of current procedure being worked over }

  intlevelrefs: boolean; {set if proc makes intermediate level references}

  lastnode: - 1..tnodetablesize; {last node allocated}
  laststmtnode: - 1..tnodetablesize; {last node for stmt}

  stack: array [stackindex] of stackrecord; {expression stack for building}
  sp: stackindex; {top of expression stack}

  cond_depth: stackindex; {count of nested conditional operators}

  newvarcount: shortint; {number of newvarop's in this tree.}

  forstack: array [0..fordepth] of
      record {describes for loop controlled variable}
  { build side }
        forref: nodeindex; {node with variable}
        forlevel: contextindex; {context level of this loop}
        literalvalue: integer; { value of induction var for 1 trip loop }
        { walk side }
        forkey1: keyindex; { key for expr1 }
        forkey2: keyindex; { key for expr1 }
        improved: boolean; { true if test at bottom }
        downloop: boolean; { true if downto loop }
        {onetrip: boolean;} { true if a one trip only loop }
      end;
  forsp: 0..fordepth; {top of forstack}

  withstack: array [1..maxwithdepth] of
      record {describes with variables}
        withref: nodeindex; {node for withed variable}
        withlevel: contextindex; {context for this with level}
        withfonmin: fonrange;  {for extending lifetime of with expr vars}
        withfoncount: fonrange; {ditto}
      end;
  withsp: 0..maxwithdepth; {top of with stack}

  keytable: array [keyindex] of nodeindex; {keys for active nodes in walk}

  context: array [contextindex] of contextrecord; {statement context stack}
  contextsp: contextindex; {top of context stack}
  overflowdepth: shortint; {counter for nesting > contextdepth}

  shorteval: boolean; {set if short-circuit eval of and/or/not wanted}
  inverted: boolean; {true if odd number of "nots" in short circuit eval}
  nowdebugging: boolean; {true if this block should generate debugging code}
  nowwalking: boolean; {true if this block should generate walkback code}
  killinput: boolean; {if true, read/readln without file name occured}
  irreducible: boolean; {this function has irreducible code}

  symbolrecord: integer; {symbol file record for this block (debugging)}

  nextintcode: 0..diskbufsize; {next entry in int file block}
  nextpseudofile: 0..diskbufsize; {next entry in pseudo file block}

  final_block_size: addressrange; {size of block as known at end of proc}

  laststmt: integer; {statement number of last statement generated(debugging)}
  controlstmt: boolean; {statement is control point (debugging)}
  branchstmt: boolean; {statement is non-local goto (debugging)}
  targetstmt: boolean; {statement is target of non-local goto (debugging)}

  fewestblocks, mostblocks: 0..tmaxblocksin; {monitor virtual buffers}

  map: packed array [operatortype, types] of pseudoop; {maps analys operators onto
                                                        pseudoops}

  castmap: packed array [types, types] of pseudoop; {casts from form to form}

  walkdepth: shortint; {depth of progress into tree}

  visitstate: boolean; { state of "visited" blocks on current search }
  regvars: array [0..regtablelimit] of simplevars; { register var hash table}
  foncount: fonrange; { flow order number }
  loopfactor: shortint; { value assigned to localworth of variables }
  loopdepth: 0..maxloopdepth; { current loop depth }
  loopoverflow: shortint; { loopoverflow value }
  loopstack: array [0..maxloopdepth] of
      record
        reads: nodeindex; { linked chain of vars only read in this loop }
        writes: nodeindex; { linked chain of vars modified in this loop }
        lastwrite: nodeindex; { tail of chain of vars modified in this loop }
        loopblock: basicblockptr; { index of loop header }
        deadlevels: levelarray; { the dead levels }
        fonmin: fonrange; { foncount at start }
      end;
  blockblocks: shortint; { temp for debug }
  hoistone: shortint; { count of 1st level hoists }
  hoisttwo: shortint; { count of 2nd level hoists }
  ptr: nodeptr; { for access to basic block node }
  needjump: boolean; { true if this block needs a jump to it's successor }
  oktolabel: boolean; { true if current basic block can be labelled }
  oktoclear: boolean; {ok to clear keys (used in walk)}
  labelnext: boolean; { true if next basic block can be labelled }
  lnk, lnk2: linkptr; { for disposing succ/pred links }
  currentblock: basicblockptr; { basic block we are walking }
  maxnodes: integer;

  localparamnode: nodeindex; {points to node describing local param levop}
  thrashing: boolean; {set true when virt mem overflows onto disk}
  lastblocksin: 1..tmaxblocksin; {actual number of node blocks}
  blocksin: array [1..tmaxblocksin] of blockmap; {map of node blocks in memory}
  blockslow: array [1..maxblockslow] of nodeblock; {fixed blocks in store}
  bignodetable: array [0..bigtnodetablesize] of node;
  exit_stmtno: integer; {save stmtno of the exit fake statement (VMS)}
 { the following are only a convenience when using pdb (setting watch point
   on a heap based variable, ...)
 }
  dbgbasicblockptr: basicblockptr;
  dbgbasicblockptr1: basicblockptr;
  dbgnodeptr: nodeptr;
  dbgnodeptr1: nodeptr;
  dbglinkptr: linkptr;
  dbglinkptr1: linkptr;

implementation
end.
