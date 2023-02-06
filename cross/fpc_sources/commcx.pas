{[b+,l+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984
  by Oregon Software, Inc.
  All Rights Reserved.

  Whether this program is copied in whole or in part and whether this
  program is copied in original or in modified form, ALL COPIES OF THIS
  PROGRAM MUST DISPLAY THIS NOTICE OF COPYRIGHT AND OWNERSHIP IN FULL.

  Release version: 0045  Level: 1
  Processor: ~processor~
  System: ~system~
  Flavor: ~flavor~
  Pascal-2 external declarations for module "commc.pas".
 Last modified by KRIS on 21-Nov-1990 15:28:25
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1
}

{ Virtual Memory System for Instruction Nodes.
  These routines implement the virtual memory tree used for instructions.
  Nodes are stored on the file "nodefile" in blocks.  A maximum of
  "cmaxblocksin" of these blocks are kept in buffers in main memory.
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
  there are less than "cmaxblocksin" blocks already in memory, a new
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

procedure accessblk(blockwanted: integer {index of desired block} );
  external;

{ Make the block with index "blockwanted" available at the top of
  "blocksin".  If not already in core, the contents of the file
  block with that index are read in.  If the least recently used block
  must be written ("written" set and no more blocks allowed in core),
  it is written to the appropriate place in the file.

  This procedure is used locally from the virtual memory package.

  Although the code within "accessblk" will work with cmaxblocksin
  equal to 1, the compiler code assumes that cmaxblocksin >= 2, and
  will not work without this condition being met.
}


procedure creadaccess(i: nodeindex; {node wanted}
                     var p: nodeptr {provides access to desired node} );
  external;

{ Make virtual element "i" available for read-only access
  and return a pointer to it.

  The element must not be changed, as it will not be written
  back to the file after use.  Note that the element is only
  to be accessable for (cmaxblocksin-1) calls to "creadaccess"
  or "cwriteaccess".

  Note: There are places in the code where it is assumed that
  the most recently accessed block is in blocksin[1], or that
  we are not yet thrashing.
}


procedure cwriteaccess(i: nodeindex; {node to access}
                      var p: nodeptr {provides access to the node} );
  external;

{ Make virtual element "i" available for access and modification, and
  return a pointer to it.

  The block in which the element resides is marked as modified, so that
  any changes will be written to the file if the block is no longer
  in memory.  Note that access is only guaranteed for (cmaxblocksin-1)
  calls to "creadaccess" or "cwriteaccess".
}


function getvartableptr(i: integer {index in vartable}): vartablerecptr;
  external;

{ Returns a pointer to the ith vartable entry.
}


procedure getpseudobuff;
  external;

{ Get and unpack the next element in the pseudofile, leaving the result
  in "pseudobuff".
}


procedure unpackpseudofile;
  external;

{ Get the next pseudo-instruction in "pseudobuf" and distribute some
  of the fields into other global variables.
}


procedure getreal(var rval: realarray);
  external;

{ Get a real value from the pseudo instructions.
}


procedure realtoints(rval: realarray; { value to convert }
                     len: unsigned; { length desired }
                     var i1, i2: integer { resulting integers } );
  external;

{ Convert a real number to integers.
}


procedure newnode;
  external;

{ Increment "lastnode", checking for instruction table overflow.  Sets
"lastptr" using cwriteaccess, to allow caller to easily fill in the
node.
}


procedure geninst(i: insttype; {instruction to generate}
                  l: operandrange; {number of operands}
                  olen: datarange {length of operands} );
  external;

{ Generate an instruction.

Actually, this creates a new node in the "nodefile" and initializes the
contents of the node according to data provided in the calling line.  The
actual instruction emission is done later from the node file.

If this instruction has been labeled, the "labelled" field is set.

All other fields not specified are cleared to zero, and will normally be
filled in by the calling procedure.  In particular, tempcount is set to
zero.
}


procedure genoprnd(o: operand {operand to generate} );
  external;

{ Generates the given operand.  If the operand contains an offset
dependent on the stack, tempcount is set appropriately.
}


procedure genlongword(data: unsigned);
  external;

{ Generates a longword of constant data.  Currently only used for mc68881
  double and extended constants that must be must be passed by pointer because
  there are no 64 or 96 bit immediate modes.
}


procedure genlabeldelta(l1, l2: integer {base and offset labels} );
  external;

{ Generates a case table entry label, the 16-bit difference between l1 and
l2.
}


procedure genlabel(l: integer {label number} );
  external;

{generate a labelnode to label "l".
}


procedure genbr(inst: insttype; {branch to generate}
                l: integer {label number} );
  external;

{ Generate a branch instruction to label "l".

The current stack level is stored in the node for later use
in case stack levels have to be equalized between the branch
point and the label definition point.  If stack adjustment has
been delayed, it is enabled again at this point.
}


procedure genrelbr(inst: insttype; {branch to generate}
                   reladd: integer {branch over reladd instructions} );
  external;

{ Generate a branch relative to the current location.
The relative argument is the number of instructions to skip over,
not nodes, to simplify peephole optimization routines.
}


procedure gendb(i: insttype; {db-style inst to gen}
                regkey: keyindex; {contains register portion of inst}
                l: integer {label to branch to} );
  external;

{gen a "db" instruction, decrement and branch register.
}


Procedure gen1(i: insttype;
               datalen: datarange;
               dst: keyindex);
  external;

{generate a single operand instruction, using keytable[dst] as
 the destination.
}


procedure gen2(i: insttype; {the instruction to generate}
               datalen: datarange; {length of operation in bytes}
               src, dst: keyindex {keytable indices of operands} );
  external;

{generate double operand instruction, using keytable[dst/src] as
 the two operands.
}


procedure gen_bf_inst(i: insttype; {the instruction to generate}
                      datalen: datarange; {length of operation in bytes}
                      src, dst: keyindex; {keytable indices of operands}
                      offset: keyindex {offset of bit field from base address});
  external;

{ Generate a 68020 bit field instruction which may have 1 to 3 operands.
  If either of the fields src or offset is equal to lowesttemp-1,
  then that field is omitted.  If either offset is omitted, then it is
  a constants that isincluded in the instnode; if it is not omitted, then
  it is in a D-register.  The width is always a constant.
}


procedure genadcon(m: modes; {what kind of thing we're referring to}
                   what: integer; {which one, or its location}
                   where: commonlong_reloc_type {which section, if known} );
  external;

{ Generate an address constant for the Apollo.
}


procedure gensymboladcon(n: string8 {symbol name} );
  external;

{ Generate an address constant for a symbol reference for the Apollo.
}


procedure setcommonkey;
  external;

{ Check the key specified in the pseudoinstruction just read, and if
  it is a new key initialize its fields from the data in the pseudo-
  instruction.

  This uses the global "key", which is the operand for the latest
  pseudoinstruction.
}


procedure settemp(lth: datarange; {length of data referenced}
                  m: modes; {args are the same as for setvalue}
                  reg, indxr: regindex;
                  indxlong: boolean;
                  offset, offset1: integer;
                  scale: scale_range;
                  commonlong_reloc: commonlong_reloc_type);
  external;

{Set up a temporary key entry with the characteristics specified.  This has
 nothing to do with runtime temp administration.  It strictly sets up a key
 entry.  Negative key values are used for these temp entries, and they are
 basically administered as a stack using "tempkey" as the stack pointer.
}


procedure settempreg(lth: datarange; {length of data referenced}
                     m: modes; {normally dreg or areg}
                     reg: regindex {associated register} );
  external;

{shorthand call to settemp when only mode and register fields are
 meaningful.
}


procedure settempareg(reg: regindex {areg to set new key temp to});
  external;

{like settempreg, but specific for aregisters}


procedure settempdreg(lth: datarange; {length of data referenced}
                    reg: regindex {associated register} );
  external;

{shorthand call to settemp when only mode and register fields are
meaningful.
}


procedure settempfpreg(reg: regindex {associated register} );
  external;

{shorthand call to settemp when only mode and register fields are
meaningful.
}


procedure settempimmediate(lth: datarange; {length of data referenced}
                           value: integer {literal value to set} );
  external;

{ Shorthand call to settemp for literal keytable entries
}


procedure settempsymbol(sym: string8 {symbol name} );
  external;

{ Shorthand call to settemp for symbol references
}


procedure settempadcon(m: modes;
                       offset: integer;
                       commonlong_reloc: commonlong_reloc_type);

  external;

{ Generate an indirect reference through an address constant, for
  Apollo's weird addressing convention.  Besides being put into
  the node table, adcons are remembered in a list based in
  "first_ind" so that duplicates can be reused.
}


procedure delete(m: nodeindex; {first node to delete}
                 n: nodeindex {number of nodes to delete} );
  external;

{ Delete "n" instructions starting at node "m".  This is done by
  converting them to "nop's" which have no length and are not
  emitted.
}


procedure aligntemps;
  external;

{ Make sure run-time stack aligns with the keytable model.
}


procedure newtemp(size: addressrange {size of temp to allocate} );
  external;

{ Create a new temp. Temps are allocated from the top of the keys,
  while expressions are allocated from the bottom.

  If our model shows a temp of the right size at the top of the
  stack, but the run-time stack has not yet allocated it, we will
  use that temp in stead of allocating another.  This saves time
  and trouble in dealing with nested function calls.  The only
  time newtemp will be called with a mismatch between the model
  and the run-time stack is when we know that the new temp will
  be used to replace the previous temp.
}


procedure adjustoffsets(firstnode: nodeindex; {first node of scan}
                        change: boolean {true if offset is to change} );
  external;

{ Scan all instructions from "firstnode" to the global "lastnode" and
  adjust any which have any offsets dependent on the stack.  Also
  deallocates the temp.

  "Change" is set if there is an actual change in the stack depth for
  the range of instructions being scanned.

     The following stack-dependent items are adjusted:

     labels:       The stack depth for any labels is changed if "change"
                 is set.

     tempcount:    (in every node) Value > 0 implies that this node is
                 dependent on the stack.  In this case, tempcount is
                 decremented and the operand is further modified as
                 described below.

     oprnd.offset: If tempcount was greater than zero, and the node is
                 an oprndnode, and "change" is true, the offset is
                 decremented by the number of bytes being removed
                 from the stack.

     stackdepth:   If this is a labelnode, and the conditions described
                 for oprnd.offset are met, stackdepth is decremented
                 similarly.
}


procedure zaptemps(cnt: integer; {Number of temps do delete}
                   change: boolean {set if temp was never used} );
  external;

{ Pop "cnt" temps from the temp stack, adjusting instructions emitted
  since the temp was allocated.  If this temp was never used, ("change"
  set), the instruction which created it is deleted, and the offsets
  of any instructions depending on the stack are decremented.  Otherwise
  only the stack bookkeeping data of the instructions are decremented.
}


procedure returntemps(cnt: integer {number of temps to return} );
  external;

{ Return temps from the stack, decrementing the book-keeping fields of
  any instructions emitted since they were allocated
}


function uselesstemp: boolean;
  external;

{ True if the top temp on the tempstack is no longer needed.
}


procedure adjusttemps;
  external;

{ Remove any temps which are no longer required.  In the process, the
  stack pointer must be adjusted, and any instructions emitted since the
  temp was allocated must be adjusted if they addressed anything relative
  to sp.

  If the temp was never used, the instruction which created it is deleted,
  and intermediate offsets must be modified because the stack pointer
  was never decremented.  Otherwise, the stack book-keeping field in the
  node is decremented but the offset itself is left alone.

  The global "adjustdelay" is used to delay adjusting temps by one instruction
  when the next instruction is going to be a "savecontext".  This avoids much
  useless stack adjustment in multi-branch statements.
}


procedure definelabel(l: integer {label number to define} );
  external;

{ Define a label with label number "l" pointing to "lastnode".

  Labels are always kept sorted in node order.  Labels which are
  defined as code is initially emitted are naturally in node order,
  but those defined as a result of peep-hole optimizations may
  have to be sorted in.
}


procedure definelastlabel;
  external;

{ Define the label with number "lastlabel".  This is used by the code
  generator to generate new labels as needed.  Such "local" labels are
  defined from "maxint" down, while labels emitted by travrs are defined
  from 1 up.
}


function findlabel(labno: integer {desired label number} ): labelindex;
  external;

{ Searches the label table for label "labno" and returns the index of
  the entry for that label.
}


procedure callsupport(bn: libroutines {support routine to call} );
  external;

{ Call the support library
}


procedure genprofstmt;
  external;


procedure supname(libroutine: libroutines;
                  var s: packed array[lo..hi: integer] of char);
  external;

{ Generate a string containing the support call name.
}


function instlength(n: nodeindex {must refer to an instruction} ): integer;
  external;

{ Return the byte length of the given instruction.  This code assumes
  that all relevant instruction optimization has been done (i.e. adds
  have been changed to addi's or addq's where appropriate).
}


procedure refglobal(m: modes; {usercall, supportcall, etc.}
                    what: integer {which routine or other global} );
  external;

{ Look up a global reference in the reference list, and if absent,
  append it to the list.
}


procedure refsymbol(n: string8 {symbol name} );
  external;

{ Look up a symbol reference in the reference list, and if absent,
  append it to the list.
}


procedure defglobal(m: modes; {usercall, supportcall, etc.}
                    what: integer; {which routine or other global}
                    sect: commonlong_reloc_type; {which section}
                    where: addressrange {where within the section} );
  external;

{ Append a global definition to the definition list.
}


procedure defsymbol(sym: string8; {symbol name}
                    sect: commonlong_reloc_type; {which section}
                    where: addressrange {where within the section} );
  external;

{ Append a symbol definition to the definition list.
}



{ These declarations are here so code and putcode may be compiled separately.
}
procedure insertnewESD;
  external;

procedure initdiags;
  external;

procedure InitMac;
  external;

procedure InitObj;
  external;

procedure FixMac;
  external;

procedure FixObj;
  external;

function dump_externals: integer;
  external;

procedure fixdiags;
  external;

procedure FixDefines;
  external;

procedure putdatax;
  external;

{ This function is in genblk, but it is needed in commc for 24-bit pic.
}

function getareg: regindex;
  external;


{ This procedure is in genblk, but it is needed in commc for Apollo
  support calls.
}

procedure markareg(r: regindex {register to clobber} );
  external;

procedure gendouble(i: insttype; {instruction to generate}
                    src, dst: keyindex {operand descriptors} );
  external;

procedure fpgendouble(i: insttype; {instruction to generate}
                      src, dst: keyindex {operand descriptors} );
  external;

procedure copy_openarrays;
  external;

{ Generate a double operand f.p. instruction.  Like gensingle, calculates
  operand length and calls fixaccess to generate stack pop and push modes.
}

procedure fptodouble(x: realarray;         { * temporary definition * }
                     var d: realarray;
                     var err: boolean);
{ Convert floating point x to IEEE double format.
  If overflow or other error occurs, set err, and d is not valid.
}
  external;


procedure fptosingle(x: realarray;         { * temporary definition * }
                     var s: realarray;
                     var err: boolean);
{ Convert floating point x to IEEE single format.
  If overflow or other error occurs, set err, and s is not valid.
}
  external;


procedure fptoint(x: realarray;         { * temporary definition * }
                  var i: integer;
                  tounsigned: boolean;
                  var err: boolean);
{ Convert floating point x to an integer i, possibly unsigned.
  If overflow or other error occurs, set err, and i is not valid.
}
  external;
