{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 

  Syntax Analyzer Common Routine Declarations

  Release version: 0045 Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Syntax/Semantic Analysis Routine Declarations

 Last modified by KRIS on 21-Nov-1990 15:18:29

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}


function lib$int_over(i: integer): integer;
  external;

{ Dumb routine to turn off dumb overflow traps when we do dumb, 32-bit
  unsigned arithmetic.  ARGHH.
  Never referenced in Analys/Body!!!
}


procedure warnbefore(err: warning {Error message number} );

{ Generate an error message at the center of the last token.
}

  external;


procedure warnbetween(err: warning {Error message number} );

{ Generate an error message half way between the last token and the
  current token.
}

  external;


procedure warn(err: warning {Error message number} );

{ Generate an error message in the middle of the current token.
}

  external;


procedure warnnonstandard(err: warning {Error message number} );

{ Generate a warning only if the standard switch is set.
  Used to warn of non-standard features.
}

  external;


procedure fatal(err: warning {Error message number} );

{ Generate a fatal warning which will terminate the compilation.
}

  external;


procedure checkmemory;

{ If we have run out of memory give a panic exit and stop
  compilation.
}

  external;



{ Virtual Memory System }

{ Virtual Memory System for Instruction Nodes.
  These routines implement the virtual memory tree used for instructions.
  Nodes are stored on the file "nodefile" in blocks.  A maximum of
  "maxblocksin" of these blocks are kept in buffers in main memory.
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
  there are less than "maxblocksin" blocks already in memory, a new
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


procedure areadaccess(i: index; {node wanted}
                      var p: entryptr {provides access to desired node} );

{ Make virtual element "i" available for read-only access
  and return a pointer to it.

}

  external;


procedure awriteaccess(i: index; {node to access}
                       var p: entryptr {provides access to the node} );

{ Make virtual element "i" available for access and modification, and
  return a pointer to it.

}

  external;


procedure adecreasebuffers;

{ Not needed on the VAX, but the code must be put back to support
  smaller machines.
}

  external;


procedure aincreasebuffers;

{ Not needed on the VAX, but the code must be put back to support
  smaller machines.
}

  external;


procedure getnexttoken;

{ Get the next token from the intermediate file into the global "nexttoken".
  In the process, linenumbers and switches are incremented as necessary.

  The file is compressed as described in the "put" routine for the scanner,
  and this routine undoes those compressions.
}

  external;

{procedure gettokenfile;}

{ Read next token from token file created by SCAN.

  The token file created by SCAN is organized as an array
  0..diskbufsize of tokens rather than simply a file of tokens.
  This greatly reduces i/o overhead, especially on record oriented
  systems.  What this routine does is modify a global level
  variable called tokenbufindex, which is an index into the array
  of tokens in the token file buffer.  All references to tokens
  are of the form: tokenfile^[nexttokenfile].
  Internal to module Comma. Never called in Analys/Body.
}


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
}

  external;

{procedure putintfile;}

{ Write intermediate code file.

  The output of ANALYS is an intermediate code file which is used   as
  input to TRAVRS.  The intermediate code file is organized as blocks
  containing arrays 0..diskbufsize of intermediate code records.  What
  this routine does is to calculate an index (nextintcode) into the
  current buffer where the next intermediate code record should be
  written.

  The intermediate code file should be accessed as: 

     tempfiletwo^[nextintcode]:=<data>; putintfile; 

  Never called from Analys/Body.
}

{ String File Processing

  The string file contains constant data collected by scan and analys.
  It is organized in blocks, each consisting of:

    array [0..diskbufsize] of byte

  The string file is always accessed as:

    stringblkptr^[nextstringfile]

  The following routines manipulate this file.
}


procedure putstringfile;

{ Do the equivalent of a "put" on the stringfile.  The global
  "nextstringfile" is incremented, and if the buffer is full an
  actual "put" on the file is done.  The last element is assumed
  to be placed in:

    stringblkptr^[nextstringfile]
}

  external;


procedure putbyte(a: integer {value of byte to access} );

{ Write the byte "a" to the next location in the string file.  This
  is assumed to be added to the constant table, and the global
  "consttablelimit" is incremented as a result.
}

  external;

{ Intermediate File Output

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


procedure genform(f: types {form to emit} );

{ If no errors found so far, emit a form to the intermediate file.
}

  external;


procedure genint(i: integer {value to emit} );

{ If no errors found so far, emit an integer value to the intermediate file.
  Since each intermediate file element is only in the range 0..255 (one byte),
  multiple elements are used.

  Note that only unsigned integers are emitted.
}

  external;


procedure genop(o: operator {operator to emit} );

{ If no errors are found so far, emit an operator to the intermediate file.
}

  external;


procedure genstmt(s: stmttype {statement to emit} );

{ If no errors are found so far, emit a statement to the intermediate file.
}

  external;


procedure verify(set1: tokenset; {acceptable tokens}
                 set2: tokenset; {tokens to finish skip}
                 err: warning {Message if token not ok} );

{ Check that the current token is in set1, emit an error message
  "err" if not, and skip until the token is in set1 + set2 +
  neverskipset.  The error message will be placed as close
  to the estimated location of the token as possible.
}

  external;


procedure verify1(set1: tokenset; {acceptable tokens}
                  err: warning {message if token not ok} );

{ Same as verify, except no separate skip set is provided.
}

  external;


procedure verifytoken(tok: tokentype; {acceptable token}
                      err: warning {message if token not ok} );

{ Check for a given token (tok) and skip it if found.  If not
  found, emit an error message set by "err".  This is used for
  redundant tokens in the syntax, where parsing can continue if it
  is missing.
}

  external;


procedure enterform(newtyp: types; {type for this form}
                    var where: index; {new entry}
                    var whereptr: entryptr {for access to new entry} );

{ Enter a new formentry at the current level.  This also
  gets a formnumber for use with the debugger, and sets the type
  to be newtyp.
}

  external;


procedure searchsection(id: scoperange; {scope id for search}
                        var wherefound: index {resulting name index} );

{ Search the symbol table for the current token identifier in scope
  "id".  If found, the name index will be placed in "wherefound".  If not
  found, zero will be returned. If the identifier found proves to be
  a constant or type identifier we will update "lastoccurrence" within
  the symbol table entry to allow enforcement of scope restrictions.
}

  external;


procedure searchlsection(value1: integer; {label value}
                         labellist: labelptr; {root of label list}
                         var wherefound: labelptr {result of search} );

{ Search a list of labels starting at "labellist" for a label with the
  value "value".  The result is returned in "wherefound".  If the label
  is not in the list, the returned entry will be "labelflag".

  "Labelflag" is set to the desired value to simplify the search algorithm.
}

  external;


procedure searchlabels(value1: integer; {label value}
                       var wherefound: labelptr {result of search} );

{ Search all available scopes for a label with "value", returning the
  result in "wherefound."  The result will be set to "labelflag"
  if the label cannot be found.
}

  external;


procedure search(var wherefound: index {result of search} );

{ Search all available scopes for the current token.  The result is
  returned in "wherefound", with zero indicating no find.  The global
  variable "lev" is set to the level where the token was found.
}

  external;


procedure searchvariants(var currentrecord: index; {record to search}
                         labvalue: operand {varnt label value} );

{ Search a record variant part starting at "currentrecord" for a
  variant with a label of "labvalue" and set "currentrecord" to that
  variant.  If there is no variant with the desired label,
  "currentrecord" is unmodified, and an error message is emitted.
}

  external;

{ Utilities for use with types}


procedure stripsubrange(var objectindex: index {form to be stripped} );

{ Convert a subrange type to the base type for use in an expression.
}

  external;


function lower(f: entryptr {form to check} ): integer;

{ Returns the lower bound of "f".  This is meaningful only for
  scalar types.
}

  external;


function upper(f: entryptr {form to check} ): integer;

{ Returns the upper bound of "f".  This is meaningful only for
  scalar types.
}

  external;


function bits(i: integer {value to find size of} ): integer;

{ Returns the number of bits needed to contain the value of i.
}

  external;


function sizeof(f: entryptr; {Form to get size of}
                packedresult: boolean {set if packed value} ): addressrange;

{ Returns the amount of storage needed to contain a value of the type
  specified by "f".  If "packedresult" is set, this is in bits, otherwise
  it is in addressing units.
}

  external;


function forcealign(size: addressrange; {value to align}
                    alignment: addressrange; {requirement}
                    packedresult: boolean {size is in bits} ): addressrange;

{ Forces "size" to the next higher multiple of "alignment".
  Used to overcome limitations built into much contemporary hardware.
}

  external;


function unsigned(f: entryptr; {type to check}
                  len: addressrange; {space actually allocated for var}
                  packedelement: boolean {set if packed var} ): boolean;

{ Returns true if the values of type "f" are unsigned.
  If "len" is not equal to the space required for the value, it is being
  allocated a space larger than required, and should be treated as signed
  or unsigned for unpacking, depending on the global "unsignedprefered".
}

  external;


function simplesize(i: integer {value to find size of} ): integer;

{ Returns the size in multiples of addressing units needed to contain
  the value of i.
}

  external;


function negaterealconst(realbuffer: realarray {real constant value} ):
 realarray;

 { function to negate a real constant independent of the host }

  external;


procedure constant(follow: tokenset; {legal following symbols}
                   dumpvalue: boolean; {true says dump string}
                   var value1: operand {resulting constant} );

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

  external;


function getform(objecttype: entryptr {desired form} ): types;

{ Get the basic form associated with a type.
}

  external;


function identical(left, right: index {types to compare} ): boolean;

{ True if two types are identical, or if either is undefined (to avoid
  redundant messages).
}

  external;


function compatible(left, right: index {types to compare} ): boolean;

{ True if the types represented by the two input forms are compatible
  as defined by the Pascal standard.  If either input is undefined,
  they are assumed to be compatible to eliminate redundant error
  messages.
}

  external;


function alignmentof(f: entryptr; {form to check}
                     packedresult: boolean {result is packed} ):
 alignmentrange;

{ Compute the alignment requirement of a type.  This function is needed
  strictly because the alignment of a subrange is kluged to the parent
  type to give better code generation on certain types of machines.  This
  kluge causes trouble with packed types, so is deleted if the result
  is to be used in a packed structure.
}

  external;


procedure seekstringfile(n: integer {byte to access} );

{ Do the equivalent of a "seek" on the string file.  This sets the
  file and "nextstringfile" to access byte "n" of the stringfile.
}

  external;


procedure getstringfile;

{ Do the equivalent of a get on the stringfile.

  The string file contains constant data collected by SCAN and
  ANALYS.  It is organized as blocks containing arrays 0..
  diskbufsize of bytes.  The string file is always accessed
  as stringblkptr^[nextstringfile].
}

  external;

function do_hash(charindex:integer; charlen:integer) : integer;
{perform a hashing of the entry of length "charlen" at the location "charindex"
 in the stringtable, using the function concealed inside the debugger package.}

  external;

