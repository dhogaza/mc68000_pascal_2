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

  Pascal-2 Compiler External Procedure Declarations

 Last modified by KRIS on 21-Nov-1990 15:16:36
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

unit hdrprc;

interface

uses config, hdr;

function min(i, j: integer): integer;

{ Returns the lesser of its two arguments.
}
  external;


function max(i, j: integer): integer;

{ Returns the greater of its two arguments.
}
  external;


procedure warnat(error: warning; {Error message number}
                 line: integer; {Text line number for message}
                 column: columnindex {text column for marker} );

{ Generate an error message at the specified line and column.
  Any error turns off generation of the intermediate file.
}
  external;

function space: unsignedint;

{ Returns unused space available for calls to new (in bytes).
}
  external;


function newok(blocksize: integer): boolean;

{ Returns true if a new of this size will not fail.
}
  external;


procedure abort(msg: abortwarning {why we are aborting} );

{ 'panic' exit -- prints message, procedure name and splits. Global var
  'blockref' is assumed to contain the block reference of the
  current procedure.  Call only from analys, travrs or code.
}
  external;


procedure exitst(i: integer);

{ Return to operating system with error status.
}
  external;


procedure timestamp(var day, month, year, {date}
                     hour, min, sec: integer {time} );

{ Returns date information from system.
}
  external;


type getch_t = function(first: boolean): char;

procedure Creadreal(getch: getch_t;
                    var result: realarray;
                    var errorcode: boolean;
                    size: addressrange);

{ Converts an ascii number to a binary value.  Used by "C" only.
}
  external;


procedure p2readreal(getch: getch_t;
                     var result: realarray;
                     var realerror: realstatus;
                     realmode: realmodetype;
                     var isdouble: boolean);

{ Converts an ascii number to a binary value.
}
  external;


procedure xheap(o: overlays);
  external;


procedure z_b129(var l: hdrline;
                 var len, site1, site2: integer);
  external;


procedure z_b130(var early, late: integer);
  external;


procedure getpos(var f: text; {file to get loc in}
                 var block, byte: integer {loc in that file} );
  external;


procedure scantoken;
  external;


procedure dumpstr(len: columnindex; {number of chars to dump}
                  buf: boolean; {which buffer}
                  dumplen: boolean {true says to dump the length byte} );

  external;


procedure codeone;
  external;


procedure initcode;
  external; { init code generator }


procedure genone;
  external;


procedure exitcode;
  external; { clean up after code generation }


procedure sreadenv;
  external;


procedure areadenv;
  external;


procedure swriteenv;
  external;


procedure awriteenv;
  external;


type p_t = procedure;

procedure doheap(p: p_t);
  external; { set heap between passes }


procedure heapinit;
  external; { special heap init routine }


procedure getfilename(which: FilenameListPtr; {file desired}
                      stripdevice: boolean; {want no device/directory field}
                      stripext: boolean; {want no extension field}
                      var result: FilenameBuf; {resulting file name}
                      var resultlength: FilenameIndex {length of file name} );
  external;


procedure getoutputname;
  external;


procedure default_file(var f: filenamelistptr; {result if not set}
                       kind: file_kind {kind of file} );
  external;

procedure openl;
  external; { open listing file }


procedure openenv;
  external; { open environment file}


procedure closel;
  external; { close listing files}


procedure closes;
  external; { close current source file }


procedure opennext;
  external; { open next source file (old scan/list only) }


procedure opens;
  external; { open inital scan file (old scan/list only) }


procedure opena;
  external; {reopen tempfiles for analys}


procedure opent;
  external; {reopen tempfiles for travrs}


procedure openc;
  external; { open code gen files (.obj/.mac) }


procedure closec;
  external; {close code gen files (.obj/.mac) }


procedure opentemp;
  external; { open temp files (string, cache, temp1, temp2) plus debug files }


procedure closed;
  external; {close the debug file}


procedure closeall;
  external; { close all open sourcefiles, except stringfile }


procedure close_list;
  external; {close list control file}


procedure close_str;
  external; { close stringfile }


procedure opentree;
  external; {open the tree dump file for travrs}


procedure closetree;
  external; {close the tree dump file for travrs}


procedure csi;
  external; { read command string }


procedure scan;
  external; { lexical scanner }


procedure scan1;
  external; { init scanner }


procedure scan2;
  external; { terminate scanner }


procedure preprocess;
  external; {preprocess a file only}


procedure analys;
  external; { declarations and syntatic driver }


procedure body;
  external; { syntatic analysis of block body }


procedure cstruct;
  external; { syntatic analysis of const structures }


procedure travrs;
  external; { common sub-expression elimination }


procedure improve;
  external; { improve tree structure through hoisting etc }


procedure walk;
  external; { walk improved tree }


procedure genblk;
  external; { generate code for one block }


procedure code;
  external; { drive genblk, emit .obj/.mac, peephole cleanup }


procedure list;
  external; { list program with errors }


procedure panic;
  external; {print panic message and abort}

type
  dbgstringblock = packed array [0..diskbufsize] of char;
  nextchar_t = function: char;

function debughash(len: integer;
                   nextchar: nextchar_t): integer;

 { hash names for debugger }

  external;


function opendebugfile(var filename: FilenameBuf;
                       ext: packed array of char): boolean;

  external;


type nextblock_t = function(var blk: dbgstringblock): boolean; {is there more}

procedure closedebugfile(nextblock: nextblock_t);

  external;


function p_debughash(len: integer;
                     nextchar: nextchar_t): integer;
  external;


procedure create_paramdesc(procformidx: unsignedword; {proc this param belongs
                                                       to}
                           xparamtype: p_symbolindex; {pointer to param type}
                           xparamkind: nametype) {kind of param (var, value?)} ;

  external;


function create_filename(xstringoff: unsignedword; {offset in string table}
                         hashvalue: integer; {returned by the "hash" function
                                              for this id}
                         xfirstchar: char; {first character in name}
                         xident_len: unsignedword {length of the name}
                         ): p_symbolindex;

  external;


function create_typename(xstringoff: unsignedword; {offset in string table}
                         hashvalue: integer; {returned by the "hash" function
                                              for this id}
                         xfirstchar: char; {first character in name}
                         xident_len: unsignedword; {length of the name}
                         visibtylink: p_symbolindex; {visibility link, points to
                                                      enclosing record,
                                                      procedure or module or
                                                      pervasives}
                         xsymboltype: p_symbolindex; {index of form descriptor
                                                      for this type}
                         xfilename: unsignedword; {index of source file name}
                         xstartpos: unsignedint; {position in source of this
                                                  name declaration}
                         xdeclare_len: unsignedword {number of lines for name
                                                     declaration}
                         ): p_symbolindex;

  external;


function create_varname(xstringoff: unsignedword; {offset in string table}
                        hashvalue: integer; {returned by the hash function for
                                             this id}
                        xfirstchar: char; {first character in name}
                        xident_len: unsignedword; {length of the name; 0 =>
                                                   formal param of a proc type}
                        visibtylink: p_symbolindex; {visibility link, points to
                                                     enclosing procedure,
                                                     module or pervasives; 0 =>
                                                     formal param of a proc
                                                     type}
                        xsymboltype: p_symbolindex; {index of form descriptor
                                                     for this type}
                        xfilename: unsignedword; {index of source file name}
                        xstartpos: unsignedint; {position in source of this name
                                                 declaration}
                        xdeclare_len: unsignedword; {number of lines for name
                                                     declaration}
                        xnamekind: nametype; {varname, param, varparam,
                                              procparam, funcparam, confparam,
                                              varconfparam, flexparam,
                                              varflexparam, boundid }
                        xoffset: addressrange; {runtime addr}
                        xlength: addressrange; {data length in units}
                        xvaralloc: allockind {how this variable is allocated}
                        ): p_symbolindex;

  external;


function create_variantlabel(xnextlabel: p_symbolindex;
                             xvalue: targetint): p_symbolindex;

  external;


function create_strucconst(
	   xstringoff: unsignedword; {offset in string table}
           hashvalue: integer; {returned by the hash function for this id}
           xfirstchar: char; {first character in name}
           xident_len: unsignedword; {length of the name;
				      0 => formal param of a proc type}
           visibtylink: unsignedword;  {visibility link, points to enclosing
 			 	      record, procedure or module or pervasives;
				      0 => formal param of a proc type}
           xsymboltype: unsignedword;  {index of form descriptor for this type}
	   xfilename: unsignedword;  {index of source file name}
           xstartpos: unsignedint; {position in source of this name declaration}
           xdeclare_len: unsignedword;  {number of lines for name declaration}
	   xconsttype: types;  {constant type - sets or stringliterals}
	   xconstoffset: targetint;  {offset from constant data area}
	   xconstlength: targetint  {length of constant}
	   ): unsignedword;

  external;

 
function create_intvalconst(
	   xstringoff: unsignedword; {offset in string table}
           hashvalue: integer; {returned by the hash function for this id}
           xfirstchar: char; {first character in name}
           xident_len: unsignedword; {length of the name;
				      0 => formal param of a proc type}
           visibtylink: unsignedword;  {visibility link, points to enclosing
 			 	      record, procedure or module or pervasives;
				      0 => formal param of a proc type}
           xsymboltype: unsignedword;  {index of form descriptor for this type}
	   xfilename: unsignedword;  {index of source file name}
           xstartpos: unsignedint; {position in source of this name declaration}
           xdeclare_len: unsignedword;  {number of lines for name declaration}
	   xconsttype: types;  {constant type - ints, bools, scalars}
	   xvalue: targetint;  {integer value of constant}
	   unsigned: boolean  {true if value is interpreted as unsigned}
	   ): unsignedword;

  external;

 
function create_realvalconst(
	   xstringoff: unsignedword; {offset in string table}
           hashvalue: integer; {returned by the hash function for this id}
           xfirstchar: char; {first character in name}
           xident_len: unsignedword; {length of the name;
				      0 => formal param of a proc type}
           visibtylink: unsignedword;  {visibility link, points to enclosing
 			 	      record, procedure or module or pervasives;
				      0 => formal param of a proc type}
           xsymboltype: unsignedword;  {index of form descriptor for this type}
	   xfilename: unsignedword;  {index of source file name}
           xstartpos: unsignedint; {position in source of this name declaration}
           xdeclare_len: unsignedword;  {number of lines for name declaration}
     
	   xconsttype: types;  {constant type reals, doubles}
	   xvalue: realarray  {value of real/double}
	   ): unsignedword;

  external;

function create_procname(xstringoff: unsignedword; {offset in string table}
                         hashvalue: integer; {returned by the hash function for
                                              this id}
                         xfirstchar: char; {first character in name}
                         xident_len: unsignedword; {length of the name}
                         visibtylink: p_symbolindex; {visibility link, points to
                                                      enclosing record,
                                                      procedure or module or
                                                      pervasives}
                         xfilename: unsignedword; {index of source file name}
                         xstartpos: unsignedint; {position in source of this
                                                  name declaration}
                         xlevel: targetint {static nesting level}
                         ): p_symbolindex;

  external;


procedure update_procname(index: p_symbolindex;
                          xdeclare_len: unsignedword; {number of lines for name
                                                       declaration}
                          xblocksize: addressrange; {size (in units) of local
                                                     storage}
                          xfirststmt: integer; {value returned by stf_stmt()}
                          xlaststmt: integer; {ditto, for last stmt.}
                          xsymboltype: p_symbolindex {index of form descriptor
                                                      for this type}
                          );

  external;


function create_simpleform(size: addressrange; {size in units}
                           typ: types; {type of simple form}
                           unsigned: boolean {true if extended representation}
                           ): p_symbolindex;

  external;


function create_scalarform: p_symbolindex;

  external;


procedure update_scalarform(index: p_symbolindex; {record to be updated}
                            xsize: addressrange); {size in units}

  external;


function create_setform(xsize: addressrange; {size in units}
                        xpackedflag: boolean; {true if user specified 'packed'}
                        xbitaddress: boolean; {true if item is bit, not unit
                                               accessed}
                        xbasetype: p_symbolindex {points to base type}
                        ): p_symbolindex;

  external;


function create_arrayform(xtyp: types; {of type arrays or flexarrays}
                          xsize: addressrange; {size in units; 0 => flex array}
                          xpackedflag: boolean; {true if user specified
                                                 'packed'}
                          xbitaddress: boolean; {true if item is bit, not unit
                                                 accessed}
                          xindextype: p_symbolindex; {pointer to index type}
                          xelementtype: p_symbolindex; {pointer to element type}
                          xelementsize: addressrange {size of an element}
                          ): p_symbolindex;

  external;


function create_subrangeform(xsize: addressrange; {size in units or bits}
                             xunsigned: boolean; {true if extended
                                                  representation}
                             xlowerord, xupperord: targetint; {was declared
                               lowerord..upperord}
                             xparenttype: p_symbolindex {points to base type}
                             ): p_symbolindex;

  external;


function create_fieldform(newscope: boolean; {create a new scope hash record?}
                          xsize: addressrange; {size in units}
                          xpackedflag: boolean; {true if user specified
                                                 'packed'}
                          xbitaddress: boolean; {true if item is bit, not unit
                                                 accessed}
                          xfieldid: integer; {scope id for symbol table search}
                          xnextvariant: p_symbolindex; {next variant record at
                                                        this level}
                          xfirstlabel: p_symbolindex; {head of label chain
                                                       describing this variant}
                          xfirstvariant: p_symbolindex; {first subvariant
                                                         defined by case at
                                                         level}
                          xtagfield: p_symbolindex; {name entry of tagfield, 0
                                                     if none}
                          xfirstfield: p_symbolindex; {index of first field in symbol
                                                 table}
                          xlastfield: p_symbolindex {index of last field in record}
                          ): p_symbolindex;

{ Create form entry for a new piece of a record.  "newscope" is necessary
  because each "record ... end" creates only one scope, though potentially
  many "fields" entries in both the compiler and debugger symbol table.
  If true, a new hash record is built.
}

  external;


procedure update_fieldform(index: p_symbolindex; {which field to update}
                           xlastfield: p_symbolindex; {update to exclude
                                                       subfields}
                           xfirstvariant: p_symbolindex; {first outermost
                                                          variant}
                           xtagfield: p_symbolindex);

{ Called only at outermost record level, i.e. "record ... end", not
  variant subfields.  These two entries were unknown a creation time.
  Records declare a new scope, thus compiler must initialize the outermost
  field before it is parsed so that we'll have a place to hook idents.
  Normally, subfields are handled to a call to "create_type" after
  all this info has been gathered, like other types.
}

  external;


function create_pointerform: p_symbolindex;

  external;


procedure update_pointerform(index: p_symbolindex; { record to be updated }
                             xsize: addressrange; {size in units}
                             xptrtype: p_symbolindex); {'POINTER TO <xptrtype>'}

  external;


function create_fileform: p_symbolindex;

  external;


procedure update_fileform(index: p_symbolindex; { record to be updated }
                          xsize: addressrange; {size in units}
                          xfilebasetype: p_symbolindex); {'POINTER TO
                                                          <xptrtype>'}

  external;


function create_procform(xrettype: p_symbolindex {points to function return
                                                  type}
                         ): p_symbolindex;

  external;


procedure dbg_alloc(index: p_symbolindex; {a variable name's index (0 ignored)}
                    new_alloc: allockind; {kind of allocation}
                    new_offset: integer {where allocated} );

{ Modify the symbol file to reflect the allocation of the variable at index.
  Used by travrs when assigning variables to registers
}
  external;


procedure dbg_regs(index: p_symbolindex; {procedure name to be updated}
                   var reg: array of boolean;
                   var pc_index: integer {map file index for pc} );

{ Modify the symbol file to reflect the actual register usage for a procedure.
  Used by code after register usage is known.  pc_index, if non-zero, is
  the location in the map file to place the function starting pc.
}
  external;


procedure putdebugblock;

{ Write Pascal-2 block-specific stuff to the debug file
}

  external;


procedure writestmtrecord(plabrecord: boolean;
                          flags, lineno, info: integer);

{ Write Pascal-2 statement-specific stuff to the stmt file
}

  external;


procedure stf_pc(where: integer; {record to patch}
                 pc: integer {new pc} );

{ Put the pc value into a statement record.
}
  external;


function stf_stmt(lineno: integer; {line number for this statement}
                  filepos: integer {file position for this statement} ): integer
 ;

{ Append a statement record and return its index.
}
  external;


function stf_tell: integer;

{ Return the next record to be written
}
  external;


procedure p_nec;

{ NEC license check.
}
  external;


procedure assertcode(b: boolean);
  external;

{ trash compiler if b is false }


procedure positionstmtfile(linecount: integer;
                           var stmtno: integer);

 { Position stmtfile to current line in listing file for PDB }

  external;


procedure updatestmtfile(linecount, temppos1, temppos2: integer);

{ Update statement file with getpos/setpos data pointing to current
  line in listing file.
}

  external;


procedure dumpsymboltable(level: levelindex; {level of block to dump}
                          regok: boolean {ok to allocate global reg variables} )
                          ;

{ Dump the PDB symbol table
}

  external;

implementation

end.
