{[b+]}
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

  Pascal-2 Compiler-Debugger Shared Declarations

 Last modified by KRIS on 21-Nov-1990 15:15:38
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

{ Common ODB/PDB version.  Gross, ugly, partially conditionalized.  I've
  tried to note the ODB-only and PDB-only things, but have certainly missed
  some.  When PDB goes away entirely, a lot can be cleaned out of this file,
  files.pas, and analys.
}

{ Common definitions used by compiler and debugger.  This header file
  defines the interface between these two programs.  There is a complication
  by the fact that there are two debuggers: the DEC operating system
  debuggers and the Unix debuggers. The interface consists of two files,
  a symbol table file (filled with debugrecords),and a statement map file
  (filled with stmtrecords).  This second file is completely different for
  the two debuggers. In addition, for the DEC operating system debuggers,
  special code not described in this file is emitted for each statement and 
  procedure prologue.
}

const

  p_maxregmask = 31;
  p_dbg_syp_span = 29; {blocking span for debug records}
  p_dbg_str_span = 1023; {block span for string table}
  p_dbg_hash_span = 14; {span of bucket table}

  { Standard record positions in p_dbgfile }

  p_init_idx = 1;
  p_globhash_idx = 2;
  p_bool_idx = 3;
  p_char_idx = 4;
  p_int_idx = 5;
  p_shortint_idx = 6;
  p_real_idx = 7;
  p_double_idx = 8;
  p_dummysubrange_idx = 9;
  p_text_idx = 10;
  p_nil_idx = 11;


  { define size of hash table used to speed debugger access to names }

  debughashtablesize = 27; { currently a bucket on ['A'..'Z','$','_'] }

  debugsymbolmax = 32;

type

  debugrecordtype = (identdesc, symboldesc, formdesc
);


  p_regmask = packed array [0..p_maxregmask] of boolean;

  p_strtblindex = 0..$FFFF;

  p_symbolindex = targetint;


  p_mapindex = 0..$FFFF;



  { built-in types }

  types = (subranges, fields, variantlabs, arrays, conformantarrays, strings,
           sets, files, ptrs, scalars, ints, bools, chars, reals,
           doubles, fptrs, funcs, procs, stringliterals, flexarrays, opaques,
           bytes, words, none);

  { register descriptions for the Unix debuggers }

  genregmask = packed array [0..maxgenregistermask] of boolean;
  ptrregmask = packed array [0..maxptrregistermask] of boolean;
  realregmask = packed array [0..maxrealregistermask] of boolean;

  { symbol table entry types }

  nametype = (noname, varparam, param, funcparam, procparam, confparam,
              varconfparam, boundid, typename, constname, scalarname, varname,
              fieldname, procname, funcname, forwardproc, forwardfunc,
              externalproc, externalfunc, undeftypename, undefname,
              standardproc, standardfunc, directivename, labelname);

  { Kinds of variable allocation}

  allockind = (normalalloc, genregister, ptrregister, realregister, absolute,
               ownalloc, pointeralloc, sharedalloc, usealloc, definealloc,
               staticalloc);

  { identifier name as written to symbol table file }

  file_addr = unsignedint;

  symbolname = packed array [1..debugsymbolmax] of char;

  debughashindex = 0..debughashtablesize;

  realarray = packed array [1..maxrealwords] of 0..maxusword; {to hold
    representation of a real}

  { symbol table entry description }

  debugrecord =
    record
      case kind: debugrecordtype of

      identdesc:

        (
         { PDB identifier }
         identchars: symbolname; {user supplied name}
         chainoffset: p_symbolindex; {previous symbol name that has same
                                      first character}
         );
        symboldesc:
          (
           { for now, the following definitions have PDB-required 'typeindex',
             'consttype' etc fields.  This wastes space for ODB but I'm
             figuring PDB will go away eventually and I didn't want to make
             an analys.pre. 
           }
           name: 0..255; {analogous to name field in analys, PDB only}
           case namekind: nametype of
             typename: (typeindex: p_symbolindex {pointer to formdesc} );
             constname:
               (consttype: p_symbolindex {type pointer, PDB only} ;
                case constform: types of {value}
                  arrays, fields:
                    (constoffset: unsignedint;
                     constlength: unsignedint);
                  ints, chars, bools, scalars:
                    (i: targetint;
                     extendedint: boolean);
                  reals, doubles: (r: realarray);
                  ptrs: (nilpointer: targetint {targetptr} ));
             varname, param, varparam, fieldname, procparam, funcparam,
             confparam, varconfparam, boundid:
               (offset: targetint; {runtime addr}
                length: targetint; {data length in units}
                varalloc: allockind; {how this variable is allocated}
                vartype: p_symbolindex {data type of item}
                 );
             procname, funcname:
               (functype: p_symbolindex; {return type of function}
                funclen: targetint; {returned data length of function}
                funcoffset: targetint; {returned value offset}
                id: targetint; {symbols for this proc have name field set to
                                id}
                firststmt: targetint; {first statement number for this block}
                laststmt: targetint; {last statement for this block (ODB only)}
                firstname, lastname: p_symbolindex; {symbol table pointers}
                level: targetint; {static nesting level}
                paramsize: targetint; {size of parameters}
                blocksize: targetint; {size (in units) of local storage}
                entryaddress: targetint; {entry point filled in by code}

                { PDB has one mask set for each kind of register }
                genregssaved: genregmask; {general registers saved on entry}
                ptrregssaved: ptrregmask; {pointer registers saved on entry}
                realregssaved: realregmask; {real registers saved on entry}
                nextprocedure: p_symbolindex; {link to next procedure in file}
               ));

        formdesc:
          (packedflag: boolean; {true if user said 'packed'}
           bitaddress: boolean; {true if item is bit, not unit accessed}
           size: targetint; {size in units or bits}
           case typ: types of
             subranges:
               (lowerord, upperord: targetint; {was declared
                                                lowerord..upperord}
                extended: boolean; {true if extended representation}
                parenttype: p_symbolindex {points to base type} );
             scalars:
               (lastord: targetint; {lower value is implied = 0}
                firstscalar: p_symbolindex {refers to first name in list} );
             fields:
               (fieldid, fieldlevel: targetint; {access info}
                tagfield: p_symbolindex; { 0 = no tag, otherwise record index}
                nextvariant: p_symbolindex; {next variant record at this level}
                firstlabel: p_symbolindex; {head of label chain describing this
                                    variant}
                firstvariant: p_symbolindex; {first subvariant defined by case at
                                      level}
                firstfield, lastfield: p_symbolindex {access info} );
             arrays, conformantarrays:
               (indextype: p_symbolindex; {array [indextype] of elementtype}
                elementtype: p_symbolindex;
                lowbound: p_symbolindex; {first of the bound identifiers}
                elementsize: targetint; {effective size of elements} );
             sets: (basetype: p_symbolindex {set of basetype} );
             files: (filebasetype: p_symbolindex {file of filebasetype} );
             ptrs:
               (ptrtype: p_symbolindex {^ptrtypename (field contains type)} ));

    end;

  {defines entries in the statement map file}

  maprecordtype = (stmntrec, plabrec);
  mapindex = targetint;

  stmtrecord =
    packed record
      lineno: targetint; {line number in listing file}
      pc: targetaddress; {relative runtime pc (filled in by code generator)}
      filepos1, filepos2: targetint; {location within list file}
      case opsystems of
        rsx, rt, rsts, cpp:
          (stmtno: targetint; {filled in by code} );
        vms, unix, msdos, vdos, apollo:
          (exit: boolean; {exit from procedure}
           profile: boolean; {profile point}
           double_real: boolean; {double precision real}
           own_section: boolean; {module compiled with $own directive}
           far_call: boolean; {far call used to enter procedure}
           case typ: maprecordtype of
             stmntrec:
               (proclinenr: targetint; {line number in procedure}
                filepos: targetint; {location within list file } );
             plabrec: (recordnr: targetint); ) {record number in symbol
                                                   file}
    end;

    dbg_stmtrecord = stmtrecord;
    dbg_stmtfile = file of dbg_stmtrecord;

  p_dbg_file_record =
    record
      case boolean of
        true:
          (sym: array [0..p_dbg_syp_span] of debugrecord { symbols } );
        false: (str: packed array [0..p_dbg_str_span] of char);
    end;

