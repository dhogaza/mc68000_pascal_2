{**************************************************************}
{                                                              }
{                          Pascal-2                            }
{                                                              }
{**************************************************************}
{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1985, 1986, 1987, 1988, 1989 Oregon Software, Inc.
  ALL RIGHTS RESERVED.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied, modified, transferred, and used only as
  provided under a signed license agreement with Oregon Software.  
  Any support purchased from Oregon Software does not apply to 
  user-modified programs.  All copies of this program must display 
  this notice and all copyright notices. 

  Release version: 0045 Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler Common Global Declarations

 Last modified by KRIS on 21-Nov-1990 15:15:17

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}



unit hdr;

interface

uses config;

const
  maxstandardlabel = 9999; {maximum value for a Pascal label, cast in cement
                            by the ISO standard.}
  maxstrlen = 255; {maximum number of chars in a string, consistent across all
                    implementations of Pascal-2}


  {---------------------------------------------------------}
  {                                                         }
  {             GLOBAL TYPE DECLARATIONS                    }
  {                                                         }
  {---------------------------------------------------------}

type

  { NEW string table definitions }

  string_len = 0..max_string_len; {for holding string data}
  string_data = packed array [1..max_string_len] of char;
  string_buffer =
    record {contains text of a string}
      len: string_len; {length of data}
      data: string_data; {actual data}
    end;

  stringindex = 0..stringtablesize; {index into string table}
  stringtableindex = stringindex; {a synonym used elsewhere}
  stringarray = packed array [0..stringspan] of char; {defines string table}
  stringptr = ^stringarray; {used because stringtable dynamically created}
  stringbasearray = array [0..stringblocksmax] of stringptr;

  double = real; {should be double precision within the compiler}

  natural = 0..32767; {a useful counter}

  hostfilebyte = 0..hostfilelim; {addressing unit of host file}

  { list of overlays called in 'cheating' fashion from root }

  overlays = (xcloses, xopenl, xbody, xcstruct, xclosel, xgenblk, xputcode,
              xpanic, xopentree, xclosetree, xcsi, xscan, xanalys, xtravrs,
              xlist, xcode, xopenenv, xopennext, xopens);

  {[f-]}
  switch = (noswitch,      {start of the switches}
            awaremode,     {specify floating coprocessor mode}
            bstep,         {handles 80186 bstep problems}
            bytealloc,     {put ints into bytes if they fit}
            caseswitch,    {makes Pascal compiler case sensitive}
            classsw,       {Pascal-F class}
            code_switch,   {???}
            codeconst,     {put constants in code segment}
            commonvars,    {emit define vars as common}
            cplusplus,     {(C) compile C++}
            checkout,      {(C) generate error checks}
            compatibility, {(C) degrade error checking}
            cpu68000,      {Specify the CPU}
            cpu68020,
            cpu8086,
            cpu80286,
            datesw,        {Pascal-F date}
            debugging,     {generate high-level debugging interface code}
            defineswitch,  {generate an environment module}
            details,       {print internal details in test mode}
            doublereals,   {(Pascal) use long reals}
            editlist,      {(MS-DOS) special error format for BRIEF}
            eis,           {(PDP-11) EIS floating point}
            enumints,      {(C) allocate all enum vars ints}
            environswitch, {read an environment module}
            versionswitch, {display compiler version information}
            expandfloat,   {(C) all floating point done in double}
            extensions,    {(C) enable any special extensions}
            fis,           {(PDP-11) FIS floating point}
            floatsafe,     {lord only knows but I doubt he cares}
            fpc68881,      {(68K) code for 68881}
            fpp,           {(PDP-11, MS-DOS) FPP floating point}
            framepointer,  {use dedicated frame pointer}
            genmask,       {specify code optimization options}
            groupown,      {(iAPX-86) allocate own section in DGROUP}
            ident_switch,  {???}
            indexcheck,    {(Pascal) generate index range checks}
            largecode,     {(iAPX-86) far procedure calls}
            largemodel,    {(iAPX-86) far pointers}
            level0,        {(Pascal) disable conformant arrays}
            listcount,     {turn listing on}
            listerrors,    {list only lines with errors}
            listexpansion, {list expanded macro lines}
            listincludes,  {list include text}
            librequest,    {include library request in output}
            longlib,       {(68K) long library calls}
            mainbody,      {(Pascal) this module contains a main program}
            mathcheck,     {generate overflow checking}
            mergelisting,  {merge assembly and source listing}
            modstrings,    {(C) makes strings modifiable}
            module_switch, {???}
            multidef,      {(Pascal) allow multiple use/define for one var}
            nearfarproc,   {determines near/far attribute of external procs}
            nilcheck,      {generate pointer check code}
            oldpacking,    {use old packing algorithm for compatibility}
            oldreswords,   {(Pascal) don't define USE/DEFINE/SHARED}
            outputebcdic,  {generate the assembly code in EBCDIC}
            outputmacro,   {generate assembly code}
            outputobj,     {generate object code}
            outputprep,    {generate the preprocessor output}
            outputprepstd, {generate the preprocessor output to standard out}
            own,           {place following globals in an own section}
            pascal1,       {generate Pascal-1 compatible code}
            pascalf,       {accept Pascal-F syntax}
            pdp11data,     {(VAX) allocate PDP-11 compatible data}
            pic,           {(VAX, 68K) generate position independent code}
            profiling,     {generate a statement count profile}
            rangecheck,    {generate subrange checking code}
            refnosw,       {Pascal-F refno}
            romconsts,     {(C) put constants in a section for ROM}
            runxenix,      {(i386) running Xenix+Intel obj, not ???}
            sectionsw,     {Specify the code section}
            sharecode,     {(Apollo)}
            shortintegers, {use short, 16 bit integers}
            shortsectsw,   {specify the code as a short section}
            signedchars,   {(C) default to signed characters}
            sim,           {(PDP-11) generate simulated floating point}
            stackcheck,    {generate stack overflow code}
            standard,      {disallow extensions (C, strict standard)}
            statistics,
            stmtnumbers,   {insert statement numbers in assembly output}
            structstand, {assume standard interface for structure return vals}
            structstatic, {place structure return in static var}
            symboltable,   {generate a symbol table file}
            targdebug,     {(C) generate target debug code}
            targprofile,   {(C) generate target profile code}
            tblock,        {specify a procedure number}
            test,          {generate output for testing compiler}
            timing,        {time compiler passes}
            truncatesw,    {truncate intermediate operations where possible}
            tswitch0,      {general purpose debugging switches}
            tswitch1,
            tswitch2,
            tswitch3,
            typesw,        {Pascal-F module type}
            verbose,       { Make various compilers obnoxiously noisy }
            vectextref, {virtual vectors are extref}
            vectextdef, {virtual vectors are extdef}
            versionsw,     {(VERSAdos) specify version}
            versnsw,       {Pascal-F version}
            usebsd42lib,   {(UNIX) controls which unix library to use}
            usesysVlib,
            walkback,      {generate walkback code}
            windows,       {generate prologue/epilogue code for MS-Windows}
            finalswitch);

  {[f+]}

  { Switch table entries. One used to log each embedded switch }

  switchentry =
    packed record
      s: switch; {switch which was encountered}
      v: - maxswitchvalue..maxswitchvalue; { bump current value by this much }
      mlow, mhi: integer; { marker into current intermediate file }
    end;

  switchcounterarray = packed array [switch] of shortint;
  switcheverplusarray = packed array [switch] of boolean;

  standardids = ( { Pascal junk }
                                 integerid, shortintid, realid, doubleid,
                                 charid, booleanid, trueid, falseid, textid,
                                 inputid, outputid, writeid, writelnid,
                                 readid, readlnid, putid, getid, seekid,
                                 resetid, rewriteid, closeid, breakid, newid,
                                 disposeid, packid, unpackid, pageid, timeid,
                                 absid, sqrid, sinid, cosid, expid, lnid,
                                 sqrtid, arctanid, oddid, eofid, eolnid,
                                 truncid, roundid, ordid, chrid, succid,
                                 predid, sizeid, bitsizeid, loopholeid,
                                 maxintid, minintid, refid, noioerrorid,
                                 ioerrorid, iostatusid, deleteid, renameid,
                                 emtid, forwardid, externalid, nonpascalid,
                                 fortranid, interruptid, insertid, strid,
                                 valprocid, copyid, concatid, lengthid, posid,
                                 deletestrid, snglid, dblid, upperid, lowerid,

  { Pascal-F only }
                                 shortunsid, longintid, longunsid, toshortid,
                                 toshortuid, tolongid, tolonguid, nearid,
                                 farid,

  { Modula-2 junk }

                                 capid, exclid, inclid,

  { C junk }

                                 acosid, asinid, atanhid, ceilid, copysignid,
                                 coshid, divid, floorid, fabsid, frexpid,
                                 isnanid, ldivid, ldexpid, labsid, logid,
                                 log10id, logbid, modfid, powid, sinhid,
                                 tanid, tanhid, memchrid, memcmpid, memcpyid,
                                 memmoveid, memsetid, strcatid, strchrid,
                                 strcmpid, strcpyid, strcspnid, strlenid,
                                 strncatid, strncmpid, strncpyid, strpbrkid,
                                 strrchrid, strspnid, strstrid, varargsid,
                                 mainid,

  { 68881 only }
                                 facosid, fasinid, fatanid, fatanhid, fcoshid,
                                 fetoxm1id, fgetexpid, fgetmanid, fintid,
                                 flog10id, flog2id, flognp1id, fmodid,
                                 fmovecrid, fnopid, fremid, fscaleid,
                                 fsgldivid, fsglmulid, fsincosid, fsincos2id,
                                 fsinhid, ftanid, ftanhid, ftentoxid,
                                 ftwotoxid, setfpcrid, readfpcrid);

  { all detected compile time errors.  currently a union of all languages
    and possibilities.  will hopefully be replaced later with a more sane
    scheme of language-dependent warning files! }

  warning = (firstwarning, linetoolong, badchar, missingdigits, badinteger,
             badexpon, toomanyerrors, zerostring, stringtableoverflow,
             levelerr, doteoferr, extraenderr, extrastmterr, extraprocerr,
             baddirective, deepinclude, badlabelnest, garbageerr,
             blockstarterr, scrambledblkerr, nosemierr, blockenderr,
             nobeginerr, noenderr, stmtenderr, nountilerr, badelseerr,
             nothenerr, nocommaerr, nocolonerr, nooferr, caselabelerr,
             nodoerr, nobecomeserr, nodowntoerr, caseelseerr, novarerr,
             badlabelerr, norparerr, badparamerr, badcolonerr, notypenameerr,
             nosemiprocerr, nofuncass, badexprerr, nooperr, nooprnderr,
             badindexerr, norbrackerr, badrparerr, noeqlerr, badconsterr,
             nosemiheaderr, baddeclerr, badtypesyntax, nolabelerr,
             nolbrackerr, nodotdoterr, nolparerr, duplicateident, tablefull,
             undeftablefull, proctablefull, undefidenterr, badsubrange,
             badindex, badsetbase, badcasetyp, badcaselab, duplicatetag,
             duplabeldef, labnotpredef, badlabeldef, badtagerr, badfunctype,
             badfuncassign, missingforindex, modifiedfor, badprocfunc,
             badassign, norecordident, dupfwdparam, dupfwdresult, dupforward,
             fwdprocfuncerr, fwdfuncprocerr, badxdef, recordexpected,
             arrayexpected, ptrexpected, farptrexpected, toomanyargs,
             toofewargs, paramtypeerr, booleanexpected, badarithtype,
             signedseterr, badreloprnds, badrealtoint, badassignment,
             typesincomp, compilerwritererr, nostrictinclusion, badinoprnds,
             badfortype, badforlimit, badcasetype, badcaselabeltype,
             indexincomp, badforvar, badprocparam, badfuncparam, varparamerr,
             badsetexpression, cantpack, badformat, illegalformat,
             badwritearg, nowritearg, badfunctionarg, nofilevar, badreadtype,
             noreadarg, nostringerr, filenameerr, nofieldtype, badnewlabel,
             noptrvar, labelundef, fwdundef, typeundef, octalconst, badoctal,
             nondecimalconst, badradix, wantvarname, nofilefile, dupcaselabel,
             unassigned, indexerror, rangeerror, overflow, bigarrayerr,
             badorigin, novaluefile, dontassignfile, longstring, bigsetbase,
             nottextfile, obsoletecomments, typenotallowed, progexpected,
             notimplemented, badmodop, badpackconform, confinconsistent,
             badconfactual, bigrecorderr, bigblockerr, biglabelerr, badnumber,
             badfornestref, badcasetags, nameundef, filenotdeclared,
             inputnotdeclared, outputnotdeclared, novarianttag, notlevel0,
             toomanyelements, eofincomment, baddouble, manyscopes, baduniv,
             badstringindex, stringoverflowerr, bodyfounderr, manyenviron,
             badenviron, badoptions, {LIST} baddefine, badcase, baddbltoreal,
             toomanyextvars, badsharedvar, badusedefinevar, badcvtfunc,
             badinterruptproc, unsupportedforvardecl, badmultidef, {LIST}
             chartoolong, badpragma, badpragval, macroredef, manymacroparms,
             noidenterr, badconcat, badmacroflag, deepmacronest, exprtoomuch,
             badppexpr, nomatchrpar, nocolonop, noquesterr, eofskipping,
             ppbadelse, divzeroerr, ppextra, ppbadendif, badppdir, badlinedir,
             noundef, badfilename, macrotoolong, usererror, badtypeerr,
             duptypeerr, storenotok, undefsize, badbftype, bftoobig,
             norcurlerr, badconexpr, nodeclerr, onlyonefunc, badfuncstore,
             noexprerr, badidentlist, incomlink, baddecl, noargname,
             nofieldident, badlinkage, incomtype, twoinits, vartablefull,
             ambigtypesize, doubdefault, notinsw, badcontin, badbreak,
             nonvoidret, mixedreturns, nostaterr, nolcurlerr, nowhileerr,
             novoidval, cantassign, nostructype, badcast, badptrsub, notconst,
             noenumerr, nostructerr, nobitaddr, noregaddr, badinit,
             scalarexpected, emptyfile, notdfunc, badifnest, lastwarning);

  { Reasons for aborting a compilation}

  abortwarning = (outofmem, undeltemps, muchcode, manylabels, manyplabels,
                  manytemps, manynodes, builderror, manykeys, walkerror,
                  interntemp, inconsistent, badadjust, manyexterns,
                  wrongversionenv, badrelfile, manynonlocals, perposdump);

  { Various supported calling sequences }

  linkages = (pascal2call, nonpascalcall, fortrancall, interruptcall,
              modulebody, definitionbody, implementationbody);

  { subranges for various tables, values are documented above }

  levelindex = 0..maxlevel;
  columnindex = 0..linelen;
  hashindex = 0..hashtablesize;
  switchindex = 0..switchtablesize;
  errorindex = 0..errortablesize;
  listindex = 1..listtablesize;
  proctableindex = 0..proctablesize;
  var_index = 0..vartablesize;
  cseregionindex = 0..cseregions;
  labelrange = 0..32767; {range of generated internal labels}
  pascallabelrange = integer; {range of pascal label numbers}

  {$include 'dbghdr'} { definitions common to compiler and debugger }

  { one fileremember is built for each input file, in order of opening }

  filerememberptr = ^fileremember;

  fileremember =
    packed record
      next: filerememberptr; {link to next}
      offset: stringtableindex; {offset in stringtable of the file name}
    end {fileremember} ;

  { one errorrecord is built for each error within the program }

  errorrecord =
    packed record
      err: warning; {message to issue}
      errcolumn: columnindex; {source column to mark}
      errline: integer; {source line for error message}
    end;

  { one listrecord is built for each list and nolist switch }

  listrecord =
    record
      start: integer; {line to start listing}
      count: integer; {number of lines to list}
    end;

  { one proctableentry is filled for every procedure }

  proctableentry =
    packed record
      charindex: stringtableindex; {index in stringfile to name of proc}
      opensfile: boolean; {true if proc any files}
      realfunction: boolean; {true if function returning real result}
      farptrfunction: boolean; {true if function returning far pointer}
      globaldeath: boolean; {true if proc deadly to globals}
      isprocparam: boolean; {true if proc is used as a parametric procedure}
      bodydefined: boolean; {true if this proc has a body}
      intlevelrefs: boolean; {true if proc needs static link}
      externallinkage: boolean; {true if proc was defined external}
      any_calls: boolean; {this proc calls other user procs}
      struct_calls: boolean; {this proc calls others with struct returns}
      struct_ret: boolean; {this proc returns a struct value}
      referenced: boolean; {true if ever referenced (after travrs)}
      ownused: boolean; {true if this proc has own data}
      farprocedure: boolean; {true if this is a far procedure}
      needsframeptr: boolean; {true if procedure needs a frame pointer}
      calllinkage: linkages; {type of procedure/function linkage}
      registerfunction: 0..8; {size of function value in regs}
      extref: var_index; {C only -- external name, if any}
      backlink: proctableindex; {link to enclosing procedure}
      charlen: 0..maxprocnamelen; {length of name (first maxprocnamelen
                                   characters)}
      levelspread: levelindex; {number of static hack instructions needed}
      level: levelindex; {static level of this proc}
    end;

  proctable_block = array [0..proctablespan] of proctableentry;

  {The cseregion table stores the lowest and highest addresses at each level
   modified by a particular procedure.  The first entry always records global
   variables modified, the second records own variables modified (if $own
   specified).  This good stuff is saved for as many procedures
   as possible.  Afterwards, only the "globaldeath" flag is recorded in the
   proctable.  This information is used by travrs to minimize the number of
   cses killed by a procedure call.
  }

  cseregionentry = array [boolean] of
      record
        low, high: addressrange;
      end;

  vartableblockptr = ^vartableblock; {pointer to 1 page in vartable}
  vartablerec =
    packed record {one entry in vartable}
      extvaralloc: allockind; {use 'name', define 'name', etc}
      initialized: boolean; {true sez it has been explicitly initialized}
      referenced: boolean; {true sez it has been referenced in body}
      aliased: boolean; {true says string followed use or define}
      faraccess: boolean; {if 'far' references to be used}
      odboffset: boolean; {is to be accounted for in odb vartab offsets}
      charindex: stringtableindex; {index in stringfile to name of proc}
      charlen: 0..maxprocnamelen; {length of name (first maxprocnamelen
                                   characters)}
      size: addressrange; {size of the variable, if known, zero otherwise}
      offset: addressrange; {offset in psect; for "defined" variables only}
    end;
  vartablerecptr = ^vartablerec; {to access one entry in vartable structure}
  vartableblock = array [0..maxvarentries] of vartablerec; {page in vartable}

  suffixindex = 1..suffixlength; {index into suffix}
  suffixtyp = packed array [suffixindex] of char; {filename suffix}
  cmdindex = 1..cmdlinelength; {index into a command line}
  cmdbuffer = packed array [cmdindex] of char; {holds command line}
  cmdptr = ^cmdbuffer; {save buffer on hostopsys = vdos }
  hdrline = packed array [1..130] of char; {header for list}

  file_kind = (source_file_kind, include_file_kind, list_file_kind,
               prep_file_kind, obj_file_kind, mac_file_kind, env_file_kind,
               sym_file_kind, stmt_file_kind, temp_file_kind);

  FilenameIndex = 0..filenamelen; {index into a filenamebuf}
  FilenameBuf = packed array [1..filenamelen] of char;

  FilenameListPtr = ^FilenameList;
  FilenameList =
    record
      next: FilenameListPtr;
      arglen: FilenameIndex;
      arg: FilenameBuf;
    end;

  extension = packed array [1..maxextension] of char; {default extension}

  diskblock_ref = ^diskblock;
  diskblock = packed array [0..diskbufsize] of hostfilebyte;
  doublediskblock = packed array [0..doublediskbuflimit] of hostfilebyte;

  bytestream = file of diskblock;

  { this describes the local vars suitable for register allocation }

  localvartype =
    record
      offset: addressrange; { location of local var }
      size: addressrange; {size of local var}
      debugrecord: integer; { for fixing debug symbol file }
      is_param: boolean; { parameter ? }
      typ: types; { type of the var }
    end;

  localfiletype = file of localvartype;

  { types of optimizations enabled/disabled by genopts switch

    The values after firstgenopt and before lastgenopt correspond
    to inhibit bits passed in through the command line. For instance,
    "-genmask 5" would disable register assignments by lifetimes
    and hoisting. }

  gentypes = (lifetimes, {0 1 mask value }
              propagation, {1 2 }
              hoisting, {2 4 }
              stuffing, {3 8 }
              folding, {4 16 }
              removedeadcode, {5 32 }
              spare6, {6 64 }
              spare7, {7 128 }
              subexpressions, {8 256 }
              tailmerging, {9 512 }
              indexopts, {10 1024 }
              spare11, {11 2048 }
              peepholes, {12 4096 }
              bitops, {13 8192 }
              padding, {14 16384 }
              spare15 {15 32768 }
              );

{ Support initialized data areas
}
  section_kind = (data_section, string_section);

  data_control = {controls initialized data}
    record
      dfile: bytestream; {the file containing the data}
      nextdbuf: 0..diskbufsize; {pointer into the block}
      size: addressrange; {size of data actually written}
    end;

const
  firstgenopt = lifetimes;
  lastgenopt = spare15;

{ Psect stuff; this is used to pass information from scan to
  the VMS backend (putcode) about psect attributes. It is a linked list
  of records containing the (lowercase, space filled) psect name, the
  set of attributes which were explicitly set by the user and to which value.

  Value is an integer for commodity; for boolean like attributes (all but
  alignment, 0 means false and 1 means true.
  For alignment, 0 = byte, 1 = word, 2 = long, 3 = quad and 4 = page.

  I wish I could put this junk elsewhere...

}

type

  psect_attr = (psect_ovl, psect_exe, psect_gbl, psect_lib, psect_pic,
                psect_rd, psect_shr, psect_vec, psect_wrt, psect_rel,
                psect_align);
  psect_value = 0..4;
  psect_attr_set = set of psect_attr;
  psect_desc_ptr = ^psect_descr_block;
  psect_descr_block =
    packed record
      name: packed array [1..maxprocnamelen] of char; {the psect name}
      next: psect_desc_ptr;
      user_set: psect_attr_set; {the user set those}
      value: packed array [psect_attr] of psect_value;
    end;



  {---------------------------------------------------------}
  {                                                         }
  {           GLOBAL VARIABLE DECLARATIONS                  }
  {                                                         }
  {---------------------------------------------------------}

var

  { Logs switches found in the source file }
  switches: array [switchindex] of switchentry;

  switchcounters, {current values of switches}
   originalswitches: switchcounterarray; {values at start of compilation}

  switcheverplus: switcheverplusarray; {true if switch was ever set on}

  errortable: array [errorindex] of errorrecord; {list of errors found}
  fatalflag: boolean; {true if fatal error found}
  anynonlocalgotos: boolean; {true if non-local goto's used in this unit}
  lastswitch: switchindex; {last switch entry made by scan}
  currentswitch: switchindex; {last switch entry activated in current pass}
  lasterror: errorindex; {last error entered}
  abortmsg: abortwarning; {reason for aborting}

  listtable: array [listindex] of listrecord; {OLD listing control entries
                                               list}
  lastlist: integer; {last entry in listtable}

  lastline: natural; {last line in program}
  listcontrol: text; {listing control file for NEW lister}

  debugfile: file of debugrecord;
  newdebugfile: file of p_dbg_file_record;

  source: array [1..sourcedepth] of text; {files for including source}
  sourcelevel: 0..sourcedepth; {current source include level}

  stmtfile: dbg_stmtfile;

  filename: FilenameBuf; {temp file name}
  filename_length: FilenameIndex; {length of name in filename buffer}

  outputname: packed array [1..maxprocnamelen] of char;
  stringblkptrtbl: array [1..maxstringblks] of diskblock_ref;
  stringblkptr: diskblock_ref; {pointer to string block}
  stringfile: bytestream;
  nextstringfile: 0..diskbufsize; {stringfile buffer pointer}
  curstringblock: integer; {currently referenced string block}
  stringfiledirty: boolean; {true if current string block changed}

  stringtable: stringptr; {pointer to old-style string table}
  new_stringtable: stringbasearray; {block of new-style string table pointers}
  stringtabletop: stringindex; {next avail char in string table}

  listing: text; {listing output file}

  { 'Cache' is used by Analys, Travrs, Code for virtual memory }
  cache: file of doublediskblock;

  {references to standard identifiers when needed}
  standardidtable: array [standardids] of hashindex;

  targetintsize: integer; {size of an integer variable on the target}
  targetrealsize: integer; {size of a real variable on the target}
  targetmaxint: integer; {value of maxint on the target}
  targetminint: integer; {value of minint on the target}
  ptrsize: integer; {size of a pointer on the target}
  globalsize: addressrange; {size of global variables}
  inputoffset: addressrange; {offset of "input"}
  ownsize: addressrange; {size of global variables}
  definesize: addressrange; {size of "defined" (i.e. global) variables}
  insertions: integer; {number of distinct variables found}
  dum: integer; {used by main program}
  startday, startmonth, startyear: integer; {starting date}
  starthour, startmin, startsec: integer; {starting time of day}
  istarthour, istartmin, istartsec: integer; {for monitoring execution time}
  endhour, endmin, endsec: integer; {for monitoring execution time}

  curstringbuf: boolean;
  stringtablelimit: integer; {top of string + identifiers in string file}
  consttablelimit: integer; {top of constant table in string file}
  stringfilecount: integer; {top of strings in string file}
  globalfiles: addressrange; {number of bytes allocated for input/output}

  lastlabel: labelrange; {last internal label value assigned}

  putlow, puthi, getlow, gethi: integer; { counts records put/got by current
                                          pass }

  returnlinksize: addressrange; {Length of return linkage for internal procs}
  extreturnlinksize: addressrange; {Length of return linkage for externals}
  farextreturnlinksize: addressrange; {Length of return linkage for far
                                       externals}

  { Table of procedure data -- built up by analys and used by coder }

  proctable: array [0..old_proctablesize] of proctableentry;
  new_proctable: array [0..procblocksmax] of ^proctable_block;
  proctabletop: proctableindex;
  blockref: proctableindex;
  mainref: proctableindex; {for main entry in C}
  import_table_ref: proctableindex;

  { Table of regions of variables modified by procedures }

  cseregiontable: array [cseregionindex] of cseregionentry;

  vartable: array [1..maxvarptrs] of vartableblockptr;
  lastvartableptr: 0..maxvarptrs;
  lastvartableentry: var_index;
  vartabodboffset: addressrange; {the last used offset in the vartable (VMS)}

  {command interface data}
  cmdlength: cmdindex; {length of command line}
  cmdline: cmdbuffer; {actual command line}
  savecmdline: cmdptr; {pointer to save buffer for listing when hostopsys =
                        vdos }
  fakelist: boolean; {fake a listing file to user's terminal}
  SourceListHead: FilenameListPtr; {list of input filenames}
  IncludeListHead: FilenameListPtr; {list of include pathnames or nil}
  define_list_head: filenamelistptr; {macro defs from cmd line}
  undef_list_head: filenamelistptr; {macro undefs from cmd line}

  objname: FilenameListPtr; {object file name pointer or nil}
  macname: FilenameListPtr; {macro file name pointer or nil}
  listname: FilenameListPtr; {list file name pointer or nil}
  envname: FilenameListPtr; {environment file name pointer or nil}
  defname: FilenameListPtr; {define file name pointer or nil}
  prepname: filenamelistptr; {preprocessor output file name}
  stmtname: filenamelistptr; {statment file name}
  symtabname: filenamelistptr; {symbol table name}
  filerememberlist: filerememberptr; {input file name list head}

  tblocknum: integer; {block number to trigger special diagnostics}
  genset: packed set of gentypes; {set of optimizations to carry out in this
                                   block}
  overrideset: packed set of gentypes; {optimizations based on genoptmask}
  genoptmask: integer; {codegen optimization mask}

  { these are global to all due to overlay considerations}
  curfile: integer; {number of current source file}
  morefiles: boolean; {set if more source files}

  early, late, today: integer; {Date rollover check}
  current_line: integer; {used by error}
  current_stmt: integer; {used by error}
  workspace: integer; {for systems where size of temp files may be specified}

  { Special VMS code parameters }

  standardfilesreferenced: boolean; {there may be both own and global psects}

  codesect_string: stringtableindex; {pointer to codesect name}
  codesect_strlength: 0..linelen; {length of codesect name}

  module_string: stringtableindex; {pointer to module name}
  module_strlength: 0..linelen; {length of module name}

  ownsect_string: stringtableindex; {pointer to ownsect name}
  ownsect_strlength: 0..linelen; {length of ownsect name}

  ident_string: stringtableindex; {pointer to ident name}
  ident_strlength: 0..linelen; {length of ident name}

  data_sect: array [section_kind] of data_control; {data section control}

  { Special VERSAdos code parameters }
  codesection: 0..15; {section under which to generate code}
  datasection: 0..15; {read/write data section}
  shortsection: boolean; {section is short}
  objversion, objrevision: 0..255; {version and revision for object code}
  identstring: stringindex; {pointer to ident text}
  identstrlength: 0..linelen; {length of ident text}

  { Special Pascal-F assembly parameters }
  versnstring, {from $VERSN}
   datestring, {from $DATE}
   refnostring, {from $REFNO}
   classstring: stringtableindex; {from $CLASS}
  versnstrlength, datestrlength, refnostrlength, classstrlength: 0..linelen;
  moduletype: (mainmodule, submodule, srmodule); {Pascal-F only}

  { Special unix parameters }
{  unixtarget: unixflavors;}

  envirinfile: bytestream; {environment input file}
  enviroutfile: bytestream; {defined environment output file}

  m2_data_area: integer; {modula2 data area index}
  string_space: var_index; {used in C compiler, needed so genblk will compile}
  psect_desc_head: psect_desc_ptr; {likewise, for putcode}
  scdlink: var_index; {static constructor/destructor vartable index}

  { New debugger statement map control record.  Could be integrated with old
    interface, but I left it separate on the theory that the old junk may
    eventually disappear.
  }

  statement_file:
    record
      mapfile: dbg_stmtfile; {the file itself}
      save_file: boolean; {save this file}
      last_rec: unsignedint; {count of total records}
    end;

implementation

end.
