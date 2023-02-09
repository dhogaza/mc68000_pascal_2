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

  Pascal-2 Compiler TRAVRS-CODE Interface Declarations

 Last modified by KRIS on 21-Nov-1990 15:23:21
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}

unit t_c;

interface

uses config;

type

  { Operators emitted by travrs, read by code }

  pseudoop = (bad, addint, addptr, addr, addreal, addset, addstr, aindx, andint,
              arraystr, blockcode, blockentry, blockexit, callroutine,
              casebranch, caseelt, caseerr, castfptrint, castint, castintfptr,
              castintptr, castptr, castptrint, castreal, castrealint, chrstr,
              clearlabel, closerange, commafake, compbool, compint, congruchk,
              copyaccess, copystack, createfalse, createtemp, createtrue,
              cvtdr, cvtrd, dataadd, dataaddr, dataend, datafaddr, datafield,
              datafill, dataint, datareal, datastart, datastore, datastruct,
              datasub, decint, defforindex, defforlitindex, definelazy,
              defunsforindex, defunsforlitindex, divint, divreal, divset,
              doext, dofptr, dofptrvar, doint, dolevel, doorigin, doown,
              doptr, doptrvar, doreal, doretptr, doseg, doset, dostruct,
              dotemp, dounsvar, dovar, dummyarg, dummyarg2, endpseudocode,
              endreflex, eqfptr, eqint, eqlitfptr, eqlitint, eqlitptr,
              eqlitreal, eqptr, eqreal, eqset, eqstr, eqstruct, flt, fmt,
              fordnbottom, fordnchk, fordnimproved, fordntop, forerrchk,
              forupbottom, forupchk, forupimproved, foruptop, geqint,
              geqlitint, geqlitptr, geqlitreal, geqptr, geqreal, geqset,
              geqstr, geqstruct, getquo, getrem, gtrint, gtrlitint, gtrlitptr,
              gtrlitreal, gtrptr, gtrreal, gtrstr, gtrstruct, incint, incstk,
              indx, indxchk, indxindr, inset, joinlabel, jointemp, jump,
              jumpf, jumpvfunc, jumpt, kwoint, leqint, leqlitint, leqlitptr,
              leqlitreal, leqptr, leqreal, leqset, leqstr, leqstruct,
              loopholefn, lssint, lsslitint, lsslitptr, lsslitreal, lssptr,
              lssreal, lssstr, lssstruct, makeroom, modint, movcstruct,
              movint, movlitint, movlitptr, movlitreal, movptr, movreal,
              movset, movstr, movstruct, mulint, mulreal, mulset, negint,
              negreal, neqfptr, neqint, neqlitfptr, neqlitint, neqlitptr,
              neqlitreal, neqptr, neqreal, neqset, neqstr, neqstruct,
              openarray, orint, paindx, pascalgoto, pascallabel, pindx,
              postint, postptr, postreal, preincptr, pseudolabel, pshaddr,
              pshfptr, pshint, pshlitfptr, pshlitint, pshlitptr, pshlitreal,
              pshproc, pshptr, pshreal, pshretptr, pshset, pshstr, pshstraddr,
              pshstruct, ptrchk, ptrtemp, rangechk, rdbin, rdchar, rdint,
              rdreal, rdst, rdxstr, realtemp, regtemp, restorelabel,
              restoreloop, returnfptr, returnint, returnptr, returnreal,
              returnstruct, savelabel, saveactkeys, setbinfile, setfile,
              setinsert, shiftlint, shiftrint, stacktarget, startreflex,
              stddivint, stdmodint, stmtbrk, subint, subptr, subreal, subset,
              sysfnint, sysfnreal, sysfnstring, sysroutine, temptarget,
              unscallroutine, wrbin, wrbool, wrchar, wrint, wrreal, wrst,
              wrxstr, xorint);

  { Define interface from travrs to code }

  keyindex = - keysize..keysize; {actual key table index}

  pseudocode =
    record { no need to pack }
      len: integer; {operand length}
      op: pseudoop; {operator}
      key: keyindex; {node label for flattened tree}
      refcount: integer; {number of references to this node}
      copycount: integer; {number of copies of this node}
      oprnds: array [1..3] of integer {misc. operands}
    end;

var

  pseudoinst: pseudocode; {next pseudo-instruction}
  pseudobuff: pseudocode; {next + 1 pseudo-instruction}

implementation

end.
