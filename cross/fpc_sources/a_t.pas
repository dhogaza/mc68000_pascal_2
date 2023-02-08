{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986, 1989 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 

  %W%  %G% %U%

  Release version: 0045 Level: 1
  Processor: All
  System: All

  Pascal-2 Compiler ANALYS-TRAVRS Interface Declarations

 Last modified by KRIS on 21-Nov-1990 15:19:05
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1
}

unit a_t;

interface

uses config, hdr;

const
  {distinguished offsets for c, set to avoid conflict with all other offsets}

  ptr_any_off = $3FFFFFFF; { Could point anywhere }
  ptr_ext_off = $3FFFFFFE; {points to global variables only}
  ptr_loc_off = $3FFFFFFD; {points to local variables only}

  { statement operators emitted by analys, read by travrs }

type

  stmttype = (begblk, endblk, begwhile, endwhile, begrpt, endrpt, begfor,
              forup, fordn, endfor, begif, endelse, begelse, endthen, gotolab,
              deflab, begwith, endwith, begcase, caselab, casedef, endcaseelt,
              caselabrange, endcase, begcfor, endcfor, begreturn, begloop,
              endloop, begexit, blksize, hiddenstmt, begswitch, endswitch,
              inits, loopbreak, loopcont, switchbreak, syscall, simple,
              begdata, endall);


  { expression operators emitted by analys, read by travrs }
  { DO NOT ALPHABETIZE OR OTHERWISE SCREW AROUND WITH THE ORDERING }

  {operators before intop are linked by travrs sequentially and are never CSEs}

  operatortype = (endexpr, newvarop, newunsvarop, globalop, localop, lit,
              defforlitindexop, defforindexop, forindexop,
              defunsforlitindexop, defunsforindexop, forupchkop, fordnchkop,
              forerrchkop, moveop, cmoveop, movelit, lssop, leqop, neqop,
              eqop, gtrop, geqop, lsslit, leqlit, eqlit, neqlit, gtrlit,
              geqlit, withop, setfileop, setfileaddrop, setinput, newset,
              bldset, setelt, setpair, bldnil, bldfmt, inop, addrop, pushaddr,
              pushstraddr, pushvalue, pushcvalue, pushfinal, pushlitvalue,
              pushret, pushfptr, retop, call, jumpvfuncop,
              unscall, callparam, unscallparam, copystackop, reserve,
              pushproc, rd, wr, switchstack, structop, closerangeop,
              dummyargop, filebufindrop, intop, ptrop, realop, doubleop,
              varop, unsvarop, definelazyop, ownop, segop, extop, levop,
              tempop, originop, float1, float, float_double, real_to_dbl,
              dbl_to_real, plusop, dummyarg2op, minusop, mulop, divop,
              stddivop, remop, quoop, kwoop, modop, stdmodop, negop, slashop,
              setbinfileop, shiftlop, incop, decop, indrop, indxop, aindxop,
              pindxop, paindxop, chrstrop, chrstrop1, arraystrop, arraystrop1,
              sysfn, andop, orop, notop, loopholeop, indxchkop, ptrchkop,
              congruchkop, cindxchkop, rangechkop, openarrayop, fptrop,
              returnop, uretop, shiftrop, addeqop, andeqop, begscope,
              castintop, castptrop, castfptrop, castrealop, commaop, compop,
              daddop, daddrop, dendop, dfaddrop, dfieldop, dfillop, dintop,
              diveqop, drealop, dstartop, dstoreop, dstructop, dsubop,
              endscope, groupop, modeqop, muleqop, oreqop, preincop,
              postincop, questop, shiftleqop, shiftreqop, subeqop, varkind,
              xoreqop, xorop, vindxop, parmop, clearnewop, saveop, restop,
              deleteop);

  { define the different kinds of records emitted by analys }

  intcodetype = (stmt, op, literal, form);

  tempfiletwocomponent =
    packed record
	case intcode: intcodetype of
        stmt: (s: stmttype);
        op: (o: operatortype);
        literal: (b: hostfilebyte);
        form: (f: types)
    end;

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

var
  tempfiletwo: file of tempfiletwocomponent;
  tempfilebuf: tempfiletwocomponent;
  locals: localfiletype;

implementation
end.
