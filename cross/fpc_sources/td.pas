{[b+]}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1986 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from 
  Oregon Software does not apply to user-modified programs.  All copies 
  of this program must display this notice and all copyright notices. 


  Release version: 0045 Level: 1
  Processor: All
  System: All

  Pascal-2 TRAVRS/IMPROVE/WALK Output Dumper (Debugging Tool)

 Last modified by KRIS on 21-Nov-1990 15:36:10

 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1
}

var
  highestkey: integer;
  nm: packed array [1..255] of char;
  dumping: boolean;


procedure initsets;

{ Set up the nokeydata and oneoperand sets.

  The assignments in this procedure were lifted verbatim from initcode.
}


  begin {initsets}
    nokeydata := [bad, blockentry, blockexit, caseelt, clearlabel, closerange,
                 endpseudocode, joinlabel, jump, jumpf, jumpt, pascallabel,
                 restorelabel, restoreloop, saveactkeys, savelabel, setfile,
                 sysroutine];

    oneoperand := [bad, arraystr, blockexit, chrstr, clearlabel, closerange,
                  copyaccess, copystack, definelazy, doint, doorigin,
                  dounsvar, dovar, endpseudocode, flt, fmt, joinlabel,
                  pseudolabel, pshaddr, pshint, pshlitint, pshlitptr,
                  pshlitreal, pshptr, pshreal, pshset, pshstr, pshstraddr,
                  pshstruct, ptrchk, rdbin, rdchar, rdint, rdreal, rdst,
                  rdxstr, restorelabel, restoreloop, savelabel, setbinfile,
                  setfile, stacktarget, wrbin, wrbool, wrchar, wrint, wrreal,
                  wrst, wrxstr];
  end {initsets} ;


procedure getpseudobuff;

{ Get and unpack the next element in the pseudofile, leaving the result
  in "pseudobuff".
}


  procedure gettempfile;

{ Do the equivalent of a get on the pseudofile, treating the file as a
  file of bytes.  The pseudo-file is actually tempfileone, and the next
  byte is accessed as tempfileone^[nextpseudofile].
}


    begin {gettempfile}
      if nextpseudofile = diskbufsize then
        begin
        nextpseudofile := 0;
        get(tempfileone);
        end
      else nextpseudofile := nextpseudofile + 1;
    end {gettempfile} ;


  function getint: integer;

{ Get an integer value from the file.  This is made into a function to
  allow returning the value into subranges of integer.  Integers are encoded
  in a single byte unless they won't fit.
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


    begin {getint}
      fudge.int := tempfileone^[nextpseudofile].byte;
      gettempfile;
      if fudge.int = hostfilelim then
        for j := 1 to hostintsize * hostfileunits do
          begin
          fudge.byte[j] := tempfileone^[nextpseudofile].byte;
          gettempfile;
          end;
      getint := fudge.int;
    end {getint} ;


  begin {getpseudobuff}
    pseudobuff.op := tempfileone^[nextpseudofile].op;
    gettempfile;

    with pseudobuff do
      begin
      len := 0;
      key := 0;
      refcount := 0;
      copycount := 0;

      { No key data}
      if not (op in nokeydata) then
        begin
        len := getint;
        key := getint;
        refcount := getint;
        copycount := getint;
        end;

      oprnds[1] := getint;
      oprnds[2] := 0;
      oprnds[3] := 0;

      { Only one operand }
      if not (op in oneoperand) then
        begin
        oprnds[2] := getint;
        oprnds[3] := getint;
        end;
      end;
  end {getpseudobuff} ;


procedure dumppseudo;


  begin {dumppseudo}
    dumping := true;
    with pseudobuff do
      begin
      if (op = blockentry) then
        begin
        writeln('op': 12, 'len': 8, 'key': 4, 'ref': 4, 'copy': 5, 'left': 7,
                'right': 6, 'target': 7);
        writeln;
        write('blockentry': 15);
        end
      else if dumping then
        case op of
          addint: write('addint': 15);
          addptr: write('addptr': 15);
          addr: write('addr': 15);
          addreal: write('addreal': 15);
          addset: write('addset': 15);
          addstr: write('addstr': 15);
          aindx: write('aindx': 15);
          andint: write('andint': 15);
          arraystr: write('arraystr': 15);
          bad: write('bad': 15);
          blockcode: write('blockcode': 15);
          blockentry: write('blockentry': 15);
          blockexit: write('blockexit': 15);
          callroutine: write('callroutine': 15);
          casebranch: write('casebranch': 15);
          caseelt: write('caseelt': 15);
          caseerr: write('caseerr': 15);
          castfptrint: write('castfptrint': 15);
          castint: write('castint': 15);
          castintfptr: write('castintfptr': 15);
          castintptr: write('castintptr': 15);
          castptr: write('castptr': 15);
          castptrint: write('castptrint': 15);
          castreal: write('castreal': 15);
          castrealint: write('castrealint': 15);
          chrstr: write('chrstr': 15);
          clearlabel: write('clearlabel': 15);
          closerange: write('closerange': 15);
          commafake: write('commafake': 15);
          compbool: write('compbool': 15);
          compint: write('compint': 15);
          congruchk: write('congruchk': 15);
          copyaccess: write('copyaccess': 15);
          copystack: write('copystack': 15);
          createfalse: write('createfalse': 15);
          createtemp: write('createtemp': 15);
          createtrue: write('createtrue': 15);
          cvtdr: write('cvtdr': 15);
          cvtrd: write('cvtrd': 15);
          dataadd: write('dataadd': 15);
          dataaddr: write('dataaddr': 15);
          dataend: write('dataend': 15);
          datafaddr: write('datafaddr': 15);
          datafield: write('datafield': 15);
          datafill: write('datafill': 15);
          dataint: write('dataint': 15);
          datareal: write('datareal': 15);
          datastart: write('datastart': 15);
          datastore: write('datastore': 15);
          datastruct: write('datastruct': 15);
          datasub: write('datasub': 15);
          decint: write('decint': 15);
          defforindex: write('defforindex': 15);
          defforlitindex: write('defforlitindex': 15);
          definelazy: write('definelazy': 15);
          defunsforindex: write('defunsforindex': 15);
          defunsforlitindex: write('defunsforlitindex': 15);
          divint: write('divint': 15);
          divreal: write('divreal': 15);
          divset: write('divset': 15);
          doext: write('doext': 15);
          dofptr: write('dofptr': 15);
          dofptrvar: write('dofptrvar': 15);
          doint: write('doint': 15);
          dolevel: write('dolevel': 15);
          doorigin: write('doorigin': 15);
          doown: write('doown': 15);
          doptr: write('doptr': 15);
          doptrvar: write('doptrvar': 15);
          doreal: write('doreal': 15);
          doretptr: write('doretptr': 15);
          doset: write('doset': 15);
          dostruct: write('dostruct': 15);
          dotemp: write('dotemp': 15);
          dounsvar: write('dounsvar': 15);
          dovar: write('dovar': 15);
          dummyarg: write('dummyarg': 15);
          dummyarg2: write('dummyarg2': 15);
          endpseudocode: write('endpseudocode': 15);
          endreflex: write('endreflex': 15);
          eqfptr: write('eqfptr': 15);
          eqint: write('eqint': 15);
          eqlitfptr: write('eqlitfptr': 15);
          eqlitint: write('eqlitint': 15);
          eqlitptr: write('eqlitptr': 15);
          eqlitreal: write('eqlitreal': 15);
          eqptr: write('eqptr': 15);
          eqreal: write('eqreal': 15);
          eqset: write('eqset': 15);
          eqstr: write('eqstr': 15);
          eqstruct: write('eqstruct': 15);
          flt: write('flt': 15);
          fmt: write('fmt': 15);
          fordnbottom: write('fordnbottom': 15);
          fordnchk: write('fordnchk': 15);
          fordnimproved: write('fordnimproved': 15);
          fordntop: write('fordntop': 15);
          forerrchk: write('forerrchk': 15);
          forupbottom: write('forupbottom': 15);
          forupchk: write('forupchk': 15);
          forupimproved: write('forupimproved': 15);
          foruptop: write('foruptop': 15);
          geqint: write('geqint': 15);
          geqlitint: write('geqlitint': 15);
          geqlitptr: write('geqlitptr': 15);
          geqlitreal: write('geqlitreal': 15);
          geqptr: write('geqptr': 15);
          geqreal: write('geqreal': 15);
          geqset: write('geqset': 15);
          geqstr: write('geqstr': 15);
          geqstruct: write('geqstruct': 15);
          getquo: write('getquo': 15);
          getrem: write('getrem': 15);
          gtrint: write('gtrint': 15);
          gtrlitint: write('gtrlitint': 15);
          gtrlitptr: write('gtrlitptr': 15);
          gtrlitreal: write('gtrlitreal': 15);
          gtrptr: write('gtrptr': 15);
          gtrreal: write('gtrreal': 15);
          gtrstr: write('gtrstr': 15);
          gtrstruct: write('gtrstruct': 15);
          incint: write('incint': 15);
          incstk: write('incstk': 15);
          indx: write('indx': 15);
          indxchk: write('indxchk': 15);
          indxindr: write('indxindr': 15);
          inset: write('inset': 15);
          joinlabel: write('joinlabel': 15);
          jointemp: write('jointemp': 15);
          jump: write('jump': 15);
          jumpf: write('jumpf': 15);
          jumpt: write('jumpt': 15);
          jumpvfunc: write('jumpvfunc': 15);
          kwoint: write('kwoint': 15);
          leqint: write('leqint': 15);
          leqlitint: write('leqlitint': 15);
          leqlitptr: write('leqlitptr': 15);
          leqlitreal: write('leqlitreal': 15);
          leqptr: write('leqptr': 15);
          leqreal: write('leqreal': 15);
          leqset: write('leqset': 15);
          leqstr: write('leqstr': 15);
          leqstruct: write('leqstruct': 15);
          loopholefn: write('loopholefn': 15);
          lssint: write('lssint': 15);
          lsslitint: write('lsslitint': 15);
          lsslitptr: write('lsslitptr': 15);
          lsslitreal: write('lsslitreal': 15);
          lssptr: write('lssptr': 15);
          lssreal: write('lssreal': 15);
          lssstr: write('lssstr': 15);
          lssstruct: write('lssstruct': 15);
          makeroom: write('makeroom': 15);
          modint: write('modint': 15);
          movcstruct: write('movcstruct': 15);
          movint: write('movint': 15);
          movlitint: write('movlitint': 15);
          movlitptr: write('movlitptr': 15);
          movlitreal: write('movlitreal': 15);
          movptr: write('movptr': 15);
          movreal: write('movreal': 15);
          movset: write('movset': 15);
          movstr: write('movstr': 15);
          movstruct: write('movstruct': 15);
          mulint: write('mulint': 15);
          mulreal: write('mulreal': 15);
          mulset: write('mulset': 15);
          negint: write('negint': 15);
          negreal: write('negreal': 15);
          neqfptr: write('neqfptr': 15);
          neqint: write('neqint': 15);
          neqlitfptr: write('neqlitfptr': 15);
          neqlitint: write('neqlitint': 15);
          neqlitptr: write('neqlitptr': 15);
          neqlitreal: write('neqlitreal': 15);
          neqptr: write('neqptr': 15);
          neqreal: write('neqreal': 15);
          neqset: write('neqset': 15);
          neqstr: write('neqstr': 15);
          neqstruct: write('neqstruct': 15);
          openarray: write('openarray': 15);
          orint: write('orint': 15);
          paindx: write('paindx': 15);
          pascalgoto: write('pascalgoto': 15);
          pascallabel: write('pascallabel': 15);
          pindx: write('pindx': 15);
          postint: write('postint': 15);
          postptr: write('postptr': 15);
          postreal: write('postreal': 15);
          preincptr: write('preincptr': 15);
          pseudolabel: write('pseudolabel': 15);
          pshaddr: write('pshaddr': 15);
          pshfptr: write('pshfptr': 15);
          pshint: write('pshint': 15);
          pshlitfptr: write('pshlitfptr': 15);
          pshlitint: write('pshlitint': 15);
          pshlitptr: write('pshlitptr': 15);
          pshlitreal: write('pshlitreal': 15);
          pshproc: write('pshproc': 15);
          pshptr: write('pshptr': 15);
          pshreal: write('pshreal': 15);
          pshretptr: write('pshretptr': 15);
          pshset: write('pshset': 15);
          pshstr: write('pshstr': 15);
          pshstraddr: write('pshstraddr': 15);
          pshstruct: write('pshstruct': 15);
          ptrchk: write('ptrchk': 15);
          ptrtemp: write('ptrtemp': 15);
          rangechk: write('rangechk': 15);
          rdbin: write('rdbin': 15);
          rdchar: write('rdchar': 15);
          rdint: write('rdint': 15);
          rdreal: write('rdreal': 15);
          rdst: write('rdst': 15);
          rdxstr: write('rdxstr': 15);
          realtemp: write('realtemp': 15);
          regtemp: write('regtemp': 15);
          restorelabel: write('restorelabel': 15);
          restoreloop: write('restoreloop': 15);
          returnfptr: write('returnfptr': 15);
          returnint: write('returnint': 15);
          returnptr: write('returnptr': 15);
          returnreal: write('returnreal': 15);
          returnstruct: write('returnstruct': 15);
          savelabel: write('savelabel': 15);
          saveactkeys: write('saveactkeys': 15);
          setbinfile: write('setbinfile': 15);
          setfile: write('setfile': 15);
          setinsert: write('setinsert': 15);
          shiftlint: write('shiftlint': 15);
          shiftrint: write('shiftrint': 15);
          stacktarget: write('stacktarget': 15);
          startreflex: write('startreflex': 15);
          stddivint: write('stddivint': 15);
          stdmodint: write('stdmodint': 15);
          stmtbrk: write('stmtbrk': 15);
          subint: write('subint': 15);
          subptr: write('subptr': 15);
          subreal: write('subreal': 15);
          subset: write('subset': 15);
          sysfnint: write('sysfnint': 15);
          sysfnreal: write('sysfnreal': 15);
          sysfnstring: write('sysfnstring': 15);
          sysroutine: write('sysroutine': 15);
          temptarget: write('temptarget': 15);
          unscallroutine: write('unscallroutine': 15);
          wrbin: write('wrbin': 15);
          wrbool: write('wrbool': 15);
          wrchar: write('wrchar': 15);
          wrint: write('wrint': 15);
          wrreal: write('wrreal': 15);
          wrst: write('wrst': 15);
          wrxstr: write('wrxstr': 15);
          xorint: write('xorint': 15);
          otherwise write(ord(op): 15);
          end;
      if dumping then
        writeln(',', len: 3, ',(', key: 3, ',', refcount: 3, ',', copycount:
                3, ') ', oprnds[1]: 5, ' ', oprnds[2]: 5, ' ', oprnds[3]: 5);
      end;
  end {dumppseudo} ;


begin {td}

  case hostopsys of
    vdos: reset(tempfileone, 'temp1.tm');
    otherwise reset(tempfileone, 'temp1.tmp');
    end;
  nextpseudofile := 0;
  case hostopsys of
    unix, msdos: ;
    otherwise
      begin
      write('Output: ');
      readln(nm);
      rewrite(output, nm, '.td');
      end;
    end;

  initsets;
  highestkey := 0;
  getpseudobuff;

  while (pseudobuff.op <> endpseudocode) and not eof(tempfileone) do
    begin
    if pseudobuff.key > highestkey then highestkey := pseudobuff.key;
    dumppseudo;
    getpseudobuff;
    end;

  writeln('*** end pseudocode ***');
  writeln('Highest key: ', highestkey: 1);

  close(output);
  close(tempfileone);
end {td} .
